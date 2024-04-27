use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// AsyncGeneratorMethod[Yield, Await] :
//      async [no LineTerminator here] * ClassElementName[?Yield, ?Await] ( UniqueFormalParameters[+Yield, +Await] ) { AsyncGeneratorBody }
#[derive(Debug)]
pub struct AsyncGeneratorMethod {
    name: Rc<ClassElementName>,
    params: Rc<UniqueFormalParameters>,
    body: Rc<AsyncGeneratorBody>,
    location: Location,
}

impl fmt::Display for AsyncGeneratorMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "async * {} ( {} ) {{ {} }}", self.name, self.params, self.body)
    }
}

impl PrettyPrint for AsyncGeneratorMethod {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AsyncGeneratorMethod: {self}")?;
        self.name.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.body.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AsyncGeneratorMethod: {self}")?;
        pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "*", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.name.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.params.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl AsyncGeneratorMethod {
    // AsyncGeneratorMethod: No caching needed. Parent: MethodDefinition
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (async_loc, after_async) =
            scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        let (_, after_star) = scan_for_punct(after_async, parser.source, ScanGoal::InputElementDiv, Punctuator::Star)?;
        let (name, after_name) = ClassElementName::parse(parser, after_star, yield_flag, await_flag)?;
        let (_, after_lp) =
            scan_for_punct(after_name, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_params) = UniqueFormalParameters::parse(parser, after_lp, true, true);
        let (_, after_rp) =
            scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let (_, after_lb) = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = AsyncGeneratorBody::parse(parser, after_lb);
        let (rb_loc, after_rb) =
            scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        let location = async_loc.merge(&rb_loc);
        Ok((Rc::new(AsyncGeneratorMethod { name, params, body, location }), after_rb))
    }

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.name.contains(kind) || self.params.contains(kind) || self.body.contains(kind)
    }

    pub fn computed_property_contains(&self, kind: ParseNodeKind) -> bool {
        self.name.computed_property_contains(kind)
    }

    pub fn private_bound_identifier(&self) -> Option<JSString> {
        // Static Semantics: PrivateBoundIdentifiers
        // AsyncGeneratorMethod : async ClassElementName ( UniqueFormalParameters ) { AsyncGeneratorBody }
        //  1. Return PrivateBoundIdentifiers of ClassElementName.
        self.name.private_bound_identifier()
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.name.all_private_identifiers_valid(names)
            && self.params.all_private_identifiers_valid(names)
            && self.body.all_private_identifiers_valid(names)
    }

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. Return ContainsArguments of ClassElementName.
        self.name.contains_arguments()
    }

    pub fn has_direct_super(&self) -> bool {
        // Static Semantics: HasDirectSuper
        //      The syntax-directed operation HasDirectSuper takes no arguments.
        // AsyncGeneratorMethod : async * ClassElementName ( UniqueFormalParameters ) { AsyncGeneratorBody }
        //  1. If UniqueFormalParameters Contains SuperCall is true, return true.
        //  2. Return AsyncGeneratorBody Contains SuperCall.
        self.params.contains(ParseNodeKind::SuperCall) || self.body.contains(ParseNodeKind::SuperCall)
    }

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Async Generator Function Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-async-generator-function-definitions-static-semantics-early-errors
    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        //  AsyncGeneratorMethod : async * ClassElementName ( UniqueFormalParameters ) { AsyncGeneratorBody }
        //  * It is a Syntax Error if HasDirectSuper of AsyncGeneratorMethod is true.
        //  * It is a Syntax Error if UniqueFormalParameters Contains YieldExpression is true.
        //  * It is a Syntax Error if UniqueFormalParameters Contains AwaitExpression is true.
        //  * It is a Syntax Error if FunctionBodyContainsUseStrict of AsyncGeneratorBody is true and
        //    IsSimpleParameterList of UniqueFormalParameters is false.
        //  * It is a Syntax Error if any element of the BoundNames of UniqueFormalParameters also occurs in the
        //    LexicallyDeclaredNames of AsyncGeneratorBody.
        let cus = self.body.function_body_contains_use_strict();
        if self.has_direct_super() {
            errs.push(create_syntax_error_object("Calls to ‘super’ not allowed here", Some(self.location)));
        }
        if self.params.contains(ParseNodeKind::YieldExpression) {
            errs.push(create_syntax_error_object(
                "Yield expressions can't be parameter initializers in generators",
                Some(self.params.location()),
            ));
        }
        if cus && !self.params.is_simple_parameter_list() {
            errs.push(create_syntax_error_object(
                "Illegal 'use strict' directive in function with non-simple parameter list",
                Some(self.params.location()),
            ));
        }
        let bn = self.params.bound_names();
        for name in self.body.lexically_declared_names().into_iter().filter(|ldn| bn.contains(ldn)) {
            errs.push(create_syntax_error_object(format!("‘{name}’ already defined"), Some(self.body.0.location())));
        }

        let strict_func = strict || cus;

        self.name.early_errors(errs, strict_func);
        self.params.early_errors(errs, strict_func);
        self.body.early_errors(errs, strict_func);
    }

    pub fn prop_name(&self) -> Option<JSString> {
        // Static Semantics: PropName
        // The syntax-directed operation PropName takes no arguments and returns a String or empty.
        //      AsyncGeneratorMethod : async * ClassElementName ( UniqueFormalParameters ) { AsyncGeneratorBody }
        //  1. Return PropName of ClassElementName.
        self.name.prop_name()
    }
}

// AsyncGeneratorDeclaration[Yield, Await, Default] :
//      async [no LineTerminator here] function * BindingIdentifier[?Yield, ?Await] ( FormalParameters[+Yield, +Await] ) { AsyncGeneratorBody }
//      [+Default] async [no LineTerminator here] function * ( FormalParameters[+Yield, +Await] ) { AsyncGeneratorBody }
#[derive(Debug)]
pub struct AsyncGeneratorDeclaration {
    ident: Option<Rc<BindingIdentifier>>,
    params: Rc<FormalParameters>,
    body: Rc<AsyncGeneratorBody>,
    location: Location,
}

impl fmt::Display for AsyncGeneratorDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            None => {
                write!(f, "async function * ( {} ) {{ {} }}", self.params, self.body)
            }
            Some(id) => write!(f, "async function * {} ( {} ) {{ {} }}", id, self.params, self.body),
        }
    }
}

impl PrettyPrint for AsyncGeneratorDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AsyncGeneratorDeclaration: {self}")?;
        if let Some(id) = &self.ident {
            id.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        self.params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.body.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AsyncGeneratorDeclaration: {self}")?;
        pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "function", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "*", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        if let Some(id) = &self.ident {
            id.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.params.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl IsFunctionDefinition for AsyncGeneratorDeclaration {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl AsyncGeneratorDeclaration {
    // No caching needed. Parent: HoistableDeclaration
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        default_flag: bool,
    ) -> ParseResult<Self> {
        let (async_loc, after_async) =
            scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let (_, after_func) =
            scan_for_keyword(after_async, parser.source, ScanGoal::InputElementRegExp, Keyword::Function)?;
        let (_, after_star) = scan_for_punct(after_func, parser.source, ScanGoal::InputElementDiv, Punctuator::Star)?;
        let (ident, after_bi) = match BindingIdentifier::parse(parser, after_star, yield_flag, await_flag) {
            Err(err) => {
                if default_flag {
                    Ok((None, after_star))
                } else {
                    Err(err)
                }
            }
            Ok((node, scan)) => Ok((Some(node), scan)),
        }?;
        let (_, after_lp) = scan_for_punct(after_bi, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_fp) = FormalParameters::parse(parser, after_lp, true, true);
        let (_, after_rp) = scan_for_punct(after_fp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let (_, after_lb) = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = AsyncGeneratorBody::parse(parser, after_lb);
        let (rb_loc, after_rb) =
            scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        let location = async_loc.merge(&rb_loc);
        Ok((Rc::new(AsyncGeneratorDeclaration { ident, params, body, location }), after_rb))
    }

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        vec![self.bound_name()]
    }

    pub fn bound_name(&self) -> JSString {
        match &self.ident {
            None => JSString::from("*default*"),
            Some(node) => node.bound_name(),
        }
    }

    pub fn contains(&self, _kind: ParseNodeKind) -> bool {
        false
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.params.all_private_identifiers_valid(names) && self.body.all_private_identifiers_valid(names)
    }

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Async Generator Function Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-async-generator-function-definitions-static-semantics-early-errors
    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        //  AsyncGeneratorDeclaration :
        //      async function * BindingIdentifier ( FormalParameters ) { AsyncGeneratorBody }
        //      async function * ( FormalParameters ) { AsyncGeneratorBody }
        //  * If the source text matched by FormalParameters is strict mode code, the Early Error rules for
        //    UniqueFormalParameters : FormalParameters are applied.
        //  * If BindingIdentifier is present and the source text matched by BindingIdentifier is strict mode code, it
        //    is a Syntax Error if the StringValue of BindingIdentifier is "eval" or "arguments".
        //  * It is a Syntax Error if FunctionBodyContainsUseStrict of AsyncGeneratorBody is true and
        //    IsSimpleParameterList of FormalParameters is false.
        //  * It is a Syntax Error if any element of the BoundNames of FormalParameters also occurs in the
        //    LexicallyDeclaredNames of AsyncGeneratorBody.
        //  * It is a Syntax Error if FormalParameters Contains YieldExpression is true.
        //  * It is a Syntax Error if FormalParameters Contains AwaitExpression is true.
        //  * It is a Syntax Error if FormalParameters Contains SuperProperty is true.
        //  * It is a Syntax Error if AsyncGeneratorBody Contains SuperProperty is true.
        //  * It is a Syntax Error if FormalParameters Contains SuperCall is true.
        //  * It is a Syntax Error if AsyncGeneratorBody Contains SuperCall is true.
        let strict_function = function_early_errors(errs, strict, self.ident.as_ref(), &self.params, &self.body.0);
        if self.params.contains(ParseNodeKind::YieldExpression) {
            errs.push(create_syntax_error_object(
                "Yield expressions can't be parameter initializers in generators",
                Some(self.params.location()),
            ));
        }
        if self.params.contains(ParseNodeKind::AwaitExpression) {
            errs.push(create_syntax_error_object(
                "Await expressions can't be parameter initializers in async functions",
                Some(self.params.location()),
            ));
        }

        if let Some(bi) = &self.ident {
            bi.early_errors(errs, strict_function);
        }
        self.params.early_errors(errs, strict_function, strict_function);
        self.body.early_errors(errs, strict_function);
    }

    pub fn is_constant_declaration(&self) -> bool {
        false
    }
}

// AsyncGeneratorExpression :
//      async [no LineTerminator here] function * BindingIdentifier[+Yield, +Await]opt ( FormalParameters[+Yield, +Await] ) { AsyncGeneratorBody }
#[derive(Debug)]
pub struct AsyncGeneratorExpression {
    pub ident: Option<Rc<BindingIdentifier>>,
    pub params: Rc<FormalParameters>,
    pub body: Rc<AsyncGeneratorBody>,
    location: Location,
}

impl fmt::Display for AsyncGeneratorExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            Some(id) => write!(f, "async function * {} ( {} ) {{ {} }}", id, self.params, self.body),
            None => {
                write!(f, "async function * ( {} ) {{ {} }}", self.params, self.body)
            }
        }
    }
}

impl PrettyPrint for AsyncGeneratorExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AsyncGeneratorExpression: {self}")?;
        if let Some(id) = &self.ident {
            id.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        self.params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.body.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AsyncGeneratorExpression: {self}")?;
        pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "function", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "*", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        if let Some(id) = &self.ident {
            id.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.params.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl IsFunctionDefinition for AsyncGeneratorExpression {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl AsyncGeneratorExpression {
    // No caching needed. Parent: PrimaryExpression.
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (async_loc, after_async) =
            scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let (_, after_func) =
            scan_for_keyword(after_async, parser.source, ScanGoal::InputElementDiv, Keyword::Function)?;
        let (_, after_star) = scan_for_punct(after_func, parser.source, ScanGoal::InputElementDiv, Punctuator::Star)?;
        let (ident, after_ident) = match BindingIdentifier::parse(parser, after_star, true, true) {
            Err(_) => (None, after_star),
            Ok((node, scan)) => (Some(node), scan),
        };
        let (_, after_lp) =
            scan_for_punct(after_ident, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_params) = FormalParameters::parse(parser, after_lp, true, true);
        let (_, after_rp) =
            scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let (_, after_lb) = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = AsyncGeneratorBody::parse(parser, after_lb);
        let (rb_loc, after_rb) =
            scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        let location = async_loc.merge(&rb_loc);
        Ok((Rc::new(AsyncGeneratorExpression { ident, params, body, location }), after_rb))
    }

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn contains(&self, _: ParseNodeKind) -> bool {
        false
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.params.all_private_identifiers_valid(names) && self.body.all_private_identifiers_valid(names)
    }

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Async Generator Function Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-async-generator-function-definitions-static-semantics-early-errors
    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        //  AsyncGeneratorExpression :
        //      async function * BindingIdentifier[opt] ( FormalParameters ) { AsyncGeneratorBody }
        //  * If the source text matched by FormalParameters is strict mode code, the Early Error rules for
        //    UniqueFormalParameters : FormalParameters are applied.
        //  * If BindingIdentifier is present and the source text matched by BindingIdentifier is strict mode code, it
        //    is a Syntax Error if the StringValue of BindingIdentifier is "eval" or "arguments".
        //  * It is a Syntax Error if FunctionBodyContainsUseStrict of AsyncGeneratorBody is true and
        //    IsSimpleParameterList of FormalParameters is false.
        //  * It is a Syntax Error if any element of the BoundNames of FormalParameters also occurs in the
        //    LexicallyDeclaredNames of AsyncGeneratorBody.
        //  * It is a Syntax Error if FormalParameters Contains YieldExpression is true.
        //  * It is a Syntax Error if FormalParameters Contains AwaitExpression is true.
        //  * It is a Syntax Error if FormalParameters Contains SuperProperty is true.
        //  * It is a Syntax Error if AsyncGeneratorBody Contains SuperProperty is true.
        //  * It is a Syntax Error if FormalParameters Contains SuperCall is true.
        //  * It is a Syntax Error if AsyncGeneratorBody Contains SuperCall is true.
        function_early_errors(errs, strict, self.ident.as_ref(), &self.params, &self.body.0);
        if self.params.contains(ParseNodeKind::YieldExpression) {
            errs.push(create_syntax_error_object(
                "Yield expressions can't be parameter initializers in generators",
                Some(self.params.location()),
            ));
        }
        if self.params.contains(ParseNodeKind::AwaitExpression) {
            errs.push(create_syntax_error_object(
                "Await expressions can't be parameter initializers in async functions",
                Some(self.params.location()),
            ));
        }

        // Don't need to check the child nodes, as function_early_errors, above, already did.
    }

    pub fn is_named_function(&self) -> bool {
        self.ident.is_some()
    }
}

// AsyncGeneratorBody :
//      FunctionBody[+Yield, +Await]
#[derive(Debug)]
pub struct AsyncGeneratorBody(pub Rc<FunctionBody>);

impl fmt::Display for AsyncGeneratorBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PrettyPrint for AsyncGeneratorBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AsyncGeneratorBody: {self}")?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.0.concise_with_leftpad(writer, pad, state)
    }
}

impl AsyncGeneratorBody {
    fn parse_core(parser: &mut Parser, scanner: Scanner) -> (Rc<Self>, Scanner) {
        let (body, after_body) = FunctionBody::parse(parser, scanner, true, true);
        (Rc::new(AsyncGeneratorBody(body)), after_body)
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner) -> (Rc<Self>, Scanner) {
        match parser.async_generator_body_cache.get(&scanner) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner);
                parser.async_generator_body_cache.insert(scanner, result.clone());
                result
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.0.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.0.all_private_identifiers_valid(names)
    }

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Async Generator Function Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-async-generator-function-definitions-static-semantics-early-errors
    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.0.early_errors(errs, strict);
    }

    /// Return a list of identifiers defined by the `var` statement for this node.
    ///
    /// Note that function bodies are treated like top-level code in that top-level function identifiers are part
    /// of the var-declared list.
    ///
    /// See [VarDeclaredNames](https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames) from ECMA-262.
    pub fn var_declared_names(&self) -> Vec<JSString> {
        self.0.var_declared_names()
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        self.0.var_scoped_declarations()
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        self.0.lexically_declared_names()
    }

    pub fn lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        self.0.lexically_scoped_declarations()
    }

    pub fn function_body_contains_use_strict(&self) -> bool {
        self.0.function_body_contains_use_strict()
    }
}

#[cfg(test)]
mod tests;
