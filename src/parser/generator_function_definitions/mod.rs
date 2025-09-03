use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// GeneratorMethod[Yield, Await] :
//      * ClassElementName[?Yield, ?Await] ( UniqueFormalParameters[+Yield, ~Await] ) { GeneratorBody }
#[derive(Debug)]
pub(crate) struct GeneratorMethod {
    pub(crate) name: Rc<ClassElementName>,
    pub(crate) params: Rc<UniqueFormalParameters>,
    pub(crate) body: Rc<GeneratorBody>,
    location: Location,
}

impl fmt::Display for GeneratorMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "* {} ( ", self.name)?;
        if !matches!(self.params.formals.as_ref(), FormalParameters::Empty(..)) {
            write!(f, "{} ", self.params)?;
        }
        write!(f, ") {{ ")?;
        if !matches!(self.body.0.statements.as_ref(), FunctionStatementList::Empty(..)) {
            write!(f, "{} ", self.body)?;
        }
        write!(f, "}}")
    }
}

impl PrettyPrint for GeneratorMethod {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}GeneratorMethod: {self}")?;
        self.name.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.body.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}GeneratorMethod: {self}")?;
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

impl GeneratorMethod {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (star_loc, after_star) =
            scan_for_punct(scanner, parser.source, InputElementGoal::RegExp, Punctuator::Star)?;
        let (name, after_name) = ClassElementName::parse(parser, after_star, yield_flag, await_flag)?;
        let (_, after_lp) = scan_for_punct(after_name, parser.source, InputElementGoal::Div, Punctuator::LeftParen)?;
        let (params, after_params) = UniqueFormalParameters::parse(parser, after_lp, true, false);
        let (_, after_rp) = scan_for_punct(after_params, parser.source, InputElementGoal::Div, Punctuator::RightParen)?;
        let (_, after_lb) = scan_for_punct(after_rp, parser.source, InputElementGoal::Div, Punctuator::LeftBrace)?;
        let (body, after_body) = GeneratorBody::parse(parser, after_lb);
        let (rb_loc, after_rb) =
            scan_for_punct(after_body, parser.source, InputElementGoal::Div, Punctuator::RightBrace)?;
        let location = star_loc.merge(&rb_loc);
        Ok((Rc::new(GeneratorMethod { name, params, body, location }), after_rb))
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }

    #[cfg(test)]
    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        self.name.contains(kind) || self.params.contains(kind) || self.body.contains(kind)
    }

    pub(crate) fn computed_property_contains(&self, kind: ParseNodeKind) -> bool {
        self.name.computed_property_contains(kind)
    }

    pub(crate) fn private_bound_identifier(&self) -> Option<JSString> {
        // Static Semantics: PrivateBoundIdentifiers
        // GeneratorMethod : * ClassElementName ( UniqueFormalParameters ) { GeneratorBody }
        //  1. Return PrivateBoundIdentifiers of ClassElementName.
        self.name.private_bound_identifier()
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
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
    pub(crate) fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. Return ContainsArguments of ClassElementName.
        self.name.contains_arguments()
    }

    pub(crate) fn has_direct_super(&self) -> bool {
        // Static Semantics: HasDirectSuper
        //      The syntax-directed operation HasDirectSuper takes no arguments.
        // GeneratorMethod : * ClassElementName ( UniqueFormalParameters ) { GeneratorBody }
        //  1. If UniqueFormalParameters Contains SuperCall is true, return true.
        //  2. Return GeneratorBody Contains SuperCall.
        self.params.contains(ParseNodeKind::SuperCall) || self.body.contains(ParseNodeKind::SuperCall)
    }

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Generator Function Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-generator-function-definitions-static-semantics-early-errors
    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        //  GeneratorMethod : * ClassElementName ( UniqueFormalParameters ) { GeneratorBody }
        //  * It is a Syntax Error if HasDirectSuper of GeneratorMethod is true.
        //  * It is a Syntax Error if UniqueFormalParameters Contains YieldExpression is true.
        //  * It is a Syntax Error if FunctionBodyContainsUseStrict of GeneratorBody is true and IsSimpleParameterList
        //    of UniqueFormalParameters is false.
        //  * It is a Syntax Error if any element of the BoundNames of UniqueFormalParameters also occurs in the
        //    LexicallyDeclaredNames of GeneratorBody.
        let cus = self.body.function_body_contains_use_strict();
        if self.has_direct_super() {
            errs.push(create_syntax_error_object("Calls to ‘super’ not allowed here", Some(self.body.location())));
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
            errs.push(create_syntax_error_object(format!("‘{name}’ already defined"), Some(self.body.location())));
        }

        let strict_func = strict || cus;

        self.name.early_errors(errs, strict_func);
        self.params.early_errors(errs, strict_func);
        self.body.early_errors(errs, strict_func);
    }

    pub(crate) fn prop_name(&self) -> Option<JSString> {
        // Static Semantics: PropName
        // The syntax-directed operation PropName takes no arguments and returns a String or empty.
        //      GeneratorMethod : * ClassElementName ( UniqueFormalParameters ) { GeneratorBody }
        //  1. Return PropName of ClassElementName.
        self.name.prop_name()
    }

    #[expect(unused_variables)]
    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        todo!()
    }
}

// GeneratorDeclaration[Yield, Await, Default] :
//      function * BindingIdentifier[?Yield, ?Await] ( FormalParameters[+Yield, ~Await] ) { GeneratorBody }
//      [+Default] function * ( FormalParameters[+Yield, ~Await] ) { GeneratorBody }
#[derive(Debug)]
pub(crate) struct GeneratorDeclaration {
    pub(crate) ident: Option<Rc<BindingIdentifier>>,
    pub(crate) params: Rc<FormalParameters>,
    pub(crate) body: Rc<GeneratorBody>,
    location: Location,
}

impl fmt::Display for GeneratorDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function * ")?;
        if let Some(id) = &self.ident {
            write!(f, "{id} ")?;
        }
        write!(f, "( ")?;
        if !matches!(self.params.as_ref(), FormalParameters::Empty(..)) {
            write!(f, "{} ", self.params)?;
        }
        write!(f, ") {{ ")?;
        if !matches!(self.body.0.statements.as_ref(), FunctionStatementList::Empty(..)) {
            write!(f, "{} ", self.body)?;
        }
        write!(f, "}}")
    }
}

impl PrettyPrint for GeneratorDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}GeneratorDeclaration: {self}")?;
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
        writeln!(writer, "{first}GeneratorDeclaration: {self}")?;
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

impl GeneratorDeclaration {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        default_flag: bool,
    ) -> ParseResult<Self> {
        let (func_loc, after_func) =
            scan_for_keyword(scanner, parser.source, InputElementGoal::RegExp, Keyword::Function)?;
        let (_, after_star) = scan_for_punct(after_func, parser.source, InputElementGoal::Div, Punctuator::Star)?;
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
        let (_, after_lp) = scan_for_punct(after_bi, parser.source, InputElementGoal::Div, Punctuator::LeftParen)?;
        let (params, after_fp) = FormalParameters::parse(parser, after_lp, true, false);
        let (_, after_rp) = scan_for_punct(after_fp, parser.source, InputElementGoal::Div, Punctuator::RightParen)?;
        let (_, after_lb) = scan_for_punct(after_rp, parser.source, InputElementGoal::Div, Punctuator::LeftBrace)?;
        let (body, after_body) = GeneratorBody::parse(parser, after_lb);
        let (rb_loc, after_rb) =
            scan_for_punct(after_body, parser.source, InputElementGoal::Div, Punctuator::RightBrace)?;
        let location = func_loc.merge(&rb_loc);
        Ok((Rc::new(GeneratorDeclaration { ident, params, body, location }), after_rb))
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }

    pub(crate) fn bound_names(&self) -> Vec<JSString> {
        vec![self.bound_name()]
    }

    pub(crate) fn bound_name(&self) -> JSString {
        match &self.ident {
            None => JSString::from("*default*"),
            Some(node) => node.bound_name(),
        }
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
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
    /// See [Early Errors for Generator Function Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-generator-function-definitions-static-semantics-early-errors
    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        //  GeneratorDeclaration :
        //      function * BindingIdentifier ( FormalParameters ) { GeneratorBody }
        //      function * ( FormalParameters ) { GeneratorBody }
        //  * If the source text matched by FormalParameters is strict mode code, the Early Error rules for
        //    UniqueFormalParameters : FormalParameters are applied.
        //  * If BindingIdentifier is present and the source text matched by BindingIdentifier is strict mode code, it
        //    is a Syntax Error if the StringValue of BindingIdentifier is "eval" or "arguments".
        //  * It is a Syntax Error if FunctionBodyContainsUseStrict of GeneratorBody is true and IsSimpleParameterList
        //    of FormalParameters is false.
        //  * It is a Syntax Error if any element of the BoundNames of FormalParameters also occurs in the
        //    LexicallyDeclaredNames of GeneratorBody.
        //  * It is a Syntax Error if FormalParameters Contains YieldExpression is true.
        //  * It is a Syntax Error if FormalParameters Contains SuperProperty is true.
        //  * It is a Syntax Error if GeneratorBody Contains SuperProperty is true.
        //  * It is a Syntax Error if FormalParameters Contains SuperCall is true.
        //  * It is a Syntax Error if GeneratorBody Contains SuperCall is true.
        if self.params.contains(ParseNodeKind::YieldExpression) {
            errs.push(create_syntax_error_object(
                "Yield expressions can't be parameter initializers in generators",
                Some(self.params.location()),
            ));
        }
        function_early_errors(errs, strict, self.ident.as_ref(), &self.params, &self.body.0);
    }

    #[expect(unused_variables)]
    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        todo!()
    }
}

// GeneratorExpression :
//      function * BindingIdentifier[+Yield, ~Await]opt ( FormalParameters[+Yield, ~Await] ) { GeneratorBody }
#[derive(Debug)]
pub(crate) struct GeneratorExpression {
    pub(crate) ident: Option<Rc<BindingIdentifier>>,
    pub(crate) params: Rc<FormalParameters>,
    pub(crate) body: Rc<GeneratorBody>,
    location: Location,
}

impl fmt::Display for GeneratorExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function * ")?;
        if let Some(id) = &self.ident {
            write!(f, "{id} ")?;
        }
        write!(f, "( ")?;
        if !matches!(self.params.as_ref(), FormalParameters::Empty(..)) {
            write!(f, "{} ", self.params)?;
        }
        write!(f, ") {{ ")?;
        if !matches!(self.body.0.statements.as_ref(), FunctionStatementList::Empty(..)) {
            write!(f, "{} ", self.body)?;
        }
        write!(f, "}}")
    }
}

impl PrettyPrint for GeneratorExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}GeneratorExpression: {self}")?;
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
        writeln!(writer, "{first}GeneratorExpression: {self}")?;
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

impl GeneratorExpression {
    pub(crate) fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (func_loc, after_func) =
            scan_for_keyword(scanner, parser.source, InputElementGoal::RegExp, Keyword::Function)?;
        let (_, after_star) = scan_for_punct(after_func, parser.source, InputElementGoal::Div, Punctuator::Star)?;
        let (ident, after_bi) = match BindingIdentifier::parse(parser, after_star, true, false) {
            Err(_) => (None, after_star),
            Ok((node, scan)) => (Some(node), scan),
        };
        let (_, after_lp) = scan_for_punct(after_bi, parser.source, InputElementGoal::Div, Punctuator::LeftParen)?;
        let (params, after_fp) = FormalParameters::parse(parser, after_lp, true, false);
        let (_, after_rp) = scan_for_punct(after_fp, parser.source, InputElementGoal::Div, Punctuator::RightParen)?;
        let (_, after_lb) = scan_for_punct(after_rp, parser.source, InputElementGoal::Div, Punctuator::LeftBrace)?;
        let (body, after_body) = GeneratorBody::parse(parser, after_lb);
        let (rb_loc, after_rb) =
            scan_for_punct(after_body, parser.source, InputElementGoal::Div, Punctuator::RightBrace)?;
        let location = func_loc.merge(&rb_loc);
        Ok((Rc::new(GeneratorExpression { ident, params, body, location }), after_rb))
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
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
    /// See [Early Errors for Generator Function Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-generator-function-definitions-static-semantics-early-errors
    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        //  GeneratorExpression :
        //      function * BindingIdentifier[opt] ( FormalParameters ) { GeneratorBody }
        //  * If the source text matched by FormalParameters is strict mode code, the Early Error rules for
        //    UniqueFormalParameters : FormalParameters are applied.
        //  * If BindingIdentifier is present and the source text matched by BindingIdentifier is strict mode code, it
        //    is a Syntax Error if the StringValue of BindingIdentifier is "eval" or "arguments".
        //  * It is a Syntax Error if FunctionBodyContainsUseStrict of GeneratorBody is true and IsSimpleParameterList
        //    of FormalParameters is false.
        //  * It is a Syntax Error if any element of the BoundNames of FormalParameters also occurs in the
        //    LexicallyDeclaredNames of GeneratorBody.
        //  * It is a Syntax Error if FormalParameters Contains YieldExpression is true.
        //  * It is a Syntax Error if FormalParameters Contains SuperProperty is true.
        //  * It is a Syntax Error if GeneratorBody Contains SuperProperty is true.
        //  * It is a Syntax Error if FormalParameters Contains SuperCall is true.
        //  * It is a Syntax Error if GeneratorBody Contains SuperCall is true.
        if self.params.contains(ParseNodeKind::YieldExpression) {
            errs.push(create_syntax_error_object(
                "Yield expressions can't be parameter initializers in generators",
                Some(self.params.location()),
            ));
        }
        function_early_errors(errs, strict, self.ident.as_ref(), &self.params, &self.body.0);
    }

    pub(crate) fn is_named_function(&self) -> bool {
        self.ident.is_some()
    }
    #[expect(unused_variables)]
    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        todo!()
    }
}

// GeneratorBody :
//      FunctionBody[+Yield, ~Await]
#[derive(Debug)]
pub(crate) struct GeneratorBody(pub(crate) Rc<FunctionBody>);

impl fmt::Display for GeneratorBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PrettyPrint for GeneratorBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}GeneratorBody: {self}")?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.0.concise_with_leftpad(writer, pad, state)
    }
}

impl GeneratorBody {
    fn parse_core(parser: &mut Parser, scanner: Scanner) -> (Rc<Self>, Scanner) {
        let (fb, after_fb) = FunctionBody::parse(parser, scanner, true, false, FunctionBodyParent::Generator);
        (Rc::new(GeneratorBody(fb)), after_fb)
    }

    pub(crate) fn parse(parser: &mut Parser, scanner: Scanner) -> (Rc<Self>, Scanner) {
        match parser.generator_body_cache.get(&scanner) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner);
                parser.generator_body_cache.insert(scanner, result.clone());
                result
            }
        }
    }

    pub(crate) fn location(&self) -> Location {
        self.0.location()
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        self.0.contains(kind)
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
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
    /// See [Early Errors for Generator Function Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-generator-function-definitions-static-semantics-early-errors
    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.0.early_errors(errs, strict);
    }

    pub(crate) fn function_body_contains_use_strict(&self) -> bool {
        self.0.function_body_contains_use_strict()
    }

    pub(crate) fn lexically_declared_names(&self) -> Vec<JSString> {
        self.0.lexically_declared_names()
    }

    /// Return a list of identifiers defined by the `var` statement for this node.
    ///
    /// Note that function bodies are treated like top-level code in that top-level function identifiers are part
    /// of the var-declared list.
    ///
    /// See [VarDeclaredNames](https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames) from ECMA-262.
    pub(crate) fn var_declared_names(&self) -> Vec<JSString> {
        self.0.var_declared_names()
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub(crate) fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        self.0.var_scoped_declarations()
    }

    pub(crate) fn lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        self.0.lexically_scoped_declarations()
    }
    #[expect(unused_variables)]
    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        todo!()
    }
}

// YieldExpression[In, Await] :
//      yield
//      yield [no LineTerminator here] AssignmentExpression[?In, +Yield, ?Await]
//      yield [no LineTerminator here] * AssignmentExpression[?In, +Yield, ?Await]
#[derive(Debug)]
pub(crate) enum YieldExpression {
    Simple { location: Location },
    Expression { exp: Rc<AssignmentExpression>, location: Location },
    From { exp: Rc<AssignmentExpression>, location: Location },
}

impl fmt::Display for YieldExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            YieldExpression::Simple { .. } => f.write_str("yield"),
            YieldExpression::Expression { exp, .. } => write!(f, "yield {exp}"),
            YieldExpression::From { exp, .. } => write!(f, "yield * {exp}"),
        }
    }
}

impl PrettyPrint for YieldExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}YieldExpression: {self}")?;
        match self {
            YieldExpression::Simple { .. } => Ok(()),
            YieldExpression::Expression { exp, .. } | YieldExpression::From { exp, .. } => {
                exp.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let head = |writer: &mut T| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{first}YieldExpression: {self}").and(Ok(successive))
        };
        match self {
            YieldExpression::Simple { .. } => pprint_token(writer, "yield", TokenType::Keyword, pad, state),
            YieldExpression::Expression { exp, .. } => {
                let successive = head(writer)?;
                pprint_token(writer, "yield", TokenType::Keyword, &successive, Spot::NotFinal)?;
                exp.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            YieldExpression::From { exp, .. } => {
                let successive = head(writer)?;
                pprint_token(writer, "yield", TokenType::Keyword, &successive, Spot::NotFinal)?;
                pprint_token(writer, "*", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                exp.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl YieldExpression {
    fn parse_after_nlt(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        await_flag: bool,
        yield_loc: Location,
    ) -> ParseResult<Self> {
        (|| {
            let (_, after_star) = scan_for_punct(scanner, parser.source, InputElementGoal::RegExp, Punctuator::Star)?;
            let (exp, after_ae) = AssignmentExpression::parse(parser, after_star, in_flag, true, await_flag)?;
            let location = yield_loc.merge(&exp.location());
            Ok((Rc::new(YieldExpression::From { exp, location }), after_ae))
        })()
        .otherwise(|| {
            let (exp, after_ae) = AssignmentExpression::parse(parser, scanner, in_flag, true, await_flag)?;
            let location = yield_loc.merge(&exp.location());
            Ok((Rc::new(YieldExpression::Expression { exp, location }), after_ae))
        })
    }

    pub(crate) fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (yield_loc, after_yield) =
            scan_for_keyword(scanner, parser.source, InputElementGoal::RegExp, Keyword::Yield)?;
        no_line_terminator(after_yield, parser.source)
            .and_then(|()| Self::parse_after_nlt(parser, after_yield, in_flag, await_flag, yield_loc))
            .otherwise(|| Ok((Rc::new(YieldExpression::Simple { location: yield_loc }), after_yield)))
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            YieldExpression::Simple { location }
            | YieldExpression::Expression { location, .. }
            | YieldExpression::From { location, .. } => *location,
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        kind == ParseNodeKind::YieldExpression
            || match self {
                YieldExpression::Simple { .. } => false,
                YieldExpression::Expression { exp, .. } | YieldExpression::From { exp, .. } => exp.contains(kind),
            }
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        match self {
            YieldExpression::Simple { .. } => true,
            YieldExpression::Expression { exp, .. } | YieldExpression::From { exp, .. } => {
                exp.all_private_identifiers_valid(names)
            }
        }
    }

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub(crate) fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        match self {
            YieldExpression::Simple { .. } => false,
            YieldExpression::Expression { exp, .. } | YieldExpression::From { exp, .. } => exp.contains_arguments(),
        }
    }

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Generator Function Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-generator-function-definitions-static-semantics-early-errors
    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            YieldExpression::Expression { exp, .. } | YieldExpression::From { exp, .. } => {
                exp.early_errors(errs, strict);
            }
            YieldExpression::Simple { .. } => (),
        }
    }

    #[expect(unused_variables)]
    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        todo!()
    }
}

#[cfg(test)]
mod tests;
