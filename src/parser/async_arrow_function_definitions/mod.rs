use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// AsyncArrowFunction[In, Yield, Await] :
//      async [no LineTerminator here] AsyncArrowBindingIdentifier[?Yield] [no LineTerminator here] => AsyncConciseBody[?In]
//      CoverCallExpressionAndAsyncArrowHead[?Yield, ?Await] [no LineTerminator here] => AsyncConciseBody[?In]
#[derive(Debug)]
pub(crate) enum AsyncArrowFunction {
    IdentOnly(Rc<AsyncArrowBindingIdentifier>, Rc<AsyncConciseBody>, Location),
    Formals(Rc<AsyncArrowHead>, Rc<AsyncConciseBody>),
}

impl fmt::Display for AsyncArrowFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AsyncArrowFunction::IdentOnly(id, body, ..) => {
                write!(f, "async {id} => {body}")
            }
            AsyncArrowFunction::Formals(params, body) => {
                write!(f, "{params} => {body}")
            }
        }
    }
}

impl PrettyPrint for AsyncArrowFunction {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AsyncArrowFunction: {self}")?;
        match self {
            AsyncArrowFunction::IdentOnly(id, body, ..) => {
                id.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                body.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            AsyncArrowFunction::Formals(params, body) => {
                params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                body.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AsyncArrowFunction: {self}")?;
        match self {
            AsyncArrowFunction::IdentOnly(head, tail, ..) => {
                pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
                head.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "=>", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                tail.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AsyncArrowFunction::Formals(head, tail) => {
                head.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "=>", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                tail.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl AsyncArrowFunction {
    // AsyncArrowFunction's (only) parent is AssignmentExpression. It doesn't need caching.
    fn parse_normal_form(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool) -> ParseResult<Self> {
        let (async_loc, after_async) =
            scan_for_keyword(scanner, parser.source, InputElementGoal::RegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let (id, after_id) = AsyncArrowBindingIdentifier::parse(parser, after_async, yield_flag)?;
        no_line_terminator(after_id, parser.source)?;
        let (_, after_arrow) = scan_for_punct(after_id, parser.source, InputElementGoal::Div, Punctuator::EqGt)?;
        let (body, after_body) = AsyncConciseBody::parse(parser, after_arrow, in_flag)?;
        let location = async_loc.merge(&body.location());
        Ok((Rc::new(AsyncArrowFunction::IdentOnly(id, body, location)), after_body))
    }
    fn parse_covered_form(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<(Rc<AsyncArrowHead>, Scanner, Rc<AsyncConciseBody>, Scanner), ParseError> {
        let (_cceaaah, after_params) =
            CoverCallExpressionAndAsyncArrowHead::parse(parser, scanner, yield_flag, await_flag)?;
        let (real_params, after_reals) = AsyncArrowHead::parse(parser, scanner)?;
        assert!(after_params == after_reals);
        no_line_terminator(after_params, parser.source)?;
        let (_, after_arrow) = scan_for_punct(after_params, parser.source, InputElementGoal::Div, Punctuator::EqGt)?;
        let (body, after_body) = AsyncConciseBody::parse(parser, after_arrow, in_flag)?;
        Ok((real_params, after_params, body, after_body))
    }

    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let pot_norm = Self::parse_normal_form(parser, scanner, in_flag, yield_flag);
        let pot_covered = Self::parse_covered_form(parser, scanner, in_flag, yield_flag, await_flag);
        match (pot_norm, pot_covered) {
            (Err(err1), Err(err2)) => Err(cmp::max_by(err2, err1, ParseError::compare)),
            (Err(_), Ok((real_params, _, body, after_covered))) => {
                Ok((Rc::new(AsyncArrowFunction::Formals(real_params, body)), after_covered))
            }
            // (Ok(norm), Ok(covered)) can never happen, given the particulars of the productions
            (norm, covered) => {
                assert!(covered.is_err() && norm.is_ok());
                norm
            }
        }
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            AsyncArrowFunction::IdentOnly(_, _, location) => *location,
            AsyncArrowFunction::Formals(head, body) => head.location().merge(&body.location()),
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        (kind == ParseNodeKind::NewTarget
            || kind == ParseNodeKind::SuperProperty
            || kind == ParseNodeKind::SuperCall
            || kind == ParseNodeKind::Super
            || kind == ParseNodeKind::This)
            && match self {
                AsyncArrowFunction::IdentOnly(_, body, ..) => body.contains(kind),
                AsyncArrowFunction::Formals(params, body) => params.contains(kind) || body.contains(kind),
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
            AsyncArrowFunction::IdentOnly(_, node, ..) => node.all_private_identifiers_valid(names),
            AsyncArrowFunction::Formals(node1, node2) => {
                node1.all_private_identifiers_valid(names) && node2.all_private_identifiers_valid(names)
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
            AsyncArrowFunction::IdentOnly(_, acb, ..) => acb.contains_arguments(),
            AsyncArrowFunction::Formals(aah, acb) => aah.contains_arguments() || acb.contains_arguments(),
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // AsyncArrowFunction : async AsyncArrowBindingIdentifier => AsyncConciseBody
        //  * It is a Syntax Error if any element of the BoundNames of AsyncArrowBindingIdentifier also occurs in the
        //    LexicallyDeclaredNames of AsyncConciseBody.
        // AsyncArrowFunction : CoverCallExpressionAndAsyncArrowHead => AsyncConciseBody
        //  * CoverCallExpressionAndAsyncArrowHead must cover an AsyncArrowHead.
        //  * It is a Syntax Error if CoverCallExpressionAndAsyncArrowHead Contains YieldExpression is true.
        //  * It is a Syntax Error if CoverCallExpressionAndAsyncArrowHead Contains AwaitExpression is true.
        //  * It is a Syntax Error if any element of the BoundNames of CoverCallExpressionAndAsyncArrowHead also occurs
        //    in the LexicallyDeclaredNames of AsyncConciseBody.
        //  * It is a Syntax Error if AsyncConciseBodyContainsUseStrict of AsyncConciseBody is true and
        //    IsSimpleParameterList of CoverCallExpressionAndAsyncArrowHead is false.
        match self {
            AsyncArrowFunction::IdentOnly(async_arrow_binding_identifier, async_concise_body, ..) => {
                let ids = async_arrow_binding_identifier.bound_names();
                let declared_names = async_concise_body.lexically_declared_names();
                let mut duplicated_names = vec![];
                for name in ids {
                    if declared_names.contains(&name) {
                        duplicated_names.push(name);
                    }
                }
                for dup in duplicated_names {
                    errs.push(create_syntax_error_object(
                        format!("Identifier '{dup}' has already been declared"),
                        Some(self.location()),
                    ));
                }
                async_arrow_binding_identifier.early_errors(errs, strict);
                async_concise_body.early_errors(errs, strict);
            }
            AsyncArrowFunction::Formals(async_arrow_head, async_concise_body) => {
                if async_arrow_head.contains(ParseNodeKind::YieldExpression) {
                    errs.push(create_syntax_error_object(
                        "Yield expression not allowed in formal parameter",
                        Some(self.location()),
                    ));
                }
                if async_arrow_head.contains(ParseNodeKind::AwaitExpression) {
                    errs.push(create_syntax_error_object(
                        "Await expression cannot be a default value",
                        Some(self.location()),
                    ));
                }
                let ids = async_arrow_head.bound_names();
                let declared_names = async_concise_body.lexically_declared_names();
                let mut duplicated_names = vec![];
                for name in ids {
                    if declared_names.contains(&name) {
                        duplicated_names.push(name);
                    }
                }
                for dup in duplicated_names {
                    errs.push(create_syntax_error_object(
                        format!("Identifier '{dup}' has already been declared"),
                        Some(self.location()),
                    ));
                }
                if async_concise_body.contains_use_strict() && !async_arrow_head.is_simple_parameter_list() {
                    errs.push(create_syntax_error_object(
                        "Illegal 'use strict' directive in function with non-simple parameter list",
                        Some(self.location()),
                    ));
                }
                async_arrow_head.early_errors(errs, strict);
                async_concise_body.early_errors(errs, strict);
            }
        }
    }

    #[expect(unused_variables)]
    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        todo!()
    }
}

// AsyncArrowHead :
//      async [no LineTerminator here] ArrowFormalParameters[~Yield, +Await]
#[derive(Debug)]
pub(crate) struct AsyncArrowHead {
    pub(crate) params: Rc<ArrowFormalParameters>,
    location: Location,
}

impl fmt::Display for AsyncArrowHead {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "async {}", self.params)
    }
}

impl PrettyPrint for AsyncArrowHead {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AsyncArrowHead: {self}")?;
        self.params.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AsyncArrowHead: {self}")?;
        pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
        self.params.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl AsyncArrowHead {
    // No caching needed. Parent: AsyncArrowFunction
    pub(crate) fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (async_loc, after_async) =
            scan_for_keyword(scanner, parser.source, InputElementGoal::RegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let (params, after_params) = ArrowFormalParameters::parse(parser, after_async, false, true)?;
        let location = async_loc.merge(&params.location());
        Ok((Rc::new(AsyncArrowHead { params, location }), after_params))
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        self.params.contains(kind)
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.params.all_private_identifiers_valid(names)
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
        self.params.contains_arguments()
    }

    /// Returns a list of bound names for a given syntactic construct, as defined by the ECMA-262 specification.
    ///
    /// In the context of JavaScript parsing and interpretation, "bound names" are the identifiers
    /// introduced by a declaration. This function inspects the provided AST node and returns
    /// a list of names that are bound by it.
    ///
    /// This corresponds to the `BoundNames` abstract operation in the ECMA-262 specification,
    /// which is used to determine the lexical bindings introduced by various syntactic forms such
    /// as `VariableDeclaration`, `FunctionDeclaration`, `ClassDeclaration`, and destructuring patterns.
    ///
    /// # Returns
    ///
    /// A `Vec<JSString>` containing the names of all identifiers that are lexically bound by the node.
    ///
    /// # Examples
    ///
    /// ```rust
    /// // Assuming `node` is a AsyncArrowHead with `async (x, y)`
    /// let node = Maker::new("async (x, y)").async_arrow_head();
    /// let bound_names = node.bound_names();
    /// assert_eq!(bound_names, vec!["x", "y"]);
    /// ```
    ///
    /// # Notes
    ///
    /// - This function does not evaluate the code or check runtime semantics; it only inspects the syntax.
    ///
    /// # Specification Reference
    ///
    /// - [ECMA-262, §8.2.1](https://tc39.es/ecma262/multipage/syntax-directed-operations.html#sec-static-semantics-boundnames)
    pub(crate) fn bound_names(&self) -> Vec<JSString> {
        self.params.bound_names()
    }

    /// Determines whether the parameter list of an `AsyncArrowHead` is a *simple* parameter list,
    /// as defined by the ECMA-262 specification.
    ///
    /// In the context of JavaScript async arrow functions, a simple parameter list consists solely
    /// of identifiers — without any default values, rest parameters, or destructuring patterns.
    ///
    /// This function implements the `IsSimpleParameterList` abstract operation for the `AsyncArrowHead`
    /// grammar production. It delegates to the `IsSimpleParameterList` of the underlying
    /// `ArrowFormalParameters`.
    ///
    /// # Returns
    ///
    /// A `bool` indicating whether the parameter list is simple (`true`) or not (`false`).
    ///
    /// # Examples
    ///
    /// ```rust
    /// // For: async (x, y) => x + y   → simple
    /// let node = Maker::new("async (x, y)").async_arrow_head();
    /// assert!(node.is_simple_parameter_list_for_async_arrow_head());
    ///
    /// // For: async ({ x }) => x     → not simple (destructuring)
    /// let node = Maker::new("async ({ x })").async_arrow_head();
    /// assert!(!node.is_simple_parameter_list_for_async_arrow_head());
    /// ```
    ///
    /// # Specification Reference
    ///
    /// - [ECMA-262 §15.9 (Async Arrow Function Definitions)](https://tc39.es/ecma262/multipage/ecmascript-language-functions-and-classes.html#prod-AsyncArrowHead)
    /// - [ECMA-262 §15.1.3 (IsSimpleParameterList)](https://tc39.es/ecma262/multipage/ecmascript-language-functions-and-classes.html#sec-static-semantics-issimpleparameterlist)
    ///
    /// # Notes
    ///
    /// - A simple parameter list must contain only identifier bindings, with no rest elements,
    ///   default initializers, or patterns.
    /// - This function performs only syntactic checks; no runtime behavior is involved.
    pub(crate) fn is_simple_parameter_list(&self) -> bool {
        self.params.is_simple_parameter_list()
    }

    /// Performs early error checks for this `AsyncArrowHead`, as defined in the ECMAScript specification.
    ///
    /// This method delegates to the `early_errors` of the contained `ArrowFormalParameters`, as
    /// `AsyncArrowHead` introduces no additional early errors of its own.
    ///
    /// # Specification Reference
    ///
    /// - [ECMA-262 §15.9 (Async Arrow Function Definitions)](https://tc39.es/ecma262/multipage/ecmascript-language-functions-and-classes.html#prod-AsyncArrowHead)
    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.params.early_errors(errs, strict);
    }

    //#[expect(unused_variables)]
    //pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
    //    // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
    //    todo!()
    //}
}

// AsyncConciseBody[In] :
//      [lookahead ≠ {] ExpressionBody[?In, +Await]
//      { AsyncFunctionBody }
#[derive(Debug)]
pub(crate) enum AsyncConciseBody {
    Expression(Rc<ExpressionBody>),
    Function(Rc<AsyncFunctionBody>, Location),
}

impl fmt::Display for AsyncConciseBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AsyncConciseBody::Expression(node) => node.fmt(f),
            AsyncConciseBody::Function(node, ..) => write!(f, "{{ {node} }}"),
        }
    }
}

impl PrettyPrint for AsyncConciseBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AsyncConciseBody: {self}")?;
        match self {
            AsyncConciseBody::Expression(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            AsyncConciseBody::Function(node, ..) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            AsyncConciseBody::Expression(node) => node.concise_with_leftpad(writer, pad, state),
            AsyncConciseBody::Function(node, ..) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{first}AsyncConciseBody: {self}")?;
                pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
            }
        }
    }
}

impl AsyncConciseBody {
    // No caching required. Only parent is AsyncArrowFunction
    pub(crate) fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::AsyncConciseBody), scanner))
            .otherwise(|| {
                let (curly_loc, after_curly) =
                    scan_for_punct(scanner, parser.source, InputElementGoal::RegExp, Punctuator::LeftBrace)?;
                let (fb, after_fb) = AsyncFunctionBody::parse(parser, after_curly);
                let (rb_loc, after_rb) =
                    scan_for_punct(after_fb, parser.source, InputElementGoal::Div, Punctuator::RightBrace)?;
                let location = curly_loc.merge(&rb_loc);
                Ok((Rc::new(AsyncConciseBody::Function(fb, location)), after_rb))
            })
            .otherwise(|| {
                let r = scan_for_punct(scanner, parser.source, InputElementGoal::RegExp, Punctuator::LeftBrace);
                match r {
                    Err(_) => {
                        let (exp, after_exp) = ExpressionBody::parse(parser, scanner, in_flag, true)?;
                        Ok((Rc::new(AsyncConciseBody::Expression(exp)), after_exp))
                    }
                    Ok(_) => Err(ParseError::new(PECode::Generic, scanner)),
                }
            })
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            AsyncConciseBody::Expression(exp) => exp.location(),
            AsyncConciseBody::Function(_, location) => *location,
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            AsyncConciseBody::Expression(node) => node.contains(kind),
            AsyncConciseBody::Function(node, ..) => node.contains(kind),
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
            AsyncConciseBody::Expression(node) => node.all_private_identifiers_valid(names),
            AsyncConciseBody::Function(node, ..) => node.all_private_identifiers_valid(names),
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
            AsyncConciseBody::Expression(eb) => eb.contains_arguments(),
            AsyncConciseBody::Function(afb, ..) => afb.contains_arguments(),
        }
    }

    /// Performs early error checks for this `AsyncConciseBody`, as defined in the ECMAScript specification.
    ///
    /// This method delegates to the early error checks of the contained `ExpressionBody` or `AsyncFunctionBody`,
    /// depending on the form of the concise body. `AsyncConciseBody` itself introduces no additional early errors.
    ///
    /// # Specification Reference
    ///
    /// - [ECMA-262 §15.9 (AsyncConciseBody)](https://tc39.es/ecma262/multipage/ecmascript-language-functions-and-classes.html#prod-AsyncConciseBody)
    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            AsyncConciseBody::Expression(expression_body) => expression_body.early_errors(errs, strict),
            AsyncConciseBody::Function(async_function_body, _) => async_function_body.early_errors(errs, strict),
        }
    }

    pub(crate) fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            AsyncConciseBody::Expression(_) => vec![],
            AsyncConciseBody::Function(node, _) => node.var_declared_names(),
        }
    }

    pub(crate) fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        match self {
            AsyncConciseBody::Expression(_) => vec![],
            AsyncConciseBody::Function(node, _) => node.var_scoped_declarations(),
        }
    }

    pub(crate) fn lexically_declared_names(&self) -> Vec<JSString> {
        match self {
            AsyncConciseBody::Expression(_) => vec![],
            AsyncConciseBody::Function(node, _) => node.lexically_declared_names(),
        }
    }

    pub(crate) fn lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        match self {
            AsyncConciseBody::Expression(_) => vec![],
            AsyncConciseBody::Function(node, _) => node.lexically_scoped_declarations(),
        }
    }

    pub(crate) fn contains_use_strict(&self) -> bool {
        match self {
            AsyncConciseBody::Expression(_) => false,
            AsyncConciseBody::Function(node, _) => node.function_body_contains_use_strict(),
        }
    }

    //#[expect(unused_variables)]
    //pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
    //    // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
    //    todo!()
    //}
}

// AsyncArrowBindingIdentifier[Yield] :
//      BindingIdentifier[?Yield, +Await]
#[derive(Debug)]
pub(crate) struct AsyncArrowBindingIdentifier(Rc<BindingIdentifier>);

impl fmt::Display for AsyncArrowBindingIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PrettyPrint for AsyncArrowBindingIdentifier {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AsyncArrowBindingIdentifier: {self}")?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.0.concise_with_leftpad(writer, pad, state)
    }
}

impl AsyncArrowBindingIdentifier {
    // No caching needed. Only parent: AsyncArrowFunction
    pub(crate) fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool) -> ParseResult<Self> {
        let (ident, after_ident) = BindingIdentifier::parse(parser, scanner, yield_flag, true)?;
        Ok((Rc::new(AsyncArrowBindingIdentifier(ident)), after_ident))
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.0.early_errors(errs, strict);
    }

    pub(crate) fn bound_names(&self) -> Vec<JSString> {
        self.0.bound_names()
    }

    //#[expect(unused_variables)]
    //pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
    //    // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
    //    todo!()
    //}
}

// CoverCallExpressionAndAsyncArrowHead[Yield, Await] :
//      MemberExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
#[derive(Debug)]
pub(crate) struct CoverCallExpressionAndAsyncArrowHead {
    expression: Rc<MemberExpression>,
    args: Rc<Arguments>,
}

impl fmt::Display for CoverCallExpressionAndAsyncArrowHead {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.expression, self.args)
    }
}

impl PrettyPrint for CoverCallExpressionAndAsyncArrowHead {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}CoverCallExpressionAndAsyncArrowHead: {self}")?;
        self.expression.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.args.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}CoverCallExpressionAndAsyncArrowHead: {self}")?;
        self.expression.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.args.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl CoverCallExpressionAndAsyncArrowHead {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (expression, after_exp) = MemberExpression::parse(parser, scanner, yield_flag, await_flag)?;
        let (args, after_args) = Arguments::parse(parser, after_exp, yield_flag, await_flag)?;
        Ok((Rc::new(CoverCallExpressionAndAsyncArrowHead { expression, args }), after_args))
    }

    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.cover_call_expression_and_async_arrow_head_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.cover_call_expression_and_async_arrow_head_cache.insert(key, result.clone());
                result
            }
        }
    }
}

#[cfg(test)]
mod tests;
