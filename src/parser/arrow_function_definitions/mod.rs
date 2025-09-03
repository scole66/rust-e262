use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// ArrowFunction[In, Yield, Await] :
//      ArrowParameters[?Yield, ?Await] [no LineTerminator here] => ConciseBody[?In]
#[derive(Debug)]
pub(crate) struct ArrowFunction {
    pub(crate) parameters: Rc<ArrowParameters>,
    pub(crate) body: Rc<ConciseBody>,
}

impl fmt::Display for ArrowFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} => {}", self.parameters, self.body)
    }
}

impl PrettyPrint for ArrowFunction {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ArrowFunction: {self}")?;
        self.parameters.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.body.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ArrowFunction: {self}")?;
        self.parameters.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "=>", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.body.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl ArrowFunction {
    // ArrowFunction's only parent is AssignmentExpression. It doesn't need to be cached.
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (parameters, after_params) = ArrowParameters::parse(parser, scanner, yield_flag, await_flag)?;
        no_line_terminator(after_params, parser.source)?;
        let (_, after_arrow) = scan_for_punct(after_params, parser.source, InputElementGoal::Div, Punctuator::EqGt)?;
        let (body, after_body) = ConciseBody::parse(parser, after_arrow, in_flag)?;
        Ok((Rc::new(ArrowFunction { parameters, body }), after_body))
    }

    pub(crate) fn location(&self) -> Location {
        self.parameters.location().merge(&self.body.location())
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        (kind == ParseNodeKind::Super
            || kind == ParseNodeKind::This
            || kind == ParseNodeKind::NewTarget
            || kind == ParseNodeKind::SuperProperty
            || kind == ParseNodeKind::SuperCall)
            && (self.parameters.contains(kind) || self.body.contains(kind))
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.parameters.all_private_identifiers_valid(names) && self.body.all_private_identifiers_valid(names)
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
        self.parameters.contains_arguments() || self.body.contains_arguments()
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        //  ArrowFunction : ArrowParameters => ConciseBody
        //  * It is a Syntax Error if ArrowParameters Contains YieldExpression is true.
        //  * It is a Syntax Error if ArrowParameters Contains AwaitExpression is true.
        //  * It is a Syntax Error if ConciseBodyContainsUseStrict of ConciseBody is true and IsSimpleParameterList of
        //    ArrowParameters is false.
        //  * It is a Syntax Error if any element of the BoundNames of ArrowParameters also occurs in the
        //    LexicallyDeclaredNames of ConciseBody.
        if self.parameters.contains(ParseNodeKind::YieldExpression) {
            errs.push(create_syntax_error_object(
                "Illegal yield expression in arrow function parameters",
                Some(self.parameters.location()),
            ));
        }
        if self.parameters.contains(ParseNodeKind::AwaitExpression) {
            errs.push(create_syntax_error_object(
                "Illegal await expression in arrow function parameters",
                Some(self.parameters.location()),
            ));
        }
        if self.body.concise_body_contains_use_strict() && !self.parameters.is_simple_parameter_list() {
            errs.push(create_syntax_error_object(
                "Illegal 'use strict' directive in function with non-simple parameter list",
                Some(self.parameters.location()),
            ));
        }
        let bn = self.parameters.bound_names();
        let ldn = self.body.lexically_declared_names();
        for name in bn.into_iter().filter(|n| ldn.contains(n)) {
            errs.push(create_syntax_error_object(format!("‘{name}’ already defined"), Some(self.body.location())));
        }

        let strict_function = strict || self.body.concise_body_contains_use_strict();
        self.parameters.early_errors(errs, strict_function);
        self.body.early_errors(errs, strict_function);
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            self.parameters.body_containing_location(location).or_else(|| self.body.body_containing_location(location))
        } else {
            None
        }
    }
}

// ArrowParameters[Yield, Await] :
//      BindingIdentifier[?Yield, ?Await]
//      CoverParenthesizedExpressionAndArrowParameterList[?Yield, ?Await]
#[derive(Debug)]
pub(crate) enum ArrowParameters {
    Identifier(Rc<BindingIdentifier>),
    Formals(Rc<ArrowFormalParameters>),
}

impl fmt::Display for ArrowParameters {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArrowParameters::Identifier(node) => node.fmt(f),
            ArrowParameters::Formals(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for ArrowParameters {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ArrowParameters: {self}")?;
        match self {
            ArrowParameters::Identifier(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            ArrowParameters::Formals(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            ArrowParameters::Identifier(node) => node.concise_with_leftpad(writer, pad, state),
            ArrowParameters::Formals(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl ArrowParameters {
    // ArrowParameters's only direct parent is ArrowFunction. It doesn't need to be cached.
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        Err(ParseError::new(PECode::IdOrFormalsExpected, scanner))
            .otherwise(|| {
                BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)
                    .map(|(bi, after_bi)| (Rc::new(ArrowParameters::Identifier(bi)), after_bi))
            })
            .otherwise(|| {
                let (_covered_formals, after_formals) =
                    CoverParenthesizedExpressionAndArrowParameterList::parse(parser, scanner, yield_flag, await_flag)?;
                let (formals, after_reparse) = ArrowFormalParameters::parse(parser, scanner, yield_flag, await_flag)?;

                // This is only a successful cover if the parsed production and its cover end at the same place. But
                // particular cover is all about balanced parenthses. Since both productions require starting and ending
                // with parentheses and they also both require correct nesting of parentheses, it's actually impossible
                // for "after_formals" and "after_reparse" to be different. (Which means I can never get coverage with
                // the "make an error if they're different" case.) So rather than do that, I'll just debug_assert.

                // if after_formals != after_reparse {
                //     Err(ParseError::new("‘)’ expected", after_reparse.line, after_reparse.column))
                // } else {
                //     Ok((Box::new(ArrowParameters::Formals(formals)), after_formals))
                // }
                debug_assert!(after_formals == after_reparse);
                Ok((Rc::new(ArrowParameters::Formals(formals)), after_formals))
            })
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            ArrowParameters::Identifier(id) => id.location(),
            ArrowParameters::Formals(formals) => formals.location(),
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ArrowParameters::Identifier(_) => false,
            ArrowParameters::Formals(node) => node.contains(kind),
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
            ArrowParameters::Identifier(_) => true,
            ArrowParameters::Formals(node) => node.all_private_identifiers_valid(names),
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
            ArrowParameters::Identifier(_) => false,
            ArrowParameters::Formals(afp) => afp.contains_arguments(),
        }
    }

    pub(crate) fn bound_names(&self) -> Vec<JSString> {
        match self {
            ArrowParameters::Identifier(id) => id.bound_names(),
            ArrowParameters::Formals(afp) => afp.bound_names(),
        }
    }

    pub(crate) fn is_simple_parameter_list(&self) -> bool {
        // Static Semantics: IsSimpleParameterList
        // The syntax-directed operation IsSimpleParameterList takes no arguments and returns a Boolean.
        //  ArrowParameters : BindingIdentifier
        //      1. Return true.
        //  ArrowParameters : CoverParenthesizedExpressionAndArrowParameterList
        //      1. Let formals be the ArrowFormalParameters that is covered by CoverParenthesizedExpressionAndArrowParameterList.
        //      2. Return IsSimpleParameterList of formals.
        match self {
            ArrowParameters::Identifier(_) => true,
            ArrowParameters::Formals(formals) => formals.is_simple_parameter_list(),
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ArrowParameters::Identifier(id) => id.early_errors(errs, strict),
            ArrowParameters::Formals(afp) => afp.early_errors(errs, strict),
        }
    }

    pub(crate) fn expected_argument_count(&self) -> f64 {
        match self {
            ArrowParameters::Identifier(_) => 1.0,
            ArrowParameters::Formals(formals) => formals.expected_argument_count(),
        }
    }

    /// Report whether this portion of a parameter list contains an expression
    ///
    /// See [ContainsExpression](https://tc39.es/ecma262/#sec-static-semantics-containsexpression) in ECMA-262.
    pub(crate) fn contains_expression(&self) -> bool {
        match self {
            ArrowParameters::Identifier(_) => false,
            ArrowParameters::Formals(f) => f.contains_expression(),
        }
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                ArrowParameters::Identifier(_) => None,
                ArrowParameters::Formals(arrow_formal_parameters) => {
                    arrow_formal_parameters.body_containing_location(location)
                }
            }
        } else {
            None
        }
    }
}

// ArrowFormalParameters[Yield, Await] :
//      ( UniqueFormalParameters[?Yield, ?Await] )
#[derive(Debug)]
pub(crate) struct ArrowFormalParameters {
    pub(crate) params: Rc<UniqueFormalParameters>,
    location: Location,
}

impl fmt::Display for ArrowFormalParameters {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "( ")?;
        if !matches!(self.params.formals.as_ref(), FormalParameters::Empty(..)) {
            write!(f, "{} ", self.params)?;
        }
        write!(f, ")")
    }
}

impl PrettyPrint for ArrowFormalParameters {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ArrowFormalParameters: {self}")?;
        self.params.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ArrowFormalParameters: {self}")?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.params.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ArrowFormalParameters {
    // I _think_ this needs to be cached.
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (lp_loc, after_lp) = scan_for_punct(scanner, parser.source, InputElementGoal::Div, Punctuator::LeftParen)?;
        let (params, after_params) = UniqueFormalParameters::parse(parser, after_lp, yield_flag, await_flag);
        let (rp_loc, after_rp) =
            scan_for_punct(after_params, parser.source, InputElementGoal::Div, Punctuator::RightParen)?;
        let location = lp_loc.merge(&rp_loc);
        Ok((Rc::new(ArrowFormalParameters { params, location }), after_rp))
    }

    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.arrow_formal_parameters_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.arrow_formal_parameters_cache.insert(key, result.clone());
                result
            }
        }
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

    pub(crate) fn bound_names(&self) -> Vec<JSString> {
        self.params.bound_names()
    }

    pub(crate) fn is_simple_parameter_list(&self) -> bool {
        // Static Semantics: IsSimpleParameterList
        // The syntax-directed operation IsSimpleParameterList takes no arguments and returns a Boolean.
        //  ArrowFormalParameters : ( UniqueFormalParameters )
        //      1. Return IsSimpleParameterList of UniqueFormalParameters.
        self.params.is_simple_parameter_list()
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.params.early_errors(errs, strict);
    }

    pub(crate) fn expected_argument_count(&self) -> f64 {
        self.params.expected_argument_count()
    }

    /// Report whether this portion of a parameter list contains an expression
    ///
    /// See [ContainsExpression](https://tc39.es/ecma262/#sec-static-semantics-containsexpression) in ECMA-262.
    pub(crate) fn contains_expression(&self) -> bool {
        self.params.contains_expression()
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) { self.params.body_containing_location(location) } else { None }
    }
}

// ConciseBody[In] :
//      [lookahead ≠ {] ExpressionBody[?In, ~Await]
//      { FunctionBody[~Yield, ~Await] }
#[derive(Debug)]
pub(crate) enum ConciseBody {
    Expression(Rc<ExpressionBody>),
    Function { body: Rc<FunctionBody>, location: Location },
}

impl fmt::Display for ConciseBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConciseBody::Expression(node) => node.fmt(f),
            ConciseBody::Function { body, .. } => write!(f, "{{ {body} }}"),
        }
    }
}

impl PrettyPrint for ConciseBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ConciseBody: {self}")?;
        match self {
            ConciseBody::Expression(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            ConciseBody::Function { body, .. } => body.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            ConciseBody::Expression(node) => node.concise_with_leftpad(writer, pad, state),
            ConciseBody::Function { body, .. } => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{first}ConciseBody: {self}")?;
                pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
            }
        }
    }
}

impl ConciseBody {
    // ConciseBody's only direct parent is ArrowFunction. It doesn't need to be cached.
    pub(crate) fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::ConciseBody), scanner))
            .otherwise(|| {
                let (lb_loc, after_curly) =
                    scan_for_punct(scanner, parser.source, InputElementGoal::RegExp, Punctuator::LeftBrace)?;
                let (body, after_fb) =
                    FunctionBody::parse(parser, after_curly, in_flag, false, FunctionBodyParent::Function);
                let (rb_loc, after_rb) =
                    scan_for_punct(after_fb, parser.source, InputElementGoal::Div, Punctuator::RightBrace)?;
                let location = lb_loc.merge(&rb_loc);
                let concise_body = Rc::new(ConciseBody::Function { body, location });
                Ok((concise_body, after_rb))
            })
            .otherwise(|| {
                let r = scan_for_punct(scanner, parser.source, InputElementGoal::RegExp, Punctuator::LeftBrace);
                match r {
                    Err(_) => {
                        let (exp, after_exp) = ExpressionBody::parse(parser, scanner, in_flag, false)?;
                        let concise_body = Rc::new(ConciseBody::Expression(exp));
                        Ok((concise_body, after_exp))
                    }
                    Ok(_) => Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::ExpressionBody), scanner)),
                }
            })
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            ConciseBody::Expression(exp) => exp.location(),
            ConciseBody::Function { location, .. } => *location,
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ConciseBody::Expression(node) => node.contains(kind),
            ConciseBody::Function { body, .. } => body.contains(kind),
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
            ConciseBody::Expression(node) => node.all_private_identifiers_valid(names),
            ConciseBody::Function { body, .. } => body.all_private_identifiers_valid(names),
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
            ConciseBody::Expression(eb) => eb.contains_arguments(),
            ConciseBody::Function { body, .. } => body.contains_arguments(),
        }
    }

    pub(crate) fn lexically_declared_names(&self) -> Vec<JSString> {
        // Static Semantics: LexicallyDeclaredNames
        // The syntax-directed operation LexicallyDeclaredNames takes no arguments and returns a List of Strings.
        //  ConciseBody : ExpressionBody
        //      1. Return a new empty List.
        //  ConciseBody : { FunctionBody }
        //      1. Return LexicallyDeclaredNames of FunctionBody.
        match self {
            ConciseBody::Expression(_) => vec![],
            ConciseBody::Function { body, .. } => body.lexically_declared_names(),
        }
    }

    pub(crate) fn concise_body_contains_use_strict(&self) -> bool {
        // Static Semantics: ConciseBodyContainsUseStrict
        // The syntax-directed operation ConciseBodyContainsUseStrict takes no arguments and returns a Boolean. It is
        // defined piecewise over the following productions:
        //
        //  ConciseBody : ExpressionBody
        //      1. Return false.
        //  ConciseBody : { FunctionBody }
        //      1. Return FunctionBodyContainsUseStrict of FunctionBody.
        match self {
            ConciseBody::Expression(_) => false,
            ConciseBody::Function { body, .. } => body.function_body_contains_use_strict(),
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ConciseBody::Expression(exp) => exp.early_errors(errs, strict),
            ConciseBody::Function { body, .. } => body.early_errors(errs, strict),
        }
    }

    /// Return a list of identifiers defined by the `var` statement for this node.
    ///
    /// Note that function bodies are treated like top-level code in that top-level function identifiers are part
    /// of the var-declared list.
    ///
    /// See [VarDeclaredNames](https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames) from ECMA-262.
    pub(crate) fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            ConciseBody::Expression(_) => vec![],
            ConciseBody::Function { body, .. } => body.var_declared_names(),
        }
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub(crate) fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        match self {
            ConciseBody::Expression(_) => vec![],
            ConciseBody::Function { body, .. } => body.var_scoped_declarations(),
        }
    }

    pub(crate) fn lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        match self {
            ConciseBody::Expression(_) => vec![],
            ConciseBody::Function { body, .. } => body.lexically_scoped_declarations(),
        }
    }

    pub(crate) fn has_call_in_tail_position(&self, location: &Location) -> bool {
        // Static Semantics: HasCallInTailPosition
        // The syntax-directed operation HasCallInTailPosition takes argument call (a CallExpression Parse Node, a
        // MemberExpression Parse Node, or an OptionalChain Parse Node) and returns a Boolean.
        //
        // Note 1: call is a Parse Node that represents a specific range of source text. When the following algorithms
        //         compare call to another Parse Node, it is a test of whether they represent the same source text.
        //
        // Note 2: A potential tail position call that is immediately followed by return GetValue of the call result is
        //         also a possible tail position call. A function call cannot return a Reference Record, so such a
        //         GetValue operation will always return the same value as the actual function call result.
        match self {
            ConciseBody::Expression(expression_body) => expression_body.has_call_in_tail_position(location),
            ConciseBody::Function { body, .. } => body.has_call_in_tail_position(location),
        }
    }

    pub(crate) fn body_containing_location(self: &Rc<Self>, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self.as_ref() {
                ConciseBody::Expression(expression_body) => expression_body
                    .body_containing_location(location)
                    .or_else(|| Some(ContainingBody::ConciseBody(Rc::clone(self)))),
                ConciseBody::Function { body, .. } => body
                    .body_containing_location(location)
                    .or_else(|| Some(ContainingBody::ConciseBody(Rc::clone(self)))),
            }
        } else {
            None
        }
    }
}

// ExpressionBody[In, Await] :
//      AssignmentExpression[?In, ~Yield, ?Await]
#[derive(Debug)]
pub(crate) struct ExpressionBody {
    pub(crate) expression: Rc<AssignmentExpression>,
}

impl fmt::Display for ExpressionBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.expression.fmt(f)
    }
}

impl PrettyPrint for ExpressionBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ExpressionBody: {self}")?;
        self.expression.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.expression.concise_with_leftpad(writer, pad, state)
    }
}

impl ExpressionBody {
    // ExpressionBody has multiple potential parents. It should be cached.
    fn parse_core(parser: &mut Parser, scanner: Scanner, in_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (ae, after_ae) = AssignmentExpression::parse(parser, scanner, in_flag, false, await_flag)?;
        Ok((Rc::new(ExpressionBody { expression: ae }), after_ae))
    }

    pub(crate) fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = InAwaitKey { scanner, in_flag, await_flag };
        match parser.expression_body_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag, await_flag);
                parser.expression_body_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub(crate) fn location(&self) -> Location {
        self.expression.location()
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        self.expression.contains(kind)
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.expression.all_private_identifiers_valid(names)
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
        self.expression.contains_arguments()
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.expression.early_errors(errs, strict);
    }

    pub(crate) fn has_call_in_tail_position(&self, location: &Location) -> bool {
        // Static Semantics: HasCallInTailPosition
        // The syntax-directed operation HasCallInTailPosition takes argument call (a CallExpression Parse Node, a
        // MemberExpression Parse Node, or an OptionalChain Parse Node) and returns a Boolean.
        //
        // Note 1: call is a Parse Node that represents a specific range of source text. When the following algorithms
        //         compare call to another Parse Node, it is a test of whether they represent the same source text.
        //
        // Note 2: A potential tail position call that is immediately followed by return GetValue of the call result is
        //         also a possible tail position call. A function call cannot return a Reference Record, so such a
        //         GetValue operation will always return the same value as the actual function call result.
        self.expression.has_call_in_tail_position(location)
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) { self.expression.body_containing_location(location) } else { None }
    }
}

#[cfg(test)]
mod tests;
