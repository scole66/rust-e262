use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// AsyncArrowFunction[In, Yield, Await] :
//      async [no LineTerminator here] AsyncArrowBindingIdentifier[?Yield] [no LineTerminator here] => AsyncConciseBody[?In]
//      CoverCallExpressionAndAsyncArrowHead[?Yield, ?Await] [no LineTerminator here] => AsyncConciseBody[?In]
#[derive(Debug)]
pub enum AsyncArrowFunction {
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
            scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let (id, after_id) = AsyncArrowBindingIdentifier::parse(parser, after_async, yield_flag)?;
        no_line_terminator(after_id, parser.source)?;
        let (_, after_arrow) = scan_for_punct(after_id, parser.source, ScanGoal::InputElementDiv, Punctuator::EqGt)?;
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
        let (_, after_arrow) =
            scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::EqGt)?;
        let (body, after_body) = AsyncConciseBody::parse(parser, after_arrow, in_flag)?;
        Ok((real_params, after_params, body, after_body))
    }

    pub fn parse(
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

    pub fn location(&self) -> Location {
        match self {
            AsyncArrowFunction::IdentOnly(_, _, location) => *location,
            AsyncArrowFunction::Formals(head, body) => head.location().merge(&body.location()),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
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

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
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
    pub fn contains_arguments(&self) -> bool {
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

    #[expect(clippy::ptr_arg)]
    pub fn early_errors(&self, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// AsyncArrowHead :
//      async [no LineTerminator here] ArrowFormalParameters[~Yield, +Await]
#[derive(Debug)]
pub struct AsyncArrowHead {
    pub params: Rc<ArrowFormalParameters>,
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
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (async_loc, after_async) =
            scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let (params, after_params) = ArrowFormalParameters::parse(parser, after_async, false, true)?;
        let location = async_loc.merge(&params.location());
        Ok((Rc::new(AsyncArrowHead { params, location }), after_params))
    }

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.params.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
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
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.params.contains_arguments()
    }

    #[expect(clippy::ptr_arg)]
    pub fn early_errors(&self, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// AsyncConciseBody[In] :
//      [lookahead ≠ {] ExpressionBody[?In, +Await]
//      { AsyncFunctionBody }
#[derive(Debug)]
pub enum AsyncConciseBody {
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::AsyncConciseBody), scanner))
            .otherwise(|| {
                let (curly_loc, after_curly) =
                    scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBrace)?;
                let (fb, after_fb) = AsyncFunctionBody::parse(parser, after_curly);
                let (rb_loc, after_rb) =
                    scan_for_punct(after_fb, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                let location = curly_loc.merge(&rb_loc);
                Ok((Rc::new(AsyncConciseBody::Function(fb, location)), after_rb))
            })
            .otherwise(|| {
                let r = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBrace);
                match r {
                    Err(_) => {
                        let (exp, after_exp) = ExpressionBody::parse(parser, scanner, in_flag, true)?;
                        Ok((Rc::new(AsyncConciseBody::Expression(exp)), after_exp))
                    }
                    Ok(_) => Err(ParseError::new(PECode::Generic, scanner)),
                }
            })
    }

    pub fn location(&self) -> Location {
        match self {
            AsyncConciseBody::Expression(exp) => exp.location(),
            AsyncConciseBody::Function(_, location) => *location,
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            AsyncConciseBody::Expression(node) => node.contains(kind),
            AsyncConciseBody::Function(node, ..) => node.contains(kind),
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
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
    pub fn contains_arguments(&self) -> bool {
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

    #[expect(clippy::ptr_arg)]
    pub fn early_errors(&self, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            AsyncConciseBody::Expression(_) => vec![],
            AsyncConciseBody::Function(node, _) => node.var_declared_names(),
        }
    }

    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        match self {
            AsyncConciseBody::Expression(_) => vec![],
            AsyncConciseBody::Function(node, _) => node.var_scoped_declarations(),
        }
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        match self {
            AsyncConciseBody::Expression(_) => vec![],
            AsyncConciseBody::Function(node, _) => node.lexically_declared_names(),
        }
    }

    pub fn lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        match self {
            AsyncConciseBody::Expression(_) => vec![],
            AsyncConciseBody::Function(node, _) => node.lexically_scoped_declarations(),
        }
    }

    pub fn contains_use_strict(&self) -> bool {
        match self {
            AsyncConciseBody::Expression(_) => false,
            AsyncConciseBody::Function(node, _) => node.function_body_contains_use_strict(),
        }
    }
}

// AsyncArrowBindingIdentifier[Yield] :
//      BindingIdentifier[?Yield, +Await]
#[derive(Debug)]
pub struct AsyncArrowBindingIdentifier(Rc<BindingIdentifier>);

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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool) -> ParseResult<Self> {
        let (ident, after_ident) = BindingIdentifier::parse(parser, scanner, yield_flag, true)?;
        Ok((Rc::new(AsyncArrowBindingIdentifier(ident)), after_ident))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.0.contains(kind)
    }

    #[expect(clippy::ptr_arg)]
    pub fn early_errors(&self, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }

    pub fn expected_argument_count(&self) -> f64 {
        1.0
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        self.0.bound_names()
    }

    pub fn is_simple_parameter_list(&self) -> bool {
        true
    }

    pub fn contains_expression(&self) -> bool {
        false
    }
}

// CoverCallExpressionAndAsyncArrowHead[Yield, Await] :
//      MemberExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
#[derive(Debug)]
pub struct CoverCallExpressionAndAsyncArrowHead {
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

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
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

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.expression.contains(kind) || self.args.contains(kind)
    }

    #[expect(clippy::ptr_arg)]
    pub fn early_errors(&self, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

#[cfg(test)]
mod tests;
