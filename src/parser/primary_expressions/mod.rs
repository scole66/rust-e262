use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

//////// 12.2 Primary Expression
// PrimaryExpression[Yield, Await] :
//      this
//      IdentifierReference[?Yield, ?Await]
//      Literal
//      ArrayLiteral[?Yield, ?Await]
//      ObjectLiteral[?Yield, ?Await]
//      FunctionExpression
//      ClassExpression[?Yield, ?Await]
//      GeneratorExpression
//      AsyncFunctionExpression
//      AsyncGeneratorExpression
//      RegularExpressionLiteral
//      TemplateLiteral[?Yield, ?Await, ~Tagged]
//      CoverParenthesizedExpressionAndArrowParameterList[?Yield, ?Await]

#[derive(Debug)]
pub(crate) enum PrimaryExpression {
    This { location: Location },
    IdentifierReference { node: Rc<IdentifierReference> },
    Literal { node: Rc<Literal> },
    ArrayLiteral { node: Rc<ArrayLiteral> },
    ObjectLiteral { node: Rc<ObjectLiteral> },
    Parenthesized { node: Rc<ParenthesizedExpression> },
    TemplateLiteral { node: Rc<TemplateLiteral> },
    Function { node: Rc<FunctionExpression> },
    Class { node: Rc<ClassExpression> },
    Generator { node: Rc<GeneratorExpression> },
    AsyncFunction { node: Rc<AsyncFunctionExpression> },
    AsyncGenerator { node: Rc<AsyncGeneratorExpression> },
    RegularExpression { regex: RegularExpressionData, location: Location },
}

impl fmt::Display for PrimaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PrimaryExpression::This { .. } => write!(f, "this"),
            PrimaryExpression::IdentifierReference { node } => node.fmt(f),
            PrimaryExpression::Literal { node } => node.fmt(f),
            PrimaryExpression::ArrayLiteral { node } => node.fmt(f),
            PrimaryExpression::ObjectLiteral { node } => node.fmt(f),
            PrimaryExpression::Parenthesized { node } => node.fmt(f),
            PrimaryExpression::TemplateLiteral { node } => node.fmt(f),
            PrimaryExpression::Function { node } => node.fmt(f),
            PrimaryExpression::Class { node } => node.fmt(f),
            PrimaryExpression::Generator { node } => node.fmt(f),
            PrimaryExpression::AsyncFunction { node } => node.fmt(f),
            PrimaryExpression::AsyncGenerator { node } => node.fmt(f),
            PrimaryExpression::RegularExpression { regex, .. } => regex.fmt(f),
        }
    }
}

impl PrettyPrint for PrimaryExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}PrimaryExpression: {self}")?;
        match self {
            PrimaryExpression::This { .. } | PrimaryExpression::RegularExpression { .. } => Ok(()),
            PrimaryExpression::IdentifierReference { node } => {
                node.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            PrimaryExpression::Literal { node } => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpression::ArrayLiteral { node } => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpression::ObjectLiteral { node } => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpression::Parenthesized { node } => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpression::TemplateLiteral { node } => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpression::Function { node } => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpression::Class { node } => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpression::Generator { node } => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpression::AsyncFunction { node } => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpression::AsyncGenerator { node } => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            PrimaryExpression::This { .. } => pprint_token(writer, "this", TokenType::Keyword, pad, state),
            PrimaryExpression::IdentifierReference { node } => node.concise_with_leftpad(writer, pad, state),
            PrimaryExpression::Literal { node } => node.concise_with_leftpad(writer, pad, state),
            PrimaryExpression::ArrayLiteral { node } => node.concise_with_leftpad(writer, pad, state),
            PrimaryExpression::ObjectLiteral { node } => node.concise_with_leftpad(writer, pad, state),
            PrimaryExpression::Parenthesized { node } => node.concise_with_leftpad(writer, pad, state),
            PrimaryExpression::TemplateLiteral { node } => node.concise_with_leftpad(writer, pad, state),
            PrimaryExpression::Function { node } => node.concise_with_leftpad(writer, pad, state),
            PrimaryExpression::Class { node } => node.concise_with_leftpad(writer, pad, state),
            PrimaryExpression::Generator { node } => node.concise_with_leftpad(writer, pad, state),
            PrimaryExpression::AsyncFunction { node } => node.concise_with_leftpad(writer, pad, state),
            PrimaryExpression::AsyncGenerator { node } => node.concise_with_leftpad(writer, pad, state),
            PrimaryExpression::RegularExpression { regex: item, .. } => {
                pprint_token(writer, item, TokenType::RegularExpression, pad, state)
            }
        }
    }
}

impl From<Rc<IdentifierReference>> for PrimaryExpression {
    fn from(node: Rc<IdentifierReference>) -> Self {
        Self::IdentifierReference { node }
    }
}

impl From<Rc<Literal>> for PrimaryExpression {
    fn from(node: Rc<Literal>) -> Self {
        Self::Literal { node }
    }
}

impl From<Rc<ArrayLiteral>> for PrimaryExpression {
    fn from(node: Rc<ArrayLiteral>) -> Self {
        Self::ArrayLiteral { node }
    }
}

impl From<Rc<ObjectLiteral>> for PrimaryExpression {
    fn from(node: Rc<ObjectLiteral>) -> Self {
        Self::ObjectLiteral { node }
    }
}

impl From<Rc<ParenthesizedExpression>> for PrimaryExpression {
    fn from(node: Rc<ParenthesizedExpression>) -> Self {
        Self::Parenthesized { node }
    }
}

impl From<Rc<TemplateLiteral>> for PrimaryExpression {
    fn from(node: Rc<TemplateLiteral>) -> Self {
        Self::TemplateLiteral { node }
    }
}

impl From<Rc<FunctionExpression>> for PrimaryExpression {
    fn from(node: Rc<FunctionExpression>) -> Self {
        Self::Function { node }
    }
}

impl From<Rc<ClassExpression>> for PrimaryExpression {
    fn from(node: Rc<ClassExpression>) -> Self {
        Self::Class { node }
    }
}

impl From<Rc<GeneratorExpression>> for PrimaryExpression {
    fn from(node: Rc<GeneratorExpression>) -> Self {
        Self::Generator { node }
    }
}

impl From<Rc<AsyncFunctionExpression>> for PrimaryExpression {
    fn from(node: Rc<AsyncFunctionExpression>) -> Self {
        Self::AsyncFunction { node }
    }
}

impl From<Rc<AsyncGeneratorExpression>> for PrimaryExpression {
    fn from(node: Rc<AsyncGeneratorExpression>) -> Self {
        Self::AsyncGenerator { node }
    }
}

impl PrimaryExpression {
    fn parse_this(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (tok_loc, after) = scan_for_keyword(scanner, parser.source, InputElementGoal::RegExp, Keyword::This)?;
        Ok((Rc::new(PrimaryExpression::This { location: tok_loc }), after))
    }

    fn parse_idref(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (node, after) = IdentifierReference::parse(parser, scanner, yield_flag, await_flag)?;
        Ok((Rc::new(PrimaryExpression::from(node)), after))
    }

    fn parse_literal(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (node, after) = Literal::parse(parser, scanner)?;
        Ok((Rc::new(PrimaryExpression::from(node)), after))
    }

    fn parse_array_literal(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (node, after) = ArrayLiteral::parse(parser, scanner, yield_flag, await_flag)?;
        Ok((Rc::new(PrimaryExpression::from(node)), after))
    }

    fn parse_object_literal(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (node, after) = ObjectLiteral::parse(parser, scanner, yield_flag, await_flag)?;
        Ok((Rc::new(PrimaryExpression::from(node)), after))
    }

    fn parse_function_exp(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (node, after) = FunctionExpression::parse(parser, scanner)?;
        Ok((Rc::new(PrimaryExpression::from(node)), after))
    }
    fn parse_parenthesized_exp(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (node, after) = ParenthesizedExpression::parse(parser, scanner, yield_flag, await_flag)?;
        Ok((Rc::new(PrimaryExpression::from(node)), after))
    }
    fn parse_template_literal(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (node, after) = TemplateLiteral::parse(parser, scanner, yield_flag, await_flag, false)?;
        Ok((Rc::new(PrimaryExpression::from(node)), after))
    }

    fn parse_class_exp(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (node, after) = ClassExpression::parse(parser, scanner, yield_flag, await_flag)?;
        Ok((Rc::new(PrimaryExpression::from(node)), after))
    }

    fn parse_generator_exp(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (node, after) = GeneratorExpression::parse(parser, scanner)?;
        Ok((Rc::new(PrimaryExpression::from(node)), after))
    }

    fn parse_async_func(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (node, after) = AsyncFunctionExpression::parse(parser, scanner)?;
        Ok((Rc::new(PrimaryExpression::from(node)), after))
    }

    fn parse_async_gen(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (node, after) = AsyncGeneratorExpression::parse(parser, scanner)?;
        Ok((Rc::new(PrimaryExpression::from(node)), after))
    }

    fn parse_regex(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (tok, tok_loc, after) = scan_token(&scanner, parser.source, InputElementGoal::RegExp);
        match tok {
            Token::RegularExpression(rd) => {
                Ok((Rc::new(PrimaryExpression::RegularExpression { regex: rd, location: tok_loc }), after))
            }
            _ => Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::RegularExpression), tok_loc)),
        }
    }

    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::PrimaryExpression), scanner))
            .otherwise(|| Self::parse_this(parser, scanner))
            .otherwise(|| Self::parse_async_func(parser, scanner))
            .otherwise(|| Self::parse_async_gen(parser, scanner))
            .otherwise(|| Self::parse_idref(parser, scanner, yield_flag, await_flag))
            .otherwise(|| Self::parse_literal(parser, scanner))
            .otherwise(|| Self::parse_array_literal(parser, scanner, yield_flag, await_flag))
            .otherwise(|| Self::parse_object_literal(parser, scanner, yield_flag, await_flag))
            .otherwise(|| Self::parse_function_exp(parser, scanner))
            .otherwise(|| Self::parse_parenthesized_exp(parser, scanner, yield_flag, await_flag))
            .otherwise(|| Self::parse_template_literal(parser, scanner, yield_flag, await_flag))
            .otherwise(|| Self::parse_class_exp(parser, scanner, yield_flag, await_flag))
            .otherwise(|| Self::parse_generator_exp(parser, scanner))
            .otherwise(|| Self::parse_regex(parser, scanner))
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            PrimaryExpression::This { location } | PrimaryExpression::RegularExpression { location, .. } => *location,
            PrimaryExpression::IdentifierReference { node } => node.location(),
            PrimaryExpression::Literal { node } => node.location(),
            PrimaryExpression::ArrayLiteral { node } => node.location(),
            PrimaryExpression::ObjectLiteral { node } => node.location(),
            PrimaryExpression::Parenthesized { node } => node.location(),
            PrimaryExpression::TemplateLiteral { node } => node.location(),
            PrimaryExpression::Function { node } => node.location(),
            PrimaryExpression::Class { node } => node.location(),
            PrimaryExpression::Generator { node } => node.location(),
            PrimaryExpression::AsyncFunction { node } => node.location(),
            PrimaryExpression::AsyncGenerator { node } => node.location(),
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            PrimaryExpression::This { .. } => kind == ParseNodeKind::This,
            PrimaryExpression::Literal { node } => kind == ParseNodeKind::Literal || node.contains(kind),
            PrimaryExpression::ArrayLiteral { node } => node.contains(kind),
            PrimaryExpression::ObjectLiteral { node } => node.contains(kind),
            PrimaryExpression::Parenthesized { node } => node.contains(kind),
            PrimaryExpression::TemplateLiteral { node } => node.contains(kind),
            PrimaryExpression::Class { node } => node.contains(kind),
            PrimaryExpression::IdentifierReference { .. }
            | PrimaryExpression::Generator { .. }
            | PrimaryExpression::Function { .. }
            | PrimaryExpression::AsyncFunction { .. }
            | PrimaryExpression::AsyncGenerator { .. }
            | PrimaryExpression::RegularExpression { .. } => false,
        }
    }

    pub(crate) fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            PrimaryExpression::Literal { node: n } => n.as_string_literal(),
            _ => None,
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
            PrimaryExpression::This { .. }
            | PrimaryExpression::IdentifierReference { .. }
            | PrimaryExpression::Literal { .. }
            | PrimaryExpression::RegularExpression { .. } => true,
            PrimaryExpression::ArrayLiteral { node } => node.all_private_identifiers_valid(names),
            PrimaryExpression::ObjectLiteral { node } => node.all_private_identifiers_valid(names),
            PrimaryExpression::Parenthesized { node } => node.all_private_identifiers_valid(names),
            PrimaryExpression::TemplateLiteral { node } => node.all_private_identifiers_valid(names),
            PrimaryExpression::Function { node } => node.all_private_identifiers_valid(names),
            PrimaryExpression::Class { node } => node.all_private_identifiers_valid(names),
            PrimaryExpression::Generator { node } => node.all_private_identifiers_valid(names),
            PrimaryExpression::AsyncFunction { node } => node.all_private_identifiers_valid(names),
            PrimaryExpression::AsyncGenerator { node } => node.all_private_identifiers_valid(names),
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
            PrimaryExpression::This { .. }
            | PrimaryExpression::Literal { .. }
            | PrimaryExpression::Function { .. }
            | PrimaryExpression::Generator { .. }
            | PrimaryExpression::AsyncFunction { .. }
            | PrimaryExpression::AsyncGenerator { .. }
            | PrimaryExpression::RegularExpression { .. } => false,
            PrimaryExpression::IdentifierReference { node: ir } => ir.contains_arguments(),
            PrimaryExpression::ArrayLiteral { node: al } => al.contains_arguments(),
            PrimaryExpression::ObjectLiteral { node: ol } => ol.contains_arguments(),
            PrimaryExpression::Parenthesized { node: pe } => pe.contains_arguments(),
            PrimaryExpression::Class { node: ce } => ce.contains_arguments(),
            PrimaryExpression::TemplateLiteral { node: tl } => tl.contains_arguments(),
        }
    }

    pub(crate) fn is_object_or_array_literal(&self) -> bool {
        matches!(self, PrimaryExpression::ArrayLiteral { .. } | PrimaryExpression::ObjectLiteral { .. })
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            PrimaryExpression::This { .. } => {}
            PrimaryExpression::IdentifierReference { node: id } => id.early_errors(errs, strict),
            PrimaryExpression::Literal { node: lit } => lit.early_errors(),
            PrimaryExpression::ArrayLiteral { node } => node.early_errors(errs, strict),
            PrimaryExpression::ObjectLiteral { node } => node.early_errors(errs, strict),
            PrimaryExpression::Parenthesized { node } => node.early_errors(errs, strict),
            PrimaryExpression::TemplateLiteral { node } => node.early_errors(errs, strict, 0xffff_ffff),
            PrimaryExpression::Function { node } => node.early_errors(errs, strict),
            PrimaryExpression::Class { node } => node.early_errors(errs),
            PrimaryExpression::Generator { node } => node.early_errors(errs, strict),
            PrimaryExpression::AsyncFunction { node } => node.early_errors(errs, strict),
            PrimaryExpression::AsyncGenerator { node } => node.early_errors(errs, strict),
            PrimaryExpression::RegularExpression { regex, location } => {
                // Static Semantics: Early Errors
                //      PrimaryExpression : RegularExpressionLiteral
                //  * It is a Syntax Error if IsValidRegularExpressionLiteral(RegularExpressionLiteral) is false.
                if let Err(msg) = regex.validate_regular_expression_literal() {
                    errs.push(create_syntax_error_object(msg, Some(*location)));
                }
            }
        }
    }

    pub(crate) fn is_strictly_deletable(&self) -> bool {
        match self {
            PrimaryExpression::IdentifierReference { .. } => false,
            PrimaryExpression::Parenthesized { node: exp } => exp.is_strictly_deletable(),
            _ => true,
        }
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub(crate) fn assignment_target_type(&self, strict: bool) -> ATTKind {
        use PrimaryExpression::{
            ArrayLiteral, AsyncFunction, AsyncGenerator, Class, Function, Generator, IdentifierReference, Literal,
            ObjectLiteral, Parenthesized, RegularExpression, TemplateLiteral, This,
        };
        match self {
            This { .. }
            | Literal { .. }
            | ArrayLiteral { .. }
            | ObjectLiteral { .. }
            | TemplateLiteral { .. }
            | RegularExpression { .. }
            | Function { .. }
            | Class { .. }
            | Generator { .. }
            | AsyncFunction { .. }
            | AsyncGenerator { .. } => ATTKind::Invalid,
            IdentifierReference { node: id } => id.assignment_target_type(strict),
            Parenthesized { node: expr } => expr.assignment_target_type(strict),
        }
    }

    /// True if this production winds up being an IdentifierRef
    ///
    /// See [IsIdentifierRef](https://tc39.es/ecma262/#sec-static-semantics-isidentifierref) from ECMA-262.
    //pub(crate) fn is_identifier_ref(&self) -> bool {
    //    matches!(self, PrimaryExpression::IdentifierReference { .. })
    //}
    pub(crate) fn identifier_ref(&self) -> Option<Rc<IdentifierReference>> {
        match self {
            PrimaryExpression::IdentifierReference { node } => Some(node.clone()),
            PrimaryExpression::This { .. }
            | PrimaryExpression::Literal { .. }
            | PrimaryExpression::ArrayLiteral { .. }
            | PrimaryExpression::ObjectLiteral { .. }
            | PrimaryExpression::Parenthesized { .. }
            | PrimaryExpression::TemplateLiteral { .. }
            | PrimaryExpression::Function { .. }
            | PrimaryExpression::Class { .. }
            | PrimaryExpression::Generator { .. }
            | PrimaryExpression::AsyncFunction { .. }
            | PrimaryExpression::AsyncGenerator { .. }
            | PrimaryExpression::RegularExpression { .. } => None,
        }
    }

    //pub(crate) fn is_named_function(&self) -> bool {
    //    match self {
    //        PrimaryExpression::Function { node } => node.is_named_function(),
    //        PrimaryExpression::Class { node } => node.is_named_function(),
    //        PrimaryExpression::Generator { node } => node.is_named_function(),
    //        PrimaryExpression::AsyncFunction { node } => node.is_named_function(),
    //        PrimaryExpression::AsyncGenerator { node } => node.is_named_function(),
    //        PrimaryExpression::Parenthesized { node } => node.is_named_function(),
    //        _ => false,
    //    }
    //}

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        match self {
            PrimaryExpression::This { .. }
            | PrimaryExpression::IdentifierReference { .. }
            | PrimaryExpression::Literal { .. }
            | PrimaryExpression::RegularExpression { .. } => None,
            PrimaryExpression::ArrayLiteral { node } => node.body_containing_location(location),
            PrimaryExpression::ObjectLiteral { node } => node.body_containing_location(location),
            PrimaryExpression::Parenthesized { node } => node.body_containing_location(location),
            PrimaryExpression::TemplateLiteral { node } => node.body_containing_location(location),
            PrimaryExpression::Function { node } => node.body_containing_location(location),
            PrimaryExpression::Class { node } => node.body_containing_location(location),
            PrimaryExpression::Generator { node } => node.body_containing_location(location),
            PrimaryExpression::AsyncFunction { node } => node.body_containing_location(location),
            PrimaryExpression::AsyncGenerator { node } => node.body_containing_location(location),
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
        //
        // PrimaryExpression[Yield, Await] :
        //      this
        //      IdentifierReference[?Yield, ?Await]
        //      Literal
        //      ArrayLiteral[?Yield, ?Await]
        //      ObjectLiteral[?Yield, ?Await]
        //      FunctionExpression
        //      ClassExpression[?Yield, ?Await]
        //      GeneratorExpression
        //      AsyncFunctionExpression
        //      AsyncGeneratorExpression
        //      RegularExpressionLiteral
        //      TemplateLiteral[?Yield, ?Await, ~Tagged]
        //      CoverParenthesizedExpressionAndArrowParameterList[?Yield, ?Await]
        match self {
            PrimaryExpression::This { .. }
            | PrimaryExpression::IdentifierReference { .. }
            | PrimaryExpression::Literal { .. }
            | PrimaryExpression::ArrayLiteral { .. }
            | PrimaryExpression::ObjectLiteral { .. }
            | PrimaryExpression::TemplateLiteral { .. }
            | PrimaryExpression::Function { .. }
            | PrimaryExpression::Class { .. }
            | PrimaryExpression::Generator { .. }
            | PrimaryExpression::AsyncFunction { .. }
            | PrimaryExpression::AsyncGenerator { .. }
            | PrimaryExpression::RegularExpression { .. } => false,
            PrimaryExpression::Parenthesized { node } => node.has_call_in_tail_position(location),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Elisions {
    pub(crate) count: usize,
    pub(crate) location: Location,
}

impl fmt::Display for Elisions {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        assert!(self.count > 0);
        write!(f, ",")?;
        for _ in 1..self.count {
            write!(f, " ,")?;
        }
        Ok(())
    }
}

impl PrettyPrint for Elisions {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{first}Elisions: {self}")
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.pprint_with_leftpad(writer, pad, state)
    }
}

impl Elisions {
    fn parse_core(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        // Note: This function only ever returns an error at the same lexical position as the input args. Generally this
        // means it's never a reportable error. If this production is used optionally, throwing away the error makes the
        // most sense, otherwise you get unreachable code.
        let mut comma_count: usize = 0;
        let mut current_scanner = scanner;
        let mut current_location = None;
        loop {
            let (token, tok_loc, after_comma) = scan_token(&current_scanner, parser.source, InputElementGoal::RegExp);
            if !token.matches_punct(Punctuator::Comma) {
                return if comma_count == 0 {
                    Err(ParseError::new(PECode::PunctuatorExpected(Punctuator::Comma), tok_loc))
                } else {
                    Ok((Rc::new(Elisions { count: comma_count, location: current_location.unwrap() }), current_scanner))
                };
            }
            comma_count += 1;
            current_scanner = after_comma;
            current_location = match current_location {
                None => Some(tok_loc),
                Some(prior) => Some(prior.merge(&tok_loc)),
            };
        }
    }

    pub(crate) fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        match parser.elision_cache.get(&scanner) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner);
                parser.elision_cache.insert(scanner, result.clone());
                result
            }
        }
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }
}

// SpreadElement[Yield, Await] :
//      ... AssignmentExpression[+In, ?Yield, ?Await]
#[derive(Debug)]
pub(crate) struct SpreadElement {
    pub(crate) ae: Rc<AssignmentExpression>,
    location: Location,
}

impl fmt::Display for SpreadElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "... {}", self.ae)
    }
}

impl PrettyPrint for SpreadElement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}SpreadElement: {self}")?;
        self.ae.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}SpreadElement: {self}")?;
        pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.ae.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl SpreadElement {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (tok_loc, after_ellipsis) =
            scan_for_punct(scanner, parser.source, InputElementGoal::RegExp, Punctuator::Ellipsis)?;
        let (ae, after_ae) = AssignmentExpression::parse(parser, after_ellipsis, true, yield_flag, await_flag)?;
        Ok((
            Rc::new({
                let location = tok_loc.merge(&ae.location());
                SpreadElement { ae, location }
            }),
            after_ae,
        ))
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        self.ae.contains(kind)
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.ae.all_private_identifiers_valid(names)
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
        self.ae.contains_arguments()
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.ae.early_errors(errs, strict);
    }

    #[expect(unused_variables)]
    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        todo!()
    }
}

// ElementList[Yield, Await] :
//      Elisionopt AssignmentExpression[+In, ?Yield, ?Await]
//      Elisionopt SpreadElement[?Yield, ?Await]
//      ElementList[?Yield, ?Await] , Elisionopt AssignmentExpression[+In, ?Yield, ?Await]
//      ElementList[?Yield, ?Await] , Elisionopt SpreadElement[?Yield, ?Await]
#[derive(Debug)]
pub(crate) enum ElementList {
    AssignmentExpression { elision: Option<Rc<Elisions>>, ae: Rc<AssignmentExpression> },
    SpreadElement { elision: Option<Rc<Elisions>>, se: Rc<SpreadElement> },
    ListAssignmentExpression { el: Rc<ElementList>, elision: Option<Rc<Elisions>>, ae: Rc<AssignmentExpression> },
    ListSpreadElement { el: Rc<ElementList>, elision: Option<Rc<Elisions>>, se: Rc<SpreadElement> },
}

impl fmt::Display for ElementList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ElementList::AssignmentExpression { elision, ae } => match elision {
                None => write!(f, "{ae}"),
                Some(commas) => write!(f, "{commas} {ae}"),
            },
            ElementList::SpreadElement { elision, se } => match elision {
                None => write!(f, "{se}"),
                Some(commas) => write!(f, "{commas} {se}"),
            },
            ElementList::ListAssignmentExpression { el, elision, ae } => match elision {
                None => write!(f, "{el} , {ae}"),
                Some(commas) => write!(f, "{el} , {commas} {ae}"),
            },
            ElementList::ListSpreadElement { el, elision, se } => match elision {
                None => write!(f, "{el} , {se}"),
                Some(commas) => write!(f, "{el} , {commas} {se}"),
            },
        }
    }
}

impl PrettyPrint for ElementList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ElementList: {self}")?;
        match self {
            ElementList::AssignmentExpression { elision, ae } => match elision {
                None => ae.pprint_with_leftpad(writer, &successive, Spot::Final),
                Some(commas) => {
                    commas.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    ae.pprint_with_leftpad(writer, &successive, Spot::Final)
                }
            },
            ElementList::SpreadElement { elision, se: boxed } => match elision {
                None => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
                Some(commas) => {
                    commas.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
                }
            },
            ElementList::ListAssignmentExpression { el: right, elision, ae: left } => match elision {
                None => {
                    right.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    left.pprint_with_leftpad(writer, &successive, Spot::Final)
                }
                Some(commas) => {
                    right.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    commas.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    left.pprint_with_leftpad(writer, &successive, Spot::Final)
                }
            },
            ElementList::ListSpreadElement { el: right, elision, se: left } => match elision {
                None => {
                    right.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    left.pprint_with_leftpad(writer, &successive, Spot::Final)
                }
                Some(commas) => {
                    right.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    commas.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    left.pprint_with_leftpad(writer, &successive, Spot::Final)
                }
            },
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        match self {
            ElementList::AssignmentExpression { elision: None, ae } => ae.concise_with_leftpad(writer, pad, state),
            ElementList::AssignmentExpression { elision: Some(commas), ae } => {
                writeln!(writer, "{first}ElementList: {self}")?;
                commas.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ae.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ElementList::SpreadElement { elision: None, se } => se.concise_with_leftpad(writer, pad, state),
            ElementList::SpreadElement { elision: Some(commas), se } => {
                writeln!(writer, "{first}ElementList: {self}")?;
                commas.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                se.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ElementList::ListAssignmentExpression { el, elision: None, ae } => {
                writeln!(writer, "{first}ElementList: {self}")?;
                el.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                ae.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ElementList::ListAssignmentExpression { el, elision: Some(commas), ae } => {
                writeln!(writer, "{first}ElementList: {self}")?;
                el.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                commas.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ae.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ElementList::ListSpreadElement { el, elision: None, se } => {
                writeln!(writer, "{first}ElementList: {self}")?;
                el.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                se.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ElementList::ListSpreadElement { el, elision: Some(commas), se } => {
                writeln!(writer, "{first}ElementList: {self}")?;
                el.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                commas.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                se.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

enum ELItemKind {
    AE(Rc<AssignmentExpression>),
    SE(Rc<SpreadElement>),
}

impl ElementList {
    fn non_recursive_part(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<(Option<Rc<Elisions>>, ELItemKind, Scanner), ParseError> {
        let pot_elision = Elisions::parse(parser, scanner);
        let (elision, after_e_scanner) = match pot_elision {
            Ok((boxed, after_elision)) => (Some(boxed), after_elision),
            Err(_) => (None, scanner),
        };
        let pot_ae = AssignmentExpression::parse(parser, after_e_scanner, true, yield_flag, await_flag);
        match pot_ae {
            Ok((boxed, after_ae_scanner)) => Ok((elision, ELItemKind::AE(boxed), after_ae_scanner)),
            Err(pe) => {
                let err_ae = Some(pe);

                let pot_se = SpreadElement::parse(parser, after_e_scanner, yield_flag, await_flag);
                match pot_se {
                    Ok((boxed, after_se_scanner)) => Ok((elision, ELItemKind::SE(boxed), after_se_scanner)),
                    Err(pe) => {
                        let err_default =
                            Some(ParseError::new(PECode::AssignmentExpressionOrSpreadElementExpected, after_e_scanner));
                        let err_se = Some(pe);
                        let err1 =
                            if ParseError::compare_option(err_default.as_ref(), err_ae.as_ref()) == Ordering::Less {
                                err_ae
                            } else {
                                err_default
                            };
                        let err2 = if ParseError::compare_option(err1.as_ref(), err_se.as_ref()) == Ordering::Less {
                            err_se
                        } else {
                            err1
                        };
                        Err(err2.unwrap())
                    }
                }
            }
        }
    }

    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (elision, item, after) = Self::non_recursive_part(parser, scanner, yield_flag, await_flag)?;
        let mut current_production = match item {
            ELItemKind::AE(boxed_ae) => Rc::new(ElementList::AssignmentExpression { elision, ae: boxed_ae }),
            ELItemKind::SE(boxed_se) => Rc::new(ElementList::SpreadElement { elision, se: boxed_se }),
        };
        let mut current_scanner = after;

        while let Ok((elision, item, after)) =
            scan_for_punct(current_scanner, parser.source, InputElementGoal::Div, Punctuator::Comma)
                .and_then(|(_, after_comma)| Self::non_recursive_part(parser, after_comma, yield_flag, await_flag))
        {
            current_production = match item {
                ELItemKind::AE(boxed_ae) => {
                    Rc::new(ElementList::ListAssignmentExpression { el: current_production, elision, ae: boxed_ae })
                }
                ELItemKind::SE(boxed_se) => {
                    Rc::new(ElementList::ListSpreadElement { el: current_production, elision, se: boxed_se })
                }
            };
            current_scanner = after;
        }
        Ok((current_production, current_scanner))
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            ElementList::AssignmentExpression { elision: None, ae } => ae.location(),
            ElementList::AssignmentExpression { elision: Some(elision), ae } => {
                elision.location().merge(&ae.location())
            }
            ElementList::SpreadElement { elision: None, se } => se.location(),
            ElementList::SpreadElement { elision: Some(elision), se } => elision.location().merge(&se.location()),
            ElementList::ListAssignmentExpression { el, elision: _, ae } => el.location().merge(&ae.location()),
            ElementList::ListSpreadElement { el, elision: _, se } => el.location().merge(&se.location()),
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ElementList::AssignmentExpression { elision, ae } => {
                elision.as_ref().is_some_and(|_| kind == ParseNodeKind::Elisions) || ae.contains(kind)
            }
            ElementList::SpreadElement { elision, se } => {
                elision.as_ref().is_some_and(|_| kind == ParseNodeKind::Elisions) || se.contains(kind)
            }
            ElementList::ListAssignmentExpression { el, elision, ae } => {
                el.contains(kind)
                    || elision.as_ref().is_some_and(|_| kind == ParseNodeKind::Elisions)
                    || ae.contains(kind)
            }
            ElementList::ListSpreadElement { el, elision, se } => {
                el.contains(kind)
                    || elision.as_ref().is_some_and(|_| kind == ParseNodeKind::Elisions)
                    || se.contains(kind)
            }
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
            ElementList::AssignmentExpression { elision: _, ae } => ae.all_private_identifiers_valid(names),
            ElementList::SpreadElement { elision: _, se } => se.all_private_identifiers_valid(names),
            ElementList::ListAssignmentExpression { el, elision: _, ae } => {
                el.all_private_identifiers_valid(names) && ae.all_private_identifiers_valid(names)
            }
            ElementList::ListSpreadElement { el, elision: _, se } => {
                el.all_private_identifiers_valid(names) && se.all_private_identifiers_valid(names)
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
            ElementList::AssignmentExpression { ae, .. } => ae.contains_arguments(),
            ElementList::SpreadElement { se, .. } => se.contains_arguments(),
            ElementList::ListAssignmentExpression { el, ae, .. } => el.contains_arguments() || ae.contains_arguments(),
            ElementList::ListSpreadElement { el, se, .. } => el.contains_arguments() || se.contains_arguments(),
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ElementList::AssignmentExpression { ae: b, .. } => {
                b.early_errors(errs, strict);
            }
            ElementList::SpreadElement { se: b, .. } => {
                b.early_errors(errs, strict);
            }
            ElementList::ListAssignmentExpression { el: a, ae: c, .. } => {
                a.early_errors(errs, strict);
                c.early_errors(errs, strict);
            }
            ElementList::ListSpreadElement { el: a, se: c, .. } => {
                a.early_errors(errs, strict);
                c.early_errors(errs, strict);
            }
        }
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                ElementList::AssignmentExpression { elision: _, ae } => ae.body_containing_location(location),
                ElementList::SpreadElement { elision: _, se } => se.body_containing_location(location),
                ElementList::ListAssignmentExpression { el, elision: _, ae } => {
                    el.body_containing_location(location).or_else(|| ae.body_containing_location(location))
                }
                ElementList::ListSpreadElement { el, elision: _, se } => {
                    el.body_containing_location(location).or_else(|| se.body_containing_location(location))
                }
            }
        } else {
            None
        }
    }
}

// ArrayLiteral[Yield, Await] :
//      [ Elisionopt ]
//      [ ElementList[?Yield, ?Await] ]
//      [ ElementList[?Yield, ?Await] , Elisionopt ]
#[derive(Debug)]
pub(crate) enum ArrayLiteral {
    Empty { elision: Option<Rc<Elisions>>, location: Location },
    ElementList { el: Rc<ElementList>, location: Location },
    ElementListElision { el: Rc<ElementList>, elision: Option<Rc<Elisions>>, location: Location },
}

impl fmt::Display for ArrayLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArrayLiteral::Empty { elision: None, .. } => write!(f, "[ ]"),
            ArrayLiteral::Empty { elision: Some(elision), .. } => write!(f, "[ {elision} ]"),
            ArrayLiteral::ElementList { el: boxed, .. } => write!(f, "[ {boxed} ]"),
            ArrayLiteral::ElementListElision { el: boxed, elision: None, .. } => write!(f, "[ {boxed} , ]"),
            ArrayLiteral::ElementListElision { el: boxed, elision: Some(elision), .. } => {
                write!(f, "[ {boxed} , {elision} ]")
            }
        }
    }
}

impl PrettyPrint for ArrayLiteral {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ArrayLiteral: {self}")?;
        match self {
            ArrayLiteral::Empty { elision: None, .. } => Ok(()),
            ArrayLiteral::Empty { elision: Some(elision), .. } => {
                elision.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayLiteral::ElementList { el: boxed, .. }
            | ArrayLiteral::ElementListElision { el: boxed, elision: None, .. } => {
                boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayLiteral::ElementListElision { el: boxed, elision: Some(elision), .. } => {
                boxed.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                elision.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ArrayLiteral: {self}")?;
        match self {
            ArrayLiteral::Empty { elision: None, .. } => {
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            ArrayLiteral::Empty { elision: Some(elision), .. } => {
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                elision.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            ArrayLiteral::ElementList { el: node, .. } => {
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            ArrayLiteral::ElementListElision { el: node, elision: None, .. } => {
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            ArrayLiteral::ElementListElision { el: node, elision: Some(elision), .. } => {
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                elision.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
        }
    }
}

impl ArrayLiteral {
    // ArrayLiteral's only parent is PrimaryExpression. It doesn't need to be cached.
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (open_loc, after) =
            scan_for_punct(scanner, parser.source, InputElementGoal::RegExp, Punctuator::LeftBracket)?;
        Err(ParseError::new(PECode::CommaLeftBracketElementListExpected, after))
            .otherwise(|| {
                let (el, after_el) = ElementList::parse(parser, after, yield_flag, await_flag)?;
                let (punct, punct_loc, after_punct) = scan_for_punct_set(
                    after_el,
                    parser.source,
                    InputElementGoal::Div,
                    &[Punctuator::Comma, Punctuator::RightBracket],
                )?;
                match punct {
                    Punctuator::RightBracket => Ok((
                        Rc::new(ArrayLiteral::ElementList { el, location: open_loc.merge(&punct_loc) }),
                        after_punct,
                    )),
                    _ => {
                        let (elision, after_elisions) = match Elisions::parse(parser, after_punct) {
                            Ok((node, scan)) => (Some(node), scan),
                            Err(_) => (None, after_punct),
                        };
                        let (end_loc, end_scan) = scan_for_punct(
                            after_elisions,
                            parser.source,
                            InputElementGoal::RegExp,
                            Punctuator::RightBracket,
                        )?;
                        Ok((
                            Rc::new(ArrayLiteral::ElementListElision {
                                el,
                                elision,
                                location: open_loc.merge(&end_loc),
                            }),
                            end_scan,
                        ))
                    }
                }
            })
            .otherwise(|| {
                let (elision, after_elisions) = match Elisions::parse(parser, after) {
                    Ok((node, scan)) => (Some(node), scan),
                    Err(_) => (None, after),
                };
                let (end_loc, end_scan) =
                    scan_for_punct(after_elisions, parser.source, InputElementGoal::RegExp, Punctuator::RightBracket)?;
                Ok((Rc::new(ArrayLiteral::Empty { elision, location: open_loc.merge(&end_loc) }), end_scan))
            })
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            ArrayLiteral::Empty { location, .. }
            | ArrayLiteral::ElementList { location, .. }
            | ArrayLiteral::ElementListElision { location, .. } => *location,
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ArrayLiteral::Empty { elision: pot_elision, .. } => {
                pot_elision.as_ref().is_some_and(|_| kind == ParseNodeKind::Elisions)
            }
            ArrayLiteral::ElementList { el: boxed, .. } => boxed.contains(kind),
            ArrayLiteral::ElementListElision { el: boxed, elision: pot_elision, .. } => {
                boxed.contains(kind) || pot_elision.as_ref().is_some_and(|_| kind == ParseNodeKind::Elisions)
            }
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
            ArrayLiteral::Empty { .. } => true,
            ArrayLiteral::ElementList { el: boxed, .. } | ArrayLiteral::ElementListElision { el: boxed, .. } => {
                boxed.all_private_identifiers_valid(names)
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
            ArrayLiteral::Empty { .. } => false,
            ArrayLiteral::ElementList { el, .. } | ArrayLiteral::ElementListElision { el, .. } => {
                el.contains_arguments()
            }
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ArrayLiteral::Empty { .. } => {}
            ArrayLiteral::ElementList { el: node, .. } | ArrayLiteral::ElementListElision { el: node, .. } => {
                node.early_errors(errs, strict);
            }
        }
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                ArrayLiteral::Empty { .. } => None,
                ArrayLiteral::ElementList { el: node, .. } | ArrayLiteral::ElementListElision { el: node, .. } => {
                    node.body_containing_location(location)
                }
            }
        } else {
            None
        }
    }
}

// Initializer[In, Yield, Await] :
//      = AssignmentExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub(crate) struct Initializer {
    pub(crate) ae: Rc<AssignmentExpression>,
    pub(crate) location: Location,
}

impl fmt::Display for Initializer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "= {}", self.ae)
    }
}

impl PrettyPrint for Initializer {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}Initializer: {self}")?;
        self.ae.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}Initializer: {self}")?;
        pprint_token(writer, "=", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.ae.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl Initializer {
    fn parse_core(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Initializer> {
        let (tok_loc, after_tok) = scan_for_punct(scanner, parser.source, InputElementGoal::RegExp, Punctuator::Eq)?;
        let (boxed_ae, after_ae) = AssignmentExpression::parse(parser, after_tok, in_flag, yield_flag, await_flag)?;
        Ok((
            Rc::new({
                let location = tok_loc.merge(&boxed_ae.location());
                Initializer { ae: boxed_ae, location }
            }),
            after_ae,
        ))
    }

    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Initializer> {
        let key = InYieldAwaitKey { scanner, in_flag, yield_flag, await_flag };
        match parser.initializer_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag, yield_flag, await_flag);
                parser.initializer_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        self.ae.contains(kind)
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.ae.all_private_identifiers_valid(names)
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
        self.ae.contains_arguments()
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.ae.early_errors(errs, strict);
    }

    /// Determine if this parse node is an anonymous function
    ///
    /// See [IsAnonymousFunctionDefinition](https://tc39.es/ecma262/#sec-isanonymousfunctiondefinition) in ECMA-262.
    pub(crate) fn anonymous_function_definition(&self) -> Option<NameableProduction> {
        self.ae.anonymous_function_definition()
    }

    //pub(crate) fn is_anonymous_function_definition(&self) -> bool {
    //    self.ae.is_anonymous_function_definition()
    //}

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        self.ae.body_containing_location(location)
    }
}

// CoverInitializedName[Yield, Await] :
//      IdentifierReference[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]
#[derive(Debug)]
pub(crate) enum CoverInitializedName {
    InitializedName(Rc<IdentifierReference>, Rc<Initializer>),
}

impl fmt::Display for CoverInitializedName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let CoverInitializedName::InitializedName(idref, izer) = self;
        write!(f, "{idref} {izer}")
    }
}

impl PrettyPrint for CoverInitializedName {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}CoverInitializedName: {self}")?;
        let CoverInitializedName::InitializedName(idref, izer) = self;
        idref.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        izer.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}CoverInitializedName: {self}")?;
        let CoverInitializedName::InitializedName(idref, izer) = self;
        idref.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        izer.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl CoverInitializedName {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (idref, after_idref) = IdentifierReference::parse(parser, scanner, yield_flag, await_flag)?;
        let (izer, after_izer) = Initializer::parse(parser, after_idref, true, yield_flag, await_flag)?;
        Ok((Rc::new(CoverInitializedName::InitializedName(idref, izer)), after_izer))
    }

    pub(crate) fn location(&self) -> Location {
        let CoverInitializedName::InitializedName(idref, izer) = self;
        idref.location().merge(&izer.location())
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        let CoverInitializedName::InitializedName(_, izer) = self;
        izer.contains(kind)
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        let CoverInitializedName::InitializedName(_, izer) = self;
        izer.all_private_identifiers_valid(names)
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        let CoverInitializedName::InitializedName(a, b) = self;
        a.early_errors(errs, strict);
        b.early_errors(errs, strict);
    }

    //pub(crate) fn prop_name(&self) -> JSString {
    //    // Static Semantics: PropName
    //    // The syntax-directed operation PropName takes no arguments and returns a String or empty.
    //    let CoverInitializedName::InitializedName(idref, _) = self;
    //    idref.string_value()
    //}

    #[expect(unused_variables)]
    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        todo!()
    }
}

// ComputedPropertyName[Yield, Await] :
//      [ AssignmentExpression[+In, ?Yield, ?Await] ]
#[derive(Debug)]
pub(crate) struct ComputedPropertyName {
    pub(crate) ae: Rc<AssignmentExpression>,
    pub(crate) location: Location,
}

impl fmt::Display for ComputedPropertyName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[ {} ]", self.ae)
    }
}

impl PrettyPrint for ComputedPropertyName {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ComputedPropertyName: {self}")?;
        self.ae.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ComputedPropertyName: {self}")?;
        pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.ae.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ComputedPropertyName {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (open_loc, after_tok) =
            scan_for_punct(scanner, parser.source, InputElementGoal::RegExp, Punctuator::LeftBracket)?;
        let (ae, after_ae) = AssignmentExpression::parse(parser, after_tok, true, yield_flag, await_flag)?;
        let (close_loc, after_rb) =
            scan_for_punct(after_ae, parser.source, InputElementGoal::RegExp, Punctuator::RightBracket)?;
        Ok((Rc::new(ComputedPropertyName { ae, location: open_loc.merge(&close_loc) }), after_rb))
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        self.ae.contains(kind)
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.ae.all_private_identifiers_valid(names)
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
        self.ae.contains_arguments()
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.ae.early_errors(errs, strict);
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location.contains(location) { self.ae.body_containing_location(location) } else { None }
    }
}

// LiteralPropertyName :
//      IdentifierName
//      StringLiteral
//      NumericLiteral
#[derive(Debug)]
pub(crate) enum LiteralPropertyName {
    IdentifierName { data: IdentifierData, location: Location },
    StringLiteral { data: StringToken, location: Location },
    NumericLiteral { data: Numeric, location: Location },
}

impl fmt::Display for LiteralPropertyName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LiteralPropertyName::IdentifierName { data: id, .. } => write!(f, "{id}"),
            LiteralPropertyName::StringLiteral { data: s, .. } => write!(f, "{s}"),
            LiteralPropertyName::NumericLiteral { data: Numeric::Number(n), .. } => {
                let mut s = Vec::new();
                number_to_string(&mut s, *n).unwrap();
                write!(f, "{}", String::from_utf8(s).unwrap())
            }
            LiteralPropertyName::NumericLiteral { data: Numeric::BigInt(b), .. } => {
                write!(f, "{b}")
            }
        }
    }
}

impl PrettyPrint for LiteralPropertyName {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{first}LiteralPropertyName: {self}")
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            LiteralPropertyName::IdentifierName { data: id, .. } => {
                pprint_token(writer, id, TokenType::IdentifierName, pad, state)
            }
            LiteralPropertyName::StringLiteral { data: s, .. } => {
                pprint_token(writer, s, TokenType::String, pad, state)
            }
            LiteralPropertyName::NumericLiteral { data: n, .. } => {
                pprint_token(writer, n, TokenType::Numeric, pad, state)
            }
        }
    }
}

impl LiteralPropertyName {
    pub(crate) fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (tok, tok_loc, after_tok) = scan_token(&scanner, parser.source, InputElementGoal::RegExp);
        match tok {
            Token::Identifier(id) => {
                Ok((Rc::new(LiteralPropertyName::IdentifierName { data: id, location: tok_loc }), after_tok))
            }
            Token::String(s) => {
                Ok((Rc::new(LiteralPropertyName::StringLiteral { data: s, location: tok_loc }), after_tok))
            }
            Token::Number(n) => Ok((
                Rc::new(LiteralPropertyName::NumericLiteral { data: Numeric::Number(n), location: tok_loc }),
                after_tok,
            )),
            Token::BigInt(b) => Ok((
                Rc::new(LiteralPropertyName::NumericLiteral { data: Numeric::BigInt(Rc::new(b)), location: tok_loc }),
                after_tok,
            )),
            _ => Err(ParseError::new(PECode::IdentifierStringNumberExpected, tok_loc)),
        }
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            LiteralPropertyName::IdentifierName { location, .. }
            | LiteralPropertyName::StringLiteral { location, .. }
            | LiteralPropertyName::NumericLiteral { location, .. } => *location,
        }
    }

    pub(crate) fn prop_name(&self) -> JSString {
        // Static Semantics: PropName
        // The syntax-directed operation PropName takes no arguments and returns a String or empty.
        match self {
            LiteralPropertyName::IdentifierName { data: id, .. } => {
                // LiteralPropertyName : IdentifierName
                //  1. Return StringValue of IdentifierName.
                id.string_value.clone()
            }
            LiteralPropertyName::StringLiteral { data: s, .. } => {
                // LiteralPropertyName : StringLiteral
                //  1. Return the SV of StringLiteral.
                s.value.clone()
            }
            LiteralPropertyName::NumericLiteral { data: Numeric::Number(num), .. } => {
                // LiteralPropertyName : NumericLiteral
                //  1. Let nbr be the NumericValue of NumericLiteral.
                //  2. Return ! ToString(nbr).
                let mut s = Vec::new();
                number_to_string(&mut s, *num).unwrap();
                JSString::from(s)
            }
            LiteralPropertyName::NumericLiteral { data: Numeric::BigInt(bi), .. } => JSString::from(bi.to_string()),
        }
    }
}

// PropertyName[Yield, Await] :
//      LiteralPropertyName
//      ComputedPropertyName[?Yield, ?Await]
#[derive(Debug)]
pub(crate) enum PropertyName {
    LiteralPropertyName(Rc<LiteralPropertyName>),
    ComputedPropertyName(Rc<ComputedPropertyName>),
}

impl fmt::Display for PropertyName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PropertyName::LiteralPropertyName(lpn) => write!(f, "{lpn}"),
            PropertyName::ComputedPropertyName(cpn) => write!(f, "{cpn}"),
        }
    }
}

impl PrettyPrint for PropertyName {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}PropertyName: {self}")?;
        match &self {
            PropertyName::LiteralPropertyName(lpn) => lpn.pprint_with_leftpad(writer, &successive, Spot::Final),
            PropertyName::ComputedPropertyName(cpn) => cpn.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            PropertyName::LiteralPropertyName(node) => node.concise_with_leftpad(writer, pad, state),
            PropertyName::ComputedPropertyName(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl PropertyName {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::PropertyName), scanner))
            .otherwise(|| {
                LiteralPropertyName::parse(parser, scanner)
                    .map(|(lpn, after_lpn)| (Rc::new(PropertyName::LiteralPropertyName(lpn)), after_lpn))
            })
            .otherwise(|| {
                ComputedPropertyName::parse(parser, scanner, yield_flag, await_flag)
                    .map(|(cpn, after_cpn)| (Rc::new(PropertyName::ComputedPropertyName(cpn)), after_cpn))
            })
    }

    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.property_name_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.property_name_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            PropertyName::LiteralPropertyName(node) => node.location(),
            PropertyName::ComputedPropertyName(node) => node.location(),
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            PropertyName::LiteralPropertyName(_) => false,
            PropertyName::ComputedPropertyName(n) => n.contains(kind),
        }
    }

    pub(crate) fn computed_property_contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            PropertyName::LiteralPropertyName(..) => false,
            PropertyName::ComputedPropertyName(n) => n.contains(kind),
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
            PropertyName::LiteralPropertyName(_) => true,
            PropertyName::ComputedPropertyName(n) => n.all_private_identifiers_valid(names),
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
            PropertyName::LiteralPropertyName(_) => false,
            PropertyName::ComputedPropertyName(cpn) => cpn.contains_arguments(),
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            PropertyName::LiteralPropertyName(_) => (),
            PropertyName::ComputedPropertyName(x) => x.early_errors(errs, strict),
        }
    }

    pub(crate) fn prop_name(&self) -> Option<JSString> {
        // Static Semantics: PropName
        // The syntax-directed operation PropName takes no arguments and returns a String or empty.
        match self {
            PropertyName::LiteralPropertyName(lpn) => Some(lpn.prop_name()),
            PropertyName::ComputedPropertyName(_) => None,
        }
    }

    /// Reports whether this property key is formed by computation or not
    ///
    /// See [IsComputedPropertyKey](https://tc39.es/ecma262/#sec-static-semantics-iscomputedpropertykey) in ECMA-262.
    pub(crate) fn is_computed_property_key(&self) -> bool {
        match self {
            PropertyName::LiteralPropertyName(_) => false,
            PropertyName::ComputedPropertyName(_) => true,
        }
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        match self {
            PropertyName::LiteralPropertyName(_) => None,
            PropertyName::ComputedPropertyName(cpn) => cpn.body_containing_location(location),
        }
    }
}

// PropertyDefinition[Yield, Await] :
//      IdentifierReference[?Yield, ?Await]
//      CoverInitializedName[?Yield, ?Await]
//      PropertyName[?Yield, ?Await] : AssignmentExpression[+In, ?Yield, ?Await]
//      MethodDefinition[?Yield, ?Await]
//      ... AssignmentExpression[+In, ?Yield, ?Await]
#[derive(Debug)]
pub(crate) enum PropertyDefinition {
    IdentifierReference(Rc<IdentifierReference>),
    CoverInitializedName(Rc<CoverInitializedName>),
    PropertyNameAssignmentExpression(Rc<PropertyName>, Rc<AssignmentExpression>),
    MethodDefinition(Rc<MethodDefinition>),
    AssignmentExpression(Rc<AssignmentExpression>, Location),
}

impl fmt::Display for PropertyDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PropertyDefinition::IdentifierReference(idref) => write!(f, "{idref}"),
            PropertyDefinition::CoverInitializedName(cin) => write!(f, "{cin}"),
            PropertyDefinition::PropertyNameAssignmentExpression(pn, ae) => {
                write!(f, "{pn} : {ae}")
            }
            PropertyDefinition::MethodDefinition(md) => write!(f, "{md}"),
            PropertyDefinition::AssignmentExpression(ae, _) => write!(f, "... {ae}"),
        }
    }
}

impl PrettyPrint for PropertyDefinition {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}PropertyDefinition: {self}")?;
        match self {
            PropertyDefinition::IdentifierReference(idref) => {
                idref.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            PropertyDefinition::CoverInitializedName(cin) => cin.pprint_with_leftpad(writer, &successive, Spot::Final),
            PropertyDefinition::PropertyNameAssignmentExpression(pn, ae) => {
                pn.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ae.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            PropertyDefinition::MethodDefinition(md) => md.pprint_with_leftpad(writer, &successive, Spot::Final),
            PropertyDefinition::AssignmentExpression(ae, _) => ae.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            PropertyDefinition::IdentifierReference(node) => node.concise_with_leftpad(writer, pad, state),
            PropertyDefinition::CoverInitializedName(node) => node.concise_with_leftpad(writer, pad, state),
            PropertyDefinition::MethodDefinition(node) => node.concise_with_leftpad(writer, pad, state),
            PropertyDefinition::PropertyNameAssignmentExpression(left, right) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{first}PropertyDefinition: {self}")?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            PropertyDefinition::AssignmentExpression(node, _) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{first}PropertyDefinition: {self}")?;
                pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl PropertyDefinition {
    fn parse_pn_ae(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (pn, after_pn) = PropertyName::parse(parser, scanner, yield_flag, await_flag)?;
        let (tok, tok_loc, after_tok) = scan_token(&after_pn, parser.source, InputElementGoal::RegExp);
        match tok {
            Token::Punctuator(Punctuator::Colon) => {
                let (ae, after_ae) = AssignmentExpression::parse(parser, after_tok, true, yield_flag, await_flag)?;
                Ok((Rc::new(PropertyDefinition::PropertyNameAssignmentExpression(pn, ae)), after_ae))
            }
            _ => Err(ParseError::new(PECode::PunctuatorExpected(Punctuator::Colon), tok_loc)),
        }
    }

    fn parse_cin(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (cin, after_cin) = CoverInitializedName::parse(parser, scanner, yield_flag, await_flag)?;
        Ok((Rc::new(PropertyDefinition::CoverInitializedName(cin)), after_cin))
    }

    fn parse_md(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (md, after_md) = MethodDefinition::parse(parser, scanner, yield_flag, await_flag)?;
        Ok((Rc::new(PropertyDefinition::MethodDefinition(md)), after_md))
    }

    fn parse_idref(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (idref, after_idref) = IdentifierReference::parse(parser, scanner, yield_flag, await_flag)?;
        Ok((Rc::new(PropertyDefinition::IdentifierReference(idref)), after_idref))
    }

    fn parse_ae(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (dots_loc, after_tok) =
            scan_for_punct(scanner, parser.source, InputElementGoal::RegExp, Punctuator::Ellipsis)?;
        let (ae, after_ae) = AssignmentExpression::parse(parser, after_tok, true, yield_flag, await_flag)?;
        Ok((
            Rc::new({
                let location = dots_loc.merge(&ae.location());
                PropertyDefinition::AssignmentExpression(ae, location)
            }),
            after_ae,
        ))
    }

    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::PropertyName), scanner))
            .otherwise(|| Self::parse_pn_ae(parser, scanner, yield_flag, await_flag))
            .otherwise(|| Self::parse_cin(parser, scanner, yield_flag, await_flag))
            .otherwise(|| Self::parse_md(parser, scanner, yield_flag, await_flag))
            .otherwise(|| Self::parse_idref(parser, scanner, yield_flag, await_flag))
            .otherwise(|| Self::parse_ae(parser, scanner, yield_flag, await_flag))
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            PropertyDefinition::IdentifierReference(node) => node.location(),
            PropertyDefinition::CoverInitializedName(node) => node.location(),
            PropertyDefinition::PropertyNameAssignmentExpression(name, exp) => name.location().merge(&exp.location()),
            PropertyDefinition::MethodDefinition(node) => node.location(),
            PropertyDefinition::AssignmentExpression(_, location) => *location,
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            PropertyDefinition::IdentifierReference(_) => false,
            PropertyDefinition::CoverInitializedName(cin) => cin.contains(kind),
            PropertyDefinition::PropertyNameAssignmentExpression(pn, ae) => pn.contains(kind) || ae.contains(kind),
            PropertyDefinition::MethodDefinition(md) => {
                kind == ParseNodeKind::MethodDefinition || md.computed_property_contains(kind)
            }
            PropertyDefinition::AssignmentExpression(ae, _) => ae.contains(kind),
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
            PropertyDefinition::IdentifierReference(_) => true,
            PropertyDefinition::CoverInitializedName(cin) => cin.all_private_identifiers_valid(names),
            PropertyDefinition::PropertyNameAssignmentExpression(pn, ae) => {
                pn.all_private_identifiers_valid(names) && ae.all_private_identifiers_valid(names)
            }
            PropertyDefinition::MethodDefinition(md) => md.all_private_identifiers_valid(names),
            PropertyDefinition::AssignmentExpression(ae, _) => ae.all_private_identifiers_valid(names),
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
            PropertyDefinition::IdentifierReference(ir) => ir.contains_arguments(),
            PropertyDefinition::CoverInitializedName(_) => false, // This triggers a syntax error elsewhere; so ignore it now
            PropertyDefinition::PropertyNameAssignmentExpression(pn, ae) => {
                pn.contains_arguments() || ae.contains_arguments()
            }
            PropertyDefinition::MethodDefinition(md) => md.contains_arguments(),
            PropertyDefinition::AssignmentExpression(ae, _) => ae.contains_arguments(),
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        match self {
            PropertyDefinition::IdentifierReference(idref) => idref.early_errors(errs, strict),
            PropertyDefinition::PropertyNameAssignmentExpression(pn, ae) => {
                pn.early_errors(errs, strict);
                ae.early_errors(errs, strict);
            }
            PropertyDefinition::AssignmentExpression(ae, _) => ae.early_errors(errs, strict),
            PropertyDefinition::MethodDefinition(md) => {
                // PropertyDefinition : MethodDefinition
                //  * It is a Syntax Error if HasDirectSuper of MethodDefinition is true.
                //  * It is a Syntax Error if PrivateBoundIdentifiers of MethodDefinition is not empty.
                if md.has_direct_super() {
                    // E.g.: x = { b() { super(); } };
                    errs.push(create_syntax_error_object("'super' keyword unexpected here", Some(md.location())));
                }
                if md.private_bound_identifier().is_some() {
                    // E.g.: x = { #b() {} };
                    errs.push(create_syntax_error_object("Private identifier unexpected here", Some(md.location())));
                }
                md.early_errors(errs, strict);
            }
            PropertyDefinition::CoverInitializedName(cin) => {
                // In addition to describing an actual object initializer, the ObjectLiteral productions are also used
                // as a cover grammar for ObjectAssignmentPattern and may be recognized as part of a
                // CoverParenthesizedExpressionAndArrowParameterList. When ObjectLiteral appears in a context where
                // ObjectAssignmentPattern is required the following Early Error rules are not applied. In addition,
                // they are not applied when initially parsing a CoverParenthesizedExpressionAndArrowParameterList or
                // CoverCallExpressionAndAsyncArrowHead.
                //
                // PropertyDefinition : CoverInitializedName
                //  * It is a Syntax Error if any source text is matched by this production.
                //
                // NOTE |   This production exists so that ObjectLiteral can serve as a cover grammar for
                //      |   ObjectAssignmentPattern. It cannot occur in an actual object initializer.

                // Programming Note. Since covered expressions always wind up getting uncovered before early errors are
                // checked, if we _actually_ get here, this really is an error.
                errs.push(create_syntax_error_object(
                    "Illegal destructuring syntax in non-destructuring context",
                    Some(cin.location()),
                ));
                cin.early_errors(errs, strict);
            }
        }
    }

    //pub(crate) fn prop_name(&self) -> Option<JSString> {
    //    // Static Semantics: PropName
    //    // The syntax-directed operation PropName takes no arguments and returns a String or empty.
    //    match self {
    //        PropertyDefinition::IdentifierReference(id) => {
    //            // PropertyDefinition : IdentifierReference
    //            //  1. Return StringValue of IdentifierReference.
    //            Some(id.string_value())
    //        }
    //        PropertyDefinition::AssignmentExpression(..) => {
    //            // PropertyDefinition : ... AssignmentExpression
    //            //  1. Return empty.
    //            None
    //        }
    //        PropertyDefinition::PropertyNameAssignmentExpression(pn, _) => {
    //            // PropertyDefinition : PropertyName : AssignmentExpression
    //            //  1. Return PropName of PropertyName.
    //            pn.prop_name()
    //        }
    //        PropertyDefinition::CoverInitializedName(cin) => Some(cin.prop_name()),
    //        PropertyDefinition::MethodDefinition(md) => md.prop_name(),
    //    }
    //}

    pub(crate) fn special_proto_count(&self) -> u64 {
        match self {
            PropertyDefinition::PropertyNameAssignmentExpression(pn, _) => match pn.prop_name() {
                Some(x) if x == "__proto__" => 1,
                _ => 0,
            },
            _ => 0,
        }
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if !self.location().contains(location) {
            return None;
        }
        match self {
            PropertyDefinition::IdentifierReference(_) => None,
            PropertyDefinition::CoverInitializedName(cin) => cin.body_containing_location(location),
            PropertyDefinition::PropertyNameAssignmentExpression(pn, ae) => {
                pn.body_containing_location(location).or_else(|| ae.body_containing_location(location))
            }
            PropertyDefinition::MethodDefinition(md) => md.body_containing_location(location),
            PropertyDefinition::AssignmentExpression(ae, _) => ae.body_containing_location(location),
        }
    }
}

// PropertyDefinitionList[Yield, Await] :
//      PropertyDefinition[?Yield, ?Await]
//      PropertyDefinitionList[?Yield, ?Await] , PropertyDefinition[?Yield, ?Await]
#[derive(Debug)]
pub(crate) enum PropertyDefinitionList {
    OneDef(Rc<PropertyDefinition>),
    ManyDefs(Rc<PropertyDefinitionList>, Rc<PropertyDefinition>),
}

impl fmt::Display for PropertyDefinitionList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PropertyDefinitionList::OneDef(pd) => write!(f, "{pd}"),
            PropertyDefinitionList::ManyDefs(pdl, pd) => {
                write!(f, "{pdl} , {pd}")
            }
        }
    }
}

impl PrettyPrint for PropertyDefinitionList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}PropertyDefinitionList: {self}")?;
        match self {
            PropertyDefinitionList::OneDef(pd) => pd.pprint_with_leftpad(writer, &successive, Spot::Final),
            PropertyDefinitionList::ManyDefs(pdl, pd) => {
                pdl.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pd.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            PropertyDefinitionList::OneDef(node) => node.concise_with_leftpad(writer, pad, state),
            PropertyDefinitionList::ManyDefs(pdl, pd) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{first}PropertyDefinitionList: {self}")?;
                pdl.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pd.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl PropertyDefinitionList {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (pd, after_pd) = PropertyDefinition::parse(parser, scanner, yield_flag, await_flag)?;
        let mut current_production = Rc::new(PropertyDefinitionList::OneDef(pd));
        let mut current_scanner = after_pd;
        while let Ok((pd2, after_pd2)) =
            scan_for_punct(current_scanner, parser.source, InputElementGoal::Div, Punctuator::Comma)
                .and_then(|(_, after_comma)| PropertyDefinition::parse(parser, after_comma, yield_flag, await_flag))
        {
            current_production = Rc::new(PropertyDefinitionList::ManyDefs(current_production, pd2));
            current_scanner = after_pd2;
        }
        Ok((current_production, current_scanner))
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            PropertyDefinitionList::OneDef(node) => node.location(),
            PropertyDefinitionList::ManyDefs(lst, item) => lst.location().merge(&item.location()),
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            PropertyDefinitionList::OneDef(pd) => pd.contains(kind),
            PropertyDefinitionList::ManyDefs(pdl, pd) => pdl.contains(kind) || pd.contains(kind),
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
            PropertyDefinitionList::OneDef(pd) => pd.all_private_identifiers_valid(names),
            PropertyDefinitionList::ManyDefs(pdl, pd) => {
                pdl.all_private_identifiers_valid(names) && pd.all_private_identifiers_valid(names)
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
            PropertyDefinitionList::OneDef(pd) => pd.contains_arguments(),
            PropertyDefinitionList::ManyDefs(pdl, pd) => pdl.contains_arguments() || pd.contains_arguments(),
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        match self {
            PropertyDefinitionList::OneDef(pd) => pd.early_errors(errs, strict),
            PropertyDefinitionList::ManyDefs(pdl, pd) => {
                pdl.early_errors(errs, strict);
                pd.early_errors(errs, strict);
            }
        }
    }

    pub(crate) fn special_proto_count(&self) -> u64 {
        match self {
            PropertyDefinitionList::OneDef(pd) => pd.special_proto_count(),
            PropertyDefinitionList::ManyDefs(pdl, pd) => pdl.special_proto_count() + pd.special_proto_count(),
        }
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                PropertyDefinitionList::OneDef(pd) => pd.body_containing_location(location),
                PropertyDefinitionList::ManyDefs(pdl, pd) => {
                    pdl.body_containing_location(location).or_else(|| pd.body_containing_location(location))
                }
            }
        } else {
            None
        }
    }
}

// ObjectLiteral[Yield, Await] :
//      { }
//      { PropertyDefinitionList[?Yield, ?Await] }
//      { PropertyDefinitionList[?Yield, ?Await] , }
#[derive(Debug)]
pub(crate) enum ObjectLiteral {
    Empty { location: Location },
    Normal { pdl: Rc<PropertyDefinitionList>, location: Location },
    TrailingComma { pdl: Rc<PropertyDefinitionList>, location: Location },
}

impl fmt::Display for ObjectLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ObjectLiteral::Empty { .. } => write!(f, "{{ }}"),
            ObjectLiteral::Normal { pdl, .. } => write!(f, "{{ {pdl} }}"),
            ObjectLiteral::TrailingComma { pdl, .. } => write!(f, "{{ {pdl} , }}"),
        }
    }
}

impl PrettyPrint for ObjectLiteral {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ObjectLiteral: {self}")?;
        match self {
            ObjectLiteral::Empty { .. } => Ok(()),
            ObjectLiteral::Normal { pdl, .. } | ObjectLiteral::TrailingComma { pdl, .. } => {
                pdl.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ObjectLiteral: {self}")?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match self {
            ObjectLiteral::Empty { .. } => {}
            ObjectLiteral::Normal { pdl: node, .. } => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ObjectLiteral::TrailingComma { pdl: node, .. } => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
            }
        }
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ObjectLiteral {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (open_loc, after_brace) =
            scan_for_punct(scanner, parser.source, InputElementGoal::RegExp, Punctuator::LeftBrace)?;
        match PropertyDefinitionList::parse(parser, after_brace, yield_flag, await_flag) {
            Err(_) => {
                let (close_loc, after_brace2) =
                    scan_for_punct(after_brace, parser.source, InputElementGoal::Div, Punctuator::RightBrace)?;
                Ok((Rc::new(ObjectLiteral::Empty { location: open_loc.merge(&close_loc) }), after_brace2))
            }
            Ok((pdl, after_pdl)) => {
                let (comma_or_brace, punct_loc, after_punct) = scan_for_punct_set(
                    after_pdl,
                    parser.source,
                    InputElementGoal::Div,
                    &[Punctuator::RightBrace, Punctuator::Comma],
                )?;
                match comma_or_brace {
                    Punctuator::RightBrace => {
                        Ok((Rc::new(ObjectLiteral::Normal { pdl, location: open_loc.merge(&punct_loc) }), after_punct))
                    }
                    _ => {
                        let (close_loc, after_brace3) =
                            scan_for_punct(after_punct, parser.source, InputElementGoal::Div, Punctuator::RightBrace)?;
                        Ok((
                            Rc::new(ObjectLiteral::TrailingComma { pdl, location: open_loc.merge(&close_loc) }),
                            after_brace3,
                        ))
                    }
                }
            }
        }
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            ObjectLiteral::Empty { location }
            | ObjectLiteral::Normal { location, .. }
            | ObjectLiteral::TrailingComma { location, .. } => *location,
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ObjectLiteral::Empty { .. } => false,
            ObjectLiteral::Normal { pdl, .. } | ObjectLiteral::TrailingComma { pdl, .. } => pdl.contains(kind),
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
            ObjectLiteral::Empty { .. } => true,
            ObjectLiteral::Normal { pdl, .. } | ObjectLiteral::TrailingComma { pdl, .. } => {
                pdl.all_private_identifiers_valid(names)
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
            ObjectLiteral::Empty { .. } => false,
            ObjectLiteral::Normal { pdl, .. } | ObjectLiteral::TrailingComma { pdl, .. } => pdl.contains_arguments(),
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        match self {
            ObjectLiteral::Empty { .. } => {}
            ObjectLiteral::Normal { pdl, location } | ObjectLiteral::TrailingComma { pdl, location } => {
                // ObjectLiteral :
                //      { PropertyDefinitionList }
                //      { PropertyDefinitionList , }
                //  * It is a Syntax Error if PropertyNameList of PropertyDefinitionList contains any duplicate entries
                //    for "__proto__" and at least two of those entries were obtained from productions of the form
                //    PropertyDefinition : PropertyName : AssignmentExpression . This rule is not applied if this
                //    ObjectLiteral is contained within a Script that is being parsed for JSON.parse (see step 4 of
                //    JSON.parse).
                //
                // NOTE |   The List returned by PropertyNameList does not include property names defined using a
                //          ComputedPropertyName.
                if pdl.special_proto_count() >= 2 {
                    errs.push(create_syntax_error_object(
                        "Duplicate __proto__ fields are not allowed in object literals",
                        Some(*location),
                    ));
                }
                pdl.early_errors(errs, strict);
            }
        }
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                ObjectLiteral::Empty { .. } => None,
                ObjectLiteral::Normal { pdl, .. } | ObjectLiteral::TrailingComma { pdl, .. } => {
                    pdl.body_containing_location(location)
                }
            }
        } else {
            None
        }
    }
}

//////// 12.2.4 Literals
// Literal :
//      NullLiteral
//      BooleanLiteral
//      NumericLiteral
//      StringLiteral

impl fmt::Display for Numeric {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Numeric::Number(n) => {
                let mut s = Vec::new();
                number_to_string(&mut s, *n).unwrap(); // writing to a Vec never errors
                let printable = String::from_utf8(s).unwrap(); // the utf-8 will always be valid
                printable.fmt(f)
            }
            Numeric::BigInt(b) => b.fmt(f),
        }
    }
}

impl Numeric {
    #[expect(clippy::unused_self)]
    fn has_legacy_octal_syntax(&self) -> bool {
        // Need to actually implement legacy octal before this makes any sense at all
        false
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub(crate) enum DebugKind {
    Char(char),
    Number(i64),
}

impl fmt::Display for DebugKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DebugKind::Char(c) => c.fmt(f),
            DebugKind::Number(n) => write!(f, "({n})"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Literal {
    Null { location: Location },
    Boolean { val: bool, location: Location },
    Numeric { val: Numeric, location: Location },
    String { val: StringToken, location: Location },
    Debug { val: DebugKind, location: Location },
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Null { .. } => write!(f, "null"),
            Literal::Boolean { val: b, .. } => {
                if *b {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                }
            }
            Literal::Numeric { val: Numeric::Number(n), .. } => {
                let mut s = Vec::new();
                number_to_string(&mut s, *n).unwrap();
                write!(f, "{}", String::from_utf8(s).unwrap())
            }
            Literal::Numeric { val: Numeric::BigInt(b), .. } => write!(f, "{}", *b),
            Literal::String { val: s, .. } => write!(f, "{}", *s),
            Literal::Debug { val: ch, .. } => write!(f, "@@{ch}"),
        }
    }
}

impl PrettyPrint for Literal {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{first}Literal: {self}")
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            Literal::Null { .. } => pprint_token(writer, "null", TokenType::Keyword, pad, state),
            Literal::Boolean { .. } => pprint_token(writer, self, TokenType::Keyword, pad, state),
            Literal::Numeric { .. } => pprint_token(writer, self, TokenType::Numeric, pad, state),
            Literal::String { .. } => pprint_token(writer, self, TokenType::String, pad, state),
            Literal::Debug { .. } => pprint_token(writer, self, TokenType::Punctuator, pad, state),
        }
    }
}

impl Literal {
    pub(crate) fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Literal> {
        let (token, tok_loc, newscanner) = scan_token(&scanner, parser.source, InputElementGoal::RegExp);
        match token {
            Token::Identifier(id) if id.matches(Keyword::Null) => {
                Ok((Rc::new(Literal::Null { location: tok_loc }), newscanner))
            }
            Token::Identifier(id) if id.matches(Keyword::True) => {
                Ok((Rc::new(Literal::Boolean { val: true, location: tok_loc }), newscanner))
            }
            Token::Identifier(id) if id.matches(Keyword::False) => {
                Ok((Rc::new(Literal::Boolean { val: false, location: tok_loc }), newscanner))
            }
            Token::Number(num) => {
                Ok((Rc::new(Literal::Numeric { val: Numeric::Number(num), location: tok_loc }), newscanner))
            }
            Token::BigInt(bi) => {
                Ok((Rc::new(Literal::Numeric { val: Numeric::BigInt(Rc::new(bi)), location: tok_loc }), newscanner))
            }
            Token::String(s) => Ok((Rc::new(Literal::String { val: s, location: tok_loc }), newscanner)),
            Token::Debug(ch) => Ok((Rc::new(Literal::Debug { val: ch, location: tok_loc }), newscanner)),
            _ => Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::Literal), tok_loc)),
        }
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            Literal::Null { location }
            | Literal::Boolean { location, .. }
            | Literal::Numeric { location, .. }
            | Literal::String { location, .. }
            | Literal::Debug { location, .. } => *location,
        }
    }

    #[expect(clippy::unused_self)]
    pub(crate) fn contains(&self, _: ParseNodeKind) -> bool {
        false
    }

    pub(crate) fn as_string_literal(&self) -> Option<StringToken> {
        if let Literal::String { val: s, .. } = self { Some(s.clone()) } else { None }
    }

    pub(crate) fn early_errors(&self) {
        // Since we don't implement Legacy Octal syntax (yet), these two errors are never generated. That makes this
        // function impossible to test. I hate untestable code. So here's what's gonna happen: we just make some
        // assertions that are supposed to fail once we do actually implement legacy octal. That will be my reminder to
        // uncomment the rest of this function.
        match self {
            Literal::Numeric { val: n, .. } => {
                assert!(!n.has_legacy_octal_syntax());
            }
            Literal::String { val: s, .. } => {
                assert!(!s.has_legacy_octal_escapes());
            }
            Literal::Boolean { .. } | Literal::Null { .. } | Literal::Debug { .. } => (),
        }

        //match &self.kind {
        //    Literal::BooleanLiteral(..) | Literal::NullLiteral => {},
        //    Literal::StringLiteral(s) => {
        //        if strict && s.has_legacy_octal_escapes() {
        //            errs.push(create_syntax_error_object("Legacy octal escapes not allowed in strict mode"));
        //        }
        //    }
        //    Literal::NumericLiteral(n) => {
        //        if strict && n.has_legacy_octal_syntax() {
        //            errs.push(create_syntax_error_object("Legacy octal syntax not allowed in strict mode"));
        //        }
        //    }
        //}
    }

    //pub(crate) fn body_containing_location(&self, _location: &Location) -> Option<ContainingBody> {
    //    // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
    //    None
    //}
}

// TemplateLiteral[Yield, Await, Tagged] :
//      NoSubstitutionTemplate
//      SubstitutionTemplate[?Yield, ?Await, ?Tagged]
#[derive(Debug)]
pub(crate) enum TemplateLiteral {
    NoSubstitutionTemplate { data: TemplateData, tagged: bool, location: Location },
    SubstitutionTemplate(Rc<SubstitutionTemplate>),
}

impl fmt::Display for TemplateLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TemplateLiteral::NoSubstitutionTemplate { data: td, .. } => write!(f, "`{td}`"),
            TemplateLiteral::SubstitutionTemplate(boxed) => write!(f, "{boxed}"),
        }
    }
}

impl PrettyPrint for TemplateLiteral {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}TemplateLiteral: {self}")?;
        match self {
            TemplateLiteral::NoSubstitutionTemplate { .. } => Ok(()),
            TemplateLiteral::SubstitutionTemplate(st) => st.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            TemplateLiteral::NoSubstitutionTemplate { .. } => {
                pprint_token(writer, self, TokenType::NoSubTemplate, pad, state)
            }
            TemplateLiteral::SubstitutionTemplate(st) => st.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl TemplateLiteral {
    fn parse_nst(parser: &mut Parser, scanner: Scanner, tagged_flag: bool) -> ParseResult<Self> {
        let (tok, tok_loc, after_nst) = scan_token(&scanner, parser.source, InputElementGoal::RegExp);
        if let Token::NoSubstitutionTemplate(td) = tok {
            Ok((
                Rc::new(TemplateLiteral::NoSubstitutionTemplate { data: td, tagged: tagged_flag, location: tok_loc }),
                after_nst,
            ))
        } else {
            Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::NoSubstitutionTemplate), tok_loc))
        }
    }

    fn parse_subst(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        tagged_flag: bool,
    ) -> ParseResult<Self> {
        let (node, after) = SubstitutionTemplate::parse(parser, scanner, yield_flag, await_flag, tagged_flag)?;
        Ok((Rc::new(TemplateLiteral::SubstitutionTemplate(node)), after))
    }

    fn parse_core(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        tagged_flag: bool,
    ) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::TemplateLiteral), scanner))
            .otherwise(|| Self::parse_nst(parser, scanner, tagged_flag))
            .otherwise(|| Self::parse_subst(parser, scanner, yield_flag, await_flag, tagged_flag))
    }

    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        tagged_flag: bool,
    ) -> ParseResult<Self> {
        let key = YieldAwaitTaggedKey { scanner, yield_flag, await_flag, tagged_flag };
        match parser.template_literal_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag, tagged_flag);
                parser.template_literal_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            TemplateLiteral::NoSubstitutionTemplate { location, .. } => *location,
            TemplateLiteral::SubstitutionTemplate(st) => st.location(),
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            TemplateLiteral::SubstitutionTemplate(boxed) => boxed.contains(kind),
            TemplateLiteral::NoSubstitutionTemplate { .. } => false,
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
            TemplateLiteral::NoSubstitutionTemplate { .. } => true,
            TemplateLiteral::SubstitutionTemplate(boxed) => boxed.all_private_identifiers_valid(names),
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
            TemplateLiteral::NoSubstitutionTemplate { .. } => false,
            TemplateLiteral::SubstitutionTemplate(st) => st.contains_arguments(),
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool, ts_limit: usize) {
        // Static Semantics: Early Errors
        match self {
            TemplateLiteral::NoSubstitutionTemplate { data: td, tagged, location } => {
                // TemplateLiteral : NoSubstitutionTemplate
                //  * It is a Syntax Error if the [Tagged] parameter was not set and
                //    NoSubstitutionTemplate Contains NotEscapeSequence.
                if !tagged && td.tv.is_none() {
                    errs.push(create_syntax_error_object(
                        "Invalid escape sequence in template literal",
                        Some(*location),
                    ));
                }
            }
            TemplateLiteral::SubstitutionTemplate(st) => {
                // TemplateLiteral : SubstitutionTemplate
                //  * It is a Syntax Error if the number of elements in the result of
                //    TemplateStrings of TemplateLiteral with argument false is greater
                //    than 2^32 - 1.
                if self.template_strings(false).len() > ts_limit {
                    errs.push(create_syntax_error_object("Template literal too complex", Some(st.location())));
                }
                st.early_errors(errs, strict);
            }
        }
    }

    pub(crate) fn template_strings(&self, raw: bool) -> Vec<Option<JSString>> {
        // Static Semantics: TemplateStrings
        //
        // The syntax-directed operation TemplateStrings takes argument raw and returns a List of Strings. It is
        // defined piecewise over the following productions:
        match self {
            TemplateLiteral::NoSubstitutionTemplate { data: nst, .. } => {
                // TemplateLiteral : NoSubstitutionTemplate
                //  1. If raw is false, then
                //      a. Let string be the TV of NoSubstitutionTemplate.
                //  2. Else,
                //      a. Let string be the TRV of NoSubstitutionTemplate.
                //  3. Return « string ».
                if raw { vec![Some(nst.trv.clone())] } else { vec![nst.tv.clone()] }
            }
            TemplateLiteral::SubstitutionTemplate(st) => {
                // TemplateLiteral : SubstitutionTemplate
                //  1. Return TemplateStrings of SubstitutionTemplate with argument raw.
                st.template_strings(raw)
            }
        }
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                TemplateLiteral::NoSubstitutionTemplate { .. } => None,
                TemplateLiteral::SubstitutionTemplate(st) => st.body_containing_location(location),
            }
        } else {
            None
        }
    }
}

// SubstitutionTemplate[Yield, Await, Tagged] :
//      TemplateHead Expression[+In, ?Yield, ?Await] TemplateSpans[?Yield, ?Await, ?Tagged]
#[derive(Debug)]
pub(crate) struct SubstitutionTemplate {
    pub(crate) template_head: TemplateData,
    tagged: bool,
    pub(crate) expression: Rc<Expression>,
    pub(crate) template_spans: Rc<TemplateSpans>,
    location: Location,
}

impl fmt::Display for SubstitutionTemplate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "`{}${{ {} {}", self.template_head, self.expression, self.template_spans)
    }
}

impl PrettyPrint for SubstitutionTemplate {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}SubstitutionTemplate: {self}")?;
        self.expression.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.template_spans.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}SubstitutionTemplate: {self}")?;
        pprint_token(
            writer,
            format!("`{}${{", self.template_head),
            TokenType::TemplateHead,
            &successive,
            Spot::NotFinal,
        )?;
        self.expression.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.template_spans.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl SubstitutionTemplate {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        tagged_flag: bool,
    ) -> ParseResult<Self> {
        let (head, tok_loc, after_head) = scan_token(&scanner, parser.source, InputElementGoal::RegExp);
        if let Token::TemplateHead(td) = head {
            let (exp_boxed, after_exp) = Expression::parse(parser, after_head, true, yield_flag, await_flag)?;
            let (spans_boxed, after_spans) =
                TemplateSpans::parse(parser, after_exp, yield_flag, await_flag, tagged_flag)?;
            Ok((
                Rc::new({
                    let location = tok_loc.merge(&spans_boxed.location());
                    SubstitutionTemplate {
                        template_head: td,
                        tagged: tagged_flag,
                        expression: exp_boxed,
                        template_spans: spans_boxed,
                        location,
                    }
                }),
                after_spans,
            ))
        } else {
            Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::SubstitutionTemplate), tok_loc))
        }
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        self.expression.contains(kind) || self.template_spans.contains(kind)
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.expression.all_private_identifiers_valid(names) && self.template_spans.all_private_identifiers_valid(names)
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
        self.expression.contains_arguments() || self.template_spans.contains_arguments()
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        // SubstitutionTemplate : TemplateHead Expression TemplateSpans
        //  * It is a Syntax Error if the [Tagged] parameter was not set and TemplateHead Contains NotEscapeSequence.
        if !self.tagged && self.template_head.tv.is_none() {
            errs.push(create_syntax_error_object("Invalid escape sequence in template literal", Some(self.location)));
        }
        self.expression.early_errors(errs, strict);
        self.template_spans.early_errors(errs, strict);
    }

    pub(crate) fn template_strings(&self, raw: bool) -> Vec<Option<JSString>> {
        // Static Semantics: TemplateStrings
        //
        // The syntax-directed operation TemplateStrings takes argument raw and returns a List of Strings. It is
        // defined piecewise over the following productions:

        // SubstitutionTemplate : TemplateHead Expression TemplateSpans
        //  1. If raw is false, then
        //      a. Let head be the TV of TemplateHead.
        //  2. Else,
        //      a. Let head be the TRV of TemplateHead.
        //  3. Let tail be TemplateStrings of TemplateSpans with argument raw.
        //  4. Return the list-concatenation of « head » and tail.
        let mut head =
            if raw { vec![Some(self.template_head.trv.clone())] } else { vec![self.template_head.tv.clone()] };
        let tail = self.template_spans.template_strings(raw);
        head.extend(tail);
        head
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            self.expression
                .body_containing_location(location)
                .or_else(|| self.template_spans.body_containing_location(location))
        } else {
            None
        }
    }
}

// TemplateSpans[Yield, Await, Tagged] :
//      TemplateTail
//      TemplateMiddleList[?Yield, ?Await, ?Tagged] TemplateTail
#[derive(Debug)]
pub(crate) enum TemplateSpans {
    Tail { data: TemplateData, tagged: bool, location: Location },
    List { tml: Rc<TemplateMiddleList>, data: TemplateData, tagged: bool, location: Location },
}

impl fmt::Display for TemplateSpans {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TemplateSpans::Tail { data: td, .. } => {
                write!(f, "}}{}`", format!("{}", td.trv).replace(char::is_control, "\u{2426}"))
            }
            TemplateSpans::List { tml, data: td, .. } => {
                write!(f, "{} }}{}`", tml, format!("{}", td.trv).replace(char::is_control, "\u{2426}"))
            }
        }
    }
}

impl PrettyPrint for TemplateSpans {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}TemplateSpans: {self}")?;
        match self {
            TemplateSpans::Tail { .. } => Ok(()),
            TemplateSpans::List { tml, .. } => tml.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            TemplateSpans::Tail { data: td, .. } => {
                pprint_token(writer, format!("}}{}`", td.trv), TokenType::TemplateTail, pad, state)
            }
            TemplateSpans::List { tml, data: td, .. } => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{first}TemplateSpans: {self}")?;
                tml.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, format!("}}{td}`"), TokenType::TemplateTail, &successive, Spot::Final)
            }
        }
    }
}

impl TemplateSpans {
    fn parse_tail(parser: &mut Parser, scanner: Scanner, tagged_flag: bool) -> ParseResult<Self> {
        let (token, tok_loc, after_tmplt) = scan_token(&scanner, parser.source, InputElementGoal::TemplateTail);
        if let Token::TemplateTail(td) = token {
            Ok((Rc::new(TemplateSpans::Tail { data: td, tagged: tagged_flag, location: tok_loc }), after_tmplt))
        } else {
            Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::TemplateTail), tok_loc))
        }
    }

    fn parse_tml_tail(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        tagged_flag: bool,
    ) -> ParseResult<Self> {
        let (tml, after_tml) = TemplateMiddleList::parse(parser, scanner, yield_flag, await_flag, tagged_flag)?;
        let (token, tok_loc, after_tmplt) = scan_token(&after_tml, parser.source, InputElementGoal::TemplateTail);
        if let Token::TemplateTail(td) = token {
            Ok((
                Rc::new({
                    let location = tml.location().merge(&tok_loc);
                    TemplateSpans::List { tml, data: td, tagged: tagged_flag, location }
                }),
                after_tmplt,
            ))
        } else {
            Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::TemplateTail), tok_loc))
        }
    }
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        tagged_flag: bool,
    ) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::TemplateSpans), scanner))
            .otherwise(|| Self::parse_tail(parser, scanner, tagged_flag))
            .otherwise(|| Self::parse_tml_tail(parser, scanner, yield_flag, await_flag, tagged_flag))
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            TemplateSpans::Tail { location, .. } | TemplateSpans::List { location, .. } => *location,
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            TemplateSpans::Tail { .. } => false,
            TemplateSpans::List { tml, .. } => tml.contains(kind),
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
            TemplateSpans::Tail { .. } => true,
            TemplateSpans::List { tml, .. } => tml.all_private_identifiers_valid(names),
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
            TemplateSpans::Tail { .. } => false,
            TemplateSpans::List { tml, .. } => tml.contains_arguments(),
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        //  TemplateSpans :
        //      TemplateTail
        //      TemplateMiddleList TemplateTail
        //  * It is a Syntax Error if the [Tagged] parameter was not set and TemplateTail Contains NotEscapeSequence.
        if let TemplateSpans::List { tml: lst, .. } = self {
            lst.early_errors(errs, strict);
        }
        match self {
            TemplateSpans::Tail { data: tail, tagged, location }
            | TemplateSpans::List { data: tail, tagged, location, .. } => {
                if !tagged && tail.tv.is_none() {
                    errs.push(create_syntax_error_object(
                        "Invalid character escape in template literal",
                        Some(*location),
                    ));
                }
            }
        }
    }

    pub(crate) fn template_strings(&self, raw: bool) -> Vec<Option<JSString>> {
        // Static Semantics: TemplateStrings
        //
        // The syntax-directed operation TemplateStrings takes argument raw and returns a List of Strings. It is
        // defined piecewise over the following productions:
        match self {
            TemplateSpans::Tail { data: tail, .. } => {
                // TemplateSpans : TemplateTail
                //  1. If raw is false, then
                //      a. Let tail be the TV of TemplateTail.
                //  2. Else,
                //      a. Let tail be the TRV of TemplateTail.
                //  3. Return « tail ».
                if raw { vec![Some(tail.trv.clone())] } else { vec![tail.tv.clone()] }
            }
            TemplateSpans::List { tml: template_middle_list, data: template_tail, .. } => {
                // TemplateSpans : TemplateMiddleList TemplateTail
                //  1. Let middle be TemplateStrings of TemplateMiddleList with argument raw.
                //  2. If raw is false, then
                //      a. Let tail be the TV of TemplateTail.
                //  3. Else,
                //      a. Let tail be the TRV of TemplateTail.
                //  4. Return the list-concatenation of middle and « tail ».
                let mut middle = template_middle_list.template_strings(raw);
                let tail = if raw { Some(template_tail.trv.clone()) } else { template_tail.tv.clone() };
                middle.push(tail);
                middle
            }
        }
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                TemplateSpans::Tail { .. } => None,
                TemplateSpans::List { tml, .. } => tml.body_containing_location(location),
            }
        } else {
            None
        }
    }
}

// TemplateMiddleList[Yield, Await, Tagged] :
//      TemplateMiddle Expression[+In, ?Yield, ?Await]
//      TemplateMiddleList[?Yield, ?Await, ?Tagged] TemplateMiddle Expression[+In, ?Yield, ?Await]
#[derive(Debug)]
pub(crate) enum TemplateMiddleList {
    ListHead { data: TemplateData, exp: Rc<Expression>, tagged: bool, location: Location },
    ListMid(Rc<TemplateMiddleList>, TemplateData, Rc<Expression>, bool),
}

impl fmt::Display for TemplateMiddleList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TemplateMiddleList::ListHead { data: td, exp, .. } => {
                write!(f, "}}{}${{ {}", format!("{}", td.trv).replace(char::is_control, "\u{2426}"), exp)
            }
            TemplateMiddleList::ListMid(tml, td, exp, _) => {
                write!(f, "{} }}{}${{ {}", tml, format!("{}", td.trv).replace(char::is_control, "\u{2426}"), exp)
            }
        }
    }
}

impl PrettyPrint for TemplateMiddleList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}TemplateMiddleList: {self}")?;
        match self {
            TemplateMiddleList::ListHead { exp, .. } => exp.pprint_with_leftpad(writer, &successive, Spot::Final),
            TemplateMiddleList::ListMid(tml, _, exp, _) => {
                tml.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                exp.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}TemplateMiddleList: {self}")?;
        match self {
            TemplateMiddleList::ListHead { data: td, exp, .. } => {
                pprint_token(writer, format!("}}{td}${{"), TokenType::TemplateMiddle, &successive, Spot::NotFinal)?;
                exp.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            TemplateMiddleList::ListMid(tml, td, exp, _) => {
                tml.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, format!("}}{td}${{"), TokenType::TemplateMiddle, &successive, Spot::NotFinal)?;
                exp.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl TemplateMiddleList {
    fn parse_tm_exp_unboxed(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<(TemplateData, Location, Rc<Expression>, Scanner), ParseError> {
        let (middle, tok_loc, after_mid) = scan_token(&scanner, parser.source, InputElementGoal::TemplateTail);
        if let Token::TemplateMiddle(td) = middle {
            let (exp, after_exp) = Expression::parse(parser, after_mid, true, yield_flag, await_flag)?;
            Ok((td, tok_loc, exp, after_exp))
        } else {
            Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::TemplateMiddle), tok_loc))
        }
    }

    fn parse_tm_exp(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        tagged_flag: bool,
    ) -> ParseResult<Self> {
        let (td, tok_loc, exp, after_exp) = Self::parse_tm_exp_unboxed(parser, scanner, yield_flag, await_flag)?;
        Ok((
            Rc::new({
                let location = tok_loc.merge(&exp.location());
                TemplateMiddleList::ListHead { data: td, exp, tagged: tagged_flag, location }
            }),
            after_exp,
        ))
    }

    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        tagged_flag: bool,
    ) -> ParseResult<Self> {
        let (mut current_node, mut current_scanner) =
            Self::parse_tm_exp(parser, scanner, yield_flag, await_flag, tagged_flag)?;
        while let Ok((middle, _, exp, after)) =
            Self::parse_tm_exp_unboxed(parser, current_scanner, yield_flag, await_flag)
        {
            current_node = Rc::new(TemplateMiddleList::ListMid(current_node, middle, exp, tagged_flag));
            current_scanner = after;
        }
        Ok((current_node, current_scanner))
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            TemplateMiddleList::ListHead { location, .. } => *location,
            TemplateMiddleList::ListMid(tml, _, exp, _) => tml.location().merge(&exp.location()),
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            TemplateMiddleList::ListHead { exp, .. } => exp.contains(kind),
            TemplateMiddleList::ListMid(tml, _, exp, _) => tml.contains(kind) || exp.contains(kind),
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
            TemplateMiddleList::ListHead { exp, .. } => exp.all_private_identifiers_valid(names),
            TemplateMiddleList::ListMid(tml, _, exp, _) => {
                tml.all_private_identifiers_valid(names) && exp.all_private_identifiers_valid(names)
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
            TemplateMiddleList::ListHead { exp, .. } => exp.contains_arguments(),
            TemplateMiddleList::ListMid(tml, _, e, _) => tml.contains_arguments() || e.contains_arguments(),
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        //  TemplateMiddleList :
        //      TemplateMiddle Expression
        //      TemplateMiddleList TemplateMiddle Expression
        //  * It is a Syntax Error if the [Tagged] parameter was not set and TemplateMiddle Contains NotEscapeSequence.
        if let TemplateMiddleList::ListMid(lst, _, _, _) = self {
            lst.early_errors(errs, strict);
        }
        match self {
            TemplateMiddleList::ListHead { data: tmid, exp, tagged, .. }
            | TemplateMiddleList::ListMid(_, tmid, exp, tagged) => {
                if !tagged && tmid.tv.is_none() {
                    errs.push(create_syntax_error_object(
                        "Invalid character escape in template literal",
                        Some(self.location()),
                    ));
                }
                exp.early_errors(errs, strict);
            }
        }
    }

    pub(crate) fn template_strings(&self, raw: bool) -> Vec<Option<JSString>> {
        // Static Semantics: TemplateStrings
        //
        // The syntax-directed operation TemplateStrings takes argument raw and returns a List of Strings. It is
        // defined piecewise over the following productions:
        match self {
            TemplateMiddleList::ListHead { data: template_middle, .. } => {
                // TemplateMiddleList : TemplateMiddle Expression
                //  1. If raw is false, then
                //      a. Let string be the TV of TemplateMiddle.
                //  2. Else,
                //      a. Let string be the TRV of TemplateMiddle.
                //  3. Return « string ».
                if raw { vec![Some(template_middle.trv.clone())] } else { vec![template_middle.tv.clone()] }
            }
            TemplateMiddleList::ListMid(template_middle_list, template_middle, _, _) => {
                // TemplateMiddleList : TemplateMiddleList TemplateMiddle Expression
                //  1. Let front be TemplateStrings of TemplateMiddleList with argument raw.
                //  2. If raw is false, then
                //      a. Let last be the TV of TemplateMiddle.
                //  3. Else,
                //      a. Let last be the TRV of TemplateMiddle.
                //  4. Return the list-concatenation of front and « last ».
                let mut front = template_middle_list.template_strings(raw);
                let last = if raw { Some(template_middle.trv.clone()) } else { template_middle.tv.clone() };
                front.push(last);
                front
            }
        }
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                TemplateMiddleList::ListHead { data: _, exp, tagged: _, location: _ } => {
                    exp.body_containing_location(location)
                }
                TemplateMiddleList::ListMid(tml, _, exp, _) => {
                    tml.body_containing_location(location).or_else(|| exp.body_containing_location(location))
                }
            }
        } else {
            None
        }
    }
}

// ParenthesizedExpression[Yield, Await] :
//      ( Expression[+In, ?Yield, ?Await] )
#[derive(Debug)]
pub(crate) struct ParenthesizedExpression {
    pub(crate) exp: Rc<Expression>,
    location: Location,
}

impl fmt::Display for ParenthesizedExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "( {} )", self.exp)
    }
}

impl PrettyPrint for ParenthesizedExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ParenthesizedExpression: {self}")?;
        self.exp.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ParenthesizedExpression: {self}")?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.exp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ParenthesizedExpression {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (open_loc, after_lp) =
            scan_for_punct(scanner, parser.source, InputElementGoal::RegExp, Punctuator::LeftParen)?;
        let (exp, after_exp) = Expression::parse(parser, after_lp, true, yield_flag, await_flag)?;
        let (close_loc, after_rp) =
            scan_for_punct(after_exp, parser.source, InputElementGoal::RegExp, Punctuator::RightParen)?;
        Ok((Rc::new(ParenthesizedExpression { exp, location: open_loc.merge(&close_loc) }), after_rp))
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        self.exp.contains(kind)
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.exp.all_private_identifiers_valid(names)
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
        self.exp.contains_arguments()
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.exp.early_errors(errs, strict);
    }

    pub(crate) fn is_strictly_deletable(&self) -> bool {
        self.exp.is_strictly_deletable()
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub(crate) fn assignment_target_type(&self, strict: bool) -> ATTKind {
        self.exp.assignment_target_type(strict)
    }

    //pub(crate) fn is_named_function(&self) -> bool {
    //    self.exp.is_named_function()
    //}

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) { self.exp.body_containing_location(location) } else { None }
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
        //
        self.exp.has_call_in_tail_position(location)
    }
}

// CoverParenthesizedExpressionAndArrowParameterList[Yield, Await] :
//      ( Expression[+In, ?Yield, ?Await] )
//      ( Expression[+In, ?Yield, ?Await] , )
//      ( )
//      ( ... BindingIdentifier[?Yield, ?Await] )
//      ( ... BindingPattern[?Yield, ?Await] )
//      ( Expression[+In, ?Yield, ?Await] , ... BindingIdentifier[?Yield, ?Await] )
//      ( Expression[+In, ?Yield, ?Await] , ... BindingPattern[?Yield, ?Await] )
#[derive(Debug)]
pub(crate) enum CoverParenthesizedExpressionAndArrowParameterList {
    Expression { exp: Rc<Expression> },
    ExpComma { exp: Rc<Expression> },
    Empty,
    Ident { bi: Rc<BindingIdentifier> },
    Pattern { bp: Rc<BindingPattern> },
    ExpIdent { exp: Rc<Expression>, bi: Rc<BindingIdentifier> },
    ExpPattern { exp: Rc<Expression>, bp: Rc<BindingPattern> },
}

impl fmt::Display for CoverParenthesizedExpressionAndArrowParameterList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CoverParenthesizedExpressionAndArrowParameterList::Expression { exp: node, .. } => {
                write!(f, "( {node} )")
            }
            CoverParenthesizedExpressionAndArrowParameterList::ExpComma { exp: node, .. } => {
                write!(f, "( {node} , )")
            }
            CoverParenthesizedExpressionAndArrowParameterList::Empty => {
                write!(f, "( )")
            }
            CoverParenthesizedExpressionAndArrowParameterList::Ident { bi: node, .. } => {
                write!(f, "( ... {node} )")
            }
            CoverParenthesizedExpressionAndArrowParameterList::Pattern { bp: node, .. } => {
                write!(f, "( ... {node} )")
            }
            CoverParenthesizedExpressionAndArrowParameterList::ExpIdent { exp, bi: id, .. } => {
                write!(f, "( {exp} , ... {id} )")
            }
            CoverParenthesizedExpressionAndArrowParameterList::ExpPattern { exp, bp: pat, .. } => {
                write!(f, "( {exp} , ... {pat} )")
            }
        }
    }
}

impl PrettyPrint for CoverParenthesizedExpressionAndArrowParameterList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}CoverParenthesizedExpressionAndArrowParameterList: {self}")?;
        match self {
            CoverParenthesizedExpressionAndArrowParameterList::Empty => Ok(()),
            CoverParenthesizedExpressionAndArrowParameterList::Expression { exp: node, .. }
            | CoverParenthesizedExpressionAndArrowParameterList::ExpComma { exp: node, .. } => {
                node.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CoverParenthesizedExpressionAndArrowParameterList::Ident { bi: node, .. } => {
                node.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CoverParenthesizedExpressionAndArrowParameterList::Pattern { bp: node, .. } => {
                node.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CoverParenthesizedExpressionAndArrowParameterList::ExpIdent { exp, bi: next, .. } => {
                exp.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                next.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CoverParenthesizedExpressionAndArrowParameterList::ExpPattern { exp, bp: next, .. } => {
                exp.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                next.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}CoverParenthesizedExpressionAndArrowParameterList: {self}")?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match self {
            CoverParenthesizedExpressionAndArrowParameterList::Expression { exp: node, .. } => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            CoverParenthesizedExpressionAndArrowParameterList::ExpComma { exp: node, .. } => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
            }
            CoverParenthesizedExpressionAndArrowParameterList::Empty => {}
            CoverParenthesizedExpressionAndArrowParameterList::Ident { bi: node, .. } => {
                pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            CoverParenthesizedExpressionAndArrowParameterList::Pattern { bp: node, .. } => {
                pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            CoverParenthesizedExpressionAndArrowParameterList::ExpIdent { exp, bi: id, .. } => {
                exp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                id.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            CoverParenthesizedExpressionAndArrowParameterList::ExpPattern { exp, bp: pat, .. } => {
                exp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pat.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
        }
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl CoverParenthesizedExpressionAndArrowParameterList {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        enum BndType {
            Id(Rc<BindingIdentifier>),
            Pat(Rc<BindingPattern>),
        }
        let (_, after_lparen) =
            scan_for_punct(scanner, parser.source, InputElementGoal::RegExp, Punctuator::LeftParen)?;
        Err(ParseError::new(PECode::ExpressionSpreadOrRPExpected, after_lparen))
            .otherwise(|| {
                // ( )
                let (_, after_rparen) =
                    scan_for_punct(after_lparen, parser.source, InputElementGoal::RegExp, Punctuator::RightParen)?;
                Ok((Rc::new(CoverParenthesizedExpressionAndArrowParameterList::Empty), after_rparen))
            })
            .otherwise(|| {
                // ( ... BindingIdentifier )
                // ( ... BindingPattern )
                let (_, after_ellipsis) =
                    scan_for_punct(after_lparen, parser.source, InputElementGoal::RegExp, Punctuator::Ellipsis)?;
                Err(ParseError::new(PECode::BindingIdOrPatternExpected, after_ellipsis)).otherwise(|| {
                    BindingIdentifier::parse(parser, after_ellipsis, yield_flag, await_flag)
                        .map(|(bi, scan)| (BndType::Id(bi), scan))
                        .otherwise(|| {
                            BindingPattern::parse(parser, after_ellipsis, yield_flag, await_flag)
                                .map(|(bp, scan)| (BndType::Pat(bp), scan))
                        })
                        .and_then(|(bnd, scan)| {
                            scan_for_punct(scan, parser.source, InputElementGoal::Div, Punctuator::RightParen)
                                .map(|after_rp| (bnd, after_rp))
                        })
                        .map(|(bnd, (_, scan))| {
                            (
                                Rc::new(match bnd {
                                    BndType::Id(id) => {
                                        CoverParenthesizedExpressionAndArrowParameterList::Ident { bi: id }
                                    }
                                    BndType::Pat(pat) => {
                                        CoverParenthesizedExpressionAndArrowParameterList::Pattern { bp: pat }
                                    }
                                }),
                                scan,
                            )
                        })
                })
            })
            .otherwise(|| {
                enum AfterExp {
                    Empty,
                    Comma,
                    SpreadId(Rc<BindingIdentifier>),
                    SpreadPat(Rc<BindingPattern>),
                }
                let (exp, after_exp) = Expression::parse(parser, after_lparen, true, yield_flag, await_flag)?;
                scan_for_punct(after_exp, parser.source, InputElementGoal::Div, Punctuator::RightParen)
                    .map(|(close_loc, after_rparen)| (AfterExp::Empty, close_loc, after_rparen))
                    .otherwise(|| {
                        scan_for_punct(after_exp, parser.source, InputElementGoal::Div, Punctuator::Comma).and_then(
                            |(_, after_comma)| {
                                scan_for_punct(
                                    after_comma,
                                    parser.source,
                                    InputElementGoal::Div,
                                    Punctuator::RightParen,
                                )
                                .map(|(close_loc, after_rparen)| (AfterExp::Comma, close_loc, after_rparen))
                                .otherwise(|| {
                                    scan_for_punct(
                                        after_comma,
                                        parser.source,
                                        InputElementGoal::Div,
                                        Punctuator::Ellipsis,
                                    )
                                    .and_then(|(_, after_ellipsis)| {
                                        BindingIdentifier::parse(parser, after_ellipsis, yield_flag, await_flag)
                                            .and_then(|(bi, after)| {
                                                scan_for_punct(
                                                    after,
                                                    parser.source,
                                                    InputElementGoal::Div,
                                                    Punctuator::RightParen,
                                                )
                                                .map(
                                                    |(close_loc, after_rp)| {
                                                        (AfterExp::SpreadId(bi), close_loc, after_rp)
                                                    },
                                                )
                                            })
                                            .otherwise(|| {
                                                BindingPattern::parse(parser, after_ellipsis, yield_flag, await_flag)
                                                    .and_then(|(bp, after)| {
                                                        scan_for_punct(
                                                            after,
                                                            parser.source,
                                                            InputElementGoal::Div,
                                                            Punctuator::RightParen,
                                                        )
                                                        .map(
                                                            |(close_loc, after_rp)| {
                                                                (AfterExp::SpreadPat(bp), close_loc, after_rp)
                                                            },
                                                        )
                                                    })
                                            })
                                    })
                                })
                            },
                        )
                    })
                    .map(|(aftexp, _, scan)| {
                        (
                            Rc::new(match aftexp {
                                AfterExp::Empty => {
                                    CoverParenthesizedExpressionAndArrowParameterList::Expression { exp }
                                }
                                AfterExp::Comma => CoverParenthesizedExpressionAndArrowParameterList::ExpComma { exp },
                                AfterExp::SpreadId(id) => {
                                    CoverParenthesizedExpressionAndArrowParameterList::ExpIdent { exp, bi: id }
                                }
                                AfterExp::SpreadPat(pat) => {
                                    CoverParenthesizedExpressionAndArrowParameterList::ExpPattern { exp, bp: pat }
                                }
                            }),
                            scan,
                        )
                    })
            })
    }

    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.cpeaapl_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.cpeaapl_cache.insert(key, result.clone());
                result
            }
        }
    }

    //pub(crate) fn location(&self) -> Location {
    //    match self {
    //        CoverParenthesizedExpressionAndArrowParameterList::Expression { location, .. }
    //        | CoverParenthesizedExpressionAndArrowParameterList::ExpComma { location, .. }
    //        | CoverParenthesizedExpressionAndArrowParameterList::Empty { location, .. }
    //        | CoverParenthesizedExpressionAndArrowParameterList::Ident { location, .. }
    //        | CoverParenthesizedExpressionAndArrowParameterList::Pattern { location, .. }
    //        | CoverParenthesizedExpressionAndArrowParameterList::ExpIdent { location, .. }
    //        | CoverParenthesizedExpressionAndArrowParameterList::ExpPattern { location, .. } => *location,
    //    }
    //}
    //
    //pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
    //    match self {
    //        CoverParenthesizedExpressionAndArrowParameterList::Expression { exp: node, .. }
    //        | CoverParenthesizedExpressionAndArrowParameterList::ExpComma { exp: node, .. } => node.contains(kind),
    //        CoverParenthesizedExpressionAndArrowParameterList::Empty { .. } => false,
    //        CoverParenthesizedExpressionAndArrowParameterList::Ident { bi: node, .. } => node.contains(kind),
    //        CoverParenthesizedExpressionAndArrowParameterList::Pattern { bp: node, .. } => node.contains(kind),
    //        CoverParenthesizedExpressionAndArrowParameterList::ExpIdent { exp, bi: id, .. } => {
    //            exp.contains(kind) || id.contains(kind)
    //        }
    //        CoverParenthesizedExpressionAndArrowParameterList::ExpPattern { exp, bp: pat, .. } => {
    //            exp.contains(kind) || pat.contains(kind)
    //        }
    //    }
    //}
    //
    //pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
    //    match self {
    //        CoverParenthesizedExpressionAndArrowParameterList::Expression { exp: node, .. }
    //        | CoverParenthesizedExpressionAndArrowParameterList::ExpComma { exp: node, .. } => {
    //            node.early_errors(errs, strict);
    //        }
    //        CoverParenthesizedExpressionAndArrowParameterList::Empty { .. } => {}
    //        CoverParenthesizedExpressionAndArrowParameterList::Ident { bi: node, .. } => {
    //            node.early_errors(errs, strict);
    //        }
    //        CoverParenthesizedExpressionAndArrowParameterList::Pattern { bp: node, .. } => {
    //            node.early_errors(errs, strict);
    //        }
    //        CoverParenthesizedExpressionAndArrowParameterList::ExpIdent { exp, bi: id, .. } => {
    //            exp.early_errors(errs, strict);
    //            id.early_errors(errs, strict);
    //        }
    //        CoverParenthesizedExpressionAndArrowParameterList::ExpPattern { exp, bp: pat, .. } => {
    //            exp.early_errors(errs, strict);
    //            pat.early_errors(errs, strict);
    //        }
    //    }
    //}
    //
    //#[expect(unused_variables)]
    //pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
    //    // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
    //    todo!()
    //}
}

#[cfg(test)]
mod tests;
