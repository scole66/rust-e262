use super::assignment_operators::AssignmentExpression;
use super::async_function_definitions::AsyncFunctionExpression;
use super::async_generator_function_definitions::AsyncGeneratorExpression;
use super::class_definitions::ClassExpression;
use super::comma_operator::Expression;
use super::declarations_and_variables::BindingPattern;
use super::function_definitions::FunctionExpression;
use super::generator_function_definitions::GeneratorExpression;
use super::identifiers::{BindingIdentifier, IdentifierReference};
use super::method_definitions::MethodDefinition;
use super::scanner::{scan_token, Keyword, Punctuator, RegularExpressionData, ScanGoal, Scanner, StringToken, TemplateData, Token};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};
use crate::values::number_to_string;
use num::bigint::BigInt;
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
pub enum PrimaryExpressionKind {
    This,
    IdentifierReference(Rc<IdentifierReference>),
    Literal(Rc<Literal>),
    ArrayLiteral(Rc<ArrayLiteral>),
    ObjectLiteral(Rc<ObjectLiteral>),
    Parenthesized(Rc<ParenthesizedExpression>),
    TemplateLiteral(Rc<TemplateLiteral>),
    Function(Rc<FunctionExpression>),
    Class(Rc<ClassExpression>),
    Generator(Rc<GeneratorExpression>),
    AsyncFunction(Rc<AsyncFunctionExpression>),
    AsyncGenerator(Rc<AsyncGeneratorExpression>),
    RegularExpression(RegularExpressionData),
}

#[derive(Debug)]
pub struct PrimaryExpression {
    kind: PrimaryExpressionKind,
}

impl fmt::Display for PrimaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            PrimaryExpressionKind::This => write!(f, "this"),
            PrimaryExpressionKind::IdentifierReference(boxed) => boxed.fmt(f),
            PrimaryExpressionKind::Literal(boxed) => boxed.fmt(f),
            PrimaryExpressionKind::ArrayLiteral(boxed) => boxed.fmt(f),
            PrimaryExpressionKind::ObjectLiteral(boxed) => boxed.fmt(f),
            PrimaryExpressionKind::Parenthesized(boxed) => boxed.fmt(f),
            PrimaryExpressionKind::TemplateLiteral(boxed) => boxed.fmt(f),
            PrimaryExpressionKind::Function(node) => node.fmt(f),
            PrimaryExpressionKind::Class(node) => node.fmt(f),
            PrimaryExpressionKind::Generator(node) => node.fmt(f),
            PrimaryExpressionKind::AsyncFunction(node) => node.fmt(f),
            PrimaryExpressionKind::AsyncGenerator(node) => node.fmt(f),
            PrimaryExpressionKind::RegularExpression(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for PrimaryExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}PrimaryExpression: {}", first, self)?;
        match &self.kind {
            PrimaryExpressionKind::This => Ok(()),
            PrimaryExpressionKind::IdentifierReference(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::Literal(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::ArrayLiteral(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::ObjectLiteral(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::Parenthesized(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::TemplateLiteral(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::Function(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::Class(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::Generator(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::AsyncFunction(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::AsyncGenerator(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::RegularExpression(_) => Ok(()),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match &self.kind {
            PrimaryExpressionKind::This => pprint_token(writer, "this", TokenType::Keyword, pad, state),
            PrimaryExpressionKind::IdentifierReference(boxed) => boxed.concise_with_leftpad(writer, pad, state),
            PrimaryExpressionKind::Literal(boxed) => boxed.concise_with_leftpad(writer, pad, state),
            PrimaryExpressionKind::ArrayLiteral(boxed) => boxed.concise_with_leftpad(writer, pad, state),
            PrimaryExpressionKind::ObjectLiteral(boxed) => boxed.concise_with_leftpad(writer, pad, state),
            PrimaryExpressionKind::Parenthesized(boxed) => boxed.concise_with_leftpad(writer, pad, state),
            PrimaryExpressionKind::TemplateLiteral(boxed) => boxed.concise_with_leftpad(writer, pad, state),
            PrimaryExpressionKind::Function(node) => node.concise_with_leftpad(writer, pad, state),
            PrimaryExpressionKind::Class(node) => node.concise_with_leftpad(writer, pad, state),
            PrimaryExpressionKind::Generator(node) => node.concise_with_leftpad(writer, pad, state),
            PrimaryExpressionKind::AsyncFunction(node) => node.concise_with_leftpad(writer, pad, state),
            PrimaryExpressionKind::AsyncGenerator(node) => node.concise_with_leftpad(writer, pad, state),
            PrimaryExpressionKind::RegularExpression(item) => pprint_token(writer, item, TokenType::RegularExpression, pad, state),
        }
    }
}

impl IsFunctionDefinition for PrimaryExpression {
    fn is_function_definition(&self) -> bool {
        use PrimaryExpressionKind::*;
        match &self.kind {
            This | IdentifierReference(_) | Literal(_) | ArrayLiteral(_) | ObjectLiteral(_) | TemplateLiteral(_) | RegularExpression(_) => false,
            Parenthesized(exp) => exp.is_function_definition(),
            Function(node) => node.is_function_definition(),
            Class(node) => node.is_function_definition(),
            Generator(node) => node.is_function_definition(),
            AsyncFunction(node) => node.is_function_definition(),
            AsyncGenerator(node) => node.is_function_definition(),
        }
    }
}

impl IsIdentifierReference for PrimaryExpression {
    fn is_identifier_reference(&self) -> bool {
        use PrimaryExpressionKind::*;
        match &self.kind {
            This | Literal(_) | ArrayLiteral(_) | ObjectLiteral(_) | Parenthesized(_) | TemplateLiteral(_) | RegularExpression(_) | Function(_) | Class(_) | Generator(_)
            | AsyncFunction(_) | AsyncGenerator(_) => false,
            IdentifierReference(_) => true,
        }
    }
}

impl AssignmentTargetType for PrimaryExpression {
    fn assignment_target_type(&self) -> ATTKind {
        use PrimaryExpressionKind::*;
        match &self.kind {
            This | Literal(_) | ArrayLiteral(_) | ObjectLiteral(_) | TemplateLiteral(_) | RegularExpression(_) | Function(_) | Class(_) | Generator(_) | AsyncFunction(_)
            | AsyncGenerator(_) => ATTKind::Invalid,
            IdentifierReference(id) => id.assignment_target_type(),
            Parenthesized(expr) => expr.assignment_target_type(),
        }
    }
}

pub trait ToPrimaryExpressionKind {
    fn to_primary_expression_kind(node: Rc<Self>) -> PrimaryExpressionKind;
    fn to_primary_expression_result(node: Rc<Self>, scanner: Scanner) -> ParseResult<PrimaryExpression> {
        Ok((Rc::new(PrimaryExpression { kind: Self::to_primary_expression_kind(node) }), scanner))
    }
}

impl ToPrimaryExpressionKind for IdentifierReference {
    fn to_primary_expression_kind(node: Rc<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::IdentifierReference(node)
    }
}

impl ToPrimaryExpressionKind for Literal {
    fn to_primary_expression_kind(node: Rc<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::Literal(node)
    }
}

impl ToPrimaryExpressionKind for ArrayLiteral {
    fn to_primary_expression_kind(node: Rc<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::ArrayLiteral(node)
    }
}

impl ToPrimaryExpressionKind for ObjectLiteral {
    fn to_primary_expression_kind(node: Rc<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::ObjectLiteral(node)
    }
}

impl ToPrimaryExpressionKind for ParenthesizedExpression {
    fn to_primary_expression_kind(node: Rc<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::Parenthesized(node)
    }
}

impl ToPrimaryExpressionKind for TemplateLiteral {
    fn to_primary_expression_kind(node: Rc<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::TemplateLiteral(node)
    }
}

impl ToPrimaryExpressionKind for FunctionExpression {
    fn to_primary_expression_kind(node: Rc<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::Function(node)
    }
}

impl ToPrimaryExpressionKind for ClassExpression {
    fn to_primary_expression_kind(node: Rc<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::Class(node)
    }
}

impl ToPrimaryExpressionKind for GeneratorExpression {
    fn to_primary_expression_kind(node: Rc<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::Generator(node)
    }
}

impl ToPrimaryExpressionKind for AsyncFunctionExpression {
    fn to_primary_expression_kind(node: Rc<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::AsyncFunction(node)
    }
}

impl ToPrimaryExpressionKind for AsyncGeneratorExpression {
    fn to_primary_expression_kind(node: Rc<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::AsyncGenerator(node)
    }
}

impl PrimaryExpression {
    fn parse_this(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let after = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::This)?;
        Ok((Rc::new(PrimaryExpression { kind: PrimaryExpressionKind::This }), after))
    }

    fn parse_idref(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (node, after) = IdentifierReference::parse(parser, scanner, yield_flag, await_flag)?;
        IdentifierReference::to_primary_expression_result(node, after)
    }

    fn parse_literal(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (node, after) = Literal::parse(parser, scanner)?;
        Literal::to_primary_expression_result(node, after)
    }

    fn parse_array_literal(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (node, after) = ArrayLiteral::parse(parser, scanner, yield_flag, await_flag)?;
        ArrayLiteral::to_primary_expression_result(node, after)
    }

    fn parse_object_literal(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (node, after) = ObjectLiteral::parse(parser, scanner, yield_flag, await_flag)?;
        ObjectLiteral::to_primary_expression_result(node, after)
    }

    fn parse_function_exp(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (node, after) = FunctionExpression::parse(parser, scanner)?;
        FunctionExpression::to_primary_expression_result(node, after)
    }
    fn parse_parenthesized_exp(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (node, after) = ParenthesizedExpression::parse(parser, scanner, yield_flag, await_flag)?;
        ParenthesizedExpression::to_primary_expression_result(node, after)
    }
    fn parse_template_literal(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (node, after) = TemplateLiteral::parse(parser, scanner, yield_flag, await_flag, false)?;
        TemplateLiteral::to_primary_expression_result(node, after)
    }

    fn parse_class_exp(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (node, after) = ClassExpression::parse(parser, scanner, yield_flag, await_flag)?;
        ClassExpression::to_primary_expression_result(node, after)
    }

    fn parse_generator_exp(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (node, after) = GeneratorExpression::parse(parser, scanner)?;
        GeneratorExpression::to_primary_expression_result(node, after)
    }

    fn parse_async_func(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (node, after) = AsyncFunctionExpression::parse(parser, scanner)?;
        AsyncFunctionExpression::to_primary_expression_result(node, after)
    }

    fn parse_async_gen(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (node, after) = AsyncGeneratorExpression::parse(parser, scanner)?;
        AsyncGeneratorExpression::to_primary_expression_result(node, after)
    }

    fn parse_regex(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (tok, after) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        match tok {
            Token::RegularExpression(rd) => Ok((Rc::new(PrimaryExpression { kind: PrimaryExpressionKind::RegularExpression(rd) }), after)),
            _ => Err(ParseError::new("Expected regular expression", scanner.line, scanner.column)),
        }
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("Expected a PrimaryExpression", scanner.line, scanner.column))
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

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match &self.kind {
            PrimaryExpressionKind::This => kind == ParseNodeKind::This,
            PrimaryExpressionKind::IdentifierReference(boxed) => boxed.contains(kind),
            PrimaryExpressionKind::Literal(boxed) => kind == ParseNodeKind::Literal || boxed.contains(kind),
            PrimaryExpressionKind::ArrayLiteral(boxed) => boxed.contains(kind),
            PrimaryExpressionKind::ObjectLiteral(boxed) => boxed.contains(kind),
            PrimaryExpressionKind::Parenthesized(boxed) => boxed.contains(kind),
            PrimaryExpressionKind::TemplateLiteral(boxed) => boxed.contains(kind),
            PrimaryExpressionKind::Function(node) => node.contains(kind),
            PrimaryExpressionKind::Class(node) => node.contains(kind),
            PrimaryExpressionKind::Generator(node) => node.contains(kind),
            PrimaryExpressionKind::AsyncFunction(node) => node.contains(kind),
            PrimaryExpressionKind::AsyncGenerator(node) => node.contains(kind),
            PrimaryExpressionKind::RegularExpression(..) => false,
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match &self.kind {
            PrimaryExpressionKind::Literal(n) => n.as_string_literal(),
            _ => None,
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        match &self.kind {
            PrimaryExpressionKind::This => true,
            PrimaryExpressionKind::IdentifierReference(_) => true,
            PrimaryExpressionKind::Literal(_) => true,
            PrimaryExpressionKind::ArrayLiteral(boxed) => boxed.all_private_identifiers_valid(names),
            PrimaryExpressionKind::ObjectLiteral(boxed) => boxed.all_private_identifiers_valid(names),
            PrimaryExpressionKind::Parenthesized(boxed) => boxed.all_private_identifiers_valid(names),
            PrimaryExpressionKind::TemplateLiteral(boxed) => boxed.all_private_identifiers_valid(names),
            PrimaryExpressionKind::Function(node) => node.all_private_identifiers_valid(names),
            PrimaryExpressionKind::Class(node) => node.all_private_identifiers_valid(names),
            PrimaryExpressionKind::Generator(node) => node.all_private_identifiers_valid(names),
            PrimaryExpressionKind::AsyncFunction(node) => node.all_private_identifiers_valid(names),
            PrimaryExpressionKind::AsyncGenerator(node) => node.all_private_identifiers_valid(names),
            PrimaryExpressionKind::RegularExpression(..) => true,
        }
    }
}

#[derive(Debug)]
pub struct Elisions {
    count: usize,
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
        writeln!(writer, "{}Elisions: {}", first, self)
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
        loop {
            let (token, after_comma) = scan_token(&current_scanner, parser.source, ScanGoal::InputElementRegExp);
            if !token.matches_punct(Punctuator::Comma) {
                return if comma_count == 0 {
                    Err(ParseError::new("Expected one or more commas", current_scanner.line, current_scanner.column))
                } else {
                    Ok((Rc::new(Elisions { count: comma_count }), current_scanner))
                };
            }
            comma_count += 1;
            current_scanner = after_comma;
        }
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        match parser.elision_cache.get(&scanner) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner);
                parser.elision_cache.insert(scanner, result.clone());
                result
            }
        }
    }

    pub fn contains(&self, _kind: ParseNodeKind) -> bool {
        false
    }
}

// SpreadElement[Yield, Await] :
//      ... AssignmentExpression[+In, ?Yield, ?Await]
#[derive(Debug)]
pub enum SpreadElement {
    AssignmentExpression(Rc<AssignmentExpression>),
}

impl fmt::Display for SpreadElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let SpreadElement::AssignmentExpression(boxed) = self;
        write!(f, "... {}", boxed)
    }
}

impl PrettyPrint for SpreadElement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}SpreadElement: {}", first, self)?;
        let SpreadElement::AssignmentExpression(boxed) = self;
        boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}SpreadElement: {}", first, self)?;
        pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        let SpreadElement::AssignmentExpression(node) = self;
        node.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl SpreadElement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_ellipsis = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Ellipsis)?;
        let (ae, after_ae) = AssignmentExpression::parse(parser, after_ellipsis, true, yield_flag, await_flag)?;
        Ok((Rc::new(SpreadElement::AssignmentExpression(ae)), after_ae))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let SpreadElement::AssignmentExpression(boxed) = self;
        boxed.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        let SpreadElement::AssignmentExpression(boxed) = self;
        boxed.all_private_identifiers_valid(names)
    }
}

// ElementList[Yield, Await] :
//      Elisionopt AssignmentExpression[+In, ?Yield, ?Await]
//      Elisionopt SpreadElement[?Yield, ?Await]
//      ElementList[?Yield, ?Await] , Elisionopt AssignmentExpression[+In, ?Yield, ?Await]
//      ElementList[?Yield, ?Await] , Elisionopt SpreadElement[?Yield, ?Await]
#[derive(Debug)]
pub enum ElementList {
    AssignmentExpression((Option<Rc<Elisions>>, Rc<AssignmentExpression>)),
    SpreadElement((Option<Rc<Elisions>>, Rc<SpreadElement>)),
    ElementListAssignmentExpression((Rc<ElementList>, Option<Rc<Elisions>>, Rc<AssignmentExpression>)),
    ElementListSpreadElement((Rc<ElementList>, Option<Rc<Elisions>>, Rc<SpreadElement>)),
}

impl fmt::Display for ElementList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ElementList::AssignmentExpression((elisions, ae)) => match elisions {
                None => write!(f, "{}", ae),
                Some(commas) => write!(f, "{} {}", commas, ae),
            },
            ElementList::SpreadElement((elisions, se)) => match elisions {
                None => write!(f, "{}", se),
                Some(commas) => write!(f, "{} {}", commas, se),
            },
            ElementList::ElementListAssignmentExpression((el, elisions, ae)) => match elisions {
                None => write!(f, "{} , {}", el, ae),
                Some(commas) => write!(f, "{} , {} {}", el, commas, ae),
            },
            ElementList::ElementListSpreadElement((el, elisions, se)) => match elisions {
                None => write!(f, "{} , {}", el, se),
                Some(commas) => write!(f, "{} , {} {}", el, commas, se),
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
        writeln!(writer, "{}ElementList: {}", first, self)?;
        match self {
            ElementList::AssignmentExpression((elisions, ae)) => match elisions {
                None => ae.pprint_with_leftpad(writer, &successive, Spot::Final),
                Some(commas) => {
                    commas.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    ae.pprint_with_leftpad(writer, &successive, Spot::Final)
                }
            },
            ElementList::SpreadElement((elisions, boxed)) => match elisions {
                None => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
                Some(commas) => {
                    commas.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
                }
            },
            ElementList::ElementListAssignmentExpression((right, elisions, left)) => match elisions {
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
            ElementList::ElementListSpreadElement((right, elisions, left)) => match elisions {
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
            ElementList::AssignmentExpression((None, ae)) => ae.concise_with_leftpad(writer, pad, state),
            ElementList::AssignmentExpression((Some(commas), ae)) => {
                writeln!(writer, "{}ElementList: {}", first, self)?;
                commas.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ae.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ElementList::SpreadElement((None, se)) => se.concise_with_leftpad(writer, pad, state),
            ElementList::SpreadElement((Some(commas), se)) => {
                writeln!(writer, "{}ElementList: {}", first, self)?;
                commas.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                se.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ElementList::ElementListAssignmentExpression((el, None, ae)) => {
                writeln!(writer, "{}ElementList: {}", first, self)?;
                el.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                ae.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ElementList::ElementListAssignmentExpression((el, Some(commas), ae)) => {
                writeln!(writer, "{}ElementList: {}", first, self)?;
                el.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                commas.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ae.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ElementList::ElementListSpreadElement((el, None, se)) => {
                writeln!(writer, "{}ElementList: {}", first, self)?;
                el.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                se.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ElementList::ElementListSpreadElement((el, Some(commas), se)) => {
                writeln!(writer, "{}ElementList: {}", first, self)?;
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
    fn non_recursive_part(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Option<Rc<Elisions>>, ELItemKind, Scanner), ParseError> {
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
                        let err_default = Some(ParseError::new("AssignmentExpression or SpreadElement expected", after_e_scanner.line, after_e_scanner.column));
                        let err_se = Some(pe);
                        let err1 = if ParseError::compare_option(&err_default, &err_ae) == Ordering::Less { err_ae } else { err_default };
                        let err2 = if ParseError::compare_option(&err1, &err_se) == Ordering::Less { err_se } else { err1 };
                        Err(err2.unwrap())
                    }
                }
            }
        }
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let mut current_production;
        let mut current_scanner;

        let (elision, item, after) = Self::non_recursive_part(parser, scanner, yield_flag, await_flag)?;
        current_production = match item {
            ELItemKind::AE(boxed_ae) => Rc::new(ElementList::AssignmentExpression((elision, boxed_ae))),
            ELItemKind::SE(boxed_se) => Rc::new(ElementList::SpreadElement((elision, boxed_se))),
        };
        current_scanner = after;

        while let Ok((elision, item, after)) = scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
            .and_then(|after_comma| Self::non_recursive_part(parser, after_comma, yield_flag, await_flag))
        {
            current_production = match item {
                ELItemKind::AE(boxed_ae) => Rc::new(ElementList::ElementListAssignmentExpression((current_production, elision, boxed_ae))),
                ELItemKind::SE(boxed_se) => Rc::new(ElementList::ElementListSpreadElement((current_production, elision, boxed_se))),
            };
            current_scanner = after;
        }
        Ok((current_production, current_scanner))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ElementList::AssignmentExpression((elisions, ae)) => elisions.as_ref().map_or(false, |n| n.contains(kind)) || ae.contains(kind),
            ElementList::SpreadElement((elisions, se)) => elisions.as_ref().map_or(false, |n| n.contains(kind)) || se.contains(kind),
            ElementList::ElementListAssignmentExpression((el, elisions, ae)) => el.contains(kind) || elisions.as_ref().map_or(false, |n| n.contains(kind)) || ae.contains(kind),
            ElementList::ElementListSpreadElement((el, elisions, se)) => el.contains(kind) || elisions.as_ref().map_or(false, |n| n.contains(kind)) || se.contains(kind),
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
            ElementList::AssignmentExpression((_, ae)) => ae.all_private_identifiers_valid(names),
            ElementList::SpreadElement((_, se)) => se.all_private_identifiers_valid(names),
            ElementList::ElementListAssignmentExpression((el, _, ae)) => el.all_private_identifiers_valid(names) && ae.all_private_identifiers_valid(names),
            ElementList::ElementListSpreadElement((el, _, se)) => el.all_private_identifiers_valid(names) && se.all_private_identifiers_valid(names),
        }
    }
}

// ArrayLiteral[Yield, Await] :
//      [ Elisionopt ]
//      [ ElementList[?Yield, ?Await] ]
//      [ ElementList[?Yield, ?Await] , Elisionopt ]
#[derive(Debug)]
pub enum ArrayLiteral {
    Empty(Option<Rc<Elisions>>),
    ElementList(Rc<ElementList>),
    ElementListElision(Rc<ElementList>, Option<Rc<Elisions>>),
}

impl fmt::Display for ArrayLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArrayLiteral::Empty(pot_elision) => match pot_elision {
                None => write!(f, "[ ]"),
                Some(elision) => write!(f, "[ {} ]", elision),
            },
            ArrayLiteral::ElementList(boxed) => write!(f, "[ {} ]", boxed),
            ArrayLiteral::ElementListElision(boxed, pot_elision) => match pot_elision {
                None => write!(f, "[ {} , ]", boxed),
                Some(elision) => write!(f, "[ {} , {} ]", boxed, elision),
            },
        }
    }
}

impl PrettyPrint for ArrayLiteral {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ArrayLiteral: {}", first, self)?;
        match self {
            ArrayLiteral::Empty(None) => Ok(()),
            ArrayLiteral::Empty(Some(elision)) => elision.pprint_with_leftpad(writer, &successive, Spot::Final),
            ArrayLiteral::ElementList(boxed) | ArrayLiteral::ElementListElision(boxed, None) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            ArrayLiteral::ElementListElision(boxed, Some(elision)) => {
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
        writeln!(writer, "{}ArrayLiteral: {}", first, self)?;
        match self {
            ArrayLiteral::Empty(None) => {
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            ArrayLiteral::Empty(Some(elision)) => {
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                elision.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            ArrayLiteral::ElementList(node) => {
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            ArrayLiteral::ElementListElision(node, None) => {
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            ArrayLiteral::ElementListElision(node, Some(elision)) => {
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBracket)?;
        Err(ParseError::new("‘,’, ‘]’, or an ElementList expected", after.line, after.column))
            .otherwise(|| {
                let (el, after_el) = ElementList::parse(parser, after, yield_flag, await_flag)?;
                let (punct, after_punct) = scan_for_punct_set(after_el, parser.source, ScanGoal::InputElementDiv, &[Punctuator::Comma, Punctuator::RightBracket])?;
                match punct {
                    Punctuator::RightBracket => Ok((Rc::new(ArrayLiteral::ElementList(el)), after_punct)),
                    _ => {
                        let (elisions, after_elisions) = match Elisions::parse(parser, after_punct) {
                            Ok((node, scan)) => (Some(node), scan),
                            Err(_) => (None, after_punct),
                        };
                        let end_scan = scan_for_punct(after_elisions, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBracket)?;
                        Ok((Rc::new(ArrayLiteral::ElementListElision(el, elisions)), end_scan))
                    }
                }
            })
            .otherwise(|| {
                let (elisions, after_elisions) = match Elisions::parse(parser, after) {
                    Ok((node, scan)) => (Some(node), scan),
                    Err(_) => (None, after),
                };
                let end_scan = scan_for_punct(after_elisions, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBracket)?;
                Ok((Rc::new(ArrayLiteral::Empty(elisions)), end_scan))
            })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ArrayLiteral::Empty(pot_elision) => pot_elision.as_ref().map_or(false, |n| n.contains(kind)),
            ArrayLiteral::ElementList(boxed) => boxed.contains(kind),
            ArrayLiteral::ElementListElision(boxed, pot_elision) => boxed.contains(kind) || pot_elision.as_ref().map_or(false, |n| n.contains(kind)),
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
            ArrayLiteral::Empty(_) => true,
            ArrayLiteral::ElementList(boxed) => boxed.all_private_identifiers_valid(names),
            ArrayLiteral::ElementListElision(boxed, _) => boxed.all_private_identifiers_valid(names),
        }
    }
}

// Initializer[In, Yield, Await] :
//      = AssignmentExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum Initializer {
    AssignmentExpression(Rc<AssignmentExpression>),
}

impl fmt::Display for Initializer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Initializer::AssignmentExpression(boxed_ae) = self;
        write!(f, "= {}", *boxed_ae)
    }
}

impl PrettyPrint for Initializer {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Initializer: {}", first, self)?;
        let Initializer::AssignmentExpression(boxed_ae) = self;
        boxed_ae.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Initializer: {}", first, self)?;
        pprint_token(writer, "=", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        let Initializer::AssignmentExpression(node) = self;
        node.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl Initializer {
    fn parse_core(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Initializer> {
        let after_tok = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Eq)?;
        let (boxed_ae, after_ae) = AssignmentExpression::parse(parser, after_tok, in_flag, yield_flag, await_flag)?;
        Ok((Rc::new(Initializer::AssignmentExpression(boxed_ae)), after_ae))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Initializer> {
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

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let Initializer::AssignmentExpression(node) = self;
        node.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        let Initializer::AssignmentExpression(node) = self;
        node.all_private_identifiers_valid(names)
    }
}

// CoverInitializedName[Yield, Await] :
//      IdentifierReference[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]
#[derive(Debug)]
pub enum CoverInitializedName {
    InitializedName(Rc<IdentifierReference>, Rc<Initializer>),
}

impl fmt::Display for CoverInitializedName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let CoverInitializedName::InitializedName(idref, izer) = self;
        write!(f, "{} {}", idref, izer)
    }
}

impl PrettyPrint for CoverInitializedName {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CoverInitializedName: {}", first, self)?;
        let CoverInitializedName::InitializedName(idref, izer) = self;
        idref.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        izer.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CoverInitializedName: {}", first, self)?;
        let CoverInitializedName::InitializedName(idref, izer) = self;
        idref.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        izer.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl CoverInitializedName {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (idref, after_idref) = IdentifierReference::parse(parser, scanner, yield_flag, await_flag)?;
        let (izer, after_izer) = Initializer::parse(parser, after_idref, true, yield_flag, await_flag)?;
        Ok((Rc::new(CoverInitializedName::InitializedName(idref, izer)), after_izer))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let CoverInitializedName::InitializedName(idref, izer) = self;
        idref.contains(kind) || izer.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        let CoverInitializedName::InitializedName(_, izer) = self;
        izer.all_private_identifiers_valid(names)
    }
}

// ComputedPropertyName[Yield, Await] :
//      [ AssignmentExpression[+In, ?Yield, ?Await] ]
#[derive(Debug)]
pub enum ComputedPropertyName {
    AssignmentExpression(Rc<AssignmentExpression>),
}

impl fmt::Display for ComputedPropertyName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ComputedPropertyName::AssignmentExpression(ae) = self;
        write!(f, "[ {} ]", ae)
    }
}

impl PrettyPrint for ComputedPropertyName {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ComputedPropertyName: {}", first, self)?;
        let ComputedPropertyName::AssignmentExpression(ae) = self;
        ae.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ComputedPropertyName: {}", first, self)?;
        pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        let ComputedPropertyName::AssignmentExpression(ae) = self;
        ae.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ComputedPropertyName {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_tok = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBracket)?;
        let (ae, after_ae) = AssignmentExpression::parse(parser, after_tok, true, yield_flag, await_flag)?;
        let after_rb = scan_for_punct(after_ae, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBracket)?;
        Ok((Rc::new(ComputedPropertyName::AssignmentExpression(ae)), after_rb))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let ComputedPropertyName::AssignmentExpression(n) = self;
        n.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        let ComputedPropertyName::AssignmentExpression(n) = self;
        n.all_private_identifiers_valid(names)
    }
}

// LiteralPropertyName :
//      IdentifierName
//      StringLiteral
//      NumericLiteral
#[derive(Debug)]
pub enum LiteralPropertyName {
    IdentifierName(IdentifierData),
    StringLiteral(StringToken),
    NumericLiteral(Numeric),
}

impl fmt::Display for LiteralPropertyName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LiteralPropertyName::IdentifierName(id) => write!(f, "{}", id),
            LiteralPropertyName::StringLiteral(s) => write!(f, "{}", s),
            LiteralPropertyName::NumericLiteral(Numeric::Number(n)) => {
                let mut s = Vec::new();
                number_to_string(&mut s, *n).unwrap();
                write!(f, "{}", String::from_utf8(s).unwrap())
            }
            LiteralPropertyName::NumericLiteral(Numeric::BigInt(b)) => write!(f, "{}", b),
        }
    }
}

impl PrettyPrint for LiteralPropertyName {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{}LiteralPropertyName: {}", first, self)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            LiteralPropertyName::IdentifierName(id) => pprint_token(writer, id, TokenType::IdentifierName, pad, state),
            LiteralPropertyName::StringLiteral(s) => pprint_token(writer, &format!("{}", s), TokenType::String, pad, state),
            LiteralPropertyName::NumericLiteral(n) => pprint_token(writer, n, TokenType::Numeric, pad, state),
        }
    }
}

impl LiteralPropertyName {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (tok, after_tok) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        match tok {
            Token::Identifier(id) => Ok((Rc::new(LiteralPropertyName::IdentifierName(id)), after_tok)),
            Token::String(s) => Ok((Rc::new(LiteralPropertyName::StringLiteral(s)), after_tok)),
            Token::Number(n) => Ok((Rc::new(LiteralPropertyName::NumericLiteral(Numeric::Number(n))), after_tok)),
            Token::BigInt(b) => Ok((Rc::new(LiteralPropertyName::NumericLiteral(Numeric::BigInt(b))), after_tok)),
            _ => Err(ParseError::new("Identifier, String, or Number expected", scanner.line, scanner.column)),
        }
    }

    pub fn contains(&self, _kind: ParseNodeKind) -> bool {
        false
    }
}

// PropertyName[Yield, Await] :
//      LiteralPropertyName
//      ComputedPropertyName[?Yield, ?Await]
#[derive(Debug)]
pub enum PropertyName {
    LiteralPropertyName(Rc<LiteralPropertyName>),
    ComputedPropertyName(Rc<ComputedPropertyName>),
}

impl fmt::Display for PropertyName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PropertyName::LiteralPropertyName(lpn) => write!(f, "{}", lpn),
            PropertyName::ComputedPropertyName(cpn) => write!(f, "{}", cpn),
        }
    }
}

impl PrettyPrint for PropertyName {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}PropertyName: {}", first, self)?;
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
        Err(ParseError::new("PropertyName expected", scanner.line, scanner.column))
            .otherwise(|| LiteralPropertyName::parse(parser, scanner).map(|(lpn, after_lpn)| (Rc::new(PropertyName::LiteralPropertyName(lpn)), after_lpn)))
            .otherwise(|| ComputedPropertyName::parse(parser, scanner, yield_flag, await_flag).map(|(cpn, after_cpn)| (Rc::new(PropertyName::ComputedPropertyName(cpn)), after_cpn)))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
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

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            PropertyName::LiteralPropertyName(n) => n.contains(kind),
            PropertyName::ComputedPropertyName(n) => n.contains(kind),
        }
    }

    pub fn computed_property_contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            PropertyName::LiteralPropertyName(..) => false,
            PropertyName::ComputedPropertyName(n) => n.contains(kind),
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
            PropertyName::LiteralPropertyName(_) => true,
            PropertyName::ComputedPropertyName(n) => n.all_private_identifiers_valid(names),
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
pub enum PropertyDefinition {
    IdentifierReference(Rc<IdentifierReference>),
    CoverInitializedName(Rc<CoverInitializedName>),
    PropertyNameAssignmentExpression(Rc<PropertyName>, Rc<AssignmentExpression>),
    MethodDefinition(Rc<MethodDefinition>),
    AssignmentExpression(Rc<AssignmentExpression>),
}

impl fmt::Display for PropertyDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PropertyDefinition::IdentifierReference(idref) => write!(f, "{}", idref),
            PropertyDefinition::CoverInitializedName(cin) => write!(f, "{}", cin),
            PropertyDefinition::PropertyNameAssignmentExpression(pn, ae) => write!(f, "{} : {}", pn, ae),
            PropertyDefinition::MethodDefinition(md) => write!(f, "{}", md),
            PropertyDefinition::AssignmentExpression(ae) => write!(f, "... {}", ae),
        }
    }
}

impl PrettyPrint for PropertyDefinition {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}PropertyDefinition: {}", first, self)?;
        match self {
            PropertyDefinition::IdentifierReference(idref) => idref.pprint_with_leftpad(writer, &successive, Spot::Final),
            PropertyDefinition::CoverInitializedName(cin) => cin.pprint_with_leftpad(writer, &successive, Spot::Final),
            PropertyDefinition::PropertyNameAssignmentExpression(pn, ae) => {
                pn.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ae.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            PropertyDefinition::MethodDefinition(md) => md.pprint_with_leftpad(writer, &successive, Spot::Final),
            PropertyDefinition::AssignmentExpression(ae) => ae.pprint_with_leftpad(writer, &successive, Spot::Final),
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
                writeln!(writer, "{}PropertyDefinition: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            PropertyDefinition::AssignmentExpression(node) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}PropertyDefinition: {}", first, self)?;
                pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl PropertyDefinition {
    fn parse_pn_ae(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (pn, after_pn) = PropertyName::parse(parser, scanner, yield_flag, await_flag)?;
        let (tok, after_tok) = scan_token(&after_pn, parser.source, ScanGoal::InputElementRegExp);
        match tok {
            Token::Punctuator(Punctuator::Colon) => {
                let (ae, after_ae) = AssignmentExpression::parse(parser, after_tok, true, yield_flag, await_flag)?;
                Ok((Rc::new(PropertyDefinition::PropertyNameAssignmentExpression(pn, ae)), after_ae))
            }
            _ => Err(ParseError::new("‘:’ expected", after_pn.line, after_pn.column)),
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
        let after_tok = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Ellipsis)?;
        let (ae, after_ae) = AssignmentExpression::parse(parser, after_tok, true, yield_flag, await_flag)?;
        Ok((Rc::new(PropertyDefinition::AssignmentExpression(ae)), after_ae))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("PropertyName expected", scanner.line, scanner.column))
            .otherwise(|| Self::parse_pn_ae(parser, scanner, yield_flag, await_flag))
            .otherwise(|| Self::parse_cin(parser, scanner, yield_flag, await_flag))
            .otherwise(|| Self::parse_md(parser, scanner, yield_flag, await_flag))
            .otherwise(|| Self::parse_idref(parser, scanner, yield_flag, await_flag))
            .otherwise(|| Self::parse_ae(parser, scanner, yield_flag, await_flag))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            PropertyDefinition::IdentifierReference(idref) => idref.contains(kind),
            PropertyDefinition::CoverInitializedName(cin) => cin.contains(kind),
            PropertyDefinition::PropertyNameAssignmentExpression(pn, ae) => pn.contains(kind) || ae.contains(kind),
            PropertyDefinition::MethodDefinition(md) => kind == ParseNodeKind::MethodDefinition || md.computed_property_contains(kind),
            PropertyDefinition::AssignmentExpression(ae) => ae.contains(kind),
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
            PropertyDefinition::IdentifierReference(_) => true,
            PropertyDefinition::CoverInitializedName(cin) => cin.all_private_identifiers_valid(names),
            PropertyDefinition::PropertyNameAssignmentExpression(pn, ae) => pn.all_private_identifiers_valid(names) && ae.all_private_identifiers_valid(names),
            PropertyDefinition::MethodDefinition(md) => md.all_private_identifiers_valid(names),
            PropertyDefinition::AssignmentExpression(ae) => ae.all_private_identifiers_valid(names),
        }
    }
}

// PropertyDefinitionList[Yield, Await] :
//      PropertyDefinition[?Yield, ?Await]
//      PropertyDefinitionList[?Yield, ?Await] , PropertyDefinition[?Yield, ?Await]
#[derive(Debug)]
pub enum PropertyDefinitionList {
    OneDef(Rc<PropertyDefinition>),
    ManyDefs(Rc<PropertyDefinitionList>, Rc<PropertyDefinition>),
}

impl fmt::Display for PropertyDefinitionList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PropertyDefinitionList::OneDef(pd) => write!(f, "{}", pd),
            PropertyDefinitionList::ManyDefs(pdl, pd) => write!(f, "{} , {}", pdl, pd),
        }
    }
}

impl PrettyPrint for PropertyDefinitionList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}PropertyDefinitionList: {}", first, self)?;
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
                writeln!(writer, "{}PropertyDefinitionList: {}", first, self)?;
                pdl.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pd.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl PropertyDefinitionList {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (pd, after_pd) = PropertyDefinition::parse(parser, scanner, yield_flag, await_flag)?;
        let mut current_production = Rc::new(PropertyDefinitionList::OneDef(pd));
        let mut current_scanner = after_pd;
        while let Ok((pd2, after_pd2)) = scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
            .and_then(|after_comma| PropertyDefinition::parse(parser, after_comma, yield_flag, await_flag))
        {
            current_production = Rc::new(PropertyDefinitionList::ManyDefs(current_production, pd2));
            current_scanner = after_pd2;
        }
        Ok((current_production, current_scanner))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            PropertyDefinitionList::OneDef(pd) => pd.contains(kind),
            PropertyDefinitionList::ManyDefs(pdl, pd) => pdl.contains(kind) || pd.contains(kind),
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
            PropertyDefinitionList::OneDef(pd) => pd.all_private_identifiers_valid(names),
            PropertyDefinitionList::ManyDefs(pdl, pd) => pdl.all_private_identifiers_valid(names) && pd.all_private_identifiers_valid(names),
        }
    }
}

// ObjectLiteral[Yield, Await] :
//      { }
//      { PropertyDefinitionList[?Yield, ?Await] }
//      { PropertyDefinitionList[?Yield, ?Await] , }
#[derive(Debug)]
pub enum ObjectLiteral {
    Empty,
    Normal(Rc<PropertyDefinitionList>),
    TrailingComma(Rc<PropertyDefinitionList>),
}

impl fmt::Display for ObjectLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ObjectLiteral::Empty => write!(f, "{{ }}"),
            ObjectLiteral::Normal(pdl) => write!(f, "{{ {} }}", pdl),
            ObjectLiteral::TrailingComma(pdl) => write!(f, "{{ {} , }}", pdl),
        }
    }
}

impl PrettyPrint for ObjectLiteral {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ObjectLiteral: {}", first, self)?;
        match self {
            ObjectLiteral::Empty => Ok(()),
            ObjectLiteral::Normal(pdl) | ObjectLiteral::TrailingComma(pdl) => pdl.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ObjectLiteral: {}", first, self)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match self {
            ObjectLiteral::Empty => {}
            ObjectLiteral::Normal(node) => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ObjectLiteral::TrailingComma(node) => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
            }
        }
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ObjectLiteral {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_brace = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBrace)?;
        match PropertyDefinitionList::parse(parser, after_brace, yield_flag, await_flag) {
            Err(_) => {
                let after_brace2 = scan_for_punct(after_brace, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                Ok((Rc::new(ObjectLiteral::Empty), after_brace2))
            }
            Ok((pdl, after_pdl)) => {
                let (comma_or_brace, after_punct) = scan_for_punct_set(after_pdl, parser.source, ScanGoal::InputElementDiv, &[Punctuator::RightBrace, Punctuator::Comma])?;
                match comma_or_brace {
                    Punctuator::RightBrace => Ok((Rc::new(ObjectLiteral::Normal(pdl)), after_punct)),
                    _ => {
                        let after_brace3 = scan_for_punct(after_punct, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                        Ok((Rc::new(ObjectLiteral::TrailingComma(pdl)), after_brace3))
                    }
                }
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ObjectLiteral::Empty => false,
            ObjectLiteral::Normal(pdl) => pdl.contains(kind),
            ObjectLiteral::TrailingComma(pdl) => pdl.contains(kind),
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
            ObjectLiteral::Empty => true,
            ObjectLiteral::Normal(pdl) => pdl.all_private_identifiers_valid(names),
            ObjectLiteral::TrailingComma(pdl) => pdl.all_private_identifiers_valid(names),
        }
    }
}

//////// 12.2.4 Literals
// Literal :
//      NullLiteral
//      BooleanLiteral
//      NumericLiteral
//      StringLiteral
#[derive(Debug, PartialEq)]
pub enum Numeric {
    Number(f64),
    BigInt(BigInt),
}

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

#[derive(Debug, PartialEq)]
pub enum LiteralKind {
    NullLiteral,
    BooleanLiteral(bool),
    NumericLiteral(Numeric),
    StringLiteral(StringToken),
}
#[derive(Debug)]
pub struct Literal {
    kind: LiteralKind,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            LiteralKind::NullLiteral => write!(f, "null"),
            LiteralKind::BooleanLiteral(b) => {
                if *b {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                }
            }
            LiteralKind::NumericLiteral(Numeric::Number(n)) => {
                let mut s = Vec::new();
                number_to_string(&mut s, *n).unwrap();
                write!(f, "{}", String::from_utf8(s).unwrap())
            }
            LiteralKind::NumericLiteral(Numeric::BigInt(b)) => write!(f, "{}", *b),
            LiteralKind::StringLiteral(s) => write!(f, "{}", *s),
        }
    }
}

impl PrettyPrint for Literal {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{}Literal: {}", first, self)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match &self.kind {
            LiteralKind::NullLiteral => pprint_token(writer, "null", TokenType::Keyword, pad, state),
            LiteralKind::BooleanLiteral(_) => pprint_token(writer, self, TokenType::Keyword, pad, state),
            LiteralKind::NumericLiteral(_) => pprint_token(writer, self, TokenType::Numeric, pad, state),
            LiteralKind::StringLiteral(_) => pprint_token(writer, self, TokenType::String, pad, state),
        }
    }
}

impl Literal {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Literal> {
        let (token, newscanner) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        match token {
            Token::Identifier(id) if id.matches(Keyword::Null) => Ok((Rc::new(Literal { kind: LiteralKind::NullLiteral }), newscanner)),
            Token::Identifier(id) if id.matches(Keyword::True) => Ok((Rc::new(Literal { kind: LiteralKind::BooleanLiteral(true) }), newscanner)),
            Token::Identifier(id) if id.matches(Keyword::False) => Ok((Rc::new(Literal { kind: LiteralKind::BooleanLiteral(false) }), newscanner)),
            Token::Number(num) => Ok((Rc::new(Literal { kind: LiteralKind::NumericLiteral(Numeric::Number(num)) }), newscanner)),
            Token::BigInt(bi) => Ok((Rc::new(Literal { kind: LiteralKind::NumericLiteral(Numeric::BigInt(bi)) }), newscanner)),
            Token::String(s) => Ok((Rc::new(Literal { kind: LiteralKind::StringLiteral(s) }), newscanner)),
            _ => Err(ParseError::new("Literal expected", scanner.line, scanner.column)),
        }
    }

    pub fn contains(&self, _: ParseNodeKind) -> bool {
        false
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        if let LiteralKind::StringLiteral(s) = &self.kind {
            Some(s.clone())
        } else {
            None
        }
    }
}

// TemplateLiteral[Yield, Await, Tagged] :
//      NoSubstitutionTemplate
//      SubstitutionTemplate[?Yield, ?Await, ?Tagged]
#[derive(Debug)]
pub enum TemplateLiteral {
    NoSubstitutionTemplate(TemplateData, bool),
    SubstitutionTemplate(Rc<SubstitutionTemplate>),
}

impl fmt::Display for TemplateLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TemplateLiteral::NoSubstitutionTemplate(td, _) => write!(f, "`{}`", td),
            TemplateLiteral::SubstitutionTemplate(boxed) => write!(f, "{}", boxed),
        }
    }
}

impl PrettyPrint for TemplateLiteral {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}TemplateLiteral: {}", first, self)?;
        match self {
            TemplateLiteral::NoSubstitutionTemplate(_, _) => Ok(()),
            TemplateLiteral::SubstitutionTemplate(st) => st.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            TemplateLiteral::NoSubstitutionTemplate(..) => pprint_token(writer, self, TokenType::NoSubTemplate, pad, state),
            TemplateLiteral::SubstitutionTemplate(st) => st.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl TemplateLiteral {
    fn parse_nst(parser: &mut Parser, scanner: Scanner, tagged_flag: bool) -> ParseResult<Self> {
        let (tok, after_nst) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        if let Token::NoSubstitutionTemplate(td) = tok {
            Ok((Rc::new(TemplateLiteral::NoSubstitutionTemplate(td, tagged_flag)), after_nst))
        } else {
            Err(ParseError::new("NoSubstitutionTemplate expected", scanner.line, scanner.column))
        }
    }

    fn parse_subst(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, tagged_flag: bool) -> ParseResult<Self> {
        let (node, after) = SubstitutionTemplate::parse(parser, scanner, yield_flag, await_flag, tagged_flag)?;
        Ok((Rc::new(TemplateLiteral::SubstitutionTemplate(node)), after))
    }

    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, tagged_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("TemplateLiteral expected", scanner.line, scanner.column))
            .otherwise(|| Self::parse_nst(parser, scanner, tagged_flag))
            .otherwise(|| Self::parse_subst(parser, scanner, yield_flag, await_flag, tagged_flag))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, tagged_flag: bool) -> ParseResult<Self> {
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

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            TemplateLiteral::NoSubstitutionTemplate(..) => false,
            TemplateLiteral::SubstitutionTemplate(boxed) => boxed.contains(kind),
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
            TemplateLiteral::NoSubstitutionTemplate(..) => true,
            TemplateLiteral::SubstitutionTemplate(boxed) => boxed.all_private_identifiers_valid(names),
        }
    }
}

// SubstitutionTemplate[Yield, Await, Tagged] :
//      TemplateHead Expression[+In, ?Yield, ?Await] TemplateSpans[?Yield, ?Await, ?Tagged]
#[derive(Debug)]
pub struct SubstitutionTemplate {
    template_head: TemplateData,
    tagged: bool,
    expression: Rc<Expression>,
    template_spans: Rc<TemplateSpans>,
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
        writeln!(writer, "{}SubstitutionTemplate: {}", first, self)?;
        self.expression.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.template_spans.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}SubstitutionTemplate: {}", first, self)?;
        pprint_token(writer, &format!("`{}${{", self.template_head), TokenType::TemplateHead, &successive, Spot::NotFinal)?;
        self.expression.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.template_spans.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl SubstitutionTemplate {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, tagged_flag: bool) -> ParseResult<Self> {
        let (head, after_head) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        if let Token::TemplateHead(td) = head {
            let (exp_boxed, after_exp) = Expression::parse(parser, after_head, true, yield_flag, await_flag)?;
            let (spans_boxed, after_spans) = TemplateSpans::parse(parser, after_exp, yield_flag, await_flag, tagged_flag)?;
            Ok((Rc::new(SubstitutionTemplate { template_head: td, tagged: tagged_flag, expression: exp_boxed, template_spans: spans_boxed }), after_spans))
        } else {
            Err(ParseError::new("SubstitutionTemplate expected", scanner.line, scanner.column))
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.expression.contains(kind) || self.template_spans.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.expression.all_private_identifiers_valid(names) && self.template_spans.all_private_identifiers_valid(names)
    }

    pub fn early_errors(&self, agent: &mut Agent) -> Vec<Object> {
        // Static Semantics: Early Errors
        // SubstitutionTemplate[Yield, Await, Tagged] : TemplateHead Expression[+In, ?Yield, ?Await] TemplateSpans[?Yield, ?Await, ?Tagged]
        //  * It is a Syntax Error if the [Tagged] parameter was not set and TemplateHead Contains NotEscapeSequence.
        let mut errs = vec![];
        if !self.tagged && self.template_head.tv.is_none() {
            errs.push(create_syntax_error_object(agent, "invalid escape sequence in template literal"));
        }
        errs.extend(self.expression.early_errors(agent));
        errs.extend(self.template_spans.early_errors(agent));
        errs
    }
}

// TemplateSpans[Yield, Await, Tagged] :
//      TemplateTail
//      TemplateMiddleList[?Yield, ?Await, ?Tagged] TemplateTail
#[derive(Debug)]
pub enum TemplateSpans {
    Tail(TemplateData, bool),
    List(Rc<TemplateMiddleList>, TemplateData, bool),
}

impl fmt::Display for TemplateSpans {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TemplateSpans::Tail(td, _) => {
                write!(f, "}}{}`", format!("{}", td.trv).replace(char::is_control, "\u{2426}"))
            }
            TemplateSpans::List(tml, td, _) => write!(f, "{} }}{}`", tml, format!("{}", td.trv).replace(char::is_control, "\u{2426}")),
        }
    }
}

impl PrettyPrint for TemplateSpans {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}TemplateSpans: {}", first, self)?;
        match self {
            TemplateSpans::Tail(_, _) => Ok(()),
            TemplateSpans::List(tml, _, _) => tml.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            TemplateSpans::Tail(td, _) => pprint_token(writer, &format!("}}{}`", td.trv), TokenType::TemplateTail, pad, state),
            TemplateSpans::List(tml, td, _) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}TemplateSpans: {}", first, self)?;
                tml.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, &format!("}}{}`", td), TokenType::TemplateTail, &successive, Spot::Final)
            }
        }
    }
}

impl TemplateSpans {
    fn parse_tail(parser: &mut Parser, scanner: Scanner, tagged_flag: bool) -> ParseResult<Self> {
        let (token, after_tmplt) = scan_token(&scanner, parser.source, ScanGoal::InputElementTemplateTail);
        if let Token::TemplateTail(td) = token {
            Ok((Rc::new(TemplateSpans::Tail(td, tagged_flag)), after_tmplt))
        } else {
            Err(ParseError::new("TemplateTail expected", scanner.line, scanner.column))
        }
    }

    fn parse_tml_tail(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, tagged_flag: bool) -> ParseResult<Self> {
        let (tml, after_tml) = TemplateMiddleList::parse(parser, scanner, yield_flag, await_flag, tagged_flag)?;
        let (token, after_tmplt) = scan_token(&after_tml, parser.source, ScanGoal::InputElementTemplateTail);
        if let Token::TemplateTail(td) = token {
            Ok((Rc::new(TemplateSpans::List(tml, td, tagged_flag)), after_tmplt))
        } else {
            Err(ParseError::new("TemplateTail expected", after_tml.line, after_tml.column))
        }
    }
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, tagged_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("TemplateSpans expected", scanner.line, scanner.column))
            .otherwise(|| Self::parse_tail(parser, scanner, tagged_flag))
            .otherwise(|| Self::parse_tml_tail(parser, scanner, yield_flag, await_flag, tagged_flag))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            TemplateSpans::Tail(..) => false,
            TemplateSpans::List(tml, _, _) => tml.contains(kind),
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
            TemplateSpans::Tail(..) => true,
            TemplateSpans::List(tml, _, _) => tml.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent) -> Vec<Object> {
        // Static Semantics: Early Errors
        // TemplateSpans[Yield, Await, Tagged] : TemplateTail
        //  * It is a Syntax Error if the [Tagged] parameter was not set and TemplateTail Contains NotEscapeSequence.
        let mut errs = vec![];
        let (tail, tagged) = match self {
            TemplateSpans::Tail(tail, tagged) | TemplateSpans::List(_, tail, tagged) => (tail, *tagged),
        };
        if !tagged && tail.tv.is_none() {
            errs.push(create_syntax_error_object(agent, "invalid escape sequence in template literal"));
        }
        match self {
            TemplateSpans::Tail(..) => {}
            TemplateSpans::List(lst, ..) => {
                errs.extend(lst.early_errors(agent));
            }
        }
        errs
    }
}

// TemplateMiddleList[Yield, Await, Tagged] :
//      TemplateMiddle Expression[+In, ?Yield, ?Await]
//      TemplateMiddleList[?Yield, ?Await, ?Tagged] TemplateMiddle Expression[+In, ?Yield, ?Await]
#[derive(Debug)]
pub enum TemplateMiddleList {
    ListHead(TemplateData, Rc<Expression>, bool),
    ListMid(Rc<TemplateMiddleList>, TemplateData, Rc<Expression>, bool),
}

impl fmt::Display for TemplateMiddleList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TemplateMiddleList::ListHead(td, exp, _) => {
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
        writeln!(writer, "{}TemplateMiddleList: {}", first, self)?;
        match self {
            TemplateMiddleList::ListHead(_, exp, _) => exp.pprint_with_leftpad(writer, &successive, Spot::Final),
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
        writeln!(writer, "{}TemplateMiddleList: {}", first, self)?;
        match self {
            TemplateMiddleList::ListHead(td, exp, _) => {
                pprint_token(writer, &format!("}}{}${{", td), TokenType::TemplateMiddle, &successive, Spot::NotFinal)?;
                exp.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            TemplateMiddleList::ListMid(tml, td, exp, _) => {
                tml.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, &format!("}}{}${{", td), TokenType::TemplateMiddle, &successive, Spot::NotFinal)?;
                exp.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl TemplateMiddleList {
    fn parse_tm_exp_unboxed(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(TemplateData, Rc<Expression>, Scanner), ParseError> {
        let (middle, after_mid) = scan_token(&scanner, parser.source, ScanGoal::InputElementTemplateTail);
        if let Token::TemplateMiddle(td) = middle {
            let (exp, after_exp) = Expression::parse(parser, after_mid, true, yield_flag, await_flag)?;
            Ok((td, exp, after_exp))
        } else {
            Err(ParseError::new("TemplateMiddle expected", scanner.line, scanner.column))
        }
    }

    fn parse_tm_exp(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, tagged_flag: bool) -> ParseResult<Self> {
        let (td, exp, after_exp) = Self::parse_tm_exp_unboxed(parser, scanner, yield_flag, await_flag)?;
        Ok((Rc::new(TemplateMiddleList::ListHead(td, exp, tagged_flag)), after_exp))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, tagged_flag: bool) -> ParseResult<Self> {
        let (mut current_node, mut current_scanner) = Self::parse_tm_exp(parser, scanner, yield_flag, await_flag, tagged_flag)?;
        while let Ok((middle, exp, after)) = Self::parse_tm_exp_unboxed(parser, current_scanner, yield_flag, await_flag) {
            current_node = Rc::new(TemplateMiddleList::ListMid(current_node, middle, exp, tagged_flag));
            current_scanner = after;
        }
        Ok((current_node, current_scanner))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            TemplateMiddleList::ListHead(_, exp, _) => exp.contains(kind),
            TemplateMiddleList::ListMid(tml, _, exp, _) => tml.contains(kind) || exp.contains(kind),
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
            TemplateMiddleList::ListHead(_, exp, _) => exp.all_private_identifiers_valid(names),
            TemplateMiddleList::ListMid(tml, _, exp, _) => tml.all_private_identifiers_valid(names) && exp.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent) -> Vec<Object> {
        // Static Semantics: Early Errors
        //  TemplateMiddleList[Yield, Await, Tagged] :
        //      TemplateMiddle Expression[+In, ?Yield, ?Await]
        //      TemplateMiddleList[?Yield, ?Await, ?Tagged] TemplateMiddle Expression[+In, ?Yield, ?Await]
        //
        //  * It is a Syntax Error if the [Tagged] parameter was not set and TemplateMiddle Contains NotEscapeSequence.
        let mut errs = vec![];
        let (template_middle, tagged) = match self {
            TemplateMiddleList::ListHead(tm, _, tagged) | TemplateMiddleList::ListMid(_, tm, _, tagged) => (tm, *tagged),
        };
        if !tagged && template_middle.tv.is_none() {
            errs.push(create_syntax_error_object(agent, "Invalid escape sequence in template"));
        }
        match self {
            TemplateMiddleList::ListHead(_, exp, _) => {
                errs.extend(exp.early_errors(agent));
            }
            TemplateMiddleList::ListMid(tml, _, exp, _) => {
                errs.extend(tml.early_errors(agent));
                errs.extend(exp.early_errors(agent));
            }
        }
        errs
    }
}

// ParenthesizedExpression[Yield, Await] :
//      ( Expression[+In, ?Yield, ?Await] )
#[derive(Debug)]
pub enum ParenthesizedExpression {
    Expression(Rc<Expression>),
}

impl fmt::Display for ParenthesizedExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ParenthesizedExpression::Expression(e) = self;
        write!(f, "( {} )", e)
    }
}

impl PrettyPrint for ParenthesizedExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ParenthesizedExpression: {}", first, self)?;
        let ParenthesizedExpression::Expression(e) = self;
        e.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ParenthesizedExpression: {}", first, self)?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        let ParenthesizedExpression::Expression(e) = self;
        e.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl IsFunctionDefinition for ParenthesizedExpression {
    fn is_function_definition(&self) -> bool {
        let ParenthesizedExpression::Expression(e) = self;
        e.is_function_definition()
    }
}

impl AssignmentTargetType for ParenthesizedExpression {
    fn assignment_target_type(&self) -> ATTKind {
        let ParenthesizedExpression::Expression(e) = self;
        e.assignment_target_type()
    }
}

impl ParenthesizedExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_lp = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftParen)?;
        let (exp, after_exp) = Expression::parse(parser, after_lp, true, yield_flag, await_flag)?;
        let after_rp = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightParen)?;
        Ok((Rc::new(ParenthesizedExpression::Expression(exp)), after_rp))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let ParenthesizedExpression::Expression(e) = self;
        e.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        let ParenthesizedExpression::Expression(e) = self;
        e.all_private_identifiers_valid(names)
    }

    pub fn early_errors(&self, agent: &mut Agent) -> Vec<Object> {
        // Static Semantics: Early Errors
        // Nothing specific: just pitch to the child nodes
        let ParenthesizedExpression::Expression(e) = self;
        e.early_errors(agent)
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
pub enum CoverParenthesizedExpressionAndArrowParameterList {
    Expression(Rc<Expression>),
    ExpComma(Rc<Expression>),
    Empty,
    Ident(Rc<BindingIdentifier>),
    Pattern(Rc<BindingPattern>),
    ExpIdent(Rc<Expression>, Rc<BindingIdentifier>),
    ExpPattern(Rc<Expression>, Rc<BindingPattern>),
}

impl fmt::Display for CoverParenthesizedExpressionAndArrowParameterList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CoverParenthesizedExpressionAndArrowParameterList::Expression(node) => write!(f, "( {} )", node),
            CoverParenthesizedExpressionAndArrowParameterList::ExpComma(node) => write!(f, "( {} , )", node),
            CoverParenthesizedExpressionAndArrowParameterList::Empty => write!(f, "( )"),
            CoverParenthesizedExpressionAndArrowParameterList::Ident(node) => write!(f, "( ... {} )", node),
            CoverParenthesizedExpressionAndArrowParameterList::Pattern(node) => write!(f, "( ... {} )", node),
            CoverParenthesizedExpressionAndArrowParameterList::ExpIdent(exp, id) => {
                write!(f, "( {} , ... {} )", exp, id)
            }
            CoverParenthesizedExpressionAndArrowParameterList::ExpPattern(exp, pat) => {
                write!(f, "( {} , ... {} )", exp, pat)
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
        writeln!(writer, "{}CoverParenthesizedExpressionAndArrowParameterList: {}", first, self)?;
        match self {
            CoverParenthesizedExpressionAndArrowParameterList::Empty => Ok(()),
            CoverParenthesizedExpressionAndArrowParameterList::Expression(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            CoverParenthesizedExpressionAndArrowParameterList::ExpComma(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            CoverParenthesizedExpressionAndArrowParameterList::Ident(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            CoverParenthesizedExpressionAndArrowParameterList::Pattern(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            CoverParenthesizedExpressionAndArrowParameterList::ExpIdent(exp, next) => {
                exp.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                next.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CoverParenthesizedExpressionAndArrowParameterList::ExpPattern(exp, next) => {
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
        writeln!(writer, "{}CoverParenthesizedExpressionAndArrowParameterList: {}", first, self)?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match self {
            CoverParenthesizedExpressionAndArrowParameterList::Expression(node) => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            CoverParenthesizedExpressionAndArrowParameterList::ExpComma(node) => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
            }
            CoverParenthesizedExpressionAndArrowParameterList::Empty => {}
            CoverParenthesizedExpressionAndArrowParameterList::Ident(node) => {
                pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            CoverParenthesizedExpressionAndArrowParameterList::Pattern(node) => {
                pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            CoverParenthesizedExpressionAndArrowParameterList::ExpIdent(exp, id) => {
                exp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                id.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            CoverParenthesizedExpressionAndArrowParameterList::ExpPattern(exp, pat) => {
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
        let after_lparen = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftParen)?;
        Err(ParseError::new("Expression, spread pattern, or closing paren expected", after_lparen.line, after_lparen.column))
            .otherwise(|| {
                // ( )
                let after_rparen = scan_for_punct(after_lparen, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightParen)?;
                Ok((Rc::new(CoverParenthesizedExpressionAndArrowParameterList::Empty), after_rparen))
            })
            .otherwise(|| {
                // ( ... BindingIdentifier )
                // ( ... BindingPattern )
                let after_ellipsis = scan_for_punct(after_lparen, parser.source, ScanGoal::InputElementRegExp, Punctuator::Ellipsis)?;
                Err(ParseError::new("BindingIdentifier or BindingPattern expected", after_ellipsis.line, after_ellipsis.column)).otherwise(|| {
                    BindingIdentifier::parse(parser, after_ellipsis, yield_flag, await_flag)
                        .map(|(bi, scan)| (BndType::Id(bi), scan))
                        .otherwise(|| BindingPattern::parse(parser, after_ellipsis, yield_flag, await_flag).map(|(bp, scan)| (BndType::Pat(bp), scan)))
                        .and_then(|(bnd, scan)| scan_for_punct(scan, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen).map(|after_rp| (bnd, after_rp)))
                        .map(|(bnd, scan)| {
                            (
                                Rc::new(match bnd {
                                    BndType::Id(id) => CoverParenthesizedExpressionAndArrowParameterList::Ident(id),
                                    BndType::Pat(pat) => CoverParenthesizedExpressionAndArrowParameterList::Pattern(pat),
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
                scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)
                    .map(|after_rparen| (AfterExp::Empty, after_rparen))
                    .otherwise(|| {
                        scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma).and_then(|after_comma| {
                            scan_for_punct(after_comma, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen).map(|after_rparen| (AfterExp::Comma, after_rparen)).otherwise(
                                || {
                                    scan_for_punct(after_comma, parser.source, ScanGoal::InputElementDiv, Punctuator::Ellipsis).and_then(|after_ellipsis| {
                                        BindingIdentifier::parse(parser, after_ellipsis, yield_flag, await_flag)
                                            .and_then(|(bi, after)| {
                                                scan_for_punct(after, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen).map(|after_rp| (AfterExp::SpreadId(bi), after_rp))
                                            })
                                            .otherwise(|| {
                                                BindingPattern::parse(parser, after_ellipsis, yield_flag, await_flag).and_then(|(bp, after)| {
                                                    scan_for_punct(after, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)
                                                        .map(|after_rp| (AfterExp::SpreadPat(bp), after_rp))
                                                })
                                            })
                                    })
                                },
                            )
                        })
                    })
                    .map(|(aftexp, scan)| {
                        (
                            Rc::new(match aftexp {
                                AfterExp::Empty => CoverParenthesizedExpressionAndArrowParameterList::Expression(exp),
                                AfterExp::Comma => CoverParenthesizedExpressionAndArrowParameterList::ExpComma(exp),
                                AfterExp::SpreadId(id) => CoverParenthesizedExpressionAndArrowParameterList::ExpIdent(exp, id),
                                AfterExp::SpreadPat(pat) => CoverParenthesizedExpressionAndArrowParameterList::ExpPattern(exp, pat),
                            }),
                            scan,
                        )
                    })
            })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
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

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            CoverParenthesizedExpressionAndArrowParameterList::Expression(node) => node.contains(kind),
            CoverParenthesizedExpressionAndArrowParameterList::ExpComma(node) => node.contains(kind),
            CoverParenthesizedExpressionAndArrowParameterList::Empty => false,
            CoverParenthesizedExpressionAndArrowParameterList::Ident(node) => node.contains(kind),
            CoverParenthesizedExpressionAndArrowParameterList::Pattern(node) => node.contains(kind),
            CoverParenthesizedExpressionAndArrowParameterList::ExpIdent(exp, id) => exp.contains(kind) || id.contains(kind),
            CoverParenthesizedExpressionAndArrowParameterList::ExpPattern(exp, pat) => exp.contains(kind) || pat.contains(kind),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, strict: bool) -> Vec<Object> {
        // Static Semantics: Early Errors
        // Nothing specific: just pitch to the child nodes
        match self {
            CoverParenthesizedExpressionAndArrowParameterList::Expression(node) => node.early_errors(agent),
            CoverParenthesizedExpressionAndArrowParameterList::ExpComma(node) => node.early_errors(agent),
            CoverParenthesizedExpressionAndArrowParameterList::Empty => vec![],
            CoverParenthesizedExpressionAndArrowParameterList::Ident(node) => node.early_errors(agent, strict),
            CoverParenthesizedExpressionAndArrowParameterList::Pattern(node) => node.early_errors(agent),
            CoverParenthesizedExpressionAndArrowParameterList::ExpIdent(exp, id) => {
                let mut errs = exp.early_errors(agent);
                errs.extend(id.early_errors(agent, strict));
                errs
            }
            CoverParenthesizedExpressionAndArrowParameterList::ExpPattern(exp, pat) => {
                let mut errs = exp.early_errors(agent);
                errs.extend(pat.early_errors(agent));
                errs
            }
        }
    }
}

#[cfg(test)]
mod tests;
