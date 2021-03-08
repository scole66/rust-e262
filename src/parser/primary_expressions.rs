use num::bigint::BigInt;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

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
use super::scanner::{scan_token, Keyword, Punctuator, RegularExpressionData, ScanGoal, Scanner, TemplateData, Token};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};
use crate::strings::JSString;
use crate::values::number_to_string;

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
    IdentifierReference(Box<IdentifierReference>),
    Literal(Box<Literal>),
    ArrayLiteral(Box<ArrayLiteral>),
    ObjectLiteral(Box<ObjectLiteral>),
    Parenthesized(Box<ParenthesizedExpression>),
    TemplateLiteral(Box<TemplateLiteral>),
    Function(Box<FunctionExpression>),
    Class(Box<ClassExpression>),
    Generator(Box<GeneratorExpression>),
    AsyncFunction(Box<AsyncFunctionExpression>),
    AsyncGenerator(Box<AsyncGeneratorExpression>),
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
            PrimaryExpressionKind::RegularExpression(_) => pprint_token(writer, "regular_expression", TokenType::RegularExpression, pad, state),
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
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind;
    fn to_primary_expression_result(node: Box<Self>, scanner: Scanner) -> Result<(Box<PrimaryExpression>, Scanner), ParseError> {
        Ok((Box::new(PrimaryExpression { kind: Self::to_primary_expression_kind(node) }), scanner))
    }
}

impl ToPrimaryExpressionKind for IdentifierReference {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::IdentifierReference(node)
    }
}

impl ToPrimaryExpressionKind for Literal {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::Literal(node)
    }
}

impl ToPrimaryExpressionKind for ArrayLiteral {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::ArrayLiteral(node)
    }
}

impl ToPrimaryExpressionKind for ObjectLiteral {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::ObjectLiteral(node)
    }
}

impl ToPrimaryExpressionKind for ParenthesizedExpression {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::Parenthesized(node)
    }
}

impl ToPrimaryExpressionKind for TemplateLiteral {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::TemplateLiteral(node)
    }
}

impl ToPrimaryExpressionKind for FunctionExpression {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::Function(node)
    }
}

impl ToPrimaryExpressionKind for ClassExpression {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::Class(node)
    }
}

impl ToPrimaryExpressionKind for GeneratorExpression {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::Generator(node)
    }
}

impl ToPrimaryExpressionKind for AsyncFunctionExpression {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::AsyncFunction(node)
    }
}

impl ToPrimaryExpressionKind for AsyncGeneratorExpression {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::AsyncGenerator(node)
    }
}

impl PrimaryExpression {
    fn parse_this(parser: &mut Parser, scanner: Scanner) -> Result<(Box<Self>, Scanner), ParseError> {
        let after = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::This)?;
        Ok((Box::new(PrimaryExpression { kind: PrimaryExpressionKind::This }), after))
    }

    fn parse_idref(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (node, after) = IdentifierReference::parse(parser, scanner, yield_flag, await_flag)?;
        IdentifierReference::to_primary_expression_result(node, after)
    }

    fn parse_literal(parser: &mut Parser, scanner: Scanner) -> Result<(Box<Self>, Scanner), ParseError> {
        let (node, after) = Literal::parse(parser, scanner)?;
        Literal::to_primary_expression_result(node, after)
    }

    fn parse_array_literal(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (node, after) = ArrayLiteral::parse(parser, scanner, yield_flag, await_flag)?;
        ArrayLiteral::to_primary_expression_result(node, after)
    }

    fn parse_object_literal(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (node, after) = ObjectLiteral::parse(parser, scanner, yield_flag, await_flag)?;
        ObjectLiteral::to_primary_expression_result(node, after)
    }

    fn parse_function_exp(parser: &mut Parser, scanner: Scanner) -> Result<(Box<Self>, Scanner), ParseError> {
        let (node, after) = FunctionExpression::parse(parser, scanner)?;
        FunctionExpression::to_primary_expression_result(node, after)
    }
    fn parse_parenthesized_exp(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (node, after) = ParenthesizedExpression::parse(parser, scanner, yield_flag, await_flag)?;
        ParenthesizedExpression::to_primary_expression_result(node, after)
    }
    fn parse_template_literal(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (node, after) = TemplateLiteral::parse(parser, scanner, yield_flag, await_flag, false)?;
        TemplateLiteral::to_primary_expression_result(node, after)
    }

    fn parse_class_exp(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (node, after) = ClassExpression::parse(parser, scanner, yield_flag, await_flag)?;
        ClassExpression::to_primary_expression_result(node, after)
    }

    fn parse_generator_exp(parser: &mut Parser, scanner: Scanner) -> Result<(Box<Self>, Scanner), ParseError> {
        let (node, after) = GeneratorExpression::parse(parser, scanner)?;
        GeneratorExpression::to_primary_expression_result(node, after)
    }

    fn parse_async_func(parser: &mut Parser, scanner: Scanner) -> Result<(Box<Self>, Scanner), ParseError> {
        let (node, after) = AsyncFunctionExpression::parse(parser, scanner)?;
        AsyncFunctionExpression::to_primary_expression_result(node, after)
    }

    fn parse_async_gen(parser: &mut Parser, scanner: Scanner) -> Result<(Box<Self>, Scanner), ParseError> {
        let (node, after) = AsyncGeneratorExpression::parse(parser, scanner)?;
        AsyncGeneratorExpression::to_primary_expression_result(node, after)
    }

    fn parse_regex(parser: &mut Parser, scanner: Scanner) -> Result<(Box<Self>, Scanner), ParseError> {
        let (tok, after) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        match tok {
            Token::RegularExpression(rd) => Ok((Box::new(PrimaryExpression { kind: PrimaryExpressionKind::RegularExpression(rd) }), after)),
            _ => Err(ParseError::new("Expected regular expression", scanner.line, scanner.column)),
        }
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
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
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> Result<(Box<Elisions>, Scanner), ParseError> {
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
                    Ok((Box::new(Elisions { count: comma_count }), current_scanner))
                };
            }
            comma_count += 1;
            current_scanner = after_comma;
        }
    }
}

// SpreadElement[Yield, Await] :
//      ... AssignmentExpression[+In, ?Yield, ?Await]
#[derive(Debug)]
pub enum SpreadElement {
    AssignmentExpression(Box<AssignmentExpression>),
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
    fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<SpreadElement>, Scanner), ParseError> {
        let after_ellipsis = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Ellipsis)?;
        let (ae, after_ae) = AssignmentExpression::parse(parser, after_ellipsis, true, yield_flag, await_flag)?;
        Ok((Box::new(SpreadElement::AssignmentExpression(ae)), after_ae))
    }
}

// ElementList[Yield, Await] :
//      Elisionopt AssignmentExpression[+In, ?Yield, ?Await]
//      Elisionopt SpreadElement[?Yield, ?Await]
//      ElementList[?Yield, ?Await] , Elisionopt AssignmentExpression[+In, ?Yield, ?Await]
//      ElementList[?Yield, ?Await] , Elisionopt SpreadElement[?Yield, ?Await]
#[derive(Debug)]
pub enum ElementList {
    AssignmentExpression((Option<Box<Elisions>>, Box<AssignmentExpression>)),
    SpreadElement((Option<Box<Elisions>>, Box<SpreadElement>)),
    ElementListAssignmentExpression((Box<ElementList>, Option<Box<Elisions>>, Box<AssignmentExpression>)),
    ElementListSpreadElement((Box<ElementList>, Option<Box<Elisions>>, Box<SpreadElement>)),
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
    AE(Box<AssignmentExpression>),
    SE(Box<SpreadElement>),
}

impl ElementList {
    fn non_recursive_part(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Option<Box<Elisions>>, ELItemKind, Scanner), ParseError> {
        let pot_elision = Elisions::parse(parser, scanner);
        let (elision, after_e_scanner) = match pot_elision {
            Ok((boxed, after_elision)) => (Some(boxed), after_elision),
            Err(pe) => (None, scanner),
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

    fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<ElementList>, Scanner), ParseError> {
        let mut current_production: Box<ElementList>;
        let mut current_scanner: Scanner;

        let (elision, item, after) = Self::non_recursive_part(parser, scanner, yield_flag, await_flag)?;
        current_production = match item {
            ELItemKind::AE(boxed_ae) => Box::new(ElementList::AssignmentExpression((elision, boxed_ae))),
            ELItemKind::SE(boxed_se) => Box::new(ElementList::SpreadElement((elision, boxed_se))),
        };
        current_scanner = after;

        loop {
            let (token, after_tok) = scan_token(&current_scanner, parser.source, ScanGoal::InputElementRegExp);
            match token {
                Token::Punctuator(Punctuator::Comma) => {
                    match Self::non_recursive_part(parser, after_tok, yield_flag, await_flag) {
                        Ok((elision, item, after)) => {
                            current_production = match item {
                                ELItemKind::AE(boxed_ae) => Box::new(ElementList::ElementListAssignmentExpression((current_production, elision, boxed_ae))),
                                ELItemKind::SE(boxed_se) => Box::new(ElementList::ElementListSpreadElement((current_production, elision, boxed_se))),
                            };
                            current_scanner = after;
                        }
                        Err(_) => {
                            // Errors are swallowed in recursive productions. We just break out.
                            break;
                        }
                    }
                }
                _ => {
                    break;
                }
            }
        }
        Ok((current_production, current_scanner))
    }
}

// ArrayLiteral[Yield, Await] :
//      [ Elisionopt ]
//      [ ElementList[?Yield, ?Await] ]
//      [ ElementList[?Yield, ?Await] , Elisionopt ]
#[derive(Debug)]
pub enum ArrayLiteral {
    Empty(Option<Box<Elisions>>),
    ElementList(Box<ElementList>),
    ElementListElision(Box<ElementList>, Option<Box<Elisions>>),
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
    fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBracket)?;
        Err(ParseError::new("‘,’, ‘]’, or an ElementList expected", after.line, after.column))
            .otherwise(|| {
                let (el, after_el) = ElementList::parse(parser, after, yield_flag, await_flag)?;
                let (punct, after_punct) = scan_for_punct_set(after_el, parser.source, ScanGoal::InputElementDiv, &[Punctuator::Comma, Punctuator::RightBracket])?;
                match punct {
                    Punctuator::RightBracket => Ok((Box::new(ArrayLiteral::ElementList(el)), after_punct)),
                    Punctuator::Comma | _ => {
                        let (elisions, after_elisions) = match Elisions::parse(parser, after_punct) {
                            Ok((node, scan)) => (Some(node), scan),
                            Err(_) => (None, after_punct),
                        };
                        let end_scan = scan_for_punct(after_elisions, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBracket)?;
                        Ok((Box::new(ArrayLiteral::ElementListElision(el, elisions)), end_scan))
                    }
                }
            })
            .otherwise(|| {
                let (elisions, after_elisions) = match Elisions::parse(parser, after) {
                    Ok((node, scan)) => (Some(node), scan),
                    Err(_) => (None, after),
                };
                let end_scan = scan_for_punct(after_elisions, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBracket)?;
                Ok((Box::new(ArrayLiteral::Empty(elisions)), end_scan))
            })
    }
}

// Initializer[In, Yield, Await] :
//      = AssignmentExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum Initializer {
    AssignmentExpression(Box<AssignmentExpression>),
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<(Box<Initializer>, Scanner), ParseError> {
        let after_tok = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Eq)?;
        let (boxed_ae, after_ae) = AssignmentExpression::parse(parser, after_tok, in_flag, yield_flag, await_flag)?;
        Ok((Box::new(Initializer::AssignmentExpression(boxed_ae)), after_ae))
    }
}

// CoverInitializedName[Yield, Await] :
//      IdentifierReference[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]
#[derive(Debug)]
pub enum CoverInitializedName {
    InitializedName(Box<IdentifierReference>, Box<Initializer>),
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
    fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (idref, after_idref) = IdentifierReference::parse(parser, scanner, yield_flag, await_flag)?;
        let (izer, after_izer) = Initializer::parse(parser, after_idref, true, yield_flag, await_flag)?;
        Ok((Box::new(CoverInitializedName::InitializedName(idref, izer)), after_izer))
    }
}

// ComputedPropertyName[Yield, Await] :
//      [ AssignmentExpression[+In, ?Yield, ?Await] ]
#[derive(Debug)]
pub enum ComputedPropertyName {
    AssignmentExpression(Box<AssignmentExpression>),
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
    fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_tok = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBracket)?;
        let (ae, after_ae) = AssignmentExpression::parse(parser, after_tok, true, yield_flag, await_flag)?;
        let after_rb = scan_for_punct(after_ae, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBracket)?;
        Ok((Box::new(ComputedPropertyName::AssignmentExpression(ae)), after_rb))
    }
}

// LiteralPropertyName :
//      IdentifierName
//      StringLiteral
//      NumericLiteral
#[derive(Debug)]
pub enum LiteralPropertyName {
    IdentifierName(IdentifierData),
    StringLiteral(JSString),
    NumericLiteral(Numeric),
}

impl fmt::Display for LiteralPropertyName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LiteralPropertyName::IdentifierName(id) => write!(f, "{}", id),
            LiteralPropertyName::StringLiteral(s) => write!(f, "{:?}", s),
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
            LiteralPropertyName::StringLiteral(s) => pprint_token(writer, &format!("{:?}", s), TokenType::String, pad, state),
            LiteralPropertyName::NumericLiteral(n) => pprint_token(writer, n, TokenType::Numeric, pad, state),
        }
    }
}

impl LiteralPropertyName {
    fn parse(parser: &mut Parser, scanner: Scanner) -> Result<(Box<Self>, Scanner), ParseError> {
        let (tok, after_tok) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        match tok {
            Token::Identifier(id) => Ok((Box::new(LiteralPropertyName::IdentifierName(id)), after_tok)),
            Token::String(s) => Ok((Box::new(LiteralPropertyName::StringLiteral(s)), after_tok)),
            Token::Number(n) => Ok((Box::new(LiteralPropertyName::NumericLiteral(Numeric::Number(n))), after_tok)),
            Token::BigInt(b) => Ok((Box::new(LiteralPropertyName::NumericLiteral(Numeric::BigInt(b))), after_tok)),
            _ => Err(ParseError::new("Identifier, String, or Number expected", scanner.line, scanner.column)),
        }
    }
}

// PropertyName[Yield, Await] :
//      LiteralPropertyName
//      ComputedPropertyName[?Yield, ?Await]
#[derive(Debug)]
pub enum PropertyName {
    LiteralPropertyName(Box<LiteralPropertyName>),
    ComputedPropertyName(Box<ComputedPropertyName>),
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("PropertyName expected", scanner.line, scanner.column))
            .otherwise(|| LiteralPropertyName::parse(parser, scanner).map(|(lpn, after_lpn)| (Box::new(PropertyName::LiteralPropertyName(lpn)), after_lpn)))
            .otherwise(|| ComputedPropertyName::parse(parser, scanner, yield_flag, await_flag).map(|(cpn, after_cpn)| (Box::new(PropertyName::ComputedPropertyName(cpn)), after_cpn)))
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
    IdentifierReference(Box<IdentifierReference>),
    CoverInitializedName(Box<CoverInitializedName>),
    PropertyNameAssignmentExpression(Box<PropertyName>, Box<AssignmentExpression>),
    MethodDefinition(Box<MethodDefinition>),
    AssignmentExpression(Box<AssignmentExpression>),
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
    fn parse_pn_ae(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (pn, after_pn) = PropertyName::parse(parser, scanner, yield_flag, await_flag)?;
        let (tok, after_tok) = scan_token(&after_pn, parser.source, ScanGoal::InputElementRegExp);
        match tok {
            Token::Punctuator(Punctuator::Colon) => {
                let (ae, after_ae) = AssignmentExpression::parse(parser, after_tok, true, yield_flag, await_flag)?;
                Ok((Box::new(PropertyDefinition::PropertyNameAssignmentExpression(pn, ae)), after_ae))
            }
            _ => Err(ParseError::new("‘:’ expected", after_pn.line, after_pn.column)),
        }
    }

    fn parse_cin(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (cin, after_cin) = CoverInitializedName::parse(parser, scanner, yield_flag, await_flag)?;
        Ok((Box::new(PropertyDefinition::CoverInitializedName(cin)), after_cin))
    }

    fn parse_md(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (md, after_md) = MethodDefinition::parse(parser, scanner, yield_flag, await_flag)?;
        Ok((Box::new(PropertyDefinition::MethodDefinition(md)), after_md))
    }

    fn parse_idref(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (idref, after_idref) = IdentifierReference::parse(parser, scanner, yield_flag, await_flag)?;
        Ok((Box::new(PropertyDefinition::IdentifierReference(idref)), after_idref))
    }

    fn parse_ae(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (tok, after_tok) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        match tok {
            Token::Punctuator(Punctuator::Ellipsis) => {
                let (ae, after_ae) = AssignmentExpression::parse(parser, after_tok, true, yield_flag, await_flag)?;
                Ok((Box::new(PropertyDefinition::AssignmentExpression(ae)), after_ae))
            }
            _ => Err(ParseError::new("‘...’ expected", scanner.line, scanner.column)),
        }
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("PropertyName expected", scanner.line, scanner.column))
            .otherwise(|| Self::parse_pn_ae(parser, scanner, yield_flag, await_flag))
            .otherwise(|| Self::parse_cin(parser, scanner, yield_flag, await_flag))
            .otherwise(|| Self::parse_md(parser, scanner, yield_flag, await_flag))
            .otherwise(|| Self::parse_idref(parser, scanner, yield_flag, await_flag))
            .otherwise(|| Self::parse_ae(parser, scanner, yield_flag, await_flag))
    }
}

// PropertyDefinitionList[Yield, Await] :
//      PropertyDefinition[?Yield, ?Await]
//      PropertyDefinitionList[?Yield, ?Await] , PropertyDefinition[?Yield, ?Await]
#[derive(Debug)]
pub enum PropertyDefinitionList {
    OneDef(Box<PropertyDefinition>),
    ManyDefs(Box<PropertyDefinitionList>, Box<PropertyDefinition>),
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (pd, after_pd) = PropertyDefinition::parse(parser, scanner, yield_flag, await_flag)?;
        let mut current_production = Box::new(PropertyDefinitionList::OneDef(pd));
        let mut current_scanner = after_pd;
        loop {
            let (comma, after_comma) = scan_token(&current_scanner, parser.source, ScanGoal::InputElementRegExp);
            match comma {
                Token::Punctuator(Punctuator::Comma) => {
                    match PropertyDefinition::parse(parser, after_comma, yield_flag, await_flag) {
                        Err(_) => {
                            // Swallow errors in recursive productions
                            break;
                        }
                        Ok((pd2, after_pd2)) => {
                            current_production = Box::new(PropertyDefinitionList::ManyDefs(current_production, pd2));
                            current_scanner = after_pd2;
                        }
                    }
                }
                _ => {
                    break;
                }
            }
        }
        Ok((current_production, current_scanner))
    }
}

// ObjectLiteral[Yield, Await] :
//      { }
//      { PropertyDefinitionList[?Yield, ?Await] }
//      { PropertyDefinitionList[?Yield, ?Await] , }
#[derive(Debug)]
pub enum ObjectLiteral {
    Empty,
    Normal(Box<PropertyDefinitionList>),
    TrailingComma(Box<PropertyDefinitionList>),
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (lb, after_brace) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        match lb {
            Token::Punctuator(Punctuator::LeftBrace) => match PropertyDefinitionList::parse(parser, after_brace, yield_flag, await_flag) {
                Err(_) => {
                    let (rb, after_brace2) = scan_token(&after_brace, parser.source, ScanGoal::InputElementRegExp);
                    match rb {
                        Token::Punctuator(Punctuator::RightBrace) => Ok((Box::new(ObjectLiteral::Empty), after_brace2)),
                        _ => Err(ParseError::new("‘}’ expected", after_brace.line, after_brace.column)),
                    }
                }
                Ok((pdl, after_pdl)) => {
                    let (comma_or_brace, after_punct) = scan_token(&after_pdl, parser.source, ScanGoal::InputElementRegExp);
                    match comma_or_brace {
                        Token::Punctuator(Punctuator::RightBrace) => Ok((Box::new(ObjectLiteral::Normal(pdl)), after_punct)),
                        Token::Punctuator(Punctuator::Comma) => {
                            let (rb2, after_brace3) = scan_token(&after_punct, parser.source, ScanGoal::InputElementRegExp);
                            match rb2 {
                                Token::Punctuator(Punctuator::RightBrace) => Ok((Box::new(ObjectLiteral::TrailingComma(pdl)), after_brace3)),
                                _ => Err(ParseError::new("‘}’ expected", after_punct.line, after_punct.column)),
                            }
                        }
                        _ => Err(ParseError::new("‘,’ or ‘}’ expected", after_pdl.line, after_pdl.column)),
                    }
                }
            },
            _ => Err(ParseError::new("‘{’ expected", scanner.line, scanner.column)),
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
    StringLiteral(JSString),
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
            LiteralKind::StringLiteral(s) => write!(f, "{:?}", *s),
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
            LiteralKind::NumericLiteral(num) => pprint_token(writer, self, TokenType::Numeric, pad, state),
            LiteralKind::StringLiteral(jsstring) => pprint_token(writer, self, TokenType::String, pad, state),
        }
    }
}

impl Literal {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> Result<(Box<Literal>, Scanner), ParseError> {
        let (token, newscanner) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        match token {
            Token::Identifier(id) if id.matches(Keyword::Null) => Ok((Box::new(Literal { kind: LiteralKind::NullLiteral }), newscanner)),
            Token::Identifier(id) if id.matches(Keyword::True) => Ok((Box::new(Literal { kind: LiteralKind::BooleanLiteral(true) }), newscanner)),
            Token::Identifier(id) if id.matches(Keyword::False) => Ok((Box::new(Literal { kind: LiteralKind::BooleanLiteral(false) }), newscanner)),
            Token::Number(num) => Ok((Box::new(Literal { kind: LiteralKind::NumericLiteral(Numeric::Number(num)) }), newscanner)),
            Token::BigInt(bi) => Ok((Box::new(Literal { kind: LiteralKind::NumericLiteral(Numeric::BigInt(bi)) }), newscanner)),
            Token::String(s) => Ok((Box::new(Literal { kind: LiteralKind::StringLiteral(s) }), newscanner)),
            _ => Err(ParseError::new("Literal expected", scanner.line, scanner.column)),
        }
    }
}

// TemplateLiteral[Yield, Await, Tagged] :
//      NoSubstitutionTemplate
//      SubstitutionTemplate[?Yield, ?Await, ?Tagged]
#[derive(Debug)]
pub enum TemplateLiteral {
    NoSubstitutionTemplate(TemplateData, bool),
    SubstitutionTemplate(Box<SubstitutionTemplate>),
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
    fn parse_nst(parser: &mut Parser, scanner: Scanner, tagged_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (tok, after_nst) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        if let Token::NoSubstitutionTemplate(td) = tok {
            Ok((Box::new(TemplateLiteral::NoSubstitutionTemplate(td, tagged_flag)), after_nst))
        } else {
            Err(ParseError::new("NoSubstitutionTemplate expected", scanner.line, scanner.column))
        }
    }

    fn parse_subst(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, tagged_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (node, after) = SubstitutionTemplate::parse(parser, scanner, yield_flag, await_flag, tagged_flag)?;
        Ok((Box::new(TemplateLiteral::SubstitutionTemplate(node)), after))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, tagged_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("TemplateLiteral expected", scanner.line, scanner.column))
            .otherwise(|| Self::parse_nst(parser, scanner, tagged_flag))
            .otherwise(|| Self::parse_subst(parser, scanner, yield_flag, await_flag, tagged_flag))
    }
}

// SubstitutionTemplate[Yield, Await, Tagged] :
//      TemplateHead Expression[+In, ?Yield, ?Await] TemplateSpans[?Yield, ?Await, ?Tagged]
#[derive(Debug)]
pub struct SubstitutionTemplate {
    template_head: TemplateData,
    tagged: bool,
    expression: Box<Expression>,
    template_spans: Box<TemplateSpans>,
}

impl fmt::Display for SubstitutionTemplate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "`{}${{ {} {}", format!("{}", self.template_head), self.expression, self.template_spans)
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, tagged_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (head, after_head) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        if let Token::TemplateHead(td) = head {
            let (exp_boxed, after_exp) = Expression::parse(parser, after_head, true, yield_flag, await_flag)?;
            let (spans_boxed, after_spans) = TemplateSpans::parse(parser, after_exp, yield_flag, await_flag, tagged_flag)?;
            Ok((Box::new(SubstitutionTemplate { template_head: td, tagged: tagged_flag, expression: exp_boxed, template_spans: spans_boxed }), after_spans))
        } else {
            Err(ParseError::new("SubstitutionTemplate expected", scanner.line, scanner.column))
        }
    }
}

// TemplateSpans[Yield, Await, Tagged] :
//      TemplateTail
//      TemplateMiddleList[?Yield, ?Await, ?Tagged] TemplateTail
#[derive(Debug)]
pub enum TemplateSpans {
    Tail(TemplateData, bool),
    List(Box<TemplateMiddleList>, TemplateData, bool),
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
    fn parse_tail(parser: &mut Parser, scanner: Scanner, tagged_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (token, after_tmplt) = scan_token(&scanner, parser.source, ScanGoal::InputElementTemplateTail);
        if let Token::TemplateTail(td) = token {
            Ok((Box::new(TemplateSpans::Tail(td, tagged_flag)), after_tmplt))
        } else {
            Err(ParseError::new("TemplateTail expected", scanner.line, scanner.column))
        }
    }

    fn parse_tml_tail(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, tagged_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (tml, after_tml) = TemplateMiddleList::parse(parser, scanner, yield_flag, await_flag, tagged_flag)?;
        let (token, after_tmplt) = scan_token(&after_tml, parser.source, ScanGoal::InputElementTemplateTail);
        if let Token::TemplateTail(td) = token {
            Ok((Box::new(TemplateSpans::List(tml, td, tagged_flag)), after_tmplt))
        } else {
            Err(ParseError::new("TemplateTail expected", after_tml.line, after_tml.column))
        }
    }
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, tagged_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("TemplateSpans expected", scanner.line, scanner.column))
            .otherwise(|| Self::parse_tail(parser, scanner, tagged_flag))
            .otherwise(|| Self::parse_tml_tail(parser, scanner, yield_flag, await_flag, tagged_flag))
    }
}

// TemplateMiddleList[Yield, Await, Tagged] :
//      TemplateMiddle Expression[+In, ?Yield, ?Await]
//      TemplateMiddleList[?Yield, ?Await, ?Tagged] TemplateMiddle Expression[+In, ?Yield, ?Await]
#[derive(Debug)]
pub enum TemplateMiddleList {
    ListHead(TemplateData, Box<Expression>, bool),
    ListMid(Box<TemplateMiddleList>, TemplateData, Box<Expression>, bool),
}

impl fmt::Display for TemplateMiddleList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TemplateMiddleList::ListHead(td, exp, _) => write!(f, "}}{}${{ {}", format!("{}", td.trv).replace(char::is_control, "\u{2426}"), exp),
            TemplateMiddleList::ListMid(tml, td, exp, _) => write!(f, "{} }}{}${{ {}", tml, format!("{}", td.trv).replace(char::is_control, "\u{2426}"), exp),
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
    fn parse_tm_exp_unboxed(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(TemplateData, Box<Expression>, Scanner), ParseError> {
        let (middle, after_mid) = scan_token(&scanner, parser.source, ScanGoal::InputElementTemplateTail);
        if let Token::TemplateMiddle(td) = middle {
            let (exp, after_exp) = Expression::parse(parser, after_mid, true, yield_flag, await_flag)?;
            Ok((td, exp, after_exp))
        } else {
            Err(ParseError::new("TemplateMiddle expected", scanner.line, scanner.column))
        }
    }

    fn parse_tm_exp(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, tagged_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (td, exp, after_exp) = Self::parse_tm_exp_unboxed(parser, scanner, yield_flag, await_flag)?;
        Ok((Box::new(TemplateMiddleList::ListHead(td, exp, tagged_flag)), after_exp))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, tagged_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (mut current_node, mut current_scanner) = Self::parse_tm_exp(parser, scanner, yield_flag, await_flag, tagged_flag)?;
        loop {
            match Self::parse_tm_exp_unboxed(parser, current_scanner, yield_flag, await_flag) {
                Ok((middle, exp, after)) => {
                    current_node = Box::new(TemplateMiddleList::ListMid(current_node, middle, exp, tagged_flag));
                    current_scanner = after;
                }
                Err(_) => {
                    // Errors in recursive productions are swallowed
                    break;
                }
            }
        }
        Ok((current_node, current_scanner))
    }
}

// ParenthesizedExpression[Yield, Await] :
//      ( Expression[+In, ?Yield, ?Await] )
#[derive(Debug)]
pub enum ParenthesizedExpression {
    Expression(Box<Expression>),
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_lp = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftParen)?;
        let (exp, after_exp) = Expression::parse(parser, after_lp, true, yield_flag, await_flag)?;
        let after_rp = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightParen)?;
        Ok((Box::new(ParenthesizedExpression::Expression(exp)), after_rp))
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
    Expression(Box<Expression>),
    ExpComma(Box<Expression>),
    Empty,
    Ident(Box<BindingIdentifier>),
    Pattern(Box<BindingPattern>),
    ExpIdent(Box<Expression>, Box<BindingIdentifier>),
    ExpPattern(Box<Expression>, Box<BindingPattern>),
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        enum BndType {
            Id(Box<BindingIdentifier>),
            Pat(Box<BindingPattern>),
        }
        let after_lparen = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftParen)?;
        Err(ParseError::new("Expression, spread pattern, or closing paren expected", after_lparen.line, after_lparen.column))
            .otherwise(|| {
                // ( )
                let after_rparen = scan_for_punct(after_lparen, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightParen)?;
                Ok((Box::new(CoverParenthesizedExpressionAndArrowParameterList::Empty), after_rparen))
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
                                Box::new(match bnd {
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
                    SpreadId(Box<BindingIdentifier>),
                    SpreadPat(Box<BindingPattern>),
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
                            Box::new(match aftexp {
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
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // PRIMARY EXPRESSION
    #[test]
    fn primary_expression_test_debug() {
        let pe = PrimaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false);
        let (exp, _) = check(pe);
        assert_eq!(format!("{:?}", exp), "PrimaryExpression { kind: This }");
    }
    #[test]
    fn primary_expression_test_pprint() {
        let (pe1, _) = check(PrimaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false));
        pretty_check(&*pe1, "PrimaryExpression: this", vec![]);
        concise_check(&*pe1, "Keyword: this", vec![]);
        let (pe2, _) = check(PrimaryExpression::parse(&mut newparser("1"), Scanner::new(), false, false));
        pretty_check(&*pe2, "PrimaryExpression: 1", vec!["Literal: 1"]);
        concise_check(&*pe2, "Numeric: 1", vec![]);
        let (pe3, _) = check(PrimaryExpression::parse(&mut newparser("i"), Scanner::new(), false, false));
        pretty_check(&*pe3, "PrimaryExpression: i", vec!["IdentifierReference: i"]);
        concise_check(&*pe3, "IdentifierName: i", vec![]);
        let (pe4, _) = check(PrimaryExpression::parse(&mut newparser("[]"), Scanner::new(), false, false));
        pretty_check(&*pe4, "PrimaryExpression: [ ]", vec!["ArrayLiteral: [ ]"]);
        concise_check(&*pe4, "ArrayLiteral: [ ]", vec!["Punctuator: [", "Punctuator: ]"]);
        let (pe5, _) = check(PrimaryExpression::parse(&mut newparser("{}"), Scanner::new(), false, false));
        pretty_check(&*pe5, "PrimaryExpression: { }", vec!["ObjectLiteral: { }"]);
        concise_check(&*pe5, "ObjectLiteral: { }", vec!["Punctuator: {", "Punctuator: }"]);
        let (pe6, _) = check(PrimaryExpression::parse(&mut newparser("(a)"), Scanner::new(), false, false));
        pretty_check(&*pe6, "PrimaryExpression: ( a )", vec!["ParenthesizedExpression: ( a )"]);
        concise_check(&*pe6, "ParenthesizedExpression: ( a )", vec!["Punctuator: (", "IdentifierName: a", "Punctuator: )"]);
        let (pe7, _) = check(PrimaryExpression::parse(&mut newparser("`rust`"), Scanner::new(), false, false));
        pretty_check(&*pe7, "PrimaryExpression: `rust`", vec!["TemplateLiteral: `rust`"]);
        concise_check(&*pe7, "NoSubTemplate: `rust`", vec![]);
    }
    #[test]
    fn primary_expression_test_idref() {
        let pe_res = PrimaryExpression::parse(&mut newparser("blue"), Scanner::new(), false, false);
        let (boxed_pe, scanner) = check(pe_res);
        chk_scan(&scanner, 4);
        assert!(matches!(boxed_pe.kind, PrimaryExpressionKind::IdentifierReference(_)));
        assert_eq!(boxed_pe.is_function_definition(), false);
        assert_eq!(boxed_pe.is_identifier_reference(), true);
        assert_eq!(boxed_pe.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn primary_expression_test_literal() {
        let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("371"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(node.kind, PrimaryExpressionKind::Literal(_)));
        assert_eq!(node.is_function_definition(), false);
        assert_eq!(node.is_identifier_reference(), false);
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn primary_expression_test_this() {
        let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(node.kind, PrimaryExpressionKind::This));
        assert_eq!(node.is_function_definition(), false);
        assert_eq!(node.is_identifier_reference(), false);
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn primary_expression_test_arraylit() {
        let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("[]"), Scanner::new(), false, false));
        chk_scan(&scanner, 2);
        assert!(matches!(node.kind, PrimaryExpressionKind::ArrayLiteral(_)));
        assert_eq!(node.is_function_definition(), false);
        assert_eq!(node.is_identifier_reference(), false);
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn primary_expression_test_objlit() {
        let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("{}"), Scanner::new(), false, false));
        chk_scan(&scanner, 2);
        assert!(matches!(node.kind, PrimaryExpressionKind::ObjectLiteral(_)));
        assert_eq!(node.is_function_definition(), false);
        assert_eq!(node.is_identifier_reference(), false);
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn primary_expression_test_group() {
        let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("(a)"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(node.kind, PrimaryExpressionKind::Parenthesized(_)));
        assert_eq!(node.is_function_definition(), false);
        assert_eq!(node.is_identifier_reference(), false);
        assert_eq!(node.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn primary_expression_test_func() {
        let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("function a(){}"), Scanner::new(), false, false));
        chk_scan(&scanner, 14);
        assert!(matches!(node.kind, PrimaryExpressionKind::Function(..)));
        assert_eq!(node.is_function_definition(), true);
        assert_eq!(node.is_identifier_reference(), false);
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
        pretty_check(&*node, "PrimaryExpression: function a (  ) {  }", vec!["FunctionExpression: function a (  ) {  }"]);
        concise_check(&*node, "FunctionExpression: function a (  ) {  }", vec!["Keyword: function", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
    }
    #[test]
    fn primary_expression_test_generator() {
        let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("function *a(b){c;}"), Scanner::new(), false, false));
        chk_scan(&scanner, 18);
        assert!(matches!(node.kind, PrimaryExpressionKind::Generator(..)));
        assert_eq!(node.is_function_definition(), true);
        assert_eq!(node.is_identifier_reference(), false);
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
        pretty_check(&*node, "PrimaryExpression: function * a ( b ) { c ; }", vec!["GeneratorExpression: function * a ( b ) { c ; }"]);
        concise_check(
            &*node,
            "GeneratorExpression: function * a ( b ) { c ; }",
            vec![
                "Keyword: function",
                "Punctuator: *",
                "IdentifierName: a",
                "Punctuator: (",
                "IdentifierName: b",
                "Punctuator: )",
                "Punctuator: {",
                "ExpressionStatement: c ;",
                "Punctuator: }",
            ],
        );
    }
    #[test]
    fn primary_expression_test_async_generator() {
        let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("async function *a(b){c;}"), Scanner::new(), false, false));
        chk_scan(&scanner, 24);
        assert!(matches!(node.kind, PrimaryExpressionKind::AsyncGenerator(..)));
        assert_eq!(node.is_function_definition(), true);
        assert_eq!(node.is_identifier_reference(), false);
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
        pretty_check(&*node, "PrimaryExpression: async function * a ( b ) { c ; }", vec!["AsyncGeneratorExpression: async function * a ( b ) { c ; }"]);
        concise_check(
            &*node,
            "AsyncGeneratorExpression: async function * a ( b ) { c ; }",
            vec![
                "Keyword: async",
                "Keyword: function",
                "Punctuator: *",
                "IdentifierName: a",
                "Punctuator: (",
                "IdentifierName: b",
                "Punctuator: )",
                "Punctuator: {",
                "ExpressionStatement: c ;",
                "Punctuator: }",
            ],
        );
    }
    #[test]
    fn primary_expression_test_async_function() {
        let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("async function a(b){c;}"), Scanner::new(), false, false));
        chk_scan(&scanner, 23);
        assert!(matches!(node.kind, PrimaryExpressionKind::AsyncFunction(..)));
        assert_eq!(node.is_function_definition(), true);
        assert_eq!(node.is_identifier_reference(), false);
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
        pretty_check(&*node, "PrimaryExpression: async function a ( b ) { c ; }", vec!["AsyncFunctionExpression: async function a ( b ) { c ; }"]);
        concise_check(
            &*node,
            "AsyncFunctionExpression: async function a ( b ) { c ; }",
            vec![
                "Keyword: async",
                "Keyword: function",
                "IdentifierName: a",
                "Punctuator: (",
                "IdentifierName: b",
                "Punctuator: )",
                "Punctuator: {",
                "ExpressionStatement: c ;",
                "Punctuator: }",
            ],
        );
    }
    #[test]
    fn primary_expression_test_prettyerrors_1() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_prettyerrors_2() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_prettyerrors_3() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("null"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_prettyerrors_4() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("[]"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_prettyerrors_5() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("{}"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_prettyerrors_6() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("function (){}"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_prettyerrors_7() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("`rust`"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_prettyerrors_8() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("(0)"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_prettyerrors_9() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("function *a(){}"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_prettyerrors_10() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("async function *a(){}"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_prettyerrors_11() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("async function a(){}"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_conciseerrors_1() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_conciseerrors_2() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_conciseerrors_3() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("null"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_conciseerrors_4() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("[]"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_conciseerrors_5() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("{}"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_conciseerrors_6() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("function (){}"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_conciseerrors_7() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("`rust`"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_conciseerrors_8() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("(0)"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_conciseerrors_9() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("function *a(){}"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_conciseerrors_10() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("async function *a(){}"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn primary_expression_test_conciseerrors_11() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("async function a(){}"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }

    // LITERAL
    #[test]
    fn literal_test_debug() {
        assert_eq!(format!("{:?}", Literal { kind: LiteralKind::NullLiteral }), "Literal { kind: NullLiteral }");
    }
    #[test]
    fn literal_test_null() {
        let (lit, scanner) = check(Literal::parse(&mut newparser("null"), Scanner::new()));
        chk_scan(&scanner, 4);
        assert!(matches!(lit.kind, LiteralKind::NullLiteral));
        pretty_check(&*lit, "Literal: null", vec![]);
        concise_check(&*lit, "Keyword: null", vec![]);
    }
    #[test]
    fn literal_test_boolean_01() {
        let (lit, scanner) = check(Literal::parse(&mut newparser("true"), Scanner::new()));
        chk_scan(&scanner, 4);
        assert!(matches!(lit.kind, LiteralKind::BooleanLiteral(true)));
        pretty_check(&*lit, "Literal: true", vec![]);
        concise_check(&*lit, "Keyword: true", vec![]);
    }
    #[test]
    fn literal_test_boolean_02() {
        let (lit, scanner) = check(Literal::parse(&mut newparser("false"), Scanner::new()));
        chk_scan(&scanner, 5);
        assert!(matches!(lit.kind, LiteralKind::BooleanLiteral(false)));
        pretty_check(&*lit, "Literal: false", vec![]);
        concise_check(&*lit, "Keyword: false", vec![]);
    }
    #[test]
    fn literal_test_leading_dot() {
        let (lit, scanner) = check(Literal::parse(&mut newparser(".25"), Scanner::new()));
        chk_scan(&scanner, 3);
        assert_eq!(lit.kind, LiteralKind::NumericLiteral(Numeric::Number(0.25)));
        pretty_check(&*lit, "Literal: 0.25", vec![]);
        concise_check(&*lit, "Numeric: 0.25", vec![]);
    }
    #[test]
    fn literal_test_bigint() {
        let (lit, scanner) = check(Literal::parse(&mut newparser("7173n"), Scanner::new()));
        chk_scan(&scanner, 5);
        assert!(matches!(lit.kind, LiteralKind::NumericLiteral(Numeric::BigInt(_))));
        pretty_check(&*lit, "Literal: 7173", vec![]);
        concise_check(&*lit, "Numeric: 7173", vec![]);
        format!("{:?}", lit);
    }
    #[test]
    fn literal_test_string() {
        let (lit, scanner) = check(Literal::parse(&mut newparser("'string'"), Scanner::new()));
        chk_scan(&scanner, 8);
        assert!(matches!(lit.kind, LiteralKind::StringLiteral(_)));
        pretty_check(&*lit, "Literal: \"string\"", vec![]);
        concise_check(&*lit, "String: \"string\"", vec![]);
    }
    #[test]
    fn literal_test_keyword() {
        check_err(Literal::parse(&mut newparser("function"), Scanner::new()), "Literal expected", 1, 1);
    }
    #[test]
    fn literal_test_punct() {
        check_err(Literal::parse(&mut newparser("**"), Scanner::new()), "Literal expected", 1, 1);
    }
    #[test]
    fn literal_test_prettyerrors_1() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("null"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn literal_test_prettyerrors_2() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("true"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn literal_test_prettyerrors_3() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn literal_test_prettyerrors_4() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("'a'"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn literal_test_conciseerrors_1() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("null"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn literal_test_conciseerrors_2() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("true"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn literal_test_conciseerrors_3() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn literal_test_conciseerrors_4() {
        let (item, _) = PrimaryExpression::parse(&mut newparser("'a'"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }

    // ELISION
    #[test]
    fn elision_test_01() {
        check_err(Elisions::parse(&mut newparser(""), Scanner::new()), "Expected one or more commas", 1, 1);
    }
    #[test]
    fn elision_test_02() {
        let (e, scanner) = check(Elisions::parse(&mut newparser(",,"), Scanner::new()));
        chk_scan(&scanner, 2);
        assert!(matches!(*e, Elisions { count: 2 }));
    }
    #[test]
    fn elision_test_03() {
        let (e, scanner) = check(Elisions::parse(&mut newparser(",,,]"), Scanner::new()));
        chk_scan(&scanner, 3);
        assert!(matches!(*e, Elisions { count: 3 }));
    }
    #[test]
    fn elision_test_pprint() {
        let (e1, _) = check(Elisions::parse(&mut newparser(","), Scanner::new()));
        pretty_check(&*e1, "Elisions: ,", vec![]);
        concise_check(&*e1, "Elisions: ,", vec![]);
        let (e2, _) = check(Elisions::parse(&mut newparser(",,,,,,"), Scanner::new()));
        pretty_check(&*e2, "Elisions: , , , , , ,", vec![]);
        concise_check(&*e2, "Elisions: , , , , , ,", vec![]);
        format!("{:?}", e1);
    }
    #[test]
    fn elision_test_prettyerrors_1() {
        let (item, _) = Elisions::parse(&mut newparser(",,,"), Scanner::new()).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn elision_test_conciseerrors_1() {
        let (item, _) = Elisions::parse(&mut newparser(",,,"), Scanner::new()).unwrap();
        concise_error_validate(*item);
    }

    // SPREAD ELEMENT
    #[test]
    fn spread_element_test_empty() {
        check_err(SpreadElement::parse(&mut newparser(""), Scanner::new(), false, false), "‘...’ expected", 1, 1);
        check_err(SpreadElement::parse(&mut newparser("..."), Scanner::new(), false, false), "AssignmentExpression expected", 1, 4);
    }
    #[test]
    fn spread_element_test_assignment_expression() {
        let (se, scanner) = check(SpreadElement::parse(&mut newparser("...1"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*se, SpreadElement::AssignmentExpression(_)));
    }
    #[test]
    fn spread_element_test_pretty() {
        let (se, _) = check(SpreadElement::parse(&mut newparser("...1"), Scanner::new(), false, false));
        pretty_check(&*se, "SpreadElement: ... 1", vec!["AssignmentExpression: 1"]);
        concise_check(&*se, "SpreadElement: ... 1", vec!["Punctuator: ...", "Numeric: 1"]);
        format!("{:?}", se);
    }
    #[test]
    fn spread_element_test_prettyerrors_1() {
        let (item, _) = SpreadElement::parse(&mut newparser("...a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn spread_element_test_conciseerrors_1() {
        let (item, _) = SpreadElement::parse(&mut newparser("...a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }

    // ELEMENT LIST
    #[test]
    fn element_list_test_01() {
        check_err(ElementList::parse(&mut newparser(""), Scanner::new(), false, false), "AssignmentExpression or SpreadElement expected", 1, 1);
    }
    #[test]
    fn element_list_test_02() {
        let (el, scanner) = check(ElementList::parse(&mut newparser("3"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(*el, ElementList::AssignmentExpression((None, _))));
        pretty_check(&*el, "ElementList: 3", vec!["AssignmentExpression: 3"]);
        concise_check(&*el, "Numeric: 3", vec![]);
        format!("{:?}", *el);
    }
    #[test]
    fn element_list_test_03() {
        let (el, scanner) = check(ElementList::parse(&mut newparser(",,3"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*el, ElementList::AssignmentExpression((Some(be), _)) if be.count == 2));
        pretty_check(&*el, "ElementList: , , 3", vec!["Elisions: , ,", "AssignmentExpression: 3"]);
        concise_check(&*el, "ElementList: , , 3", vec!["Elisions: , ,", "Numeric: 3"]);
    }
    #[test]
    fn element_list_test_05() {
        let (el, scanner) = check(ElementList::parse(&mut newparser("...a"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*el, ElementList::SpreadElement((None, _))));
        pretty_check(&*el, "ElementList: ... a", vec!["SpreadElement: ... a"]);
        concise_check(&*el, "SpreadElement: ... a", vec!["Punctuator: ...", "IdentifierName: a"]);
    }
    #[test]
    fn element_list_test_06() {
        let (el, scanner) = check(ElementList::parse(&mut newparser(",,...a"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(&*el, ElementList::SpreadElement((Some(be), _)) if be.count == 2));
        pretty_check(&*el, "ElementList: , , ... a", vec!["Elisions: , ,", "SpreadElement: ... a"]);
        concise_check(&*el, "ElementList: , , ... a", vec!["Elisions: , ,", "SpreadElement: ... a"]);
    }
    #[test]
    fn element_list_test_07() {
        let (el, scanner) = check(ElementList::parse(&mut newparser("a,b"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*el, ElementList::ElementListAssignmentExpression((_, None, _))));
        pretty_check(&*el, "ElementList: a , b", vec!["ElementList: a", "AssignmentExpression: b"]);
        concise_check(&*el, "ElementList: a , b", vec!["IdentifierName: a", "Punctuator: ,", "IdentifierName: b"]);
    }
    #[test]
    fn element_list_test_08() {
        let (el, scanner) = check(ElementList::parse(&mut newparser("a,,b"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*el, ElementList::ElementListAssignmentExpression((_, Some(be), _)) if be.count == 1));
        pretty_check(&*el, "ElementList: a , , b", vec!["ElementList: a", "Elisions: ,", "AssignmentExpression: b"]);
        concise_check(&*el, "ElementList: a , , b", vec!["IdentifierName: a", "Punctuator: ,", "Elisions: ,", "IdentifierName: b"]);
    }
    #[test]
    fn element_list_test_09() {
        let (el, scanner) = check(ElementList::parse(&mut newparser("a,...b"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(&*el, ElementList::ElementListSpreadElement((_, None, _))));
        pretty_check(&*el, "ElementList: a , ... b", vec!["ElementList: a", "SpreadElement: ... b"]);
        concise_check(&*el, "ElementList: a , ... b", vec!["IdentifierName: a", "Punctuator: ,", "SpreadElement: ... b"]);
    }
    #[test]
    fn element_list_test_10() {
        let (el, scanner) = check(ElementList::parse(&mut newparser("a,,...b"), Scanner::new(), false, false));
        chk_scan(&scanner, 7);
        assert!(matches!(&*el, ElementList::ElementListSpreadElement((_, Some(be), _)) if be.count == 1));
        pretty_check(&*el, "ElementList: a , , ... b", vec!["ElementList: a", "Elisions: ,", "SpreadElement: ... b"]);
        concise_check(&*el, "ElementList: a , , ... b", vec!["IdentifierName: a", "Punctuator: ,", "Elisions: ,", "SpreadElement: ... b"]);
    }
    #[test]
    fn element_list_test_04() {
        let (el, scanner) = check(ElementList::parse(&mut newparser("0,"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(*el, ElementList::AssignmentExpression((None, _))));
    }
    #[test]
    fn element_list_test_11() {
        check_err(ElementList::parse(&mut newparser(",,,,"), Scanner::new(), false, false), "AssignmentExpression or SpreadElement expected", 1, 5);
    }
    #[test]
    fn element_list_test_12() {
        check_err(ElementList::parse(&mut newparser("...@"), Scanner::new(), false, false), "AssignmentExpression expected", 1, 4);
    }
    #[test]
    fn element_list_test_13() {
        check_err(ElementList::parse(&mut newparser("(while)"), Scanner::new(), false, false), "Expression, spread pattern, or closing paren expected", 1, 2);
    }
    #[test]
    fn element_list_test_prettyerrors_1() {
        let (item, _) = ElementList::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn element_list_test_prettyerrors_2() {
        let (item, _) = ElementList::parse(&mut newparser(",,,a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn element_list_test_prettyerrors_3() {
        let (item, _) = ElementList::parse(&mut newparser("...a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn element_list_test_prettyerrors_4() {
        let (item, _) = ElementList::parse(&mut newparser(",,,...a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn element_list_test_prettyerrors_5() {
        let (item, _) = ElementList::parse(&mut newparser("a,b"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn element_list_test_prettyerrors_6() {
        let (item, _) = ElementList::parse(&mut newparser("a,,,b"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn element_list_test_prettyerrors_7() {
        let (item, _) = ElementList::parse(&mut newparser("a,...b"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn element_list_test_prettyerrors_8() {
        let (item, _) = ElementList::parse(&mut newparser("a,,,...b"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn element_list_test_conciseerrors_1() {
        let (item, _) = ElementList::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn element_list_test_conciseerrors_2() {
        let (item, _) = ElementList::parse(&mut newparser(",,,a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn element_list_test_conciseerrors_3() {
        let (item, _) = ElementList::parse(&mut newparser("...a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn element_list_test_conciseerrors_4() {
        let (item, _) = ElementList::parse(&mut newparser(",,,...a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn element_list_test_conciseerrors_5() {
        let (item, _) = ElementList::parse(&mut newparser("a,b"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn element_list_test_conciseerrors_6() {
        let (item, _) = ElementList::parse(&mut newparser("a,,,b"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn element_list_test_conciseerrors_7() {
        let (item, _) = ElementList::parse(&mut newparser("a,...b"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn element_list_test_conciseerrors_8() {
        let (item, _) = ElementList::parse(&mut newparser("a,,,...b"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }

    // ARRAY LITERAL
    #[test]
    fn array_literal_test_01() {
        let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[]"), Scanner::new(), false, false));
        chk_scan(&scanner, 2);
        assert!(matches!(&*al, ArrayLiteral::Empty(None)));
        pretty_check(&*al, "ArrayLiteral: [ ]", vec![]);
        concise_check(&*al, "ArrayLiteral: [ ]", vec!["Punctuator: [", "Punctuator: ]"]);
        format!("{:?}", &*al);
    }
    #[test]
    fn array_literal_test_02() {
        let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[,]"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*al, ArrayLiteral::Empty(Some(be)) if be.count == 1));
        pretty_check(&*al, "ArrayLiteral: [ , ]", vec!["Elisions: ,"]);
        concise_check(&*al, "ArrayLiteral: [ , ]", vec!["Punctuator: [", "Elisions: ,", "Punctuator: ]"]);
    }
    #[test]
    fn array_literal_test_03() {
        let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[a]"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*al, ArrayLiteral::ElementList(_)));
        pretty_check(&*al, "ArrayLiteral: [ a ]", vec!["ElementList: a"]);
        concise_check(&*al, "ArrayLiteral: [ a ]", vec!["Punctuator: [", "IdentifierName: a", "Punctuator: ]"]);
    }
    #[test]
    fn array_literal_test_04() {
        let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[a,]"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*al, ArrayLiteral::ElementListElision(_, None)));
        pretty_check(&*al, "ArrayLiteral: [ a , ]", vec!["ElementList: a"]);
        concise_check(&*al, "ArrayLiteral: [ a , ]", vec!["Punctuator: [", "IdentifierName: a", "Punctuator: ,", "Punctuator: ]"])
    }
    #[test]
    fn array_literal_test_05() {
        let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[a,,]"), Scanner::new(), false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*al, ArrayLiteral::ElementListElision(_, Some(be)) if be.count == 1));
        pretty_check(&*al, "ArrayLiteral: [ a , , ]", vec!["ElementList: a", "Elisions: ,"]);
        concise_check(&*al, "ArrayLiteral: [ a , , ]", vec!["Punctuator: [", "IdentifierName: a", "Punctuator: ,", "Elisions: ,", "Punctuator: ]"]);
    }
    #[test]
    fn array_literal_test_err_01() {
        check_err(ArrayLiteral::parse(&mut newparser(""), Scanner::new(), false, false), "‘[’ expected", 1, 1);
    }
    #[test]
    fn array_literal_test_err_02() {
        check_err(ArrayLiteral::parse(&mut newparser("["), Scanner::new(), false, false), "‘,’, ‘]’, or an ElementList expected", 1, 2);
    }
    #[test]
    fn array_literal_test_err_03() {
        check_err(ArrayLiteral::parse(&mut newparser("[,,"), Scanner::new(), false, false), "AssignmentExpression or SpreadElement expected", 1, 4);
    }
    #[test]
    fn array_literal_test_err_04() {
        check_err(ArrayLiteral::parse(&mut newparser("[a"), Scanner::new(), false, false), "One of [‘,’, ‘]’] expected", 1, 3);
    }
    #[test]
    fn array_literal_test_err_05() {
        check_err(ArrayLiteral::parse(&mut newparser("[a,"), Scanner::new(), false, false), "‘]’ expected", 1, 4);
    }
    #[test]
    fn array_literal_test_err_06() {
        check_err(ArrayLiteral::parse(&mut newparser("[a,,"), Scanner::new(), false, false), "‘]’ expected", 1, 5);
    }
    #[test]
    fn array_literal_test_prettyerrors_1() {
        let (item, _) = ArrayLiteral::parse(&mut newparser("[]"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn array_literal_test_prettyerrors_2() {
        let (item, _) = ArrayLiteral::parse(&mut newparser("[,,,]"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn array_literal_test_prettyerrors_3() {
        let (item, _) = ArrayLiteral::parse(&mut newparser("[a]"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn array_literal_test_prettyerrors_4() {
        let (item, _) = ArrayLiteral::parse(&mut newparser("[a,]"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn array_literal_test_prettyerrors_5() {
        let (item, _) = ArrayLiteral::parse(&mut newparser("[a,,,,]"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn array_literal_test_conciseerrors_1() {
        let (item, _) = ArrayLiteral::parse(&mut newparser("[]"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn array_literal_test_conciseerrors_2() {
        let (item, _) = ArrayLiteral::parse(&mut newparser("[,,,]"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn array_literal_test_conciseerrors_3() {
        let (item, _) = ArrayLiteral::parse(&mut newparser("[a]"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn array_literal_test_conciseerrors_4() {
        let (item, _) = ArrayLiteral::parse(&mut newparser("[a,]"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn array_literal_test_conciseerrors_5() {
        let (item, _) = ArrayLiteral::parse(&mut newparser("[a,,,,]"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }

    // INITIALIZER
    #[test]
    fn initializer_test_nomatch() {
        check_err(Initializer::parse(&mut newparser(""), Scanner::new(), false, false, false), "‘=’ expected", 1, 1);
        check_err(Initializer::parse(&mut newparser("="), Scanner::new(), false, false, false), "AssignmentExpression expected", 1, 2);
    }
    #[test]
    fn initializer_test_01() {
        let (izer, scanner) = check(Initializer::parse(&mut newparser("=a"), Scanner::new(), false, false, false));
        chk_scan(&scanner, 2);
        assert!(matches!(&*izer, Initializer::AssignmentExpression(_)));
        pretty_check(&*izer, "Initializer: = a", vec!["AssignmentExpression: a"]);
        concise_check(&*izer, "Initializer: = a", vec!["Punctuator: =", "IdentifierName: a"]);
        format!("{:?}", *izer);
    }
    #[test]
    fn initializer_test_prettyerrors_1() {
        let (item, _) = Initializer::parse(&mut newparser("=2"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn initializer_test_conciseerrors_1() {
        let (item, _) = Initializer::parse(&mut newparser("=2"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(*item);
    }

    // COVER INITIALIZED NAME
    #[test]
    fn cover_initialized_name_test_nomatch_1() {
        check_err(CoverInitializedName::parse(&mut newparser(""), Scanner::new(), false, false), "Not an identifier", 1, 1);
    }
    #[test]
    fn cover_initialized_name_test_nomatch_2() {
        check_err(CoverInitializedName::parse(&mut newparser("a"), Scanner::new(), false, false), "‘=’ expected", 1, 2);
    }
    #[test]
    fn cover_initialized_name_test_01() {
        let (cin, scanner) = check(CoverInitializedName::parse(&mut newparser("a=b"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*cin, CoverInitializedName::InitializedName(_, _)));
        pretty_check(&*cin, "CoverInitializedName: a = b", vec!["IdentifierReference: a", "Initializer: = b"]);
        concise_check(&*cin, "CoverInitializedName: a = b", vec!["IdentifierName: a", "Initializer: = b"]);
        format!("{:?}", *cin);
    }
    #[test]
    fn cover_initialized_name_test_prettyerrors_1() {
        let (item, _) = CoverInitializedName::parse(&mut newparser("a=2"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn cover_initialized_name_test_conciseerrors_1() {
        let (item, _) = CoverInitializedName::parse(&mut newparser("a=2"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }

    // COMPUTED PROPERTY NAME
    #[test]
    fn computed_property_name_test_nomatch_1() {
        check_err(ComputedPropertyName::parse(&mut newparser(""), Scanner::new(), false, false), "‘[’ expected", 1, 1);
    }
    #[test]
    fn computed_property_name_test_nomatch_2() {
        check_err(ComputedPropertyName::parse(&mut newparser("["), Scanner::new(), false, false), "AssignmentExpression expected", 1, 2);
    }
    #[test]
    fn computed_property_name_test_nomatch_3() {
        check_err(ComputedPropertyName::parse(&mut newparser("[a"), Scanner::new(), false, false), "‘]’ expected", 1, 3);
    }
    #[test]
    fn computed_property_name_test_01() {
        let (cpn, scanner) = check(ComputedPropertyName::parse(&mut newparser("[a]"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*cpn, ComputedPropertyName::AssignmentExpression(_)));
        pretty_check(&*cpn, "ComputedPropertyName: [ a ]", vec!["AssignmentExpression: a"]);
        concise_check(&*cpn, "ComputedPropertyName: [ a ]", vec!["Punctuator: [", "IdentifierName: a", "Punctuator: ]"]);
        format!("{:?}", &*cpn);
    }
    #[test]
    fn computed_property_name_test_prettyerrors_1() {
        let (item, _) = ComputedPropertyName::parse(&mut newparser("[4]"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn computed_property_name_test_conciseerrors_1() {
        let (item, _) = ComputedPropertyName::parse(&mut newparser("[4]"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }

    // LITERAL PROPERTY NAME
    #[test]
    fn literal_property_name_test_none() {
        check_err(LiteralPropertyName::parse(&mut newparser(""), Scanner::new()), "Identifier, String, or Number expected", 1, 1);
    }
    #[test]
    fn literal_property_name_test_01() {
        let (lpn, scanner) = check(LiteralPropertyName::parse(&mut newparser("b"), Scanner::new()));
        chk_scan(&scanner, 1);
        assert!(matches!(&*lpn, LiteralPropertyName::IdentifierName(_)));
        pretty_check(&*lpn, "LiteralPropertyName: b", vec![]);
        concise_check(&*lpn, "IdentifierName: b", vec![]);
        format!("{:?}", *lpn);
    }
    #[test]
    fn literal_property_name_test_02() {
        let (lpn, scanner) = check(LiteralPropertyName::parse(&mut newparser("'b'"), Scanner::new()));
        chk_scan(&scanner, 3);
        assert!(matches!(&*lpn, LiteralPropertyName::StringLiteral(_)));
        pretty_check(&*lpn, "LiteralPropertyName: \"b\"", vec![]);
        concise_check(&*lpn, "String: \"b\"", vec![]);
    }
    #[test]
    fn literal_property_name_test_03() {
        let (lpn, scanner) = check(LiteralPropertyName::parse(&mut newparser("0"), Scanner::new()));
        chk_scan(&scanner, 1);
        assert!(matches!(&*lpn, LiteralPropertyName::NumericLiteral(_)));
        pretty_check(&*lpn, "LiteralPropertyName: 0", vec![]);
        concise_check(&*lpn, "Numeric: 0", vec![]);
    }
    #[test]
    fn literal_property_name_test_04() {
        let (lpn, scanner) = check(LiteralPropertyName::parse(&mut newparser("1n"), Scanner::new()));
        chk_scan(&scanner, 2);
        assert!(matches!(&*lpn, LiteralPropertyName::NumericLiteral(_)));
        pretty_check(&*lpn, "LiteralPropertyName: 1", vec![]);
        concise_check(&*lpn, "Numeric: 1", vec![]);
    }
    #[test]
    fn literal_property_name_test_prettyerrors_1() {
        let (item, _) = LiteralPropertyName::parse(&mut newparser("a"), Scanner::new()).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn literal_property_name_test_prettyerrors_2() {
        let (item, _) = LiteralPropertyName::parse(&mut newparser("0"), Scanner::new()).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn literal_property_name_test_prettyerrors_3() {
        let (item, _) = LiteralPropertyName::parse(&mut newparser("'a'"), Scanner::new()).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn literal_property_name_test_conciseerrors_1() {
        let (item, _) = LiteralPropertyName::parse(&mut newparser("a"), Scanner::new()).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn literal_property_name_test_conciseerrors_2() {
        let (item, _) = LiteralPropertyName::parse(&mut newparser("0"), Scanner::new()).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn literal_property_name_test_conciseerrors_3() {
        let (item, _) = LiteralPropertyName::parse(&mut newparser("'a'"), Scanner::new()).unwrap();
        concise_error_validate(*item);
    }

    // PROPERTY NAME
    #[test]
    fn property_name_test_nomatch() {
        check_err(PropertyName::parse(&mut newparser(""), Scanner::new(), false, false), "PropertyName expected", 1, 1);
    }
    #[test]
    fn property_name_test_01() {
        let (pn, scanner) = check(PropertyName::parse(&mut newparser("a"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, PropertyName::LiteralPropertyName(_)));
        pretty_check(&*pn, "PropertyName: a", vec!["LiteralPropertyName: a"]);
        concise_check(&*pn, "IdentifierName: a", vec![]);
        format!("{:?}", *pn);
    }
    #[test]
    fn property_name_test_02() {
        let (pn, scanner) = check(PropertyName::parse(&mut newparser("[a]"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*pn, PropertyName::ComputedPropertyName(_)));
        pretty_check(&*pn, "PropertyName: [ a ]", vec!["ComputedPropertyName: [ a ]"]);
        concise_check(&*pn, "ComputedPropertyName: [ a ]", vec!["Punctuator: [", "IdentifierName: a", "Punctuator: ]"]);
        format!("{:?}", *pn);
    }
    #[test]
    fn property_name_test_prettyerrors_1() {
        let (item, _) = PropertyName::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn property_name_test_prettyerrors_2() {
        let (item, _) = PropertyName::parse(&mut newparser("[0]"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn property_name_test_conciseerrors_1() {
        let (item, _) = PropertyName::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn property_name_test_conciseerrors_2() {
        let (item, _) = PropertyName::parse(&mut newparser("[0]"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }

    // PROPERTY DEFINITION
    #[test]
    fn property_definition_test_01() {
        let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("a"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pd, PropertyDefinition::IdentifierReference(_)));
        pretty_check(&*pd, "PropertyDefinition: a", vec!["IdentifierReference: a"]);
        concise_check(&*pd, "IdentifierName: a", vec![]);
        format!("{:?}", *pd);
    }
    #[test]
    fn property_definition_test_02() {
        let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("a=b"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*pd, PropertyDefinition::CoverInitializedName(_)));
        pretty_check(&*pd, "PropertyDefinition: a = b", vec!["CoverInitializedName: a = b"]);
        concise_check(&*pd, "CoverInitializedName: a = b", vec!["IdentifierName: a", "Initializer: = b"]);
    }
    #[test]
    fn property_definition_test_03() {
        let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("a:b"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*pd, PropertyDefinition::PropertyNameAssignmentExpression(_, _)));
        pretty_check(&*pd, "PropertyDefinition: a : b", vec!["PropertyName: a", "AssignmentExpression: b"]);
        concise_check(&*pd, "PropertyDefinition: a : b", vec!["IdentifierName: a", "Punctuator: :", "IdentifierName: b"]);
    }
    #[test]
    fn property_definition_test_04() {
        let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("...a"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*pd, PropertyDefinition::AssignmentExpression(_)));
        pretty_check(&*pd, "PropertyDefinition: ... a", vec!["AssignmentExpression: a"]);
        concise_check(&*pd, "PropertyDefinition: ... a", vec!["Punctuator: ...", "IdentifierName: a"]);
    }
    #[test]
    fn property_definition_test_05() {
        let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("a(){}"), Scanner::new(), false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*pd, PropertyDefinition::MethodDefinition(..)));
        pretty_check(&*pd, "PropertyDefinition: a (  ) {  }", vec!["MethodDefinition: a (  ) {  }"]);
        concise_check(&*pd, "MethodDefinition: a (  ) {  }", vec!["IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
    }
    #[test]
    fn property_definition_test_nomatch_1() {
        check_err(PropertyDefinition::parse(&mut newparser(""), Scanner::new(), false, false), "PropertyName expected", 1, 1);
    }
    #[test]
    fn property_definition_test_nomatch_2() {
        check_err(PropertyDefinition::parse(&mut newparser("..."), Scanner::new(), false, false), "AssignmentExpression expected", 1, 4);
    }
    #[test]
    fn property_definition_test_nomatch_3() {
        check_err(PropertyDefinition::parse(&mut newparser("3"), Scanner::new(), false, false), "‘:’ expected", 1, 2);
    }
    #[test]
    fn property_definition_test_nomatch_4() {
        check_err(PropertyDefinition::parse(&mut newparser("3:"), Scanner::new(), false, false), "AssignmentExpression expected", 1, 3);
    }
    #[test]
    fn property_definition_test_prettyerrors_1() {
        let (item, _) = PropertyDefinition::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn property_definition_test_prettyerrors_2() {
        let (item, _) = PropertyDefinition::parse(&mut newparser("a=2"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn property_definition_test_prettyerrors_3() {
        let (item, _) = PropertyDefinition::parse(&mut newparser("a:2"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn property_definition_test_prettyerrors_4() {
        let (item, _) = PropertyDefinition::parse(&mut newparser("a(){}"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn property_definition_test_prettyerrors_5() {
        let (item, _) = PropertyDefinition::parse(&mut newparser("...a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn property_definition_test_conciseerrors_1() {
        let (item, _) = PropertyDefinition::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn property_definition_test_conciseerrors_2() {
        let (item, _) = PropertyDefinition::parse(&mut newparser("a=2"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn property_definition_test_conciseerrors_3() {
        let (item, _) = PropertyDefinition::parse(&mut newparser("a:2"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn property_definition_test_conciseerrors_4() {
        let (item, _) = PropertyDefinition::parse(&mut newparser("a(){}"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn property_definition_test_conciseerrors_5() {
        let (item, _) = PropertyDefinition::parse(&mut newparser("...a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }

    // PROPERTY DEFINITION LIST
    #[test]
    fn property_definition_list_test_01() {
        let (pdl, scanner) = check(PropertyDefinitionList::parse(&mut newparser("a"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pdl, PropertyDefinitionList::OneDef(_)));
        pretty_check(&*pdl, "PropertyDefinitionList: a", vec!["PropertyDefinition: a"]);
        concise_check(&*pdl, "IdentifierName: a", vec![]);
        format!("{:?}", *pdl);
    }
    #[test]
    fn property_definition_list_test_02() {
        let (pdl, scanner) = check(PropertyDefinitionList::parse(&mut newparser("a,"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pdl, PropertyDefinitionList::OneDef(_)));
        pretty_check(&*pdl, "PropertyDefinitionList: a", vec!["PropertyDefinition: a"]);
        concise_check(&*pdl, "IdentifierName: a", vec![]);
    }
    #[test]
    fn property_definition_list_test_03() {
        let (pdl, scanner) = check(PropertyDefinitionList::parse(&mut newparser("a,b"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*pdl, PropertyDefinitionList::ManyDefs(_, _)));
        pretty_check(&*pdl, "PropertyDefinitionList: a , b", vec!["PropertyDefinitionList: a", "PropertyDefinition: b"]);
        concise_check(&*pdl, "PropertyDefinitionList: a , b", vec!["IdentifierName: a", "Punctuator: ,", "IdentifierName: b"]);
    }
    #[test]
    fn property_definition_list_test_04() {
        check_err(PropertyDefinitionList::parse(&mut newparser(""), Scanner::new(), false, false), "PropertyName expected", 1, 1);
    }
    #[test]
    fn property_definition_list_test_prettyerrors_1() {
        let (item, _) = PropertyDefinitionList::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn property_definition_list_test_prettyerrors_2() {
        let (item, _) = PropertyDefinitionList::parse(&mut newparser("a,b"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn property_definition_list_test_conciseerrors_1() {
        let (item, _) = PropertyDefinitionList::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn property_definition_list_test_conciseerrors_2() {
        let (item, _) = PropertyDefinitionList::parse(&mut newparser("a,b"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }

    // OBJECT LITERAL
    #[test]
    fn object_literal_test_01() {
        let (ol, scanner) = check(ObjectLiteral::parse(&mut newparser("{}"), Scanner::new(), false, false));
        chk_scan(&scanner, 2);
        assert!(matches!(&*ol, ObjectLiteral::Empty));
        pretty_check(&*ol, "ObjectLiteral: { }", vec![]);
        concise_check(&*ol, "ObjectLiteral: { }", vec!["Punctuator: {", "Punctuator: }"]);
        format!("{:?}", *ol);
    }
    #[test]
    fn object_literal_test_02() {
        let (ol, scanner) = check(ObjectLiteral::parse(&mut newparser("{a:b}"), Scanner::new(), false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*ol, ObjectLiteral::Normal(_)));
        pretty_check(&*ol, "ObjectLiteral: { a : b }", vec!["PropertyDefinitionList: a : b"]);
        concise_check(&*ol, "ObjectLiteral: { a : b }", vec!["Punctuator: {", "PropertyDefinition: a : b", "Punctuator: }"]);
    }
    #[test]
    fn object_literal_test_03() {
        let (ol, scanner) = check(ObjectLiteral::parse(&mut newparser("{a:b,}"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(&*ol, ObjectLiteral::TrailingComma(_)));
        pretty_check(&*ol, "ObjectLiteral: { a : b , }", vec!["PropertyDefinitionList: a : b"]);
        concise_check(&*ol, "ObjectLiteral: { a : b , }", vec!["Punctuator: {", "PropertyDefinition: a : b", "Punctuator: ,", "Punctuator: }"]);
    }
    #[test]
    fn object_literal_test_04() {
        check_err(ObjectLiteral::parse(&mut newparser(""), Scanner::new(), false, false), "‘{’ expected", 1, 1);
    }
    #[test]
    fn object_literal_test_05() {
        check_err(ObjectLiteral::parse(&mut newparser("{"), Scanner::new(), false, false), "‘}’ expected", 1, 2);
    }
    #[test]
    fn object_literal_test_06() {
        check_err(ObjectLiteral::parse(&mut newparser("{a:b"), Scanner::new(), false, false), "‘,’ or ‘}’ expected", 1, 5);
    }
    #[test]
    fn object_literal_test_07() {
        check_err(ObjectLiteral::parse(&mut newparser("{a:b,"), Scanner::new(), false, false), "‘}’ expected", 1, 6);
    }
    #[test]
    fn object_literal_test_prettyerrors_1() {
        let (item, _) = ObjectLiteral::parse(&mut newparser("{}"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn object_literal_test_prettyerrors_2() {
        let (item, _) = ObjectLiteral::parse(&mut newparser("{a:b}"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn object_literal_test_prettyerrors_3() {
        let (item, _) = ObjectLiteral::parse(&mut newparser("{A:B,}"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn object_literal_test_conciseerrors_1() {
        let (item, _) = ObjectLiteral::parse(&mut newparser("{}"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn object_literal_test_conciseerrors_2() {
        let (item, _) = ObjectLiteral::parse(&mut newparser("{a:b}"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn object_literal_test_conciseerrors_3() {
        let (item, _) = ObjectLiteral::parse(&mut newparser("{A:B,}"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }

    // PARENTHESIZED EXPRESSION
    #[test]
    fn parenthesized_expression_test_01() {
        let (pe, scanner) = check(ParenthesizedExpression::parse(&mut newparser("(a)"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*pe, ParenthesizedExpression::Expression(_)));
        pretty_check(&*pe, "ParenthesizedExpression: ( a )", vec!["Expression: a"]);
        concise_check(&*pe, "ParenthesizedExpression: ( a )", vec!["Punctuator: (", "IdentifierName: a", "Punctuator: )"]);
        format!("{:?}", pe);
        assert_eq!(pe.is_function_definition(), false);
        assert_eq!(pe.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn parenthesized_expression_test_02() {
        check_err(ParenthesizedExpression::parse(&mut newparser(""), Scanner::new(), false, false), "‘(’ expected", 1, 1);
    }
    #[test]
    fn parenthesized_expression_test_03() {
        check_err(ParenthesizedExpression::parse(&mut newparser("("), Scanner::new(), false, false), "Expression expected", 1, 2);
    }
    #[test]
    fn parenthesized_expression_test_04() {
        check_err(ParenthesizedExpression::parse(&mut newparser("(0"), Scanner::new(), false, false), "‘)’ expected", 1, 3);
    }
    #[test]
    fn parenthesized_expression_test_prettyerrors_1() {
        let (item, _) = ParenthesizedExpression::parse(&mut newparser("(0)"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn parenthesized_expression_test_conciseerrors_1() {
        let (item, _) = ParenthesizedExpression::parse(&mut newparser("(0)"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }

    // TEMPLATE MIDDLE LIST
    #[test]
    fn template_middle_list_test_01() {
        let (tml, scanner) = check(TemplateMiddleList::parse(&mut newparser("}a${0"), Scanner::new(), false, false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*tml, TemplateMiddleList::ListHead(_, _, _)));
        pretty_check(&*tml, "TemplateMiddleList: }a${ 0", vec!["Expression: 0"]);
        concise_check(&*tml, "TemplateMiddleList: }a${ 0", vec!["TemplateMiddle: }a${", "Numeric: 0"]);
        format!("{:?}", tml);
    }
    #[test]
    fn template_middle_list_test_02() {
        let (tml, scanner) = check(TemplateMiddleList::parse(&mut newparser("}${a}${b}"), Scanner::new(), false, false, false));
        chk_scan(&scanner, 8);
        println!("{:?}", tml);
        assert!(matches!(&*tml, TemplateMiddleList::ListMid(_, _, _, _)));
        pretty_check(&*tml, "TemplateMiddleList: }${ a }${ b", vec!["TemplateMiddleList: }${ a", "Expression: b"]);
        concise_check(&*tml, "TemplateMiddleList: }${ a }${ b", vec!["TemplateMiddleList: }${ a", "TemplateMiddle: }${", "IdentifierName: b"]);
        format!("{:?}", tml);
    }
    #[test]
    fn template_middle_list_test_03() {
        check_err(TemplateMiddleList::parse(&mut newparser(""), Scanner::new(), false, false, false), "TemplateMiddle expected", 1, 1);
        check_err(TemplateMiddleList::parse(&mut newparser("}abc${@"), Scanner::new(), false, false, false), "Expression expected", 1, 7);
    }
    #[test]
    fn template_middle_list_test_04() {
        let (tml, scanner) = check(TemplateMiddleList::parse(&mut newparser("}${a}${@}"), Scanner::new(), false, false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*tml, TemplateMiddleList::ListHead(_, _, _)));
        pretty_check(&*tml, "TemplateMiddleList: }${ a", vec!["Expression: a"]);
        concise_check(&*tml, "TemplateMiddleList: }${ a", vec!["TemplateMiddle: }${", "IdentifierName: a"]);
        format!("{:?}", tml);
    }
    #[test]
    fn template_middle_list_test_prettyerrors_1() {
        let (item, _) = TemplateMiddleList::parse(&mut newparser("}${0"), Scanner::new(), false, false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn template_middle_list_test_prettyerrors_2() {
        let (item, _) = TemplateMiddleList::parse(&mut newparser("}${0}${1"), Scanner::new(), false, false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn template_middle_list_test_conciseerrors_1() {
        let (item, _) = TemplateMiddleList::parse(&mut newparser("}${0"), Scanner::new(), false, false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn template_middle_list_test_conciseerrors_2() {
        let (item, _) = TemplateMiddleList::parse(&mut newparser("}${0}${1"), Scanner::new(), false, false, false).unwrap();
        concise_error_validate(*item);
    }

    // TEMPLATE SPANS
    #[test]
    fn template_spans_test_01() {
        let (ts, scanner) = check(TemplateSpans::parse(&mut newparser("}done`"), Scanner::new(), false, false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(&*ts, TemplateSpans::Tail(_, _)));
        pretty_check(&*ts, "TemplateSpans: }done`", vec![]);
        concise_check(&*ts, "TemplateTail: }done`", vec![]);
        format!("{:?}", ts);
    }
    #[test]
    fn template_spans_test_02() {
        let (ts, scanner) = check(TemplateSpans::parse(&mut newparser("}${a}done`"), Scanner::new(), false, false, false));
        chk_scan(&scanner, 10);
        assert!(matches!(&*ts, TemplateSpans::List(_, _, _)));
        pretty_check(&*ts, "TemplateSpans: }${ a }done`", vec!["TemplateMiddleList: }${ a"]);
        concise_check(&*ts, "TemplateSpans: }${ a }done`", vec!["TemplateMiddleList: }${ a", "TemplateTail: }done`"]);
        format!("{:?}", ts);
    }
    #[test]
    fn template_spans_test_03() {
        check_err(TemplateSpans::parse(&mut newparser(""), Scanner::new(), false, false, false), "TemplateSpans expected", 1, 1);
        check_err(TemplateSpans::parse(&mut newparser("}${blue"), Scanner::new(), false, false, false), "TemplateTail expected", 1, 8);
    }
    #[test]
    fn template_spans_test_prettyerrors_1() {
        let (item, _) = TemplateSpans::parse(&mut newparser("}`"), Scanner::new(), false, false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn template_spans_test_prettyerrors_2() {
        let (item, _) = TemplateSpans::parse(&mut newparser("}${0}`"), Scanner::new(), false, false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn template_spans_test_conciseerrors_1() {
        let (item, _) = TemplateSpans::parse(&mut newparser("}`"), Scanner::new(), false, false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn template_spans_test_conciseerrors_2() {
        let (item, _) = TemplateSpans::parse(&mut newparser("}${0}`"), Scanner::new(), false, false, false).unwrap();
        concise_error_validate(*item);
    }

    // SUBSTITUTION TEMPLATE
    #[test]
    fn substitution_template_test_01() {
        let (st, scanner) = check(SubstitutionTemplate::parse(&mut newparser("`${a}`"), Scanner::new(), false, false, false));
        chk_scan(&scanner, 6);
        assert_eq!(st.tagged, false);
        pretty_check(&*st, "SubstitutionTemplate: `${ a }`", vec!["Expression: a", "TemplateSpans: }`"]);
        concise_check(&*st, "SubstitutionTemplate: `${ a }`", vec!["TemplateHead: `${", "IdentifierName: a", "TemplateTail: }`"]);
        format!("{:?}", st);
    }
    #[test]
    fn substitution_template_test_02() {
        check_err(SubstitutionTemplate::parse(&mut newparser(""), Scanner::new(), false, false, false), "SubstitutionTemplate expected", 1, 1);
    }
    #[test]
    fn substitution_template_test_03() {
        check_err(SubstitutionTemplate::parse(&mut newparser("`${"), Scanner::new(), false, false, false), "Expression expected", 1, 4);
    }
    #[test]
    fn substitution_template_test_04() {
        check_err(SubstitutionTemplate::parse(&mut newparser("`${a"), Scanner::new(), false, false, false), "TemplateSpans expected", 1, 5);
    }
    #[test]
    fn substitution_template_test_prettyerrors_1() {
        let (item, _) = SubstitutionTemplate::parse(&mut newparser("`${0}`"), Scanner::new(), false, false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn substitution_template_test_conciseerrors_1() {
        let (item, _) = SubstitutionTemplate::parse(&mut newparser("`${0}`"), Scanner::new(), false, false, false).unwrap();
        concise_error_validate(*item);
    }

    // TEMPLATE LITERAL
    #[test]
    fn template_literal_test_01() {
        let (tl, scanner) = check(TemplateLiteral::parse(&mut newparser("`rust`"), Scanner::new(), false, false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(&*tl, TemplateLiteral::NoSubstitutionTemplate(_, _)));
        if let TemplateLiteral::NoSubstitutionTemplate(_, tagged) = &*tl {
            assert_eq!(*tagged, false);
        }
        pretty_check(&*tl, "TemplateLiteral: `rust`", vec![]);
        concise_check(&*tl, "NoSubTemplate: `rust`", vec![]);
        format!("{:?}", tl);
    }
    #[test]
    fn template_literal_test_02() {
        let (tl, scanner) = check(TemplateLiteral::parse(&mut newparser("`${a}`"), Scanner::new(), false, false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(&*tl, TemplateLiteral::SubstitutionTemplate(_)));
        pretty_check(&*tl, "TemplateLiteral: `${ a }`", vec!["SubstitutionTemplate: `${ a }`"]);
        concise_check(&*tl, "SubstitutionTemplate: `${ a }`", vec!["TemplateHead: `${", "IdentifierName: a", "TemplateTail: }`"]);
        format!("{:?}", tl);
    }
    #[test]
    fn template_literal_test_03() {
        check_err(TemplateLiteral::parse(&mut newparser(""), Scanner::new(), false, false, false), "TemplateLiteral expected", 1, 1);
        check_err(TemplateLiteral::parse(&mut newparser("`${"), Scanner::new(), false, false, false), "Expression expected", 1, 4);
    }
    #[test]
    fn template_literal_test_prettyerrors_1() {
        let (item, _) = TemplateLiteral::parse(&mut newparser("`${0}`"), Scanner::new(), false, false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn template_literal_test_prettyerrors_2() {
        let (item, _) = TemplateLiteral::parse(&mut newparser("``"), Scanner::new(), false, false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn template_literal_test_conciseerrors_1() {
        let (item, _) = TemplateLiteral::parse(&mut newparser("`${0}`"), Scanner::new(), false, false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn template_literal_test_conciseerrors_2() {
        let (item, _) = TemplateLiteral::parse(&mut newparser("``"), Scanner::new(), false, false, false).unwrap();
        concise_error_validate(*item);
    }

    // COVER PARENTHESIZED EXPRESSION AND ARROW PARAMETER LIST
    #[test]
    fn cpeaapl_test_01() {
        let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("()"), Scanner::new(), false, false));
        chk_scan(&scanner, 2);
        assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::Empty));
        pretty_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( )", vec![]);
        concise_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( )", vec!["Punctuator: (", "Punctuator: )"]);
        format!("{:?}", node);
    }
    #[test]
    fn cpeaapl_test_02() {
        let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(8 in [1,2,3])"), Scanner::new(), false, false));
        chk_scan(&scanner, 14);
        assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::Expression(_)));
        pretty_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( 8 in [ 1 , 2 , 3 ] )", vec!["Expression: 8 in [ 1 , 2 , 3 ]"]);
        concise_check(
            &*node,
            "CoverParenthesizedExpressionAndArrowParameterList: ( 8 in [ 1 , 2 , 3 ] )",
            vec!["Punctuator: (", "RelationalExpression: 8 in [ 1 , 2 , 3 ]", "Punctuator: )"],
        );
        format!("{:?}", node);
    }
    #[test]
    fn cpeaapl_test_03() {
        let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(8 in a,)"), Scanner::new(), false, false));
        chk_scan(&scanner, 9);
        assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::ExpComma(_)));
        pretty_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( 8 in a , )", vec!["Expression: 8 in a"]);
        concise_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( 8 in a , )", vec!["Punctuator: (", "RelationalExpression: 8 in a", "Punctuator: ,", "Punctuator: )"]);
        format!("{:?}", node);
    }
    #[test]
    fn cpeaapl_test_04() {
        let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(...a)"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::Ident(_)));
        pretty_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( ... a )", vec!["BindingIdentifier: a"]);
        concise_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( ... a )", vec!["Punctuator: (", "Punctuator: ...", "IdentifierName: a", "Punctuator: )"]);
        format!("{:?}", node);
    }
    #[test]
    fn cpeaapl_test_05() {
        let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(...{})"), Scanner::new(), false, false));
        chk_scan(&scanner, 7);
        assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::Pattern(_)));
        pretty_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( ... { } )", vec!["BindingPattern: { }"]);
        concise_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( ... { } )", vec!["Punctuator: (", "Punctuator: ...", "ObjectBindingPattern: { }", "Punctuator: )"]);
        format!("{:?}", node);
    }
    #[test]
    fn cpeaapl_test_06() {
        let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(a,...b)"), Scanner::new(), false, false));
        chk_scan(&scanner, 8);
        assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::ExpIdent(..)));
        pretty_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( a , ... b )", vec!["Expression: a", "BindingIdentifier: b"]);
        concise_check(
            &*node,
            "CoverParenthesizedExpressionAndArrowParameterList: ( a , ... b )",
            vec!["Punctuator: (", "IdentifierName: a", "Punctuator: ,", "Punctuator: ...", "IdentifierName: b", "Punctuator: )"],
        );
        format!("{:?}", node);
    }
    #[test]
    fn cpeaapl_test_07() {
        let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(a,...[])"), Scanner::new(), false, false));
        chk_scan(&scanner, 9);
        assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::ExpPattern(..)));
        pretty_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( a , ... [ ] )", vec!["Expression: a", "BindingPattern: [ ]"]);
        concise_check(
            &*node,
            "CoverParenthesizedExpressionAndArrowParameterList: ( a , ... [ ] )",
            vec!["Punctuator: (", "IdentifierName: a", "Punctuator: ,", "Punctuator: ...", "ArrayBindingPattern: [ ]", "Punctuator: )"],
        );
        format!("{:?}", node);
    }
    #[test]
    fn cpeaapl_test_08() {
        check_err(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser(""), Scanner::new(), false, false), "‘(’ expected", 1, 1);
    }
    #[test]
    fn cpeaapl_test_09() {
        check_err(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("("), Scanner::new(), false, false), "Expression, spread pattern, or closing paren expected", 1, 2);
    }
    #[test]
    fn cpeaapl_test_10() {
        check_err(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(..."), Scanner::new(), false, false), "BindingIdentifier or BindingPattern expected", 1, 5);
    }
    #[test]
    fn cpeaapl_test_11() {
        check_err(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(...a"), Scanner::new(), false, false), "‘)’ expected", 1, 6);
    }
    #[test]
    fn cpeaapl_test_12() {
        check_err(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(...[]"), Scanner::new(), false, false), "‘)’ expected", 1, 7);
    }
    #[test]
    fn cpeaapl_test_13() {
        check_err(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(p"), Scanner::new(), false, false), "‘)’ expected", 1, 3);
    }
    #[test]
    fn cpeaapl_test_14() {
        check_err(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(p,"), Scanner::new(), false, false), "‘)’ expected", 1, 4);
    }
    #[test]
    fn cpeaapl_test_prettyerrors_1() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(0)"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn cpeaapl_test_prettyerrors_2() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(0,)"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn cpeaapl_test_prettyerrors_3() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("()"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn cpeaapl_test_prettyerrors_4() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(...a)"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn cpeaapl_test_prettyerrors_5() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(...{})"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn cpeaapl_test_prettyerrors_6() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(0,...a)"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn cpeaapl_test_prettyerrors_7() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(0,...{})"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn cpeaapl_test_conciseerrors_1() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(0)"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn cpeaapl_test_conciseerrors_2() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(0,)"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn cpeaapl_test_conciseerrors_3() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("()"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn cpeaapl_test_conciseerrors_4() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(...a)"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn cpeaapl_test_conciseerrors_5() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(...{})"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn cpeaapl_test_conciseerrors_6() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(0,...a)"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
    #[test]
    fn cpeaapl_test_conciseerrors_7() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(0,...{})"), Scanner::new(), false, false).unwrap();
        concise_error_validate(*item);
    }
}
