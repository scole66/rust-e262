use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::arrow_function_definitions::ArrowFunction;
use super::async_arrow_function_definitions::AsyncArrowFunction;
use super::conditional_operator::ConditionalExpression;
use super::generator_function_definitions::YieldExpression;
use super::left_hand_side_expressions::LeftHandSideExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner, StringToken};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// AssignmentExpression[In, Yield, Await] :
//      ConditionalExpression[?In, ?Yield, ?Await]
//      [+Yield]YieldExpression[?In, ?Await]
//      ArrowFunction[?In, ?Yield, ?Await]
//      AsyncArrowFunction[?In, ?Yield, ?Await]
//      LeftHandSideExpression[?Yield, ?Await] = AssignmentExpression[?In, ?Yield, ?Await]
//      LeftHandSideExpression[?Yield, ?Await] AssignmentOperator AssignmentExpression[?In, ?Yield, ?Await]
//      LeftHandSideExpression[?Yield, ?Await] &&= AssignmentExpression[?In, ?Yield, ?Await]
//      LeftHandSideExpression[?Yield, ?Await] ||= AssignmentExpression[?In, ?Yield, ?Await]
//      LeftHandSideExpression[?Yield, ?Await] ??= AssignmentExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum AssignmentExpression {
    FallThru(Rc<ConditionalExpression>),
    Yield(Rc<YieldExpression>),
    Arrow(Rc<ArrowFunction>),
    AsyncArrow(Rc<AsyncArrowFunction>),
    Assignment(Rc<LeftHandSideExpression>, Rc<AssignmentExpression>),
    OpAssignment(Rc<LeftHandSideExpression>, AssignmentOperator, Rc<AssignmentExpression>),
    LandAssignment(Rc<LeftHandSideExpression>, Rc<AssignmentExpression>),
    LorAssignment(Rc<LeftHandSideExpression>, Rc<AssignmentExpression>),
    CoalAssignment(Rc<LeftHandSideExpression>, Rc<AssignmentExpression>),
}

impl fmt::Display for AssignmentExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            AssignmentExpression::FallThru(node) => node.fmt(f),
            AssignmentExpression::Yield(node) => node.fmt(f),
            AssignmentExpression::Arrow(node) => node.fmt(f),
            AssignmentExpression::AsyncArrow(node) => node.fmt(f),
            AssignmentExpression::Assignment(left, right) => write!(f, "{} = {}", left, right),
            AssignmentExpression::OpAssignment(left, op, right) => write!(f, "{} {} {}", left, op, right),
            AssignmentExpression::LandAssignment(left, right) => write!(f, "{} &&= {}", left, right),
            AssignmentExpression::LorAssignment(left, right) => write!(f, "{} ||= {}", left, right),
            AssignmentExpression::CoalAssignment(left, right) => write!(f, "{} ??= {}", left, right),
        }
    }
}

impl PrettyPrint for AssignmentExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AssignmentExpression: {}", first, self)?;
        match &self {
            AssignmentExpression::FallThru(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentExpression::Yield(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentExpression::Arrow(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentExpression::AsyncArrow(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentExpression::Assignment(left, right)
            | AssignmentExpression::LandAssignment(left, right)
            | AssignmentExpression::LorAssignment(left, right)
            | AssignmentExpression::CoalAssignment(left, right) => {
                left.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                right.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::OpAssignment(left, op, right) => {
                left.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                op.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                right.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        match self {
            AssignmentExpression::FallThru(node) => node.concise_with_leftpad(writer, pad, state),
            AssignmentExpression::Yield(node) => node.concise_with_leftpad(writer, pad, state),
            AssignmentExpression::Arrow(node) => node.concise_with_leftpad(writer, pad, state),
            AssignmentExpression::AsyncArrow(node) => node.concise_with_leftpad(writer, pad, state),
            AssignmentExpression::Assignment(left, right) => {
                writeln!(writer, "{}AssignmentExpression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "=", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::OpAssignment(left, op, right) => {
                writeln!(writer, "{}AssignmentExpression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                op.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::LandAssignment(left, right) => {
                writeln!(writer, "{}AssignmentExpression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "&&=", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::LorAssignment(left, right) => {
                writeln!(writer, "{}AssignmentExpression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "||=", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::CoalAssignment(left, right) => {
                writeln!(writer, "{}AssignmentExpression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "??=", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for AssignmentExpression {
    fn is_function_definition(&self) -> bool {
        match &self {
            AssignmentExpression::Yield(_)
            | AssignmentExpression::Assignment(_, _)
            | AssignmentExpression::OpAssignment(_, _, _)
            | AssignmentExpression::LandAssignment(_, _)
            | AssignmentExpression::LorAssignment(_, _)
            | AssignmentExpression::CoalAssignment(_, _) => false,
            AssignmentExpression::Arrow(_) | AssignmentExpression::AsyncArrow(_) => true,
            AssignmentExpression::FallThru(node) => node.is_function_definition(),
        }
    }
}

impl AssignmentTargetType for AssignmentExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match &self {
            AssignmentExpression::Yield(_)
            | AssignmentExpression::Arrow(_)
            | AssignmentExpression::AsyncArrow(_)
            | AssignmentExpression::Assignment(_, _)
            | AssignmentExpression::OpAssignment(_, _, _)
            | AssignmentExpression::LandAssignment(_, _)
            | AssignmentExpression::LorAssignment(_, _)
            | AssignmentExpression::CoalAssignment(_, _) => ATTKind::Invalid,
            AssignmentExpression::FallThru(node) => node.assignment_target_type(),
        }
    }
}

impl AssignmentExpression {
    fn parse_core(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("AssignmentExpression expected", scanner.line, scanner.column))
            .otherwise(|| {
                if yield_flag {
                    YieldExpression::parse(parser, scanner, in_flag, await_flag).map(|(yieldexp, after_yield)| (Rc::new(AssignmentExpression::Yield(yieldexp)), after_yield))
                } else {
                    Err(ParseError::new(String::new(), scanner.line, scanner.column))
                }
            })
            .otherwise(|| ArrowFunction::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(af, after_af)| (Rc::new(AssignmentExpression::Arrow(af)), after_af)))
            .otherwise(|| AsyncArrowFunction::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(aaf, after_aaf)| (Rc::new(AssignmentExpression::AsyncArrow(aaf)), after_aaf)))
            .otherwise(|| {
                LeftHandSideExpression::parse(parser, scanner, yield_flag, await_flag).and_then(|(lhs, after_lhs)| {
                    scan_for_punct_set(
                        after_lhs,
                        parser.source,
                        ScanGoal::InputElementDiv,
                        &[
                            Punctuator::Eq,
                            Punctuator::AmpAmpEq,
                            Punctuator::PipePipeEq,
                            Punctuator::QQEq,
                            Punctuator::StarEq,
                            Punctuator::SlashEq,
                            Punctuator::PercentEq,
                            Punctuator::PlusEq,
                            Punctuator::MinusEq,
                            Punctuator::LtLtEq,
                            Punctuator::GtGtEq,
                            Punctuator::GtGtGtEq,
                            Punctuator::AmpEq,
                            Punctuator::PipeEq,
                            Punctuator::StarStarEq,
                            Punctuator::CaretEq,
                        ],
                    )
                    .and_then(|(op, after_op)| {
                        let make_ae = match op {
                            Punctuator::Eq => AssignmentExpression::Assignment,
                            Punctuator::AmpAmpEq => AssignmentExpression::LandAssignment,
                            Punctuator::PipePipeEq => AssignmentExpression::LorAssignment,
                            Punctuator::QQEq => AssignmentExpression::CoalAssignment,
                            Punctuator::StarEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Multiply, ae),
                            Punctuator::SlashEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Divide, ae),
                            Punctuator::PercentEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Modulo, ae),
                            Punctuator::PlusEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Add, ae),
                            Punctuator::MinusEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Subtract, ae),
                            Punctuator::LtLtEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::LeftShift, ae),
                            Punctuator::GtGtEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::SignedRightShift, ae),
                            Punctuator::GtGtGtEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::UnsignedRightShift, ae),
                            Punctuator::AmpEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::BitwiseAnd, ae),
                            Punctuator::PipeEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::BitwiseOr, ae),
                            Punctuator::StarStarEq => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Exponentiate, ae),
                            _ => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::BitwiseXor, ae),
                        };
                        AssignmentExpression::parse(parser, after_op, in_flag, yield_flag, await_flag).map(|(ae, after_ae)| (Rc::new(make_ae(lhs, ae)), after_ae))
                    })
                })
            })
            .otherwise(|| ConditionalExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(ce, after_ce)| (Rc::new(AssignmentExpression::FallThru(ce)), after_ce)))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = InYieldAwaitKey { scanner, in_flag, yield_flag, await_flag };
        match parser.assignment_expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag, yield_flag, await_flag);
                parser.assignment_expression_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            AssignmentExpression::FallThru(node) => node.contains(kind),
            AssignmentExpression::Yield(node) => node.contains(kind),
            AssignmentExpression::Arrow(node) => node.contains(kind),
            AssignmentExpression::AsyncArrow(node) => node.contains(kind),
            AssignmentExpression::Assignment(left, right) => left.contains(kind) || right.contains(kind),
            AssignmentExpression::OpAssignment(left, op, right) => left.contains(kind) || op.contains(kind) || right.contains(kind),
            AssignmentExpression::LandAssignment(left, right) => left.contains(kind) || right.contains(kind),
            AssignmentExpression::LorAssignment(left, right) => left.contains(kind) || right.contains(kind),
            AssignmentExpression::CoalAssignment(left, right) => left.contains(kind) || right.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            AssignmentExpression::FallThru(node) => node.as_string_literal(),
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
        match self {
            AssignmentExpression::FallThru(node) => node.all_private_identifiers_valid(names),
            AssignmentExpression::Yield(node) => node.all_private_identifiers_valid(names),
            AssignmentExpression::Arrow(node) => node.all_private_identifiers_valid(names),
            AssignmentExpression::AsyncArrow(node) => node.all_private_identifiers_valid(names),
            AssignmentExpression::Assignment(left, right) => left.all_private_identifiers_valid(names) && right.all_private_identifiers_valid(names),
            AssignmentExpression::OpAssignment(left, _, right) => left.all_private_identifiers_valid(names) && right.all_private_identifiers_valid(names),
            AssignmentExpression::LandAssignment(left, right) => left.all_private_identifiers_valid(names) && right.all_private_identifiers_valid(names),
            AssignmentExpression::LorAssignment(left, right) => left.all_private_identifiers_valid(names) && right.all_private_identifiers_valid(names),
            AssignmentExpression::CoalAssignment(left, right) => left.all_private_identifiers_valid(names) && right.all_private_identifiers_valid(names),
        }
    }
}

// AssignmentOperator : one of
//      *= /= %= += -= <<= >>= >>>= &= ^= |= **=
#[derive(Debug)]
pub enum AssignmentOperator {
    Multiply,
    Divide,
    Modulo,
    Add,
    Subtract,
    LeftShift,
    SignedRightShift,
    UnsignedRightShift,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    Exponentiate,
}

impl fmt::Display for AssignmentOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            AssignmentOperator::Multiply => write!(f, "*="),
            AssignmentOperator::Divide => write!(f, "/="),
            AssignmentOperator::Modulo => write!(f, "%="),
            AssignmentOperator::Add => write!(f, "+="),
            AssignmentOperator::Subtract => write!(f, "-="),
            AssignmentOperator::LeftShift => write!(f, "<<="),
            AssignmentOperator::SignedRightShift => write!(f, ">>="),
            AssignmentOperator::UnsignedRightShift => write!(f, ">>>="),
            AssignmentOperator::BitwiseAnd => write!(f, "&="),
            AssignmentOperator::BitwiseXor => write!(f, "^="),
            AssignmentOperator::BitwiseOr => write!(f, "|="),
            AssignmentOperator::Exponentiate => write!(f, "**="),
        }
    }
}

impl PrettyPrint for AssignmentOperator {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{}AssignmentOperator: {}", first, self)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        pprint_token(writer, self, TokenType::Punctuator, pad, state)
    }
}

impl AssignmentOperator {
    pub fn contains(&self, _kind: ParseNodeKind) -> bool {
        false
    }
}

// AssignmentPropertyList[Yield, Await] :
//      AssignmentProperty[?Yield, ?Await]
//      AssignmentPropertyList[?Yield, ?Await] , AssignmentProperty[?Yield, ?Await]
#[derive(Debug)]
pub enum AssignmentPropertyList {
    Item(Rc<AssignmentProperty>),
    List(Rc<AssignmentPropertyList>, Rc<AssignmentProperty>),
}

impl fmt::Display for AssignmentPropertyList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AssignmentPropertyList::Item(item) => item.fmt(f),
            AssignmentPropertyList::List(lst, item) => write!(f, "{} , {}", lst, item),
        }
    }
}

impl PrettyPrint for AssignmentPropertyList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AssignmentPropertyList: {}", first, self)?;
        match self {
            AssignmentPropertyList::Item(item) => item.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentPropertyList::List(lst, item) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            AssignmentPropertyList::Item(item) => item.concise_with_leftpad(writer, pad, state),
            AssignmentPropertyList::List(lst, item) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}AssignmentPropertyList: {}", first, self)?;
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl AssignmentPropertyList {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (item, after_item) = AssignmentProperty::parse(parser, scanner, yield_flag, await_flag)?;
        let mut current_production = Rc::new(AssignmentPropertyList::Item(item));
        let mut current_scanner = after_item;
        while let Ok((next_item, after_next)) = scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
            .and_then(|after_comma| AssignmentProperty::parse(parser, after_comma, yield_flag, await_flag))
        {
            current_production = Rc::new(AssignmentPropertyList::List(current_production, next_item));
            current_scanner = after_next;
        }
        Ok((current_production, current_scanner))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            AssignmentPropertyList::Item(item) => item.contains(kind),
            AssignmentPropertyList::List(lst, item) => lst.contains(kind) || item.contains(kind),
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
            AssignmentPropertyList::Item(item) => item.all_private_identifiers_valid(names),
            AssignmentPropertyList::List(list, item) => list.all_private_identifiers_valid(names) && item.all_private_identifiers_valid(names),
        }
    }
}

// AssignmentElementList[Yield, Await] :
//      AssignmentElisionElement[?Yield, ?Await]
//      AssignmentElementList[?Yield, ?Await] , AssignmentElisionElement[?Yield, ?Await]
#[derive(Debug)]
pub enum AssignmentElementList {
    Item(Rc<AssignmentElisionElement>),
    List(Rc<AssignmentElementList>, Rc<AssignmentElisionElement>),
}

impl fmt::Display for AssignmentElementList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AssignmentElementList::Item(item) => item.fmt(f),
            AssignmentElementList::List(list, item) => write!(f, "{} , {}", list, item),
        }
    }
}

impl PrettyPrint for AssignmentElementList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AssignmentElementList: {}", first, self)?;
        match self {
            AssignmentElementList::Item(item) => item.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentElementList::List(list, item) => {
                list.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            AssignmentElementList::Item(item) => item.concise_with_leftpad(writer, pad, state),
            AssignmentElementList::List(list, item) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}AssignmentElementList: {}", first, self)?;
                list.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl AssignmentElementList {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (item, after_item) = AssignmentElisionElement::parse(parser, scanner, yield_flag, await_flag)?;
        let mut current_production = Rc::new(AssignmentElementList::Item(item));
        let mut current_scanner = after_item;
        while let Ok((next_item, after_next)) = scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
            .and_then(|after_comma| AssignmentElisionElement::parse(parser, after_comma, yield_flag, await_flag))
        {
            current_production = Rc::new(AssignmentElementList::List(current_production, next_item));
            current_scanner = after_next;
        }
        Ok((current_production, current_scanner))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            AssignmentElementList::Item(item) => item.contains(kind),
            AssignmentElementList::List(list, item) => list.contains(kind) || item.contains(kind),
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
            AssignmentElementList::Item(item) => item.all_private_identifiers_valid(names),
            AssignmentElementList::List(list, item) => list.all_private_identifiers_valid(names) && item.all_private_identifiers_valid(names),
        }
    }
}

// AssignmentElisionElement[Yield, Await] :
//      Elision_opt AssignmentElement[?Yield, ?Await]
#[derive(Debug)]
pub struct AssignmentElisionElement {
    elisions: Option<Rc<Elisions>>,
    element: Rc<AssignmentElement>,
}

impl fmt::Display for AssignmentElisionElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.elisions {
            Some(elisions) => write!(f, "{} {}", elisions, self.element),
            None => self.element.fmt(f),
        }
    }
}

impl PrettyPrint for AssignmentElisionElement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AssignmentElisionElement: {}", first, self)?;
        if let Some(elisions) = &self.elisions {
            elisions.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        self.element.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match &self.elisions {
            Some(elisions) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}AssignmentElisionElement: {}", first, self)?;
                elisions.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                self.element.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            None => self.element.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl AssignmentElisionElement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (elisions, after_elision) = match Elisions::parse(parser, scanner) {
            Ok((node, scan)) => (Some(node), scan),
            Err(_) => (None, scanner),
        };
        let (element, after_ae) = AssignmentElement::parse(parser, after_elision, yield_flag, await_flag)?;
        Ok((Rc::new(AssignmentElisionElement { elisions, element }), after_ae))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.element.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.element.all_private_identifiers_valid(names)
    }
}

// AssignmentProperty[Yield, Await] :
//      IdentifierReference[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]opt
//      PropertyName[?Yield, ?Await] : AssignmentElement[?Yield, ?Await]
#[derive(Debug)]
pub enum AssignmentProperty {
    Ident(Rc<IdentifierReference>, Option<Rc<Initializer>>),
    Property(Rc<PropertyName>, Rc<AssignmentElement>),
}

impl fmt::Display for AssignmentProperty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AssignmentProperty::Ident(id, None) => id.fmt(f),
            AssignmentProperty::Ident(id, Some(init)) => write!(f, "{} {}", id, init),
            AssignmentProperty::Property(name, ae) => write!(f, "{} : {}", name, ae),
        }
    }
}

impl PrettyPrint for AssignmentProperty {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AssignmentProperty: {}", first, self)?;
        match self {
            AssignmentProperty::Ident(id, None) => id.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentProperty::Ident(id, Some(init)) => {
                id.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                init.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentProperty::Property(name, ae) => {
                name.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ae.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let write_head = |writer: &mut T, pad, state| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}AssignmentProperty: {}", first, self)?;
            IoResult::<String>::Ok(successive)
        };

        match self {
            AssignmentProperty::Ident(id, None) => id.concise_with_leftpad(writer, pad, state),
            AssignmentProperty::Ident(id, Some(init)) => {
                let successive = write_head(writer, pad, state)?;
                id.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                init.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentProperty::Property(name, ae) => {
                let successive = write_head(writer, pad, state)?;
                name.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                ae.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl AssignmentProperty {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("IdentifierReference or PropertyName expected", scanner.line, scanner.column))
            .otherwise(|| {
                let (name, after_name) = PropertyName::parse(parser, scanner, yield_flag, await_flag)?;
                let after_colon = scan_for_punct(after_name, parser.source, ScanGoal::InputElementDiv, Punctuator::Colon)?;
                let (element, after_element) = AssignmentElement::parse(parser, after_colon, yield_flag, await_flag)?;
                Ok((Rc::new(AssignmentProperty::Property(name, element)), after_element))
            })
            .otherwise(|| {
                let (idref, after_id) = IdentifierReference::parse(parser, scanner, yield_flag, await_flag)?;
                let (init, after_init) = match Initializer::parse(parser, after_id, true, yield_flag, await_flag) {
                    Ok((node, scan)) => (Some(node), scan),
                    Err(_) => (None, after_id),
                };
                Ok((Rc::new(AssignmentProperty::Ident(idref, init)), after_init))
            })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            AssignmentProperty::Ident(id, None) => id.contains(kind),
            AssignmentProperty::Ident(id, Some(init)) => id.contains(kind) || init.contains(kind),
            AssignmentProperty::Property(name, element) => name.contains(kind) || element.contains(kind),
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
            AssignmentProperty::Ident(_, None) => true,
            AssignmentProperty::Ident(_, Some(init)) => init.all_private_identifiers_valid(names),
            AssignmentProperty::Property(name, element) => name.all_private_identifiers_valid(names) && element.all_private_identifiers_valid(names),
        }
    }
}

// AssignmentElement[Yield, Await] :
//      DestructuringAssignmentTarget[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]opt
#[derive(Debug)]
pub struct AssignmentElement {
    target: Rc<DestructuringAssignmentTarget>,
    initializer: Option<Rc<Initializer>>,
}

impl fmt::Display for AssignmentElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.initializer {
            None => self.target.fmt(f),
            Some(init) => write!(f, "{} {}", self.target, init),
        }
    }
}

impl PrettyPrint for AssignmentElement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AssignmentElement: {}", first, self)?;
        match &self.initializer {
            None => self.target.pprint_with_leftpad(writer, &successive, Spot::Final),
            Some(init) => {
                self.target.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                init.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match &self.initializer {
            None => self.target.concise_with_leftpad(writer, pad, state),
            Some(init) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}AssignmentElement: {}", first, self)?;
                self.target.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                init.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl AssignmentElement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (target, after_target) = DestructuringAssignmentTarget::parse(parser, scanner, yield_flag, await_flag)?;
        let (initializer, after_init) = match Initializer::parse(parser, after_target, true, yield_flag, await_flag) {
            Ok((node, scan)) => (Some(node), scan),
            Err(_) => (None, after_target),
        };
        Ok((Rc::new(AssignmentElement { target, initializer }), after_init))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match &self.initializer {
            Some(init) => self.target.contains(kind) || init.contains(kind),
            None => self.target.contains(kind),
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        match &self.initializer {
            Some(init) => self.target.all_private_identifiers_valid(names) && init.all_private_identifiers_valid(names),
            None => self.target.all_private_identifiers_valid(names),
        }
    }
}

// AssignmentRestElement[Yield, Await] :
//      ... DestructuringAssignmentTarget[?Yield, ?Await]
#[derive(Debug)]
pub struct AssignmentRestElement(Rc<DestructuringAssignmentTarget>);

impl fmt::Display for AssignmentRestElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "... {}", self.0)
    }
}

impl PrettyPrint for AssignmentRestElement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AssignmentRestElement: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AssignmentRestElement: {}", first, self)?;
        pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.0.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl AssignmentRestElement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_dots = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Ellipsis)?;
        let (target, after_target) = DestructuringAssignmentTarget::parse(parser, after_dots, yield_flag, await_flag)?;
        Ok((Rc::new(AssignmentRestElement(target)), after_target))
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
}

// DestructuringAssignmentTarget[Yield, Await] :
//      LeftHandSideExpression[?Yield, ?Await]
#[derive(Debug)]
pub struct DestructuringAssignmentTarget(Rc<LeftHandSideExpression>);

impl fmt::Display for DestructuringAssignmentTarget {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PrettyPrint for DestructuringAssignmentTarget {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}DestructuringAssignmentTarget: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.0.concise_with_leftpad(writer, pad, state)
    }
}

impl DestructuringAssignmentTarget {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (lhs, after_lhs) = LeftHandSideExpression::parse(parser, scanner, yield_flag, await_flag)?;
        Ok((Rc::new(DestructuringAssignmentTarget(lhs)), after_lhs))
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
}

#[cfg(test)]
mod tests;
