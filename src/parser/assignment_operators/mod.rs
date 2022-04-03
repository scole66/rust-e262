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
    Destructuring(Rc<AssignmentPattern>, Rc<AssignmentExpression>),
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
            AssignmentExpression::Destructuring(left, right) => write!(f, "{} = {}", left, right),
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
        match self {
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
            AssignmentExpression::Destructuring(pat, exp) => {
                pat.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                exp.pprint_with_leftpad(writer, &successive, Spot::Final)
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
            AssignmentExpression::Destructuring(pat, exp) => {
                writeln!(writer, "{}AssignmentExpression: {}", first, self)?;
                pat.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "=", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                exp.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for AssignmentExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            AssignmentExpression::Yield(_)
            | AssignmentExpression::Assignment(..)
            | AssignmentExpression::OpAssignment(..)
            | AssignmentExpression::LandAssignment(..)
            | AssignmentExpression::LorAssignment(..)
            | AssignmentExpression::CoalAssignment(..)
            | AssignmentExpression::Destructuring(..) => false,
            AssignmentExpression::Arrow(_) | AssignmentExpression::AsyncArrow(_) => true,
            AssignmentExpression::FallThru(node) => node.is_function_definition(),
        }
    }
}

impl AssignmentTargetType for AssignmentExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            AssignmentExpression::Yield(_)
            | AssignmentExpression::Arrow(_)
            | AssignmentExpression::AsyncArrow(_)
            | AssignmentExpression::Assignment(_, _)
            | AssignmentExpression::OpAssignment(_, _, _)
            | AssignmentExpression::LandAssignment(_, _)
            | AssignmentExpression::LorAssignment(_, _)
            | AssignmentExpression::CoalAssignment(_, _)
            | AssignmentExpression::Destructuring(..) => ATTKind::Invalid,
            AssignmentExpression::FallThru(node) => node.assignment_target_type(),
        }
    }
}

impl AssignmentExpression {
    fn parse_core(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let result = Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::AssignmentExpression), scanner))
            .otherwise(|| {
                if yield_flag {
                    YieldExpression::parse(parser, scanner, in_flag, await_flag).map(|(yieldexp, after_yield)| (Rc::new(AssignmentExpression::Yield(yieldexp)), after_yield, Scanner::new()))
                } else {
                    Err(ParseError::new(PECode::Generic, scanner))
                }
            })
            .otherwise(|| ArrowFunction::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(af, after_af)| (Rc::new(AssignmentExpression::Arrow(af)), after_af, Scanner::new())))
            .otherwise(|| {
                AsyncArrowFunction::parse(parser, scanner, in_flag, yield_flag, await_flag)
                    .map(|(aaf, after_aaf)| (Rc::new(AssignmentExpression::AsyncArrow(aaf)), after_aaf, Scanner::new()))
            })
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
                        AssignmentExpression::parse(parser, after_op, in_flag, yield_flag, await_flag).map(|(ae, after_ae)| (Rc::new(make_ae(lhs, ae)), after_ae, after_lhs))
                    })
                })
            })
            .otherwise(|| {
                ConditionalExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(ce, after_ce)| (Rc::new(AssignmentExpression::FallThru(ce)), after_ce, Scanner::new()))
            })?;

        if let AssignmentExpression::Assignment(lhs, ae) = &*result.0 {
            if lhs.is_object_or_array_literal() {
                // Re-parse the LHS as an AssignmentPattern.
                let (ap, after_ap) = AssignmentPattern::parse(parser, scanner, yield_flag, await_flag)?;
                // Note: because the object/array literals require proper nested brackets/braces, and so do the
                // assignment patterns, we're guaranteed the the text we just parsed as an AssignmentPattern was the
                // same text that was parsed as a literal. There won't ever be an error where this new parse didn't
                // consume all the characters. (No need to write a test to cover this won't-ever-happen error
                // condition.)
                assert_eq!(after_ap, result.2);
                return Ok((Rc::new(AssignmentExpression::Destructuring(ap, ae.clone())), result.1));
            }
        }
        Ok((result.0, result.1))
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
            AssignmentExpression::Destructuring(pat, exp) => pat.contains(kind) || exp.contains(kind),
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
            AssignmentExpression::Destructuring(pat, exp) => pat.all_private_identifiers_valid(names) && exp.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        // AssignmentExpression :
        // LeftHandSideExpression AssignmentOperator AssignmentExpression
        // LeftHandSideExpression &&= AssignmentExpression
        // LeftHandSideExpression ||= AssignmentExpression
        // LeftHandSideExpression ??= AssignmentExpression
        // It is a Syntax Error if AssignmentTargetType of LeftHandSideExpression is not simple.
        match self {
            AssignmentExpression::FallThru(node) => node.early_errors(agent, errs, strict),
            AssignmentExpression::Yield(node) => node.early_errors(agent, errs, strict),
            AssignmentExpression::Arrow(node) => node.early_errors(agent, errs, strict),
            AssignmentExpression::AsyncArrow(node) => node.early_errors(agent, errs, strict),
            AssignmentExpression::Assignment(left, right)
            | AssignmentExpression::OpAssignment(left, _, right)
            | AssignmentExpression::LandAssignment(left, right)
            | AssignmentExpression::LorAssignment(left, right)
            | AssignmentExpression::CoalAssignment(left, right) => {
                // AssignmentExpression :
                //      LeftHandSideExpression = AssignmentExpression
                //      LeftHandSideExpression AssignmentOperator AssignmentExpression
                //      LeftHandSideExpression &&= AssignmentExpression
                //      LeftHandSideExpression ||= AssignmentExpression
                //      LeftHandSideExpression ??= AssignmentExpression
                //
                //  * It is a Syntax Error if AssignmentTargetType of LeftHandSideExpression is not simple.
                if left.assignment_target_type() != ATTKind::Simple {
                    errs.push(create_syntax_error_object(agent, "Invalid left-hand side in assignment"));
                }
                left.early_errors(agent, errs, strict);
                right.early_errors(agent, errs, strict);
            }
            AssignmentExpression::Destructuring(pat, exp) => {
                pat.early_errors(agent, errs, strict);
                exp.early_errors(agent, errs, strict);
            }
        }
    }

    pub fn is_strictly_deletable(&self) -> bool {
        match self {
            AssignmentExpression::FallThru(node) => node.is_strictly_deletable(),
            _ => true,
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

// AssignmentPattern[Yield, Await] :
//      ObjectAssignmentPattern[?Yield, ?Await]
//      ArrayAssignmentPattern[?Yield, ?Await]
#[derive(Debug)]
pub enum AssignmentPattern {
    Object(Rc<ObjectAssignmentPattern>),
    Array(Rc<ArrayAssignmentPattern>),
}

impl fmt::Display for AssignmentPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AssignmentPattern::Object(obj) => obj.fmt(f),
            AssignmentPattern::Array(ary) => ary.fmt(f),
        }
    }
}

impl PrettyPrint for AssignmentPattern {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AssignmentPattern: {}", first, self)?;
        match self {
            AssignmentPattern::Object(obj) => obj.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentPattern::Array(ary) => ary.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            AssignmentPattern::Object(obj) => obj.concise_with_leftpad(writer, pad, state),
            AssignmentPattern::Array(ary) => ary.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl AssignmentPattern {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::AssignmentPattern), scanner))
            .otherwise(|| ObjectAssignmentPattern::parse(parser, scanner, yield_flag, await_flag).map(|(oap, after_oap)| (Rc::new(AssignmentPattern::Object(oap)), after_oap)))
            .otherwise(|| ArrayAssignmentPattern::parse(parser, scanner, yield_flag, await_flag).map(|(aap, after_aap)| (Rc::new(AssignmentPattern::Array(aap)), after_aap)))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            AssignmentPattern::Object(obj) => obj.contains(kind),
            AssignmentPattern::Array(ary) => ary.contains(kind),
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
            AssignmentPattern::Object(obj) => obj.all_private_identifiers_valid(names),
            AssignmentPattern::Array(ary) => ary.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            AssignmentPattern::Object(obj) => obj.early_errors(agent, errs, strict),
            AssignmentPattern::Array(ary) => ary.early_errors(agent, errs, strict),
        }
    }
}

// ObjectAssignmentPattern[Yield, Await] :
//      { }
//      { AssignmentRestProperty[?Yield, ?Await] }
//      { AssignmentPropertyList[?Yield, ?Await] }
//      { AssignmentPropertyList[?Yield, ?Await] , AssignmentRestProperty[?Yield, ?Await]opt }
#[derive(Debug)]
pub enum ObjectAssignmentPattern {
    Empty,
    RestOnly(Rc<AssignmentRestProperty>),
    ListOnly(Rc<AssignmentPropertyList>),
    ListRest(Rc<AssignmentPropertyList>, Option<Rc<AssignmentRestProperty>>),
}

impl fmt::Display for ObjectAssignmentPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ObjectAssignmentPattern::Empty => write!(f, "{{ }}"),
            ObjectAssignmentPattern::RestOnly(arp) => write!(f, "{{ {} }}", arp),
            ObjectAssignmentPattern::ListOnly(apl) => write!(f, "{{ {} }}", apl),
            ObjectAssignmentPattern::ListRest(apl, None) => write!(f, "{{ {} , }}", apl),
            ObjectAssignmentPattern::ListRest(apl, Some(arp)) => write!(f, "{{ {} , {} }}", apl, arp),
        }
    }
}

impl PrettyPrint for ObjectAssignmentPattern {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ObjectAssignmentPattern: {}", first, self)?;
        match self {
            ObjectAssignmentPattern::Empty => Ok(()),
            ObjectAssignmentPattern::RestOnly(arp) => arp.pprint_with_leftpad(writer, &successive, Spot::Final),
            ObjectAssignmentPattern::ListOnly(apl) | ObjectAssignmentPattern::ListRest(apl, None) => apl.pprint_with_leftpad(writer, &successive, Spot::Final),
            ObjectAssignmentPattern::ListRest(apl, Some(arp)) => {
                apl.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                arp.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ObjectAssignmentPattern: {}", first, self)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match self {
            ObjectAssignmentPattern::ListOnly(apl) | ObjectAssignmentPattern::ListRest(apl, _) => {
                apl.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            _ => {}
        }
        if matches!(self, ObjectAssignmentPattern::ListRest(..)) {
            pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        }
        match self {
            ObjectAssignmentPattern::RestOnly(arp) | ObjectAssignmentPattern::ListRest(_, Some(arp)) => {
                arp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            _ => {}
        }
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ObjectAssignmentPattern {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_brace = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBrace)?;
        Err(ParseError::new(PECode::ObjectAssignmentPatternEndFailure, after_brace))
            .otherwise(|| {
                let (apl, after_apl) = AssignmentPropertyList::parse(parser, after_brace, yield_flag, await_flag)?;
                let (punct, after_punct) = scan_for_punct_set(after_apl, parser.source, ScanGoal::InputElementDiv, &[Punctuator::Comma, Punctuator::RightBrace])?;
                match punct {
                    Punctuator::RightBrace => Ok((Rc::new(ObjectAssignmentPattern::ListOnly(apl)), after_punct)),
                    _ => {
                        let (arp, after_arp) = match AssignmentRestProperty::parse(parser, after_punct, yield_flag, await_flag) {
                            Ok((node, scan)) => (Some(node), scan),
                            Err(_) => (None, after_punct),
                        };
                        let after_close = scan_for_punct(after_arp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                        Ok((Rc::new(ObjectAssignmentPattern::ListRest(apl, arp)), after_close))
                    }
                }
            })
            .otherwise(|| {
                let (arp, after_arp) = AssignmentRestProperty::parse(parser, after_brace, yield_flag, await_flag)?;
                let after_close = scan_for_punct(after_arp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                Ok((Rc::new(ObjectAssignmentPattern::RestOnly(arp)), after_close))
            })
            .otherwise(|| {
                let after_close = scan_for_punct(after_brace, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                Ok((Rc::new(ObjectAssignmentPattern::Empty), after_close))
            })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ObjectAssignmentPattern::Empty => false,
            ObjectAssignmentPattern::RestOnly(arp) => arp.contains(kind),
            ObjectAssignmentPattern::ListOnly(apl) | ObjectAssignmentPattern::ListRest(apl, None) => apl.contains(kind),
            ObjectAssignmentPattern::ListRest(apl, Some(arp)) => apl.contains(kind) || arp.contains(kind),
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
            ObjectAssignmentPattern::Empty => true,
            ObjectAssignmentPattern::RestOnly(arp) => arp.all_private_identifiers_valid(names),
            ObjectAssignmentPattern::ListOnly(apl) | ObjectAssignmentPattern::ListRest(apl, None) => apl.all_private_identifiers_valid(names),
            ObjectAssignmentPattern::ListRest(apl, Some(apr)) => apl.all_private_identifiers_valid(names) && apr.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ObjectAssignmentPattern::Empty => (),
            ObjectAssignmentPattern::RestOnly(arp) => arp.early_errors(agent, errs, strict),
            ObjectAssignmentPattern::ListOnly(apl) | ObjectAssignmentPattern::ListRest(apl, None) => apl.early_errors(agent, errs, strict),
            ObjectAssignmentPattern::ListRest(apl, Some(apr)) => {
                apl.early_errors(agent, errs, strict);
                apr.early_errors(agent, errs, strict);
            }
        }
    }
}

// ArrayAssignmentPattern[Yield, Await] :
//      [ Elision_opt AssignmentRestElement[?Yield, ?Await]opt ]
//      [ AssignmentElementList[?Yield, ?Await] ]
//      [ AssignmentElementList[?Yield, ?Await] , Elision_opt AssignmentRestElement[?Yield, ?Await]opt ]
#[derive(Debug)]
pub enum ArrayAssignmentPattern {
    RestOnly(Option<Rc<Elisions>>, Option<Rc<AssignmentRestElement>>),
    ListOnly(Rc<AssignmentElementList>),
    ListRest(Rc<AssignmentElementList>, Option<Rc<Elisions>>, Option<Rc<AssignmentRestElement>>),
}

impl fmt::Display for ArrayAssignmentPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArrayAssignmentPattern::RestOnly(None, None) => write!(f, "[ ]"),
            ArrayAssignmentPattern::RestOnly(Some(elisions), None) => write!(f, "[ {} ]", elisions),
            ArrayAssignmentPattern::RestOnly(None, Some(are)) => write!(f, "[ {} ]", are),
            ArrayAssignmentPattern::RestOnly(Some(elisions), Some(are)) => write!(f, "[ {} {} ]", elisions, are),
            ArrayAssignmentPattern::ListOnly(ael) => write!(f, "[ {} ]", ael),
            ArrayAssignmentPattern::ListRest(ael, None, None) => write!(f, "[ {} , ]", ael),
            ArrayAssignmentPattern::ListRest(ael, Some(elisions), None) => write!(f, "[ {} , {} ]", ael, elisions),
            ArrayAssignmentPattern::ListRest(ael, None, Some(are)) => write!(f, "[ {} , {} ]", ael, are),
            ArrayAssignmentPattern::ListRest(ael, Some(elisions), Some(are)) => write!(f, "[ {} , {} {} ]", ael, elisions, are),
        }
    }
}

impl PrettyPrint for ArrayAssignmentPattern {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ArrayAssignmentPattern: {}", first, self)?;
        match self {
            ArrayAssignmentPattern::RestOnly(None, None) => Ok(()),
            ArrayAssignmentPattern::RestOnly(Some(elisions), None) => elisions.pprint_with_leftpad(writer, &successive, Spot::Final),
            ArrayAssignmentPattern::RestOnly(None, Some(are)) => are.pprint_with_leftpad(writer, &successive, Spot::Final),
            ArrayAssignmentPattern::RestOnly(Some(elisions), Some(are)) => {
                elisions.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                are.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayAssignmentPattern::ListOnly(ael) | ArrayAssignmentPattern::ListRest(ael, None, None) => ael.pprint_with_leftpad(writer, &successive, Spot::Final),
            ArrayAssignmentPattern::ListRest(ael, Some(elisions), None) => {
                ael.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                elisions.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayAssignmentPattern::ListRest(ael, None, Some(are)) => {
                ael.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                are.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayAssignmentPattern::ListRest(ael, Some(elisions), Some(are)) => {
                ael.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                elisions.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                are.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ArrayAssignmentPattern: {}", first, self)?;
        pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match self {
            ArrayAssignmentPattern::ListOnly(ael) | ArrayAssignmentPattern::ListRest(ael, ..) => {
                ael.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            _ => {}
        }
        if matches!(self, ArrayAssignmentPattern::ListRest(..)) {
            pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        }
        match self {
            ArrayAssignmentPattern::RestOnly(Some(e), _) | ArrayAssignmentPattern::ListRest(_, Some(e), _) => {
                e.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            _ => {}
        }
        match self {
            ArrayAssignmentPattern::RestOnly(_, Some(are)) | ArrayAssignmentPattern::ListRest(_, _, Some(are)) => {
                are.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            _ => {}
        }
        pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ArrayAssignmentPattern {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_open = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBracket)?;
        Err(ParseError::new(PECode::ArrayAssignmentPatternEndFailure, after_open))
            .otherwise(|| {
                let (el, after_el) = AssignmentElementList::parse(parser, after_open, yield_flag, await_flag)?;
                let (punct, after_punct) = scan_for_punct_set(after_el, parser.source, ScanGoal::InputElementDiv, &[Punctuator::Comma, Punctuator::RightBracket])?;
                match punct {
                    Punctuator::RightBracket => Ok((Rc::new(ArrayAssignmentPattern::ListOnly(el)), after_punct)),
                    _ => {
                        let (elisions, after_elisions) = match Elisions::parse(parser, after_punct) {
                            Ok((node, scan)) => (Some(node), scan),
                            Err(_) => (None, after_punct),
                        };
                        let (are, after_are) = match AssignmentRestElement::parse(parser, after_elisions, yield_flag, await_flag) {
                            Ok((node, scan)) => (Some(node), scan),
                            Err(_) => (None, after_elisions),
                        };
                        let after_close = scan_for_punct(after_are, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBracket)?;
                        Ok((Rc::new(ArrayAssignmentPattern::ListRest(el, elisions, are)), after_close))
                    }
                }
            })
            .otherwise(|| {
                let (elisions, after_elisions) = match Elisions::parse(parser, after_open) {
                    Ok((node, scan)) => (Some(node), scan),
                    Err(_) => (None, after_open),
                };
                let (are, after_are) = match AssignmentRestElement::parse(parser, after_elisions, yield_flag, await_flag) {
                    Ok((node, scan)) => (Some(node), scan),
                    Err(_) => (None, after_elisions),
                };
                let after_close = scan_for_punct(after_are, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBracket)?;
                Ok((Rc::new(ArrayAssignmentPattern::RestOnly(elisions, are)), after_close))
            })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ArrayAssignmentPattern::RestOnly(_, None) => false,
            ArrayAssignmentPattern::RestOnly(_, Some(are)) => are.contains(kind),
            ArrayAssignmentPattern::ListOnly(ael) | ArrayAssignmentPattern::ListRest(ael, _, None) => ael.contains(kind),
            ArrayAssignmentPattern::ListRest(ael, _, Some(are)) => ael.contains(kind) || are.contains(kind),
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
            ArrayAssignmentPattern::RestOnly(_, None) => true,
            ArrayAssignmentPattern::RestOnly(_, Some(are)) => are.all_private_identifiers_valid(names),
            ArrayAssignmentPattern::ListOnly(ael) | ArrayAssignmentPattern::ListRest(ael, _, None) => ael.all_private_identifiers_valid(names),
            ArrayAssignmentPattern::ListRest(ael, _, Some(are)) => ael.all_private_identifiers_valid(names) && are.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ArrayAssignmentPattern::RestOnly(_, None) => (),
            ArrayAssignmentPattern::RestOnly(_, Some(are)) => are.early_errors(agent, errs, strict),
            ArrayAssignmentPattern::ListOnly(ael) | ArrayAssignmentPattern::ListRest(ael, _, None) => ael.early_errors(agent, errs, strict),
            ArrayAssignmentPattern::ListRest(ael, _, Some(are)) => {
                ael.early_errors(agent, errs, strict);
                are.early_errors(agent, errs, strict);
            }
        }
    }
}

// AssignmentRestProperty[Yield, Await] :
//      ... DestructuringAssignmentTarget[?Yield, ?Await]
#[derive(Debug)]
pub struct AssignmentRestProperty(Rc<DestructuringAssignmentTarget>);

impl fmt::Display for AssignmentRestProperty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "... {}", self.0)
    }
}

impl PrettyPrint for AssignmentRestProperty {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AssignmentRestProperty: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AssignmentRestProperty: {}", first, self)?;
        pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.0.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl AssignmentRestProperty {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_dots = scan_for_punct(scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Ellipsis)?;
        let (dat, after_dat) = DestructuringAssignmentTarget::parse(parser, after_dots, yield_flag, await_flag)?;
        Ok((Rc::new(AssignmentRestProperty(dat)), after_dat))
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

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        // AssignmentRestProperty : ... DestructuringAssignmentTarget
        //  * It is a Syntax Error if DestructuringAssignmentTarget is an ArrayLiteral or an ObjectLiteral.
        if let DestructuringAssignmentTarget::AssignmentPattern(_) = &*self.0 {
            // e.g.: ({...{a}}=b)
            errs.push(create_syntax_error_object(agent, "`...` must be followed by an assignable reference in assignment contexts"));
        }
        self.0.early_errors(agent, errs, strict);
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

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            AssignmentPropertyList::Item(item) => item.early_errors(agent, errs, strict),
            AssignmentPropertyList::List(list, item) => {
                list.early_errors(agent, errs, strict);
                item.early_errors(agent, errs, strict);
            }
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

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            AssignmentElementList::Item(item) => item.early_errors(agent, errs, strict),
            AssignmentElementList::List(list, item) => {
                list.early_errors(agent, errs, strict);
                item.early_errors(agent, errs, strict);
            }
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

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        self.element.early_errors(agent, errs, strict);
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
        Err(ParseError::new(PECode::IdRefOrPropertyNameExpected, scanner))
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

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        // AssignmentProperty : IdentifierReference Initializeropt
        //  * It is a Syntax Error if AssignmentTargetType of IdentifierReference is not simple.
        match self {
            AssignmentProperty::Ident(idref, izer) => {
                if idref.assignment_target_type() != ATTKind::Simple {
                    // node.js reports:
                    //     "Unexpected eval or arguments in strict mode"
                    // But that feels like it knows too much about the cause for !Simple. Which might mean that we
                    // should have a different API for the "simple or not" query. Something like validate_simple_target
                    // that returns a Result<(), String>, where the error case is a description of why things aren't
                    // simple.
                    errs.push(create_syntax_error_object(agent, format!("Identifier {} is an invalid left-hand-side", idref.string_value())));
                }
                idref.early_errors(agent, errs, strict);
                if let Some(i) = izer {
                    i.early_errors(agent, errs, strict);
                }
            }
            AssignmentProperty::Property(pn, ae) => {
                pn.early_errors(agent, errs, strict);
                ae.early_errors(agent, errs, strict);
            }
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

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        self.target.early_errors(agent, errs, strict);
        if let Some(izer) = &self.initializer {
            izer.early_errors(agent, errs, strict);
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

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        self.0.early_errors(agent, errs, strict);
    }
}

// DestructuringAssignmentTarget[Yield, Await] :
//      LeftHandSideExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum DestructuringAssignmentTarget {
    LeftHandSideExpression(Rc<LeftHandSideExpression>),
    AssignmentPattern(Rc<AssignmentPattern>),
}

impl fmt::Display for DestructuringAssignmentTarget {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DestructuringAssignmentTarget::LeftHandSideExpression(lhs) => lhs.fmt(f),
            DestructuringAssignmentTarget::AssignmentPattern(pat) => pat.fmt(f),
        }
    }
}

impl PrettyPrint for DestructuringAssignmentTarget {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}DestructuringAssignmentTarget: {}", first, self)?;
        match self {
            DestructuringAssignmentTarget::LeftHandSideExpression(lhs) => lhs.pprint_with_leftpad(writer, &successive, Spot::Final),
            DestructuringAssignmentTarget::AssignmentPattern(pat) => pat.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            DestructuringAssignmentTarget::LeftHandSideExpression(lhs) => lhs.concise_with_leftpad(writer, pad, state),
            DestructuringAssignmentTarget::AssignmentPattern(pat) => pat.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl DestructuringAssignmentTarget {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (lhs, after_lhs) = LeftHandSideExpression::parse(parser, scanner, yield_flag, await_flag)?;

        if lhs.is_object_or_array_literal() {
            // Re-parse the LHS as an AssignmentPattern.
            let (ap, after_ap) = AssignmentPattern::parse(parser, scanner, yield_flag, await_flag)?;
            assert_eq!(after_ap, after_lhs);
            Ok((Rc::new(DestructuringAssignmentTarget::AssignmentPattern(ap)), after_ap))
        } else {
            Ok((Rc::new(DestructuringAssignmentTarget::LeftHandSideExpression(lhs)), after_lhs))
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            DestructuringAssignmentTarget::AssignmentPattern(pat) => pat.contains(kind),
            DestructuringAssignmentTarget::LeftHandSideExpression(lhs) => lhs.contains(kind),
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
            DestructuringAssignmentTarget::LeftHandSideExpression(lhs) => lhs.all_private_identifiers_valid(names),
            DestructuringAssignmentTarget::AssignmentPattern(pat) => pat.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        // DestructuringAssignmentTarget : LeftHandSideExpression
        //  * If LeftHandSideExpression is an ObjectLiteral or an ArrayLiteral, the following Early Error rules are applied:
        //      * LeftHandSideExpression must cover an AssignmentPattern.
        //  * If LeftHandSideExpression is neither an ObjectLiteral nor an ArrayLiteral, the following Early Error rule is applied:
        //      * It is a Syntax Error if AssignmentTargetType of LeftHandSideExpression is not simple.
        match self {
            DestructuringAssignmentTarget::AssignmentPattern(pat) => pat.early_errors(agent, errs, strict),
            DestructuringAssignmentTarget::LeftHandSideExpression(lhs) => {
                if lhs.assignment_target_type() != ATTKind::Simple {
                    errs.push(create_syntax_error_object(agent, "Invalid left-hand side in assignment"));
                }
                lhs.early_errors(agent, errs, strict);
            }
        }
    }
}

#[cfg(test)]
mod tests;
