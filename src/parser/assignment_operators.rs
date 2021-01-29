use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

use super::arrow_function_definitions::ArrowFunction;
use super::async_arrow_function_definitions::AsyncArrowFunction;
use super::conditional_operator::ConditionalExpression;
use super::generator_function_definitions::YieldExpression;
use super::left_hand_side_expressions::LeftHandSideExpression;

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
    FallThru(Box<ConditionalExpression>),
    Yield(Box<YieldExpression>),
    Arrow(Box<ArrowFunction>),
    AsyncArrow(Box<AsyncArrowFunction>),
    Assignment(Box<LeftHandSideExpression>, Box<AssignmentExpression>),
    OpAssignment(
        Box<LeftHandSideExpression>,
        AssignmentOperator,
        Box<AssignmentExpression>,
    ),
    LandAssignment(Box<LeftHandSideExpression>, Box<AssignmentExpression>),
    LorAssignment(Box<LeftHandSideExpression>, Box<AssignmentExpression>),
    CoalAssignment(Box<LeftHandSideExpression>, Box<AssignmentExpression>),
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
                pprint_token(writer, "=", &successive, Spot::NotFinal)?;
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
                pprint_token(writer, "&&=", &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::LorAssignment(left, right) => {
                writeln!(writer, "{}AssignmentExpression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "||=", &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::CoalAssignment(left, right) => {
                writeln!(writer, "{}AssignmentExpression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "??=", &successive, Spot::NotFinal)?;
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
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<AssignmentExpression>, Scanner)>, String> {
        if yield_flag {
            let pot_yield = YieldExpression::parse(parser, scanner, in_flag, await_flag)?;
            if let Some((yieldexp, after_yield)) = pot_yield {
                return Ok(Some((Box::new(AssignmentExpression::Yield(yieldexp)), after_yield)));
            }
        }

        let pot_arrow = ArrowFunction::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        if let Some((af, after_af)) = pot_arrow {
            return Ok(Some((Box::new(AssignmentExpression::Arrow(af)), after_af)));
        }

        let pot_asyncarrow = AsyncArrowFunction::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        if let Some((aaf, after_aaf)) = pot_asyncarrow {
            return Ok(Some((Box::new(AssignmentExpression::AsyncArrow(aaf)), after_aaf)));
        }

        let pot_lhs = LeftHandSideExpression::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((lhs, after_lhs)) = pot_lhs {
            let (op, after_op) = scanner::scan_token(&after_lhs, parser.source, scanner::ScanGoal::InputElementDiv);
            if matches!(
                &op,
                scanner::Token::Eq
                    | scanner::Token::AmpAmpEq
                    | scanner::Token::PipePipeEq
                    | scanner::Token::QQEq
                    | scanner::Token::StarEq
                    | scanner::Token::SlashEq
                    | scanner::Token::PercentEq
                    | scanner::Token::PlusEq
                    | scanner::Token::MinusEq
                    | scanner::Token::LtLtEq
                    | scanner::Token::GtGtEq
                    | scanner::Token::GtGtGtEq
                    | scanner::Token::AmpEq
                    | scanner::Token::PipeEq
                    | scanner::Token::StarStarEq
                    | scanner::Token::CaretEq
            ) {
                let make_ae = match &op {
                    scanner::Token::Eq => |lhs, ae| AssignmentExpression::Assignment(lhs, ae),
                    scanner::Token::AmpAmpEq => |lhs, ae| AssignmentExpression::LandAssignment(lhs, ae),
                    scanner::Token::PipePipeEq => |lhs, ae| AssignmentExpression::LorAssignment(lhs, ae),
                    scanner::Token::QQEq => |lhs, ae| AssignmentExpression::CoalAssignment(lhs, ae),
                    scanner::Token::StarEq => {
                        |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Multiply, ae)
                    }
                    scanner::Token::SlashEq => {
                        |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Divide, ae)
                    }
                    scanner::Token::PercentEq => {
                        |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Modulo, ae)
                    }
                    scanner::Token::PlusEq => {
                        |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Add, ae)
                    }
                    scanner::Token::MinusEq => {
                        |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Subtract, ae)
                    }
                    scanner::Token::LtLtEq => {
                        |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::LeftShift, ae)
                    }
                    scanner::Token::GtGtEq => {
                        |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::SignedRightShift, ae)
                    }
                    scanner::Token::GtGtGtEq => {
                        |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::UnsignedRightShift, ae)
                    }
                    scanner::Token::AmpEq => {
                        |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::BitwiseAnd, ae)
                    }
                    scanner::Token::PipeEq => {
                        |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::BitwiseOr, ae)
                    }
                    scanner::Token::StarStarEq => {
                        |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Exponentiate, ae)
                    }
                    scanner::Token::CaretEq | _ => {
                        |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::BitwiseXor, ae)
                    }
                };

                let pot_ae = AssignmentExpression::parse(parser, after_op, in_flag, yield_flag, await_flag)?;
                if let Some((ae, after_ae)) = pot_ae {
                    return Ok(Some((Box::new(make_ae(lhs, ae)), after_ae)));
                }
            }
        }

        let pot_ce = ConditionalExpression::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        match pot_ce {
            None => Ok(None),
            Some((ce, after_ce)) => Ok(Some((Box::new(AssignmentExpression::FallThru(ce)), after_ce))),
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
        self.pprint_with_leftpad(writer, pad, state)
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::pretty_check;
//}
