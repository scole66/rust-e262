use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::arrow_function_definitions::ArrowFunction;
use super::async_arrow_function_definitions::AsyncArrowFunction;
use super::conditional_operator::ConditionalExpression;
use super::generator_function_definitions::YieldExpression;
use super::left_hand_side_expressions::LeftHandSideExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

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
    OpAssignment(Box<LeftHandSideExpression>, AssignmentOperator, Box<AssignmentExpression>),
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<(Box<AssignmentExpression>, Scanner), ParseError> {
        Err(ParseError::new("AssignmentExpression expected", scanner.line, scanner.column))
            //if yield_flag {
            //    let pot_yield = YieldExpression::parse(parser, scanner, in_flag, await_flag)?;
            //    if let Some((yieldexp, after_yield)) = pot_yield {
            //        return Ok(Some((Box::new(AssignmentExpression::Yield(yieldexp)), after_yield)));
            //    }
            //}
            .otherwise(|| {
                if yield_flag {
                    YieldExpression::parse(parser, scanner, in_flag, await_flag).and_then(|(yieldexp, after_yield)| Ok((Box::new(AssignmentExpression::Yield(yieldexp)), after_yield)))
                } else {
                    Err(ParseError::new("message should never appear", scanner.line, scanner.column))
                }
            })
            //let pot_arrow = ArrowFunction::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
            //if let Some((af, after_af)) = pot_arrow {
            //    return Ok(Some((Box::new(AssignmentExpression::Arrow(af)), after_af)));
            //}
            .otherwise(|| ArrowFunction::parse(parser, scanner, in_flag, yield_flag, await_flag).and_then(|(af, after_af)| Ok((Box::new(AssignmentExpression::Arrow(af)), after_af))))
            //let pot_asyncarrow = AsyncArrowFunction::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
            //if let Some((aaf, after_aaf)) = pot_asyncarrow {
            //    return Ok(Some((Box::new(AssignmentExpression::AsyncArrow(aaf)), after_aaf)));
            //}
            .otherwise(|| {
                AsyncArrowFunction::parse(parser, scanner, in_flag, yield_flag, await_flag).and_then(|(aaf, after_aaf)| Ok((Box::new(AssignmentExpression::AsyncArrow(aaf)), after_aaf)))
            })
            // let pot_lhs = LeftHandSideExpression::parse(parser, scanner, yield_flag, await_flag)?;
            // if let Some((lhs, after_lhs)) = pot_lhs {
            //     let (op, after_op) = scan_token(&after_lhs, parser.source, ScanGoal::InputElementDiv);
            //     if matches!(
            //         &op,
            //         Token::Punctuator(Punctuator::Eq)
            //             | Token::Punctuator(Punctuator::AmpAmpEq)
            //             | Token::Punctuator(Punctuator::PipePipeEq)
            //             | Token::Punctuator(Punctuator::QQEq)
            //             | Token::Punctuator(Punctuator::StarEq)
            //             | Token::Punctuator(Punctuator::SlashEq)
            //             | Token::Punctuator(Punctuator::PercentEq)
            //             | Token::Punctuator(Punctuator::PlusEq)
            //             | Token::Punctuator(Punctuator::MinusEq)
            //             | Token::Punctuator(Punctuator::LtLtEq)
            //             | Token::Punctuator(Punctuator::GtGtEq)
            //             | Token::Punctuator(Punctuator::GtGtGtEq)
            //             | Token::Punctuator(Punctuator::AmpEq)
            //             | Token::Punctuator(Punctuator::PipeEq)
            //             | Token::Punctuator(Punctuator::StarStarEq)
            //             | Token::Punctuator(Punctuator::CaretEq)
            //     ) {
            //         let make_ae = match &op {
            //             Token::Punctuator(Punctuator::Eq) => |lhs, ae| AssignmentExpression::Assignment(lhs, ae),
            //             Token::Punctuator(Punctuator::AmpAmpEq) => |lhs, ae| AssignmentExpression::LandAssignment(lhs, ae),
            //             Token::Punctuator(Punctuator::PipePipeEq) => |lhs, ae| AssignmentExpression::LorAssignment(lhs, ae),
            //             Token::Punctuator(Punctuator::QQEq) => |lhs, ae| AssignmentExpression::CoalAssignment(lhs, ae),
            //             Token::Punctuator(Punctuator::StarEq) => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Multiply, ae),
            //             Token::Punctuator(Punctuator::SlashEq) => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Divide, ae),
            //             Token::Punctuator(Punctuator::PercentEq) => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Modulo, ae),
            //             Token::Punctuator(Punctuator::PlusEq) => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Add, ae),
            //             Token::Punctuator(Punctuator::MinusEq) => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Subtract, ae),
            //             Token::Punctuator(Punctuator::LtLtEq) => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::LeftShift, ae),
            //             Token::Punctuator(Punctuator::GtGtEq) => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::SignedRightShift, ae),
            //             Token::Punctuator(Punctuator::GtGtGtEq) => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::UnsignedRightShift, ae),
            //             Token::Punctuator(Punctuator::AmpEq) => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::BitwiseAnd, ae),
            //             Token::Punctuator(Punctuator::PipeEq) => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::BitwiseOr, ae),
            //             Token::Punctuator(Punctuator::StarStarEq) => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Exponentiate, ae),
            //             Token::Punctuator(Punctuator::CaretEq) | _ => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::BitwiseXor, ae),
            //         };
            //
            //         let pot_ae = AssignmentExpression::parse(parser, after_op, in_flag, yield_flag, await_flag)?;
            //         if let Some((ae, after_ae)) = pot_ae {
            //             return Ok(Some((Box::new(make_ae(lhs, ae)), after_ae)));
            //         }
            //     }
            // }
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
                            Punctuator::Eq => |lhs, ae| AssignmentExpression::Assignment(lhs, ae),
                            Punctuator::AmpAmpEq => |lhs, ae| AssignmentExpression::LandAssignment(lhs, ae),
                            Punctuator::PipePipeEq => |lhs, ae| AssignmentExpression::LorAssignment(lhs, ae),
                            Punctuator::QQEq => |lhs, ae| AssignmentExpression::CoalAssignment(lhs, ae),
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
                            Punctuator::CaretEq | _ => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::BitwiseXor, ae),
                        };
                        AssignmentExpression::parse(parser, after_op, in_flag, yield_flag, await_flag).and_then(|(ae, after_ae)| Ok((Box::new(make_ae(lhs, ae)), after_ae)))
                    })
                })
            })
            // let pot_ce = ConditionalExpression::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
            // match pot_ce {
            //     None => Ok(None),
            //     Some((ce, after_ce)) => Ok(Some((Box::new(AssignmentExpression::FallThru(ce)), after_ce))),
            // }
            .otherwise(|| {
                ConditionalExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).and_then(|(ce, after_ce)| Ok((Box::new(AssignmentExpression::FallThru(ce)), after_ce)))
            })
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
