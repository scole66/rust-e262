use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::assignment_operators::AssignmentExpression;
use super::binary_logical_operators::ShortCircuitExpression;
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

// ConditionalExpression[In, Yield, Await] :
//      ShortCircuitExpression[?In, ?Yield, ?Await]
//      ShortCircuitExpression[?In, ?Yield, ?Await] ? AssignmentExpression[+In, ?Yield, ?Await] : AssignmentExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum ConditionalExpression {
    FallThru(Box<ShortCircuitExpression>),
    Conditional(
        Box<ShortCircuitExpression>,
        Box<AssignmentExpression>,
        Box<AssignmentExpression>,
    ),
}

impl fmt::Display for ConditionalExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ConditionalExpression::FallThru(node) => node.fmt(f),
            ConditionalExpression::Conditional(condition, thenish, elseish) => {
                write!(f, "{} ? {} : {}", condition, thenish, elseish)
            }
        }
    }
}

impl PrettyPrint for ConditionalExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ConditionalExpression: {}", first, self)?;
        match &self {
            ConditionalExpression::FallThru(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            ConditionalExpression::Conditional(condition, thenish, elseish) => {
                condition.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                thenish.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                elseish.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            ConditionalExpression::FallThru(node) => node.concise_with_leftpad(writer, pad, state),
            ConditionalExpression::Conditional(a, b, c) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}ConditionalExpression: {}", first, self)?;
                a.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "?", &successive, Spot::NotFinal)?;
                b.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ":", &successive, Spot::NotFinal)?;
                c.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for ConditionalExpression {
    fn is_function_definition(&self) -> bool {
        match &self {
            ConditionalExpression::Conditional(_, _, _) => false,
            ConditionalExpression::FallThru(node) => node.is_function_definition(),
        }
    }
}

impl AssignmentTargetType for ConditionalExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match &self {
            ConditionalExpression::Conditional(_, _, _) => ATTKind::Invalid,
            ConditionalExpression::FallThru(node) => node.assignment_target_type(),
        }
    }
}

impl ConditionalExpression {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_left = ShortCircuitExpression::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        match pot_left {
            None => Ok(None),
            Some((left, after_left)) => {
                let (op, after_op) =
                    scanner::scan_token(&after_left, parser.source, scanner::ScanGoal::InputElementDiv);
                match op {
                    scanner::Token::Question => {
                        let pot_then = AssignmentExpression::parse(parser, after_op, true, yield_flag, await_flag)?;
                        match pot_then {
                            None => Ok(Some((Box::new(ConditionalExpression::FallThru(left)), after_left))),
                            Some((thenish, after_then)) => {
                                let (nextop, after_nextop) =
                                    scanner::scan_token(&after_then, parser.source, scanner::ScanGoal::InputElementDiv);
                                match nextop {
                                    scanner::Token::Colon => {
                                        let pot_else = AssignmentExpression::parse(
                                            parser,
                                            after_nextop,
                                            in_flag,
                                            yield_flag,
                                            await_flag,
                                        )?;
                                        match pot_else {
                                            None => {
                                                Ok(Some((Box::new(ConditionalExpression::FallThru(left)), after_left)))
                                            }
                                            Some((elseish, after_else)) => Ok(Some((
                                                Box::new(ConditionalExpression::Conditional(left, thenish, elseish)),
                                                after_else,
                                            ))),
                                        }
                                    }
                                    _ => Ok(Some((Box::new(ConditionalExpression::FallThru(left)), after_left))),
                                }
                            }
                        }
                    }
                    _ => Ok(Some((Box::new(ConditionalExpression::FallThru(left)), after_left))),
                }
            }
        }
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}
