use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::assignment_operators::AssignmentExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// Expression[In, Yield, Await] :
//      AssignmentExpression[?In, ?Yield, ?Await]
//      Expression[?In, ?Yield, ?Await] , AssignmentExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum Expression {
    FallThru(Box<AssignmentExpression>),
    Comma(Box<Expression>, Box<AssignmentExpression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Expression::FallThru(node) => node.fmt(f),
            Expression::Comma(left, right) => write!(f, "{} , {}", left, right),
        }
    }
}

impl PrettyPrint for Expression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Expression: {}", first, self)?;
        match &self {
            Expression::FallThru(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Expression::Comma(left, right) => {
                left.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                right.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            Expression::FallThru(node) => node.concise_with_leftpad(writer, pad, state),
            Expression::Comma(left, right) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}Expression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for Expression {
    fn is_function_definition(&self) -> bool {
        match &self {
            Expression::FallThru(node) => node.is_function_definition(),
            Expression::Comma(_, _) => false,
        }
    }
}

impl AssignmentTargetType for Expression {
    fn assignment_target_type(&self) -> ATTKind {
        match &self {
            Expression::FallThru(node) => node.assignment_target_type(),
            Expression::Comma(_, _) => ATTKind::Invalid,
        }
    }
}

impl Expression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("Expression expected", scanner.line, scanner.column)).otherwise(|| {
            AssignmentExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).and_then(|(left, after_left)| {
                let mut current = Box::new(Expression::FallThru(left));
                let mut current_scanner = after_left;
                loop {
                    match scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
                        .and_then(|after_token| AssignmentExpression::parse(parser, after_token, in_flag, yield_flag, await_flag))
                    {
                        Err(_) => {
                            break;
                        }
                        Ok((right, after_right)) => {
                            current = Box::new(Expression::Comma(current, right));
                            current_scanner = after_right;
                        }
                    }
                }
                Ok((current, current_scanner))
            })
        })

        //let pot_left = AssignmentExpression::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        //match pot_left {
        //    None => Ok(None),
        //    Some((left, after_left)) => {
        //        let mut current = Box::new(Expression::FallThru(left));
        //        let mut current_scanner = after_left;
        //        loop {
        //            let (token, after_token) = scan_token(&current_scanner, parser.source, ScanGoal::InputElementDiv);
        //            match token {
        //                Token::Punctuator(Punctuator::Comma) => {
        //                    let pot_right = AssignmentExpression::parse(parser, after_token, in_flag, yield_flag, await_flag)?;
        //                    match pot_right {
        //                        None => {
        //                            break;
        //                        }
        //                        Some((right, after_right)) => {
        //                            current = Box::new(Expression::Comma(current, right));
        //                            current_scanner = after_right;
        //                        }
        //                    }
        //                }
        //                _ => {
        //                    break;
        //                }
        //            }
        //        }
        //        Ok(Some((current, current_scanner)))
        //    }
        //}
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::pretty_check;
//}
