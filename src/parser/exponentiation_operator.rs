use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::scanner::Scanner;
use super::unary_operators::UnaryExpression;
use super::update_expressions::UpdateExpression;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

#[derive(Debug)]
pub enum ExponentiationExpression {
    UnaryExpression(Box<UnaryExpression>),
    Exponentiation((Box<UpdateExpression>, Box<ExponentiationExpression>)),
}

impl fmt::Display for ExponentiationExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ExponentiationExpression::UnaryExpression(boxed) => write!(f, "{}", boxed),
            ExponentiationExpression::Exponentiation((ue, ee)) => write!(f, "{} ** {}", ue, ee),
        }
    }
}

impl PrettyPrint for ExponentiationExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ExponentiationExpression: {}", first, self)?;
        match &self {
            ExponentiationExpression::UnaryExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            ExponentiationExpression::Exponentiation((ue, ee)) => {
                ue.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ee.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl ExponentiationExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_ue = UpdateExpression::parse(parser, scanner, yield_flag, await_flag)?;
        let mut result = match pot_ue {
            Some((boxed_ue, after_ue)) => {
                let (token, scanner) = scanner::scan_token(&after_ue, parser.source, scanner::ScanGoal::InputElementRegExp)?;
                match token {
                    scanner::Token::StarStar => {
                        let pot_ee = ExponentiationExpression::parse(parser, scanner, yield_flag, await_flag)?;
                        match pot_ee {
                            Some((boxed_ee, after_ee)) => Some((Box::new(ExponentiationExpression::Exponentiation((boxed_ue, boxed_ee))), after_ee)),
                            None => None,
                        }
                    }
                    _ => None,
                }
            }
            None => None,
        };
        if result.is_none() {
            let pot_unary = UnaryExpression::parse(parser, scanner, yield_flag, await_flag)?;
            result = match pot_unary {
                Some((boxed_unary, after_unary)) => Some((Box::new(ExponentiationExpression::UnaryExpression(boxed_unary)), after_unary)),
                None => None,
            }
        }
        Ok(result)
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::pretty_check;
//}
