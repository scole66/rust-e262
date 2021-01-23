use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::left_hand_side_expressions::LeftHandSideExpression;
use super::scanner::Scanner;
use super::unary_operators::UnaryExpression;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

#[derive(Debug)]
pub enum UpdateExpression {
    LeftHandSideExpression(Box<LeftHandSideExpression>),
    PostIncrement(Box<LeftHandSideExpression>),
    PostDecrement(Box<LeftHandSideExpression>),
    PreIncrement(Box<UnaryExpression>),
    PreDecrement(Box<UnaryExpression>),
}

impl fmt::Display for UpdateExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            UpdateExpression::LeftHandSideExpression(boxed) => write!(f, "{}", boxed),
            UpdateExpression::PostIncrement(boxed) => write!(f, "{} ++", boxed),
            UpdateExpression::PostDecrement(boxed) => write!(f, "{} --", boxed),
            UpdateExpression::PreIncrement(boxed) => write!(f, "++ {}", boxed),
            UpdateExpression::PreDecrement(boxed) => write!(f, "-- {}", boxed),
        }
    }
}

impl PrettyPrint for UpdateExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}UpdateExpression: {}", first, self)?;
        match &self {
            UpdateExpression::LeftHandSideExpression(boxed)
            | UpdateExpression::PostIncrement(boxed)
            | UpdateExpression::PostDecrement(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            UpdateExpression::PreIncrement(boxed) | UpdateExpression::PreDecrement(boxed) => {
                boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for UpdateExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            UpdateExpression::LeftHandSideExpression(boxed) => boxed.is_function_definition(),
            UpdateExpression::PostIncrement(_)
            | UpdateExpression::PostDecrement(_)
            | UpdateExpression::PreIncrement(_)
            | UpdateExpression::PreDecrement(_) => false,
        }
    }
}

impl AssignmentTargetType for UpdateExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            UpdateExpression::LeftHandSideExpression(boxed) => boxed.assignment_target_type(),
            UpdateExpression::PostIncrement(_)
            | UpdateExpression::PostDecrement(_)
            | UpdateExpression::PreIncrement(_)
            | UpdateExpression::PreDecrement(_) => ATTKind::Invalid,
        }
    }
}

impl UpdateExpression {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (token, after_token) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        match token {
            scanner::Token::PlusPlus => {
                // Seen ++ ...
                let pot_ue = UnaryExpression::parse(parser, after_token, yield_flag, await_flag)?;
                match pot_ue {
                    Some((boxed, after_exp)) => {
                        // Seen ++ UnaryExpression
                        Ok(Some((Box::new(Self::PreIncrement(boxed)), after_exp)))
                    }
                    None => Ok(None),
                }
            }
            scanner::Token::MinusMinus => {
                // Seen -- ...
                let pot_ue = UnaryExpression::parse(parser, after_token, yield_flag, await_flag)?;
                match pot_ue {
                    Some((boxed, after_exp)) => {
                        // Seen -- UnaryExpression
                        Ok(Some((Box::new(Self::PreDecrement(boxed)), after_exp)))
                    }
                    None => Ok(None),
                }
            }
            _ => {
                let pot_lhs = LeftHandSideExpression::parse(parser, scanner, yield_flag, await_flag)?;
                match pot_lhs {
                    Some((boxed, after_lhs)) => {
                        let (token, after_token) =
                            scanner::scan_token(&after_lhs, parser.source, scanner::ScanGoal::InputElementRegExp);
                        if after_token.line != after_lhs.line {
                            Ok(Some((
                                Box::new(UpdateExpression::LeftHandSideExpression(boxed)),
                                after_lhs,
                            )))
                        } else {
                            match token {
                                scanner::Token::PlusPlus => {
                                    Ok(Some((Box::new(UpdateExpression::PostIncrement(boxed)), after_token)))
                                }
                                scanner::Token::MinusMinus => {
                                    Ok(Some((Box::new(UpdateExpression::PostDecrement(boxed)), after_token)))
                                }
                                _ => Ok(Some((
                                    Box::new(UpdateExpression::LeftHandSideExpression(boxed)),
                                    after_lhs,
                                ))),
                            }
                        }
                    }
                    None => Ok(None),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_none, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::pretty_check;
    // UPDATE EXPRESSION
    #[test]
    fn update_expression_test_lhs() {
        let (ue, scanner) = check(UpdateExpression::parse(
            &mut newparser("78"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 2);
        assert!(matches!(*ue, UpdateExpression::LeftHandSideExpression(_)));
        format!("{:?}", ue);
        pretty_check(&*ue, "UpdateExpression: 78", vec!["LeftHandSideExpression: 78"]);
    }
    #[test]
    fn update_expression_test_preinc() {
        let (ue, scanner) = check(UpdateExpression::parse(
            &mut newparser("++a"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UpdateExpression::PreIncrement(_)));
        format!("{:?}", ue);
        pretty_check(&*ue, "UpdateExpression: ++ a", vec!["UnaryExpression: a"]);
    }
    #[test]
    fn update_expression_test_predec() {
        let (ue, scanner) = check(UpdateExpression::parse(
            &mut newparser("--a"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UpdateExpression::PreDecrement(_)));
        format!("{:?}", ue);
        pretty_check(&*ue, "UpdateExpression: -- a", vec!["UnaryExpression: a"]);
    }
    #[test]
    fn update_expression_test_postinc() {
        let (ue, scanner) = check(UpdateExpression::parse(
            &mut newparser("a++"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UpdateExpression::PostIncrement(_)));
        format!("{:?}", ue);
        pretty_check(&*ue, "UpdateExpression: a ++", vec!["LeftHandSideExpression: a"]);
    }
    #[test]
    fn update_expression_test_postdec() {
        let (ue, scanner) = check(UpdateExpression::parse(
            &mut newparser("a--"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UpdateExpression::PostDecrement(_)));
        format!("{:?}", ue);
        pretty_check(&*ue, "UpdateExpression: a --", vec!["LeftHandSideExpression: a"]);
    }
    #[test]
    fn update_expression_test_newline() {
        let (ue, scanner) = check(UpdateExpression::parse(
            &mut newparser("a\n++"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 1);
        assert!(matches!(*ue, UpdateExpression::LeftHandSideExpression(_)));
    }
    #[test]
    fn update_expression_test_nomatch() {
        check_none(UpdateExpression::parse(
            &mut newparser("**"),
            Scanner::new(),
            false,
            false,
        ));
    }
    #[test]
    fn update_expression_test_syntax_error_01() {
        check_none(UpdateExpression::parse(
            &mut newparser("++ ++"),
            Scanner::new(),
            false,
            false,
        ));
    }
    #[test]
    fn update_expression_test_syntax_error_02() {
        check_none(UpdateExpression::parse(
            &mut newparser("-- ++"),
            Scanner::new(),
            false,
            false,
        ));
    }
}
