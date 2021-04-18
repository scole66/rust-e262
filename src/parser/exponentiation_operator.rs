use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::unary_operators::UnaryExpression;
use super::update_expressions::UpdateExpression;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// ExponentiationExpression[Yield, Await] :
//      UnaryExpression[?Yield, ?Await]
//      UpdateExpression[?Yield, ?Await] ** ExponentiationExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum ExponentiationExpression {
    UnaryExpression(Rc<UnaryExpression>),
    Exponentiation(Rc<UpdateExpression>, Rc<ExponentiationExpression>),
}

impl fmt::Display for ExponentiationExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ExponentiationExpression::UnaryExpression(boxed) => write!(f, "{}", boxed),
            ExponentiationExpression::Exponentiation(ue, ee) => write!(f, "{} ** {}", ue, ee),
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
            ExponentiationExpression::Exponentiation(ue, ee) => {
                ue.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ee.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            ExponentiationExpression::UnaryExpression(node) => node.concise_with_leftpad(writer, pad, state),
            ExponentiationExpression::Exponentiation(left, right) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}ExponentiationExpression: {}", first, self)?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "**", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for ExponentiationExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            ExponentiationExpression::Exponentiation(..) => false,
            ExponentiationExpression::UnaryExpression(ue) => ue.is_function_definition(),
        }
    }
}

impl AssignmentTargetType for ExponentiationExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            ExponentiationExpression::Exponentiation(..) => ATTKind::Invalid,
            ExponentiationExpression::UnaryExpression(ue) => ue.assignment_target_type(),
        }
    }
}

impl ExponentiationExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("ExponentiationExpression expected", scanner.line, scanner.column))
            .otherwise(|| {
                let (ue, after_ue) = UpdateExpression::parse(parser, scanner, yield_flag, await_flag)?;
                let after_op = scan_for_punct(after_ue, parser.source, ScanGoal::InputElementDiv, Punctuator::StarStar)?;
                let (ee, after_ee) = ExponentiationExpression::parse(parser, after_op, yield_flag, await_flag)?;
                Ok((Rc::new(ExponentiationExpression::Exponentiation(ue, ee)), after_ee))
            })
            .otherwise(|| {
                let (unary, after_unary) = UnaryExpression::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(ExponentiationExpression::UnaryExpression(unary)), after_unary))
            })
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // EXPONENTIATION EXPRESSION
    #[test]
    fn exponentiation_expression_test_01() {
        let (se, scanner) = check(ExponentiationExpression::parse(&mut newparser("a"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, ExponentiationExpression::UnaryExpression(_)));
        pretty_check(&*se, "ExponentiationExpression: a", vec!["UnaryExpression: a"]);
        concise_check(&*se, "IdentifierName: a", vec![]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn exponentiation_expression_test_02() {
        let (se, scanner) = check(ExponentiationExpression::parse(&mut newparser("a ** b"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(&*se, ExponentiationExpression::Exponentiation(..)));
        pretty_check(&*se, "ExponentiationExpression: a ** b", vec!["UpdateExpression: a", "ExponentiationExpression: b"]);
        concise_check(&*se, "ExponentiationExpression: a ** b", vec!["IdentifierName: a", "Punctuator: **", "IdentifierName: b"]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn exponentiation_expression_test_03() {
        check_err(ExponentiationExpression::parse(&mut newparser(""), Scanner::new(), false, false), "ExponentiationExpression expected", 1, 1);
    }
    #[test]
    fn exponentiation_expression_test_04() {
        let (se, scanner) = check(ExponentiationExpression::parse(&mut newparser("a ** @"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, ExponentiationExpression::UnaryExpression(_)));
        pretty_check(&*se, "ExponentiationExpression: a", vec!["UnaryExpression: a"]);
        concise_check(&*se, "IdentifierName: a", vec![]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn exponentiation_expression_test_prettyerrors_1() {
        let (item, _) = ExponentiationExpression::parse(&mut newparser("3**4"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn exponentiation_expression_test_prettyerrors_2() {
        let (item, _) = ExponentiationExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn exponentiation_expression_test_conciseerrors_1() {
        let (item, _) = ExponentiationExpression::parse(&mut newparser("3**4"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn exponentiation_expression_test_conciseerrors_2() {
        let (item, _) = ExponentiationExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
}
