use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::exponentiation_operator::ExponentiationExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// MultiplicativeOperator : one of
//      * / %
#[derive(Debug, PartialEq)]
pub enum MultiplicativeOperator {
    Multiply,
    Divide,
    Modulo,
}

impl fmt::Display for MultiplicativeOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            MultiplicativeOperator::Multiply => write!(f, "*"),
            MultiplicativeOperator::Divide => write!(f, "/"),
            MultiplicativeOperator::Modulo => write!(f, "%"),
        }
    }
}

impl PrettyPrint for MultiplicativeOperator {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{}MultiplicativeOperator: {}", first, self)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        pprint_token(writer, &format!("{}", self), TokenType::Punctuator, pad, state)
    }
}

impl MultiplicativeOperator {
    fn parse(parser: &mut Parser, scanner: Scanner) -> Result<(Box<MultiplicativeOperator>, Scanner), ParseError> {
        let (op, after_op) = scan_for_punct_set(scanner, parser.source, ScanGoal::InputElementDiv, &[Punctuator::Star, Punctuator::Slash, Punctuator::Percent])?;
        match op {
            Punctuator::Star => Ok((Box::new(MultiplicativeOperator::Multiply), after_op)),
            Punctuator::Slash => Ok((Box::new(MultiplicativeOperator::Divide), after_op)),
            Punctuator::Percent | _ => Ok((Box::new(MultiplicativeOperator::Modulo), after_op)),
        }
    }
}

// MultiplicativeExpression[Yield, Await] :
//      ExponentiationExpression[?Yield, ?Await]
//      MultiplicativeExpression[?Yield, ?Await] MultiplicativeOperator ExponentiationExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum MultiplicativeExpression {
    ExponentiationExpression(Box<ExponentiationExpression>),
    MultiplicativeExpressionExponentiationExpression(Box<MultiplicativeExpression>, Box<MultiplicativeOperator>, Box<ExponentiationExpression>),
}

impl fmt::Display for MultiplicativeExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            MultiplicativeExpression::ExponentiationExpression(boxed) => write!(f, "{}", boxed),
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(me, mo, ee) => {
                write!(f, "{} {} {}", me, mo, ee)
            }
        }
    }
}

impl IsFunctionDefinition for MultiplicativeExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(..) => false,
            MultiplicativeExpression::ExponentiationExpression(ee) => ee.is_function_definition(),
        }
    }
}

impl AssignmentTargetType for MultiplicativeExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(..) => ATTKind::Invalid,
            MultiplicativeExpression::ExponentiationExpression(ee) => ee.assignment_target_type(),
        }
    }
}

impl PrettyPrint for MultiplicativeExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}MultiplicativeExpression: {}", first, self)?;
        match &self {
            MultiplicativeExpression::ExponentiationExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(me, mo, ee) => {
                me.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                mo.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ee.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            MultiplicativeExpression::ExponentiationExpression(node) => node.concise_with_leftpad(writer, pad, state),
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(me, mo, ee) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}MultiplicativeExpression: {}", first, self)?;
                me.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                mo.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ee.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl MultiplicativeExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (ee, after_ee) = ExponentiationExpression::parse(parser, scanner, yield_flag, await_flag)?;
        let mut current = Box::new(MultiplicativeExpression::ExponentiationExpression(ee));
        let mut current_scanner = after_ee;
        loop {
            match MultiplicativeOperator::parse(parser, current_scanner).and_then(|(op, after_op)| {
                let (ee2, after_ee2) = ExponentiationExpression::parse(parser, after_op, yield_flag, await_flag)?;
                Ok((op, ee2, after_ee2))
            }) {
                Ok((op, ee2, scan)) => {
                    current = Box::new(MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(current, op, ee2));
                    current_scanner = scan;
                }
                Err(_) => {
                    break;
                }
            }
        }
        Ok((current, current_scanner))
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::pretty_check;

    // MULTIPLICATIVE OPERATOR
    #[test]
    fn multiplicative_operator_test_01() {
        let (mo, scanner) = check(MultiplicativeOperator::parse(&mut newparser("*"), Scanner::new()));
        chk_scan(&scanner, 1);
        assert_eq!(*mo, MultiplicativeOperator::Multiply);
        pretty_check(&*mo, "MultiplicativeOperator: *", vec![]);
        format!("{:?}", mo);
    }
    #[test]
    fn multiplicative_operator_test_02() {
        let (mo, scanner) = check(MultiplicativeOperator::parse(&mut newparser("/"), Scanner::new()));
        chk_scan(&scanner, 1);
        assert_eq!(*mo, MultiplicativeOperator::Divide);
        pretty_check(&*mo, "MultiplicativeOperator: /", vec![]);
        format!("{:?}", mo);
    }
    #[test]
    fn multiplicative_operator_test_03() {
        let (mo, scanner) = check(MultiplicativeOperator::parse(&mut newparser("%"), Scanner::new()));
        chk_scan(&scanner, 1);
        assert_eq!(*mo, MultiplicativeOperator::Modulo);
        pretty_check(&*mo, "MultiplicativeOperator: %", vec![]);
        format!("{:?}", mo);
    }
    #[test]
    fn multiplicative_operator_test_04() {
        check_err(MultiplicativeOperator::parse(&mut newparser("@"), Scanner::new()), "One of [‘*’, ‘/’, ‘%’] expected", 1, 1);
    }

    // MULTIPLICATIVE EXPRESSION
    #[test]
    fn multiplicative_expression_test_01() {
        let (me, scanner) = check(MultiplicativeExpression::parse(&mut newparser("a"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*me, MultiplicativeExpression::ExponentiationExpression(_)));
        pretty_check(&*me, "MultiplicativeExpression: a", vec!["ExponentiationExpression: a"]);
        format!("{:?}", me);
        assert_eq!(me.is_function_definition(), false);
        assert_eq!(me.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn multiplicative_expression_test_02() {
        let (me, scanner) = check(MultiplicativeExpression::parse(&mut newparser("a/b"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*me, MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(..)));
        pretty_check(&*me, "MultiplicativeExpression: a / b", vec!["MultiplicativeExpression: a", "MultiplicativeOperator: /", "ExponentiationExpression: b"]);
        format!("{:?}", me);
        assert_eq!(me.is_function_definition(), false);
        assert_eq!(me.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn multiplicative_expression_test_04() {
        let (me, scanner) = check(MultiplicativeExpression::parse(&mut newparser("a/b * @"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*me, MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(..)));
        pretty_check(&*me, "MultiplicativeExpression: a / b", vec!["MultiplicativeExpression: a", "MultiplicativeOperator: /", "ExponentiationExpression: b"]);
        format!("{:?}", me);
    }
    #[test]
    fn multiplicative_expression_test_03() {
        check_err(MultiplicativeExpression::parse(&mut newparser(""), Scanner::new(), false, false), "ExponentiationExpression expected", 1, 1);
    }
}
