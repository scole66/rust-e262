use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::exponentiation_operator::ExponentiationExpression;
use super::scanner::{scan_token, Punctuator, ScanGoal, Scanner, Token};
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

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
        self.pprint_with_leftpad(writer, pad, state)
    }
}

impl MultiplicativeOperator {
    fn parse(parser: &mut Parser, scanner: Scanner) -> Result<Option<(Box<MultiplicativeOperator>, Scanner)>, String> {
        let (tok, after_tok) = scan_token(&scanner, parser.source, ScanGoal::InputElementDiv);
        match tok {
            Token::Punctuator(Punctuator::Star) => Ok(Some((Box::new(MultiplicativeOperator::Multiply), after_tok))),
            Token::Punctuator(Punctuator::Slash) => Ok(Some((Box::new(MultiplicativeOperator::Divide), after_tok))),
            Token::Punctuator(Punctuator::Percent) => Ok(Some((Box::new(MultiplicativeOperator::Modulo), after_tok))),
            _ => Ok(None),
        }
    }
}

// MultiplicativeExpression[Yield, Await] :
//      ExponentiationExpression[?Yield, ?Await]
//      MultiplicativeExpression[?Yield, ?Await] MultiplicativeOperator ExponentiationExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum MultiplicativeExpression {
    ExponentiationExpression(Box<ExponentiationExpression>),
    MultiplicativeExpressionExponentiationExpression(
        (
            Box<MultiplicativeExpression>,
            Box<MultiplicativeOperator>,
            Box<ExponentiationExpression>,
        ),
    ),
}

impl fmt::Display for MultiplicativeExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            MultiplicativeExpression::ExponentiationExpression(boxed) => write!(f, "{}", boxed),
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression((me, mo, ee)) => {
                write!(f, "{} {} {}", me, mo, ee)
            }
        }
    }
}

impl IsFunctionDefinition for MultiplicativeExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(_) => false,
            MultiplicativeExpression::ExponentiationExpression(ee) => ee.is_function_definition(),
        }
    }
}

impl AssignmentTargetType for MultiplicativeExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(_) => ATTKind::Invalid,
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
            MultiplicativeExpression::ExponentiationExpression(boxed) => {
                boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression((me, mo, ee)) => {
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
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression((me, mo, ee)) => {
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
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_ee = ExponentiationExpression::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_ee {
            Some((ee, after_ee)) => {
                let mut current = Box::new(MultiplicativeExpression::ExponentiationExpression(ee));
                let mut current_scanner = after_ee;
                loop {
                    let pot_op = MultiplicativeOperator::parse(parser, current_scanner)?;
                    if pot_op.is_none() {
                        break;
                    } else {
                        let (op, after_op) = pot_op.unwrap();
                        let pot_ee2 = ExponentiationExpression::parse(parser, after_op, yield_flag, await_flag)?;
                        if pot_ee2.is_none() {
                            break;
                        } else {
                            let (ee2, after_ee2) = pot_ee2.unwrap();
                            current = Box::new(
                                MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression((
                                    current, op, ee2,
                                )),
                            );
                            current_scanner = after_ee2;
                        }
                    }
                }
                Ok(Some((current, current_scanner)))
            }
            _ => Ok(None),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_none, chk_scan, newparser};
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
        check_none(MultiplicativeOperator::parse(&mut newparser("@"), Scanner::new()));
    }

    // MULTIPLICATIVE EXPRESSION
    #[test]
    fn multiplicative_expression_test_01() {
        let (me, scanner) = check(MultiplicativeExpression::parse(
            &mut newparser("a"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 1);
        assert!(matches!(&*me, MultiplicativeExpression::ExponentiationExpression(_)));
        pretty_check(&*me, "MultiplicativeExpression: a", vec!["ExponentiationExpression: a"]);
        format!("{:?}", me);
        assert_eq!(me.is_function_definition(), false);
        assert_eq!(me.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn multiplicative_expression_test_02() {
        let (me, scanner) = check(MultiplicativeExpression::parse(
            &mut newparser("a/b"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 3);
        assert!(matches!(
            &*me,
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(_)
        ));
        pretty_check(
            &*me,
            "MultiplicativeExpression: a / b",
            vec![
                "MultiplicativeExpression: a",
                "MultiplicativeOperator: /",
                "ExponentiationExpression: b",
            ],
        );
        format!("{:?}", me);
        assert_eq!(me.is_function_definition(), false);
        assert_eq!(me.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn multiplicative_expression_test_04() {
        let (me, scanner) = check(MultiplicativeExpression::parse(
            &mut newparser("a/b * @"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 3);
        assert!(matches!(
            &*me,
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(_)
        ));
        pretty_check(
            &*me,
            "MultiplicativeExpression: a / b",
            vec![
                "MultiplicativeExpression: a",
                "MultiplicativeOperator: /",
                "ExponentiationExpression: b",
            ],
        );
        format!("{:?}", me);
    }
    #[test]
    fn multiplicative_expression_test_03() {
        check_none(MultiplicativeExpression::parse(
            &mut newparser(""),
            Scanner::new(),
            false,
            false,
        ));
    }
}
