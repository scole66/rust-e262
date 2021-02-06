use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::additive_operators::AdditiveExpression;
use super::scanner::{scan_token, Punctuator, ScanGoal, Scanner, Token};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

// ShiftExpression[Yield, Await] :
//      AdditiveExpression[?Yield, ?Await]
//      ShiftExpression[?Yield, ?Await] << AdditiveExpression[?Yield, ?Await]
//      ShiftExpression[?Yield, ?Await] >> AdditiveExpression[?Yield, ?Await]
//      ShiftExpression[?Yield, ?Await] >>> AdditiveExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum ShiftExpression {
    AdditiveExpression(Box<AdditiveExpression>),
    LeftShift(Box<ShiftExpression>, Box<AdditiveExpression>),
    SignedRightShift(Box<ShiftExpression>, Box<AdditiveExpression>),
    UnsignedRightShift(Box<ShiftExpression>, Box<AdditiveExpression>),
}

impl fmt::Display for ShiftExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ShiftExpression::AdditiveExpression(ae) => write!(f, "{}", ae),
            ShiftExpression::LeftShift(se, ae) => write!(f, "{} << {}", se, ae),
            ShiftExpression::SignedRightShift(se, ae) => write!(f, "{} >> {}", se, ae),
            ShiftExpression::UnsignedRightShift(se, ae) => write!(f, "{} >>> {}", se, ae),
        }
    }
}

impl PrettyPrint for ShiftExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ShiftExpression: {}", first, self)?;
        match self {
            ShiftExpression::AdditiveExpression(ae) => ae.pprint_with_leftpad(writer, &successive, Spot::Final),
            ShiftExpression::LeftShift(se, ae)
            | ShiftExpression::SignedRightShift(se, ae)
            | ShiftExpression::UnsignedRightShift(se, ae) => {
                se.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ae.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut work = |left: &Box<ShiftExpression>, right: &Box<AdditiveExpression>, op| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}ShiftExpression: {}", first, self)
                .and_then(|_| left.concise_with_leftpad(writer, &successive, Spot::NotFinal))
                .and_then(|_| pprint_token(writer, op, &successive, Spot::NotFinal))
                .and_then(|_| right.concise_with_leftpad(writer, &successive, Spot::Final))
        };

        match self {
            ShiftExpression::AdditiveExpression(node) => node.concise_with_leftpad(writer, pad, state),
            ShiftExpression::LeftShift(left, right) => work(left, right, "<<"),
            ShiftExpression::SignedRightShift(left, right) => work(left, right, ">>"),
            ShiftExpression::UnsignedRightShift(left, right) => work(left, right, ">>>"),
        }
    }
}

impl IsFunctionDefinition for ShiftExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            ShiftExpression::AdditiveExpression(child) => child.is_function_definition(),
            _ => false,
        }
    }
}

impl AssignmentTargetType for ShiftExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            ShiftExpression::AdditiveExpression(child) => child.assignment_target_type(),
            _ => ATTKind::Invalid,
        }
    }
}

impl ShiftExpression {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_ae = AdditiveExpression::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_ae {
            None => Ok(None),
            Some((ae, after_ae)) => {
                let mut current = Box::new(ShiftExpression::AdditiveExpression(ae));
                let mut current_scan = after_ae;
                loop {
                    let (shift_op, after_op) = scan_token(&current_scan, parser.source, ScanGoal::InputElementDiv);
                    match shift_op {
                        Token::Punctuator(Punctuator::GtGt)
                        | Token::Punctuator(Punctuator::GtGtGt)
                        | Token::Punctuator(Punctuator::LtLt) => {
                            let pot_ae2 = AdditiveExpression::parse(parser, after_op, yield_flag, await_flag)?;
                            let make_se = if shift_op.matches_punct(Punctuator::GtGt) {
                                |se, ae| ShiftExpression::SignedRightShift(se, ae)
                            } else if shift_op.matches_punct(Punctuator::LtLt) {
                                |se, ae| ShiftExpression::LeftShift(se, ae)
                            } else {
                                |se, ae| ShiftExpression::UnsignedRightShift(se, ae)
                            };
                            match pot_ae2 {
                                None => {
                                    break;
                                }
                                Some((ae2, after_ae2)) => {
                                    current = Box::new(make_se(current, ae2));
                                    current_scan = after_ae2;
                                }
                            }
                        }
                        _ => {
                            break;
                        }
                    }
                }
                Ok(Some((current, current_scan)))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_none, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};

    // SHIFT EXPRESSION
    #[test]
    fn shift_expression_test_01() {
        let (se, scanner) = check(ShiftExpression::parse(
            &mut newparser("a"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, ShiftExpression::AdditiveExpression(_)));
        pretty_check(&*se, "ShiftExpression: a", vec!["AdditiveExpression: a"]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn shift_expression_test_02() {
        let (se, scanner) = check(ShiftExpression::parse(
            &mut newparser("a << b"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 6);
        assert!(matches!(&*se, ShiftExpression::LeftShift(_, _)));
        pretty_check(
            &*se,
            "ShiftExpression: a << b",
            vec!["ShiftExpression: a", "AdditiveExpression: b"],
        );
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn shift_expression_test_03() {
        let (se, scanner) = check(ShiftExpression::parse(
            &mut newparser("a >> b"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 6);
        assert!(matches!(&*se, ShiftExpression::SignedRightShift(_, _)));
        pretty_check(
            &*se,
            "ShiftExpression: a >> b",
            vec!["ShiftExpression: a", "AdditiveExpression: b"],
        );
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn shift_expression_test_04() {
        let (se, scanner) = check(ShiftExpression::parse(
            &mut newparser("a >>> b"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 7);
        assert!(matches!(&*se, ShiftExpression::UnsignedRightShift(_, _)));
        pretty_check(
            &*se,
            "ShiftExpression: a >>> b",
            vec!["ShiftExpression: a", "AdditiveExpression: b"],
        );
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn shift_expression_test_05() {
        check_none(ShiftExpression::parse(&mut newparser(""), Scanner::new(), false, false));
    }
    #[test]
    fn shift_expression_test_06() {
        let (se, scanner) = check(ShiftExpression::parse(
            &mut newparser("a >>> @"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, ShiftExpression::AdditiveExpression(_)));
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn shift_expression_test_07() {
        assert!(ShiftExpression::parse(&mut newparser("\\u0066or"), Scanner::new(), false, false).is_err());
        assert!(ShiftExpression::parse(&mut newparser("a >> \\u0066or"), Scanner::new(), false, false).is_err());
    }
    #[test]
    fn shift_expression_test_prettyerrors() {
        let (item, _) = ShiftExpression::parse(&mut newparser("3>>4"), Scanner::new(), false, false)
            .unwrap()
            .unwrap();
        pretty_error_validate(*item);
    }
}
