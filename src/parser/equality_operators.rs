use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::relational_operators::RelationalExpression;
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

// EqualityExpression[In, Yield, Await] :
//      RelationalExpression[?In, ?Yield, ?Await]
//      EqualityExpression[?In, ?Yield, ?Await] == RelationalExpression[?In, ?Yield, ?Await]
//      EqualityExpression[?In, ?Yield, ?Await] != RelationalExpression[?In, ?Yield, ?Await]
//      EqualityExpression[?In, ?Yield, ?Await] === RelationalExpression[?In, ?Yield, ?Await]
//      EqualityExpression[?In, ?Yield, ?Await] !== RelationalExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum EqualityExpression {
    RelationalExpression(Box<RelationalExpression>),
    Equal(Box<EqualityExpression>, Box<RelationalExpression>),
    NotEqual(Box<EqualityExpression>, Box<RelationalExpression>),
    StrictEqual(Box<EqualityExpression>, Box<RelationalExpression>),
    NotStrictEqual(Box<EqualityExpression>, Box<RelationalExpression>),
}

impl fmt::Display for EqualityExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            EqualityExpression::RelationalExpression(re) => write!(f, "{}", re),
            EqualityExpression::Equal(ee, re) => write!(f, "{} == {}", ee, re),
            EqualityExpression::NotEqual(ee, re) => write!(f, "{} != {}", ee, re),
            EqualityExpression::StrictEqual(ee, re) => write!(f, "{} === {}", ee, re),
            EqualityExpression::NotStrictEqual(ee, re) => write!(f, "{} !== {}", ee, re),
        }
    }
}

impl PrettyPrint for EqualityExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}EqualityExpression: {}", first, self)?;
        match &self {
            EqualityExpression::RelationalExpression(re) => re.pprint_with_leftpad(writer, &successive, Spot::Final),
            EqualityExpression::Equal(ee, re)
            | EqualityExpression::NotEqual(ee, re)
            | EqualityExpression::StrictEqual(ee, re)
            | EqualityExpression::NotStrictEqual(ee, re) => {
                ee.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                re.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut work = |ee: &Box<EqualityExpression>, re: &Box<RelationalExpression>, op| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}EqualityExpression: {}", first, self)
                .and_then(|_| ee.concise_with_leftpad(writer, &successive, Spot::NotFinal))
                .and_then(|_| pprint_token(writer, op, &successive, Spot::NotFinal))
                .and_then(|_| re.concise_with_leftpad(writer, &successive, Spot::Final))
        };

        match self {
            EqualityExpression::RelationalExpression(node) => node.concise_with_leftpad(writer, pad, state),
            EqualityExpression::Equal(ee, re) => work(ee, re, "=="),
            EqualityExpression::NotEqual(ee, re) => work(ee, re, "!="),
            EqualityExpression::StrictEqual(ee, re) => work(ee, re, "==="),
            EqualityExpression::NotStrictEqual(ee, re) => work(ee, re, "!=="),
        }
    }
}

impl IsFunctionDefinition for EqualityExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            EqualityExpression::RelationalExpression(re) => re.is_function_definition(),
            _ => false,
        }
    }
}

impl AssignmentTargetType for EqualityExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            EqualityExpression::RelationalExpression(re) => re.assignment_target_type(),
            _ => ATTKind::Invalid,
        }
    }
}

impl EqualityExpression {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_re1 = RelationalExpression::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        match pot_re1 {
            None => Ok(None),
            Some((re1, after_re1)) => {
                let mut current = Box::new(EqualityExpression::RelationalExpression(re1));
                let mut current_scanner = after_re1;
                loop {
                    let (op_token, after_op) =
                        scanner::scan_token(&current_scanner, parser.source, scanner::ScanGoal::InputElementDiv);
                    let make_ee = match op_token {
                        scanner::Token::EqEq => |ee, re| EqualityExpression::Equal(ee, re),
                        scanner::Token::BangEq => |ee, re| EqualityExpression::NotEqual(ee, re),
                        scanner::Token::EqEqEq => |ee, re| EqualityExpression::StrictEqual(ee, re),
                        scanner::Token::BangEqEq => |ee, re| EqualityExpression::NotStrictEqual(ee, re),
                        _ => {
                            break;
                        }
                    };
                    let pot_re2 = RelationalExpression::parse(parser, after_op, in_flag, yield_flag, await_flag)?;
                    match pot_re2 {
                        None => {
                            break;
                        }
                        Some((re2, after_re2)) => {
                            current = Box::new(make_ee(current, re2));
                            current_scanner = after_re2;
                        }
                    }
                }
                Ok(Some((current, current_scanner)))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_none, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};

    // EQUALITY EXPRESSION
    #[test]
    fn equality_expression_test_01() {
        let (se, scanner) = check(EqualityExpression::parse(
            &mut newparser("a"),
            Scanner::new(),
            true,
            false,
            false,
        ));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, EqualityExpression::RelationalExpression(_)));
        pretty_check(&*se, "EqualityExpression: a", vec!["RelationalExpression: a"]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn equality_expression_test_02() {
        let (se, scanner) = check(EqualityExpression::parse(
            &mut newparser("a==b"),
            Scanner::new(),
            true,
            false,
            false,
        ));
        chk_scan(&scanner, 4);
        assert!(matches!(&*se, EqualityExpression::Equal(_, _)));
        pretty_check(
            &*se,
            "EqualityExpression: a == b",
            vec!["EqualityExpression: a", "RelationalExpression: b"],
        );
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn equality_expression_test_03() {
        let (se, scanner) = check(EqualityExpression::parse(
            &mut newparser("a!=b"),
            Scanner::new(),
            true,
            false,
            false,
        ));
        chk_scan(&scanner, 4);
        assert!(matches!(&*se, EqualityExpression::NotEqual(_, _)));
        pretty_check(
            &*se,
            "EqualityExpression: a != b",
            vec!["EqualityExpression: a", "RelationalExpression: b"],
        );
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn equality_expression_test_04() {
        let (se, scanner) = check(EqualityExpression::parse(
            &mut newparser("a===b"),
            Scanner::new(),
            true,
            false,
            false,
        ));
        chk_scan(&scanner, 5);
        assert!(matches!(&*se, EqualityExpression::StrictEqual(_, _)));
        pretty_check(
            &*se,
            "EqualityExpression: a === b",
            vec!["EqualityExpression: a", "RelationalExpression: b"],
        );
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn equality_expression_test_05() {
        let (se, scanner) = check(EqualityExpression::parse(
            &mut newparser("a!==b"),
            Scanner::new(),
            true,
            false,
            false,
        ));
        chk_scan(&scanner, 5);
        assert!(matches!(&*se, EqualityExpression::NotStrictEqual(_, _)));
        pretty_check(
            &*se,
            "EqualityExpression: a !== b",
            vec!["EqualityExpression: a", "RelationalExpression: b"],
        );
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn equality_expression_test_06() {
        check_none(EqualityExpression::parse(
            &mut newparser(""),
            Scanner::new(),
            true,
            false,
            false,
        ));
    }
    #[test]
    fn equality_expression_test_07() {
        assert!(EqualityExpression::parse(&mut newparser("\\u0066or"), Scanner::new(), true, false, false).is_err());
        assert!(
            EqualityExpression::parse(&mut newparser("a == \\u0066or"), Scanner::new(), true, false, false).is_err()
        );
    }
    #[test]
    fn equality_expression_test_08() {
        let (se, scanner) = check(EqualityExpression::parse(
            &mut newparser("a != @"),
            Scanner::new(),
            true,
            false,
            false,
        ));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, EqualityExpression::RelationalExpression(_)));
        pretty_check(&*se, "EqualityExpression: a", vec!["RelationalExpression: a"]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn equality_expression_test_prettyerrors() {
        let (item, _) = EqualityExpression::parse(&mut newparser("3!=4"), Scanner::new(), true, false, false)
            .unwrap()
            .unwrap();
        pretty_error_validate(*item);
    }
}
