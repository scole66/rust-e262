use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::bitwise_shift_operators::ShiftExpression;
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

// RelationalExpression[In, Yield, Await] :
//      ShiftExpression[?Yield, ?Await]
//      RelationalExpression[?In, ?Yield, ?Await] < ShiftExpression[?Yield, ?Await]
//      RelationalExpression[?In, ?Yield, ?Await] > ShiftExpression[?Yield, ?Await]
//      RelationalExpression[?In, ?Yield, ?Await] <= ShiftExpression[?Yield, ?Await]
//      RelationalExpression[?In, ?Yield, ?Await] >= ShiftExpression[?Yield, ?Await]
//      RelationalExpression[?In, ?Yield, ?Await] instanceof ShiftExpression[?Yield, ?Await]
//      [+In] RelationalExpression[+In, ?Yield, ?Await] in ShiftExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum RelationalExpression {
    ShiftExpression(Box<ShiftExpression>),
    Less(Box<RelationalExpression>, Box<ShiftExpression>),
    Greater(Box<RelationalExpression>, Box<ShiftExpression>),
    LessEqual(Box<RelationalExpression>, Box<ShiftExpression>),
    GreaterEqual(Box<RelationalExpression>, Box<ShiftExpression>),
    InstanceOf(Box<RelationalExpression>, Box<ShiftExpression>),
    In(Box<RelationalExpression>, Box<ShiftExpression>),
}

impl fmt::Display for RelationalExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RelationalExpression::ShiftExpression(se) => write!(f, "{}", se),
            RelationalExpression::Less(re, se) => write!(f, "{} < {}", re, se),
            RelationalExpression::Greater(re, se) => write!(f, "{} > {}", re, se),
            RelationalExpression::LessEqual(re, se) => write!(f, "{} <= {}", re, se),
            RelationalExpression::GreaterEqual(re, se) => write!(f, "{} >= {}", re, se),
            RelationalExpression::InstanceOf(re, se) => write!(f, "{} instanceof {}", re, se),
            RelationalExpression::In(re, se) => write!(f, "{} in {}", re, se),
        }
    }
}

impl PrettyPrint for RelationalExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}RelationalExpression: {}", first, self)?;
        match &self {
            RelationalExpression::ShiftExpression(se) => se.pprint_with_leftpad(writer, &successive, Spot::Final),
            RelationalExpression::Less(re, se)
            | RelationalExpression::Greater(re, se)
            | RelationalExpression::LessEqual(re, se)
            | RelationalExpression::GreaterEqual(re, se)
            | RelationalExpression::InstanceOf(re, se)
            | RelationalExpression::In(re, se) => {
                re.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                se.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut work = |re: &Box<RelationalExpression>, se: &Box<ShiftExpression>, op| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}RelationalExpression: {}", first, self)
                .and_then(|_| re.concise_with_leftpad(writer, &successive, Spot::NotFinal))
                .and_then(|_| pprint_token(writer, op, &successive, Spot::NotFinal))
                .and_then(|_| se.concise_with_leftpad(writer, &successive, Spot::Final))
        };

        match self {
            RelationalExpression::ShiftExpression(node) => node.concise_with_leftpad(writer, pad, state),
            RelationalExpression::Less(re, se) => work(re, se, "<"),
            RelationalExpression::Greater(re, se) => work(re, se, ">"),
            RelationalExpression::LessEqual(re, se) => work(re, se, "<="),
            RelationalExpression::GreaterEqual(re, se) => work(re, se, ">="),
            RelationalExpression::InstanceOf(re, se) => work(re, se, "instanceof"),
            RelationalExpression::In(re, se) => work(re, se, "in"),
        }
    }
}

impl IsFunctionDefinition for RelationalExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            RelationalExpression::ShiftExpression(se) => se.is_function_definition(),
            _ => false,
        }
    }
}

impl AssignmentTargetType for RelationalExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            RelationalExpression::ShiftExpression(se) => se.assignment_target_type(),
            _ => ATTKind::Invalid,
        }
    }
}

impl RelationalExpression {
    fn is_relational_token(tok: &scanner::Token, in_flag: bool) -> bool {
        match tok {
            scanner::Token::Lt | scanner::Token::Gt | scanner::Token::LtEq | scanner::Token::GtEq => true,
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Instanceof) => true,
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::In) => in_flag,
            _ => false,
        }
    }
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_shift = ShiftExpression::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_shift {
            None => Ok(None),
            Some((se, after_se)) => {
                let mut current = Box::new(RelationalExpression::ShiftExpression(se));
                let mut current_scanner = after_se;
                loop {
                    let (op, after_op) =
                        scanner::scan_token(&current_scanner, parser.source, scanner::ScanGoal::InputElementDiv);
                    let make_re = match &op {
                        scanner::Token::Lt => |re, se| RelationalExpression::Less(re, se),
                        scanner::Token::Gt => |re, se| RelationalExpression::Greater(re, se),
                        scanner::Token::LtEq => |re, se| RelationalExpression::LessEqual(re, se),
                        scanner::Token::GtEq => |re, se| RelationalExpression::GreaterEqual(re, se),
                        scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Instanceof) => {
                            |re, se| RelationalExpression::InstanceOf(re, se)
                        }
                        _ => |re, se| RelationalExpression::In(re, se),
                    };
                    if Self::is_relational_token(&op, in_flag) {
                        let pot_shift2 = ShiftExpression::parse(parser, after_op, yield_flag, await_flag)?;
                        match pot_shift2 {
                            None => {
                                break;
                            }
                            Some((se2, after_se2)) => {
                                current = Box::new(make_re(current, se2));
                                current_scanner = after_se2;
                            }
                        }
                    } else {
                        break;
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
    use crate::prettyprint::testhelp::pretty_check;

    // RELATIONAL EXPRESSION
    #[test]
    fn relational_expression_test_01() {
        let (se, scanner) = check(RelationalExpression::parse(
            &mut newparser("a"),
            Scanner::new(),
            true,
            false,
            false,
        ));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, RelationalExpression::ShiftExpression(_)));
        pretty_check(&*se, "RelationalExpression: a", vec!["ShiftExpression: a"]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn relational_expression_test_02() {
        let (se, scanner) = check(RelationalExpression::parse(
            &mut newparser("a < b"),
            Scanner::new(),
            true,
            false,
            false,
        ));
        chk_scan(&scanner, 5);
        assert!(matches!(&*se, RelationalExpression::Less(_, _)));
        pretty_check(
            &*se,
            "RelationalExpression: a < b",
            vec!["RelationalExpression: a", "ShiftExpression: b"],
        );
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn relational_expression_test_03() {
        let (se, scanner) = check(RelationalExpression::parse(
            &mut newparser("a > b"),
            Scanner::new(),
            true,
            false,
            false,
        ));
        chk_scan(&scanner, 5);
        assert!(matches!(&*se, RelationalExpression::Greater(_, _)));
        pretty_check(
            &*se,
            "RelationalExpression: a > b",
            vec!["RelationalExpression: a", "ShiftExpression: b"],
        );
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn relational_expression_test_04() {
        let (se, scanner) = check(RelationalExpression::parse(
            &mut newparser("a <= b"),
            Scanner::new(),
            true,
            false,
            false,
        ));
        chk_scan(&scanner, 6);
        assert!(matches!(&*se, RelationalExpression::LessEqual(_, _)));
        pretty_check(
            &*se,
            "RelationalExpression: a <= b",
            vec!["RelationalExpression: a", "ShiftExpression: b"],
        );
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn relational_expression_test_05() {
        let (se, scanner) = check(RelationalExpression::parse(
            &mut newparser("a >= b"),
            Scanner::new(),
            true,
            false,
            false,
        ));
        chk_scan(&scanner, 6);
        assert!(matches!(&*se, RelationalExpression::GreaterEqual(_, _)));
        pretty_check(
            &*se,
            "RelationalExpression: a >= b",
            vec!["RelationalExpression: a", "ShiftExpression: b"],
        );
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn relational_expression_test_06() {
        let (se, scanner) = check(RelationalExpression::parse(
            &mut newparser("a instanceof b"),
            Scanner::new(),
            true,
            false,
            false,
        ));
        chk_scan(&scanner, 14);
        assert!(matches!(&*se, RelationalExpression::InstanceOf(_, _)));
        pretty_check(
            &*se,
            "RelationalExpression: a instanceof b",
            vec!["RelationalExpression: a", "ShiftExpression: b"],
        );
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn relational_expression_test_07() {
        let (se, scanner) = check(RelationalExpression::parse(
            &mut newparser("a in b"),
            Scanner::new(),
            true,
            false,
            false,
        ));
        chk_scan(&scanner, 6);
        assert!(matches!(&*se, RelationalExpression::In(_, _)));
        pretty_check(
            &*se,
            "RelationalExpression: a in b",
            vec!["RelationalExpression: a", "ShiftExpression: b"],
        );
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn relational_expression_test_08() {
        let (se, scanner) = check(RelationalExpression::parse(
            &mut newparser("a in b"),
            Scanner::new(),
            false,
            false,
            false,
        ));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, RelationalExpression::ShiftExpression(_)));
        pretty_check(&*se, "RelationalExpression: a", vec!["ShiftExpression: a"]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn relational_expression_test_09() {
        let (se, scanner) = check(RelationalExpression::parse(
            &mut newparser("a >= @"),
            Scanner::new(),
            true,
            false,
            false,
        ));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, RelationalExpression::ShiftExpression(_)));
        pretty_check(&*se, "RelationalExpression: a", vec!["ShiftExpression: a"]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn relational_expression_test_10() {
        check_none(RelationalExpression::parse(
            &mut newparser(""),
            Scanner::new(),
            true,
            false,
            false,
        ));
    }
}
