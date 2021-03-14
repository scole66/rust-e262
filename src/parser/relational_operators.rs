use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::bitwise_shift_operators::ShiftExpression;
use super::scanner::{scan_token, Keyword, Punctuator, ScanGoal, Scanner, Token};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

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
    ShiftExpression(Rc<ShiftExpression>),
    Less(Rc<RelationalExpression>, Rc<ShiftExpression>),
    Greater(Rc<RelationalExpression>, Rc<ShiftExpression>),
    LessEqual(Rc<RelationalExpression>, Rc<ShiftExpression>),
    GreaterEqual(Rc<RelationalExpression>, Rc<ShiftExpression>),
    InstanceOf(Rc<RelationalExpression>, Rc<ShiftExpression>),
    In(Rc<RelationalExpression>, Rc<ShiftExpression>),
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
        let mut work = |re: &RelationalExpression, se: &ShiftExpression, op, kind| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}RelationalExpression: {}", first, self)
                .and_then(|_| re.concise_with_leftpad(writer, &successive, Spot::NotFinal))
                .and_then(|_| pprint_token(writer, op, kind, &successive, Spot::NotFinal))
                .and_then(|_| se.concise_with_leftpad(writer, &successive, Spot::Final))
        };

        match self {
            RelationalExpression::ShiftExpression(node) => node.concise_with_leftpad(writer, pad, state),
            RelationalExpression::Less(re, se) => work(re, se, "<", TokenType::Punctuator),
            RelationalExpression::Greater(re, se) => work(re, se, ">", TokenType::Punctuator),
            RelationalExpression::LessEqual(re, se) => work(re, se, "<=", TokenType::Punctuator),
            RelationalExpression::GreaterEqual(re, se) => work(re, se, ">=", TokenType::Punctuator),
            RelationalExpression::InstanceOf(re, se) => work(re, se, "instanceof", TokenType::Keyword),
            RelationalExpression::In(re, se) => work(re, se, "in", TokenType::Keyword),
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
    fn is_relational_token(tok: &Token, in_flag: bool) -> bool {
        tok.matches_punct(Punctuator::Lt)
            || tok.matches_punct(Punctuator::Gt)
            || tok.matches_punct(Punctuator::LtEq)
            || tok.matches_punct(Punctuator::GtEq)
            || tok.matches_keyword(Keyword::Instanceof)
            || (tok.matches_keyword(Keyword::In) && in_flag)
    }

    fn parse_core(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (se, after_se) = ShiftExpression::parse(parser, scanner, yield_flag, await_flag)?;
        let mut current = Rc::new(RelationalExpression::ShiftExpression(se));
        let mut current_scanner = after_se;
        loop {
            let (op, after_op) = scan_token(&current_scanner, parser.source, ScanGoal::InputElementDiv);
            let make_re = match &op {
                Token::Punctuator(Punctuator::Lt) => |re, se| RelationalExpression::Less(re, se),
                Token::Punctuator(Punctuator::Gt) => |re, se| RelationalExpression::Greater(re, se),
                Token::Punctuator(Punctuator::LtEq) => |re, se| RelationalExpression::LessEqual(re, se),
                Token::Punctuator(Punctuator::GtEq) => |re, se| RelationalExpression::GreaterEqual(re, se),
                Token::Identifier(id) if id.matches(Keyword::Instanceof) => |re, se| RelationalExpression::InstanceOf(re, se),
                _ => |re, se| RelationalExpression::In(re, se),
            };
            if Self::is_relational_token(&op, in_flag) {
                match ShiftExpression::parse(parser, after_op, yield_flag, await_flag) {
                    Err(_) => {
                        break;
                    }
                    Ok((se2, after_se2)) => {
                        current = Rc::new(make_re(current, se2));
                        current_scanner = after_se2;
                    }
                }
            } else {
                break;
            }
        }
        Ok((current, current_scanner))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = InYieldAwaitKey { scanner, in_flag, yield_flag, await_flag };
        match parser.relational_expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag, yield_flag, await_flag);
                parser.relational_expression_cache.insert(key, result.clone());
                result
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // RELATIONAL EXPRESSION
    #[test]
    fn relational_expression_test_01() {
        let (se, scanner) = check(RelationalExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, RelationalExpression::ShiftExpression(_)));
        pretty_check(&*se, "RelationalExpression: a", vec!["ShiftExpression: a"]);
        concise_check(&*se, "IdentifierName: a", vec![]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn relational_expression_test_02() {
        let (se, scanner) = check(RelationalExpression::parse(&mut newparser("a < b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*se, RelationalExpression::Less(_, _)));
        pretty_check(&*se, "RelationalExpression: a < b", vec!["RelationalExpression: a", "ShiftExpression: b"]);
        concise_check(&*se, "RelationalExpression: a < b", vec!["IdentifierName: a", "Punctuator: <", "IdentifierName: b"]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn relational_expression_test_03() {
        let (se, scanner) = check(RelationalExpression::parse(&mut newparser("a > b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*se, RelationalExpression::Greater(_, _)));
        pretty_check(&*se, "RelationalExpression: a > b", vec!["RelationalExpression: a", "ShiftExpression: b"]);
        concise_check(&*se, "RelationalExpression: a > b", vec!["IdentifierName: a", "Punctuator: >", "IdentifierName: b"]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn relational_expression_test_04() {
        let (se, scanner) = check(RelationalExpression::parse(&mut newparser("a <= b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(&*se, RelationalExpression::LessEqual(_, _)));
        pretty_check(&*se, "RelationalExpression: a <= b", vec!["RelationalExpression: a", "ShiftExpression: b"]);
        concise_check(&*se, "RelationalExpression: a <= b", vec!["IdentifierName: a", "Punctuator: <=", "IdentifierName: b"]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn relational_expression_test_05() {
        let (se, scanner) = check(RelationalExpression::parse(&mut newparser("a >= b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(&*se, RelationalExpression::GreaterEqual(_, _)));
        pretty_check(&*se, "RelationalExpression: a >= b", vec!["RelationalExpression: a", "ShiftExpression: b"]);
        concise_check(&*se, "RelationalExpression: a >= b", vec!["IdentifierName: a", "Punctuator: >=", "IdentifierName: b"]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn relational_expression_test_06() {
        let (se, scanner) = check(RelationalExpression::parse(&mut newparser("a instanceof b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 14);
        assert!(matches!(&*se, RelationalExpression::InstanceOf(_, _)));
        pretty_check(&*se, "RelationalExpression: a instanceof b", vec!["RelationalExpression: a", "ShiftExpression: b"]);
        concise_check(&*se, "RelationalExpression: a instanceof b", vec!["IdentifierName: a", "Keyword: instanceof", "IdentifierName: b"]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn relational_expression_test_07() {
        let (se, scanner) = check(RelationalExpression::parse(&mut newparser("a in b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(&*se, RelationalExpression::In(_, _)));
        pretty_check(&*se, "RelationalExpression: a in b", vec!["RelationalExpression: a", "ShiftExpression: b"]);
        concise_check(&*se, "RelationalExpression: a in b", vec!["IdentifierName: a", "Keyword: in", "IdentifierName: b"]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn relational_expression_test_08() {
        let (se, scanner) = check(RelationalExpression::parse(&mut newparser("a in b"), Scanner::new(), false, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, RelationalExpression::ShiftExpression(_)));
        pretty_check(&*se, "RelationalExpression: a", vec!["ShiftExpression: a"]);
        concise_check(&*se, "IdentifierName: a", vec![]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn relational_expression_test_09() {
        let (se, scanner) = check(RelationalExpression::parse(&mut newparser("a >= @"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, RelationalExpression::ShiftExpression(_)));
        pretty_check(&*se, "RelationalExpression: a", vec!["ShiftExpression: a"]);
        concise_check(&*se, "IdentifierName: a", vec![]);
        format!("{:?}", se);
        assert_eq!(se.is_function_definition(), false);
        assert_eq!(se.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn relational_expression_test_10() {
        check_err(RelationalExpression::parse(&mut newparser(""), Scanner::new(), true, false, false), "ExponentiationExpression expected", 1, 1);
    }
    #[test]
    fn relational_expression_test_prettycheck_1() {
        let (item, _) = RelationalExpression::parse(&mut newparser("3>4"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn relational_expression_test_prettycheck_2() {
        let (item, _) = RelationalExpression::parse(&mut newparser("3<4"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn relational_expression_test_prettycheck_3() {
        let (item, _) = RelationalExpression::parse(&mut newparser("3>=4"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn relational_expression_test_prettycheck_4() {
        let (item, _) = RelationalExpression::parse(&mut newparser("3<=4"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn relational_expression_test_prettycheck_5() {
        let (item, _) = RelationalExpression::parse(&mut newparser("3 instanceof 4"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn relational_expression_test_prettycheck_6() {
        let (item, _) = RelationalExpression::parse(&mut newparser("3 in 4"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn relational_expression_test_concisecheck_1() {
        let (item, _) = RelationalExpression::parse(&mut newparser("3>4"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn relational_expression_test_concisecheck_2() {
        let (item, _) = RelationalExpression::parse(&mut newparser("3<4"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn relational_expression_test_concisecheck_3() {
        let (item, _) = RelationalExpression::parse(&mut newparser("3>=4"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn relational_expression_test_concisecheck_4() {
        let (item, _) = RelationalExpression::parse(&mut newparser("3<=4"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn relational_expression_test_concisecheck_5() {
        let (item, _) = RelationalExpression::parse(&mut newparser("3 instanceof 4"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn relational_expression_test_concisecheck_6() {
        let (item, _) = RelationalExpression::parse(&mut newparser("3 in 4"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
}
