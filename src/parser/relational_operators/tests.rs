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
#[test]
fn relational_expression_test_contains_01() {
    let (item, _) = RelationalExpression::parse(&mut newparser("this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn relational_expression_test_contains_02() {
    let (item, _) = RelationalExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn relational_expression_test_contains_03() {
    let (item, _) = RelationalExpression::parse(&mut newparser("this < 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn relational_expression_test_contains_04() {
    let (item, _) = RelationalExpression::parse(&mut newparser("0 < this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn relational_expression_test_contains_05() {
    let (item, _) = RelationalExpression::parse(&mut newparser("0 < 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn relational_expression_test_contains_06() {
    let (item, _) = RelationalExpression::parse(&mut newparser("this > 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn relational_expression_test_contains_07() {
    let (item, _) = RelationalExpression::parse(&mut newparser("0 > this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn relational_expression_test_contains_08() {
    let (item, _) = RelationalExpression::parse(&mut newparser("0 > 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn relational_expression_test_contains_09() {
    let (item, _) = RelationalExpression::parse(&mut newparser("this <= 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn relational_expression_test_contains_10() {
    let (item, _) = RelationalExpression::parse(&mut newparser("0 <= this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn relational_expression_test_contains_11() {
    let (item, _) = RelationalExpression::parse(&mut newparser("0 <= 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn relational_expression_test_contains_12() {
    let (item, _) = RelationalExpression::parse(&mut newparser("this >= 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn relational_expression_test_contains_13() {
    let (item, _) = RelationalExpression::parse(&mut newparser("0 >= this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn relational_expression_test_contains_14() {
    let (item, _) = RelationalExpression::parse(&mut newparser("0 >= 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn relational_expression_test_contains_15() {
    let (item, _) = RelationalExpression::parse(&mut newparser("this instanceof 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn relational_expression_test_contains_16() {
    let (item, _) = RelationalExpression::parse(&mut newparser("0 instanceof this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn relational_expression_test_contains_17() {
    let (item, _) = RelationalExpression::parse(&mut newparser("0 instanceof 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn relational_expression_test_contains_18() {
    let (item, _) = RelationalExpression::parse(&mut newparser("this in 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn relational_expression_test_contains_19() {
    let (item, _) = RelationalExpression::parse(&mut newparser("0 in this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn relational_expression_test_contains_20() {
    let (item, _) = RelationalExpression::parse(&mut newparser("0 in 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
