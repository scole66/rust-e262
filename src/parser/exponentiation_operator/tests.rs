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
#[test]
fn exponentiation_expression_test_contains_01() {
    let (item, _) = ExponentiationExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn exponentiation_expression_test_contains_02() {
    let (item, _) = ExponentiationExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn exponentiation_expression_test_contains_03() {
    let (item, _) = ExponentiationExpression::parse(&mut newparser("this ** 1"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn exponentiation_expression_test_contains_04() {
    let (item, _) = ExponentiationExpression::parse(&mut newparser("1 ** this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn exponentiation_expression_test_contains_05() {
    let (item, _) = ExponentiationExpression::parse(&mut newparser("1 ** 1"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
