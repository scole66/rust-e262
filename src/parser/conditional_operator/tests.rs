use super::testhelp::{check, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

// CONDITIONAL EXPRESSION
#[test]
fn conditional_expression_test_01() {
    let (se, scanner) = check(ConditionalExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*se, ConditionalExpression::FallThru(_)));
    pretty_check(&*se, "ConditionalExpression: a", vec!["ShortCircuitExpression: a"]);
    concise_check(&*se, "IdentifierName: a", vec![]);
    format!("{:?}", se);
    assert_eq!(se.is_function_definition(), false);
    assert_eq!(se.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn conditional_expression_test_02() {
    let (se, scanner) = check(ConditionalExpression::parse(&mut newparser("a?b:c"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*se, ConditionalExpression::Conditional(..)));
    pretty_check(&*se, "ConditionalExpression: a ? b : c", vec!["ShortCircuitExpression: a", "AssignmentExpression: b", "AssignmentExpression: c"]);
    concise_check(&*se, "ConditionalExpression: a ? b : c", vec!["IdentifierName: a", "Punctuator: ?", "IdentifierName: b", "Punctuator: :", "IdentifierName: c"]);
    format!("{:?}", se);
    assert_eq!(se.is_function_definition(), false);
    assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn conditional_expression_test_03() {
    let (_, scanner) = check(ConditionalExpression::parse(&mut newparser("a?"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
}
#[test]
fn conditional_expression_test_04() {
    let (_, scanner) = check(ConditionalExpression::parse(&mut newparser("a?b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
}
#[test]
fn conditional_expression_test_05() {
    let (_, scanner) = check(ConditionalExpression::parse(&mut newparser("a?b:"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
}
#[test]
fn conditional_expression_test_prettyerrors_1() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn conditional_expression_test_prettyerrors_2() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("true?a:b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn conditional_expression_test_conciseerrors_1() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn conditional_expression_test_conciseerrors_2() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("true?a:b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn conditional_expression_test_contains_01() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn conditional_expression_test_contains_02() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn conditional_expression_test_contains_03() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("this?1:0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn conditional_expression_test_contains_04() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("0?this:0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn conditional_expression_test_contains_05() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("0?0:this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn conditional_expression_test_contains_06() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("0?0:0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
