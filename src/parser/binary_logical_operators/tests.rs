use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

// LOGICAL AND EXPRESSION
#[test]
fn logical_and_expression_test_01() {
    let (pn, scanner) = check(LogicalANDExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pn, LogicalANDExpression::BitwiseORExpression(_)));
    pretty_check(&*pn, "LogicalANDExpression: a", vec!["BitwiseORExpression: a"]);
    concise_check(&*pn, "IdentifierName: a", vec![]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
    assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn logical_and_expression_test_02() {
    let (pn, scanner) = check(LogicalANDExpression::parse(&mut newparser("a&&b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*pn, LogicalANDExpression::LogicalAND(..)));
    pretty_check(&*pn, "LogicalANDExpression: a && b", vec!["LogicalANDExpression: a", "BitwiseORExpression: b"]);
    concise_check(&*pn, "LogicalANDExpression: a && b", vec!["IdentifierName: a", "Punctuator: &&", "IdentifierName: b"]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
    assert_eq!(pn.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn logical_and_expression_test_03() {
    let (pn, scanner) = check(LogicalANDExpression::parse(&mut newparser("a&&"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pn, LogicalANDExpression::BitwiseORExpression(..)));
    pretty_check(&*pn, "LogicalANDExpression: a", vec!["BitwiseORExpression: a"]);
    concise_check(&*pn, "IdentifierName: a", vec![]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
    assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn logical_and_expression_test_prettyerrors_1() {
    let (item, _) = LogicalANDExpression::parse(&mut newparser("3"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn logical_and_expression_test_prettyerrors_2() {
    let (item, _) = LogicalANDExpression::parse(&mut newparser("3&&b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn logical_and_expression_test_conciseerrors_1() {
    let (item, _) = LogicalANDExpression::parse(&mut newparser("3"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn logical_and_expression_test_conciseerrors_2() {
    let (item, _) = LogicalANDExpression::parse(&mut newparser("3&&b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn logical_and_expression_test_contains_01() {
    let (item, _) = LogicalANDExpression::parse(&mut newparser("this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn logical_and_expression_test_contains_02() {
    let (item, _) = LogicalANDExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn logical_and_expression_test_contains_03() {
    let (item, _) = LogicalANDExpression::parse(&mut newparser("this && 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn logical_and_expression_test_contains_04() {
    let (item, _) = LogicalANDExpression::parse(&mut newparser("0 && this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn logical_and_expression_test_contains_05() {
    let (item, _) = LogicalANDExpression::parse(&mut newparser("0 && 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

// LOGICAL OR EXPRESSION
#[test]
fn logical_or_expression_test_01() {
    let (pn, scanner) = check(LogicalORExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pn, LogicalORExpression::LogicalANDExpression(_)));
    pretty_check(&*pn, "LogicalORExpression: a", vec!["LogicalANDExpression: a"]);
    concise_check(&*pn, "IdentifierName: a", vec![]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
    assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn logical_or_expression_test_02() {
    let (pn, scanner) = check(LogicalORExpression::parse(&mut newparser("a||b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*pn, LogicalORExpression::LogicalOR(..)));
    pretty_check(&*pn, "LogicalORExpression: a || b", vec!["LogicalORExpression: a", "LogicalANDExpression: b"]);
    concise_check(&*pn, "LogicalORExpression: a || b", vec!["IdentifierName: a", "Punctuator: ||", "IdentifierName: b"]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
    assert_eq!(pn.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn logical_or_expression_test_03() {
    let (pn, scanner) = check(LogicalORExpression::parse(&mut newparser("a||"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pn, LogicalORExpression::LogicalANDExpression(..)));
    pretty_check(&*pn, "LogicalORExpression: a", vec!["LogicalANDExpression: a"]);
    concise_check(&*pn, "IdentifierName: a", vec![]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
    assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn logical_or_expression_test_prettyerrors_1() {
    let (item, _) = LogicalORExpression::parse(&mut newparser("3"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn logical_or_expression_test_prettyerrors_2() {
    let (item, _) = LogicalORExpression::parse(&mut newparser("3||b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn logical_or_expression_test_conciseerrors_1() {
    let (item, _) = LogicalORExpression::parse(&mut newparser("3"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn logical_or_expression_test_conciseerrors_2() {
    let (item, _) = LogicalORExpression::parse(&mut newparser("3||b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn logical_or_expression_test_contains_01() {
    let (item, _) = LogicalORExpression::parse(&mut newparser("this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn logical_or_expression_test_contains_02() {
    let (item, _) = LogicalORExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn logical_or_expression_test_contains_03() {
    let (item, _) = LogicalORExpression::parse(&mut newparser("this || 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn logical_or_expression_test_contains_04() {
    let (item, _) = LogicalORExpression::parse(&mut newparser("0 || this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn logical_or_expression_test_contains_05() {
    let (item, _) = LogicalORExpression::parse(&mut newparser("0 || 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

// COALESCE EXPRESSION
#[test]
fn coalesce_expression_test_01() {
    let (pn, scanner) = check(CoalesceExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    pretty_check(&*pn, "CoalesceExpression: a ?? b", vec!["CoalesceExpressionHead: a", "BitwiseORExpression: b"]);
    concise_check(&*pn, "CoalesceExpression: a ?? b", vec!["IdentifierName: a", "Punctuator: ??", "IdentifierName: b"]);
    format!("{:?}", pn);
}
#[test]
fn coalesce_expression_test_02() {
    let (pn, scanner) = check(CoalesceExpression::parse(&mut newparser("z??a??b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 7);
    pretty_check(&*pn, "CoalesceExpression: z ?? a ?? b", vec!["CoalesceExpressionHead: z ?? a", "BitwiseORExpression: b"]);
    concise_check(&*pn, "CoalesceExpression: z ?? a ?? b", vec!["CoalesceExpression: z ?? a", "Punctuator: ??", "IdentifierName: b"]);
    format!("{:?}", pn);
}
#[test]
fn coalesce_expression_test_cache_01() {
    let mut parser = newparser("z??a??b");
    let (node, scanner) = check(CoalesceExpression::parse(&mut parser, Scanner::new(), true, false, false));
    let (node2, scanner2) = check(CoalesceExpression::parse(&mut parser, Scanner::new(), true, false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn coalesce_expression_test_03() {
    check_err(CoalesceExpression::parse(&mut newparser(""), Scanner::new(), true, false, false), "ExponentiationExpression expected", 1, 1);
}
#[test]
fn coalesce_expression_test_04() {
    check_err(CoalesceExpression::parse(&mut newparser("a??"), Scanner::new(), true, false, false), "Invalid Coalesce Expression", 1, 1);
}
#[test]
fn coalesce_expression_test_prettyerrors_1() {
    let (item, _) = CoalesceExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn coalesce_expression_test_conciseerrors_1() {
    let (item, _) = CoalesceExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn coalesce_expression_test_contains_01() {
    let (item, _) = CoalesceExpression::parse(&mut newparser("this ?? 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn coalesce_expression_test_contains_02() {
    let (item, _) = CoalesceExpression::parse(&mut newparser("0 ?? this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn coalesce_expression_test_contains_03() {
    let (item, _) = CoalesceExpression::parse(&mut newparser("0 ?? 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

// COALESCE EXPRESSION HEAD
#[test]
fn coalesce_expression_head_test_01() {
    let (pn, scanner) = check(CoalesceExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    let head = &*pn.head;
    pretty_check(&*head, "CoalesceExpressionHead: a", vec!["BitwiseORExpression: a"]);
    concise_check(&*head, "IdentifierName: a", vec![]);
}
#[test]
fn coalesce_expression_head_test_02() {
    let (pn, scanner) = check(CoalesceExpression::parse(&mut newparser("z??a??b"), Scanner::new(), true, false, false));
    let head = &*pn.head;
    chk_scan(&scanner, 7);
    pretty_check(&*head, "CoalesceExpressionHead: z ?? a", vec!["CoalesceExpression: z ?? a"]);
    concise_check(&*head, "CoalesceExpression: z ?? a", vec!["IdentifierName: z", "Punctuator: ??", "IdentifierName: a"]);
}
#[test]
fn coalesce_expression_head_test_prettyerrors_1() {
    let (item, _) = CoalesceExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item.head);
}
#[test]
fn coalesce_expression_head_test_prettyerrors_2() {
    let (item, _) = CoalesceExpression::parse(&mut newparser("z??a??b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item.head);
}
#[test]
fn coalesce_expression_head_test_conciseerrors_1() {
    let (item, _) = CoalesceExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item.head);
}
#[test]
fn coalesce_expression_head_test_conciseerrors_2() {
    let (item, _) = CoalesceExpression::parse(&mut newparser("z??a??b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item.head);
}
#[test]
fn coalesce_expression_head_test_contains_01() {
    let (item_ce, _) = CoalesceExpression::parse(&mut newparser("this ?? 0"), Scanner::new(), true, false, false).unwrap();
    let item = &item_ce.head;
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn coalesce_expression_head_test_contains_02() {
    let (item_ce, _) = CoalesceExpression::parse(&mut newparser("0 ?? 0"), Scanner::new(), true, false, false).unwrap();
    let item = &item_ce.head;
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn coalesce_expression_head_test_contains_03() {
    let (item_ce, _) = CoalesceExpression::parse(&mut newparser("this ?? 0 ?? 1"), Scanner::new(), true, false, false).unwrap();
    let item = &item_ce.head;
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn coalesce_expression_head_test_contains_04() {
    let (item_ce, _) = CoalesceExpression::parse(&mut newparser("a ?? 0 ?? 1"), Scanner::new(), true, false, false).unwrap();
    let item = &item_ce.head;
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

// SHORT CIRCUIT EXPRESSION
#[test]
fn short_circuit_expression_test_01() {
    let (pn, scanner) = check(ShortCircuitExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*pn, ShortCircuitExpression::CoalesceExpression(..)));
    pretty_check(&*pn, "ShortCircuitExpression: a ?? b", vec!["CoalesceExpression: a ?? b"]);
    concise_check(&*pn, "CoalesceExpression: a ?? b", vec!["IdentifierName: a", "Punctuator: ??", "IdentifierName: b"]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
    assert_eq!(pn.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn short_circuit_expression_test_02() {
    let (pn, scanner) = check(ShortCircuitExpression::parse(&mut newparser("6"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pn, ShortCircuitExpression::LogicalORExpression(..)));
    pretty_check(&*pn, "ShortCircuitExpression: 6", vec!["LogicalORExpression: 6"]);
    concise_check(&*pn, "Numeric: 6", vec![]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
    assert_eq!(pn.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn short_circuit_expression_test_03() {
    check_err(ShortCircuitExpression::parse(&mut newparser(""), Scanner::new(), true, false, false), "Improper Expression", 1, 1);
}
#[test]
fn short_circuit_expression_test_prettyerrors_1() {
    let (item, _) = ShortCircuitExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn short_circuit_expression_test_prettyerrors_2() {
    let (item, _) = ShortCircuitExpression::parse(&mut newparser("h || q"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn short_circuit_expression_test_conciseerrors_1() {
    let (item, _) = ShortCircuitExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn short_circuit_expression_test_conciseerrors_2() {
    let (item, _) = ShortCircuitExpression::parse(&mut newparser("h || q"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn short_circuit_expression_test_contains_01() {
    let (item, _) = ShortCircuitExpression::parse(&mut newparser("this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn short_circuit_expression_test_contains_02() {
    let (item, _) = ShortCircuitExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn short_circuit_expression_test_contains_03() {
    let (item, _) = ShortCircuitExpression::parse(&mut newparser("this ?? 1"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn short_circuit_expression_test_contains_04() {
    let (item, _) = ShortCircuitExpression::parse(&mut newparser("0 ?? 1"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
