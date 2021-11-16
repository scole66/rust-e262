use super::testhelp::{check, chk_scan, expected_scan, newparser, sv};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_data, concise_error_validate, pretty_check, pretty_data, pretty_error_validate};
use test_case::test_case;

#[test]
fn assignment_expression_test_01() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, AssignmentExpression::FallThru(..)));
    pretty_check(&*node, "AssignmentExpression: a", vec!["ConditionalExpression: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
    assert!(!node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn assignment_expression_test_02() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("yield a"), Scanner::new(), true, true, false));
    chk_scan(&scanner, 7);
    assert!(matches!(&*node, AssignmentExpression::Yield(..)));
    pretty_check(&*node, "AssignmentExpression: yield a", vec!["YieldExpression: yield a"]);
    concise_check(&*node, "YieldExpression: yield a", vec!["Keyword: yield", "IdentifierName: a"]);
    format!("{:?}", node);
    assert!(!node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_03() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a=>a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*node, AssignmentExpression::Arrow(..)));
    pretty_check(&*node, "AssignmentExpression: a => a", vec!["ArrowFunction: a => a"]);
    concise_check(&*node, "ArrowFunction: a => a", vec!["IdentifierName: a", "Punctuator: =>", "IdentifierName: a"]);
    format!("{:?}", node);
    assert!(node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_031() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("async a=>a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 10);
    assert!(matches!(&*node, AssignmentExpression::AsyncArrow(..)));
    pretty_check(&*node, "AssignmentExpression: async a => a", vec!["AsyncArrowFunction: async a => a"]);
    concise_check(&*node, "AsyncArrowFunction: async a => a", vec!["Keyword: async", "IdentifierName: a", "Punctuator: =>", "IdentifierName: a"]);
    format!("{:?}", node);
    assert!(node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_04() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*node, AssignmentExpression::Assignment(..)));
    pretty_check(&*node, "AssignmentExpression: a = b", vec!["LeftHandSideExpression: a", "AssignmentExpression: b"]);
    concise_check(&*node, "AssignmentExpression: a = b", vec!["IdentifierName: a", "Punctuator: =", "IdentifierName: b"]);
    format!("{:?}", node);
    assert!(!node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_05() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a*=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
    pretty_check(&*node, "AssignmentExpression: a *= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: *=", "AssignmentExpression: b"]);
    concise_check(&*node, "AssignmentExpression: a *= b", vec!["IdentifierName: a", "Punctuator: *=", "IdentifierName: b"]);
    format!("{:?}", node);
    assert!(!node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_06() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a/=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
    pretty_check(&*node, "AssignmentExpression: a /= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: /=", "AssignmentExpression: b"]);
    concise_check(&*node, "AssignmentExpression: a /= b", vec!["IdentifierName: a", "Punctuator: /=", "IdentifierName: b"]);
    format!("{:?}", node);
    assert!(!node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_07() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a%=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
    pretty_check(&*node, "AssignmentExpression: a %= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: %=", "AssignmentExpression: b"]);
    concise_check(&*node, "AssignmentExpression: a %= b", vec!["IdentifierName: a", "Punctuator: %=", "IdentifierName: b"]);
    format!("{:?}", node);
    assert!(!node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_08() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a+=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
    pretty_check(&*node, "AssignmentExpression: a += b", vec!["LeftHandSideExpression: a", "AssignmentOperator: +=", "AssignmentExpression: b"]);
    concise_check(&*node, "AssignmentExpression: a += b", vec!["IdentifierName: a", "Punctuator: +=", "IdentifierName: b"]);
    format!("{:?}", node);
    assert!(!node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_09() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a-=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
    pretty_check(&*node, "AssignmentExpression: a -= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: -=", "AssignmentExpression: b"]);
    concise_check(&*node, "AssignmentExpression: a -= b", vec!["IdentifierName: a", "Punctuator: -=", "IdentifierName: b"]);
    format!("{:?}", node);
    assert!(!node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_10() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a<<=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
    pretty_check(&*node, "AssignmentExpression: a <<= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: <<=", "AssignmentExpression: b"]);
    concise_check(&*node, "AssignmentExpression: a <<= b", vec!["IdentifierName: a", "Punctuator: <<=", "IdentifierName: b"]);
    format!("{:?}", node);
    assert!(!node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_11() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a>>=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
    pretty_check(&*node, "AssignmentExpression: a >>= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: >>=", "AssignmentExpression: b"]);
    concise_check(&*node, "AssignmentExpression: a >>= b", vec!["IdentifierName: a", "Punctuator: >>=", "IdentifierName: b"]);
    format!("{:?}", node);
    assert!(!node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_12() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a>>>=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
    pretty_check(&*node, "AssignmentExpression: a >>>= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: >>>=", "AssignmentExpression: b"]);
    concise_check(&*node, "AssignmentExpression: a >>>= b", vec!["IdentifierName: a", "Punctuator: >>>=", "IdentifierName: b"]);
    format!("{:?}", node);
    assert!(!node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_13() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a&=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
    pretty_check(&*node, "AssignmentExpression: a &= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: &=", "AssignmentExpression: b"]);
    concise_check(&*node, "AssignmentExpression: a &= b", vec!["IdentifierName: a", "Punctuator: &=", "IdentifierName: b"]);
    format!("{:?}", node);
    assert!(!node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_14() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a^=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
    pretty_check(&*node, "AssignmentExpression: a ^= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: ^=", "AssignmentExpression: b"]);
    concise_check(&*node, "AssignmentExpression: a ^= b", vec!["IdentifierName: a", "Punctuator: ^=", "IdentifierName: b"]);
    format!("{:?}", node);
    assert!(!node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_15() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a|=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
    pretty_check(&*node, "AssignmentExpression: a |= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: |=", "AssignmentExpression: b"]);
    concise_check(&*node, "AssignmentExpression: a |= b", vec!["IdentifierName: a", "Punctuator: |=", "IdentifierName: b"]);
    format!("{:?}", node);
    assert!(!node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_16() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a**=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*node, AssignmentExpression::OpAssignment(..)));
    pretty_check(&*node, "AssignmentExpression: a **= b", vec!["LeftHandSideExpression: a", "AssignmentOperator: **=", "AssignmentExpression: b"]);
    concise_check(&*node, "AssignmentExpression: a **= b", vec!["IdentifierName: a", "Punctuator: **=", "IdentifierName: b"]);
    format!("{:?}", node);
    assert!(!node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_17() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a&&=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*node, AssignmentExpression::LandAssignment(..)));
    pretty_check(&*node, "AssignmentExpression: a &&= b", vec!["LeftHandSideExpression: a", "AssignmentExpression: b"]);
    concise_check(&*node, "AssignmentExpression: a &&= b", vec!["IdentifierName: a", "Punctuator: &&=", "IdentifierName: b"]);
    format!("{:?}", node);
    assert!(!node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_18() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a||=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*node, AssignmentExpression::LorAssignment(..)));
    pretty_check(&*node, "AssignmentExpression: a ||= b", vec!["LeftHandSideExpression: a", "AssignmentExpression: b"]);
    concise_check(&*node, "AssignmentExpression: a ||= b", vec!["IdentifierName: a", "Punctuator: ||=", "IdentifierName: b"]);
    format!("{:?}", node);
    assert!(!node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_19() {
    let (node, scanner) = check(AssignmentExpression::parse(&mut newparser("a??=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*node, AssignmentExpression::CoalAssignment(..)));
    pretty_check(&*node, "AssignmentExpression: a ??= b", vec!["LeftHandSideExpression: a", "AssignmentExpression: b"]);
    concise_check(&*node, "AssignmentExpression: a ??= b", vec!["IdentifierName: a", "Punctuator: ??=", "IdentifierName: b"]);
    format!("{:?}", node);
    assert!(!node.is_function_definition());
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn assignment_expression_test_cache_01() {
    let mut parser = newparser("a+=b+c+d+e");
    let (node, scanner) = check(AssignmentExpression::parse(&mut parser, Scanner::new(), true, false, false));
    let (node2, scanner2) = check(AssignmentExpression::parse(&mut parser, Scanner::new(), true, false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn assignment_expression_test_prettyerrors_1() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_2() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("yield a"), Scanner::new(), true, true, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_3() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a=>a"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_31() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("async a=>a"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_4() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a=b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_5() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a*=b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_6() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a/=b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_7() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a%=b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_8() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a+=b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_9() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a-=b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_10() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a<<=b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_11() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a>>=b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_12() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a>>>=b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_13() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a&=b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_14() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a^=b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_15() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a|=b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_16() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a**=b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_17() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a&&=b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_18() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a||=b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_prettyerrors_19() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a??=b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_1() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_2() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("yield a"), Scanner::new(), true, true, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_3() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a=>a"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_31() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("async a=>a"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_4() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a=b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_5() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a*=b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_6() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a/=b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_7() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a%=b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_8() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a+=b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_9() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a-=b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_10() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a<<=b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_11() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a>>=b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_12() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a>>>=b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_13() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a&=b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_14() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a^=b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_15() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a|=b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_16() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a**=b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_17() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a&&=b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_18() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a||=b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_conciseerrors_19() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a??=b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn assignment_expression_test_contains_01() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("this"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn assignment_expression_test_contains_02() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("0"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_expression_test_contains_03() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("yield this"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn assignment_expression_test_contains_04() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("yield 0"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_expression_test_contains_05() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("x => { this; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn assignment_expression_test_contains_06() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("x => { 0; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_expression_test_contains_07() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("async x => { this; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn assignment_expression_test_contains_08() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("async x => { 0; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_expression_test_contains_09() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("this.a = 0"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn assignment_expression_test_contains_10() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a = this"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn assignment_expression_test_contains_11() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a = 0"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_expression_test_contains_12() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("this.a *= 0"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn assignment_expression_test_contains_13() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a *= this"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn assignment_expression_test_contains_14() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a *= 0"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_expression_test_contains_15() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("this.a &&= 0"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn assignment_expression_test_contains_16() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a &&= this"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn assignment_expression_test_contains_17() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a &&= 0"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_expression_test_contains_18() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("this.a ||= 0"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn assignment_expression_test_contains_19() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a ||= this"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn assignment_expression_test_contains_20() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a ||= 0"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_expression_test_contains_21() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("this.a ??= 0"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn assignment_expression_test_contains_22() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a ??= this"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn assignment_expression_test_contains_23() {
    let (item, _) = AssignmentExpression::parse(&mut newparser("a ??= 0"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("'string'" => Some(JSString::from("string")); "String Token")]
#[test_case("a=b" => None; "Not token")]
fn assignment_expression_test_as_string_literal(src: &str) -> Option<JSString> {
    let (item, _) = AssignmentExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.as_string_literal().map(|st| st.value)
}
#[test_case("item.#valid" => true; "Fallthru valid")]
#[test_case("yield item.#valid" => true; "Yield valid")]
#[test_case("x => x.#valid" => true; "ArrowFunction valid")]
#[test_case("async x => x.#valid" => true; "AsyncArrowFunction valid")]
#[test_case("a.#valid = 0" => true; "Assignment/Left valid")]
#[test_case("a = item.#valid" => true; "Assignment/Right valid")]
#[test_case("a.#valid += 0" => true; "AssignmentOp/Left valid")]
#[test_case("a += item.#valid" => true; "AssignmentOp/Right valid")]
#[test_case("a.#valid &&= 0" => true; "AssignmentLand/Left valid")]
#[test_case("a &&= item.#valid" => true; "AssignmentLand/Right valid")]
#[test_case("a.#valid ||= 0" => true; "AssignmentLor/Left valid")]
#[test_case("a ||= item.#valid" => true; "AssignmentLor/Right valid")]
#[test_case("a.#valid ??= 0" => true; "AssignmentCoal/Left valid")]
#[test_case("a ??= item.#valid" => true; "AssignmentCoal/Right valid")]
#[test_case("item.#invalid" => false; "Fallthru invalid")]
#[test_case("yield item.#invalid" => false; "Yield invalid")]
#[test_case("x => x.#invalid" => false; "ArrowFunction invalid")]
#[test_case("async x => x.#invalid" => false; "AsyncArrowFunction invalid")]
#[test_case("a.#invalid = 0" => false; "Assignment/Left invalid")]
#[test_case("a = item.#invalid" => false; "Assignment/Right invalid")]
#[test_case("a.#invalid += 0" => false; "AssignmentOp/Left invalid")]
#[test_case("a += item.#invalid" => false; "AssignmentOp/Right invalid")]
#[test_case("a.#invalid &&= 0" => false; "AssignmentLand/Left invalid")]
#[test_case("a &&= item.#invalid" => false; "AssignmentLand/Right invalid")]
#[test_case("a.#invalid ||= 0" => false; "AssignmentLor/Left invalid")]
#[test_case("a ||= item.#invalid" => false; "AssignmentLor/Right invalid")]
#[test_case("a.#invalid ??= 0" => false; "AssignmentCoal/Left invalid")]
#[test_case("a ??= item.#invalid" => false; "AssignmentCoal/Right invalid")]
fn assignment_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = AssignmentExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}

#[test]
fn assignment_operator_test_contains_01() {
    assert_eq!(AssignmentOperator::Multiply.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_02() {
    assert_eq!(AssignmentOperator::Divide.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_03() {
    assert_eq!(AssignmentOperator::Modulo.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_04() {
    assert_eq!(AssignmentOperator::Add.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_05() {
    assert_eq!(AssignmentOperator::Subtract.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_06() {
    assert_eq!(AssignmentOperator::LeftShift.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_07() {
    assert_eq!(AssignmentOperator::SignedRightShift.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_08() {
    assert_eq!(AssignmentOperator::UnsignedRightShift.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_09() {
    assert_eq!(AssignmentOperator::BitwiseAnd.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_10() {
    assert_eq!(AssignmentOperator::BitwiseXor.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_11() {
    assert_eq!(AssignmentOperator::BitwiseOr.contains(ParseNodeKind::This), false);
}
#[test]
fn assignment_operator_test_contains_12() {
    assert_eq!(AssignmentOperator::Exponentiate.contains(ParseNodeKind::This), false);
}

mod assignment_element {
    use super::*;
    use test_case::test_case;

    #[test_case("a" => Ok((
        expected_scan(1),
        sv(&["AssignmentElement: a", "DestructuringAssignmentTarget: a"]),
        sv(&["IdentifierName: a"]),
    )); "alone")]
    #[test_case("a=0" => Ok((
        expected_scan(3),
        sv(&["AssignmentElement: a = 0", "DestructuringAssignmentTarget: a", "Initializer: = 0"]),
        sv(&["AssignmentElement: a = 0", "IdentifierName: a", "Initializer: = 0"]),
    )); "with initializer")]
    #[test_case("" => Err(ParseError::new("LeftHandSideExpression expected", 1, 1)); "empty")]
    fn parse(src: &str) -> Result<(Scanner, Vec<String>, Vec<String>), ParseError> {
        let (node, scanner) = AssignmentElement::parse(&mut newparser(src), Scanner::new(), false, false)?;
        let pretty_elements = pretty_data(&*node);
        let concise_elements = concise_data(&*node);
        Ok((scanner, pretty_elements, concise_elements))
    }

    #[test]
    fn debug() {
        assert_ne!("", format!("{:?}", AssignmentElement::parse(&mut newparser("A"), Scanner::new(), false, false).unwrap()));
    }

    #[test_case("a")]
    #[test_case("a=0")]
    fn pretty_errors(src: &str) {
        let (item, _) = AssignmentElement::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test_case("a")]
    #[test_case("a=0")]
    fn concise_errors(src: &str) {
        let (item, _) = AssignmentElement::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }

    #[test_case("this" => true; "no init; present")]
    #[test_case("a" => false; "no init; not present")]
    #[test_case("this=0" => true; "init; present in target")]
    #[test_case("a=this" => true; "init; present in init")]
    #[test_case("a=0" => false; "init; not present")]
    fn contains(src: &str) -> bool {
        let (item, _) = AssignmentElement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.contains(ParseNodeKind::This)
    }

    #[test_case("item.#valid" => true; "no init; valid")]
    #[test_case("item.#valid=0" => true; "init; target valid")]
    #[test_case("a=item.#valid" => true; "init; init valid")]
    #[test_case("item.#invalid" => false; "no init; invalid")]
    #[test_case("item.#invalid=0" => false; "init; target invalid")]
    #[test_case("a=item.#invalid" => false; "init; init invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = AssignmentElement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("valid")])
    }
}

mod assignment_rest_element {
    use super::*;
    use test_case::test_case;

    #[test_case("...a" => Ok((
        expected_scan(4),
        sv(&["AssignmentRestElement: ... a", "DestructuringAssignmentTarget: a"]),
        sv(&["AssignmentRestElement: ... a", "Punctuator: ...", "IdentifierName: a"])
    )); "normal")]
    #[test_case("" => Err(ParseError::new("‘...’ expected", 1, 1)); "empty")]
    #[test_case("..." => Err(ParseError::new("LeftHandSideExpression expected", 1, 4)); "dots")]
    fn parse(src: &str) -> Result<(Scanner, Vec<String>, Vec<String>), ParseError> {
        let (node, scanner) = AssignmentRestElement::parse(&mut newparser(src), Scanner::new(), false, false)?;
        let pretty_elements = pretty_data(&*node);
        let concise_elements = concise_data(&*node);
        Ok((scanner, pretty_elements, concise_elements))
    }
    #[test]
    fn debug() {
        let (item, _) = AssignmentRestElement::parse(&mut newparser("...a"), Scanner::new(), false, false).unwrap();
        assert_ne!("", format!("{:?}", item));
    }
    #[test_case("...blue")]
    fn pretty_errors(src: &str) {
        let (item, _) = AssignmentRestElement::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test_case("...blue")]
    fn concise_errors(src: &str) {
        let (item, _) = AssignmentRestElement::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test_case("...this" => true; "has this")]
    #[test_case("...3" => false; "missing this")]
    fn contains(src: &str) -> bool {
        let (node, _) = AssignmentRestElement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        node.contains(ParseNodeKind::This)
    }

    #[test_case("...item.#valid" => true; "valid")]
    #[test_case("...item.#invalid" => false; "invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = AssignmentRestElement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("valid")])
    }
}

mod destructuring_assignment_target {
    use super::*;
    use test_case::test_case;

    #[test_case("a")]
    fn parse(src: &str) {
        let (node, scanner) = check(DestructuringAssignmentTarget::parse(&mut newparser(src), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        pretty_check(&*node, "DestructuringAssignmentTarget: a", vec!["LeftHandSideExpression: a"]);
        concise_check(&*node, "IdentifierName: a", vec![]);
        assert_ne!(format!("{:?}", node), "");
    }
    #[test_case("" => ("LeftHandSideExpression expected".to_string(), 1); "mismatch")]
    fn error(src: &str) -> (String, u32) {
        let err = DestructuringAssignmentTarget::parse(&mut newparser(src), Scanner::new(), true, true).unwrap_err();
        (err.msg, err.column)
    }
    #[test_case("blue")]
    fn pretty_errors(src: &str) {
        let (item, _) = DestructuringAssignmentTarget::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test_case("blue")]
    fn concise_errors(src: &str) {
        let (item, _) = DestructuringAssignmentTarget::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }

    #[test_case("this" => true; "has this")]
    #[test_case("3" => false; "missing this")]
    fn contains(src: &str) -> bool {
        let (node, _) = DestructuringAssignmentTarget::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        node.contains(ParseNodeKind::This)
    }

    #[test_case("item.#valid" => true; "valid")]
    #[test_case("item.#invalid" => false; "invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = DestructuringAssignmentTarget::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("valid")])
    }
}
