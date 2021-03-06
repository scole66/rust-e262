use super::testhelp::{check, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

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
