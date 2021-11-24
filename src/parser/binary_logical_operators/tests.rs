use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::test_agent;
use test_case::test_case;

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
#[test_case("'string'" => Some(JSString::from("string")); "String Token")]
#[test_case("a&&b" => None; "Not token")]
fn logical_and_expression_test_as_string_literal(src: &str) -> Option<JSString> {
    let (item, _) = LogicalANDExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.as_string_literal().map(|st| st.value)
}
#[test_case("item.#valid" => true; "Fallthru valid")]
#[test_case("item.#valid && a" => true; "Left valid")]
#[test_case("a && item.#valid" => true; "Right valid")]
#[test_case("item.#invalid" => false; "Fallthru invalid")]
#[test_case("item.#invalid && a" => false; "Left invalid")]
#[test_case("a && item.#invalid" => false; "Right invalid")]
fn logical_and_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = LogicalANDExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod logical_and_expression {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        LogicalANDExpression::parse(&mut newparser("0"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }
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
#[test_case("'string'" => Some(JSString::from("string")); "String Token")]
#[test_case("a||b" => None; "Not token")]
fn logical_or_expression_test_as_string_literal(src: &str) -> Option<JSString> {
    let (item, _) = LogicalORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.as_string_literal().map(|st| st.value)
}
#[test_case("item.#valid" => true; "Fallthru valid")]
#[test_case("item.#valid || a" => true; "Left valid")]
#[test_case("a || item.#valid" => true; "Right valid")]
#[test_case("item.#invalid" => false; "Fallthru invalid")]
#[test_case("item.#invalid || a" => false; "Left invalid")]
#[test_case("a || item.#invalid" => false; "Right invalid")]
fn logical_or_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = LogicalORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod logical_or_expression {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        LogicalORExpression::parse(&mut newparser("0"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }
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
    check_err(CoalesceExpression::parse(&mut newparser(""), Scanner::new(), true, false, false), "RelationalExpression expected", 1, 1);
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
#[test_case("item.#valid ?? a" => true; "Left valid")]
#[test_case("a ?? item.#valid" => true; "Right valid")]
#[test_case("item.#invalid ?? a" => false; "Left invalid")]
#[test_case("a ?? item.#invalid" => false; "Right invalid")]
fn coalesce_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = CoalesceExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod coalesce_expression {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        CoalesceExpression::parse(&mut newparser("0??b"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }
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
#[test_case("item.#valid ?? a" => true; "BitwiseOR valid")]
#[test_case("item.#valid ?? a ?? b" => true; "Coalesce valid")]
#[test_case("item.#invalid ?? a" => false; "BitwiseOR invalid")]
#[test_case("item.#invalid ?? a ?? b" => false; "Coalesce invalid")]
fn coalesce_expression_head_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item_ce, _) = CoalesceExpression::parse(&mut newparser(src), Scanner::new(), true, false, false).unwrap();
    let item = &item_ce.head;
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod coalesce_expression_head {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        let (item_ce, _) = CoalesceExpression::parse(&mut newparser("item.#valid ?? a"), Scanner::new(), true, false, false).unwrap();
        let item = &item_ce.head;
        item.early_errors(&mut test_agent(), &mut vec![], true);
    }
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
#[test_case("'string'" => Some(JSString::from("string")); "String Token")]
#[test_case("a??b" => None; "Not token")]
fn short_circuit_expression_test_as_string_literal(src: &str) -> Option<JSString> {
    let (item, _) = ShortCircuitExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.as_string_literal().map(|st| st.value)
}
#[test_case("item.#valid" => true; "fallthru valid")]
#[test_case("item.#valid ?? a" => true; "coalesce valid")]
#[test_case("item.#invalid" => false; "fallthru invalid")]
#[test_case("item.#invalid ?? a" => false; "coalesce invalid")]
fn short_circuit_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ShortCircuitExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod short_circuit_expression {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ShortCircuitExpression::parse(&mut newparser("0??b"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }
}
