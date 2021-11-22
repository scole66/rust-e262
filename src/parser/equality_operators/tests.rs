use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::test_agent;
use test_case::test_case;

// EQUALITY EXPRESSION
#[test]
fn equality_expression_test_01() {
    let (se, scanner) = check(EqualityExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*se, EqualityExpression::RelationalExpression(_)));
    pretty_check(&*se, "EqualityExpression: a", vec!["RelationalExpression: a"]);
    concise_check(&*se, "IdentifierName: a", vec![]);
    format!("{:?}", se);
    assert_eq!(se.is_function_definition(), false);
    assert_eq!(se.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn equality_expression_test_02() {
    let (se, scanner) = check(EqualityExpression::parse(&mut newparser("a==b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*se, EqualityExpression::Equal(_, _)));
    pretty_check(&*se, "EqualityExpression: a == b", vec!["EqualityExpression: a", "RelationalExpression: b"]);
    concise_check(&*se, "EqualityExpression: a == b", vec!["IdentifierName: a", "Punctuator: ==", "IdentifierName: b"]);
    format!("{:?}", se);
    assert_eq!(se.is_function_definition(), false);
    assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn equality_expression_test_03() {
    let (se, scanner) = check(EqualityExpression::parse(&mut newparser("a!=b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*se, EqualityExpression::NotEqual(_, _)));
    pretty_check(&*se, "EqualityExpression: a != b", vec!["EqualityExpression: a", "RelationalExpression: b"]);
    concise_check(&*se, "EqualityExpression: a != b", vec!["IdentifierName: a", "Punctuator: !=", "IdentifierName: b"]);
    format!("{:?}", se);
    assert_eq!(se.is_function_definition(), false);
    assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn equality_expression_test_04() {
    let (se, scanner) = check(EqualityExpression::parse(&mut newparser("a===b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*se, EqualityExpression::StrictEqual(_, _)));
    pretty_check(&*se, "EqualityExpression: a === b", vec!["EqualityExpression: a", "RelationalExpression: b"]);
    concise_check(&*se, "EqualityExpression: a === b", vec!["IdentifierName: a", "Punctuator: ===", "IdentifierName: b"]);
    format!("{:?}", se);
    assert_eq!(se.is_function_definition(), false);
    assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn equality_expression_test_05() {
    let (se, scanner) = check(EqualityExpression::parse(&mut newparser("a!==b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*se, EqualityExpression::NotStrictEqual(_, _)));
    pretty_check(&*se, "EqualityExpression: a !== b", vec!["EqualityExpression: a", "RelationalExpression: b"]);
    concise_check(&*se, "EqualityExpression: a !== b", vec!["IdentifierName: a", "Punctuator: !==", "IdentifierName: b"]);
    format!("{:?}", se);
    assert_eq!(se.is_function_definition(), false);
    assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn equality_expression_test_06() {
    check_err(EqualityExpression::parse(&mut newparser(""), Scanner::new(), true, false, false), "RelationalExpression expected", 1, 1);
}
#[test]
fn equality_expression_test_08() {
    let (se, scanner) = check(EqualityExpression::parse(&mut newparser("a != @"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*se, EqualityExpression::RelationalExpression(_)));
    pretty_check(&*se, "EqualityExpression: a", vec!["RelationalExpression: a"]);
    concise_check(&*se, "IdentifierName: a", vec![]);
    format!("{:?}", se);
    assert_eq!(se.is_function_definition(), false);
    assert_eq!(se.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn equality_expression_test_prettyerrors_1() {
    let (item, _) = EqualityExpression::parse(&mut newparser("3!=4"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn equality_expression_test_conciseerrors_1() {
    let (item, _) = EqualityExpression::parse(&mut newparser("3!=4"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn equality_expression_test_prettyerrors_2() {
    let (item, _) = EqualityExpression::parse(&mut newparser("3==4"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn equality_expression_test_conciseerrors_2() {
    let (item, _) = EqualityExpression::parse(&mut newparser("3==4"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn equality_expression_test_prettyerrors_3() {
    let (item, _) = EqualityExpression::parse(&mut newparser("3!==4"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn equality_expression_test_conciseerrors_3() {
    let (item, _) = EqualityExpression::parse(&mut newparser("3!==4"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn equality_expression_test_prettyerrors_4() {
    let (item, _) = EqualityExpression::parse(&mut newparser("3===4"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn equality_expression_test_conciseerrors_4() {
    let (item, _) = EqualityExpression::parse(&mut newparser("3===4"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn equality_expression_test_contains_01() {
    let (item, _) = EqualityExpression::parse(&mut newparser("this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn equality_expression_test_contains_02() {
    let (item, _) = EqualityExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn equality_expression_test_contains_03() {
    let (item, _) = EqualityExpression::parse(&mut newparser("this == 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn equality_expression_test_contains_04() {
    let (item, _) = EqualityExpression::parse(&mut newparser("0 == this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn equality_expression_test_contains_05() {
    let (item, _) = EqualityExpression::parse(&mut newparser("0 == 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn equality_expression_test_contains_06() {
    let (item, _) = EqualityExpression::parse(&mut newparser("this != 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn equality_expression_test_contains_07() {
    let (item, _) = EqualityExpression::parse(&mut newparser("0 != this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn equality_expression_test_contains_08() {
    let (item, _) = EqualityExpression::parse(&mut newparser("0 != 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn equality_expression_test_contains_09() {
    let (item, _) = EqualityExpression::parse(&mut newparser("this === 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn equality_expression_test_contains_10() {
    let (item, _) = EqualityExpression::parse(&mut newparser("0 === this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn equality_expression_test_contains_11() {
    let (item, _) = EqualityExpression::parse(&mut newparser("0 === 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn equality_expression_test_contains_12() {
    let (item, _) = EqualityExpression::parse(&mut newparser("this !== 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn equality_expression_test_contains_13() {
    let (item, _) = EqualityExpression::parse(&mut newparser("0 !== this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn equality_expression_test_contains_14() {
    let (item, _) = EqualityExpression::parse(&mut newparser("0 !== 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("'string'" => Some(JSString::from("string")); "String Token")]
#[test_case("a==b" => None; "Not token")]
fn equality_expression_test_as_string_literal(src: &str) -> Option<JSString> {
    let (item, _) = EqualityExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.as_string_literal().map(|st| st.value)
}
#[test_case("item.#valid" => true; "fallthru valid")]
#[test_case("item.#valid==a" => true; "Eq Left valid")]
#[test_case("a==item.#valid" => true; "Eq Right valid")]
#[test_case("item.#valid!=a" => true; "Ne Left valid")]
#[test_case("a!=item.#valid" => true; "Ne Right valid")]
#[test_case("item.#valid===a" => true; "StrictEq Left valid")]
#[test_case("a===item.#valid" => true; "StrictEq Right valid")]
#[test_case("item.#valid!==a" => true; "StrictNe Left valid")]
#[test_case("a!==item.#valid" => true; "StrictNe Right valid")]
#[test_case("item.#invalid" => false; "fallthru invalid")]
#[test_case("item.#invalid==a" => false; "Eq Left invalid")]
#[test_case("a==item.#invalid" => false; "Eq Right invalid")]
#[test_case("item.#invalid!=a" => false; "Ne Left invalid")]
#[test_case("a!=item.#invalid" => false; "Ne Right invalid")]
#[test_case("item.#invalid===a" => false; "StrictEq Left invalid")]
#[test_case("a===item.#invalid" => false; "StrictEq Right invalid")]
#[test_case("item.#invalid!==a" => false; "StrictNe Left invalid")]
#[test_case("a!==item.#invalid" => false; "StrictNe Right invalid")]
fn equality_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = EqualityExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod equality_expression {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        EqualityExpression::parse(&mut newparser("a"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}
