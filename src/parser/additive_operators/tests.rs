use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::test_agent;
use test_case::test_case;

// ADDITIVE EXPRESSION
#[test]
fn additive_expression_test_01() {
    let (ae, scanner) = check(AdditiveExpression::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*ae, AdditiveExpression::MultiplicativeExpression(_)));
    pretty_check(&*ae, "AdditiveExpression: a", vec!["MultiplicativeExpression: a"]);
    concise_check(&*ae, "IdentifierName: a", vec![]);
    format!("{:?}", ae);
    assert_eq!(ae.is_function_definition(), false);
    assert_eq!(ae.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn additive_expression_test_02() {
    let (ae, scanner) = check(AdditiveExpression::parse(&mut newparser("a+b"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*ae, AdditiveExpression::Add(..)));
    pretty_check(&*ae, "AdditiveExpression: a + b", vec!["AdditiveExpression: a", "MultiplicativeExpression: b"]);
    concise_check(&*ae, "AdditiveExpression: a + b", vec!["IdentifierName: a", "Punctuator: +", "IdentifierName: b"]);
    format!("{:?}", ae);
    assert_eq!(ae.is_function_definition(), false);
    assert_eq!(ae.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn additive_expression_test_03() {
    let (ae, scanner) = check(AdditiveExpression::parse(&mut newparser("a-b"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*ae, AdditiveExpression::Subtract(..)));
    pretty_check(&*ae, "AdditiveExpression: a - b", vec!["AdditiveExpression: a", "MultiplicativeExpression: b"]);
    concise_check(&*ae, "AdditiveExpression: a - b", vec!["IdentifierName: a", "Punctuator: -", "IdentifierName: b"]);
    format!("{:?}", ae);
    assert_eq!(ae.is_function_definition(), false);
    assert_eq!(ae.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn additive_expression_test_04() {
    let (ae, scanner) = check(AdditiveExpression::parse(&mut newparser("a-@"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*ae, AdditiveExpression::MultiplicativeExpression(..)));
    pretty_check(&*ae, "AdditiveExpression: a", vec!["MultiplicativeExpression: a"]);
    concise_check(&*ae, "IdentifierName: a", vec![]);
    format!("{:?}", ae);
    assert_eq!(ae.is_function_definition(), false);
    assert_eq!(ae.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn additive_expression_test_05() {
    check_err(AdditiveExpression::parse(&mut newparser(""), Scanner::new(), false, false), "ExponentiationExpression expected", 1, 1);
}
#[test]
fn additive_expression_test_prettyerrors_1() {
    let (item, _) = AdditiveExpression::parse(&mut newparser("3+4"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn additive_expression_test_prettyerrors_2() {
    let (item, _) = AdditiveExpression::parse(&mut newparser("3-4"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn additive_expression_test_conciseerrors_1() {
    let (item, _) = AdditiveExpression::parse(&mut newparser("3+4"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn additive_expression_test_conciseerrors_2() {
    let (item, _) = AdditiveExpression::parse(&mut newparser("3-4"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn additive_expression_test_contains_01() {
    let (item, _) = AdditiveExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn additive_expression_test_contains_02() {
    let (item, _) = AdditiveExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn additive_expression_test_contains_03() {
    let (item, _) = AdditiveExpression::parse(&mut newparser("this + 1"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn additive_expression_test_contains_04() {
    let (item, _) = AdditiveExpression::parse(&mut newparser("1 + this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn additive_expression_test_contains_05() {
    let (item, _) = AdditiveExpression::parse(&mut newparser("1 + 1"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn additive_expression_test_contains_06() {
    let (item, _) = AdditiveExpression::parse(&mut newparser("this - 1"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn additive_expression_test_contains_07() {
    let (item, _) = AdditiveExpression::parse(&mut newparser("1 - this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn additive_expression_test_contains_08() {
    let (item, _) = AdditiveExpression::parse(&mut newparser("1 - 1"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("'string'" => Some(String::from("string")); "String Token")]
#[test_case("a+b" => None; "Not token")]
fn additive_expression_test_as_string_literal(src: &str) -> Option<String> {
    let (item, _) = AdditiveExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.as_string_literal().map(|st| String::from(st.value))
}
#[test_case("item.#valid + blue" => true; "Add valid left")]
#[test_case("blue + item.#valid" => true; "Add valid right")]
#[test_case("blue + item.#invalid" => false; "Add invalid right")]
#[test_case("item.#invalid + blue" => false; "Add invalid left")]
#[test_case("item.#valid - blue" => true; "Sub valid left")]
#[test_case("blue - item.#valid" => true; "Sub valid right")]
#[test_case("blue - item.#invalid" => false; "Sub invalid right")]
#[test_case("item.#invalid - blue" => false; "Sub invalid left")]
#[test_case("item.#invalid" => false; "Fallthru: invalid")]
#[test_case("item.#valid" => true; "Fallthru: valid")]
fn additive_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = AdditiveExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}

#[test]
#[should_panic(expected = "not yet implemented")]
fn additive_expression_test_early_errors() {
    let mut agent = test_agent();
    AdditiveExpression::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, true);
}
