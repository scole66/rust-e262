use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use test_case::test_case;

// MULTIPLICATIVE OPERATOR
#[test]
fn multiplicative_operator_test_01() {
    let (mo, scanner) = check(MultiplicativeOperator::parse(&mut newparser("*"), Scanner::new()));
    chk_scan(&scanner, 1);
    assert!(matches!(*mo, MultiplicativeOperator::Multiply));
    pretty_check(&*mo, "MultiplicativeOperator: *", vec![]);
    concise_check(&*mo, "Punctuator: *", vec![]);
    format!("{:?}", mo);
}
#[test]
fn multiplicative_operator_test_02() {
    let (mo, scanner) = check(MultiplicativeOperator::parse(&mut newparser("/"), Scanner::new()));
    chk_scan(&scanner, 1);
    assert!(matches!(*mo, MultiplicativeOperator::Divide));
    pretty_check(&*mo, "MultiplicativeOperator: /", vec![]);
    concise_check(&*mo, "Punctuator: /", vec![]);
    format!("{:?}", mo);
}
#[test]
fn multiplicative_operator_test_03() {
    let (mo, scanner) = check(MultiplicativeOperator::parse(&mut newparser("%"), Scanner::new()));
    chk_scan(&scanner, 1);
    assert!(matches!(*mo, MultiplicativeOperator::Modulo));
    pretty_check(&*mo, "MultiplicativeOperator: %", vec![]);
    concise_check(&*mo, "Punctuator: %", vec![]);
    format!("{:?}", mo);
}
#[test]
fn multiplicative_operator_test_04() {
    check_err(MultiplicativeOperator::parse(&mut newparser("@"), Scanner::new()), "One of [‘*’, ‘/’, ‘%’] expected", 1, 1);
}
#[test]
fn multiplicative_operator_test_prettycheck_1() {
    let (item, _) = MultiplicativeOperator::parse(&mut newparser("*"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn multiplicative_operator_test_prettycheck_2() {
    let (item, _) = MultiplicativeOperator::parse(&mut newparser("/"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn multiplicative_operator_test_prettycheck_3() {
    let (item, _) = MultiplicativeOperator::parse(&mut newparser("%"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn multiplicative_operator_test_concisecheck_1() {
    let (item, _) = MultiplicativeOperator::parse(&mut newparser("*"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn multiplicative_operator_test_concisecheck_2() {
    let (item, _) = MultiplicativeOperator::parse(&mut newparser("/"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn multiplicative_operator_test_concisecheck_3() {
    let (item, _) = MultiplicativeOperator::parse(&mut newparser("%"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn multiplicative_operator_test_contains_01() {
    let (item, _) = MultiplicativeOperator::parse(&mut newparser("*"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

// MULTIPLICATIVE EXPRESSION
#[test]
fn multiplicative_expression_test_01() {
    let (me, scanner) = check(MultiplicativeExpression::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*me, MultiplicativeExpression::ExponentiationExpression(_)));
    pretty_check(&*me, "MultiplicativeExpression: a", vec!["ExponentiationExpression: a"]);
    concise_check(&*me, "IdentifierName: a", vec![]);
    format!("{:?}", me);
    assert_eq!(me.is_function_definition(), false);
    assert_eq!(me.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn multiplicative_expression_test_02() {
    let (me, scanner) = check(MultiplicativeExpression::parse(&mut newparser("a/b"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*me, MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(..)));
    pretty_check(&*me, "MultiplicativeExpression: a / b", vec!["MultiplicativeExpression: a", "MultiplicativeOperator: /", "ExponentiationExpression: b"]);
    concise_check(&*me, "MultiplicativeExpression: a / b", vec!["IdentifierName: a", "Punctuator: /", "IdentifierName: b"]);
    format!("{:?}", me);
    assert_eq!(me.is_function_definition(), false);
    assert_eq!(me.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn multiplicative_expression_test_04() {
    let (me, scanner) = check(MultiplicativeExpression::parse(&mut newparser("a/b * @"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*me, MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(..)));
    pretty_check(&*me, "MultiplicativeExpression: a / b", vec!["MultiplicativeExpression: a", "MultiplicativeOperator: /", "ExponentiationExpression: b"]);
    concise_check(&*me, "MultiplicativeExpression: a / b", vec!["IdentifierName: a", "Punctuator: /", "IdentifierName: b"]);
    format!("{:?}", me);
}
#[test]
fn multiplicative_expression_test_03() {
    check_err(MultiplicativeExpression::parse(&mut newparser(""), Scanner::new(), false, false), "ExponentiationExpression expected", 1, 1);
}
#[test]
fn multiplicative_expression_test_prettycheck_1() {
    let (item, _) = MultiplicativeExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn multiplicative_expression_test_prettycheck_2() {
    let (item, _) = MultiplicativeExpression::parse(&mut newparser("a*1"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn multiplicative_expression_test_concisecheck_1() {
    let (item, _) = MultiplicativeExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn multiplicative_expression_test_concisecheck_2() {
    let (item, _) = MultiplicativeExpression::parse(&mut newparser("a*1"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn multiplicative_expression_test_contains_01() {
    let (item, _) = MultiplicativeExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn multiplicative_expression_test_contains_02() {
    let (item, _) = MultiplicativeExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn multiplicative_expression_test_contains_03() {
    let (item, _) = MultiplicativeExpression::parse(&mut newparser("this * 0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn multiplicative_expression_test_contains_04() {
    let (item, _) = MultiplicativeExpression::parse(&mut newparser("0 * this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn multiplicative_expression_test_contains_05() {
    let (item, _) = MultiplicativeExpression::parse(&mut newparser("0 * 0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("'string'" => Some(String::from("string")); "String Token")]
#[test_case("a*b" => None; "Not token")]
fn multiplicative_expression_test_as_string_literal(src: &str) -> Option<String> {
    let (item, _) = MultiplicativeExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.as_string_literal().map(|st| String::from(st.value))
}
#[test_case("item.#valid" => true; "Fallthru valid")]
#[test_case("item.#valid * b" => true; "Left valid")]
#[test_case("a * item.#valid" => true; "Right valid")]
#[test_case("item.#invalid" => false; "Fallthru invalid")]
#[test_case("item.#invalid * b" => false; "Left invalid")]
#[test_case("a * item.#invalid" => false; "Right invalid")]
fn multiplicative_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = MultiplicativeExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
