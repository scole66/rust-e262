use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use test_case::test_case;

// UPDATE EXPRESSION
#[test]
fn update_expression_test_lhs() {
    let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("78"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(*ue, UpdateExpression::LeftHandSideExpression(_)));
    format!("{:?}", ue);
    pretty_check(&*ue, "UpdateExpression: 78", vec!["LeftHandSideExpression: 78"]);
    concise_check(&*ue, "Numeric: 78", vec![]);
    assert!(!ue.is_function_definition());
    assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn update_expression_test_lhs_2() {
    let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("(x=>x*2)"), Scanner::new(), false, false));
    chk_scan(&scanner, 8);
    assert!(matches!(*ue, UpdateExpression::LeftHandSideExpression(_)));
    format!("{:?}", ue);
    pretty_check(&*ue, "UpdateExpression: ( x => x * 2 )", vec!["LeftHandSideExpression: ( x => x * 2 )"]);
    concise_check(&*ue, "ParenthesizedExpression: ( x => x * 2 )", vec!["Punctuator: (", "ArrowFunction: x => x * 2", "Punctuator: )"]);
    assert!(ue.is_function_definition());
    assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn update_expression_test_lhs_3() {
    let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("x"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(*ue, UpdateExpression::LeftHandSideExpression(_)));
    format!("{:?}", ue);
    pretty_check(&*ue, "UpdateExpression: x", vec!["LeftHandSideExpression: x"]);
    concise_check(&*ue, "IdentifierName: x", vec![]);
    assert!(!ue.is_function_definition());
    assert_eq!(ue.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn update_expression_test_preinc() {
    let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("++a"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(*ue, UpdateExpression::PreIncrement(_)));
    format!("{:?}", ue);
    pretty_check(&*ue, "UpdateExpression: ++ a", vec!["UnaryExpression: a"]);
    concise_check(&*ue, "UpdateExpression: ++ a", vec!["Punctuator: ++", "IdentifierName: a"]);
    assert!(!ue.is_function_definition());
    assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn update_expression_test_predec() {
    let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("--a"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(*ue, UpdateExpression::PreDecrement(_)));
    format!("{:?}", ue);
    pretty_check(&*ue, "UpdateExpression: -- a", vec!["UnaryExpression: a"]);
    concise_check(&*ue, "UpdateExpression: -- a", vec!["Punctuator: --", "IdentifierName: a"]);
    assert!(!ue.is_function_definition());
    assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn update_expression_test_postinc() {
    let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("a++"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(*ue, UpdateExpression::PostIncrement(_)));
    format!("{:?}", ue);
    pretty_check(&*ue, "UpdateExpression: a ++", vec!["LeftHandSideExpression: a"]);
    concise_check(&*ue, "UpdateExpression: a ++", vec!["IdentifierName: a", "Punctuator: ++"]);
    assert!(!ue.is_function_definition());
    assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn update_expression_test_postdec() {
    let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("a--"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(*ue, UpdateExpression::PostDecrement(_)));
    format!("{:?}", ue);
    pretty_check(&*ue, "UpdateExpression: a --", vec!["LeftHandSideExpression: a"]);
    concise_check(&*ue, "UpdateExpression: a --", vec!["IdentifierName: a", "Punctuator: --"]);
    assert!(!ue.is_function_definition());
    assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn update_expression_test_newline() {
    let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("a\n++"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(*ue, UpdateExpression::LeftHandSideExpression(_)));
}
#[test]
fn update_expression_test_nomatch() {
    check_err(UpdateExpression::parse(&mut newparser("**"), Scanner::new(), false, false), "UpdateExpression expected", 1, 1);
}
#[test]
fn update_expression_test_syntax_error_01() {
    check_err(UpdateExpression::parse(&mut newparser("++ ++"), Scanner::new(), false, false), "UnaryExpression expected", 1, 6);
}
#[test]
fn update_expression_test_syntax_error_02() {
    check_err(UpdateExpression::parse(&mut newparser("-- ++"), Scanner::new(), false, false), "UnaryExpression expected", 1, 6);
}
#[test]
fn update_expression_prettycheck_1() {
    let (item, _) = UpdateExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn update_expression_prettycheck_2() {
    let (item, _) = UpdateExpression::parse(&mut newparser("a++"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn update_expression_prettycheck_3() {
    let (item, _) = UpdateExpression::parse(&mut newparser("a--"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn update_expression_prettycheck_4() {
    let (item, _) = UpdateExpression::parse(&mut newparser("++a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn update_expression_prettycheck_5() {
    let (item, _) = UpdateExpression::parse(&mut newparser("--a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn update_expression_concisecheck_1() {
    let (item, _) = UpdateExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn update_expression_concisecheck_2() {
    let (item, _) = UpdateExpression::parse(&mut newparser("a++"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn update_expression_concisecheck_3() {
    let (item, _) = UpdateExpression::parse(&mut newparser("a--"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn update_expression_concisecheck_4() {
    let (item, _) = UpdateExpression::parse(&mut newparser("++a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn update_expression_concisecheck_5() {
    let (item, _) = UpdateExpression::parse(&mut newparser("--a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn update_expression_test_contains_01() {
    let (item, _) = UpdateExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn update_expression_test_contains_02() {
    let (item, _) = UpdateExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn update_expression_test_contains_03() {
    let (item, _) = UpdateExpression::parse(&mut newparser("this++"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn update_expression_test_contains_04() {
    let (item, _) = UpdateExpression::parse(&mut newparser("0++"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn update_expression_test_contains_05() {
    let (item, _) = UpdateExpression::parse(&mut newparser("this--"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn update_expression_test_contains_06() {
    let (item, _) = UpdateExpression::parse(&mut newparser("0--"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn update_expression_test_contains_07() {
    let (item, _) = UpdateExpression::parse(&mut newparser("++this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn update_expression_test_contains_08() {
    let (item, _) = UpdateExpression::parse(&mut newparser("++0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn update_expression_test_contains_09() {
    let (item, _) = UpdateExpression::parse(&mut newparser("--this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn update_expression_test_contains_10() {
    let (item, _) = UpdateExpression::parse(&mut newparser("--0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("\"string\"" => Some(String::from("string")); "String Token")]
#[test_case("--a" => None; "Not token")]
fn update_expression_test_as_string_literal(src: &str) -> Option<String> {
    let (item, _) = UpdateExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.as_string_literal().map(|st| String::from(st.value))
}
#[test_case("item.#valid" => true; "FallThru valid")]
#[test_case("item.#valid++" => true; "PostInc valid")]
#[test_case("item.#valid--" => true; "PostDec valid")]
#[test_case("++item.#valid" => true; "PreInc valid")]
#[test_case("--item.#valid" => true; "PreDec valid")]
#[test_case("item.#invalid" => false; "FallThru invalid")]
#[test_case("item.#invalid++" => false; "PostInc invalid")]
#[test_case("item.#invalid--" => false; "PostDec invalid")]
#[test_case("++item.#invalid" => false; "PreInc invalid")]
#[test_case("--item.#invalid" => false; "PreDec invalid")]
fn update_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = UpdateExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
