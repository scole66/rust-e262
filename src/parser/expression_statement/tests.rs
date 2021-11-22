use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::test_agent;
use test_case::test_case;

// EXPRESSION STATEMENT
#[test]
fn expression_statement_test_01() {
    let (node, scanner) = check(ExpressionStatement::parse(&mut newparser("a;"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    pretty_check(&*node, "ExpressionStatement: a ;", vec!["Expression: a"]);
    concise_check(&*node, "ExpressionStatement: a ;", vec!["IdentifierName: a", "Punctuator: ;"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn expression_statement_test_02() {
    let (node, scanner) = check(ExpressionStatement::parse(&mut newparser("async;"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    pretty_check(&*node, "ExpressionStatement: async ;", vec!["Expression: async"]);
    concise_check(&*node, "ExpressionStatement: async ;", vec!["IdentifierName: async", "Punctuator: ;"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn expression_statement_test_asi_01() {
    let (node, scanner) = check(ExpressionStatement::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    pretty_check(&*node, "ExpressionStatement: a ;", vec!["Expression: a"]);
    concise_check(&*node, "ExpressionStatement: a ;", vec!["IdentifierName: a", "Punctuator: ;"]);
    format!("{:?}", node);
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn expression_statement_test_err_01() {
    check_err(ExpressionStatement::parse(&mut newparser(""), Scanner::new(), false, false), "Expression expected", 1, 1);
}
#[test]
fn expression_statement_test_err_02() {
    check_err(ExpressionStatement::parse(&mut newparser("{"), Scanner::new(), false, false), "ExpressionStatement expected", 1, 1);
}
#[test]
fn expression_statement_test_err_03() {
    check_err(ExpressionStatement::parse(&mut newparser("function"), Scanner::new(), false, false), "ExpressionStatement expected", 1, 1);
}
#[test]
fn expression_statement_test_err_04() {
    check_err(ExpressionStatement::parse(&mut newparser("class"), Scanner::new(), false, false), "ExpressionStatement expected", 1, 1);
}
#[test]
fn expression_statement_test_err_05() {
    check_err(ExpressionStatement::parse(&mut newparser("let ["), Scanner::new(), false, false), "ExpressionStatement expected", 1, 1);
}
#[test]
fn expression_statement_test_err_06() {
    check_err(ExpressionStatement::parse(&mut newparser("async function"), Scanner::new(), false, false), "ExpressionStatement expected", 1, 1);
}
#[test]
fn expression_statement_test_err_07() {
    check_err(ExpressionStatement::parse(&mut newparser("0 7"), Scanner::new(), false, false), "‘;’ expected", 1, 2);
}
#[test]
fn expression_statement_test_contains_01() {
    let (item, _) = ExpressionStatement::parse(&mut newparser("0;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn expression_statement_test_contains_02() {
    let (item, _) = ExpressionStatement::parse(&mut newparser("a;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("'string';" => Some(JSString::from("string")); "String Token")]
#[test_case("a??b;" => None; "Not token")]
fn expression_statement_test_as_string_literal(src: &str) -> Option<JSString> {
    let (item, _) = ExpressionStatement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.as_string_literal().map(|st| st.value)
}
#[test_case("a.#valid" => true; "valid")]
#[test_case("a.#invalid" => false; "invalid")]
fn expression_statement_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ExpressionStatement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod expression_statement {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ExpressionStatement::parse(&mut newparser("a;"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}
