use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::test_agent;
use test_case::test_case;

// RETURN STATEMENT
#[test]
fn return_statement_test_01() {
    let (node, scanner) = check(ReturnStatement::parse(&mut newparser("return;"), Scanner::new(), false, false));
    chk_scan(&scanner, 7);
    pretty_check(&*node, "ReturnStatement: return ;", vec![]);
    concise_check(&*node, "ReturnStatement: return ;", vec!["Keyword: return", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn return_statement_test_02() {
    let (node, scanner) = check(ReturnStatement::parse(&mut newparser("return null;"), Scanner::new(), false, false));
    chk_scan(&scanner, 12);
    pretty_check(&*node, "ReturnStatement: return null ;", vec!["Expression: null"]);
    concise_check(&*node, "ReturnStatement: return null ;", vec!["Keyword: return", "Keyword: null", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn return_statement_test_asi_01() {
    let (node, scanner) = check(ReturnStatement::parse(&mut newparser("return"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    pretty_check(&*node, "ReturnStatement: return ;", vec![]);
    concise_check(&*node, "ReturnStatement: return ;", vec!["Keyword: return", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn return_statement_test_asi_02() {
    let (node, scanner) = check(ReturnStatement::parse(&mut newparser("return null"), Scanner::new(), false, false));
    chk_scan(&scanner, 11);
    pretty_check(&*node, "ReturnStatement: return null ;", vec!["Expression: null"]);
    concise_check(&*node, "ReturnStatement: return null ;", vec!["Keyword: return", "Keyword: null", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn return_statement_test_err_01() {
    check_err(ReturnStatement::parse(&mut newparser(""), Scanner::new(), false, false), "‘return’ expected", 1, 1);
}
#[test]
fn return_statement_test_err_02() {
    check_err(ReturnStatement::parse(&mut newparser("return ="), Scanner::new(), false, false), "‘;’ expected", 1, 7);
}
#[test]
fn return_statement_test_err_03() {
    check_err(ReturnStatement::parse(&mut newparser("return 0 for"), Scanner::new(), false, false), "‘;’ expected", 1, 9);
}
#[test]
fn return_statement_test_prettyerrors_1() {
    let (item, _) = ReturnStatement::parse(&mut newparser("return;"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn return_statement_test_prettyerrors_2() {
    let (item, _) = ReturnStatement::parse(&mut newparser("return undefined;"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn return_statement_test_conciseerrors_1() {
    let (item, _) = ReturnStatement::parse(&mut newparser("return;"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn return_statement_test_conciseerrors_2() {
    let (item, _) = ReturnStatement::parse(&mut newparser("return undefined;"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn return_statement_test_contains_01() {
    let (item, _) = ReturnStatement::parse(&mut newparser("return;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn return_statement_test_contains_02() {
    let (item, _) = ReturnStatement::parse(&mut newparser("return 0;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn return_statement_test_contains_03() {
    let (item, _) = ReturnStatement::parse(&mut newparser("return a;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("return;" => true; "no expression")]
#[test_case("return a.#valid;" => true; "expression valid")]
#[test_case("return a.#invalid;" => false; "expression invalid")]
fn return_statement_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ReturnStatement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod return_statement {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ReturnStatement::parse(&mut newparser("return;"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }
}
