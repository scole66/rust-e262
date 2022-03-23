use super::testhelp::{check, check_err, chk_scan, newparser, set, strictparser, PACKAGE_NOT_ALLOWED};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::{test_agent, unwind_syntax_error_object};
use ahash::AHashSet;
use test_case::test_case;

const CONTINUE_ITER: &str = "Continue statements must lie within iteration statements.";

// CONTINUE STATEMENT
#[test]
fn continue_statement_test_01() {
    let (node, scanner) = check(ContinueStatement::parse(&mut newparser("continue;"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "ContinueStatement: continue ;", vec![]);
    concise_check(&*node, "ContinueStatement: continue ;", vec!["Keyword: continue", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn continue_statement_test_02() {
    let (node, scanner) = check(ContinueStatement::parse(&mut newparser("continue a;"), Scanner::new(), false, false));
    chk_scan(&scanner, 11);
    pretty_check(&*node, "ContinueStatement: continue a ;", vec!["LabelIdentifier: a"]);
    concise_check(&*node, "ContinueStatement: continue a ;", vec!["Keyword: continue", "IdentifierName: a", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn continue_statement_test_03() {
    let (node, scanner) = check(ContinueStatement::parse(&mut newparser("continue"), Scanner::new(), false, false));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "ContinueStatement: continue ;", vec![]);
    concise_check(&*node, "ContinueStatement: continue ;", vec!["Keyword: continue", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn continue_statement_test_04() {
    let (node, scanner) = check(ContinueStatement::parse(&mut newparser("continue label"), Scanner::new(), false, false));
    chk_scan(&scanner, 14);
    pretty_check(&*node, "ContinueStatement: continue label ;", vec!["LabelIdentifier: label"]);
    concise_check(&*node, "ContinueStatement: continue label ;", vec!["Keyword: continue", "IdentifierName: label", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn continue_statement_test_05() {
    let (node, scanner) = check(ContinueStatement::parse(&mut newparser("continue\nlabel"), Scanner::new(), false, false));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "ContinueStatement: continue ;", vec![]);
    concise_check(&*node, "ContinueStatement: continue ;", vec!["Keyword: continue", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn continue_statement_test_err_01() {
    check_err(ContinueStatement::parse(&mut newparser(""), Scanner::new(), false, false), "‘continue’ expected", 1, 1);
}
#[test]
fn continue_statement_test_err_02() {
    check_err(ContinueStatement::parse(&mut newparser("continue for"), Scanner::new(), false, false), "‘;’ expected", 1, 9);
}
#[test]
fn continue_statement_test_err_03() {
    check_err(ContinueStatement::parse(&mut newparser("continue a for"), Scanner::new(), false, false), "‘;’ expected", 1, 11);
}
#[test]
fn continue_statement_test_prettyerrors_1() {
    let (item, _) = ContinueStatement::parse(&mut newparser("continue;"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn continue_statement_test_prettyerrors_2() {
    let (item, _) = ContinueStatement::parse(&mut newparser("continue label;"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn continue_statement_test_conciseerrors_1() {
    let (item, _) = ContinueStatement::parse(&mut newparser("continue;"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn continue_statement_test_conciseerrors_2() {
    let (item, _) = ContinueStatement::parse(&mut newparser("continue label;"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn continue_statement_test_contains_01() {
    let (item, _) = ContinueStatement::parse(&mut newparser("continue label;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn continue_statement_test_contains_02() {
    let (item, _) = ContinueStatement::parse(&mut newparser("continue;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("continue x;" => (false, true); "continue x;")]
#[test_case("continue;" => (false, false); "continue;")]
fn continue_statement_test_contains_undefined_continue_target(src: &str) -> (bool, bool) {
    let (item, _) = ContinueStatement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    (item.contains_undefined_continue_target(&[JSString::from("x")]), item.contains_undefined_continue_target(&[JSString::from("y")]))
}

#[test_case("continue;", true, false => set(&[CONTINUE_ITER]); "continue, beyond iteration")]
#[test_case("continue;", true, true => set(&[]); "continue, within iteration")]
#[test_case("continue package;", true, true => set(&[PACKAGE_NOT_ALLOWED]); "continue LabelIdentifier ; (within)")]
#[test_case("continue package;", true, false => set(&[PACKAGE_NOT_ALLOWED, CONTINUE_ITER]); "continue LabelIdentifier ; (beyond)")]
fn early_errors(src: &str, strict: bool, within_iteration: bool) -> AHashSet<String> {
    let mut agent = test_agent();
    let mut errs = vec![];
    ContinueStatement::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict, within_iteration);
    AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
}
