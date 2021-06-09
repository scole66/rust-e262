use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

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
