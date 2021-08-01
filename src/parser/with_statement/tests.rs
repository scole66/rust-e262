use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

// WITH STATEMENT
#[test]
fn with_statement_test_01() {
    let (node, scanner) = check(WithStatement::parse(&mut newparser("with ( x in obj ) { x.used = true; }"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 36);
    format!("{:?}", node);
    pretty_check(&*node, "WithStatement: with ( x in obj ) { x . used = true ; }", vec!["Expression: x in obj", "Statement: { x . used = true ; }"]);
    concise_check(
        &*node,
        "WithStatement: with ( x in obj ) { x . used = true ; }",
        vec!["Keyword: with", "Punctuator: (", "RelationalExpression: x in obj", "Punctuator: )", "Block: { x . used = true ; }"],
    );
}
#[test]
fn with_statement_test_err_01() {
    check_err(WithStatement::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘with’ expected", 1, 1);
}
#[test]
fn with_statement_test_err_02() {
    check_err(WithStatement::parse(&mut newparser("with"), Scanner::new(), false, false, true), "‘(’ expected", 1, 5);
}
#[test]
fn with_statement_test_err_03() {
    check_err(WithStatement::parse(&mut newparser("with("), Scanner::new(), false, false, true), "Expression expected", 1, 6);
}
#[test]
fn with_statement_test_err_04() {
    check_err(WithStatement::parse(&mut newparser("with(a"), Scanner::new(), false, false, true), "‘)’ expected", 1, 7);
}
#[test]
fn with_statement_test_err_05() {
    check_err(WithStatement::parse(&mut newparser("with(a)"), Scanner::new(), false, false, true), "Statement expected", 1, 8);
}
#[test]
fn with_statement_prettycheck_1() {
    let (item, _) = WithStatement::parse(&mut newparser("with (returns_an_object()) { obj = 12; }"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn with_statement_concisecheck_1() {
    let (item, _) = WithStatement::parse(&mut newparser("with (returns_an_object()) { obj = 12; }"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn with_statement_test_var_declared_names() {
    let (item, _) = WithStatement::parse(&mut newparser("with(0)var a;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a"]);
}
#[test]
fn with_statement_test_contains_undefined_break_target() {
    let (item, _) = WithStatement::parse(&mut newparser("with(0)break t;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
fn with_contains_check(src: &str, has_literal: bool) {
    let (item, _) = WithStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), has_literal);
}
#[test]
fn with_statement_test_contains() {
    with_contains_check("with(0);", true);
    with_contains_check("with(a)0;", true);
    with_contains_check("with(a);", false);
}

#[test]
fn with_statement_test_contains_duplicate_labels() {
    let (item, _) = WithStatement::parse(&mut newparser("with(0) lbl: { 0; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("lbl")]), true);
}
