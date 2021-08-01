use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

// IF STATEMENT
#[test]
fn if_statement_test_01() {
    let (node, scanner) = check(IfStatement::parse(&mut newparser("if (true) { a; }"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 16);
    pretty_check(&*node, "IfStatement: if ( true ) { a ; }", vec!["Expression: true", "Statement: { a ; }"]);
    concise_check(&*node, "IfStatement: if ( true ) { a ; }", vec!["Keyword: if", "Punctuator: (", "Keyword: true", "Punctuator: )", "Block: { a ; }"]);
    format!("{:?}", node);
}
#[test]
fn if_statement_test_02() {
    let (node, scanner) = check(IfStatement::parse(&mut newparser("if (0) { a; } else { b; }"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 25);
    pretty_check(&*node, "IfStatement: if ( 0 ) { a ; } else { b ; }", vec!["Expression: 0", "Statement: { a ; }", "Statement: { b ; }"]);
    concise_check(
        &*node,
        "IfStatement: if ( 0 ) { a ; } else { b ; }",
        vec!["Keyword: if", "Punctuator: (", "Numeric: 0", "Punctuator: )", "Block: { a ; }", "Keyword: else", "Block: { b ; }"],
    );
    format!("{:?}", node);
}
#[test]
fn if_statement_test_err_01() {
    check_err(IfStatement::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘if’ expected", 1, 1);
}
#[test]
fn if_statement_test_err_02() {
    check_err(IfStatement::parse(&mut newparser("if"), Scanner::new(), false, false, true), "‘(’ expected", 1, 3);
}
#[test]
fn if_statement_test_err_03() {
    check_err(IfStatement::parse(&mut newparser("if ("), Scanner::new(), false, false, true), "Expression expected", 1, 5);
}
#[test]
fn if_statement_test_err_04() {
    check_err(IfStatement::parse(&mut newparser("if (0"), Scanner::new(), false, false, true), "‘)’ expected", 1, 6);
}
#[test]
fn if_statement_test_err_05() {
    check_err(IfStatement::parse(&mut newparser("if (0)"), Scanner::new(), false, false, true), "Statement expected", 1, 7);
}
#[test]
fn if_statement_test_err_06() {
    check_err(IfStatement::parse(&mut newparser("if (0) a; else"), Scanner::new(), false, false, true), "Statement expected", 1, 15);
}
#[test]
fn if_statement_test_prettyerrors_1() {
    let (item, _) = IfStatement::parse(&mut newparser("if (false) b;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn if_statement_test_prettyerrors_2() {
    let (item, _) = IfStatement::parse(&mut newparser("if (false) b; else f;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn if_statement_test_conciseerrors_1() {
    let (item, _) = IfStatement::parse(&mut newparser("if (false) b;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn if_statement_test_conciseerrors_2() {
    let (item, _) = IfStatement::parse(&mut newparser("if (false) b; else f;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn if_statement_test_var_declared_names_01() {
    let (item, _) = IfStatement::parse(&mut newparser("if (false) { var x; } else { var y; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["x", "y"]);
}
#[test]
fn if_statement_test_var_declared_names_02() {
    let (item, _) = IfStatement::parse(&mut newparser("if (false) { var x, y; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["x", "y"]);
}
#[test]
fn if_statement_test_contains_undefined_break_target_01() {
    let (item, _) = IfStatement::parse(&mut newparser("if (false) { break k; } else { ; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn if_statement_test_contains_undefined_break_target_02() {
    let (item, _) = IfStatement::parse(&mut newparser("if (false) { break k; } else { ; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("k")]), false);
}
#[test]
fn if_statement_test_contains_undefined_break_target_03() {
    let (item, _) = IfStatement::parse(&mut newparser("if (false) { ; } else { break k; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn if_statement_test_contains_undefined_break_target_04() {
    let (item, _) = IfStatement::parse(&mut newparser("if (false) { ; } else { break k; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("k")]), false);
}
#[test]
fn if_statement_test_contains_undefined_break_target_05() {
    let (item, _) = IfStatement::parse(&mut newparser("if (false) { break k; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn if_statement_test_contains_undefined_break_target_06() {
    let (item, _) = IfStatement::parse(&mut newparser("if (false) { break k; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("k")]), false);
}
#[test]
fn if_statement_test_contains_01() {
    let (item, _) = IfStatement::parse(&mut newparser("if (0) {} else {}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn if_statement_test_contains_02() {
    let (item, _) = IfStatement::parse(&mut newparser("if (a) {0;} else {}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn if_statement_test_contains_03() {
    let (item, _) = IfStatement::parse(&mut newparser("if (a) {} else {0;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn if_statement_test_contains_04() {
    let (item, _) = IfStatement::parse(&mut newparser("if (a) {} else {}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn if_statement_test_contains_05() {
    let (item, _) = IfStatement::parse(&mut newparser("if (0) {}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn if_statement_test_contains_06() {
    let (item, _) = IfStatement::parse(&mut newparser("if (a) {0;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn if_statement_test_contains_07() {
    let (item, _) = IfStatement::parse(&mut newparser("if (a) {}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
fn if_cdl_check(src: &str) {
    let (item, _) = IfStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
}
#[test]
fn if_statement_test_contains_duplicate_labels() {
    if_cdl_check("if(0){t:;}");
    if_cdl_check("if(0){t:;}else{}");
    if_cdl_check("if(0){}else{t:;}");
}
