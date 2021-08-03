use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use test_case::test_case;

// LABELLED STATEMENT
#[test]
fn labelled_statement_test_01() {
    let (node, scanner) = check(LabelledStatement::parse(&mut newparser("blue: orange;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 13);
    pretty_check(&*node, "LabelledStatement: blue : orange ;", vec!["LabelIdentifier: blue", "LabelledItem: orange ;"]);
    concise_check(&*node, "LabelledStatement: blue : orange ;", vec!["IdentifierName: blue", "Punctuator: :", "ExpressionStatement: orange ;"]);
    format!("{:?}", node);
}
#[test]
fn labelled_statement_test_err_01() {
    check_err(LabelledStatement::parse(&mut newparser(""), Scanner::new(), false, false, true), "Not an identifier", 1, 1);
}
#[test]
fn labelled_statement_test_err_02() {
    check_err(LabelledStatement::parse(&mut newparser("a"), Scanner::new(), false, false, true), "‘:’ expected", 1, 2);
}
#[test]
fn labelled_statement_test_err_03() {
    check_err(LabelledStatement::parse(&mut newparser("a:"), Scanner::new(), false, false, true), "LabelledItem expected", 1, 3);
}
#[test]
fn labelled_statement_test_prettyerrors_1() {
    let (item, _) = LabelledStatement::parse(&mut newparser("i:b;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn labelled_statement_test_conciseerrors_1() {
    let (item, _) = LabelledStatement::parse(&mut newparser("i:b;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn labelled_statement_test_top_level_var_declared_names_01() {
    let (item, _) = LabelledStatement::parse(&mut newparser("i:var a;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.top_level_var_declared_names(), &["a"]);
}
#[test]
fn labelled_statement_test_top_level_var_declared_names_02() {
    let (item, _) = LabelledStatement::parse(&mut newparser("i:function a(){}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.top_level_var_declared_names(), &["a"]);
}
#[test]
fn labelled_statement_test_var_declared_names_01() {
    let (item, _) = LabelledStatement::parse(&mut newparser("i:var a;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a"]);
}
#[test]
fn labelled_statement_test_var_declared_names_02() {
    let (item, _) = LabelledStatement::parse(&mut newparser("i:function a(){}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &[] as &[JSString]);
}
#[test]
fn labelled_statement_test_contains_undefined_break_target_01() {
    let (item, _) = LabelledStatement::parse(&mut newparser("i:break t;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("i")]), true);
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn labelled_statement_test_contains_undefined_break_target_02() {
    let (item, _) = LabelledStatement::parse(&mut newparser("i:break i;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("i")]), false);
    assert_eq!(item.contains_undefined_break_target(&[]), false);
}
#[test]
fn labelled_statement_test_contains_01() {
    let (item, _) = LabelledStatement::parse(&mut newparser("i:0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn labelled_statement_test_contains_02() {
    let (item, _) = LabelledStatement::parse(&mut newparser("i:a;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn labelled_statement_test_contains_duplicate_labels_01() {
    let (item, _) = LabelledStatement::parse(&mut newparser("t:;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("u")]), false);
}
#[test]
fn labelled_statement_test_contains_duplicate_labels_02() {
    let (item, _) = LabelledStatement::parse(&mut newparser("t:t:;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), true);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("u")]), true);
}
#[test_case("a:continue x;" => (false, true, true, true); "a: continue x;")]
#[test_case("a:for(;;)continue x;" => (false, true, false, true); "a: for (;;) continue x;")]
#[test_case("a:for(;;)continue y;" => (true, false, true, false); "a: for (;;) continue y;")]
#[test_case("a:for(;;)continue a;" => (false, false, false, false); "a: for (;;) continue a;")]
fn labelled_statement_test_contains_undefined_continue_target(src: &str) -> (bool, bool, bool, bool) {
    let (item, _) = LabelledStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (
        item.contains_undefined_continue_target(&[JSString::from("x")], &[]),
        item.contains_undefined_continue_target(&[JSString::from("y")], &[]),
        item.contains_undefined_continue_target(&[], &[JSString::from("x")]),
        item.contains_undefined_continue_target(&[], &[JSString::from("y")]),
    )
}

// LABELLED ITEM
#[test]
fn labelled_item_test_01() {
    let (node, scanner) = check(LabelledItem::parse(&mut newparser("orange;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 7);
    pretty_check(&*node, "LabelledItem: orange ;", vec!["Statement: orange ;"]);
    concise_check(&*node, "ExpressionStatement: orange ;", vec!["IdentifierName: orange", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn labelled_item_test_02() {
    let (node, scanner) = check(LabelledItem::parse(&mut newparser("function a(){}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 14);
    pretty_check(&*node, "LabelledItem: function a (  ) {  }", vec!["FunctionDeclaration: function a (  ) {  }"]);
    concise_check(&*node, "FunctionDeclaration: function a (  ) {  }", vec!["Keyword: function", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn labelled_item_test_err_01() {
    check_err(LabelledItem::parse(&mut newparser(""), Scanner::new(), false, false, true), "LabelledItem expected", 1, 1);
}
#[test]
fn labelled_item_test_prettyerrors_1() {
    let (item, _) = LabelledItem::parse(&mut newparser("a;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn labelled_item_test_prettyerrors_2() {
    let (item, _) = LabelledItem::parse(&mut newparser("function a(){}"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn labelled_item_test_conciseerrors_1() {
    let (item, _) = LabelledItem::parse(&mut newparser("a;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn labelled_item_test_conciseerrors_2() {
    let (item, _) = LabelledItem::parse(&mut newparser("function a(){}"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn labelled_item_test_top_level_var_declared_names_01() {
    let (item, _) = LabelledItem::parse(&mut newparser("var a;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.top_level_var_declared_names(), &["a"]);
}
#[test]
fn labelled_item_test_top_level_var_declared_names_02() {
    let (item, _) = LabelledItem::parse(&mut newparser("i:var a;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.top_level_var_declared_names(), &["a"]);
}
#[test]
fn labelled_item_test_top_level_var_declared_names_03() {
    let (item, _) = LabelledItem::parse(&mut newparser("i:function a(){}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.top_level_var_declared_names(), &["a"]);
}
#[test]
fn labelled_item_test_top_level_var_declared_names_04() {
    let (item, _) = LabelledItem::parse(&mut newparser("function a(){}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.top_level_var_declared_names(), &["a"]);
}
#[test]
fn labelled_item_test_var_declared_names_01() {
    let (item, _) = LabelledItem::parse(&mut newparser("var a;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a"]);
}
#[test]
fn labelled_item_test_var_declared_names_02() {
    let (item, _) = LabelledItem::parse(&mut newparser("i:var a;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a"]);
}
#[test]
fn labelled_item_test_var_declared_names_03() {
    let (item, _) = LabelledItem::parse(&mut newparser("i:function a(){}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &[] as &[JSString]);
}
#[test]
fn labelled_item_test_var_declared_names_04() {
    let (item, _) = LabelledItem::parse(&mut newparser("function a(){}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &[] as &[JSString]);
}
#[test]
fn labelled_item_test_contains_undefined_break_target_01() {
    let (item, _) = LabelledItem::parse(&mut newparser("break t;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn labelled_item_test_contains_undefined_break_target_02() {
    let (item, _) = LabelledItem::parse(&mut newparser("function a(){}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
    assert_eq!(item.contains_undefined_break_target(&[]), false);
}
#[test]
fn labelled_item_test_contains_01() {
    let (item, _) = LabelledItem::parse(&mut newparser("0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn labelled_item_test_contains_02() {
    let (item, _) = LabelledItem::parse(&mut newparser("a;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn labelled_item_test_contains_03() {
    let (item, _) = LabelledItem::parse(&mut newparser("function a(){}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn labelled_item_test_contains_duplicate_labels_01() {
    let (item, _) = LabelledItem::parse(&mut newparser("function a(){}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
}
#[test]
fn labelled_item_test_contains_duplicate_labels_02() {
    let (item, _) = LabelledItem::parse(&mut newparser("t:;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
}
#[test_case("continue x;" => (false, true, true, true); "continue x;")]
#[test_case("for(;;)continue x;" => (false, true, false, true); "for (;;) continue x;")]
#[test_case("function x(){}" => (false, false, false, false); "function x() {}")]
fn labelled_item_test_contains_undefined_continue_target(src: &str) -> (bool, bool, bool, bool) {
    let (item, _) = LabelledItem::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (
        item.contains_undefined_continue_target(&[JSString::from("x")], &[]),
        item.contains_undefined_continue_target(&[JSString::from("y")], &[]),
        item.contains_undefined_continue_target(&[], &[JSString::from("x")]),
        item.contains_undefined_continue_target(&[], &[JSString::from("y")]),
    )
}
