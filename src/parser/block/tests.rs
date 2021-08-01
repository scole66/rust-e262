use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

// BLOCK STATEMENT
#[test]
fn block_statement_test_01() {
    let (node, scanner) = check(BlockStatement::parse(&mut newparser("{q;}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 4);
    pretty_check(&*node, "BlockStatement: { q ; }", vec!["Block: { q ; }"]);
    concise_check(&*node, "Block: { q ; }", vec!["Punctuator: {", "ExpressionStatement: q ;", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn block_statement_test_err_01() {
    check_err(BlockStatement::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘{’ expected", 1, 1);
}
#[test]
fn block_statement_test_prettyerrors_1() {
    let (item, _) = BlockStatement::parse(&mut newparser("{ statement_list; }"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn block_statement_test_conciseerrors_1() {
    let (item, _) = BlockStatement::parse(&mut newparser("{ statement_list; }"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn block_statement_test_contains_01() {
    let (item, _) = BlockStatement::parse(&mut newparser("{ 10; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn block_statement_test_contains_02() {
    let (item, _) = BlockStatement::parse(&mut newparser("{ ; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn block_statement_test_var_declared_names_01() {
    let (item, _) = BlockStatement::parse(&mut newparser("{ var a, b; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), vec!["a", "b"]);
}
#[test]
fn block_statement_test_contains_undefined_break_target_01() {
    let (item, _) = BlockStatement::parse(&mut newparser("{ }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), false);
}
#[test]
fn block_statement_test_contains_undefined_break_target_02() {
    let (item, _) = BlockStatement::parse(&mut newparser("{ break bob; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn block_statement_test_contains_undefined_break_target_03() {
    let (item, _) = BlockStatement::parse(&mut newparser("{ break bob; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("bob")]), false);
}
#[test]
fn block_statement_test_contains_duplicate_labels() {
    let (item, _) = BlockStatement::parse(&mut newparser("{t:;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
}

// BLOCK
#[test]
fn block_test_01() {
    let (node, scanner) = check(Block::parse(&mut newparser("{q;}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 4);
    pretty_check(&*node, "Block: { q ; }", vec!["StatementList: q ;"]);
    concise_check(&*node, "Block: { q ; }", vec!["Punctuator: {", "ExpressionStatement: q ;", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn block_test_02() {
    let (node, scanner) = check(Block::parse(&mut newparser("{}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 2);
    pretty_check(&*node, "Block: { }", vec![]);
    concise_check(&*node, "Block: { }", vec!["Punctuator: {", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn block_test_cache_01() {
    let mut parser = newparser("{ a=1; b=2; c=3; }");
    let (node, scanner) = check(Block::parse(&mut parser, Scanner::new(), false, false, true));
    let (node2, scanner2) = check(Block::parse(&mut parser, Scanner::new(), false, false, true));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn block_test_err_01() {
    check_err(Block::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘{’ expected", 1, 1);
}
#[test]
fn block_test_err_02() {
    check_err(Block::parse(&mut newparser("{"), Scanner::new(), false, false, true), "‘}’ expected", 1, 2);
}
#[test]
fn block_test_prettyerrors_1() {
    let (item, _) = Block::parse(&mut newparser("{ statement_list; }"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn block_test_prettyerrors_2() {
    let (item, _) = Block::parse(&mut newparser("{}"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn block_test_conciseerrors_1() {
    let (item, _) = Block::parse(&mut newparser("{ statement_list; }"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn block_test_conciseerrors_2() {
    let (item, _) = Block::parse(&mut newparser("{}"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn block_test_contains_01() {
    let (item, _) = Block::parse(&mut newparser("{}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn block_test_contains_02() {
    let (item, _) = Block::parse(&mut newparser("{ return 10; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn block_test_contains_03() {
    let (item, _) = Block::parse(&mut newparser("{ return; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn block_test_var_declared_names_01() {
    let (item, _) = Block::parse(&mut newparser("{ var a, b; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), vec!["a", "b"]);
}
#[test]
fn block_test_var_declared_names_02() {
    let (item, _) = Block::parse(&mut newparser("{}"), Scanner::new(), true, true, true).unwrap();
    let expected: Vec<JSString> = Vec::new();
    assert_eq!(item.var_declared_names(), expected);
}
#[test]
fn block_test_contains_undefined_break_target_01() {
    let (item, _) = Block::parse(&mut newparser("{ }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), false);
}
#[test]
fn block_test_contains_undefined_break_target_02() {
    let (item, _) = Block::parse(&mut newparser("{ break bob; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn block_test_contains_undefined_break_target_03() {
    let (item, _) = Block::parse(&mut newparser("{ break bob; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("bob")]), false);
}
#[test]
fn block_test_contains_duplicate_labels_01() {
    let (item, _) = Block::parse(&mut newparser("{}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
}
#[test]
fn block_test_contains_duplicate_labels_02() {
    let (item, _) = Block::parse(&mut newparser("{t:;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
}

// STATEMENT LIST
#[test]
fn statement_list_test_01() {
    let (node, scanner) = check(StatementList::parse(&mut newparser("a;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 2);
    pretty_check(&*node, "StatementList: a ;", vec!["StatementListItem: a ;"]);
    concise_check(&*node, "ExpressionStatement: a ;", vec!["IdentifierName: a", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn statement_list_test_02() {
    let (node, scanner) = check(StatementList::parse(&mut newparser("a; b;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 5);
    pretty_check(&*node, "StatementList: a ; b ;", vec!["StatementList: a ;", "StatementListItem: b ;"]);
    concise_check(&*node, "StatementList: a ; b ;", vec!["ExpressionStatement: a ;", "ExpressionStatement: b ;"]);
    format!("{:?}", node);
}
#[test]
fn statement_list_test_cache_01() {
    let mut parser = newparser("a=1; b=2; c=3;");
    let (node, scanner) = check(StatementList::parse(&mut parser, Scanner::new(), false, false, true));
    let (node2, scanner2) = check(StatementList::parse(&mut parser, Scanner::new(), false, false, true));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn statement_list_test_err_01() {
    check_err(StatementList::parse(&mut newparser(""), Scanner::new(), false, false, true), "Declaration or Statement expected", 1, 1);
}
#[test]
fn statement_list_test_prettyerrors_1() {
    let (item, _) = StatementList::parse(&mut newparser("statement_list;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_list_test_prettyerrors_2() {
    let (item, _) = StatementList::parse(&mut newparser("statement; statement; statement;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_list_test_conciseerrors_1() {
    let (item, _) = StatementList::parse(&mut newparser("statement_list;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_list_test_conciseerrors_2() {
    let (item, _) = StatementList::parse(&mut newparser("statement; statement; statement;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_list_test_contains_01() {
    let (item, _) = StatementList::parse(&mut newparser("10;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
    assert_eq!(item.contains(ParseNodeKind::StatementListItem), true);
    assert_eq!(item.contains(ParseNodeKind::StatementList), false);
}
#[test]
fn statement_list_test_contains_02() {
    let (item, _) = StatementList::parse(&mut newparser("a;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
    assert_eq!(item.contains(ParseNodeKind::StatementListItem), true);
    assert_eq!(item.contains(ParseNodeKind::StatementList), false);
}
#[test]
fn statement_list_test_contains_03() {
    let (item, _) = StatementList::parse(&mut newparser("10;a;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
    assert_eq!(item.contains(ParseNodeKind::StatementListItem), true);
    assert_eq!(item.contains(ParseNodeKind::StatementList), true);
}
#[test]
fn statement_list_test_contains_04() {
    let (item, _) = StatementList::parse(&mut newparser("a;10;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
    assert_eq!(item.contains(ParseNodeKind::StatementListItem), true);
    assert_eq!(item.contains(ParseNodeKind::StatementList), true);
}
#[test]
fn statement_list_test_contains_05() {
    let (item, _) = StatementList::parse(&mut newparser("a;b;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
    assert_eq!(item.contains(ParseNodeKind::StatementListItem), true);
    assert_eq!(item.contains(ParseNodeKind::StatementList), true);
}
#[test]
fn statement_list_test_top_level_lexically_declared_names_01() {
    let (item, _) = StatementList::parse(&mut newparser("let a=1, b=2;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.top_level_lexically_declared_names(), &["a", "b"]);
}
#[test]
fn statement_list_test_top_level_lexically_declared_names_02() {
    let (item, _) = StatementList::parse(&mut newparser("let a=1, b=2; const alpha='q', beta='b';"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.top_level_lexically_declared_names(), &["a", "b", "alpha", "beta"]);
}
#[test]
fn statement_list_test_top_level_var_declared_names_01() {
    let (item, _) = StatementList::parse(&mut newparser("var a=1, b=2;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.top_level_var_declared_names(), &["a", "b"]);
}
#[test]
fn statement_list_test_top_level_var_declared_names_02() {
    let (item, _) = StatementList::parse(&mut newparser("var a=1, b=2; var alpha='q', beta='b';"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.top_level_var_declared_names(), &["a", "b", "alpha", "beta"]);
}
#[test]
fn statement_list_test_var_declared_names_01() {
    let (item, _) = StatementList::parse(&mut newparser("var a=1, b=2;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a", "b"]);
}
#[test]
fn statement_list_test_var_declared_names_02() {
    let (item, _) = StatementList::parse(&mut newparser("var a=1, b=2; var alpha='q', beta='b';"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a", "b", "alpha", "beta"]);
}
#[test]
fn statement_list_test_contains_undefined_break_target_01() {
    let (item, _) = StatementList::parse(&mut newparser("break bob;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("bob")]), false);
}
#[test]
fn statement_list_test_contains_undefined_break_target_02() {
    let (item, _) = StatementList::parse(&mut newparser("break bob;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn statement_list_test_contains_undefined_break_target_03() {
    let (item, _) = StatementList::parse(&mut newparser("break a; break b;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("b")]), true);
}
#[test]
fn statement_list_test_contains_undefined_break_target_04() {
    let (item, _) = StatementList::parse(&mut newparser("break a; break b;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("a")]), true);
}
#[test]
fn statement_list_test_contains_undefined_break_target_05() {
    let (item, _) = StatementList::parse(&mut newparser("break a; break b;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("a"), JSString::from("b")]), false);
}
fn statement_list_cdl_check(src: &str) {
    let (item, _) = StatementList::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
}
#[test]
fn statement_list_test_contains_duplicate_labels() {
    statement_list_cdl_check("t:;");
    statement_list_cdl_check("t:;;");
    statement_list_cdl_check(";t:;");
}

// STATEMENT LIST ITEM
#[test]
fn statement_list_item_test_01() {
    let (node, scanner) = check(StatementListItem::parse(&mut newparser("a;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 2);
    pretty_check(&*node, "StatementListItem: a ;", vec!["Statement: a ;"]);
    concise_check(&*node, "ExpressionStatement: a ;", vec!["IdentifierName: a", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn statement_list_item_test_02() {
    let (node, scanner) = check(StatementListItem::parse(&mut newparser("let a;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 6);
    pretty_check(&*node, "StatementListItem: let a ;", vec!["Declaration: let a ;"]);
    concise_check(&*node, "LexicalDeclaration: let a ;", vec!["Keyword: let", "IdentifierName: a", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn statement_list_item_test_err_01() {
    check_err(StatementListItem::parse(&mut newparser(""), Scanner::new(), false, false, true), "Declaration or Statement expected", 1, 1);
}
#[test]
fn statement_list_item_test_prettyerrors_1() {
    let (item, _) = StatementListItem::parse(&mut newparser("statement_list;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_list_item_test_prettyerrors_2() {
    let (item, _) = StatementListItem::parse(&mut newparser("const declaration = 0;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_list_item_test_conciseerrors_1() {
    let (item, _) = StatementListItem::parse(&mut newparser("statement_list;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_list_item_test_conciseerrors_2() {
    let (item, _) = StatementListItem::parse(&mut newparser("const declaration = 0;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_list_item_test_top_level_lexically_declared_names_01() {
    let (item, _) = StatementListItem::parse(&mut newparser("a;"), Scanner::new(), true, true, true).unwrap();
    let expected: &[JSString] = &[];
    assert_eq!(item.top_level_lexically_declared_names(), expected);
}
#[test]
fn statement_list_item_test_top_level_lexically_declared_names_02() {
    let (item, _) = StatementListItem::parse(&mut newparser("function a(){}"), Scanner::new(), true, true, true).unwrap();
    let expected: &[JSString] = &[];
    assert_eq!(item.top_level_lexically_declared_names(), expected);
}
#[test]
fn statement_list_item_test_top_level_lexically_declared_names_03() {
    let (item, _) = StatementListItem::parse(&mut newparser("const a=12;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.top_level_lexically_declared_names(), &[JSString::from("a")]);
}
#[test]
fn statement_list_item_test_top_level_var_declared_names_01() {
    let (item, _) = StatementListItem::parse(&mut newparser("a: function b(){}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.top_level_var_declared_names(), &[JSString::from("b")]);
}
#[test]
fn statement_list_item_test_top_level_var_declared_names_02() {
    let (item, _) = StatementListItem::parse(&mut newparser("{ function b(){} }"), Scanner::new(), true, true, true).unwrap();
    let expected: &[JSString] = &[];
    assert_eq!(item.top_level_var_declared_names(), expected);
}
#[test]
fn statement_list_item_test_top_level_var_declared_names_03() {
    let (item, _) = StatementListItem::parse(&mut newparser("function b(){}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.top_level_var_declared_names(), &[JSString::from("b")]);
}
#[test]
fn statement_list_item_test_top_level_var_declared_names_04() {
    let (item, _) = StatementListItem::parse(&mut newparser("let a=3;"), Scanner::new(), true, true, true).unwrap();
    let expected: &[JSString] = &[];
    assert_eq!(item.top_level_var_declared_names(), expected);
}
#[test]
fn statement_list_item_test_var_declared_names_01() {
    let (item, _) = StatementListItem::parse(&mut newparser("function b(){}"), Scanner::new(), true, true, true).unwrap();
    let expected: &[JSString] = &[];
    assert_eq!(item.var_declared_names(), expected);
}
#[test]
fn statement_list_item_test_var_declared_names_02() {
    let (item, _) = StatementListItem::parse(&mut newparser("var x;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &[JSString::from("x")]);
}
#[test]
fn statement_list_item_test_contains_undefined_break_target_01() {
    let (item, _) = StatementListItem::parse(&mut newparser("break a;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn statement_list_item_test_contains_undefined_break_target_02() {
    let (item, _) = StatementListItem::parse(&mut newparser("break a;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("a")]), false);
}
#[test]
fn statement_list_item_test_contains_undefined_break_target_03() {
    let (item, _) = StatementListItem::parse(&mut newparser("let a=3;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("a")]), false);
}
#[test]
fn statement_list_item_test_contains_01() {
    let (item, _) = StatementListItem::parse(&mut newparser("let a=3;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
    assert_eq!(item.contains(ParseNodeKind::Statement), false);
    assert_eq!(item.contains(ParseNodeKind::Declaration), true);
}
#[test]
fn statement_list_item_test_contains_02() {
    let (item, _) = StatementListItem::parse(&mut newparser("12;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
    assert_eq!(item.contains(ParseNodeKind::Statement), true);
    assert_eq!(item.contains(ParseNodeKind::Declaration), false);
}
#[test]
fn statement_list_item_test_contains_03() {
    let (item, _) = StatementListItem::parse(&mut newparser("let a;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
    assert_eq!(item.contains(ParseNodeKind::Statement), false);
    assert_eq!(item.contains(ParseNodeKind::Declaration), true);
}
#[test]
fn statement_list_item_test_contains_04() {
    let (item, _) = StatementListItem::parse(&mut newparser("a;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
    assert_eq!(item.contains(ParseNodeKind::Statement), true);
    assert_eq!(item.contains(ParseNodeKind::Declaration), false);
}
#[test]
fn statement_list_item_test_contains_duplicate_labels_01() {
    let (item, _) = StatementListItem::parse(&mut newparser("t:;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
}
#[test]
fn statement_list_item_test_contains_duplicate_labels_02() {
    let (item, _) = StatementListItem::parse(&mut newparser("let t;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), false);
}
