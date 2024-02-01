use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

// BLOCK STATEMENT
#[test]
fn block_statement_test_01() {
    let (node, scanner) = check(BlockStatement::parse(&mut newparser("{q;}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 4);
    pretty_check(&*node, "BlockStatement: { q ; }", vec!["Block: { q ; }"]);
    concise_check(&*node, "Block: { q ; }", vec!["Punctuator: {", "ExpressionStatement: q ;", "Punctuator: }"]);
    format!("{node:?}");
}
#[test]
fn block_statement_test_err_01() {
    check_err(BlockStatement::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘{’ expected", 1, 1);
}
#[test]
fn block_statement_test_prettyerrors_1() {
    let (item, _) =
        BlockStatement::parse(&mut newparser("{ statement_list; }"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn block_statement_test_conciseerrors_1() {
    let (item, _) =
        BlockStatement::parse(&mut newparser("{ statement_list; }"), Scanner::new(), false, false, true).unwrap();
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
#[test_case("{continue x;}" => (false, true, true, true); "{ continue x; }")]
#[test_case("{for(;;)continue x;}" => (false, true, false, true); "{ for (;;) continue x; }")]
fn block_statement_test_contains_undefined_continue_target(src: &str) -> (bool, bool, bool, bool) {
    let (item, _) = BlockStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (
        item.contains_undefined_continue_target(&[JSString::from("x")], &[]),
        item.contains_undefined_continue_target(&[JSString::from("y")], &[]),
        item.contains_undefined_continue_target(&[], &[JSString::from("x")]),
        item.contains_undefined_continue_target(&[], &[JSString::from("y")]),
    )
}
#[test_case("{item.#valid;}" => true; "StatementList valid")]
#[test_case("{item.#invalid;}" => false; "StatementList invalid")]
fn block_statement_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = BlockStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod block_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("{package;}", true => sset(&[PACKAGE_NOT_ALLOWED]); "Block")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        BlockStatement::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict, false, false);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("{arguments;}" => true; "yes")]
    #[test_case("{;}" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        BlockStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.contains_arguments()
    }

    #[test_case("{var rust;}" => svec(&["rust"]); "vsd")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).block_statement().var_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("    { b; let a; }" => Location { starting_line: 1, starting_column: 5, span: Span { starting_index: 4, length: 13 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).block_statement().location()
    }
}

// BLOCK
#[test]
fn block_test_01() {
    let (node, scanner) = check(Block::parse(&mut newparser("{q;}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 4);
    pretty_check(&*node, "Block: { q ; }", vec!["StatementList: q ;"]);
    concise_check(&*node, "Block: { q ; }", vec!["Punctuator: {", "ExpressionStatement: q ;", "Punctuator: }"]);
    format!("{node:?}");
}
#[test]
fn block_test_02() {
    let (node, scanner) = check(Block::parse(&mut newparser("{}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 2);
    pretty_check(&*node, "Block: { }", vec![]);
    concise_check(&*node, "Block: { }", vec!["Punctuator: {", "Punctuator: }"]);
    format!("{node:?}");
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
#[test_case("{continue x;}" => (false, true, true, true); "{ continue x; }")]
#[test_case("{for(;;)continue x;}" => (false, true, false, true); "{ for (;;) continue x; }")]
#[test_case("{}" => (false, false, false, false); "empty")]
fn block_test_contains_undefined_continue_target(src: &str) -> (bool, bool, bool, bool) {
    let (item, _) = Block::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (
        item.contains_undefined_continue_target(&[JSString::from("x")], &[]),
        item.contains_undefined_continue_target(&[JSString::from("y")], &[]),
        item.contains_undefined_continue_target(&[], &[JSString::from("x")]),
        item.contains_undefined_continue_target(&[], &[JSString::from("y")]),
    )
}
#[test_case("{}" => true; "No statements")]
#[test_case("{item.#valid;}" => true; "StatementList valid")]
#[test_case("{item.#invalid;}" => false; "StatementList invalid")]
fn block_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = Block::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod block {
    use super::*;
    use test_case::test_case;

    #[test_case("{let a, b, c;}" => vec!["a", "b", "c"]; "not-empty")]
    #[test_case("{}" => Vec::<String>::new(); "empty")]
    fn lexically_declared_names(src: &str) -> Vec<String> {
        Block::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .lexically_declared_names()
            .into_iter()
            .map(String::from)
            .collect::<Vec<String>>()
    }

    #[test_case("{package;}", true => sset(&[PACKAGE_NOT_ALLOWED]); "{ StatementList }")]
    #[test_case("{}", true => sset(&[]); "{ } (empty)")]
    #[test_case("{ let a = 10; const a = 20; }", true => sset(&[DUPLICATE_LEXICAL]); "Duplicate lexically declared names")]
    #[test_case("{ var x; print(x); let x = 27; }", true => sset(&[LEX_DUPED_BY_VAR]); "Name declared both lex & var")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Block::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict, false, false);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("{arguments;}" => true; "yes")]
    #[test_case("{;}" => false; "no")]
    #[test_case("{}" => false; "empty")]
    fn contains_arguments(src: &str) -> bool {
        Block::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.contains_arguments()
    }

    #[test_case("{var rust;}" => svec(&["rust"]); "vsd")]
    #[test_case("{}" => svec(&([])); "empty")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).block().var_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("    { b; let a; }" => Location { starting_line: 1, starting_column: 5, span: Span { starting_index: 4, length: 13 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).block().location()
    }
}

// STATEMENT LIST
#[test]
fn statement_list_test_01() {
    let (node, scanner) = check(StatementList::parse(&mut newparser("a;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 2);
    pretty_check(&*node, "StatementList: a ;", vec!["StatementListItem: a ;"]);
    concise_check(&*node, "ExpressionStatement: a ;", vec!["IdentifierName: a", "Punctuator: ;"]);
    format!("{node:?}");
}
#[test]
fn statement_list_test_02() {
    let (node, scanner) = check(StatementList::parse(&mut newparser("a; b;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 5);
    pretty_check(&*node, "StatementList: a ; b ;", vec!["StatementListItem: a ;", "StatementListItem: b ;"]);
    concise_check(&*node, "StatementList: a ; b ;", vec!["ExpressionStatement: a ;", "ExpressionStatement: b ;"]);
    format!("{node:?}");
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
    check_err(
        StatementList::parse(&mut newparser(""), Scanner::new(), false, false, true),
        "Declaration or Statement expected",
        1,
        1,
    );
}
#[test]
fn statement_list_test_prettyerrors_1() {
    let (item, _) =
        StatementList::parse(&mut newparser("statement_list;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_list_test_prettyerrors_2() {
    let (item, _) =
        StatementList::parse(&mut newparser("statement; statement; statement;"), Scanner::new(), false, false, true)
            .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_list_test_conciseerrors_1() {
    let (item, _) =
        StatementList::parse(&mut newparser("statement_list;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_list_test_conciseerrors_2() {
    let (item, _) =
        StatementList::parse(&mut newparser("statement; statement; statement;"), Scanner::new(), false, false, true)
            .unwrap();
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
    let (item, _) = StatementList::parse(
        &mut newparser("let a=1, b=2; const alpha='q', beta='b';"),
        Scanner::new(),
        true,
        true,
        true,
    )
    .unwrap();
    assert_eq!(item.top_level_lexically_declared_names(), &["a", "b", "alpha", "beta"]);
}
#[test]
fn statement_list_test_top_level_var_declared_names_01() {
    let (item, _) = StatementList::parse(&mut newparser("var a=1, b=2;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.top_level_var_declared_names(), &["a", "b"]);
}
#[test]
fn statement_list_test_top_level_var_declared_names_02() {
    let (item, _) = StatementList::parse(
        &mut newparser("var a=1, b=2; var alpha='q', beta='b';"),
        Scanner::new(),
        true,
        true,
        true,
    )
    .unwrap();
    assert_eq!(item.top_level_var_declared_names(), &["a", "b", "alpha", "beta"]);
}
#[test]
fn statement_list_test_var_declared_names_01() {
    let (item, _) = StatementList::parse(&mut newparser("var a=1, b=2;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a", "b"]);
}
#[test]
fn statement_list_test_var_declared_names_02() {
    let (item, _) = StatementList::parse(
        &mut newparser("var a=1, b=2; var alpha='q', beta='b';"),
        Scanner::new(),
        true,
        true,
        true,
    )
    .unwrap();
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
    let (item, _) =
        StatementList::parse(&mut newparser("break a; break b;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("b")]), true);
}
#[test]
fn statement_list_test_contains_undefined_break_target_04() {
    let (item, _) =
        StatementList::parse(&mut newparser("break a; break b;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("a")]), true);
}
#[test]
fn statement_list_test_contains_undefined_break_target_05() {
    let (item, _) =
        StatementList::parse(&mut newparser("break a; break b;"), Scanner::new(), true, true, true).unwrap();
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
#[test_case("continue x;" => (false, true, true, true); "continue x;")]
#[test_case("for (;;) continue x;" => (false, true, false, true); "for (;;) continue x;")]
#[test_case("0;continue x;" => (false, true, true, true); "0; continue x;")]
#[test_case("0;for (;;) continue x;" => (false, true, true, true); "0; for (;;) continue x;")]
#[test_case("continue x;0;" => (false, true, true, true); "continue x; 0;")]
#[test_case("for (;;) continue x;0;" => (false, true, true, true); "for (;;) continue x; 0;")]
fn statement_list_test_contains_undefined_continue_target(src: &str) -> (bool, bool, bool, bool) {
    let (item, _) = StatementList::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (
        item.contains_undefined_continue_target(&[JSString::from("x")], &[]),
        item.contains_undefined_continue_target(&[JSString::from("y")], &[]),
        item.contains_undefined_continue_target(&[], &[JSString::from("x")]),
        item.contains_undefined_continue_target(&[], &[JSString::from("y")]),
    )
}
#[test_case("'use strict';" => vec![JSString::from("use strict")]; "One String")]
#[test_case("'blue'; 'green'; true; 0; 'hello';" => vec![JSString::from("blue"), JSString::from("green")]; "Many Statements")]
#[test_case("0; 1; 2;" => Vec::<JSString>::new(); "No strings")]
fn statement_list_test_initial_string_tokens(src: &str) -> Vec<JSString> {
    let (item, _) = StatementList::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.initial_string_tokens().into_iter().map(|st| st.value).collect()
}
#[test_case("let a;" => vec![JSString::from("a")]; "One Decl")]
#[test_case("a; let b; c;" => vec![JSString::from("b")]; "Decl In List")]
#[test_case("let a; b; let q;" => vec![JSString::from("a"), JSString::from("q")]; "Decl In head, tail")]
fn statement_list_test_lexically_declared_names(src: &str) -> Vec<JSString> {
    let (item, _) = StatementList::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.lexically_declared_names()
}
#[test_case("item.#valid" => true; "One item valid")]
#[test_case("item.#valid; a;" => true; "Multi first valid")]
#[test_case("a; item.#valid;" => true; "Multi second valid")]
#[test_case("item.#invalid" => false; "One item invalid")]
#[test_case("item.#invalid; a;" => false; "Multi first invalid")]
#[test_case("a; item.#invalid;" => false; "Multi second invalid")]
fn statement_list_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = StatementList::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod statement_list {
    use super::*;
    use test_case::test_case;

    #[test_case("package;", true => sset(&[PACKAGE_NOT_ALLOWED]); "StatementListItem")]
    #[test_case("package;implements;", true => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "StatementList StatementListItem")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        StatementList::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict, false, false);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }
    #[test_case("arguments;" => true; "Item (yes)")]
    #[test_case("no;" => false; "Item (no)")]
    #[test_case("arguments; bob;" => true; "List (left)")]
    #[test_case("bob; arguments;" => true; "List (right)")]
    #[test_case("left; right;" => false; "List (none)")]
    fn contains_arguments(src: &str) -> bool {
        StatementList::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.contains_arguments()
    }

    #[test_case("var rust='bob';" => svec(&["rust = 'bob'"]); "item")]
    #[test_case("a; function b(){}; var third;" => svec(&["third"]); "list")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).statement_list().var_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("var rust='bob';" => svec(&["rust = 'bob'"]); "item")]
    #[test_case("a; function b(){}; var third;" => svec(&["function b (  ) {  }", "third"]); "list")]
    fn top_level_var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src)
            .statement_list()
            .top_level_var_scoped_declarations()
            .iter()
            .map(String::from)
            .collect::<Vec<_>>()
    }

    #[test_case("let dragon=xorn;" => svec(&["let dragon = xorn ;"]); "item")]
    #[test_case("const a=0; function b(){} class c{}" => svec(&["const a = 0 ;", "class c { }"]); "list")]
    fn top_level_lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src)
            .statement_list()
            .top_level_lexically_scoped_declarations()
            .iter()
            .map(String::from)
            .collect::<Vec<_>>()
    }

    #[test_case("let dragon=xorn;" => svec(&["let dragon = xorn ;"]); "item")]
    #[test_case("const a=0; function b(){} class c{}" => svec(&["const a = 0 ;", "function b (  ) {  }", "class c { }"]); "list")]
    fn lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).statement_list().lexically_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("    a;" => Location { starting_line: 1, starting_column: 5, span: Span { starting_index: 4, length: 2 } }; "statement")]
    #[test_case("    b; let a;" => Location { starting_line: 1, starting_column: 5, span: Span { starting_index: 4, length: 9 } }; "list")]
    fn location(src: &str) -> Location {
        Maker::new(src).statement_list().location()
    }
}

// STATEMENT LIST ITEM
#[test]
fn statement_list_item_test_01() {
    let (node, scanner) = check(StatementListItem::parse(&mut newparser("a;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 2);
    pretty_check(&*node, "StatementListItem: a ;", vec!["Statement: a ;"]);
    concise_check(&*node, "ExpressionStatement: a ;", vec!["IdentifierName: a", "Punctuator: ;"]);
    format!("{node:?}");
}
#[test]
fn statement_list_item_test_02() {
    let (node, scanner) = check(StatementListItem::parse(&mut newparser("let a;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 6);
    pretty_check(&*node, "StatementListItem: let a ;", vec!["Declaration: let a ;"]);
    concise_check(&*node, "LexicalDeclaration: let a ;", vec!["Keyword: let", "IdentifierName: a", "Punctuator: ;"]);
    format!("{node:?}");
}
#[test]
fn statement_list_item_test_err_01() {
    check_err(
        StatementListItem::parse(&mut newparser(""), Scanner::new(), false, false, true),
        "Declaration or Statement expected",
        1,
        1,
    );
}
#[test]
fn statement_list_item_test_prettyerrors_1() {
    let (item, _) =
        StatementListItem::parse(&mut newparser("statement_list;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_list_item_test_prettyerrors_2() {
    let (item, _) =
        StatementListItem::parse(&mut newparser("const declaration = 0;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_list_item_test_conciseerrors_1() {
    let (item, _) =
        StatementListItem::parse(&mut newparser("statement_list;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_list_item_test_conciseerrors_2() {
    let (item, _) =
        StatementListItem::parse(&mut newparser("const declaration = 0;"), Scanner::new(), false, false, true).unwrap();
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
    let (item, _) =
        StatementListItem::parse(&mut newparser("function a(){}"), Scanner::new(), true, true, true).unwrap();
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
    let (item, _) =
        StatementListItem::parse(&mut newparser("a: function b(){}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.top_level_var_declared_names(), &[JSString::from("b")]);
}
#[test]
fn statement_list_item_test_top_level_var_declared_names_02() {
    let (item, _) =
        StatementListItem::parse(&mut newparser("{ function b(){} }"), Scanner::new(), true, true, true).unwrap();
    let expected: &[JSString] = &[];
    assert_eq!(item.top_level_var_declared_names(), expected);
}
#[test]
fn statement_list_item_test_top_level_var_declared_names_03() {
    let (item, _) =
        StatementListItem::parse(&mut newparser("function b(){}"), Scanner::new(), true, true, true).unwrap();
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
    let (item, _) =
        StatementListItem::parse(&mut newparser("function b(){}"), Scanner::new(), true, true, true).unwrap();
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
#[test_case("continue x;" => (false, true, true, true); "continue x;")]
#[test_case("for (;;) continue x;" => (false, true, false, true); "for (;;) continue x;")]
#[test_case("let x;" => (false, false, false, false); "let x;")]
fn statement_list_item_test_contains_undefined_continue_target(src: &str) -> (bool, bool, bool, bool) {
    let (item, _) = StatementListItem::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (
        item.contains_undefined_continue_target(&[JSString::from("x")], &[]),
        item.contains_undefined_continue_target(&[JSString::from("y")], &[]),
        item.contains_undefined_continue_target(&[], &[JSString::from("x")]),
        item.contains_undefined_continue_target(&[], &[JSString::from("y")]),
    )
}
#[test_case("'string';" => Some(JSString::from("string")); "String Token")]
#[test_case("let a;" => None; "Declaration")]
fn statement_list_item_test_as_string_literal(src: &str) -> Option<JSString> {
    let (item, _) = StatementListItem::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.as_string_literal().map(|st| st.value)
}
#[test_case("a: b;" => Vec::<JSString>::new(); "Labelled Plain")]
#[test_case("b;" => Vec::<JSString>::new(); "Plain")]
#[test_case("a: function f(){}" => vec![JSString::from("f")]; "Labelled Function")]
#[test_case("const t=true,f=false;" => vec![JSString::from("t"), JSString::from("f")]; "Unlabelled Decl")]
fn statement_list_item_test_lexically_declared_names(src: &str) -> Vec<JSString> {
    let (item, _) = StatementListItem::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.lexically_declared_names()
}
#[test_case("item.#valid;" => true; "Statement valid")]
#[test_case("const a=item.#valid;" => true; "Declaration valid")]
#[test_case("item.#invalid;" => false; "Statement invalid")]
#[test_case("const a=item.#invalid;" => false; "Declaration invalid")]
fn statement_list_item_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = StatementListItem::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod statement_list_item {
    use super::*;
    use test_case::test_case;

    #[test_case("let package;", true => sset(&[PACKAGE_NOT_ALLOWED]); "Declaration")]
    #[test_case("package;", true => sset(&[PACKAGE_NOT_ALLOWED]); "Statement")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        StatementListItem::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict, false, false);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("arguments;" => true; "Stmt (yes)")]
    #[test_case(";" => false; "Stmt (no)")]
    #[test_case("let a=arguments;" => true; "Decl (yes)")]
    #[test_case("let b;" => false; "Decl (no)")]
    fn contains_arguments(src: &str) -> bool {
        StatementListItem::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.contains_arguments()
    }

    #[test_case("for(var idx=0; idx<10; idx++){a();}" => svec(&["idx = 0"]); "stmt")]
    #[test_case("function abcd(efg){hij;}" => svec(&[]); "decl")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).statement_list_item().var_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("for(var idx=0; idx<10; idx++){a();}" => svec(&["idx = 0"]); "stmt")]
    #[test_case("blue: function a(){}" => svec(&["function a (  ) {  }"]); "labelled stmt")]
    #[test_case("{blue: function a(){}}" => svec(&[]); "no longer top level")]
    #[test_case("function abcd(efg){hij;}" => svec(&["function abcd ( efg ) { hij ; }"]); "decl")]
    #[test_case("const rust=10;" => svec(&[]); "not hoistable")]
    fn top_level_var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src)
            .statement_list_item()
            .top_level_var_scoped_declarations()
            .iter()
            .map(String::from)
            .collect::<Vec<_>>()
    }

    #[test_case("var a=27;" => svec(&[]); "statement")]
    #[test_case("class rocket{}" => svec(&["class rocket { }"]); "declaration")]
    fn top_level_lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src)
            .statement_list_item()
            .top_level_lexically_scoped_declarations()
            .iter()
            .map(String::from)
            .collect::<Vec<_>>()
    }

    #[test_case("var a=27;" => svec(&[]); "statement")]
    #[test_case("class rocket{}" => svec(&["class rocket { }"]); "declaration")]
    #[test_case("lbl: function x() {}" => svec(&["function x (  ) {  }"]); "labelled function")]
    fn lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src)
            .statement_list_item()
            .lexically_scoped_declarations()
            .iter()
            .map(String::from)
            .collect::<Vec<_>>()
    }

    #[test_case("    a;" => Location { starting_line: 1, starting_column: 5, span: Span { starting_index: 4, length: 2 } }; "statement")]
    #[test_case("    let a;" => Location { starting_line: 1, starting_column: 5, span: Span { starting_index: 4, length: 6 } }; "declaration")]
    fn location(src: &str) -> Location {
        Maker::new(src).statement_list_item().location()
    }
}
