use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

// LABELLED STATEMENT
#[test]
fn labelled_statement_test_01() {
    let (node, scanner) =
        check(LabelledStatement::parse(&mut newparser("blue: orange;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 13);
    pretty_check(&*node, "LabelledStatement: blue : orange ;", &["LabelIdentifier: blue", "LabelledItem: orange ;"]);
    concise_check(
        &*node,
        "LabelledStatement: blue : orange ;",
        &["IdentifierName: blue", "Punctuator: :", "ExpressionStatement: orange ;"],
    );
    format!("{node:?}");
}
#[test]
fn labelled_statement_test_err_01() {
    check_err(
        LabelledStatement::parse(&mut newparser(""), Scanner::new(), false, false, true),
        "not an identifier",
        1,
        1,
    );
}
#[test]
fn labelled_statement_test_err_02() {
    check_err(LabelledStatement::parse(&mut newparser("a"), Scanner::new(), false, false, true), "‘:’ expected", 1, 2);
}
#[test]
fn labelled_statement_test_err_03() {
    check_err(
        LabelledStatement::parse(&mut newparser("a:"), Scanner::new(), false, false, true),
        "LabelledItem expected",
        1,
        3,
    );
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
    let (item, _) =
        LabelledStatement::parse(&mut newparser("i:function a(){}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.top_level_var_declared_names(), &["a"]);
}
#[test]
fn labelled_statement_test_var_declared_names_01() {
    let (item, _) = LabelledStatement::parse(&mut newparser("i:var a;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a"]);
}
#[test]
fn labelled_statement_test_var_declared_names_02() {
    let (item, _) =
        LabelledStatement::parse(&mut newparser("i:function a(){}"), Scanner::new(), true, true, true).unwrap();
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
#[test_case("a: function a(){}" => vec![JSString::from("a")]; "Function Def")]
fn labelled_statement_test_lexically_declared_names(src: &str) -> Vec<JSString> {
    let (item, _) = LabelledStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.lexically_declared_names()
}
#[test_case("a: b.#valid;" => true; "valid")]
#[test_case("a: b.#invalid;" => false; "invalid")]
fn labelled_statement_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = LabelledStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod labelled_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("package:implements;", true => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "LabelIdentifier : LabelledItem")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).labelled_statement().early_errors(&mut errs, strict, false, false);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("bob: function alice(){}" => true; "direct labelled function")]
    #[test_case("bob: alice: function foo(){}" => true; "indirect labelled function")]
    #[test_case("bob:;" => false; "not a function")]
    fn is_labelled_function(src: &str) -> bool {
        Maker::new(src).labelled_statement().is_labelled_function()
    }

    #[test_case("a:arguments;" => true; "yes")]
    #[test_case("a:b;" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).labelled_statement().contains_arguments()
    }

    #[test_case("a:function b(){}" => svec(&["function b (  ) {  }"]); "function")]
    #[test_case("a:{ function b(){} }" => svec(&[]); "too deep")]
    fn top_level_var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src)
            .labelled_statement()
            .top_level_var_scoped_declarations()
            .iter()
            .map(String::from)
            .collect::<Vec<_>>()
    }

    #[test_case("a:function b(){}" => svec(&[]); "function")]
    #[test_case("a:var u;" => svec(&["u"]); "not a function")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).labelled_statement().var_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("a:function b(){}" => svec(&["function b (  ) {  }"]); "function")]
    #[test_case("a:var u;" => svec(&[]); "not a function")]
    fn lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).labelled_statement().lexically_scoped_declarations().iter().map(String::from).collect()
    }

    #[test_case("   a:b();" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 6 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).labelled_statement().location()
    }
}

// LABELLED ITEM
#[test]
fn labelled_item_test_01() {
    let (node, scanner) = check(LabelledItem::parse(&mut newparser("orange;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 7);
    pretty_check(&*node, "LabelledItem: orange ;", &["Statement: orange ;"]);
    concise_check(&*node, "ExpressionStatement: orange ;", &["IdentifierName: orange", "Punctuator: ;"]);
    format!("{node:?}");
}
#[test]
fn labelled_item_test_02() {
    let (node, scanner) =
        check(LabelledItem::parse(&mut newparser("function a(){}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 14);
    pretty_check(&*node, "LabelledItem: function a (  ) {  }", &["FunctionDeclaration: function a (  ) {  }"]);
    concise_check(
        &*node,
        "FunctionDeclaration: function a (  ) {  }",
        &["Keyword: function", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
    );
    format!("{node:?}");
}
#[test]
fn labelled_item_test_err_01() {
    check_err(
        LabelledItem::parse(&mut newparser(""), Scanner::new(), false, false, true),
        "LabelledItem expected",
        1,
        1,
    );
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
#[test_case("a;" => Vec::<JSString>::new(); "Statement")]
#[test_case("function a(){}" => vec![JSString::from("a")]; "Function Def")]
fn labelled_item_test_lexically_declared_names(src: &str) -> Vec<JSString> {
    let (item, _) = LabelledItem::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.lexically_declared_names()
}
#[test_case("a.#valid;" => true; "stmt valid")]
#[test_case("function a(){b.#valid;}" => true; "fcn valid")]
#[test_case("a.#invalid;" => false; "stmt invalid")]
#[test_case("function a(){b.#invalid;}" => false; "fcn invalid")]
fn labelled_item_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = LabelledItem::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}

mod labelled_item {
    use super::*;
    use test_case::test_case;

    const LBL_FUNC_NOT_ALLOWED: &str = "Labelled functions not allowed in modern ECMAScript code";

    #[test_case("package;", true => sset(&[PACKAGE_NOT_ALLOWED]); "Statement")]
    #[test_case("function package(){}", true => sset(&[PACKAGE_NOT_ALLOWED, LBL_FUNC_NOT_ALLOWED]); "FunctionDeclaration (strict)")]
    #[test_case("function a(){}", false => sset(&[LBL_FUNC_NOT_ALLOWED]); "FunctionDeclaration (non-strict)")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).labelled_item().early_errors(&mut errs, strict, false, false);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("function alice(){}" => true; "direct labelled function")]
    #[test_case("alice: function foo(){}" => true; "indirect labelled function")]
    #[test_case("bob;" => false; "not a function")]
    fn is_labelled_function(src: &str) -> bool {
        Maker::new(src).labelled_item().is_labelled_function()
    }

    #[test_case("arguments;" => true; "Statement (yes)")]
    #[test_case(";" => false; "Statement (no)")]
    #[test_case("function a(){}" => false; "func")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).labelled_item().contains_arguments()
    }

    #[test_case("function b(){}" => svec(&["function b (  ) {  }"]); "function")]
    #[test_case("a:function b(){}" => svec(&["function b (  ) {  }"]); "labelled function")]
    #[test_case("var a;" => svec(&["a"]); "statement")]
    fn top_level_var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).labelled_item().top_level_var_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("function a(){}" => svec(&[]); "function")]
    #[test_case("var a;" => svec(&["a"]); "statement")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).labelled_item().var_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("   b();" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 4 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).labelled_item().location()
    }

    #[test_case("10;" => svec(&[]); "statement")]
    #[test_case("function blue(){}" => svec(&["function blue (  ) {  }"]); "function")]
    fn lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).labelled_item().lexically_scoped_declarations().iter().map(String::from).collect()
    }
}
