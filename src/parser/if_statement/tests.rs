#![expect(clippy::bool_assert_comparison)]
use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

// IF STATEMENT
#[test]
fn if_statement_test_01() {
    let (node, scanner) =
        check(IfStatement::parse(&mut newparser("if (true) { a; }"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 16);
    pretty_check(&*node, "IfStatement: if ( true ) { a ; }", &["Expression: true", "Statement: { a ; }"]);
    concise_check(
        &*node,
        "IfStatement: if ( true ) { a ; }",
        &["Keyword: if", "Punctuator: (", "Keyword: true", "Punctuator: )", "Block: { a ; }"],
    );
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn if_statement_test_02() {
    let (node, scanner) =
        check(IfStatement::parse(&mut newparser("if (0) { a; } else { b; }"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 25);
    pretty_check(
        &*node,
        "IfStatement: if ( 0 ) { a ; } else { b ; }",
        &["Expression: 0", "Statement: { a ; }", "Statement: { b ; }"],
    );
    concise_check(
        &*node,
        "IfStatement: if ( 0 ) { a ; } else { b ; }",
        &[
            "Keyword: if",
            "Punctuator: (",
            "Numeric: 0",
            "Punctuator: )",
            "Block: { a ; }",
            "Keyword: else",
            "Block: { b ; }",
        ],
    );
    assert_ne!(format!("{node:?}"), "");
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
    check_err(
        IfStatement::parse(&mut newparser("if ("), Scanner::new(), false, false, true),
        "Expression expected",
        1,
        5,
    );
}
#[test]
fn if_statement_test_err_04() {
    check_err(IfStatement::parse(&mut newparser("if (0"), Scanner::new(), false, false, true), "‘)’ expected", 1, 6);
}
#[test]
fn if_statement_test_err_05() {
    check_err(
        IfStatement::parse(&mut newparser("if (0)"), Scanner::new(), false, false, true),
        "Statement expected",
        1,
        7,
    );
}
#[test]
fn if_statement_test_err_06() {
    check_err(
        IfStatement::parse(&mut newparser("if (0) a; else"), Scanner::new(), false, false, true),
        "Statement expected",
        1,
        15,
    );
}
#[test]
fn if_statement_test_prettyerrors_1() {
    let (item, _) = IfStatement::parse(&mut newparser("if (false) b;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn if_statement_test_prettyerrors_2() {
    let (item, _) =
        IfStatement::parse(&mut newparser("if (false) b; else f;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn if_statement_test_conciseerrors_1() {
    let (item, _) = IfStatement::parse(&mut newparser("if (false) b;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn if_statement_test_conciseerrors_2() {
    let (item, _) =
        IfStatement::parse(&mut newparser("if (false) b; else f;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn if_statement_test_var_declared_names_01() {
    let (item, _) =
        IfStatement::parse(&mut newparser("if (false) { var x; } else { var y; }"), Scanner::new(), true, true, true)
            .unwrap();
    assert_eq!(item.var_declared_names(), &["x", "y"]);
}
#[test]
fn if_statement_test_var_declared_names_02() {
    let (item, _) =
        IfStatement::parse(&mut newparser("if (false) { var x, y; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["x", "y"]);
}
#[test]
fn if_statement_test_contains_undefined_break_target_01() {
    let (item, _) =
        IfStatement::parse(&mut newparser("if (false) { break k; } else { ; }"), Scanner::new(), true, true, true)
            .unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn if_statement_test_contains_undefined_break_target_02() {
    let (item, _) =
        IfStatement::parse(&mut newparser("if (false) { break k; } else { ; }"), Scanner::new(), true, true, true)
            .unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("k")]), false);
}
#[test]
fn if_statement_test_contains_undefined_break_target_03() {
    let (item, _) =
        IfStatement::parse(&mut newparser("if (false) { ; } else { break k; }"), Scanner::new(), true, true, true)
            .unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn if_statement_test_contains_undefined_break_target_04() {
    let (item, _) =
        IfStatement::parse(&mut newparser("if (false) { ; } else { break k; }"), Scanner::new(), true, true, true)
            .unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("k")]), false);
}
#[test]
fn if_statement_test_contains_undefined_break_target_05() {
    let (item, _) =
        IfStatement::parse(&mut newparser("if (false) { break k; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn if_statement_test_contains_undefined_break_target_06() {
    let (item, _) =
        IfStatement::parse(&mut newparser("if (false) { break k; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("k")]), false);
}
#[test]
fn if_statement_test_contains_01() {
    let (item, _) = IfStatement::parse(&mut newparser("if (0) {} else {}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn if_statement_test_contains_02() {
    let (item, _) =
        IfStatement::parse(&mut newparser("if (a) {0;} else {}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn if_statement_test_contains_03() {
    let (item, _) =
        IfStatement::parse(&mut newparser("if (a) {} else {0;}"), Scanner::new(), true, true, true).unwrap();
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
#[test_case("if(1)continue x;" => (false, true); "if (1) continue x;")]
#[test_case("if(1)continue x; else 0;" => (false, true); "if (1) continue x; else 0;")]
#[test_case("if(1)0; else continue x;" => (false, true); "if (1) 0; else continue x;")]
fn if_statement_test_contains_undefined_continue_target(src: &str) -> (bool, bool) {
    let (item, _) = IfStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (
        item.contains_undefined_continue_target(&[JSString::from("x")]),
        item.contains_undefined_continue_target(&[JSString::from("y")]),
    )
}
#[test_case("if(a.#valid){}" => true; "elseless cond valid")]
#[test_case("if(a){b.#valid;}" => true; "elseless truthy valid")]
#[test_case("if(a.#valid){}else{}" => true; "else cond valid")]
#[test_case("if(a){b.#valid;}else{}" => true; "else truthy valid")]
#[test_case("if(a){}else{b.#valid;}" => true; "else falsey valid")]
#[test_case("if(a.#invalid){}" => false; "elseless cond invalid")]
#[test_case("if(a){b.#invalid;}" => false; "elseless truthy invalid")]
#[test_case("if(a.#invalid){}else{}" => false; "else cond invalid")]
#[test_case("if(a){b.#invalid;}else{}" => false; "else truthy invalid")]
#[test_case("if(a){}else{b.#invalid;}" => false; "else falsey invalid")]
fn if_statement_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = IfStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod if_statement {
    use super::*;
    use test_case::test_case;

    const LABELLED_FUNCTION_NOT_ALLOWED: &str = "Labelled functions not allowed in modern ECMAScript code";

    #[test_case("if (package) interface;", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "if (Expression) Statement")]
    #[test_case("if (package) interface; else implements;", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "if (Expression) Statement else Statement")]
    #[test_case("if (a) bob: function f(){}", false => sset(&[LABELLED_FUNCTION_NOT_ALLOWED]); "labelled function (no else)")]
    #[test_case("if (a) alpha; else b: function f(){}", false => sset(&[LABELLED_FUNCTION_NOT_ALLOWED]); "labelled function (in else clause)")]
    #[test_case("if (a) b: function f(){} else c;", false => sset(&[LABELLED_FUNCTION_NOT_ALLOWED]); "labelled fucntion (in then clause)")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        IfStatement::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict, false, false);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("if(arguments);else;" => true; "trinary (left)")]
    #[test_case("if(1)arguments;else;" => true; "trinary (middle)")]
    #[test_case("if(1);else arguments;" => true; "trinary (right)")]
    #[test_case("if(1);else;" => false; "trinary (none)")]
    #[test_case("if(arguments);" => true; "binary (left)")]
    #[test_case("if(1)arguments;" => true; "binary (right)")]
    #[test_case("if(1);" => false; "binary (none)")]
    fn contains_arguments(src: &str) -> bool {
        IfStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.contains_arguments()
    }

    #[test_case("if (a==b) { var x=67; } else { var y=12, b=0; }" => svec(&["x = 67", "y = 12", "b = 0"]); "with else")]
    #[test_case("if(0){var x;}" => svec(&["x"]); "without else")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).if_statement().var_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("   if (true) {}" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 12 } }; "without else")]
    #[test_case("   if (true) {} else {}" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 20 } }; "with else")]
    fn location(src: &str) -> Location {
        Maker::new(src).if_statement().location()
    }

    #[test_case("if (expr) first;" => "first ;"; "without else")]
    #[test_case("if (expr) first; else second;" => "first ;"; "with else")]
    fn first_statement(src: &str) -> String {
        Maker::new(src).if_statement().first_statement().to_string()
    }

    #[test_case("if (expr) first;" => "expr"; "without else")]
    #[test_case("if (expr) first; else second;" => "expr"; "with else")]
    fn expression(src: &str) -> String {
        Maker::new(src).if_statement().expression().to_string()
    }
}
