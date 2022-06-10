use super::testhelp::{check, check_err, chk_scan, newparser, set, svec, Maker, WITH_NOT_ALLOWED};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::{test_agent, unwind_syntax_error_object};
use ahash::AHashSet;
use test_case::test_case;

// WITH STATEMENT
#[test]
fn with_statement_test_01() {
    let (node, scanner) = check(WithStatement::parse(
        &mut newparser("with ( x in obj ) { x.used = true; }"),
        Scanner::new(),
        false,
        false,
        true,
    ));
    chk_scan(&scanner, 36);
    format!("{:?}", node);
    pretty_check(
        &*node,
        "WithStatement: with ( x in obj ) { x . used = true ; }",
        vec!["Expression: x in obj", "Statement: { x . used = true ; }"],
    );
    concise_check(
        &*node,
        "WithStatement: with ( x in obj ) { x . used = true ; }",
        vec![
            "Keyword: with",
            "Punctuator: (",
            "RelationalExpression: x in obj",
            "Punctuator: )",
            "Block: { x . used = true ; }",
        ],
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
    check_err(
        WithStatement::parse(&mut newparser("with("), Scanner::new(), false, false, true),
        "Expression expected",
        1,
        6,
    );
}
#[test]
fn with_statement_test_err_04() {
    check_err(WithStatement::parse(&mut newparser("with(a"), Scanner::new(), false, false, true), "‘)’ expected", 1, 7);
}
#[test]
fn with_statement_test_err_05() {
    check_err(
        WithStatement::parse(&mut newparser("with(a)"), Scanner::new(), false, false, true),
        "Statement expected",
        1,
        8,
    );
}
#[test]
fn with_statement_prettycheck_1() {
    let (item, _) = WithStatement::parse(
        &mut newparser("with (returns_an_object()) { obj = 12; }"),
        Scanner::new(),
        false,
        false,
        true,
    )
    .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn with_statement_concisecheck_1() {
    let (item, _) = WithStatement::parse(
        &mut newparser("with (returns_an_object()) { obj = 12; }"),
        Scanner::new(),
        false,
        false,
        true,
    )
    .unwrap();
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
    let (item, _) =
        WithStatement::parse(&mut newparser("with(0) lbl: { 0; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("lbl")]), true);
}
#[test_case("with(0)continue x;" => (false, true); "with (0) continue x;")]
fn with_statement_test_contains_undefined_continue_target(src: &str) -> (bool, bool) {
    let (item, _) = WithStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (
        item.contains_undefined_continue_target(&[JSString::from("x")]),
        item.contains_undefined_continue_target(&[JSString::from("y")]),
    )
}
#[test_case("with (a.#valid) {}" => true; "Expression valid")]
#[test_case("with (a) {a.#valid}" => true; "Statement valid")]
#[test_case("with (a.#invalid) {}" => false; "Expression invalid")]
#[test_case("with (a) {a.#invalid}" => false; "Statement invalid")]
fn with_statement_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = WithStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod with_statement {
    use super::*;
    use test_case::test_case;

    const BAD_BREAK: &str = "break statement must lie within iteration or switch statement";
    const BAD_CONTINUE: &str = "Continue statements must lie within iteration statements.";

    #[test_case("with(a){}", true => set(&[WITH_NOT_ALLOWED]); "strict")]
    #[test_case("with(function (){break;}()){continue;}", false => set(&[BAD_BREAK, BAD_CONTINUE]); "non-strict")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        Maker::new(src).with_statement().early_errors(&mut agent, &mut errs, strict, false, false);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }

    #[test_case("with(arguments);" => true; "Left")]
    #[test_case("with(1)arguments;" => true; "Right")]
    #[test_case("with(1);" => false; "None")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).with_statement().contains_arguments()
    }

    #[test_case("with (Object) { var a=3; var alpha=undefined; }" => svec(&["a = 3", "alpha = undefined"]); "with")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).with_statement().var_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("  with(1);" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 8 }}; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).with_statement().location()
    }
}
