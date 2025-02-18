#![expect(clippy::bool_assert_comparison)]
use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

// CONTINUE STATEMENT
#[test]
fn continue_statement_test_01() {
    let (node, scanner) = check(ContinueStatement::parse(&mut newparser("continue;"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "ContinueStatement: continue ;", &[]);
    concise_check(&*node, "ContinueStatement: continue ;", &["Keyword: continue", "Punctuator: ;"]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn continue_statement_test_02() {
    let (node, scanner) = check(ContinueStatement::parse(&mut newparser("continue a;"), Scanner::new(), false, false));
    chk_scan(&scanner, 11);
    pretty_check(&*node, "ContinueStatement: continue a ;", &["LabelIdentifier: a"]);
    concise_check(
        &*node,
        "ContinueStatement: continue a ;",
        &["Keyword: continue", "IdentifierName: a", "Punctuator: ;"],
    );
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn continue_statement_test_03() {
    let (node, scanner) = check(ContinueStatement::parse(&mut newparser("continue"), Scanner::new(), false, false));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "ContinueStatement: continue ;", &[]);
    concise_check(&*node, "ContinueStatement: continue ;", &["Keyword: continue", "Punctuator: ;"]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn continue_statement_test_04() {
    let (node, scanner) =
        check(ContinueStatement::parse(&mut newparser("continue label"), Scanner::new(), false, false));
    chk_scan(&scanner, 14);
    pretty_check(&*node, "ContinueStatement: continue label ;", &["LabelIdentifier: label"]);
    concise_check(
        &*node,
        "ContinueStatement: continue label ;",
        &["Keyword: continue", "IdentifierName: label", "Punctuator: ;"],
    );
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn continue_statement_test_05() {
    let (node, scanner) =
        check(ContinueStatement::parse(&mut newparser("continue\nlabel"), Scanner::new(), false, false));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "ContinueStatement: continue ;", &[]);
    concise_check(&*node, "ContinueStatement: continue ;", &["Keyword: continue", "Punctuator: ;"]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn continue_statement_test_err_01() {
    check_err(ContinueStatement::parse(&mut newparser(""), Scanner::new(), false, false), "‘continue’ expected", 1, 1);
}
#[test]
fn continue_statement_test_err_02() {
    check_err(
        ContinueStatement::parse(&mut newparser("continue for"), Scanner::new(), false, false),
        "‘;’ expected",
        1,
        10,
    );
}
#[test]
fn continue_statement_test_err_03() {
    check_err(
        ContinueStatement::parse(&mut newparser("continue a for"), Scanner::new(), false, false),
        "‘;’ expected",
        1,
        12,
    );
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
    (
        item.contains_undefined_continue_target(&[JSString::from("x")]),
        item.contains_undefined_continue_target(&[JSString::from("y")]),
    )
}

mod continue_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("continue;", true, false => sset(&[CONTINUE_ITER]); "continue, beyond iteration")]
    #[test_case("continue;", true, true => sset(&[]); "continue, within iteration")]
    #[test_case("continue package;", true, true => sset(&[PACKAGE_NOT_ALLOWED]); "continue LabelIdentifier ; (within)")]
    #[test_case("continue package;", true, false => sset(&[PACKAGE_NOT_ALLOWED, CONTINUE_ITER]); "continue LabelIdentifier ; (beyond)")]
    fn early_errors(src: &str, strict: bool, within_iteration: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        ContinueStatement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.early_errors(
            &mut errs,
            strict,
            within_iteration,
        );
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("   continue;" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 9 } }; "no label")]
    #[test_case("   continue lbl;" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 13 } }; "label")]
    fn location(src: &str) -> Location {
        Maker::new(src).continue_statement().location()
    }
}
