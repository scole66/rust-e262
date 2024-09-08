use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

// RETURN STATEMENT
#[test]
fn return_statement_test_01() {
    let (node, scanner) = check(ReturnStatement::parse(&mut newparser("return;"), Scanner::new(), false, false));
    chk_scan(&scanner, 7);
    pretty_check(&*node, "ReturnStatement: return ;", &[]);
    concise_check(&*node, "ReturnStatement: return ;", &["Keyword: return", "Punctuator: ;"]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn return_statement_test_02() {
    let (node, scanner) = check(ReturnStatement::parse(&mut newparser("return null;"), Scanner::new(), false, false));
    chk_scan(&scanner, 12);
    pretty_check(&*node, "ReturnStatement: return null ;", &["Expression: null"]);
    concise_check(&*node, "ReturnStatement: return null ;", &["Keyword: return", "Keyword: null", "Punctuator: ;"]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn return_statement_test_asi_01() {
    let (node, scanner) = check(ReturnStatement::parse(&mut newparser("return"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    pretty_check(&*node, "ReturnStatement: return ;", &[]);
    concise_check(&*node, "ReturnStatement: return ;", &["Keyword: return", "Punctuator: ;"]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn return_statement_test_asi_02() {
    let (node, scanner) = check(ReturnStatement::parse(&mut newparser("return null"), Scanner::new(), false, false));
    chk_scan(&scanner, 11);
    pretty_check(&*node, "ReturnStatement: return null ;", &["Expression: null"]);
    concise_check(&*node, "ReturnStatement: return null ;", &["Keyword: return", "Keyword: null", "Punctuator: ;"]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn return_statement_test_err_01() {
    check_err(ReturnStatement::parse(&mut newparser(""), Scanner::new(), false, false), "‘return’ expected", 1, 1);
}
#[test]
fn return_statement_test_err_02() {
    check_err(ReturnStatement::parse(&mut newparser("return ="), Scanner::new(), false, false), "‘;’ expected", 1, 8);
}
#[test]
fn return_statement_test_err_03() {
    check_err(
        ReturnStatement::parse(&mut newparser("return 0 for"), Scanner::new(), false, false),
        "‘;’ expected",
        1,
        10,
    );
}
#[test]
fn return_statement_test_prettyerrors_1() {
    let (item, _) = ReturnStatement::parse(&mut newparser("return;"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn return_statement_test_prettyerrors_2() {
    let (item, _) = ReturnStatement::parse(&mut newparser("return undefined;"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn return_statement_test_conciseerrors_1() {
    let (item, _) = ReturnStatement::parse(&mut newparser("return;"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn return_statement_test_conciseerrors_2() {
    let (item, _) = ReturnStatement::parse(&mut newparser("return undefined;"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn return_statement_test_contains_01() {
    let (item, _) = ReturnStatement::parse(&mut newparser("return;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn return_statement_test_contains_02() {
    let (item, _) = ReturnStatement::parse(&mut newparser("return 0;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn return_statement_test_contains_03() {
    let (item, _) = ReturnStatement::parse(&mut newparser("return a;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("return;" => true; "no expression")]
#[test_case("return a.#valid;" => true; "expression valid")]
#[test_case("return a.#invalid;" => false; "expression invalid")]
fn return_statement_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ReturnStatement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod return_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("return;", true => sset(&[]); "return ;")]
    #[test_case("return package;", true => sset(&[PACKAGE_NOT_ALLOWED]); "return Expression ;")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).return_statement().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("return;" => false; "no exp")]
    #[test_case("return arguments;" => true; "Exp (yes)")]
    #[test_case("return a;" => false; "Exp (no)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).return_statement().contains_arguments()
    }

    #[test_case("  return;" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 7 }}; "bare")]
    #[test_case("  return  9;" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 10 }}; "value")]
    fn location(src: &str) -> Location {
        Maker::new(src).return_statement().location()
    }
}
