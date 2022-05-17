use super::testhelp::{check, check_err, chk_scan, newparser, set, Maker, PACKAGE_NOT_ALLOWED};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::{test_agent, unwind_syntax_error_object};
use ahash::AHashSet;
use test_case::test_case;

// THROW STATEMENT
#[test]
fn throw_statement_test_01() {
    let (node, scanner) = check(ThrowStatement::parse(&mut newparser("throw 0;"), Scanner::new(), false, false));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "ThrowStatement: throw 0 ;", vec!["Expression: 0"]);
    concise_check(&*node, "ThrowStatement: throw 0 ;", vec!["Keyword: throw", "Numeric: 0", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn throw_statement_test_errs() {
    check_err(ThrowStatement::parse(&mut newparser(""), Scanner::new(), false, false), "‘throw’ expected", 1, 1);
    check_err(
        ThrowStatement::parse(&mut newparser("throw"), Scanner::new(), false, false),
        "Expression expected",
        1,
        6,
    );
    check_err(
        ThrowStatement::parse(&mut newparser("throw\n0;"), Scanner::new(), false, false),
        "newline not allowed here",
        1,
        6,
    );
    check_err(ThrowStatement::parse(&mut newparser("throw 0@"), Scanner::new(), false, false), "‘;’ expected", 1, 8);
}
#[test]
fn throw_statement_test_printer_errs() {
    let src = "throw 0;";
    let (item, _) = ThrowStatement::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
    concise_error_validate(&*item);
}
fn throw_contains_check(src: &str, has_literal: bool) {
    let (item, _) = ThrowStatement::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), has_literal);
}
#[test]
fn throw_statement_test_contains() {
    throw_contains_check("throw 0;", true);
    throw_contains_check("throw a;", false);
}
#[test_case("throw a.#valid;" => true; "valid")]
#[test_case("throw a.#invalid;" => false; "invalid")]
fn all_private_identifiers_valid(src: &str) -> bool {
    Maker::new(src).throw_statement().all_private_identifiers_valid(&[JSString::from("#valid")])
}

#[test_case("throw package;", true => set(&[PACKAGE_NOT_ALLOWED]); "normal")]
fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
    let mut agent = test_agent();
    let mut errs = vec![];
    Maker::new(src).throw_statement().early_errors(&mut agent, &mut errs, strict);
    AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
}

#[test_case("throw arguments;" => true; "yes")]
#[test_case("throw a;" => false; "no")]
fn contains_arguments(src: &str) -> bool {
    Maker::new(src).throw_statement().contains_arguments()
}
