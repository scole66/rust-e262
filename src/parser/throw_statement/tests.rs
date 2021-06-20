use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

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
    check_err(ThrowStatement::parse(&mut newparser("throw"), Scanner::new(), false, false), "Expression expected", 1, 6);
    check_err(ThrowStatement::parse(&mut newparser("throw\n0;"), Scanner::new(), false, false), "Newline not allowed here.", 1, 6);
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
