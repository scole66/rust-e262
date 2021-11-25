use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::test_agent;

// EMPTY STATEMENT
#[test]
fn empty_statement_test_01() {
    let (se, scanner) = check(EmptyStatement::parse(&mut newparser(";"), Scanner::new()));
    chk_scan(&scanner, 1);
    pretty_check(&*se, "EmptyStatement: ;", vec![]);
    concise_check(&*se, "Punctuator: ;", vec![]);
    format!("{:?}", se);
}
#[test]
fn empty_statement_test_02() {
    check_err(EmptyStatement::parse(&mut newparser(""), Scanner::new()), "‘;’ expected", 1, 1);
}
#[test]
fn empty_statement_test_prettyerrors_1() {
    let (item, _) = EmptyStatement::parse(&mut newparser(";"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn empty_statement_test_conciseerrors_1() {
    let (item, _) = EmptyStatement::parse(&mut newparser(";"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn empty_statement_test_contains_01() {
    let (item, _) = EmptyStatement::parse(&mut newparser(";"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
mod empty_statement {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        EmptyStatement::parse(&mut newparser(";"), Scanner::new()).unwrap().0.early_errors(&mut test_agent(), &mut vec![]);
    }
}
