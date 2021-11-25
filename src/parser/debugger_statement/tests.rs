use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::test_agent;

// DEBUGGER STATEMENT
#[test]
fn debugger_statement_test_01() {
    let (se, scanner) = check(DebuggerStatement::parse(&mut newparser("debugger;"), Scanner::new()));
    chk_scan(&scanner, 9);
    pretty_check(&*se, "DebuggerStatement: debugger ;", vec![]);
    concise_check(&*se, "DebuggerStatement: debugger ;", vec!["Keyword: debugger", "Punctuator: ;"]);
    format!("{:?}", se);
}
#[test]
fn debugger_statement_test_asi_01() {
    let (se, scanner) = check(DebuggerStatement::parse(&mut newparser("debugger"), Scanner::new()));
    chk_scan(&scanner, 8);
    pretty_check(&*se, "DebuggerStatement: debugger ;", vec![]);
    concise_check(&*se, "DebuggerStatement: debugger ;", vec!["Keyword: debugger", "Punctuator: ;"]);
    format!("{:?}", se);
}
#[test]
fn debugger_statement_test_err_01() {
    check_err(DebuggerStatement::parse(&mut newparser(""), Scanner::new()), "‘debugger’ expected", 1, 1);
}
#[test]
fn debugger_statement_test_err_02() {
    check_err(DebuggerStatement::parse(&mut newparser("debugger for"), Scanner::new()), "‘;’ expected", 1, 9);
}
#[test]
fn debugger_statement_test_prettyerrors_1() {
    let (item, _) = DebuggerStatement::parse(&mut newparser("debugger;"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn debugger_statement_test_conciseerrors_1() {
    let (item, _) = DebuggerStatement::parse(&mut newparser("debugger;"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn debugger_statement_test_contains_01() {
    let (item, _) = DebuggerStatement::parse(&mut newparser("debugger;"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
mod debugger_statement {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        DebuggerStatement::parse(&mut newparser("debugger;"), Scanner::new()).unwrap().0.early_errors(&mut test_agent(), &mut vec![]);
    }
}
