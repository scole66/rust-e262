use super::testhelp::{check, check_err, chk_scan, newparser, set, PACKAGE_NOT_ALLOWED};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::{test_agent, unwind_syntax_error_object};
use ahash::AHashSet;

// BREAK STATEMENT
#[test]
fn break_statement_test_01() {
    let (node, scanner) = check(BreakStatement::parse(&mut newparser("break;"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    pretty_check(&*node, "BreakStatement: break ;", vec![]);
    concise_check(&*node, "BreakStatement: break ;", vec!["Keyword: break", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn break_statement_test_02() {
    let (node, scanner) = check(BreakStatement::parse(&mut newparser("break a;"), Scanner::new(), false, false));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "BreakStatement: break a ;", vec!["LabelIdentifier: a"]);
    concise_check(&*node, "BreakStatement: break a ;", vec!["Keyword: break", "IdentifierName: a", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn break_statement_test_03() {
    let (node, scanner) = check(BreakStatement::parse(&mut newparser("break"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    pretty_check(&*node, "BreakStatement: break ;", vec![]);
    concise_check(&*node, "BreakStatement: break ;", vec!["Keyword: break", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn break_statement_test_04() {
    let (node, scanner) = check(BreakStatement::parse(&mut newparser("break\na"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    pretty_check(&*node, "BreakStatement: break ;", vec![]);
    concise_check(&*node, "BreakStatement: break ;", vec!["Keyword: break", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn break_statement_test_05() {
    let (node, scanner) = check(BreakStatement::parse(&mut newparser("break a\nb"), Scanner::new(), false, false));
    chk_scan(&scanner, 7);
    pretty_check(&*node, "BreakStatement: break a ;", vec!["LabelIdentifier: a"]);
    concise_check(&*node, "BreakStatement: break a ;", vec!["Keyword: break", "IdentifierName: a", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn break_statement_test_err_01() {
    check_err(BreakStatement::parse(&mut newparser(""), Scanner::new(), false, false), "‘break’ expected", 1, 1);
}
#[test]
fn break_statement_test_err_02() {
    check_err(BreakStatement::parse(&mut newparser("break for"), Scanner::new(), false, false), "‘;’ expected", 1, 6);
}
#[test]
fn break_statement_test_err_03() {
    check_err(BreakStatement::parse(&mut newparser("break a for"), Scanner::new(), false, false), "‘;’ expected", 1, 8);
}
#[test]
fn break_statement_test_prettyerrors_1() {
    let (item, _) = BreakStatement::parse(&mut newparser("break;"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn break_statement_test_prettyerrors_2() {
    let (item, _) = BreakStatement::parse(&mut newparser("break label;"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn break_statement_test_conciseerrors_1() {
    let (item, _) = BreakStatement::parse(&mut newparser("break;"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn break_statement_test_conciseerrors_2() {
    let (item, _) = BreakStatement::parse(&mut newparser("break label;"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn break_statement_test_contains_undefined_break_target_01() {
    let (item, _) = BreakStatement::parse(&mut newparser("break label;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("label")]), false);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("bob")]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("bob"), JSString::from("label")]), false);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("label"), JSString::from("bob")]), false);
}
#[test]
fn break_statement_test_contains_undefined_break_target_02() {
    let (item, _) = BreakStatement::parse(&mut newparser("break;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), false);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("label")]), false);
}
#[test]
fn break_statement_test_contains_01() {
    let (item, _) = BreakStatement::parse(&mut newparser("break label;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn break_statement_test_contains_02() {
    let (item, _) = BreakStatement::parse(&mut newparser("break;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
mod break_statement {
    use super::*;
    use test_case::test_case;

    const ILLEGAL_BREAK: &str = "break statement must lie within iteration or switch statement";

    #[test_case("break;", true, true => set(&[]); "simple break; ok")]
    #[test_case("break;", true, false => set(&[ILLEGAL_BREAK]); "break not in a good spot")]
    #[test_case("break package;", true, true => set(&[PACKAGE_NOT_ALLOWED]); "break LabelIdentifier ;")]
    fn early_errors(src: &str, strict: bool, within_breakable: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        BreakStatement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.early_errors(
            &mut agent,
            &mut errs,
            strict,
            within_breakable,
        );
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}
