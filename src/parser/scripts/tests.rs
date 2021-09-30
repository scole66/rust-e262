use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::scanner::StringDelimiter;
use crate::tests::unwind_syntax_error_object;

// SCRIPT
#[test]
fn script_test_01() {
    let (node, scanner) = check(Script::parse(&mut newparser("let a=1; /* a bunch more text */\n/* even new lines */"), Scanner::new()));
    assert_eq!(scanner.line, 2);
    assert_eq!(scanner.column, 21);
    assert_eq!(scanner.start_idx, 53);
    pretty_check(&*node, "Script: let a = 1 ;", vec!["ScriptBody: let a = 1 ;"]);
    concise_check(&*node, "LexicalDeclaration: let a = 1 ;", vec!["Keyword: let", "LexicalBinding: a = 1", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn script_test_02() {
    let (node, scanner) = check(Script::parse(&mut newparser(""), Scanner::new()));
    chk_scan(&scanner, 0);
    pretty_check(&*node, "Script: ", vec![]);
    concise_check(&*node, "Script:", vec![]);
    format!("{:?}", node);
}
#[test]
fn script_test_err_01() {
    check_err(Script::parse(&mut newparser("for"), Scanner::new()), "‘(’ expected", 1, 4);
}
#[test]
fn script_test_err_02() {
    check_err(Script::parse(&mut newparser("0;if;"), Scanner::new()), "EoF expected", 1, 3);
}
#[test]
fn script_test_prettyerrors_1() {
    let (item, _) = Script::parse(&mut newparser("null;"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn script_test_prettyerrors_2() {
    let (item, _) = Script::parse(&mut newparser(""), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn script_test_conciseerrors_1() {
    let (item, _) = Script::parse(&mut newparser("null;"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn script_test_conciseerrors_2() {
    let (item, _) = Script::parse(&mut newparser(""), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn script_test_contains_01() {
    let (item, _) = Script::parse(&mut newparser(""), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::ScriptBody), false);
}
#[test]
fn script_test_contains_02() {
    let (item, _) = Script::parse(&mut newparser("0;"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::ScriptBody), true);
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn script_test_contains_03() {
    let (item, _) = Script::parse(&mut newparser("a;"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::ScriptBody), true);
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn script_test_early_errors_01() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let (item, _) = Script::parse(&mut newparser(""), Scanner::new()).unwrap();
    assert_eq!(item.early_errors(&mut agent), &[] as &[Object]);
}
#[test]
fn script_test_early_errors_02() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let (item, _) = Script::parse(&mut newparser("0;"), Scanner::new()).unwrap();
    assert_eq!(item.early_errors(&mut agent), &[] as &[Object]);
}
#[test]
fn script_test_early_errors_03() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let (item, _) = Script::parse(&mut newparser("let x; const x=10;"), Scanner::new()).unwrap();
    let mut errs = item.early_errors(&mut agent);
    assert_eq!(errs.len(), 1);
    let err = errs.pop().unwrap();
    let msg = unwind_syntax_error_object(&mut agent, err);
    assert_eq!(msg, "Duplicate lexically declared names");
}
#[test]
fn script_test_early_errors_04() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let (item, _) = Script::parse(&mut newparser("let x; var x=10;"), Scanner::new()).unwrap();
    let mut errs = item.early_errors(&mut agent);
    assert_eq!(errs.len(), 1);
    let err = errs.pop().unwrap();
    let msg = unwind_syntax_error_object(&mut agent, err);
    assert_eq!(msg, "Name defined both lexically and var-style");
}
#[test]
fn script_test_early_errors_05() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let (item, _) = Script::parse(&mut newparser("break a;"), Scanner::new()).unwrap();
    let mut errs = item.early_errors(&mut agent);
    assert_eq!(errs.len(), 1);
    let err = errs.pop().unwrap();
    let msg = unwind_syntax_error_object(&mut agent, err);
    assert_eq!(msg, "undefined break target detected");
}

// SCRIPT BODY
#[test]
fn script_body_test_01() {
    let (node, scanner) = check(ScriptBody::parse(&mut newparser("let a=1;"), Scanner::new()));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "ScriptBody: let a = 1 ;", vec!["StatementList: let a = 1 ;"]);
    concise_check(&*node, "LexicalDeclaration: let a = 1 ;", vec!["Keyword: let", "LexicalBinding: a = 1", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn script_body_test_err_01() {
    check_err(ScriptBody::parse(&mut newparser(""), Scanner::new()), "Declaration or Statement expected", 1, 1);
}
#[test]
fn script_body_test_prettyerrors_1() {
    let (item, _) = ScriptBody::parse(&mut newparser(";"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn script_body_test_conciseerrors_1() {
    let (item, _) = ScriptBody::parse(&mut newparser(";"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn script_body_test_contains_01() {
    let (item, _) = ScriptBody::parse(&mut newparser("0;"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::StatementList), true);
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn script_body_test_contains_02() {
    let (item, _) = ScriptBody::parse(&mut newparser(";"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::StatementList), true);
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
fn script_body_ee_check_core(p: &mut Parser, desired: Option<&str>) {
    let (item, _) = ScriptBody::parse(p, Scanner::new()).unwrap();
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let mut errs = item.early_errors(&mut agent);
    match desired {
        Some(expected_message) => {
            assert_eq!(errs.len(), 1);
            let err = errs.pop().unwrap();
            let msg = unwind_syntax_error_object(&mut agent, err);
            assert_eq!(msg, expected_message);
        }
        None => {
            assert_eq!(errs, &[] as &[Object])
        }
    }
}
fn script_body_ee_check(src: &str, desired: Option<&str>) {
    let mut p = newparser(src);
    script_body_ee_check_core(&mut p, desired);
}
fn script_body_ee_check_direct(src: &str, desired: Option<&str>) {
    let mut p = newparser(src);
    p.direct = true;
    script_body_ee_check_core(&mut p, desired);
}
#[test]
fn script_body_test_early_errors_01() {
    script_body_ee_check("super();", Some("`super' not allowed in top-level code"));
}
#[test]
fn script_body_test_early_errors_02() {
    script_body_ee_check("b=new.target;", Some("`new.target` not allowed in top-level code"));
}
#[test]
fn script_body_test_early_errors_03() {
    script_body_ee_check("break t;", Some("undefined break target detected"));
}
#[test]
fn script_body_test_early_errors_04() {
    script_body_ee_check_direct("super();", None);
}
#[test]
fn script_body_test_early_errors_05() {
    script_body_ee_check_direct("a=new.target;", None);
}
#[test]
fn script_body_test_early_errors_06() {
    script_body_ee_check(";", None);
}
#[test]
fn script_body_test_early_errors_07() {
    script_body_ee_check("t:{t:;}", Some("duplicate labels detected"));
}
#[test]
fn script_body_test_early_errors_08() {
    script_body_ee_check("for (;;) continue bob;", Some("undefined continue target detected"));
}
#[test]
fn script_body_test_early_errors_09() {
    script_body_ee_check("a.#mystery;", Some("invalid private identifier detected"));
}
#[test]
fn script_body_test_directive_prologue_01() {
    let (item, _) = ScriptBody::parse(&mut newparser("'blue'; 'green'; 'orange'; 'use\\x20strict'; print(12.0); 'dinosaur';"), Scanner::new()).unwrap();

    let dp = item.directive_prologue();
    assert_eq!(
        dp,
        vec![
            StringToken { value: JSString::from("blue"), delimiter: StringDelimiter::Single, raw: None },
            StringToken { value: JSString::from("green"), delimiter: StringDelimiter::Single, raw: None },
            StringToken { value: JSString::from("orange"), delimiter: StringDelimiter::Single, raw: None },
            StringToken { value: JSString::from("use strict"), delimiter: StringDelimiter::Single, raw: Some(String::from("use\\x20strict")) }
        ]
    );
}
#[test]
fn script_body_test_directive_prologue_02() {
    let (item, _) = ScriptBody::parse(&mut newparser("print(12.0); 'dinosaur';"), Scanner::new()).unwrap();

    let dp = item.directive_prologue();
    assert_eq!(dp, &[]);
}
