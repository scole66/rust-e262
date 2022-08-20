use super::testhelp::*;
use super::*;
use crate::prettyprint::testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

// STATEMENT
#[test]
fn statement_test_01() {
    let (node, scanner) = check(Statement::parse(&mut newparser("{ a=0; }"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "Statement: { a = 0 ; }", vec!["BlockStatement: { a = 0 ; }"]);
    concise_check(&*node, "Block: { a = 0 ; }", vec!["Punctuator: {", "ExpressionStatement: a = 0 ;", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn statement_test_02() {
    let (node, scanner) = check(Statement::parse(&mut newparser("var a=0;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "Statement: var a = 0 ;", vec!["VariableStatement: var a = 0 ;"]);
    concise_check(
        &*node,
        "VariableStatement: var a = 0 ;",
        vec!["Keyword: var", "VariableDeclaration: a = 0", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn statement_test_03() {
    let (node, scanner) = check(Statement::parse(&mut newparser(";"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 1);
    pretty_check(&*node, "Statement: ;", vec!["EmptyStatement: ;"]);
    concise_check(&*node, "Punctuator: ;", vec![]);
    format!("{:?}", node);
}
#[test]
fn statement_test_04() {
    let (node, scanner) = check(Statement::parse(&mut newparser("a();"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 4);
    pretty_check(&*node, "Statement: a ( ) ;", vec!["ExpressionStatement: a ( ) ;"]);
    concise_check(&*node, "ExpressionStatement: a ( ) ;", vec!["CallMemberExpression: a ( )", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn statement_test_05() {
    let (node, scanner) = check(Statement::parse(&mut newparser("if (a) {}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "Statement: if ( a ) { }", vec!["IfStatement: if ( a ) { }"]);
    concise_check(
        &*node,
        "IfStatement: if ( a ) { }",
        vec!["Keyword: if", "Punctuator: (", "IdentifierName: a", "Punctuator: )", "Block: { }"],
    );
    format!("{:?}", node);
}
#[test]
fn statement_test_06() {
    let (node, scanner) = check(Statement::parse(&mut newparser("switch (a) {}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 13);
    pretty_check(&*node, "Statement: switch ( a ) { }", vec!["BreakableStatement: switch ( a ) { }"]);
    concise_check(
        &*node,
        "SwitchStatement: switch ( a ) { }",
        vec!["Keyword: switch", "Punctuator: (", "IdentifierName: a", "Punctuator: )", "CaseBlock: { }"],
    );
    format!("{:?}", node);
}
#[test]
fn statement_test_07() {
    let (node, scanner) = check(Statement::parse(&mut newparser("continue;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "Statement: continue ;", vec!["ContinueStatement: continue ;"]);
    concise_check(&*node, "ContinueStatement: continue ;", vec!["Keyword: continue", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn statement_test_08() {
    let (node, scanner) = check(Statement::parse(&mut newparser("break;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 6);
    pretty_check(&*node, "Statement: break ;", vec!["BreakStatement: break ;"]);
    concise_check(&*node, "BreakStatement: break ;", vec!["Keyword: break", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn statement_test_09() {
    let (node, scanner) = check(Statement::parse(&mut newparser("return;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 7);
    pretty_check(&*node, "Statement: return ;", vec!["ReturnStatement: return ;"]);
    concise_check(&*node, "ReturnStatement: return ;", vec!["Keyword: return", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn statement_test_10() {
    let (node, scanner) = check(Statement::parse(&mut newparser("with(a)b;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "Statement: with ( a ) b ;", vec!["WithStatement: with ( a ) b ;"]);
    concise_check(
        &*node,
        "WithStatement: with ( a ) b ;",
        vec!["Keyword: with", "Punctuator: (", "IdentifierName: a", "Punctuator: )", "ExpressionStatement: b ;"],
    );
    format!("{:?}", node);
}
#[test]
fn statement_test_11() {
    let (node, scanner) = check(Statement::parse(&mut newparser("a:b;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 4);
    pretty_check(&*node, "Statement: a : b ;", vec!["LabelledStatement: a : b ;"]);
    concise_check(
        &*node,
        "LabelledStatement: a : b ;",
        vec!["IdentifierName: a", "Punctuator: :", "ExpressionStatement: b ;"],
    );
    format!("{:?}", node);
}
#[test]
fn statement_test_12() {
    let (node, scanner) = check(Statement::parse(&mut newparser("throw a;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "Statement: throw a ;", vec!["ThrowStatement: throw a ;"]);
    concise_check(&*node, "ThrowStatement: throw a ;", vec!["Keyword: throw", "IdentifierName: a", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn statement_test_13() {
    let (node, scanner) =
        check(Statement::parse(&mut newparser("try {} catch {}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 15);
    pretty_check(&*node, "Statement: try { } catch { }", vec!["TryStatement: try { } catch { }"]);
    concise_check(&*node, "TryStatement: try { } catch { }", vec!["Keyword: try", "Block: { }", "Catch: catch { }"]);
    format!("{:?}", node);
}
#[test]
fn statement_test_14() {
    let (node, scanner) = check(Statement::parse(&mut newparser("debugger;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "Statement: debugger ;", vec!["DebuggerStatement: debugger ;"]);
    concise_check(&*node, "DebuggerStatement: debugger ;", vec!["Keyword: debugger", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn statement_test_err_01() {
    check_err(Statement::parse(&mut newparser(""), Scanner::new(), false, false, true), "Statement expected", 1, 1);
}
#[test]
fn statement_test_err_02() {
    check_err(
        Statement::parse(&mut newparser("return;"), Scanner::new(), false, false, false),
        "Statement expected",
        1,
        1,
    );
}
#[test]
fn statement_test_prettyerrors_1() {
    let (item, _) = Statement::parse(&mut newparser("{}"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_test_prettyerrors_2() {
    let (item, _) = Statement::parse(&mut newparser("var a;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_test_prettyerrors_3() {
    let (item, _) = Statement::parse(&mut newparser(";"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_test_prettyerrors_4() {
    let (item, _) = Statement::parse(&mut newparser("if(a)b;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_test_prettyerrors_5() {
    let (item, _) = Statement::parse(&mut newparser("switch(a){}"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_test_prettyerrors_6() {
    let (item, _) = Statement::parse(&mut newparser("continue;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_test_prettyerrors_7() {
    let (item, _) = Statement::parse(&mut newparser("break;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_test_prettyerrors_8() {
    let (item, _) = Statement::parse(&mut newparser("return;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_test_prettyerrors_9() {
    let (item, _) = Statement::parse(&mut newparser("with(a){};"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_test_prettyerrors_10() {
    let (item, _) = Statement::parse(&mut newparser("a:b;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_test_prettyerrors_11() {
    let (item, _) = Statement::parse(&mut newparser("throw a;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_test_prettyerrors_12() {
    let (item, _) = Statement::parse(&mut newparser("try{}catch{}"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_test_prettyerrors_13() {
    let (item, _) = Statement::parse(&mut newparser("debugger;"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_test_prettyerrors_14() {
    let (item, _) = Statement::parse(&mut newparser("a();"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn statement_test_conciseerrors_1() {
    let (item, _) = Statement::parse(&mut newparser("{}"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_test_conciseerrors_2() {
    let (item, _) = Statement::parse(&mut newparser("var a;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_test_conciseerrors_3() {
    let (item, _) = Statement::parse(&mut newparser(";"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_test_conciseerrors_4() {
    let (item, _) = Statement::parse(&mut newparser("if(a)b;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_test_conciseerrors_5() {
    let (item, _) = Statement::parse(&mut newparser("switch(a){}"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_test_conciseerrors_6() {
    let (item, _) = Statement::parse(&mut newparser("continue;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_test_conciseerrors_7() {
    let (item, _) = Statement::parse(&mut newparser("break;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_test_conciseerrors_8() {
    let (item, _) = Statement::parse(&mut newparser("return;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_test_conciseerrors_9() {
    let (item, _) = Statement::parse(&mut newparser("with(a){};"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_test_conciseerrors_10() {
    let (item, _) = Statement::parse(&mut newparser("a:b;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_test_conciseerrors_11() {
    let (item, _) = Statement::parse(&mut newparser("throw a;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_test_conciseerrors_12() {
    let (item, _) = Statement::parse(&mut newparser("try{}catch{}"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_test_conciseerrors_13() {
    let (item, _) = Statement::parse(&mut newparser("debugger;"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_test_conciseerrors_14() {
    let (item, _) = Statement::parse(&mut newparser("a();"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn statement_test_cache_01() {
    let mut parser = newparser("A += 34;");
    let (node, scanner) = check(Statement::parse(&mut parser, Scanner::new(), true, false, false));
    let (node2, scanner2) = check(Statement::parse(&mut parser, Scanner::new(), true, false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
fn statement_vdn_check(src: &str, expected: &[&str]) {
    let (item, _) = Statement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), expected);
}
#[test]
fn statement_test_var_declared_names_01() {
    statement_vdn_check("{var a;}", &["a"]);
}
#[test]
fn statement_test_var_declared_names_02() {
    statement_vdn_check("var a;", &["a"]);
}
#[test]
fn statement_test_var_declared_names_03() {
    statement_vdn_check(";", &[]);
}
#[test]
fn statement_test_var_declared_names_04() {
    statement_vdn_check("0;", &[]);
}
#[test]
fn statement_test_var_declared_names_05() {
    statement_vdn_check("if(a)var x;", &["x"]);
}
#[test]
fn statement_test_var_declared_names_06() {
    statement_vdn_check("for(;;)var x;", &["x"]);
}
#[test]
fn statement_test_var_declared_names_07() {
    statement_vdn_check("continue;", &[]);
}
#[test]
fn statement_test_var_declared_names_08() {
    statement_vdn_check("break;", &[]);
}
#[test]
fn statement_test_var_declared_names_09() {
    statement_vdn_check("return;", &[]);
}
#[test]
fn statement_test_var_declared_names_10() {
    statement_vdn_check("with(0)var a;", &["a"]);
}
#[test]
fn statement_test_var_declared_names_11() {
    statement_vdn_check("t:var a;", &["a"]);
}
#[test]
fn statement_test_var_declared_names_12() {
    statement_vdn_check("throw 0;", &[]);
}
#[test]
fn statement_test_var_declared_names_13() {
    statement_vdn_check("try{var a;}catch{}", &["a"]);
}
#[test]
fn statement_test_var_declared_names_14() {
    statement_vdn_check("debugger;", &[]);
}
fn statement_cubt_check(src: &str, empty_set_result: bool, t_in_set_result: bool) {
    let (item, _) = Statement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), empty_set_result);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), t_in_set_result);
}
#[test]
fn statement_test_contains_undefined_break_target() {
    statement_cubt_check("var a;", false, false);
    statement_cubt_check(";", false, false);
    statement_cubt_check("0;", false, false);
    statement_cubt_check("continue;", false, false);
    statement_cubt_check("return;", false, false);
    statement_cubt_check("throw 0;", false, false);
    statement_cubt_check("debugger;", false, false);
    statement_cubt_check("{break t;}", true, false);
    statement_cubt_check("if(0)break t;", true, false);
    statement_cubt_check("for(;;)break t;", true, false);
    statement_cubt_check("break t;", true, false);
    statement_cubt_check("with(0)break t;", true, false);
    statement_cubt_check("a:break t;", true, false);
    statement_cubt_check("try{break t;}catch{}", true, false);
}
fn statement_contains_check(src: &str, kind: ParseNodeKind, has_literal: bool) {
    let (item, _) = Statement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(kind), true);
    assert_eq!(item.contains(ParseNodeKind::Literal), has_literal);
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn statement_test_contains_01() {
    statement_contains_check("{0;}", ParseNodeKind::BlockStatement, true);
    statement_contains_check("var a=0;", ParseNodeKind::VariableStatement, true);
    statement_contains_check(";", ParseNodeKind::EmptyStatement, false);
    statement_contains_check("0;", ParseNodeKind::ExpressionStatement, true);
    statement_contains_check("if(0)0;", ParseNodeKind::IfStatement, true);
    statement_contains_check("for(;;)0;", ParseNodeKind::BreakableStatement, true);
    statement_contains_check("continue;", ParseNodeKind::ContinueStatement, false);
    statement_contains_check("break;", ParseNodeKind::BreakStatement, false);
    statement_contains_check("with(0)0;", ParseNodeKind::WithStatement, true);
    statement_contains_check("a:0;", ParseNodeKind::LabelledStatement, true);
    statement_contains_check("throw 0;", ParseNodeKind::ThrowStatement, true);
    statement_contains_check("try{0;}catch{}", ParseNodeKind::TryStatement, true);
    statement_contains_check("debugger;", ParseNodeKind::DebuggerStatement, false);
    statement_contains_check("return 0;", ParseNodeKind::ReturnStatement, true);
}
fn stmt_cdl_check(src: &str, has_label: bool) {
    let (item, _) = Statement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), has_label);
}
#[test]
fn statement_test_contains_duplicate_labels() {
    stmt_cdl_check("{t:;}", true);
    stmt_cdl_check("break;", false);
    stmt_cdl_check("for(;;){t:;}", true);
    stmt_cdl_check("continue;", false);
    stmt_cdl_check("debugger;", false);
    stmt_cdl_check(";", false);
    stmt_cdl_check("0;", false);
    stmt_cdl_check("if(0)t:;", true);
    stmt_cdl_check("t:;", true);
    stmt_cdl_check("return;", false);
    stmt_cdl_check("throw x;", false);
    stmt_cdl_check("try{t:;}finally{}", true);
    stmt_cdl_check("var x;", false);
    stmt_cdl_check("with(a){t:;}", true);
}
#[test_case("{ continue x; }" => (false, true, true, true); "{ continue x; }")]
#[test_case("{ for (;;) continue x; }" => (false, true, true, true); "{ for (;;) continue x; }")]
#[test_case("break;" => (false, false, false, false); "break;")]
#[test_case("for (;;) continue x;" => (false, true, false, true); "for (;;) continue x;")]
#[test_case("for (;;) for (;;) continue x;" => (false, true, false, true); "for (;;) for (;;) continue x;")]
#[test_case("continue x;" => (false, true, true, true); "continue x;")]
#[test_case("debugger;" => (false, false, false, false); "debugger;")]
#[test_case(";" => (false, false, false, false); "semicolon")]
#[test_case("true;" => (false, false, false, false); "true;")]
#[test_case("if (true) continue x;" => (false, true, true, true); "if (true) continue x;")]
#[test_case("if (true) for (;;) continue x;" => (false, true, true, true); "if (true) for (;;) continue x;")]
#[test_case("lbl: continue x;" => (false, true, true, true); "lbl: continue x;")]
#[test_case("lbl: for (;;) continue x;" => (false, true, false, true); "lbl: for (;;) continue x;")]
#[test_case("return;" => (false, false, false, false); "return;")]
#[test_case("throw a;" => (false, false, false, false); "throw a;")]
#[test_case("try { continue x; } finally {}" => (false, true, true, true); "try { continue x; } finally {}")]
#[test_case("try { for (;;) continue x; } finally {}" => (false, true, true, true); "try { for (;;) continue x; } finally {}")]
#[test_case("var a;" => (false, false, false, false); "var a;")]
#[test_case("with (a) continue x;" => (false, true, true, true); "with (a) continue x;")]
#[test_case("with (a) for (;;) continue x;" => (false, true, true, true); "with (a) for (;;) continue x;")]
fn statement_test_contains_undefined_continue_target(src: &str) -> (bool, bool, bool, bool) {
    let (item, _) = Statement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (
        item.contains_undefined_continue_target(&[JSString::from("x")], &[]),
        item.contains_undefined_continue_target(&[JSString::from("y")], &[]),
        item.contains_undefined_continue_target(&[], &[JSString::from("x")]),
        item.contains_undefined_continue_target(&[], &[JSString::from("y")]),
    )
}
#[test_case("'string';" => Some(JSString::from("string")); "String Token")]
#[test_case(";" => None; "Empty Statement")]
#[test_case("{}" => None; "Block Statement")]
#[test_case("break;" => None; "Break Statement")]
#[test_case("for (;;) {}" => None; "Breakable Statement")]
#[test_case("continue;" => None; "Continue Statement")]
#[test_case("debugger;" => None; "Debugger Statement")]
#[test_case("if (true) a();" => None; "If Statement")]
#[test_case("blue: debugger;" => None; "Labelled Statement")]
#[test_case("return;" => None; "Return Statement")]
#[test_case("throw a;" => None; "Throw Statement")]
#[test_case("try{} catch(e) {}" => None; "Try Statement")]
#[test_case("var a;" => None; "Var Statement")]
#[test_case("with(a){};" => None; "With Statement")]
fn statement_test_as_string_literal(src: &str) -> Option<JSString> {
    let (item, _) = Statement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.as_string_literal().map(|st| st.value)
}
#[test_case(";" => true; "empty")]
#[test_case("break;" => true; "break_")]
#[test_case("continue;" => true; "continue_")]
#[test_case("debugger;" => true; "debugger")]
#[test_case("{ a.#valid; }" => true; "block valid")]
#[test_case("for (;;) a.#valid;" => true; "breakable valid")]
#[test_case("if (1) a.#valid;" => true; "if valid")]
#[test_case("blue: a.#valid;" => true; "labelled valid")]
#[test_case("return a.#valid;" => true; "return valid")]
#[test_case("throw a.#valid;" => true; "throw valid")]
#[test_case("try { a.#valid; } catch {}" => true; "try valid")]
#[test_case("var a=b.#valid;" => true; "var valid")]
#[test_case("with (a) b.#valid;" => true; "with valid")]
#[test_case("a.#valid;" => true; "expression valid")]
#[test_case("{ a.#invalid; }" => false; "block invalid")]
#[test_case("for (;;) a.#invalid;" => false; "breakable invalid")]
#[test_case("if (1) a.#invalid;" => false; "if invalid")]
#[test_case("blue: a.#invalid;" => false; "labelled invalid")]
#[test_case("return a.#invalid;" => false; "return invalid")]
#[test_case("throw a.#invalid;" => false; "throw invalid")]
#[test_case("try { a.#invalid; } catch {}" => false; "try invalid")]
#[test_case("var a=b.#invalid;" => false; "var invalid")]
#[test_case("with (a) b.#invalid;" => false; "with invalid")]
#[test_case("a.#invalid;" => false; "expression invalid")]
fn statement_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = Statement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod statement {
    use super::*;
    use test_case::test_case;
    #[test_case("{package;}", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "BlockStatement")]
    #[test_case("var package;", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "VariableStatement")]
    #[test_case(";", true, false => sset(&[]); "EmptyStatement")]
    #[test_case("package;", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "ExpressionStatement")]
    #[test_case("if (package);", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "IfStatement")]
    #[test_case("for(package=0;;);", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "BreakableStatement")]
    #[test_case("continue package;", true, false => sset(&[PACKAGE_NOT_ALLOWED, CONTINUE_ITER]); "ContinueStatement")]
    #[test_case("break package;", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "BreakStatement")]
    #[test_case("return package;", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "ReturnStatement")]
    #[test_case("with (package) {}", true, false => sset(&[WITH_NOT_ALLOWED, PACKAGE_NOT_ALLOWED]); "WithStatement")]
    #[test_case("package: implements;", true, false => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "LabelledStatement")]
    #[test_case("throw package;", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "ThrowStatement")]
    #[test_case("try {} catch (package) {}", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "TryStatement")]
    #[test_case("debugger;", true, false => sset(&[]); "DebuggerStatement")]
    fn early_errors(src: &str, strict: bool, wi: bool) -> AHashSet<String> {
        let agent = test_agent();
        let mut errs = vec![];
        Statement::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&agent, &mut errs, strict, wi, false);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&agent, err.clone())))
    }

    #[test_case("bob: function alice(){}" => true; "direct labelled function")]
    #[test_case("bob;" => false; "not a function")]
    fn is_labelled_function(src: &str) -> bool {
        Statement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.is_labelled_function()
    }

    #[test_case("debugger;" => false; "Debugger stmt")]
    #[test_case("continue;" => false; "Continued stmt")]
    #[test_case("break;" => false; "Break stmt")]
    #[test_case(";" => false; "Empty")]
    #[test_case("{arguments;}" => true; "Block (yes)")]
    #[test_case("var bob=arguments;" => true; "Var (yes)")]
    #[test_case("arguments;" => true; "Exp (yes)")]
    #[test_case("if (arguments) ;" => true; "If (yes)")]
    #[test_case("for (arguments;;);" => true; "Breakable (yes)")]
    #[test_case("return arguments;" => true; "Return (yes)")]
    #[test_case("with (arguments) {}" => true; "With (yes)")]
    #[test_case("a:arguments;" => true; "Labelled (yes)")]
    #[test_case("throw arguments;" => true; "Throw (yes)")]
    #[test_case("try {arguments;} finally{}" => true; "Try (yes)")]
    #[test_case("{xyzzy;}" => false; "Block (no)")]
    #[test_case("var bob=xyzzy;" => false; "Var (no)")]
    #[test_case("xyzzy;" => false; "Exp (no)")]
    #[test_case("if (xyzzy) ;" => false; "If (no)")]
    #[test_case("for (xyzzy;;);" => false; "Breakable (no)")]
    #[test_case("return xyzzy;" => false; "Return (no)")]
    #[test_case("with (xyzzy) {}" => false; "With (no)")]
    #[test_case("a:xyzzy;" => false; "Labelled (no)")]
    #[test_case("throw xyzzy;" => false; "Throw (no)")]
    #[test_case("try {xyzzy;} finally{}" => false; "Try (no)")]
    fn contains_arguments(src: &str) -> bool {
        Statement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.contains_arguments()
    }

    #[test_case(";" => svec(&[]); "empty")]
    #[test_case("debugger;" => svec(&[]); "debugger stmt")]
    #[test_case("continue;" => svec(&[]); "continue stmt")]
    #[test_case("break;" => svec(&[]); "break stmt")]
    #[test_case("3;" => svec(&[]); "Expression")]
    #[test_case("throw a;" => svec(&[]); "throw stmt")]
    #[test_case("return;" => svec(&[]); "return stmt")]
    #[test_case("{ var a; }" => svec(&["a"]); "block stmt")]
    #[test_case("var a;" => svec(&["a"]); "var stmt")]
    #[test_case("if(1){var a;}" => svec(&["a"]); "if stmt")]
    #[test_case("for(;;){var a;}" => svec(&["a"]); "breakable")]
    #[test_case("with(0){var a;}" => svec(&["a"]); "with stmt")]
    #[test_case("lbl:var a;" => svec(&["a"]); "labelled stmt")]
    #[test_case("try { var a; } finally {}" => svec(&["a"]); "try stmt")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).statement().var_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("   ;" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 1 } }; "empty")]
    #[test_case("   debugger;" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 9 } }; "debugger stmt")]
    #[test_case("   continue;" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 9 } }; "continue stmt")]
    #[test_case("   break;" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 6 } }; "break stmt")]
    #[test_case("   3;" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 2 } }; "Expression")]
    #[test_case("   throw a;" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 8 } }; "throw stmt")]
    #[test_case("   return;" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 7 } }; "return stmt")]
    #[test_case("   { var a; }" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 10 } }; "block stmt")]
    #[test_case("   var a;" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 6 } }; "var stmt")]
    #[test_case("   if(1){var a;}" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 13 } }; "if stmt")]
    #[test_case("   for(;;){var a;}" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 15 } }; "breakable")]
    #[test_case("   with(0){var a;}" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 15 } }; "with stmt")]
    #[test_case("   lbl:var a;" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 10 } }; "labelled stmt")]
    #[test_case("   try { var a; } finally {}" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 25 } }; "try stmt")]
    fn location(src: &str) -> Location {
        Maker::new(src).statement().location()
    }
}

mod hoistable_decl_part {
    use super::*;
    use test_case::test_case;

    #[test_case(HoistableDeclPart::FunctionDeclaration(Maker::new("function a(){}").function_declaration()) => with |s| assert_ne!(s, ""); "Function Decl")]
    #[test_case(HoistableDeclPart::GeneratorDeclaration(Maker::new("function *a(){}").generator_declaration()) => with |s| assert_ne!(s, ""); "Generator Decl")]
    #[test_case(HoistableDeclPart::AsyncFunctionDeclaration(Maker::new("async function a(){}").async_function_declaration()) => with |s| assert_ne!(s, ""); "Async Function Decl")]
    #[test_case(HoistableDeclPart::AsyncGeneratorDeclaration(Maker::new("async function *a(){}").async_generator_declaration()) => with |s| assert_ne!(s, ""); "Async Generator Decl")]
    fn debug(part: HoistableDeclPart) -> String {
        format!("{:?}", part)
    }

    #[test]
    fn clone() {
        let hdp = HoistableDeclPart::FunctionDeclaration(Maker::new("function a(){'hdp';}").function_declaration());
        let my_copy = hdp.clone();
        assert_eq!(hdp.to_string(), my_copy.to_string());
    }

    #[test_case(HoistableDeclPart::FunctionDeclaration(Maker::new("function banana(){}").function_declaration()) => "function banana (  ) {  }"; "Function Decl")]
    #[test_case(HoistableDeclPart::GeneratorDeclaration(Maker::new("function *a(){'apple';}").generator_declaration()) => "function * a (  ) { 'apple' ; }"; "Generator Decl")]
    #[test_case(HoistableDeclPart::AsyncFunctionDeclaration(Maker::new("async function a(strawberry){}").async_function_declaration()) => "async function a ( strawberry ) {  }"; "Async Function Decl")]
    #[test_case(HoistableDeclPart::AsyncGeneratorDeclaration(Maker::new("async function *a(){plum();}").async_generator_declaration()) => "async function * a (  ) { plum ( ) ; }"; "Async Generator Decl")]
    fn display(part: HoistableDeclPart) -> String {
        part.to_string()
    }
}

mod decl_part {
    use super::*;
    use test_case::test_case;

    #[test_case(DeclPart::FunctionDeclaration(Maker::new("function banana(){}").function_declaration()) => with |s| assert_ne!(s, ""); "function decl")]
    fn debug(part: DeclPart) -> String {
        format!("{:?}", part)
    }

    #[test_case(HoistableDeclPart::FunctionDeclaration(Maker::new("function banana(){}").function_declaration()) => "function banana (  ) {  }"; "Function Decl")]
    #[test_case(HoistableDeclPart::GeneratorDeclaration(Maker::new("function *a(){'apple';}").generator_declaration()) => "function * a (  ) { 'apple' ; }"; "Generator Decl")]
    #[test_case(HoistableDeclPart::AsyncFunctionDeclaration(Maker::new("async function a(strawberry){}").async_function_declaration()) => "async function a ( strawberry ) {  }"; "Async Function Decl")]
    #[test_case(HoistableDeclPart::AsyncGeneratorDeclaration(Maker::new("async function *a(){plum();}").async_generator_declaration()) => "async function * a (  ) { plum ( ) ; }"; "Async Generator Decl")]
    fn from_hoistable(part: HoistableDeclPart) -> String {
        DeclPart::from(part).to_string()
    }

    #[test_case(DeclPart::FunctionDeclaration(Maker::new("function banana(){}").function_declaration()) => "function banana (  ) {  }"; "Function Decl")]
    #[test_case(DeclPart::GeneratorDeclaration(Maker::new("function *a(){'apple';}").generator_declaration()) => "function * a (  ) { 'apple' ; }"; "Generator Decl")]
    #[test_case(DeclPart::AsyncFunctionDeclaration(Maker::new("async function a(strawberry){}").async_function_declaration()) => "async function a ( strawberry ) {  }"; "Async Function Decl")]
    #[test_case(DeclPart::AsyncGeneratorDeclaration(Maker::new("async function *a(){plum();}").async_generator_declaration()) => "async function * a (  ) { plum ( ) ; }"; "Async Generator Decl")]
    #[test_case(DeclPart::ClassDeclaration(Maker::new("class rust {}").class_declaration()) => "class rust { }"; "class def")]
    #[test_case(DeclPart::LexicalDeclaration(Maker::new("const PI = 4.0;").lexical_declaration()) => "const PI = 4 ;"; "lexical decl")]
    fn display(part: DeclPart) -> String {
        part.to_string()
    }

    #[test_case(DeclPart::FunctionDeclaration(Maker::new("function banana(){}").function_declaration()) => "function banana (  ) {  }"; "Function Decl")]
    #[test_case(DeclPart::GeneratorDeclaration(Maker::new("function *a(){'apple';}").generator_declaration()) => "function * a (  ) { 'apple' ; }"; "Generator Decl")]
    #[test_case(DeclPart::AsyncFunctionDeclaration(Maker::new("async function a(strawberry){}").async_function_declaration()) => "async function a ( strawberry ) {  }"; "Async Function Decl")]
    #[test_case(DeclPart::AsyncGeneratorDeclaration(Maker::new("async function *a(){plum();}").async_generator_declaration()) => "async function * a (  ) { plum ( ) ; }"; "Async Generator Decl")]
    #[test_case(DeclPart::ClassDeclaration(Maker::new("class rust {}").class_declaration()) => "class rust { }"; "class def")]
    #[test_case(DeclPart::LexicalDeclaration(Maker::new("const PI = 4.0;").lexical_declaration()) => "const PI = 4 ;"; "lexical decl")]
    fn string_from(part: DeclPart) -> String {
        String::from(&part)
    }

    #[test_case(Maker::new("function a(){}").function_declaration() => "function a (  ) {  }"; "function decl")]
    fn from_funcdecl(part: Rc<FunctionDeclaration>) -> String {
        DeclPart::from(part).to_string()
    }

    #[test_case(DeclPart::FunctionDeclaration(Maker::new("function banana(){}").function_declaration()) => false; "Function Decl")]
    #[test_case(DeclPart::GeneratorDeclaration(Maker::new("function *a(){'apple';}").generator_declaration()) => false; "Generator Decl")]
    #[test_case(DeclPart::AsyncFunctionDeclaration(Maker::new("async function a(strawberry){}").async_function_declaration()) => false; "Async Function Decl")]
    #[test_case(DeclPart::AsyncGeneratorDeclaration(Maker::new("async function *a(){plum();}").async_generator_declaration()) => false; "Async Generator Decl")]
    #[test_case(DeclPart::ClassDeclaration(Maker::new("class rust {}").class_declaration()) => false; "class def")]
    #[test_case(DeclPart::LexicalDeclaration(Maker::new("const PI = 4.0;").lexical_declaration()) => true; "const lexical decl")]
    #[test_case(DeclPart::LexicalDeclaration(Maker::new("let age = 49.0;").lexical_declaration()) => false; "mutable lexical decl")]
    fn is_constant_declaration(part: DeclPart) -> bool {
        part.is_constant_declaration()
    }

    #[test_case(DeclPart::FunctionDeclaration(Maker::new("function banana(){}").function_declaration()) => svec(&["banana"]); "Function Decl")]
    #[test_case(DeclPart::GeneratorDeclaration(Maker::new("function *a(){'apple';}").generator_declaration()) => svec(&["a"]); "Generator Decl")]
    #[test_case(DeclPart::AsyncFunctionDeclaration(Maker::new("async function a(strawberry){}").async_function_declaration()) => svec(&["a"]); "Async Function Decl")]
    #[test_case(DeclPart::AsyncGeneratorDeclaration(Maker::new("async function *a(){plum();}").async_generator_declaration()) => svec(&["a"]); "Async Generator Decl")]
    #[test_case(DeclPart::ClassDeclaration(Maker::new("class rust {}").class_declaration()) => svec(&["rust"]); "class def")]
    #[test_case(DeclPart::LexicalDeclaration(Maker::new("const PI = 4.0;").lexical_declaration()) => svec(&["PI"]); "const lexical decl")]
    #[test_case(DeclPart::LexicalDeclaration(Maker::new("let age = 49.0, b, c, elephant;").lexical_declaration()) => svec(&["age", "b", "c", "elephant"]); "many lexical decl")]
    fn bound_names(part: DeclPart) -> Vec<String> {
        part.bound_names().iter().map(String::from).collect()
    }
}

// DECLARATION
#[test]
fn declaration_test_01() {
    let (node, scanner) = check(Declaration::parse(&mut newparser("function a(){}"), Scanner::new(), false, false));
    chk_scan(&scanner, 14);
    pretty_check(&*node, "Declaration: function a (  ) {  }", vec!["HoistableDeclaration: function a (  ) {  }"]);
    concise_check(
        &*node,
        "FunctionDeclaration: function a (  ) {  }",
        vec![
            "Keyword: function",
            "IdentifierName: a",
            "Punctuator: (",
            "Punctuator: )",
            "Punctuator: {",
            "Punctuator: }",
        ],
    );
    format!("{:?}", node);
}
#[test]
fn declaration_test_02() {
    let (node, scanner) = check(Declaration::parse(&mut newparser("class a{}"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "Declaration: class a { }", vec!["ClassDeclaration: class a { }"]);
    concise_check(
        &*node,
        "ClassDeclaration: class a { }",
        vec!["Keyword: class", "IdentifierName: a", "ClassTail: { }"],
    );
    format!("{:?}", node);
}
#[test]
fn declaration_test_03() {
    let (node, scanner) = check(Declaration::parse(&mut newparser("let a;"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    pretty_check(&*node, "Declaration: let a ;", vec!["LexicalDeclaration: let a ;"]);
    concise_check(&*node, "LexicalDeclaration: let a ;", vec!["Keyword: let", "IdentifierName: a", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn declaration_test_err_01() {
    check_err(Declaration::parse(&mut newparser(""), Scanner::new(), false, false), "Declaration expected", 1, 1);
}
#[test]
fn declaration_test_prettyerrors_1() {
    let (item, _) = Declaration::parse(&mut newparser("function a(){}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn declaration_test_prettyerrors_2() {
    let (item, _) = Declaration::parse(&mut newparser("class a{}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn declaration_test_prettyerrors_3() {
    let (item, _) = Declaration::parse(&mut newparser("let a;"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn declaration_test_conciseerrors_1() {
    let (item, _) = Declaration::parse(&mut newparser("function a(){}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn declaration_test_conciseerrors_2() {
    let (item, _) = Declaration::parse(&mut newparser("class a{}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn declaration_test_conciseerrors_3() {
    let (item, _) = Declaration::parse(&mut newparser("let a;"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
fn declaration_bn_check(src: &str) {
    let (item, _) = Declaration::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn declaration_test_bound_names() {
    declaration_bn_check("function a(){}");
    declaration_bn_check("class a{}");
    declaration_bn_check("let a;");
}
fn declaration_contains_check(src: &str, has_literal: bool) {
    let (item, _) = Declaration::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), has_literal);
}
#[test]
fn declaration_test_contains() {
    declaration_contains_check("function a(){}", false);
    declaration_contains_check("class a extends b[0]{}", true);
    declaration_contains_check("class a{}", false);
    declaration_contains_check("let a=0;", true);
    declaration_contains_check("let a;", false);
}
#[test_case("function b(a){a.#valid;}" => true; "Hoistable valid")]
#[test_case("class a {b=c.#valid;}" => true; "ClassDefinition valid")]
#[test_case("let a=b.#valid;" => true; "LexicalDeclaration valid")]
#[test_case("function b(a){a.#invalid;}" => false; "Hoistable invalid")]
#[test_case("class a {b=c.#invalid;}" => false; "ClassDefinition invalid")]
#[test_case("let a=b.#invalid;" => false; "LexicalDeclaration invalid")]
fn declaration_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = Declaration::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod declaration {
    use super::*;
    use test_case::test_case;

    #[test_case("function package(){}", true => sset(&[PACKAGE_NOT_ALLOWED]); "HoistableDeclaration")]
    #[test_case("class package{}", true => sset(&[PACKAGE_NOT_ALLOWED]); "ClassDeclaration")]
    #[test_case("let package;", true => sset(&[PACKAGE_NOT_ALLOWED]); "LexicalDeclaration")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let agent = test_agent();
        let mut errs = vec![];
        Declaration::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .early_errors(&agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&agent, err.clone())))
    }

    #[test_case("function a(){}" => false; "Hoistable")]
    #[test_case("class foo { [arguments] = 12; }" => true; "Class (yes)")]
    #[test_case("let a=arguments;" => true; "Lexical (yes)")]
    #[test_case("class foo { [xyzzy] = 12; }" => false; "Class (no)")]
    #[test_case("let a=xyzzy;" => false; "Lexical (no)")]
    fn contains_arguments(src: &str) -> bool {
        Declaration::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("function kobold(){}" => "function kobold (  ) {  }"; "hoistable")]
    #[test_case("class goblin {}" => "class goblin { }"; "class def")]
    #[test_case("const pixie = bullywug;" => "const pixie = bullywug ;"; "lexical decl")]
    fn declaration_part(src: &str) -> String {
        Maker::new(src).declaration().declaration_part().to_string()
    }

    #[test_case("function kobold(){}" => svec(&[]); "hoistable")]
    #[test_case("class goblin {}" => svec(&["class goblin { }"]); "class def")]
    #[test_case("const pixie = bullywug;" => svec(&["const pixie = bullywug ;"]); "lexical decl")]
    fn top_level_lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src)
            .declaration()
            .top_level_lexically_scoped_declarations()
            .iter()
            .map(String::from)
            .collect::<Vec<_>>()
    }

    #[test_case("   function kobold(){}" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 19 } }; "hoistable")]
    #[test_case("   class goblin {}" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 15 } }; "class def")]
    #[test_case("   const pixie = bullywug;" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 23 } }; "lexical decl")]
    fn location(src: &str) -> Location {
        Maker::new(src).declaration().location()
    }
}

// HOISTABLE DECLARATION
#[test]
fn hoistable_declaration_test_01() {
    let (node, scanner) =
        check(HoistableDeclaration::parse(&mut newparser("function a(){}"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 14);
    pretty_check(
        &*node,
        "HoistableDeclaration: function a (  ) {  }",
        vec!["FunctionDeclaration: function a (  ) {  }"],
    );
    concise_check(
        &*node,
        "FunctionDeclaration: function a (  ) {  }",
        vec![
            "Keyword: function",
            "IdentifierName: a",
            "Punctuator: (",
            "Punctuator: )",
            "Punctuator: {",
            "Punctuator: }",
        ],
    );
    format!("{:?}", node);
}
#[test]
fn hoistable_declaration_test_02() {
    let (node, scanner) =
        check(HoistableDeclaration::parse(&mut newparser("function *a(){}"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 15);
    pretty_check(
        &*node,
        "HoistableDeclaration: function * a (  ) {  }",
        vec!["GeneratorDeclaration: function * a (  ) {  }"],
    );
    concise_check(
        &*node,
        "GeneratorDeclaration: function * a (  ) {  }",
        vec![
            "Keyword: function",
            "Punctuator: *",
            "IdentifierName: a",
            "Punctuator: (",
            "Punctuator: )",
            "Punctuator: {",
            "Punctuator: }",
        ],
    );
    format!("{:?}", node);
}
#[test]
fn hoistable_declaration_test_03() {
    let (node, scanner) =
        check(HoistableDeclaration::parse(&mut newparser("async function a(){}"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 20);
    pretty_check(
        &*node,
        "HoistableDeclaration: async function a (  ) {  }",
        vec!["AsyncFunctionDeclaration: async function a (  ) {  }"],
    );
    concise_check(
        &*node,
        "AsyncFunctionDeclaration: async function a (  ) {  }",
        vec![
            "Keyword: async",
            "Keyword: function",
            "IdentifierName: a",
            "Punctuator: (",
            "Punctuator: )",
            "Punctuator: {",
            "Punctuator: }",
        ],
    );
    format!("{:?}", node);
}
#[test]
fn hoistable_declaration_test_04() {
    let (node, scanner) = check(HoistableDeclaration::parse(
        &mut newparser("async function *a(){}"),
        Scanner::new(),
        false,
        false,
        false,
    ));
    chk_scan(&scanner, 21);
    pretty_check(
        &*node,
        "HoistableDeclaration: async function * a (  ) {  }",
        vec!["AsyncGeneratorDeclaration: async function * a (  ) {  }"],
    );
    concise_check(
        &*node,
        "AsyncGeneratorDeclaration: async function * a (  ) {  }",
        vec![
            "Keyword: async",
            "Keyword: function",
            "Punctuator: *",
            "IdentifierName: a",
            "Punctuator: (",
            "Punctuator: )",
            "Punctuator: {",
            "Punctuator: }",
        ],
    );
    format!("{:?}", node);
}
#[test]
fn hoistable_declaration_test_err_01() {
    check_err(
        HoistableDeclaration::parse(&mut newparser(""), Scanner::new(), false, false, false),
        "HoistableDeclaration expected",
        1,
        1,
    );
}
#[test]
fn hoistable_declaration_test_prettyerrors_1() {
    let (item, _) =
        HoistableDeclaration::parse(&mut newparser("function a(){}"), Scanner::new(), false, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn hoistable_declaration_test_prettyerrors_2() {
    let (item, _) =
        HoistableDeclaration::parse(&mut newparser("function *a(){}"), Scanner::new(), false, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn hoistable_declaration_test_prettyerrors_3() {
    let (item, _) =
        HoistableDeclaration::parse(&mut newparser("async function a(){}"), Scanner::new(), false, false, false)
            .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn hoistable_declaration_test_prettyerrors_4() {
    let (item, _) =
        HoistableDeclaration::parse(&mut newparser("async function *a(){}"), Scanner::new(), false, false, false)
            .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn hoistable_declaration_test_conciseerrors_1() {
    let (item, _) =
        HoistableDeclaration::parse(&mut newparser("function a(){}"), Scanner::new(), false, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn hoistable_declaration_test_conciseerrors_2() {
    let (item, _) =
        HoistableDeclaration::parse(&mut newparser("function *a(){}"), Scanner::new(), false, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn hoistable_declaration_test_conciseerrors_3() {
    let (item, _) =
        HoistableDeclaration::parse(&mut newparser("async function a(){}"), Scanner::new(), false, false, false)
            .unwrap();
    concise_error_validate(&*item);
}
#[test]
fn hoistable_declaration_test_conciseerrors_4() {
    let (item, _) =
        HoistableDeclaration::parse(&mut newparser("async function *a(){}"), Scanner::new(), false, false, false)
            .unwrap();
    concise_error_validate(&*item);
}
fn hoistable_bn_check(src: &str) {
    let (item, _) = HoistableDeclaration::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn hoistable_declaration_test_bound_names() {
    hoistable_bn_check("function a(){}");
    hoistable_bn_check("function *a(){}");
    hoistable_bn_check("async function a(){}");
    hoistable_bn_check("async function *a(){}");
}
fn hoistable_contains_check(src: &str) {
    let (item, _) = HoistableDeclaration::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn hoistable_declaration_test_contains() {
    hoistable_contains_check("function a(b=0){0;}");
    hoistable_contains_check("function *a(b=0){0;}");
    hoistable_contains_check("async function a(b=0){0;}");
    hoistable_contains_check("async function *a(b=0){0;}");
}
#[test_case("function a(b){b.#valid;}" => true; "function valid")]
#[test_case("function *a(b){b.#valid;}" => true; "generator valid")]
#[test_case("async function a(b){b.#valid;}" => true; "async function valid")]
#[test_case("async function *a(b){b.#valid;}" => true; "async generator valid")]
#[test_case("function a(b){b.#invalid;}" => false; "function invalid")]
#[test_case("function *a(b){b.#invalid;}" => false; "generator invalid")]
#[test_case("async function a(b){b.#invalid;}" => false; "async function invalid")]
#[test_case("async function *a(b){b.#invalid;}" => false; "async generator invalid")]
fn hoistable_declaration_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = HoistableDeclaration::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod hoistable_declaration {
    use super::*;
    use test_case::test_case;

    #[test_case("function package(){}", true => sset(&[PACKAGE_NOT_ALLOWED]); "FunctionDeclaration")]
    #[test_case("function *package(){}", true => sset(&[PACKAGE_NOT_ALLOWED]); "GeneratorDeclaration")]
    #[test_case("async function package(){}", true => sset(&[PACKAGE_NOT_ALLOWED]); "AsyncFunctionDeclaration")]
    #[test_case("async function *package(){}", true => panics "not yet implemented" /* sset(&[PACKAGE_NOT_ALLOWED]) */; "AsyncGeneratorDeclaration")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let agent = test_agent();
        let mut errs = vec![];
        HoistableDeclaration::parse(&mut newparser(src), Scanner::new(), true, true, false)
            .unwrap()
            .0
            .early_errors(&agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&agent, err.clone())))
    }

    #[test_case("function a(){}" => "function a (  ) {  }"; "function def")]
    #[test_case("function *a(){}" => "function * a (  ) {  }"; "generator def")]
    #[test_case("async function a(){}" => "async function a (  ) {  }"; "async fcn")]
    #[test_case("async function *a(){}" => "async function * a (  ) {  }"; "async gen")]
    fn declaration_part(src: &str) -> String {
        Maker::new(src).hoistable_declaration().declaration_part().to_string()
    }

    #[test_case("   function kobold(){}" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 19 } }; "function def")]
    #[test_case("   function *kobold(){}" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 20 } }; "generator def")]
    #[test_case("   async function kobold(){}" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 25 } }; "async fcn")]
    #[test_case("   async function *kobold(){}" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 26 } }; "async gen")]
    fn location(src: &str) -> Location {
        Maker::new(src).hoistable_declaration().location()
    }
}

// BREAKABLE STATEMENT
#[test]
fn breakable_statement_test_01() {
    let (node, scanner) =
        check(BreakableStatement::parse(&mut newparser("while (a);"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 10);
    pretty_check(&*node, "BreakableStatement: while ( a ) ;", vec!["IterationStatement: while ( a ) ;"]);
    concise_check(
        &*node,
        "WhileStatement: while ( a ) ;",
        vec!["Keyword: while", "Punctuator: (", "IdentifierName: a", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn breakable_statement_test_02() {
    let (node, scanner) =
        check(BreakableStatement::parse(&mut newparser("switch(a){}"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 11);
    pretty_check(&*node, "BreakableStatement: switch ( a ) { }", vec!["SwitchStatement: switch ( a ) { }"]);
    concise_check(
        &*node,
        "SwitchStatement: switch ( a ) { }",
        vec!["Keyword: switch", "Punctuator: (", "IdentifierName: a", "Punctuator: )", "CaseBlock: { }"],
    );
    format!("{:?}", node);
}
#[test]
fn breable_statement_test_err_01() {
    check_err(
        BreakableStatement::parse(&mut newparser(""), Scanner::new(), false, false, false),
        "BreakableStatement expected",
        1,
        1,
    );
}
#[test]
fn breakable_statement_test_prettyerrors_1() {
    let (item, _) =
        BreakableStatement::parse(&mut newparser("while(a);"), Scanner::new(), false, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn breakable_statement_test_prettyerrors_2() {
    let (item, _) =
        BreakableStatement::parse(&mut newparser("switch(a){}"), Scanner::new(), false, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn breakable_statement_test_conciseerrors_1() {
    let (item, _) =
        BreakableStatement::parse(&mut newparser("while(a);"), Scanner::new(), false, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn breakable_statement_test_conciseerrors_2() {
    let (item, _) =
        BreakableStatement::parse(&mut newparser("switch(a){}"), Scanner::new(), false, false, false).unwrap();
    concise_error_validate(&*item);
}
fn breakable_vdn_check(src: &str) {
    let (item, _) = BreakableStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a"]);
}
#[test]
fn breakable_statement_test_var_declared_names() {
    breakable_vdn_check("for(;;)var a;");
    breakable_vdn_check("switch(0){default:var a;}");
}
fn breakable_cubt_check(src: &str) {
    let (item, _) = BreakableStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn breakable_statement_test_contains_undefined_break_target() {
    breakable_cubt_check("for(;;)break t;");
    breakable_cubt_check("switch(0){default:break t;}");
}
fn breakable_contains_check(src: &str, has_literal: bool) {
    let (item, _) = BreakableStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), has_literal);
}
#[test]
fn breakable_statement_test_contains() {
    breakable_contains_check("for(;;)0;", true);
    breakable_contains_check("switch(a){default:0;}", true);
    breakable_contains_check("for(;;);", false);
    breakable_contains_check("switch(a){default:;}", false);
}
fn bs_cdl_check(src: &str) {
    let (item, _) = BreakableStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
}
#[test]
fn breakable_statement_test_contains_duplicate_labels() {
    bs_cdl_check("do{t:;}while(0);");
    bs_cdl_check("switch(a){case 3:t:;}");
}
#[test_case("for (;;) continue x;" => (false, true, false, true); "for (;;) continue x;")]
#[test_case("for (;;) for (;;) continue x;" => (false, true, false, true); "for (;;) for (;;) continue x;")]
#[test_case("switch (a) { case 3: continue x; }" => (false, true, true, true); "switch (a) { case 3: continue x; }")]
#[test_case("switch (a) { case 3: for (;;) continue x; }" => (false, true, true, true); "switch (a) { case 3: for (;;) continue x; }")]
fn breakable_statement_test_contains_undefined_continue_target(src: &str) -> (bool, bool, bool, bool) {
    let (item, _) = BreakableStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (
        item.contains_undefined_continue_target(&[JSString::from("x")], &[]),
        item.contains_undefined_continue_target(&[JSString::from("y")], &[]),
        item.contains_undefined_continue_target(&[], &[JSString::from("x")]),
        item.contains_undefined_continue_target(&[], &[JSString::from("y")]),
    )
}
#[test_case("while(a.#valid);" => true; "iteration statement valid")]
#[test_case("switch(a.#valid){}" => true; "switch statement valid")]
#[test_case("while(a.#invalid);" => false; "iteration statement invalid")]
#[test_case("switch(a.#invalid){}" => false; "switch statement invalid")]
fn breakable_statement_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = BreakableStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod breakable_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("while(package);", true => sset(&[PACKAGE_NOT_ALLOWED]); "IterationStatement")]
    #[test_case("switch(package){}", true => sset(&[PACKAGE_NOT_ALLOWED]); "SwitchStatement")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let agent = test_agent();
        let mut errs = vec![];
        BreakableStatement::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&agent, &mut errs, strict, false, false);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&agent, err.clone())))
    }

    #[test_case("for(;;)arguments;" => true; "Iteration (yes)")]
    #[test_case("switch (a) { case 1: arguments; }" => true; "Switch (yes)")]
    #[test_case("for(;;);" => false; "Iteration (no)")]
    #[test_case("switch (a) {}" => false; "Switch (no)")]
    fn contains_arguments(src: &str) -> bool {
        BreakableStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.contains_arguments()
    }

    #[test_case("for(;;)var d;" => svec(&["d"]); "iteration")]
    #[test_case("switch (x) { case 1: var p; break; }" => svec(&["p"]); "switch stmt")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).breakable_statement().var_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("   for(;;);" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 8 } }; "iteration")]
    #[test_case("   switch (x) { case 1: break; }" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 29 } }; "switch stmt")]
    fn location(src: &str) -> Location {
        Maker::new(src).breakable_statement().location()
    }
}
