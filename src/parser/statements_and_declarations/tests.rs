use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

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
    concise_check(&*node, "VariableStatement: var a = 0 ;", vec!["Keyword: var", "VariableDeclaration: a = 0", "Punctuator: ;"]);
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
    concise_check(&*node, "IfStatement: if ( a ) { }", vec!["Keyword: if", "Punctuator: (", "IdentifierName: a", "Punctuator: )", "Block: { }"]);
    format!("{:?}", node);
}
#[test]
fn statement_test_06() {
    let (node, scanner) = check(Statement::parse(&mut newparser("switch (a) {}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 13);
    pretty_check(&*node, "Statement: switch ( a ) { }", vec!["BreakableStatement: switch ( a ) { }"]);
    concise_check(&*node, "SwitchStatement: switch ( a ) { }", vec!["Keyword: switch", "Punctuator: (", "IdentifierName: a", "Punctuator: )", "CaseBlock: { }"]);
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
    concise_check(&*node, "WithStatement: with ( a ) b ;", vec!["Keyword: with", "Punctuator: (", "IdentifierName: a", "Punctuator: )", "ExpressionStatement: b ;"]);
    format!("{:?}", node);
}
#[test]
fn statement_test_11() {
    let (node, scanner) = check(Statement::parse(&mut newparser("a:b;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 4);
    pretty_check(&*node, "Statement: a : b ;", vec!["LabelledStatement: a : b ;"]);
    concise_check(&*node, "LabelledStatement: a : b ;", vec!["IdentifierName: a", "Punctuator: :", "ExpressionStatement: b ;"]);
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
    let (node, scanner) = check(Statement::parse(&mut newparser("try {} catch {}"), Scanner::new(), false, false, true));
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
    check_err(Statement::parse(&mut newparser("return;"), Scanner::new(), false, false, false), "Statement expected", 1, 1);
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

// DECLARATION
#[test]
fn declaration_test_01() {
    let (node, scanner) = check(Declaration::parse(&mut newparser("function a(){}"), Scanner::new(), false, false));
    chk_scan(&scanner, 14);
    pretty_check(&*node, "Declaration: function a (  ) {  }", vec!["HoistableDeclaration: function a (  ) {  }"]);
    concise_check(&*node, "FunctionDeclaration: function a (  ) {  }", vec!["Keyword: function", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn declaration_test_02() {
    let (node, scanner) = check(Declaration::parse(&mut newparser("class a{}"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "Declaration: class a { }", vec!["ClassDeclaration: class a { }"]);
    concise_check(&*node, "ClassDeclaration: class a { }", vec!["Keyword: class", "IdentifierName: a", "ClassTail: { }"]);
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

// HOISTABLE DECLARATION
#[test]
fn hoistable_declaration_test_01() {
    let (node, scanner) = check(HoistableDeclaration::parse(&mut newparser("function a(){}"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 14);
    pretty_check(&*node, "HoistableDeclaration: function a (  ) {  }", vec!["FunctionDeclaration: function a (  ) {  }"]);
    concise_check(&*node, "FunctionDeclaration: function a (  ) {  }", vec!["Keyword: function", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn hoistable_declaration_test_02() {
    let (node, scanner) = check(HoistableDeclaration::parse(&mut newparser("function *a(){}"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 15);
    pretty_check(&*node, "HoistableDeclaration: function * a (  ) {  }", vec!["GeneratorDeclaration: function * a (  ) {  }"]);
    concise_check(
        &*node,
        "GeneratorDeclaration: function * a (  ) {  }",
        vec!["Keyword: function", "Punctuator: *", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
    );
    format!("{:?}", node);
}
#[test]
fn hoistable_declaration_test_03() {
    let (node, scanner) = check(HoistableDeclaration::parse(&mut newparser("async function a(){}"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 20);
    pretty_check(&*node, "HoistableDeclaration: async function a (  ) {  }", vec!["AsyncFunctionDeclaration: async function a (  ) {  }"]);
    concise_check(
        &*node,
        "AsyncFunctionDeclaration: async function a (  ) {  }",
        vec!["Keyword: async", "Keyword: function", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
    );
    format!("{:?}", node);
}
#[test]
fn hoistable_declaration_test_04() {
    let (node, scanner) = check(HoistableDeclaration::parse(&mut newparser("async function *a(){}"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 21);
    pretty_check(&*node, "HoistableDeclaration: async function * a (  ) {  }", vec!["AsyncGeneratorDeclaration: async function * a (  ) {  }"]);
    concise_check(
        &*node,
        "AsyncGeneratorDeclaration: async function * a (  ) {  }",
        vec!["Keyword: async", "Keyword: function", "Punctuator: *", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
    );
    format!("{:?}", node);
}
#[test]
fn hoistable_declaration_test_err_01() {
    check_err(HoistableDeclaration::parse(&mut newparser(""), Scanner::new(), false, false, false), "HoistableDeclaration expected", 1, 1);
}
#[test]
fn hoistable_declaration_test_prettyerrors_1() {
    let (item, _) = HoistableDeclaration::parse(&mut newparser("function a(){}"), Scanner::new(), false, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn hoistable_declaration_test_prettyerrors_2() {
    let (item, _) = HoistableDeclaration::parse(&mut newparser("function *a(){}"), Scanner::new(), false, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn hoistable_declaration_test_prettyerrors_3() {
    let (item, _) = HoistableDeclaration::parse(&mut newparser("async function a(){}"), Scanner::new(), false, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn hoistable_declaration_test_prettyerrors_4() {
    let (item, _) = HoistableDeclaration::parse(&mut newparser("async function *a(){}"), Scanner::new(), false, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn hoistable_declaration_test_conciseerrors_1() {
    let (item, _) = HoistableDeclaration::parse(&mut newparser("function a(){}"), Scanner::new(), false, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn hoistable_declaration_test_conciseerrors_2() {
    let (item, _) = HoistableDeclaration::parse(&mut newparser("function *a(){}"), Scanner::new(), false, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn hoistable_declaration_test_conciseerrors_3() {
    let (item, _) = HoistableDeclaration::parse(&mut newparser("async function a(){}"), Scanner::new(), false, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn hoistable_declaration_test_conciseerrors_4() {
    let (item, _) = HoistableDeclaration::parse(&mut newparser("async function *a(){}"), Scanner::new(), false, false, false).unwrap();
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

// BREAKABLE STATEMENT
#[test]
fn breakable_statement_test_01() {
    let (node, scanner) = check(BreakableStatement::parse(&mut newparser("while (a);"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 10);
    pretty_check(&*node, "BreakableStatement: while ( a ) ;", vec!["IterationStatement: while ( a ) ;"]);
    concise_check(&*node, "WhileStatement: while ( a ) ;", vec!["Keyword: while", "Punctuator: (", "IdentifierName: a", "Punctuator: )", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn breakable_statement_test_02() {
    let (node, scanner) = check(BreakableStatement::parse(&mut newparser("switch(a){}"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 11);
    pretty_check(&*node, "BreakableStatement: switch ( a ) { }", vec!["SwitchStatement: switch ( a ) { }"]);
    concise_check(&*node, "SwitchStatement: switch ( a ) { }", vec!["Keyword: switch", "Punctuator: (", "IdentifierName: a", "Punctuator: )", "CaseBlock: { }"]);
    format!("{:?}", node);
}
#[test]
fn breable_statement_test_err_01() {
    check_err(BreakableStatement::parse(&mut newparser(""), Scanner::new(), false, false, false), "BreakableStatement expected", 1, 1);
}
#[test]
fn breakable_statement_test_prettyerrors_1() {
    let (item, _) = BreakableStatement::parse(&mut newparser("while(a);"), Scanner::new(), false, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn breakable_statement_test_prettyerrors_2() {
    let (item, _) = BreakableStatement::parse(&mut newparser("switch(a){}"), Scanner::new(), false, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn breakable_statement_test_conciseerrors_1() {
    let (item, _) = BreakableStatement::parse(&mut newparser("while(a);"), Scanner::new(), false, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn breakable_statement_test_conciseerrors_2() {
    let (item, _) = BreakableStatement::parse(&mut newparser("switch(a){}"), Scanner::new(), false, false, false).unwrap();
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
