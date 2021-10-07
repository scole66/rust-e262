use super::scanner::StringDelimiter;
use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use test_case::test_case;

// FUNCTION DECLARATION
#[test]
fn function_declaration_test_01() {
    let (node, scanner) = check(FunctionDeclaration::parse(&mut newparser("function bob(a,b) { return foo(a+b); }"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 38);
    pretty_check(
        &*node,
        "FunctionDeclaration: function bob ( a , b ) { return foo ( a + b ) ; }",
        vec!["BindingIdentifier: bob", "FormalParameters: a , b", "FunctionBody: return foo ( a + b ) ;"],
    );
    concise_check(
        &*node,
        "FunctionDeclaration: function bob ( a , b ) { return foo ( a + b ) ; }",
        vec![
            "Keyword: function",
            "IdentifierName: bob",
            "Punctuator: (",
            "FormalParameterList: a , b",
            "Punctuator: )",
            "Punctuator: {",
            "ReturnStatement: return foo ( a + b ) ;",
            "Punctuator: }",
        ],
    );
    format!("{:?}", node);
}
#[test]
fn function_declaration_test_02() {
    let (node, scanner) = check(FunctionDeclaration::parse(&mut newparser("function (z) {}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 15);
    pretty_check(&*node, "FunctionDeclaration: function ( z ) {  }", vec!["FormalParameters: z", "FunctionBody: "]);
    concise_check(&*node, "FunctionDeclaration: function ( z ) {  }", vec!["Keyword: function", "Punctuator: (", "IdentifierName: z", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn function_declaration_test_err_01() {
    check_err(FunctionDeclaration::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘function’ expected", 1, 1);
}
#[test]
fn function_declaration_test_err_02() {
    check_err(FunctionDeclaration::parse(&mut newparser("function"), Scanner::new(), false, false, true), "‘(’ expected", 1, 9);
}
#[test]
fn function_declaration_test_err_03() {
    check_err(FunctionDeclaration::parse(&mut newparser("function (z)"), Scanner::new(), false, false, false), "Not an identifier", 1, 9);
}
#[test]
fn function_declaration_test_err_04() {
    check_err(FunctionDeclaration::parse(&mut newparser("function foo"), Scanner::new(), false, false, true), "‘(’ expected", 1, 13);
}
#[test]
fn function_declaration_test_err_05() {
    check_err(FunctionDeclaration::parse(&mut newparser("function foo("), Scanner::new(), false, false, true), "‘)’ expected", 1, 14);
}
#[test]
fn function_declaration_test_err_06() {
    check_err(FunctionDeclaration::parse(&mut newparser("function foo()"), Scanner::new(), false, false, true), "‘{’ expected", 1, 15);
}
#[test]
fn function_declaration_test_err_07() {
    check_err(FunctionDeclaration::parse(&mut newparser("function foo(){"), Scanner::new(), false, false, true), "‘}’ expected", 1, 16);
}
#[test]
fn function_declaration_test_prettyerrors_1() {
    let (item, _) = FunctionDeclaration::parse(&mut newparser("function bob(a, b) { return foo(a+b); }"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn function_declaration_test_prettyerrors_2() {
    let (item, _) = FunctionDeclaration::parse(&mut newparser("function (a, b) { return foo(a+b); }"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn function_declaration_test_conciseerrors_1() {
    let (item, _) = FunctionDeclaration::parse(&mut newparser("function bob(a, b) { return foo(a+b); }"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn function_declaration_test_conciseerrors_2() {
    let (item, _) = FunctionDeclaration::parse(&mut newparser("function (a, b) { return foo(a+b); }"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn function_declaration_test_cache_01() {
    let mut parser = newparser("function f() {}");
    let (node, scanner) = check(FunctionDeclaration::parse(&mut parser, Scanner::new(), false, false, false));
    let (node2, scanner2) = check(FunctionDeclaration::parse(&mut parser, Scanner::new(), false, false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn function_declaration_test_bound_names_01() {
    let (item, _) = FunctionDeclaration::parse(&mut newparser("function a(){}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn function_declaration_test_bound_names_02() {
    let (item, _) = FunctionDeclaration::parse(&mut newparser("function (){}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["*default*"]);
}
#[test]
fn function_declaration_test_contains_01() {
    let (item, _) = FunctionDeclaration::parse(&mut newparser("function a(b=0){0;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("function a(a=b.#valid){}" => true; "Params valid")]
#[test_case("function a(b){c.#valid;}" => true; "Body valid")]
#[test_case("function a(a=b.#invalid){}" => false; "Params invalid")]
#[test_case("function a(b){c.#invalid;}" => false; "Body invalid")]
fn function_declaration_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = FunctionDeclaration::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}

// FUNCTION EXPRESSION
#[test]
fn function_expression_test_01() {
    let (node, scanner) = check(FunctionExpression::parse(&mut newparser("function bob(a,b) { return foo(a+b); }"), Scanner::new()));
    chk_scan(&scanner, 38);
    pretty_check(
        &*node,
        "FunctionExpression: function bob ( a , b ) { return foo ( a + b ) ; }",
        vec!["BindingIdentifier: bob", "FormalParameters: a , b", "FunctionBody: return foo ( a + b ) ;"],
    );
    concise_check(
        &*node,
        "FunctionExpression: function bob ( a , b ) { return foo ( a + b ) ; }",
        vec![
            "Keyword: function",
            "IdentifierName: bob",
            "Punctuator: (",
            "FormalParameterList: a , b",
            "Punctuator: )",
            "Punctuator: {",
            "ReturnStatement: return foo ( a + b ) ;",
            "Punctuator: }",
        ],
    );
    format!("{:?}", node);
    assert!(node.is_function_definition());
}
#[test]
fn function_expression_test_02() {
    let (node, scanner) = check(FunctionExpression::parse(&mut newparser("function (z) {}"), Scanner::new()));
    chk_scan(&scanner, 15);
    pretty_check(&*node, "FunctionExpression: function ( z ) {  }", vec!["FormalParameters: z", "FunctionBody: "]);
    concise_check(&*node, "FunctionExpression: function ( z ) {  }", vec!["Keyword: function", "Punctuator: (", "IdentifierName: z", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
    format!("{:?}", node);
    assert!(node.is_function_definition());
}
#[test]
fn function_expression_test_err_01() {
    check_err(FunctionExpression::parse(&mut newparser(""), Scanner::new()), "‘function’ expected", 1, 1);
}
#[test]
fn function_expression_test_err_02() {
    check_err(FunctionExpression::parse(&mut newparser("function"), Scanner::new()), "‘(’ expected", 1, 9);
}
#[test]
fn function_expression_test_err_04() {
    check_err(FunctionExpression::parse(&mut newparser("function foo"), Scanner::new()), "‘(’ expected", 1, 13);
}
#[test]
fn function_expression_test_err_05() {
    check_err(FunctionExpression::parse(&mut newparser("function foo("), Scanner::new()), "‘)’ expected", 1, 14);
}
#[test]
fn function_expression_test_err_06() {
    check_err(FunctionExpression::parse(&mut newparser("function foo()"), Scanner::new()), "‘{’ expected", 1, 15);
}
#[test]
fn function_expression_test_err_07() {
    check_err(FunctionExpression::parse(&mut newparser("function foo(){"), Scanner::new()), "‘}’ expected", 1, 16);
}
#[test]
fn function_expression_test_prettyerrors_1() {
    let (item, _) = FunctionExpression::parse(&mut newparser("function bob(a, b) { return foo(a+b); }"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn function_expression_test_prettyerrors_2() {
    let (item, _) = FunctionExpression::parse(&mut newparser("function (a, b) { return foo(a+b); }"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn function_expression_test_conciseerrors_1() {
    let (item, _) = FunctionExpression::parse(&mut newparser("function bob(a, b) { return foo(a+b); }"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn function_expression_test_conciseerrors_2() {
    let (item, _) = FunctionExpression::parse(&mut newparser("function (a, b) { return foo(a+b); }"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn function_expression_test_contains_01() {
    let (item, _) = FunctionExpression::parse(&mut newparser("function bob(a=0) { 0; }"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("function a(a=b.#valid){}" => true; "Params valid")]
#[test_case("function a(b){c.#valid;}" => true; "Body valid")]
#[test_case("function a(a=b.#invalid){}" => false; "Params invalid")]
#[test_case("function a(b){c.#invalid;}" => false; "Body invalid")]
fn function_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = FunctionExpression::parse(&mut newparser(src), Scanner::new()).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}

// FUNCTION BODY
#[test]
fn function_body_test_01() {
    let (node, scanner) = FunctionBody::parse(&mut newparser(""), Scanner::new(), false, false);
    chk_scan(&scanner, 0);
    pretty_check(&*node, "FunctionBody: ", vec!["FunctionStatementList: "]);
    concise_check(&*node, "", vec![]);
    format!("{:?}", node);
}
#[test]
fn function_body_test_prettyerrors_1() {
    let (item, _) = FunctionBody::parse(&mut newparser("a; b; c;"), Scanner::new(), false, false);
    pretty_error_validate(&*item);
}
#[test]
fn function_body_test_conciseeerrors_1() {
    let (item, _) = FunctionBody::parse(&mut newparser("a; b; c;"), Scanner::new(), false, false);
    concise_error_validate(&*item);
}
#[test]
fn function_body_test_cache_01() {
    let mut parser = newparser("a; b; c;");
    let (node, scanner) = FunctionBody::parse(&mut parser, Scanner::new(), false, false);
    let (node2, scanner2) = FunctionBody::parse(&mut parser, Scanner::new(), false, false);
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn function_body_test_contains_01() {
    let (item, _) = FunctionBody::parse(&mut newparser("0;"), Scanner::new(), true, true);
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn function_body_test_contains_02() {
    let (item, _) = FunctionBody::parse(&mut newparser("a;"), Scanner::new(), true, true);
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("a.#valid;" => true; "Statement valid")]
#[test_case("a.#invalid;" => false; "Statement invalid")]
fn function_body_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = FunctionBody::parse(&mut newparser(src), Scanner::new(), true, true);
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
#[test_case("'one'; 'two'; 'three'; blue;" => vec![JSString::from("one"), JSString::from("two"), JSString::from("three")]; "normal")]
fn function_body_test_directive_prologue(src: &str) -> Vec<JSString> {
    let (item, _) = FunctionBody::parse(&mut newparser(src), Scanner::new(), true, true);
    item.directive_prologue().into_iter().map(|tok| tok.value).collect()
}
#[test_case("'one'; 3;" => false; "directive no strict")]
#[test_case("12;" => false; "no prologue")]
#[test_case("'a'; 'use strict';" => true; "good strict")]
#[test_case("'use\\x20strict';" => false; "bad strict")]
fn function_body_test_function_body_contains_use_strict(src: &str) -> bool {
    let (item, _) = FunctionBody::parse(&mut newparser(src), Scanner::new(), true, true);
    item.function_body_contains_use_strict()
}

// FUNCTION STATEMENT LIST
#[test]
fn function_statement_list_test_01() {
    let (node, scanner) = FunctionStatementList::parse(&mut newparser(""), Scanner::new(), false, false);
    chk_scan(&scanner, 0);
    pretty_check(&*node, "FunctionStatementList: ", vec![]);
    concise_check(&*node, "", vec![]);
    format!("{:?}", node);
}
#[test]
fn function_statement_list_test_prettyerrors_1() {
    let (item, _) = FunctionStatementList::parse(&mut newparser("a; b; c;"), Scanner::new(), false, false);
    pretty_error_validate(&*item);
}
#[test]
fn function_statement_list_test_prettyerrors_2() {
    let (item, _) = FunctionStatementList::parse(&mut newparser(""), Scanner::new(), false, false);
    pretty_error_validate(&*item);
}
#[test]
fn function_statement_list_test_conciseeerrors_1() {
    let (item, _) = FunctionStatementList::parse(&mut newparser("a; b; c;"), Scanner::new(), false, false);
    concise_error_validate(&*item);
}
#[test]
fn function_statement_list_test_conciseerrors_2() {
    let (item, _) = FunctionStatementList::parse(&mut newparser(""), Scanner::new(), false, false);
    concise_error_validate(&*item);
}
#[test]
fn function_statement_list_test_contains_01() {
    let (item, _) = FunctionStatementList::parse(&mut newparser(""), Scanner::new(), true, true);
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn function_statement_list_test_contains_02() {
    let (item, _) = FunctionStatementList::parse(&mut newparser("0;"), Scanner::new(), true, true);
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn function_statement_list_test_contains_03() {
    let (item, _) = FunctionStatementList::parse(&mut newparser("a;"), Scanner::new(), true, true);
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("" => true; "empty")]
#[test_case("a.#valid;" => true; "statement valid")]
#[test_case("a.#invalid;" => false; "statement invalid")]
fn function_statement_list_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = FunctionStatementList::parse(&mut newparser(src), Scanner::new(), true, true);
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
#[test_case("" => Vec::<StringToken>::new(); "empty")]
#[test_case("\"use strict\";" => vec![StringToken { value: JSString::from("use strict"), delimiter: StringDelimiter::Double, raw: None }]; "list")]
fn function_statement_list_test_initial_string_tokens(src: &str) -> Vec<StringToken> {
    let (item, _) = FunctionStatementList::parse(&mut newparser(src), Scanner::new(), true, true);
    item.initial_string_tokens()
}
