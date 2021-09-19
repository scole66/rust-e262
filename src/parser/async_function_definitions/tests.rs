use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

// ASYNC FUNCTION DECLARATION
#[test]
fn async_function_declaration_test_01() {
    let (node, scanner) = check(AsyncFunctionDeclaration::parse(&mut newparser("async function bob(a,b) { return await foo(a+b); }"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 50);
    pretty_check(
        &*node,
        "AsyncFunctionDeclaration: async function bob ( a , b ) { return await foo ( a + b ) ; }",
        vec!["BindingIdentifier: bob", "FormalParameters: a , b", "AsyncFunctionBody: return await foo ( a + b ) ;"],
    );
    concise_check(
        &*node,
        "AsyncFunctionDeclaration: async function bob ( a , b ) { return await foo ( a + b ) ; }",
        vec![
            "Keyword: async",
            "Keyword: function",
            "IdentifierName: bob",
            "Punctuator: (",
            "FormalParameterList: a , b",
            "Punctuator: )",
            "Punctuator: {",
            "ReturnStatement: return await foo ( a + b ) ;",
            "Punctuator: }",
        ],
    );
    format!("{:?}", node);
}
#[test]
fn async_function_declaration_test_02() {
    let (node, scanner) = check(AsyncFunctionDeclaration::parse(&mut newparser("async function (a,b) { return await foo(a+b); }"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 47);
    pretty_check(
        &*node,
        "AsyncFunctionDeclaration: async function ( a , b ) { return await foo ( a + b ) ; }",
        vec!["FormalParameters: a , b", "AsyncFunctionBody: return await foo ( a + b ) ;"],
    );
    concise_check(
        &*node,
        "AsyncFunctionDeclaration: async function ( a , b ) { return await foo ( a + b ) ; }",
        vec![
            "Keyword: async",
            "Keyword: function",
            "Punctuator: (",
            "FormalParameterList: a , b",
            "Punctuator: )",
            "Punctuator: {",
            "ReturnStatement: return await foo ( a + b ) ;",
            "Punctuator: }",
        ],
    );
    format!("{:?}", node);
}
#[test]
fn async_function_declaration_test_err_01() {
    check_err(AsyncFunctionDeclaration::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘async’ expected", 1, 1);
}
#[test]
fn async_function_declaration_test_err_02() {
    check_err(AsyncFunctionDeclaration::parse(&mut newparser("async\n"), Scanner::new(), false, false, true), "Newline not allowed here.", 1, 6);
}
#[test]
fn async_function_declaration_test_err_03() {
    check_err(AsyncFunctionDeclaration::parse(&mut newparser("async bob"), Scanner::new(), false, false, true), "‘function’ expected", 1, 6);
}
#[test]
fn async_function_declaration_test_err_04() {
    check_err(AsyncFunctionDeclaration::parse(&mut newparser("async function"), Scanner::new(), false, false, false), "Not an identifier", 1, 15);
}
#[test]
fn async_function_declaration_test_err_05() {
    check_err(AsyncFunctionDeclaration::parse(&mut newparser("async function"), Scanner::new(), false, false, true), "‘(’ expected", 1, 15);
}
#[test]
fn async_function_declaration_test_err_06() {
    check_err(AsyncFunctionDeclaration::parse(&mut newparser("async function bob("), Scanner::new(), false, false, true), "‘)’ expected", 1, 20);
}
#[test]
fn async_function_declaration_test_err_07() {
    check_err(AsyncFunctionDeclaration::parse(&mut newparser("async function bob()"), Scanner::new(), false, false, true), "‘{’ expected", 1, 21);
}
#[test]
fn async_function_declaration_test_err_08() {
    check_err(AsyncFunctionDeclaration::parse(&mut newparser("async function bob() {"), Scanner::new(), false, false, true), "‘}’ expected", 1, 23);
}
#[test]
fn async_function_declaration_test_prettyerrors_1() {
    let (item, _) = AsyncFunctionDeclaration::parse(&mut newparser("async function bob(a, b) { return await foo(a+b); }"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_function_declaration_test_prettyerrors_2() {
    let (item, _) = AsyncFunctionDeclaration::parse(&mut newparser("async function (a, b) { return await foo(a+b); }"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_function_declaration_test_conciseerrors_1() {
    let (item, _) = AsyncFunctionDeclaration::parse(&mut newparser("async function bob(a, b) { return await foo(a+b); }"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_function_declaration_test_conciseerrors_2() {
    let (item, _) = AsyncFunctionDeclaration::parse(&mut newparser("async function (a, b) { return await foo(a+b); }"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_function_declaration_test_contains_01() {
    let (item, _) = AsyncFunctionDeclaration::parse(&mut newparser("async function bob() { return 11; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn async_function_declaration_test_contains_02() {
    let (item, _) = AsyncFunctionDeclaration::parse(&mut newparser("async function () { return 10; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn async_function_declaration_test_bound_names_01() {
    let (item, _) = AsyncFunctionDeclaration::parse(&mut newparser("async function a() { return 10; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), vec!["a"]);
}
#[test]
fn async_function_declaration_test_bound_names_02() {
    let (item, _) = AsyncFunctionDeclaration::parse(&mut newparser("async function () { return 10; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), vec!["*default*"]);
}

// ASYNC FUNCTION EXPRESSION
#[test]
fn async_function_expression_test_01() {
    let (node, scanner) = check(AsyncFunctionExpression::parse(&mut newparser("async function bob(a,b) { return await foo(a+b); }"), Scanner::new()));
    chk_scan(&scanner, 50);
    pretty_check(
        &*node,
        "AsyncFunctionExpression: async function bob ( a , b ) { return await foo ( a + b ) ; }",
        vec!["BindingIdentifier: bob", "FormalParameters: a , b", "AsyncFunctionBody: return await foo ( a + b ) ;"],
    );
    concise_check(
        &*node,
        "AsyncFunctionExpression: async function bob ( a , b ) { return await foo ( a + b ) ; }",
        vec![
            "Keyword: async",
            "Keyword: function",
            "IdentifierName: bob",
            "Punctuator: (",
            "FormalParameterList: a , b",
            "Punctuator: )",
            "Punctuator: {",
            "ReturnStatement: return await foo ( a + b ) ;",
            "Punctuator: }",
        ],
    );
    format!("{:?}", node);
    assert!(node.is_function_definition());
}
#[test]
fn async_function_expression_test_02() {
    let (node, scanner) = check(AsyncFunctionExpression::parse(&mut newparser("async function (a,b) { return await foo(a+b); }"), Scanner::new()));
    chk_scan(&scanner, 47);
    pretty_check(
        &*node,
        "AsyncFunctionExpression: async function ( a , b ) { return await foo ( a + b ) ; }",
        vec!["FormalParameters: a , b", "AsyncFunctionBody: return await foo ( a + b ) ;"],
    );
    concise_check(
        &*node,
        "AsyncFunctionExpression: async function ( a , b ) { return await foo ( a + b ) ; }",
        vec![
            "Keyword: async",
            "Keyword: function",
            "Punctuator: (",
            "FormalParameterList: a , b",
            "Punctuator: )",
            "Punctuator: {",
            "ReturnStatement: return await foo ( a + b ) ;",
            "Punctuator: }",
        ],
    );
    format!("{:?}", node);
    assert!(node.is_function_definition());
}
#[test]
fn async_function_expression_test_err_01() {
    check_err(AsyncFunctionExpression::parse(&mut newparser(""), Scanner::new()), "‘async’ expected", 1, 1);
}
#[test]
fn async_function_expression_test_err_02() {
    check_err(AsyncFunctionExpression::parse(&mut newparser("async\n"), Scanner::new()), "Newline not allowed here.", 1, 6);
}
#[test]
fn async_function_expression_test_err_03() {
    check_err(AsyncFunctionExpression::parse(&mut newparser("async bob"), Scanner::new()), "‘function’ expected", 1, 6);
}
#[test]
fn async_function_expression_test_err_05() {
    check_err(AsyncFunctionExpression::parse(&mut newparser("async function"), Scanner::new()), "‘(’ expected", 1, 15);
}
#[test]
fn async_function_expression_test_err_06() {
    check_err(AsyncFunctionExpression::parse(&mut newparser("async function bob("), Scanner::new()), "‘)’ expected", 1, 20);
}
#[test]
fn async_function_expression_test_err_07() {
    check_err(AsyncFunctionExpression::parse(&mut newparser("async function bob()"), Scanner::new()), "‘{’ expected", 1, 21);
}
#[test]
fn async_function_expression_test_err_08() {
    check_err(AsyncFunctionExpression::parse(&mut newparser("async function bob() {"), Scanner::new()), "‘}’ expected", 1, 23);
}
#[test]
fn async_function_expression_test_prettyerrors_1() {
    let (item, _) = AsyncFunctionExpression::parse(&mut newparser("async function bob(a, b) { return await foo(a+b); }"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_function_expression_test_prettyerrors_2() {
    let (item, _) = AsyncFunctionExpression::parse(&mut newparser("async function (a, b) { return await foo(a+b); }"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_function_expression_test_conciseerrors_1() {
    let (item, _) = AsyncFunctionExpression::parse(&mut newparser("async function bob(a, b) { return await foo(a+b); }"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_function_expression_test_conciseerrors_2() {
    let (item, _) = AsyncFunctionExpression::parse(&mut newparser("async function (a, b) { return await foo(a+b); }"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_function_expression_test_contains_01() {
    let (item, _) = AsyncFunctionExpression::parse(&mut newparser("async function bob() { return 11; }"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn async_function_expression_test_contains_02() {
    let (item, _) = AsyncFunctionExpression::parse(&mut newparser("async function () { return 10; }"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}

// ASYNC METHOD
#[test]
fn async_method_test_01() {
    let (node, scanner) = check(AsyncMethod::parse(&mut newparser("async bob(a,b) { return await foo(a+b); }"), Scanner::new(), false, false));
    chk_scan(&scanner, 41);
    pretty_check(
        &*node,
        "AsyncMethod: async bob ( a , b ) { return await foo ( a + b ) ; }",
        vec!["ClassElementName: bob", "UniqueFormalParameters: a , b", "AsyncFunctionBody: return await foo ( a + b ) ;"],
    );
    concise_check(
        &*node,
        "AsyncMethod: async bob ( a , b ) { return await foo ( a + b ) ; }",
        vec![
            "Keyword: async",
            "IdentifierName: bob",
            "Punctuator: (",
            "FormalParameterList: a , b",
            "Punctuator: )",
            "Punctuator: {",
            "ReturnStatement: return await foo ( a + b ) ;",
            "Punctuator: }",
        ],
    );
    format!("{:?}", node);
}
#[test]
fn async_method_test_err_01() {
    check_err(AsyncMethod::parse(&mut newparser(""), Scanner::new(), false, false), "‘async’ expected", 1, 1);
}
#[test]
fn async_method_test_err_02() {
    check_err(AsyncMethod::parse(&mut newparser("async\n"), Scanner::new(), false, false), "Newline not allowed here.", 1, 6);
}
#[test]
fn async_method_test_err_03() {
    check_err(AsyncMethod::parse(&mut newparser("async"), Scanner::new(), false, false), "ClassElementName expected", 1, 6);
}
#[test]
fn async_method_test_err_04() {
    check_err(AsyncMethod::parse(&mut newparser("async bob"), Scanner::new(), false, false), "‘(’ expected", 1, 10);
}
#[test]
fn async_method_test_err_06() {
    check_err(AsyncMethod::parse(&mut newparser("async bob("), Scanner::new(), false, false), "‘)’ expected", 1, 11);
}
#[test]
fn async_method_test_err_07() {
    check_err(AsyncMethod::parse(&mut newparser("async bob()"), Scanner::new(), false, false), "‘{’ expected", 1, 12);
}
#[test]
fn async_method_test_err_08() {
    check_err(AsyncMethod::parse(&mut newparser("async bob() {"), Scanner::new(), false, false), "‘}’ expected", 1, 14);
}
#[test]
fn async_method_test_prettyerrors_1() {
    let (item, _) = AsyncMethod::parse(&mut newparser("async bob(a, b) { return await foo(a+b); }"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_method_test_conciseerrors_1() {
    let (item, _) = AsyncMethod::parse(&mut newparser("async bob(a, b) { return await foo(a+b); }"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_method_test_contains_01() {
    let (item, _) = AsyncMethod::parse(&mut newparser("async [10](b) { ; }"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn async_method_test_contains_02() {
    let (item, _) = AsyncMethod::parse(&mut newparser("async a(b=10) { ; }"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn async_method_test_contains_03() {
    let (item, _) = AsyncMethod::parse(&mut newparser("async a(b) { 10; }"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn async_method_test_contains_04() {
    let (item, _) = AsyncMethod::parse(&mut newparser("async a(b) { ; }"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn async_method_test_computed_property_contains_01() {
    let (item, _) = AsyncMethod::parse(&mut newparser("async [19]() {}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn async_method_test_computed_property_contains_02() {
    let (item, _) = AsyncMethod::parse(&mut newparser("async [name]() {}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test]
fn async_method_test_private_bound_identifiers() {
    let (item, _) = AsyncMethod::parse(&mut newparser("async #blue() {}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.private_bound_identifiers(), vec![JSString::from("blue")]);
}

// ASYNC FUNCTION BODY
#[test]
fn async_function_body_test_01() {
    let (node, scanner) = AsyncFunctionBody::parse(&mut newparser("yield = 3;"), Scanner::new());
    chk_scan(&scanner, 10);
    pretty_check(&*node, "AsyncFunctionBody: yield = 3 ;", vec!["FunctionBody: yield = 3 ;"]);
    concise_check(&*node, "ExpressionStatement: yield = 3 ;", vec!["AssignmentExpression: yield = 3", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn async_function_body_test_02() {
    let (node, scanner) = AsyncFunctionBody::parse(&mut newparser("await = 3;"), Scanner::new());
    chk_scan(&scanner, 0);
    pretty_check(&*node, "AsyncFunctionBody: ", vec!["FunctionBody: "]);
    concise_check(&*node, "", vec![]);
    format!("{:?}", node);
}
#[test]
fn async_function_body_test_cache_01() {
    let mut parser = newparser("blue(67); orange(30);");
    let (node, scanner) = AsyncFunctionBody::parse(&mut parser, Scanner::new());
    let (node2, scanner2) = AsyncFunctionBody::parse(&mut parser, Scanner::new());
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn async_function_body_test_prettyerrors_1() {
    let (item, _) = AsyncFunctionBody::parse(&mut newparser("bananas;"), Scanner::new());
    pretty_error_validate(&*item);
}
#[test]
fn async_function_body_test_conciseerrors_1() {
    let (item, _) = AsyncFunctionBody::parse(&mut newparser("bananas;"), Scanner::new());
    concise_error_validate(&*item);
}
#[test]
fn async_function_body_test_contains_01() {
    let (item, _) = AsyncFunctionBody::parse(&mut newparser("10;"), Scanner::new());
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn async_function_body_test_contains_02() {
    let (item, _) = AsyncFunctionBody::parse(&mut newparser("return;"), Scanner::new());
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}

// AWAIT EXPRESSION
#[test]
fn await_expression_test_01() {
    let (node, scanner) = check(AwaitExpression::parse(&mut newparser("await a()"), Scanner::new(), false));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "AwaitExpression: await a ( )", vec!["UnaryExpression: a ( )"]);
    concise_check(&*node, "AwaitExpression: await a ( )", vec!["Keyword: await", "CallMemberExpression: a ( )"]);
    format!("{:?}", node);
}
#[test]
fn await_expression_test_err_01() {
    check_err(AwaitExpression::parse(&mut newparser(""), Scanner::new(), false), "‘await’ expected", 1, 1);
}
#[test]
fn await_expression_test_err_02() {
    check_err(AwaitExpression::parse(&mut newparser("await"), Scanner::new(), false), "UnaryExpression expected", 1, 6);
}
#[test]
fn await_expression_test_prettyerrors_1() {
    let (item, _) = AwaitExpression::parse(&mut newparser("await a()"), Scanner::new(), false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn await_expression_test_conciseerrors_1() {
    let (item, _) = AwaitExpression::parse(&mut newparser("await a()"), Scanner::new(), false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn await_expression_test_contains_01() {
    let (item, _) = AwaitExpression::parse(&mut newparser("await 10"), Scanner::new(), true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn await_expression_test_contains_02() {
    let (item, _) = AwaitExpression::parse(&mut newparser("await a"), Scanner::new(), true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
