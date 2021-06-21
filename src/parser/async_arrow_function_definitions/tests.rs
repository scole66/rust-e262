use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

// ASYNC ARROW FUNCTION
#[test]
fn async_arrow_function_test_01() {
    let (node, scanner) = check(AsyncArrowFunction::parse(&mut newparser("async a=>a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 10);
    assert!(matches!(&*node, AsyncArrowFunction::IdentOnly(..)));
    pretty_check(&*node, "AsyncArrowFunction: async a => a", vec!["AsyncArrowBindingIdentifier: a", "AsyncConciseBody: a"]);
    concise_check(&*node, "AsyncArrowFunction: async a => a", vec!["Keyword: async", "IdentifierName: a", "Punctuator: =>", "IdentifierName: a"]);
    format!("{:?}", node);
}
#[test]
fn async_arrow_function_test_02() {
    let (node, scanner) = check(AsyncArrowFunction::parse(&mut newparser("async (a,b)=>a+b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 16);
    assert!(matches!(&*node, AsyncArrowFunction::Formals(..)));
    pretty_check(&*node, "AsyncArrowFunction: async ( a , b ) => a + b", vec!["AsyncArrowHead: async ( a , b )", "AsyncConciseBody: a + b"]);
    concise_check(&*node, "AsyncArrowFunction: async ( a , b ) => a + b", vec!["AsyncArrowHead: async ( a , b )", "Punctuator: =>", "AdditiveExpression: a + b"]);
    format!("{:?}", node);
}
#[test]
fn async_arrow_function_test_err_01() {
    check_err(AsyncArrowFunction::parse(&mut newparser(""), Scanner::new(), true, false, false), "‘async’ expected", 1, 1);
}
#[test]
fn async_arrow_function_test_err_02() {
    check_err(AsyncArrowFunction::parse(&mut newparser("async\n"), Scanner::new(), true, false, false), "Newline not allowed here.", 1, 6);
}
#[test]
fn async_arrow_function_test_err_03() {
    check_err(AsyncArrowFunction::parse(&mut newparser("async"), Scanner::new(), true, false, false), "Not an identifier", 1, 6);
}
#[test]
fn async_arrow_function_test_err_04() {
    check_err(AsyncArrowFunction::parse(&mut newparser("async a\n"), Scanner::new(), true, false, false), "Newline not allowed here.", 1, 8);
}
#[test]
fn async_arrow_function_test_err_05() {
    check_err(AsyncArrowFunction::parse(&mut newparser("async a"), Scanner::new(), true, false, false), "‘=>’ expected", 1, 8);
}
#[test]
fn async_arrow_function_test_err_06() {
    check_err(AsyncArrowFunction::parse(&mut newparser("async a=>"), Scanner::new(), true, false, false), "AsyncConciseBody expected", 1, 10);
}
#[test]
fn async_arrow_function_test_err_07() {
    check_err(AsyncArrowFunction::parse(&mut newparser("async ("), Scanner::new(), true, false, false), "‘)’ expected", 1, 8);
}
#[test]
fn async_arrow_function_test_err_08() {
    check_err(AsyncArrowFunction::parse(&mut newparser("async (5)"), Scanner::new(), true, false, false), "‘)’ expected", 1, 8);
}
#[test]
fn async_arrow_function_test_err_09() {
    check_err(AsyncArrowFunction::parse(&mut newparser("blue (5)"), Scanner::new(), true, false, false), "‘async’ expected", 1, 1);
}
#[test]
fn async_arrow_function_test_err_10() {
    check_err(AsyncArrowFunction::parse(&mut newparser("async (a)\n"), Scanner::new(), true, false, false), "Newline not allowed here.", 1, 10);
}
#[test]
fn async_arrow_function_test_err_11() {
    check_err(AsyncArrowFunction::parse(&mut newparser("async (a)"), Scanner::new(), true, false, false), "‘=>’ expected", 1, 10);
}
#[test]
fn async_arrow_function_test_err_12() {
    check_err(AsyncArrowFunction::parse(&mut newparser("async (a)=>"), Scanner::new(), true, false, false), "AsyncConciseBody expected", 1, 12);
}
#[test]
fn async_arrow_function_test_prettyerrors_1() {
    let (item, _) = AsyncArrowFunction::parse(&mut newparser("async a=>a"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_arrow_function_test_prettyerrors_2() {
    let (item, _) = AsyncArrowFunction::parse(&mut newparser("async (a,b)=>a+b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_arrow_function_test_conciseerrors_1() {
    let (item, _) = AsyncArrowFunction::parse(&mut newparser("async a=>a"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_arrow_function_test_conciseerrors_2() {
    let (item, _) = AsyncArrowFunction::parse(&mut newparser("async (a,b)=>a+b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_arrow_function_test_contains_01() {
    let (item, _) = AsyncArrowFunction::parse(&mut newparser("async bob => this"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn async_arrow_function_test_contains_02() {
    let (item, _) = AsyncArrowFunction::parse(&mut newparser("async bob => a"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn async_arrow_function_test_contains_03() {
    let (item, _) = AsyncArrowFunction::parse(&mut newparser("async (b=this) => a"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn async_arrow_function_test_contains_04() {
    let (item, _) = AsyncArrowFunction::parse(&mut newparser("async (b) => this"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn async_arrow_function_test_contains_05() {
    let (item, _) = AsyncArrowFunction::parse(&mut newparser("async (b) => b"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn async_arrow_function_test_contains_06() {
    let (item, _) = AsyncArrowFunction::parse(&mut newparser("async (b) => new.target"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::NewTarget), true);
}
#[test]
fn async_arrow_function_test_contains_07() {
    let (item, _) = AsyncArrowFunction::parse(&mut newparser("async (b) => super.x"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::SuperProperty), true);
}
#[test]
fn async_arrow_function_test_contains_08() {
    let (item, _) = AsyncArrowFunction::parse(&mut newparser("async (b) => super()"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::SuperCall), true);
}
#[test]
fn async_arrow_function_test_contains_09() {
    let (item, _) = AsyncArrowFunction::parse(&mut newparser("async (b) => super()"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Super), true);
}
#[test]
fn async_arrow_function_test_contains_10() {
    let (item, _) = AsyncArrowFunction::parse(&mut newparser("async (b) => 2048"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}

// ASYNC CONCISE BODY
#[test]
fn async_concise_body_test_01() {
    let (node, scanner) = check(AsyncConciseBody::parse(&mut newparser("a"), Scanner::new(), true));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, AsyncConciseBody::Expression(..)));
    pretty_check(&*node, "AsyncConciseBody: a", vec!["ExpressionBody: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
}
#[test]
fn async_concise_body_test_02() {
    let (node, scanner) = check(AsyncConciseBody::parse(&mut newparser("{a;}"), Scanner::new(), true));
    chk_scan(&scanner, 4);
    assert!(matches!(&*node, AsyncConciseBody::Function(..)));
    pretty_check(&*node, "AsyncConciseBody: { a ; }", vec!["AsyncFunctionBody: a ;"]);
    concise_check(&*node, "AsyncConciseBody: { a ; }", vec!["Punctuator: {", "ExpressionStatement: a ;", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn async_concise_body_test_err_01() {
    check_err(AsyncConciseBody::parse(&mut newparser(""), Scanner::new(), true), "AsyncConciseBody expected", 1, 1);
}
#[test]
fn async_concise_body_test_err_02() {
    check_err(AsyncConciseBody::parse(&mut newparser("{"), Scanner::new(), true), "‘}’ expected", 1, 2);
}
#[test]
fn async_concise_body_test_prettyerrors_1() {
    let (item, _) = AsyncConciseBody::parse(&mut newparser("expression"), Scanner::new(), true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_concise_body_test_prettyerrors_2() {
    let (item, _) = AsyncConciseBody::parse(&mut newparser("{ statement_list; }"), Scanner::new(), true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_concise_body_test_conciseerrors_1() {
    let (item, _) = AsyncConciseBody::parse(&mut newparser("expression"), Scanner::new(), true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_concise_body_test_conciseerrors_2() {
    let (item, _) = AsyncConciseBody::parse(&mut newparser("{ statement_list; }"), Scanner::new(), true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_concise_body_test_contains_01() {
    let (item, _) = AsyncConciseBody::parse(&mut newparser("3"), Scanner::new(), true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn async_concise_body_test_contains_02() {
    let (item, _) = AsyncConciseBody::parse(&mut newparser("a"), Scanner::new(), true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn async_concise_body_test_contains_03() {
    let (item, _) = AsyncConciseBody::parse(&mut newparser("{ return 3; }"), Scanner::new(), true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn async_concise_body_test_contains_04() {
    let (item, _) = AsyncConciseBody::parse(&mut newparser("{ return a; }"), Scanner::new(), true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}

// ASYNC ARROW BINDING IDENTIFIER
#[test]
fn async_arrow_binding_identifier_test_01() {
    let (node, scanner) = check(AsyncArrowBindingIdentifier::parse(&mut newparser("a"), Scanner::new(), false));
    chk_scan(&scanner, 1);
    pretty_check(&*node, "AsyncArrowBindingIdentifier: a", vec!["BindingIdentifier: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
}
#[test]
fn async_arrow_binding_identifier_test_err_01() {
    check_err(AsyncArrowBindingIdentifier::parse(&mut newparser(""), Scanner::new(), false), "Not an identifier", 1, 1);
}
#[test]
fn async_arrow_binding_identifier_test_prettyerrors_1() {
    let (item, _) = AsyncArrowBindingIdentifier::parse(&mut newparser("identifier"), Scanner::new(), false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_arrow_binding_identifier_test_conciseerrors_1() {
    let (item, _) = AsyncArrowBindingIdentifier::parse(&mut newparser("identifier"), Scanner::new(), false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_arrow_binding_identifier_test_contains_01() {
    let (item, _) = AsyncArrowBindingIdentifier::parse(&mut newparser("identifier"), Scanner::new(), true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}

// COVER CALL EXPRESSION AND ASYNC ARROW HEAD
#[test]
fn cceaaah_test_01() {
    let (node, scanner) = check(CoverCallExpressionAndAsyncArrowHead::parse(&mut newparser("a(10)"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    pretty_check(&*node, "CoverCallExpressionAndAsyncArrowHead: a ( 10 )", vec!["MemberExpression: a", "Arguments: ( 10 )"]);
    concise_check(&*node, "CoverCallExpressionAndAsyncArrowHead: a ( 10 )", vec!["IdentifierName: a", "Arguments: ( 10 )"]);
    format!("{:?}", node);
}
#[test]
fn cceaaah_test_cache_01() {
    let mut parser = newparser("blue(67)");
    let (node, scanner) = check(CoverCallExpressionAndAsyncArrowHead::parse(&mut parser, Scanner::new(), false, false));
    let (node2, scanner2) = check(CoverCallExpressionAndAsyncArrowHead::parse(&mut parser, Scanner::new(), false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn cceaaah_test_err_01() {
    check_err(CoverCallExpressionAndAsyncArrowHead::parse(&mut newparser(""), Scanner::new(), false, false), "MemberExpression expected", 1, 1);
}
#[test]
fn cceaaah_test_err_02() {
    check_err(CoverCallExpressionAndAsyncArrowHead::parse(&mut newparser("name"), Scanner::new(), false, false), "‘(’ expected", 1, 5);
}
#[test]
fn cceaaah_test_prettyerrors_1() {
    let (item, _) = CoverCallExpressionAndAsyncArrowHead::parse(&mut newparser("async(a,b,c)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn cceaaah_test_conciseerrors_1() {
    let (item, _) = CoverCallExpressionAndAsyncArrowHead::parse(&mut newparser("async(a,b,c)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn cceaaah_test_contains_01() {
    let (item, _) = CoverCallExpressionAndAsyncArrowHead::parse(&mut newparser("this.a(10)"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn cceaaah_test_contains_02() {
    let (item, _) = CoverCallExpressionAndAsyncArrowHead::parse(&mut newparser("big[30]()"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn cceaaah_test_contains_03() {
    let (item, _) = CoverCallExpressionAndAsyncArrowHead::parse(&mut newparser("a()"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}

// ASYNC ARROW HEAD
#[test]
fn async_arrow_head_test_01() {
    let (node, scanner) = check(AsyncArrowHead::parse(&mut newparser("async (a)"), Scanner::new()));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "AsyncArrowHead: async ( a )", vec!["ArrowFormalParameters: ( a )"]);
    concise_check(&*node, "AsyncArrowHead: async ( a )", vec!["Keyword: async", "ArrowFormalParameters: ( a )"]);
    format!("{:?}", node);
}
#[test]
fn async_arrow_head_test_err_01() {
    check_err(AsyncArrowHead::parse(&mut newparser(""), Scanner::new()), "‘async’ expected", 1, 1);
}
#[test]
fn async_arrow_head_test_err_02() {
    check_err(AsyncArrowHead::parse(&mut newparser("async\n"), Scanner::new()), "Newline not allowed here.", 1, 6);
}
#[test]
fn async_arrow_head_test_err_03() {
    check_err(AsyncArrowHead::parse(&mut newparser("async"), Scanner::new()), "‘(’ expected", 1, 6);
}
#[test]
fn async_arrow_head_test_prettyerrors_1() {
    let (item, _) = AsyncArrowHead::parse(&mut newparser("async(a,b,c)"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_arrow_head_test_conciseerrors_1() {
    let (item, _) = AsyncArrowHead::parse(&mut newparser("async(a,b,c)"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_arrow_head_test_contains_01() {
    let (item, _) = AsyncArrowHead::parse(&mut newparser("async (b=this)"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn async_arrow_head_test_contains_02() {
    let (item, _) = AsyncArrowHead::parse(&mut newparser("async (b)"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
