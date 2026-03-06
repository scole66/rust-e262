#![expect(clippy::bool_assert_comparison)]
use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

// ASYNC ARROW FUNCTION
#[test]
fn async_arrow_function_test_01() {
    let (node, scanner) =
        check(AsyncArrowFunction::parse(&mut newparser("async a=>a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 10);
    assert!(matches!(&*node, AsyncArrowFunction::IdentOnly(..)));
    pretty_check(
        &*node,
        "AsyncArrowFunction: async a => a",
        &["AsyncArrowBindingIdentifier: a", "AsyncConciseBody: a"],
    );
    concise_check(
        &*node,
        "AsyncArrowFunction: async a => a",
        &["Keyword: async", "IdentifierName: a", "Punctuator: =>", "IdentifierName: a"],
    );
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn async_arrow_function_test_02() {
    let (node, scanner) =
        check(AsyncArrowFunction::parse(&mut newparser("async (a,b)=>a+b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 16);
    assert!(matches!(&*node, AsyncArrowFunction::Formals(..)));
    pretty_check(
        &*node,
        "AsyncArrowFunction: async ( a , b ) => a + b",
        &["AsyncArrowHead: async ( a , b )", "AsyncConciseBody: a + b"],
    );
    concise_check(
        &*node,
        "AsyncArrowFunction: async ( a , b ) => a + b",
        &["AsyncArrowHead: async ( a , b )", "Punctuator: =>", "AdditiveExpression: a + b"],
    );
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn async_arrow_function_test_err_01() {
    check_err(
        AsyncArrowFunction::parse(&mut newparser(""), Scanner::new(), true, false, false),
        "‘async’ expected",
        1,
        1,
    );
}
#[test]
fn async_arrow_function_test_err_02() {
    check_err(
        AsyncArrowFunction::parse(&mut newparser("async\n"), Scanner::new(), true, false, false),
        "‘(’ expected",
        2,
        1,
    );
}
#[test]
fn async_arrow_function_test_err_03() {
    check_err(
        AsyncArrowFunction::parse(&mut newparser("async"), Scanner::new(), true, false, false),
        "not an identifier",
        1,
        6,
    );
}
#[test]
fn async_arrow_function_test_err_04() {
    check_err(
        AsyncArrowFunction::parse(&mut newparser("async a\n"), Scanner::new(), true, false, false),
        "newline not allowed here",
        1,
        8,
    );
}
#[test]
fn async_arrow_function_test_err_05() {
    check_err(
        AsyncArrowFunction::parse(&mut newparser("async a"), Scanner::new(), true, false, false),
        "‘=>’ expected",
        1,
        8,
    );
}
#[test]
fn async_arrow_function_test_err_06() {
    check_err(
        AsyncArrowFunction::parse(&mut newparser("async a=>"), Scanner::new(), true, false, false),
        "AsyncConciseBody expected",
        1,
        10,
    );
}
#[test]
fn async_arrow_function_test_err_07() {
    check_err(
        AsyncArrowFunction::parse(&mut newparser("async ("), Scanner::new(), true, false, false),
        "‘)’ expected",
        1,
        8,
    );
}
#[test]
fn async_arrow_function_test_err_08() {
    check_err(
        AsyncArrowFunction::parse(&mut newparser("async (5)"), Scanner::new(), true, false, false),
        "‘)’ expected",
        1,
        8,
    );
}
#[test]
fn async_arrow_function_test_err_09() {
    check_err(
        AsyncArrowFunction::parse(&mut newparser("blue (5)"), Scanner::new(), true, false, false),
        "‘async’ expected",
        1,
        1,
    );
}
#[test]
fn async_arrow_function_test_err_10() {
    check_err(
        AsyncArrowFunction::parse(&mut newparser("async (a)\n"), Scanner::new(), true, false, false),
        "newline not allowed here",
        1,
        10,
    );
}
#[test]
fn async_arrow_function_test_err_11() {
    check_err(
        AsyncArrowFunction::parse(&mut newparser("async (a)"), Scanner::new(), true, false, false),
        "‘=>’ expected",
        1,
        10,
    );
}
#[test]
fn async_arrow_function_test_err_12() {
    check_err(
        AsyncArrowFunction::parse(&mut newparser("async (a)=>"), Scanner::new(), true, false, false),
        "AsyncConciseBody expected",
        1,
        12,
    );
}
#[test]
fn async_arrow_function_test_prettyerrors_1() {
    let (item, _) =
        AsyncArrowFunction::parse(&mut newparser("async a=>a"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_arrow_function_test_prettyerrors_2() {
    let (item, _) =
        AsyncArrowFunction::parse(&mut newparser("async (a,b)=>a+b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_arrow_function_test_conciseerrors_1() {
    let (item, _) =
        AsyncArrowFunction::parse(&mut newparser("async a=>a"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_arrow_function_test_conciseerrors_2() {
    let (item, _) =
        AsyncArrowFunction::parse(&mut newparser("async (a,b)=>a+b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_arrow_function_test_contains_01() {
    let (item, _) =
        AsyncArrowFunction::parse(&mut newparser("async bob => this"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn async_arrow_function_test_contains_02() {
    let (item, _) =
        AsyncArrowFunction::parse(&mut newparser("async bob => a"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn async_arrow_function_test_contains_03() {
    let (item, _) =
        AsyncArrowFunction::parse(&mut newparser("async (b=this) => a"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn async_arrow_function_test_contains_04() {
    let (item, _) =
        AsyncArrowFunction::parse(&mut newparser("async (b) => this"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn async_arrow_function_test_contains_05() {
    let (item, _) =
        AsyncArrowFunction::parse(&mut newparser("async (b) => b"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn async_arrow_function_test_contains_06() {
    let (item, _) =
        AsyncArrowFunction::parse(&mut newparser("async (b) => new.target"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::NewTarget), true);
}
#[test]
fn async_arrow_function_test_contains_07() {
    let (item, _) =
        AsyncArrowFunction::parse(&mut newparser("async (b) => super.x"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::SuperProperty), true);
}
#[test]
fn async_arrow_function_test_contains_08() {
    let (item, _) =
        AsyncArrowFunction::parse(&mut newparser("async (b) => super()"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::SuperCall), true);
}
#[test]
fn async_arrow_function_test_contains_09() {
    let (item, _) =
        AsyncArrowFunction::parse(&mut newparser("async (b) => super()"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Super), true);
}
#[test]
fn async_arrow_function_test_contains_10() {
    let (item, _) =
        AsyncArrowFunction::parse(&mut newparser("async (b) => 2048"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("async x => item.#valid" => true; "Simple valid")]
#[test_case("async (a=item.#valid) => a" => true; "WithParams AsyncArrowHead valid")]
#[test_case("async (a,b) => a.#valid - b" => true; "WithParams AsyncConciseBody valid")]
#[test_case("async x => item.#invalid" => false; "Simple invalid")]
#[test_case("async (a=item.#invalid) => a" => false; "WithParams AsyncArrowHead invalid")]
#[test_case("async (a,b) => a.#invalid - b" => false; "WithParams AsyncConciseBody invalid")]
fn async_arrow_function_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = AsyncArrowFunction::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}

mod async_arrow_function {
    use super::*;
    use test_case::test_case;

    #[test_case("async x => x", true => sset(&[]); "ident; no errors")]
    #[test_case("async y => { let y; }", true => sset(&["Identifier 'y' has already been declared"]); "ident; duplicate bound names")]
    #[test_case("async package => { return interface; }", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "ident; child production errors")]
    #[test_case("async (x=await y) => x", true => sset(&["Await expression cannot be a default value"]); "formals; await expr")]
    #[test_case("async (x,y) => { let x; }", true => sset(&["Identifier 'x' has already been declared"]); "formals; duplicate bound names")]
    #[test_case("async (x=10) => { 'use strict'; return x; }", true => sset(&["Illegal 'use strict' directive in function with non-simple parameter list"]); "formals; non simple parameter list")]
    #[test_case("async (package) => { return interface; }", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "formals; child production errors")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).async_arrow_function().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("async a => arguments" => true; "no-formals (yes)")]
    #[test_case("async a => a" => false; "no-formals (no)")]
    #[test_case("async ({a=arguments}) => a" => true; "formals (left)")]
    #[test_case("async (a) => arguments" => true; "formals (right)")]
    #[test_case("async (a) => a" => false; "formals (none)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).async_arrow_function().contains_arguments()
    }

    #[test_case("  async (formals) => { return formals ** factor; }" => Location { starting_line: 1, starting_column: 3, span: Span { starting_index: 2, length: 48 } }; "with-formals")]
    #[test_case("  async x => x + bias" => Location { starting_line: 1, starting_column: 3, span: Span { starting_index: 2, length: 19 } }; "ident-only")]
    fn location(src: &str) -> Location {
        Maker::new(src).async_arrow_function().location()
    }

    #[test_case("async a => call()*a" => ssome("call ( ) * a"); "ident-only; location in body")]
    #[test_case("async a /* call() */ => 2 * a" => None; "ident-only; location not in body")]
    #[test_case("async (x, y) => call()*x*y" => ssome("call ( ) * x * y"); "formals; location in body")]
    #[test_case("async (x=call(), y) => x*y" => None; "formals; location in params; no body")]
    #[test_case("async (x=(function(){return call();})(),y) => x*y" => ssome("return call ( ) ;"); "formals; location in params; has body")]
    #[test_case("async (x,y) => /* call() */ x*y" => None; "formals; location not in child productions")]
    fn body_containing_location(src: &str) -> Option<String> {
        let location = find_call(src);
        Maker::new(src).async_arrow_function().body_containing_location(&location).map(|node| node.to_string())
    }
}

// ASYNC CONCISE BODY
#[test]
fn async_concise_body_test_01() {
    let (node, scanner) = check(AsyncConciseBody::parse(&mut newparser("a"), Scanner::new(), true));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, AsyncConciseBody::Expression(..)));
    pretty_check(&*node, "AsyncConciseBody: a", &["ExpressionBody: a"]);
    concise_check(&*node, "IdentifierName: a", &[]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn async_concise_body_test_02() {
    let (node, scanner) = check(AsyncConciseBody::parse(&mut newparser("{a;}"), Scanner::new(), true));
    chk_scan(&scanner, 4);
    assert!(matches!(&*node, AsyncConciseBody::Function(..)));
    pretty_check(&*node, "AsyncConciseBody: { a ; }", &["AsyncFunctionBody: a ;"]);
    concise_check(&*node, "AsyncConciseBody: { a ; }", &["Punctuator: {", "ExpressionStatement: a ;", "Punctuator: }"]);
    assert_ne!(format!("{node:?}"), "");
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
#[test_case("item.#valid" => true; "Expression valid")]
#[test_case("{ return item.#valid; }" => true; "Function valid")]
#[test_case("item.#invalid" => false; "Expression invalid")]
#[test_case("{ return item.#invalid; }" => false; "Function invalid")]
fn async_concise_body_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = AsyncConciseBody::parse(&mut newparser(src), Scanner::new(), true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}

mod async_concise_body {
    use super::*;
    use test_case::test_case;

    #[test_case("{ return interface; }", true => sset(&[INTERFACE_NOT_ALLOWED]); "functionbody; child production errors")]
    #[test_case("interface", true => sset(&[INTERFACE_NOT_ALLOWED]); "expression body; child production errors")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).async_concise_body().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("arguments" => true; "exp (yes)")]
    #[test_case("a" => false; "exp (no)")]
    #[test_case("{ arguments; }" => true; "block (yes)")]
    #[test_case("{}" => false; "block (no)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).async_concise_body().contains_arguments()
    }

    #[test_case("  x-y" => Location { starting_line: 1, starting_column: 3, span: Span { starting_index: 2, length: 3 } }; "expression")]
    #[test_case("  {side_effect();}" => Location { starting_line: 1, starting_column: 3, span: Span { starting_index: 2, length: 16 } }; "body")]
    fn location(src: &str) -> Location {
        Maker::new(src).async_concise_body().location()
    }

    #[test_case("x + 3" => svec(&[]); "expression body")]
    #[test_case("{ let a; const b=0; var c; function d() {} }" => svec(&["c", "d"]); "function body")]
    fn var_declared_names(src: &str) -> Vec<String> {
        Maker::new(src).async_concise_body().var_declared_names().into_iter().map(String::from).collect()
    }

    #[test_case("3" => false; "ExpressionBody")]
    #[test_case("{ 'use strict'; }" => true; "{ FunctionBody }")]
    #[test_case("{ 3; 'use strict'; }" => false; "FunctionBody without")]
    fn contains_use_strict(src: &str) -> bool {
        Maker::new(src).async_concise_body().contains_use_strict()
    }

    #[test_case("x + 3" => svec(&[]); "expression body")]
    #[test_case("{ let a; const b=0; var c; function d() {} }" => svec(&["c", "function d ( ) { }"]); "function body")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).async_concise_body().var_scoped_declarations().iter().map(String::from).collect()
    }

    #[test_case("expression" => Vec::<String>::new(); "ExpressionBody")]
    #[test_case("{ let a; }" => vec!["a"]; "{ FunctionBody }")]
    fn lexically_declared_names(src: &str) -> Vec<String> {
        Maker::new(src)
            .async_concise_body()
            .lexically_declared_names()
            .into_iter()
            .map(String::from)
            .collect::<Vec<_>>()
    }

    #[test_case("x + 3" => svec(&[]); "expression body")]
    #[test_case("{ let a; const b=0; var c; function d() {} }" => svec(&["let a ;", "const b = 0 ;"]); "function body")]
    fn lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).async_concise_body().lexically_scoped_declarations().iter().map(String::from).collect()
    }

    #[test_case("blue" => None; "location not in production")]
    #[test_case("(function(){return call();})()" => ssome("return call ( ) ;"); "expression; body in statements")]
    #[test_case("/* call() */ 3" => None; "expression; no body")]
    #[test_case("call()" => ssome("call ( )"); "expression; body is the match")]
    #[test_case("{ g = 1; /* call() */ }" => ssome("{ g = 1 ; }"); "function; body is the match")]
    #[test_case("{ return (function(){return call();})(); }" => ssome("return call ( ) ;"); "function; body in the statements")]
    fn body_containing_location(src: &str) -> Option<String> {
        let location = find_call(src);
        Maker::new(src).async_concise_body().body_containing_location(&location).map(|node| node.to_string())
    }
}
// ASYNC ARROW BINDING IDENTIFIER
#[test]
fn async_arrow_binding_identifier_test_01() {
    let (node, scanner) = check(AsyncArrowBindingIdentifier::parse(&mut newparser("a"), Scanner::new(), false));
    chk_scan(&scanner, 1);
    pretty_check(&*node, "AsyncArrowBindingIdentifier: a", &["BindingIdentifier: a"]);
    concise_check(&*node, "IdentifierName: a", &[]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn async_arrow_binding_identifier_test_err_01() {
    check_err(AsyncArrowBindingIdentifier::parse(&mut newparser(""), Scanner::new(), false), "not an identifier", 1, 1);
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
fn async_arrow_binding_identifier_test_early_errors() {
    AsyncArrowBindingIdentifier::parse(&mut newparser("x"), Scanner::new(), true)
        .unwrap()
        .0
        .early_errors(&mut vec![], true);
}

mod async_arrow_binding_identifier {
    use super::*;
    use test_case::test_case;

    #[test_case("a" => vec!["a"]; "typical")]
    fn bound_names(src: &str) -> Vec<String> {
        Maker::new(src)
            .async_arrow_binding_identifier()
            .bound_names()
            .into_iter()
            .map(String::from)
            .collect::<Vec<String>>()
    }
}

// COVER CALL EXPRESSION AND ASYNC ARROW HEAD
#[test]
fn cceaaah_test_01() {
    let (node, scanner) =
        check(CoverCallExpressionAndAsyncArrowHead::parse(&mut newparser("a(10)"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    pretty_check(
        &*node,
        "CoverCallExpressionAndAsyncArrowHead: a ( 10 )",
        &["MemberExpression: a", "Arguments: ( 10 )"],
    );
    concise_check(
        &*node,
        "CoverCallExpressionAndAsyncArrowHead: a ( 10 )",
        &["IdentifierName: a", "Arguments: ( 10 )"],
    );
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn cceaaah_test_cache_01() {
    let mut parser = newparser("blue(67)");
    let (node, scanner) = check(CoverCallExpressionAndAsyncArrowHead::parse(&mut parser, Scanner::new(), false, false));
    let (node2, scanner2) =
        check(CoverCallExpressionAndAsyncArrowHead::parse(&mut parser, Scanner::new(), false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn cceaaah_test_err_01() {
    check_err(
        CoverCallExpressionAndAsyncArrowHead::parse(&mut newparser(""), Scanner::new(), false, false),
        "MemberExpression expected",
        1,
        1,
    );
}
#[test]
fn cceaaah_test_err_02() {
    check_err(
        CoverCallExpressionAndAsyncArrowHead::parse(&mut newparser("name"), Scanner::new(), false, false),
        "‘(’ expected",
        1,
        5,
    );
}
#[test]
fn cceaaah_test_prettyerrors_1() {
    let (item, _) =
        CoverCallExpressionAndAsyncArrowHead::parse(&mut newparser("async(a,b,c)"), Scanner::new(), false, false)
            .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn cceaaah_test_conciseerrors_1() {
    let (item, _) =
        CoverCallExpressionAndAsyncArrowHead::parse(&mut newparser("async(a,b,c)"), Scanner::new(), false, false)
            .unwrap();
    concise_error_validate(&*item);
}

// ASYNC ARROW HEAD
#[test]
fn async_arrow_head_test_01() {
    let (node, scanner) = check(AsyncArrowHead::parse(&mut newparser("async (a)"), Scanner::new()));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "AsyncArrowHead: async ( a )", &["ArrowFormalParameters: ( a )"]);
    concise_check(&*node, "AsyncArrowHead: async ( a )", &["Keyword: async", "ArrowFormalParameters: ( a )"]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn async_arrow_head_test_err_01() {
    check_err(AsyncArrowHead::parse(&mut newparser(""), Scanner::new()), "‘async’ expected", 1, 1);
}
#[test]
fn async_arrow_head_test_err_02() {
    check_err(AsyncArrowHead::parse(&mut newparser("async\n"), Scanner::new()), "newline not allowed here", 1, 6);
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
#[test_case("async (a=item.#valid)" => true; "ArrowFormalParameters valid")]
#[test_case("async (a=item.#invalid)" => false; "ArrowFormalParameters invalid")]
fn async_arrow_head_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = AsyncArrowHead::parse(&mut newparser(src), Scanner::new()).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
#[test]
fn async_arrow_head_test_early_errors() {
    AsyncArrowHead::parse(&mut newparser("async(a)"), Scanner::new()).unwrap().0.early_errors(&mut vec![], true);
}

mod async_arrow_head {
    use super::*;
    use test_case::test_case;

    #[test_case("async ({a=arguments})" => true; "yes")]
    #[test_case("async (a)" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).async_arrow_head().contains_arguments()
    }

    #[test_case("  async (formals)" => Location { starting_line: 1, starting_column: 3, span: Span { starting_index: 2, length: 15 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).async_arrow_head().location()
    }
}
