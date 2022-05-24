use super::testhelp::{
    check, check_err, chk_scan, newparser, set, Maker, A_ALREADY_DEFN, BAD_USE_STRICT, ILLEGAL_ASYNC_AWAIT,
    IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED, PACKAGE_NOT_ALLOWED, UNEXPECTED_AWAIT, UNEXPECTED_SUPER,
};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::{test_agent, unwind_syntax_error_object};
use ahash::AHashSet;
use test_case::test_case;

// ASYNC FUNCTION DECLARATION
#[test]
fn async_function_declaration_test_01() {
    let (node, scanner) = check(AsyncFunctionDeclaration::parse(
        &mut newparser("async function bob(a,b) { return await foo(a+b); }"),
        Scanner::new(),
        false,
        false,
        true,
    ));
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
    let (node, scanner) = check(AsyncFunctionDeclaration::parse(
        &mut newparser("async function (a,b) { return await foo(a+b); }"),
        Scanner::new(),
        false,
        false,
        true,
    ));
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
    check_err(
        AsyncFunctionDeclaration::parse(&mut newparser(""), Scanner::new(), false, false, true),
        "‘async’ expected",
        1,
        1,
    );
}
#[test]
fn async_function_declaration_test_err_02() {
    check_err(
        AsyncFunctionDeclaration::parse(&mut newparser("async\n"), Scanner::new(), false, false, true),
        "newline not allowed here",
        1,
        6,
    );
}
#[test]
fn async_function_declaration_test_err_03() {
    check_err(
        AsyncFunctionDeclaration::parse(&mut newparser("async bob"), Scanner::new(), false, false, true),
        "‘function’ expected",
        1,
        7,
    );
}
#[test]
fn async_function_declaration_test_err_04() {
    check_err(
        AsyncFunctionDeclaration::parse(&mut newparser("async function"), Scanner::new(), false, false, false),
        "not an identifier",
        1,
        15,
    );
}
#[test]
fn async_function_declaration_test_err_05() {
    check_err(
        AsyncFunctionDeclaration::parse(&mut newparser("async function"), Scanner::new(), false, false, true),
        "‘(’ expected",
        1,
        15,
    );
}
#[test]
fn async_function_declaration_test_err_06() {
    check_err(
        AsyncFunctionDeclaration::parse(&mut newparser("async function bob("), Scanner::new(), false, false, true),
        "‘)’ expected",
        1,
        20,
    );
}
#[test]
fn async_function_declaration_test_err_07() {
    check_err(
        AsyncFunctionDeclaration::parse(&mut newparser("async function bob()"), Scanner::new(), false, false, true),
        "‘{’ expected",
        1,
        21,
    );
}
#[test]
fn async_function_declaration_test_err_08() {
    check_err(
        AsyncFunctionDeclaration::parse(&mut newparser("async function bob() {"), Scanner::new(), false, false, true),
        "‘}’ expected",
        1,
        23,
    );
}
#[test]
fn async_function_declaration_test_prettyerrors_1() {
    let (item, _) = AsyncFunctionDeclaration::parse(
        &mut newparser("async function bob(a, b) { return await foo(a+b); }"),
        Scanner::new(),
        false,
        false,
        true,
    )
    .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_function_declaration_test_prettyerrors_2() {
    let (item, _) = AsyncFunctionDeclaration::parse(
        &mut newparser("async function (a, b) { return await foo(a+b); }"),
        Scanner::new(),
        false,
        false,
        true,
    )
    .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_function_declaration_test_conciseerrors_1() {
    let (item, _) = AsyncFunctionDeclaration::parse(
        &mut newparser("async function bob(a, b) { return await foo(a+b); }"),
        Scanner::new(),
        false,
        false,
        true,
    )
    .unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_function_declaration_test_conciseerrors_2() {
    let (item, _) = AsyncFunctionDeclaration::parse(
        &mut newparser("async function (a, b) { return await foo(a+b); }"),
        Scanner::new(),
        false,
        false,
        true,
    )
    .unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_function_declaration_test_contains_01() {
    let (item, _) = AsyncFunctionDeclaration::parse(
        &mut newparser("async function bob() { return 11; }"),
        Scanner::new(),
        true,
        true,
        true,
    )
    .unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn async_function_declaration_test_contains_02() {
    let (item, _) = AsyncFunctionDeclaration::parse(
        &mut newparser("async function () { return 10; }"),
        Scanner::new(),
        true,
        true,
        true,
    )
    .unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn async_function_declaration_test_bound_names_01() {
    let (item, _) = AsyncFunctionDeclaration::parse(
        &mut newparser("async function a() { return 10; }"),
        Scanner::new(),
        true,
        true,
        true,
    )
    .unwrap();
    assert_eq!(item.bound_names(), vec!["a"]);
}
#[test]
fn async_function_declaration_test_bound_names_02() {
    let (item, _) = AsyncFunctionDeclaration::parse(
        &mut newparser("async function () { return 10; }"),
        Scanner::new(),
        true,
        true,
        true,
    )
    .unwrap();
    assert_eq!(item.bound_names(), vec!["*default*"]);
}
mod async_function_declaration {
    use super::*;
    use test_case::test_case;

    #[test_case("async function a(arg=item.#valid){}" => true; "Params valid")]
    #[test_case("async function a(arg) {return item.#valid;}" => true; "Body valid")]
    #[test_case("async function a(arg=item.#invalid){}" => false; "Params invalid")]
    #[test_case("async function a(arg) {return item.#invalid;}" => false; "Body invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        AsyncFunctionDeclaration::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("async function([a]=b){'use strict';}", false => set(&["Strict functions must also have simple parameter lists"]); "strict body; complex params")]
    #[test_case("async function(a=await b()){}", false => set(&[UNEXPECTED_AWAIT]); "await param")]
    #[test_case("async function(a,a){'use strict';}", false => set(&[A_ALREADY_DEFN]); "duplicate; strict body")]
    #[test_case("async function(a,a){}", true => set(&[A_ALREADY_DEFN]); "duplicate; strict context")]
    #[test_case("async function(lex) { const lex=10; return lex; }", false => set(&["Lexical decls in body duplicate parameters"]); "lexical duplication")]
    #[test_case("async function(a=super.prop){}", false => set(&["Parameters may not include super properties"]); "superprop params")]
    #[test_case("async function(){return super.prop;}", false => set(&["Body may not contain super properties"]); "superprop body")]
    #[test_case("async function(a=super()){}", false => set(&["Parameters may not include super calls"]); "supercall params")]
    #[test_case("async function(){return super();}", false => set(&["Body may not contain super calls"]); "supercall body")]
    #[test_case("async function package(interface){implements;}", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "without default")]
    #[test_case("async function(package){interface;}", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "with default")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        AsyncFunctionDeclaration::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// ASYNC FUNCTION EXPRESSION
#[test]
fn async_function_expression_test_01() {
    let (node, scanner) = check(AsyncFunctionExpression::parse(
        &mut newparser("async function bob(a,b) { return await foo(a+b); }"),
        Scanner::new(),
    ));
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
    let (node, scanner) = check(AsyncFunctionExpression::parse(
        &mut newparser("async function (a,b) { return await foo(a+b); }"),
        Scanner::new(),
    ));
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
    check_err(
        AsyncFunctionExpression::parse(&mut newparser("async\n"), Scanner::new()),
        "newline not allowed here",
        1,
        6,
    );
}
#[test]
fn async_function_expression_test_err_03() {
    check_err(AsyncFunctionExpression::parse(&mut newparser("async bob"), Scanner::new()), "‘function’ expected", 1, 7);
}
#[test]
fn async_function_expression_test_err_05() {
    check_err(AsyncFunctionExpression::parse(&mut newparser("async function"), Scanner::new()), "‘(’ expected", 1, 15);
}
#[test]
fn async_function_expression_test_err_06() {
    check_err(
        AsyncFunctionExpression::parse(&mut newparser("async function bob("), Scanner::new()),
        "‘)’ expected",
        1,
        20,
    );
}
#[test]
fn async_function_expression_test_err_07() {
    check_err(
        AsyncFunctionExpression::parse(&mut newparser("async function bob()"), Scanner::new()),
        "‘{’ expected",
        1,
        21,
    );
}
#[test]
fn async_function_expression_test_err_08() {
    check_err(
        AsyncFunctionExpression::parse(&mut newparser("async function bob() {"), Scanner::new()),
        "‘}’ expected",
        1,
        23,
    );
}
#[test]
fn async_function_expression_test_prettyerrors_1() {
    let (item, _) = AsyncFunctionExpression::parse(
        &mut newparser("async function bob(a, b) { return await foo(a+b); }"),
        Scanner::new(),
    )
    .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_function_expression_test_prettyerrors_2() {
    let (item, _) = AsyncFunctionExpression::parse(
        &mut newparser("async function (a, b) { return await foo(a+b); }"),
        Scanner::new(),
    )
    .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_function_expression_test_conciseerrors_1() {
    let (item, _) = AsyncFunctionExpression::parse(
        &mut newparser("async function bob(a, b) { return await foo(a+b); }"),
        Scanner::new(),
    )
    .unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_function_expression_test_conciseerrors_2() {
    let (item, _) = AsyncFunctionExpression::parse(
        &mut newparser("async function (a, b) { return await foo(a+b); }"),
        Scanner::new(),
    )
    .unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_function_expression_test_contains_01() {
    let (item, _) =
        AsyncFunctionExpression::parse(&mut newparser("async function bob() { return 11; }"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn async_function_expression_test_contains_02() {
    let (item, _) =
        AsyncFunctionExpression::parse(&mut newparser("async function () { return 10; }"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("async function x(arg=item.#valid){}" => true; "Params valid")]
#[test_case("async function x(arg) { arg.#valid(); }" => true; "Body valid")]
#[test_case("async function x(arg=item.#invalid){}" => false; "Params invalid")]
#[test_case("async function x(arg) { arg.#invalid(); }" => false; "Body invalid")]
fn async_function_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = AsyncFunctionExpression::parse(&mut newparser(src), Scanner::new()).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}

mod async_function_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("async function([a]=b){'use strict';}", false => set(&["Strict functions must also have simple parameter lists"]); "strict body; complex params")]
    #[test_case("async function(a=await b()){}", false => set(&[UNEXPECTED_AWAIT]); "await param")]
    #[test_case("async function(a,a){'use strict';}", false => set(&[A_ALREADY_DEFN]); "duplicate; strict body")]
    #[test_case("async function(a,a){}", true => set(&[A_ALREADY_DEFN]); "duplicate; strict context")]
    #[test_case("async function(lex) { const lex=10; return lex; }", false => set(&["Lexical decls in body duplicate parameters"]); "lexical duplication")]
    #[test_case("async function(a=super.prop){}", false => set(&["Parameters may not include super properties"]); "superprop params")]
    #[test_case("async function(){return super.prop;}", false => set(&["Body may not contain super properties"]); "superprop body")]
    #[test_case("async function(a=super()){}", false => set(&["Parameters may not include super calls"]); "supercall params")]
    #[test_case("async function(){return super();}", false => set(&["Body may not contain super calls"]); "supercall body")]
    #[test_case("async function package(interface){implements;}", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "without default")]
    #[test_case("async function(package){interface;}", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "with default")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        Maker::new(src).async_function_expression().early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// ASYNC METHOD
#[test]
fn async_method_test_01() {
    let (node, scanner) = check(AsyncMethod::parse(
        &mut newparser("async bob(a,b) { return await foo(a+b); }"),
        Scanner::new(),
        false,
        false,
    ));
    chk_scan(&scanner, 41);
    pretty_check(
        &*node,
        "AsyncMethod: async bob ( a , b ) { return await foo ( a + b ) ; }",
        vec![
            "ClassElementName: bob",
            "UniqueFormalParameters: a , b",
            "AsyncFunctionBody: return await foo ( a + b ) ;",
        ],
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
    check_err(
        AsyncMethod::parse(&mut newparser("async\n"), Scanner::new(), false, false),
        "newline not allowed here",
        1,
        6,
    );
}
#[test]
fn async_method_test_err_03() {
    check_err(
        AsyncMethod::parse(&mut newparser("async"), Scanner::new(), false, false),
        "ClassElementName expected",
        1,
        6,
    );
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
    let (item, _) =
        AsyncMethod::parse(&mut newparser("async bob(a, b) { return await foo(a+b); }"), Scanner::new(), false, false)
            .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_method_test_conciseerrors_1() {
    let (item, _) =
        AsyncMethod::parse(&mut newparser("async bob(a, b) { return await foo(a+b); }"), Scanner::new(), false, false)
            .unwrap();
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
#[test_case("async [item.#valid](){}" => true; "ElementName valid")]
#[test_case("async bob(arg=item.#valid){}" => true; "Params valid")]
#[test_case("async bob(arg){item.#valid;}" => true; "Body valid")]
#[test_case("async [item.#invalid](){}" => false; "ElementName invalid")]
#[test_case("async bob(arg=item.#invalid){}" => false; "Params invalid")]
#[test_case("async bob(arg){item.#invalid;}" => false; "Body invalid")]
fn async_method_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = AsyncMethod::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod async_method {
    use super::*;
    use test_case::test_case;

    #[test_case("async #blue() {}" => Some("#blue".to_string()); "pid there")]
    #[test_case("async blue(){}" => None; "nothing private")]
    fn private_bound_identifier(src: &str) -> Option<String> {
        Maker::new(src).async_method().private_bound_identifier().map(String::from)
    }

    #[test_case("async a(){}" => false; "without")]
    #[test_case("async a(b=super(0)){}" => true; "params")]
    #[test_case("async a(){super(-1);}" => true; "body")]
    fn has_direct_super(src: &str) -> bool {
        let (item, _) = AsyncMethod::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.has_direct_super()
    }

    #[test]
    fn prop_name() {
        let (item, _) = AsyncMethod::parse(&mut newparser("async a(){}"), Scanner::new(), true, true).unwrap();
        assert_eq!(item.prop_name(), Some(JSString::from("a")));
    }

    #[test_case("async [package](interface) { implements; }", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "async ClassElementName ( UniqueFormalParameters ) { AsyncFunctionBody }")]
    #[test_case("async a([b]){'use strict';}", false => set(&[BAD_USE_STRICT]); "complex params; directive")]
    #[test_case("async a([b]){}", true => set(&[]); "complex params; no directive")]
    #[test_case("async a(){super();}", false => set(&[UNEXPECTED_SUPER]); "supercall")]
    #[test_case("async a(x=await j){}", false => set(&[ILLEGAL_ASYNC_AWAIT]); "await in params")]
    #[test_case("async w(a){let a; const bb=0;}", false => set(&[A_ALREADY_DEFN]); "duplicate lex")]
    #[test_case("async f(a){'use strict'; package;}", false => set(&[PACKAGE_NOT_ALLOWED]); "directive works")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        AsyncMethod::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }

    #[test_case("async [arguments](){}" => true; "yes")]
    #[test_case("async a(){}" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).async_method().contains_arguments()
    }
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
#[test_case("item.#valid;" => true; "FunctionBody valid")]
#[test_case("item.#invalid;" => false; "FunctionBody invalid")]
fn async_function_body_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = AsyncFunctionBody::parse(&mut newparser(src), Scanner::new());
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
#[test_case("0;" => false; "not strict")]
#[test_case("'use strict';" => true; "strict")]
fn async_function_body_test_function_body_contains_use_strict(src: &str) -> bool {
    let (item, _) = AsyncFunctionBody::parse(&mut newparser(src), Scanner::new());
    item.function_body_contains_use_strict()
}
#[test_case("var a; setup(); let alpha=\"a\"; const BETA='β';" => vec![JSString::from("alpha"), JSString::from("BETA")]; "normal")]
fn async_function_body_test_lexically_declared_names(src: &str) -> Vec<JSString> {
    let (item, _) = AsyncFunctionBody::parse(&mut newparser(src), Scanner::new());
    item.lexically_declared_names()
}
mod async_function_body {
    use super::*;
    use test_case::test_case;

    #[test_case("package;", true => set(&[PACKAGE_NOT_ALLOWED]); "FunctionBody")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        AsyncFunctionBody::parse(&mut newparser(src), Scanner::new()).0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }

    #[test_case("arguments;" => true; "yes")]
    #[test_case("" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).async_function_body().contains_arguments()
    }
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
#[test]
fn await_expression_test_contains_03() {
    let item = AwaitExpression::parse(&mut newparser("await a"), Scanner::new(), true).unwrap().0;
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), true);
}
#[test_case("await item.#valid" => true; "Expression valid")]
#[test_case("await item.#invalid" => false; "Expression invalid")]
fn await_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = AwaitExpression::parse(&mut newparser(src), Scanner::new(), true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod await_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("await package", true => set(&[PACKAGE_NOT_ALLOWED]); "await UnaryExpression")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        AwaitExpression::parse(&mut newparser(src), Scanner::new(), true)
            .unwrap()
            .0
            .early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }

    #[test_case("await arguments" => true; "yes")]
    #[test_case("await a" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).await_expression().contains_arguments()
    }
}
