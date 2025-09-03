#![expect(clippy::bool_assert_comparison)]
use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

const B_ALREADY_DEFINED: &str = "‘b’ already defined";
const COMPLEX_PARAMS: &str = "Illegal 'use strict' directive in function with non-simple parameter list";
const BAD_SUPER: &str = "‘super’ not allowed here";

// FUNCTION DECLARATION
#[test]
fn function_declaration_test_01() {
    let (node, scanner) = check(FunctionDeclaration::parse(
        &mut newparser("function bob(a,b) { return foo(a+b); }"),
        Scanner::new(),
        false,
        false,
        true,
    ));
    chk_scan(&scanner, 38);
    pretty_check(
        &*node,
        "FunctionDeclaration: function bob ( a , b ) { return foo ( a + b ) ; }",
        &["BindingIdentifier: bob", "FormalParameters: a , b", "FunctionBody: return foo ( a + b ) ;"],
    );
    concise_check(
        &*node,
        "FunctionDeclaration: function bob ( a , b ) { return foo ( a + b ) ; }",
        &[
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
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn function_declaration_test_02() {
    let (node, scanner) =
        check(FunctionDeclaration::parse(&mut newparser("function (z) {}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 15);
    pretty_check(&*node, "FunctionDeclaration: function ( z ) { }", &["FormalParameters: z", "FunctionBody: "]);
    concise_check(
        &*node,
        "FunctionDeclaration: function ( z ) { }",
        &["Keyword: function", "Punctuator: (", "IdentifierName: z", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
    );
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn function_declaration_test_err_01() {
    check_err(
        FunctionDeclaration::parse(&mut newparser(""), Scanner::new(), false, false, true),
        "‘function’ expected",
        1,
        1,
    );
}
#[test]
fn function_declaration_test_err_02() {
    check_err(
        FunctionDeclaration::parse(&mut newparser("function"), Scanner::new(), false, false, true),
        "‘(’ expected",
        1,
        9,
    );
}
#[test]
fn function_declaration_test_err_03() {
    check_err(
        FunctionDeclaration::parse(&mut newparser("function (z)"), Scanner::new(), false, false, false),
        "not an identifier",
        1,
        10,
    );
}
#[test]
fn function_declaration_test_err_04() {
    check_err(
        FunctionDeclaration::parse(&mut newparser("function foo"), Scanner::new(), false, false, true),
        "‘(’ expected",
        1,
        13,
    );
}
#[test]
fn function_declaration_test_err_05() {
    check_err(
        FunctionDeclaration::parse(&mut newparser("function foo("), Scanner::new(), false, false, true),
        "‘)’ expected",
        1,
        14,
    );
}
#[test]
fn function_declaration_test_err_06() {
    check_err(
        FunctionDeclaration::parse(&mut newparser("function foo()"), Scanner::new(), false, false, true),
        "‘{’ expected",
        1,
        15,
    );
}
#[test]
fn function_declaration_test_err_07() {
    check_err(
        FunctionDeclaration::parse(&mut newparser("function foo(){"), Scanner::new(), false, false, true),
        "‘}’ expected",
        1,
        16,
    );
}
#[test]
fn function_declaration_test_prettyerrors_1() {
    let (item, _) = FunctionDeclaration::parse(
        &mut newparser("function bob(a, b) { return foo(a+b); }"),
        Scanner::new(),
        false,
        false,
        true,
    )
    .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn function_declaration_test_prettyerrors_2() {
    let (item, _) = FunctionDeclaration::parse(
        &mut newparser("function (a, b) { return foo(a+b); }"),
        Scanner::new(),
        false,
        false,
        true,
    )
    .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn function_declaration_test_conciseerrors_1() {
    let (item, _) = FunctionDeclaration::parse(
        &mut newparser("function bob(a, b) { return foo(a+b); }"),
        Scanner::new(),
        false,
        false,
        true,
    )
    .unwrap();
    concise_error_validate(&*item);
}
#[test]
fn function_declaration_test_conciseerrors_2() {
    let (item, _) = FunctionDeclaration::parse(
        &mut newparser("function (a, b) { return foo(a+b); }"),
        Scanner::new(),
        false,
        false,
        true,
    )
    .unwrap();
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
    let (item, _) =
        FunctionDeclaration::parse(&mut newparser("function a(){}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn function_declaration_test_bound_names_02() {
    let (item, _) =
        FunctionDeclaration::parse(&mut newparser("function (){}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["*default*"]);
}
#[test_case("function a(a=b.#valid){}" => true; "Params valid")]
#[test_case("function a(b){c.#valid;}" => true; "Body valid")]
#[test_case("function a(a=b.#invalid){}" => false; "Params invalid")]
#[test_case("function a(b){c.#invalid;}" => false; "Body invalid")]
fn function_declaration_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = FunctionDeclaration::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod function_declaration {
    use super::*;
    use test_case::test_case;

    #[test_case("function a(){}" => "function a ( ) { }"; "id only")]
    #[test_case("function (){}" => "function ( ) { }"; "nothing")]
    #[test_case("function a(b){}" => "function a ( b ) { }"; "id + params")]
    #[test_case("function (b){}" => "function ( b ) { }"; "params only")]
    #[test_case("function a(){null;}" => "function a ( ) { null ; }"; "id + body")]
    #[test_case("function (){null;}" => "function ( ) { null ; }"; "body only")]
    #[test_case("function a(b){null;}" => "function a ( b ) { null ; }"; "id + params + body")]
    #[test_case("function (b){null;}" => "function ( b ) { null ; }"; "params + body")]
    fn display(src: &str) -> String {
        format!("{}", Maker::new(src).function_declaration())
    }

    #[test_case("function package(implements) {interface;}", true => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "named function")]
    #[test_case("function (implements) {interface;}", true => sset(&[IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "anonymous function")]
    #[test_case("function a(b,b){}", true => sset(&[B_ALREADY_DEFINED]); "duplicated params (strict)")]
    #[test_case("function a(b,b){}", false => sset(&[]); "duplicated params (non-strict)")]
    #[test_case("function a(...b){}", true => sset(&[]); "complex params in strict mode")]
    #[test_case("function a(...b){'use strict';}", true => sset(&[COMPLEX_PARAMS]); "complex params with use strict (strict mode)")]
    #[test_case("function a(...b){'use strict';}", false => sset(&[COMPLEX_PARAMS]); "complex params with use strict (non-strict mode)")]
    #[test_case("function a(b){let b=3;}", true => sset(&[B_ALREADY_DEFINED]); "lexname shadowing params")]
    #[test_case("function a(b=super()){}", false => sset(&[BAD_SUPER]); "SuperCall in params")]
    #[test_case("function a(b=super.c){}", false => sset(&[BAD_SUPER]); "SuperProperty in params")]
    #[test_case("function a(){super();}", false => sset(&[BAD_SUPER]); "SuperCall in body")]
    #[test_case("function a(){super.b;}", false => sset(&[BAD_SUPER]); "SuperProperty in body")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        FunctionDeclaration::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("   function a(){}" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 14 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).function_declaration().location()
    }

    #[test_case("function named() {}" => "named"; "named")]
    #[test_case("function (){}" => "*default*"; "unnamed")]
    fn bound_name(src: &str) -> String {
        Maker::new(src).function_declaration().bound_name().into()
    }
}

// FUNCTION EXPRESSION
#[test]
fn function_expression_test_01() {
    let (node, scanner) =
        check(FunctionExpression::parse(&mut newparser("function bob(a,b) { return foo(a+b); }"), Scanner::new()));
    chk_scan(&scanner, 38);
    pretty_check(
        &*node,
        "FunctionExpression: function bob ( a , b ) { return foo ( a + b ) ; }",
        &["BindingIdentifier: bob", "FormalParameters: a , b", "FunctionBody: return foo ( a + b ) ;"],
    );
    concise_check(
        &*node,
        "FunctionExpression: function bob ( a , b ) { return foo ( a + b ) ; }",
        &[
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
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn function_expression_test_02() {
    let (node, scanner) = check(FunctionExpression::parse(&mut newparser("function (z) {}"), Scanner::new()));
    chk_scan(&scanner, 15);
    pretty_check(&*node, "FunctionExpression: function ( z ) { }", &["FormalParameters: z", "FunctionBody: "]);
    concise_check(
        &*node,
        "FunctionExpression: function ( z ) { }",
        &["Keyword: function", "Punctuator: (", "IdentifierName: z", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
    );
    assert_ne!(format!("{node:?}"), "");
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
    let (item, _) =
        FunctionExpression::parse(&mut newparser("function bob(a, b) { return foo(a+b); }"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn function_expression_test_prettyerrors_2() {
    let (item, _) =
        FunctionExpression::parse(&mut newparser("function (a, b) { return foo(a+b); }"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn function_expression_test_conciseerrors_1() {
    let (item, _) =
        FunctionExpression::parse(&mut newparser("function bob(a, b) { return foo(a+b); }"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn function_expression_test_conciseerrors_2() {
    let (item, _) =
        FunctionExpression::parse(&mut newparser("function (a, b) { return foo(a+b); }"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test_case("function a(a=b.#valid){}" => true; "Params valid")]
#[test_case("function a(b){c.#valid;}" => true; "Body valid")]
#[test_case("function a(a=b.#invalid){}" => false; "Params invalid")]
#[test_case("function a(b){c.#invalid;}" => false; "Body invalid")]
fn function_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = FunctionExpression::parse(&mut newparser(src), Scanner::new()).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod function_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("function a(){}" => "function a ( ) { }"; "id only")]
    #[test_case("function (){}" => "function ( ) { }"; "nothing")]
    #[test_case("function a(b){}" => "function a ( b ) { }"; "id + params")]
    #[test_case("function (b){}" => "function ( b ) { }"; "params only")]
    #[test_case("function a(){null;}" => "function a ( ) { null ; }"; "id + body")]
    #[test_case("function (){null;}" => "function ( ) { null ; }"; "body only")]
    #[test_case("function a(b){null;}" => "function a ( b ) { null ; }"; "id + params + body")]
    #[test_case("function (b){null;}" => "function ( b ) { null ; }"; "params + body")]
    fn display(src: &str) -> String {
        format!("{}", Maker::new(src).function_expression())
    }
    #[test_case("function package(implements) {interface;}", true => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "named function")]
    #[test_case("function (implements) {interface;}", true => sset(&[IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "anonymous function")]
    #[test_case("function a(b,b){}", true => sset(&[B_ALREADY_DEFINED]); "duplicated params (strict)")]
    #[test_case("function a(b,b){}", false => sset(&[]); "duplicated params (non-strict)")]
    #[test_case("function a(...b){}", true => sset(&[]); "complex params in strict mode")]
    #[test_case("function a(...b){'use strict';}", true => sset(&[COMPLEX_PARAMS]); "complex params with use strict (strict mode)")]
    #[test_case("function a(...b){'use strict';}", false => sset(&[COMPLEX_PARAMS]); "complex params with use strict (non-strict mode)")]
    #[test_case("function a(a,b){let b=3;let c=10;}", true => sset(&[B_ALREADY_DEFINED]); "lexname shadowing params")]
    #[test_case("function a(b=super()){}", false => sset(&[BAD_SUPER]); "SuperCall in params")]
    #[test_case("function a(b=super.c){}", false => sset(&[BAD_SUPER]); "SuperProperty in params")]
    #[test_case("function a(){super();}", false => sset(&[BAD_SUPER]); "SuperCall in body")]
    #[test_case("function a(){super.b;}", false => sset(&[BAD_SUPER]); "SuperProperty in body")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        FunctionExpression::parse(&mut newparser(src), Scanner::new()).unwrap().0.early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("function a(){}" => true; "named")]
    #[test_case("function (){}" => false; "unnamed")]
    fn is_named_function(src: &str) -> bool {
        Maker::new(src).function_expression().is_named_function()
    }

    #[test_case("   function a(){}" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 14 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).function_expression().location()
    }
}

// FUNCTION BODY
#[test]
fn function_body_test_01() {
    let (node, scanner) =
        FunctionBody::parse(&mut newparser(""), Scanner::new(), false, false, FunctionBodyParent::Function);
    chk_scan(&scanner, 0);
    pretty_check(&*node, "FunctionBody: ", &["FunctionStatementList: "]);
    concise_check(&*node, "", &[]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn function_body_test_prettyerrors_1() {
    let (item, _) =
        FunctionBody::parse(&mut newparser("a; b; c;"), Scanner::new(), false, false, FunctionBodyParent::Function);
    pretty_error_validate(&*item);
}
#[test]
fn function_body_test_conciseeerrors_1() {
    let (item, _) =
        FunctionBody::parse(&mut newparser("a; b; c;"), Scanner::new(), false, false, FunctionBodyParent::Function);
    concise_error_validate(&*item);
}
#[test]
fn function_body_test_cache_01() {
    let mut parser = newparser("a; b; c;");
    let (node, scanner) = FunctionBody::parse(&mut parser, Scanner::new(), false, false, FunctionBodyParent::Function);
    let (node2, scanner2) =
        FunctionBody::parse(&mut parser, Scanner::new(), false, false, FunctionBodyParent::Function);
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn function_body_test_contains_01() {
    let (item, _) = FunctionBody::parse(&mut newparser("0;"), Scanner::new(), true, true, FunctionBodyParent::Function);
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn function_body_test_contains_02() {
    let (item, _) = FunctionBody::parse(&mut newparser("a;"), Scanner::new(), true, true, FunctionBodyParent::Function);
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("a.#valid;" => true; "Statement valid")]
#[test_case("a.#invalid;" => false; "Statement invalid")]
fn function_body_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = FunctionBody::parse(&mut newparser(src), Scanner::new(), true, true, FunctionBodyParent::Function);
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
#[test_case("'one'; 'two'; 'three'; blue;" => vec![JSString::from("one"), JSString::from("two"), JSString::from("three")]; "normal")]
fn function_body_test_directive_prologue(src: &str) -> Vec<JSString> {
    let (item, _) = FunctionBody::parse(&mut newparser(src), Scanner::new(), true, true, FunctionBodyParent::Function);
    item.directive_prologue().into_iter().map(|tok| tok.value).collect()
}
#[test_case("'one'; 3;" => false; "directive no strict")]
#[test_case("12;" => false; "no prologue")]
#[test_case("'a'; 'use strict';" => true; "good strict")]
#[test_case("'use\\x20strict';" => false; "bad strict")]
fn function_body_test_function_body_contains_use_strict(src: &str) -> bool {
    let (item, _) = FunctionBody::parse(&mut newparser(src), Scanner::new(), true, true, FunctionBodyParent::Function);
    item.function_body_contains_use_strict()
}
#[test_case("var a; setup(); let alpha=\"a\"; const BETA='β';" => vec![JSString::from("alpha"), JSString::from("BETA")]; "normal")]
fn function_body_test_lexically_declared_names(src: &str) -> Vec<JSString> {
    let (item, _) = FunctionBody::parse(&mut newparser(src), Scanner::new(), true, true, FunctionBodyParent::Function);
    item.lexically_declared_names()
}
mod function_body {
    use super::*;
    use test_case::test_case;

    const B_ALREADY_LEX: &str = "‘b’ cannot be used in a var statement, as it is also lexically declared";
    const DUP_LABLES: &str = "duplicate labels detected";
    const UNDEF_BREAK: &str = "undefined break target detected";
    const UNDEF_CONTINUE: &str = "undefined continue target detected";

    #[test_case("package;", true => sset(&[PACKAGE_NOT_ALLOWED]); "FunctionStatementList")]
    #[test_case("let b; let b;", false => sset(&[B_ALREADY_DEFINED]); "duplicate lexical bindings")]
    #[test_case("var b; var c; let b; let a;", false => sset(&[B_ALREADY_LEX]); "var/lex name clash")]
    #[test_case("a:a:a:;", false => sset(&[DUP_LABLES]); "duplicate labels")]
    #[test_case("break a;", false => sset(&[UNDEF_BREAK]); "undefined break")]
    #[test_case("while (1) continue a;", false => sset(&[UNDEF_CONTINUE]); "undefined continue")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).function_body().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("arguments;" => true; "yes")]
    #[test_case("" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).function_body().contains_arguments()
    }

    #[test_case("   return 3;" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 9 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).function_body().location()
    }

    #[test_case("let a; const b=0; var c; function d() {}" => svec(&["c", "d"]); "function body")]
    fn var_declared_names(src: &str) -> Vec<String> {
        Maker::new(src).function_body().var_declared_names().into_iter().map(String::from).collect()
    }

    #[test_case("let a; const b=0; var c; function d() {}" => svec(&["c", "function d ( ) { }"]); "function body")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).function_body().var_scoped_declarations().iter().map(String::from).collect()
    }

    #[test_case("let a; const b=0; var c; function d() {}" => svec(&["let a ;", "const b = 0 ;"]); "typical")]
    fn lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).function_body().lexically_scoped_declarations().iter().map(String::from).collect()
    }
}

// FUNCTION STATEMENT LIST
#[test]
fn function_statement_list_test_01() {
    let (node, scanner) = FunctionStatementList::parse(&mut newparser(""), Scanner::new(), false, false);
    chk_scan(&scanner, 0);
    pretty_check(&*node, "FunctionStatementList: ", &[]);
    concise_check(&*node, "", &[]);
    assert_ne!(format!("{node:?}"), "");
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
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
#[test_case("" => Vec::<StringToken>::new(); "empty")]
#[test_case("\"use strict\";" => vec![StringToken { value: JSString::from("use strict"), delimiter: StringDelimiter::Double, raw: None }]; "list")]
fn function_statement_list_test_initial_string_tokens(src: &str) -> Vec<StringToken> {
    let (item, _) = FunctionStatementList::parse(&mut newparser(src), Scanner::new(), true, true);
    item.initial_string_tokens()
}
#[test_case("let a;" => vec![JSString::from("a")]; "one item")]
#[test_case("" => Vec::<JSString>::new(); "empty")]
#[test_case("let a; var b=3; const Q=99;" => vec![JSString::from("a"), JSString::from("Q")]; "many items")]
fn function_statement_list_test_lexically_declared_names(src: &str) -> Vec<JSString> {
    let (item, _) = FunctionStatementList::parse(&mut newparser(src), Scanner::new(), true, true);
    item.lexically_declared_names()
}
mod function_statement_list {
    use super::*;
    use test_case::test_case;

    #[test_case("var a,b,c;" => vec!["a", "b", "c"]; "one statement")]
    #[test_case("" => Vec::<String>::new(); "no statements")]
    #[test_case("var a; var b; var c;" => vec!["a", "b", "c"]; "three statements")]
    fn var_declared_names(src: &str) -> Vec<String> {
        Maker::new(src).function_statement_list().var_declared_names().into_iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("a:a:;" => true; "one statement")]
    #[test_case("a:;" => false; "one statement (no dups)")]
    #[test_case("" => false; "no statements")]
    fn contains_duplicate_labels(src: &str) -> bool {
        Maker::new(src).function_statement_list().contains_duplicate_labels(&[])
    }

    #[test_case("break a;" => true; "one statement")]
    #[test_case(";" => false; "one statement (no break)")]
    #[test_case("" => false; "no statements")]
    fn contains_undefined_break_target(src: &str) -> bool {
        Maker::new(src).function_statement_list().contains_undefined_break_target(&[])
    }

    #[test_case("continue x;" => true; "one statement")]
    #[test_case(";" => false; "one statement (no continue)")]
    #[test_case("" => false; "no statements")]
    fn contains_undefined_continue_target(src: &str) -> bool {
        Maker::new(src).function_statement_list().contains_undefined_continue_target(&[], &[])
    }

    #[test_case("package;", true => sset(&[PACKAGE_NOT_ALLOWED]); "StatementList")]
    #[test_case("", true => sset(&[]); "[empty]")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).function_statement_list().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("" => false; "empty")]
    #[test_case("arguments;" => true; "stmt (yes)")]
    #[test_case("a;" => false; "stmt (no)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).function_statement_list().contains_arguments()
    }

    #[test_case("   return 3;" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 9 } }; "something")]
    #[test_case("   " => Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 0 } }; "empty")]
    fn location(src: &str) -> Location {
        Maker::new(src).function_statement_list().location()
    }

    #[test_case("var alpha, beta;" => svec(&["alpha", "beta"]); "var statement")]
    #[test_case("" => svec(&[]); "empty")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).function_statement_list().var_scoped_declarations().iter().map(String::from).collect()
    }

    #[test_case("let a, b, c; function pp(){}" => svec(&["let a , b , c ;"]); "stuff")]
    #[test_case("" => svec(&[]); "empty")]
    fn lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).function_statement_list().lexically_scoped_declarations().iter().map(String::from).collect()
    }
}
