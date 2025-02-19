#![expect(clippy::bool_assert_comparison)]
use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

// ARROW FUNCTION
#[test]
fn arrow_function_test_01() {
    let (node, scanner) = check(ArrowFunction::parse(&mut newparser("a=>a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    pretty_check(&*node, "ArrowFunction: a => a", &["ArrowParameters: a", "ConciseBody: a"]);
    concise_check(&*node, "ArrowFunction: a => a", &["IdentifierName: a", "Punctuator: =>", "IdentifierName: a"]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn arrow_function_test_02() {
    check_err(
        ArrowFunction::parse(&mut newparser(""), Scanner::new(), true, false, false),
        "Identifier or Formal Parameters expected",
        1,
        1,
    );
}
#[test]
fn arrow_function_test_03() {
    check_err(ArrowFunction::parse(&mut newparser("a"), Scanner::new(), true, false, false), "‘=>’ expected", 1, 2);
}
#[test]
fn arrow_function_test_04() {
    check_err(
        ArrowFunction::parse(&mut newparser("a=>"), Scanner::new(), true, false, false),
        "ConciseBody expected",
        1,
        4,
    );
}
#[test]
fn arrow_function_test_05() {
    check_err(
        ArrowFunction::parse(&mut newparser("a\n=>a"), Scanner::new(), true, false, false),
        "newline not allowed here",
        1,
        2,
    );
}
#[test]
fn arrow_function_test_prettyerrors_1() {
    let (item, _) = ArrowFunction::parse(&mut newparser("a=>a"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn arrow_function_test_conciseerrors_1() {
    let (item, _) = ArrowFunction::parse(&mut newparser("a=>a"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn arrow_function_test_contains_01() {
    let (item, _) = ArrowFunction::parse(&mut newparser("(a=this)=>a"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn arrow_function_test_contains_02() {
    let (item, _) = ArrowFunction::parse(&mut newparser("a=>this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn arrow_function_test_contains_03() {
    let (item, _) = ArrowFunction::parse(&mut newparser("a=>a"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn arrow_function_test_contains_04() {
    let (item, _) = ArrowFunction::parse(&mut newparser("a=>new.target"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::NewTarget), true);
}
#[test]
fn arrow_function_test_contains_05() {
    let (item, _) = ArrowFunction::parse(&mut newparser("a=>super.b"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::SuperProperty), true);
}
#[test]
fn arrow_function_test_contains_06() {
    let (item, _) = ArrowFunction::parse(&mut newparser("a=>super(0)"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::SuperCall), true);
}
#[test]
fn arrow_function_test_contains_07() {
    let (item, _) = ArrowFunction::parse(&mut newparser("a=>super(0)"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Super), true);
}
#[test]
fn arrow_function_test_contains_08() {
    let (item, _) = ArrowFunction::parse(&mut newparser("a=>10"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn arrow_function_test_contains_09() {
    let (item, _) = ArrowFunction::parse(&mut newparser("(a=10)=>a"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("(x=item.#valid) => x*2" => true; "Parameter valid")]
#[test_case("(x=item.#invalid) => x*2" => false; "Parameter invalid")]
#[test_case("x => x.#valid" => true; "Body valid")]
#[test_case("x => x.#invalid" => false; "Body invalid")]
fn arrow_function_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ArrowFunction::parse(&mut newparser(src), Scanner::new(), true, false, false).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}

mod arrow_function {
    use super::*;
    use test_case::test_case;

    const ILLEGAL_YIELD: &str = "Illegal yield expression in arrow function parameters";
    const ILLEGAL_AWAIT: &str = "Illegal await expression in arrow function parameters";
    const ILLEGAL_USE_STRICT: &str = "Illegal 'use strict' directive in function with non-simple parameter list";
    const A_DUPLICATED: &str = "‘a’ already defined";

    #[test_case("package => implements", true => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "ArrowParameters => ConciseBody")]
    #[test_case("(a=yield b) => a", false => sset(&[ILLEGAL_YIELD]); "Yield in params")]
    #[test_case("(a=await b) => a", false => sset(&[ILLEGAL_AWAIT]); "Await in params")]
    #[test_case("(...a) => { 'use strict'; }", false => sset(&[ILLEGAL_USE_STRICT]); "complex params")]
    #[test_case("a => { let a; }", false => sset(&[A_DUPLICATED]); "param/lex clash")]
    #[test_case("package => { 'use strict'; implements; }", false => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "strict mode trigger")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).arrow_function().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("(a=arguments) => a" => true; "left")]
    #[test_case("a => arguments" => true; "right")]
    #[test_case("a => a" => false; "none")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).arrow_function().contains_arguments()
    }

    #[test_case("   (x,y) => { return x(y); }" => Location { starting_line: 1, starting_column: 4, span: Span{ starting_index: 3, length: 25} }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).arrow_function().location()
    }
}

// ARROW PARAMETERS
#[test]
fn arrow_parameters_test_01() {
    let (node, scanner) = check(ArrowParameters::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, ArrowParameters::Identifier(..)));
    pretty_check(&*node, "ArrowParameters: a", &["BindingIdentifier: a"]);
    concise_check(&*node, "IdentifierName: a", &[]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn arrow_parameters_test_02() {
    let r = ArrowParameters::parse(&mut newparser("(a)"), Scanner::new(), false, false);
    let (node, scanner) = check(r);
    chk_scan(&scanner, 3);
    assert!(matches!(&*node, ArrowParameters::Formals(..)));
    pretty_check(&*node, "ArrowParameters: ( a )", &["ArrowFormalParameters: ( a )"]);
    concise_check(&*node, "ArrowFormalParameters: ( a )", &["Punctuator: (", "IdentifierName: a", "Punctuator: )"]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn arrow_parameters_test_err_01() {
    check_err(
        ArrowParameters::parse(&mut newparser(""), Scanner::new(), false, false),
        "Identifier or Formal Parameters expected",
        1,
        1,
    );
}
#[test]
fn arrow_parameters_test_err_02() {
    check_err(
        ArrowParameters::parse(&mut newparser("("), Scanner::new(), false, false),
        "Expression, spread pattern, or closing paren expected",
        1,
        2,
    );
}
#[test]
fn arrow_parameters_test_err_03() {
    check_err(ArrowParameters::parse(&mut newparser("(a"), Scanner::new(), false, false), "‘)’ expected", 1, 3);
}
#[test]
fn arrow_parameters_test_err_04() {
    check_err(ArrowParameters::parse(&mut newparser("(5 * 3)"), Scanner::new(), false, false), "‘)’ expected", 1, 2);
}
#[test]
fn arrow_parameters_test_prettyerrors_1() {
    let (item, _) = ArrowParameters::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn arrow_parameters_test_prettyerrors_2() {
    let (item, _) = ArrowParameters::parse(&mut newparser("(a)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn arrow_parameters_test_conciseerrors_1() {
    let (item, _) = ArrowParameters::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn arrow_parameters_test_conciseerrors_2() {
    let (item, _) = ArrowParameters::parse(&mut newparser("(a)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn arrow_parameters_test_contains_01() {
    let (item, _) = ArrowParameters::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn arrow_parameters_test_contains_02() {
    let (item, _) = ArrowParameters::parse(&mut newparser("(a=this)"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn arrow_parameters_test_contains_03() {
    let (item, _) = ArrowParameters::parse(&mut newparser("(a=0)"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("identifier" => true; "BindingIdentifier")]
#[test_case("(a=item.#valid)" => true; "ArrowFormalParameters valid")]
#[test_case("(a=item.#invalid)" => false; "ArrowFormalParameters invalid")]
fn arrow_parameters_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ArrowParameters::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}

mod arrow_parameters {
    use super::*;
    use test_case::test_case;

    #[test_case("x" => vec!["x"]; "BindingIdentifier")]
    #[test_case("(left, right)" => vec!["left", "right"]; "ArrowFormalParameters")]
    fn bound_names(src: &str) -> Vec<String> {
        Maker::new(src).arrow_parameters().bound_names().into_iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("x" => true; "simple id")]
    #[test_case("(x)" => true; "simple formals")]
    #[test_case("({x})" => false; "complex formals")]
    fn is_simple_parameter_list(src: &str) -> bool {
        Maker::new(src).arrow_parameters().is_simple_parameter_list()
    }

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "BindingIdentifier")]
    #[test_case("(package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "ArrowFormalParameters")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).arrow_parameters().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("a" => false; "id")]
    #[test_case("(a=arguments)" => true; "yes")]
    #[test_case("(a)" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).arrow_parameters().contains_arguments()
    }

    #[test_case(" x" => Location { starting_line: 1, starting_column: 2, span: Span{ starting_index: 1, length: 1 } }; "id")]
    #[test_case(" (x, y)" => Location { starting_line: 1, starting_column: 2, span: Span{ starting_index: 1, length: 6 } }; "formals")]
    fn location(src: &str) -> Location {
        Maker::new(src).arrow_parameters().location()
    }

    #[test_case("a" => 1.0; "identifier")]
    #[test_case("(a, b, c)" => 3.0; "formals")]
    #[test_case("(a, b=33, c)" => 1.0; "formals with initializer")]
    fn expected_argument_count(src: &str) -> f64 {
        Maker::new(src).arrow_parameters().expected_argument_count()
    }

    #[test_case("a" => false; "id")]
    #[test_case("(a)" => false; "formals; missing")]
    #[test_case("(a=0)" => true; "formals; present")]
    fn contains_expression(src: &str) -> bool {
        Maker::new(src).arrow_parameters().contains_expression()
    }
}

// CONCISE BODY
#[test]
fn concise_body_test_01() {
    let (node, scanner) = check(ConciseBody::parse(&mut newparser("a"), Scanner::new(), true));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, ConciseBody::Expression(..)));
    pretty_check(&*node, "ConciseBody: a", &["ExpressionBody: a"]);
    concise_check(&*node, "IdentifierName: a", &[]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn concise_body_test_02() {
    let (node, scanner) = check(ConciseBody::parse(&mut newparser("{q;}"), Scanner::new(), true));
    println!("node = {node:?}");
    chk_scan(&scanner, 4);
    assert!(matches!(&*node, ConciseBody::Function { .. }));
    pretty_check(&*node, "ConciseBody: { q ; }", &["FunctionBody: q ;"]);
    concise_check(&*node, "ConciseBody: { q ; }", &["Punctuator: {", "ExpressionStatement: q ;", "Punctuator: }"]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn concise_body_test_err_01() {
    check_err(ConciseBody::parse(&mut newparser(""), Scanner::new(), true), "ConciseBody expected", 1, 1);
}
#[test]
fn concise_body_test_err_02() {
    check_err(ConciseBody::parse(&mut newparser("{"), Scanner::new(), true), "‘}’ expected", 1, 2);
}
#[test]
fn concise_body_test_prettyerrors_1() {
    let (item, _) = ConciseBody::parse(&mut newparser("a"), Scanner::new(), true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn concise_body_test_prettyerrors_2() {
    let (item, _) = ConciseBody::parse(&mut newparser("{q;}"), Scanner::new(), true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn concise_body_test_conciseerrors_1() {
    let (item, _) = ConciseBody::parse(&mut newparser("a"), Scanner::new(), true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn concise_body_test_conciseerrors_2() {
    let (item, _) = ConciseBody::parse(&mut newparser("{q;}"), Scanner::new(), true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn concise_body_test_contains_01() {
    let (item, _) = ConciseBody::parse(&mut newparser("{ this.a = 12; }"), Scanner::new(), true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn concise_body_test_contains_02() {
    let (item, _) = ConciseBody::parse(&mut newparser("{ a = 12; }"), Scanner::new(), true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn concise_body_test_contains_03() {
    let (item, _) = ConciseBody::parse(&mut newparser("this.a"), Scanner::new(), true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn concise_body_test_contains_04() {
    let (item, _) = ConciseBody::parse(&mut newparser("a"), Scanner::new(), true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("item.#valid" => true; "ExpressionBody valid")]
#[test_case("item.#invalid" => false; "ExpressionBody invalid")]
#[test_case("{ item.#valid }" => true; "FunctionBody valid")]
#[test_case("{ item.#invalid }" => false; "FunctionBody invalid")]
fn concise_body_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ConciseBody::parse(&mut newparser(src), Scanner::new(), true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}

mod concise_body {
    use super::*;
    use test_case::test_case;

    #[test_case("3" => false; "ExpressionBody")]
    #[test_case("{ 'use strict'; }" => true; "{ FunctionBody }")]
    #[test_case("{ 3; 'use strict'; }" => false; "FunctionBody without")]
    fn concise_body_contains_use_strict(src: &str) -> bool {
        Maker::new(src).concise_body().concise_body_contains_use_strict()
    }

    #[test_case("expression" => Vec::<String>::new(); "ExpressionBody")]
    #[test_case("{ let a; }" => vec!["a"]; "{ FunctionBody }")]
    fn lexically_declared_names(src: &str) -> Vec<String> {
        Maker::new(src).concise_body().lexically_declared_names().into_iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "ExpressionBody")]
    #[test_case("{ package; }", true => sset(&[PACKAGE_NOT_ALLOWED]); "{ FunctionBody }")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).concise_body().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("a" => false; "Exp (no)")]
    #[test_case("{arguments;}" => true; "body (yes)")]
    #[test_case("{;}" => false; "body (no)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).concise_body().contains_arguments()
    }

    #[test_case("  p+3" => Location { starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 } }; "exp")]
    #[test_case("  { return p+3; }" => Location { starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 15 } }; "body")]
    fn location(src: &str) -> Location {
        Maker::new(src).concise_body().location()
    }

    #[test_case("x + 3" => svec(&[]); "expression body")]
    #[test_case("{ let a; const b=0; var c; function d() {} }" => svec(&["c", "d"]); "function body")]
    fn var_declared_names(src: &str) -> Vec<String> {
        Maker::new(src).concise_body().var_declared_names().into_iter().map(String::from).collect()
    }

    #[test_case("x + 3" => svec(&[]); "expression body")]
    #[test_case("{ let a; const b=0; var c; function d() {} }" => svec(&["c", "function d ( ) { }"]); "function body")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).concise_body().var_scoped_declarations().iter().map(String::from).collect()
    }

    #[test_case("x + 3" => svec(&[]); "expression body")]
    #[test_case("{ let a; const b=0; var c; function d() {} }" => svec(&["let a ;", "const b = 0 ;"]); "function body")]
    fn lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).concise_body().lexically_scoped_declarations().iter().map(String::from).collect()
    }
}

// EXPRESSION BODY
#[test]
fn expression_body_test_01() {
    let (node, scanner) = check(ExpressionBody::parse(&mut newparser("a"), Scanner::new(), true, false));
    chk_scan(&scanner, 1);
    pretty_check(&*node, "ExpressionBody: a", &["AssignmentExpression: a"]);
    concise_check(&*node, "IdentifierName: a", &[]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn expression_body_test_cache_01() {
    let mut parser = newparser("a+b+c+d+e");
    let (node, scanner) = check(ExpressionBody::parse(&mut parser, Scanner::new(), false, false));
    let (node2, scanner2) = check(ExpressionBody::parse(&mut parser, Scanner::new(), false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn expression_body_test_err_01() {
    check_err(
        ExpressionBody::parse(&mut newparser(""), Scanner::new(), true, false),
        "AssignmentExpression expected",
        1,
        1,
    );
}
#[test]
fn expression_body_test_prettyerrors_1() {
    let (item, _) = ExpressionBody::parse(&mut newparser("a"), Scanner::new(), true, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn expression_body_test_conciseerrors_1() {
    let (item, _) = ExpressionBody::parse(&mut newparser("a"), Scanner::new(), true, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn expression_body_test_contains_01() {
    let (item, _) = ExpressionBody::parse(&mut newparser("this.a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn expression_body_test_contains_02() {
    let (item, _) = ExpressionBody::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("item.#valid" => true; "ExpressionBody valid")]
#[test_case("item.#invalid" => false; "ExpressionBody invalid")]
fn expression_body_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ExpressionBody::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod expression_body {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "AssignmentExpression")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).expression_body().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("arguments" => true; "yes")]
    #[test_case("a" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).expression_body().contains_arguments()
    }

    #[test_case(" x+y" => Location { starting_line: 1, starting_column: 2, span: Span{ starting_index: 1, length: 3 } }; "expr")]
    fn location(src: &str) -> Location {
        Maker::new(src).expression_body().location()
    }
}

// ARROW FORMAL PARAMETERS
#[test]
fn arrow_formal_parameters_test_01() {
    let (node, scanner) = check(ArrowFormalParameters::parse(&mut newparser("(a,b)"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    pretty_check(&*node, "ArrowFormalParameters: ( a , b )", &["UniqueFormalParameters: a , b"]);
    concise_check(
        &*node,
        "ArrowFormalParameters: ( a , b )",
        &["Punctuator: (", "FormalParameterList: a , b", "Punctuator: )"],
    );
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn arrow_formal_parameters_test_cache_01() {
    let mut parser = newparser("(a,b)");
    let (node, scanner) = check(ArrowFormalParameters::parse(&mut parser, Scanner::new(), false, false));
    let (node2, scanner2) = check(ArrowFormalParameters::parse(&mut parser, Scanner::new(), false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn arrow_formal_parameters_test_err_01() {
    check_err(ArrowFormalParameters::parse(&mut newparser(""), Scanner::new(), false, false), "‘(’ expected", 1, 1);
}
#[test]
fn arrow_formal_parameters_test_err_02() {
    check_err(ArrowFormalParameters::parse(&mut newparser("("), Scanner::new(), false, false), "‘)’ expected", 1, 2);
}
#[test]
fn arrow_formal_parameters_test_prettyerrors_1() {
    let (item, _) = ArrowFormalParameters::parse(&mut newparser("(a)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn arrow_formal_parameters_test_conciseerrors_1() {
    let (item, _) = ArrowFormalParameters::parse(&mut newparser("(a)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn arrow_formal_parameters_test_contains_01() {
    let (item, _) = ArrowFormalParameters::parse(&mut newparser("(a=this)"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn arrow_formal_parameters_test_contains_02() {
    let (item, _) = ArrowFormalParameters::parse(&mut newparser("(a)"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("(a=item.#valid)" => true; "UniqueFormalParameters valid")]
#[test_case("(a=item.#invalid)" => false; "UniqueFormalParameters invalid")]
fn arrow_formal_parameters_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ArrowFormalParameters::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}

mod arrow_formal_parameters {
    use super::*;
    use test_case::test_case;

    #[test_case("(a,b)" => vec!["a", "b"]; "( UniqueFormalParameters )")]
    fn bound_names(src: &str) -> Vec<String> {
        Maker::new(src).arrow_formal_parameters().bound_names().into_iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("(a)" => true; "simple")]
    #[test_case("({a})" => false; "complex")]
    fn is_simple_parameter_list(src: &str) -> bool {
        Maker::new(src).arrow_formal_parameters().is_simple_parameter_list()
    }

    #[test_case("(package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "( UniqueFormalParameters )")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).arrow_formal_parameters().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("(a=arguments)" => true; "yes")]
    #[test_case("(a)" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).arrow_formal_parameters().contains_arguments()
    }

    #[test_case("  ( a = arguments) " => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 16 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).arrow_formal_parameters().location()
    }

    #[test_case("(a,b,...c)" => 2.0; "typical")]
    fn expected_argument_count(src: &str) -> f64 {
        Maker::new(src).arrow_formal_parameters().expected_argument_count()
    }

    #[test_case("(a)" => false; "missing")]
    #[test_case("(a=0)" => true; "present")]
    fn contains_expression(src: &str) -> bool {
        Maker::new(src).arrow_formal_parameters().contains_expression()
    }

    #[test_case("(a=0)" => "( a = 0 )"; "has args")]
    #[test_case("()" => "( )"; "no args")]
    fn display(src: &str) -> String {
        format!("{}", Maker::new(src).arrow_formal_parameters())
    }
}
