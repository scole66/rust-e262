use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

// GENERATOR METHOD
#[test]
fn generator_method_test_01() {
    let (node, scanner) = check(GeneratorMethod::parse(&mut newparser("*a(){}"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    pretty_check(
        &*node,
        "GeneratorMethod: * a (  ) {  }",
        &["ClassElementName: a", "UniqueFormalParameters: ", "GeneratorBody: "],
    );
    concise_check(
        &*node,
        "GeneratorMethod: * a (  ) {  }",
        &["Punctuator: *", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
    );
    format!("{node:?}");
}
#[test]
fn generator_method_test_02() {
    check_err(GeneratorMethod::parse(&mut newparser(""), Scanner::new(), false, false), "‘*’ expected", 1, 1);
}
#[test]
fn generator_method_test_03() {
    check_err(
        GeneratorMethod::parse(&mut newparser("*"), Scanner::new(), false, false),
        "ClassElementName expected",
        1,
        2,
    );
}
#[test]
fn generator_method_test_04() {
    check_err(GeneratorMethod::parse(&mut newparser("*a"), Scanner::new(), false, false), "‘(’ expected", 1, 3);
}
#[test]
fn generator_method_test_05() {
    check_err(GeneratorMethod::parse(&mut newparser("*a("), Scanner::new(), false, false), "‘)’ expected", 1, 4);
}
#[test]
fn generator_method_test_06() {
    check_err(GeneratorMethod::parse(&mut newparser("*a()"), Scanner::new(), false, false), "‘{’ expected", 1, 5);
}
#[test]
fn generator_method_test_07() {
    check_err(GeneratorMethod::parse(&mut newparser("*a(){"), Scanner::new(), false, false), "‘}’ expected", 1, 6);
}
#[test]
fn generator_method_test_prettyerrors_1() {
    let (item, _) = GeneratorMethod::parse(
        &mut newparser("* bob(blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
        false,
        false,
    )
    .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn generator_method_test_conciseerrors_1() {
    let (item, _) = GeneratorMethod::parse(
        &mut newparser("* bob(blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
        false,
        false,
    )
    .unwrap();
    concise_error_validate(&*item);
}
#[test]
fn generator_method_test_contains_01() {
    let (item, _) = GeneratorMethod::parse(&mut newparser("*[0](){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn generator_method_test_contains_02() {
    let (item, _) = GeneratorMethod::parse(&mut newparser("*a(x=0){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn generator_method_test_contains_03() {
    let (item, _) = GeneratorMethod::parse(&mut newparser("*a(){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn generator_method_test_contains_04() {
    let (item, _) = GeneratorMethod::parse(&mut newparser("*a(){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn generator_method_test_computed_property_contains_01() {
    let (item, _) = GeneratorMethod::parse(&mut newparser("*[0](){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn generator_method_test_computed_property_contains_02() {
    let (item, _) = GeneratorMethod::parse(&mut newparser("*[a](x=0){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test_case("*[a.#valid](){}" => true; "name valid")]
#[test_case("*a(b=c.#valid){}" => true; "params valid")]
#[test_case("*a(){b.#valid;}" => true; "body valid")]
#[test_case("*[a.#invalid](){}" => false; "name invalid")]
#[test_case("*a(b=c.#invalid){}" => false; "params invalid")]
#[test_case("*a(){b.#invalid;}" => false; "body invalid")]
fn generator_method_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = GeneratorMethod::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod generator_method {
    use super::*;
    use test_case::test_case;

    #[test_case("*a(){}" => false; "without")]
    #[test_case("*a(b=super(0)){}" => true; "params")]
    #[test_case("*a(){super(1);}" => true; "body")]
    fn has_direct_super(src: &str) -> bool {
        Maker::new(src).generator_method().has_direct_super()
    }

    #[test_case("*[package](implements) {interface;}", true => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "named function")]
    #[test_case("*a(...b){}", true => sset(&[]); "complex params in strict mode")]
    #[test_case("*a(...b){'use strict';}", true => sset(&[BAD_USE_STRICT]); "complex params with use strict (strict mode)")]
    #[test_case("*a(...b){'use strict';}", false => sset(&[BAD_USE_STRICT]); "complex params with use strict (non-strict mode)")]
    #[test_case("*a(a){let a=3; const p=0;}", true => sset(&[A_ALREADY_DEFN]); "lexname shadowing params")]
    #[test_case("*a(b=yield 10){}", false => sset(&[YIELD_IN_GENPARAM]); "yield in generator params")]
    #[test_case("*a(b){super(b);}", false => sset(&[UNEXPECTED_SUPER]); "direct super")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).generator_method().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test]
    fn prop_name() {
        let item = Maker::new("*a(){}").generator_method();
        assert_eq!(item.prop_name(), Some(JSString::from("a")));
    }

    #[test_case("*[arguments](){}" => true; "yes")]
    #[test_case("*a(){}" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).generator_method().contains_arguments()
    }

    #[test_case("*#PRIVATE(x=0){0;}" => Some("#PRIVATE".to_string()); "private")]
    #[test_case("*public(){}" => None; "public")]
    fn private_bound_identifier(src: &str) -> Option<String> {
        Maker::new(src).generator_method().private_bound_identifier().map(String::from)
    }

    #[test_case("   *a(){}" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 6 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).generator_method().location()
    }
}

// GENERATOR DECLARATION
#[test]
fn generator_declaration_test_01() {
    let (node, scanner) =
        check(GeneratorDeclaration::parse(&mut newparser("function *a(){}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 15);
    pretty_check(
        &*node,
        "GeneratorDeclaration: function * a (  ) {  }",
        &["BindingIdentifier: a", "FormalParameters: ", "GeneratorBody: "],
    );
    concise_check(
        &*node,
        "GeneratorDeclaration: function * a (  ) {  }",
        &[
            "Keyword: function",
            "Punctuator: *",
            "IdentifierName: a",
            "Punctuator: (",
            "Punctuator: )",
            "Punctuator: {",
            "Punctuator: }",
        ],
    );
    format!("{node:?}");
}
#[test]
fn generator_declaration_test_02() {
    let (node, scanner) =
        check(GeneratorDeclaration::parse(&mut newparser("function *(){}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 14);
    pretty_check(&*node, "GeneratorDeclaration: function * (  ) {  }", &["FormalParameters: ", "GeneratorBody: "]);
    concise_check(
        &*node,
        "GeneratorDeclaration: function * (  ) {  }",
        &["Keyword: function", "Punctuator: *", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
    );
    format!("{node:?}");
}
#[test]
fn generator_declaration_test_03() {
    check_err(
        GeneratorDeclaration::parse(&mut newparser(""), Scanner::new(), false, false, true),
        "‘function’ expected",
        1,
        1,
    );
}
#[test]
fn generator_declaration_test_04() {
    check_err(
        GeneratorDeclaration::parse(&mut newparser("function"), Scanner::new(), false, false, true),
        "‘*’ expected",
        1,
        9,
    );
}
#[test]
fn generator_declaration_test_05() {
    check_err(
        GeneratorDeclaration::parse(&mut newparser("function *"), Scanner::new(), false, false, true),
        "‘(’ expected",
        1,
        11,
    );
}
#[test]
fn generator_declaration_test_06() {
    check_err(
        GeneratorDeclaration::parse(&mut newparser("function * h"), Scanner::new(), false, false, true),
        "‘(’ expected",
        1,
        13,
    );
}
#[test]
fn generator_declaration_test_07() {
    check_err(
        GeneratorDeclaration::parse(&mut newparser("function * h ("), Scanner::new(), false, false, true),
        "‘)’ expected",
        1,
        15,
    );
}
#[test]
fn generator_declaration_test_075() {
    check_err(
        GeneratorDeclaration::parse(&mut newparser("function * ("), Scanner::new(), false, false, true),
        "‘)’ expected",
        1,
        13,
    );
}
#[test]
fn generator_declaration_test_076() {
    check_err(
        GeneratorDeclaration::parse(&mut newparser("function * ("), Scanner::new(), false, false, false),
        "not an identifier",
        1,
        12,
    );
}
#[test]
fn generator_declaration_test_08() {
    check_err(
        GeneratorDeclaration::parse(&mut newparser("function * h ( u"), Scanner::new(), false, false, true),
        "‘)’ expected",
        1,
        17,
    );
}
#[test]
fn generator_declaration_test_09() {
    check_err(
        GeneratorDeclaration::parse(&mut newparser("function * h ( u )"), Scanner::new(), false, false, true),
        "‘{’ expected",
        1,
        19,
    );
}
#[test]
fn generator_declaration_test_10() {
    check_err(
        GeneratorDeclaration::parse(&mut newparser("function * h ( u ) {"), Scanner::new(), false, false, true),
        "‘}’ expected",
        1,
        21,
    );
}
#[test]
fn generator_declaration_test_11() {
    check_err(
        GeneratorDeclaration::parse(&mut newparser("function * h ( u ) { z;"), Scanner::new(), false, false, true),
        "‘}’ expected",
        1,
        24,
    );
}
#[test]
fn generator_declaration_test_prettyerrors_1() {
    let (item, _) = GeneratorDeclaration::parse(
        &mut newparser("function * bob(blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
        false,
        false,
        true,
    )
    .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn generator_declaration_test_prettyerrors_2() {
    let (item, _) = GeneratorDeclaration::parse(
        &mut newparser("function * (blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
        false,
        false,
        true,
    )
    .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn generator_declaration_test_conciseerrors_1() {
    let (item, _) = GeneratorDeclaration::parse(
        &mut newparser("function * bob(blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
        false,
        false,
        true,
    )
    .unwrap();
    concise_error_validate(&*item);
}
#[test]
fn generator_declaration_test_conciseerrors_2() {
    let (item, _) = GeneratorDeclaration::parse(
        &mut newparser("function * (blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
        false,
        false,
        true,
    )
    .unwrap();
    concise_error_validate(&*item);
}
#[test]
fn generator_declaration_test_bound_names_01() {
    let (item, _) =
        GeneratorDeclaration::parse(&mut newparser("function * bob() {}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["bob"]);
}
#[test]
fn generator_declaration_test_bound_names_02() {
    let (item, _) =
        GeneratorDeclaration::parse(&mut newparser("function * () {}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["*default*"]);
}
#[test]
fn generator_declaration_test_contains_01() {
    let (item, _) =
        GeneratorDeclaration::parse(&mut newparser("function * a(x=0) {0;}"), Scanner::new(), true, true, true)
            .unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("function *a(b=c.#valid){}" => true; "params valid")]
#[test_case("function *a(){b.#valid;}" => true; "body valid")]
#[test_case("function *a(b=c.#invalid){}" => false; "params invalid")]
#[test_case("function *a(){b.#invalid;}" => false; "body invalid")]
fn generator_declaration_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = GeneratorDeclaration::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod generator_declaration {
    use super::*;
    use test_case::test_case;

    #[test_case("function *package(implements) {interface;}", true => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "named function")]
    #[test_case("function *(implements) {interface;}", true => sset(&[IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "anonymous function")]
    #[test_case("function *a(a,a){}", true => sset(&[A_ALREADY_DEFN]); "duplicated params (strict)")]
    #[test_case("function *a(a,a){}", false => sset(&[]); "duplicated params (non-strict)")]
    #[test_case("function *a(...b){}", true => sset(&[]); "complex params in strict mode")]
    #[test_case("function *a(...b){'use strict';}", true => sset(&[BAD_USE_STRICT]); "complex params with use strict (strict mode)")]
    #[test_case("function *a(...b){'use strict';}", false => sset(&[BAD_USE_STRICT]); "complex params with use strict (non-strict mode)")]
    #[test_case("function *a(a){let a=3; const p=0;}", true => sset(&[A_ALREADY_DEFN]); "lexname shadowing params")]
    #[test_case("function *a(b=super()){}", false => sset(&[UNEXPECTED_SUPER2]); "SuperCall in params")]
    #[test_case("function *a(b=super.c){}", false => sset(&[UNEXPECTED_SUPER2]); "SuperProperty in params")]
    #[test_case("function *a(){super();}", false => sset(&[UNEXPECTED_SUPER2]); "SuperCall in body")]
    #[test_case("function *a(){super.b;}", false => sset(&[UNEXPECTED_SUPER2]); "SuperProperty in body")]
    #[test_case("function *a(b=yield 10){}", false => sset(&[YIELD_IN_GENPARAM]); "yield in generator params")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).generator_declaration().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("   function *a(){}" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 15 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).generator_declaration().location()
    }

    #[test_case("function *named() {}" => "named"; "named")]
    #[test_case("function *(){}" => "*default*"; "unnamed")]
    fn bound_name(src: &str) -> String {
        Maker::new(src).generator_declaration().bound_name().into()
    }

    #[test_case("function *x() {}" => false; "typical")]
    fn is_constant_declaration(src: &str) -> bool {
        Maker::new(src).generator_declaration().is_constant_declaration()
    }
}

// GENERATOR EXPRESSION
#[test]
fn generator_expression_test_01() {
    let (node, scanner) = check(GeneratorExpression::parse(&mut newparser("function *a(){}"), Scanner::new()));
    chk_scan(&scanner, 15);
    pretty_check(
        &*node,
        "GeneratorExpression: function * a (  ) {  }",
        &["BindingIdentifier: a", "FormalParameters: ", "GeneratorBody: "],
    );
    concise_check(
        &*node,
        "GeneratorExpression: function * a (  ) {  }",
        &[
            "Keyword: function",
            "Punctuator: *",
            "IdentifierName: a",
            "Punctuator: (",
            "Punctuator: )",
            "Punctuator: {",
            "Punctuator: }",
        ],
    );
    format!("{node:?}");
    assert!(node.is_function_definition());
}
#[test]
fn generator_expression_test_02() {
    let (node, scanner) = check(GeneratorExpression::parse(&mut newparser("function *(){}"), Scanner::new()));
    chk_scan(&scanner, 14);
    pretty_check(&*node, "GeneratorExpression: function * (  ) {  }", &["FormalParameters: ", "GeneratorBody: "]);
    concise_check(
        &*node,
        "GeneratorExpression: function * (  ) {  }",
        &["Keyword: function", "Punctuator: *", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
    );
    format!("{node:?}");
    assert!(node.is_function_definition());
}
#[test]
fn generator_expression_test_03() {
    check_err(GeneratorExpression::parse(&mut newparser(""), Scanner::new()), "‘function’ expected", 1, 1);
}
#[test]
fn generator_expression_test_04() {
    check_err(GeneratorExpression::parse(&mut newparser("function"), Scanner::new()), "‘*’ expected", 1, 9);
}
#[test]
fn generator_expression_test_05() {
    check_err(GeneratorExpression::parse(&mut newparser("function *"), Scanner::new()), "‘(’ expected", 1, 11);
}
#[test]
fn generator_expression_test_06() {
    check_err(GeneratorExpression::parse(&mut newparser("function * h"), Scanner::new()), "‘(’ expected", 1, 13);
}
#[test]
fn generator_expression_test_07() {
    check_err(GeneratorExpression::parse(&mut newparser("function * h ("), Scanner::new()), "‘)’ expected", 1, 15);
}
#[test]
fn generator_expression_test_08() {
    check_err(GeneratorExpression::parse(&mut newparser("function * h ( u"), Scanner::new()), "‘)’ expected", 1, 17);
}
#[test]
fn generator_expression_test_09() {
    check_err(GeneratorExpression::parse(&mut newparser("function * h ( u )"), Scanner::new()), "‘{’ expected", 1, 19);
}
#[test]
fn generator_expression_test_10() {
    check_err(
        GeneratorExpression::parse(&mut newparser("function * h ( u ) {"), Scanner::new()),
        "‘}’ expected",
        1,
        21,
    );
}
#[test]
fn generator_expression_test_11() {
    check_err(
        GeneratorExpression::parse(&mut newparser("function * h ( u ) { z;"), Scanner::new()),
        "‘}’ expected",
        1,
        24,
    );
}
#[test]
fn generator_expression_test_prettyerrors_1() {
    let (item, _) = GeneratorExpression::parse(
        &mut newparser("function * bob(blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
    )
    .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn generator_expression_test_prettyerrors_2() {
    let (item, _) = GeneratorExpression::parse(
        &mut newparser("function * (blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
    )
    .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn generator_expression_test_conciseerrors_1() {
    let (item, _) = GeneratorExpression::parse(
        &mut newparser("function * bob(blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
    )
    .unwrap();
    concise_error_validate(&*item);
}
#[test]
fn generator_expression_test_conciseerrors_2() {
    let (item, _) = GeneratorExpression::parse(
        &mut newparser("function * (blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
    )
    .unwrap();
    concise_error_validate(&*item);
}
#[test]
fn generator_expression_test_contains_01() {
    let (item, _) = GeneratorExpression::parse(&mut newparser("function * a(x=0) {0;}"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("function *a(b=c.#valid){}" => true; "params valid")]
#[test_case("function *a(){b.#valid;}" => true; "body valid")]
#[test_case("function *a(b=c.#invalid){}" => false; "params invalid")]
#[test_case("function *a(){b.#invalid;}" => false; "body invalid")]
fn generator_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = GeneratorExpression::parse(&mut newparser(src), Scanner::new()).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod generator_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("function *package(implements) {interface;}", true => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "named function")]
    #[test_case("function *(implements) {interface;}", true => sset(&[IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "anonymous function")]
    #[test_case("function *a(a,a){}", true => sset(&[A_ALREADY_DEFN]); "duplicated params (strict)")]
    #[test_case("function *a(a,a){}", false => sset(&[]); "duplicated params (non-strict)")]
    #[test_case("function *a(...b){}", true => sset(&[]); "complex params in strict mode")]
    #[test_case("function *a(...b){'use strict';}", true => sset(&[BAD_USE_STRICT]); "complex params with use strict (strict mode)")]
    #[test_case("function *a(...b){'use strict';}", false => sset(&[BAD_USE_STRICT]); "complex params with use strict (non-strict mode)")]
    #[test_case("function *a(a){let a=3; const p=0;}", true => sset(&[A_ALREADY_DEFN]); "lexname shadowing params")]
    #[test_case("function *a(b=super()){}", false => sset(&[UNEXPECTED_SUPER2]); "SuperCall in params")]
    #[test_case("function *a(b=super.c){}", false => sset(&[UNEXPECTED_SUPER2]); "SuperProperty in params")]
    #[test_case("function *a(){super();}", false => sset(&[UNEXPECTED_SUPER2]); "SuperCall in body")]
    #[test_case("function *a(){super.b;}", false => sset(&[UNEXPECTED_SUPER2]); "SuperProperty in body")]
    #[test_case("function *a(b=yield 10){}", false => sset(&[YIELD_IN_GENPARAM]); "yield in generator params")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).generator_expression().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("function *a(){}" => true; "named")]
    #[test_case("function *(){}" => false; "unnamed")]
    fn is_named_function(src: &str) -> bool {
        Maker::new(src).generator_expression().is_named_function()
    }

    #[test_case("   function *a(){}" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 15 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).generator_expression().location()
    }
}

// GENERATOR BODY
#[test]
fn generator_body_test_01() {
    let (node, scanner) = GeneratorBody::parse(&mut newparser("yield 1;"), Scanner::new());
    chk_scan(&scanner, 8);
    pretty_check(&*node, "GeneratorBody: yield 1 ;", &["FunctionBody: yield 1 ;"]);
    concise_check(&*node, "ExpressionStatement: yield 1 ;", &["YieldExpression: yield 1", "Punctuator: ;"]);
    format!("{node:?}");
}
#[test]
fn generator_body_test_prettyerrors_1() {
    let (item, _) = GeneratorBody::parse(&mut newparser("yield 1;"), Scanner::new());
    pretty_error_validate(&*item);
}
#[test]
fn generator_body_test_conciseerrors_1() {
    let (item, _) = GeneratorBody::parse(&mut newparser("yield 1;"), Scanner::new());
    concise_error_validate(&*item);
}
#[test]
fn generator_body_test_cache_01() {
    let mut parser = newparser("yield 1;");
    let (node, scanner) = GeneratorBody::parse(&mut parser, Scanner::new());
    let (node2, scanner2) = GeneratorBody::parse(&mut parser, Scanner::new());
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn generator_body_test_contains_01() {
    let (item, _) = GeneratorBody::parse(&mut newparser("yield 1;"), Scanner::new());
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn generator_body_test_contains_02() {
    let (item, _) = GeneratorBody::parse(&mut newparser("yield a;"), Scanner::new());
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("a.#valid" => true; "valid")]
#[test_case("a.#invalid" => false; "invalid")]
fn generator_body_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = GeneratorBody::parse(&mut newparser(src), Scanner::new());
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod generator_body {
    use super::*;
    use test_case::test_case;

    #[test_case("package;", true => sset(&[PACKAGE_NOT_ALLOWED]); "statements")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).generator_body().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("'one'; 3;" => false; "directive no strict")]
    #[test_case("12;" => false; "no prologue")]
    #[test_case("'a'; 'use strict';" => true; "good strict")]
    #[test_case("'use\\x20strict';" => false; "bad strict")]
    fn function_body_contains_use_strict(src: &str) -> bool {
        Maker::new(src).generator_body().function_body_contains_use_strict()
    }

    #[test_case("var a; setup(); let alpha=\"a\"; const BETA='β';" => svec(&["alpha", "BETA"]); "normal")]
    fn lexically_declared_names(src: &str) -> Vec<String> {
        Maker::new(src).generator_body().lexically_declared_names().into_iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("let a; var b; const c; function d() {}" => svec(&["let a ;", "const c ;"]); "typical")]
    fn lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).generator_body().lexically_scoped_declarations().iter().map(String::from).collect()
    }

    #[test_case("   yield 1;" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 8 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).generator_body().location()
    }

    #[test_case("let a; const b=0; var c; function d() {}" => svec(&["c", "d"]); "function body")]
    fn var_declared_names(src: &str) -> Vec<String> {
        Maker::new(src).generator_body().var_declared_names().into_iter().map(String::from).collect()
    }

    #[test_case("let a; const b=0; var c; function d() {}" => svec(&["c", "function d (  ) {  }"]); "function body")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).generator_body().var_scoped_declarations().iter().map(String::from).collect()
    }
}

// YIELD EXPRESSION
#[test]
fn yield_expression_test_01() {
    let (pn, scanner) = check(YieldExpression::parse(&mut newparser("yield"), Scanner::new(), true, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*pn, YieldExpression::Simple { .. }));
    pretty_check(&*pn, "YieldExpression: yield", &[]);
    concise_check(&*pn, "Keyword: yield", &[]);
    format!("{pn:?}");
}
#[test]
fn yield_expression_test_02() {
    let (pn, scanner) = check(YieldExpression::parse(&mut newparser("yield 5"), Scanner::new(), true, false));
    chk_scan(&scanner, 7);
    assert!(matches!(&*pn, YieldExpression::Expression { .. }));
    pretty_check(&*pn, "YieldExpression: yield 5", &["AssignmentExpression: 5"]);
    concise_check(&*pn, "YieldExpression: yield 5", &["Keyword: yield", "Numeric: 5"]);
    format!("{pn:?}");
}
#[test]
fn yield_expression_test_03() {
    let (pn, scanner) = check(YieldExpression::parse(&mut newparser("yield *5"), Scanner::new(), true, false));
    chk_scan(&scanner, 8);
    assert!(matches!(&*pn, YieldExpression::From { .. }));
    pretty_check(&*pn, "YieldExpression: yield * 5", &["AssignmentExpression: 5"]);
    concise_check(&*pn, "YieldExpression: yield * 5", &["Keyword: yield", "Punctuator: *", "Numeric: 5"]);
    format!("{pn:?}");
}
#[test]
fn yield_expression_test_04() {
    let (pn, scanner) = check(YieldExpression::parse(&mut newparser("yield \n*5"), Scanner::new(), true, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*pn, YieldExpression::Simple { .. }));
    pretty_check(&*pn, "YieldExpression: yield", &[]);
    concise_check(&*pn, "Keyword: yield", &[]);
    format!("{pn:?}");
}
#[test]
fn yield_expression_test_05() {
    let (pn, scanner) = check(YieldExpression::parse(&mut newparser("yield @"), Scanner::new(), true, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*pn, YieldExpression::Simple { .. }));
    pretty_check(&*pn, "YieldExpression: yield", &[]);
    concise_check(&*pn, "Keyword: yield", &[]);
    format!("{pn:?}");
}
#[test]
fn yield_expression_test_06() {
    let (pn, scanner) = check(YieldExpression::parse(&mut newparser("yield *@"), Scanner::new(), true, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*pn, YieldExpression::Simple { .. }));
    pretty_check(&*pn, "YieldExpression: yield", &[]);
    concise_check(&*pn, "Keyword: yield", &[]);
    format!("{pn:?}");
}
#[test]
fn yield_expression_test_07() {
    check_err(YieldExpression::parse(&mut newparser(""), Scanner::new(), true, false), "‘yield’ expected", 1, 1);
}
#[test]
fn yield_expression_test_prettyerrors_1() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield"), Scanner::new(), true, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn yield_expression_test_prettyerrors_2() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield a"), Scanner::new(), true, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn yield_expression_test_prettyerrors_3() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield *a"), Scanner::new(), true, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn yield_expression_test_conciseerrors_1() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield"), Scanner::new(), true, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn yield_expression_test_conciseerrors_2() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield a"), Scanner::new(), true, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn yield_expression_test_conciseerrors_3() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield *a"), Scanner::new(), true, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn yield_expression_test_contains_01() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield 1;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn yield_expression_test_contains_02() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield a;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn yield_expression_test_contains_03() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield *1;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn yield_expression_test_contains_04() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield *a;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn yield_expression_test_contains_05() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn yield_expression_test_contains_06() {
    let item = YieldExpression::parse(&mut newparser("yield 3"), Scanner::new(), true, true).unwrap().0;
    assert_eq!(item.contains(ParseNodeKind::YieldExpression), true);
}
#[test_case("yield" => true; "Yield only")]
#[test_case("yield a.#valid" => true; "expression valid")]
#[test_case("yield *a.#valid" => true; "from valid")]
#[test_case("yield a.#invalid" => false; "expression invalid")]
#[test_case("yield *a.#invalid" => false; "from invalid")]
fn yield_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = YieldExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod yield_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("yield package", true => sset(&[PACKAGE_NOT_ALLOWED]); "yield exp")]
    #[test_case("yield", true => sset(&[]); "no expresion")]
    #[test_case("yield *package", true => sset(&[PACKAGE_NOT_ALLOWED]); "yield from")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).yield_expression().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("yield" => false; "bare")]
    #[test_case("yield arguments" => true; "yield x (yes)")]
    #[test_case("yield a" => false; "yield x (no)")]
    #[test_case("yield *arguments" => true; "yield from (yes)")]
    #[test_case("yield *a" => false; "yield from (no)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).yield_expression().contains_arguments()
    }

    #[test_case("   yield" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 5 } }; "simple")]
    #[test_case("   yield 1" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 7 } }; "value")]
    #[test_case("   yield *u" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 8 } }; "from")]
    fn location(src: &str) -> Location {
        Maker::new(src).yield_expression().location()
    }
}
