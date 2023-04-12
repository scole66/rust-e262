use super::testhelp::*;
use super::*;
use crate::prettyprint::testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

// ASYNC GENERATOR METHOD
#[test]
fn async_generator_method_test_01() {
    let (node, scanner) =
        check(AsyncGeneratorMethod::parse(&mut newparser("async *a(){}"), Scanner::new(), false, false));
    chk_scan(&scanner, 6 + 6);
    pretty_check(
        &*node,
        "AsyncGeneratorMethod: async * a (  ) {  }",
        vec!["ClassElementName: a", "UniqueFormalParameters: ", "AsyncGeneratorBody: "],
    );
    concise_check(
        &*node,
        "AsyncGeneratorMethod: async * a (  ) {  }",
        vec![
            "Keyword: async",
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
fn async_generator_method_test_025() {
    check_err(AsyncGeneratorMethod::parse(&mut newparser(""), Scanner::new(), false, false), "‘async’ expected", 1, 1);
}
#[test]
fn async_generator_method_test_02() {
    check_err(AsyncGeneratorMethod::parse(&mut newparser("async"), Scanner::new(), false, false), "‘*’ expected", 1, 6);
}
#[test]
fn async_generator_method_test_03() {
    check_err(
        AsyncGeneratorMethod::parse(&mut newparser("async *"), Scanner::new(), false, false),
        "ClassElementName expected",
        1,
        2 + 6,
    );
}
#[test]
fn async_generator_method_test_04() {
    check_err(
        AsyncGeneratorMethod::parse(&mut newparser("async *a"), Scanner::new(), false, false),
        "‘(’ expected",
        1,
        3 + 6,
    );
}
#[test]
fn async_generator_method_test_05() {
    check_err(
        AsyncGeneratorMethod::parse(&mut newparser("async *a("), Scanner::new(), false, false),
        "‘)’ expected",
        1,
        4 + 6,
    );
}
#[test]
fn async_generator_method_test_06() {
    check_err(
        AsyncGeneratorMethod::parse(&mut newparser("async *a()"), Scanner::new(), false, false),
        "‘{’ expected",
        1,
        5 + 6,
    );
}
#[test]
fn async_generator_method_test_07() {
    check_err(
        AsyncGeneratorMethod::parse(&mut newparser("async *a(){"), Scanner::new(), false, false),
        "‘}’ expected",
        1,
        6 + 6,
    );
}
#[test]
fn async_generator_method_test_prettyerrors_1() {
    let (item, _) = AsyncGeneratorMethod::parse(
        &mut newparser("async * bob(blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
        false,
        false,
    )
    .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_generator_method_test_conciseerrors_1() {
    let (item, _) = AsyncGeneratorMethod::parse(
        &mut newparser("async * bob(blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
        false,
        false,
    )
    .unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_generator_method_test_contains_01() {
    let (item, _) =
        AsyncGeneratorMethod::parse(&mut newparser("async * [10]() { return; }"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn async_generator_method_test_contains_02() {
    let (item, _) =
        AsyncGeneratorMethod::parse(&mut newparser("async * a(b=10) { return; }"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn async_generator_method_test_contains_03() {
    let (item, _) =
        AsyncGeneratorMethod::parse(&mut newparser("async * a() { return 10; }"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn async_generator_method_test_contains_04() {
    let (item, _) =
        AsyncGeneratorMethod::parse(&mut newparser("async * a() { return; }"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn async_generator_method_test_computed_property_contains_01() {
    let (item, _) =
        AsyncGeneratorMethod::parse(&mut newparser("async * [10]() { return; }"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn async_generator_method_test_comptued_property_contains_02() {
    let (item, _) =
        AsyncGeneratorMethod::parse(&mut newparser("async * a(b=10) { return; }"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test]
fn async_generator_method_test_comptued_property_contains_03() {
    let (item, _) =
        AsyncGeneratorMethod::parse(&mut newparser("async * a() { return 10; }"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test]
fn async_generator_method_test_comptued_property_contains_04() {
    let (item, _) =
        AsyncGeneratorMethod::parse(&mut newparser("async * a() { return; }"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test_case("async *[item.#valid](){}" => true; "Name valid")]
#[test_case("async *a(arg=item.#valid){}" => true; "Params valid")]
#[test_case("async *a(arg){arg.#valid;}" => true; "Body valid")]
#[test_case("async *[item.#invalid](){}" => false; "Name invalid")]
#[test_case("async *a(arg=item.#invalid){}" => false; "Params invalid")]
#[test_case("async *a(arg){arg.#invalid;}" => false; "Body invalid")]
fn async_generator_method_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = AsyncGeneratorMethod::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod async_generator_method {
    use super::*;
    use test_case::test_case;

    #[test_case("async *a(){}" => false; "without")]
    #[test_case("async *a(b=super(true)){}" => true; "params")]
    #[test_case("async *a(){super(false);}" => true; "body")]
    fn has_direct_super(src: &str) -> bool {
        let (item, _) = AsyncGeneratorMethod::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.has_direct_super()
    }

    #[test_case("async *a(){}", false => sset(&[]); "all good")]
    #[test_case("async *a(b=super()){}", false => sset(&[UNEXPECTED_SUPER]); "direct super")]
    #[test_case("async *a(b=yield 3){}", false => sset(&[YIELD_IN_GENPARAM]); "yield in param")]
    #[test_case("async *a(...b){'use strict';}", false => sset(&[BAD_USE_STRICT]); "complex params")]
    #[test_case("async *b(a){let a;}", false => sset(&[A_ALREADY_DEFN]); "param/body var clash")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        let node = Maker::new(src).async_generator_method();
        node.early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test]
    fn prop_name() {
        let (item, _) =
            AsyncGeneratorMethod::parse(&mut newparser("async *a(){}"), Scanner::new(), true, true).unwrap();
        assert_eq!(item.prop_name(), Some(JSString::from("a")));
    }

    #[test_case("async *[arguments](){}" => true; "yes")]
    #[test_case("async *a(){}" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).async_generator_method().contains_arguments()
    }

    #[test_case("async * #private() { return; }" => Some("#private".to_string()); "private")]
    #[test_case("async * public() {}" => None; "public")]
    fn private_bound_identifier(src: &str) -> Option<String> {
        Maker::new(src).async_generator_method().private_bound_identifier().map(String::from)
    }

    #[test_case("   async *a(){}" => Location { starting_line: 1, starting_column: 4, span: Span{ starting_index: 3, length: 12 }})]
    fn location(src: &str) -> Location {
        Maker::new(src).async_generator_method().location()
    }
}

// ASYNC GENERATOR DECLARATION
#[test]
fn async_generator_declaration_test_01() {
    let (node, scanner) = check(AsyncGeneratorDeclaration::parse(
        &mut newparser("async function *a(){}"),
        Scanner::new(),
        false,
        false,
        true,
    ));
    chk_scan(&scanner, 15 + 6);
    pretty_check(
        &*node,
        "AsyncGeneratorDeclaration: async function * a (  ) {  }",
        vec!["BindingIdentifier: a", "FormalParameters: ", "AsyncGeneratorBody: "],
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
    assert!(node.is_function_definition());
}
#[test]
fn async_generator_declaration_test_02() {
    let (node, scanner) = check(AsyncGeneratorDeclaration::parse(
        &mut newparser("async function *(){}"),
        Scanner::new(),
        false,
        false,
        true,
    ));
    chk_scan(&scanner, 14 + 6);
    pretty_check(
        &*node,
        "AsyncGeneratorDeclaration: async function * (  ) {  }",
        vec!["FormalParameters: ", "AsyncGeneratorBody: "],
    );
    concise_check(
        &*node,
        "AsyncGeneratorDeclaration: async function * (  ) {  }",
        vec![
            "Keyword: async",
            "Keyword: function",
            "Punctuator: *",
            "Punctuator: (",
            "Punctuator: )",
            "Punctuator: {",
            "Punctuator: }",
        ],
    );
    format!("{:?}", node);
    assert!(node.is_function_definition());
}
#[test]
fn async_generator_declaration_test_03() {
    check_err(
        AsyncGeneratorDeclaration::parse(&mut newparser(""), Scanner::new(), false, false, true),
        "‘async’ expected",
        1,
        1,
    );
}
#[test]
fn async_generator_declaration_test_04() {
    check_err(
        AsyncGeneratorDeclaration::parse(&mut newparser("async function"), Scanner::new(), false, false, true),
        "‘*’ expected",
        1,
        9 + 6,
    );
}
#[test]
fn async_generator_declaration_test_041() {
    check_err(
        AsyncGeneratorDeclaration::parse(&mut newparser("async \nfunction"), Scanner::new(), false, false, true),
        "newline not allowed here",
        1,
        6,
    );
}
#[test]
fn async_generator_declaration_test_05() {
    check_err(
        AsyncGeneratorDeclaration::parse(&mut newparser("async function *"), Scanner::new(), false, false, true),
        "‘(’ expected",
        1,
        11 + 6,
    );
}
#[test]
fn async_generator_declaration_test_06() {
    check_err(
        AsyncGeneratorDeclaration::parse(&mut newparser("async function * h"), Scanner::new(), false, false, true),
        "‘(’ expected",
        1,
        13 + 6,
    );
}
#[test]
fn async_generator_declaration_test_07() {
    check_err(
        AsyncGeneratorDeclaration::parse(&mut newparser("async function * h ("), Scanner::new(), false, false, true),
        "‘)’ expected",
        1,
        15 + 6,
    );
}
#[test]
fn async_generator_declaration_test_075() {
    check_err(
        AsyncGeneratorDeclaration::parse(&mut newparser("async function * ("), Scanner::new(), false, false, true),
        "‘)’ expected",
        1,
        13 + 6,
    );
}
#[test]
fn async_generator_declaration_test_076() {
    check_err(
        AsyncGeneratorDeclaration::parse(&mut newparser("async function * ("), Scanner::new(), false, false, false),
        "not an identifier",
        1,
        18,
    );
}
#[test]
fn async_generator_declaration_test_08() {
    check_err(
        AsyncGeneratorDeclaration::parse(&mut newparser("async function * h ( u"), Scanner::new(), false, false, true),
        "‘)’ expected",
        1,
        17 + 6,
    );
}
#[test]
fn async_generator_declaration_test_09() {
    check_err(
        AsyncGeneratorDeclaration::parse(
            &mut newparser("async function * h ( u )"),
            Scanner::new(),
            false,
            false,
            true,
        ),
        "‘{’ expected",
        1,
        19 + 6,
    );
}
#[test]
fn async_generator_declaration_test_10() {
    check_err(
        AsyncGeneratorDeclaration::parse(
            &mut newparser("async function * h ( u ) {"),
            Scanner::new(),
            false,
            false,
            true,
        ),
        "‘}’ expected",
        1,
        21 + 6,
    );
}
#[test]
fn async_generator_declaration_test_11() {
    check_err(
        AsyncGeneratorDeclaration::parse(
            &mut newparser("async function * h ( u ) { z;"),
            Scanner::new(),
            false,
            false,
            true,
        ),
        "‘}’ expected",
        1,
        24 + 6,
    );
}
#[test]
fn async_generator_declaration_test_12() {
    check_err(
        AsyncGeneratorDeclaration::parse(&mut newparser("async"), Scanner::new(), false, false, true),
        "‘function’ expected",
        1,
        6,
    );
}
#[test]
fn async_generator_declaration_test_prettyerrors_1() {
    let (item, _) = AsyncGeneratorDeclaration::parse(
        &mut newparser("async function * bob(blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
        false,
        false,
        true,
    )
    .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_generator_declaration_test_prettyerrors_2() {
    let (item, _) = AsyncGeneratorDeclaration::parse(
        &mut newparser("async function * (blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
        false,
        false,
        true,
    )
    .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_generator_declaration_test_conciseerrors_1() {
    let (item, _) = AsyncGeneratorDeclaration::parse(
        &mut newparser("async function * bob(blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
        false,
        false,
        true,
    )
    .unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_generator_declaration_test_conciseerrors_2() {
    let (item, _) = AsyncGeneratorDeclaration::parse(
        &mut newparser("async function * (blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
        false,
        false,
        true,
    )
    .unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_generator_declaration_test_contains_01() {
    let (item, _) = AsyncGeneratorDeclaration::parse(
        &mut newparser("async function * a(b=10) { return 10; }"),
        Scanner::new(),
        true,
        true,
        true,
    )
    .unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn async_generator_declaration_test_contains_02() {
    let (item, _) = AsyncGeneratorDeclaration::parse(
        &mut newparser("async function * (b=10) { return 10; }"),
        Scanner::new(),
        true,
        true,
        true,
    )
    .unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn async_generator_declaration_test_bound_names_01() {
    let (item, _) = AsyncGeneratorDeclaration::parse(
        &mut newparser("async function * a() { return; }"),
        Scanner::new(),
        true,
        true,
        true,
    )
    .unwrap();
    assert_eq!(item.bound_names(), vec!["a"]);
}
#[test]
fn async_generator_declaration_test_bound_names_02() {
    let (item, _) = AsyncGeneratorDeclaration::parse(
        &mut newparser("async function * () { return; }"),
        Scanner::new(),
        true,
        true,
        true,
    )
    .unwrap();
    assert_eq!(item.bound_names(), vec!["*default*"]);
}
#[test_case("async function *f(arg=item.#valid){}" => true; "Params valid")]
#[test_case("async function *f(arg){item.#valid;}" => true; "Body valid")]
#[test_case("async function *f(arg=item.#invalid){}" => false; "Params invalid")]
#[test_case("async function *f(arg){item.#invalid;}" => false; "Body invalid")]
fn async_generator_declaration_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = AsyncGeneratorDeclaration::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}

mod async_generator_declaration {
    use super::*;
    use test_case::test_case;

    #[test_case("async function *a(){}", false => sset(&[]); "all good")]
    #[test_case("async function *a(b=super()){}", false => sset(&[UNEXPECTED_SUPER2]); "direct super")]
    #[test_case("async function *a(b=yield 3){}", false => sset(&[YIELD_IN_GENPARAM]); "yield in param")]
    #[test_case("async function *a(...b){'use strict';}", false => sset(&[BAD_USE_STRICT]); "complex params")]
    #[test_case("async function *b(a){let a;}", false => sset(&[A_ALREADY_DEFN]); "param/body var clash")]
    #[test_case("async function *(a,a){}", false => sset(&[]); "not strict, dup params")]
    #[test_case("async function *(a,a){}", true => sset(&[A_ALREADY_DEFN]); "dups, strict form 1")]
    #[test_case("async function *(a,a){'use strict';}", false => sset(&[A_ALREADY_DEFN]); "dups, strict form 2")]
    #[test_case("async function *eval(a){}", true => sset(&[BAD_EVAL]); "eval used as fname")]
    #[test_case("async function *arguments(a){}", true => sset(&[BAD_ARGUMENTS]); "arguments used as fname")]
    #[test_case("async function *a(b=await x){}", false => sset(&[UNEXPECTED_AWAIT]); "await in parameter")]
    #[test_case("async function *a(b=super.x){}", false => sset(&[UNEXPECTED_SUPER2]); "superprop in params")]
    #[test_case("async function *(){super.x;}", false => sset(&[UNEXPECTED_SUPER2]); "superprop in body")]
    #[test_case("async function *(){super();}", false => sset(&[UNEXPECTED_SUPER2]); "supercall in body")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        let node = Maker::new(src).async_generator_declaration();
        node.early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("   async function *a(){}" => Location { starting_line: 1, starting_column: 4, span: Span{ starting_index: 3, length: 21 }})]
    fn location(src: &str) -> Location {
        Maker::new(src).async_generator_declaration().location()
    }

    #[test_case("async function *named() {}" => "named"; "named")]
    #[test_case("async function *(){}" => "*default*"; "unnamed")]
    fn bound_name(src: &str) -> String {
        Maker::new(src).async_generator_declaration().bound_name().into()
    }

    #[test_case("async function *foo () {}" => false; "typical")]
    fn is_constant_declaration(src: &str) -> bool {
        Maker::new(src).async_generator_declaration().is_constant_declaration()
    }
}

// ASYNC GENERATOR EXPRESSION
#[test]
fn async_generator_expression_test_01() {
    let (node, scanner) =
        check(AsyncGeneratorExpression::parse(&mut newparser("async function *a(){}"), Scanner::new()));
    chk_scan(&scanner, 15 + 6);
    pretty_check(
        &*node,
        "AsyncGeneratorExpression: async function * a (  ) {  }",
        vec!["BindingIdentifier: a", "FormalParameters: ", "AsyncGeneratorBody: "],
    );
    concise_check(
        &*node,
        "AsyncGeneratorExpression: async function * a (  ) {  }",
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
    assert!(node.is_function_definition());
}
#[test]
fn async_generator_expression_test_02() {
    let (node, scanner) =
        check(AsyncGeneratorExpression::parse(&mut newparser("async function *(){}"), Scanner::new()));
    chk_scan(&scanner, 14 + 6);
    pretty_check(
        &*node,
        "AsyncGeneratorExpression: async function * (  ) {  }",
        vec!["FormalParameters: ", "AsyncGeneratorBody: "],
    );
    concise_check(
        &*node,
        "AsyncGeneratorExpression: async function * (  ) {  }",
        vec![
            "Keyword: async",
            "Keyword: function",
            "Punctuator: *",
            "Punctuator: (",
            "Punctuator: )",
            "Punctuator: {",
            "Punctuator: }",
        ],
    );
    format!("{:?}", node);
    assert!(node.is_function_definition());
}
#[test]
fn async_generator_expression_test_03() {
    check_err(AsyncGeneratorExpression::parse(&mut newparser(""), Scanner::new()), "‘async’ expected", 1, 1);
}
#[test]
fn async_generator_expression_test_031() {
    check_err(AsyncGeneratorExpression::parse(&mut newparser("async"), Scanner::new()), "‘function’ expected", 1, 6);
}
#[test]
fn async_generator_expression_test_04() {
    check_err(
        AsyncGeneratorExpression::parse(&mut newparser("async function"), Scanner::new()),
        "‘*’ expected",
        1,
        9 + 6,
    );
}
#[test]
fn async_generator_expression_test_041() {
    check_err(
        AsyncGeneratorExpression::parse(&mut newparser("async \nfunction"), Scanner::new()),
        "newline not allowed here",
        1,
        6,
    );
}
#[test]
fn async_generator_expression_test_05() {
    check_err(
        AsyncGeneratorExpression::parse(&mut newparser("async function *"), Scanner::new()),
        "‘(’ expected",
        1,
        11 + 6,
    );
}
#[test]
fn async_generator_expression_test_06() {
    check_err(
        AsyncGeneratorExpression::parse(&mut newparser("async function * h"), Scanner::new()),
        "‘(’ expected",
        1,
        13 + 6,
    );
}
#[test]
fn async_generator_expression_test_07() {
    check_err(
        AsyncGeneratorExpression::parse(&mut newparser("async function * h ("), Scanner::new()),
        "‘)’ expected",
        1,
        15 + 6,
    );
}
#[test]
fn async_generator_expression_test_08() {
    check_err(
        AsyncGeneratorExpression::parse(&mut newparser("async function * h ( u"), Scanner::new()),
        "‘)’ expected",
        1,
        17 + 6,
    );
}
#[test]
fn async_generator_expression_test_09() {
    check_err(
        AsyncGeneratorExpression::parse(&mut newparser("async function * h ( u )"), Scanner::new()),
        "‘{’ expected",
        1,
        19 + 6,
    );
}
#[test]
fn async_generator_expression_test_10() {
    check_err(
        AsyncGeneratorExpression::parse(&mut newparser("async function * h ( u ) {"), Scanner::new()),
        "‘}’ expected",
        1,
        21 + 6,
    );
}
#[test]
fn async_generator_expression_test_11() {
    check_err(
        AsyncGeneratorExpression::parse(&mut newparser("async function * h ( u ) { z;"), Scanner::new()),
        "‘}’ expected",
        1,
        24 + 6,
    );
}
#[test]
fn async_generator_expression_test_prettyerrors_1() {
    let (item, _) = AsyncGeneratorExpression::parse(
        &mut newparser("async function * bob(blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
    )
    .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_generator_expression_test_prettyerrors_2() {
    let (item, _) = AsyncGeneratorExpression::parse(
        &mut newparser("async function * (blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
    )
    .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn async_generator_expression_test_conciseerrors_1() {
    let (item, _) = AsyncGeneratorExpression::parse(
        &mut newparser("async function * bob(blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
    )
    .unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_generator_expression_test_conciseerrors_2() {
    let (item, _) = AsyncGeneratorExpression::parse(
        &mut newparser("async function * (blue, red, green) { yield blue + red + green; }"),
        Scanner::new(),
    )
    .unwrap();
    concise_error_validate(&*item);
}
#[test]
fn async_generator_expression_test_contains_01() {
    let (item, _) =
        AsyncGeneratorExpression::parse(&mut newparser("async function * a(b=10) { return 10; }"), Scanner::new())
            .unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn async_generator_expression_test_contains_02() {
    let (item, _) =
        AsyncGeneratorExpression::parse(&mut newparser("async function * (b=10) { return 10; }"), Scanner::new())
            .unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("async function *f(arg=item.#valid){}" => true; "Params valid")]
#[test_case("async function *f(arg){item.#valid;}" => true; "Body valid")]
#[test_case("async function *f(arg=item.#invalid){}" => false; "Params invalid")]
#[test_case("async function *f(arg){item.#invalid;}" => false; "Body invalid")]
fn async_generator_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = AsyncGeneratorExpression::parse(&mut newparser(src), Scanner::new()).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod async_generator_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("async function *package(a=implements) {interface;}", true => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "sub exprs (with id)")]
    #[test_case("async function *(a=package) { implements; }", true => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "sub exprs (no id)")]
    #[test_case("async function *foo() { super(); }", false => sset(&[UNEXPECTED_SUPER2]); "body supercall")]
    #[test_case("async function *foo(a=super()){}", false => sset(&[UNEXPECTED_SUPER2]); "params supercall")]
    #[test_case("async function *foo(){return super.a;}", false => sset(&[UNEXPECTED_SUPER2]); "body superprop")]
    #[test_case("async function *foo(a=super.a){}", false => sset(&[UNEXPECTED_SUPER2]); "params superprop")]
    #[test_case("async function *foo(a, a, a){}", false => sset(&[]); "dups, but not strict")]
    #[test_case("async function *foo(a, a, a){'use strict';}", false => sset(&[A_ALREADY_DEFN]); "dups; strict via directive")]
    #[test_case("async function *foo(a, a, a){}", true => sset(&[A_ALREADY_DEFN]); "dups; strict via function call")]
    #[test_case("async function *eval(){}", true => sset(&[BAD_EVAL]); "named eval in strict")]
    #[test_case("async function *arguments(){'use strict';}", false => sset(&[BAD_ARGUMENTS]); "named arguments via directive")]
    #[test_case("async function *foo(...a) { 'use strict'; f(a);}", false => sset(&[BAD_USE_STRICT]); "complex params")]
    #[test_case("async function *foo(a){let a; const b=0;}", false => sset(&[A_ALREADY_DEFN]); "param/body lex dup")]
    #[test_case("async function *foo(a=await j){}", false => sset(&[UNEXPECTED_AWAIT]); "await in params")]
    #[test_case("async function *foo(a=yield j){}", false => sset(&[YIELD_IN_GENPARAM]); "yield in params")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        let node = Maker::new(src).async_generator_expression();
        node.early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("async function *a(){}" => true; "named")]
    #[test_case("async function *(){}" => false; "unnamed")]
    fn is_named_function(src: &str) -> bool {
        Maker::new(src).async_generator_expression().is_named_function()
    }

    #[test_case("   async function *foop(q,r) { return calculate(q, r);}" => Location { starting_line: 1, starting_column: 4, span: Span{ starting_index: 3, length: 52 } })]
    fn location(src: &str) -> Location {
        Maker::new(src).async_generator_expression().location()
    }
}

// ASYNC GENERATOR BODY
#[test]
fn async_generator_body_test_01() {
    let (node, scanner) = AsyncGeneratorBody::parse(&mut newparser("yield 1;"), Scanner::new());
    chk_scan(&scanner, 8);
    pretty_check(&*node, "AsyncGeneratorBody: yield 1 ;", vec!["FunctionBody: yield 1 ;"]);
    concise_check(&*node, "ExpressionStatement: yield 1 ;", vec!["YieldExpression: yield 1", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn async_generator_body_test_cache_01() {
    let mut parser = newparser("blue(67); yield orange(30);");
    let (node, scanner) = AsyncGeneratorBody::parse(&mut parser, Scanner::new());
    let (node2, scanner2) = AsyncGeneratorBody::parse(&mut parser, Scanner::new());
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn async_generator_body_test_prettyerrors_1() {
    let (item, _) = AsyncGeneratorBody::parse(&mut newparser("yield 1;"), Scanner::new());
    pretty_error_validate(&*item);
}
#[test]
fn async_generator_body_test_conciseerrors_1() {
    let (item, _) = AsyncGeneratorBody::parse(&mut newparser("yield 1;"), Scanner::new());
    concise_error_validate(&*item);
}
#[test]
fn async_generator_body_test_contains_01() {
    let (item, _) = AsyncGeneratorBody::parse(&mut newparser("yield 1;"), Scanner::new());
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn async_generator_body_test_contains_02() {
    let (item, _) = AsyncGeneratorBody::parse(&mut newparser("yield a;"), Scanner::new());
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("item.#valid;" => true; "StatementList valid")]
#[test_case("item.#invalid;" => false; "StatementList invalid")]
fn async_generator_body_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = AsyncGeneratorBody::parse(&mut newparser(src), Scanner::new());
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod async_generator_body {
    use super::*;
    use test_case::test_case;

    #[test_case("package;", true => sset(&[PACKAGE_NOT_ALLOWED]); "std")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).async_generator_body().early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("let a; const b=0; var c; function d() {}" => svec(&["c", "d"]); "function body")]
    fn var_declared_names(src: &str) -> Vec<String> {
        Maker::new(src).async_generator_body().var_declared_names().into_iter().map(String::from).collect()
    }

    #[test_case("let a; const b=0; var c; function d() {}" => svec(&["c", "function d (  ) {  }"]); "function body")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).async_generator_body().var_scoped_declarations().iter().map(String::from).collect()
    }

    #[test_case("let a; const b=0; var c; function d() {}" => svec(&["a", "b"]); "typical")]
    fn lexically_declared_names(src: &str) -> Vec<String> {
        Maker::new(src).async_generator_body().lexically_declared_names().into_iter().map(String::from).collect()
    }

    #[test_case("let a; const b=0; var c; function d() {}" => svec(&["let a ;", "const b = 0 ;"]); "typical")]
    fn lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).async_generator_body().lexically_scoped_declarations().iter().map(String::from).collect()
    }

    #[test_case("'use strict';" => true; "present")]
    #[test_case("'bob is a putz';" => false; "not present")]
    fn function_body_contains_use_strict(src: &str) -> bool {
        Maker::new(src).async_generator_body().function_body_contains_use_strict()
    }
}
