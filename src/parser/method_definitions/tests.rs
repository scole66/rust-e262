use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

mod method_type {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", MethodType::Setter), "");
    }

    #[test_case(MethodType::Setter, MethodType::Setter => true; "equal")]
    #[test_case(MethodType::Getter, MethodType::Normal => false; "not equal")]
    fn eq(left: MethodType, right: MethodType) -> bool {
        left == right
    }
}

// METHOD DEFINITION
#[test]
fn method_definition_test_01() {
    let (pn, scanner) = check(MethodDefinition::parse(&mut newparser("a(b){c;}"), Scanner::new(), false, false));
    chk_scan(&scanner, 8);
    assert!(matches!(&*pn, MethodDefinition::NamedFunction(..)));
    pretty_check(
        &*pn,
        "MethodDefinition: a ( b ) { c ; }",
        &["ClassElementName: a", "UniqueFormalParameters: b", "FunctionBody: c ;"],
    );
    concise_check(
        &*pn,
        "MethodDefinition: a ( b ) { c ; }",
        &[
            "IdentifierName: a",
            "Punctuator: (",
            "IdentifierName: b",
            "Punctuator: )",
            "Punctuator: {",
            "ExpressionStatement: c ;",
            "Punctuator: }",
        ],
    );
    format!("{pn:?}");
}
#[test]
fn method_definition_test_02() {
    let (pn, scanner) =
        check(MethodDefinition::parse(&mut newparser("get a() { return 1; }"), Scanner::new(), false, false));
    chk_scan(&scanner, 21);
    assert!(matches!(&*pn, MethodDefinition::Getter(..)));
    pretty_check(
        &*pn,
        "MethodDefinition: get a ( ) { return 1 ; }",
        &["ClassElementName: a", "FunctionBody: return 1 ;"],
    );
    concise_check(
        &*pn,
        "MethodDefinition: get a ( ) { return 1 ; }",
        &[
            "Keyword: get",
            "IdentifierName: a",
            "Punctuator: (",
            "Punctuator: )",
            "Punctuator: {",
            "ReturnStatement: return 1 ;",
            "Punctuator: }",
        ],
    );
    format!("{pn:?}");
}
#[test]
fn method_definition_test_03() {
    let (pn, scanner) =
        check(MethodDefinition::parse(&mut newparser("set a(blue) { this.a=blue; }"), Scanner::new(), false, false));
    chk_scan(&scanner, 28);
    assert!(matches!(&*pn, MethodDefinition::Setter(..)));
    pretty_check(
        &*pn,
        "MethodDefinition: set a ( blue ) { this . a = blue ; }",
        &["ClassElementName: a", "PropertySetParameterList: blue", "FunctionBody: this . a = blue ;"],
    );
    concise_check(
        &*pn,
        "MethodDefinition: set a ( blue ) { this . a = blue ; }",
        &[
            "Keyword: set",
            "IdentifierName: a",
            "Punctuator: (",
            "IdentifierName: blue",
            "Punctuator: )",
            "Punctuator: {",
            "ExpressionStatement: this . a = blue ;",
            "Punctuator: }",
        ],
    );
    format!("{pn:?}");
}
#[test]
fn method_definition_test_04() {
    let (pn, scanner) =
        check(MethodDefinition::parse(&mut newparser("* a(blue) { this.a=blue; }"), Scanner::new(), false, false));
    chk_scan(&scanner, 26);
    assert!(matches!(&*pn, MethodDefinition::Generator(..)));
    pretty_check(
        &*pn,
        "MethodDefinition: * a ( blue ) { this . a = blue ; }",
        &["GeneratorMethod: * a ( blue ) { this . a = blue ; }"],
    );
    concise_check(
        &*pn,
        "GeneratorMethod: * a ( blue ) { this . a = blue ; }",
        &[
            "Punctuator: *",
            "IdentifierName: a",
            "Punctuator: (",
            "IdentifierName: blue",
            "Punctuator: )",
            "Punctuator: {",
            "ExpressionStatement: this . a = blue ;",
            "Punctuator: }",
        ],
    );
    format!("{pn:?}");
}
#[test]
fn method_definition_test_05() {
    let (pn, scanner) =
        check(MethodDefinition::parse(&mut newparser("async a(blue) { this.a=blue; }"), Scanner::new(), false, false));
    chk_scan(&scanner, 30);
    assert!(matches!(&*pn, MethodDefinition::Async(..)));
    pretty_check(
        &*pn,
        "MethodDefinition: async a ( blue ) { this . a = blue ; }",
        &["AsyncMethod: async a ( blue ) { this . a = blue ; }"],
    );
    concise_check(
        &*pn,
        "AsyncMethod: async a ( blue ) { this . a = blue ; }",
        &[
            "Keyword: async",
            "IdentifierName: a",
            "Punctuator: (",
            "IdentifierName: blue",
            "Punctuator: )",
            "Punctuator: {",
            "ExpressionStatement: this . a = blue ;",
            "Punctuator: }",
        ],
    );
    format!("{pn:?}");
}
#[test]
fn method_definition_test_06() {
    let (pn, scanner) =
        check(MethodDefinition::parse(&mut newparser("async *a(blue) { this.a=blue; }"), Scanner::new(), false, false));
    chk_scan(&scanner, 31);
    assert!(matches!(&*pn, MethodDefinition::AsyncGenerator(..)));
    pretty_check(
        &*pn,
        "MethodDefinition: async * a ( blue ) { this . a = blue ; }",
        &["AsyncGeneratorMethod: async * a ( blue ) { this . a = blue ; }"],
    );
    concise_check(
        &*pn,
        "AsyncGeneratorMethod: async * a ( blue ) { this . a = blue ; }",
        &[
            "Keyword: async",
            "Punctuator: *",
            "IdentifierName: a",
            "Punctuator: (",
            "IdentifierName: blue",
            "Punctuator: )",
            "Punctuator: {",
            "ExpressionStatement: this . a = blue ;",
            "Punctuator: }",
        ],
    );
    format!("{pn:?}");
}
#[test]
fn method_definition_test_errs_01() {
    check_err(
        MethodDefinition::parse(&mut newparser(""), Scanner::new(), false, false),
        "MethodDefinition expected",
        1,
        1,
    );
}
#[test]
fn method_definition_test_errs_02() {
    check_err(MethodDefinition::parse(&mut newparser("a"), Scanner::new(), false, false), "‘(’ expected", 1, 2);
}
#[test]
fn method_definition_test_errs_03() {
    check_err(MethodDefinition::parse(&mut newparser("a("), Scanner::new(), false, false), "‘)’ expected", 1, 3);
}
#[test]
fn method_definition_test_errs_04() {
    check_err(MethodDefinition::parse(&mut newparser("a(x"), Scanner::new(), false, false), "‘)’ expected", 1, 4);
}
#[test]
fn method_definition_test_errs_05() {
    check_err(MethodDefinition::parse(&mut newparser("a(x,y)"), Scanner::new(), false, false), "‘{’ expected", 1, 7);
}
#[test]
fn method_definition_test_errs_06() {
    check_err(MethodDefinition::parse(&mut newparser("a(x,y){"), Scanner::new(), false, false), "‘}’ expected", 1, 8);
}
#[test]
fn method_definition_test_errs_07() {
    check_err(
        MethodDefinition::parse(&mut newparser("get"), Scanner::new(), false, false),
        "ClassElementName expected",
        1,
        4,
    );
}
#[test]
fn method_definition_test_errs_08() {
    check_err(MethodDefinition::parse(&mut newparser("get a"), Scanner::new(), false, false), "‘(’ expected", 1, 6);
}
#[test]
fn method_definition_test_errs_09() {
    check_err(MethodDefinition::parse(&mut newparser("get a("), Scanner::new(), false, false), "‘)’ expected", 1, 7);
}
#[test]
fn method_definition_test_errs_10() {
    check_err(MethodDefinition::parse(&mut newparser("get a()"), Scanner::new(), false, false), "‘{’ expected", 1, 8);
}
#[test]
fn method_definition_test_errs_11() {
    check_err(MethodDefinition::parse(&mut newparser("get a(){"), Scanner::new(), false, false), "‘}’ expected", 1, 9);
}
#[test]
fn method_definition_test_errs_12() {
    check_err(
        MethodDefinition::parse(&mut newparser("set"), Scanner::new(), false, false),
        "ClassElementName expected",
        1,
        4,
    );
}
#[test]
fn method_definition_test_errs_13() {
    check_err(MethodDefinition::parse(&mut newparser("set a"), Scanner::new(), false, false), "‘(’ expected", 1, 6);
}
#[test]
fn method_definition_test_errs_14() {
    check_err(
        MethodDefinition::parse(&mut newparser("set a("), Scanner::new(), false, false),
        "BindingElement expected",
        1,
        7,
    );
}
#[test]
fn method_definition_test_errs_15() {
    check_err(
        MethodDefinition::parse(&mut newparser("set a()"), Scanner::new(), false, false),
        "BindingElement expected",
        1,
        7,
    );
}
#[test]
fn method_definition_test_errs_16() {
    check_err(MethodDefinition::parse(&mut newparser("set a(h)"), Scanner::new(), false, false), "‘{’ expected", 1, 9);
}
#[test]
fn method_definition_test_errs_17() {
    check_err(
        MethodDefinition::parse(&mut newparser("set a(h){"), Scanner::new(), false, false),
        "‘}’ expected",
        1,
        10,
    );
}
#[test]
fn method_definition_test_errs_18() {
    check_err(MethodDefinition::parse(&mut newparser("set a(h"), Scanner::new(), false, false), "‘)’ expected", 1, 8);
}
#[test]
fn method_definition_test_prettyerrors_1() {
    let (item, _) = MethodDefinition::parse(&mut newparser("a(b){c;}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn method_definition_test_prettyerrors_2() {
    let (item, _) =
        MethodDefinition::parse(&mut newparser("get a() { return 1; }"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn method_definition_test_prettyerrors_3() {
    let (item, _) =
        MethodDefinition::parse(&mut newparser("set a(blue) { this.a=blue; }"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn method_definition_test_prettyerrors_4() {
    let (item, _) =
        MethodDefinition::parse(&mut newparser("* a(blue) { this.a=blue; }"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn method_definition_test_prettyerrors_5() {
    let (item, _) =
        MethodDefinition::parse(&mut newparser("async a(blue) { this.a=blue; }"), Scanner::new(), false, false)
            .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn method_definition_test_prettyerrors_6() {
    let (item, _) =
        MethodDefinition::parse(&mut newparser("async *a(blue) { this.a=blue; }"), Scanner::new(), false, false)
            .unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn method_definition_test_conciseerrors_1() {
    let (item, _) = MethodDefinition::parse(&mut newparser("a(b){c;}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn method_definition_test_conciseerrors_2() {
    let (item, _) =
        MethodDefinition::parse(&mut newparser("get a() { return 1; }"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn method_definition_test_conciseerrors_3() {
    let (item, _) =
        MethodDefinition::parse(&mut newparser("set a(blue) { this.a=blue; }"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn method_definition_test_conciseerrors_4() {
    let (item, _) =
        MethodDefinition::parse(&mut newparser("* a(blue) { this.a=blue; }"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn method_definition_test_conciseerrors_5() {
    let (item, _) =
        MethodDefinition::parse(&mut newparser("async a(blue) { this.a=blue; }"), Scanner::new(), false, false)
            .unwrap();
    concise_error_validate(&*item);
}
#[test]
fn method_definition_test_conciseerrors_6() {
    let (item, _) =
        MethodDefinition::parse(&mut newparser("async *a(blue) { this.a=blue; }"), Scanner::new(), false, false)
            .unwrap();
    concise_error_validate(&*item);
}
#[test]
fn method_definition_test_cache_01() {
    let mut parser = newparser("a(b){c;}");
    let (node, scanner) = check(MethodDefinition::parse(&mut parser, Scanner::new(), false, false));
    let (node2, scanner2) = check(MethodDefinition::parse(&mut parser, Scanner::new(), false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn method_definition_test_contains_01() {
    let (item, _) = MethodDefinition::parse(&mut newparser("[0](){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn method_definition_test_contains_02() {
    let (item, _) = MethodDefinition::parse(&mut newparser("a(b=0){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn method_definition_test_contains_03() {
    let (item, _) = MethodDefinition::parse(&mut newparser("a(){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn method_definition_test_contains_04() {
    let (item, _) = MethodDefinition::parse(&mut newparser("a(){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn method_definition_test_contains_05() {
    let (item, _) = MethodDefinition::parse(&mut newparser("*a(){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn method_definition_test_contains_06() {
    let (item, _) = MethodDefinition::parse(&mut newparser("*a(){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn method_definition_test_contains_07() {
    let (item, _) = MethodDefinition::parse(&mut newparser("async a(){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn method_definition_test_contains_08() {
    let (item, _) = MethodDefinition::parse(&mut newparser("async a(){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn method_definition_test_contains_09() {
    let (item, _) = MethodDefinition::parse(&mut newparser("async *a(){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn method_definition_test_contains_10() {
    let (item, _) = MethodDefinition::parse(&mut newparser("async *a(){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn method_definition_test_contains_11() {
    let (item, _) = MethodDefinition::parse(&mut newparser("get a(){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn method_definition_test_contains_12() {
    let (item, _) = MethodDefinition::parse(&mut newparser("get [0](){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn method_definition_test_contains_13() {
    let (item, _) = MethodDefinition::parse(&mut newparser("get a(){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn method_definition_test_contains_14() {
    let (item, _) = MethodDefinition::parse(&mut newparser("set a(b){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn method_definition_test_contains_15() {
    let (item, _) = MethodDefinition::parse(&mut newparser("set [0](b){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn method_definition_test_contains_16() {
    let (item, _) = MethodDefinition::parse(&mut newparser("set a(b=0){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn method_definition_test_contains_17() {
    let (item, _) = MethodDefinition::parse(&mut newparser("set a(b){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn method_definition_test_computed_property_contains_01() {
    let (item, _) = MethodDefinition::parse(&mut newparser("a(b=0){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test]
fn method_definition_test_computed_property_contains_02() {
    let (item, _) = MethodDefinition::parse(&mut newparser("[0](){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn method_definition_test_computed_property_contains_03() {
    let (item, _) = MethodDefinition::parse(&mut newparser("*a(b=0){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test]
fn method_definition_test_computed_property_contains_04() {
    let (item, _) = MethodDefinition::parse(&mut newparser("*[0](){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn method_definition_test_computed_property_contains_05() {
    let (item, _) = MethodDefinition::parse(&mut newparser("async a(b=0){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test]
fn method_definition_test_computed_property_contains_06() {
    let (item, _) = MethodDefinition::parse(&mut newparser("async [0](){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn method_definition_test_computed_property_contains_07() {
    let (item, _) = MethodDefinition::parse(&mut newparser("async *a(b=0){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test]
fn method_definition_test_computed_property_contains_08() {
    let (item, _) = MethodDefinition::parse(&mut newparser("async *[0](){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn method_definition_test_computed_property_contains_09() {
    let (item, _) = MethodDefinition::parse(&mut newparser("get a(){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test]
fn method_definition_test_computed_property_contains_10() {
    let (item, _) = MethodDefinition::parse(&mut newparser("get [0](){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn method_definition_test_computed_property_contains_11() {
    let (item, _) = MethodDefinition::parse(&mut newparser("set a(b=0){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test]
fn method_definition_test_computed_property_contains_12() {
    let (item, _) = MethodDefinition::parse(&mut newparser("set [0](b){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test_case("a(){b.#valid;}" => true; "method valid")]
#[test_case("*a(){b.#valid;}" => true; "generator valid")]
#[test_case("async a(){b.#valid;}" => true; "async method valid")]
#[test_case("async *a(){b.#valid;}" => true; "async generator valid")]
#[test_case("get a(){b.#valid;}" => true; "getter valid")]
#[test_case("set a(b){c.#valid;}" => true; "setter valid")]
#[test_case("a(){b.#invalid;}" => false; "method invalid")]
#[test_case("*a(){b.#invalid;}" => false; "generator invalid")]
#[test_case("async a(){b.#invalid;}" => false; "async method invalid")]
#[test_case("async *a(){b.#invalid;}" => false; "async generator invalid")]
#[test_case("get a(){b.#invalid;}" => false; "getter invalid")]
#[test_case("set a(b){c.#invalid;}" => false; "setter invalid")]
fn method_definition_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = MethodDefinition::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}

mod method_definition {
    use super::*;
    use test_case::test_case;

    #[test_case("a(){}" => false; "method without")]
    #[test_case("a(b=super(undefined)){}" => true; "method params with")]
    #[test_case("a(){super(a);}" => true; "method body with")]
    #[test_case("get a(){}" => false; "getter without")]
    #[test_case("get a(){super(b);}" => true; "getter with")]
    #[test_case("set a(b){}" => false; "setter without")]
    #[test_case("set a(b=super(c)){}" => true; "setter params with")]
    #[test_case("set a(b){super(c);}" => true; "setter body with")]
    #[test_case("*a(){}" => false; "generator without")]
    #[test_case("*a(){super(0);}" => true; "generator with")]
    #[test_case("async *a(){}" => false; "async generator without")]
    #[test_case("async *a(){super(0);}" => true; "async generator with")]
    #[test_case("async a(){}" => false; "async method without")]
    #[test_case("async a(){super(0);}" => true; "async method with")]
    fn has_direct_super(src: &str) -> bool {
        Maker::new(src).method_definition().has_direct_super()
    }

    #[test_case("[package](implements){interface;}", true => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "ClassElementName ( UniqueFormalParameters ) { FunctionBody }")]
    #[test_case("*[package](){}", true => sset(&[PACKAGE_NOT_ALLOWED]); "GeneratorMethod")]
    #[test_case("async [package](){}", true => sset(&[PACKAGE_NOT_ALLOWED]); "AsyncMethod")]
    #[test_case("async *[package](){}", true => sset(&[PACKAGE_NOT_ALLOWED]); "AsyncGeneratorMethod")]
    #[test_case("get [package](){implements;}", true => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "get ClassElementName () { FunctionBody }")]
    #[test_case("set [package](implements){interface;}", true => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "set ClassElementName ( PropertySetParameterList ) { FunctionBody }")]
    #[test_case("[package](implements){'use strict'; interface;}", false => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "contains strict; ordinary")]
    #[test_case("get [package](){'use strict'; implements;}", false => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "contains strict; getter")]
    #[test_case("set [package](implements){'use strict'; interface;}", false => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "contains strict; setter")]
    #[test_case("a([b]){'use strict';}", false => sset(&[BAD_USE_STRICT]); "ordinary; bad use-strict")]
    #[test_case("set a([b]){'use strict';}", false => sset(&[BAD_USE_STRICT]); "setter; bad use-strict")]
    #[test_case("foo(a){let a;}", false => sset(&[A_ALREADY_DEFN]); "ordinary; duped lexical")]
    #[test_case("set foo(a){let a;}", false => sset(&[A_ALREADY_DEFN]); "setter; duped lexical")]
    #[test_case("set foo([a, a]){}", false => sset(&[A_ALREADY_DEFN]); "setter; duped params")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).method_definition().early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("a(){}" => Some(JSString::from("a")); "simple")]
    #[test_case("get a(){}" => Some(JSString::from("a")); "getter")]
    #[test_case("set a(arg){}" => Some(JSString::from("a")); "setter")]
    #[test_case("*a(){}" => Some(JSString::from("a")); "generator")]
    #[test_case("async a(){}" => Some(JSString::from("a")); "async fun")]
    #[test_case("async *a(){}" => Some(JSString::from("a")); "async gen")]
    fn prop_name(src: &str) -> Option<JSString> {
        Maker::new(src).method_definition().prop_name()
    }

    #[test_case("[arguments](){}" => true; "normal method (yes)")]
    #[test_case("a(){}" => false; "normal method (no)")]
    #[test_case("*[arguments](){}" => true; "generator (yes)")]
    #[test_case("*a(){}" => false; "generator (no)")]
    #[test_case("async [arguments](){}" => true; "async method (yes)")]
    #[test_case("async a(){}" => false; "async method (no)")]
    #[test_case("async *[arguments](){}" => true; "async gen (yes)")]
    #[test_case("async *a(){}" => false; "async gen (no)")]
    #[test_case("get [arguments](){}" => true; "getter (yes)")]
    #[test_case("get a(){}" => false; "getter (no)")]
    #[test_case("set [arguments](a){}" => true; "setter (yes)")]
    #[test_case("set a(b){}" => false; "setter (no)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).method_definition().contains_arguments()
    }

    #[test_case("#standard_method(){}" => Some((String::from("#standard_method"), MethodType::Normal)); "Standard Method")]
    #[test_case("*#generator(){}" => Some((String::from("#generator"), MethodType::Normal)); "Generator")]
    #[test_case("async #async_method(){}" => Some((String::from("#async_method"), MethodType::Normal)); "Async Method")]
    #[test_case("async *#async_gen(){}" => Some((String::from("#async_gen"), MethodType::Normal)); "Async Generator")]
    #[test_case("get #getter(){}" => Some((String::from("#getter"), MethodType::Getter)); "Getter")]
    #[test_case("set #setter(val){}" => Some((String::from("#setter"), MethodType::Setter)); "Setter")]
    #[test_case("standard_method(){}" => None; "Standard Method; not private")]
    #[test_case("*generator(){}" => None; "Generator; not private")]
    #[test_case("async async_method(){}" => None; "Async Method; not private")]
    #[test_case("async *async_gen(){}" => None; "Async Generator; not private")]
    #[test_case("get getter(){}" => None; "Getter; not private")]
    #[test_case("set setter(val){}" => None; "Setter; not private")]
    fn private_bound_identifier(src: &str) -> Option<(String, MethodType)> {
        Maker::new(src).method_definition().private_bound_identifier().map(|(jss, mt)| (String::from(jss), mt))
    }

    #[test_case("a(){}" => false; "standard method")]
    #[test_case("*a(){}" => true; "generator")]
    #[test_case("async m(){}" => true; "async fcn")]
    #[test_case("async *m(){}" => true; "async gen")]
    #[test_case("get foo(){}" => true; "getter")]
    #[test_case("set foo(val){}" => true; "setter")]
    fn special_method(src: &str) -> bool {
        Maker::new(src).method_definition().special_method()
    }

    #[test_case("  a(){}" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 5 }}; "standard method")]
    #[test_case("  *a(){}" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 6 }}; "generator")]
    #[test_case("  async m(){}" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 11 }}; "async fcn")]
    #[test_case("  async *m(){}" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 12 }}; "async gen")]
    #[test_case("  get foo(){}" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 11 }}; "getter")]
    #[test_case("  set foo(val){}" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 14 }}; "setter")]
    fn location(src: &str) -> Location {
        Maker::new(src).method_definition().location()
    }
}

mod property_set_parameter_list {
    use super::*;
    use test_case::test_case;

    #[test]
    fn parse() {
        let (pn, scanner) = check(PropertySetParameterList::parse(&mut newparser("a"), Scanner::new()));
        chk_scan(&scanner, 1);
        pretty_check(&*pn, "PropertySetParameterList: a", &["FormalParameter: a"]);
        concise_check(&*pn, "IdentifierName: a", &[]);
        format!("{pn:?}");
    }

    #[test]
    fn pretty_errors() {
        let (item, _) = PropertySetParameterList::parse(&mut newparser("a"), Scanner::new()).unwrap();
        pretty_error_validate(&*item);
    }

    #[test]
    fn concise_errors() {
        let (item, _) = PropertySetParameterList::parse(&mut newparser("a"), Scanner::new()).unwrap();
        concise_error_validate(&*item);
    }

    #[test_case("a" => false; "no initializer")]
    #[test_case("a=27" => true; "literal initializer")]
    fn contains(src: &str) -> bool {
        let (item, _) = PropertySetParameterList::parse(&mut newparser(src), Scanner::new()).unwrap();
        item.contains(ParseNodeKind::Literal)
    }

    #[test_case("a=b.#valid" => true; "valid")]
    #[test_case("a=b.#invalid" => false; "invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = PropertySetParameterList::parse(&mut newparser(src), Scanner::new()).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "FormalParameter")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        PropertySetParameterList::parse(&mut newparser(src), Scanner::new()).unwrap().0.early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("a" => vec!["a"]; "FormalParameter")]
    fn bound_names(src: &str) -> Vec<String> {
        PropertySetParameterList::parse(&mut newparser(src), Scanner::new())
            .unwrap()
            .0
            .bound_names()
            .into_iter()
            .map(String::from)
            .collect::<Vec<_>>()
    }

    #[test_case("a" => true; "simple")]
    #[test_case("[a]" => false; "complex")]
    fn is_simple_parameter_list(src: &str) -> bool {
        PropertySetParameterList::parse(&mut newparser(src), Scanner::new()).unwrap().0.is_simple_parameter_list()
    }

    #[test_case("  a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 1 }}; "setter")]
    fn location(src: &str) -> Location {
        Maker::new(src).property_set_parameter_list().location()
    }
}
