use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::test_agent;
use test_case::test_case;

// METHOD DEFINITION
#[test]
fn method_definition_test_01() {
    let (pn, scanner) = check(MethodDefinition::parse(&mut newparser("a(b){c;}"), Scanner::new(), false, false));
    chk_scan(&scanner, 8);
    assert!(matches!(&*pn, MethodDefinition::NamedFunction(..)));
    pretty_check(&*pn, "MethodDefinition: a ( b ) { c ; }", vec!["ClassElementName: a", "UniqueFormalParameters: b", "FunctionBody: c ;"]);
    concise_check(
        &*pn,
        "MethodDefinition: a ( b ) { c ; }",
        vec!["IdentifierName: a", "Punctuator: (", "IdentifierName: b", "Punctuator: )", "Punctuator: {", "ExpressionStatement: c ;", "Punctuator: }"],
    );
    format!("{:?}", pn);
}
#[test]
fn method_definition_test_02() {
    let (pn, scanner) = check(MethodDefinition::parse(&mut newparser("get a() { return 1; }"), Scanner::new(), false, false));
    chk_scan(&scanner, 21);
    assert!(matches!(&*pn, MethodDefinition::Getter(..)));
    pretty_check(&*pn, "MethodDefinition: get a ( ) { return 1 ; }", vec!["ClassElementName: a", "FunctionBody: return 1 ;"]);
    concise_check(
        &*pn,
        "MethodDefinition: get a ( ) { return 1 ; }",
        vec!["Keyword: get", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "ReturnStatement: return 1 ;", "Punctuator: }"],
    );
    format!("{:?}", pn);
}
#[test]
fn method_definition_test_03() {
    let (pn, scanner) = check(MethodDefinition::parse(&mut newparser("set a(blue) { this.a=blue; }"), Scanner::new(), false, false));
    chk_scan(&scanner, 28);
    assert!(matches!(&*pn, MethodDefinition::Setter(..)));
    pretty_check(&*pn, "MethodDefinition: set a ( blue ) { this . a = blue ; }", vec!["ClassElementName: a", "PropertySetParameterList: blue", "FunctionBody: this . a = blue ;"]);
    concise_check(
        &*pn,
        "MethodDefinition: set a ( blue ) { this . a = blue ; }",
        vec!["Keyword: set", "IdentifierName: a", "Punctuator: (", "IdentifierName: blue", "Punctuator: )", "Punctuator: {", "ExpressionStatement: this . a = blue ;", "Punctuator: }"],
    );
    format!("{:?}", pn);
}
#[test]
fn method_definition_test_04() {
    let (pn, scanner) = check(MethodDefinition::parse(&mut newparser("* a(blue) { this.a=blue; }"), Scanner::new(), false, false));
    chk_scan(&scanner, 26);
    assert!(matches!(&*pn, MethodDefinition::Generator(..)));
    pretty_check(&*pn, "MethodDefinition: * a ( blue ) { this . a = blue ; }", vec!["GeneratorMethod: * a ( blue ) { this . a = blue ; }"]);
    concise_check(
        &*pn,
        "GeneratorMethod: * a ( blue ) { this . a = blue ; }",
        vec!["Punctuator: *", "IdentifierName: a", "Punctuator: (", "IdentifierName: blue", "Punctuator: )", "Punctuator: {", "ExpressionStatement: this . a = blue ;", "Punctuator: }"],
    );
    format!("{:?}", pn);
}
#[test]
fn method_definition_test_05() {
    let (pn, scanner) = check(MethodDefinition::parse(&mut newparser("async a(blue) { this.a=blue; }"), Scanner::new(), false, false));
    chk_scan(&scanner, 30);
    assert!(matches!(&*pn, MethodDefinition::Async(..)));
    pretty_check(&*pn, "MethodDefinition: async a ( blue ) { this . a = blue ; }", vec!["AsyncMethod: async a ( blue ) { this . a = blue ; }"]);
    concise_check(
        &*pn,
        "AsyncMethod: async a ( blue ) { this . a = blue ; }",
        vec!["Keyword: async", "IdentifierName: a", "Punctuator: (", "IdentifierName: blue", "Punctuator: )", "Punctuator: {", "ExpressionStatement: this . a = blue ;", "Punctuator: }"],
    );
    format!("{:?}", pn);
}
#[test]
fn method_definition_test_06() {
    let (pn, scanner) = check(MethodDefinition::parse(&mut newparser("async *a(blue) { this.a=blue; }"), Scanner::new(), false, false));
    chk_scan(&scanner, 31);
    assert!(matches!(&*pn, MethodDefinition::AsyncGenerator(..)));
    pretty_check(&*pn, "MethodDefinition: async * a ( blue ) { this . a = blue ; }", vec!["AsyncGeneratorMethod: async * a ( blue ) { this . a = blue ; }"]);
    concise_check(
        &*pn,
        "AsyncGeneratorMethod: async * a ( blue ) { this . a = blue ; }",
        vec![
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
    format!("{:?}", pn);
}
#[test]
fn method_definition_test_errs_01() {
    check_err(MethodDefinition::parse(&mut newparser(""), Scanner::new(), false, false), "MethodDefinition expected", 1, 1);
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
    check_err(MethodDefinition::parse(&mut newparser("get"), Scanner::new(), false, false), "ClassElementName expected", 1, 4);
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
    check_err(MethodDefinition::parse(&mut newparser("set"), Scanner::new(), false, false), "ClassElementName expected", 1, 4);
}
#[test]
fn method_definition_test_errs_13() {
    check_err(MethodDefinition::parse(&mut newparser("set a"), Scanner::new(), false, false), "‘(’ expected", 1, 6);
}
#[test]
fn method_definition_test_errs_14() {
    check_err(MethodDefinition::parse(&mut newparser("set a("), Scanner::new(), false, false), "BindingElement expected", 1, 7);
}
#[test]
fn method_definition_test_errs_15() {
    check_err(MethodDefinition::parse(&mut newparser("set a()"), Scanner::new(), false, false), "BindingElement expected", 1, 7);
}
#[test]
fn method_definition_test_errs_16() {
    check_err(MethodDefinition::parse(&mut newparser("set a(h)"), Scanner::new(), false, false), "‘{’ expected", 1, 9);
}
#[test]
fn method_definition_test_errs_17() {
    check_err(MethodDefinition::parse(&mut newparser("set a(h){"), Scanner::new(), false, false), "‘}’ expected", 1, 10);
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
    let (item, _) = MethodDefinition::parse(&mut newparser("get a() { return 1; }"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn method_definition_test_prettyerrors_3() {
    let (item, _) = MethodDefinition::parse(&mut newparser("set a(blue) { this.a=blue; }"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn method_definition_test_prettyerrors_4() {
    let (item, _) = MethodDefinition::parse(&mut newparser("* a(blue) { this.a=blue; }"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn method_definition_test_prettyerrors_5() {
    let (item, _) = MethodDefinition::parse(&mut newparser("async a(blue) { this.a=blue; }"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn method_definition_test_prettyerrors_6() {
    let (item, _) = MethodDefinition::parse(&mut newparser("async *a(blue) { this.a=blue; }"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn method_definition_test_conciseerrors_1() {
    let (item, _) = MethodDefinition::parse(&mut newparser("a(b){c;}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn method_definition_test_conciseerrors_2() {
    let (item, _) = MethodDefinition::parse(&mut newparser("get a() { return 1; }"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn method_definition_test_conciseerrors_3() {
    let (item, _) = MethodDefinition::parse(&mut newparser("set a(blue) { this.a=blue; }"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn method_definition_test_conciseerrors_4() {
    let (item, _) = MethodDefinition::parse(&mut newparser("* a(blue) { this.a=blue; }"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn method_definition_test_conciseerrors_5() {
    let (item, _) = MethodDefinition::parse(&mut newparser("async a(blue) { this.a=blue; }"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn method_definition_test_conciseerrors_6() {
    let (item, _) = MethodDefinition::parse(&mut newparser("async *a(blue) { this.a=blue; }"), Scanner::new(), false, false).unwrap();
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
#[test_case("#standard_method(){}" => vec![JSString::from("standard_method")]; "Standard Method")]
#[test_case("*#generator(){}" => vec![JSString::from("generator")]; "Generator")]
#[test_case("async #async_method(){}" => vec![JSString::from("async_method")]; "Async Method")]
#[test_case("async *#async_gen(){}" => vec![JSString::from("async_gen")]; "Async Generator")]
#[test_case("get #getter(){}" => vec![JSString::from("getter")]; "Getter")]
#[test_case("set #setter(val){}" => vec![JSString::from("setter")]; "Setter")]
fn method_definition_test_private_bound_identifiers(src: &str) -> Vec<JSString> {
    let (item, _) = MethodDefinition::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.private_bound_identifiers()
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
    item.all_private_identifiers_valid(&[JSString::from("valid")])
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
        let (item, _) = MethodDefinition::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.has_direct_super()
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        MethodDefinition::parse(&mut newparser("a(){}"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }
}

mod property_set_parameter_list {
    use super::*;
    use test_case::test_case;

    #[test]
    fn parse() {
        let (pn, scanner) = check(PropertySetParameterList::parse(&mut newparser("a"), Scanner::new()));
        chk_scan(&scanner, 1);
        pretty_check(&*pn, "PropertySetParameterList: a", vec!["FormalParameter: a"]);
        concise_check(&*pn, "IdentifierName: a", vec![]);
        format!("{:?}", pn);
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
        item.all_private_identifiers_valid(&[JSString::from("valid")])
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        PropertySetParameterList::parse(&mut newparser("a"), Scanner::new()).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }
}
