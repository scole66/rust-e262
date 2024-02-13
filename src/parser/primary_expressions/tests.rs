use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

// PRIMARY EXPRESSION
#[test]
fn primary_expression_test_debug() {
    let pe = PrimaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false);
    let (exp, _) = check(pe);
    assert_ne!(format!("{exp:?}"), "");
}
#[test]
fn primary_expression_test_pprint() {
    let (pe1, _) = check(PrimaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false));
    pretty_check(&*pe1, "PrimaryExpression: this", &[]);
    concise_check(&*pe1, "Keyword: this", &[]);
    let (pe2, _) = check(PrimaryExpression::parse(&mut newparser("1"), Scanner::new(), false, false));
    pretty_check(&*pe2, "PrimaryExpression: 1", &["Literal: 1"]);
    concise_check(&*pe2, "Numeric: 1", &[]);
    let (pe3, _) = check(PrimaryExpression::parse(&mut newparser("i"), Scanner::new(), false, false));
    pretty_check(&*pe3, "PrimaryExpression: i", &["IdentifierReference: i"]);
    concise_check(&*pe3, "IdentifierName: i", &[]);
    let (pe4, _) = check(PrimaryExpression::parse(&mut newparser("[]"), Scanner::new(), false, false));
    pretty_check(&*pe4, "PrimaryExpression: [ ]", &["ArrayLiteral: [ ]"]);
    concise_check(&*pe4, "ArrayLiteral: [ ]", &["Punctuator: [", "Punctuator: ]"]);
    let (pe5, _) = check(PrimaryExpression::parse(&mut newparser("{}"), Scanner::new(), false, false));
    pretty_check(&*pe5, "PrimaryExpression: { }", &["ObjectLiteral: { }"]);
    concise_check(&*pe5, "ObjectLiteral: { }", &["Punctuator: {", "Punctuator: }"]);
    let (pe6, _) = check(PrimaryExpression::parse(&mut newparser("(a)"), Scanner::new(), false, false));
    pretty_check(&*pe6, "PrimaryExpression: ( a )", &["ParenthesizedExpression: ( a )"]);
    concise_check(&*pe6, "ParenthesizedExpression: ( a )", &["Punctuator: (", "IdentifierName: a", "Punctuator: )"]);
    let (pe7, _) = check(PrimaryExpression::parse(&mut newparser("`rust`"), Scanner::new(), false, false));
    pretty_check(&*pe7, "PrimaryExpression: `rust`", &["TemplateLiteral: `rust`"]);
    concise_check(&*pe7, "NoSubTemplate: `rust`", &[]);
    let (pe8, _) = check(PrimaryExpression::parse(&mut newparser("/rust/"), Scanner::new(), false, false));
    pretty_check(&*pe8, "PrimaryExpression: /rust/", &[]);
    concise_check(&*pe8, "RegularExpressionLiteral: /rust/", &[]);
}
#[test]
fn primary_expression_test_idref() {
    let pe_res = PrimaryExpression::parse(&mut newparser("blue"), Scanner::new(), false, false);
    let (boxed_pe, scanner) = check(pe_res);
    chk_scan(&scanner, 4);
    assert!(matches!(*boxed_pe, PrimaryExpression::IdentifierReference { .. }));
    assert_eq!(boxed_pe.is_function_definition(), false);
}
#[test]
fn primary_expression_test_literal() {
    let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("371"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(*node, PrimaryExpression::Literal { .. }));
    assert_eq!(node.is_function_definition(), false);
}
#[test]
fn primary_expression_test_this() {
    let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(*node, PrimaryExpression::This { .. }));
    assert_eq!(node.is_function_definition(), false);
}
#[test]
fn primary_expression_test_arraylit() {
    let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("[]"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(*node, PrimaryExpression::ArrayLiteral { .. }));
    assert_eq!(node.is_function_definition(), false);
}
#[test]
fn primary_expression_test_objlit() {
    let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("{}"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(*node, PrimaryExpression::ObjectLiteral { .. }));
    assert_eq!(node.is_function_definition(), false);
}
#[test]
fn primary_expression_test_group() {
    let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("(a)"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(*node, PrimaryExpression::Parenthesized { .. }));
    assert_eq!(node.is_function_definition(), false);
}
#[test]
fn primary_expression_test_func() {
    let (node, scanner) =
        check(PrimaryExpression::parse(&mut newparser("function a(){}"), Scanner::new(), false, false));
    chk_scan(&scanner, 14);
    assert!(matches!(*node, PrimaryExpression::Function { .. }));
    assert_eq!(node.is_function_definition(), true);
    pretty_check(&*node, "PrimaryExpression: function a (  ) {  }", &["FunctionExpression: function a (  ) {  }"]);
    concise_check(
        &*node,
        "FunctionExpression: function a (  ) {  }",
        &["Keyword: function", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
    );
}
#[test]
fn primary_expression_test_generator() {
    let (node, scanner) =
        check(PrimaryExpression::parse(&mut newparser("function *a(b){c;}"), Scanner::new(), false, false));
    chk_scan(&scanner, 18);
    assert!(matches!(*node, PrimaryExpression::Generator { .. }));
    assert_eq!(node.is_function_definition(), true);
    pretty_check(
        &*node,
        "PrimaryExpression: function * a ( b ) { c ; }",
        &["GeneratorExpression: function * a ( b ) { c ; }"],
    );
    concise_check(
        &*node,
        "GeneratorExpression: function * a ( b ) { c ; }",
        &[
            "Keyword: function",
            "Punctuator: *",
            "IdentifierName: a",
            "Punctuator: (",
            "IdentifierName: b",
            "Punctuator: )",
            "Punctuator: {",
            "ExpressionStatement: c ;",
            "Punctuator: }",
        ],
    );
}
#[test]
fn primary_expression_test_async_generator() {
    let (node, scanner) =
        check(PrimaryExpression::parse(&mut newparser("async function *a(b){c;}"), Scanner::new(), false, false));
    chk_scan(&scanner, 24);
    assert!(matches!(*node, PrimaryExpression::AsyncGenerator { .. }));
    assert_eq!(node.is_function_definition(), true);
    pretty_check(
        &*node,
        "PrimaryExpression: async function * a ( b ) { c ; }",
        &["AsyncGeneratorExpression: async function * a ( b ) { c ; }"],
    );
    concise_check(
        &*node,
        "AsyncGeneratorExpression: async function * a ( b ) { c ; }",
        &[
            "Keyword: async",
            "Keyword: function",
            "Punctuator: *",
            "IdentifierName: a",
            "Punctuator: (",
            "IdentifierName: b",
            "Punctuator: )",
            "Punctuator: {",
            "ExpressionStatement: c ;",
            "Punctuator: }",
        ],
    );
}
#[test]
fn primary_expression_test_async_function() {
    let (node, scanner) =
        check(PrimaryExpression::parse(&mut newparser("async function a(b){c;}"), Scanner::new(), false, false));
    chk_scan(&scanner, 23);
    assert!(matches!(*node, PrimaryExpression::AsyncFunction { .. }));
    assert_eq!(node.is_function_definition(), true);
    pretty_check(
        &*node,
        "PrimaryExpression: async function a ( b ) { c ; }",
        &["AsyncFunctionExpression: async function a ( b ) { c ; }"],
    );
    concise_check(
        &*node,
        "AsyncFunctionExpression: async function a ( b ) { c ; }",
        &[
            "Keyword: async",
            "Keyword: function",
            "IdentifierName: a",
            "Punctuator: (",
            "IdentifierName: b",
            "Punctuator: )",
            "Punctuator: {",
            "ExpressionStatement: c ;",
            "Punctuator: }",
        ],
    );
}
#[test]
fn primary_expression_test_regexp() {
    let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("/rust/"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(*node, PrimaryExpression::RegularExpression { .. }));
    assert_eq!(node.is_function_definition(), false);
}
#[test]
fn primary_expression_test_class_expression() {
    let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("class{}"), Scanner::new(), false, false));
    chk_scan(&scanner, 7);
    assert!(matches!(*node, PrimaryExpression::Class { .. }));
    assert_eq!(node.is_function_definition(), true);
    pretty_check(&*node, "PrimaryExpression: class { }", &["ClassExpression: class { }"]);
    concise_check(&*node, "ClassExpression: class { }", &["Keyword: class", "ClassTail: { }"]);
}
#[test]
fn primary_expression_test_prettyerrors_1() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn primary_expression_test_prettyerrors_2() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn primary_expression_test_prettyerrors_3() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("null"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn primary_expression_test_prettyerrors_4() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("[]"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn primary_expression_test_prettyerrors_5() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("{}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn primary_expression_test_prettyerrors_6() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("function (){}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn primary_expression_test_prettyerrors_7() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("`rust`"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn primary_expression_test_prettyerrors_8() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("(0)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn primary_expression_test_prettyerrors_9() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("function *a(){}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn primary_expression_test_prettyerrors_10() {
    let (item, _) =
        PrimaryExpression::parse(&mut newparser("async function *a(){}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn primary_expression_test_prettyerrors_11() {
    let (item, _) =
        PrimaryExpression::parse(&mut newparser("async function a(){}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn primary_expression_test_prettyerrors_12() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("/rust/"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn primary_expression_test_prettyerrors_13() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("class rust{}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn primary_expression_test_conciseerrors_1() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn primary_expression_test_conciseerrors_2() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn primary_expression_test_conciseerrors_3() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("null"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn primary_expression_test_conciseerrors_4() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("[]"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn primary_expression_test_conciseerrors_5() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("{}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn primary_expression_test_conciseerrors_6() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("function (){}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn primary_expression_test_conciseerrors_7() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("`rust`"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn primary_expression_test_conciseerrors_8() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("(0)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn primary_expression_test_conciseerrors_9() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("function *a(){}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn primary_expression_test_conciseerrors_10() {
    let (item, _) =
        PrimaryExpression::parse(&mut newparser("async function *a(){}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn primary_expression_test_conciseerrors_11() {
    let (item, _) =
        PrimaryExpression::parse(&mut newparser("async function a(){}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn primary_expression_test_conciseerrors_12() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("/rust/"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn primary_expression_test_conciseerrors_13() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("class rust{}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn primary_expression_test_contains_01() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn primary_expression_test_contains_02() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn primary_expression_test_contains_03() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn primary_expression_test_contains_04() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("[this]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn primary_expression_test_contains_05() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("[0]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn primary_expression_test_contains_06() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("{a: this}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn primary_expression_test_contains_07() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("{a: 0}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn primary_expression_test_contains_08() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("(this)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn primary_expression_test_contains_09() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("(0)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn primary_expression_test_contains_10() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("`${this}`"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn primary_expression_test_contains_11() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("`${0}`"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn primary_expression_test_contains_12() {
    let (item, _) =
        PrimaryExpression::parse(&mut newparser("function a(){this;}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn primary_expression_test_contains_13() {
    let (item, _) =
        PrimaryExpression::parse(&mut newparser("class a{[this.name](){}}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn primary_expression_test_contains_14() {
    let (item, _) =
        PrimaryExpression::parse(&mut newparser("class a{[b.name](){this;}}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn primary_expression_test_contains_15() {
    let (item, _) =
        PrimaryExpression::parse(&mut newparser("function *a(){this;}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn primary_expression_test_contains_16() {
    let (item, _) =
        PrimaryExpression::parse(&mut newparser("async function a(){this;}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn primary_expression_test_contains_17() {
    let (item, _) =
        PrimaryExpression::parse(&mut newparser("async function *a(){this;}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn primary_expression_test_contains_18() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("/abcd/"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn primary_expression_test_contains_19() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("2048"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
mod primary_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("\"string\"" => Some(String::from("string")); "String Token")]
    #[test_case("string" => None; "Identifier Reference")]
    fn as_string_literal(src: &str) -> Option<String> {
        let (item, _) = PrimaryExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.as_string_literal().map(|st| String::from(st.value))
    }

    #[test_case("this" => true; "this")]
    #[test_case("a" => true; "identifier reference")]
    #[test_case("10" => true; "literal")]
    #[test_case("/bob/" => true; "regular expression")]
    #[test_case("[a.#valid]" => true; "ArrayLiteral valid")]
    #[test_case("{a=b.#valid}" => true; "ObjectLiteral valid")]
    #[test_case("function (){a.#valid;}" => true; "FunctionExpression valid")]
    #[test_case("class {a=b.#valid;}" => true; "ClassExpression valid")]
    #[test_case("function *(){a.#valid;}" => true; "GeneratorExpression valid")]
    #[test_case("async function (){a.#valid;}" => true; "AsyncFunctionExpression valid")]
    #[test_case("async function *(){a.#valid;}" => true; "AsyncGeneratorExpression valid")]
    #[test_case("`${a.#valid}`" => true; "TemplateLiteral valid")]
    #[test_case("(a.#valid)" => true; "Grouping valid")]
    #[test_case("[a.#invalid]" => false; "ArrayLiteral invalid")]
    #[test_case("{a=b.#invalid}" => false; "ObjectLiteral invalid")]
    #[test_case("function (){a.#invalid;}" => false; "FunctionExpression invalid")]
    #[test_case("class {a=b.#invalid;}" => false; "ClassExpression invalid")]
    #[test_case("function *(){a.#invalid;}" => false; "GeneratorExpression invalid")]
    #[test_case("async function (){a.#invalid;}" => false; "AsyncFunctionExpression invalid")]
    #[test_case("async function *(){a.#invalid;}" => false; "AsyncGeneratorExpression invalid")]
    #[test_case("`${a.#invalid}`" => false; "TemplateLiteral invalid")]
    #[test_case("(a.#invalid)" => false; "Grouping invalid")]
    fn all_parameters_valid(src: &str) -> bool {
        let (item, _) = PrimaryExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("this" => false; "This")]
    #[test_case("a" => false; "IdentifierReference")]
    #[test_case("3" => false; "Literal")]
    #[test_case("[]" => true; "ArrayLiteral")]
    #[test_case("{}" => true; "ObjectLiteral")]
    #[test_case("function (){}" => false; "FunctionExpression")]
    #[test_case("class {}" => false; "ClassExpression")]
    #[test_case("function *(){}" => false; "GeneratorExpression")]
    #[test_case("async function (){}" => false; "AsyncFunctionExpression")]
    #[test_case("async function *(){}" => false; "AsyncGeneratorExpression")]
    #[test_case("/a/" => false; "RegularExpressionLiteral")]
    #[test_case("``" => false; "TemplateLiteral")]
    #[test_case("(a)" => false; "ParenthesizedExpression")]
    fn is_object_or_array_literal(src: &str) -> bool {
        PrimaryExpression::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .is_object_or_array_literal()
    }

    #[test_case("this", true => sset(&[]); "this")]
    #[test_case("a", true => sset(&[]); "simple identifier")]
    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "package/strict")]
    #[test_case("yield", false => sset(&[]); "yield/non-strict")]
    #[test_case("3", true => sset(&[]); "literal")]
    #[test_case("[]", true => sset(&[]); "ArrayLiteral")]
    #[test_case("[package]", true => sset(&[PACKAGE_NOT_ALLOWED]); "package-in-array-strict")]
    #[test_case("{b(a=super()){}}", true => sset(&["'super' keyword unexpected here"]); "ObjectLiteral with error")]
    #[test_case("function eval(){}", true => sset(&["identifier not allowed in strict mode: eval"]); "FunctionExpression")]
    #[test_case("class package {}", true => sset(&[PACKAGE_NOT_ALLOWED]); "ClassExpression")]
    #[test_case("function *package(){}", true => sset(&[PACKAGE_NOT_ALLOWED]); "GeneratorExpression")]
    #[test_case("async function package(){}", true => sset(&[PACKAGE_NOT_ALLOWED]); "AsyncFunctionExpression")]
    #[test_case("async function *package(){}", true => sset(&[PACKAGE_NOT_ALLOWED]); "AsyncGeneratorExpression")]
    #[test_case("/a/", true => sset(&[]); "RegularExpressionLiteral")]
    #[test_case("/a/xx", true => sset(&["Unknown regex flag ‘x’ in flags ‘xx’"]); "RegularExpressionLiteral with errors")]
    #[test_case("`${package}`", true => sset(&[PACKAGE_NOT_ALLOWED]); "TemplateLiteral")]
    #[test_case("(package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "ParenthesizedExpression")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        PrimaryExpression::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("this" => true; "this")]
    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("[1]" => true; "array literal")]
    #[test_case("{a:1}" => true; "object literal")]
    #[test_case("function (){}" => true; "function expression")]
    #[test_case("class {}" => true; "class expression")]
    #[test_case("function *(){}" => true; "generator expression")]
    #[test_case("async function (){}" => true; "async fn")]
    #[test_case("async function *(){}" => true; "async gen")]
    #[test_case("/a/" => true; "regex")]
    #[test_case("``" => true; "template")]
    #[test_case("(a)" => false; "parenthesized idref")]
    #[test_case("(1)" => true; "parenthesized literal")]
    fn is_strictly_deletable(src: &str) -> bool {
        PrimaryExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.is_strictly_deletable()
    }

    #[test_case("this" => false; "this")]
    #[test_case("1" => false; "Literal")]
    #[test_case("function arguments(arguments) { return arguments; }" => false; "Func Exp")]
    #[test_case("function *arguments(arguments) { return arguments; }" => false; "Gen Exp")]
    #[test_case("async function arguments(arguments) { return arguments; }" => false; "Async Func Exp")]
    #[test_case("async function *arguments(arguments) { return arguments; }" => false; "Async Gen Exp")]
    #[test_case("/a/" => false; "Regexp")]
    #[test_case("`${arguments}`" => true; "Template (yes)")]
    #[test_case("class bob {value=arguments;}" => true; "Class Exp (yes)")]
    #[test_case("arguments" => true; "IdRef (yes)")]
    #[test_case("[arguments]" => true; "ArrayLit (yes)")]
    #[test_case("{a:arguments}" => true; "ObjectLit (yes)")]
    #[test_case("(arguments)" => true; "Parenthesis (yes)")]
    #[test_case("`${xyzzy}`" => false; "Template (no)")]
    #[test_case("class bob {value=xyzzy;}" => false; "Class Exp (no)")]
    #[test_case("xyzzy" => false; "IdRef (no)")]
    #[test_case("[xyzzy]" => false; "ArrayLit (no)")]
    #[test_case("{a:xyzzy}" => false; "ObjectLit (no)")]
    #[test_case("(xyzzy)" => false; "Parenthesis (no)")]

    fn contains_arguments(src: &str) -> bool {
        PrimaryExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("this", false => ATTKind::Invalid; "this")]
    #[test_case("1", false => ATTKind::Invalid; "literal")]
    #[test_case("[a]", false => ATTKind::Invalid; "array literal")]
    #[test_case("{a}", false => ATTKind::Invalid; "object literal")]
    #[test_case("function (){}", false => ATTKind::Invalid; "function expression")]
    #[test_case("class a{}", false => ATTKind::Invalid; "class expression")]
    #[test_case("function *(){}", false => ATTKind::Invalid; "generator expression")]
    #[test_case("async function (){}", false => ATTKind::Invalid; "async function expression")]
    #[test_case("async function *(){}", false => ATTKind::Invalid; "async generator expression")]
    #[test_case("/a/", false => ATTKind::Invalid; "Regular Expression")]
    #[test_case("``", false => ATTKind::Invalid; "Template Literal")]
    #[test_case("(a)", false => ATTKind::Simple; "Parenthesized Expression (simple)")]
    #[test_case("(this)", false => ATTKind::Invalid; "Parenthesized Expression (invalid)")]
    #[test_case("a", false => ATTKind::Simple; "idref")]
    #[test_case("eval", false => ATTKind::Simple; "eval (not-strict)")]
    #[test_case("eval", true => ATTKind::Invalid; "eval (strict)")]
    #[test_case("(eval)", true => ATTKind::Invalid; "eval (parens) (strict)")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).primary_expression().assignment_target_type(strict)
    }

    #[test_case("this" => false; "this")]
    #[test_case("a" => true; "identifier ref")]
    #[test_case("1" => false; "literal")]
    #[test_case("[1]" => false; "array literal")]
    #[test_case("{a:1}" => false; "object literal")]
    #[test_case("function (){}" => false; "function expression")]
    #[test_case("class {}" => false; "class expression")]
    #[test_case("function *(){}" => false; "generator expression")]
    #[test_case("async function (){}" => false; "async fn")]
    #[test_case("async function *(){}" => false; "async gen")]
    #[test_case("/a/" => false; "regex")]
    #[test_case("``" => false; "template")]
    #[test_case("(a)" => false; "parenthesized")]
    fn is_identifier_ref(src: &str) -> bool {
        Maker::new(src).primary_expression().is_identifier_ref()
    }

    #[test_case("this" => None; "this")]
    #[test_case("a" => ssome("a"); "identifier ref")]
    #[test_case("1" => None; "literal")]
    #[test_case("[1]" => None; "array literal")]
    #[test_case("{a:1}" => None; "object literal")]
    #[test_case("function (){}" => None; "function expression")]
    #[test_case("class {}" => None; "class expression")]
    #[test_case("function *(){}" => None; "generator expression")]
    #[test_case("async function (){}" => None; "async fn")]
    #[test_case("async function *(){}" => None; "async gen")]
    #[test_case("/a/" => None; "regex")]
    #[test_case("``" => None; "template")]
    #[test_case("(a)" => None; "parenthesized")]
    fn identifier_ref(src: &str) -> Option<String> {
        Maker::new(src).primary_expression().identifier_ref().map(|id| id.to_string())
    }

    #[test_case("this" => false; "this")]
    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => false; "literal")]
    #[test_case("[1]" => false; "array literal")]
    #[test_case("{a:1}" => false; "object literal")]
    #[test_case("function (){}" => false; "function expression")]
    #[test_case("class {}" => false; "class expression")]
    #[test_case("function *(){}" => false; "generator expression")]
    #[test_case("async function (){}" => false; "async fn")]
    #[test_case("async function *(){}" => false; "async gen")]
    #[test_case("/a/" => false; "regex")]
    #[test_case("``" => false; "template")]
    #[test_case("(a)" => false; "parenthesized")]
    #[test_case("function bob(){}" => true; "named function expression")]
    #[test_case("class bob {}" => true; "named class expression")]
    #[test_case("function *bob(){}" => true; "named generator expression")]
    #[test_case("async function bob(){}" => true; "named async fn")]
    #[test_case("async function *bob(){}" => true; "named async gen")]
    #[test_case("(class bob {})" => true; "named parenthesized")]
    fn is_named_function(src: &str) -> bool {
        Maker::new(src).primary_expression().is_named_function()
    }

    #[test_case("   this" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 4 } }; "this")]
    #[test_case("   a" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 1 } }; "identifier ref")]
    #[test_case("   1" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 1 } }; "literal")]
    #[test_case("   [1]" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 3 } }; "array literal")]
    #[test_case("   {a:1}" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 5 } }; "object literal")]
    #[test_case("   function (){}" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 13 } }; "function expression")]
    #[test_case("   class {}" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 8 } }; "class expression")]
    #[test_case("   function *(){}" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 14 } }; "generator expression")]
    #[test_case("   async function (){}" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 19 } }; "async fn")]
    #[test_case("   async function *(){}" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 20 } }; "async gen")]
    #[test_case("   /a/" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 3 } }; "regex")]
    #[test_case("   ``" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 2 } }; "template")]
    #[test_case("   (a)" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 3 } }; "parenthesized")]
    fn location(src: &str) -> Location {
        Maker::new(src).primary_expression().location()
    }
}

// LITERAL
#[test]
fn literal_test_debug() {
    let literal = Maker::new("null").literal();
    assert_ne!(format!("{literal:?}"), "");
}
#[test]
fn literal_test_null() {
    let (lit, scanner) = check(Literal::parse(&mut newparser("null"), Scanner::new()));
    chk_scan(&scanner, 4);
    assert!(matches!(
        *lit,
        Literal::NullLiteral {
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 4 } }
        }
    ));
    pretty_check(&*lit, "Literal: null", &[]);
    concise_check(&*lit, "Keyword: null", &[]);
}
#[test]
fn literal_test_boolean_01() {
    let (lit, scanner) = check(Literal::parse(&mut newparser("true"), Scanner::new()));
    chk_scan(&scanner, 4);
    assert!(matches!(
        *lit,
        Literal::BooleanLiteral {
            val: true,
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 4 } }
        }
    ));
    pretty_check(&*lit, "Literal: true", &[]);
    concise_check(&*lit, "Keyword: true", &[]);
}
#[test]
fn literal_test_boolean_02() {
    let (lit, scanner) = check(Literal::parse(&mut newparser("false"), Scanner::new()));
    chk_scan(&scanner, 5);
    assert!(matches!(
        *lit,
        Literal::BooleanLiteral {
            val: false,
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 5 } }
        }
    ));
    pretty_check(&*lit, "Literal: false", &[]);
    concise_check(&*lit, "Keyword: false", &[]);
}
#[test]
fn literal_test_leading_dot() {
    let (lit, scanner) = check(Literal::parse(&mut newparser(".25"), Scanner::new()));
    chk_scan(&scanner, 3);
    assert_eq!(
        *lit,
        Literal::NumericLiteral {
            val: Numeric::Number(0.25),
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 3 } }
        }
    );
    pretty_check(&*lit, "Literal: 0.25", &[]);
    concise_check(&*lit, "Numeric: 0.25", &[]);
}
#[test]
fn literal_test_bigint() {
    let (lit, scanner) = check(Literal::parse(&mut newparser("7173n"), Scanner::new()));
    chk_scan(&scanner, 5);
    assert!(matches!(
        *lit,
        Literal::NumericLiteral {
            val: Numeric::BigInt(_),
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 5 } }
        }
    ));
    pretty_check(&*lit, "Literal: 7173", &[]);
    concise_check(&*lit, "Numeric: 7173", &[]);
    format!("{lit:?}");
}
#[test]
fn literal_test_string() {
    let (lit, scanner) = check(Literal::parse(&mut newparser("'string'"), Scanner::new()));
    chk_scan(&scanner, 8);
    assert!(matches!(
        *lit,
        Literal::StringLiteral {
            val: _,
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 8 } }
        }
    ));
    pretty_check(&*lit, "Literal: 'string'", &[]);
    concise_check(&*lit, "String: 'string'", &[]);
}
#[test]
fn literal_test_debugmark() {
    let (lit, scanner) = check(Literal::parse(&mut newparser("@@H"), Scanner::new()));
    chk_scan(&scanner, 3);
    assert!(matches!(
        *lit,
        Literal::DebugLiteral {
            val: DebugKind::Char('H'),
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 3 } }
        }
    ));
    pretty_check(&*lit, "Literal: @@H", &[]);
    concise_check(&*lit, "Punctuator: @@H", &[]);
}
#[test]
fn literal_test_keyword() {
    check_err(Literal::parse(&mut newparser("function"), Scanner::new()), "Literal expected", 1, 1);
}
#[test]
fn literal_test_punct() {
    check_err(Literal::parse(&mut newparser("*"), Scanner::new()), "Literal expected", 1, 1);
}
#[test]
fn literal_test_prettyerrors_1() {
    let (item, _) = Literal::parse(&mut newparser("null"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn literal_test_prettyerrors_2() {
    let (item, _) = Literal::parse(&mut newparser("true"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn literal_test_prettyerrors_3() {
    let (item, _) = Literal::parse(&mut newparser("0"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn literal_test_prettyerrors_4() {
    let (item, _) = Literal::parse(&mut newparser("'a'"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn literal_test_conciseerrors_1() {
    let (item, _) = Literal::parse(&mut newparser("null"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn literal_test_conciseerrors_2() {
    let (item, _) = Literal::parse(&mut newparser("true"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn literal_test_conciseerrors_3() {
    let (item, _) = Literal::parse(&mut newparser("0"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn literal_test_conciseerrors_4() {
    let (item, _) = Literal::parse(&mut newparser("'a'"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn numeric_has_legacy_octal() {
    assert!(!Numeric::Number(0.0).has_legacy_octal_syntax());
}
#[test]
fn literal_test_contains_01() {
    let (item, _) = Literal::parse(&mut newparser("10"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("\"string\"" => Some(String::from("string")); "String Token")]
#[test_case("10" => None; "Number Token")]
fn literal_test_as_string_literal(src: &str) -> Option<String> {
    let (item, _) = Literal::parse(&mut newparser(src), Scanner::new()).unwrap();
    item.as_string_literal().map(|st| String::from(st.value))
}
mod literal {
    use super::*;
    use test_case::test_case;

    #[test_case("3" => AHashSet::<String>::new(); "Numeric")]
    #[test_case("null" => AHashSet::<String>::new(); "Null")]
    #[test_case("true" => AHashSet::<String>::new(); "Boolean")]
    #[test_case("'a'" => AHashSet::<String>::new(); "String")]
    fn early_errors(src: &str) -> AHashSet<String> {
        Literal::parse(&mut newparser(src), Scanner::new()).unwrap().0.early_errors();
        AHashSet::<String>::new()
    }

    #[test_case("null", "true" => true; "not eq")]
    #[test_case("false", "false" => false; "eq")]
    #[test_case("false", "    false" => true; "not eq by location")]
    fn ne(s1: &str, s2: &str) -> bool {
        let lit1 = Maker::new(s1).literal();
        let lit2 = Maker::new(s2).literal();
        lit1 != lit2
    }

    #[test_case("   null" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 4 } }; "null lit")]
    #[test_case("   true" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 4 } }; "bool lit")]
    #[test_case("   0" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 1 } }; "number lit")]
    #[test_case("   '0'" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 3 } }; "string lit")]
    #[test_case("   @@0" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 3 } }; "debug lit")]
    fn location(src: &str) -> Location {
        Maker::new(src).primary_expression().location()
    }
}

// ELISION
#[test]
fn elision_test_01() {
    check_err(Elisions::parse(&mut newparser(""), Scanner::new()), "‘,’ expected", 1, 1);
}
#[test]
fn elision_test_02() {
    let (e, scanner) = check(Elisions::parse(&mut newparser(",,"), Scanner::new()));
    chk_scan(&scanner, 2);
    assert!(matches!(
        *e,
        Elisions {
            count: 2,
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 2 } }
        }
    ));
}
#[test]
fn elision_test_03() {
    let (e, scanner) = check(Elisions::parse(&mut newparser(",,,]"), Scanner::new()));
    chk_scan(&scanner, 3);
    assert!(matches!(
        *e,
        Elisions {
            count: 3,
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 3 } }
        }
    ));
}
#[test]
fn elision_test_pprint() {
    let (e1, _) = check(Elisions::parse(&mut newparser(","), Scanner::new()));
    pretty_check(&*e1, "Elisions: ,", &[]);
    concise_check(&*e1, "Elisions: ,", &[]);
    let (e2, _) = check(Elisions::parse(&mut newparser(",,,,,,"), Scanner::new()));
    pretty_check(&*e2, "Elisions: , , , , , ,", &[]);
    concise_check(&*e2, "Elisions: , , , , , ,", &[]);
    format!("{e1:?}");
}
#[test]
fn elision_test_prettyerrors_1() {
    let (item, _) = Elisions::parse(&mut newparser(",,,"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn elision_test_conciseerrors_1() {
    let (item, _) = Elisions::parse(&mut newparser(",,,"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn elision_test_contains_01() {
    let (item, _) = Elisions::parse(&mut newparser(",,,"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
mod elision {
    use super::*;
    use test_case::test_case;

    #[test_case("   ," => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 1 } }; "one")]
    fn location(src: &str) -> Location {
        Maker::new(src).elision().location()
    }

    #[test]
    fn cache() {
        let mut parser = newparser(",,,");
        let (node1, scan1) = Elisions::parse(&mut parser, Scanner::new()).unwrap();
        let (node2, scan2) = Elisions::parse(&mut parser, Scanner::new()).unwrap();
        assert!(scan1 == scan2);
        assert!(Rc::ptr_eq(&node1, &node2));
    }
}

// SPREAD ELEMENT
#[test]
fn spread_element_test_empty() {
    check_err(SpreadElement::parse(&mut newparser(""), Scanner::new(), false, false), "‘...’ expected", 1, 1);
    check_err(
        SpreadElement::parse(&mut newparser("..."), Scanner::new(), false, false),
        "AssignmentExpression expected",
        1,
        4,
    );
}
#[test]
fn spread_element_test_assignment_expression() {
    let (_, scanner) = check(SpreadElement::parse(&mut newparser("...1"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
}
#[test]
fn spread_element_test_pretty() {
    let (se, _) = check(SpreadElement::parse(&mut newparser("...1"), Scanner::new(), false, false));
    pretty_check(&*se, "SpreadElement: ... 1", &["AssignmentExpression: 1"]);
    concise_check(&*se, "SpreadElement: ... 1", &["Punctuator: ...", "Numeric: 1"]);
    format!("{se:?}");
}
#[test]
fn spread_element_test_prettyerrors_1() {
    let (item, _) = SpreadElement::parse(&mut newparser("...a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn spread_element_test_conciseerrors_1() {
    let (item, _) = SpreadElement::parse(&mut newparser("...a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn spread_element_test_contains_01() {
    let (item, _) = SpreadElement::parse(&mut newparser("...this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn spread_element_test_contains_02() {
    let (item, _) = SpreadElement::parse(&mut newparser("...a"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

mod spread_element {
    use super::*;
    use test_case::test_case;

    #[test_case("...a.#valid" => true; "valid")]
    #[test_case("...a.#invalid" => false; "invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = SpreadElement::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("...package", true => sset(&[PACKAGE_NOT_ALLOWED]); "... AssignmentExpression")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        SpreadElement::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("...xyzzy" => false; "no")]
    #[test_case("...arguments" => true; "yes")]
    fn contains_arguments(src: &str) -> bool {
        SpreadElement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("   ...z" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 4 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).spread_element().location()
    }
}

// ELEMENT LIST
#[test]
fn element_list_test_01() {
    check_err(
        ElementList::parse(&mut newparser(""), Scanner::new(), false, false),
        "AssignmentExpression or SpreadElement expected",
        1,
        1,
    );
}
#[test]
fn element_list_test_02() {
    let (el, scanner) = check(ElementList::parse(&mut newparser("3"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(*el, ElementList::AssignmentExpression { elision: None, .. }));
    pretty_check(&*el, "ElementList: 3", &["AssignmentExpression: 3"]);
    concise_check(&*el, "Numeric: 3", &[]);
    format!("{:?}", *el);
}
#[test]
fn element_list_test_03() {
    let (el, scanner) = check(ElementList::parse(&mut newparser(",,3"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*el, ElementList::AssignmentExpression{elision: Some(be), ..} if be.count == 2));
    pretty_check(&*el, "ElementList: , , 3", &["Elisions: , ,", "AssignmentExpression: 3"]);
    concise_check(&*el, "ElementList: , , 3", &["Elisions: , ,", "Numeric: 3"]);
}
#[test]
fn element_list_test_05() {
    let (el, scanner) = check(ElementList::parse(&mut newparser("...a"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(*el, ElementList::SpreadElement { elision: None, .. }));
    pretty_check(&*el, "ElementList: ... a", &["SpreadElement: ... a"]);
    concise_check(&*el, "SpreadElement: ... a", &["Punctuator: ...", "IdentifierName: a"]);
}
#[test]
fn element_list_test_06() {
    let (el, scanner) = check(ElementList::parse(&mut newparser(",,...a"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(&*el, ElementList::SpreadElement{elision:Some(be), ..} if be.count == 2));
    pretty_check(&*el, "ElementList: , , ... a", &["Elisions: , ,", "SpreadElement: ... a"]);
    concise_check(&*el, "ElementList: , , ... a", &["Elisions: , ,", "SpreadElement: ... a"]);
}
#[test]
fn element_list_test_07() {
    let (el, scanner) = check(ElementList::parse(&mut newparser("a,b"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*el, ElementList::ElementListAssignmentExpression { elision: None, .. }));
    pretty_check(&*el, "ElementList: a , b", &["ElementList: a", "AssignmentExpression: b"]);
    concise_check(&*el, "ElementList: a , b", &["IdentifierName: a", "Punctuator: ,", "IdentifierName: b"]);
}
#[test]
fn element_list_test_08() {
    let (el, scanner) = check(ElementList::parse(&mut newparser("a,,b"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*el, ElementList::ElementListAssignmentExpression{elision: Some(be), ..} if be.count == 1));
    pretty_check(&*el, "ElementList: a , , b", &["ElementList: a", "Elisions: ,", "AssignmentExpression: b"]);
    concise_check(
        &*el,
        "ElementList: a , , b",
        &["IdentifierName: a", "Punctuator: ,", "Elisions: ,", "IdentifierName: b"],
    );
}
#[test]
fn element_list_test_09() {
    let (el, scanner) = check(ElementList::parse(&mut newparser("a,...b"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(&*el, ElementList::ElementListSpreadElement { elision: None, .. }));
    pretty_check(&*el, "ElementList: a , ... b", &["ElementList: a", "SpreadElement: ... b"]);
    concise_check(&*el, "ElementList: a , ... b", &["IdentifierName: a", "Punctuator: ,", "SpreadElement: ... b"]);
}
#[test]
fn element_list_test_10() {
    let (el, scanner) = check(ElementList::parse(&mut newparser("a,,...b"), Scanner::new(), false, false));
    chk_scan(&scanner, 7);
    assert!(matches!(&*el, ElementList::ElementListSpreadElement{elision: Some(be), ..} if be.count == 1));
    pretty_check(&*el, "ElementList: a , , ... b", &["ElementList: a", "Elisions: ,", "SpreadElement: ... b"]);
    concise_check(
        &*el,
        "ElementList: a , , ... b",
        &["IdentifierName: a", "Punctuator: ,", "Elisions: ,", "SpreadElement: ... b"],
    );
}
#[test]
fn element_list_test_04() {
    let (el, scanner) = check(ElementList::parse(&mut newparser("0,"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(*el, ElementList::AssignmentExpression { elision: None, .. }));
}
#[test]
fn element_list_test_11() {
    check_err(
        ElementList::parse(&mut newparser(",,,,"), Scanner::new(), false, false),
        "AssignmentExpression or SpreadElement expected",
        1,
        5,
    );
}
#[test]
fn element_list_test_12() {
    check_err(
        ElementList::parse(&mut newparser("...@"), Scanner::new(), false, false),
        "AssignmentExpression expected",
        1,
        4,
    );
}
#[test]
fn element_list_test_13() {
    check_err(
        ElementList::parse(&mut newparser("(while)"), Scanner::new(), false, false),
        "Expression, spread pattern, or closing paren expected",
        1,
        2,
    );
}
#[test]
fn element_list_test_prettyerrors_1() {
    let (item, _) = ElementList::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn element_list_test_prettyerrors_2() {
    let (item, _) = ElementList::parse(&mut newparser(",,,a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn element_list_test_prettyerrors_3() {
    let (item, _) = ElementList::parse(&mut newparser("...a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn element_list_test_prettyerrors_4() {
    let (item, _) = ElementList::parse(&mut newparser(",,,...a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn element_list_test_prettyerrors_5() {
    let (item, _) = ElementList::parse(&mut newparser("a,b"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn element_list_test_prettyerrors_6() {
    let (item, _) = ElementList::parse(&mut newparser("a,,,b"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn element_list_test_prettyerrors_7() {
    let (item, _) = ElementList::parse(&mut newparser("a,...b"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn element_list_test_prettyerrors_8() {
    let (item, _) = ElementList::parse(&mut newparser("a,,,...b"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn element_list_test_conciseerrors_1() {
    let (item, _) = ElementList::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn element_list_test_conciseerrors_2() {
    let (item, _) = ElementList::parse(&mut newparser(",,,a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn element_list_test_conciseerrors_3() {
    let (item, _) = ElementList::parse(&mut newparser("...a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn element_list_test_conciseerrors_4() {
    let (item, _) = ElementList::parse(&mut newparser(",,,...a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn element_list_test_conciseerrors_5() {
    let (item, _) = ElementList::parse(&mut newparser("a,b"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn element_list_test_conciseerrors_6() {
    let (item, _) = ElementList::parse(&mut newparser("a,,,b"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn element_list_test_conciseerrors_7() {
    let (item, _) = ElementList::parse(&mut newparser("a,...b"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn element_list_test_conciseerrors_8() {
    let (item, _) = ElementList::parse(&mut newparser("a,,,...b"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn element_list_test_contains_01() {
    let (item, _) = ElementList::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn element_list_test_contains_02() {
    let (item, _) = ElementList::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn element_list_test_contains_03() {
    let (item, _) = ElementList::parse(&mut newparser(",,,this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn element_list_test_contains_04() {
    let (item, _) = ElementList::parse(&mut newparser(",,,a"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn element_list_test_contains_05() {
    let (item, _) = ElementList::parse(&mut newparser("...this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn element_list_test_contains_06() {
    let (item, _) = ElementList::parse(&mut newparser("...a"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn element_list_test_contains_07() {
    let (item, _) = ElementList::parse(&mut newparser(",,,...this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn element_list_test_contains_08() {
    let (item, _) = ElementList::parse(&mut newparser(",,,...a"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn element_list_test_contains_09() {
    let (item, _) = ElementList::parse(&mut newparser("c,this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn element_list_test_contains_10() {
    let (item, _) = ElementList::parse(&mut newparser("c,a"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn element_list_test_contains_11() {
    let (item, _) = ElementList::parse(&mut newparser("c,,,this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn element_list_test_contains_12() {
    let (item, _) = ElementList::parse(&mut newparser("c,,,a"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn element_list_test_contains_13() {
    let (item, _) = ElementList::parse(&mut newparser("c,...this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn element_list_test_contains_14() {
    let (item, _) = ElementList::parse(&mut newparser("c,...a"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn element_list_test_contains_15() {
    let (item, _) = ElementList::parse(&mut newparser("c,,,...this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn element_list_test_contains_16() {
    let (item, _) = ElementList::parse(&mut newparser("c,,,...a"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn element_list_test_contains_17() {
    let (item, _) = ElementList::parse(&mut newparser("this,c"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn element_list_test_contains_18() {
    let (item, _) = ElementList::parse(&mut newparser("this,,,c"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn element_list_test_contains_19() {
    let (item, _) = ElementList::parse(&mut newparser("this,...c"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn element_list_test_contains_20() {
    let (item, _) = ElementList::parse(&mut newparser("this,,,...c"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
mod element_list {
    use super::*;
    use test_case::test_case;

    #[test_case("a.#valid" => true; "AssignmentExpression: valid")]
    #[test_case(",a.#valid" => true; "Elision AssignmentExpression: valid")]
    #[test_case("...a.#valid" => true; "SpreadElement: valid")]
    #[test_case(",...a.#valid" => true; "Elision SpreadElement: valid")]
    #[test_case("a.#valid,b" => true; "ElementList AssignmentExpression: list valid")]
    #[test_case("a,b.#valid" => true; "ElementList AssignmentExpression: expression valid")]
    #[test_case("a.#valid,,b" => true; "ElementList Elision AssignmentExpression: list valid")]
    #[test_case("a,,b.#valid" => true; "ElementList Elision AssignmentExpression: expression valid")]
    #[test_case("a.#valid,...b" => true; "ElementList SpreadElement: list valid")]
    #[test_case("a,...b.#valid" => true; "ElementList SpreadElement: element valid")]
    #[test_case("a.#valid,,...b" => true; "ElementList Elision SpreadElement: list valid")]
    #[test_case("a,,...b.#valid" => true; "ElementList Elision SpreadElement: element valid")]
    #[test_case("a.#invalid" => false; "AssignmentExpression: invalid")]
    #[test_case(",a.#invalid" => false; "Elision AssignmentExpression: invalid")]
    #[test_case("...a.#invalid" => false; "SpreadElement: invalid")]
    #[test_case(",...a.#invalid" => false; "Elision SpreadElement: invalid")]
    #[test_case("a.#invalid,b" => false; "ElementList AssignmentExpression: list invalid")]
    #[test_case("a,b.#invalid" => false; "ElementList AssignmentExpression: expression invalid")]
    #[test_case("a.#invalid,,b" => false; "ElementList Elision AssignmentExpression: list invalid")]
    #[test_case("a,,b.#invalid" => false; "ElementList Elision AssignmentExpression: expression invalid")]
    #[test_case("a.#invalid,...b" => false; "ElementList SpreadElement: list invalid")]
    #[test_case("a,...b.#invalid" => false; "ElementList SpreadElement: element invalid")]
    #[test_case("a.#invalid,,...b" => false; "ElementList Elision SpreadElement: list invalid")]
    #[test_case("a,,...b.#invalid" => false; "ElementList Elision SpreadElement: element invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = ElementList::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "AssignmentExpression: err")]
    #[test_case(",package", true => sset(&[PACKAGE_NOT_ALLOWED]); "Elision AssignmentExpression: err")]
    #[test_case("...package", true => sset(&[PACKAGE_NOT_ALLOWED]); "SpreadElement: err")]
    #[test_case(",...package", true => sset(&[PACKAGE_NOT_ALLOWED]); "Elision SpreadElement: err")]
    #[test_case("package,b", true => sset(&[PACKAGE_NOT_ALLOWED]); "ElementList AssignmentExpression: list err")]
    #[test_case("a,package", true => sset(&[PACKAGE_NOT_ALLOWED]); "ElementList AssignmentExpression: expression err")]
    #[test_case("package,,b", true => sset(&[PACKAGE_NOT_ALLOWED]); "ElementList Elision AssignmentExpression: list err")]
    #[test_case("a,,package", true => sset(&[PACKAGE_NOT_ALLOWED]); "ElementList Elision AssignmentExpression: expression err")]
    #[test_case("package,...b", true => sset(&[PACKAGE_NOT_ALLOWED]); "ElementList SpreadElement: list err")]
    #[test_case("a,...package", true => sset(&[PACKAGE_NOT_ALLOWED]); "ElementList SpreadElement: element err")]
    #[test_case("package,,...b", true => sset(&[PACKAGE_NOT_ALLOWED]); "ElementList Elision SpreadElement: list err")]
    #[test_case("a,,...package", true => sset(&[PACKAGE_NOT_ALLOWED]); "ElementList Elision SpreadElement: element err")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        ElementList::parse(&mut newparser(src), Scanner::new(), false, true).unwrap().0.early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("xyzzy" => false; "AssignmentExpression (no)")]
    #[test_case(",xyzzy" => false; "Elision AssignmentExpression (no)")]
    #[test_case("...xyzzy" => false; "SpreadElement (no)")]
    #[test_case(",...xyzzy" => false; "Elision SpreadElement (no)")]
    #[test_case("xyzzy,bob" => false; "ElementList , AssignmentExpression (no)")]
    #[test_case("xyzzy,,bob" => false; "ElementList , Elision AssignmentExpression (no)")]
    #[test_case("xyzzy,...bob" => false; "ElementList , SpreadElement (no)")]
    #[test_case("xyzzy,,...bob" => false; "ElementList , Elision SpreadElement (no)")]
    #[test_case("arguments" => true; "AssignmentExpression (yes)")]
    #[test_case(",arguments" => true; "Elision AssignmentExpression (yes)")]
    #[test_case("...arguments" => true; "SpreadElement (yes)")]
    #[test_case(",...arguments" => true; "Elision SpreadElement (yes)")]
    #[test_case("arguments,bob" => true; "ElementList , AssignmentExpression (left)")]
    #[test_case("arguments,,bob" => true; "ElementList , Elision AssignmentExpression (left)")]
    #[test_case("arguments,...bob" => true; "ElementList , SpreadElement (left)")]
    #[test_case("arguments,,...bob" => true; "ElementList , Elision SpreadElement (left)")]
    #[test_case("alice,arguments" => true; "ElementList , AssignmentExpression (right)")]
    #[test_case("alice,,arguments" => true; "ElementList , Elision AssignmentExpression (right)")]
    #[test_case("alice,...arguments" => true; "ElementList , SpreadElement (right)")]
    #[test_case("alice,,...arguments" => true; "ElementList , Elision SpreadElement (right)")]
    fn contains_arguments(src: &str) -> bool {
        ElementList::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("   xyzzy" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 5 } }; "AssignmentExpression")]
    #[test_case("   ,xyzzy" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 6 } }; "Elision AssignmentExpression")]
    #[test_case("   ...xyzzy" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 8 } }; "SpreadElement")]
    #[test_case("   ,...xyzzy" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 9 } }; "Elision SpreadElement")]
    #[test_case("   xyzzy,bob" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 9 } }; "ElementList , AssignmentExpression")]
    #[test_case("   xyzzy,,bob" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 10 } }; "ElementList , Elision AssignmentExpression")]
    #[test_case("   xyzzy,...bob" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 12 } }; "ElementList , SpreadElement")]
    #[test_case("   xyzzy,,...bob" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 13 } }; "ElementList , Elision SpreadElement")]
    fn location(src: &str) -> Location {
        Maker::new(src).element_list().location()
    }
}

// ARRAY LITERAL
#[test]
fn array_literal_test_01() {
    let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[]"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(
        &*al,
        ArrayLiteral::Empty {
            elision: None,
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 2 } }
        }
    ));
    pretty_check(&*al, "ArrayLiteral: [ ]", &[]);
    concise_check(&*al, "ArrayLiteral: [ ]", &["Punctuator: [", "Punctuator: ]"]);
    format!("{:?}", &*al);
}
#[test]
fn array_literal_test_02() {
    let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[,]"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(
        matches!(&*al, ArrayLiteral::Empty{elision: Some(be), location: Location{ starting_line:1, starting_column:1, span:Span{ starting_index:0, length:3 } }} if be.count == 1)
    );
    pretty_check(&*al, "ArrayLiteral: [ , ]", &["Elisions: ,"]);
    concise_check(&*al, "ArrayLiteral: [ , ]", &["Punctuator: [", "Elisions: ,", "Punctuator: ]"]);
}
#[test]
fn array_literal_test_03() {
    let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[a]"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(
        &*al,
        ArrayLiteral::ElementList {
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 3 } },
            ..
        }
    ));
    pretty_check(&*al, "ArrayLiteral: [ a ]", &["ElementList: a"]);
    concise_check(&*al, "ArrayLiteral: [ a ]", &["Punctuator: [", "IdentifierName: a", "Punctuator: ]"]);
}
#[test]
fn array_literal_test_04() {
    let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[a,]"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(
        *al,
        ArrayLiteral::ElementListElision {
            elision: None,
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 4 } },
            ..
        }
    ));
    pretty_check(&*al, "ArrayLiteral: [ a , ]", &["ElementList: a"]);
    concise_check(
        &*al,
        "ArrayLiteral: [ a , ]",
        &["Punctuator: [", "IdentifierName: a", "Punctuator: ,", "Punctuator: ]"],
    );
}
#[test]
fn array_literal_test_05() {
    let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[a,,]"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(
        &*al,
        ArrayLiteral::ElementListElision{
            elision: Some(be),
            location: Location { starting_line:1, starting_column:1, span: Span{ starting_index: 0, length: 5 } },
            ..
        } if be.count == 1
    ));
    pretty_check(&*al, "ArrayLiteral: [ a , , ]", &["ElementList: a", "Elisions: ,"]);
    concise_check(
        &*al,
        "ArrayLiteral: [ a , , ]",
        &["Punctuator: [", "IdentifierName: a", "Punctuator: ,", "Elisions: ,", "Punctuator: ]"],
    );
}
#[test]
fn array_literal_test_err_01() {
    check_err(ArrayLiteral::parse(&mut newparser(""), Scanner::new(), false, false), "‘[’ expected", 1, 1);
}
#[test]
fn array_literal_test_err_02() {
    check_err(
        ArrayLiteral::parse(&mut newparser("["), Scanner::new(), false, false),
        "‘,’, ‘]’, or an ElementList expected",
        1,
        2,
    );
}
#[test]
fn array_literal_test_err_03() {
    check_err(
        ArrayLiteral::parse(&mut newparser("[,,"), Scanner::new(), false, false),
        "AssignmentExpression or SpreadElement expected",
        1,
        4,
    );
}
#[test]
fn array_literal_test_err_04() {
    check_err(
        ArrayLiteral::parse(&mut newparser("[a"), Scanner::new(), false, false),
        "one of [‘,’, ‘]’] expected",
        1,
        3,
    );
}
#[test]
fn array_literal_test_err_05() {
    check_err(ArrayLiteral::parse(&mut newparser("[a,"), Scanner::new(), false, false), "‘]’ expected", 1, 4);
}
#[test]
fn array_literal_test_err_06() {
    check_err(ArrayLiteral::parse(&mut newparser("[a,,"), Scanner::new(), false, false), "‘]’ expected", 1, 5);
}
#[test]
fn array_literal_test_prettyerrors_1() {
    let (item, _) = ArrayLiteral::parse(&mut newparser("[]"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn array_literal_test_prettyerrors_2() {
    let (item, _) = ArrayLiteral::parse(&mut newparser("[,,,]"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn array_literal_test_prettyerrors_3() {
    let (item, _) = ArrayLiteral::parse(&mut newparser("[a]"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn array_literal_test_prettyerrors_4() {
    let (item, _) = ArrayLiteral::parse(&mut newparser("[a,]"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn array_literal_test_prettyerrors_5() {
    let (item, _) = ArrayLiteral::parse(&mut newparser("[a,,,,]"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn array_literal_test_conciseerrors_1() {
    let (item, _) = ArrayLiteral::parse(&mut newparser("[]"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn array_literal_test_conciseerrors_2() {
    let (item, _) = ArrayLiteral::parse(&mut newparser("[,,,]"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn array_literal_test_conciseerrors_3() {
    let (item, _) = ArrayLiteral::parse(&mut newparser("[a]"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn array_literal_test_conciseerrors_4() {
    let (item, _) = ArrayLiteral::parse(&mut newparser("[a,]"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn array_literal_test_conciseerrors_5() {
    let (item, _) = ArrayLiteral::parse(&mut newparser("[a,,,,]"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn array_literal_test_contains_01() {
    let (item, _) = ArrayLiteral::parse(&mut newparser("[]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn array_literal_test_contains_02() {
    let (item, _) = ArrayLiteral::parse(&mut newparser("[,,,]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn array_literal_test_contains_03() {
    let (item, _) = ArrayLiteral::parse(&mut newparser("[this]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn array_literal_test_contains_04() {
    let (item, _) = ArrayLiteral::parse(&mut newparser("[a]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn array_literal_test_contains_05() {
    let (item, _) = ArrayLiteral::parse(&mut newparser("[this,]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn array_literal_test_contains_06() {
    let (item, _) = ArrayLiteral::parse(&mut newparser("[0,]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn array_literal_test_contains_07() {
    let (item, _) = ArrayLiteral::parse(&mut newparser("[this,,,,]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn array_literal_test_contains_08() {
    let (item, _) = ArrayLiteral::parse(&mut newparser("[0,,,,]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("[]" => true; "empty brackets")]
#[test_case("[,]" => true; "Elision")]
#[test_case("[a.#valid]" => true; "ElementList valid")]
#[test_case("[a.#valid,]" => true; "ElementList <comma> valid")]
#[test_case("[a.#valid,,]" => true; "ElementList Elision valid")]
#[test_case("[a.#invalid]" => false; "ElementList invalid")]
#[test_case("[a.#invalid,]" => false; "ElementList <comma> invalid")]
#[test_case("[a.#invalid,,]" => false; "ElementList Elision invalid")]
fn array_literal_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ArrayLiteral::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod array_literal {
    use super::*;
    use test_case::test_case;

    #[test_case("[]", true => AHashSet::<String>::new(); "empty")]
    #[test_case("[package]", true => sset(&[PACKAGE_NOT_ALLOWED]); "ElementList")]
    #[test_case("[package,,]", true => sset(&[PACKAGE_NOT_ALLOWED]); "ElementList Elision")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        ArrayLiteral::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("[]" => false; "empty")]
    #[test_case("[,]" => false; "Elision")]
    #[test_case("[a]" => false; "ElementList (no)")]
    #[test_case("[arguments]" => true; "ElementList (yes)")]
    #[test_case("[a,,]" => false; "ElementList Elision (no)")]
    #[test_case("[arguments,,]" => true; "ElementList Elision (yes)")]
    #[test_case("[a,]" => false; "ElementList (Comma) (no)")]
    #[test_case("[arguments,]" => true; "ElementList (Comma) (yes)")]
    fn contains_arguments(src: &str) -> bool {
        ArrayLiteral::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("   []" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 2 } }; "empty")]
    #[test_case("   [,]" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 3 } }; "Elision")]
    #[test_case("   [a]" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 3 } }; "ElementList")]
    #[test_case("   [a,,]" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 5 } }; "ElementList Elision")]
    #[test_case("   [a,]" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 4 } }; "ElementList (Comma)")]
    fn location(src: &str) -> Location {
        Maker::new(src).array_literal().location()
    }
}

// INITIALIZER
#[test]
fn initializer_test_nomatch() {
    check_err(Initializer::parse(&mut newparser(""), Scanner::new(), false, false, false), "‘=’ expected", 1, 1);
    check_err(
        Initializer::parse(&mut newparser("="), Scanner::new(), false, false, false),
        "AssignmentExpression expected",
        1,
        2,
    );
}
#[test]
fn initializer_test_01() {
    let (izer, scanner) = check(Initializer::parse(&mut newparser("=a"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(
        &*izer,
        Initializer {
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 2 } },
            ..
        }
    ));
    pretty_check(&*izer, "Initializer: = a", &["AssignmentExpression: a"]);
    concise_check(&*izer, "Initializer: = a", &["Punctuator: =", "IdentifierName: a"]);
    format!("{:?}", *izer);
}
#[test]
fn initializer_test_prettyerrors_1() {
    let (item, _) = Initializer::parse(&mut newparser("=2"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn initializer_test_conciseerrors_1() {
    let (item, _) = Initializer::parse(&mut newparser("=2"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn initializer_test_cache_01() {
    let mut parser = newparser("='ble'");
    let (node, scanner) = check(Initializer::parse(&mut parser, Scanner::new(), true, false, false));
    let (node2, scanner2) = check(Initializer::parse(&mut parser, Scanner::new(), true, false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn initializer_test_contains_01() {
    let (item, _) = Initializer::parse(&mut newparser("=this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn initializer_test_contains_02() {
    let (item, _) = Initializer::parse(&mut newparser("=0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("=a.#valid" => true; "valid")]
#[test_case("=a.#invalid" => false; "invalid")]
fn initializer_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = Initializer::parse(&mut newparser(src), Scanner::new(), true, false, false).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod initializer {
    use super::*;
    use test_case::test_case;

    #[test_case("=package", true => sset(&[PACKAGE_NOT_ALLOWED]); "normal")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Initializer::parse(&mut newparser(src), Scanner::new(), true, false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("=xyzzy" => false; "no")]
    #[test_case("=arguments" => true; "yes")]
    fn contains_arguments(src: &str) -> bool {
        Initializer::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.contains_arguments()
    }

    #[test_case("   =a" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 2 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).initializer().location()
    }

    #[test_case("=a" => false; "not anon func")]
    #[test_case("=function (){}" => true; "anon func")]
    fn is_anonymous_function_definition(src: &str) -> bool {
        Maker::new(src).initializer().is_anonymous_function_definition()
    }

    #[test_case("=a" => None; "not function")]
    #[test_case("=function a(){}" => None; "named function")]
    #[test_case("=function (){}" => ssome("function (  ) {  }"); "unnamed function")]
    fn anonymous_function_definition(src: &str) -> Option<String> {
        Maker::new(src).initializer().anonymous_function_definition().map(|fd| fd.to_string())
    }
}

// COVER INITIALIZED NAME
#[test]
fn cover_initialized_name_test_nomatch_1() {
    check_err(CoverInitializedName::parse(&mut newparser(""), Scanner::new(), false, false), "not an identifier", 1, 1);
}
#[test]
fn cover_initialized_name_test_nomatch_2() {
    check_err(CoverInitializedName::parse(&mut newparser("a"), Scanner::new(), false, false), "‘=’ expected", 1, 2);
}
#[test]
fn cover_initialized_name_test_01() {
    let (cin, scanner) = check(CoverInitializedName::parse(&mut newparser("a=b"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*cin, CoverInitializedName::InitializedName(_, _)));
    pretty_check(&*cin, "CoverInitializedName: a = b", &["IdentifierReference: a", "Initializer: = b"]);
    concise_check(&*cin, "CoverInitializedName: a = b", &["IdentifierName: a", "Initializer: = b"]);
    format!("{:?}", *cin);
}
#[test]
fn cover_initialized_name_test_prettyerrors_1() {
    let (item, _) = CoverInitializedName::parse(&mut newparser("a=2"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn cover_initialized_name_test_conciseerrors_1() {
    let (item, _) = CoverInitializedName::parse(&mut newparser("a=2"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn cover_initialized_name_test_contains_01() {
    let (item, _) = CoverInitializedName::parse(&mut newparser("a=this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn cover_initialized_name_test_contains_02() {
    let (item, _) = CoverInitializedName::parse(&mut newparser("a=0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("a=b.#valid" => true; "valid")]
#[test_case("a=b.#invalid" => false; "invalid")]
fn cover_initialized_name_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = CoverInitializedName::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod cover_initialized_name {
    use super::*;
    use test_case::test_case;

    #[test_case("a=package", true => sset(&[PACKAGE_NOT_ALLOWED]); "exp errs")]
    #[test_case("package=3", true => sset(&[PACKAGE_NOT_ALLOWED]); "id errs")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        CoverInitializedName::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test]
    fn prop_name() {
        let (item, _) = CoverInitializedName::parse(&mut newparser("a=b"), Scanner::new(), true, true).unwrap();
        assert_eq!(item.prop_name(), JSString::from("a"));
    }

    #[test_case("   n=a" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 3 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).cover_initialized_name().location()
    }
}

// COMPUTED PROPERTY NAME
#[test]
fn computed_property_name_test_nomatch_1() {
    check_err(ComputedPropertyName::parse(&mut newparser(""), Scanner::new(), false, false), "‘[’ expected", 1, 1);
}
#[test]
fn computed_property_name_test_nomatch_2() {
    check_err(
        ComputedPropertyName::parse(&mut newparser("["), Scanner::new(), false, false),
        "AssignmentExpression expected",
        1,
        2,
    );
}
#[test]
fn computed_property_name_test_nomatch_3() {
    check_err(ComputedPropertyName::parse(&mut newparser("[a"), Scanner::new(), false, false), "‘]’ expected", 1, 3);
}
#[test]
fn computed_property_name_test_01() {
    let (cpn, scanner) = check(ComputedPropertyName::parse(&mut newparser("[a]"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(
        &*cpn,
        ComputedPropertyName {
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 3 } },
            ..
        }
    ));
    pretty_check(&*cpn, "ComputedPropertyName: [ a ]", &["AssignmentExpression: a"]);
    concise_check(&*cpn, "ComputedPropertyName: [ a ]", &["Punctuator: [", "IdentifierName: a", "Punctuator: ]"]);
    format!("{:?}", &*cpn);
}
#[test]
fn computed_property_name_test_prettyerrors_1() {
    let (item, _) = ComputedPropertyName::parse(&mut newparser("[4]"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn computed_property_name_test_conciseerrors_1() {
    let (item, _) = ComputedPropertyName::parse(&mut newparser("[4]"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn computed_property_name_test_contains_01() {
    let (item, _) = ComputedPropertyName::parse(&mut newparser("[this]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn computed_property_name_test_contains_02() {
    let (item, _) = ComputedPropertyName::parse(&mut newparser("[a]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("[a.#valid]" => true; "valid")]
#[test_case("[a.#invalid]" => false; "invalid")]
fn computed_property_name_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ComputedPropertyName::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod computed_property_name {
    use super::*;
    use test_case::test_case;

    #[test_case("[package]", true => sset(&[PACKAGE_NOT_ALLOWED]); "[ AssignmentExpression ]")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        ComputedPropertyName::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }
    #[test_case("[xyzzy]" => false; "no")]
    #[test_case("[arguments]" => true; "yes")]
    fn contains_arguments(src: &str) -> bool {
        ComputedPropertyName::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("   [3]" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 3 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).computed_property_name().location()
    }

    #[test_case("[computed]" => true; "typical")]
    fn is_computed_property_key(src: &str) -> bool {
        Maker::new(src).computed_property_name().is_computed_property_key()
    }
}

// LITERAL PROPERTY NAME
#[test]
fn literal_property_name_test_none() {
    check_err(
        LiteralPropertyName::parse(&mut newparser(""), Scanner::new()),
        "Identifier, String, or Number expected",
        1,
        1,
    );
}
#[test]
fn literal_property_name_test_01() {
    let (lpn, scanner) = check(LiteralPropertyName::parse(&mut newparser("b"), Scanner::new()));
    chk_scan(&scanner, 1);
    assert!(matches!(
        &*lpn,
        LiteralPropertyName::IdentifierName {
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 1 } },
            ..
        }
    ));
    pretty_check(&*lpn, "LiteralPropertyName: b", &[]);
    concise_check(&*lpn, "IdentifierName: b", &[]);
    format!("{:?}", *lpn);
}
#[test]
fn literal_property_name_test_02() {
    let (lpn, scanner) = check(LiteralPropertyName::parse(&mut newparser("'b'"), Scanner::new()));
    chk_scan(&scanner, 3);
    assert!(matches!(
        &*lpn,
        LiteralPropertyName::StringLiteral {
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 3 } },
            ..
        }
    ));
    pretty_check(&*lpn, "LiteralPropertyName: 'b'", &[]);
    concise_check(&*lpn, "String: 'b'", &[]);
}
#[test]
fn literal_property_name_test_03() {
    let (lpn, scanner) = check(LiteralPropertyName::parse(&mut newparser("0"), Scanner::new()));
    chk_scan(&scanner, 1);
    assert!(matches!(
        &*lpn,
        LiteralPropertyName::NumericLiteral {
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 1 } },
            ..
        }
    ));
    pretty_check(&*lpn, "LiteralPropertyName: 0", &[]);
    concise_check(&*lpn, "Numeric: 0", &[]);
}
#[test]
fn literal_property_name_test_04() {
    let (lpn, scanner) = check(LiteralPropertyName::parse(&mut newparser("1n"), Scanner::new()));
    chk_scan(&scanner, 2);
    assert!(matches!(
        &*lpn,
        LiteralPropertyName::NumericLiteral {
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 2 } },
            ..
        }
    ));
    pretty_check(&*lpn, "LiteralPropertyName: 1", &[]);
    concise_check(&*lpn, "Numeric: 1", &[]);
}
#[test]
fn literal_property_name_test_prettyerrors_1() {
    let (item, _) = LiteralPropertyName::parse(&mut newparser("a"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn literal_property_name_test_prettyerrors_2() {
    let (item, _) = LiteralPropertyName::parse(&mut newparser("0"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn literal_property_name_test_prettyerrors_3() {
    let (item, _) = LiteralPropertyName::parse(&mut newparser("'a'"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn literal_property_name_test_conciseerrors_1() {
    let (item, _) = LiteralPropertyName::parse(&mut newparser("a"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn literal_property_name_test_conciseerrors_2() {
    let (item, _) = LiteralPropertyName::parse(&mut newparser("0"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn literal_property_name_test_conciseerrors_3() {
    let (item, _) = LiteralPropertyName::parse(&mut newparser("'a'"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn literal_property_name_test_contains_01() {
    let (item, _) = LiteralPropertyName::parse(&mut newparser("'a'"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
mod literal_property_name {
    use super::*;
    use test_case::test_case;

    #[test_case("bob" => JSString::from("bob"); "identifier")]
    #[test_case("'lit'" => JSString::from("lit"); "string literal")]
    #[test_case("45" => JSString::from("45"); "numeric 64-bit")]
    #[test_case("7732n" => JSString::from("7732"); "numeric bigint")]
    fn prop_name(src: &str) -> JSString {
        let (item, _) = LiteralPropertyName::parse(&mut newparser(src), Scanner::new()).unwrap();
        item.prop_name()
    }

    #[test_case("   a" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 1 } }; "identifier")]
    #[test_case("   'a'" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 3 } }; "string")]
    #[test_case("   3" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 1 } }; "numeric")]
    fn location(src: &str) -> Location {
        Maker::new(src).literal_property_name().location()
    }

    #[test_case("ident" => false; "typical")]
    fn is_computed_property_key(src: &str) -> bool {
        Maker::new(src).literal_property_name().is_computed_property_key()
    }
}

// PROPERTY NAME
#[test]
fn property_name_test_nomatch() {
    check_err(PropertyName::parse(&mut newparser(""), Scanner::new(), false, false), "PropertyName expected", 1, 1);
}
#[test]
fn property_name_test_01() {
    let (pn, scanner) = check(PropertyName::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pn, PropertyName::LiteralPropertyName(_)));
    pretty_check(&*pn, "PropertyName: a", &["LiteralPropertyName: a"]);
    concise_check(&*pn, "IdentifierName: a", &[]);
    format!("{:?}", *pn);
}
#[test]
fn property_name_test_02() {
    let (pn, scanner) = check(PropertyName::parse(&mut newparser("[a]"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*pn, PropertyName::ComputedPropertyName(_)));
    pretty_check(&*pn, "PropertyName: [ a ]", &["ComputedPropertyName: [ a ]"]);
    concise_check(&*pn, "ComputedPropertyName: [ a ]", &["Punctuator: [", "IdentifierName: a", "Punctuator: ]"]);
    format!("{:?}", *pn);
}
#[test]
fn property_name_test_prettyerrors_1() {
    let (item, _) = PropertyName::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn property_name_test_prettyerrors_2() {
    let (item, _) = PropertyName::parse(&mut newparser("[0]"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn property_name_test_conciseerrors_1() {
    let (item, _) = PropertyName::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn property_name_test_conciseerrors_2() {
    let (item, _) = PropertyName::parse(&mut newparser("[0]"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn property_name_test_contains_01() {
    let (item, _) = PropertyName::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn property_name_test_contains_02() {
    let (item, _) = PropertyName::parse(&mut newparser("[this]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn property_name_test_contains_03() {
    let (item, _) = PropertyName::parse(&mut newparser("[0]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn property_name_test_computed_property_contains_01() {
    let (item, _) = PropertyName::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::This), false);
}
#[test]
fn property_name_test_computed_property_contains_02() {
    let (item, _) = PropertyName::parse(&mut newparser("[this]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::This), true);
}
#[test]
fn property_name_test_computed_property_contains_03() {
    let (item, _) = PropertyName::parse(&mut newparser("[0]"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::This), false);
}

mod property_name {
    use super::*;
    use test_case::test_case;

    #[test_case("a" => true; "literal property name")]
    #[test_case("[a.#valid]" => true; "computed property name valid")]
    #[test_case("[a.#invalid]" => false; "computed property name invalid")]
    fn private_identifiers_valid(src: &str) -> bool {
        let (item, _) = PropertyName::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package", true => sset(&[]); "LiteralPropertyName")]
    #[test_case("[package]", true => sset(&[PACKAGE_NOT_ALLOWED]); "ComputedPropertyName")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        PropertyName::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("a" => Some(JSString::from("a")); "normal")]
    #[test_case("[67 + 3]" => None; "computed")]
    fn prop_name(src: &str) -> Option<JSString> {
        let (item, _) = PropertyName::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.prop_name()
    }

    #[test_case("arguments" => false; "LPN")]
    #[test_case("[arguments]" => true; "CPN (yes)")]
    #[test_case("[xyzyz]" => false; "CPN (no)")]
    fn contains_arguments(src: &str) -> bool {
        PropertyName::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test]
    fn cache() {
        let mut parser = newparser("killroy");
        let (node1, scan1) = PropertyName::parse(&mut parser, Scanner::new(), true, true).unwrap();
        let (node2, scan2) = PropertyName::parse(&mut parser, Scanner::new(), true, true).unwrap();
        assert!(scan1 == scan2);
        assert!(Rc::ptr_eq(&node1, &node2));
    }

    #[test_case("   jdj" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 3 } }; "literal name")]
    #[test_case("   [a]" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 3 } }; "computed name")]
    fn location(src: &str) -> Location {
        Maker::new(src).property_name().location()
    }

    #[test_case("ident" => false; "identifier")]
    #[test_case("[computed]" => true; "computed")]
    fn is_computed_property_key(src: &str) -> bool {
        Maker::new(src).property_name().is_computed_property_key()
    }
}

// PROPERTY DEFINITION
#[test]
fn property_definition_test_01() {
    let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pd, PropertyDefinition::IdentifierReference(_)));
    pretty_check(&*pd, "PropertyDefinition: a", &["IdentifierReference: a"]);
    concise_check(&*pd, "IdentifierName: a", &[]);
    format!("{:?}", *pd);
}
#[test]
fn property_definition_test_02() {
    let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("a=b"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*pd, PropertyDefinition::CoverInitializedName(_)));
    pretty_check(&*pd, "PropertyDefinition: a = b", &["CoverInitializedName: a = b"]);
    concise_check(&*pd, "CoverInitializedName: a = b", &["IdentifierName: a", "Initializer: = b"]);
}
#[test]
fn property_definition_test_03() {
    let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("a:b"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*pd, PropertyDefinition::PropertyNameAssignmentExpression(_, _)));
    pretty_check(&*pd, "PropertyDefinition: a : b", &["PropertyName: a", "AssignmentExpression: b"]);
    concise_check(&*pd, "PropertyDefinition: a : b", &["IdentifierName: a", "Punctuator: :", "IdentifierName: b"]);
}
#[test]
fn property_definition_test_04() {
    let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("...a"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(
        &*pd,
        PropertyDefinition::AssignmentExpression(
            _,
            Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 4 } }
        )
    ));
    pretty_check(&*pd, "PropertyDefinition: ... a", &["AssignmentExpression: a"]);
    concise_check(&*pd, "PropertyDefinition: ... a", &["Punctuator: ...", "IdentifierName: a"]);
}
#[test]
fn property_definition_test_05() {
    let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("a(){}"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*pd, PropertyDefinition::MethodDefinition(..)));
    pretty_check(&*pd, "PropertyDefinition: a (  ) {  }", &["MethodDefinition: a (  ) {  }"]);
    concise_check(
        &*pd,
        "MethodDefinition: a (  ) {  }",
        &["IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
    );
}
#[test]
fn property_definition_test_nomatch_1() {
    check_err(
        PropertyDefinition::parse(&mut newparser(""), Scanner::new(), false, false),
        "PropertyName expected",
        1,
        1,
    );
}
#[test]
fn property_definition_test_nomatch_2() {
    check_err(
        PropertyDefinition::parse(&mut newparser("..."), Scanner::new(), false, false),
        "AssignmentExpression expected",
        1,
        4,
    );
}
#[test]
fn property_definition_test_nomatch_3() {
    check_err(PropertyDefinition::parse(&mut newparser("3"), Scanner::new(), false, false), "‘:’ expected", 1, 2);
}
#[test]
fn property_definition_test_nomatch_4() {
    check_err(
        PropertyDefinition::parse(&mut newparser("3:"), Scanner::new(), false, false),
        "AssignmentExpression expected",
        1,
        3,
    );
}
#[test]
fn property_definition_test_prettyerrors_1() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn property_definition_test_prettyerrors_2() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("a=2"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn property_definition_test_prettyerrors_3() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("a:2"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn property_definition_test_prettyerrors_4() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("a(){}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn property_definition_test_prettyerrors_5() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("...a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn property_definition_test_conciseerrors_1() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn property_definition_test_conciseerrors_2() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("a=2"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn property_definition_test_conciseerrors_3() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("a:2"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn property_definition_test_conciseerrors_4() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("a(){}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn property_definition_test_conciseerrors_5() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("...a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn property_definition_test_contains_01() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn property_definition_test_contains_02() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("a=this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn property_definition_test_contains_03() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("a=0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn property_definition_test_contains_04() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("[this]: 10"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn property_definition_test_contains_05() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("a: this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn property_definition_test_contains_06() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("a: 0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn property_definition_test_contains_07() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("[this](){}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn property_definition_test_contains_08() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("a(){this;}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn property_definition_test_contains_09() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("...this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn property_definition_test_contains_10() {
    let (item, _) = PropertyDefinition::parse(&mut newparser("...obj"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("a" => true; "IdentifierReference")]
#[test_case("a=b.#valid" => true; "CoverInitializedName valid")]
#[test_case("[a.#valid]:b" => true; "PropertyName:AssignmentExpression name valid")]
#[test_case("a:b.#valid" => true; "PropertyName:AssignmentExpression expr valid")]
#[test_case("a(){b.#valid;}" => true; "MethodDefinition valid")]
#[test_case("...a.#valid" => true; "...AssignmentExpression valid")]
#[test_case("a=b.#invalid" => false; "CoverInitializedName invalid")]
#[test_case("[a.#invalid]:b" => false; "PropertyName:AssignmentExpression name invalid")]
#[test_case("a:b.#invalid" => false; "PropertyName:AssignmentExpression expr invalid")]
#[test_case("a(){b.#invalid;}" => false; "MethodDefinition invalid")]
#[test_case("...a.#invalid" => false; "...AssignmentExpression invalid")]
fn property_definition_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = PropertyDefinition::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod property_definition {
    use super::*;
    use test_case::test_case;

    const BAD_DESTRUCTURE: &str = "Illegal destructuring syntax in non-destructuring context";
    const UNEXPECTED_PRIVATE: &str = "Private identifier unexpected here";
    const UNEXPECTED_SUPER: &str = "'super' keyword unexpected here";

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "identifier")]
    #[test_case("package=b", true => sset(&[PACKAGE_NOT_ALLOWED, BAD_DESTRUCTURE]); "cover init")]
    #[test_case("[package]:interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "property name")]
    #[test_case("[package](){}", true => sset(&[PACKAGE_NOT_ALLOWED]); "method def")]
    #[test_case("...package", true => sset(&[PACKAGE_NOT_ALLOWED]); "spread element")]
    #[test_case("a(b=super()){}", true => sset(&[UNEXPECTED_SUPER]); "HasDirectSuper of MethodDefinition")]
    #[test_case("#a(){}", true => sset(&[UNEXPECTED_PRIVATE]); "unexpected private id in MethodDef")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        PropertyDefinition::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("a" => Some(JSString::from("a")); "identifier")]
    #[test_case("a=b" => Some(JSString::from("a")); "cover init")]
    #[test_case("a:3" => Some(JSString::from("a")); "property name")]
    #[test_case("a(){}" => Some(JSString::from("a")); "method def")]
    #[test_case("...3" => None; "spread element")]
    fn prop_name(src: &str) -> Option<JSString> {
        let (item, _) = PropertyDefinition::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.prop_name()
    }

    #[test_case("__proto__" => 0; "identifier")]
    #[test_case("__proto__=b" => 0; "cover init")]
    #[test_case("a:3" => 0; "property name")]
    #[test_case("__proto__:3" => 1; "proto detected")]
    #[test_case("__proto__(){}" => 0; "method def")]
    #[test_case("...__proto__" => 0; "spread element")]
    fn special_proto_count(src: &str) -> u64 {
        let (item, _) = PropertyDefinition::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.special_proto_count()
    }

    #[test_case("arguments" => true; "IdRef (yes)")]
    #[test_case("arguments=0" => false; "CoverInitializedName")]
    #[test_case("[arguments]:bob" => true; "Name : AE (left)")]
    #[test_case("bob:arguments" => true; "Name : AE (right)")]
    #[test_case("[arguments](){}" => true; "MethodDef (yes)")]
    #[test_case("...arguments" => true; "spread (yes)")]
    #[test_case("xyzzy" => false; "IdRef (no)")]
    #[test_case("[xyzzy]:bob" => false; "Name : AE (no)")]
    #[test_case("[xyzzy](){}" => false; "MethodDef (no)")]
    #[test_case("...xyzzy" => false; "spread (no)")]
    fn contains_arguments(src: &str) -> bool {
        PropertyDefinition::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("   jdj" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 3 } }; "idref")]
    #[test_case("   a=b" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 3 } }; "covered")]
    #[test_case("   a:b" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 3 } }; "colon-form")]
    #[test_case("   ...a" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 4 } }; "spread")]
    #[test_case("   a(){}" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 5 } }; "method")]
    fn location(src: &str) -> Location {
        Maker::new(src).property_definition().location()
    }
}

// PROPERTY DEFINITION LIST
#[test]
fn property_definition_list_test_01() {
    let (pdl, scanner) = check(PropertyDefinitionList::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pdl, PropertyDefinitionList::OneDef(_)));
    pretty_check(&*pdl, "PropertyDefinitionList: a", &["PropertyDefinition: a"]);
    concise_check(&*pdl, "IdentifierName: a", &[]);
    format!("{:?}", *pdl);
}
#[test]
fn property_definition_list_test_02() {
    let (pdl, scanner) = check(PropertyDefinitionList::parse(&mut newparser("a,"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pdl, PropertyDefinitionList::OneDef(_)));
    pretty_check(&*pdl, "PropertyDefinitionList: a", &["PropertyDefinition: a"]);
    concise_check(&*pdl, "IdentifierName: a", &[]);
}
#[test]
fn property_definition_list_test_03() {
    let (pdl, scanner) = check(PropertyDefinitionList::parse(&mut newparser("a,b"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*pdl, PropertyDefinitionList::ManyDefs(_, _)));
    pretty_check(&*pdl, "PropertyDefinitionList: a , b", &["PropertyDefinitionList: a", "PropertyDefinition: b"]);
    concise_check(&*pdl, "PropertyDefinitionList: a , b", &["IdentifierName: a", "Punctuator: ,", "IdentifierName: b"]);
}
#[test]
fn property_definition_list_test_04() {
    check_err(
        PropertyDefinitionList::parse(&mut newparser(""), Scanner::new(), false, false),
        "PropertyName expected",
        1,
        1,
    );
}
#[test]
fn property_definition_list_test_prettyerrors_1() {
    let (item, _) = PropertyDefinitionList::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn property_definition_list_test_prettyerrors_2() {
    let (item, _) = PropertyDefinitionList::parse(&mut newparser("a,b"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn property_definition_list_test_conciseerrors_1() {
    let (item, _) = PropertyDefinitionList::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn property_definition_list_test_conciseerrors_2() {
    let (item, _) = PropertyDefinitionList::parse(&mut newparser("a,b"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn property_definition_list_test_contains_01() {
    let (item, _) = PropertyDefinitionList::parse(&mut newparser("a=this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn property_definition_list_test_contains_02() {
    let (item, _) = PropertyDefinitionList::parse(&mut newparser("a=0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn property_definition_list_test_contains_03() {
    let (item, _) = PropertyDefinitionList::parse(&mut newparser("a=this,b=2"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn property_definition_list_test_contains_04() {
    let (item, _) = PropertyDefinitionList::parse(&mut newparser("a=0,b=this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn property_definition_list_test_contains_05() {
    let (item, _) = PropertyDefinitionList::parse(&mut newparser("a=0,b=2"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

mod property_definition_list {
    use super::*;
    use test_case::test_case;

    #[test_case("a:b.#valid" => true; "Item valid")]
    #[test_case("a:b.#valid,c" => true; "List head valid")]
    #[test_case("a,b:c.#valid" => true; "List tail vaild")]
    #[test_case("a:b.#invalid" => false; "Item invalid")]
    #[test_case("a:b.#invalid,c" => false; "List head invalid")]
    #[test_case("a,b:c.#invalid" => false; "List tail invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = PropertyDefinitionList::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("[package]:3", true => sset(&[PACKAGE_NOT_ALLOWED]); "item")]
    #[test_case("[package]:3,b", true => sset(&[PACKAGE_NOT_ALLOWED]); "list head")]
    #[test_case("a,[package]:3", true => sset(&[PACKAGE_NOT_ALLOWED]); "list tail")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        PropertyDefinitionList::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("a:x" => 0; "one item, not proto")]
    #[test_case("__proto__:x" => 1; "one item, proto")]
    #[test_case("__proto__:x,a:3" => 1; "two items, first proto")]
    #[test_case("a:3,__proto__:0" => 1; "two items, second proto")]
    #[test_case("__proto__:a,__proto__:b" => 2; "two items, all proto")]
    #[test_case("p:3,x:4" => 0; "two items, no proto")]
    fn special_proto_count(src: &str) -> u64 {
        let (item, _) = PropertyDefinitionList::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.special_proto_count()
    }

    #[test_case("arguments" => true; "item (yes)")]
    #[test_case("arguments,bob" => true; "list, item (left)")]
    #[test_case("bob,arguments" => true; "list, item (right)")]
    #[test_case("xyzzy" => false; "item (no)")]
    #[test_case("xyzzy,bob" => false; "list, item (no)")]
    fn contains_arguments(src: &str) -> bool {
        PropertyDefinitionList::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("   a" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 1 } }; "item")]
    #[test_case("   a,b" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 3 } }; "list")]
    fn location(src: &str) -> Location {
        Maker::new(src).property_definition_list().location()
    }
}

// OBJECT LITERAL
#[test]
fn object_literal_test_01() {
    let (ol, scanner) = check(ObjectLiteral::parse(&mut newparser("{}"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(
        &*ol,
        ObjectLiteral::Empty {
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 2 } }
        }
    ));
    pretty_check(&*ol, "ObjectLiteral: { }", &[]);
    concise_check(&*ol, "ObjectLiteral: { }", &["Punctuator: {", "Punctuator: }"]);
    format!("{:?}", *ol);
}
#[test]
fn object_literal_test_02() {
    let (ol, scanner) = check(ObjectLiteral::parse(&mut newparser("{a:b}"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(
        &*ol,
        ObjectLiteral::Normal {
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 5 } },
            ..
        }
    ));
    pretty_check(&*ol, "ObjectLiteral: { a : b }", &["PropertyDefinitionList: a : b"]);
    concise_check(&*ol, "ObjectLiteral: { a : b }", &["Punctuator: {", "PropertyDefinition: a : b", "Punctuator: }"]);
}
#[test]
fn object_literal_test_03() {
    let (ol, scanner) = check(ObjectLiteral::parse(&mut newparser("{a:b,}"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(
        &*ol,
        ObjectLiteral::TrailingComma {
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 6 } },
            ..
        }
    ));
    pretty_check(&*ol, "ObjectLiteral: { a : b , }", &["PropertyDefinitionList: a : b"]);
    concise_check(
        &*ol,
        "ObjectLiteral: { a : b , }",
        &["Punctuator: {", "PropertyDefinition: a : b", "Punctuator: ,", "Punctuator: }"],
    );
}
#[test]
fn object_literal_test_04() {
    check_err(ObjectLiteral::parse(&mut newparser(""), Scanner::new(), false, false), "‘{’ expected", 1, 1);
}
#[test]
fn object_literal_test_05() {
    check_err(ObjectLiteral::parse(&mut newparser("{"), Scanner::new(), false, false), "‘}’ expected", 1, 2);
}
#[test]
fn object_literal_test_06() {
    check_err(
        ObjectLiteral::parse(&mut newparser("{a:b"), Scanner::new(), false, false),
        "one of [‘}’, ‘,’] expected",
        1,
        5,
    );
}
#[test]
fn object_literal_test_07() {
    check_err(ObjectLiteral::parse(&mut newparser("{a:b,"), Scanner::new(), false, false), "‘}’ expected", 1, 6);
}
#[test]
fn object_literal_test_prettyerrors_1() {
    let (item, _) = ObjectLiteral::parse(&mut newparser("{}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn object_literal_test_prettyerrors_2() {
    let (item, _) = ObjectLiteral::parse(&mut newparser("{a:b}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn object_literal_test_prettyerrors_3() {
    let (item, _) = ObjectLiteral::parse(&mut newparser("{A:B,}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn object_literal_test_conciseerrors_1() {
    let (item, _) = ObjectLiteral::parse(&mut newparser("{}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn object_literal_test_conciseerrors_2() {
    let (item, _) = ObjectLiteral::parse(&mut newparser("{a:b}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn object_literal_test_conciseerrors_3() {
    let (item, _) = ObjectLiteral::parse(&mut newparser("{A:B,}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn object_literal_test_contains_01() {
    let (item, _) = ObjectLiteral::parse(&mut newparser("{}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn object_literal_test_contains_02() {
    let (item, _) = ObjectLiteral::parse(&mut newparser("{a=this}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn object_literal_test_contains_03() {
    let (item, _) = ObjectLiteral::parse(&mut newparser("{a=2}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn object_literal_test_contains_04() {
    let (item, _) = ObjectLiteral::parse(&mut newparser("{a=this,}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn object_literal_test_contains_05() {
    let (item, _) = ObjectLiteral::parse(&mut newparser("{a=0,}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

mod object_literal {
    use super::*;
    use test_case::test_case;

    #[test_case("{}" => true; "empty")]
    #[test_case("{a:b.#valid}" => true; "List valid")]
    #[test_case("{a:b.#valid,}" => true; "List comma valid")]
    #[test_case("{a:b.#invalid}" => false; "List invalid")]
    #[test_case("{a:b.#invalid,}" => false; "List comma invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = ObjectLiteral::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    const DUP_PROTO: &str = "Duplicate __proto__ fields are not allowed in object literals";

    #[test_case("{}", true => AHashSet::<String>::new(); "empty")]
    #[test_case("{a:package}", true => sset(&[PACKAGE_NOT_ALLOWED]); "list")]
    #[test_case("{a:package,}", true => sset(&[PACKAGE_NOT_ALLOWED]); "trailing comma")]
    #[test_case("{__proto__:a,__proto__:b}", true => sset(&[DUP_PROTO]); "duplicate proto")]
    #[test_case("{__proto__:a,__proto__:b,}", true => sset(&[DUP_PROTO]); "duplicate proto; trailing comma")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        ObjectLiteral::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("{}" => false; "{} (empty)")]
    #[test_case("{xyzzy}" => false; "{ PDL } (no)")]
    #[test_case("{xyzzy,}" => false; "{ PDL, } (comma) (no)")]
    #[test_case("{arguments}" => true; "{ PDL } (yes)")]
    #[test_case("{arguments,}" => true; "{ PDL, } (comma) (yes)")]
    fn contains_arguments(src: &str) -> bool {
        ObjectLiteral::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("   {}" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 2 } }; "empty")]
    #[test_case("   {a:10}" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 6 } }; "typical")]
    #[test_case("   {a:10,}" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 7 } }; "trailing comma")]
    fn location(src: &str) -> Location {
        Maker::new(src).object_literal().location()
    }
}

// PARENTHESIZED EXPRESSION
#[test]
fn parenthesized_expression_test_01() {
    let (pe, scanner) = check(ParenthesizedExpression::parse(&mut newparser("(a)"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert_eq!(
        pe.location,
        Location { starting_column: 1, starting_line: 1, span: Span { starting_index: 0, length: 3 } }
    );
    pretty_check(&*pe, "ParenthesizedExpression: ( a )", &["Expression: a"]);
    concise_check(&*pe, "ParenthesizedExpression: ( a )", &["Punctuator: (", "IdentifierName: a", "Punctuator: )"]);
    format!("{pe:?}");
    assert_eq!(pe.is_function_definition(), false);
}
#[test]
fn parenthesized_expression_test_02() {
    check_err(ParenthesizedExpression::parse(&mut newparser(""), Scanner::new(), false, false), "‘(’ expected", 1, 1);
}
#[test]
fn parenthesized_expression_test_03() {
    check_err(
        ParenthesizedExpression::parse(&mut newparser("("), Scanner::new(), false, false),
        "Expression expected",
        1,
        2,
    );
}
#[test]
fn parenthesized_expression_test_04() {
    check_err(ParenthesizedExpression::parse(&mut newparser("(0"), Scanner::new(), false, false), "‘)’ expected", 1, 3);
}
#[test]
fn parenthesized_expression_test_prettyerrors_1() {
    let (item, _) = ParenthesizedExpression::parse(&mut newparser("(0)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn parenthesized_expression_test_conciseerrors_1() {
    let (item, _) = ParenthesizedExpression::parse(&mut newparser("(0)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn parenthesized_expression_test_contains_01() {
    let (item, _) = ParenthesizedExpression::parse(&mut newparser("(this)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn parenthesized_expression_test_contains_02() {
    let (item, _) = ParenthesizedExpression::parse(&mut newparser("(1)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

mod parenthesized_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("(a.#valid)" => true; "valid")]
    #[test_case("(a.#invalid)" => false; "invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = ParenthesizedExpression::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("(package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "( Expression )")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        ParenthesizedExpression::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("(a)" => false; "parenthesized idref")]
    #[test_case("(1)" => true; "parenthesized literal")]
    fn is_strictly_deletable(src: &str) -> bool {
        ParenthesizedExpression::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .is_strictly_deletable()
    }

    #[test_case("(arguments)" => true; "yes")]
    #[test_case("(xyzzy)" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        ParenthesizedExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("(this)", false => ATTKind::Invalid; "invalid")]
    #[test_case("(a)", false => ATTKind::Simple; "valid")]
    #[test_case("(eval)", false => ATTKind::Simple; "not-strict eval")]
    #[test_case("(eval)", true => ATTKind::Invalid; "strict eval")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).parenthesized_expression().assignment_target_type(strict)
    }

    #[test_case("(function a(){})" => true; "named")]
    #[test_case("(function (){})" => false; "unnamed")]
    fn is_named_function(src: &str) -> bool {
        Maker::new(src).parenthesized_expression().is_named_function()
    }

    #[test_case("   (a)" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 3 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).parenthesized_expression().location()
    }
}

// TEMPLATE MIDDLE LIST
#[test]
fn template_middle_list_test_01() {
    let (tml, scanner) = check(TemplateMiddleList::parse(&mut newparser("}a${0"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(
        &*tml,
        TemplateMiddleList::ListHead {
            location: Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 5 } },
            ..
        }
    ));
    pretty_check(&*tml, "TemplateMiddleList: }a${ 0", &["Expression: 0"]);
    concise_check(&*tml, "TemplateMiddleList: }a${ 0", &["TemplateMiddle: }a${", "Numeric: 0"]);
    format!("{tml:?}");
}
#[test]
fn template_middle_list_test_02() {
    let (tml, scanner) =
        check(TemplateMiddleList::parse(&mut newparser("}${a}${b}"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 8);
    println!("{tml:?}");
    assert!(matches!(&*tml, TemplateMiddleList::ListMid(_, _, _, _)));
    pretty_check(&*tml, "TemplateMiddleList: }${ a }${ b", &["TemplateMiddleList: }${ a", "Expression: b"]);
    concise_check(
        &*tml,
        "TemplateMiddleList: }${ a }${ b",
        &["TemplateMiddleList: }${ a", "TemplateMiddle: }${", "IdentifierName: b"],
    );
    format!("{tml:?}");
}
#[test]
fn template_middle_list_test_03() {
    check_err(
        TemplateMiddleList::parse(&mut newparser(""), Scanner::new(), false, false, false),
        "TemplateMiddle expected",
        1,
        1,
    );
    check_err(
        TemplateMiddleList::parse(&mut newparser("}abc${@"), Scanner::new(), false, false, false),
        "Expression expected",
        1,
        7,
    );
}
#[test]
fn template_middle_list_test_04() {
    let (tml, scanner) =
        check(TemplateMiddleList::parse(&mut newparser("}${a}${@}"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*tml, TemplateMiddleList::ListHead { .. }));
    pretty_check(&*tml, "TemplateMiddleList: }${ a", &["Expression: a"]);
    concise_check(&*tml, "TemplateMiddleList: }${ a", &["TemplateMiddle: }${", "IdentifierName: a"]);
    format!("{tml:?}");
}
#[test]
fn template_middle_list_test_prettyerrors_1() {
    let (item, _) = TemplateMiddleList::parse(&mut newparser("}${0"), Scanner::new(), false, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn template_middle_list_test_prettyerrors_2() {
    let (item, _) = TemplateMiddleList::parse(&mut newparser("}${0}${1"), Scanner::new(), false, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn template_middle_list_test_conciseerrors_1() {
    let (item, _) = TemplateMiddleList::parse(&mut newparser("}${0"), Scanner::new(), false, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn template_middle_list_test_conciseerrors_2() {
    let (item, _) = TemplateMiddleList::parse(&mut newparser("}${0}${1"), Scanner::new(), false, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn template_middle_list_test_contains_01() {
    let (item, _) = TemplateMiddleList::parse(&mut newparser("}${a"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn template_middle_list_test_contains_02() {
    let (item, _) = TemplateMiddleList::parse(&mut newparser("}${this"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn template_middle_list_test_contains_03() {
    let (item, _) =
        TemplateMiddleList::parse(&mut newparser("}${this}${a"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn template_middle_list_test_contains_04() {
    let (item, _) =
        TemplateMiddleList::parse(&mut newparser("}${a}${this"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn template_middle_list_test_contains_05() {
    let (item, _) = TemplateMiddleList::parse(&mut newparser("}${a}${a"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
mod template_middle_list {
    use super::*;
    use test_case::test_case;

    #[test_case("}${a.#valid" => true; "Item valid")]
    #[test_case("}${a.#valid}${b" => true; "List head valid")]
    #[test_case("}${a}${b.#valid" => true; "List tail valid")]
    #[test_case("}${a.#invalid" => false; "Item invalid")]
    #[test_case("}${a.#invalid}${b" => false; "List head invalid")]
    #[test_case("}${a}${b.#invalid" => false; "List tail invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let item = TemplateMiddleList::parse(&mut newparser(src), Scanner::new(), false, false, false).unwrap().0;
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    const RE_ESCAPE_ONE: &str = "Invalid character escape in template literal";

    #[test_case("}${package", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "mid exp; exp bad")]
    #[test_case("}\\u{}${0", true, false => sset(&[RE_ESCAPE_ONE]); "mid exp; mid bad")]
    #[test_case("}\\u{}${0", true, true => sset(&[]); "mid exp; mid bad, but tagged")]
    #[test_case("}list${package}${3", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "list mid exp; list bad")]
    #[test_case("}list${1}\\u{}${3", true, false => sset(&[RE_ESCAPE_ONE]); "list mid exp; mid bad")]
    #[test_case("}list${1}\\u{}${3", true, true => sset(&[]); "list mid exp; mid bad, but tagged")]
    #[test_case("}list${1}${package", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "list mid exp; exp bad")]
    fn early_errors(src: &str, strict: bool, tagged: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        TemplateMiddleList::parse(&mut newparser(src), Scanner::new(), false, true, tagged)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("}\\u{66}${0", true => vec![Some(JSString::from("\\u{66}"))]; "item-raw")]
    #[test_case("}\\u{66}${0", false => vec![Some(JSString::from("f"))]; "item-cooked")]
    #[test_case("}a${9}\\u{66}${0", true => vec![Some(JSString::from("a")), Some(JSString::from("\\u{66}"))]; "list-raw")]
    #[test_case("}a${9}\\u{66}${0", false => vec![Some(JSString::from("a")), Some(JSString::from("f"))]; "list-cooked")]
    fn template_strings(src: &str, raw: bool) -> Vec<Option<JSString>> {
        let item = TemplateMiddleList::parse(&mut newparser(src), Scanner::new(), true, true, false).unwrap().0;
        item.template_strings(raw)
    }

    #[test_case("}${arguments" => true; "TM Expression (yes)")]
    #[test_case("}${xyzzy" => false; "TM Expression (no)")]
    #[test_case("}${arguments}${bob" => true; "TML TM Exp (left)")]
    #[test_case("}${bob}${arguments" => true; "TML TM Exp (right)")]
    #[test_case("}${bob}${xyzzy" => false; "TML TM Exp (no)")]
    fn contains_arguments(src: &str) -> bool {
        TemplateMiddleList::parse(&mut newparser(src), Scanner::new(), true, true, false)
            .unwrap()
            .0
            .contains_arguments()
    }
}

// TEMPLATE SPANS
#[test]
fn template_spans_test_01() {
    let (ts, scanner) = check(TemplateSpans::parse(&mut newparser("}done`"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(&*ts, TemplateSpans::Tail { .. }));
    pretty_check(&*ts, "TemplateSpans: }done`", &[]);
    concise_check(&*ts, "TemplateTail: }done`", &[]);
    format!("{ts:?}");
}
#[test]
fn template_spans_test_02() {
    let (ts, scanner) = check(TemplateSpans::parse(&mut newparser("}${a}done`"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 10);
    assert!(matches!(&*ts, TemplateSpans::List { .. }));
    pretty_check(&*ts, "TemplateSpans: }${ a }done`", &["TemplateMiddleList: }${ a"]);
    concise_check(&*ts, "TemplateSpans: }${ a }done`", &["TemplateMiddleList: }${ a", "TemplateTail: }done`"]);
    format!("{ts:?}");
}
#[test]
fn template_spans_test_03() {
    check_err(
        TemplateSpans::parse(&mut newparser(""), Scanner::new(), false, false, false),
        "TemplateSpans expected",
        1,
        1,
    );
    check_err(
        TemplateSpans::parse(&mut newparser("}${blue"), Scanner::new(), false, false, false),
        "TemplateTail expected",
        1,
        8,
    );
}
#[test]
fn template_spans_test_prettyerrors_1() {
    let (item, _) = TemplateSpans::parse(&mut newparser("}`"), Scanner::new(), false, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn template_spans_test_prettyerrors_2() {
    let (item, _) = TemplateSpans::parse(&mut newparser("}${0}`"), Scanner::new(), false, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn template_spans_test_conciseerrors_1() {
    let (item, _) = TemplateSpans::parse(&mut newparser("}`"), Scanner::new(), false, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn template_spans_test_conciseerrors_2() {
    let (item, _) = TemplateSpans::parse(&mut newparser("}${0}`"), Scanner::new(), false, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn template_spans_test_contains_01() {
    let (item, _) = TemplateSpans::parse(&mut newparser("}`"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn template_spans_test_contains_02() {
    let (item, _) = TemplateSpans::parse(&mut newparser("} ${ this }`"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn template_spans_test_contains_03() {
    let (item, _) = TemplateSpans::parse(&mut newparser("} ${ a }`"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

mod template_spans {
    use super::*;
    use test_case::test_case;

    #[test_case("}`" => true; "TemplateTail")]
    #[test_case("}${a.#valid}`" => true; "valid")]
    #[test_case("}${a.#invalid}`" => false; "invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = TemplateSpans::parse(&mut newparser(src), Scanner::new(), false, false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    const RE_ESCAPE_ONE: &str = "Invalid character escape in template literal";

    #[test_case("}\\u{}`", true, false => sset(&[RE_ESCAPE_ONE]); "tail; tail bad")]
    #[test_case("}\\u{}`", true, true => sset(&[]); "tail; tail bad, but tagged")]
    #[test_case("}\\u{}${0}`", true, false => sset(&[RE_ESCAPE_ONE]); "list tail; list bad")]
    #[test_case("}${0}\\u{}`", true, false => sset(&[RE_ESCAPE_ONE]); "list tail; tail bad")]
    #[test_case("}${0}\\u{}`", true, true => sset(&[]); "list tail; tail bad, but tagged")]
    fn early_errors(src: &str, strict: bool, tagged: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        TemplateSpans::parse(&mut newparser(src), Scanner::new(), false, true, tagged)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("}\\u{66}`", true => vec![Some(JSString::from("\\u{66}"))]; "tail-raw")]
    #[test_case("}\\u{66}`", false => vec![Some(JSString::from("f"))]; "tail-cooked")]
    #[test_case("}${0}\\u{66}`", true => vec![Some(JSString::from("")), Some(JSString::from("\\u{66}"))]; "list-raw")]
    #[test_case("}${0}\\u{66}`", false => vec![Some(JSString::from("")), Some(JSString::from("f"))]; "list-cooked")]
    fn template_strings(src: &str, raw: bool) -> Vec<Option<JSString>> {
        let (item, _) = TemplateSpans::parse(&mut newparser(src), Scanner::new(), true, true, false).unwrap();
        item.template_strings(raw)
    }

    #[test_case("}`" => false; "Tail")]
    #[test_case("}${arguments}`" => true; "Middle Tail (yes)")]
    #[test_case("}${xyzzy}`" => false; "Middle Tail (no)")]
    fn contains_arguments(src: &str) -> bool {
        TemplateSpans::parse(&mut newparser(src), Scanner::new(), true, true, false).unwrap().0.contains_arguments()
    }

    #[test_case("   }`" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 2 } }; "tail")]
    #[test_case("   }${a}`" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 6 } }; "list")]
    fn location(src: &str) -> Location {
        Maker::new(src).template_spans().location()
    }
}

// SUBSTITUTION TEMPLATE
#[test]
fn substitution_template_test_01() {
    let (st, scanner) =
        check(SubstitutionTemplate::parse(&mut newparser("`${a}`"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 6);
    assert_eq!(st.tagged, false);
    pretty_check(&*st, "SubstitutionTemplate: `${ a }`", &["Expression: a", "TemplateSpans: }`"]);
    concise_check(
        &*st,
        "SubstitutionTemplate: `${ a }`",
        &["TemplateHead: `${", "IdentifierName: a", "TemplateTail: }`"],
    );
    format!("{st:?}");
}
#[test]
fn substitution_template_test_02() {
    check_err(
        SubstitutionTemplate::parse(&mut newparser(""), Scanner::new(), false, false, false),
        "SubstitutionTemplate expected",
        1,
        1,
    );
}
#[test]
fn substitution_template_test_03() {
    check_err(
        SubstitutionTemplate::parse(&mut newparser("`${"), Scanner::new(), false, false, false),
        "Expression expected",
        1,
        4,
    );
}
#[test]
fn substitution_template_test_04() {
    check_err(
        SubstitutionTemplate::parse(&mut newparser("`${a"), Scanner::new(), false, false, false),
        "TemplateSpans expected",
        1,
        5,
    );
}
#[test]
fn substitution_template_test_prettyerrors_1() {
    let (item, _) = SubstitutionTemplate::parse(&mut newparser("`${0}`"), Scanner::new(), false, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn substitution_template_test_conciseerrors_1() {
    let (item, _) = SubstitutionTemplate::parse(&mut newparser("`${0}`"), Scanner::new(), false, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn substitution_template_test_contains_01() {
    let (item, _) =
        SubstitutionTemplate::parse(&mut newparser("`${this}`"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn substitution_template_test_contains_02() {
    let (item, _) =
        SubstitutionTemplate::parse(&mut newparser("`${10}${this}`"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn substitution_template_test_contains_03() {
    let (item, _) =
        SubstitutionTemplate::parse(&mut newparser("`${10}`"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

mod substitution_template {
    use super::*;
    use test_case::test_case;

    #[test_case("`${a.#valid}${b}`" => true; "expr valid")]
    #[test_case("`${a}${b.#valid}`" => true; "spans valid")]
    #[test_case("`${a.#invalid}${b}`" => false; "expr invalid")]
    #[test_case("`${a}${b.#invalid}`" => false; "spans invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = SubstitutionTemplate::parse(&mut newparser(src), Scanner::new(), false, false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    const RE_ESCAPE_ONE: &str = "Invalid character escape in template literal";
    const RE_ESCAPE_TWO: &str = "Invalid escape sequence in template literal";

    #[test_case("`\\u{999999999999}${package}\\u{0x987654321987654321}`", true, false => sset(&[PACKAGE_NOT_ALLOWED, RE_ESCAPE_ONE, RE_ESCAPE_TWO]); "TemplateHead Expression TemplateSpans")]
    #[test_case("`\\u{999999999999}${package}\\u{0x987654321987654321}`", true, true => sset(&[PACKAGE_NOT_ALLOWED]); "tagged")]
    #[test_case("`\\u{99}${package}\\u{98}`", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "not tagged, but no unicode errs")]
    fn early_errors(src: &str, strict: bool, tagged: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).tagged_ok(tagged).substitution_template().early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("`a${0}\\u{66}`", true => vec![Some(JSString::from("a")), Some(JSString::from("\\u{66}"))]; "raw")]
    #[test_case("`a${0}\\u{66}`", false => vec![Some(JSString::from("a")), Some(JSString::from("f"))]; "cooked")]
    fn template_strings(src: &str, raw: bool) -> Vec<Option<JSString>> {
        let (item, _) = SubstitutionTemplate::parse(&mut newparser(src), Scanner::new(), true, true, false).unwrap();
        item.template_strings(raw)
    }

    #[test_case("`${arguments}`" => true; "Head - Expression - Spans (left)")]
    #[test_case("`${bob}${arguments}`" => true; "Head - Expression - Spans (right)")]
    #[test_case("`${xyzzy}`" => false; "Head - Expression - Spans (no)")]
    fn contains_arguments(src: &str) -> bool {
        SubstitutionTemplate::parse(&mut newparser(src), Scanner::new(), true, true, false)
            .unwrap()
            .0
            .contains_arguments()
    }

    #[test_case("   `${a}`" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 6 } }; "sub")]
    fn location(src: &str) -> Location {
        Maker::new(src).substitution_template().location()
    }
}

// TEMPLATE LITERAL
#[test]
fn template_literal_test_01() {
    let (tl, scanner) = check(TemplateLiteral::parse(&mut newparser("`rust`"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(&*tl, TemplateLiteral::NoSubstitutionTemplate { .. }));
    if let TemplateLiteral::NoSubstitutionTemplate { tagged, .. } = &*tl {
        assert!(!*tagged);
    }
    pretty_check(&*tl, "TemplateLiteral: `rust`", &[]);
    concise_check(&*tl, "NoSubTemplate: `rust`", &[]);
    format!("{tl:?}");
}
#[test]
fn template_literal_test_02() {
    let (tl, scanner) = check(TemplateLiteral::parse(&mut newparser("`${a}`"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(&*tl, TemplateLiteral::SubstitutionTemplate(_)));
    pretty_check(&*tl, "TemplateLiteral: `${ a }`", &["SubstitutionTemplate: `${ a }`"]);
    concise_check(
        &*tl,
        "SubstitutionTemplate: `${ a }`",
        &["TemplateHead: `${", "IdentifierName: a", "TemplateTail: }`"],
    );
    format!("{tl:?}");
}
#[test]
fn template_literal_test_03() {
    check_err(
        TemplateLiteral::parse(&mut newparser(""), Scanner::new(), false, false, false),
        "TemplateLiteral expected",
        1,
        1,
    );
    check_err(
        TemplateLiteral::parse(&mut newparser("`${"), Scanner::new(), false, false, false),
        "Expression expected",
        1,
        4,
    );
}
#[test]
fn template_literal_test_prettyerrors_1() {
    let (item, _) = TemplateLiteral::parse(&mut newparser("`${0}`"), Scanner::new(), false, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn template_literal_test_prettyerrors_2() {
    let (item, _) = TemplateLiteral::parse(&mut newparser("``"), Scanner::new(), false, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn template_literal_test_conciseerrors_1() {
    let (item, _) = TemplateLiteral::parse(&mut newparser("`${0}`"), Scanner::new(), false, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn template_literal_test_conciseerrors_2() {
    let (item, _) = TemplateLiteral::parse(&mut newparser("``"), Scanner::new(), false, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn template_literal_test_cache_01() {
    let mut parser = newparser("`Name: ${name}; Phone: ${phone}.`");
    let (node, scanner) = check(TemplateLiteral::parse(&mut parser, Scanner::new(), false, false, false));
    let (node2, scanner2) = check(TemplateLiteral::parse(&mut parser, Scanner::new(), false, false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn template_literal_test_contains_01() {
    let (item, _) = TemplateLiteral::parse(&mut newparser("`nope`"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn template_literal_test_contains_02() {
    let (item, _) = TemplateLiteral::parse(&mut newparser("`${this}`"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn template_literal_test_contains_03() {
    let (item, _) = TemplateLiteral::parse(&mut newparser("`${10}`"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

mod template_literal {
    use super::*;
    use test_case::test_case;

    #[test_case("`a`" => true; "no substitution")]
    #[test_case("`${a.#valid}`" => true; "sub valid")]
    #[test_case("`${a.#invalid}`" => false; "sub invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = TemplateLiteral::parse(&mut newparser(src), Scanner::new(), false, false, false).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    mod early_errors {
        use super::*;
        use test_case::test_case;

        const RE_ESCAPE_TWO: &str = "Invalid escape sequence in template literal";

        #[test_case("``", true, false => sset(&[]); "no-substitution template ok")]
        #[test_case("`\\u{`", true, false => sset(&[RE_ESCAPE_TWO]); "no-substitution bad escape")]
        #[test_case("`\\u{`", true, true => sset(&[]); "no-substitution bad escape; tagged")]
        #[test_case("`${package}`", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "substitution template")]
        fn simple(src: &str, strict: bool, tagged: bool) -> AHashSet<String> {
            setup_test_agent();
            let mut errs = vec![];
            TemplateLiteral::parse(&mut newparser(src), Scanner::new(), false, true, tagged).unwrap().0.early_errors(
                &mut errs,
                strict,
                4_294_967_295,
            );
            AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
        }

        #[test]
        fn complex() {
            let limit = 1000;
            setup_test_agent();
            let mut src = String::with_capacity(2 + 4 * limit);
            src.push('`');
            for _ in 0..limit {
                src.push_str("${0}");
            }
            src.push('`');
            let mut errs = vec![];
            TemplateLiteral::parse(&mut newparser(&src), Scanner::new(), false, true, false).unwrap().0.early_errors(
                &mut errs,
                false,
                limit - 1,
            );
            let err_set =
                AHashSet::<String>::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())));
            assert_eq!(err_set, sset(&["Template literal too complex"]));
        }
    }

    #[test_case("`hello\\u{67}`", true => vec![Some(JSString::from("hello\\u{67}"))]; "nosub-raw")]
    #[test_case("`hello\\u{67}`", false => vec![Some(JSString::from("hellog"))]; "nosub-cooked")]
    #[test_case("`before${expression}a\\u{66}ter`", true => vec![Some(JSString::from("before")), Some(JSString::from("a\\u{66}ter"))]; "sub-raw")]
    #[test_case("`before${expression}a\\u{66}ter`", false => vec![Some(JSString::from("before")), Some(JSString::from("after"))]; "sub-cooked")]
    fn template_strings(src: &str, raw: bool) -> Vec<Option<JSString>> {
        let (item, _) = TemplateLiteral::parse(&mut newparser(src), Scanner::new(), true, true, false).unwrap();
        item.template_strings(raw)
    }

    #[test_case("``" => false; "NoSub")]
    #[test_case("`${arguments}`" => true; "Sub (yes)")]
    #[test_case("`${xyzzy}`" => false; "Sub (no)")]
    fn contains_arguments(src: &str) -> bool {
        TemplateLiteral::parse(&mut newparser(src), Scanner::new(), true, true, false).unwrap().0.contains_arguments()
    }

    #[test_case("   ``" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 2 } }; "no sub")]
    #[test_case("   `${a}`" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 6 } }; "sub")]
    fn location(src: &str) -> Location {
        Maker::new(src).template_literal().location()
    }
}

// COVER PARENTHESIZED EXPRESSION AND ARROW PARAMETER LIST
mod cover_parenthesized_expression_and_arrow_parameter_list {
    use super::*;
    use test_case::test_case;

    #[test]
    fn test_01() {
        let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("()"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 2);
        assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::Empty { .. }));
        pretty_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( )", &[]);
        concise_check(
            &*node,
            "CoverParenthesizedExpressionAndArrowParameterList: ( )",
            &["Punctuator: (", "Punctuator: )"],
        );
        format!("{node:?}");
    }
    #[test]
    fn test_02() {
        let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(8 in [1,2,3])"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 14);
        assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::Expression { .. }));
        pretty_check(
            &*node,
            "CoverParenthesizedExpressionAndArrowParameterList: ( 8 in [ 1 , 2 , 3 ] )",
            &["Expression: 8 in [ 1 , 2 , 3 ]"],
        );
        concise_check(
            &*node,
            "CoverParenthesizedExpressionAndArrowParameterList: ( 8 in [ 1 , 2 , 3 ] )",
            &["Punctuator: (", "RelationalExpression: 8 in [ 1 , 2 , 3 ]", "Punctuator: )"],
        );
        format!("{node:?}");
    }
    #[test]
    fn test_03() {
        let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(8 in a,)"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 9);
        assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::ExpComma { .. }));
        pretty_check(
            &*node,
            "CoverParenthesizedExpressionAndArrowParameterList: ( 8 in a , )",
            &["Expression: 8 in a"],
        );
        concise_check(
            &*node,
            "CoverParenthesizedExpressionAndArrowParameterList: ( 8 in a , )",
            &["Punctuator: (", "RelationalExpression: 8 in a", "Punctuator: ,", "Punctuator: )"],
        );
        format!("{node:?}");
    }
    #[test]
    fn test_04() {
        let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(...a)"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 6);
        assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::Ident { .. }));
        pretty_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( ... a )", &["BindingIdentifier: a"]);
        concise_check(
            &*node,
            "CoverParenthesizedExpressionAndArrowParameterList: ( ... a )",
            &["Punctuator: (", "Punctuator: ...", "IdentifierName: a", "Punctuator: )"],
        );
        format!("{node:?}");
    }
    #[test]
    fn test_05() {
        let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(...{})"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 7);
        assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::Pattern { .. }));
        pretty_check(
            &*node,
            "CoverParenthesizedExpressionAndArrowParameterList: ( ... { } )",
            &["BindingPattern: { }"],
        );
        concise_check(
            &*node,
            "CoverParenthesizedExpressionAndArrowParameterList: ( ... { } )",
            &["Punctuator: (", "Punctuator: ...", "ObjectBindingPattern: { }", "Punctuator: )"],
        );
        format!("{node:?}");
    }
    #[test]
    fn test_06() {
        let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(a,...b)"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 8);
        assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::ExpIdent { .. }));
        pretty_check(
            &*node,
            "CoverParenthesizedExpressionAndArrowParameterList: ( a , ... b )",
            &["Expression: a", "BindingIdentifier: b"],
        );
        concise_check(
            &*node,
            "CoverParenthesizedExpressionAndArrowParameterList: ( a , ... b )",
            &[
                "Punctuator: (",
                "IdentifierName: a",
                "Punctuator: ,",
                "Punctuator: ...",
                "IdentifierName: b",
                "Punctuator: )",
            ],
        );
        format!("{node:?}");
    }
    #[test]
    fn test_07() {
        let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(a,...[])"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 9);
        assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::ExpPattern { .. }));
        pretty_check(
            &*node,
            "CoverParenthesizedExpressionAndArrowParameterList: ( a , ... [ ] )",
            &["Expression: a", "BindingPattern: [ ]"],
        );
        concise_check(
            &*node,
            "CoverParenthesizedExpressionAndArrowParameterList: ( a , ... [ ] )",
            &[
                "Punctuator: (",
                "IdentifierName: a",
                "Punctuator: ,",
                "Punctuator: ...",
                "ArrayBindingPattern: [ ]",
                "Punctuator: )",
            ],
        );
        format!("{node:?}");
    }
    #[test]
    fn test_08() {
        check_err(
            CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser(""), Scanner::new(), false, false),
            "‘(’ expected",
            1,
            1,
        );
    }
    #[test]
    fn test_09() {
        check_err(
            CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("("), Scanner::new(), false, false),
            "Expression, spread pattern, or closing paren expected",
            1,
            2,
        );
    }
    #[test]
    fn test_10() {
        check_err(
            CoverParenthesizedExpressionAndArrowParameterList::parse(
                &mut newparser("(..."),
                Scanner::new(),
                false,
                false,
            ),
            "BindingIdentifier or BindingPattern expected",
            1,
            5,
        );
    }
    #[test]
    fn test_11() {
        check_err(
            CoverParenthesizedExpressionAndArrowParameterList::parse(
                &mut newparser("(...a"),
                Scanner::new(),
                false,
                false,
            ),
            "‘)’ expected",
            1,
            6,
        );
    }
    #[test]
    fn test_12() {
        check_err(
            CoverParenthesizedExpressionAndArrowParameterList::parse(
                &mut newparser("(...[]"),
                Scanner::new(),
                false,
                false,
            ),
            "‘)’ expected",
            1,
            7,
        );
    }
    #[test]
    fn test_13() {
        check_err(
            CoverParenthesizedExpressionAndArrowParameterList::parse(
                &mut newparser("(p"),
                Scanner::new(),
                false,
                false,
            ),
            "‘)’ expected",
            1,
            3,
        );
    }
    #[test]
    fn test_14() {
        check_err(
            CoverParenthesizedExpressionAndArrowParameterList::parse(
                &mut newparser("(p,"),
                Scanner::new(),
                false,
                false,
            ),
            "‘)’ expected",
            1,
            4,
        );
    }
    #[test]
    fn test_prettyerrors_1() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(0)"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn test_prettyerrors_2() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(0,)"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn test_prettyerrors_3() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("()"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn test_prettyerrors_4() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(...a)"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn test_prettyerrors_5() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(...{})"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn test_prettyerrors_6() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(0,...a)"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn test_prettyerrors_7() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(0,...{})"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn test_conciseerrors_1() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(0)"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn test_conciseerrors_2() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(0,)"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn test_conciseerrors_3() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("()"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn test_conciseerrors_4() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(...a)"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn test_conciseerrors_5() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(...{})"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn test_conciseerrors_6() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(0,...a)"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn test_conciseerrors_7() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(0,...{})"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn test_cache_01() {
        let mut parser = newparser("(a+b+c)");
        let (node, scanner) =
            check(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut parser, Scanner::new(), false, false));
        let (node2, scanner2) =
            check(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut parser, Scanner::new(), false, false));
        assert!(scanner == scanner2);
        assert!(Rc::ptr_eq(&node, &node2));
    }

    #[test]
    fn test_contains_01() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(this)"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn test_contains_02() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(a)"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn test_contains_03() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(this,)"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn test_contains_04() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(a,)"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn test_contains_05() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("()"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn test_contains_06() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(...blue)"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn test_contains_07() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(...[a,b,c])"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn test_contains_08() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(this, ...thing)"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn test_contains_09() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(a, ...thing)"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn test_contains_10() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(this, ...[a])"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn test_contains_11() {
        let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(
            &mut newparser("(b, ...[a])"),
            Scanner::new(),
            false,
            false,
        )
        .unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test_case("(package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "Expression")]
    #[test_case("(package,)", true => sset(&[PACKAGE_NOT_ALLOWED]); "Expression+Comma")]
    #[test_case("()", true => sset(&[]); "Empty")]
    #[test_case("(...package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "rest id")]
    #[test_case("(...{package=a})", true => sset(&[PACKAGE_NOT_ALLOWED]); "rest pattern")]
    #[test_case("(package, ...a)", true => sset(&[PACKAGE_NOT_ALLOWED]); "exp rest id; exp bad")]
    #[test_case("(a, ...package)", true => sset(&[PACKAGE_NOT_ALLOWED]); "exp rest id; id bad")]
    #[test_case("(package, ...{a=b})", true => sset(&[PACKAGE_NOT_ALLOWED]); "exp rest pat; exp bad")]
    #[test_case("(a, ...{package=b})", true => sset(&[PACKAGE_NOT_ALLOWED]); "exp rest pat; pat bad")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("   (a)" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 3 } }; "parenthesized")]
    #[test_case("   (a,)" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 4 } }; "Expression+Comma")]
    #[test_case("   ()" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 2 } }; "Empty")]
    #[test_case("   (...a)" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 6 } }; "rest id")]
    #[test_case("   (...{z=a})" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 10 } }; "rest pattern")]
    #[test_case("   (z, ...a)" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 9 } }; "exp rest id; exp")]
    #[test_case("   (z, ...{a=b})" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 13 } }; "exp rest pat; exp")]
    fn location(src: &str) -> Location {
        Maker::new(src).cover_parenthesized_expression_and_arrow_parameter_list().location()
    }
}

mod debug_kind {
    use super::*;
    use test_case::test_case;

    #[test_case(DebugKind::Char('a'), DebugKind::Char('a') => true; "equal")]
    #[test_case(DebugKind::Number(13), DebugKind::Char('A') => false; "unequal")]
    fn eq(left: DebugKind, right: DebugKind) -> bool {
        left == right
    }

    #[test_case(DebugKind::Char('&') => with |s| assert_ne!(s, ""); "char type")]
    fn debug(item: DebugKind) -> String {
        format!("{item:?}")
    }

    #[test_case(DebugKind::Char('%') => "%"; "char type")]
    #[test_case(DebugKind::Number(67) => "(67)"; "number type")]
    fn display(item: DebugKind) -> String {
        format!("{item}")
    }
}
