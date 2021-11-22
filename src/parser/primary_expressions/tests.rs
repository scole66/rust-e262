use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::test_agent;
use test_case::test_case;

// PRIMARY EXPRESSION
#[test]
fn primary_expression_test_debug() {
    let pe = PrimaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false);
    let (exp, _) = check(pe);
    assert_eq!(format!("{:?}", exp), "PrimaryExpression { kind: This }");
}
#[test]
fn primary_expression_test_pprint() {
    let (pe1, _) = check(PrimaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false));
    pretty_check(&*pe1, "PrimaryExpression: this", vec![]);
    concise_check(&*pe1, "Keyword: this", vec![]);
    let (pe2, _) = check(PrimaryExpression::parse(&mut newparser("1"), Scanner::new(), false, false));
    pretty_check(&*pe2, "PrimaryExpression: 1", vec!["Literal: 1"]);
    concise_check(&*pe2, "Numeric: 1", vec![]);
    let (pe3, _) = check(PrimaryExpression::parse(&mut newparser("i"), Scanner::new(), false, false));
    pretty_check(&*pe3, "PrimaryExpression: i", vec!["IdentifierReference: i"]);
    concise_check(&*pe3, "IdentifierName: i", vec![]);
    let (pe4, _) = check(PrimaryExpression::parse(&mut newparser("[]"), Scanner::new(), false, false));
    pretty_check(&*pe4, "PrimaryExpression: [ ]", vec!["ArrayLiteral: [ ]"]);
    concise_check(&*pe4, "ArrayLiteral: [ ]", vec!["Punctuator: [", "Punctuator: ]"]);
    let (pe5, _) = check(PrimaryExpression::parse(&mut newparser("{}"), Scanner::new(), false, false));
    pretty_check(&*pe5, "PrimaryExpression: { }", vec!["ObjectLiteral: { }"]);
    concise_check(&*pe5, "ObjectLiteral: { }", vec!["Punctuator: {", "Punctuator: }"]);
    let (pe6, _) = check(PrimaryExpression::parse(&mut newparser("(a)"), Scanner::new(), false, false));
    pretty_check(&*pe6, "PrimaryExpression: ( a )", vec!["ParenthesizedExpression: ( a )"]);
    concise_check(&*pe6, "ParenthesizedExpression: ( a )", vec!["Punctuator: (", "IdentifierName: a", "Punctuator: )"]);
    let (pe7, _) = check(PrimaryExpression::parse(&mut newparser("`rust`"), Scanner::new(), false, false));
    pretty_check(&*pe7, "PrimaryExpression: `rust`", vec!["TemplateLiteral: `rust`"]);
    concise_check(&*pe7, "NoSubTemplate: `rust`", vec![]);
    let (pe8, _) = check(PrimaryExpression::parse(&mut newparser("/rust/"), Scanner::new(), false, false));
    pretty_check(&*pe8, "PrimaryExpression: /rust/", vec![]);
    concise_check(&*pe8, "RegularExpressionLiteral: /rust/", vec![]);
}
#[test]
fn primary_expression_test_idref() {
    let pe_res = PrimaryExpression::parse(&mut newparser("blue"), Scanner::new(), false, false);
    let (boxed_pe, scanner) = check(pe_res);
    chk_scan(&scanner, 4);
    assert!(matches!(boxed_pe.kind, PrimaryExpressionKind::IdentifierReference(_)));
    assert_eq!(boxed_pe.is_function_definition(), false);
    assert_eq!(boxed_pe.is_identifier_reference(), true);
    assert_eq!(boxed_pe.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn primary_expression_test_literal() {
    let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("371"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(node.kind, PrimaryExpressionKind::Literal(_)));
    assert_eq!(node.is_function_definition(), false);
    assert_eq!(node.is_identifier_reference(), false);
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn primary_expression_test_this() {
    let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(node.kind, PrimaryExpressionKind::This));
    assert_eq!(node.is_function_definition(), false);
    assert_eq!(node.is_identifier_reference(), false);
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn primary_expression_test_arraylit() {
    let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("[]"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(node.kind, PrimaryExpressionKind::ArrayLiteral(_)));
    assert_eq!(node.is_function_definition(), false);
    assert_eq!(node.is_identifier_reference(), false);
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn primary_expression_test_objlit() {
    let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("{}"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(node.kind, PrimaryExpressionKind::ObjectLiteral(_)));
    assert_eq!(node.is_function_definition(), false);
    assert_eq!(node.is_identifier_reference(), false);
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn primary_expression_test_group() {
    let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("(a)"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(node.kind, PrimaryExpressionKind::Parenthesized(_)));
    assert_eq!(node.is_function_definition(), false);
    assert_eq!(node.is_identifier_reference(), false);
    assert_eq!(node.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn primary_expression_test_func() {
    let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("function a(){}"), Scanner::new(), false, false));
    chk_scan(&scanner, 14);
    assert!(matches!(node.kind, PrimaryExpressionKind::Function(..)));
    assert_eq!(node.is_function_definition(), true);
    assert_eq!(node.is_identifier_reference(), false);
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    pretty_check(&*node, "PrimaryExpression: function a (  ) {  }", vec!["FunctionExpression: function a (  ) {  }"]);
    concise_check(&*node, "FunctionExpression: function a (  ) {  }", vec!["Keyword: function", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
}
#[test]
fn primary_expression_test_generator() {
    let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("function *a(b){c;}"), Scanner::new(), false, false));
    chk_scan(&scanner, 18);
    assert!(matches!(node.kind, PrimaryExpressionKind::Generator(..)));
    assert_eq!(node.is_function_definition(), true);
    assert_eq!(node.is_identifier_reference(), false);
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    pretty_check(&*node, "PrimaryExpression: function * a ( b ) { c ; }", vec!["GeneratorExpression: function * a ( b ) { c ; }"]);
    concise_check(
        &*node,
        "GeneratorExpression: function * a ( b ) { c ; }",
        vec!["Keyword: function", "Punctuator: *", "IdentifierName: a", "Punctuator: (", "IdentifierName: b", "Punctuator: )", "Punctuator: {", "ExpressionStatement: c ;", "Punctuator: }"],
    );
}
#[test]
fn primary_expression_test_async_generator() {
    let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("async function *a(b){c;}"), Scanner::new(), false, false));
    chk_scan(&scanner, 24);
    assert!(matches!(node.kind, PrimaryExpressionKind::AsyncGenerator(..)));
    assert_eq!(node.is_function_definition(), true);
    assert_eq!(node.is_identifier_reference(), false);
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    pretty_check(&*node, "PrimaryExpression: async function * a ( b ) { c ; }", vec!["AsyncGeneratorExpression: async function * a ( b ) { c ; }"]);
    concise_check(
        &*node,
        "AsyncGeneratorExpression: async function * a ( b ) { c ; }",
        vec![
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
    let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("async function a(b){c;}"), Scanner::new(), false, false));
    chk_scan(&scanner, 23);
    assert!(matches!(node.kind, PrimaryExpressionKind::AsyncFunction(..)));
    assert_eq!(node.is_function_definition(), true);
    assert_eq!(node.is_identifier_reference(), false);
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    pretty_check(&*node, "PrimaryExpression: async function a ( b ) { c ; }", vec!["AsyncFunctionExpression: async function a ( b ) { c ; }"]);
    concise_check(
        &*node,
        "AsyncFunctionExpression: async function a ( b ) { c ; }",
        vec!["Keyword: async", "Keyword: function", "IdentifierName: a", "Punctuator: (", "IdentifierName: b", "Punctuator: )", "Punctuator: {", "ExpressionStatement: c ;", "Punctuator: }"],
    );
}
#[test]
fn primary_expression_test_regexp() {
    let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("/rust/"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(node.kind, PrimaryExpressionKind::RegularExpression(..)));
    assert_eq!(node.is_function_definition(), false);
    assert_eq!(node.is_identifier_reference(), false);
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn primary_expression_test_class_expression() {
    let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("class{}"), Scanner::new(), false, false));
    chk_scan(&scanner, 7);
    assert!(matches!(node.kind, PrimaryExpressionKind::Class(..)));
    assert_eq!(node.is_function_definition(), true);
    assert_eq!(node.is_identifier_reference(), false);
    assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    pretty_check(&*node, "PrimaryExpression: class { }", vec!["ClassExpression: class { }"]);
    concise_check(&*node, "ClassExpression: class { }", vec!["Keyword: class", "ClassTail: { }"]);
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
    let (item, _) = PrimaryExpression::parse(&mut newparser("async function *a(){}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn primary_expression_test_prettyerrors_11() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("async function a(){}"), Scanner::new(), false, false).unwrap();
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
    let (item, _) = PrimaryExpression::parse(&mut newparser("async function *a(){}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn primary_expression_test_conciseerrors_11() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("async function a(){}"), Scanner::new(), false, false).unwrap();
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
    let (item, _) = PrimaryExpression::parse(&mut newparser("function a(){this;}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn primary_expression_test_contains_13() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("class a{[this.name](){}}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn primary_expression_test_contains_14() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("class a{[b.name](){this;}}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn primary_expression_test_contains_15() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("function *a(){this;}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn primary_expression_test_contains_16() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("async function a(){this;}"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn primary_expression_test_contains_17() {
    let (item, _) = PrimaryExpression::parse(&mut newparser("async function *a(){this;}"), Scanner::new(), false, false).unwrap();
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
#[test_case("\"string\"" => Some(String::from("string")); "String Token")]
#[test_case("string" => None; "Identifier Reference")]
fn primary_expression_test_as_string_literal(src: &str) -> Option<String> {
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
fn primary_expression_test_all_parameters_valid(src: &str) -> bool {
    let (item, _) = PrimaryExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
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
fn primary_expression_test_is_object_or_array_literal(src: &str) -> bool {
    PrimaryExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.is_object_or_array_literal()
}
mod primary_expression {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        PrimaryExpression::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// LITERAL
#[test]
fn literal_test_debug() {
    assert_eq!(format!("{:?}", Literal { kind: LiteralKind::NullLiteral }), "Literal { kind: NullLiteral }");
}
#[test]
fn literal_test_null() {
    let (lit, scanner) = check(Literal::parse(&mut newparser("null"), Scanner::new()));
    chk_scan(&scanner, 4);
    assert!(matches!(lit.kind, LiteralKind::NullLiteral));
    pretty_check(&*lit, "Literal: null", vec![]);
    concise_check(&*lit, "Keyword: null", vec![]);
}
#[test]
fn literal_test_boolean_01() {
    let (lit, scanner) = check(Literal::parse(&mut newparser("true"), Scanner::new()));
    chk_scan(&scanner, 4);
    assert!(matches!(lit.kind, LiteralKind::BooleanLiteral(true)));
    pretty_check(&*lit, "Literal: true", vec![]);
    concise_check(&*lit, "Keyword: true", vec![]);
}
#[test]
fn literal_test_boolean_02() {
    let (lit, scanner) = check(Literal::parse(&mut newparser("false"), Scanner::new()));
    chk_scan(&scanner, 5);
    assert!(matches!(lit.kind, LiteralKind::BooleanLiteral(false)));
    pretty_check(&*lit, "Literal: false", vec![]);
    concise_check(&*lit, "Keyword: false", vec![]);
}
#[test]
fn literal_test_leading_dot() {
    let (lit, scanner) = check(Literal::parse(&mut newparser(".25"), Scanner::new()));
    chk_scan(&scanner, 3);
    assert_eq!(lit.kind, LiteralKind::NumericLiteral(Numeric::Number(0.25)));
    pretty_check(&*lit, "Literal: 0.25", vec![]);
    concise_check(&*lit, "Numeric: 0.25", vec![]);
}
#[test]
fn literal_test_bigint() {
    let (lit, scanner) = check(Literal::parse(&mut newparser("7173n"), Scanner::new()));
    chk_scan(&scanner, 5);
    assert!(matches!(lit.kind, LiteralKind::NumericLiteral(Numeric::BigInt(_))));
    pretty_check(&*lit, "Literal: 7173", vec![]);
    concise_check(&*lit, "Numeric: 7173", vec![]);
    format!("{:?}", lit);
}
#[test]
fn literal_test_string() {
    let (lit, scanner) = check(Literal::parse(&mut newparser("'string'"), Scanner::new()));
    chk_scan(&scanner, 8);
    assert!(matches!(lit.kind, LiteralKind::StringLiteral(_)));
    pretty_check(&*lit, "Literal: 'string'", vec![]);
    concise_check(&*lit, "String: 'string'", vec![]);
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
fn literal_kind_ne() {
    let lk1 = LiteralKind::NumericLiteral(Numeric::Number(99.0));
    let lk2 = LiteralKind::NumericLiteral(Numeric::Number(100.0));
    let lk3 = LiteralKind::BooleanLiteral(false);
    let lk4 = LiteralKind::BooleanLiteral(false);

    assert!(lk1 != lk2);
    assert!(lk1 != lk3);
    assert!(lk3 == lk4);
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
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        Literal::parse(&mut newparser("3"), Scanner::new()).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// ELISION
#[test]
fn elision_test_01() {
    check_err(Elisions::parse(&mut newparser(""), Scanner::new()), "Expected one or more commas", 1, 1);
}
#[test]
fn elision_test_02() {
    let (e, scanner) = check(Elisions::parse(&mut newparser(",,"), Scanner::new()));
    chk_scan(&scanner, 2);
    assert!(matches!(*e, Elisions { count: 2 }));
}
#[test]
fn elision_test_03() {
    let (e, scanner) = check(Elisions::parse(&mut newparser(",,,]"), Scanner::new()));
    chk_scan(&scanner, 3);
    assert!(matches!(*e, Elisions { count: 3 }));
}
#[test]
fn elision_test_pprint() {
    let (e1, _) = check(Elisions::parse(&mut newparser(","), Scanner::new()));
    pretty_check(&*e1, "Elisions: ,", vec![]);
    concise_check(&*e1, "Elisions: ,", vec![]);
    let (e2, _) = check(Elisions::parse(&mut newparser(",,,,,,"), Scanner::new()));
    pretty_check(&*e2, "Elisions: , , , , , ,", vec![]);
    concise_check(&*e2, "Elisions: , , , , , ,", vec![]);
    format!("{:?}", e1);
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
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        Elisions::parse(&mut newparser(","), Scanner::new()).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// SPREAD ELEMENT
#[test]
fn spread_element_test_empty() {
    check_err(SpreadElement::parse(&mut newparser(""), Scanner::new(), false, false), "‘...’ expected", 1, 1);
    check_err(SpreadElement::parse(&mut newparser("..."), Scanner::new(), false, false), "AssignmentExpression expected", 1, 4);
}
#[test]
fn spread_element_test_assignment_expression() {
    let (se, scanner) = check(SpreadElement::parse(&mut newparser("...1"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(*se, SpreadElement::AssignmentExpression(_)));
}
#[test]
fn spread_element_test_pretty() {
    let (se, _) = check(SpreadElement::parse(&mut newparser("...1"), Scanner::new(), false, false));
    pretty_check(&*se, "SpreadElement: ... 1", vec!["AssignmentExpression: 1"]);
    concise_check(&*se, "SpreadElement: ... 1", vec!["Punctuator: ...", "Numeric: 1"]);
    format!("{:?}", se);
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
#[test_case("...a.#valid" => true; "valid")]
#[test_case("...a.#invalid" => false; "invalid")]
fn spread_element_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = SpreadElement::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod spread_element {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        SpreadElement::parse(&mut newparser("...a"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// ELEMENT LIST
#[test]
fn element_list_test_01() {
    check_err(ElementList::parse(&mut newparser(""), Scanner::new(), false, false), "AssignmentExpression or SpreadElement expected", 1, 1);
}
#[test]
fn element_list_test_02() {
    let (el, scanner) = check(ElementList::parse(&mut newparser("3"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(*el, ElementList::AssignmentExpression((None, _))));
    pretty_check(&*el, "ElementList: 3", vec!["AssignmentExpression: 3"]);
    concise_check(&*el, "Numeric: 3", vec![]);
    format!("{:?}", *el);
}
#[test]
fn element_list_test_03() {
    let (el, scanner) = check(ElementList::parse(&mut newparser(",,3"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*el, ElementList::AssignmentExpression((Some(be), _)) if be.count == 2));
    pretty_check(&*el, "ElementList: , , 3", vec!["Elisions: , ,", "AssignmentExpression: 3"]);
    concise_check(&*el, "ElementList: , , 3", vec!["Elisions: , ,", "Numeric: 3"]);
}
#[test]
fn element_list_test_05() {
    let (el, scanner) = check(ElementList::parse(&mut newparser("...a"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(*el, ElementList::SpreadElement((None, _))));
    pretty_check(&*el, "ElementList: ... a", vec!["SpreadElement: ... a"]);
    concise_check(&*el, "SpreadElement: ... a", vec!["Punctuator: ...", "IdentifierName: a"]);
}
#[test]
fn element_list_test_06() {
    let (el, scanner) = check(ElementList::parse(&mut newparser(",,...a"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(&*el, ElementList::SpreadElement((Some(be), _)) if be.count == 2));
    pretty_check(&*el, "ElementList: , , ... a", vec!["Elisions: , ,", "SpreadElement: ... a"]);
    concise_check(&*el, "ElementList: , , ... a", vec!["Elisions: , ,", "SpreadElement: ... a"]);
}
#[test]
fn element_list_test_07() {
    let (el, scanner) = check(ElementList::parse(&mut newparser("a,b"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*el, ElementList::ElementListAssignmentExpression((_, None, _))));
    pretty_check(&*el, "ElementList: a , b", vec!["ElementList: a", "AssignmentExpression: b"]);
    concise_check(&*el, "ElementList: a , b", vec!["IdentifierName: a", "Punctuator: ,", "IdentifierName: b"]);
}
#[test]
fn element_list_test_08() {
    let (el, scanner) = check(ElementList::parse(&mut newparser("a,,b"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*el, ElementList::ElementListAssignmentExpression((_, Some(be), _)) if be.count == 1));
    pretty_check(&*el, "ElementList: a , , b", vec!["ElementList: a", "Elisions: ,", "AssignmentExpression: b"]);
    concise_check(&*el, "ElementList: a , , b", vec!["IdentifierName: a", "Punctuator: ,", "Elisions: ,", "IdentifierName: b"]);
}
#[test]
fn element_list_test_09() {
    let (el, scanner) = check(ElementList::parse(&mut newparser("a,...b"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(&*el, ElementList::ElementListSpreadElement((_, None, _))));
    pretty_check(&*el, "ElementList: a , ... b", vec!["ElementList: a", "SpreadElement: ... b"]);
    concise_check(&*el, "ElementList: a , ... b", vec!["IdentifierName: a", "Punctuator: ,", "SpreadElement: ... b"]);
}
#[test]
fn element_list_test_10() {
    let (el, scanner) = check(ElementList::parse(&mut newparser("a,,...b"), Scanner::new(), false, false));
    chk_scan(&scanner, 7);
    assert!(matches!(&*el, ElementList::ElementListSpreadElement((_, Some(be), _)) if be.count == 1));
    pretty_check(&*el, "ElementList: a , , ... b", vec!["ElementList: a", "Elisions: ,", "SpreadElement: ... b"]);
    concise_check(&*el, "ElementList: a , , ... b", vec!["IdentifierName: a", "Punctuator: ,", "Elisions: ,", "SpreadElement: ... b"]);
}
#[test]
fn element_list_test_04() {
    let (el, scanner) = check(ElementList::parse(&mut newparser("0,"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(*el, ElementList::AssignmentExpression((None, _))));
}
#[test]
fn element_list_test_11() {
    check_err(ElementList::parse(&mut newparser(",,,,"), Scanner::new(), false, false), "AssignmentExpression or SpreadElement expected", 1, 5);
}
#[test]
fn element_list_test_12() {
    check_err(ElementList::parse(&mut newparser("...@"), Scanner::new(), false, false), "AssignmentExpression expected", 1, 4);
}
#[test]
fn element_list_test_13() {
    check_err(ElementList::parse(&mut newparser("(while)"), Scanner::new(), false, false), "Expression, spread pattern, or closing paren expected", 1, 2);
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
fn element_list_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ElementList::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod element_list {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ElementList::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// ARRAY LITERAL
#[test]
fn array_literal_test_01() {
    let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[]"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(&*al, ArrayLiteral::Empty(None)));
    pretty_check(&*al, "ArrayLiteral: [ ]", vec![]);
    concise_check(&*al, "ArrayLiteral: [ ]", vec!["Punctuator: [", "Punctuator: ]"]);
    format!("{:?}", &*al);
}
#[test]
fn array_literal_test_02() {
    let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[,]"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*al, ArrayLiteral::Empty(Some(be)) if be.count == 1));
    pretty_check(&*al, "ArrayLiteral: [ , ]", vec!["Elisions: ,"]);
    concise_check(&*al, "ArrayLiteral: [ , ]", vec!["Punctuator: [", "Elisions: ,", "Punctuator: ]"]);
}
#[test]
fn array_literal_test_03() {
    let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[a]"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*al, ArrayLiteral::ElementList(_)));
    pretty_check(&*al, "ArrayLiteral: [ a ]", vec!["ElementList: a"]);
    concise_check(&*al, "ArrayLiteral: [ a ]", vec!["Punctuator: [", "IdentifierName: a", "Punctuator: ]"]);
}
#[test]
fn array_literal_test_04() {
    let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[a,]"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(*al, ArrayLiteral::ElementListElision(_, None)));
    pretty_check(&*al, "ArrayLiteral: [ a , ]", vec!["ElementList: a"]);
    concise_check(&*al, "ArrayLiteral: [ a , ]", vec!["Punctuator: [", "IdentifierName: a", "Punctuator: ,", "Punctuator: ]"])
}
#[test]
fn array_literal_test_05() {
    let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[a,,]"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*al, ArrayLiteral::ElementListElision(_, Some(be)) if be.count == 1));
    pretty_check(&*al, "ArrayLiteral: [ a , , ]", vec!["ElementList: a", "Elisions: ,"]);
    concise_check(&*al, "ArrayLiteral: [ a , , ]", vec!["Punctuator: [", "IdentifierName: a", "Punctuator: ,", "Elisions: ,", "Punctuator: ]"]);
}
#[test]
fn array_literal_test_err_01() {
    check_err(ArrayLiteral::parse(&mut newparser(""), Scanner::new(), false, false), "‘[’ expected", 1, 1);
}
#[test]
fn array_literal_test_err_02() {
    check_err(ArrayLiteral::parse(&mut newparser("["), Scanner::new(), false, false), "‘,’, ‘]’, or an ElementList expected", 1, 2);
}
#[test]
fn array_literal_test_err_03() {
    check_err(ArrayLiteral::parse(&mut newparser("[,,"), Scanner::new(), false, false), "AssignmentExpression or SpreadElement expected", 1, 4);
}
#[test]
fn array_literal_test_err_04() {
    check_err(ArrayLiteral::parse(&mut newparser("[a"), Scanner::new(), false, false), "One of [‘,’, ‘]’] expected", 1, 3);
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
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod array_literal {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ArrayLiteral::parse(&mut newparser("[]"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// INITIALIZER
#[test]
fn initializer_test_nomatch() {
    check_err(Initializer::parse(&mut newparser(""), Scanner::new(), false, false, false), "‘=’ expected", 1, 1);
    check_err(Initializer::parse(&mut newparser("="), Scanner::new(), false, false, false), "AssignmentExpression expected", 1, 2);
}
#[test]
fn initializer_test_01() {
    let (izer, scanner) = check(Initializer::parse(&mut newparser("=a"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(&*izer, Initializer::AssignmentExpression(_)));
    pretty_check(&*izer, "Initializer: = a", vec!["AssignmentExpression: a"]);
    concise_check(&*izer, "Initializer: = a", vec!["Punctuator: =", "IdentifierName: a"]);
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
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod initializer {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        Initializer::parse(&mut newparser("=a"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// COVER INITIALIZED NAME
#[test]
fn cover_initialized_name_test_nomatch_1() {
    check_err(CoverInitializedName::parse(&mut newparser(""), Scanner::new(), false, false), "Not an identifier", 1, 1);
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
    pretty_check(&*cin, "CoverInitializedName: a = b", vec!["IdentifierReference: a", "Initializer: = b"]);
    concise_check(&*cin, "CoverInitializedName: a = b", vec!["IdentifierName: a", "Initializer: = b"]);
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
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod cover_initialized_name {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        CoverInitializedName::parse(&mut newparser("b=a"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// COMPUTED PROPERTY NAME
#[test]
fn computed_property_name_test_nomatch_1() {
    check_err(ComputedPropertyName::parse(&mut newparser(""), Scanner::new(), false, false), "‘[’ expected", 1, 1);
}
#[test]
fn computed_property_name_test_nomatch_2() {
    check_err(ComputedPropertyName::parse(&mut newparser("["), Scanner::new(), false, false), "AssignmentExpression expected", 1, 2);
}
#[test]
fn computed_property_name_test_nomatch_3() {
    check_err(ComputedPropertyName::parse(&mut newparser("[a"), Scanner::new(), false, false), "‘]’ expected", 1, 3);
}
#[test]
fn computed_property_name_test_01() {
    let (cpn, scanner) = check(ComputedPropertyName::parse(&mut newparser("[a]"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*cpn, ComputedPropertyName::AssignmentExpression(_)));
    pretty_check(&*cpn, "ComputedPropertyName: [ a ]", vec!["AssignmentExpression: a"]);
    concise_check(&*cpn, "ComputedPropertyName: [ a ]", vec!["Punctuator: [", "IdentifierName: a", "Punctuator: ]"]);
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
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod computed_property_name {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ComputedPropertyName::parse(&mut newparser("[a]"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// LITERAL PROPERTY NAME
#[test]
fn literal_property_name_test_none() {
    check_err(LiteralPropertyName::parse(&mut newparser(""), Scanner::new()), "Identifier, String, or Number expected", 1, 1);
}
#[test]
fn literal_property_name_test_01() {
    let (lpn, scanner) = check(LiteralPropertyName::parse(&mut newparser("b"), Scanner::new()));
    chk_scan(&scanner, 1);
    assert!(matches!(&*lpn, LiteralPropertyName::IdentifierName(_)));
    pretty_check(&*lpn, "LiteralPropertyName: b", vec![]);
    concise_check(&*lpn, "IdentifierName: b", vec![]);
    format!("{:?}", *lpn);
}
#[test]
fn literal_property_name_test_02() {
    let (lpn, scanner) = check(LiteralPropertyName::parse(&mut newparser("'b'"), Scanner::new()));
    chk_scan(&scanner, 3);
    assert!(matches!(&*lpn, LiteralPropertyName::StringLiteral(_)));
    pretty_check(&*lpn, "LiteralPropertyName: 'b'", vec![]);
    concise_check(&*lpn, "String: 'b'", vec![]);
}
#[test]
fn literal_property_name_test_03() {
    let (lpn, scanner) = check(LiteralPropertyName::parse(&mut newparser("0"), Scanner::new()));
    chk_scan(&scanner, 1);
    assert!(matches!(&*lpn, LiteralPropertyName::NumericLiteral(_)));
    pretty_check(&*lpn, "LiteralPropertyName: 0", vec![]);
    concise_check(&*lpn, "Numeric: 0", vec![]);
}
#[test]
fn literal_property_name_test_04() {
    let (lpn, scanner) = check(LiteralPropertyName::parse(&mut newparser("1n"), Scanner::new()));
    chk_scan(&scanner, 2);
    assert!(matches!(&*lpn, LiteralPropertyName::NumericLiteral(_)));
    pretty_check(&*lpn, "LiteralPropertyName: 1", vec![]);
    concise_check(&*lpn, "Numeric: 1", vec![]);
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
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        LiteralPropertyName::parse(&mut newparser("b"), Scanner::new()).unwrap().0.early_errors(&mut test_agent(), true);
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
    pretty_check(&*pn, "PropertyName: a", vec!["LiteralPropertyName: a"]);
    concise_check(&*pn, "IdentifierName: a", vec![]);
    format!("{:?}", *pn);
}
#[test]
fn property_name_test_02() {
    let (pn, scanner) = check(PropertyName::parse(&mut newparser("[a]"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*pn, PropertyName::ComputedPropertyName(_)));
    pretty_check(&*pn, "PropertyName: [ a ]", vec!["ComputedPropertyName: [ a ]"]);
    concise_check(&*pn, "ComputedPropertyName: [ a ]", vec!["Punctuator: [", "IdentifierName: a", "Punctuator: ]"]);
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
#[test_case("a" => true; "literal property name")]
#[test_case("[a.#valid]" => true; "computed property name valid")]
#[test_case("[a.#invalid]" => false; "computed property name invalid")]
fn property_name_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = PropertyName::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod property_name {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        PropertyName::parse(&mut newparser("b"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// PROPERTY DEFINITION
#[test]
fn property_definition_test_01() {
    let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pd, PropertyDefinition::IdentifierReference(_)));
    pretty_check(&*pd, "PropertyDefinition: a", vec!["IdentifierReference: a"]);
    concise_check(&*pd, "IdentifierName: a", vec![]);
    format!("{:?}", *pd);
}
#[test]
fn property_definition_test_02() {
    let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("a=b"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*pd, PropertyDefinition::CoverInitializedName(_)));
    pretty_check(&*pd, "PropertyDefinition: a = b", vec!["CoverInitializedName: a = b"]);
    concise_check(&*pd, "CoverInitializedName: a = b", vec!["IdentifierName: a", "Initializer: = b"]);
}
#[test]
fn property_definition_test_03() {
    let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("a:b"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*pd, PropertyDefinition::PropertyNameAssignmentExpression(_, _)));
    pretty_check(&*pd, "PropertyDefinition: a : b", vec!["PropertyName: a", "AssignmentExpression: b"]);
    concise_check(&*pd, "PropertyDefinition: a : b", vec!["IdentifierName: a", "Punctuator: :", "IdentifierName: b"]);
}
#[test]
fn property_definition_test_04() {
    let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("...a"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*pd, PropertyDefinition::AssignmentExpression(_)));
    pretty_check(&*pd, "PropertyDefinition: ... a", vec!["AssignmentExpression: a"]);
    concise_check(&*pd, "PropertyDefinition: ... a", vec!["Punctuator: ...", "IdentifierName: a"]);
}
#[test]
fn property_definition_test_05() {
    let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("a(){}"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*pd, PropertyDefinition::MethodDefinition(..)));
    pretty_check(&*pd, "PropertyDefinition: a (  ) {  }", vec!["MethodDefinition: a (  ) {  }"]);
    concise_check(&*pd, "MethodDefinition: a (  ) {  }", vec!["IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
}
#[test]
fn property_definition_test_nomatch_1() {
    check_err(PropertyDefinition::parse(&mut newparser(""), Scanner::new(), false, false), "PropertyName expected", 1, 1);
}
#[test]
fn property_definition_test_nomatch_2() {
    check_err(PropertyDefinition::parse(&mut newparser("..."), Scanner::new(), false, false), "AssignmentExpression expected", 1, 4);
}
#[test]
fn property_definition_test_nomatch_3() {
    check_err(PropertyDefinition::parse(&mut newparser("3"), Scanner::new(), false, false), "‘:’ expected", 1, 2);
}
#[test]
fn property_definition_test_nomatch_4() {
    check_err(PropertyDefinition::parse(&mut newparser("3:"), Scanner::new(), false, false), "AssignmentExpression expected", 1, 3);
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
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod property_definition {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        PropertyDefinition::parse(&mut newparser("b"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// PROPERTY DEFINITION LIST
#[test]
fn property_definition_list_test_01() {
    let (pdl, scanner) = check(PropertyDefinitionList::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pdl, PropertyDefinitionList::OneDef(_)));
    pretty_check(&*pdl, "PropertyDefinitionList: a", vec!["PropertyDefinition: a"]);
    concise_check(&*pdl, "IdentifierName: a", vec![]);
    format!("{:?}", *pdl);
}
#[test]
fn property_definition_list_test_02() {
    let (pdl, scanner) = check(PropertyDefinitionList::parse(&mut newparser("a,"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pdl, PropertyDefinitionList::OneDef(_)));
    pretty_check(&*pdl, "PropertyDefinitionList: a", vec!["PropertyDefinition: a"]);
    concise_check(&*pdl, "IdentifierName: a", vec![]);
}
#[test]
fn property_definition_list_test_03() {
    let (pdl, scanner) = check(PropertyDefinitionList::parse(&mut newparser("a,b"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*pdl, PropertyDefinitionList::ManyDefs(_, _)));
    pretty_check(&*pdl, "PropertyDefinitionList: a , b", vec!["PropertyDefinitionList: a", "PropertyDefinition: b"]);
    concise_check(&*pdl, "PropertyDefinitionList: a , b", vec!["IdentifierName: a", "Punctuator: ,", "IdentifierName: b"]);
}
#[test]
fn property_definition_list_test_04() {
    check_err(PropertyDefinitionList::parse(&mut newparser(""), Scanner::new(), false, false), "PropertyName expected", 1, 1);
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
#[test_case("a:b.#valid" => true; "Item valid")]
#[test_case("a:b.#valid,c" => true; "List head valid")]
#[test_case("a,b:c.#valid" => true; "List tail vaild")]
#[test_case("a:b.#invalid" => false; "Item invalid")]
#[test_case("a:b.#invalid,c" => false; "List head invalid")]
#[test_case("a,b:c.#invalid" => false; "List tail invalid")]
fn property_definition_list_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = PropertyDefinitionList::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod property_definition_list {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        PropertyDefinitionList::parse(&mut newparser("b"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// OBJECT LITERAL
#[test]
fn object_literal_test_01() {
    let (ol, scanner) = check(ObjectLiteral::parse(&mut newparser("{}"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(&*ol, ObjectLiteral::Empty));
    pretty_check(&*ol, "ObjectLiteral: { }", vec![]);
    concise_check(&*ol, "ObjectLiteral: { }", vec!["Punctuator: {", "Punctuator: }"]);
    format!("{:?}", *ol);
}
#[test]
fn object_literal_test_02() {
    let (ol, scanner) = check(ObjectLiteral::parse(&mut newparser("{a:b}"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*ol, ObjectLiteral::Normal(_)));
    pretty_check(&*ol, "ObjectLiteral: { a : b }", vec!["PropertyDefinitionList: a : b"]);
    concise_check(&*ol, "ObjectLiteral: { a : b }", vec!["Punctuator: {", "PropertyDefinition: a : b", "Punctuator: }"]);
}
#[test]
fn object_literal_test_03() {
    let (ol, scanner) = check(ObjectLiteral::parse(&mut newparser("{a:b,}"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(&*ol, ObjectLiteral::TrailingComma(_)));
    pretty_check(&*ol, "ObjectLiteral: { a : b , }", vec!["PropertyDefinitionList: a : b"]);
    concise_check(&*ol, "ObjectLiteral: { a : b , }", vec!["Punctuator: {", "PropertyDefinition: a : b", "Punctuator: ,", "Punctuator: }"]);
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
    check_err(ObjectLiteral::parse(&mut newparser("{a:b"), Scanner::new(), false, false), "One of [‘}’, ‘,’] expected", 1, 5);
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
#[test_case("{}" => true; "empty")]
#[test_case("{a:b.#valid}" => true; "List valid")]
#[test_case("{a:b.#valid,}" => true; "List comma valid")]
#[test_case("{a:b.#invalid}" => false; "List invalid")]
#[test_case("{a:b.#invalid,}" => false; "List comma invalid")]
fn object_literal_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ObjectLiteral::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod object_literal {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ObjectLiteral::parse(&mut newparser("{}"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// PARENTHESIZED EXPRESSION
#[test]
fn parenthesized_expression_test_01() {
    let (pe, scanner) = check(ParenthesizedExpression::parse(&mut newparser("(a)"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*pe, ParenthesizedExpression::Expression(_)));
    pretty_check(&*pe, "ParenthesizedExpression: ( a )", vec!["Expression: a"]);
    concise_check(&*pe, "ParenthesizedExpression: ( a )", vec!["Punctuator: (", "IdentifierName: a", "Punctuator: )"]);
    format!("{:?}", pe);
    assert_eq!(pe.is_function_definition(), false);
    assert_eq!(pe.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn parenthesized_expression_test_02() {
    check_err(ParenthesizedExpression::parse(&mut newparser(""), Scanner::new(), false, false), "‘(’ expected", 1, 1);
}
#[test]
fn parenthesized_expression_test_03() {
    check_err(ParenthesizedExpression::parse(&mut newparser("("), Scanner::new(), false, false), "Expression expected", 1, 2);
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
#[test_case("(a.#valid)" => true; "valid")]
#[test_case("(a.#invalid)" => false; "invalid")]
fn parenthesized_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ParenthesizedExpression::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod parenthesized_expression {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ParenthesizedExpression::parse(&mut newparser("(a)"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// TEMPLATE MIDDLE LIST
#[test]
fn template_middle_list_test_01() {
    let (tml, scanner) = check(TemplateMiddleList::parse(&mut newparser("}a${0"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*tml, TemplateMiddleList::ListHead(_, _, _)));
    pretty_check(&*tml, "TemplateMiddleList: }a${ 0", vec!["Expression: 0"]);
    concise_check(&*tml, "TemplateMiddleList: }a${ 0", vec!["TemplateMiddle: }a${", "Numeric: 0"]);
    format!("{:?}", tml);
}
#[test]
fn template_middle_list_test_02() {
    let (tml, scanner) = check(TemplateMiddleList::parse(&mut newparser("}${a}${b}"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 8);
    println!("{:?}", tml);
    assert!(matches!(&*tml, TemplateMiddleList::ListMid(_, _, _, _)));
    pretty_check(&*tml, "TemplateMiddleList: }${ a }${ b", vec!["TemplateMiddleList: }${ a", "Expression: b"]);
    concise_check(&*tml, "TemplateMiddleList: }${ a }${ b", vec!["TemplateMiddleList: }${ a", "TemplateMiddle: }${", "IdentifierName: b"]);
    format!("{:?}", tml);
}
#[test]
fn template_middle_list_test_03() {
    check_err(TemplateMiddleList::parse(&mut newparser(""), Scanner::new(), false, false, false), "TemplateMiddle expected", 1, 1);
    check_err(TemplateMiddleList::parse(&mut newparser("}abc${@"), Scanner::new(), false, false, false), "Expression expected", 1, 7);
}
#[test]
fn template_middle_list_test_04() {
    let (tml, scanner) = check(TemplateMiddleList::parse(&mut newparser("}${a}${@}"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*tml, TemplateMiddleList::ListHead(_, _, _)));
    pretty_check(&*tml, "TemplateMiddleList: }${ a", vec!["Expression: a"]);
    concise_check(&*tml, "TemplateMiddleList: }${ a", vec!["TemplateMiddle: }${", "IdentifierName: a"]);
    format!("{:?}", tml);
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
    let (item, _) = TemplateMiddleList::parse(&mut newparser("}${this}${a"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn template_middle_list_test_contains_04() {
    let (item, _) = TemplateMiddleList::parse(&mut newparser("}${a}${this"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn template_middle_list_test_contains_05() {
    let (item, _) = TemplateMiddleList::parse(&mut newparser("}${a}${a"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("}${a.#valid" => true; "Item valid")]
#[test_case("}${a.#valid}${b" => true; "List head valid")]
#[test_case("}${a}${b.#valid" => true; "List tail valid")]
#[test_case("}${a.#invalid" => false; "Item invalid")]
#[test_case("}${a.#invalid}${b" => false; "List head invalid")]
#[test_case("}${a}${b.#invalid" => false; "List tail invalid")]
fn template_middle_list_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = TemplateMiddleList::parse(&mut newparser(src), Scanner::new(), false, false, false).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod template_middle_list {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        TemplateMiddleList::parse(&mut newparser("}${a"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// TEMPLATE SPANS
#[test]
fn template_spans_test_01() {
    let (ts, scanner) = check(TemplateSpans::parse(&mut newparser("}done`"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(&*ts, TemplateSpans::Tail(_, _)));
    pretty_check(&*ts, "TemplateSpans: }done`", vec![]);
    concise_check(&*ts, "TemplateTail: }done`", vec![]);
    format!("{:?}", ts);
}
#[test]
fn template_spans_test_02() {
    let (ts, scanner) = check(TemplateSpans::parse(&mut newparser("}${a}done`"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 10);
    assert!(matches!(&*ts, TemplateSpans::List(_, _, _)));
    pretty_check(&*ts, "TemplateSpans: }${ a }done`", vec!["TemplateMiddleList: }${ a"]);
    concise_check(&*ts, "TemplateSpans: }${ a }done`", vec!["TemplateMiddleList: }${ a", "TemplateTail: }done`"]);
    format!("{:?}", ts);
}
#[test]
fn template_spans_test_03() {
    check_err(TemplateSpans::parse(&mut newparser(""), Scanner::new(), false, false, false), "TemplateSpans expected", 1, 1);
    check_err(TemplateSpans::parse(&mut newparser("}${blue"), Scanner::new(), false, false, false), "TemplateTail expected", 1, 8);
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
#[test_case("}`" => true; "TemplateTail")]
#[test_case("}${a.#valid}`" => true; "valid")]
#[test_case("}${a.#invalid}`" => false; "invalid")]
fn template_spans_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = TemplateSpans::parse(&mut newparser(src), Scanner::new(), false, false, false).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod template_spans {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        TemplateSpans::parse(&mut newparser("}`"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// SUBSTITUTION TEMPLATE
#[test]
fn substitution_template_test_01() {
    let (st, scanner) = check(SubstitutionTemplate::parse(&mut newparser("`${a}`"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 6);
    assert_eq!(st.tagged, false);
    pretty_check(&*st, "SubstitutionTemplate: `${ a }`", vec!["Expression: a", "TemplateSpans: }`"]);
    concise_check(&*st, "SubstitutionTemplate: `${ a }`", vec!["TemplateHead: `${", "IdentifierName: a", "TemplateTail: }`"]);
    format!("{:?}", st);
}
#[test]
fn substitution_template_test_02() {
    check_err(SubstitutionTemplate::parse(&mut newparser(""), Scanner::new(), false, false, false), "SubstitutionTemplate expected", 1, 1);
}
#[test]
fn substitution_template_test_03() {
    check_err(SubstitutionTemplate::parse(&mut newparser("`${"), Scanner::new(), false, false, false), "Expression expected", 1, 4);
}
#[test]
fn substitution_template_test_04() {
    check_err(SubstitutionTemplate::parse(&mut newparser("`${a"), Scanner::new(), false, false, false), "TemplateSpans expected", 1, 5);
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
    let (item, _) = SubstitutionTemplate::parse(&mut newparser("`${this}`"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn substitution_template_test_contains_02() {
    let (item, _) = SubstitutionTemplate::parse(&mut newparser("`${10}${this}`"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn substitution_template_test_contains_03() {
    let (item, _) = SubstitutionTemplate::parse(&mut newparser("`${10}`"), Scanner::new(), false, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("`${a.#valid}${b}`" => true; "expr valid")]
#[test_case("`${a}${b.#valid}`" => true; "spans valid")]
#[test_case("`${a.#invalid}${b}`" => false; "expr invalid")]
#[test_case("`${a}${b.#invalid}`" => false; "spans invalid")]
fn substitution_template_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = SubstitutionTemplate::parse(&mut newparser(src), Scanner::new(), false, false, false).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod substitution_template {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        SubstitutionTemplate::parse(&mut newparser("`${a}`"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// TEMPLATE LITERAL
#[test]
fn template_literal_test_01() {
    let (tl, scanner) = check(TemplateLiteral::parse(&mut newparser("`rust`"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(&*tl, TemplateLiteral::NoSubstitutionTemplate(_, _)));
    if let TemplateLiteral::NoSubstitutionTemplate(_, tagged) = &*tl {
        assert!(!*tagged);
    }
    pretty_check(&*tl, "TemplateLiteral: `rust`", vec![]);
    concise_check(&*tl, "NoSubTemplate: `rust`", vec![]);
    format!("{:?}", tl);
}
#[test]
fn template_literal_test_02() {
    let (tl, scanner) = check(TemplateLiteral::parse(&mut newparser("`${a}`"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(&*tl, TemplateLiteral::SubstitutionTemplate(_)));
    pretty_check(&*tl, "TemplateLiteral: `${ a }`", vec!["SubstitutionTemplate: `${ a }`"]);
    concise_check(&*tl, "SubstitutionTemplate: `${ a }`", vec!["TemplateHead: `${", "IdentifierName: a", "TemplateTail: }`"]);
    format!("{:?}", tl);
}
#[test]
fn template_literal_test_03() {
    check_err(TemplateLiteral::parse(&mut newparser(""), Scanner::new(), false, false, false), "TemplateLiteral expected", 1, 1);
    check_err(TemplateLiteral::parse(&mut newparser("`${"), Scanner::new(), false, false, false), "Expression expected", 1, 4);
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
#[test_case("`a`" => true; "no substitution")]
#[test_case("`${a.#valid}`" => true; "sub valid")]
#[test_case("`${a.#invalid}`" => false; "sub invalid")]
fn template_literal_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = TemplateLiteral::parse(&mut newparser(src), Scanner::new(), false, false, false).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod template_literal {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        TemplateLiteral::parse(&mut newparser("`${a}`"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}

// COVER PARENTHESIZED EXPRESSION AND ARROW PARAMETER LIST
#[test]
fn cpeaapl_test_01() {
    let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("()"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::Empty));
    pretty_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( )", vec![]);
    concise_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( )", vec!["Punctuator: (", "Punctuator: )"]);
    format!("{:?}", node);
}
#[test]
fn cpeaapl_test_02() {
    let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(8 in [1,2,3])"), Scanner::new(), false, false));
    chk_scan(&scanner, 14);
    assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::Expression(_)));
    pretty_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( 8 in [ 1 , 2 , 3 ] )", vec!["Expression: 8 in [ 1 , 2 , 3 ]"]);
    concise_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( 8 in [ 1 , 2 , 3 ] )", vec!["Punctuator: (", "RelationalExpression: 8 in [ 1 , 2 , 3 ]", "Punctuator: )"]);
    format!("{:?}", node);
}
#[test]
fn cpeaapl_test_03() {
    let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(8 in a,)"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::ExpComma(_)));
    pretty_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( 8 in a , )", vec!["Expression: 8 in a"]);
    concise_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( 8 in a , )", vec!["Punctuator: (", "RelationalExpression: 8 in a", "Punctuator: ,", "Punctuator: )"]);
    format!("{:?}", node);
}
#[test]
fn cpeaapl_test_04() {
    let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(...a)"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::Ident(_)));
    pretty_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( ... a )", vec!["BindingIdentifier: a"]);
    concise_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( ... a )", vec!["Punctuator: (", "Punctuator: ...", "IdentifierName: a", "Punctuator: )"]);
    format!("{:?}", node);
}
#[test]
fn cpeaapl_test_05() {
    let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(...{})"), Scanner::new(), false, false));
    chk_scan(&scanner, 7);
    assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::Pattern(_)));
    pretty_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( ... { } )", vec!["BindingPattern: { }"]);
    concise_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( ... { } )", vec!["Punctuator: (", "Punctuator: ...", "ObjectBindingPattern: { }", "Punctuator: )"]);
    format!("{:?}", node);
}
#[test]
fn cpeaapl_test_06() {
    let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(a,...b)"), Scanner::new(), false, false));
    chk_scan(&scanner, 8);
    assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::ExpIdent(..)));
    pretty_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( a , ... b )", vec!["Expression: a", "BindingIdentifier: b"]);
    concise_check(
        &*node,
        "CoverParenthesizedExpressionAndArrowParameterList: ( a , ... b )",
        vec!["Punctuator: (", "IdentifierName: a", "Punctuator: ,", "Punctuator: ...", "IdentifierName: b", "Punctuator: )"],
    );
    format!("{:?}", node);
}
#[test]
fn cpeaapl_test_07() {
    let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(a,...[])"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    assert!(matches!(&*node, CoverParenthesizedExpressionAndArrowParameterList::ExpPattern(..)));
    pretty_check(&*node, "CoverParenthesizedExpressionAndArrowParameterList: ( a , ... [ ] )", vec!["Expression: a", "BindingPattern: [ ]"]);
    concise_check(
        &*node,
        "CoverParenthesizedExpressionAndArrowParameterList: ( a , ... [ ] )",
        vec!["Punctuator: (", "IdentifierName: a", "Punctuator: ,", "Punctuator: ...", "ArrayBindingPattern: [ ]", "Punctuator: )"],
    );
    format!("{:?}", node);
}
#[test]
fn cpeaapl_test_08() {
    check_err(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser(""), Scanner::new(), false, false), "‘(’ expected", 1, 1);
}
#[test]
fn cpeaapl_test_09() {
    check_err(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("("), Scanner::new(), false, false), "Expression, spread pattern, or closing paren expected", 1, 2);
}
#[test]
fn cpeaapl_test_10() {
    check_err(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(..."), Scanner::new(), false, false), "BindingIdentifier or BindingPattern expected", 1, 5);
}
#[test]
fn cpeaapl_test_11() {
    check_err(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(...a"), Scanner::new(), false, false), "‘)’ expected", 1, 6);
}
#[test]
fn cpeaapl_test_12() {
    check_err(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(...[]"), Scanner::new(), false, false), "‘)’ expected", 1, 7);
}
#[test]
fn cpeaapl_test_13() {
    check_err(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(p"), Scanner::new(), false, false), "‘)’ expected", 1, 3);
}
#[test]
fn cpeaapl_test_14() {
    check_err(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(p,"), Scanner::new(), false, false), "‘)’ expected", 1, 4);
}
#[test]
fn cpeaapl_test_prettyerrors_1() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(0)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn cpeaapl_test_prettyerrors_2() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(0,)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn cpeaapl_test_prettyerrors_3() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("()"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn cpeaapl_test_prettyerrors_4() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(...a)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn cpeaapl_test_prettyerrors_5() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(...{})"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn cpeaapl_test_prettyerrors_6() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(0,...a)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn cpeaapl_test_prettyerrors_7() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(0,...{})"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn cpeaapl_test_conciseerrors_1() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(0)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn cpeaapl_test_conciseerrors_2() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(0,)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn cpeaapl_test_conciseerrors_3() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("()"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn cpeaapl_test_conciseerrors_4() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(...a)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn cpeaapl_test_conciseerrors_5() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(...{})"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn cpeaapl_test_conciseerrors_6() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(0,...a)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn cpeaapl_test_conciseerrors_7() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(0,...{})"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn cpeaapl_test_cache_01() {
    let mut parser = newparser("(a+b+c)");
    let (node, scanner) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut parser, Scanner::new(), false, false));
    let (node2, scanner2) = check(CoverParenthesizedExpressionAndArrowParameterList::parse(&mut parser, Scanner::new(), false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}

#[test]
fn cpeaapl_test_contains_01() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(this)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn cpeaapl_test_contains_02() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(a)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn cpeaapl_test_contains_03() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(this,)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn cpeaapl_test_contains_04() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(a,)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn cpeaapl_test_contains_05() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("()"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn cpeaapl_test_contains_06() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(...blue)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn cpeaapl_test_contains_07() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(...[a,b,c])"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn cpeaapl_test_contains_08() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(this, ...thing)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn cpeaapl_test_contains_09() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(a, ...thing)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn cpeaapl_test_contains_10() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(this, ...[a])"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn cpeaapl_test_contains_11() {
    let (item, _) = CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("(b, ...[a])"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
mod cover_parenthesized_expression_and_arrow_parameter_list {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        CoverParenthesizedExpressionAndArrowParameterList::parse(&mut newparser("()"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), true);
    }
}
