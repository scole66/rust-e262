#![expect(clippy::bool_assert_comparison)]
use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

// CONDITIONAL EXPRESSION
#[test]
fn conditional_expression_test_01() {
    let (se, scanner) = check(ConditionalExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*se, ConditionalExpression::FallThru(_)));
    pretty_check(&*se, "ConditionalExpression: a", &["ShortCircuitExpression: a"]);
    concise_check(&*se, "IdentifierName: a", &[]);
    assert_ne!(format!("{se:?}"), "");
    assert_eq!(se.is_function_definition(), false);
}
#[test]
fn conditional_expression_test_02() {
    let (se, scanner) =
        check(ConditionalExpression::parse(&mut newparser("a?b:c"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*se, ConditionalExpression::Conditional(..)));
    pretty_check(
        &*se,
        "ConditionalExpression: a ? b : c",
        &["ShortCircuitExpression: a", "AssignmentExpression: b", "AssignmentExpression: c"],
    );
    concise_check(
        &*se,
        "ConditionalExpression: a ? b : c",
        &["IdentifierName: a", "Punctuator: ?", "IdentifierName: b", "Punctuator: :", "IdentifierName: c"],
    );
    assert_ne!(format!("{se:?}"), "");
    assert_eq!(se.is_function_definition(), false);
}
#[test]
fn conditional_expression_test_03() {
    let (_, scanner) = check(ConditionalExpression::parse(&mut newparser("a?"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
}
#[test]
fn conditional_expression_test_04() {
    let (_, scanner) = check(ConditionalExpression::parse(&mut newparser("a?b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
}
#[test]
fn conditional_expression_test_05() {
    let (_, scanner) = check(ConditionalExpression::parse(&mut newparser("a?b:"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
}
#[test]
fn conditional_expression_test_prettyerrors_1() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn conditional_expression_test_prettyerrors_2() {
    let (item, _) =
        ConditionalExpression::parse(&mut newparser("true?a:b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn conditional_expression_test_conciseerrors_1() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn conditional_expression_test_conciseerrors_2() {
    let (item, _) =
        ConditionalExpression::parse(&mut newparser("true?a:b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn conditional_expression_test_contains_01() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn conditional_expression_test_contains_02() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn conditional_expression_test_contains_03() {
    let (item, _) =
        ConditionalExpression::parse(&mut newparser("this?1:0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn conditional_expression_test_contains_04() {
    let (item, _) =
        ConditionalExpression::parse(&mut newparser("0?this:0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn conditional_expression_test_contains_05() {
    let (item, _) =
        ConditionalExpression::parse(&mut newparser("0?0:this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn conditional_expression_test_contains_06() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("0?0:0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("'string'" => Some(JSString::from("string")); "String Token")]
#[test_case("a?b:C" => None; "Not token")]
fn conditional_expression_test_as_string_literal(src: &str) -> Option<JSString> {
    let (item, _) = ConditionalExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.as_string_literal().map(|st| st.value)
}
#[test_case("item.#valid" => true; "Fallthru valid")]
#[test_case("item.#valid?a:b" => true; "Condition valid")]
#[test_case("a?item.#valid:b" => true; "Truthy valid")]
#[test_case("a?b:item.#valid" => true; "Falsey valid")]
#[test_case("item.#invalid" => false; "Fallthru invalid")]
#[test_case("item.#invalid?a:b" => false; "Condition invalid")]
#[test_case("a?item.#invalid:b" => false; "Truthy invalid")]
#[test_case("a?b:item.#invalid" => false; "Falsey invalid")]
fn conditional_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ConditionalExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod conditional_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "fall thru")]
    #[test_case("package?interface:implements", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "conditional")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        ConditionalExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a ? 0 : b" => true; "expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        ConditionalExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .is_strictly_deletable()
    }

    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("arguments ? bob : alice" => true; "trinary (left)")]
    #[test_case("bob ? arguments : alice" => true; "trinary (middle)")]
    #[test_case("bob ? alice : arguments" => true; "trinary (right)")]
    #[test_case("xyzzy" => false; "Exp (no)")]
    #[test_case("xyzzy ? bob : alice" => false; "trinary (no)")]
    fn contains_arguments(src: &str) -> bool {
        ConditionalExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .contains_arguments()
    }

    #[test_case("eval", false => ATTKind::Simple; "simple eval")]
    #[test_case("eval", true => ATTKind::Invalid; "strict eval")]
    #[test_case("a?b:c", false => ATTKind::Invalid; "conditional")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).conditional_expression().assignment_target_type(strict)
    }

    #[test_case("a?b:c" => false; "conditional")]
    #[test_case("function bob(){}" => true; "function fallthru")]
    #[test_case("1" => false; "literal fallthru")]
    fn is_named_function(src: &str) -> bool {
        Maker::new(src).conditional_expression().is_named_function()
    }

    #[test_case("  a?b:c" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 5 }}; "conditional")]
    #[test_case("  998" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "literal")]
    fn location(src: &str) -> Location {
        Maker::new(src).conditional_expression().location()
    }
}
