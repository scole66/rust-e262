use super::testhelp::{check, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::{test_agent, unwind_syntax_error_object};
use ahash::AHashSet;
use test_case::test_case;

// CONDITIONAL EXPRESSION
#[test]
fn conditional_expression_test_01() {
    let (se, scanner) = check(ConditionalExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*se, ConditionalExpression::FallThru(_)));
    pretty_check(&*se, "ConditionalExpression: a", vec!["ShortCircuitExpression: a"]);
    concise_check(&*se, "IdentifierName: a", vec![]);
    format!("{:?}", se);
    assert_eq!(se.is_function_definition(), false);
    assert_eq!(se.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn conditional_expression_test_02() {
    let (se, scanner) = check(ConditionalExpression::parse(&mut newparser("a?b:c"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*se, ConditionalExpression::Conditional(..)));
    pretty_check(&*se, "ConditionalExpression: a ? b : c", vec!["ShortCircuitExpression: a", "AssignmentExpression: b", "AssignmentExpression: c"]);
    concise_check(&*se, "ConditionalExpression: a ? b : c", vec!["IdentifierName: a", "Punctuator: ?", "IdentifierName: b", "Punctuator: :", "IdentifierName: c"]);
    format!("{:?}", se);
    assert_eq!(se.is_function_definition(), false);
    assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
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
    let (item, _) = ConditionalExpression::parse(&mut newparser("true?a:b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn conditional_expression_test_conciseerrors_1() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn conditional_expression_test_conciseerrors_2() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("true?a:b"), Scanner::new(), true, false, false).unwrap();
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
    let (item, _) = ConditionalExpression::parse(&mut newparser("this?1:0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn conditional_expression_test_contains_04() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("0?this:0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn conditional_expression_test_contains_05() {
    let (item, _) = ConditionalExpression::parse(&mut newparser("0?0:this"), Scanner::new(), true, false, false).unwrap();
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
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod conditional_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => AHashSet::from_iter(["‘package’ not allowed as an identifier in strict mode".to_string()]); "fall thru")]
    #[test_case("package?interface:implements", true => panics "not yet implemented"; "conditional")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        ConditionalExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a ? 0 : b" => true; "expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        ConditionalExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.is_strictly_deletable()
    }
}
