use super::testhelp::{check, chk_scan, newparser, set, strictparser, IMPLEMENTS_NOT_ALLOWED, PACKAGE_NOT_ALLOWED};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::{test_agent, unwind_syntax_error_object};
use ahash::AHashSet;
use test_case::test_case;

// EXPRESSION
#[test]
fn expression_test_01() {
    let (se, scanner) = check(Expression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*se, Expression::FallThru(_)));
    pretty_check(&*se, "Expression: a", vec!["AssignmentExpression: a"]);
    concise_check(&*se, "IdentifierName: a", vec![]);
    format!("{:?}", se);
    assert_eq!(se.is_function_definition(), false);
    assert_eq!(se.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn expression_test_02() {
    let (se, scanner) = check(Expression::parse(&mut newparser("a,b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*se, Expression::Comma(..)));
    pretty_check(&*se, "Expression: a , b", vec!["Expression: a", "AssignmentExpression: b"]);
    concise_check(&*se, "Expression: a , b", vec!["IdentifierName: a", "Punctuator: ,", "IdentifierName: b"]);
    format!("{:?}", se);
    assert_eq!(se.is_function_definition(), false);
    assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn expression_test_cache_01() {
    let mut parser = newparser("blue(67)+90");
    let (node, scanner) = check(Expression::parse(&mut parser, Scanner::new(), true, false, false));
    let (node2, scanner2) = check(Expression::parse(&mut parser, Scanner::new(), true, false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn expression_test_03() {
    let (_, scanner) = check(Expression::parse(&mut newparser("a,"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
}
#[test]
fn expression_test_prettyerrors_1() {
    let (item, _) = Expression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn expression_test_prettyerrors_2() {
    let (item, _) = Expression::parse(&mut newparser("a,b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn expression_test_conciseerrors_1() {
    let (item, _) = Expression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn expression_test_conciseerrors_2() {
    let (item, _) = Expression::parse(&mut newparser("a,b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn expression_test_contains_01() {
    let (item, _) = Expression::parse(&mut newparser("0"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn expression_test_contains_02() {
    let (item, _) = Expression::parse(&mut newparser("a"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn expression_test_contains_03() {
    let (item, _) = Expression::parse(&mut newparser("0,a"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn expression_test_contains_04() {
    let (item, _) = Expression::parse(&mut newparser("a,0"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn expression_test_contains_05() {
    let (item, _) = Expression::parse(&mut newparser("a,a"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("'string'" => Some(JSString::from("string")); "String Token")]
#[test_case("a,b" => None; "Not token")]
fn expression_test_as_string_literal(src: &str) -> Option<JSString> {
    let (item, _) = Expression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.as_string_literal().map(|st| st.value)
}
#[test_case("item.#valid" => true; "Fallthru valid")]
#[test_case("item.#valid,a" => true; "Left valid")]
#[test_case("a, item.#valid" => true; "Right valid")]
#[test_case("item.#invalid" => false; "Fallthru invalid")]
#[test_case("item.#invalid,a" => false; "Left invalid")]
#[test_case("a, item.#invalid" => false; "Right invalid")]
fn expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = Expression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod expression {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "AssignmentExpression")]
    #[test_case("package,implements", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "Expression , AssignmentExpression")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        Expression::parse(&mut strictparser(src, strict), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a , b" => true; "expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        Expression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.is_strictly_deletable()
    }
}
