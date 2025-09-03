use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

// EXPRESSION
#[test]
fn expression_test_01() {
    let (se, scanner) = check(Expression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*se, Expression::FallThru(_)));
    pretty_check(&*se, "Expression: a", &["AssignmentExpression: a"]);
    concise_check(&*se, "IdentifierName: a", &[]);
    assert_ne!(format!("{se:?}"), "");
}
#[test]
fn expression_test_02() {
    let (se, scanner) = check(Expression::parse(&mut newparser("a,b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*se, Expression::Comma(..)));
    pretty_check(&*se, "Expression: a , b", &["Expression: a", "AssignmentExpression: b"]);
    concise_check(&*se, "Expression: a , b", &["IdentifierName: a", "Punctuator: ,", "IdentifierName: b"]);
    assert_ne!(format!("{se:?}"), "");
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

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "AssignmentExpression")]
    #[test_case("package,implements", true => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "Expression , AssignmentExpression")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Expression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a , b" => true; "expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        Expression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.is_strictly_deletable()
    }

    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("arguments, bob" => true; "Comma (left)")]
    #[test_case("bob, arguments" => true; "Comma (right)")]
    #[test_case("no" => false; "Exp (no)")]
    #[test_case("no, bob" => false; "Comma (no)")]
    fn contains_arguments(src: &str) -> bool {
        Expression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.contains_arguments()
    }

    #[test_case("a,b", false => ATTKind::Invalid; "comma")]
    #[test_case("eval", false => ATTKind::Simple; "eval; non-strict")]
    #[test_case("eval", true => ATTKind::Invalid; "eval; strict")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).expression().assignment_target_type(strict)
    }

    #[test_case("0", ParseNodeKind::Literal => true; "expr; literal; yes")]
    #[test_case("a", ParseNodeKind::Literal => false; "expr; literal; no")]
    #[test_case("a,0", ParseNodeKind::Literal => true; "commas; literal; right")]
    #[test_case("0,a", ParseNodeKind::Literal => true; "commas; literal; left")]
    #[test_case("a,a", ParseNodeKind::Literal => false; "commas; literal; none")]
    #[test_case("a", ParseNodeKind::AssignmentExpression => true; "expr; ae")]
    #[test_case("a,a", ParseNodeKind::Expression => true; "comma; expr")]
    #[test_case("a,a", ParseNodeKind::AssignmentExpression => true; "comma; ae")]
    fn contains(src: &str, target: ParseNodeKind) -> bool {
        Maker::new(src).expression().contains(target)
    }

    //#[test_case("a,b" => false; "comma")]
    //#[test_case("function bob(){}" => true; "function fallthru")]
    //#[test_case("1" => false; "literal fallthru")]
    //fn is_named_function(src: &str) -> bool {
    //    Maker::new(src).expression().is_named_function()
    //}

    #[test_case("  a,b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "comma")]
    #[test_case("  998" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "literal")]
    fn location(src: &str) -> Location {
        Maker::new(src).expression().location()
    }
}
