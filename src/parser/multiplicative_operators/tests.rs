#![expect(clippy::bool_assert_comparison)]
use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;

// MULTIPLICATIVE OPERATOR
#[test]
fn multiplicative_operator_test_01() {
    let (mo, scanner) = check(MultiplicativeOperator::parse(&mut newparser("*"), Scanner::new()));
    chk_scan(&scanner, 1);
    assert!(matches!(*mo, MultiplicativeOperator::Multiply));
    pretty_check(&*mo, "MultiplicativeOperator: *", &[]);
    concise_check(&*mo, "Punctuator: *", &[]);
    assert_ne!(format!("{mo:?}"), "");
}
#[test]
fn multiplicative_operator_test_02() {
    let (mo, scanner) = check(MultiplicativeOperator::parse(&mut newparser("/"), Scanner::new()));
    chk_scan(&scanner, 1);
    assert!(matches!(*mo, MultiplicativeOperator::Divide));
    pretty_check(&*mo, "MultiplicativeOperator: /", &[]);
    concise_check(&*mo, "Punctuator: /", &[]);
    assert_ne!(format!("{mo:?}"), "");
}
#[test]
fn multiplicative_operator_test_03() {
    let (mo, scanner) = check(MultiplicativeOperator::parse(&mut newparser("%"), Scanner::new()));
    chk_scan(&scanner, 1);
    assert!(matches!(*mo, MultiplicativeOperator::Modulo));
    pretty_check(&*mo, "MultiplicativeOperator: %", &[]);
    concise_check(&*mo, "Punctuator: %", &[]);
    assert_ne!(format!("{mo:?}"), "");
}
#[test]
fn multiplicative_operator_test_04() {
    check_err(
        MultiplicativeOperator::parse(&mut newparser("@"), Scanner::new()),
        "one of [‘*’, ‘/’, ‘%’] expected",
        1,
        1,
    );
}
#[test]
fn multiplicative_operator_test_prettycheck_1() {
    let (item, _) = MultiplicativeOperator::parse(&mut newparser("*"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn multiplicative_operator_test_prettycheck_2() {
    let (item, _) = MultiplicativeOperator::parse(&mut newparser("/"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn multiplicative_operator_test_prettycheck_3() {
    let (item, _) = MultiplicativeOperator::parse(&mut newparser("%"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn multiplicative_operator_test_concisecheck_1() {
    let (item, _) = MultiplicativeOperator::parse(&mut newparser("*"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn multiplicative_operator_test_concisecheck_2() {
    let (item, _) = MultiplicativeOperator::parse(&mut newparser("/"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn multiplicative_operator_test_concisecheck_3() {
    let (item, _) = MultiplicativeOperator::parse(&mut newparser("%"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn multiplicative_operator_test_contains_01() {
    let (item, _) = MultiplicativeOperator::parse(&mut newparser("*"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}

// MULTIPLICATIVE EXPRESSION
mod multiplicative_expression {
    use super::*;
    use test_case::test_case;

    #[test]
    fn parse_01() {
        let (me, scanner) = check(MultiplicativeExpression::parse(&mut newparser("a"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*me, MultiplicativeExpression::ExponentiationExpression(_)));
        pretty_check(&*me, "MultiplicativeExpression: a", &["ExponentiationExpression: a"]);
        concise_check(&*me, "IdentifierName: a", &[]);
        assert_ne!(format!("{me:?}"), "");
        assert_eq!(me.is_function_definition(), false);
    }
    #[test]
    fn parse_02() {
        let (me, scanner) = check(MultiplicativeExpression::parse(&mut newparser("a/b"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*me, MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(..)));
        pretty_check(
            &*me,
            "MultiplicativeExpression: a / b",
            &["MultiplicativeExpression: a", "MultiplicativeOperator: /", "ExponentiationExpression: b"],
        );
        concise_check(
            &*me,
            "MultiplicativeExpression: a / b",
            &["IdentifierName: a", "Punctuator: /", "IdentifierName: b"],
        );
        assert_ne!(format!("{me:?}"), "");
        assert_eq!(me.is_function_definition(), false);
    }
    #[test]
    fn parse_04() {
        let (me, scanner) =
            check(MultiplicativeExpression::parse(&mut newparser("a/b * @"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*me, MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(..)));
        pretty_check(
            &*me,
            "MultiplicativeExpression: a / b",
            &["MultiplicativeExpression: a", "MultiplicativeOperator: /", "ExponentiationExpression: b"],
        );
        concise_check(
            &*me,
            "MultiplicativeExpression: a / b",
            &["IdentifierName: a", "Punctuator: /", "IdentifierName: b"],
        );
        assert_ne!(format!("{me:?}"), "");
    }
    #[test]
    fn parse_03() {
        check_err(
            MultiplicativeExpression::parse(&mut newparser(""), Scanner::new(), false, false),
            "ExponentiationExpression expected",
            1,
            1,
        );
    }
    #[test]
    fn prettycheck_1() {
        let (item, _) = MultiplicativeExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettycheck_2() {
        let (item, _) = MultiplicativeExpression::parse(&mut newparser("a*1"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn concisecheck_1() {
        let (item, _) = MultiplicativeExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn concisecheck_2() {
        let (item, _) = MultiplicativeExpression::parse(&mut newparser("a*1"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) = MultiplicativeExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_02() {
        let (item, _) = MultiplicativeExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_03() {
        let (item, _) =
            MultiplicativeExpression::parse(&mut newparser("this * 0"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_04() {
        let (item, _) =
            MultiplicativeExpression::parse(&mut newparser("0 * this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_05() {
        let (item, _) = MultiplicativeExpression::parse(&mut newparser("0 * 0"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test_case("'string'" => Some(String::from("string")); "String Token")]
    #[test_case("a*b" => None; "Not token")]
    fn as_string_literal(src: &str) -> Option<String> {
        let (item, _) = MultiplicativeExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.as_string_literal().map(|st| String::from(st.value))
    }
    #[test_case("item.#valid" => true; "Fallthru valid")]
    #[test_case("item.#valid * b" => true; "Left valid")]
    #[test_case("a * item.#valid" => true; "Right valid")]
    #[test_case("item.#invalid" => false; "Fallthru invalid")]
    #[test_case("item.#invalid * b" => false; "Left invalid")]
    #[test_case("a * item.#invalid" => false; "Right invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = MultiplicativeExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "fall thru")]
    #[test_case("package*3", true => sset(&[PACKAGE_NOT_ALLOWED]); "left times right; left bad")]
    #[test_case("3*package", true => sset(&[PACKAGE_NOT_ALLOWED]); "left times right; right bad")]
    #[test_case("package/3", true => sset(&[PACKAGE_NOT_ALLOWED]); "left div right; left bad")]
    #[test_case("3/package", true => sset(&[PACKAGE_NOT_ALLOWED]); "left div right; right bad")]
    #[test_case("package%3", true => sset(&[PACKAGE_NOT_ALLOWED]); "left mod right; left bad")]
    #[test_case("3%package", true => sset(&[PACKAGE_NOT_ALLOWED]); "left mod right; right bad")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        MultiplicativeExpression::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a * 0" => true; "expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        MultiplicativeExpression::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .is_strictly_deletable()
    }

    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("arguments * bob" => true; "a * b (left)")]
    #[test_case("bob * arguments" => true; "a * b (right)")]
    #[test_case("xyzzy" => false; "Exp (no)")]
    #[test_case("xyzzy * bob" => false; "a * b (no)")]
    fn contains_arguments(src: &str) -> bool {
        MultiplicativeExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("eval", false => ATTKind::Simple; "simple eval")]
    #[test_case("eval", true => ATTKind::Invalid; "strict eval")]
    #[test_case("a*b", false => ATTKind::Invalid; "multiply")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).multiplicative_expression().assignment_target_type(strict)
    }

    #[test_case("a*b" => false; "expr")]
    #[test_case("function bob(){}" => true; "function fallthru")]
    #[test_case("1" => false; "literal fallthru")]
    fn is_named_function(src: &str) -> bool {
        Maker::new(src).multiplicative_expression().is_named_function()
    }

    #[test_case("  a*b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "expr")]
    #[test_case("  998" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "literal")]
    fn location(src: &str) -> Location {
        Maker::new(src).multiplicative_expression().location()
    }
}
