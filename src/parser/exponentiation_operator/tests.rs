use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;

// EXPONENTIATION EXPRESSION
mod exponentiation_expression {
    use super::*;
    use test_case::test_case;

    #[test]
    fn parse_01() {
        let (se, scanner) = check(ExponentiationExpression::parse(&mut newparser("a"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, ExponentiationExpression::UnaryExpression(_)));
        pretty_check(&*se, "ExponentiationExpression: a", &["UnaryExpression: a"]);
        concise_check(&*se, "IdentifierName: a", &[]);
        format!("{se:?}");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn parse_02() {
        let (se, scanner) =
            check(ExponentiationExpression::parse(&mut newparser("a ** b"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(&*se, ExponentiationExpression::Exponentiation(..)));
        pretty_check(&*se, "ExponentiationExpression: a ** b", &["UpdateExpression: a", "ExponentiationExpression: b"]);
        concise_check(
            &*se,
            "ExponentiationExpression: a ** b",
            &["IdentifierName: a", "Punctuator: **", "IdentifierName: b"],
        );
        format!("{se:?}");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn parse_03() {
        check_err(
            ExponentiationExpression::parse(&mut newparser(""), Scanner::new(), false, false),
            "ExponentiationExpression expected",
            1,
            1,
        );
    }
    #[test]
    fn parse_04() {
        let (se, scanner) =
            check(ExponentiationExpression::parse(&mut newparser("a ** @"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, ExponentiationExpression::UnaryExpression(_)));
        pretty_check(&*se, "ExponentiationExpression: a", &["UnaryExpression: a"]);
        concise_check(&*se, "IdentifierName: a", &[]);
        format!("{se:?}");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn prettyerrors_1() {
        let (item, _) = ExponentiationExpression::parse(&mut newparser("3**4"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_2() {
        let (item, _) = ExponentiationExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_1() {
        let (item, _) = ExponentiationExpression::parse(&mut newparser("3**4"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_2() {
        let (item, _) = ExponentiationExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) = ExponentiationExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_02() {
        let (item, _) = ExponentiationExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_03() {
        let (item, _) =
            ExponentiationExpression::parse(&mut newparser("this ** 1"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_04() {
        let (item, _) =
            ExponentiationExpression::parse(&mut newparser("1 ** this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_05() {
        let (item, _) =
            ExponentiationExpression::parse(&mut newparser("1 ** 1"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test_case("'string'" => Some(String::from("string")); "String Token")]
    #[test_case("a**b" => None; "Not token")]
    fn as_string_literal(src: &str) -> Option<String> {
        let (item, _) = ExponentiationExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.as_string_literal().map(|st| String::from(st.value))
    }
    #[test_case("a.#valid" => true; "Fallthru valid")]
    #[test_case("a.#valid ** b" => true; "Left valid")]
    #[test_case("a ** b.#valid" => true; "Right valid")]
    #[test_case("a.#invalid" => false; "Fallthru invalid")]
    #[test_case("a.#invalid ** b" => false; "Left invalid")]
    #[test_case("a ** b.#invalid" => false; "Right invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = ExponentiationExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "fall thru")]
    #[test_case("package**3", true => sset(&[PACKAGE_NOT_ALLOWED]); "left ** right; left bad")]
    #[test_case("3**package", true => sset(&[PACKAGE_NOT_ALLOWED]); "left ** right; right bad")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        ExponentiationExpression::parse(&mut newparser(src), Scanner::new(), false, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a ** 0" => true; "expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        ExponentiationExpression::parse(&mut newparser(src), Scanner::new(), true, true)
            .unwrap()
            .0
            .is_strictly_deletable()
    }

    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("arguments ** bob" => true; "a ** b (left)")]
    #[test_case("bob ** arguments" => true; "a ** b (right)")]
    #[test_case("xyzzy" => false; "Exp (no)")]
    #[test_case("xyzzy ** bob" => false; "a ** b (no)")]
    fn contains_arguments(src: &str) -> bool {
        ExponentiationExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }

    #[test_case("eval", false => ATTKind::Simple; "simple eval")]
    #[test_case("eval", true => ATTKind::Invalid; "strict eval")]
    #[test_case("a**b", false => ATTKind::Invalid; "expression")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).exponentiation_expression().assignment_target_type(strict)
    }

    #[test_case("a**b" => false; "expr")]
    #[test_case("function bob(){}" => true; "function fallthru")]
    #[test_case("1" => false; "literal fallthru")]
    fn is_named_function(src: &str) -> bool {
        Maker::new(src).exponentiation_expression().is_named_function()
    }

    #[test_case("  a**b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 4 }}; "power")]
    #[test_case("  998" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "literal")]
    fn location(src: &str) -> Location {
        Maker::new(src).exponentiation_expression().location()
    }
}
