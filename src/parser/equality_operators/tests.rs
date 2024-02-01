use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;

// EQUALITY EXPRESSION
mod equality_expression {
    use super::*;
    use test_case::test_case;

    #[test]
    fn parse_01() {
        let (se, scanner) = check(EqualityExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, EqualityExpression::RelationalExpression(_)));
        pretty_check(&*se, "EqualityExpression: a", vec!["RelationalExpression: a"]);
        concise_check(&*se, "IdentifierName: a", vec![]);
        format!("{se:?}");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn parse_02() {
        let (se, scanner) =
            check(EqualityExpression::parse(&mut newparser("a==b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*se, EqualityExpression::Equal(_, _)));
        pretty_check(&*se, "EqualityExpression: a == b", vec!["EqualityExpression: a", "RelationalExpression: b"]);
        concise_check(
            &*se,
            "EqualityExpression: a == b",
            vec!["IdentifierName: a", "Punctuator: ==", "IdentifierName: b"],
        );
        format!("{se:?}");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn parse_03() {
        let (se, scanner) =
            check(EqualityExpression::parse(&mut newparser("a!=b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*se, EqualityExpression::NotEqual(_, _)));
        pretty_check(&*se, "EqualityExpression: a != b", vec!["EqualityExpression: a", "RelationalExpression: b"]);
        concise_check(
            &*se,
            "EqualityExpression: a != b",
            vec!["IdentifierName: a", "Punctuator: !=", "IdentifierName: b"],
        );
        format!("{se:?}");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn parse_04() {
        let (se, scanner) =
            check(EqualityExpression::parse(&mut newparser("a===b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*se, EqualityExpression::StrictEqual(_, _)));
        pretty_check(&*se, "EqualityExpression: a === b", vec!["EqualityExpression: a", "RelationalExpression: b"]);
        concise_check(
            &*se,
            "EqualityExpression: a === b",
            vec!["IdentifierName: a", "Punctuator: ===", "IdentifierName: b"],
        );
        format!("{se:?}");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn parse_05() {
        let (se, scanner) =
            check(EqualityExpression::parse(&mut newparser("a!==b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*se, EqualityExpression::NotStrictEqual(_, _)));
        pretty_check(&*se, "EqualityExpression: a !== b", vec!["EqualityExpression: a", "RelationalExpression: b"]);
        concise_check(
            &*se,
            "EqualityExpression: a !== b",
            vec!["IdentifierName: a", "Punctuator: !==", "IdentifierName: b"],
        );
        format!("{se:?}");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn parse_06() {
        check_err(
            EqualityExpression::parse(&mut newparser(""), Scanner::new(), true, false, false),
            "RelationalExpression expected",
            1,
            1,
        );
    }
    #[test]
    fn parse_08() {
        let (se, scanner) =
            check(EqualityExpression::parse(&mut newparser("a != @"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, EqualityExpression::RelationalExpression(_)));
        pretty_check(&*se, "EqualityExpression: a", vec!["RelationalExpression: a"]);
        concise_check(&*se, "IdentifierName: a", vec![]);
        format!("{se:?}");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn prettyerrors_1() {
        let (item, _) = EqualityExpression::parse(&mut newparser("3!=4"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_1() {
        let (item, _) = EqualityExpression::parse(&mut newparser("3!=4"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_2() {
        let (item, _) = EqualityExpression::parse(&mut newparser("3==4"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_2() {
        let (item, _) = EqualityExpression::parse(&mut newparser("3==4"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_3() {
        let (item, _) = EqualityExpression::parse(&mut newparser("3!==4"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_3() {
        let (item, _) = EqualityExpression::parse(&mut newparser("3!==4"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_4() {
        let (item, _) = EqualityExpression::parse(&mut newparser("3===4"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_4() {
        let (item, _) = EqualityExpression::parse(&mut newparser("3===4"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) = EqualityExpression::parse(&mut newparser("this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_02() {
        let (item, _) = EqualityExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_03() {
        let (item, _) =
            EqualityExpression::parse(&mut newparser("this == 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_04() {
        let (item, _) =
            EqualityExpression::parse(&mut newparser("0 == this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_05() {
        let (item, _) =
            EqualityExpression::parse(&mut newparser("0 == 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_06() {
        let (item, _) =
            EqualityExpression::parse(&mut newparser("this != 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_07() {
        let (item, _) =
            EqualityExpression::parse(&mut newparser("0 != this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_08() {
        let (item, _) =
            EqualityExpression::parse(&mut newparser("0 != 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_09() {
        let (item, _) =
            EqualityExpression::parse(&mut newparser("this === 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_10() {
        let (item, _) =
            EqualityExpression::parse(&mut newparser("0 === this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_11() {
        let (item, _) =
            EqualityExpression::parse(&mut newparser("0 === 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_12() {
        let (item, _) =
            EqualityExpression::parse(&mut newparser("this !== 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_13() {
        let (item, _) =
            EqualityExpression::parse(&mut newparser("0 !== this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_14() {
        let (item, _) =
            EqualityExpression::parse(&mut newparser("0 !== 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test_case("'string'" => Some(JSString::from("string")); "String Token")]
    #[test_case("a==b" => None; "Not token")]
    fn as_string_literal(src: &str) -> Option<JSString> {
        let (item, _) = EqualityExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
        item.as_string_literal().map(|st| st.value)
    }
    #[test_case("item.#valid" => true; "fallthru valid")]
    #[test_case("item.#valid==a" => true; "Eq Left valid")]
    #[test_case("a==item.#valid" => true; "Eq Right valid")]
    #[test_case("item.#valid!=a" => true; "Ne Left valid")]
    #[test_case("a!=item.#valid" => true; "Ne Right valid")]
    #[test_case("item.#valid===a" => true; "StrictEq Left valid")]
    #[test_case("a===item.#valid" => true; "StrictEq Right valid")]
    #[test_case("item.#valid!==a" => true; "StrictNe Left valid")]
    #[test_case("a!==item.#valid" => true; "StrictNe Right valid")]
    #[test_case("item.#invalid" => false; "fallthru invalid")]
    #[test_case("item.#invalid==a" => false; "Eq Left invalid")]
    #[test_case("a==item.#invalid" => false; "Eq Right invalid")]
    #[test_case("item.#invalid!=a" => false; "Ne Left invalid")]
    #[test_case("a!=item.#invalid" => false; "Ne Right invalid")]
    #[test_case("item.#invalid===a" => false; "StrictEq Left invalid")]
    #[test_case("a===item.#invalid" => false; "StrictEq Right invalid")]
    #[test_case("item.#invalid!==a" => false; "StrictNe Left invalid")]
    #[test_case("a!==item.#invalid" => false; "StrictNe Right invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = EqualityExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "fall thru")]
    #[test_case("package==3", true => sset(&[PACKAGE_NOT_ALLOWED]); "left eq right; left bad")]
    #[test_case("3==package", true => sset(&[PACKAGE_NOT_ALLOWED]); "left eq right; right bad")]
    #[test_case("package!=3", true => sset(&[PACKAGE_NOT_ALLOWED]); "left ne right; left bad")]
    #[test_case("3!=package", true => sset(&[PACKAGE_NOT_ALLOWED]); "left ne right; right bad")]
    #[test_case("package===3", true => sset(&[PACKAGE_NOT_ALLOWED]); "left se right; left bad")]
    #[test_case("3===package", true => sset(&[PACKAGE_NOT_ALLOWED]); "left se right; right bad")]
    #[test_case("package!==3", true => sset(&[PACKAGE_NOT_ALLOWED]); "left nse right; left bad")]
    #[test_case("3!==package", true => sset(&[PACKAGE_NOT_ALLOWED]); "left nse right; right bad")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        EqualityExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a == 0" => true; "expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        EqualityExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .is_strictly_deletable()
    }

    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("arguments == bob" => true; "a eq b (left)")]
    #[test_case("bob == arguments" => true; "a eq b (right)")]
    #[test_case("arguments != bob" => true; "a ne b (left)")]
    #[test_case("bob != arguments" => true; "a ne b (right)")]
    #[test_case("arguments === bob" => true; "a seq b (left)")]
    #[test_case("bob === arguments" => true; "a seq b (right)")]
    #[test_case("arguments !== bob" => true; "a sne b (left)")]
    #[test_case("bob !== arguments" => true; "a sne b (right)")]
    #[test_case("xyzzy" => false; "Exp (no)")]
    #[test_case("xyzzy == bob" => false; "a eq b (no)")]
    #[test_case("xyzzy != bob" => false; "a ne b (no)")]
    #[test_case("xyzzy === bob" => false; "a seq b (no)")]
    #[test_case("xyzzy !== bob" => false; "a sne b (no)")]
    fn contains_arguments(src: &str) -> bool {
        EqualityExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.contains_arguments()
    }

    #[test_case("eval", false => ATTKind::Simple; "simple eval")]
    #[test_case("eval", true => ATTKind::Invalid; "strict eval")]
    #[test_case("a==b", false => ATTKind::Invalid; "eq")]
    #[test_case("a===b", false => ATTKind::Invalid; "seq")]
    #[test_case("a!=b", false => ATTKind::Invalid; "ne")]
    #[test_case("a!==b", false => ATTKind::Invalid; "sne")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).equality_expression().assignment_target_type(strict)
    }

    #[test_case("a==b" => false; "expr")]
    #[test_case("function bob(){}" => true; "function fallthru")]
    #[test_case("1" => false; "literal fallthru")]
    fn is_named_function(src: &str) -> bool {
        Maker::new(src).equality_expression().is_named_function()
    }

    #[test_case("  a==b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 4 }}; "eq")]
    #[test_case("  a===b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 5 }}; "seq")]
    #[test_case("  a!=b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 4 }}; "ne")]
    #[test_case("  a!==b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 5 }}; "sne")]
    #[test_case("  998" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "literal")]
    fn location(src: &str) -> Location {
        Maker::new(src).equality_expression().location()
    }
}
