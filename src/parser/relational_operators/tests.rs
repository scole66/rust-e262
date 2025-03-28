#![expect(clippy::bool_assert_comparison)]
use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;

// RELATIONAL EXPRESSION
mod relational_expression {
    use super::*;
    use test_case::test_case;

    #[test]
    fn parse_01() {
        let (se, scanner) = check(RelationalExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, RelationalExpression::ShiftExpression(_)));
        pretty_check(&*se, "RelationalExpression: a", &["ShiftExpression: a"]);
        concise_check(&*se, "IdentifierName: a", &[]);
        assert_ne!(format!("{se:?}"), "");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn parse_02() {
        let (se, scanner) =
            check(RelationalExpression::parse(&mut newparser("a < b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*se, RelationalExpression::Less(_, _)));
        pretty_check(&*se, "RelationalExpression: a < b", &["RelationalExpression: a", "ShiftExpression: b"]);
        concise_check(
            &*se,
            "RelationalExpression: a < b",
            &["IdentifierName: a", "Punctuator: <", "IdentifierName: b"],
        );
        assert_ne!(format!("{se:?}"), "");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn parse_03() {
        let (se, scanner) =
            check(RelationalExpression::parse(&mut newparser("a > b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(&*se, RelationalExpression::Greater(_, _)));
        pretty_check(&*se, "RelationalExpression: a > b", &["RelationalExpression: a", "ShiftExpression: b"]);
        concise_check(
            &*se,
            "RelationalExpression: a > b",
            &["IdentifierName: a", "Punctuator: >", "IdentifierName: b"],
        );
        assert_ne!(format!("{se:?}"), "");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn parse_04() {
        let (se, scanner) =
            check(RelationalExpression::parse(&mut newparser("a <= b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(&*se, RelationalExpression::LessEqual(_, _)));
        pretty_check(&*se, "RelationalExpression: a <= b", &["RelationalExpression: a", "ShiftExpression: b"]);
        concise_check(
            &*se,
            "RelationalExpression: a <= b",
            &["IdentifierName: a", "Punctuator: <=", "IdentifierName: b"],
        );
        assert_ne!(format!("{se:?}"), "");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn parse_05() {
        let (se, scanner) =
            check(RelationalExpression::parse(&mut newparser("a >= b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(&*se, RelationalExpression::GreaterEqual(_, _)));
        pretty_check(&*se, "RelationalExpression: a >= b", &["RelationalExpression: a", "ShiftExpression: b"]);
        concise_check(
            &*se,
            "RelationalExpression: a >= b",
            &["IdentifierName: a", "Punctuator: >=", "IdentifierName: b"],
        );
        assert_ne!(format!("{se:?}"), "");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn parse_06() {
        let (se, scanner) =
            check(RelationalExpression::parse(&mut newparser("a instanceof b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 14);
        assert!(matches!(&*se, RelationalExpression::InstanceOf(_, _)));
        pretty_check(&*se, "RelationalExpression: a instanceof b", &["RelationalExpression: a", "ShiftExpression: b"]);
        concise_check(
            &*se,
            "RelationalExpression: a instanceof b",
            &["IdentifierName: a", "Keyword: instanceof", "IdentifierName: b"],
        );
        assert_ne!(format!("{se:?}"), "");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn parse_07() {
        let (se, scanner) =
            check(RelationalExpression::parse(&mut newparser("a in b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(&*se, RelationalExpression::In(_, _)));
        pretty_check(&*se, "RelationalExpression: a in b", &["RelationalExpression: a", "ShiftExpression: b"]);
        concise_check(&*se, "RelationalExpression: a in b", &["IdentifierName: a", "Keyword: in", "IdentifierName: b"]);
        assert_ne!(format!("{se:?}"), "");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn parse_08() {
        let (se, scanner) =
            check(RelationalExpression::parse(&mut newparser("a in b"), Scanner::new(), false, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, RelationalExpression::ShiftExpression(_)));
        pretty_check(&*se, "RelationalExpression: a", &["ShiftExpression: a"]);
        concise_check(&*se, "IdentifierName: a", &[]);
        assert_ne!(format!("{se:?}"), "");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn parse_09() {
        let (se, scanner) =
            check(RelationalExpression::parse(&mut newparser("a >= @"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*se, RelationalExpression::ShiftExpression(_)));
        pretty_check(&*se, "RelationalExpression: a", &["ShiftExpression: a"]);
        concise_check(&*se, "IdentifierName: a", &[]);
        assert_ne!(format!("{se:?}"), "");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn parse_10() {
        check_err(
            RelationalExpression::parse(&mut newparser(""), Scanner::new(), true, false, false),
            "RelationalExpression expected",
            1,
            1,
        );
    }
    #[test]
    fn parse_11() {
        let (se, scanner) =
            check(RelationalExpression::parse(&mut newparser("#a in b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 7);
        assert!(matches!(&*se, RelationalExpression::PrivateIn(..)));
        pretty_check(&*se, "RelationalExpression: #a in b", &["ShiftExpression: b"]);
        concise_check(
            &*se,
            "RelationalExpression: #a in b",
            &["PrivateIdentifier: #a", "Keyword: in", "IdentifierName: b"],
        );
        assert_ne!(format!("{se:?}"), "");
        assert_eq!(se.is_function_definition(), false);
    }
    #[test]
    fn parse_12() {
        check_err(
            RelationalExpression::parse(&mut newparser("#a in b"), Scanner::new(), false, false, false),
            "RelationalExpression expected",
            1,
            1,
        );
    }
    #[test]
    fn parse_13() {
        check_err(
            RelationalExpression::parse(&mut newparser("#a"), Scanner::new(), true, false, false),
            "‘in’ expected",
            1,
            3,
        );
    }
    #[test]
    fn parse_14() {
        check_err(
            RelationalExpression::parse(&mut newparser("#a in"), Scanner::new(), true, false, false),
            "ExponentiationExpression expected",
            1,
            6,
        );
    }
    #[test]
    fn prettycheck_1() {
        let (item, _) = RelationalExpression::parse(&mut newparser("3>4"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettycheck_2() {
        let (item, _) = RelationalExpression::parse(&mut newparser("3<4"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettycheck_3() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("3>=4"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettycheck_4() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("3<=4"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettycheck_5() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("3 instanceof 4"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettycheck_6() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("3 in 4"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettycheck_7() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("#b in 4"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn concisecheck_1() {
        let (item, _) = RelationalExpression::parse(&mut newparser("3>4"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn concisecheck_2() {
        let (item, _) = RelationalExpression::parse(&mut newparser("3<4"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn concisecheck_3() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("3>=4"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn concisecheck_4() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("3<=4"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn concisecheck_5() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("3 instanceof 4"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn concisecheck_6() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("3 in 4"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn concisecheck_7() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("#b in 4"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_02() {
        let (item, _) = RelationalExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_03() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("this < 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_04() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("0 < this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_05() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("0 < 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_06() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("this > 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_07() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("0 > this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_08() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("0 > 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_09() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("this <= 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_10() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("0 <= this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_11() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("0 <= 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_12() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("this >= 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_13() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("0 >= this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_14() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("0 >= 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_15() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("this instanceof 0"), Scanner::new(), true, false, false)
                .unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_16() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("0 instanceof this"), Scanner::new(), true, false, false)
                .unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_17() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("0 instanceof 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_18() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("this in 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_19() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("0 in this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_20() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("0 in 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_21() {
        let (item, _) =
            RelationalExpression::parse(&mut newparser("#a in this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test_case("'string'" => Some(JSString::from("string")); "String Token")]
    #[test_case("a>b" => None; "Not token")]
    fn as_string_literal(src: &str) -> Option<JSString> {
        let (item, _) = RelationalExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
        item.as_string_literal().map(|st| st.value)
    }
    #[test_case("a.#valid" => true; "fallthru valid")]
    #[test_case("a.#valid<b" => true; "lt left valid")]
    #[test_case("a<b.#valid" => true; "lt right valid")]
    #[test_case("a.#valid>b" => true; "gt left valid")]
    #[test_case("a>b.#valid" => true; "gt right valid")]
    #[test_case("a.#valid<=b" => true; "le left valid")]
    #[test_case("a<=b.#valid" => true; "le right valid")]
    #[test_case("a.#valid>=b" => true; "ge left valid")]
    #[test_case("a>=b.#valid" => true; "ge right valid")]
    #[test_case("a.#valid instanceof b" => true; "instanceof left valid")]
    #[test_case("a instanceof b.#valid" => true; "instanceof right valid")]
    #[test_case("a.#valid in b" => true; "in left valid")]
    #[test_case("a in b.#valid" => true; "in right valid")]
    #[test_case("a.#invalid" => false; "fallthru invalid")]
    #[test_case("a.#invalid<b" => false; "lt left invalid")]
    #[test_case("a<b.#invalid" => false; "lt right invalid")]
    #[test_case("a.#invalid>b" => false; "gt left invalid")]
    #[test_case("a>b.#invalid" => false; "gt right invalid")]
    #[test_case("a.#invalid<=b" => false; "le left invalid")]
    #[test_case("a<=b.#invalid" => false; "le right invalid")]
    #[test_case("a.#invalid>=b" => false; "ge left invalid")]
    #[test_case("a>=b.#invalid" => false; "ge right invalid")]
    #[test_case("a.#invalid instanceof b" => false; "instanceof left invalid")]
    #[test_case("a instanceof b.#invalid" => false; "instanceof right invalid")]
    #[test_case("a.#invalid in b" => false; "in left invalid")]
    #[test_case("a in b.#invalid" => false; "in right invalid")]
    #[test_case("#other in a.#valid" => true; "privateid: both valid")]
    #[test_case("#nonsense in a.#valid" => false; "privateid: id invalid")]
    #[test_case("#other in a.#invalid" => false; "privateid: expr invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = RelationalExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#other"), JSString::from("#valid")])
    }

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "fall thru")]
    #[test_case("package<3", true => sset(&[PACKAGE_NOT_ALLOWED]); "left lt right; left bad")]
    #[test_case("3<package", true => sset(&[PACKAGE_NOT_ALLOWED]); "left lt right; right bad")]
    #[test_case("package>3", true => sset(&[PACKAGE_NOT_ALLOWED]); "left gt right; left bad")]
    #[test_case("3>package", true => sset(&[PACKAGE_NOT_ALLOWED]); "left gt right; right bad")]
    #[test_case("package<=3", true => sset(&[PACKAGE_NOT_ALLOWED]); "left le right; left bad")]
    #[test_case("3<=package", true => sset(&[PACKAGE_NOT_ALLOWED]); "left le right; right bad")]
    #[test_case("package>=3", true => sset(&[PACKAGE_NOT_ALLOWED]); "left ge right; left bad")]
    #[test_case("3>=package", true => sset(&[PACKAGE_NOT_ALLOWED]); "left ge right; right bad")]
    #[test_case("package instanceof 3", true => sset(&[PACKAGE_NOT_ALLOWED]); "left instanceof right; left bad")]
    #[test_case("3 instanceof package", true => sset(&[PACKAGE_NOT_ALLOWED]); "left instanceof right; right bad")]
    #[test_case("package in 3", true => sset(&[PACKAGE_NOT_ALLOWED]); "left in right; left bad")]
    #[test_case("3 in package", true => sset(&[PACKAGE_NOT_ALLOWED]); "left in right; right bad")]
    #[test_case("#a in package", true => sset(&[PACKAGE_NOT_ALLOWED]); "privateid")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        RelationalExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a > 0" => true; "expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        RelationalExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .is_strictly_deletable()
    }

    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("arguments < bob" => true; "a lt b (left)")]
    #[test_case("bob < arguments" => true; "a lt b (right)")]
    #[test_case("arguments > bob" => true; "a gt b (left)")]
    #[test_case("bob > arguments" => true; "a gt b (right)")]
    #[test_case("arguments <= bob" => true; "a le b (left)")]
    #[test_case("bob <= arguments" => true; "a le b (right)")]
    #[test_case("arguments >= bob" => true; "a ge b (left)")]
    #[test_case("bob >= arguments" => true; "a ge b (right)")]
    #[test_case("arguments instanceof bob" => true; "a instanceof b (left)")]
    #[test_case("bob instanceof arguments" => true; "a instanceof b (right)")]
    #[test_case("arguments in bob" => true; "a in b (left)")]
    #[test_case("bob in arguments" => true; "a in b (right)")]
    #[test_case("#bob in arguments" => true; "private in (yes)")]
    #[test_case("xyzzy" => false; "Exp (no)")]
    #[test_case("xyzzy < bob" => false; "a lt b (no)")]
    #[test_case("xyzzy > bob" => false; "a gt b (no)")]
    #[test_case("xyzzy <= bob" => false; "a le b (no)")]
    #[test_case("xyzzy >= bob" => false; "a ge b (no)")]
    #[test_case("xyzzy instanceof bob" => false; "a instanceof b (no)")]
    #[test_case("xyzzy in bob" => false; "a in b (no)")]
    #[test_case("#bob in xyzzy" => false; "private in (no)")]
    fn contains_arguments(src: &str) -> bool {
        RelationalExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .contains_arguments()
    }

    #[test_case("eval", false => ATTKind::Simple; "simple eval")]
    #[test_case("eval", true => ATTKind::Invalid; "strict eval")]
    #[test_case("a<b", false => ATTKind::Invalid; "lt")]
    #[test_case("a<=b", false => ATTKind::Invalid; "le")]
    #[test_case("a>b", false => ATTKind::Invalid; "gt")]
    #[test_case("a>=b", false => ATTKind::Invalid; "ge")]
    #[test_case("a in b", false => ATTKind::Invalid; "a in b")]
    #[test_case("a instanceof b", false => ATTKind::Invalid; "instanceof")]
    #[test_case("#a in b", false => ATTKind::Invalid; "privateid")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).relational_expression().assignment_target_type(strict)
    }

    #[test_case("a<b" => false; "lt")]
    #[test_case("a<=b" => false; "le")]
    #[test_case("a>b" => false; "gt")]
    #[test_case("a>=b" => false; "ge")]
    #[test_case("a in b" => false; "a in b")]
    #[test_case("a instanceof b" => false; "instanceof")]
    #[test_case("#a in b" => false; "privateid")]
    #[test_case("function bob(){}" => true; "function fallthru")]
    #[test_case("1" => false; "literal fallthru")]
    fn is_named_function(src: &str) -> bool {
        Maker::new(src).relational_expression().is_named_function()
    }

    #[test_case("  a<b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "lt")]
    #[test_case("  a<=b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 4 }}; "le")]
    #[test_case("  a>b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "gt")]
    #[test_case("  a>=b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 4 }}; "ge")]
    #[test_case("  a in b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 6 }}; "a in b")]
    #[test_case("  a instanceof b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 14 }}; "instanceof")]
    #[test_case("  #a in b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 7 }}; "privateid")]
    #[test_case("  998" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "literal")]
    fn location(src: &str) -> Location {
        Maker::new(src).relational_expression().location()
    }
}
