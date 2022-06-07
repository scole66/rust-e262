use super::testhelp::{check, check_err, chk_scan, newparser, set, Maker, INTERFACE_NOT_ALLOWED, PACKAGE_NOT_ALLOWED};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::{test_agent, unwind_syntax_error_object};
use ahash::AHashSet;
use test_case::test_case;

// LOGICAL AND EXPRESSION
mod logical_and_expression {
    use super::*;
    use test_case::test_case;

    #[test]
    fn parse_01() {
        let (pn, scanner) = check(LogicalANDExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, LogicalANDExpression::BitwiseORExpression(_)));
        pretty_check(&*pn, "LogicalANDExpression: a", vec!["BitwiseORExpression: a"]);
        concise_check(&*pn, "IdentifierName: a", vec![]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
    }
    #[test]
    fn parse_02() {
        let (pn, scanner) =
            check(LogicalANDExpression::parse(&mut newparser("a&&b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*pn, LogicalANDExpression::LogicalAND(..)));
        pretty_check(&*pn, "LogicalANDExpression: a && b", vec!["LogicalANDExpression: a", "BitwiseORExpression: b"]);
        concise_check(
            &*pn,
            "LogicalANDExpression: a && b",
            vec!["IdentifierName: a", "Punctuator: &&", "IdentifierName: b"],
        );
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
    }
    #[test]
    fn parse_03() {
        let (pn, scanner) =
            check(LogicalANDExpression::parse(&mut newparser("a&&"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, LogicalANDExpression::BitwiseORExpression(..)));
        pretty_check(&*pn, "LogicalANDExpression: a", vec!["BitwiseORExpression: a"]);
        concise_check(&*pn, "IdentifierName: a", vec![]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
    }
    #[test]
    fn prettyerrors_1() {
        let (item, _) = LogicalANDExpression::parse(&mut newparser("3"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_2() {
        let (item, _) =
            LogicalANDExpression::parse(&mut newparser("3&&b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_1() {
        let (item, _) = LogicalANDExpression::parse(&mut newparser("3"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_2() {
        let (item, _) =
            LogicalANDExpression::parse(&mut newparser("3&&b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) =
            LogicalANDExpression::parse(&mut newparser("this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_02() {
        let (item, _) = LogicalANDExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_03() {
        let (item, _) =
            LogicalANDExpression::parse(&mut newparser("this && 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_04() {
        let (item, _) =
            LogicalANDExpression::parse(&mut newparser("0 && this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_05() {
        let (item, _) =
            LogicalANDExpression::parse(&mut newparser("0 && 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test_case("'string'" => Some(JSString::from("string")); "String Token")]
    #[test_case("a&&b" => None; "Not token")]
    fn as_string_literal(src: &str) -> Option<JSString> {
        let (item, _) = LogicalANDExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
        item.as_string_literal().map(|st| st.value)
    }
    #[test_case("item.#valid" => true; "Fallthru valid")]
    #[test_case("item.#valid && a" => true; "Left valid")]
    #[test_case("a && item.#valid" => true; "Right valid")]
    #[test_case("item.#invalid" => false; "Fallthru invalid")]
    #[test_case("item.#invalid && a" => false; "Left invalid")]
    #[test_case("a && item.#invalid" => false; "Right invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = LogicalANDExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "fall thru")]
    #[test_case("package&&interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "logical and")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        LogicalANDExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a && b" => true; "expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        LogicalANDExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .is_strictly_deletable()
    }

    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("arguments && bob" => true; "a and b (left)")]
    #[test_case("bob && arguments" => true; "a and b (right)")]
    #[test_case("xyzzy" => false; "Exp (no)")]
    #[test_case("xyzzy && bob" => false; "a and b (no)")]
    fn contains_arguments(src: &str) -> bool {
        LogicalANDExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .contains_arguments()
    }

    #[test_case("eval", false => ATTKind::Simple; "simple eval")]
    #[test_case("eval", true => ATTKind::Invalid; "strict eval")]
    #[test_case("a&&b", false => ATTKind::Invalid; "land")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).logical_and_expression().assignment_target_type(strict)
    }
}

// LOGICAL OR EXPRESSION
mod logical_or_expression {
    use super::*;
    use test_case::test_case;

    #[test]
    fn parse_01() {
        let (pn, scanner) = check(LogicalORExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, LogicalORExpression::LogicalANDExpression(_)));
        pretty_check(&*pn, "LogicalORExpression: a", vec!["LogicalANDExpression: a"]);
        concise_check(&*pn, "IdentifierName: a", vec![]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
    }
    #[test]
    fn parse_02() {
        let (pn, scanner) =
            check(LogicalORExpression::parse(&mut newparser("a||b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*pn, LogicalORExpression::LogicalOR(..)));
        pretty_check(&*pn, "LogicalORExpression: a || b", vec!["LogicalORExpression: a", "LogicalANDExpression: b"]);
        concise_check(
            &*pn,
            "LogicalORExpression: a || b",
            vec!["IdentifierName: a", "Punctuator: ||", "IdentifierName: b"],
        );
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
    }
    #[test]
    fn parse_03() {
        let (pn, scanner) =
            check(LogicalORExpression::parse(&mut newparser("a||"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, LogicalORExpression::LogicalANDExpression(..)));
        pretty_check(&*pn, "LogicalORExpression: a", vec!["LogicalANDExpression: a"]);
        concise_check(&*pn, "IdentifierName: a", vec![]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
    }
    #[test]
    fn prettyerrors_1() {
        let (item, _) = LogicalORExpression::parse(&mut newparser("3"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_2() {
        let (item, _) = LogicalORExpression::parse(&mut newparser("3||b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_1() {
        let (item, _) = LogicalORExpression::parse(&mut newparser("3"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_2() {
        let (item, _) = LogicalORExpression::parse(&mut newparser("3||b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) = LogicalORExpression::parse(&mut newparser("this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_02() {
        let (item, _) = LogicalORExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_03() {
        let (item, _) =
            LogicalORExpression::parse(&mut newparser("this || 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_04() {
        let (item, _) =
            LogicalORExpression::parse(&mut newparser("0 || this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_05() {
        let (item, _) =
            LogicalORExpression::parse(&mut newparser("0 || 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test_case("'string'" => Some(JSString::from("string")); "String Token")]
    #[test_case("a||b" => None; "Not token")]
    fn as_string_literal(src: &str) -> Option<JSString> {
        let (item, _) = LogicalORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
        item.as_string_literal().map(|st| st.value)
    }
    #[test_case("item.#valid" => true; "Fallthru valid")]
    #[test_case("item.#valid || a" => true; "Left valid")]
    #[test_case("a || item.#valid" => true; "Right valid")]
    #[test_case("item.#invalid" => false; "Fallthru invalid")]
    #[test_case("item.#invalid || a" => false; "Left invalid")]
    #[test_case("a || item.#invalid" => false; "Right invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = LogicalORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "fall thru")]
    #[test_case("package||interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "logical or")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        LogicalORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a || b" => true; "expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        LogicalORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .is_strictly_deletable()
    }
    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("arguments || bob" => true; "a or b (left)")]
    #[test_case("bob || arguments" => true; "a or b (right)")]
    #[test_case("xyzzy" => false; "Exp (no)")]
    #[test_case("xyzzy || bob" => false; "a or b (no)")]
    fn contains_arguments(src: &str) -> bool {
        LogicalORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .contains_arguments()
    }

    #[test_case("eval", false => ATTKind::Simple; "simple eval")]
    #[test_case("eval", true => ATTKind::Invalid; "strict eval")]
    #[test_case("a||b", false => ATTKind::Invalid; "lor")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).logical_or_expression().assignment_target_type(strict)
    }
}

// COALESCE EXPRESSION
mod coalesce_expression {
    use super::*;
    use test_case::test_case;

    #[test]
    fn parse_01() {
        let (pn, scanner) =
            check(CoalesceExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 4);
        pretty_check(&*pn, "CoalesceExpression: a ?? b", vec!["CoalesceExpressionHead: a", "BitwiseORExpression: b"]);
        concise_check(
            &*pn,
            "CoalesceExpression: a ?? b",
            vec!["IdentifierName: a", "Punctuator: ??", "IdentifierName: b"],
        );
        format!("{:?}", pn);
    }
    #[test]
    fn parse_02() {
        let (pn, scanner) =
            check(CoalesceExpression::parse(&mut newparser("z??a??b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 7);
        pretty_check(
            &*pn,
            "CoalesceExpression: z ?? a ?? b",
            vec!["CoalesceExpressionHead: z ?? a", "BitwiseORExpression: b"],
        );
        concise_check(
            &*pn,
            "CoalesceExpression: z ?? a ?? b",
            vec!["CoalesceExpression: z ?? a", "Punctuator: ??", "IdentifierName: b"],
        );
        format!("{:?}", pn);
    }
    #[test]
    fn cache_01() {
        let mut parser = newparser("z??a??b");
        let (node, scanner) = check(CoalesceExpression::parse(&mut parser, Scanner::new(), true, false, false));
        let (node2, scanner2) = check(CoalesceExpression::parse(&mut parser, Scanner::new(), true, false, false));
        assert!(scanner == scanner2);
        assert!(Rc::ptr_eq(&node, &node2));
    }
    #[test]
    fn parse_03() {
        check_err(
            CoalesceExpression::parse(&mut newparser(""), Scanner::new(), true, false, false),
            "RelationalExpression expected",
            1,
            1,
        );
    }
    #[test]
    fn parse_04() {
        check_err(
            CoalesceExpression::parse(&mut newparser("a??"), Scanner::new(), true, false, false),
            "Invalid Coalesce Expression",
            1,
            1,
        );
    }
    #[test]
    fn prettyerrors_1() {
        let (item, _) = CoalesceExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_1() {
        let (item, _) = CoalesceExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) =
            CoalesceExpression::parse(&mut newparser("this ?? 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_02() {
        let (item, _) =
            CoalesceExpression::parse(&mut newparser("0 ?? this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_03() {
        let (item, _) =
            CoalesceExpression::parse(&mut newparser("0 ?? 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test_case("item.#valid ?? a" => true; "Left valid")]
    #[test_case("a ?? item.#valid" => true; "Right valid")]
    #[test_case("item.#invalid ?? a" => false; "Left invalid")]
    #[test_case("a ?? item.#invalid" => false; "Right invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = CoalesceExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package??interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "coalesce")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        CoalesceExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }

    #[test_case("arguments??bob" => true; "a coal b (left)")]
    #[test_case("bob??arguments" => true; "a coal b (right)")]
    #[test_case("xyzzy??bob" => false; "a coal b (no)")]
    fn contains_arguments(src: &str) -> bool {
        CoalesceExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.contains_arguments()
    }
}

// COALESCE EXPRESSION HEAD
#[test]
fn coalesce_expression_head_test_01() {
    let (pn, scanner) = check(CoalesceExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    let head = &*pn.head;
    pretty_check(head, "CoalesceExpressionHead: a", vec!["BitwiseORExpression: a"]);
    concise_check(head, "IdentifierName: a", vec![]);
}
#[test]
fn coalesce_expression_head_test_02() {
    let (pn, scanner) = check(CoalesceExpression::parse(&mut newparser("z??a??b"), Scanner::new(), true, false, false));
    let head = &*pn.head;
    chk_scan(&scanner, 7);
    pretty_check(head, "CoalesceExpressionHead: z ?? a", vec!["CoalesceExpression: z ?? a"]);
    concise_check(
        head,
        "CoalesceExpression: z ?? a",
        vec!["IdentifierName: z", "Punctuator: ??", "IdentifierName: a"],
    );
}
#[test]
fn coalesce_expression_head_test_prettyerrors_1() {
    let (item, _) = CoalesceExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item.head);
}
#[test]
fn coalesce_expression_head_test_prettyerrors_2() {
    let (item, _) = CoalesceExpression::parse(&mut newparser("z??a??b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item.head);
}
#[test]
fn coalesce_expression_head_test_conciseerrors_1() {
    let (item, _) = CoalesceExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item.head);
}
#[test]
fn coalesce_expression_head_test_conciseerrors_2() {
    let (item, _) = CoalesceExpression::parse(&mut newparser("z??a??b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item.head);
}
#[test]
fn coalesce_expression_head_test_contains_01() {
    let (item_ce, _) =
        CoalesceExpression::parse(&mut newparser("this ?? 0"), Scanner::new(), true, false, false).unwrap();
    let item = &item_ce.head;
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn coalesce_expression_head_test_contains_02() {
    let (item_ce, _) = CoalesceExpression::parse(&mut newparser("0 ?? 0"), Scanner::new(), true, false, false).unwrap();
    let item = &item_ce.head;
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn coalesce_expression_head_test_contains_03() {
    let (item_ce, _) =
        CoalesceExpression::parse(&mut newparser("this ?? 0 ?? 1"), Scanner::new(), true, false, false).unwrap();
    let item = &item_ce.head;
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn coalesce_expression_head_test_contains_04() {
    let (item_ce, _) =
        CoalesceExpression::parse(&mut newparser("a ?? 0 ?? 1"), Scanner::new(), true, false, false).unwrap();
    let item = &item_ce.head;
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("item.#valid ?? a" => true; "BitwiseOR valid")]
#[test_case("item.#valid ?? a ?? b" => true; "Coalesce valid")]
#[test_case("item.#invalid ?? a" => false; "BitwiseOR invalid")]
#[test_case("item.#invalid ?? a ?? b" => false; "Coalesce invalid")]
fn coalesce_expression_head_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item_ce, _) = CoalesceExpression::parse(&mut newparser(src), Scanner::new(), true, false, false).unwrap();
    let item = &item_ce.head;
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod coalesce_expression_head {
    use super::*;
    use test_case::test_case;

    #[test_case("package??interface", true => set(&[PACKAGE_NOT_ALLOWED]); "MultiplicativeExpression")]
    #[test_case("package??interface??q", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "AE plus ME; AE bad")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        let ce = CoalesceExpression::parse(&mut newparser(src), Scanner::new(), false, true, true).unwrap().0;
        let ceh = &ce.head;
        ceh.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }

    #[test_case("arguments??bob" => true; "Exp (yes)")]
    #[test_case("arguments??bob??alice" => true; "Coal (yes)")]
    #[test_case("xyyzz??bob" => false; "Exp (no)")]
    #[test_case("xyyzz??bob??alice" => false; "Coal (no)")]
    fn contains_arguments(src: &str) -> bool {
        CoalesceExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .head
            .contains_arguments()
    }
}

// SHORT CIRCUIT EXPRESSION
#[test]
fn short_circuit_expression_test_01() {
    let (pn, scanner) =
        check(ShortCircuitExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(&*pn, ShortCircuitExpression::CoalesceExpression(..)));
    pretty_check(&*pn, "ShortCircuitExpression: a ?? b", vec!["CoalesceExpression: a ?? b"]);
    concise_check(&*pn, "CoalesceExpression: a ?? b", vec!["IdentifierName: a", "Punctuator: ??", "IdentifierName: b"]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
}
#[test]
fn short_circuit_expression_test_02() {
    let (pn, scanner) = check(ShortCircuitExpression::parse(&mut newparser("6"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pn, ShortCircuitExpression::LogicalORExpression(..)));
    pretty_check(&*pn, "ShortCircuitExpression: 6", vec!["LogicalORExpression: 6"]);
    concise_check(&*pn, "Numeric: 6", vec![]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
}
#[test]
fn short_circuit_expression_test_03() {
    check_err(
        ShortCircuitExpression::parse(&mut newparser(""), Scanner::new(), true, false, false),
        "Improper Expression",
        1,
        1,
    );
}
#[test]
fn short_circuit_expression_test_prettyerrors_1() {
    let (item, _) = ShortCircuitExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn short_circuit_expression_test_prettyerrors_2() {
    let (item, _) =
        ShortCircuitExpression::parse(&mut newparser("h || q"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn short_circuit_expression_test_conciseerrors_1() {
    let (item, _) = ShortCircuitExpression::parse(&mut newparser("a??b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn short_circuit_expression_test_conciseerrors_2() {
    let (item, _) =
        ShortCircuitExpression::parse(&mut newparser("h || q"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn short_circuit_expression_test_contains_01() {
    let (item, _) = ShortCircuitExpression::parse(&mut newparser("this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn short_circuit_expression_test_contains_02() {
    let (item, _) = ShortCircuitExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn short_circuit_expression_test_contains_03() {
    let (item, _) =
        ShortCircuitExpression::parse(&mut newparser("this ?? 1"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn short_circuit_expression_test_contains_04() {
    let (item, _) =
        ShortCircuitExpression::parse(&mut newparser("0 ?? 1"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("'string'" => Some(JSString::from("string")); "String Token")]
#[test_case("a??b" => None; "Not token")]
fn short_circuit_expression_test_as_string_literal(src: &str) -> Option<JSString> {
    let (item, _) = ShortCircuitExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.as_string_literal().map(|st| st.value)
}
#[test_case("item.#valid" => true; "fallthru valid")]
#[test_case("item.#valid ?? a" => true; "coalesce valid")]
#[test_case("item.#invalid" => false; "fallthru invalid")]
#[test_case("item.#invalid ?? a" => false; "coalesce invalid")]
fn short_circuit_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ShortCircuitExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod short_circuit_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "fall thru")]
    #[test_case("package??interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "coalesce")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        ShortCircuitExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a ?? b" => true; "expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        ShortCircuitExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .is_strictly_deletable()
    }

    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("arguments??bob" => true; "Coal (yes)")]
    #[test_case("xyzzy" => false; "Exp (no)")]
    #[test_case("xyzzy??bob" => false; "Coal (no)")]
    fn contains_arguments(src: &str) -> bool {
        ShortCircuitExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .contains_arguments()
    }

    #[test_case("eval", false => ATTKind::Simple; "simple eval")]
    #[test_case("eval", true => ATTKind::Invalid; "strict eval")]
    #[test_case("a??b", false => ATTKind::Invalid; "coalesce")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).short_circuit_expression().assignment_target_type(strict)
    }
}
