use super::testhelp::*;
use super::*;
use crate::prettyprint::testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

mod bitwise_and_expression {
    use super::*;
    use test_case::test_case;

    #[test]
    fn parse_01() {
        let (pn, scanner) = check(BitwiseANDExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, BitwiseANDExpression::EqualityExpression(_)));
        pretty_check(&*pn, "BitwiseANDExpression: a", vec!["EqualityExpression: a"]);
        concise_check(&*pn, "IdentifierName: a", vec![]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
    }
    #[test]
    fn parse_02() {
        let (pn, scanner) =
            check(BitwiseANDExpression::parse(&mut newparser("a&b"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*pn, BitwiseANDExpression::BitwiseAND(_, _)));
        pretty_check(&*pn, "BitwiseANDExpression: a & b", vec!["BitwiseANDExpression: a", "EqualityExpression: b"]);
        concise_check(
            &*pn,
            "BitwiseANDExpression: a & b",
            vec!["IdentifierName: a", "Punctuator: &", "IdentifierName: b"],
        );
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
    }
    #[test]
    fn parse_03() {
        let (pn, scanner) =
            check(BitwiseANDExpression::parse(&mut newparser("a&@"), Scanner::new(), true, false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, BitwiseANDExpression::EqualityExpression(_)));
        pretty_check(&*pn, "BitwiseANDExpression: a", vec!["EqualityExpression: a"]);
        concise_check(&*pn, "IdentifierName: a", vec![]);
        format!("{:?}", pn);
        assert_eq!(pn.is_function_definition(), false);
    }
    #[test]
    fn parse_04() {
        check_err(
            BitwiseANDExpression::parse(&mut newparser(""), Scanner::new(), true, false, false),
            "RelationalExpression expected",
            1,
            1,
        );
    }
    #[test]
    fn prettyerrors() {
        let (item, _) =
            BitwiseANDExpression::parse(&mut newparser("a & b"), Scanner::new(), true, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors() {
        let (item, _) =
            BitwiseANDExpression::parse(&mut newparser("a & b"), Scanner::new(), true, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) =
            BitwiseANDExpression::parse(&mut newparser("this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_02() {
        let (item, _) = BitwiseANDExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_03() {
        let (item, _) =
            BitwiseANDExpression::parse(&mut newparser("this & 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_04() {
        let (item, _) =
            BitwiseANDExpression::parse(&mut newparser("0 & this"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_05() {
        let (item, _) =
            BitwiseANDExpression::parse(&mut newparser("0 & 0"), Scanner::new(), true, false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test_case("'string'" => Some(JSString::from("string")); "String Token")]
    #[test_case("a&b" => None; "Not token")]
    fn as_string_literal(src: &str) -> Option<JSString> {
        let (item, _) = BitwiseANDExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
        item.as_string_literal().map(|st| st.value)
    }
    #[test_case("item.#valid" => true; "Fallthru valid")]
    #[test_case("item.#valid & 0" => true; "Left valid")]
    #[test_case("0 & item.#valid" => true; "Right valid")]
    #[test_case("item.#invalid" => false; "Fallthru invalid")]
    #[test_case("item.#invalid & 0" => false; "Left invalid")]
    #[test_case("0 & item.#invalid" => false; "Right invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = BitwiseANDExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "fall thru")]
    #[test_case("package&interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "bitwise and")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let agent = test_agent();
        let mut errs = vec![];
        BitwiseANDExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&agent, err.clone())))
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a & b" => true; "expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        BitwiseANDExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .is_strictly_deletable()
    }

    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("arguments & bob" => true; "a & b (left)")]
    #[test_case("bob & arguments" => true; "a & b (right)")]
    #[test_case("xyzzy" => false; "Exp (no)")]
    #[test_case("xyzzy & bob" => false; "a & b (no)")]
    fn contains_arguments(src: &str) -> bool {
        BitwiseANDExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .contains_arguments()
    }

    #[test_case("a&b", false => ATTKind::Invalid; "bitwise and")]
    #[test_case("eval", false => ATTKind::Simple; "eval non-strict")]
    #[test_case("eval", true => ATTKind::Invalid; "eval strict")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).bitwise_and_expression().assignment_target_type(strict)
    }

    #[test_case("a&b" => false; "bitwise and")]
    #[test_case("function bob(){}" => true; "function fallthru")]
    #[test_case("1" => false; "literal fallthru")]
    fn is_named_function(src: &str) -> bool {
        Maker::new(src).bitwise_and_expression().is_named_function()
    }

    #[test_case("  a&b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "bitwise and")]
    #[test_case("  998" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "literal")]
    fn location(src: &str) -> Location {
        Maker::new(src).bitwise_and_expression().location()
    }
}

#[test]
fn bitwise_xor_expression_test_01() {
    let (pn, scanner) = check(BitwiseXORExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pn, BitwiseXORExpression::BitwiseANDExpression(_)));
    pretty_check(&*pn, "BitwiseXORExpression: a", vec!["BitwiseANDExpression: a"]);
    concise_check(&*pn, "IdentifierName: a", vec![]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
}
#[test]
fn bitwise_xor_expression_test_02() {
    let (pn, scanner) = check(BitwiseXORExpression::parse(&mut newparser("a^b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*pn, BitwiseXORExpression::BitwiseXOR(_, _)));
    pretty_check(&*pn, "BitwiseXORExpression: a ^ b", vec!["BitwiseXORExpression: a", "BitwiseANDExpression: b"]);
    concise_check(&*pn, "BitwiseXORExpression: a ^ b", vec!["IdentifierName: a", "Punctuator: ^", "IdentifierName: b"]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
}
#[test]
fn bitwise_xor_expression_test_03() {
    let (pn, scanner) = check(BitwiseXORExpression::parse(&mut newparser("a^@"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pn, BitwiseXORExpression::BitwiseANDExpression(_)));
    pretty_check(&*pn, "BitwiseXORExpression: a", vec!["BitwiseANDExpression: a"]);
    concise_check(&*pn, "IdentifierName: a", vec![]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
}
#[test]
fn bitwise_xor_expression_test_04() {
    check_err(
        BitwiseXORExpression::parse(&mut newparser(""), Scanner::new(), true, false, false),
        "RelationalExpression expected",
        1,
        1,
    );
}
#[test]
fn bitwise_xor_expression_test_prettyerrors() {
    let (item, _) = BitwiseXORExpression::parse(&mut newparser("a ^ b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn bitwise_xor_expression_test_conciseerrors() {
    let (item, _) = BitwiseXORExpression::parse(&mut newparser("a ^ b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn bitwise_xor_expression_test_contains_01() {
    let (item, _) = BitwiseXORExpression::parse(&mut newparser("this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn bitwise_xor_expression_test_contains_02() {
    let (item, _) = BitwiseXORExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn bitwise_xor_expression_test_contains_03() {
    let (item, _) =
        BitwiseXORExpression::parse(&mut newparser("this ^ 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn bitwise_xor_expression_test_contains_04() {
    let (item, _) =
        BitwiseXORExpression::parse(&mut newparser("0 ^ this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn bitwise_xor_expression_test_contains_05() {
    let (item, _) = BitwiseXORExpression::parse(&mut newparser("0 ^ 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("'string'" => Some(JSString::from("string")); "String Token")]
#[test_case("a^b" => None; "Not token")]
fn bitwise_xor_expression_test_as_string_literal(src: &str) -> Option<JSString> {
    let (item, _) = BitwiseXORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.as_string_literal().map(|st| st.value)
}
#[test_case("item.#valid" => true; "Fallthru valid")]
#[test_case("item.#valid ^ 0" => true; "Left valid")]
#[test_case("0 ^ item.#valid" => true; "Right valid")]
#[test_case("item.#invalid" => false; "Fallthru invalid")]
#[test_case("item.#invalid ^ 0" => false; "Left invalid")]
#[test_case("0 ^ item.#invalid" => false; "Right invalid")]
fn bitwise_xor_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = BitwiseXORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod bitwise_xor_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "fall thru")]
    #[test_case("package^interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "bitwise xor")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let agent = test_agent();
        let mut errs = vec![];
        BitwiseXORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&agent, err.clone())))
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a ^ b" => true; "expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        BitwiseXORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .is_strictly_deletable()
    }

    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("arguments ^ bob" => true; "a ^ b (left)")]
    #[test_case("bob ^ arguments" => true; "a ^ b (right)")]
    #[test_case("xyzzy" => false; "Exp (no)")]
    #[test_case("xyzzy ^ bob" => false; "a ^ b (no)")]
    fn contains_arguments(src: &str) -> bool {
        BitwiseXORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .contains_arguments()
    }

    #[test_case("a^b", false => ATTKind::Invalid; "bitwise xor")]
    #[test_case("eval", false => ATTKind::Simple; "eval non-strict")]
    #[test_case("eval", true => ATTKind::Invalid; "eval strict")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).bitwise_xor_expression().assignment_target_type(strict)
    }

    #[test_case("a^b" => false; "bitwise xor")]
    #[test_case("function bob(){}" => true; "function fallthru")]
    #[test_case("1" => false; "literal fallthru")]
    fn is_named_function(src: &str) -> bool {
        Maker::new(src).bitwise_xor_expression().is_named_function()
    }

    #[test_case("  a^b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "bitwise xor")]
    #[test_case("  998" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "literal")]
    fn location(src: &str) -> Location {
        Maker::new(src).bitwise_xor_expression().location()
    }
}

#[test]
fn bitwise_or_expression_test_01() {
    let (pn, scanner) = check(BitwiseORExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pn, BitwiseORExpression::BitwiseXORExpression(_)));
    pretty_check(&*pn, "BitwiseORExpression: a", vec!["BitwiseXORExpression: a"]);
    concise_check(&*pn, "IdentifierName: a", vec![]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
}
#[test]
fn bitwise_or_expression_test_02() {
    let (pn, scanner) = check(BitwiseORExpression::parse(&mut newparser("a|b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*pn, BitwiseORExpression::BitwiseOR(_, _)));
    pretty_check(&*pn, "BitwiseORExpression: a | b", vec!["BitwiseORExpression: a", "BitwiseXORExpression: b"]);
    concise_check(&*pn, "BitwiseORExpression: a | b", vec!["IdentifierName: a", "Punctuator: |", "IdentifierName: b"]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
}
#[test]
fn bitwise_or_expression_test_03() {
    let (pn, scanner) = check(BitwiseORExpression::parse(&mut newparser("a|@"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pn, BitwiseORExpression::BitwiseXORExpression(_)));
    pretty_check(&*pn, "BitwiseORExpression: a", vec!["BitwiseXORExpression: a"]);
    concise_check(&*pn, "IdentifierName: a", vec![]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
}
#[test]
fn bitwise_or_expression_test_cache_01() {
    let mut parser = newparser("6|7");
    let (node, scanner) = check(BitwiseORExpression::parse(&mut parser, Scanner::new(), true, false, false));
    let (node2, scanner2) = check(BitwiseORExpression::parse(&mut parser, Scanner::new(), true, false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn bitwise_or_expression_test_04() {
    check_err(
        BitwiseORExpression::parse(&mut newparser(""), Scanner::new(), true, false, false),
        "RelationalExpression expected",
        1,
        1,
    );
}
#[test]
fn bitwise_or_expression_test_prettyerrors() {
    let (item, _) = BitwiseORExpression::parse(&mut newparser("a | b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn bitwise_or_expression_test_conciseerrors() {
    let (item, _) = BitwiseORExpression::parse(&mut newparser("a | b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn bitwise_or_expression_test_contains_01() {
    let (item, _) = BitwiseORExpression::parse(&mut newparser("this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn bitwise_or_expression_test_contains_02() {
    let (item, _) = BitwiseORExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn bitwise_or_expression_test_contains_03() {
    let (item, _) = BitwiseORExpression::parse(&mut newparser("this | 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn bitwise_or_expression_test_contains_04() {
    let (item, _) = BitwiseORExpression::parse(&mut newparser("0 | this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn bitwise_or_expression_test_contains_05() {
    let (item, _) = BitwiseORExpression::parse(&mut newparser("0 | 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("'string'" => Some(JSString::from("string")); "String Token")]
#[test_case("a|b" => None; "Not token")]
fn bitwise_or_expression_test_as_string_literal(src: &str) -> Option<JSString> {
    let (item, _) = BitwiseORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.as_string_literal().map(|st| st.value)
}
#[test_case("item.#valid" => true; "Fallthru valid")]
#[test_case("item.#valid | 0" => true; "Left valid")]
#[test_case("0 | item.#valid" => true; "Right valid")]
#[test_case("item.#invalid" => false; "Fallthru invalid")]
#[test_case("item.#invalid | 0" => false; "Left invalid")]
#[test_case("0 | item.#invalid" => false; "Right invalid")]
fn bitwise_or_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = BitwiseORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod bitwise_or_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "fall thru")]
    #[test_case("package|interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "bitwise or")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let agent = test_agent();
        let mut errs = vec![];
        BitwiseORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .early_errors(&agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&agent, err.clone())))
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a | b" => true; "expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        BitwiseORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .is_strictly_deletable()
    }

    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("arguments | bob" => true; "a | b (left)")]
    #[test_case("bob | arguments" => true; "a | b (right)")]
    #[test_case("xyzzy" => false; "Exp (no)")]
    #[test_case("xyzzy | bob" => false; "a | b (no)")]
    fn contains_arguments(src: &str) -> bool {
        BitwiseORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true)
            .unwrap()
            .0
            .contains_arguments()
    }

    #[test_case("a|b", false => ATTKind::Invalid; "bitwise or")]
    #[test_case("eval", false => ATTKind::Simple; "eval non-strict")]
    #[test_case("eval", true => ATTKind::Invalid; "eval strict")]
    fn assignment_target_type(src: &str, strict: bool) -> ATTKind {
        Maker::new(src).bitwise_or_expression().assignment_target_type(strict)
    }

    #[test_case("a|b" => false; "bitwise or")]
    #[test_case("function bob(){}" => true; "function fallthru")]
    #[test_case("1" => false; "literal fallthru")]
    fn is_named_function(src: &str) -> bool {
        Maker::new(src).bitwise_or_expression().is_named_function()
    }

    #[test_case("  a|b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "bitwise or")]
    #[test_case("  998" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3 }}; "literal")]
    fn location(src: &str) -> Location {
        Maker::new(src).bitwise_or_expression().location()
    }
}
