use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::test_agent;
use test_case::test_case;

#[test]
fn bitwise_and_expression_test_01() {
    let (pn, scanner) = check(BitwiseANDExpression::parse(&mut newparser("a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pn, BitwiseANDExpression::EqualityExpression(_)));
    pretty_check(&*pn, "BitwiseANDExpression: a", vec!["EqualityExpression: a"]);
    concise_check(&*pn, "IdentifierName: a", vec![]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
    assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn bitwise_and_expression_test_02() {
    let (pn, scanner) = check(BitwiseANDExpression::parse(&mut newparser("a&b"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(&*pn, BitwiseANDExpression::BitwiseAND(_, _)));
    pretty_check(&*pn, "BitwiseANDExpression: a & b", vec!["BitwiseANDExpression: a", "EqualityExpression: b"]);
    concise_check(&*pn, "BitwiseANDExpression: a & b", vec!["IdentifierName: a", "Punctuator: &", "IdentifierName: b"]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
    assert_eq!(pn.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn bitwise_and_expression_test_03() {
    let (pn, scanner) = check(BitwiseANDExpression::parse(&mut newparser("a&@"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*pn, BitwiseANDExpression::EqualityExpression(_)));
    pretty_check(&*pn, "BitwiseANDExpression: a", vec!["EqualityExpression: a"]);
    concise_check(&*pn, "IdentifierName: a", vec![]);
    format!("{:?}", pn);
    assert_eq!(pn.is_function_definition(), false);
    assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn bitwise_and_expression_test_04() {
    check_err(BitwiseANDExpression::parse(&mut newparser(""), Scanner::new(), true, false, false), "RelationalExpression expected", 1, 1);
}
#[test]
fn bitwise_and_expression_test_prettyerrors() {
    let (item, _) = BitwiseANDExpression::parse(&mut newparser("a & b"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn bitwise_and_expression_test_conciseerrors() {
    let (item, _) = BitwiseANDExpression::parse(&mut newparser("a & b"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn bitwise_and_expression_test_contains_01() {
    let (item, _) = BitwiseANDExpression::parse(&mut newparser("this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn bitwise_and_expression_test_contains_02() {
    let (item, _) = BitwiseANDExpression::parse(&mut newparser("0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn bitwise_and_expression_test_contains_03() {
    let (item, _) = BitwiseANDExpression::parse(&mut newparser("this & 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn bitwise_and_expression_test_contains_04() {
    let (item, _) = BitwiseANDExpression::parse(&mut newparser("0 & this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn bitwise_and_expression_test_contains_05() {
    let (item, _) = BitwiseANDExpression::parse(&mut newparser("0 & 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("'string'" => Some(JSString::from("string")); "String Token")]
#[test_case("a&b" => None; "Not token")]
fn bitwise_and_expression_test_as_string_literal(src: &str) -> Option<JSString> {
    let (item, _) = BitwiseANDExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.as_string_literal().map(|st| st.value)
}
#[test_case("item.#valid" => true; "Fallthru valid")]
#[test_case("item.#valid & 0" => true; "Left valid")]
#[test_case("0 & item.#valid" => true; "Right valid")]
#[test_case("item.#invalid" => false; "Fallthru invalid")]
#[test_case("item.#invalid & 0" => false; "Left invalid")]
#[test_case("0 & item.#invalid" => false; "Right invalid")]
fn bitwise_and_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = BitwiseANDExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod bitwise_and_expression {
    use super::*;
    use test_case::test_case;

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        BitwiseANDExpression::parse(&mut newparser("0"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a & b" => true; "expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        BitwiseANDExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.is_strictly_deletable()
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
    assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
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
    assert_eq!(pn.assignment_target_type(), ATTKind::Invalid);
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
    assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn bitwise_xor_expression_test_04() {
    check_err(BitwiseXORExpression::parse(&mut newparser(""), Scanner::new(), true, false, false), "RelationalExpression expected", 1, 1);
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
    let (item, _) = BitwiseXORExpression::parse(&mut newparser("this ^ 0"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn bitwise_xor_expression_test_contains_04() {
    let (item, _) = BitwiseXORExpression::parse(&mut newparser("0 ^ this"), Scanner::new(), true, false, false).unwrap();
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
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod bitwise_xor_expression {
    use super::*;
    use test_case::test_case;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        BitwiseXORExpression::parse(&mut newparser("0"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a ^ b" => true; "expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        BitwiseXORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.is_strictly_deletable()
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
    assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
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
    assert_eq!(pn.assignment_target_type(), ATTKind::Invalid);
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
    assert_eq!(pn.assignment_target_type(), ATTKind::Simple);
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
    check_err(BitwiseORExpression::parse(&mut newparser(""), Scanner::new(), true, false, false), "RelationalExpression expected", 1, 1);
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
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod bitwise_or_expression {
    use super::*;
    use test_case::test_case;

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        BitwiseORExpression::parse(&mut newparser("0"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a | b" => true; "expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        BitwiseORExpression::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap().0.is_strictly_deletable()
    }
}
