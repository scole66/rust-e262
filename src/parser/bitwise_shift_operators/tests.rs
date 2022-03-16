use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::{test_agent, unwind_syntax_error_object};
use ahash::AHashSet;
use test_case::test_case;

// SHIFT EXPRESSION
#[test]
fn shift_expression_test_01() {
    let (se, scanner) = check(ShiftExpression::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*se, ShiftExpression::AdditiveExpression(_)));
    pretty_check(&*se, "ShiftExpression: a", vec!["AdditiveExpression: a"]);
    concise_check(&*se, "IdentifierName: a", vec![]);
    format!("{:?}", se);
    assert_eq!(se.is_function_definition(), false);
    assert_eq!(se.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn shift_expression_test_02() {
    let (se, scanner) = check(ShiftExpression::parse(&mut newparser("a << b"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(&*se, ShiftExpression::LeftShift(_, _)));
    pretty_check(&*se, "ShiftExpression: a << b", vec!["ShiftExpression: a", "AdditiveExpression: b"]);
    concise_check(&*se, "ShiftExpression: a << b", vec!["IdentifierName: a", "Punctuator: <<", "IdentifierName: b"]);
    format!("{:?}", se);
    assert_eq!(se.is_function_definition(), false);
    assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn shift_expression_test_03() {
    let (se, scanner) = check(ShiftExpression::parse(&mut newparser("a >> b"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    assert!(matches!(&*se, ShiftExpression::SignedRightShift(_, _)));
    pretty_check(&*se, "ShiftExpression: a >> b", vec!["ShiftExpression: a", "AdditiveExpression: b"]);
    concise_check(&*se, "ShiftExpression: a >> b", vec!["IdentifierName: a", "Punctuator: >>", "IdentifierName: b"]);
    format!("{:?}", se);
    assert_eq!(se.is_function_definition(), false);
    assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn shift_expression_test_04() {
    let (se, scanner) = check(ShiftExpression::parse(&mut newparser("a >>> b"), Scanner::new(), false, false));
    chk_scan(&scanner, 7);
    assert!(matches!(&*se, ShiftExpression::UnsignedRightShift(_, _)));
    pretty_check(&*se, "ShiftExpression: a >>> b", vec!["ShiftExpression: a", "AdditiveExpression: b"]);
    concise_check(&*se, "ShiftExpression: a >>> b", vec!["IdentifierName: a", "Punctuator: >>>", "IdentifierName: b"]);
    format!("{:?}", se);
    assert_eq!(se.is_function_definition(), false);
    assert_eq!(se.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn shift_expression_test_05() {
    check_err(ShiftExpression::parse(&mut newparser(""), Scanner::new(), false, false), "ExponentiationExpression expected", 1, 1);
}
#[test]
fn shift_expression_test_06() {
    let (se, scanner) = check(ShiftExpression::parse(&mut newparser("a >>> @"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*se, ShiftExpression::AdditiveExpression(_)));
    assert_eq!(se.is_function_definition(), false);
    assert_eq!(se.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn shift_expression_test_prettyerrors_1() {
    let (item, _) = ShiftExpression::parse(&mut newparser("3>>4"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn shift_expression_test_prettyerrors_2() {
    let (item, _) = ShiftExpression::parse(&mut newparser("3"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn shift_expression_test_prettyerrors_3() {
    let (item, _) = ShiftExpression::parse(&mut newparser("3<<4"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn shift_expression_test_prettyerrors_4() {
    let (item, _) = ShiftExpression::parse(&mut newparser("3>>>4"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn shift_expression_test_conciseerrors_1() {
    let (item, _) = ShiftExpression::parse(&mut newparser("3>>4"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn shift_expression_test_conciseerrors_2() {
    let (item, _) = ShiftExpression::parse(&mut newparser("3"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn shift_expression_test_conciseerrors_3() {
    let (item, _) = ShiftExpression::parse(&mut newparser("3<<4"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn shift_expression_test_conciseerrors_4() {
    let (item, _) = ShiftExpression::parse(&mut newparser("3>>>4"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn shift_expression_test_contains_01() {
    let (item, _) = ShiftExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn shift_expression_test_contains_02() {
    let (item, _) = ShiftExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn shift_expression_test_contains_03() {
    let (item, _) = ShiftExpression::parse(&mut newparser("this << 1"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn shift_expression_test_contains_04() {
    let (item, _) = ShiftExpression::parse(&mut newparser("1 << this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn shift_expression_test_contains_05() {
    let (item, _) = ShiftExpression::parse(&mut newparser("1 << 1"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn shift_expression_test_contains_06() {
    let (item, _) = ShiftExpression::parse(&mut newparser("this >> 1"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn shift_expression_test_contains_07() {
    let (item, _) = ShiftExpression::parse(&mut newparser("1 >> this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn shift_expression_test_contains_08() {
    let (item, _) = ShiftExpression::parse(&mut newparser("1 >> 1"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn shift_expression_test_contains_09() {
    let (item, _) = ShiftExpression::parse(&mut newparser("this >>> 1"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn shift_expression_test_contains_10() {
    let (item, _) = ShiftExpression::parse(&mut newparser("1 >>> this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn shift_expression_test_contains_11() {
    let (item, _) = ShiftExpression::parse(&mut newparser("1 >>> 1"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("'string'" => Some(String::from("string")); "String Token")]
#[test_case("a>>>b" => None; "Not token")]
fn shift_expression_test_as_string_literal(src: &str) -> Option<String> {
    let (item, _) = ShiftExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.as_string_literal().map(|st| String::from(st.value))
}
#[test_case("item.#valid" => true; "Fallthru valid")]
#[test_case("item.#valid << a" => true; "LeftShift Left valid")]
#[test_case("a << item.#valid" => true; "LeftShift Right valid")]
#[test_case("item.#valid >> a" => true; "SignedRightShift Left valid")]
#[test_case("a >> item.#valid" => true; "SignedRightShift Right valid")]
#[test_case("item.#valid >>> a" => true; "UnsignedRightShift Left valid")]
#[test_case("a >>> item.#valid" => true; "UnsignedRightShift Right valid")]
#[test_case("item.#invalid" => false; "Fallthru invalid")]
#[test_case("item.#invalid << a" => false; "LeftShift Left invalid")]
#[test_case("a << item.#invalid" => false; "LeftShift Right invalid")]
#[test_case("item.#invalid >> a" => false; "SignedRightShift Left invalid")]
#[test_case("a >> item.#invalid" => false; "SignedRightShift Right invalid")]
#[test_case("item.#invalid >>> a" => false; "UnsignedRightShift Left invalid")]
#[test_case("a >>> item.#invalid" => false; "UnsignedRightShift Right invalid")]
fn shift_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ShiftExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod shift_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => AHashSet::from_iter(["‘package’ not allowed as an identifier in strict mode".to_string()]); "fall thru")]
    #[test_case("package<<3", true => AHashSet::from_iter(["‘package’ not allowed as an identifier in strict mode".to_string()]); "left shl right; left bad")]
    #[test_case("3<<package", true => AHashSet::from_iter(["‘package’ not allowed as an identifier in strict mode".to_string()]); "left shl right; right bad")]
    #[test_case("package>>3", true => AHashSet::from_iter(["‘package’ not allowed as an identifier in strict mode".to_string()]); "left shr right; left bad")]
    #[test_case("3>>package", true => AHashSet::from_iter(["‘package’ not allowed as an identifier in strict mode".to_string()]); "left shr right; right bad")]
    #[test_case("package>>>3", true => AHashSet::from_iter(["‘package’ not allowed as an identifier in strict mode".to_string()]); "left ushr right; left bad")]
    #[test_case("3>>>package", true => AHashSet::from_iter(["‘package’ not allowed as an identifier in strict mode".to_string()]); "left ushr right; right bad")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        ShiftExpression::parse(&mut newparser(src), Scanner::new(), false, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a >> b" => true; "expression")]
    fn is_strictly_deletable(src: &str) -> bool {
        ShiftExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.is_strictly_deletable()
    }
}
