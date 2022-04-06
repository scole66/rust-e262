use super::testhelp::{check, check_err, chk_scan, newparser, set, PACKAGE_NOT_ALLOWED};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::{test_agent, unwind_syntax_error_object};
use ahash::AHashSet;

// ADDITIVE EXPRESSION
mod additive_expression {
    use super::*;
    use test_case::test_case;

    #[test]
    fn parse_01() {
        let (ae, scanner) = check(AdditiveExpression::parse(&mut newparser("a"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*ae, AdditiveExpression::MultiplicativeExpression(_)));
        pretty_check(&*ae, "AdditiveExpression: a", vec!["MultiplicativeExpression: a"]);
        concise_check(&*ae, "IdentifierName: a", vec![]);
        format!("{:?}", ae);
        assert_eq!(ae.is_function_definition(), false);
        assert_eq!(ae.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn parse_02() {
        let (ae, scanner) = check(AdditiveExpression::parse(&mut newparser("a+b"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*ae, AdditiveExpression::Add(..)));
        pretty_check(&*ae, "AdditiveExpression: a + b", vec!["AdditiveExpression: a", "MultiplicativeExpression: b"]);
        concise_check(&*ae, "AdditiveExpression: a + b", vec!["IdentifierName: a", "Punctuator: +", "IdentifierName: b"]);
        format!("{:?}", ae);
        assert_eq!(ae.is_function_definition(), false);
        assert_eq!(ae.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn parse_03() {
        let (ae, scanner) = check(AdditiveExpression::parse(&mut newparser("a-b"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*ae, AdditiveExpression::Subtract(..)));
        pretty_check(&*ae, "AdditiveExpression: a - b", vec!["AdditiveExpression: a", "MultiplicativeExpression: b"]);
        concise_check(&*ae, "AdditiveExpression: a - b", vec!["IdentifierName: a", "Punctuator: -", "IdentifierName: b"]);
        format!("{:?}", ae);
        assert_eq!(ae.is_function_definition(), false);
        assert_eq!(ae.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn parse_04() {
        let (ae, scanner) = check(AdditiveExpression::parse(&mut newparser("a-@"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*ae, AdditiveExpression::MultiplicativeExpression(..)));
        pretty_check(&*ae, "AdditiveExpression: a", vec!["MultiplicativeExpression: a"]);
        concise_check(&*ae, "IdentifierName: a", vec![]);
        format!("{:?}", ae);
        assert_eq!(ae.is_function_definition(), false);
        assert_eq!(ae.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn parse_05() {
        check_err(AdditiveExpression::parse(&mut newparser(""), Scanner::new(), false, false), "ExponentiationExpression expected", 1, 1);
    }
    #[test]
    fn prettyerrors_1() {
        let (item, _) = AdditiveExpression::parse(&mut newparser("3+4"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn prettyerrors_2() {
        let (item, _) = AdditiveExpression::parse(&mut newparser("3-4"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_1() {
        let (item, _) = AdditiveExpression::parse(&mut newparser("3+4"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_2() {
        let (item, _) = AdditiveExpression::parse(&mut newparser("3-4"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) = AdditiveExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_02() {
        let (item, _) = AdditiveExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_03() {
        let (item, _) = AdditiveExpression::parse(&mut newparser("this + 1"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_04() {
        let (item, _) = AdditiveExpression::parse(&mut newparser("1 + this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_05() {
        let (item, _) = AdditiveExpression::parse(&mut newparser("1 + 1"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_06() {
        let (item, _) = AdditiveExpression::parse(&mut newparser("this - 1"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_07() {
        let (item, _) = AdditiveExpression::parse(&mut newparser("1 - this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_08() {
        let (item, _) = AdditiveExpression::parse(&mut newparser("1 - 1"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test_case("'string'" => Some(String::from("string")); "String Token")]
    #[test_case("a+b" => None; "Not token")]
    fn as_string_literal(src: &str) -> Option<String> {
        let (item, _) = AdditiveExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.as_string_literal().map(|st| String::from(st.value))
    }
    #[test_case("item.#valid + blue" => true; "Add valid left")]
    #[test_case("blue + item.#valid" => true; "Add valid right")]
    #[test_case("blue + item.#invalid" => false; "Add invalid right")]
    #[test_case("item.#invalid + blue" => false; "Add invalid left")]
    #[test_case("item.#valid - blue" => true; "Sub valid left")]
    #[test_case("blue - item.#valid" => true; "Sub valid right")]
    #[test_case("blue - item.#invalid" => false; "Sub invalid right")]
    #[test_case("item.#invalid - blue" => false; "Sub invalid left")]
    #[test_case("item.#invalid" => false; "Fallthru: invalid")]
    #[test_case("item.#valid" => true; "Fallthru: valid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = AdditiveExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "MultiplicativeExpression")]
    #[test_case("package+3", true => set(&[PACKAGE_NOT_ALLOWED]); "AE plus ME; AE bad")]
    #[test_case("3+package", true => set(&[PACKAGE_NOT_ALLOWED]); "AE plus ME; ME bad")]
    #[test_case("package-3", true => set(&[PACKAGE_NOT_ALLOWED]); "AE minus ME; AE bad")]
    #[test_case("3-package", true => set(&[PACKAGE_NOT_ALLOWED]); "AE minus ME; ME bad")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        AdditiveExpression::parse(&mut newparser(src), Scanner::new(), false, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("a+b" => true; "expression")]
    #[test_case("1" => true; "literal")]
    fn is_strictly_deletable(src: &str) -> bool {
        AdditiveExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.is_strictly_deletable()
    }

    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("arguments + bob" => true; "Add (left)")]
    #[test_case("bob + arguments" => true; "Add (right)")]
    #[test_case("arguments - bob" => true; "Subtract (left)")]
    #[test_case("bob - arguments" => true; "Subtract (right)")]
    #[test_case("xyzzy" => false; "Exp (no)")]
    #[test_case("xyzzy + bob" => false; "Add (no)")]
    #[test_case("xyzzy - bob" => false; "Subtract (no)")]
    fn contains_arguments(src: &str) -> bool {
        AdditiveExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }
}
