use super::testhelp::{check, check_err, chk_scan, newparser, set, PACKAGE_NOT_ALLOWED};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::{test_agent, unwind_syntax_error_object};
use ahash::AHashSet;

// UPDATE EXPRESSION
mod update_expression {
    use super::*;
    use test_case::test_case;

    #[test]
    fn lhs() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("78"), Scanner::new(), false, false));
        chk_scan(&scanner, 2);
        assert!(matches!(*ue, UpdateExpression::LeftHandSideExpression(_)));
        format!("{:?}", ue);
        pretty_check(&*ue, "UpdateExpression: 78", vec!["LeftHandSideExpression: 78"]);
        concise_check(&*ue, "Numeric: 78", vec![]);
        assert!(!ue.is_function_definition());
        assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn lhs_2() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("(x=>x*2)"), Scanner::new(), false, false));
        chk_scan(&scanner, 8);
        assert!(matches!(*ue, UpdateExpression::LeftHandSideExpression(_)));
        format!("{:?}", ue);
        pretty_check(&*ue, "UpdateExpression: ( x => x * 2 )", vec!["LeftHandSideExpression: ( x => x * 2 )"]);
        concise_check(&*ue, "ParenthesizedExpression: ( x => x * 2 )", vec!["Punctuator: (", "ArrowFunction: x => x * 2", "Punctuator: )"]);
        assert!(ue.is_function_definition());
        assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn lhs_3() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("x"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(*ue, UpdateExpression::LeftHandSideExpression(_)));
        format!("{:?}", ue);
        pretty_check(&*ue, "UpdateExpression: x", vec!["LeftHandSideExpression: x"]);
        concise_check(&*ue, "IdentifierName: x", vec![]);
        assert!(!ue.is_function_definition());
        assert_eq!(ue.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn preinc() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("++a"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UpdateExpression::PreIncrement(_)));
        format!("{:?}", ue);
        pretty_check(&*ue, "UpdateExpression: ++ a", vec!["UnaryExpression: a"]);
        concise_check(&*ue, "UpdateExpression: ++ a", vec!["Punctuator: ++", "IdentifierName: a"]);
        assert!(!ue.is_function_definition());
        assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn predec() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("--a"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UpdateExpression::PreDecrement(_)));
        format!("{:?}", ue);
        pretty_check(&*ue, "UpdateExpression: -- a", vec!["UnaryExpression: a"]);
        concise_check(&*ue, "UpdateExpression: -- a", vec!["Punctuator: --", "IdentifierName: a"]);
        assert!(!ue.is_function_definition());
        assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn postinc() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("a++"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UpdateExpression::PostIncrement(_)));
        format!("{:?}", ue);
        pretty_check(&*ue, "UpdateExpression: a ++", vec!["LeftHandSideExpression: a"]);
        concise_check(&*ue, "UpdateExpression: a ++", vec!["IdentifierName: a", "Punctuator: ++"]);
        assert!(!ue.is_function_definition());
        assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn postdec() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("a--"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UpdateExpression::PostDecrement(_)));
        format!("{:?}", ue);
        pretty_check(&*ue, "UpdateExpression: a --", vec!["LeftHandSideExpression: a"]);
        concise_check(&*ue, "UpdateExpression: a --", vec!["IdentifierName: a", "Punctuator: --"]);
        assert!(!ue.is_function_definition());
        assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn newline() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("a\n++"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(*ue, UpdateExpression::LeftHandSideExpression(_)));
    }
    #[test]
    fn nomatch() {
        check_err(UpdateExpression::parse(&mut newparser("**"), Scanner::new(), false, false), "UpdateExpression expected", 1, 1);
    }
    #[test]
    fn syntax_error_01() {
        check_err(UpdateExpression::parse(&mut newparser("++ ++"), Scanner::new(), false, false), "UnaryExpression expected", 1, 6);
    }
    #[test]
    fn syntax_error_02() {
        check_err(UpdateExpression::parse(&mut newparser("-- ++"), Scanner::new(), false, false), "UnaryExpression expected", 1, 6);
    }
    #[test]
    fn update_expression_prettycheck_1() {
        let (item, _) = UpdateExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn update_expression_prettycheck_2() {
        let (item, _) = UpdateExpression::parse(&mut newparser("a++"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn update_expression_prettycheck_3() {
        let (item, _) = UpdateExpression::parse(&mut newparser("a--"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn update_expression_prettycheck_4() {
        let (item, _) = UpdateExpression::parse(&mut newparser("++a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn update_expression_prettycheck_5() {
        let (item, _) = UpdateExpression::parse(&mut newparser("--a"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn update_expression_concisecheck_1() {
        let (item, _) = UpdateExpression::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn update_expression_concisecheck_2() {
        let (item, _) = UpdateExpression::parse(&mut newparser("a++"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn update_expression_concisecheck_3() {
        let (item, _) = UpdateExpression::parse(&mut newparser("a--"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn update_expression_concisecheck_4() {
        let (item, _) = UpdateExpression::parse(&mut newparser("++a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn update_expression_concisecheck_5() {
        let (item, _) = UpdateExpression::parse(&mut newparser("--a"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) = UpdateExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_02() {
        let (item, _) = UpdateExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_03() {
        let (item, _) = UpdateExpression::parse(&mut newparser("this++"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_04() {
        let (item, _) = UpdateExpression::parse(&mut newparser("0++"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_05() {
        let (item, _) = UpdateExpression::parse(&mut newparser("this--"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_06() {
        let (item, _) = UpdateExpression::parse(&mut newparser("0--"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_07() {
        let (item, _) = UpdateExpression::parse(&mut newparser("++this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_08() {
        let (item, _) = UpdateExpression::parse(&mut newparser("++0"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test]
    fn contains_09() {
        let (item, _) = UpdateExpression::parse(&mut newparser("--this"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), true);
    }
    #[test]
    fn contains_10() {
        let (item, _) = UpdateExpression::parse(&mut newparser("--0"), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::This), false);
    }
    #[test_case("\"string\"" => Some(String::from("string")); "String Token")]
    #[test_case("--a" => None; "Not token")]
    fn as_string_literal(src: &str) -> Option<String> {
        let (item, _) = UpdateExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.as_string_literal().map(|st| String::from(st.value))
    }
    #[test_case("item.#valid" => true; "FallThru valid")]
    #[test_case("item.#valid++" => true; "PostInc valid")]
    #[test_case("item.#valid--" => true; "PostDec valid")]
    #[test_case("++item.#valid" => true; "PreInc valid")]
    #[test_case("--item.#valid" => true; "PreDec valid")]
    #[test_case("item.#invalid" => false; "FallThru invalid")]
    #[test_case("item.#invalid++" => false; "PostInc invalid")]
    #[test_case("item.#invalid--" => false; "PostDec invalid")]
    #[test_case("++item.#invalid" => false; "PreInc invalid")]
    #[test_case("--item.#invalid" => false; "PreDec invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = UpdateExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "fall-thru")]
    #[test_case("package++", true => set(&[PACKAGE_NOT_ALLOWED]); "post-inc, simple")]
    #[test_case("a(b)++", true => set(&["Invalid target for update"]); "post-inc, complex")]
    #[test_case("package--", true => set(&[PACKAGE_NOT_ALLOWED]); "post-dec, simple")]
    #[test_case("a(b)--", true => set(&["Invalid target for update"]); "post-dec, complex")]
    #[test_case("++package", true => set(&[PACKAGE_NOT_ALLOWED]); "pre-inc, simple")]
    #[test_case("++a(b)", true => set(&["Invalid target for update"]); "pre-inc, complex")]
    #[test_case("--package", true => set(&[PACKAGE_NOT_ALLOWED]); "pre-dec, simple")]
    #[test_case("--a(b)", true => set(&["Invalid target for update"]); "pre-dec, complex")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        UpdateExpression::parse(&mut newparser(src), Scanner::new(), false, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("a++" => true; "postinc")]
    #[test_case("a--" => true; "postdec")]
    #[test_case("++a" => true; "preinc")]
    #[test_case("--a" => true; "predec")]
    fn is_strictly_deletable(src: &str) -> bool {
        UpdateExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.is_strictly_deletable()
    }

    #[test_case("arguments" => true; "Exp (yes)")]
    #[test_case("arguments++" => true; "PostInc (yes)")]
    #[test_case("arguments--" => true; "PostDec (yes)")]
    #[test_case("++arguments" => true; "PreInc (yes)")]
    #[test_case("--arguments" => true; "PreDec (yes)")]
    #[test_case("xyzzy" => false; "Exp (no)")]
    #[test_case("xyzzy++" => false; "PostInc (no)")]
    #[test_case("xyzzy--" => false; "PostDec (no)")]
    #[test_case("++xyzzy" => false; "PreInc (no)")]
    #[test_case("--xyzzy" => false; "PreDec (no)")]
    fn contains_arguments(src: &str) -> bool {
        UpdateExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.contains_arguments()
    }
}
