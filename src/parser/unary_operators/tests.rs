use super::testhelp::{check, check_err, chk_scan, newparser, set, PACKAGE_NOT_ALLOWED};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::{test_agent, unwind_syntax_error_object};
use ahash::AHashSet;
use test_case::test_case;

// UNARY EXPRESSION
#[test]
fn unary_expression_test_update_expression() {
    let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("900"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    assert!(matches!(*ue, UnaryExpression::UpdateExpression(_)));
    pretty_check(&*ue, "UnaryExpression: 900", vec!["UpdateExpression: 900"]);
    concise_check(&*ue, "Numeric: 900", vec![]);
    assert!(!ue.is_function_definition());
    assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
    format!("{:?}", ue);
}
#[test]
fn unary_expression_test_delete() {
    let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("delete bob"), Scanner::new(), false, false));
    chk_scan(&scanner, 10);
    assert!(matches!(*ue, UnaryExpression::Delete(_)));
    pretty_check(&*ue, "UnaryExpression: delete bob", vec!["UnaryExpression: bob"]);
    concise_check(&*ue, "UnaryExpression: delete bob", vec!["Keyword: delete", "IdentifierName: bob"]);
    assert!(!ue.is_function_definition());
    assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
    format!("{:?}", ue);
}
#[test]
fn unary_expression_test_void() {
    let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("void bob"), Scanner::new(), false, false));
    chk_scan(&scanner, 8);
    assert!(matches!(*ue, UnaryExpression::Void(_)));
    pretty_check(&*ue, "UnaryExpression: void bob", vec!["UnaryExpression: bob"]);
    concise_check(&*ue, "UnaryExpression: void bob", vec!["Keyword: void", "IdentifierName: bob"]);
    assert!(!ue.is_function_definition());
    assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
    format!("{:?}", ue);
}
#[test]
fn unary_expression_test_typeof() {
    let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("typeof bob"), Scanner::new(), false, false));
    chk_scan(&scanner, 10);
    assert!(matches!(*ue, UnaryExpression::Typeof(_)));
    pretty_check(&*ue, "UnaryExpression: typeof bob", vec!["UnaryExpression: bob"]);
    concise_check(&*ue, "UnaryExpression: typeof bob", vec!["Keyword: typeof", "IdentifierName: bob"]);
    assert!(!ue.is_function_definition());
    assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
    format!("{:?}", ue);
}
#[test]
fn unary_expression_test_numberify() {
    let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("+bob"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(*ue, UnaryExpression::NoOp(_)));
    pretty_check(&*ue, "UnaryExpression: + bob", vec!["UnaryExpression: bob"]);
    concise_check(&*ue, "UnaryExpression: + bob", vec!["Punctuator: +", "IdentifierName: bob"]);
    assert!(!ue.is_function_definition());
    assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
    format!("{:?}", ue);
}
#[test]
fn unary_expression_test_negate() {
    let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("-bob"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(*ue, UnaryExpression::Negate(_)));
    pretty_check(&*ue, "UnaryExpression: - bob", vec!["UnaryExpression: bob"]);
    concise_check(&*ue, "UnaryExpression: - bob", vec!["Punctuator: -", "IdentifierName: bob"]);
    assert!(!ue.is_function_definition());
    assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
    format!("{:?}", ue);
}
#[test]
fn unary_expression_test_complement() {
    let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("~bob"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(*ue, UnaryExpression::Complement(_)));
    pretty_check(&*ue, "UnaryExpression: ~ bob", vec!["UnaryExpression: bob"]);
    concise_check(&*ue, "UnaryExpression: ~ bob", vec!["Punctuator: ~", "IdentifierName: bob"]);
    assert!(!ue.is_function_definition());
    assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
    format!("{:?}", ue);
}
#[test]
fn unary_expression_test_not() {
    let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("!bob"), Scanner::new(), false, false));
    chk_scan(&scanner, 4);
    assert!(matches!(*ue, UnaryExpression::Not(_)));
    pretty_check(&*ue, "UnaryExpression: ! bob", vec!["UnaryExpression: bob"]);
    concise_check(&*ue, "UnaryExpression: ! bob", vec!["Punctuator: !", "IdentifierName: bob"]);
    assert!(!ue.is_function_definition());
    assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
    format!("{:?}", ue);
}
#[test]
fn unary_expression_test_await() {
    let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("await bob"), Scanner::new(), false, true));
    chk_scan(&scanner, 9);
    assert!(matches!(*ue, UnaryExpression::Await(_)));
    pretty_check(&*ue, "UnaryExpression: await bob", vec!["AwaitExpression: await bob"]);
    concise_check(&*ue, "AwaitExpression: await bob", vec!["Keyword: await", "IdentifierName: bob"]);
    assert!(!ue.is_function_definition());
    assert_eq!(ue.assignment_target_type(), ATTKind::Invalid);
    format!("{:?}", ue);
}
#[test]
fn unary_expression_test_nomatch() {
    check_err(UnaryExpression::parse(&mut newparser(""), Scanner::new(), false, false), "UnaryExpression expected", 1, 1);
}
#[test]
fn unary_expression_test_incomplete() {
    check_err(UnaryExpression::parse(&mut newparser("delete"), Scanner::new(), false, false), "UnaryExpression expected", 1, 7);
    check_err(UnaryExpression::parse(&mut newparser("void"), Scanner::new(), false, false), "UnaryExpression expected", 1, 5);
    check_err(UnaryExpression::parse(&mut newparser("typeof"), Scanner::new(), false, false), "UnaryExpression expected", 1, 7);
    check_err(UnaryExpression::parse(&mut newparser("+"), Scanner::new(), false, false), "UnaryExpression expected", 1, 2);
    check_err(UnaryExpression::parse(&mut newparser("-"), Scanner::new(), false, false), "UnaryExpression expected", 1, 2);
    check_err(UnaryExpression::parse(&mut newparser("~"), Scanner::new(), false, false), "UnaryExpression expected", 1, 2);
    check_err(UnaryExpression::parse(&mut newparser("!"), Scanner::new(), false, false), "UnaryExpression expected", 1, 2);
    check_err(UnaryExpression::parse(&mut newparser("await"), Scanner::new(), false, true), "UnaryExpression expected", 1, 6);
}

#[test]
fn unary_expression_test_prettyerrors_1() {
    let (item, _) = UnaryExpression::parse(&mut newparser("delete a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn unary_expression_test_prettyerrors_2() {
    let (item, _) = UnaryExpression::parse(&mut newparser("void a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn unary_expression_test_prettyerrors_3() {
    let (item, _) = UnaryExpression::parse(&mut newparser("typeof a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn unary_expression_test_prettyerrors_4() {
    let (item, _) = UnaryExpression::parse(&mut newparser("+ a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn unary_expression_test_prettyerrors_5() {
    let (item, _) = UnaryExpression::parse(&mut newparser("- a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn unary_expression_test_prettyerrors_6() {
    let (item, _) = UnaryExpression::parse(&mut newparser("~ a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn unary_expression_test_prettyerrors_7() {
    let (item, _) = UnaryExpression::parse(&mut newparser("! a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn unary_expression_test_prettyerrors_8() {
    let (item, _) = UnaryExpression::parse(&mut newparser("await a"), Scanner::new(), false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn unary_expression_test_conciseerrors_1() {
    let (item, _) = UnaryExpression::parse(&mut newparser("delete a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn unary_expression_test_conciseerrors_2() {
    let (item, _) = UnaryExpression::parse(&mut newparser("void a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn unary_expression_test_conciseerrors_3() {
    let (item, _) = UnaryExpression::parse(&mut newparser("typeof a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn unary_expression_test_conciseerrors_4() {
    let (item, _) = UnaryExpression::parse(&mut newparser("+ a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn unary_expression_test_conciseerrors_5() {
    let (item, _) = UnaryExpression::parse(&mut newparser("- a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn unary_expression_test_conciseerrors_6() {
    let (item, _) = UnaryExpression::parse(&mut newparser("~ a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn unary_expression_test_conciseerrors_7() {
    let (item, _) = UnaryExpression::parse(&mut newparser("! a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn unary_expression_test_conciseerrors_8() {
    let (item, _) = UnaryExpression::parse(&mut newparser("await a"), Scanner::new(), false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn unary_expression_test_cache_01() {
    let mut parser = newparser("void blue");
    let (node, scanner) = check(UnaryExpression::parse(&mut parser, Scanner::new(), false, false));
    let (node2, scanner2) = check(UnaryExpression::parse(&mut parser, Scanner::new(), false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn unary_expression_test_contains_01() {
    let (item, _) = UnaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
}
#[test]
fn unary_expression_test_contains_02() {
    let (item, _) = UnaryExpression::parse(&mut newparser("0"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
}
#[test]
fn unary_expression_test_contains_03() {
    let (item, _) = UnaryExpression::parse(&mut newparser("delete this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
}
#[test]
fn unary_expression_test_contains_04() {
    let (item, _) = UnaryExpression::parse(&mut newparser("delete p"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
}
#[test]
fn unary_expression_test_contains_05() {
    let (item, _) = UnaryExpression::parse(&mut newparser("void this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
}
#[test]
fn unary_expression_test_contains_06() {
    let (item, _) = UnaryExpression::parse(&mut newparser("void p"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
}
#[test]
fn unary_expression_test_contains_07() {
    let (item, _) = UnaryExpression::parse(&mut newparser("typeof this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
}
#[test]
fn unary_expression_test_contains_08() {
    let (item, _) = UnaryExpression::parse(&mut newparser("typeof p"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
}
#[test]
fn unary_expression_test_contains_09() {
    let (item, _) = UnaryExpression::parse(&mut newparser("+ this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
}
#[test]
fn unary_expression_test_contains_10() {
    let (item, _) = UnaryExpression::parse(&mut newparser("+ p"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
}
#[test]
fn unary_expression_test_contains_11() {
    let (item, _) = UnaryExpression::parse(&mut newparser("- this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
}
#[test]
fn unary_expression_test_contains_12() {
    let (item, _) = UnaryExpression::parse(&mut newparser("- p"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
}
#[test]
fn unary_expression_test_contains_13() {
    let (item, _) = UnaryExpression::parse(&mut newparser("~ this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
}
#[test]
fn unary_expression_test_contains_14() {
    let (item, _) = UnaryExpression::parse(&mut newparser("~ p"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
}
#[test]
fn unary_expression_test_contains_15() {
    let (item, _) = UnaryExpression::parse(&mut newparser("! this"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
}
#[test]
fn unary_expression_test_contains_16() {
    let (item, _) = UnaryExpression::parse(&mut newparser("! p"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), false);
}
#[test]
fn unary_expression_test_contains_17() {
    let (item, _) = UnaryExpression::parse(&mut newparser("await this"), Scanner::new(), false, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), true);
}
#[test]
fn unary_expression_test_contains_18() {
    let (item, _) = UnaryExpression::parse(&mut newparser("await p"), Scanner::new(), false, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
    assert_eq!(item.contains(ParseNodeKind::AwaitExpression), true);
}
#[test_case("\"string\"" => Some(String::from("string")); "String Token")]
#[test_case("-a" => None; "Not token")]
fn unary_expression_test_as_string_literal(src: &str) -> Option<String> {
    let (item, _) = UnaryExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.as_string_literal().map(|st| String::from(st.value))
}
#[test_case("item.#valid" => true; "FallThru valid")]
#[test_case("delete item.#valid" => true; "Delete valid")]
#[test_case("void item.#valid" => true; "Void valid")]
#[test_case("typeof item.#valid" => true; "Typeof valid")]
#[test_case("+item.#valid" => true; "NoOp valid")]
#[test_case("-item.#valid" => true; "Negate valid")]
#[test_case("~item.#valid" => true; "Complement valid")]
#[test_case("!item.#valid" => true; "Not valid")]
#[test_case("await item.#valid" => true; "Await valid")]
#[test_case("item.#invalid" => false; "FallThru invalid")]
#[test_case("delete item.#invalid" => false; "Delete invalid")]
#[test_case("void item.#invalid" => false; "Void invalid")]
#[test_case("typeof item.#invalid" => false; "Typeof invalid")]
#[test_case("+item.#invalid" => false; "NoOp invalid")]
#[test_case("-item.#invalid" => false; "Negate invalid")]
#[test_case("~item.#invalid" => false; "Complement invalid")]
#[test_case("!item.#invalid" => false; "Not invalid")]
#[test_case("await item.#invalid" => false; "Await invalid")]
fn unary_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = UnaryExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod unary_expression {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "fall-thru")]
    #[test_case("delete package", true => set(&[PACKAGE_NOT_ALLOWED, "Item is not deletable"]); "delete")]
    #[test_case("delete (((foo)))", true => panics "not yet implemented"; "nested ref")]
    #[test_case("delete a", false => AHashSet::<String>::new(); "non-strict delete")]
    #[test_case("void package", true => set(&[PACKAGE_NOT_ALLOWED]); "void")]
    #[test_case("typeof package", true => set(&[PACKAGE_NOT_ALLOWED]); "typeof_")]
    #[test_case("+ package", true => set(&[PACKAGE_NOT_ALLOWED]); "noop")]
    #[test_case("- package", true => set(&[PACKAGE_NOT_ALLOWED]); "negate")]
    #[test_case("~ package", true => set(&[PACKAGE_NOT_ALLOWED]); "complement")]
    #[test_case("! package", true => set(&[PACKAGE_NOT_ALLOWED]); "not")]
    #[test_case("await package", true => panics "not yet implemented"; "await_")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        UnaryExpression::parse(&mut newparser(src), Scanner::new(), false, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }

    #[test_case("a" => false; "identifier ref")]
    #[test_case("1" => true; "literal")]
    #[test_case("delete x" => true; "delete")]
    #[test_case("void x" => true; "void")]
    #[test_case("typeof x" => true; "typeof op")]
    #[test_case("+a" => true; "unary plus")]
    #[test_case("-a" => true; "unary minus")]
    #[test_case("~a" => true; "complement")]
    #[test_case("!a" => true; "not")]
    #[test_case("await a" => true; "await op")]
    fn is_strictly_deletable(src: &str) -> bool {
        UnaryExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.is_strictly_deletable()
    }
}
