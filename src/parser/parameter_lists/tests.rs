use super::testhelp::{check, check_err, chk_scan, newparser, set, strictparser, INTERFACE_NOT_ALLOWED, PACKAGE_NOT_ALLOWED};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::{test_agent, unwind_syntax_error_object};
use ahash::AHashSet;
use test_case::test_case;

const A_ALREADY_DEFINED: &str = "‘a’ already defined";

// UNIQUE FORMAL PARAMETERS
#[test]
fn unique_formal_parameters_test_01() {
    let (node, scanner) = UniqueFormalParameters::parse(&mut newparser("a"), Scanner::new(), false, false);
    chk_scan(&scanner, 1);
    pretty_check(&*node, "UniqueFormalParameters: a", vec!["FormalParameters: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
}
#[test]
fn unique_formal_parameters_test_prettyerrors_1() {
    let (item, _) = UniqueFormalParameters::parse(&mut newparser("a,b,c"), Scanner::new(), false, false);
    pretty_error_validate(&*item);
}
#[test]
fn unique_formal_parameters_test_conciseerrors_1() {
    let (item, _) = UniqueFormalParameters::parse(&mut newparser("a,b,c"), Scanner::new(), false, false);
    concise_error_validate(&*item);
}
#[test]
fn unique_formal_parameters_test_cache_01() {
    let mut parser = newparser("a,b,c,d,e");
    let (node, scanner) = UniqueFormalParameters::parse(&mut parser, Scanner::new(), false, false);
    let (node2, scanner2) = UniqueFormalParameters::parse(&mut parser, Scanner::new(), false, false);
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn unique_formal_parameters_test_contains_01() {
    let (item, _) = UniqueFormalParameters::parse(&mut newparser("a"), Scanner::new(), true, true);
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn unique_formal_parameters_test_contains_02() {
    let (item, _) = UniqueFormalParameters::parse(&mut newparser("a=0"), Scanner::new(), true, true);
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test_case("a=b.#valid" => true; "valid")]
#[test_case("a=b.#invalid" => false; "invalid")]
fn unique_formal_parameters_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = UniqueFormalParameters::parse(&mut newparser(src), Scanner::new(), true, true);
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod unique_formal_parameters {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "FormalParameters")]
    #[test_case("a,b,a", true => set(&[A_ALREADY_DEFINED]); "strict: duplicate ids")]
    #[test_case("a,b,a", false => set(&[A_ALREADY_DEFINED]); "non-strict: duplicate ids")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        UniqueFormalParameters::parse(&mut strictparser(src, strict), Scanner::new(), true, true).0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }

    #[test_case("a,b" => vec!["a", "b"]; "FormalParameters")]
    fn bound_names(src: &str) -> Vec<String> {
        UniqueFormalParameters::parse(&mut newparser(src), Scanner::new(), true, true).0.bound_names().into_iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("a" => true; "simple")]
    #[test_case("{a}" => false; "pattern")]
    fn is_simple_parameter_list(src: &str) -> bool {
        UniqueFormalParameters::parse(&mut newparser(src), Scanner::new(), true, true).0.is_simple_parameter_list()
    }
}

// FORMAL PARAMETERS
#[test]
fn formal_parameters_test_01() {
    let (node, scanner) = FormalParameters::parse(&mut newparser(""), Scanner::new(), false, false);
    chk_scan(&scanner, 0);
    pretty_check(&*node, "FormalParameters: ", vec![]);
    concise_check(&*node, "", vec![]);
    format!("{:?}", node);
}
#[test]
fn formal_parameters_test_02() {
    let (node, scanner) = FormalParameters::parse(&mut newparser("...a"), Scanner::new(), false, false);
    chk_scan(&scanner, 4);
    pretty_check(&*node, "FormalParameters: ... a", vec!["FunctionRestParameter: ... a"]);
    concise_check(&*node, "BindingRestElement: ... a", vec!["Punctuator: ...", "IdentifierName: a"]);
    format!("{:?}", node);
}
#[test]
fn formal_parameters_test_03() {
    let (node, scanner) = FormalParameters::parse(&mut newparser("a"), Scanner::new(), false, false);
    chk_scan(&scanner, 1);
    pretty_check(&*node, "FormalParameters: a", vec!["FormalParameterList: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
}
#[test]
fn formal_parameters_test_04() {
    let (node, scanner) = FormalParameters::parse(&mut newparser("a,"), Scanner::new(), false, false);
    chk_scan(&scanner, 2);
    pretty_check(&*node, "FormalParameters: a ,", vec!["FormalParameterList: a"]);
    concise_check(&*node, "FormalParameters: a ,", vec!["IdentifierName: a", "Punctuator: ,"]);
    format!("{:?}", node);
}
#[test]
fn formal_parameters_test_05() {
    let (node, scanner) = FormalParameters::parse(&mut newparser("a,...a"), Scanner::new(), false, false);
    chk_scan(&scanner, 6);
    pretty_check(&*node, "FormalParameters: a , ... a", vec!["FormalParameterList: a", "FunctionRestParameter: ... a"]);
    concise_check(&*node, "FormalParameters: a , ... a", vec!["IdentifierName: a", "Punctuator: ,", "BindingRestElement: ... a"]);
    format!("{:?}", node);
}
#[test]
fn formal_parameters_test_prettyerrors_1() {
    let (item, _) = FormalParameters::parse(&mut newparser(""), Scanner::new(), false, false);
    pretty_error_validate(&*item);
}
#[test]
fn formal_parameters_test_prettyerrors_2() {
    let (item, _) = FormalParameters::parse(&mut newparser("...object"), Scanner::new(), false, false);
    pretty_error_validate(&*item);
}
#[test]
fn formal_parameters_test_prettyerrors_3() {
    let (item, _) = FormalParameters::parse(&mut newparser("blue, green, twelve"), Scanner::new(), false, false);
    pretty_error_validate(&*item);
}
#[test]
fn formal_parameters_test_prettyerrors_4() {
    let (item, _) = FormalParameters::parse(&mut newparser("ad, game, result, "), Scanner::new(), false, false);
    pretty_error_validate(&*item);
}
#[test]
fn formal_parameters_test_prettyerrors_5() {
    let (item, _) = FormalParameters::parse(&mut newparser("apple, banana, grape, artichoke, ... basket"), Scanner::new(), false, false);
    pretty_error_validate(&*item);
}
#[test]
fn formal_parameters_test_conciseerrors_1() {
    let (item, _) = FormalParameters::parse(&mut newparser(""), Scanner::new(), false, false);
    concise_error_validate(&*item);
}
#[test]
fn formal_parameters_test_conciseerrors_2() {
    let (item, _) = FormalParameters::parse(&mut newparser("...object"), Scanner::new(), false, false);
    concise_error_validate(&*item);
}
#[test]
fn formal_parameters_test_conciseerrors_3() {
    let (item, _) = FormalParameters::parse(&mut newparser("blue, green, twelve"), Scanner::new(), false, false);
    concise_error_validate(&*item);
}
#[test]
fn formal_parameters_test_conciseerrors_4() {
    let (item, _) = FormalParameters::parse(&mut newparser("ad, game, result, "), Scanner::new(), false, false);
    concise_error_validate(&*item);
}
#[test]
fn formal_parameters_test_conciseerrors_5() {
    let (item, _) = FormalParameters::parse(&mut newparser("apple, banana, grape, artichoke, ... basket"), Scanner::new(), false, false);
    concise_error_validate(&*item);
}
#[test]
fn formal_parameters_test_cache_01() {
    let mut parser = newparser("a, b, c, d, e");
    let (node, scanner) = FormalParameters::parse(&mut parser, Scanner::new(), false, false);
    let (node2, scanner2) = FormalParameters::parse(&mut parser, Scanner::new(), false, false);
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn formal_parameters_test_contains_01() {
    let (item, _) = FormalParameters::parse(&mut newparser(""), Scanner::new(), true, true);
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn formal_parameters_test_contains_02() {
    let (item, _) = FormalParameters::parse(&mut newparser("...[a=0]"), Scanner::new(), true, true);
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn formal_parameters_test_contains_03() {
    let (item, _) = FormalParameters::parse(&mut newparser("...a"), Scanner::new(), true, true);
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn formal_parameters_test_contains_04() {
    let (item, _) = FormalParameters::parse(&mut newparser("a=0"), Scanner::new(), true, true);
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn formal_parameters_test_contains_05() {
    let (item, _) = FormalParameters::parse(&mut newparser("a"), Scanner::new(), true, true);
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn formal_parameters_test_contains_06() {
    let (item, _) = FormalParameters::parse(&mut newparser("a=0,"), Scanner::new(), true, true);
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn formal_parameters_test_contains_07() {
    let (item, _) = FormalParameters::parse(&mut newparser("a,"), Scanner::new(), true, true);
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn formal_parameters_test_contains_08() {
    let (item, _) = FormalParameters::parse(&mut newparser("a=0,...b"), Scanner::new(), true, true);
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn formal_parameters_test_contains_09() {
    let (item, _) = FormalParameters::parse(&mut newparser("a,...[b=0]"), Scanner::new(), true, true);
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn formal_parameters_test_contains_10() {
    let (item, _) = FormalParameters::parse(&mut newparser("a,...b"), Scanner::new(), true, true);
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("" => true; "empty")]
#[test_case("...{a=b.#valid}" => true; "FunctionRestParameter only valid")]
#[test_case("a=b.#valid" => true; "fpl only valid")]
#[test_case("a=b.#valid," => true; "fpl comma valid")]
#[test_case("a=b.#valid,...c" => true; "fpl-frp: fpl valid")]
#[test_case("a,...{b=c.#valid}" => true; "fpl-frp: frp valid")]
#[test_case("...{a=b.#invalid}" => false; "FunctionRestParameter only invalid")]
#[test_case("a=b.#invalid" => false; "fpl only invalid")]
#[test_case("a=b.#invalid," => false; "fpl comma invalid")]
#[test_case("a=b.#invalid,...c" => false; "fpl-frp: fpl invalid")]
#[test_case("a,...{b=c.#invalid}" => false; "fpl-frp: frp invalid")]
fn formal_parameters_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = FormalParameters::parse(&mut newparser(src), Scanner::new(), true, true);
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
#[test_case("" => true; "empty")]
#[test_case("...a" => false; "rest only")]
#[test_case("a,...b" => false; "list rest")]
#[test_case("a" => true; "list simple")]
#[test_case("a=0" => false; "list complex")]
#[test_case("a," => true; "list comma simple")]
#[test_case("a=0," => false; "list comma complex")]
fn formal_parameters_test_is_simple_parameter_list(src: &str) -> bool {
    let (item, _) = FormalParameters::parse(&mut newparser(src), Scanner::new(), true, true);
    item.is_simple_parameter_list()
}
#[test_case("" => Vec::<JSString>::new(); "empty")]
#[test_case("a,...b" => vec![JSString::from("a"), JSString::from("b")]; "list-rest")]
#[test_case("...a" => vec![JSString::from("a")]; "rest-only")]
#[test_case("a" => vec![JSString::from("a")]; "list")]
#[test_case("a," => vec![JSString::from("a")]; "list-comma")]
fn formal_parameters_test_bound_names(src: &str) -> Vec<JSString> {
    let (item, _) = FormalParameters::parse(&mut newparser(src), Scanner::new(), true, true);
    item.bound_names()
}
mod formal_parameters {
    use super::*;
    use test_case::test_case;

    #[test_case("", true, false => set(&[]); "empty")]
    #[test_case("...package", true, false => set(&[PACKAGE_NOT_ALLOWED]); "FunctionRestParameter")]
    #[test_case("package", true, false => set(&[PACKAGE_NOT_ALLOWED]); "FormalParameterList")]
    #[test_case("package,", true, false => set(&[PACKAGE_NOT_ALLOWED]); "FormalParameterList , (trailing)")]
    #[test_case("package,...interface", true, false => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "FormalParameterList , FunctionRestParameter")]
    #[test_case("a,a", true, false => set(&[A_ALREADY_DEFINED]); "strict; duplicates")]
    #[test_case("a,a", false, false => set(&[]); "non-strict; duplicates")]
    #[test_case("a,a", true, true => set(&[]); "strict; duplicates; already reported")]
    #[test_case("a,...a", false, false => set(&[A_ALREADY_DEFINED]); "not-simple")]
    fn early_errors(src: &str, strict: bool, dups_already_checked: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        FormalParameters::parse(&mut strictparser(src, strict), Scanner::new(), true, true).0.early_errors(&mut agent, &mut errs, strict, dups_already_checked);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// FORMAL PARAMETER LIST
#[test]
fn formal_parameter_list_test_01() {
    let (node, scanner) = check(FormalParameterList::parse(&mut newparser("formal"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    pretty_check(&*node, "FormalParameterList: formal", vec!["FormalParameter: formal"]);
    concise_check(&*node, "IdentifierName: formal", vec![]);
    format!("{:?}", node);
}
#[test]
fn formal_parameter_list_test_02() {
    let (node, scanner) = check(FormalParameterList::parse(&mut newparser("formal, playful"), Scanner::new(), false, false));
    chk_scan(&scanner, 15);
    pretty_check(&*node, "FormalParameterList: formal , playful", vec!["FormalParameterList: formal", "FormalParameter: playful"]);
    concise_check(&*node, "FormalParameterList: formal , playful", vec!["IdentifierName: formal", "Punctuator: ,", "IdentifierName: playful"]);
    format!("{:?}", node);
}
#[test]
fn formal_parameter_list_test_err_01() {
    check_err(FormalParameterList::parse(&mut newparser(""), Scanner::new(), false, false), "BindingElement expected", 1, 1);
}
#[test]
fn formal_parameter_list_test_prettyerrors_1() {
    let (item, _) = FormalParameterList::parse(&mut newparser("alligator"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn formal_parameter_list_test_prettyerrors_2() {
    let (item, _) = FormalParameterList::parse(&mut newparser("bacon, lettuce, tomato"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn formal_parameter_list_test_conciseerrors_1() {
    let (item, _) = FormalParameterList::parse(&mut newparser("alligator"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn formal_parameter_list_test_conciseerrors_2() {
    let (item, _) = FormalParameterList::parse(&mut newparser("bacon, lettuce, tomato"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn formal_parameter_list_test_contains_01() {
    let (item, _) = FormalParameterList::parse(&mut newparser("a=0"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn formal_parameter_list_test_contains_02() {
    let (item, _) = FormalParameterList::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn formal_parameter_list_test_contains_03() {
    let (item, _) = FormalParameterList::parse(&mut newparser("a=0,b"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn formal_parameter_list_test_contains_04() {
    let (item, _) = FormalParameterList::parse(&mut newparser("a,b=0"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn formal_parameter_list_test_contains_05() {
    let (item, _) = FormalParameterList::parse(&mut newparser("a,b"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("a=b.#valid" => true; "item valid")]
#[test_case("a=b.#valid,c" => true; "list first valid")]
#[test_case("a,b=c.#valid" => true; "list tail valid")]
#[test_case("a=b.#invalid" => false; "item invalid")]
#[test_case("a=b.#invalid,c" => false; "list first invalid")]
#[test_case("a,b=c.#invalid" => false; "list tail invalid")]
fn formal_parameter_list_test_all_parameters_valid(src: &str) -> bool {
    let (item, _) = FormalParameterList::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
#[test_case("a" => true; "item simple")]
#[test_case("a=0" => false; "item complex")]
#[test_case("a,b" => true; "list simple")]
#[test_case("a=0,b" => false; "list left complex")]
#[test_case("a,b=0" => false; "list right complex")]
fn formal_parameter_list_test_is_simple_parameter_list(src: &str) -> bool {
    let (item, _) = FormalParameterList::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.is_simple_parameter_list()
}
#[test_case("a" => vec![JSString::from("a")]; "item")]
#[test_case("a,b" => vec![JSString::from("a"), JSString::from("b")]; "list")]
fn formal_parameter_list_test_bound_names(src: &str) -> Vec<JSString> {
    let (item, _) = FormalParameterList::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.bound_names()
}
mod formal_parameter_list {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "FormalParameter")]
    #[test_case("package,interface", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "FormalParameterList , FormalParameter")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        FormalParameterList::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// FUNCTION REST PARAMETER
#[test]
fn function_rest_parameter_test_01() {
    let (node, scanner) = check(FunctionRestParameter::parse(&mut newparser("...formal"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "FunctionRestParameter: ... formal", vec!["BindingRestElement: ... formal"]);
    concise_check(&*node, "BindingRestElement: ... formal", vec!["Punctuator: ...", "IdentifierName: formal"]);
    format!("{:?}", node);
}
#[test]
fn function_rest_parameter_test_err_01() {
    check_err(FunctionRestParameter::parse(&mut newparser(""), Scanner::new(), false, false), "‘...’ expected", 1, 1);
}
#[test]
fn function_rest_parameter_test_prettyerrors_01() {
    let (item, _) = FunctionRestParameter::parse(&mut newparser("...dippin_dots"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn function_rest_parameter_test_conciseerrors_01() {
    let (item, _) = FunctionRestParameter::parse(&mut newparser("...dippin_dots"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn function_rest_parameter_test_contains_01() {
    let (item, _) = FunctionRestParameter::parse(&mut newparser("...[a=0]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn function_rest_parameter_test_contains_02() {
    let (item, _) = FunctionRestParameter::parse(&mut newparser("...a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("...{a=b.#valid}" => true; "valid")]
#[test_case("...{a=b.#invalid}" => false; "invalid")]
fn function_rest_parameter_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = FunctionRestParameter::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
#[test_case("...a" => vec![JSString::from("a")]; "simple")]
fn function_rest_parameter_test_bound_names(src: &str) -> Vec<JSString> {
    let (item, _) = FunctionRestParameter::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.bound_names()
}
mod function_rest_parameter {
    use super::*;
    use test_case::test_case;

    #[test_case("...package", true => set(&[PACKAGE_NOT_ALLOWED]); "BindingRestElement")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        FunctionRestParameter::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// FORMAL PARAMETER
#[test]
fn formal_parameter_test_01() {
    let (node, scanner) = check(FormalParameter::parse(&mut newparser("formal"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    pretty_check(&*node, "FormalParameter: formal", vec!["BindingElement: formal"]);
    concise_check(&*node, "IdentifierName: formal", vec![]);
    format!("{:?}", node);
}
#[test]
fn formal_parameter_test_err_01() {
    check_err(FormalParameter::parse(&mut newparser(""), Scanner::new(), false, false), "BindingElement expected", 1, 1);
}
#[test]
fn formal_parameter_test_prettyerrors_01() {
    let (item, _) = FormalParameter::parse(&mut newparser("formal_parameter"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn formal_parameter_test_conciseerrors_01() {
    let (item, _) = FormalParameter::parse(&mut newparser("formal_parameter"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn formal_parameter_test_cache_01() {
    let mut parser = newparser("a");
    let (node, scanner) = check(FormalParameter::parse(&mut parser, Scanner::new(), false, false));
    let (node2, scanner2) = check(FormalParameter::parse(&mut parser, Scanner::new(), false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn formal_parameter_test_contains_01() {
    let (item, _) = FormalParameter::parse(&mut newparser("a=0"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn formal_parameter_test_contains_02() {
    let (item, _) = FormalParameter::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("a=b.#valid" => true; "valid")]
#[test_case("a=b.#invalid" => false; "invalid")]
fn formal_parameter_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = FormalParameter::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
#[test_case("a" => true; "simple")]
#[test_case("[a]" => false; "complex")]
fn formal_parameter_test_is_simple_parameter_list(src: &str) -> bool {
    let (item, _) = FormalParameter::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.is_simple_parameter_list()
}
#[test_case("a" => vec![JSString::from("a")]; "simple")]
fn formal_parameter_test_bound_names(src: &str) -> Vec<JSString> {
    let (item, _) = FormalParameter::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.bound_names()
}
mod formal_parameter {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "BindingElement")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        FormalParameter::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}
