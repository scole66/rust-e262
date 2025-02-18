#![expect(clippy::bool_assert_comparison)]
use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

const A_ALREADY_DEFINED: &str = "‘a’ already defined";

// UNIQUE FORMAL PARAMETERS
#[test]
fn unique_formal_parameters_test_01() {
    let (node, scanner) = UniqueFormalParameters::parse(&mut newparser("a"), Scanner::new(), false, false);
    chk_scan(&scanner, 1);
    pretty_check(&*node, "UniqueFormalParameters: a", &["FormalParameters: a"]);
    concise_check(&*node, "IdentifierName: a", &[]);
    assert_ne!(format!("{node:?}"), "");
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

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "FormalParameters")]
    #[test_case("a,b,a", true => sset(&[A_ALREADY_DEFINED]); "strict: duplicate ids")]
    #[test_case("a,b,a", false => sset(&[A_ALREADY_DEFINED]); "non-strict: duplicate ids")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).unique_formal_parameters().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("a,b" => vec!["a", "b"]; "FormalParameters")]
    fn bound_names(src: &str) -> Vec<String> {
        Maker::new(src).unique_formal_parameters().bound_names().into_iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("a" => true; "simple")]
    #[test_case("{a}" => false; "pattern")]
    fn is_simple_parameter_list(src: &str) -> bool {
        Maker::new(src).unique_formal_parameters().is_simple_parameter_list()
    }

    #[test_case("{a=arguments}" => true; "yes")]
    #[test_case("a" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).unique_formal_parameters().contains_arguments()
    }

    #[test_case("  a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 1 }}; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).unique_formal_parameters().location()
    }

    #[test_case("a,b,c=1,d" => 2.0; "typical")]
    fn unique_formal_parameters(src: &str) -> f64 {
        Maker::new(src).unique_formal_parameters().expected_argument_count()
    }

    #[test_case("a,b,c,d" => false; "no exprs")]
    #[test_case("a,b=99,c,d" => true; "with exprs")]
    fn contains_expressions(src: &str) -> bool {
        Maker::new(src).unique_formal_parameters().contains_expression()
    }
}

// FORMAL PARAMETERS
#[test]
fn formal_parameters_test_01() {
    let (node, scanner) = FormalParameters::parse(&mut newparser(""), Scanner::new(), false, false);
    chk_scan(&scanner, 0);
    pretty_check(&*node, "FormalParameters: ", &[]);
    concise_check(&*node, "", &[]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn formal_parameters_test_02() {
    let (node, scanner) = FormalParameters::parse(&mut newparser("...a"), Scanner::new(), false, false);
    chk_scan(&scanner, 4);
    pretty_check(&*node, "FormalParameters: ... a", &["FunctionRestParameter: ... a"]);
    concise_check(&*node, "BindingRestElement: ... a", &["Punctuator: ...", "IdentifierName: a"]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn formal_parameters_test_03() {
    let (node, scanner) = FormalParameters::parse(&mut newparser("a"), Scanner::new(), false, false);
    chk_scan(&scanner, 1);
    pretty_check(&*node, "FormalParameters: a", &["FormalParameterList: a"]);
    concise_check(&*node, "IdentifierName: a", &[]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn formal_parameters_test_04() {
    let (node, scanner) = FormalParameters::parse(&mut newparser("a,"), Scanner::new(), false, false);
    chk_scan(&scanner, 2);
    pretty_check(&*node, "FormalParameters: a ,", &["FormalParameterList: a"]);
    concise_check(&*node, "FormalParameters: a ,", &["IdentifierName: a", "Punctuator: ,"]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn formal_parameters_test_05() {
    let (node, scanner) = FormalParameters::parse(&mut newparser("a,...a"), Scanner::new(), false, false);
    chk_scan(&scanner, 6);
    pretty_check(&*node, "FormalParameters: a , ... a", &["FormalParameterList: a", "FunctionRestParameter: ... a"]);
    concise_check(
        &*node,
        "FormalParameters: a , ... a",
        &["IdentifierName: a", "Punctuator: ,", "BindingRestElement: ... a"],
    );
    assert_ne!(format!("{node:?}"), "");
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
    let (item, _) = FormalParameters::parse(
        &mut newparser("apple, banana, grape, artichoke, ... basket"),
        Scanner::new(),
        false,
        false,
    );
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
    let (item, _) = FormalParameters::parse(
        &mut newparser("apple, banana, grape, artichoke, ... basket"),
        Scanner::new(),
        false,
        false,
    );
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

    #[test_case("", true, false => sset(&[]); "empty")]
    #[test_case("...package", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "FunctionRestParameter")]
    #[test_case("package", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "FormalParameterList")]
    #[test_case("package,", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "FormalParameterList , (trailing)")]
    #[test_case("package,...interface", true, false => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "FormalParameterList , FunctionRestParameter")]
    #[test_case("a,a", true, false => sset(&[A_ALREADY_DEFINED]); "strict; duplicates")]
    #[test_case("a,a", false, false => sset(&[]); "non-strict; duplicates")]
    #[test_case("a,a", true, true => sset(&[]); "strict; duplicates; already reported")]
    #[test_case("a,...a", false, false => sset(&[A_ALREADY_DEFINED]); "not-simple")]
    fn early_errors(src: &str, strict: bool, dups_already_checked: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).formal_parameters().early_errors(&mut errs, strict, dups_already_checked);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("" => false; "empty")]
    #[test_case("...{a=arguments}" => true; "rest (yes)")]
    #[test_case("...a" => false; "rest (no)")]
    #[test_case("a=arguments" => true; "list-only (yes)")]
    #[test_case("a" => false; "list-only (no)")]
    #[test_case("a=arguments," => true; "list-comma (yes)")]
    #[test_case("a," => false; "list-comma (no)")]
    #[test_case("a=arguments,...b" => true; "list-rest (left)")]
    #[test_case("a,...{b=arguments}" => true; "list-rest (right)")]
    #[test_case("a,...b" => false; "list-rest (none)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).formal_parameters().contains_arguments()
    }

    #[test_case("  a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 1 }}; "list only")]
    #[test_case("  " => Location{ starting_line: 1, starting_column: 1, span: Span{ starting_index: 0, length: 0 }}; "empty")]
    #[test_case("  a," => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 2 }}; "list comma")]
    #[test_case("  ...a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 4 }}; "rest")]
    #[test_case("  a,...b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 6}}; "list rest")]
    fn location(src: &str) -> Location {
        Maker::new(src).formal_parameters().location()
    }

    #[test_case("" => 0.0; "empty")]
    #[test_case("...a" => 0.0; "rest only")]
    #[test_case("a,b,c=0" => 2.0; "list only")]
    #[test_case("a,b=0,c," => 1.0; "list-comma")]
    #[test_case("a,b,c,...d" => 3.0; "list/rest")]
    fn expected_argument_count(src: &str) -> f64 {
        Maker::new(src).formal_parameters().expected_argument_count()
    }

    #[test_case("" => false; "empty")]
    #[test_case("...x" => false; "rest-only; no exprs")]
    #[test_case("...{z=10}" => true; "rest-only; with exprs")]
    #[test_case("a,b" => false; "list-only; no exprs")]
    #[test_case("a=0,b" => true; "list-only; with expr")]
    #[test_case("a,b," => false; "list-comma; no exprs")]
    #[test_case("a=0,b," => true; "list-comma; with expr")]
    #[test_case("a,b,...c" => false; "list-rest; no expr")]
    #[test_case("a=0,b,...c" => true; "list-rest; expr in list")]
    #[test_case("a,b,...{c=0}" => true; "list-rest; expr in rest")]
    fn contains_expression(src: &str) -> bool {
        Maker::new(src).formal_parameters().contains_expression()
    }
}

// FORMAL PARAMETER LIST
#[test]
fn formal_parameter_list_test_01() {
    let (node, scanner) = check(FormalParameterList::parse(&mut newparser("formal"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    pretty_check(&*node, "FormalParameterList: formal", &["FormalParameter: formal"]);
    concise_check(&*node, "IdentifierName: formal", &[]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn formal_parameter_list_test_02() {
    let (node, scanner) =
        check(FormalParameterList::parse(&mut newparser("formal, playful"), Scanner::new(), false, false));
    chk_scan(&scanner, 15);
    pretty_check(
        &*node,
        "FormalParameterList: formal , playful",
        &["FormalParameterList: formal", "FormalParameter: playful"],
    );
    concise_check(
        &*node,
        "FormalParameterList: formal , playful",
        &["IdentifierName: formal", "Punctuator: ,", "IdentifierName: playful"],
    );
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn formal_parameter_list_test_err_01() {
    check_err(
        FormalParameterList::parse(&mut newparser(""), Scanner::new(), false, false),
        "BindingElement expected",
        1,
        1,
    );
}
#[test]
fn formal_parameter_list_test_prettyerrors_1() {
    let (item, _) = FormalParameterList::parse(&mut newparser("alligator"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn formal_parameter_list_test_prettyerrors_2() {
    let (item, _) =
        FormalParameterList::parse(&mut newparser("bacon, lettuce, tomato"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn formal_parameter_list_test_conciseerrors_1() {
    let (item, _) = FormalParameterList::parse(&mut newparser("alligator"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn formal_parameter_list_test_conciseerrors_2() {
    let (item, _) =
        FormalParameterList::parse(&mut newparser("bacon, lettuce, tomato"), Scanner::new(), false, false).unwrap();
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

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "FormalParameter")]
    #[test_case("package,interface", true => sset(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "FormalParameterList , FormalParameter")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).formal_parameter_list().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("a=arguments" => true; "Item (yes)")]
    #[test_case("a" => false; "Item (no)")]
    #[test_case("a=arguments,b" => true; "List (left)")]
    #[test_case("a,b=arguments" => true; "List (right)")]
    #[test_case("a,b" => false; "List (none)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).formal_parameter_list().contains_arguments()
    }

    #[test_case("  a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 1}}; "item")]
    #[test_case("  a,b" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 3}}; "list")]
    fn location(src: &str) -> Location {
        Maker::new(src).formal_parameter_list().location()
    }

    #[test_case("x" => false; "item, no init")]
    #[test_case("x=a" => true; "item, with init")]
    #[test_case("x,y" => false; "list, no init")]
    #[test_case("x=a,y" => true; "list, left init")]
    #[test_case("x,y=a" => true; "list, right init")]
    fn has_initializer(src: &str) -> bool {
        Maker::new(src).formal_parameter_list().has_initializer()
    }

    #[test_case("z,y,x" => 3.0; "list, no initializers")]
    #[test_case("x, y, z=1" => 2.0; "list, initializer in item")]
    #[test_case("x, y=3, z" => 1.0; "list, initializer in list")]
    #[test_case("x" => 1.0; "item, no init")]
    #[test_case("x=3" => 0.0; "item, with init")]
    fn expected_argument_count(src: &str) -> f64 {
        Maker::new(src).formal_parameter_list().expected_argument_count()
    }

    #[test_case("a=0" => true; "item with expr")]
    #[test_case("a" => false; "item without expr")]
    #[test_case("a,b" => false; "list+item without expr")]
    #[test_case("a=0,b" => true; "list+item expr in list")]
    #[test_case("a,b=0" => true; "list+item expr in item")]
    fn contains_expression(src: &str) -> bool {
        Maker::new(src).formal_parameter_list().contains_expression()
    }
}

// FUNCTION REST PARAMETER
#[test]
fn function_rest_parameter_test_01() {
    let (node, scanner) =
        check(FunctionRestParameter::parse(&mut newparser("...formal"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "FunctionRestParameter: ... formal", &["BindingRestElement: ... formal"]);
    concise_check(&*node, "BindingRestElement: ... formal", &["Punctuator: ...", "IdentifierName: formal"]);
    assert_ne!(format!("{node:?}"), "");
}
#[test]
fn function_rest_parameter_test_err_01() {
    check_err(FunctionRestParameter::parse(&mut newparser(""), Scanner::new(), false, false), "‘...’ expected", 1, 1);
}
#[test]
fn function_rest_parameter_test_prettyerrors_01() {
    let (item, _) =
        FunctionRestParameter::parse(&mut newparser("...dippin_dots"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn function_rest_parameter_test_conciseerrors_01() {
    let (item, _) =
        FunctionRestParameter::parse(&mut newparser("...dippin_dots"), Scanner::new(), false, false).unwrap();
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

    #[test_case("...package", true => sset(&[PACKAGE_NOT_ALLOWED]); "BindingRestElement")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).function_rest_parameter().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("...{a=arguments}" => true; "yes")]
    #[test_case("...a" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).function_rest_parameter().contains_arguments()
    }

    #[test_case("  ...a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 4}}; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).function_rest_parameter().location()
    }

    #[test_case("...a" => false; "no expr")]
    #[test_case("...{a=0}" => true; "with expr")]
    fn contains_expression(src: &str) -> bool {
        Maker::new(src).function_rest_parameter().contains_expression()
    }
}

// FORMAL PARAMETER
mod formal_parameter {
    use super::*;
    use test_case::test_case;

    #[test]
    fn formal_parameter_test_01() {
        let (node, scanner) = check(FormalParameter::parse(&mut newparser("formal"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        pretty_check(&*node, "FormalParameter: formal", &["BindingElement: formal"]);
        concise_check(&*node, "IdentifierName: formal", &[]);
        assert_ne!(format!("{node:?}"), "");
    }
    #[test]
    fn formal_parameter_test_err_01() {
        check_err(
            FormalParameter::parse(&mut newparser(""), Scanner::new(), false, false),
            "BindingElement expected",
            1,
            1,
        );
    }
    #[test]
    fn formal_parameter_test_prettyerrors_01() {
        let (item, _) =
            FormalParameter::parse(&mut newparser("formal_parameter"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn formal_parameter_test_conciseerrors_01() {
        let (item, _) =
            FormalParameter::parse(&mut newparser("formal_parameter"), Scanner::new(), false, false).unwrap();
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

    #[test_case("package", true => sset(&[PACKAGE_NOT_ALLOWED]); "BindingElement")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).formal_parameter().early_errors(&mut errs, strict);
        errs.iter().map(|err| unwind_syntax_error_object(&err.clone())).collect()
    }

    #[test_case("a=arguments" => true; "yes")]
    #[test_case("a" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).formal_parameter().contains_arguments()
    }

    #[test_case("  a" => Location{ starting_line: 1, starting_column: 3, span: Span{ starting_index: 2, length: 1}}; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).formal_parameter().location()
    }

    #[test_case("a" => false; "no init")]
    #[test_case("a=b" => true; "with init")]
    fn has_initializer(src: &str) -> bool {
        Maker::new(src).formal_parameter().has_initializer()
    }

    #[test_case("a" => false; "no expr")]
    #[test_case("a=0" => true; "with expr")]
    fn contains_expression(src: &str) -> bool {
        Maker::new(src).formal_parameter().contains_expression()
    }
}
