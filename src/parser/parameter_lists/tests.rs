use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

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
