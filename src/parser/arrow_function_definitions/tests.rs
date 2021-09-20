use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use test_case::test_case;

// ARROW FUNCTION
#[test]
fn arrow_function_test_01() {
    let (node, scanner) = check(ArrowFunction::parse(&mut newparser("a=>a"), Scanner::new(), true, false, false));
    chk_scan(&scanner, 4);
    pretty_check(&*node, "ArrowFunction: a => a", vec!["ArrowParameters: a", "ConciseBody: a"]);
    concise_check(&*node, "ArrowFunction: a => a", vec!["IdentifierName: a", "Punctuator: =>", "IdentifierName: a"]);
    format!("{:?}", node);
}
#[test]
fn arrow_function_test_02() {
    check_err(ArrowFunction::parse(&mut newparser(""), Scanner::new(), true, false, false), "Identifier or Formal Parameters expected", 1, 1);
}
#[test]
fn arrow_function_test_03() {
    check_err(ArrowFunction::parse(&mut newparser("a"), Scanner::new(), true, false, false), "‘=>’ expected", 1, 2);
}
#[test]
fn arrow_function_test_04() {
    check_err(ArrowFunction::parse(&mut newparser("a=>"), Scanner::new(), true, false, false), "ConciseBody expected", 1, 4);
}
#[test]
fn arrow_function_test_05() {
    check_err(ArrowFunction::parse(&mut newparser("a\n=>a"), Scanner::new(), true, false, false), "Newline not allowed here.", 1, 2);
}
#[test]
fn arrow_function_test_prettyerrors_1() {
    let (item, _) = ArrowFunction::parse(&mut newparser("a=>a"), Scanner::new(), true, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn arrow_function_test_conciseerrors_1() {
    let (item, _) = ArrowFunction::parse(&mut newparser("a=>a"), Scanner::new(), true, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn arrow_function_test_contains_01() {
    let (item, _) = ArrowFunction::parse(&mut newparser("(a=this)=>a"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn arrow_function_test_contains_02() {
    let (item, _) = ArrowFunction::parse(&mut newparser("a=>this"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn arrow_function_test_contains_03() {
    let (item, _) = ArrowFunction::parse(&mut newparser("a=>a"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn arrow_function_test_contains_04() {
    let (item, _) = ArrowFunction::parse(&mut newparser("a=>new.target"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::NewTarget), true);
}
#[test]
fn arrow_function_test_contains_05() {
    let (item, _) = ArrowFunction::parse(&mut newparser("a=>super.b"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::SuperProperty), true);
}
#[test]
fn arrow_function_test_contains_06() {
    let (item, _) = ArrowFunction::parse(&mut newparser("a=>super(0)"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::SuperCall), true);
}
#[test]
fn arrow_function_test_contains_07() {
    let (item, _) = ArrowFunction::parse(&mut newparser("a=>super(0)"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Super), true);
}
#[test]
fn arrow_function_test_contains_08() {
    let (item, _) = ArrowFunction::parse(&mut newparser("a=>10"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn arrow_function_test_contains_09() {
    let (item, _) = ArrowFunction::parse(&mut newparser("(a=10)=>a"), Scanner::new(), true, false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("(x=item.#valid) => x*2" => true; "Parameter valid")]
#[test_case("(x=item.#invalid) => x*2" => false; "Parameter invalid")]
#[test_case("x => x.#valid" => true; "Body valid")]
#[test_case("x => x.#invalid" => false; "Body invalid")]
fn arrow_function_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ArrowFunction::parse(&mut newparser(src), Scanner::new(), true, false, false).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}

// ARROW PARAMETERS
#[test]
fn arrow_parameters_test_01() {
    let (node, scanner) = check(ArrowParameters::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, ArrowParameters::Identifier(..)));
    pretty_check(&*node, "ArrowParameters: a", vec!["BindingIdentifier: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
}
#[test]
fn arrow_parameters_test_02() {
    let r = ArrowParameters::parse(&mut newparser("(a)"), Scanner::new(), false, false);
    let (node, scanner) = check(r);
    chk_scan(&scanner, 3);
    assert!(matches!(&*node, ArrowParameters::Formals(..)));
    pretty_check(&*node, "ArrowParameters: ( a )", vec!["ArrowFormalParameters: ( a )"]);
    concise_check(&*node, "ArrowFormalParameters: ( a )", vec!["Punctuator: (", "IdentifierName: a", "Punctuator: )"]);
    format!("{:?}", node);
}
#[test]
fn arrow_parameters_test_err_01() {
    check_err(ArrowParameters::parse(&mut newparser(""), Scanner::new(), false, false), "Identifier or Formal Parameters expected", 1, 1);
}
#[test]
fn arrow_parameters_test_err_02() {
    check_err(ArrowParameters::parse(&mut newparser("("), Scanner::new(), false, false), "Expression, spread pattern, or closing paren expected", 1, 2);
}
#[test]
fn arrow_parameters_test_err_03() {
    check_err(ArrowParameters::parse(&mut newparser("(a"), Scanner::new(), false, false), "‘)’ expected", 1, 3);
}
#[test]
fn arrow_parameters_test_err_04() {
    check_err(ArrowParameters::parse(&mut newparser("(5 * 3)"), Scanner::new(), false, false), "‘)’ expected", 1, 2);
}
#[test]
fn arrow_parameters_test_prettyerrors_1() {
    let (item, _) = ArrowParameters::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn arrow_parameters_test_prettyerrors_2() {
    let (item, _) = ArrowParameters::parse(&mut newparser("(a)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn arrow_parameters_test_conciseerrors_1() {
    let (item, _) = ArrowParameters::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn arrow_parameters_test_conciseerrors_2() {
    let (item, _) = ArrowParameters::parse(&mut newparser("(a)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn arrow_parameters_test_contains_01() {
    let (item, _) = ArrowParameters::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn arrow_parameters_test_contains_02() {
    let (item, _) = ArrowParameters::parse(&mut newparser("(a=this)"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn arrow_parameters_test_contains_03() {
    let (item, _) = ArrowParameters::parse(&mut newparser("(a=0)"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("identifier" => true; "BindingIdentifier")]
#[test_case("(a=item.#valid)" => true; "ArrowFormalParameters valid")]
#[test_case("(a=item.#invalid)" => false; "ArrowFormalParameters invalid")]
fn arrow_parameters_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ArrowParameters::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}

// CONCISE BODY
#[test]
fn concise_body_test_01() {
    let (node, scanner) = check(ConciseBody::parse(&mut newparser("a"), Scanner::new(), true));
    chk_scan(&scanner, 1);
    assert!(matches!(&*node, ConciseBody::Expression(..)));
    pretty_check(&*node, "ConciseBody: a", vec!["ExpressionBody: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
}
#[test]
fn concise_body_test_02() {
    let (node, scanner) = check(ConciseBody::parse(&mut newparser("{q;}"), Scanner::new(), true));
    println!("node = {:?}", node);
    chk_scan(&scanner, 4);
    assert!(matches!(&*node, ConciseBody::Function(..)));
    pretty_check(&*node, "ConciseBody: { q ; }", vec!["FunctionBody: q ;"]);
    concise_check(&*node, "ConciseBody: { q ; }", vec!["Punctuator: {", "ExpressionStatement: q ;", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn concise_body_test_err_01() {
    check_err(ConciseBody::parse(&mut newparser(""), Scanner::new(), true), "ConciseBody expected", 1, 1);
}
#[test]
fn concise_body_test_err_02() {
    check_err(ConciseBody::parse(&mut newparser("{"), Scanner::new(), true), "‘}’ expected", 1, 2);
}
#[test]
fn concise_body_test_prettyerrors_1() {
    let (item, _) = ConciseBody::parse(&mut newparser("a"), Scanner::new(), true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn concise_body_test_prettyerrors_2() {
    let (item, _) = ConciseBody::parse(&mut newparser("{q;}"), Scanner::new(), true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn concise_body_test_conciseerrors_1() {
    let (item, _) = ConciseBody::parse(&mut newparser("a"), Scanner::new(), true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn concise_body_test_conciseerrors_2() {
    let (item, _) = ConciseBody::parse(&mut newparser("{q;}"), Scanner::new(), true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn concise_body_test_contains_01() {
    let (item, _) = ConciseBody::parse(&mut newparser("{ this.a = 12; }"), Scanner::new(), true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn concise_body_test_contains_02() {
    let (item, _) = ConciseBody::parse(&mut newparser("{ a = 12; }"), Scanner::new(), true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test]
fn concise_body_test_contains_03() {
    let (item, _) = ConciseBody::parse(&mut newparser("this.a"), Scanner::new(), true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn concise_body_test_contains_04() {
    let (item, _) = ConciseBody::parse(&mut newparser("a"), Scanner::new(), true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("item.#valid" => true; "ExpressionBody valid")]
#[test_case("item.#invalid" => false; "ExpressionBody invalid")]
#[test_case("{ item.#valid }" => true; "FunctionBody valid")]
#[test_case("{ item.#invalid }" => false; "FunctionBody invalid")]
fn concise_body_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ConciseBody::parse(&mut newparser(src), Scanner::new(), true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}

// EXPRESSION BODY
#[test]
fn expression_body_test_01() {
    let (node, scanner) = check(ExpressionBody::parse(&mut newparser("a"), Scanner::new(), true, false));
    chk_scan(&scanner, 1);
    pretty_check(&*node, "ExpressionBody: a", vec!["AssignmentExpression: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
}
#[test]
fn expression_body_test_cache_01() {
    let mut parser = newparser("a+b+c+d+e");
    let (node, scanner) = check(ExpressionBody::parse(&mut parser, Scanner::new(), false, false));
    let (node2, scanner2) = check(ExpressionBody::parse(&mut parser, Scanner::new(), false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn expression_body_test_err_01() {
    check_err(ExpressionBody::parse(&mut newparser(""), Scanner::new(), true, false), "AssignmentExpression expected", 1, 1);
}
#[test]
fn expression_body_test_prettyerrors_1() {
    let (item, _) = ExpressionBody::parse(&mut newparser("a"), Scanner::new(), true, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn expression_body_test_conciseerrors_1() {
    let (item, _) = ExpressionBody::parse(&mut newparser("a"), Scanner::new(), true, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn expression_body_test_contains_01() {
    let (item, _) = ExpressionBody::parse(&mut newparser("this.a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn expression_body_test_contains_02() {
    let (item, _) = ExpressionBody::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("item.#valid" => true; "ExpressionBody valid")]
#[test_case("item.#invalid" => false; "ExpressionBody invalid")]
fn expression_body_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ExpressionBody::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}

// ARROW FORMAL PARAMETERS
#[test]
fn arrow_formal_parameters_test_01() {
    let (node, scanner) = check(ArrowFormalParameters::parse(&mut newparser("(a,b)"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    pretty_check(&*node, "ArrowFormalParameters: ( a , b )", vec!["UniqueFormalParameters: a , b"]);
    concise_check(&*node, "ArrowFormalParameters: ( a , b )", vec!["Punctuator: (", "FormalParameterList: a , b", "Punctuator: )"]);
    format!("{:?}", node);
}
#[test]
fn arrow_formal_parameters_test_cache_01() {
    let mut parser = newparser("(a,b)");
    let (node, scanner) = check(ArrowFormalParameters::parse(&mut parser, Scanner::new(), false, false));
    let (node2, scanner2) = check(ArrowFormalParameters::parse(&mut parser, Scanner::new(), false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn arrow_formal_parameters_test_err_01() {
    check_err(ArrowFormalParameters::parse(&mut newparser(""), Scanner::new(), false, false), "‘(’ expected", 1, 1);
}
#[test]
fn arrow_formal_parameters_test_err_02() {
    check_err(ArrowFormalParameters::parse(&mut newparser("("), Scanner::new(), false, false), "‘)’ expected", 1, 2);
}
#[test]
fn arrow_formal_parameters_test_prettyerrors_1() {
    let (item, _) = ArrowFormalParameters::parse(&mut newparser("(a)"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn arrow_formal_parameters_test_conciseerrors_1() {
    let (item, _) = ArrowFormalParameters::parse(&mut newparser("(a)"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn arrow_formal_parameters_test_contains_01() {
    let (item, _) = ArrowFormalParameters::parse(&mut newparser("(a=this)"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), true);
}
#[test]
fn arrow_formal_parameters_test_contains_02() {
    let (item, _) = ArrowFormalParameters::parse(&mut newparser("(a)"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::This), false);
}
#[test_case("(a=item.#valid)" => true; "UniqueFormalParameters valid")]
#[test_case("(a=item.#invalid)" => false; "UniqueFormalParameters invalid")]
fn arrow_formal_parameters_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ArrowFormalParameters::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
