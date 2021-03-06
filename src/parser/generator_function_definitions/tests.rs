use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

// GENERATOR METHOD
#[test]
fn generator_method_test_01() {
    let (node, scanner) = check(GeneratorMethod::parse(&mut newparser("*a(){}"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    pretty_check(&*node, "GeneratorMethod: * a (  ) {  }", vec!["ClassElementName: a", "UniqueFormalParameters: ", "GeneratorBody: "]);
    concise_check(&*node, "GeneratorMethod: * a (  ) {  }", vec!["Punctuator: *", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn generator_method_test_02() {
    check_err(GeneratorMethod::parse(&mut newparser(""), Scanner::new(), false, false), "‘*’ expected", 1, 1);
}
#[test]
fn generator_method_test_03() {
    check_err(GeneratorMethod::parse(&mut newparser("*"), Scanner::new(), false, false), "ClassElementName expected", 1, 2);
}
#[test]
fn generator_method_test_04() {
    check_err(GeneratorMethod::parse(&mut newparser("*a"), Scanner::new(), false, false), "‘(’ expected", 1, 3);
}
#[test]
fn generator_method_test_05() {
    check_err(GeneratorMethod::parse(&mut newparser("*a("), Scanner::new(), false, false), "‘)’ expected", 1, 4);
}
#[test]
fn generator_method_test_06() {
    check_err(GeneratorMethod::parse(&mut newparser("*a()"), Scanner::new(), false, false), "‘{’ expected", 1, 5);
}
#[test]
fn generator_method_test_07() {
    check_err(GeneratorMethod::parse(&mut newparser("*a(){"), Scanner::new(), false, false), "‘}’ expected", 1, 6);
}
#[test]
fn generator_method_test_prettyerrors_1() {
    let (item, _) = GeneratorMethod::parse(&mut newparser("* bob(blue, red, green) { yield blue + red + green; }"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn generator_method_test_conciseerrors_1() {
    let (item, _) = GeneratorMethod::parse(&mut newparser("* bob(blue, red, green) { yield blue + red + green; }"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn generator_method_test_contains_01() {
    let (item, _) = GeneratorMethod::parse(&mut newparser("*[0](){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn generator_method_test_contains_02() {
    let (item, _) = GeneratorMethod::parse(&mut newparser("*a(x=0){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn generator_method_test_contains_03() {
    let (item, _) = GeneratorMethod::parse(&mut newparser("*a(){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn generator_method_test_contains_04() {
    let (item, _) = GeneratorMethod::parse(&mut newparser("*a(){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn generator_method_test_computed_property_contains_01() {
    let (item, _) = GeneratorMethod::parse(&mut newparser("*[0](){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn generator_method_test_computed_property_contains_02() {
    let (item, _) = GeneratorMethod::parse(&mut newparser("*[a](x=0){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}

// GENERATOR DECLARATION
#[test]
fn generator_declaration_test_01() {
    let (node, scanner) = check(GeneratorDeclaration::parse(&mut newparser("function *a(){}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 15);
    pretty_check(&*node, "GeneratorDeclaration: function * a (  ) {  }", vec!["BindingIdentifier: a", "FormalParameters: ", "GeneratorBody: "]);
    concise_check(
        &*node,
        "GeneratorDeclaration: function * a (  ) {  }",
        vec!["Keyword: function", "Punctuator: *", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
    );
    format!("{:?}", node);
}
#[test]
fn generator_declaration_test_02() {
    let (node, scanner) = check(GeneratorDeclaration::parse(&mut newparser("function *(){}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 14);
    pretty_check(&*node, "GeneratorDeclaration: function * (  ) {  }", vec!["FormalParameters: ", "GeneratorBody: "]);
    concise_check(&*node, "GeneratorDeclaration: function * (  ) {  }", vec!["Keyword: function", "Punctuator: *", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn generator_declaration_test_03() {
    check_err(GeneratorDeclaration::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘function’ expected", 1, 1);
}
#[test]
fn generator_declaration_test_04() {
    check_err(GeneratorDeclaration::parse(&mut newparser("function"), Scanner::new(), false, false, true), "‘*’ expected", 1, 9);
}
#[test]
fn generator_declaration_test_05() {
    check_err(GeneratorDeclaration::parse(&mut newparser("function *"), Scanner::new(), false, false, true), "‘(’ expected", 1, 11);
}
#[test]
fn generator_declaration_test_06() {
    check_err(GeneratorDeclaration::parse(&mut newparser("function * h"), Scanner::new(), false, false, true), "‘(’ expected", 1, 13);
}
#[test]
fn generator_declaration_test_07() {
    check_err(GeneratorDeclaration::parse(&mut newparser("function * h ("), Scanner::new(), false, false, true), "‘)’ expected", 1, 15);
}
#[test]
fn generator_declaration_test_075() {
    check_err(GeneratorDeclaration::parse(&mut newparser("function * ("), Scanner::new(), false, false, true), "‘)’ expected", 1, 13);
}
#[test]
fn generator_declaration_test_076() {
    check_err(GeneratorDeclaration::parse(&mut newparser("function * ("), Scanner::new(), false, false, false), "Not an identifier", 1, 11);
}
#[test]
fn generator_declaration_test_08() {
    check_err(GeneratorDeclaration::parse(&mut newparser("function * h ( u"), Scanner::new(), false, false, true), "‘)’ expected", 1, 17);
}
#[test]
fn generator_declaration_test_09() {
    check_err(GeneratorDeclaration::parse(&mut newparser("function * h ( u )"), Scanner::new(), false, false, true), "‘{’ expected", 1, 19);
}
#[test]
fn generator_declaration_test_10() {
    check_err(GeneratorDeclaration::parse(&mut newparser("function * h ( u ) {"), Scanner::new(), false, false, true), "‘}’ expected", 1, 21);
}
#[test]
fn generator_declaration_test_11() {
    check_err(GeneratorDeclaration::parse(&mut newparser("function * h ( u ) { z;"), Scanner::new(), false, false, true), "‘}’ expected", 1, 24);
}
#[test]
fn generator_declaration_test_prettyerrors_1() {
    let (item, _) = GeneratorDeclaration::parse(&mut newparser("function * bob(blue, red, green) { yield blue + red + green; }"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn generator_declaration_test_prettyerrors_2() {
    let (item, _) = GeneratorDeclaration::parse(&mut newparser("function * (blue, red, green) { yield blue + red + green; }"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn generator_declaration_test_conciseerrors_1() {
    let (item, _) = GeneratorDeclaration::parse(&mut newparser("function * bob(blue, red, green) { yield blue + red + green; }"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn generator_declaration_test_conciseerrors_2() {
    let (item, _) = GeneratorDeclaration::parse(&mut newparser("function * (blue, red, green) { yield blue + red + green; }"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn generator_declaration_test_bound_names_01() {
    let (item, _) = GeneratorDeclaration::parse(&mut newparser("function * bob() {}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["bob"]);
}
#[test]
fn generator_declaration_test_bound_names_02() {
    let (item, _) = GeneratorDeclaration::parse(&mut newparser("function * () {}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["*default*"]);
}
#[test]
fn generator_declaration_test_contains_01() {
    let (item, _) = GeneratorDeclaration::parse(&mut newparser("function * a(x=0) {0;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}

// GENERATOR EXPRESSION
#[test]
fn generator_expression_test_01() {
    let (node, scanner) = check(GeneratorExpression::parse(&mut newparser("function *a(){}"), Scanner::new()));
    chk_scan(&scanner, 15);
    pretty_check(&*node, "GeneratorExpression: function * a (  ) {  }", vec!["BindingIdentifier: a", "FormalParameters: ", "GeneratorBody: "]);
    concise_check(
        &*node,
        "GeneratorExpression: function * a (  ) {  }",
        vec!["Keyword: function", "Punctuator: *", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
    );
    format!("{:?}", node);
    assert!(node.is_function_definition());
}
#[test]
fn generator_expression_test_02() {
    let (node, scanner) = check(GeneratorExpression::parse(&mut newparser("function *(){}"), Scanner::new()));
    chk_scan(&scanner, 14);
    pretty_check(&*node, "GeneratorExpression: function * (  ) {  }", vec!["FormalParameters: ", "GeneratorBody: "]);
    concise_check(&*node, "GeneratorExpression: function * (  ) {  }", vec!["Keyword: function", "Punctuator: *", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
    format!("{:?}", node);
    assert!(node.is_function_definition());
}
#[test]
fn generator_expression_test_03() {
    check_err(GeneratorExpression::parse(&mut newparser(""), Scanner::new()), "‘function’ expected", 1, 1);
}
#[test]
fn generator_expression_test_04() {
    check_err(GeneratorExpression::parse(&mut newparser("function"), Scanner::new()), "‘*’ expected", 1, 9);
}
#[test]
fn generator_expression_test_05() {
    check_err(GeneratorExpression::parse(&mut newparser("function *"), Scanner::new()), "‘(’ expected", 1, 11);
}
#[test]
fn generator_expression_test_06() {
    check_err(GeneratorExpression::parse(&mut newparser("function * h"), Scanner::new()), "‘(’ expected", 1, 13);
}
#[test]
fn generator_expression_test_07() {
    check_err(GeneratorExpression::parse(&mut newparser("function * h ("), Scanner::new()), "‘)’ expected", 1, 15);
}
#[test]
fn generator_expression_test_08() {
    check_err(GeneratorExpression::parse(&mut newparser("function * h ( u"), Scanner::new()), "‘)’ expected", 1, 17);
}
#[test]
fn generator_expression_test_09() {
    check_err(GeneratorExpression::parse(&mut newparser("function * h ( u )"), Scanner::new()), "‘{’ expected", 1, 19);
}
#[test]
fn generator_expression_test_10() {
    check_err(GeneratorExpression::parse(&mut newparser("function * h ( u ) {"), Scanner::new()), "‘}’ expected", 1, 21);
}
#[test]
fn generator_expression_test_11() {
    check_err(GeneratorExpression::parse(&mut newparser("function * h ( u ) { z;"), Scanner::new()), "‘}’ expected", 1, 24);
}
#[test]
fn generator_expression_test_prettyerrors_1() {
    let (item, _) = GeneratorExpression::parse(&mut newparser("function * bob(blue, red, green) { yield blue + red + green; }"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn generator_expression_test_prettyerrors_2() {
    let (item, _) = GeneratorExpression::parse(&mut newparser("function * (blue, red, green) { yield blue + red + green; }"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn generator_expression_test_conciseerrors_1() {
    let (item, _) = GeneratorExpression::parse(&mut newparser("function * bob(blue, red, green) { yield blue + red + green; }"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn generator_expression_test_conciseerrors_2() {
    let (item, _) = GeneratorExpression::parse(&mut newparser("function * (blue, red, green) { yield blue + red + green; }"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn generator_expression_test_contains_01() {
    let (item, _) = GeneratorExpression::parse(&mut newparser("function * a(x=0) {0;}"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}

// GENERATOR BODY
#[test]
fn generator_body_test_01() {
    let (node, scanner) = GeneratorBody::parse(&mut newparser("yield 1;"), Scanner::new());
    chk_scan(&scanner, 8);
    pretty_check(&*node, "GeneratorBody: yield 1 ;", vec!["FunctionBody: yield 1 ;"]);
    concise_check(&*node, "ExpressionStatement: yield 1 ;", vec!["YieldExpression: yield 1", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn generator_body_test_prettyerrors_1() {
    let (item, _) = GeneratorBody::parse(&mut newparser("yield 1;"), Scanner::new());
    pretty_error_validate(&*item);
}
#[test]
fn generator_body_test_conciseerrors_1() {
    let (item, _) = GeneratorBody::parse(&mut newparser("yield 1;"), Scanner::new());
    concise_error_validate(&*item);
}
#[test]
fn generator_body_test_cache_01() {
    let mut parser = newparser("yield 1;");
    let (node, scanner) = GeneratorBody::parse(&mut parser, Scanner::new());
    let (node2, scanner2) = GeneratorBody::parse(&mut parser, Scanner::new());
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn generator_body_test_contains_01() {
    let (item, _) = GeneratorBody::parse(&mut newparser("yield 1;"), Scanner::new());
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn generator_body_test_contains_02() {
    let (item, _) = GeneratorBody::parse(&mut newparser("yield a;"), Scanner::new());
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}

// YIELD EXPRESSION
#[test]
fn yield_expression_test_01() {
    let (pn, scanner) = check(YieldExpression::parse(&mut newparser("yield"), Scanner::new(), true, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*pn, YieldExpression::Simple));
    pretty_check(&*pn, "YieldExpression: yield", vec![]);
    concise_check(&*pn, "Keyword: yield", vec![]);
    format!("{:?}", pn);
}
#[test]
fn yield_expression_test_02() {
    let (pn, scanner) = check(YieldExpression::parse(&mut newparser("yield 5"), Scanner::new(), true, false));
    chk_scan(&scanner, 7);
    assert!(matches!(&*pn, YieldExpression::Expression(..)));
    pretty_check(&*pn, "YieldExpression: yield 5", vec!["AssignmentExpression: 5"]);
    concise_check(&*pn, "YieldExpression: yield 5", vec!["Keyword: yield", "Numeric: 5"]);
    format!("{:?}", pn);
}
#[test]
fn yield_expression_test_03() {
    let (pn, scanner) = check(YieldExpression::parse(&mut newparser("yield *5"), Scanner::new(), true, false));
    chk_scan(&scanner, 8);
    assert!(matches!(&*pn, YieldExpression::From(..)));
    pretty_check(&*pn, "YieldExpression: yield * 5", vec!["AssignmentExpression: 5"]);
    concise_check(&*pn, "YieldExpression: yield * 5", vec!["Keyword: yield", "Punctuator: *", "Numeric: 5"]);
    format!("{:?}", pn);
}
#[test]
fn yield_expression_test_04() {
    let (pn, scanner) = check(YieldExpression::parse(&mut newparser("yield \n*5"), Scanner::new(), true, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*pn, YieldExpression::Simple));
    pretty_check(&*pn, "YieldExpression: yield", vec![]);
    concise_check(&*pn, "Keyword: yield", vec![]);
    format!("{:?}", pn);
}
#[test]
fn yield_expression_test_05() {
    let (pn, scanner) = check(YieldExpression::parse(&mut newparser("yield @"), Scanner::new(), true, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*pn, YieldExpression::Simple));
    pretty_check(&*pn, "YieldExpression: yield", vec![]);
    concise_check(&*pn, "Keyword: yield", vec![]);
    format!("{:?}", pn);
}
#[test]
fn yield_expression_test_06() {
    let (pn, scanner) = check(YieldExpression::parse(&mut newparser("yield *@"), Scanner::new(), true, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*pn, YieldExpression::Simple));
    pretty_check(&*pn, "YieldExpression: yield", vec![]);
    concise_check(&*pn, "Keyword: yield", vec![]);
    format!("{:?}", pn);
}
#[test]
fn yield_expression_test_07() {
    check_err(YieldExpression::parse(&mut newparser(""), Scanner::new(), true, false), "‘yield’ expected", 1, 1);
}
#[test]
fn yield_expression_test_prettyerrors_1() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield"), Scanner::new(), true, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn yield_expression_test_prettyerrors_2() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield a"), Scanner::new(), true, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn yield_expression_test_prettyerrors_3() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield *a"), Scanner::new(), true, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn yield_expression_test_conciseerrors_1() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield"), Scanner::new(), true, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn yield_expression_test_conciseerrors_2() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield a"), Scanner::new(), true, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn yield_expression_test_conciseerrors_3() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield *a"), Scanner::new(), true, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn yield_expression_test_contains_01() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield 1;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn yield_expression_test_contains_02() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield a;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn yield_expression_test_contains_03() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield *1;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn yield_expression_test_contains_04() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield *a;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn yield_expression_test_contains_05() {
    let (item, _) = YieldExpression::parse(&mut newparser("yield;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
