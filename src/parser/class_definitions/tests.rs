use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

// CLASS DECLARATION
#[test]
fn class_declaration_test_01() {
    let (node, scanner) = check(ClassDeclaration::parse(&mut newparser("class a{}"), Scanner::new(), false, false, false));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "ClassDeclaration: class a { }", vec!["BindingIdentifier: a", "ClassTail: { }"]);
    concise_check(&*node, "ClassDeclaration: class a { }", vec!["Keyword: class", "IdentifierName: a", "ClassTail: { }"]);
    format!("{:?}", node);
}
#[test]
fn class_declaration_test_02() {
    let (node, scanner) = check(ClassDeclaration::parse(&mut newparser("class {}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "ClassDeclaration: class { }", vec!["ClassTail: { }"]);
    concise_check(&*node, "ClassDeclaration: class { }", vec!["Keyword: class", "ClassTail: { }"]);
    format!("{:?}", node);
}
#[test]
fn class_declaration_test_err_01() {
    check_err(ClassDeclaration::parse(&mut newparser(""), Scanner::new(), false, false, false), "‘class’ expected", 1, 1);
}
#[test]
fn class_declaration_test_err_02() {
    check_err(ClassDeclaration::parse(&mut newparser("class"), Scanner::new(), false, false, false), "Not an identifier", 1, 6);
}
#[test]
fn class_declaration_test_err_03() {
    check_err(ClassDeclaration::parse(&mut newparser("class a"), Scanner::new(), false, false, false), "‘{’ expected", 1, 8);
}
#[test]
fn class_declaration_test_prettyerrors_1() {
    let (item, _) = ClassDeclaration::parse(&mut newparser("class a { }"), Scanner::new(), false, false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_declaration_test_prettyerrors_2() {
    let (item, _) = ClassDeclaration::parse(&mut newparser("class { }"), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_declaration_test_conciseerrors_1() {
    let (item, _) = ClassDeclaration::parse(&mut newparser("class a { }"), Scanner::new(), false, false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_declaration_test_conciseerrors_2() {
    let (item, _) = ClassDeclaration::parse(&mut newparser("class { }"), Scanner::new(), false, false, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_declaration_test_bound_names_01() {
    let (item, _) = ClassDeclaration::parse(&mut newparser("class a { }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn class_declaration_test_bound_names_02() {
    let (item, _) = ClassDeclaration::parse(&mut newparser("class { }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.bound_names(), &["*default*"]);
}
#[test]
fn class_declaration_test_contains_01() {
    let (item, _) = ClassDeclaration::parse(&mut newparser("class a { [67](){} }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_declaration_test_contains_02() {
    let (item, _) = ClassDeclaration::parse(&mut newparser("class a { b(c=9){} }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_declaration_test_contains_03() {
    let (item, _) = ClassDeclaration::parse(&mut newparser("class { [67](){} }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_declaration_test_contains_04() {
    let (item, _) = ClassDeclaration::parse(&mut newparser("class { b(c=9){} }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}

// CLASS EXPRESSION
#[test]
fn class_expression_test_01() {
    let (node, scanner) = check(ClassExpression::parse(&mut newparser("class a{}"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "ClassExpression: class a { }", vec!["BindingIdentifier: a", "ClassTail: { }"]);
    concise_check(&*node, "ClassExpression: class a { }", vec!["Keyword: class", "IdentifierName: a", "ClassTail: { }"]);
    format!("{:?}", node);
    assert!(node.is_function_definition());
}
#[test]
fn class_expression_test_02() {
    let (node, scanner) = check(ClassExpression::parse(&mut newparser("class {}"), Scanner::new(), false, false));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "ClassExpression: class { }", vec!["ClassTail: { }"]);
    concise_check(&*node, "ClassExpression: class { }", vec!["Keyword: class", "ClassTail: { }"]);
    format!("{:?}", node);
    assert!(node.is_function_definition());
}
#[test]
fn class_expression_test_err_01() {
    check_err(ClassExpression::parse(&mut newparser(""), Scanner::new(), false, false), "‘class’ expected", 1, 1);
}
#[test]
fn class_expression_test_err_02() {
    check_err(ClassExpression::parse(&mut newparser("class"), Scanner::new(), false, false), "‘{’ expected", 1, 6);
}
#[test]
fn class_expression_test_prettyerrors_1() {
    let (item, _) = ClassExpression::parse(&mut newparser("class a { }"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_expression_test_prettyerrors_2() {
    let (item, _) = ClassExpression::parse(&mut newparser("class { }"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_expression_test_conciseerrors_1() {
    let (item, _) = ClassExpression::parse(&mut newparser("class a { }"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_expression_test_conciseerrors_2() {
    let (item, _) = ClassExpression::parse(&mut newparser("class { }"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_expression_test_contains_01() {
    let (item, _) = ClassExpression::parse(&mut newparser("class a { [67](){} }"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_expression_test_contains_02() {
    let (item, _) = ClassExpression::parse(&mut newparser("class a { b(c=9){} }"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_expression_test_contains_03() {
    let (item, _) = ClassExpression::parse(&mut newparser("class { [67](){} }"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_expression_test_contains_04() {
    let (item, _) = ClassExpression::parse(&mut newparser("class { b(c=9){} }"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}

// CLASS TAIL
#[test]
fn class_tail_test_01() {
    let (node, scanner) = check(ClassTail::parse(&mut newparser("{}"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    pretty_check(&*node, "ClassTail: { }", vec![]);
    concise_check(&*node, "ClassTail: { }", vec!["Punctuator: {", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn class_tail_test_02() {
    let (node, scanner) = check(ClassTail::parse(&mut newparser("{;}"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    pretty_check(&*node, "ClassTail: { ; }", vec!["ClassBody: ;"]);
    concise_check(&*node, "ClassTail: { ; }", vec!["Punctuator: {", "Punctuator: ;", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn class_tail_test_03() {
    let (node, scanner) = check(ClassTail::parse(&mut newparser("extends a { }"), Scanner::new(), false, false));
    chk_scan(&scanner, 13);
    pretty_check(&*node, "ClassTail: extends a { }", vec!["ClassHeritage: extends a"]);
    concise_check(&*node, "ClassTail: extends a { }", vec!["ClassHeritage: extends a", "Punctuator: {", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn class_tail_test_04() {
    let (node, scanner) = check(ClassTail::parse(&mut newparser("extends a{;}"), Scanner::new(), false, false));
    chk_scan(&scanner, 12);
    pretty_check(&*node, "ClassTail: extends a { ; }", vec!["ClassHeritage: extends a", "ClassBody: ;"]);
    concise_check(&*node, "ClassTail: extends a { ; }", vec!["ClassHeritage: extends a", "Punctuator: {", "Punctuator: ;", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn class_tail_test_err_01() {
    check_err(ClassTail::parse(&mut newparser(""), Scanner::new(), false, false), "‘{’ expected", 1, 1);
}
#[test]
fn class_tail_test_err_02() {
    check_err(ClassTail::parse(&mut newparser("{"), Scanner::new(), false, false), "‘}’ expected", 1, 2);
}
#[test]
fn class_tail_test_prettyerrors_1() {
    let (item, _) = ClassTail::parse(&mut newparser("{}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_tail_test_prettyerrors_2() {
    let (item, _) = ClassTail::parse(&mut newparser("extends other_object {}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_tail_test_prettyerrors_3() {
    let (item, _) =
        ClassTail::parse(&mut newparser("extends other_object { blue(left, right) { return { sum: left+right, difference: left-right }; }}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_tail_test_prettyerrors_4() {
    let (item, _) = ClassTail::parse(&mut newparser(" { blue(left, right) { return { sum: left+right, difference: left-right }; }}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_tail_test_conciseerrors_1() {
    let (item, _) = ClassTail::parse(&mut newparser("{}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_tail_test_conciseerrors_2() {
    let (item, _) = ClassTail::parse(&mut newparser("extends other_object {}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_tail_test_conciseerrors_3() {
    let (item, _) =
        ClassTail::parse(&mut newparser("extends other_object { blue(left, right) { return { sum: left+right, difference: left-right }; }}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_tail_test_conciseerrors_4() {
    let (item, _) = ClassTail::parse(&mut newparser(" { blue(left, right) { return { sum: left+right, difference: left-right }; }}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_tail_test_cache_01() {
    let mut parser = newparser("{}");
    let (node, scanner) = check(ClassTail::parse(&mut parser, Scanner::new(), false, false));
    let (node2, scanner2) = check(ClassTail::parse(&mut parser, Scanner::new(), false, false));
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn class_tail_test_contains_01() {
    let (item, _) = ClassTail::parse(&mut newparser("extends a { }"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::ClassBody), false);
    assert_eq!(item.contains(ParseNodeKind::ClassHeritage), true);
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_tail_test_contains_02() {
    let (item, _) = ClassTail::parse(&mut newparser("{ [67](){} }"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::ClassBody), true);
    assert_eq!(item.contains(ParseNodeKind::ClassHeritage), false);
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}

// CLASS HERITAGE
#[test]
fn class_heritage_test_01() {
    let (node, scanner) = check(ClassHeritage::parse(&mut newparser("extends a"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "ClassHeritage: extends a", vec!["LeftHandSideExpression: a"]);
    concise_check(&*node, "ClassHeritage: extends a", vec!["Keyword: extends", "IdentifierName: a"]);
    format!("{:?}", node);
}
#[test]
fn class_heritage_test_err_01() {
    check_err(ClassHeritage::parse(&mut newparser(""), Scanner::new(), false, false), "‘extends’ expected", 1, 1);
}
#[test]
fn class_heritage_test_err_02() {
    check_err(ClassHeritage::parse(&mut newparser("extends"), Scanner::new(), false, false), "LeftHandSideExpression expected", 1, 8);
}
#[test]
fn class_heritage_test_prettyerrors_1() {
    let (item, _) = ClassHeritage::parse(&mut newparser("extends alphabet"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_heritage_test_conciseerrors_1() {
    let (item, _) = ClassHeritage::parse(&mut newparser("extends alphabet"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_heritage_test_contains_01() {
    let (item, _) = ClassHeritage::parse(&mut newparser("extends a"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_heritage_test_contains_02() {
    let (item, _) = ClassHeritage::parse(&mut newparser("extends bob(33)"), Scanner::new(), false, false).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}

// CLASS BODY
#[test]
fn class_body_test_01() {
    let (node, scanner) = check(ClassBody::parse(&mut newparser(";"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    pretty_check(&*node, "ClassBody: ;", vec!["ClassElementList: ;"]);
    concise_check(&*node, "Punctuator: ;", vec![]);
    format!("{:?}", node);
}
#[test]
fn class_body_test_err_01() {
    check_err(ClassBody::parse(&mut newparser(""), Scanner::new(), false, false), "ClassElement expected", 1, 1);
}
#[test]
fn class_body_test_prettyerrors_1() {
    let (item, _) = ClassBody::parse(&mut newparser("a(){} b(){}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_body_test_conciseerrors_1() {
    let (item, _) = ClassBody::parse(&mut newparser("a(){} b(){}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_body_test_contains_01() {
    let (item, _) = ClassBody::parse(&mut newparser("a(){22;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_body_test_contains_02() {
    let (item, _) = ClassBody::parse(&mut newparser("a(){;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_body_test_computed_property_contains_01() {
    let (item, _) = ClassBody::parse(&mut newparser("[67](){;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_body_test_computed_property_contains_02() {
    let (item, _) = ClassBody::parse(&mut newparser("a(){;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}

// CLASS ELEMENT LIST
#[test]
fn class_element_list_test_01() {
    let (node, scanner) = check(ClassElementList::parse(&mut newparser("a(){}"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    pretty_check(&*node, "ClassElementList: a (  ) {  }", vec!["ClassElement: a (  ) {  }"]);
    concise_check(&*node, "MethodDefinition: a (  ) {  }", vec!["IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn class_element_list_test_02() {
    let (node, scanner) = check(ClassElementList::parse(&mut newparser("a(){} ; b(a){a;}"), Scanner::new(), false, false));
    chk_scan(&scanner, 16);
    pretty_check(&*node, "ClassElementList: a (  ) {  } ; b ( a ) { a ; }", vec!["ClassElementList: a (  ) {  } ;", "ClassElement: b ( a ) { a ; }"]);
    concise_check(&*node, "ClassElementList: a (  ) {  } ; b ( a ) { a ; }", vec!["ClassElementList: a (  ) {  } ;", "MethodDefinition: b ( a ) { a ; }"]);
    format!("{:?}", node);
}
#[test]
fn class_element_list_test_err_01() {
    check_err(ClassElementList::parse(&mut newparser(""), Scanner::new(), false, false), "ClassElement expected", 1, 1);
}
#[test]
fn class_element_list_test_prettyerrors_1() {
    let (item, _) = ClassElementList::parse(&mut newparser("a(){} b(){}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_element_list_test_conciseerrors_1() {
    let (item, _) = ClassElementList::parse(&mut newparser("a(){} b(){}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_element_list_test_contains_01() {
    let (item, _) = ClassElementList::parse(&mut newparser("a(){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_element_list_test_contains_02() {
    let (item, _) = ClassElementList::parse(&mut newparser("a(){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_element_list_test_contains_03() {
    let (item, _) = ClassElementList::parse(&mut newparser("a(){0;} b(){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_element_list_test_contains_04() {
    let (item, _) = ClassElementList::parse(&mut newparser("a(){} b(){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_element_list_test_contains_05() {
    let (item, _) = ClassElementList::parse(&mut newparser("a(){} b(){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_element_list_test_computed_property_contains_01() {
    let (item, _) = ClassElementList::parse(&mut newparser("[0](){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_element_list_test_computed_property_contains_02() {
    let (item, _) = ClassElementList::parse(&mut newparser("a(){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_element_list_test_computed_property_contains_03() {
    let (item, _) = ClassElementList::parse(&mut newparser("[0](){} b(){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_element_list_test_computed_property_contains_04() {
    let (item, _) = ClassElementList::parse(&mut newparser("a(){} [0](){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_element_list_test_computed_property_contains_05() {
    let (item, _) = ClassElementList::parse(&mut newparser("a(){0;} b(){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}

// CLASS ELEMENT
#[test]
fn class_element_test_01() {
    let (node, scanner) = check(ClassElement::parse(&mut newparser("a(){}"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    pretty_check(&*node, "ClassElement: a (  ) {  }", vec!["MethodDefinition: a (  ) {  }"]);
    concise_check(&*node, "MethodDefinition: a (  ) {  }", vec!["IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn class_element_test_02() {
    let (node, scanner) = check(ClassElement::parse(&mut newparser("static a(){}"), Scanner::new(), false, false));
    chk_scan(&scanner, 12);
    pretty_check(&*node, "ClassElement: static a (  ) {  }", vec!["MethodDefinition: a (  ) {  }"]);
    concise_check(&*node, "ClassElement: static a (  ) {  }", vec!["Keyword: static", "MethodDefinition: a (  ) {  }"]);
    format!("{:?}", node);
}
#[test]
fn class_element_test_03() {
    let (node, scanner) = check(ClassElement::parse(&mut newparser(";"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    pretty_check(&*node, "ClassElement: ;", vec![]);
    concise_check(&*node, "Punctuator: ;", vec![]);
    format!("{:?}", node);
}
#[test]
fn class_element_test_err_01() {
    check_err(ClassElement::parse(&mut newparser(""), Scanner::new(), false, false), "ClassElement expected", 1, 1);
}
#[test]
fn class_element_test_prettyerrors_1() {
    let (item, _) = ClassElementList::parse(&mut newparser("a(){}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_element_test_prettyerrors_2() {
    let (item, _) = ClassElementList::parse(&mut newparser("static a(){}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_element_test_prettyerrors_3() {
    let (item, _) = ClassElementList::parse(&mut newparser(";"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_element_test_conciseerrors_1() {
    let (item, _) = ClassElementList::parse(&mut newparser("a(){}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_element_test_conciseerrors_2() {
    let (item, _) = ClassElementList::parse(&mut newparser("static a(){}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_element_test_conciseerrors_3() {
    let (item, _) = ClassElementList::parse(&mut newparser(";"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_element_test_contains_01() {
    let (item, _) = ClassElement::parse(&mut newparser("a(){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_element_test_contains_02() {
    let (item, _) = ClassElement::parse(&mut newparser("a(){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_element_test_contains_03() {
    let (item, _) = ClassElement::parse(&mut newparser("static a(){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_element_test_contains_04() {
    let (item, _) = ClassElement::parse(&mut newparser("static a(){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_element_test_contains_05() {
    let (item, _) = ClassElement::parse(&mut newparser(";"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_element_test_computed_property_contains_01() {
    let (item, _) = ClassElement::parse(&mut newparser("[0](){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_element_test_computed_property_contains_02() {
    let (item, _) = ClassElement::parse(&mut newparser("a(){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_element_test_computed_property_contains_03() {
    let (item, _) = ClassElement::parse(&mut newparser("static [0](){}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_element_test_computed_property_contains_04() {
    let (item, _) = ClassElement::parse(&mut newparser("static a(){0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_element_test_computed_property_contains_05() {
    let (item, _) = ClassElement::parse(&mut newparser(";"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
