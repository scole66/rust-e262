use super::testhelp::{check, check_err, chk_scan, newparser, Maker};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::test_agent;
use test_case::test_case;

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
    check_err(ClassDeclaration::parse(&mut newparser("class"), Scanner::new(), false, false, false), "not an identifier", 1, 6);
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
#[test_case("class a { a(){item.#valid;} }" => true; "Named class valid")]
#[test_case("class { a(){item.#valid;} }" => true; "Unnamed class valid")]
#[test_case("class a { a(){item.#invalid;} }" => false; "Named class invalid")]
#[test_case("class { a(){item.#invalid;} }" => false; "Unnamed class invalid")]
fn class_declaration_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ClassDeclaration::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod class_declaration {
    use super::*;
    use test_case::test_case;

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ClassDeclaration::parse(&mut newparser("class {}"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![]);
    }

    #[test_case("class a { [arguments]; }" => true; "named (yes)")]
    #[test_case("class a {}" => false; "named (no)")]
    #[test_case("class { [arguments]; }" => true; "unnamed (yes)")]
    #[test_case("class {}" => false; "unnamed(no)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).class_declaration().contains_arguments()
    }
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
#[test_case("class a { a(){item.#valid;}}" => true; "valid")]
#[test_case("class a { a(){item.#invalid;}}" => false; "invalid")]
fn class_expression_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ClassExpression::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod class_expression {
    use super::*;
    use test_case::test_case;

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ClassExpression::parse(&mut newparser("class {}"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![]);
    }

    #[test_case("class a { [arguments]; }" => true; "named (yes)")]
    #[test_case("class a {}" => false; "named (no)")]
    #[test_case("class { [arguments]; }" => true; "unnamed (yes)")]
    #[test_case("class {}" => false; "unnamed(no)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).class_expression().contains_arguments()
    }
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
#[test_case("extends item.#valid { a(){0;}}" => true; "Heritage valid")]
#[test_case("extends a { a(){item.#valid;} }" => true; "Body valid")]
#[test_case("{ a(){item.#valid;} }" => true; "No Heritage; Body valid")]
#[test_case("extends item.#valid {}" => true; "No body; heritage valid")]
#[test_case("{}" => true; "No heritage, no body")]
#[test_case("extends item.#invalid { a(){0;}}" => false; "Heritage invalid")]
#[test_case("extends a { a(){item.#invalid;} }" => false; "Body invalid")]
#[test_case("{ a(){item.#invalid;} }" => false; "No Heritage; Body invalid")]
#[test_case("extends item.#invalid {}" => false; "No body; heritage invalid")]
fn class_tail_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ClassTail::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod class_tail {
    use super::*;
    use test_case::test_case;

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ClassTail::parse(&mut newparser("{}"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }

    #[test_case("{}" => false; "empty")]
    #[test_case("extends arguments {}" => true; "heritage only (yes)")]
    #[test_case("extends a {}" => false; "heritage only (no)")]
    #[test_case("{ [arguments]; }" => true; "body only (yes)")]
    #[test_case("{ a; }" => false; "body only (no)")]
    #[test_case("extends arguments { a; }" => true; "both (left)")]
    #[test_case("extends a { [arguments]; }" => true; "both (right)")]
    #[test_case("extends a { a; }" => false; "both (none)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).class_tail().contains_arguments()
    }
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
#[test_case("extends item.#valid" => true; "valid")]
#[test_case("extends item.#invalid" => false; "invalid")]
fn class_heritage_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ClassHeritage::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod class_heritage {
    use super::*;
    use test_case::test_case;

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ClassHeritage::parse(&mut newparser("extends a"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }

    #[test_case("extends arguments" => true; "yes")]
    #[test_case("extends a" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).class_heritage().contains_arguments()
    }
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
#[test_case("#a(){} b(){this.#a(); item.#valid();}" => true; "valid")]
#[test_case("#a(){} b(){this.#a(); item.#invalid();}" => false; "invalid")]
fn class_body_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ClassBody::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod class_body {
    use super::*;
    use test_case::test_case;

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ClassBody::parse(&mut newparser(";"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }

    #[test_case("[arguments];" => true; "yes")]
    #[test_case(";" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).class_body().contains_arguments()
    }

    #[test_case("a(){} #b(){} async *#c(){}" => vec![("#b".to_string(), IdUsage::Public), ("#c".to_string(), IdUsage::Public)]; "mix")]
    fn private_bound_identifiers(src: &str) -> Vec<(String, IdUsage)> {
        Maker::new(src).class_body().private_bound_identifiers().into_iter().map(|s| (String::from(s.name), s.usage)).collect::<Vec<_>>()
    }
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
#[test_case("a(){item.#valid;}" => true; "One item valid")]
#[test_case("a(){item.#valid;} b(){}" => true; "Multi first valid")]
#[test_case("a(){} b(){item.#valid;}" => true; "Multi second valid")]
#[test_case("a(){item.#invalid;}" => false; "One item invalid")]
#[test_case("a(){item.#invalid;} b(){}" => false; "Multi first invalid")]
#[test_case("a(){} b(){item.#invalid;}" => false; "Multi second invalid")]
fn class_element_list_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ClassElementList::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod class_element_list {
    use super::*;
    use test_case::test_case;

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ClassElementList::parse(&mut newparser("a(){}"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }

    #[test_case("[arguments];" => true; "item (yes)")]
    #[test_case("a;" => false; "item (no)")]
    #[test_case("[arguments]; a;" => true; "List (left)")]
    #[test_case("a; [arguments];" => true; "List (right)")]
    #[test_case("a; b;" => false; "List (none)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).class_element_list().contains_arguments()
    }
    #[test_case("#one_item(){}" => vec![("#one_item".to_string(), IdUsage::Public)]; "Item")]
    #[test_case("#a; #b; #c;" => vec![("#a".to_string(), IdUsage::Public), ("#b".to_string(), IdUsage::Public), ("#c".to_string(), IdUsage::Public)]; "List")]
    fn private_bound_identifiers(src: &str) -> Vec<(String, IdUsage)> {
        Maker::new(src).class_element_list().private_bound_identifiers().into_iter().map(|s| (String::from(s.name), s.usage)).collect::<Vec<_>>()
    }
}

// CLASS ELEMENT
#[test]
fn class_element_test_01() {
    let (node, scanner) = check(ClassElement::parse(&mut newparser("a(){}"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    pretty_check(&*node, "ClassElement: a (  ) {  }", vec!["MethodDefinition: a (  ) {  }"]);
    concise_check(&*node, "MethodDefinition: a (  ) {  }", vec!["IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"]);
    assert_ne!(format!("{:?}", node), "");
}
#[test]
fn class_element_test_02() {
    let (node, scanner) = check(ClassElement::parse(&mut newparser("static a(){}"), Scanner::new(), false, false));
    chk_scan(&scanner, 12);
    pretty_check(&*node, "ClassElement: static a (  ) {  }", vec!["MethodDefinition: a (  ) {  }"]);
    concise_check(&*node, "ClassElement: static a (  ) {  }", vec!["Keyword: static", "MethodDefinition: a (  ) {  }"]);
    assert_ne!(format!("{:?}", node), "");
}
#[test]
fn class_element_test_03() {
    let (node, scanner) = check(ClassElement::parse(&mut newparser(";"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    pretty_check(&*node, "ClassElement: ;", vec![]);
    concise_check(&*node, "Punctuator: ;", vec![]);
    assert_ne!(format!("{:?}", node), "");
}
#[test]
fn class_element_test_04() {
    let (node, scanner) = check(ClassElement::parse(&mut newparser("a;"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    pretty_check(&*node, "ClassElement: a ;", vec!["FieldDefinition: a"]);
    concise_check(&*node, "ClassElement: a ;", vec!["IdentifierName: a", "Punctuator: ;"]);
    assert_ne!(format!("{:?}", node), "");
}
#[test]
fn class_element_test_05() {
    let (node, scanner) = check(ClassElement::parse(&mut newparser("static a;"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "ClassElement: static a ;", vec!["FieldDefinition: a"]);
    concise_check(&*node, "ClassElement: static a ;", vec!["Keyword: static", "IdentifierName: a", "Punctuator: ;"]);
    assert_ne!(format!("{:?}", node), "");
}
#[test]
fn class_element_test_06() {
    let (node, scanner) = check(ClassElement::parse(&mut newparser("static;"), Scanner::new(), false, false));
    chk_scan(&scanner, 7);
    pretty_check(&*node, "ClassElement: static ;", vec!["FieldDefinition: static"]);
    concise_check(&*node, "ClassElement: static ;", vec!["IdentifierName: static", "Punctuator: ;"]);
    assert_ne!(format!("{:?}", node), "");
}
#[test]
fn class_element_test_07() {
    let (node, scanner) = check(ClassElement::parse(&mut newparser("static {}"), Scanner::new(), false, false));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "ClassElement: static {  }", vec!["ClassStaticBlock: static {  }"]);
    concise_check(&*node, "ClassStaticBlock: static {  }", vec!["Keyword: static", "Punctuator: {", "Punctuator: }"]);
    assert_ne!(format!("{:?}", node), "");
}
#[test]
fn class_element_test_err_01() {
    check_err(ClassElement::parse(&mut newparser(""), Scanner::new(), false, false), "ClassElement expected", 1, 1);
}
#[test]
fn class_element_test_prettyerrors_1() {
    let (item, _) = ClassElement::parse(&mut newparser("a(){}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_element_test_prettyerrors_2() {
    let (item, _) = ClassElement::parse(&mut newparser("static a(){}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_element_test_prettyerrors_3() {
    let (item, _) = ClassElement::parse(&mut newparser(";"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_element_test_prettyerrors_4() {
    let (item, _) = ClassElement::parse(&mut newparser("a;"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_element_test_prettyerrors_5() {
    let (item, _) = ClassElement::parse(&mut newparser("static a;"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_element_test_prettyerrors_6() {
    let (item, _) = ClassElement::parse(&mut newparser("static {a;}"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_element_test_conciseerrors_1() {
    let (item, _) = ClassElement::parse(&mut newparser("a(){}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_element_test_conciseerrors_2() {
    let (item, _) = ClassElement::parse(&mut newparser("static a(){}"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_element_test_conciseerrors_3() {
    let (item, _) = ClassElement::parse(&mut newparser(";"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_element_test_conciseerrors_4() {
    let (item, _) = ClassElement::parse(&mut newparser("a;"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_element_test_conciseerrors_5() {
    let (item, _) = ClassElement::parse(&mut newparser("static a;"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_element_test_conciseerrors_6() {
    let (item, _) = ClassElement::parse(&mut newparser("static {a;}"), Scanner::new(), false, false).unwrap();
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
fn class_element_test_contains_06() {
    let (item, _) = ClassElement::parse(&mut newparser("a;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_element_test_contains_07() {
    let (item, _) = ClassElement::parse(&mut newparser("[0];"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_element_test_contains_08() {
    let (item, _) = ClassElement::parse(&mut newparser("static a;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_element_test_contains_09() {
    let (item, _) = ClassElement::parse(&mut newparser("static a=0;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_element_test_contains_10() {
    let (item, _) = ClassElement::parse(&mut newparser("static {a=0};"), Scanner::new(), true, true).unwrap();
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
#[test]
fn class_element_test_computed_property_contains_06() {
    let (item, _) = ClassElement::parse(&mut newparser("a;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_element_test_computed_property_contains_07() {
    let (item, _) = ClassElement::parse(&mut newparser("[0];"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_element_test_computed_property_contains_08() {
    let (item, _) = ClassElement::parse(&mut newparser("static a;"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_element_test_computed_property_contains_09() {
    let (item, _) = ClassElement::parse(&mut newparser("static [0];"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_element_test_computed_property_contains_10() {
    let (item, _) = ClassElement::parse(&mut newparser("static {0;}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test_case(";" => true; "Empty")]
#[test_case("a(){item.#valid;}" => true; "Method valid")]
#[test_case("static a(){item.#valid;}" => true; "Static method valid")]
#[test_case("a=item.#valid;" => true; "Field valid")]
#[test_case("static a=item.#valid;" => true; "Static field valid")]
#[test_case("static { item.#valid; }" => true; "Static body valid")]
#[test_case("a(){item.#invalid;}" => false; "Method invalid")]
#[test_case("static a(){item.#invalid;}" => false; "Static method invalid")]
#[test_case("a=item.#invalid;" => false; "Field invalid")]
#[test_case("static a=item.#invalid;" => false; "Static field invalid")]
#[test_case("static { item.#invalid; }" => false; "Static body invalid")]
fn class_element_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ClassElement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod class_element {
    use super::*;
    use test_case::test_case;

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ClassElement::parse(&mut newparser("a(){}"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }

    #[test_case("[arguments](){}" => true; "Method (yes)")]
    #[test_case("a(){}" => false; "Method (no)")]
    #[test_case("static [arguments](){}" => true; "Static Method (yes)")]
    #[test_case("static a(){}" => false; "Static Method (no)")]
    #[test_case("[arguments];" => true; "Field (yes)")]
    #[test_case("a;" => false; "Field (no)")]
    #[test_case("static [arguments];" => true; "Static Field (yes)")]
    #[test_case("static a;" => false; "Static Field (no)")]
    #[test_case("static { arguments; }" => true; "Static Block (yes)")]
    #[test_case("static {}" => false; "Static Block (no)")]
    #[test_case(";" => false; "semi")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).class_element().contains_arguments()
    }
    #[test_case(";" => None; "Empty")]
    #[test_case("static { do_thing(); }" => None; "Static Block")]
    #[test_case("#method(){}" => Some(("#method".to_string(), IdUsage::Public)); "Method")]
    #[test_case("static #sm(){}" => Some(("#sm".to_string(), IdUsage::Static)); "Static Method")]
    #[test_case("#field=77;" => Some(("#field".to_string(), IdUsage::Public)); "Field")]
    #[test_case("static #sf=88;" => Some(("#sf".to_string(), IdUsage::Static)); "Static Field")]
    #[test_case("get #getter(){}" => Some(("#getter".to_string(), IdUsage::Getter)); "Getter")]
    #[test_case("set #setter(x){}" => Some(("#setter".to_string(), IdUsage::Setter)); "Setter")]
    #[test_case("static get #getter(){}" => Some(("#getter".to_string(), IdUsage::StaticGetter)); "Static Getter")]
    #[test_case("static set #setter(x){}" => Some(("#setter".to_string(), IdUsage::StaticSetter)); "Static Setter")]
    #[test_case("method(){}" => None; "Method (public)")]
    #[test_case("static sm(){}" => None; "Static Method (public)")]
    #[test_case("field=77;" => None; "Field (public)")]
    #[test_case("static sf=88;" => None; "Static Field (public)")]
    #[test_case("get getter(){}" => None; "Getter (public)")]
    #[test_case("set setter(x){}" => None; "Setter (public)")]
    #[test_case("static get getter(){}" => None; "Static Getter (public)")]
    #[test_case("static set setter(x){}" => None; "Static Setter (public)")]
    fn private_bound_identifier(src: &str) -> Option<(String, IdUsage)> {
        Maker::new(src).class_element().private_bound_identifier().map(|id| (String::from(id.name), id.usage))
    }
}

// FIELD DEFINITION
#[test]
fn field_definition_test_01() {
    let (node, scanner) = check(FieldDefinition::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    pretty_check(&*node, "FieldDefinition: a", vec!["ClassElementName: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    assert_ne!(format!("{:?}", node), "");
}
#[test]
fn field_definition_test_02() {
    let (node, scanner) = check(FieldDefinition::parse(&mut newparser("a=0"), Scanner::new(), false, false));
    chk_scan(&scanner, 3);
    pretty_check(&*node, "FieldDefinition: a = 0", vec!["ClassElementName: a", "Initializer: = 0"]);
    concise_check(&*node, "FieldDefinition: a = 0", vec!["IdentifierName: a", "Initializer: = 0"]);
    assert_ne!(format!("{:?}", node), "");
}
#[test]
fn field_definition_test_err_01() {
    check_err(FieldDefinition::parse(&mut newparser(""), Scanner::new(), false, false), "ClassElementName expected", 1, 1);
}
#[test]
fn field_definition_test_prettyerrors_1() {
    let (item, _) = FieldDefinition::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn field_definition_test_prettyerrors_2() {
    let (item, _) = FieldDefinition::parse(&mut newparser("a=0"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn field_definition_test_conciseerrors_1() {
    let (item, _) = FieldDefinition::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn field_definition_test_conciseerrors_2() {
    let (item, _) = FieldDefinition::parse(&mut newparser("a=0"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn field_definition_test_contains_01() {
    let (item, _) = FieldDefinition::parse(&mut newparser("[0]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn field_definition_test_contains_02() {
    let (item, _) = FieldDefinition::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn field_definition_test_contains_03() {
    let (item, _) = FieldDefinition::parse(&mut newparser("[0]=a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn field_definition_test_contains_04() {
    let (item, _) = FieldDefinition::parse(&mut newparser("a=0"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn field_definition_test_contains_05() {
    let (item, _) = FieldDefinition::parse(&mut newparser("a=b"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn field_definition_test_computed_property_contains_01() {
    let (item, _) = FieldDefinition::parse(&mut newparser("[0]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn field_definition_test_computed_property_contains_02() {
    let (item, _) = FieldDefinition::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test]
fn field_definition_test_computed_property_contains_03() {
    let (item, _) = FieldDefinition::parse(&mut newparser("a=0"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test_case("[item.#valid]" => true; "No init valid")]
#[test_case("[item.#valid]=0" => true; "Name valid")]
#[test_case("a=item.#valid" => true; "Initializer valid")]
#[test_case("[item.#invalid]" => false; "No init invalid")]
#[test_case("[item.#invalid]=0" => false; "Name invalid")]
#[test_case("a=item.#invalid" => false; "Initializer invalid")]
fn field_definition_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = FieldDefinition::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod field_definition {
    use super::*;
    use test_case::test_case;

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        FieldDefinition::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }

    #[test_case("[arguments]" => true; "name (yes)")]
    #[test_case("a" => false; "name (no)")]
    #[test_case("[arguments]=a" => true; "initialized (left)")]
    #[test_case("a=arguments" => true; "initialized (right)")]
    #[test_case("a=b" => false; "initialized (none)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).field_definition().contains_arguments()
    }
    #[test_case("#private" => Some("#private".to_string()); "private")]
    #[test_case("simple" => None; "public")]
    fn private_bound_identifier(src: &str) -> Option<String> {
        Maker::new(src).field_definition().private_bound_identifier().map(String::from)
    }
}

// CLASS ELEMENT NAME
#[test]
fn class_element_name_test_01() {
    let (node, scanner) = check(ClassElementName::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    pretty_check(&*node, "ClassElementName: a", vec!["PropertyName: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    assert_ne!(format!("{:?}", node), "");
}
#[test]
fn class_element_name_test_02() {
    let (node, scanner) = check(ClassElementName::parse(&mut newparser("#a"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    pretty_check(&*node, "ClassElementName: #a", vec!["PrivateIdentifier: #a"]);
    concise_check(&*node, "PrivateIdentifier: #a", vec![]);
    assert_ne!(format!("{:?}", node), "");
}
#[test]
fn class_element_name_test_err_01() {
    check_err(ClassElementName::parse(&mut newparser(""), Scanner::new(), false, false), "ClassElementName expected", 1, 1);
}
#[test]
fn class_element_name_test_prettyerrors_1() {
    let (item, _) = ClassElementName::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_element_name_test_prettyerrors_2() {
    let (item, _) = ClassElementName::parse(&mut newparser("#a"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn class_element_name_test_conciseerrors_1() {
    let (item, _) = ClassElementName::parse(&mut newparser("a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_element_name_test_conciseerrors_2() {
    let (item, _) = ClassElementName::parse(&mut newparser("#a"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn class_element_name_test_contains_01() {
    let (item, _) = ClassElementName::parse(&mut newparser("[0]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_element_name_test_contains_02() {
    let (item, _) = ClassElementName::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_element_name_test_contains_03() {
    let (item, _) = ClassElementName::parse(&mut newparser("#a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_element_name_test_computed_property_contains_01() {
    let (item, _) = ClassElementName::parse(&mut newparser("[0]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), true);
}
#[test]
fn class_element_name_test_computed_property_contains_02() {
    let (item, _) = ClassElementName::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}
#[test]
fn class_element_name_test_computed_property_contains_03() {
    let (item, _) = ClassElementName::parse(&mut newparser("#a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.computed_property_contains(ParseNodeKind::Literal), false);
}

#[test_case("#a" => true; "PrivateId")]
#[test_case("[item.#valid]" => true; "Name valid")]
#[test_case("[item.#invalid]" => false; "Name invalid")]
fn class_element_name_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ClassElementName::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod class_element_name {
    use super::*;
    use test_case::test_case;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ClassElementName::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }

    #[test_case("a" => Some(JSString::from("a")); "normal")]
    #[test_case("#a" => None; "private")]
    fn prop_name(src: &str) -> Option<JSString> {
        let (item, _) = ClassElementName::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
        item.prop_name()
    }

    #[test_case("[arguments]" => true; "name (yes)")]
    #[test_case("a" => false; "name (no)")]
    #[test_case("#a" => false; "pid")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).class_element_name().contains_arguments()
    }

    #[test_case("public" => None; "PropertyName")]
    #[test_case("#private" => Some("#private".to_string()); "PrivateIdentifier")]
    fn private_bound_identifier(src: &str) -> Option<String> {
        Maker::new(src).class_element_name().private_bound_identifier().map(String::from)
    }
}

mod class_static_block {
    // CLASS STATIC BLOCK
    use super::*;
    use test_case::test_case;
    #[test]
    fn parse() {
        let (node, scanner) = check(ClassStaticBlock::parse(&mut newparser("static { 0; }"), Scanner::new()));
        chk_scan(&scanner, 13);
        pretty_check(&*node, "ClassStaticBlock: static { 0 ; }", vec!["ClassStaticBlockBody: 0 ;"]);
        concise_check(&*node, "ClassStaticBlock: static { 0 ; }", vec!["Keyword: static", "Punctuator: {", "ExpressionStatement: 0 ;", "Punctuator: }"]);
        assert_ne!(format!("{:?}", node), "");
    }
    #[test_case("", "‘static’ expected", 1; "Empty Source")]
    #[test_case("static", "‘{’ expected", 7; "Missing Leading Brace")]
    #[test_case("static {", "‘}’ expected", 9; "Missing Trailing Brace")]
    fn err(src: &str, expected_error: &str, err_column: u32) {
        check_err(ClassStaticBlock::parse(&mut newparser(src), Scanner::new()), expected_error, 1, err_column);
    }
    #[test]
    fn contains() {
        let (node, _) = check(ClassStaticBlock::parse(&mut newparser("static { 0; }"), Scanner::new()));
        assert_eq!(node.contains(), false);
    }
    #[test]
    fn prettyerrors() {
        let (item, _) = ClassStaticBlock::parse(&mut newparser("static { 0; }"), Scanner::new()).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors() {
        let (item, _) = ClassStaticBlock::parse(&mut newparser("static { 0; }"), Scanner::new()).unwrap();
        concise_error_validate(&*item);
    }
    #[test_case("static { item.#valid; }" => true; "valid")]
    #[test_case("static { item.#invalid; }" => false; "invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = ClassStaticBlock::parse(&mut newparser(src), Scanner::new()).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ClassStaticBlock::parse(&mut newparser("static { a; }"), Scanner::new()).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }

    #[test_case("static { arguments; }" => true; "yes")]
    #[test_case("static {}" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).class_static_block().contains_arguments()
    }
}

mod class_static_block_body {
    // CLASS STATIC BLOCK BODY
    use super::*;
    use test_case::test_case;
    #[test]
    fn parse() {
        let (node, scanner) = ClassStaticBlockBody::parse(&mut newparser("0;"), Scanner::new());
        chk_scan(&scanner, 2);
        pretty_check(&*node, "ClassStaticBlockBody: 0 ;", vec!["ClassStaticBlockStatementList: 0 ;"]);
        concise_check(&*node, "ExpressionStatement: 0 ;", vec!["Numeric: 0", "Punctuator: ;"]);
        assert_ne!(format!("{:?}", node), "");
    }
    #[test]
    fn prettyerrors() {
        let (item, _) = ClassStaticBlockBody::parse(&mut newparser("0;"), Scanner::new());
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors() {
        let (item, _) = ClassStaticBlockBody::parse(&mut newparser("0;"), Scanner::new());
        concise_error_validate(&*item);
    }
    #[test_case("item.#valid;" => true; "valid")]
    #[test_case("item.#invalid;" => false; "invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = ClassStaticBlockBody::parse(&mut newparser(src), Scanner::new());
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ClassStaticBlockBody::parse(&mut newparser("a;"), Scanner::new()).0.early_errors(&mut test_agent(), &mut vec![], true);
    }

    #[test_case("arguments;" => true; "yes")]
    #[test_case("" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).class_static_block_body().contains_arguments()
    }
}

mod class_static_block_statement_list {
    // CLASS STATIC BLOCK STATEMENT LIST
    use super::*;
    use test_case::test_case;
    #[test]
    fn parse_01() {
        let (node, scanner) = ClassStaticBlockStatementList::parse(&mut newparser("0;"), Scanner::new());
        chk_scan(&scanner, 2);
        pretty_check(&*node, "ClassStaticBlockStatementList: 0 ;", vec!["StatementList: 0 ;"]);
        concise_check(&*node, "ExpressionStatement: 0 ;", vec!["Numeric: 0", "Punctuator: ;"]);
        assert_ne!(format!("{:?}", node), "");
    }
    #[test]
    fn parse_02() {
        let (node, scanner) = ClassStaticBlockStatementList::parse(&mut newparser(""), Scanner::new());
        chk_scan(&scanner, 0);
        pretty_check(&*node, "ClassStaticBlockStatementList: ", vec![]);
        concise_check(&*node, "", vec![]);
        assert_ne!(format!("{:?}", node), "");
    }
    #[test_case("0;"; "Has statements")]
    #[test_case(""; "Empty")]
    fn prettyerrors(src: &str) {
        let (item, _) = ClassStaticBlockStatementList::parse(&mut newparser(src), Scanner::new());
        pretty_error_validate(&*item);
    }
    #[test_case("0;"; "Has statements")]
    #[test_case(""; "Empty")]
    fn conciseerrors(src: &str) {
        let (item, _) = ClassStaticBlockStatementList::parse(&mut newparser(src), Scanner::new());
        concise_error_validate(&*item);
    }
    #[test_case("" => true; "empty")]
    #[test_case("item.#valid;" => true; "valid")]
    #[test_case("item.#invalid;" => false; "invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = ClassStaticBlockStatementList::parse(&mut newparser(src), Scanner::new());
        item.all_private_identifiers_valid(&[JSString::from("#valid")])
    }
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ClassStaticBlockStatementList::parse(&mut newparser("a;"), Scanner::new()).0.early_errors(&mut test_agent(), &mut vec![], true);
    }

    #[test_case("", &[] => false; "empty")]
    #[test_case("a;", &["b"] => false; "stmt (no labels)")]
    #[test_case("b: a;", &["b"] => true; "stmt (dup'd label)")]
    fn contains_duplicate_labels(src: &str, label_set: &[&str]) -> bool {
        let l_set = label_set.iter().map(|&s| JSString::from(s)).collect::<Vec<_>>();
        Maker::new(src).class_static_block_statement_list().contains_duplicate_labels(&l_set)
    }

    #[test_case("", &[] => false; "empty")]
    #[test_case("break x;", &[] => true; "bare break")]
    #[test_case("break x;", &["x"] => false; "break (labelset)")]
    fn contains_undefined_break_target(src: &str, label_set: &[&str]) -> bool {
        let l_set = label_set.iter().map(|&s| JSString::from(s)).collect::<Vec<JSString>>();
        Maker::new(src).class_static_block_statement_list().contains_undefined_break_target(&l_set)
    }

    #[test_case("", &[], &[] => false; "empty")]
    #[test_case("continue x;", &[], &[] => true; "bare continue")]
    #[test_case("continue x;", &["x"], &[] => false; "ok continue (iterset)")]
    #[test_case("for (;;) { continue x; }", &[], &["x"] => false; "ok continue (labelset)")]
    fn contains_undefined_continue_target(src: &str, iteration_set: &[&str], label_set: &[&str]) -> bool {
        let i_set = iteration_set.iter().map(|&s| JSString::from(s)).collect::<Vec<JSString>>();
        let l_set = label_set.iter().map(|&s| JSString::from(s)).collect::<Vec<JSString>>();
        Maker::new(src).class_static_block_statement_list().contains_undefined_continue_target(&i_set, &l_set)
    }

    #[test_case("" => false; "empty")]
    #[test_case("arguments;" => true; "stmt (yes)")]
    #[test_case("a;" => false; "stmt (no)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).class_static_block_statement_list().contains_arguments()
    }

    #[test_case("", ParseNodeKind::StatementList => false; "empty; has statementlist?")]
    #[test_case("a=b;", ParseNodeKind::StatementList => true; "statement; has statementlist?")]
    #[test_case("a=this;", ParseNodeKind::This => true; "statement with this; has this?")]
    #[test_case("a=b;", ParseNodeKind::This => false; "statement without this; has this?")]
    fn contains(src: &str, kind: ParseNodeKind) -> bool {
        Maker::new(src).class_static_block_statement_list().contains(kind)
    }
}
