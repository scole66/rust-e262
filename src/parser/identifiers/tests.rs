use super::testhelp::{check, check_parse_error, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

fn id_kwd_test(kwd: &str) {
    let result = Identifier::parse(&mut newparser(kwd), Scanner::new());
    check_parse_error(result, format!("‘{}’ is a reserved word and may not be used as an identifier", kwd));
}
#[test]
fn identifier_test_pprint() {
    let pot_id = Identifier::parse(&mut newparser("phil"), Scanner::new());
    let (id, _) = pot_id.unwrap();
    pretty_check(&*id, "Identifier: phil", vec![]);
    concise_check(&*id, "IdentifierName: phil", vec![]);
}
#[test]
fn identifier_test_await() {
    id_kwd_test("await")
}
#[test]
fn identifier_test_break() {
    id_kwd_test("break")
}
#[test]
fn identifier_test_case() {
    id_kwd_test("case")
}
#[test]
fn identifier_test_catch() {
    id_kwd_test("catch")
}
#[test]
fn identifier_test_class() {
    id_kwd_test("class")
}
#[test]
fn identifier_test_const() {
    id_kwd_test("const")
}
#[test]
fn identifier_test_continue() {
    id_kwd_test("continue")
}
#[test]
fn identifier_test_debugger() {
    id_kwd_test("debugger")
}
#[test]
fn identifier_test_default() {
    id_kwd_test("default")
}
#[test]
fn identifier_test_delete() {
    id_kwd_test("delete")
}
#[test]
fn identifier_test_do() {
    id_kwd_test("do")
}
#[test]
fn identifier_test_else() {
    id_kwd_test("else")
}
#[test]
fn identifier_test_enum() {
    id_kwd_test("enum")
}
#[test]
fn identifier_test_export() {
    id_kwd_test("export")
}
#[test]
fn identifier_test_extends() {
    id_kwd_test("extends")
}
#[test]
fn identifier_test_false() {
    id_kwd_test("false")
}
#[test]
fn identifier_test_finally() {
    id_kwd_test("finally")
}
#[test]
fn identifier_test_for() {
    id_kwd_test("for")
}
#[test]
fn identifier_test_function() {
    id_kwd_test("function")
}
#[test]
fn identifier_test_if() {
    id_kwd_test("if")
}
#[test]
fn identifier_test_import() {
    id_kwd_test("import")
}
#[test]
fn identifier_test_in() {
    id_kwd_test("in")
}
#[test]
fn identifier_test_instanceof() {
    id_kwd_test("instanceof")
}
#[test]
fn identifier_test_new() {
    id_kwd_test("new")
}
#[test]
fn identifier_test_null() {
    id_kwd_test("null")
}
#[test]
fn identifier_test_return() {
    id_kwd_test("return")
}
#[test]
fn identifier_test_super() {
    id_kwd_test("super")
}
#[test]
fn identifier_test_switch() {
    id_kwd_test("switch")
}
#[test]
fn identifier_test_this() {
    id_kwd_test("this")
}
#[test]
fn identifier_test_throw() {
    id_kwd_test("throw")
}
#[test]
fn identifier_test_true() {
    id_kwd_test("true")
}
#[test]
fn identifier_test_try() {
    id_kwd_test("try")
}
#[test]
fn identifier_test_typeof() {
    id_kwd_test("typeof")
}
#[test]
fn identifier_test_var() {
    id_kwd_test("var")
}
#[test]
fn identifier_test_void() {
    id_kwd_test("void")
}
#[test]
fn identifier_test_while() {
    id_kwd_test("while")
}
#[test]
fn identifier_test_with() {
    id_kwd_test("with")
}
#[test]
fn identifier_test_yield() {
    id_kwd_test("yield")
}
#[test]
fn identifier_test_err() {
    let result = Identifier::parse(&mut newparser("iden\\u{20}tifier"), Scanner::new());
    check_parse_error(result, "Not an identifier");
}
fn identifier_test_strict(kwd: &str) {
    let result = Identifier::parse(&mut Parser::new(kwd, true, false, ParseGoal::Script), Scanner::new());
    check_parse_error(result, format!("‘{}’ not allowed as an identifier in strict mode", kwd));
}
#[test]
fn identifier_test_strict_implements() {
    identifier_test_strict("implements")
}
#[test]
fn identifier_test_strict_interface() {
    identifier_test_strict("interface")
}
#[test]
fn identifier_test_strict_let() {
    identifier_test_strict("let")
}
#[test]
fn identifier_test_strict_package() {
    identifier_test_strict("package")
}
#[test]
fn identifier_test_strict_private() {
    identifier_test_strict("private")
}
#[test]
fn identifier_test_strict_protected() {
    identifier_test_strict("protected")
}
#[test]
fn identifier_test_strict_public() {
    identifier_test_strict("public")
}
#[test]
fn identifier_test_strict_static() {
    identifier_test_strict("static")
}
#[test]
fn identifier_test_await_module() {
    let result = Identifier::parse(&mut Parser::new("aw\\u0061it", false, false, ParseGoal::Module), Scanner::new());
    check_parse_error(result, "‘await’ not allowed as an identifier in modules");
}
#[test]
fn identifier_test_nothing() {
    let result = Identifier::parse(&mut newparser("."), Scanner::new());
    check_parse_error(result, "Not an identifier");
}
fn identifier_test_keyword(kwd: &str) {
    let firstch = kwd.chars().next().unwrap();
    let id_src = format!("\\u{{{:x}}}{}", firstch as u32, &kwd[firstch.len_utf8()..]);
    let result = Identifier::parse(&mut newparser(&id_src), Scanner::new());
    check_parse_error(result, format!("‘{}’ is a reserved word and may not be used as an identifier", kwd));
}
#[test]
fn identifier_test_keyword_break() {
    identifier_test_keyword("break")
}
#[test]
fn identifier_test_keyword_case() {
    identifier_test_keyword("case")
}
#[test]
fn identifier_test_keyword_catch() {
    identifier_test_keyword("catch")
}
#[test]
fn identifier_test_keyword_class() {
    identifier_test_keyword("class")
}
#[test]
fn identifier_test_keyword_const() {
    identifier_test_keyword("const")
}
#[test]
fn identifier_test_keyword_continue() {
    identifier_test_keyword("continue")
}
#[test]
fn identifier_test_keyword_debugger() {
    identifier_test_keyword("debugger")
}
#[test]
fn identifier_test_keyword_default() {
    identifier_test_keyword("default")
}
#[test]
fn identifier_test_keyword_delete() {
    identifier_test_keyword("delete")
}
#[test]
fn identifier_test_keyword_do() {
    identifier_test_keyword("do")
}
#[test]
fn identifier_test_keyword_else() {
    identifier_test_keyword("else")
}
#[test]
fn identifier_test_keyword_enum() {
    identifier_test_keyword("enum")
}
#[test]
fn identifier_test_keyword_export() {
    identifier_test_keyword("export")
}
#[test]
fn identifier_test_keyword_extends() {
    identifier_test_keyword("extends")
}
#[test]
fn identifier_test_keyword_false() {
    identifier_test_keyword("false")
}
#[test]
fn identifier_test_keyword_finally() {
    identifier_test_keyword("finally")
}
#[test]
fn identifier_test_keyword_for() {
    identifier_test_keyword("for")
}
#[test]
fn identifier_test_keyword_function() {
    identifier_test_keyword("function")
}
#[test]
fn identifier_test_keyword_if() {
    identifier_test_keyword("if")
}
#[test]
fn identifier_test_keyword_import() {
    identifier_test_keyword("import")
}
#[test]
fn identifier_test_keyword_in() {
    identifier_test_keyword("in")
}
#[test]
fn identifier_test_keyword_instanceof() {
    identifier_test_keyword("instanceof")
}
#[test]
fn identifier_test_keyword_new() {
    identifier_test_keyword("new")
}
#[test]
fn identifier_test_keyword_null() {
    identifier_test_keyword("null")
}
#[test]
fn identifier_test_keyword_return() {
    identifier_test_keyword("return")
}
#[test]
fn identifier_test_keyword_super() {
    identifier_test_keyword("super")
}
#[test]
fn identifier_test_keyword_switch() {
    identifier_test_keyword("switch")
}
#[test]
fn identifier_test_keyword_this() {
    identifier_test_keyword("this")
}
#[test]
fn identifier_test_keyword_throw() {
    identifier_test_keyword("throw")
}
#[test]
fn identifier_test_keyword_true() {
    identifier_test_keyword("true")
}
#[test]
fn identifier_test_keyword_try() {
    identifier_test_keyword("try")
}
#[test]
fn identifier_test_keyword_typeof() {
    identifier_test_keyword("typeof")
}
#[test]
fn identifier_test_keyword_var() {
    identifier_test_keyword("var")
}
#[test]
fn identifier_test_keyword_void() {
    identifier_test_keyword("void")
}
#[test]
fn identifier_test_keyword_while() {
    identifier_test_keyword("while")
}
#[test]
fn identifier_test_keyword_with() {
    identifier_test_keyword("with")
}
#[test]
fn identifier_test_successful_bob() {
    let result = check(Identifier::parse(&mut Parser::new("bob", true, false, ParseGoal::Script), Scanner::new()));
    let (identifier, scanner) = result;
    chk_scan(&scanner, 3);
    let Identifier::IdentifierName(data) = &*identifier;
    assert!(data.string_value == "bob");
    assert!(data.keyword_id.is_none());
    assert!(data.line == 1);
    assert!(data.column == 1);
}
#[test]
fn identifier_test_successful_japanese() {
    let text = "手がける黒田征太郎さんです";
    let (identifier, scanner) = check(Identifier::parse(&mut Parser::new(text, true, false, ParseGoal::Script), Scanner::new()));
    assert!(scanner == Scanner { line: 1, column: 14, start_idx: 39 });
    let Identifier::IdentifierName(data) = &*identifier;
    assert!(data.string_value == "手がける黒田征太郎さんです");
    assert!(data.keyword_id.is_none());
    assert!(data.line == 1);
    assert!(data.column == 1);
}
#[test]
fn identifier_test_cache_01() {
    let mut parser = newparser("bnana");
    let (node, scanner) = Identifier::parse(&mut parser, Scanner::new()).unwrap();
    let (node2, scanner2) = Identifier::parse(&mut parser, Scanner::new()).unwrap();
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}

#[test]
fn identifier_reference_test_debug() {
    assert_eq!(format!("{:?}", IdentifierReference { kind: IdentifierReferenceKind::Yield, strict: false }), "IdentifierReference { kind: Yield, strict: false }");
}
fn idref_create(text: &str, strict: bool) -> Rc<IdentifierReference> {
    let yield_syntax = false;
    let await_syntax = false;
    let result = IdentifierReference::parse(&mut Parser::new(text, strict, false, ParseGoal::Script), Scanner::new(), yield_syntax, await_syntax);
    assert!(result.is_ok());
    let (idref, scanner) = result.unwrap();
    assert_eq!(scanner, Scanner { line: 1, column: text.len() as u32 + 1, start_idx: text.len() });
    idref
}

#[test]
fn identifier_reference_test_simple_success() {
    let idref = idref_create("identifier", false);
    assert!(!idref.strict);
    assert!(matches!(idref.kind, IdentifierReferenceKind::Identifier(..)));
    assert_eq!(idref.string_value(), "identifier");
    assert_eq!(idref.assignment_target_type(), ATTKind::Simple);
    assert_eq!(idref.contains(ParseNodeKind::Super), false);
    pretty_check(&*idref, "IdentifierReference: identifier", vec!["Identifier: identifier"]);
    concise_check(&*idref, "IdentifierName: identifier", vec![]);
}
#[test]
fn identifier_reference_test_yield() {
    let idref = idref_create("yield", false);
    assert!(!idref.strict);
    assert!(matches!(idref.kind, IdentifierReferenceKind::Yield));
    assert_eq!(idref.string_value(), "yield");
    assert_eq!(idref.assignment_target_type(), ATTKind::Simple);
    assert_eq!(idref.contains(ParseNodeKind::Super), false);
    pretty_check(&*idref, "IdentifierReference: yield", vec![]);
    concise_check(&*idref, "Keyword: yield", vec![]);
}
#[test]
fn identifier_reference_test_yield_02() {
    let idref = IdentifierReference::parse(&mut newparser("yield"), Scanner::new(), true, true);
    check_parse_error(idref, "‘yield’ is a reserved word and may not be used as an identifier");
}
#[test]
fn identifier_reference_test_await() {
    let idref = idref_create("await", false);
    assert!(!idref.strict);
    assert!(matches!(idref.kind, IdentifierReferenceKind::Await));
    assert_eq!(idref.string_value(), "await");
    assert_eq!(idref.assignment_target_type(), ATTKind::Simple);
    assert_eq!(idref.contains(ParseNodeKind::Super), false);
    pretty_check(&*idref, "IdentifierReference: await", vec![]);
    concise_check(&*idref, "Keyword: await", vec![]);
}
#[test]
fn identifier_reference_test_await_02() {
    let idref = IdentifierReference::parse(&mut newparser("await"), Scanner::new(), true, true);
    check_parse_error(idref, "‘await’ is a reserved word and may not be used as an identifier");
}
#[test]
fn identifier_reference_test_kwd() {
    let idref = IdentifierReference::parse(&mut newparser("new"), Scanner::new(), true, true);
    check_parse_error(idref, "‘new’ is a reserved word and may not be used as an identifier");
}
#[test]
fn identifier_reference_test_punct() {
    let idref = IdentifierReference::parse(&mut newparser("*"), Scanner::new(), true, true);
    check_parse_error(idref, "Not an identifier");
}
#[test]
fn identifier_reference_test_att_strict() {
    let idref = idref_create("abcd", true);
    assert_eq!(idref.string_value(), "abcd");
    assert_eq!(idref.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn identifier_reference_test_eval_strict() {
    let idref = idref_create("eval", true);
    assert_eq!(idref.string_value(), "eval");
    assert_eq!(idref.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn identifier_reference_test_eval_loose() {
    let idref = idref_create("eval", false);
    assert_eq!(idref.string_value(), "eval");
    assert_eq!(idref.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn identifier_reference_test_arguments_strict() {
    let idref = idref_create("arguments", true);
    assert_eq!(idref.string_value(), "arguments");
    assert_eq!(idref.assignment_target_type(), ATTKind::Invalid);
}
#[test]
fn identifier_reference_test_arguments_loose() {
    let idref = idref_create("arguments", false);
    assert_eq!(idref.string_value(), "arguments");
    assert_eq!(idref.assignment_target_type(), ATTKind::Simple);
}
#[test]
fn identifier_reference_prettycheck_1() {
    let (item, _) = IdentifierReference::parse(&mut newparser("yield"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn identifier_reference_prettycheck_2() {
    let (item, _) = IdentifierReference::parse(&mut newparser("await"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn identifier_reference_prettycheck_3() {
    let (item, _) = IdentifierReference::parse(&mut newparser("bob"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn identifier_reference_concisecheck_1() {
    let (item, _) = IdentifierReference::parse(&mut newparser("yield"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn identifier_reference_concisecheck_2() {
    let (item, _) = IdentifierReference::parse(&mut newparser("await"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn identifier_reference_concisecheck_3() {
    let (item, _) = IdentifierReference::parse(&mut newparser("bob"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn identifier_reference_test_cache_01() {
    let mut parser = newparser("bnana");
    let (node, scanner) = IdentifierReference::parse(&mut parser, Scanner::new(), false, false).unwrap();
    let (node2, scanner2) = IdentifierReference::parse(&mut parser, Scanner::new(), false, false).unwrap();
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}

fn bindingid_create(text: &str, y: bool, a: bool) -> Rc<BindingIdentifier> {
    let yield_syntax = y;
    let await_syntax = a;
    let strict = false;
    let result = BindingIdentifier::parse(&mut Parser::new(text, strict, false, ParseGoal::Script), Scanner::new(), yield_syntax, await_syntax);
    assert!(result.is_ok());
    let (bid, scanner) = result.unwrap();
    assert_eq!(scanner, Scanner { line: 1, column: text.len() as u32 + 1, start_idx: text.len() });
    bid
}

fn bid_allflags(text: &str) {
    for yflag in [false, true].iter() {
        for aflag in [false, true].iter() {
            let bid = bindingid_create(text, *yflag, *aflag);
            assert_eq!(bid.string_value(), text);
            assert_eq!(bid.bound_names(), [text]);
            assert!((bid.yield_flag && *yflag) || (!bid.yield_flag && !*yflag));
            assert!((bid.await_flag && *aflag) || (!bid.await_flag && !*aflag));
            assert_eq!(bid.contains(ParseNodeKind::Super), false);
        }
    }
}

#[test]
fn binding_identifier_test_normal() {
    bid_allflags("green");
}
#[test]
fn binding_identifier_test_yield() {
    bid_allflags("yield");
}
#[test]
fn binding_identifier_test_await() {
    bid_allflags("await");
}
#[test]
fn binding_identifier_test_pprint() {
    let b1 = bindingid_create("joe", false, false);
    pretty_check(&*b1, "BindingIdentifier: joe", vec!["Identifier: joe"]);
    concise_check(&*b1, "IdentifierName: joe", vec![]);
    let b2 = bindingid_create("yield", false, false);
    pretty_check(&*b2, "BindingIdentifier: yield", vec![]);
    concise_check(&*b2, "Keyword: yield", vec![]);
    let b3 = bindingid_create("await", false, false);
    pretty_check(&*b3, "BindingIdentifier: await", vec![]);
    concise_check(&*b3, "Keyword: await", vec![]);
}
#[test]
fn binding_identifier_test_debug() {
    format!("{:?}", bindingid_create("abcd", true, true));
}
#[test]
fn binding_identifier_test_non_matches() {
    let mut p1 = newparser("function");
    let r1 = BindingIdentifier::parse(&mut p1, Scanner::new(), false, false);
    check_parse_error(r1, "‘function’ is a reserved word and may not be used as an identifier");
    let mut p2 = newparser("*");
    let r2 = BindingIdentifier::parse(&mut p2, Scanner::new(), false, false);
    check_parse_error(r2, "Not an identifier");
}
#[test]
fn binding_identifier_prettycheck_1() {
    let (item, _) = BindingIdentifier::parse(&mut newparser("yield"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn binding_identifier_prettycheck_2() {
    let (item, _) = BindingIdentifier::parse(&mut newparser("await"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn binding_identifier_prettycheck_3() {
    let (item, _) = BindingIdentifier::parse(&mut newparser("bob"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn binding_identifier_concisecheck_1() {
    let (item, _) = BindingIdentifier::parse(&mut newparser("yield"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn binding_identifier_concisecheck_2() {
    let (item, _) = BindingIdentifier::parse(&mut newparser("await"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn binding_identifier_concisecheck_3() {
    let (item, _) = BindingIdentifier::parse(&mut newparser("bob"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}

#[test]
fn binding_identifier_test_cache_01() {
    let mut parser = newparser("bnana");
    let (node, scanner) = BindingIdentifier::parse(&mut parser, Scanner::new(), false, false).unwrap();
    let (node2, scanner2) = BindingIdentifier::parse(&mut parser, Scanner::new(), false, false).unwrap();
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}

// LABEL IDENTIFIER
#[test]
fn label_identifier_test_normal_noyield_noawait() {
    let (lid, scanner) = check(LabelIdentifier::parse(&mut newparser("id"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(&*lid, LabelIdentifier::Identifier(_)));
    assert_eq!(lid.string_value(), "id");
    assert_eq!(lid.contains(ParseNodeKind::Super), false);
    pretty_check(&*lid, "LabelIdentifier: id", vec!["Identifier: id"]);
    concise_check(&*lid, "IdentifierName: id", vec![]);
    format!("{:?}", lid);
}
#[test]
fn label_identifier_test_normal_yield_noawait() {
    let (lid, scanner) = check(LabelIdentifier::parse(&mut newparser("id"), Scanner::new(), true, false));
    chk_scan(&scanner, 2);
    assert!(matches!(&*lid, LabelIdentifier::Identifier(_)));
    assert_eq!(lid.string_value(), "id");
    assert_eq!(lid.contains(ParseNodeKind::Super), false);
    pretty_check(&*lid, "LabelIdentifier: id", vec!["Identifier: id"]);
    concise_check(&*lid, "IdentifierName: id", vec![]);
    format!("{:?}", lid);
}
#[test]
fn label_identifier_test_normal_noyield_await() {
    let (lid, scanner) = check(LabelIdentifier::parse(&mut newparser("id"), Scanner::new(), false, true));
    chk_scan(&scanner, 2);
    assert!(matches!(&*lid, LabelIdentifier::Identifier(_)));
    assert_eq!(lid.string_value(), "id");
    assert_eq!(lid.contains(ParseNodeKind::Super), false);
    pretty_check(&*lid, "LabelIdentifier: id", vec!["Identifier: id"]);
    concise_check(&*lid, "IdentifierName: id", vec![]);
    format!("{:?}", lid);
}
#[test]
fn label_identifier_test_normal_yield_await() {
    let (lid, scanner) = check(LabelIdentifier::parse(&mut newparser("id"), Scanner::new(), true, true));
    chk_scan(&scanner, 2);
    assert!(matches!(&*lid, LabelIdentifier::Identifier(_)));
    assert_eq!(lid.string_value(), "id");
    assert_eq!(lid.contains(ParseNodeKind::Super), false);
    pretty_check(&*lid, "LabelIdentifier: id", vec!["Identifier: id"]);
    concise_check(&*lid, "IdentifierName: id", vec![]);
    format!("{:?}", lid);
}
#[test]
fn label_identifier_test_yield_noyield_noawait() {
    let (lid, scanner) = check(LabelIdentifier::parse(&mut newparser("yield"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*lid, LabelIdentifier::Yield));
    assert_eq!(lid.string_value(), "yield");
    assert_eq!(lid.contains(ParseNodeKind::Super), false);
    pretty_check(&*lid, "LabelIdentifier: yield", vec![]);
    concise_check(&*lid, "Keyword: yield", vec![]);
    format!("{:?}", lid);
}
#[test]
fn label_identifier_test_yield_yield_noawait() {
    check_parse_error(LabelIdentifier::parse(&mut newparser("yield"), Scanner::new(), true, false), "‘yield’ is a reserved word and may not be used as an identifier");
}
#[test]
fn label_identifier_test_yield_noyield_await() {
    let (lid, scanner) = check(LabelIdentifier::parse(&mut newparser("yield"), Scanner::new(), false, true));
    chk_scan(&scanner, 5);
    assert!(matches!(&*lid, LabelIdentifier::Yield));
    assert_eq!(lid.string_value(), "yield");
    assert_eq!(lid.contains(ParseNodeKind::Super), false);
    pretty_check(&*lid, "LabelIdentifier: yield", vec![]);
    concise_check(&*lid, "Keyword: yield", vec![]);
    format!("{:?}", lid);
}
#[test]
fn label_identifier_test_yield_yield_await() {
    check_parse_error(LabelIdentifier::parse(&mut newparser("yield"), Scanner::new(), true, true), "‘yield’ is a reserved word and may not be used as an identifier");
}
#[test]
fn label_identifier_test_await_noyield_noawait() {
    let (lid, scanner) = check(LabelIdentifier::parse(&mut newparser("await"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*lid, LabelIdentifier::Await));
    assert_eq!(lid.string_value(), "await");
    assert_eq!(lid.contains(ParseNodeKind::Super), false);
    pretty_check(&*lid, "LabelIdentifier: await", vec![]);
    concise_check(&*lid, "Keyword: await", vec![]);
    format!("{:?}", lid);
}
#[test]
fn label_identifier_test_await_yield_noawait() {
    let (lid, scanner) = check(LabelIdentifier::parse(&mut newparser("await"), Scanner::new(), true, false));
    chk_scan(&scanner, 5);
    assert!(matches!(&*lid, LabelIdentifier::Await));
    assert_eq!(lid.string_value(), "await");
    assert_eq!(lid.contains(ParseNodeKind::Super), false);
    pretty_check(&*lid, "LabelIdentifier: await", vec![]);
    concise_check(&*lid, "Keyword: await", vec![]);
    format!("{:?}", lid);
}
#[test]
fn label_identifier_test_await_noyield_await() {
    check_parse_error(LabelIdentifier::parse(&mut newparser("await"), Scanner::new(), false, true), "‘await’ is a reserved word and may not be used as an identifier");
}
#[test]
fn label_identifier_test_await_yield_await() {
    check_parse_error(LabelIdentifier::parse(&mut newparser("await"), Scanner::new(), true, true), "‘await’ is a reserved word and may not be used as an identifier");
}
#[test]
fn label_identifier_prettycheck_1() {
    let (item, _) = LabelIdentifier::parse(&mut newparser("yield"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn label_identifier_prettycheck_2() {
    let (item, _) = LabelIdentifier::parse(&mut newparser("await"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn label_identifier_prettycheck_3() {
    let (item, _) = LabelIdentifier::parse(&mut newparser("bob"), Scanner::new(), false, false).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn label_identifier_concisecheck_1() {
    let (item, _) = LabelIdentifier::parse(&mut newparser("yield"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn label_identifier_concisecheck_2() {
    let (item, _) = LabelIdentifier::parse(&mut newparser("await"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn label_identifier_concisecheck_3() {
    let (item, _) = LabelIdentifier::parse(&mut newparser("bob"), Scanner::new(), false, false).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn label_identifier_test_cache_01() {
    let mut parser = newparser("bob");
    let (node, scanner) = LabelIdentifier::parse(&mut parser, Scanner::new(), false, false).unwrap();
    let (node2, scanner2) = LabelIdentifier::parse(&mut parser, Scanner::new(), false, false).unwrap();
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
