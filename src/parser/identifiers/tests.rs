use super::testhelp::{check, check_parse_error, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::{test_agent, unwind_syntax_error_object};
use ahash::AHashSet;

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

mod identifier {
    use super::*;
    mod early_errors {
        use super::*;
        use test_case::test_case;

        fn uify_first_ch(from: &str) -> String {
            let mut ch = from.chars();
            match ch.next() {
                None => String::new(),
                Some(code) => {
                    format!("\\u{{{:x}}}{}", code as usize, ch.as_str())
                }
            }
        }

        #[test_case("implements", true => Err(String::from("‘implements’ not allowed as an identifier in strict mode")); "implements strict")]
        #[test_case("interface", true => Err(String::from("‘interface’ not allowed as an identifier in strict mode")); "interface strict")]
        #[test_case("let", true => Err(String::from("‘let’ not allowed as an identifier in strict mode")); "let strict")]
        #[test_case("package", true => Err(String::from("‘package’ not allowed as an identifier in strict mode")); "package strict")]
        #[test_case("private", true => Err(String::from("‘private’ not allowed as an identifier in strict mode")); "private strict")]
        #[test_case("protected", true => Err(String::from("‘protected’ not allowed as an identifier in strict mode")); "protected strict")]
        #[test_case("public", true => Err(String::from("‘public’ not allowed as an identifier in strict mode")); "public strict")]
        #[test_case("static", true => Err(String::from("‘static’ not allowed as an identifier in strict mode")); "static strict")]
        #[test_case("yield", true => Err(String::from("‘yield’ not allowed as an identifier in strict mode")); "yield strict")]
        #[test_case("implements", false => Ok(()); "implements non-strict")]
        #[test_case("interface", false => Ok(()); "interface non-strict")]
        #[test_case("let", false => Ok(()); "let non-strict")]
        #[test_case("package", false => Ok(()); "package non-strict")]
        #[test_case("private", false => Ok(()); "private non-strict")]
        #[test_case("protected", false => Ok(()); "protected non-strict")]
        #[test_case("public", false => Ok(()); "public non-strict")]
        #[test_case("static", false => Ok(()); "static non-strict")]
        #[test_case("yield", false => Ok(()); "yield non-strict")]
        fn strict(id: &str, strict: bool) -> Result<(), String> {
            let mut agent = test_agent();
            let (identifier, _) = Identifier::parse(&mut newparser(uify_first_ch(id).as_str()), Scanner::new()).unwrap();
            let mut errs = vec![];
            identifier.early_errors(&mut agent, &mut errs, strict, false);
            if errs.is_empty() {
                Ok(())
            } else {
                assert_eq!(errs.len(), 1);
                Err(unwind_syntax_error_object(&mut agent, errs.swap_remove(0)))
            }
        }

        #[test_case("break" => String::from("‘break’ is a reserved word and may not be used as an identifier"); "keyword break")]
        #[test_case("case" => String::from("‘case’ is a reserved word and may not be used as an identifier"); "keyword case")]
        #[test_case("catch" => String::from("‘catch’ is a reserved word and may not be used as an identifier"); "keyword catch")]
        #[test_case("class" => String::from("‘class’ is a reserved word and may not be used as an identifier"); "keyword class")]
        #[test_case("const" => String::from("‘const’ is a reserved word and may not be used as an identifier"); "keyword const")]
        #[test_case("continue" => String::from("‘continue’ is a reserved word and may not be used as an identifier"); "keyword continue")]
        #[test_case("debugger" => String::from("‘debugger’ is a reserved word and may not be used as an identifier"); "keyword debugger")]
        #[test_case("default" => String::from("‘default’ is a reserved word and may not be used as an identifier"); "keyword default")]
        #[test_case("delete" => String::from("‘delete’ is a reserved word and may not be used as an identifier"); "keyword delete")]
        #[test_case("do" => String::from("‘do’ is a reserved word and may not be used as an identifier"); "keyword do")]
        #[test_case("else" => String::from("‘else’ is a reserved word and may not be used as an identifier"); "keyword else")]
        #[test_case("enum" => String::from("‘enum’ is a reserved word and may not be used as an identifier"); "keyword enum")]
        #[test_case("export" => String::from("‘export’ is a reserved word and may not be used as an identifier"); "keyword export")]
        #[test_case("extends" => String::from("‘extends’ is a reserved word and may not be used as an identifier"); "keyword extends")]
        #[test_case("false" => String::from("‘false’ is a reserved word and may not be used as an identifier"); "keyword false")]
        #[test_case("finally" => String::from("‘finally’ is a reserved word and may not be used as an identifier"); "keyword finally")]
        #[test_case("for" => String::from("‘for’ is a reserved word and may not be used as an identifier"); "keyword for")]
        #[test_case("function" => String::from("‘function’ is a reserved word and may not be used as an identifier"); "keyword function")]
        #[test_case("if" => String::from("‘if’ is a reserved word and may not be used as an identifier"); "keyword if")]
        #[test_case("import" => String::from("‘import’ is a reserved word and may not be used as an identifier"); "keyword import")]
        #[test_case("in" => String::from("‘in’ is a reserved word and may not be used as an identifier"); "keyword in")]
        #[test_case("instanceof" => String::from("‘instanceof’ is a reserved word and may not be used as an identifier"); "keyword instanceof")]
        #[test_case("new" => String::from("‘new’ is a reserved word and may not be used as an identifier"); "keyword new")]
        #[test_case("null" => String::from("‘null’ is a reserved word and may not be used as an identifier"); "keyword null")]
        #[test_case("return" => String::from("‘return’ is a reserved word and may not be used as an identifier"); "keyword return")]
        #[test_case("super" => String::from("‘super’ is a reserved word and may not be used as an identifier"); "keyword super")]
        #[test_case("switch" => String::from("‘switch’ is a reserved word and may not be used as an identifier"); "keyword switch")]
        #[test_case("this" => String::from("‘this’ is a reserved word and may not be used as an identifier"); "keyword this")]
        #[test_case("throw" => String::from("‘throw’ is a reserved word and may not be used as an identifier"); "keyword throw")]
        #[test_case("true" => String::from("‘true’ is a reserved word and may not be used as an identifier"); "keyword true")]
        #[test_case("try" => String::from("‘try’ is a reserved word and may not be used as an identifier"); "keyword try")]
        #[test_case("typeof" => String::from("‘typeof’ is a reserved word and may not be used as an identifier"); "keyword typeof")]
        #[test_case("var" => String::from("‘var’ is a reserved word and may not be used as an identifier"); "keyword var")]
        #[test_case("void" => String::from("‘void’ is a reserved word and may not be used as an identifier"); "keyword void")]
        #[test_case("while" => String::from("‘while’ is a reserved word and may not be used as an identifier"); "keyword while")]
        #[test_case("with" => String::from("‘with’ is a reserved word and may not be used as an identifier"); "keyword with")]
        fn keyword(id: &str) -> String {
            let mut agent = test_agent();
            let (identifier, _) = Identifier::parse(&mut newparser(uify_first_ch(id).as_str()), Scanner::new()).unwrap();
            let mut errs = vec![];
            identifier.early_errors(&mut agent, &mut errs, false, false);
            assert_eq!(errs.len(), 1);
            unwind_syntax_error_object(&mut agent, errs.swap_remove(0))
        }

        #[test_case("aw\\u0061it", true => Err(String::from("‘await’ not allowed as an identifier in modules")); "await in module")]
        #[test_case("aw\\u0061it", false => Ok(()); "await in script")]
        fn module(src: &str, in_module: bool) -> Result<(), String> {
            let mut agent = test_agent();
            let (ident, _) = Identifier::parse(&mut newparser(src), Scanner::new()).unwrap();
            let mut errs = vec![];
            ident.early_errors(&mut agent, &mut errs, false, in_module);
            if errs.is_empty() {
                Ok(())
            } else {
                assert_eq!(errs.len(), 1);
                Err(unwind_syntax_error_object(&mut agent, errs.swap_remove(0)))
            }
        }
    }
}
#[test]
fn identifier_test_nothing() {
    let result = Identifier::parse(&mut newparser("."), Scanner::new());
    check_parse_error(result, "Not an identifier");
}
#[test]
fn identifier_test_successful_bob() {
    let result = check(Identifier::parse(&mut Parser::new("bob", true, false, ParseGoal::Script), Scanner::new()));
    let (identifier, scanner) = result;
    chk_scan(&scanner, 3);
    let data = &identifier.name;
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
    let data = &identifier.name;
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
    assert_ne!(format!("{:?}", IdentifierReference { kind: IdentifierReferenceKind::Yield, strict: false, in_module: false, yield_flag: false, await_flag: false }), "");
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
mod identifier_reference {
    use super::*;
    mod early_errors {
        use super::*;
        use test_case::test_case;

        #[test_case("yield", true, true, false, true => AHashSet::from_iter(["identifier not allowed in strict mode: yield".to_string()]); "yield; strict/module/await")]
        #[test_case("yield", true, true, false, false => AHashSet::from_iter(["identifier not allowed in strict mode: yield".to_string()]); "yield; strict/module")]
        #[test_case("yield", true, false, false, true => AHashSet::from_iter(["identifier not allowed in strict mode: yield".to_string()]); "yield; strict/await")]
        #[test_case("yield", true, false, false, false => AHashSet::from_iter(["identifier not allowed in strict mode: yield".to_string()]); "yield; strict")]
        #[test_case("yield", false, true, false, true => AHashSet::<String>::new(); "yield; module/await")]
        #[test_case("yield", false, true, false, false => AHashSet::<String>::new(); "yield; module")]
        #[test_case("yield", false, false, false, true => AHashSet::<String>::new(); "yield; await")]
        #[test_case("yield", false, false, false, false => AHashSet::<String>::new(); "yield; ")]
        #[test_case("await", true, true, true, false => AHashSet::from_iter(["identifier not allowed in modules: await".to_string()]); "await; strict/module/yield")]
        #[test_case("await", true, true, false, false => AHashSet::from_iter(["identifier not allowed in modules: await".to_string()]); "await; strict/module")]
        #[test_case("await", true, false, true, false => AHashSet::<String>::new(); "await; strict/yield")]
        #[test_case("await", true, false, false, false => AHashSet::<String>::new(); "await; strict")]
        #[test_case("await", false, true, true, false => AHashSet::from_iter(["identifier not allowed in modules: await".to_string()]); "await; module/yield")]
        #[test_case("await", false, true, false, false => AHashSet::from_iter(["identifier not allowed in modules: await".to_string()]); "await; module")]
        #[test_case("await", false, false, true, false => AHashSet::<String>::new(); "await; yield")]
        #[test_case("await", false, false, false, false => AHashSet::<String>::new(); "await; ")]
        #[test_case("\\u{79}ield", true, true, true, true => AHashSet::from_iter([
            "identifier 'yield' not allowed when yield expressions are valid".to_string(), "‘yield’ not allowed as an identifier in strict mode".to_string()
        ]); "id-yield; strict/module/yield/await")]
        #[test_case("\\u{79}ield", true, true, true, false => AHashSet::from_iter([
            "identifier 'yield' not allowed when yield expressions are valid".to_string(), "‘yield’ not allowed as an identifier in strict mode".to_string()
        ]); "id-yield; strict/module/yield")]
        #[test_case("\\u{79}ield", true, true, false, true => AHashSet::from_iter(["‘yield’ not allowed as an identifier in strict mode".to_string()]); "id-yield; strict/module/await")]
        #[test_case("\\u{79}ield", true, true, false, false => AHashSet::from_iter(["‘yield’ not allowed as an identifier in strict mode".to_string()]); "id-yield; strict/module")]
        #[test_case("\\u{79}ield", true, false, true, true => AHashSet::from_iter([
            "identifier 'yield' not allowed when yield expressions are valid".to_string(), "‘yield’ not allowed as an identifier in strict mode".to_string()
        ]); "id-yield; strict/yield/await")]
        #[test_case("\\u{79}ield", true, false, true, false => AHashSet::from_iter([
            "identifier 'yield' not allowed when yield expressions are valid".to_string(), "‘yield’ not allowed as an identifier in strict mode".to_string()
        ]); "id-yield; strict/yield")]
        #[test_case("\\u{79}ield", true, false, false, true => AHashSet::from_iter(["‘yield’ not allowed as an identifier in strict mode".to_string()]); "id-yield; strict/await")]
        #[test_case("\\u{79}ield", true, false, false, false => AHashSet::from_iter(["‘yield’ not allowed as an identifier in strict mode".to_string()]); "id-yield; strict")]
        #[test_case("\\u{79}ield", false, true, true, true => AHashSet::from_iter(["identifier 'yield' not allowed when yield expressions are valid".to_string()]); "id-yield; module/yield/await")]
        #[test_case("\\u{79}ield", false, true, true, false => AHashSet::from_iter(["identifier 'yield' not allowed when yield expressions are valid".to_string()]); "id-yield; module/yield")]
        #[test_case("\\u{79}ield", false, true, false, true => AHashSet::<String>::new(); "id-yield; module/await")]
        #[test_case("\\u{79}ield", false, true, false, false => AHashSet::<String>::new(); "id-yield; module")]
        #[test_case("\\u{79}ield", false, false, true, true => AHashSet::from_iter(["identifier 'yield' not allowed when yield expressions are valid".to_string()]); "id-yield; yield/await")]
        #[test_case("\\u{79}ield", false, false, true, false => AHashSet::from_iter(["identifier 'yield' not allowed when yield expressions are valid".to_string()]); "id-yield; yield")]
        #[test_case("\\u{79}ield", false, false, false, true => AHashSet::<String>::new(); "id-yield; await")]
        #[test_case("\\u{79}ield", false, false, false, false => AHashSet::<String>::new(); "id-yield; ")]
        #[test_case("\\u{61}wait", true, true, true, true => AHashSet::from_iter(
            ["identifier 'await' not allowed when await expressions are valid".to_string(), "‘await’ not allowed as an identifier in modules".to_string()
        ]); "id-await; strict/module/yield/await")]
        #[test_case("\\u{61}wait", true, true, true, false => AHashSet::from_iter(["‘await’ not allowed as an identifier in modules".to_string()]); "id-await; strict/module/yield")]
        #[test_case("\\u{61}wait", true, true, false, true => AHashSet::from_iter([
            "identifier 'await' not allowed when await expressions are valid".to_string(), "‘await’ not allowed as an identifier in modules".to_string()
        ]); "id-await; strict/module/await")]
        #[test_case("\\u{61}wait", true, true, false, false => AHashSet::from_iter(["‘await’ not allowed as an identifier in modules".to_string()]); "id-await; strict/module")]
        #[test_case("\\u{61}wait", true, false, true, true => AHashSet::from_iter(["identifier 'await' not allowed when await expressions are valid".to_string()]); "id-await; strict/yield/await")]
        #[test_case("\\u{61}wait", true, false, true, false => AHashSet::<String>::new(); "id-await; strict/yield")]
        #[test_case("\\u{61}wait", true, false, false, true => AHashSet::from_iter(["identifier 'await' not allowed when await expressions are valid".to_string()]); "id-await; strict/await")]
        #[test_case("\\u{61}wait", true, false, false, false => AHashSet::<String>::new(); "id-await; strict")]
        #[test_case("\\u{61}wait", false, true, true, true => AHashSet::from_iter([
            "identifier 'await' not allowed when await expressions are valid".to_string(), "‘await’ not allowed as an identifier in modules".to_string()
        ]); "id-await; module/yield/await")]
        #[test_case("\\u{61}wait", false, true, true, false => AHashSet::from_iter(["‘await’ not allowed as an identifier in modules".to_string()]); "id-await; module/yield")]
        #[test_case("\\u{61}wait", false, true, false, true => AHashSet::from_iter([
            "identifier 'await' not allowed when await expressions are valid".to_string(), "‘await’ not allowed as an identifier in modules".to_string()
        ]); "id-await; module/await")]
        #[test_case("\\u{61}wait", false, true, false, false => AHashSet::from_iter(["‘await’ not allowed as an identifier in modules".to_string()]); "id-await; module")]
        #[test_case("\\u{61}wait", false, false, true, true => AHashSet::from_iter(["identifier 'await' not allowed when await expressions are valid".to_string()]); "id-await; yield/await")]
        #[test_case("\\u{61}wait", false, false, true, false => AHashSet::<String>::new(); "id-await; yield")]
        #[test_case("\\u{61}wait", false, false, false, true => AHashSet::from_iter(["identifier 'await' not allowed when await expressions are valid".to_string()]); "id-await; await")]
        #[test_case("\\u{61}wait", false, false, false, false => AHashSet::<String>::new(); "id-await; ")]
        fn f(src: &str, strict: bool, in_module: bool, yield_expr_allowed: bool, await_expr_allowed: bool) -> AHashSet<String> {
            let mut agent = test_agent();
            let goal = if in_module { ParseGoal::Module } else { ParseGoal::Script };
            let (item, _) = IdentifierReference::parse(&mut Parser::new(src, strict, false, goal), Scanner::new(), yield_expr_allowed, await_expr_allowed).unwrap();
            let mut errs = vec![];
            item.early_errors(&mut agent, &mut errs, strict);
            AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
        }
    }
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
mod binding_identifier {
    use super::*;
    mod early_errors {
        use super::*;
        use test_case::test_case;

        // This is arguably too many test cases. But with 4 booleans and 6 effective names, that's 2*2*2*2*6 = 96 combinations.
        // Which to cut? Meh. Just do them all.
        #[test_case("arguments", true, true, true, true => AHashSet::from_iter(["identifier not allowed in strict mode: arguments".to_string()]); "arguments; strict/module/yield/await")]
        #[test_case("arguments", true, true, true, false => AHashSet::from_iter(["identifier not allowed in strict mode: arguments".to_string()]); "arguments; strict/module/yield")]
        #[test_case("arguments", true, true, false, true => AHashSet::from_iter(["identifier not allowed in strict mode: arguments".to_string()]); "arguments; strict/module/await")]
        #[test_case("arguments", true, true, false, false => AHashSet::from_iter(["identifier not allowed in strict mode: arguments".to_string()]); "arguments; strict/module")]
        #[test_case("arguments", true, false, true, true => AHashSet::from_iter(["identifier not allowed in strict mode: arguments".to_string()]); "arguments; strict/yield/await")]
        #[test_case("arguments", true, false, true, false => AHashSet::from_iter(["identifier not allowed in strict mode: arguments".to_string()]); "arguments; strict/yield")]
        #[test_case("arguments", true, false, false, true => AHashSet::from_iter(["identifier not allowed in strict mode: arguments".to_string()]); "arguments; strict/await")]
        #[test_case("arguments", true, false, false, false => AHashSet::from_iter(["identifier not allowed in strict mode: arguments".to_string()]); "arguments; strict")]
        #[test_case("arguments", false, true, true, true => AHashSet::<String>::new(); "arguments; module/yield/await")]
        #[test_case("arguments", false, true, true, false => AHashSet::<String>::new(); "arguments; module/yield")]
        #[test_case("arguments", false, true, false, true => AHashSet::<String>::new(); "arguments; module/await")]
        #[test_case("arguments", false, true, false, false => AHashSet::<String>::new(); "arguments; module")]
        #[test_case("arguments", false, false, true, true => AHashSet::<String>::new(); "arguments; yield/await")]
        #[test_case("arguments", false, false, true, false => AHashSet::<String>::new(); "arguments; yield")]
        #[test_case("arguments", false, false, false, true => AHashSet::<String>::new(); "arguments; await")]
        #[test_case("arguments", false, false, false, false => AHashSet::<String>::new(); "arguments; ")]
        #[test_case("eval", true, true, true, true => AHashSet::from_iter(["identifier not allowed in strict mode: eval".to_string()]); "eval; strict/module/yield/await")]
        #[test_case("eval", true, true, true, false => AHashSet::from_iter(["identifier not allowed in strict mode: eval".to_string()]); "eval; strict/module/yield")]
        #[test_case("eval", true, true, false, true => AHashSet::from_iter(["identifier not allowed in strict mode: eval".to_string()]); "eval; strict/module/await")]
        #[test_case("eval", true, true, false, false => AHashSet::from_iter(["identifier not allowed in strict mode: eval".to_string()]); "eval; strict/module")]
        #[test_case("eval", true, false, true, true => AHashSet::from_iter(["identifier not allowed in strict mode: eval".to_string()]); "eval; strict/yield/await")]
        #[test_case("eval", true, false, true, false => AHashSet::from_iter(["identifier not allowed in strict mode: eval".to_string()]); "eval; strict/yield")]
        #[test_case("eval", true, false, false, true => AHashSet::from_iter(["identifier not allowed in strict mode: eval".to_string()]); "eval; strict/await")]
        #[test_case("eval", true, false, false, false => AHashSet::from_iter(["identifier not allowed in strict mode: eval".to_string()]); "eval; strict")]
        #[test_case("eval", false, true, true, true => AHashSet::<String>::new(); "eval; module/yield/await")]
        #[test_case("eval", false, true, true, false => AHashSet::<String>::new(); "eval; module/yield")]
        #[test_case("eval", false, true, false, true => AHashSet::<String>::new(); "eval; module/await")]
        #[test_case("eval", false, true, false, false => AHashSet::<String>::new(); "eval; module")]
        #[test_case("eval", false, false, true, true => AHashSet::<String>::new(); "eval; yield/await")]
        #[test_case("eval", false, false, true, false => AHashSet::<String>::new(); "eval; yield")]
        #[test_case("eval", false, false, false, true => AHashSet::<String>::new(); "eval; await")]
        #[test_case("eval", false, false, false, false => AHashSet::<String>::new(); "eval; ")]
        #[test_case("yield", true, true, true, true => AHashSet::from_iter([
            "identifier not allowed in strict mode: yield".to_string(), "identifier 'yield' not allowed when yield expressions are valid".to_string()
        ]); "yield; strict/module/yield/await")]
        #[test_case("yield", true, true, true, false => AHashSet::from_iter([
            "identifier not allowed in strict mode: yield".to_string(), "identifier 'yield' not allowed when yield expressions are valid".to_string()
        ]); "yield; strict/module/yield")]
        #[test_case("yield", true, true, false, true => AHashSet::from_iter(["identifier not allowed in strict mode: yield".to_string()]); "yield; strict/module/await")]
        #[test_case("yield", true, true, false, false => AHashSet::from_iter(["identifier not allowed in strict mode: yield".to_string()]); "yield; strict/module")]
        #[test_case("yield", true, false, true, true => AHashSet::from_iter([
            "identifier not allowed in strict mode: yield".to_string(), "identifier 'yield' not allowed when yield expressions are valid".to_string()
        ]); "yield; strict/yield/await")]
        #[test_case("yield", true, false, true, false => AHashSet::from_iter([
            "identifier not allowed in strict mode: yield".to_string(), "identifier 'yield' not allowed when yield expressions are valid".to_string()
        ]); "yield; strict/yield")]
        #[test_case("yield", true, false, false, true => AHashSet::from_iter(["identifier not allowed in strict mode: yield".to_string()]); "yield; strict/await")]
        #[test_case("yield", true, false, false, false => AHashSet::from_iter(["identifier not allowed in strict mode: yield".to_string()]); "yield; strict")]
        #[test_case("yield", false, true, true, true => AHashSet::from_iter(["identifier 'yield' not allowed when yield expressions are valid".to_string()]); "yield; module/yield/await")]
        #[test_case("yield", false, true, true, false => AHashSet::from_iter(["identifier 'yield' not allowed when yield expressions are valid".to_string()]); "yield; module/yield")]
        #[test_case("yield", false, true, false, true => AHashSet::<String>::new(); "yield; module/await")]
        #[test_case("yield", false, true, false, false => AHashSet::<String>::new(); "yield; module")]
        #[test_case("yield", false, false, true, true => AHashSet::from_iter(["identifier 'yield' not allowed when yield expressions are valid".to_string()]); "yield; yield/await")]
        #[test_case("yield", false, false, true, false => AHashSet::from_iter(["identifier 'yield' not allowed when yield expressions are valid".to_string()]); "yield; yield")]
        #[test_case("yield", false, false, false, true => AHashSet::<String>::new(); "yield; await")]
        #[test_case("yield", false, false, false, false => AHashSet::<String>::new(); "yield; ")]
        #[test_case("await", true, true, true, true => AHashSet::from_iter([
            "identifier not allowed in modules: await".to_string(), "identifier 'await' not allowed when await expressions are valid".to_string()
        ]); "await; strict/module/yield/await")]
        #[test_case("await", true, true, true, false => AHashSet::from_iter(["identifier not allowed in modules: await".to_string()]); "await; strict/module/yield")]
        #[test_case("await", true, true, false, true => AHashSet::from_iter([
            "identifier not allowed in modules: await".to_string(), "identifier 'await' not allowed when await expressions are valid".to_string()
        ]); "await; strict/module/await")]
        #[test_case("await", true, true, false, false => AHashSet::from_iter(["identifier not allowed in modules: await".to_string()]); "await; strict/module")]
        #[test_case("await", true, false, true, true => AHashSet::from_iter(["identifier 'await' not allowed when await expressions are valid".to_string()]); "await; strict/yield/await")]
        #[test_case("await", true, false, true, false => AHashSet::<String>::new(); "await; strict/yield")]
        #[test_case("await", true, false, false, true => AHashSet::from_iter(["identifier 'await' not allowed when await expressions are valid".to_string()]); "await; strict/await")]
        #[test_case("await", true, false, false, false => AHashSet::<String>::new(); "await; strict")]
        #[test_case("await", false, true, true, true => AHashSet::from_iter([
            "identifier not allowed in modules: await".to_string(), "identifier 'await' not allowed when await expressions are valid".to_string()
        ]); "await; module/yield/await")]
        #[test_case("await", false, true, true, false => AHashSet::from_iter(["identifier not allowed in modules: await".to_string()]); "await; module/yield")]
        #[test_case("await", false, true, false, true => AHashSet::from_iter([
            "identifier not allowed in modules: await".to_string(), "identifier 'await' not allowed when await expressions are valid".to_string()
        ]); "await; module/await")]
        #[test_case("await", false, true, false, false => AHashSet::from_iter(["identifier not allowed in modules: await".to_string()]); "await; module")]
        #[test_case("await", false, false, true, true => AHashSet::from_iter(["identifier 'await' not allowed when await expressions are valid".to_string()]); "await; yield/await")]
        #[test_case("await", false, false, true, false => AHashSet::<String>::new(); "await; yield")]
        #[test_case("await", false, false, false, true => AHashSet::from_iter(["identifier 'await' not allowed when await expressions are valid".to_string()]); "await; await")]
        #[test_case("await", false, false, false, false => AHashSet::<String>::new(); "await; ")]
        #[test_case("\\u{79}ield", true, true, true, true => AHashSet::from_iter([
            "identifier 'yield' not allowed when yield expressions are valid".to_string(), "‘yield’ not allowed as an identifier in strict mode".to_string()
        ]); "id-yield; strict/module/yield/await")]
        #[test_case("\\u{79}ield", true, true, true, false => AHashSet::from_iter([
            "identifier 'yield' not allowed when yield expressions are valid".to_string(), "‘yield’ not allowed as an identifier in strict mode".to_string()
        ]); "id-yield; strict/module/yield")]
        #[test_case("\\u{79}ield", true, true, false, true => AHashSet::from_iter(["‘yield’ not allowed as an identifier in strict mode".to_string()]); "id-yield; strict/module/await")]
        #[test_case("\\u{79}ield", true, true, false, false => AHashSet::from_iter(["‘yield’ not allowed as an identifier in strict mode".to_string()]); "id-yield; strict/module")]
        #[test_case("\\u{79}ield", true, false, true, true => AHashSet::from_iter([
            "identifier 'yield' not allowed when yield expressions are valid".to_string(), "‘yield’ not allowed as an identifier in strict mode".to_string()
        ]); "id-yield; strict/yield/await")]
        #[test_case("\\u{79}ield", true, false, true, false => AHashSet::from_iter([
            "identifier 'yield' not allowed when yield expressions are valid".to_string(), "‘yield’ not allowed as an identifier in strict mode".to_string()
        ]); "id-yield; strict/yield")]
        #[test_case("\\u{79}ield", true, false, false, true => AHashSet::from_iter(["‘yield’ not allowed as an identifier in strict mode".to_string()]); "id-yield; strict/await")]
        #[test_case("\\u{79}ield", true, false, false, false => AHashSet::from_iter(["‘yield’ not allowed as an identifier in strict mode".to_string()]); "id-yield; strict")]
        #[test_case("\\u{79}ield", false, true, true, true => AHashSet::from_iter(["identifier 'yield' not allowed when yield expressions are valid".to_string()]); "id-yield; module/yield/await")]
        #[test_case("\\u{79}ield", false, true, true, false => AHashSet::from_iter(["identifier 'yield' not allowed when yield expressions are valid".to_string()]); "id-yield; module/yield")]
        #[test_case("\\u{79}ield", false, true, false, true => AHashSet::<String>::new(); "id-yield; module/await")]
        #[test_case("\\u{79}ield", false, true, false, false => AHashSet::<String>::new(); "id-yield; module")]
        #[test_case("\\u{79}ield", false, false, true, true => AHashSet::from_iter(["identifier 'yield' not allowed when yield expressions are valid".to_string()]); "id-yield; yield/await")]
        #[test_case("\\u{79}ield", false, false, true, false => AHashSet::from_iter(["identifier 'yield' not allowed when yield expressions are valid".to_string()]); "id-yield; yield")]
        #[test_case("\\u{79}ield", false, false, false, true => AHashSet::<String>::new(); "id-yield; await")]
        #[test_case("\\u{79}ield", false, false, false, false => AHashSet::<String>::new(); "id-yield; ")]
        #[test_case("\\u{61}wait", true, true, true, true => AHashSet::from_iter(
            ["identifier 'await' not allowed when await expressions are valid".to_string(), "‘await’ not allowed as an identifier in modules".to_string()
        ]); "id-await; strict/module/yield/await")]
        #[test_case("\\u{61}wait", true, true, true, false => AHashSet::from_iter(["‘await’ not allowed as an identifier in modules".to_string()]); "id-await; strict/module/yield")]
        #[test_case("\\u{61}wait", true, true, false, true => AHashSet::from_iter([
            "identifier 'await' not allowed when await expressions are valid".to_string(), "‘await’ not allowed as an identifier in modules".to_string()
        ]); "id-await; strict/module/await")]
        #[test_case("\\u{61}wait", true, true, false, false => AHashSet::from_iter(["‘await’ not allowed as an identifier in modules".to_string()]); "id-await; strict/module")]
        #[test_case("\\u{61}wait", true, false, true, true => AHashSet::from_iter(["identifier 'await' not allowed when await expressions are valid".to_string()]); "id-await; strict/yield/await")]
        #[test_case("\\u{61}wait", true, false, true, false => AHashSet::<String>::new(); "id-await; strict/yield")]
        #[test_case("\\u{61}wait", true, false, false, true => AHashSet::from_iter(["identifier 'await' not allowed when await expressions are valid".to_string()]); "id-await; strict/await")]
        #[test_case("\\u{61}wait", true, false, false, false => AHashSet::<String>::new(); "id-await; strict")]
        #[test_case("\\u{61}wait", false, true, true, true => AHashSet::from_iter([
            "identifier 'await' not allowed when await expressions are valid".to_string(), "‘await’ not allowed as an identifier in modules".to_string()
        ]); "id-await; module/yield/await")]
        #[test_case("\\u{61}wait", false, true, true, false => AHashSet::from_iter(["‘await’ not allowed as an identifier in modules".to_string()]); "id-await; module/yield")]
        #[test_case("\\u{61}wait", false, true, false, true => AHashSet::from_iter([
            "identifier 'await' not allowed when await expressions are valid".to_string(), "‘await’ not allowed as an identifier in modules".to_string()
        ]); "id-await; module/await")]
        #[test_case("\\u{61}wait", false, true, false, false => AHashSet::from_iter(["‘await’ not allowed as an identifier in modules".to_string()]); "id-await; module")]
        #[test_case("\\u{61}wait", false, false, true, true => AHashSet::from_iter(["identifier 'await' not allowed when await expressions are valid".to_string()]); "id-await; yield/await")]
        #[test_case("\\u{61}wait", false, false, true, false => AHashSet::<String>::new(); "id-await; yield")]
        #[test_case("\\u{61}wait", false, false, false, true => AHashSet::from_iter(["identifier 'await' not allowed when await expressions are valid".to_string()]); "id-await; await")]
        #[test_case("\\u{61}wait", false, false, false, false => AHashSet::<String>::new(); "id-await; ")]
        fn f(src: &str, strict: bool, in_module: bool, yield_expr_allowed: bool, await_expr_allowed: bool) -> AHashSet<String> {
            let mut agent = test_agent();
            let goal = if in_module { ParseGoal::Module } else { ParseGoal::Script };
            let (item, _) = BindingIdentifier::parse(&mut Parser::new(src, strict, false, goal), Scanner::new(), yield_expr_allowed, await_expr_allowed).unwrap();
            let mut errs = vec![];
            item.early_errors(&mut agent, &mut errs, strict);
            AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
        }
    }
}

// LABEL IDENTIFIER
#[test]
fn label_identifier_test_normal_noyield_noawait() {
    let (lid, scanner) = check(LabelIdentifier::parse(&mut newparser("id"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    assert!(matches!(&lid.kind, LabelIdentifierKind::Identifier(_)));
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
    assert!(matches!(&lid.kind, LabelIdentifierKind::Identifier(_)));
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
    assert!(matches!(&lid.kind, LabelIdentifierKind::Identifier(_)));
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
    assert!(matches!(&lid.kind, LabelIdentifierKind::Identifier(_)));
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
    assert!(matches!(&lid.kind, LabelIdentifierKind::Yield));
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
    assert!(matches!(&lid.kind, LabelIdentifierKind::Yield));
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
    assert!(matches!(&lid.kind, LabelIdentifierKind::Await));
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
    assert!(matches!(&lid.kind, LabelIdentifierKind::Await));
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
mod label_identifier {
    use super::*;
    mod early_errors {
        use super::*;
        use test_case::test_case;

        #[test_case("yield", true, true, false, true => AHashSet::from_iter(["identifier not allowed in strict mode: yield".to_string()]); "yield; strict/module/await")]
        #[test_case("yield", true, true, false, false => AHashSet::from_iter(["identifier not allowed in strict mode: yield".to_string()]); "yield; strict/module")]
        #[test_case("yield", true, false, false, true => AHashSet::from_iter(["identifier not allowed in strict mode: yield".to_string()]); "yield; strict/await")]
        #[test_case("yield", true, false, false, false => AHashSet::from_iter(["identifier not allowed in strict mode: yield".to_string()]); "yield; strict")]
        #[test_case("yield", false, true, false, true => AHashSet::<String>::new(); "yield; module/await")]
        #[test_case("yield", false, true, false, false => AHashSet::<String>::new(); "yield; module")]
        #[test_case("yield", false, false, false, true => AHashSet::<String>::new(); "yield; await")]
        #[test_case("yield", false, false, false, false => AHashSet::<String>::new(); "yield; ")]
        #[test_case("await", true, true, true, false => AHashSet::from_iter(["identifier not allowed in modules: await".to_string()]); "await; strict/module/yield")]
        #[test_case("await", true, true, false, false => AHashSet::from_iter(["identifier not allowed in modules: await".to_string()]); "await; strict/module")]
        #[test_case("await", true, false, true, false => AHashSet::<String>::new(); "await; strict/yield")]
        #[test_case("await", true, false, false, false => AHashSet::<String>::new(); "await; strict")]
        #[test_case("await", false, true, true, false => AHashSet::from_iter(["identifier not allowed in modules: await".to_string()]); "await; module/yield")]
        #[test_case("await", false, true, false, false => AHashSet::from_iter(["identifier not allowed in modules: await".to_string()]); "await; module")]
        #[test_case("await", false, false, true, false => AHashSet::<String>::new(); "await; yield")]
        #[test_case("await", false, false, false, false => AHashSet::<String>::new(); "await; ")]
        #[test_case("\\u{79}ield", true, true, true, true => AHashSet::from_iter([
            "identifier 'yield' not allowed when yield expressions are valid".to_string(), "‘yield’ not allowed as an identifier in strict mode".to_string()
        ]); "id-yield; strict/module/yield/await")]
        #[test_case("\\u{79}ield", true, true, true, false => AHashSet::from_iter([
            "identifier 'yield' not allowed when yield expressions are valid".to_string(), "‘yield’ not allowed as an identifier in strict mode".to_string()
        ]); "id-yield; strict/module/yield")]
        #[test_case("\\u{79}ield", true, true, false, true => AHashSet::from_iter(["‘yield’ not allowed as an identifier in strict mode".to_string()]); "id-yield; strict/module/await")]
        #[test_case("\\u{79}ield", true, true, false, false => AHashSet::from_iter(["‘yield’ not allowed as an identifier in strict mode".to_string()]); "id-yield; strict/module")]
        #[test_case("\\u{79}ield", true, false, true, true => AHashSet::from_iter([
            "identifier 'yield' not allowed when yield expressions are valid".to_string(), "‘yield’ not allowed as an identifier in strict mode".to_string()
        ]); "id-yield; strict/yield/await")]
        #[test_case("\\u{79}ield", true, false, true, false => AHashSet::from_iter([
            "identifier 'yield' not allowed when yield expressions are valid".to_string(), "‘yield’ not allowed as an identifier in strict mode".to_string()
        ]); "id-yield; strict/yield")]
        #[test_case("\\u{79}ield", true, false, false, true => AHashSet::from_iter(["‘yield’ not allowed as an identifier in strict mode".to_string()]); "id-yield; strict/await")]
        #[test_case("\\u{79}ield", true, false, false, false => AHashSet::from_iter(["‘yield’ not allowed as an identifier in strict mode".to_string()]); "id-yield; strict")]
        #[test_case("\\u{79}ield", false, true, true, true => AHashSet::from_iter(["identifier 'yield' not allowed when yield expressions are valid".to_string()]); "id-yield; module/yield/await")]
        #[test_case("\\u{79}ield", false, true, true, false => AHashSet::from_iter(["identifier 'yield' not allowed when yield expressions are valid".to_string()]); "id-yield; module/yield")]
        #[test_case("\\u{79}ield", false, true, false, true => AHashSet::<String>::new(); "id-yield; module/await")]
        #[test_case("\\u{79}ield", false, true, false, false => AHashSet::<String>::new(); "id-yield; module")]
        #[test_case("\\u{79}ield", false, false, true, true => AHashSet::from_iter(["identifier 'yield' not allowed when yield expressions are valid".to_string()]); "id-yield; yield/await")]
        #[test_case("\\u{79}ield", false, false, true, false => AHashSet::from_iter(["identifier 'yield' not allowed when yield expressions are valid".to_string()]); "id-yield; yield")]
        #[test_case("\\u{79}ield", false, false, false, true => AHashSet::<String>::new(); "id-yield; await")]
        #[test_case("\\u{79}ield", false, false, false, false => AHashSet::<String>::new(); "id-yield; ")]
        #[test_case("\\u{61}wait", true, true, true, true => AHashSet::from_iter(
            ["identifier 'await' not allowed when await expressions are valid".to_string(), "‘await’ not allowed as an identifier in modules".to_string()
        ]); "id-await; strict/module/yield/await")]
        #[test_case("\\u{61}wait", true, true, true, false => AHashSet::from_iter(["‘await’ not allowed as an identifier in modules".to_string()]); "id-await; strict/module/yield")]
        #[test_case("\\u{61}wait", true, true, false, true => AHashSet::from_iter([
            "identifier 'await' not allowed when await expressions are valid".to_string(), "‘await’ not allowed as an identifier in modules".to_string()
        ]); "id-await; strict/module/await")]
        #[test_case("\\u{61}wait", true, true, false, false => AHashSet::from_iter(["‘await’ not allowed as an identifier in modules".to_string()]); "id-await; strict/module")]
        #[test_case("\\u{61}wait", true, false, true, true => AHashSet::from_iter(["identifier 'await' not allowed when await expressions are valid".to_string()]); "id-await; strict/yield/await")]
        #[test_case("\\u{61}wait", true, false, true, false => AHashSet::<String>::new(); "id-await; strict/yield")]
        #[test_case("\\u{61}wait", true, false, false, true => AHashSet::from_iter(["identifier 'await' not allowed when await expressions are valid".to_string()]); "id-await; strict/await")]
        #[test_case("\\u{61}wait", true, false, false, false => AHashSet::<String>::new(); "id-await; strict")]
        #[test_case("\\u{61}wait", false, true, true, true => AHashSet::from_iter([
            "identifier 'await' not allowed when await expressions are valid".to_string(), "‘await’ not allowed as an identifier in modules".to_string()
        ]); "id-await; module/yield/await")]
        #[test_case("\\u{61}wait", false, true, true, false => AHashSet::from_iter(["‘await’ not allowed as an identifier in modules".to_string()]); "id-await; module/yield")]
        #[test_case("\\u{61}wait", false, true, false, true => AHashSet::from_iter([
            "identifier 'await' not allowed when await expressions are valid".to_string(), "‘await’ not allowed as an identifier in modules".to_string()
        ]); "id-await; module/await")]
        #[test_case("\\u{61}wait", false, true, false, false => AHashSet::from_iter(["‘await’ not allowed as an identifier in modules".to_string()]); "id-await; module")]
        #[test_case("\\u{61}wait", false, false, true, true => AHashSet::from_iter(["identifier 'await' not allowed when await expressions are valid".to_string()]); "id-await; yield/await")]
        #[test_case("\\u{61}wait", false, false, true, false => AHashSet::<String>::new(); "id-await; yield")]
        #[test_case("\\u{61}wait", false, false, false, true => AHashSet::from_iter(["identifier 'await' not allowed when await expressions are valid".to_string()]); "id-await; await")]
        #[test_case("\\u{61}wait", false, false, false, false => AHashSet::<String>::new(); "id-await; ")]
        fn f(src: &str, strict: bool, in_module: bool, yield_expr_allowed: bool, await_expr_allowed: bool) -> AHashSet<String> {
            let mut agent = test_agent();
            let goal = if in_module { ParseGoal::Module } else { ParseGoal::Script };
            let (item, _) = LabelIdentifier::parse(&mut Parser::new(src, strict, false, goal), Scanner::new(), yield_expr_allowed, await_expr_allowed).unwrap();
            let mut errs = vec![];
            item.early_errors(&mut agent, &mut errs, strict);
            AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
        }
    }
}
