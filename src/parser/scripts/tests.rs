use super::testhelp::*;
use super::*;
use crate::prettyprint::testhelp::*;
use crate::tests::*;
use ahash::AHashSet;

const UNDEF_BREAK: &str = "undefined break target detected";

mod var_scope_decl {
    use super::*;
    use test_case::test_case;

    #[test_case(HoistableDeclPart::FunctionDeclaration(Maker::new("function a(){}").function_declaration()) => "function a (  ) {  }"; "function decl")]
    #[test_case(HoistableDeclPart::GeneratorDeclaration(Maker::new("function *a(){}").generator_declaration()) => "function * a (  ) {  }"; "generator decl")]
    #[test_case(HoistableDeclPart::AsyncFunctionDeclaration(Maker::new("async function a(){}").async_function_declaration()) => "async function a (  ) {  }"; "async function decl")]
    #[test_case(HoistableDeclPart::AsyncGeneratorDeclaration(Maker::new("async function *a(){}").async_generator_declaration()) => "async function * a (  ) {  }"; "async generator decl")]
    fn from_hoistable(part: HoistableDeclPart) -> String {
        VarScopeDecl::from(part).to_string()
    }

    #[test_case(VarScopeDecl::FunctionDeclaration(Maker::new("function a(){}").function_declaration()) => "function a (  ) {  }"; "function decl")]
    #[test_case(VarScopeDecl::GeneratorDeclaration(Maker::new("function *a(){}").generator_declaration()) => "function * a (  ) {  }"; "generator decl")]
    #[test_case(VarScopeDecl::AsyncFunctionDeclaration(Maker::new("async function a(){}").async_function_declaration()) => "async function a (  ) {  }"; "async function decl")]
    #[test_case(VarScopeDecl::AsyncGeneratorDeclaration(Maker::new("async function *a(){}").async_generator_declaration()) => "async function * a (  ) {  }"; "async generator decl")]
    #[test_case(VarScopeDecl::VariableDeclaration(Maker::new("a").variable_declaration()) => "a"; "var decl")]
    #[test_case(VarScopeDecl::ForBinding(Maker::new("a").for_binding()) => "a"; "for binding")]
    fn display(part: VarScopeDecl) -> String {
        part.to_string()
    }

    #[test_case(VarScopeDecl::FunctionDeclaration(Maker::new("function a(){}").function_declaration()) => "function a (  ) {  }"; "function decl")]
    #[test_case(VarScopeDecl::GeneratorDeclaration(Maker::new("function *a(){}").generator_declaration()) => "function * a (  ) {  }"; "generator decl")]
    #[test_case(VarScopeDecl::AsyncFunctionDeclaration(Maker::new("async function a(){}").async_function_declaration()) => "async function a (  ) {  }"; "async function decl")]
    #[test_case(VarScopeDecl::AsyncGeneratorDeclaration(Maker::new("async function *a(){}").async_generator_declaration()) => "async function * a (  ) {  }"; "async generator decl")]
    #[test_case(VarScopeDecl::VariableDeclaration(Maker::new("a").variable_declaration()) => "a"; "var decl")]
    #[test_case(VarScopeDecl::ForBinding(Maker::new("a").for_binding()) => "a"; "for binding")]
    fn string_from(part: VarScopeDecl) -> String {
        String::from(&part)
    }

    #[test_case(VarScopeDecl::FunctionDeclaration(Maker::new("function a(){}").function_declaration()) => with |s| assert_ne!(s, ""); "function decl")]
    #[test_case(VarScopeDecl::GeneratorDeclaration(Maker::new("function *a(){}").generator_declaration()) => with |s| assert_ne!(s, ""); "generator decl")]
    #[test_case(VarScopeDecl::AsyncFunctionDeclaration(Maker::new("async function a(){}").async_function_declaration()) => with |s| assert_ne!(s, ""); "async function decl")]
    #[test_case(VarScopeDecl::AsyncGeneratorDeclaration(Maker::new("async function *a(){}").async_generator_declaration()) => with |s| assert_ne!(s, ""); "async generator decl")]
    #[test_case(VarScopeDecl::VariableDeclaration(Maker::new("a").variable_declaration()) => with |s| assert_ne!(s, ""); "var decl")]
    #[test_case(VarScopeDecl::ForBinding(Maker::new("a").for_binding()) => with |s| assert_ne!(s, ""); "for binding")]
    fn debug(part: VarScopeDecl) -> String {
        format!("{:?}", part)
    }

    #[test]
    fn clone() {
        let vsd = VarScopeDecl::FunctionDeclaration(Maker::new("function a(){grape;}").function_declaration());
        let copy = vsd.clone();
        assert_eq!(vsd.to_string(), copy.to_string());
    }
}

// SCRIPT
#[test]
fn script_test_01() {
    let (node, scanner) =
        check(Script::parse(&mut newparser("let a=1; /* a bunch more text */\n/* even new lines */"), Scanner::new()));
    assert_eq!(scanner.line, 2);
    assert_eq!(scanner.column, 21);
    assert_eq!(scanner.start_idx, 53);
    pretty_check(&*node, "Script: let a = 1 ;", vec!["ScriptBody: let a = 1 ;"]);
    concise_check(
        &*node,
        "LexicalDeclaration: let a = 1 ;",
        vec!["Keyword: let", "LexicalBinding: a = 1", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn script_test_02() {
    let (node, scanner) = check(Script::parse(&mut newparser(""), Scanner::new()));
    chk_scan(&scanner, 0);
    pretty_check(&*node, "Script: ", vec![]);
    concise_check(&*node, "Script:", vec![]);
    format!("{:?}", node);
}
#[test]
fn script_test_err_01() {
    check_err(Script::parse(&mut newparser("for"), Scanner::new()), "‘(’ expected", 1, 4);
}
#[test]
fn script_test_err_02() {
    check_err(Script::parse(&mut newparser("0;if;"), Scanner::new()), "end-of-file expected", 1, 3);
}
#[test]
fn script_test_prettyerrors_1() {
    let (item, _) = Script::parse(&mut newparser("null;"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn script_test_prettyerrors_2() {
    let (item, _) = Script::parse(&mut newparser(""), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn script_test_conciseerrors_1() {
    let (item, _) = Script::parse(&mut newparser("null;"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn script_test_conciseerrors_2() {
    let (item, _) = Script::parse(&mut newparser(""), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn script_test_contains_01() {
    let (item, _) = Script::parse(&mut newparser(""), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::ScriptBody), false);
}
#[test]
fn script_test_contains_02() {
    let (item, _) = Script::parse(&mut newparser("0;"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::ScriptBody), true);
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn script_test_contains_03() {
    let (item, _) = Script::parse(&mut newparser("a;"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::ScriptBody), true);
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}

mod script {
    use super::*;
    use test_case::test_case;

    #[test_case("" => sset(&[]); "emptyness")]
    #[test_case("0;" => sset(&[]); "Statement")]
    #[test_case("'use strict'; package;" => sset(&[PACKAGE_NOT_ALLOWED]); "strict expression")]
    #[test_case("package;" => sset(&[]); "non-strict")]
    #[test_case("let x; const x=10;" => sset(&[DUPLICATE_LEXICAL]); "lex duplicated")]
    #[test_case("let x; var x=10;" => sset(&[LEX_DUPED_BY_VAR]); "lex duped by var")]
    #[test_case("break a;" => sset(&[UNDEF_BREAK]); "undefined break target")]
    fn early_errors(src: &str) -> AHashSet<String> {
        let agent = test_agent();
        let mut errs = vec![];
        Script::parse(&mut newparser(src), Scanner::new()).unwrap().0.early_errors(&agent, &mut errs);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&agent, err.clone())))
    }

    #[test_case("" => svec(&[]); "empty")]
    #[test_case("let a; const pi=3; var alice; function bob(){}" => svec(&["a", "pi"]); "statement list")]
    fn lexically_declared_names(src: &str) -> Vec<String> {
        Maker::new(src).script().lexically_declared_names().into_iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("" => svec(&[]); "empty")]
    #[test_case("let a; const pi=3; var alice; function bob(){}" => svec(&["alice", "bob"]); "statement list")]
    fn var_declared_names(src: &str) -> Vec<String> {
        Maker::new(src).script().var_declared_names().into_iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("" => svec(&[]); "empty")]
    #[test_case("var a; function b(){}" => svec(&["a", "function b (  ) {  }"]); "statements")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).script().var_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("" => svec(&[]); "empty")]
    #[test_case("var a; function b(){} class q{} const h=0;" => svec(&["class q { }", "const h = 0 ;"]); "statement list")]
    fn lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).script().lexically_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("" => Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 0 } }; "empty")]
    #[test_case("56;" => Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 3 } }; "no whitespace")]
    #[test_case("// startup\n99;\n// tail\n" => Location { starting_line: 1, starting_column: 1, span: Span { starting_index: 0, length: 23 } }; "multiline; leading & trailing ws")]
    fn location(src: &str) -> Location {
        Maker::new(src).script().location()
    }
}

// SCRIPT BODY
#[test]
fn script_body_test_01() {
    let (node, scanner) = check(ScriptBody::parse(&mut newparser("let a=1;"), Scanner::new()));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "ScriptBody: let a = 1 ;", vec!["StatementList: let a = 1 ;"]);
    concise_check(
        &*node,
        "LexicalDeclaration: let a = 1 ;",
        vec!["Keyword: let", "LexicalBinding: a = 1", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn script_body_test_err_01() {
    check_err(ScriptBody::parse(&mut newparser(""), Scanner::new()), "Declaration or Statement expected", 1, 1);
}
#[test]
fn script_body_test_prettyerrors_1() {
    let (item, _) = ScriptBody::parse(&mut newparser(";"), Scanner::new()).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn script_body_test_conciseerrors_1() {
    let (item, _) = ScriptBody::parse(&mut newparser(";"), Scanner::new()).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn script_body_test_contains_01() {
    let (item, _) = ScriptBody::parse(&mut newparser("0;"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::StatementList), true);
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn script_body_test_contains_02() {
    let (item, _) = ScriptBody::parse(&mut newparser(";"), Scanner::new()).unwrap();
    assert_eq!(item.contains(ParseNodeKind::StatementList), true);
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}

#[test]
fn script_body_test_directive_prologue_01() {
    let (item, _) = ScriptBody::parse(
        &mut newparser("'blue'; 'green'; 'orange'; 'use\\x20strict'; print(12.0); 'dinosaur';"),
        Scanner::new(),
    )
    .unwrap();

    let dp = item.directive_prologue();
    assert_eq!(
        dp,
        vec![
            StringToken { value: JSString::from("blue"), delimiter: StringDelimiter::Single, raw: None },
            StringToken { value: JSString::from("green"), delimiter: StringDelimiter::Single, raw: None },
            StringToken { value: JSString::from("orange"), delimiter: StringDelimiter::Single, raw: None },
            StringToken {
                value: JSString::from("use strict"),
                delimiter: StringDelimiter::Single,
                raw: Some(String::from("use\\x20strict"))
            }
        ]
    );
}
#[test]
fn script_body_test_directive_prologue_02() {
    let (item, _) = ScriptBody::parse(&mut newparser("print(12.0); 'dinosaur';"), Scanner::new()).unwrap();

    let dp = item.directive_prologue();
    assert_eq!(dp, &[]);
}
mod script_body {
    use super::*;
    use test_case::test_case;

    #[test_case("a;" => false; "no prologue")]
    #[test_case("'a'; func();" => false; "prologue without clause")]
    #[test_case("'a'; 'use strict'; b();" => true; "prologue with clause")]
    #[test_case("'a'; 'use\\x20strict'; b();" => false; "prologue with escape")]
    fn contains_use_strict(src: &str) -> bool {
        ScriptBody::parse(&mut newparser(src), Scanner::new()).unwrap().0.contains_use_strict()
    }

    fn directparser(src: &str, direct: bool) -> Parser {
        let mut p = newparser(src);
        p.direct = direct;
        p
    }

    const SUPER_DISALLOWED: &str = "`super' not allowed in top-level code";
    const NEWTARG_DISALLOWED: &str = "`new.target` not allowed in top-level code";
    const DUPLICATE_LABELS: &str = "duplicate labels detected";

    #[test_case("super();", false => sset(&[SUPER_DISALLOWED]); "disallowed super")]
    #[test_case("super();", true => sset(&[]); "allowed super")]
    #[test_case("new.target;", false => sset(&[NEWTARG_DISALLOWED]); "disallowed new.target")]
    #[test_case("new.target;", true => sset(&[]); "allowed new.target")]
    #[test_case("break a;", false => sset(&[UNDEF_BREAK]); "undefined break")]
    #[test_case(";", false => sset(&[]); "empty stmt")]
    #[test_case("t:{t:;}", false => sset(&[DUPLICATE_LABELS]); "duplicate labels")]
    #[test_case("continue bob;", false => sset(&[CONTINUE_ITER, "undefined continue target detected"]); "undefined continue")]
    #[test_case("a.#mystery;", false => sset(&["invalid private identifier detected"]); "invalid private id")]
    fn early_errors(src: &str, direct: bool) -> AHashSet<String> {
        let agent = test_agent();
        let mut errs = vec![];
        ScriptBody::parse(&mut directparser(src, direct), Scanner::new()).unwrap().0.early_errors(&agent, &mut errs);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&agent, err.clone())))
    }

    #[test_case("var a; function b(){}" => svec(&["a", "function b (  ) {  }"]); "statements")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).script_body().var_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("class q{}" => svec(&["class q { }"]); "statements")]
    fn lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).script_body().lexically_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("/* header comment */\n    function a(){}\n    a();\n/* And that's all she wrote! */\n" => Location { starting_line: 2, starting_column: 5, span:Span { starting_index: 25, length: 23} }; "leading/trailing ws")]
    fn location(src: &str) -> Location {
        Maker::new(src).script_body().location()
    }

    #[test_case("var a; const q=1; function b(){}" => svec(&["a", "b"]); "names")]
    fn var_declared_names(src: &str) -> Vec<String> {
        Maker::new(src).script_body().var_declared_names().into_iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("let a; var b; const c=0; function foo(){}" => svec(&["a", "c"]); "names")]
    fn lexically_declared_names(src: &str) -> Vec<String> {
        Maker::new(src).script_body().lexically_declared_names().into_iter().map(String::from).collect::<Vec<_>>()
    }
}
