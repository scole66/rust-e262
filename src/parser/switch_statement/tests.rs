use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

#[test]
fn switch_statement_test_01() {
    let (node, scanner) =
        check(SwitchStatement::parse(&mut newparser("switch (0) { default: 0;}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 25);
    pretty_check(
        &*node,
        "SwitchStatement: switch ( 0 ) { default : 0 ; }",
        &["Expression: 0", "CaseBlock: { default : 0 ; }"],
    );
    concise_check(
        &*node,
        "SwitchStatement: switch ( 0 ) { default : 0 ; }",
        &["Keyword: switch", "Punctuator: (", "Numeric: 0", "Punctuator: )", "CaseBlock: { default : 0 ; }"],
    );
    format!("{node:?}");
}
#[test]
fn switch_statement_test_err() {
    check_err(
        SwitchStatement::parse(&mut newparser(""), Scanner::new(), false, false, true),
        "‘switch’ expected",
        1,
        1,
    );
    check_err(
        SwitchStatement::parse(&mut newparser("switch"), Scanner::new(), false, false, true),
        "‘(’ expected",
        1,
        7,
    );
    check_err(
        SwitchStatement::parse(&mut newparser("switch("), Scanner::new(), false, false, true),
        "Expression expected",
        1,
        8,
    );
    check_err(
        SwitchStatement::parse(&mut newparser("switch(0"), Scanner::new(), false, false, true),
        "‘)’ expected",
        1,
        9,
    );
    check_err(
        SwitchStatement::parse(&mut newparser("switch(0)"), Scanner::new(), false, false, true),
        "‘{’ expected",
        1,
        10,
    );
}
#[test]
fn switch_statement_test_printer_errs() {
    let src = "switch(0){default:0;}";
    let (item, _) = SwitchStatement::parse(&mut newparser(src), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
    concise_error_validate(&*item);
}
#[test]
fn switch_statement_test_var_declared_names() {
    let src = "switch(0){default: var a;}";
    let (item, _) = SwitchStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a"]);
}
#[test]
fn switch_statement_test_contains_undefined_break_target() {
    let src = "switch(0){default: break t;}";
    let (item, _) = SwitchStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
fn switch_contains_check(src: &str, has_literal: bool) {
    let (item, _) = SwitchStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), has_literal);
}
#[test]
fn switch_statement_test_contains() {
    switch_contains_check("switch(a){default: ;}", false);
    switch_contains_check("switch(0){default: ;}", true);
    switch_contains_check("switch(a){default: 0;}", true);
}
#[test]
fn switch_statement_test_contains_duplicate_labels() {
    let (item, _) =
        SwitchStatement::parse(&mut newparser("switch(a){default:t:;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
}
#[test_case("switch (a) { case 3: continue x; }" => (false, true); "switch (a) { case 3: continue x; }")]
#[test_case("switch (a) { case 3: for (;;) continue x; }" => (false, true); "switch (a) { case 3: for (;;) continue x; }")]
fn switch_statement_test_contains_undefined_continue_target(src: &str) -> (bool, bool) {
    let (item, _) = SwitchStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (
        item.contains_undefined_continue_target(&[JSString::from("x")]),
        item.contains_undefined_continue_target(&[JSString::from("y")]),
    )
}
#[test_case("switch (a.#valid) {default:;}" => true; "Expression valid")]
#[test_case("switch (a) {default: b.#valid;}" => true; "CaseBlock valid")]
#[test_case("switch (a.#invalid) {default:;}" => false; "Expression invalid")]
#[test_case("switch (a) {default: b.#invalid;}" => false; "CaseBlock invalid")]
fn switch_statement_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = SwitchStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod switch_statement {
    use super::*;
    use test_case::test_case;

    const B_DUPLEX: &str = "‘b’ may not be declared both lexically and var-style";
    const B_ALREADY: &str = "‘b’ already defined";

    #[test_case("switch (package) { default: implements; }", true, false => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "switch (Expression) CaseBlock")]
    #[test_case("switch (a) { case 1: let b=20; case 2: let b=30; case 3: let a=3; }", false, false => sset(&[B_ALREADY]); "duplicate lexicals")]
    #[test_case("switch (a) { case 1: let b=20; case 2: var b=30; case 3: var left; let right; }", false, false => sset(&[B_DUPLEX]); "lex/var dups")]
    fn early_errors(src: &str, strict: bool, wi: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).switch_statement().early_errors(&mut errs, strict, wi);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("switch(arguments){}" => true; "left")]
    #[test_case("switch(0){case 0:arguments;}" => true; "right")]
    #[test_case("switch(0){}" => false; "none")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).switch_statement().contains_arguments()
    }

    #[test_case("switch(x){case 0: var a;}" => svec(&["a"]); "switch")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).switch_statement().var_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("   switch(0){}" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 11 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).switch_statement().location()
    }
}

// CASE BLOCK
#[test]
fn case_block_test_01() {
    let (node, scanner) = check(CaseBlock::parse(&mut newparser("{}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 2);
    pretty_check(&*node, "CaseBlock: { }", &[]);
    concise_check(&*node, "CaseBlock: { }", &["Punctuator: {", "Punctuator: }"]);
    format!("{node:?}");
}
#[test]
fn case_block_test_02() {
    let (node, scanner) = check(CaseBlock::parse(&mut newparser("{case 0:;}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 10);
    pretty_check(&*node, "CaseBlock: { case 0 : ; }", &["CaseClauses: case 0 : ;"]);
    concise_check(&*node, "CaseBlock: { case 0 : ; }", &["Punctuator: {", "CaseClause: case 0 : ;", "Punctuator: }"]);
    format!("{node:?}");
}
#[test]
fn case_block_test_03() {
    let (node, scanner) = check(CaseBlock::parse(&mut newparser("{default:;}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 11);
    pretty_check(&*node, "CaseBlock: { default : ; }", &["DefaultClause: default : ;"]);
    concise_check(
        &*node,
        "CaseBlock: { default : ; }",
        &["Punctuator: {", "DefaultClause: default : ;", "Punctuator: }"],
    );
    format!("{node:?}");
}
#[test]
fn case_block_test_04() {
    let (node, scanner) =
        check(CaseBlock::parse(&mut newparser("{case 0:;default:;}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 19);
    pretty_check(
        &*node,
        "CaseBlock: { case 0 : ; default : ; }",
        &["CaseClauses: case 0 : ;", "DefaultClause: default : ;"],
    );
    concise_check(
        &*node,
        "CaseBlock: { case 0 : ; default : ; }",
        &["Punctuator: {", "CaseClause: case 0 : ;", "DefaultClause: default : ;", "Punctuator: }"],
    );
    format!("{node:?}");
}
#[test]
fn case_block_test_05() {
    let (node, scanner) =
        check(CaseBlock::parse(&mut newparser("{default:;case 0:;}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 19);
    pretty_check(
        &*node,
        "CaseBlock: { default : ; case 0 : ; }",
        &["DefaultClause: default : ;", "CaseClauses: case 0 : ;"],
    );
    concise_check(
        &*node,
        "CaseBlock: { default : ; case 0 : ; }",
        &["Punctuator: {", "DefaultClause: default : ;", "CaseClause: case 0 : ;", "Punctuator: }"],
    );
    format!("{node:?}");
}
#[test]
fn case_block_test_06() {
    let (node, scanner) =
        check(CaseBlock::parse(&mut newparser("{case 1:;default:;case 0:;}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 27);
    pretty_check(
        &*node,
        "CaseBlock: { case 1 : ; default : ; case 0 : ; }",
        &["CaseClauses: case 1 : ;", "DefaultClause: default : ;", "CaseClauses: case 0 : ;"],
    );
    concise_check(
        &*node,
        "CaseBlock: { case 1 : ; default : ; case 0 : ; }",
        &[
            "Punctuator: {",
            "CaseClause: case 1 : ;",
            "DefaultClause: default : ;",
            "CaseClause: case 0 : ;",
            "Punctuator: }",
        ],
    );
    format!("{node:?}");
}
#[test]
fn case_block_test_errs() {
    check_err(CaseBlock::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘{’ expected", 1, 1);
    check_err(
        CaseBlock::parse(&mut newparser("{"), Scanner::new(), false, false, true),
        "‘}’, ‘case’, or ‘default’ expected",
        1,
        2,
    );
    check_err(
        CaseBlock::parse(&mut newparser("{default:;"), Scanner::new(), false, false, true),
        "‘}’ expected",
        1,
        11,
    );
}
fn caseblock_print_check(src: &str) {
    let (item, _) = CaseBlock::parse(&mut newparser(src), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
    concise_error_validate(&*item);
}
#[test]
fn case_block_test_printer_errs() {
    caseblock_print_check("{}");
    caseblock_print_check("{case 0:;}");
    caseblock_print_check("{case 0:;default:;}");
    caseblock_print_check("{case 0:;default:;case 1:;}");
    caseblock_print_check("{default:;}");
    caseblock_print_check("{default:;case 1:;}");
}
fn caseblock_vdn_check(src: &str, expected: &[&str]) {
    let (item, _) = CaseBlock::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), expected);
}
#[test]
fn case_block_test_var_declared_names() {
    caseblock_vdn_check("{}", &[]);
    caseblock_vdn_check("{case 0:var a;}", &["a"]);
    caseblock_vdn_check("{case 0:var a;default:var b;}", &["a", "b"]);
    caseblock_vdn_check("{default:var a;}", &["a"]);
    caseblock_vdn_check("{default:var a;case 0:var b;}", &["a", "b"]);
    caseblock_vdn_check("{case 0:var a;default:var b;case 1:var c;}", &["a", "b", "c"]);
}
fn caseblock_cubt_check(src: &str, has_target: bool) {
    let (item, _) = CaseBlock::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), has_target);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn case_block_test_contains_undefined_break_target() {
    caseblock_cubt_check("{}", false);
    caseblock_cubt_check("{case 0:break t;}", true);
    caseblock_cubt_check("{default:break t;}", true);
    caseblock_cubt_check("{case 0:;default:break t;}", true);
    caseblock_cubt_check("{case 0:break t;default:;}", true);
    caseblock_cubt_check("{default:;case 1:break t;}", true);
    caseblock_cubt_check("{default:break t;case 1:;}", true);
    caseblock_cubt_check("{case 0:break t;default:;case 1:;}", true);
    caseblock_cubt_check("{case 0:;default:break t;case 1:;}", true);
    caseblock_cubt_check("{case 0:;default:;case 1:break t;}", true);
}
fn caseblock_contains_check(src: &str, has_literal: bool) {
    let (item, _) = CaseBlock::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), has_literal);
}
#[test]
fn case_block_test_contains() {
    caseblock_contains_check("{}", false);
    caseblock_contains_check("{case a:;}", false);
    caseblock_contains_check("{case 0:;}", true);
    caseblock_contains_check("{default:;}", false);
    caseblock_contains_check("{default:0;}", true);
    caseblock_contains_check("{case a:;default:;}", false);
    caseblock_contains_check("{case a:0;default:;}", true);
    caseblock_contains_check("{case a:;default:0;}", true);
    caseblock_contains_check("{default:;case b:;}", false);
    caseblock_contains_check("{default:0;case b:;}", true);
    caseblock_contains_check("{default:;case b:0;}", true);
    caseblock_contains_check("{case a:;default:;case b:;}", false);
    caseblock_contains_check("{case a:0;default:;case b:;}", true);
    caseblock_contains_check("{case a:;default:0;case b:;}", true);
    caseblock_contains_check("{case a:;default:;case b:0;}", true);
}
fn cb_cdl_check(src: &str, has_label: bool) {
    let (item, _) = CaseBlock::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), has_label);
}
#[test]
fn case_block_test_contains_duplicate_labels() {
    cb_cdl_check("{}", false);
    cb_cdl_check("{case a:t:;}", true);
    cb_cdl_check("{default:t:;}", true);
    cb_cdl_check("{case a:t:;default:}", true);
    cb_cdl_check("{case a:default:t:;}", true);
    cb_cdl_check("{default:t:;case a:}", true);
    cb_cdl_check("{default:case a:t:;}", true);
    cb_cdl_check("{case a:t:;default:case b:}", true);
    cb_cdl_check("{case a:default:t:;case b:}", true);
    cb_cdl_check("{case a:default:case b:t:;}", true);
}
#[test_case("{ /* empty */ }" => (false, false); "{ /* empty */ }")]
#[test_case("{ case a: continue x; }" => (false, true); "{ case a: continue x; }")]
#[test_case("{ default: continue x; }" => (false, true); "{ default: continue x; }")]
#[test_case("{ case a: continue x; default: ;}" => (false, true); "{ case a: continue x; } default: ;")]
#[test_case("{ case a: ; default: continue x; }" => (false, true); "{ case a: ; default: continue x; }")]
#[test_case("{ default: ; case a: continue x; }" => (false, true); "{ default: ; case a: continue x; }")]
#[test_case("{ default: continue x; case a: ; }" => (false, true); "{ default: continue x; case a: ; }")]
#[test_case("{ case a: continue x; default: ; case b: ; }" => (false, true); "{ case a: continue x; default: ; case b: ; }")]
#[test_case("{ case a: ; default: continue x; case b: ; }" => (false, true); "{ case a: ; default: continue x; case b: ; }")]
#[test_case("{ case a: ; default: ; case b: continue x; }" => (false, true); "{ case a: ; default: ; case b: continue x; }")]
fn case_block_test_contains_undefined_continue_target(src: &str) -> (bool, bool) {
    let (item, _) = CaseBlock::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (
        item.contains_undefined_continue_target(&[JSString::from("x")]),
        item.contains_undefined_continue_target(&[JSString::from("y")]),
    )
}
#[test_case("{}" => true; "empty")]
#[test_case("{ case 1: a.#valid; }" => true; "nodef valid")]
#[test_case("{ case 1:a.#valid; default:; case 2:; }" => true; "cdc c1 valid")]
#[test_case("{ case 1:; default:;a.#valid; case 2:; }" => true; "cdc d valid")]
#[test_case("{ case 1:; default:; case 2:a.#valid; }" => true; "cdc c2 valid")]
#[test_case("{ case 1:a.#valid; default:; }" => true; "cd c valid")]
#[test_case("{ case 1:; default:;a.#valid; }" => true; "cd d valid")]
#[test_case("{ default:;a.#valid; case 2:; }" => true; "dc d valid")]
#[test_case("{ default:; case 2:a.#valid; }" => true; "dc c valid")]
#[test_case("{ default:a.#valid; }" => true; "d valid")]
#[test_case("{ case 1: a.#invalid; }" => false; "nodef invalid")]
#[test_case("{ case 1:a.#invalid; default:; case 2:; }" => false; "cdc c1 invalid")]
#[test_case("{ case 1:; default:;a.#invalid; case 2:; }" => false; "cdc d invalid")]
#[test_case("{ case 1:; default:; case 2:a.#invalid; }" => false; "cdc c2 invalid")]
#[test_case("{ case 1:a.#invalid; default:; }" => false; "cd c invalid")]
#[test_case("{ case 1:; default:;a.#invalid; }" => false; "cd d invalid")]
#[test_case("{ default:;a.#invalid; case 2:; }" => false; "dc d invalid")]
#[test_case("{ default:; case 2:a.#invalid; }" => false; "dc c invalid")]
#[test_case("{ default:a.#invalid; }" => false; "d invalid")]
fn case_block_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = CaseBlock::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod case_block {
    use super::*;
    use test_case::test_case;

    #[test_case("{}", true, false => sset(&[]); "empty")]
    #[test_case("{ case package: ;}", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "cases only")]
    #[test_case("{ default: package;}", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "default only")]
    #[test_case("{ case package:; default:implements; case interface:;}", true, false => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "before+default+after")]
    #[test_case("{ case package:; default: implements;}", true, false => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "before+default")]
    #[test_case("{ default:package; case implements:;}", true, false => sset(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "default+after")]
    fn early_errors(src: &str, strict: bool, wi: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).case_block().early_errors(&mut errs, strict, wi);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("{ }" => Vec::<String>::new(); "empty")]
    #[test_case("{ case 1: let a; }" => vec!["a"]; "cases only")]
    #[test_case("{ default: let a; }" => vec!["a"]; "default only")]
    #[test_case("{ case 1: let a; default: let b; case 2: let c; }" => vec!["a", "b", "c"]; "before/after/default")]
    #[test_case("{ case 1: let a; default: let b; }" => vec!["a", "b"]; "before+default")]
    #[test_case("{ default: let b; case 2: let c; }" => vec!["b", "c"]; "default+after")]
    fn lexically_declared_names(src: &str) -> Vec<String> {
        Maker::new(src).case_block().lexically_declared_names().into_iter().map(String::from).collect::<Vec<String>>()
    }

    #[test_case("{}" => false; "empty")]
    #[test_case("{case 0:arguments;}" => true; "no-default (yes)")]
    #[test_case("{case 0:;}" => false; "no-default (no)")]
    #[test_case("{default:arguments;}" => true; "xDx (yes)")]
    #[test_case("{default:;}" => false; "xDx (no)")]
    #[test_case("{case 0:arguments;default:;}" => true; "CDx (left)")]
    #[test_case("{case 0:;default:arguments;}" => true; "CDx (right)")]
    #[test_case("{case 0:;default:;}" => false; "CDx (none)")]
    #[test_case("{default:arguments;case 0:;}" => true; "xDC (left)")]
    #[test_case("{default:;case 0:arguments;}" => true; "xDC (right)")]
    #[test_case("{default:;case 0:;}" => false; "xDC (none)")]
    #[test_case("{case 1:arguments;default:;case 0:;}" => true; "CDC (left)")]
    #[test_case("{case 1:;default:arguments;case 0:;}" => true; "CDC (middle)")]
    #[test_case("{case 1:;default:;case 0:arguments;}" => true; "CDC (right)")]
    #[test_case("{case 1:;default:;case 0:;}" => false; "CDC (none)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).case_block().contains_arguments()
    }

    #[test_case("{}" => svec(&[]); "empty")]
    #[test_case("{case 0:var q;}" => svec(&["q"]); "no default")]
    #[test_case("{case 0:var a;default:var b;case 1:var c;}" => svec(&["a", "b", "c"]); "CDC")]
    #[test_case("{case 0:var a;default:var b;}" => svec(&["a", "b"]); "CDx")]
    #[test_case("{default:var b;case 1:var c;}" => svec(&["b", "c"]); "xDC")]
    #[test_case("{default:var b;}" => svec(&["b"]); "xDx")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).case_block().var_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("   {}" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 2 } }; "no default")]
    #[test_case("   {default:;}" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 11 } }; "has default")]
    fn location(src: &str) -> Location {
        Maker::new(src).case_block().location()
    }

    #[test_case("{}" => svec(&[]); "empty")]
    #[test_case("{default:let a;}" => svec(&["let a ;"]); "only default")]
    #[test_case(
        "{case 0: const before = 'b'; default: const middle = 'd'; case 2: const after = 'a';}"
        => svec(&["const before = 'b' ;", "const middle = 'd' ;", "const after = 'a' ;"]);
        "all three"
    )]
    fn lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).case_block().lexically_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }
}

// CASE CLAUSES
#[test]
fn case_clauses_test_01() {
    let (node, scanner) = check(CaseClauses::parse(&mut newparser("case 0:;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "CaseClauses: case 0 : ;", &["CaseClause: case 0 : ;"]);
    concise_check(&*node, "CaseClause: case 0 : ;", &["Keyword: case", "Numeric: 0", "Punctuator: :", "Punctuator: ;"]);
    format!("{node:?}");
}
#[test]
fn case_clauses_test_02() {
    let (node, scanner) =
        check(CaseClauses::parse(&mut newparser("case 0:;case 1:;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 16);
    pretty_check(&*node, "CaseClauses: case 0 : ; case 1 : ;", &["CaseClauses: case 0 : ;", "CaseClause: case 1 : ;"]);
    concise_check(&*node, "CaseClauses: case 0 : ; case 1 : ;", &["CaseClause: case 0 : ;", "CaseClause: case 1 : ;"]);
    format!("{node:?}");
}
#[test]
fn case_clauses_test_errs() {
    check_err(CaseClauses::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘case’ expected", 1, 1);
}
fn caseclauses_print_check(src: &str) {
    let (item, _) = CaseClauses::parse(&mut newparser(src), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
    concise_error_validate(&*item);
}
#[test]
fn case_clauses_test_printer_errs() {
    caseclauses_print_check("case 0:;");
    caseclauses_print_check("case 0:;case 1:;");
}
fn caseclauses_vdn_check(src: &str, expected: &[&str]) {
    let (item, _) = CaseClauses::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), expected);
}
#[test]
fn case_clauses_test_var_declared_names() {
    caseclauses_vdn_check("case 0:var a;", &["a"]);
    caseclauses_vdn_check("case 0:var a;case 1:var b;", &["a", "b"]);
}
fn caseclauses_cubt_check(src: &str) {
    let (item, _) = CaseClauses::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn case_clauses_test_contains_undefined_break_target() {
    caseclauses_cubt_check("case 0:break t;");
    caseclauses_cubt_check("case 0:break t;case 1:;");
    caseclauses_cubt_check("case 0:;case 1:break t;");
}
fn caseclauses_contains_check(src: &str, has_literal: bool) {
    let (item, _) = CaseClauses::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), has_literal);
}
#[test]
fn case_clauses_test_contains() {
    caseclauses_contains_check("case a:;", false);
    caseclauses_contains_check("case a:0;", true);
    caseclauses_contains_check("case a:;case b:;", false);
    caseclauses_contains_check("case a:0;case b:;", true);
    caseclauses_contains_check("case a:;case b:0;", true);
}
fn ccs_cdl_check(src: &str) {
    let (item, _) = CaseClauses::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
}
#[test]
fn case_clauses_test_contains_duplicate_labels() {
    ccs_cdl_check("case 0:t:;");
    ccs_cdl_check("case 0:t:;case 1:");
    ccs_cdl_check("case 0:case 1:t:;");
}
#[test_case("case a: continue x;" => (false, true); "case a: continue x;")]
#[test_case("case a: continue x; case b: ;" => (false, true); "case a: continue x; case b: ;")]
#[test_case("case a: ; case b: continue x;" => (false, true); "case a: ; case b: continue x;")]
fn case_clauses_test_contains_undefined_continue_target(src: &str) -> (bool, bool) {
    let (item, _) = CaseClauses::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (
        item.contains_undefined_continue_target(&[JSString::from("x")]),
        item.contains_undefined_continue_target(&[JSString::from("y")]),
    )
}
#[test_case("case 1: a.#valid;" => true; "single valid")]
#[test_case("case 1: a.#valid; case 2: ;" => true; "multi left valid")]
#[test_case("case 1: ; case 2: a.#valid;" => true; "multi second valid")]
#[test_case("case 1: a.#invalid;" => false; "single invalid")]
#[test_case("case 1: a.#invalid; case 2: ;" => false; "multi left invalid")]
#[test_case("case 1: ; case 2: a.#invalid;" => false; "multi second invalid")]
fn case_clauses_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = CaseClauses::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod case_clauses {
    use super::*;
    use test_case::test_case;

    #[test_case("case implements: package; continue;", true, false => sset(&[PACKAGE_NOT_ALLOWED, CONTINUE_ITER, IMPLEMENTS_NOT_ALLOWED]); "statement")]
    #[test_case("case implements: package; continue;", false, true => sset(&[]); "not strict; in iter")]
    #[test_case("case implements: package; continue; case interface: break;", true, false => sset(&[PACKAGE_NOT_ALLOWED, CONTINUE_ITER, INTERFACE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "list")]
    #[test_case("case implements: package; continue; case interface: break;", false, true => sset(&[]); "not strict; in iter; list")]
    fn early_errors(src: &str, strict: bool, wi: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).case_clauses().early_errors(&mut errs, strict, wi);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("case 0: let a;" => vec!["a"]; "single")]
    #[test_case("case 0: let a; case 3: let x, y, z;" => vec!["a", "x", "y", "z"]; "multi")]
    fn lexically_declared_names(src: &str) -> Vec<String> {
        Maker::new(src).case_clauses().lexically_declared_names().into_iter().map(String::from).collect::<Vec<String>>()
    }

    #[test_case("case 0:arguments;" => true; "Item (yes)")]
    #[test_case("case 0:;" => false; "Item (no)")]
    #[test_case("case 0:arguments;case 1:;" => true; "List (left)")]
    #[test_case("case 0:;case 1:arguments;" => true; "List (right)")]
    #[test_case("case 0:;case 1:;" => false; "List (none)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).case_clauses().contains_arguments()
    }

    #[test_case("case 0:var a;" => svec(&["a"]); "item")]
    #[test_case("case 0:var a;case 1:var b;" => svec(&["a", "b"]); "list")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).case_clauses().var_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("case 0: let a=3;" => svec(&["let a = 3 ;"]); "item")]
    #[test_case("case 0: let x=1; case 1: let bob = 0;" => svec(&["let x = 1 ;", "let bob = 0 ;"]); "list")]
    fn lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).case_clauses().lexically_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case(
        "case 0: zero(); break; case 1: one(); break;"
        => svec(&["case 0 : zero ( ) ; break ;", "case 1 : one ( ) ; break ;"]);
        "list"
    )]
    fn to_vec(src: &str) -> Vec<String> {
        Maker::new(src).case_clauses().to_vec().iter().map(|c| format!("{c}")).collect::<Vec<_>>()
    }
}

// CASE CLAUSE
#[test]
fn case_clause_test_01() {
    let (node, scanner) = check(CaseClause::parse(&mut newparser("case 0:;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "CaseClause: case 0 : ;", &["Expression: 0", "StatementList: ;"]);
    concise_check(&*node, "CaseClause: case 0 : ;", &["Keyword: case", "Numeric: 0", "Punctuator: :", "Punctuator: ;"]);
    format!("{node:?}");
}
#[test]
fn case_clause_test_02() {
    let (node, scanner) = check(CaseClause::parse(&mut newparser("case 0:"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 7);
    pretty_check(&*node, "CaseClause: case 0 :", &["Expression: 0"]);
    concise_check(&*node, "CaseClause: case 0 :", &["Keyword: case", "Numeric: 0", "Punctuator: :"]);
    format!("{node:?}");
}
#[test]
fn case_clause_test_errs() {
    check_err(CaseClause::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘case’ expected", 1, 1);
    check_err(
        CaseClause::parse(&mut newparser("case"), Scanner::new(), false, false, true),
        "Expression expected",
        1,
        5,
    );
    check_err(CaseClause::parse(&mut newparser("case 0"), Scanner::new(), false, false, true), "‘:’ expected", 1, 7);
}
fn caseclause_print_check(src: &str) {
    let (item, _) = CaseClause::parse(&mut newparser(src), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
    concise_error_validate(&*item);
}
#[test]
fn case_clause_test_printer_errs() {
    caseclause_print_check("case 0:;");
    caseclause_print_check("case 0:");
}
fn caseclause_vdn_check(src: &str, expected: &[&str]) {
    let (item, _) = CaseClause::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), expected);
}
#[test]
fn case_clause_test_var_declared_names() {
    caseclause_vdn_check("case 0: var a;", &["a"]);
    caseclause_vdn_check("case 0:", &[]);
}
fn caseclause_cubt_check(src: &str, has_target: bool) {
    let (item, _) = CaseClause::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), has_target);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn case_clause_test_contains_undefined_break_target() {
    caseclause_cubt_check("case a:break t;", true);
    caseclause_cubt_check("case s:", false);
}
fn caseclause_contains_check(src: &str, has_literal: bool) {
    let (item, _) = CaseClause::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), has_literal);
}
#[test]
fn case_clause_test_contains() {
    caseclause_contains_check("case a:", false);
    caseclause_contains_check("case 0:", true);
    caseclause_contains_check("case a:;", false);
    caseclause_contains_check("case 0:;", true);
    caseclause_contains_check("case a:0;", true);
}
fn cc_cdl_check(src: &str, has_label: bool) {
    let (item, _) = CaseClause::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), has_label);
}
#[test]
fn case_clause_test_contains_duplicate_labels() {
    cc_cdl_check("case 0:", false);
    cc_cdl_check("case 0:t:;", true);
}
#[test_case("case a:" => (false, false); "case a:")]
#[test_case("case a: continue x;" => (false, true); "case a: continue x;")]
fn case_clause_test_contains_undefined_continue_target(src: &str) -> (bool, bool) {
    let (item, _) = CaseClause::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (
        item.contains_undefined_continue_target(&[JSString::from("x")]),
        item.contains_undefined_continue_target(&[JSString::from("y")]),
    )
}
#[test_case("case a.#valid:" => true; "Exp Only valid")]
#[test_case("case a.#valid: ;" => true; "Expression valid")]
#[test_case("case a: b.#valid;" => true; "Statement valid")]
#[test_case("case a.#invalid:" => false; "Exp Only invalid")]
#[test_case("case a.#invalid: ;" => false; "Expression invalid")]
#[test_case("case a: b.#invalid;" => false; "Statement invalid")]
fn case_clause_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = CaseClause::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod case_clause {
    use super::*;
    use test_case::test_case;

    #[test_case("case package:", true, false => sset(&[PACKAGE_NOT_ALLOWED]); "empty")]
    #[test_case("case implements: package; continue;", true, false => sset(&[PACKAGE_NOT_ALLOWED, CONTINUE_ITER, IMPLEMENTS_NOT_ALLOWED]); "statement")]
    #[test_case("case implements: package; continue;", false, true => sset(&[]); "not strict; in iter")]
    fn early_errors(src: &str, strict: bool, wi: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).case_clause().early_errors(&mut errs, strict, wi);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("case 0:" => Vec::<String>::new(); "no statements")]
    #[test_case("case 0: let a;" => vec!["a"]; "some decls")]
    fn lexically_declared_names(src: &str) -> Vec<String> {
        Maker::new(src).case_clause().lexically_declared_names().into_iter().map(String::from).collect::<Vec<String>>()
    }

    #[test_case("case arguments:" => true; "Exp (yes)")]
    #[test_case("case 0:" => false; "Exp (no)")]
    #[test_case("case arguments:;" => true; "Binary (left)")]
    #[test_case("case 0:arguments;" => true; "Binary (right)")]
    #[test_case("case 0:;" => false; "Binary (none)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).case_clause().contains_arguments()
    }

    #[test_case("case 0:" => svec(&[]); "empty")]
    #[test_case("case 0:var a;" => svec(&["a"]); "item")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).case_clause().var_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("case 0:" => svec(&[]); "empty")]
    #[test_case("case 0: thing(); let a=3; thong(a);" => svec(&["let a = 3 ;"]); "item")]
    fn lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).case_clause().lexically_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }
}

// DEFAULT CLAUSE
#[test]
fn default_clause_test_01() {
    let (node, scanner) = check(DefaultClause::parse(&mut newparser("default:;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "DefaultClause: default : ;", &["StatementList: ;"]);
    concise_check(&*node, "DefaultClause: default : ;", &["Keyword: default", "Punctuator: :", "Punctuator: ;"]);
    format!("{node:?}");
}
#[test]
fn default_clause_test_02() {
    let (node, scanner) = check(DefaultClause::parse(&mut newparser("default:"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "DefaultClause: default :", &[]);
    concise_check(&*node, "DefaultClause: default :", &["Keyword: default", "Punctuator: :"]);
    format!("{node:?}");
}
#[test]
fn default_clause_test_errs() {
    check_err(DefaultClause::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘default’ expected", 1, 1);
    check_err(
        DefaultClause::parse(&mut newparser("default"), Scanner::new(), false, false, true),
        "‘:’ expected",
        1,
        8,
    );
}
fn defclause_print_check(src: &str) {
    let (item, _) = DefaultClause::parse(&mut newparser(src), Scanner::new(), false, false, true).unwrap();
    pretty_error_validate(&*item);
    concise_error_validate(&*item);
}
#[test]
fn default_clause_test_printer_errs() {
    defclause_print_check("default:;");
    defclause_print_check("default:");
}
fn defclause_vdn_check(src: &str, expected: &[&str]) {
    let (item, _) = DefaultClause::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), expected);
}
#[test]
fn default_clause_test_var_declared_names() {
    defclause_vdn_check("default: var a;", &["a"]);
    defclause_vdn_check("default:", &[]);
}
fn defclause_cubt_check(src: &str, has_target: bool) {
    let (item, _) = DefaultClause::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), has_target);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn default_clause_test_contains_undefined_break_target() {
    defclause_cubt_check("default:break t;", true);
    defclause_cubt_check("default:", false);
}
fn defclause_contains_check(src: &str, has_literal: bool) {
    let (item, _) = DefaultClause::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), has_literal);
}
#[test]
fn default_clause_test_contains() {
    defclause_contains_check("default:", false);
    defclause_contains_check("default:;", false);
    defclause_contains_check("default:0;", true);
}
fn def_cdl_check(src: &str, has_label: bool) {
    let (item, _) = DefaultClause::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), has_label);
}
#[test]
fn default_clause_test_contains_duplicate_labels() {
    def_cdl_check("default:", false);
    def_cdl_check("default:t:;", true);
}
#[test_case("default:" => (false, false); "default:")]
#[test_case("default: continue x;" => (false, true); "default: continue x;")]
fn default_clause_test_contains_undefined_continue_target(src: &str) -> (bool, bool) {
    let (item, _) = DefaultClause::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (
        item.contains_undefined_continue_target(&[JSString::from("x")]),
        item.contains_undefined_continue_target(&[JSString::from("y")]),
    )
}
#[test_case("default:" => true; "no statement")]
#[test_case("default: a.#valid;" => true; "statement valid")]
#[test_case("default: a.#invalid;" => false; "statement invalid")]
fn default_clause_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = DefaultClause::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod default_clause {
    use super::*;
    use test_case::test_case;

    #[test_case("default:", true, false => sset(&[]); "empty")]
    #[test_case("default: package; continue;", true, false => sset(&[PACKAGE_NOT_ALLOWED, CONTINUE_ITER]); "statement")]
    #[test_case("default: package; continue;", false, true => sset(&[]); "not strict; in iter")]
    fn early_errors(src: &str, strict: bool, wi: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).default_clause().early_errors(&mut errs, strict, wi);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("default:" => Vec::<String>::new(); "no statements")]
    #[test_case("default: let z, w;" => vec!["z", "w"]; "statements with decls")]
    fn lexically_declared_names(src: &str) -> Vec<String> {
        Maker::new(src)
            .default_clause()
            .lexically_declared_names()
            .into_iter()
            .map(String::from)
            .collect::<Vec<String>>()
    }

    #[test_case("default:" => false; "empty")]
    #[test_case("default:arguments;" => true; "stmt (yes)")]
    #[test_case("default:;" => false; "stmt (no)")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).default_clause().contains_arguments()
    }

    #[test_case("default:" => svec(&[]); "empty")]
    #[test_case("default:var a;" => svec(&["a"]); "item")]
    fn var_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).default_clause().var_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }

    #[test_case("default:" => svec(&[]); "empty")]
    #[test_case("default: thing(); let a=3; thong(a);" => svec(&["let a = 3 ;"]); "item")]
    fn lexically_scoped_declarations(src: &str) -> Vec<String> {
        Maker::new(src).default_clause().lexically_scoped_declarations().iter().map(String::from).collect::<Vec<_>>()
    }
}
