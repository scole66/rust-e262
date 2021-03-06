use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

#[test]
fn switch_statement_test_01() {
    let (node, scanner) = check(SwitchStatement::parse(&mut newparser("switch (0) { default: 0;}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 25);
    pretty_check(&*node, "SwitchStatement: switch ( 0 ) { default : 0 ; }", vec!["Expression: 0", "CaseBlock: { default : 0 ; }"]);
    concise_check(&*node, "SwitchStatement: switch ( 0 ) { default : 0 ; }", vec!["Keyword: switch", "Punctuator: (", "Numeric: 0", "Punctuator: )", "CaseBlock: { default : 0 ; }"]);
    format!("{:?}", node);
}
#[test]
fn switch_statement_test_err() {
    check_err(SwitchStatement::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘switch’ expected", 1, 1);
    check_err(SwitchStatement::parse(&mut newparser("switch"), Scanner::new(), false, false, true), "‘(’ expected", 1, 7);
    check_err(SwitchStatement::parse(&mut newparser("switch("), Scanner::new(), false, false, true), "Expression expected", 1, 8);
    check_err(SwitchStatement::parse(&mut newparser("switch(0"), Scanner::new(), false, false, true), "‘)’ expected", 1, 9);
    check_err(SwitchStatement::parse(&mut newparser("switch(0)"), Scanner::new(), false, false, true), "‘{’ expected", 1, 10);
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

// CASE BLOCK
#[test]
fn case_block_test_01() {
    let (node, scanner) = check(CaseBlock::parse(&mut newparser("{}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 2);
    pretty_check(&*node, "CaseBlock: { }", vec![]);
    concise_check(&*node, "CaseBlock: { }", vec!["Punctuator: {", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn case_block_test_02() {
    let (node, scanner) = check(CaseBlock::parse(&mut newparser("{case 0:;}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 10);
    pretty_check(&*node, "CaseBlock: { case 0 : ; }", vec!["CaseClauses: case 0 : ;"]);
    concise_check(&*node, "CaseBlock: { case 0 : ; }", vec!["Punctuator: {", "CaseClause: case 0 : ;", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn case_block_test_03() {
    let (node, scanner) = check(CaseBlock::parse(&mut newparser("{default:;}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 11);
    pretty_check(&*node, "CaseBlock: { default : ; }", vec!["DefaultClause: default : ;"]);
    concise_check(&*node, "CaseBlock: { default : ; }", vec!["Punctuator: {", "DefaultClause: default : ;", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn case_block_test_04() {
    let (node, scanner) = check(CaseBlock::parse(&mut newparser("{case 0:;default:;}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 19);
    pretty_check(&*node, "CaseBlock: { case 0 : ; default : ; }", vec!["CaseClauses: case 0 : ;", "DefaultClause: default : ;"]);
    concise_check(&*node, "CaseBlock: { case 0 : ; default : ; }", vec!["Punctuator: {", "CaseClause: case 0 : ;", "DefaultClause: default : ;", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn case_block_test_05() {
    let (node, scanner) = check(CaseBlock::parse(&mut newparser("{default:;case 0:;}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 19);
    pretty_check(&*node, "CaseBlock: { default : ; case 0 : ; }", vec!["DefaultClause: default : ;", "CaseClauses: case 0 : ;"]);
    concise_check(&*node, "CaseBlock: { default : ; case 0 : ; }", vec!["Punctuator: {", "DefaultClause: default : ;", "CaseClause: case 0 : ;", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn case_block_test_06() {
    let (node, scanner) = check(CaseBlock::parse(&mut newparser("{case 1:;default:;case 0:;}"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 27);
    pretty_check(&*node, "CaseBlock: { case 1 : ; default : ; case 0 : ; }", vec!["CaseClauses: case 1 : ;", "DefaultClause: default : ;", "CaseClauses: case 0 : ;"]);
    concise_check(
        &*node,
        "CaseBlock: { case 1 : ; default : ; case 0 : ; }",
        vec!["Punctuator: {", "CaseClause: case 1 : ;", "DefaultClause: default : ;", "CaseClause: case 0 : ;", "Punctuator: }"],
    );
    format!("{:?}", node);
}
#[test]
fn case_block_test_errs() {
    check_err(CaseBlock::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘{’ expected", 1, 1);
    check_err(CaseBlock::parse(&mut newparser("{"), Scanner::new(), false, false, true), "‘}’, ‘case’, or ‘default’ expected", 1, 2);
    check_err(CaseBlock::parse(&mut newparser("{default:;"), Scanner::new(), false, false, true), "‘}’ expected", 1, 11);
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

// CASE CLAUSES
#[test]
fn case_clauses_test_01() {
    let (node, scanner) = check(CaseClauses::parse(&mut newparser("case 0:;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "CaseClauses: case 0 : ;", vec!["CaseClause: case 0 : ;"]);
    concise_check(&*node, "CaseClause: case 0 : ;", vec!["Keyword: case", "Numeric: 0", "Punctuator: :", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn case_clauses_test_02() {
    let (node, scanner) = check(CaseClauses::parse(&mut newparser("case 0:;case 1:;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 16);
    pretty_check(&*node, "CaseClauses: case 0 : ; case 1 : ;", vec!["CaseClauses: case 0 : ;", "CaseClause: case 1 : ;"]);
    concise_check(&*node, "CaseClauses: case 0 : ; case 1 : ;", vec!["CaseClause: case 0 : ;", "CaseClause: case 1 : ;"]);
    format!("{:?}", node);
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

// CASE CLAUSE
#[test]
fn case_clause_test_01() {
    let (node, scanner) = check(CaseClause::parse(&mut newparser("case 0:;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "CaseClause: case 0 : ;", vec!["Expression: 0", "StatementList: ;"]);
    concise_check(&*node, "CaseClause: case 0 : ;", vec!["Keyword: case", "Numeric: 0", "Punctuator: :", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn case_clause_test_02() {
    let (node, scanner) = check(CaseClause::parse(&mut newparser("case 0:"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 7);
    pretty_check(&*node, "CaseClause: case 0 :", vec!["Expression: 0"]);
    concise_check(&*node, "CaseClause: case 0 :", vec!["Keyword: case", "Numeric: 0", "Punctuator: :"]);
    format!("{:?}", node);
}
#[test]
fn case_clause_test_errs() {
    check_err(CaseClause::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘case’ expected", 1, 1);
    check_err(CaseClause::parse(&mut newparser("case"), Scanner::new(), false, false, true), "Expression expected", 1, 5);
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

// DEFAULT CLAUSE
#[test]
fn default_clause_test_01() {
    let (node, scanner) = check(DefaultClause::parse(&mut newparser("default:;"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "DefaultClause: default : ;", vec!["StatementList: ;"]);
    concise_check(&*node, "DefaultClause: default : ;", vec!["Keyword: default", "Punctuator: :", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn default_clause_test_02() {
    let (node, scanner) = check(DefaultClause::parse(&mut newparser("default:"), Scanner::new(), false, false, true));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "DefaultClause: default :", vec![]);
    concise_check(&*node, "DefaultClause: default :", vec!["Keyword: default", "Punctuator: :"]);
    format!("{:?}", node);
}
#[test]
fn default_clause_test_errs() {
    check_err(DefaultClause::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘default’ expected", 1, 1);
    check_err(DefaultClause::parse(&mut newparser("default"), Scanner::new(), false, false, true), "‘:’ expected", 1, 8);
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
