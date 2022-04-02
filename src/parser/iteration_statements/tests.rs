use super::testhelp::{check, check_err, chk_scan, expected_scan, newparser, set, strictparser, sv, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED, PACKAGE_NOT_ALLOWED, PRIVATE_NOT_ALLOWED};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_data, concise_error_validate, pretty_check, pretty_data, pretty_error_validate};
use crate::tests::{test_agent, unwind_syntax_error_object};
use ahash::AHashSet;
use test_case::test_case;

const A_LEXVARCLASH: &str = "‘a’ may not be declared both lexically and var-style";

// ITERATION STATEMENT
#[test]
fn iteration_statement_test_01() {
    let (node, scanner) = check(IterationStatement::parse(&mut newparser("do {} while (true);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 19);
    pretty_check(&*node, "IterationStatement: do { } while ( true ) ;", vec!["DoWhileStatement: do { } while ( true ) ;"]);
    concise_check(
        &*node,
        "DoWhileStatement: do { } while ( true ) ;",
        vec!["Keyword: do", "Block: { }", "Keyword: while", "Punctuator: (", "Keyword: true", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn iteration_statement_test_02() {
    let (node, scanner) = check(IterationStatement::parse(&mut newparser("while (true) {}"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 15);
    pretty_check(&*node, "IterationStatement: while ( true ) { }", vec!["WhileStatement: while ( true ) { }"]);
    concise_check(&*node, "WhileStatement: while ( true ) { }", vec!["Keyword: while", "Punctuator: (", "Keyword: true", "Punctuator: )", "Block: { }"]);
    format!("{:?}", node);
}
#[test]
fn iteration_statement_test_03() {
    let (node, scanner) = check(IterationStatement::parse(&mut newparser("for (;;) {}"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 11);
    pretty_check(&*node, "IterationStatement: for ( ; ; ) { }", vec!["ForStatement: for ( ; ; ) { }"]);
    concise_check(&*node, "ForStatement: for ( ; ; ) { }", vec!["Keyword: for", "Punctuator: (", "Punctuator: ;", "Punctuator: ;", "Punctuator: )", "Block: { }"]);
    format!("{:?}", node);
}
#[test]
fn iteration_statement_test_04() {
    let (node, scanner) = check(IterationStatement::parse(&mut newparser("for(v in x);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 12);
    pretty_check(&*node, "IterationStatement: for ( v in x ) ;", vec!["ForInOfStatement: for ( v in x ) ;"]);
    concise_check(
        &*node,
        "ForInOfStatement: for ( v in x ) ;",
        vec!["Keyword: for", "Punctuator: (", "IdentifierName: v", "Keyword: in", "IdentifierName: x", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn iteration_statement_test_err_01() {
    check_err(IterationStatement::parse(&mut newparser(""), Scanner::new(), true, true, true), "IterationStatement expected", 1, 1);
}
#[test]
fn iteration_statement_test_prettyerrors_1() {
    let (item, _) = IterationStatement::parse(&mut newparser("do;while(0);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn iteration_statement_test_prettyerrors_2() {
    let (item, _) = IterationStatement::parse(&mut newparser("while(0);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn iteration_statement_test_prettyerrors_3() {
    let (item, _) = IterationStatement::parse(&mut newparser("for(;;);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn iteration_statement_test_prettyerrors_4() {
    let (item, _) = IterationStatement::parse(&mut newparser("for(v in x);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn iteration_statement_test_conciseerrors_1() {
    let (item, _) = IterationStatement::parse(&mut newparser("do;while(0);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn iteration_statement_test_conciseerrors_2() {
    let (item, _) = IterationStatement::parse(&mut newparser("while(0);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn iteration_statement_test_conciseerrors_3() {
    let (item, _) = IterationStatement::parse(&mut newparser("for(;;);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn iteration_statement_test_conciseerrors_4() {
    let (item, _) = IterationStatement::parse(&mut newparser("for(v in x);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn iteration_statement_test_var_declared_names_01() {
    let (item, _) = IterationStatement::parse(&mut newparser("do{var x;}while(0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["x"]);
}
#[test]
fn iteration_statement_test_var_declared_names_02() {
    let (item, _) = IterationStatement::parse(&mut newparser("while(0){var x;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["x"]);
}
#[test]
fn iteration_statement_test_var_declared_names_03() {
    let (item, _) = IterationStatement::parse(&mut newparser("for(;;){var x;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["x"]);
}
#[test]
fn iteration_statement_test_var_declared_names_04() {
    let (item, _) = IterationStatement::parse(&mut newparser("for(v in z){var x;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["x"]);
}
#[test]
fn iteration_statement_test_contains_undefined_break_target_01() {
    let (item, _) = IterationStatement::parse(&mut newparser("do{break t;}while(0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn iteration_statement_test_contains_undefined_break_target_02() {
    let (item, _) = IterationStatement::parse(&mut newparser("do{break t;}while(0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn iteration_statement_test_contains_undefined_break_target_03() {
    let (item, _) = IterationStatement::parse(&mut newparser("while(0){break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn iteration_statement_test_contains_undefined_break_target_04() {
    let (item, _) = IterationStatement::parse(&mut newparser("while(0){break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn iteration_statement_test_contains_undefined_break_target_05() {
    let (item, _) = IterationStatement::parse(&mut newparser("for(;;){break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn iteration_statement_test_contains_undefined_break_target_06() {
    let (item, _) = IterationStatement::parse(&mut newparser("for(;;){break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn iteration_statement_test_contains_undefined_break_target_07() {
    let (item, _) = IterationStatement::parse(&mut newparser("for(v in z){break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn iteration_statement_test_contains_undefined_break_target_08() {
    let (item, _) = IterationStatement::parse(&mut newparser("for(v in z){break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn iteration_statement_test_contains_01() {
    let (item, _) = IterationStatement::parse(&mut newparser("do;while(0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn iteration_statement_test_contains_02() {
    let (item, _) = IterationStatement::parse(&mut newparser("do;while(a);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn iteration_statement_test_contains_03() {
    let (item, _) = IterationStatement::parse(&mut newparser("while(0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn iteration_statement_test_contains_04() {
    let (item, _) = IterationStatement::parse(&mut newparser("while(a);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn iteration_statement_test_contains_05() {
    let (item, _) = IterationStatement::parse(&mut newparser("for(;0;);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn iteration_statement_test_contains_06() {
    let (item, _) = IterationStatement::parse(&mut newparser("for(;;);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn iteration_statement_test_contains_07() {
    let (item, _) = IterationStatement::parse(&mut newparser("for(v in 0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn iteration_statement_test_contains_08() {
    let (item, _) = IterationStatement::parse(&mut newparser("for(v in x);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
fn istmt_check_cdl(src: &str) {
    let (item, _) = IterationStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
}
#[test]
fn iteration_statement_test_contains_duplicate_labels() {
    istmt_check_cdl("do t:;while(0);");
    istmt_check_cdl("while(0) t:;");
    istmt_check_cdl("for(;;)t:;");
    istmt_check_cdl("for(a in b)t:;");
}
#[test_case("do continue x; while (false);" => (false, true); "do continue x; while (false);")]
#[test_case("while (true) continue x;" => (false, true); "while (true) continue x;")]
#[test_case("for (;;) continue x;" => (false, true); "for (;;) continue x;")]
#[test_case("for (a in b) continue x;" => (false, true); "for (a in b) continue x;")]
fn iteration_statement_test_contains_undefined_continue_target(src: &str) -> (bool, bool) {
    let (item, _) = IterationStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (item.contains_undefined_continue_target(&[JSString::from("x")]), item.contains_undefined_continue_target(&[JSString::from("y")]))
}
#[test_case("do a.#valid; while (false);" => true; "DoWhile valid")]
#[test_case("while (a.#valid) ;" => true; "While valid")]
#[test_case("for (;a.#valid;) ;" => true; "For valid")]
#[test_case("for (a in b.#valid) ;" => true; "ForInOf valid")]
#[test_case("do a.#invalid; while (false);" => false; "DoWhile invalid")]
#[test_case("while (a.#invalid) ;" => false; "While invalid")]
#[test_case("for (;a.#invalid;) ;" => false; "For invalid")]
#[test_case("for (a in b.#invalid) ;" => false; "ForInOf invalid")]
fn iteration_statement_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = IterationStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod iteration_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("do package; while(implements);", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "DoWhileStatement")]
    #[test_case("while (package) implements;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "WhileStatement")]
    #[test_case("for(;;)package;", true => set(&[PACKAGE_NOT_ALLOWED]); "ForStatement")]
    #[test_case("for(let package in b);", true => set(&[PACKAGE_NOT_ALLOWED]); "ForInOfStatement")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        IterationStatement::parse(&mut strictparser(src, strict), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict, false);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// DO WHILE STATEMENT
#[test]
fn do_while_statement_test_01() {
    let (node, scanner) = check(DoWhileStatement::parse(&mut newparser("do;while(0);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 12);
    pretty_check(&*node, "DoWhileStatement: do ; while ( 0 ) ;", vec!["Statement: ;", "Expression: 0"]);
    concise_check(&*node, "DoWhileStatement: do ; while ( 0 ) ;", vec!["Keyword: do", "Punctuator: ;", "Keyword: while", "Punctuator: (", "Numeric: 0", "Punctuator: )", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn do_while_statement_test_02() {
    let (node, scanner) = check(DoWhileStatement::parse(&mut newparser("do;while(0)"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 11);
    pretty_check(&*node, "DoWhileStatement: do ; while ( 0 ) ;", vec!["Statement: ;", "Expression: 0"]);
    concise_check(&*node, "DoWhileStatement: do ; while ( 0 ) ;", vec!["Keyword: do", "Punctuator: ;", "Keyword: while", "Punctuator: (", "Numeric: 0", "Punctuator: )", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn do_while_statement_test_err_01() {
    check_err(DoWhileStatement::parse(&mut newparser(""), Scanner::new(), true, true, true), "‘do’ expected", 1, 1);
}
#[test]
fn do_while_statement_test_err_02() {
    check_err(DoWhileStatement::parse(&mut newparser("do"), Scanner::new(), true, true, true), "Statement expected", 1, 3);
}
#[test]
fn do_while_statement_test_err_03() {
    check_err(DoWhileStatement::parse(&mut newparser("do;"), Scanner::new(), true, true, true), "‘while’ expected", 1, 4);
}
#[test]
fn do_while_statement_test_err_04() {
    check_err(DoWhileStatement::parse(&mut newparser("do;while"), Scanner::new(), true, true, true), "‘(’ expected", 1, 9);
}
#[test]
fn do_while_statement_test_err_05() {
    check_err(DoWhileStatement::parse(&mut newparser("do;while("), Scanner::new(), true, true, true), "Expression expected", 1, 10);
}
#[test]
fn do_while_statement_test_err_06() {
    check_err(DoWhileStatement::parse(&mut newparser("do;while(0"), Scanner::new(), true, true, true), "‘)’ expected", 1, 11);
}
#[test]
fn do_while_statement_test_prettyerrors_1() {
    let (item, _) = DoWhileStatement::parse(&mut newparser("do;while(0);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn do_while_statement_test_conciseerrors_1() {
    let (item, _) = DoWhileStatement::parse(&mut newparser("do;while(0);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn do_while_statement_test_var_declared_names_01() {
    let (item, _) = DoWhileStatement::parse(&mut newparser("do{var a;}while(0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a"]);
}
#[test]
fn do_while_statement_test_contains_undefined_break_target_01() {
    let (item, _) = DoWhileStatement::parse(&mut newparser("do{break t;}while(0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn do_while_statement_test_contains_01() {
    let (item, _) = DoWhileStatement::parse(&mut newparser("do{0;}while(a);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn do_while_statement_test_contains_02() {
    let (item, _) = DoWhileStatement::parse(&mut newparser("do;while(0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn do_while_statement_test_contains_03() {
    let (item, _) = DoWhileStatement::parse(&mut newparser("do;while(a);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn do_while_statement_test_contains_duplicate_labels() {
    let (item, _) = DoWhileStatement::parse(&mut newparser("do t:;while(1);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
}
#[test_case("do continue x; while (true);" => (false, true); "do continue x; while (true);")]
fn do_while_statement_test_contains_undefined_continue_target(src: &str) -> (bool, bool) {
    let (item, _) = DoWhileStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (item.contains_undefined_continue_target(&[JSString::from("x")]), item.contains_undefined_continue_target(&[JSString::from("y")]))
}
#[test_case("do a.#valid; while (1);" => true; "stmt valid")]
#[test_case("do ; while(a.#valid);" => true; "cond valid")]
#[test_case("do a.#invalid; while (1);" => false; "stmt invalid")]
#[test_case("do ; while(a.#invalid);" => false; "cond invalid")]
fn do_while_statement_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = DoWhileStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod do_while_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("do package; while(implements);", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "do Statement while ( Expression ) ;")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        DoWhileStatement::parse(&mut strictparser(src, strict), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict, false);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// WHILE STATEMENT
#[test]
fn while_statement_test_01() {
    let (node, scanner) = check(WhileStatement::parse(&mut newparser("while(0);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "WhileStatement: while ( 0 ) ;", vec!["Expression: 0", "Statement: ;"]);
    concise_check(&*node, "WhileStatement: while ( 0 ) ;", vec!["Keyword: while", "Punctuator: (", "Numeric: 0", "Punctuator: )", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn while_statement_test_err_01() {
    check_err(WhileStatement::parse(&mut newparser(""), Scanner::new(), true, true, true), "‘while’ expected", 1, 1);
}
#[test]
fn while_statement_test_err_02() {
    check_err(WhileStatement::parse(&mut newparser("while"), Scanner::new(), true, true, true), "‘(’ expected", 1, 6);
}
#[test]
fn while_statement_test_err_03() {
    check_err(WhileStatement::parse(&mut newparser("while("), Scanner::new(), true, true, true), "Expression expected", 1, 7);
}
#[test]
fn while_statement_test_err_04() {
    check_err(WhileStatement::parse(&mut newparser("while(0"), Scanner::new(), true, true, true), "‘)’ expected", 1, 8);
}
#[test]
fn while_statement_test_err_05() {
    check_err(WhileStatement::parse(&mut newparser("while(0)"), Scanner::new(), true, true, true), "Statement expected", 1, 9);
}
#[test]
fn while_statement_test_prettyerrors_1() {
    let (item, _) = WhileStatement::parse(&mut newparser("while(0);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn while_statement_test_conciseerrors_1() {
    let (item, _) = WhileStatement::parse(&mut newparser("while(0);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn while_statement_test_var_declared_names_01() {
    let (item, _) = WhileStatement::parse(&mut newparser("while(0) var x;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["x"]);
}
#[test]
fn while_statement_test_contains_undefined_break_target_01() {
    let (item, _) = WhileStatement::parse(&mut newparser("while(0) break t;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn while_statement_test_contains_01() {
    let (item, _) = WhileStatement::parse(&mut newparser("while(0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn while_statement_test_contains_02() {
    let (item, _) = WhileStatement::parse(&mut newparser("while(a)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn while_statement_test_contains_03() {
    let (item, _) = WhileStatement::parse(&mut newparser("while(a);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn while_statement_test_contains_duplicate_labels() {
    let (item, _) = WhileStatement::parse(&mut newparser("while(1)t:;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
}
#[test_case("while (true) continue x;" => (false, true); "do continue x; while (true);")]
fn while_statement_test_contains_undefined_continue_target(src: &str) -> (bool, bool) {
    let (item, _) = WhileStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (item.contains_undefined_continue_target(&[JSString::from("x")]), item.contains_undefined_continue_target(&[JSString::from("y")]))
}
#[test_case("while (a.#valid) ;" => true; "cond valid")]
#[test_case("while (1) a.#valid;" => true; "stmt valid")]
#[test_case("while (a.#invalid) ;" => false; "cond invalid")]
#[test_case("while (1) a.#invalid;" => false; "stmt invalid")]
fn while_statement_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = WhileStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod while_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("while(package)implements;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED]); "while ( Expression ) Statement")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        WhileStatement::parse(&mut strictparser(src, strict), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict, false);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// FOR STATEMENT
#[test]
fn for_statement_test_01() {
    let (node, scanner) = check(ForStatement::parse(&mut newparser("for(x;y;z);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 11);
    pretty_check(&*node, "ForStatement: for ( x ; y ; z ) ;", vec!["Expression: x", "Expression: y", "Expression: z", "Statement: ;"]);
    concise_check(
        &*node,
        "ForStatement: for ( x ; y ; z ) ;",
        vec!["Keyword: for", "Punctuator: (", "IdentifierName: x", "Punctuator: ;", "IdentifierName: y", "Punctuator: ;", "IdentifierName: z", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_statement_test_02() {
    let (node, scanner) = check(ForStatement::parse(&mut newparser("for(;y;z);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 10);
    pretty_check(&*node, "ForStatement: for ( ; y ; z ) ;", vec!["Expression: y", "Expression: z", "Statement: ;"]);
    concise_check(
        &*node,
        "ForStatement: for ( ; y ; z ) ;",
        vec!["Keyword: for", "Punctuator: (", "Punctuator: ;", "IdentifierName: y", "Punctuator: ;", "IdentifierName: z", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_statement_test_03() {
    let (node, scanner) = check(ForStatement::parse(&mut newparser("for(x;;z);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 10);
    pretty_check(&*node, "ForStatement: for ( x ; ; z ) ;", vec!["Expression: x", "Expression: z", "Statement: ;"]);
    concise_check(
        &*node,
        "ForStatement: for ( x ; ; z ) ;",
        vec!["Keyword: for", "Punctuator: (", "IdentifierName: x", "Punctuator: ;", "Punctuator: ;", "IdentifierName: z", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_statement_test_04() {
    let (node, scanner) = check(ForStatement::parse(&mut newparser("for(x;y;);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 10);
    pretty_check(&*node, "ForStatement: for ( x ; y ; ) ;", vec!["Expression: x", "Expression: y", "Statement: ;"]);
    concise_check(
        &*node,
        "ForStatement: for ( x ; y ; ) ;",
        vec!["Keyword: for", "Punctuator: (", "IdentifierName: x", "Punctuator: ;", "IdentifierName: y", "Punctuator: ;", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_statement_test_05() {
    let (node, scanner) = check(ForStatement::parse(&mut newparser("for(x;;);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "ForStatement: for ( x ; ; ) ;", vec!["Expression: x", "Statement: ;"]);
    concise_check(&*node, "ForStatement: for ( x ; ; ) ;", vec!["Keyword: for", "Punctuator: (", "IdentifierName: x", "Punctuator: ;", "Punctuator: ;", "Punctuator: )", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn for_statement_test_06() {
    let (node, scanner) = check(ForStatement::parse(&mut newparser("for(;y;);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "ForStatement: for ( ; y ; ) ;", vec!["Expression: y", "Statement: ;"]);
    concise_check(&*node, "ForStatement: for ( ; y ; ) ;", vec!["Keyword: for", "Punctuator: (", "Punctuator: ;", "IdentifierName: y", "Punctuator: ;", "Punctuator: )", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn for_statement_test_07() {
    let (node, scanner) = check(ForStatement::parse(&mut newparser("for(;;z);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 9);
    pretty_check(&*node, "ForStatement: for ( ; ; z ) ;", vec!["Expression: z", "Statement: ;"]);
    concise_check(&*node, "ForStatement: for ( ; ; z ) ;", vec!["Keyword: for", "Punctuator: (", "Punctuator: ;", "Punctuator: ;", "IdentifierName: z", "Punctuator: )", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn for_statement_test_08() {
    let (node, scanner) = check(ForStatement::parse(&mut newparser("for(;;);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 8);
    pretty_check(&*node, "ForStatement: for ( ; ; ) ;", vec!["Statement: ;"]);
    concise_check(&*node, "ForStatement: for ( ; ; ) ;", vec!["Keyword: for", "Punctuator: (", "Punctuator: ;", "Punctuator: ;", "Punctuator: )", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn for_statement_test_09() {
    let (node, scanner) = check(ForStatement::parse(&mut newparser("for(var x;y;z);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 15);
    pretty_check(&*node, "ForStatement: for ( var x ; y ; z ) ;", vec!["VariableDeclarationList: x", "Expression: y", "Expression: z", "Statement: ;"]);
    concise_check(
        &*node,
        "ForStatement: for ( var x ; y ; z ) ;",
        vec![
            "Keyword: for",
            "Punctuator: (",
            "Keyword: var",
            "IdentifierName: x",
            "Punctuator: ;",
            "IdentifierName: y",
            "Punctuator: ;",
            "IdentifierName: z",
            "Punctuator: )",
            "Punctuator: ;",
        ],
    );
    format!("{:?}", node);
}
#[test]
fn for_statement_test_10() {
    let (node, scanner) = check(ForStatement::parse(&mut newparser("for(var x;;z);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 14);
    pretty_check(&*node, "ForStatement: for ( var x ; ; z ) ;", vec!["VariableDeclarationList: x", "Expression: z", "Statement: ;"]);
    concise_check(
        &*node,
        "ForStatement: for ( var x ; ; z ) ;",
        vec!["Keyword: for", "Punctuator: (", "Keyword: var", "IdentifierName: x", "Punctuator: ;", "Punctuator: ;", "IdentifierName: z", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_statement_test_11() {
    let (node, scanner) = check(ForStatement::parse(&mut newparser("for(var x;y;);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 14);
    pretty_check(&*node, "ForStatement: for ( var x ; y ; ) ;", vec!["VariableDeclarationList: x", "Expression: y", "Statement: ;"]);
    concise_check(
        &*node,
        "ForStatement: for ( var x ; y ; ) ;",
        vec!["Keyword: for", "Punctuator: (", "Keyword: var", "IdentifierName: x", "Punctuator: ;", "IdentifierName: y", "Punctuator: ;", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_statement_test_12() {
    let (node, scanner) = check(ForStatement::parse(&mut newparser("for(var x;;);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 13);
    pretty_check(&*node, "ForStatement: for ( var x ; ; ) ;", vec!["VariableDeclarationList: x", "Statement: ;"]);
    concise_check(
        &*node,
        "ForStatement: for ( var x ; ; ) ;",
        vec!["Keyword: for", "Punctuator: (", "Keyword: var", "IdentifierName: x", "Punctuator: ;", "Punctuator: ;", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_statement_test_13() {
    let (node, scanner) = check(ForStatement::parse(&mut newparser("for(let x;y;z);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 15);
    pretty_check(&*node, "ForStatement: for ( let x ; y ; z ) ;", vec!["LexicalDeclaration: let x ;", "Expression: y", "Expression: z", "Statement: ;"]);
    concise_check(
        &*node,
        "ForStatement: for ( let x ; y ; z ) ;",
        vec!["Keyword: for", "Punctuator: (", "LexicalDeclaration: let x ;", "IdentifierName: y", "Punctuator: ;", "IdentifierName: z", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_statement_test_14() {
    let (node, scanner) = check(ForStatement::parse(&mut newparser("for(let x;;z);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 14);
    pretty_check(&*node, "ForStatement: for ( let x ; ; z ) ;", vec!["LexicalDeclaration: let x ;", "Expression: z", "Statement: ;"]);
    concise_check(
        &*node,
        "ForStatement: for ( let x ; ; z ) ;",
        vec!["Keyword: for", "Punctuator: (", "LexicalDeclaration: let x ;", "Punctuator: ;", "IdentifierName: z", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_statement_test_15() {
    let (node, scanner) = check(ForStatement::parse(&mut newparser("for(let x;y;);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 14);
    pretty_check(&*node, "ForStatement: for ( let x ; y ; ) ;", vec!["LexicalDeclaration: let x ;", "Expression: y", "Statement: ;"]);
    concise_check(
        &*node,
        "ForStatement: for ( let x ; y ; ) ;",
        vec!["Keyword: for", "Punctuator: (", "LexicalDeclaration: let x ;", "IdentifierName: y", "Punctuator: ;", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_statement_test_16() {
    let (node, scanner) = check(ForStatement::parse(&mut newparser("for(let x;;);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 13);
    pretty_check(&*node, "ForStatement: for ( let x ; ; ) ;", vec!["LexicalDeclaration: let x ;", "Statement: ;"]);
    concise_check(&*node, "ForStatement: for ( let x ; ; ) ;", vec!["Keyword: for", "Punctuator: (", "LexicalDeclaration: let x ;", "Punctuator: ;", "Punctuator: )", "Punctuator: ;"]);
    format!("{:?}", node);
}
#[test]
fn for_statement_test_err_01() {
    check_err(ForStatement::parse(&mut newparser(""), Scanner::new(), true, true, true), "‘for’ expected", 1, 1);
}
#[test]
fn for_statement_test_err_02() {
    check_err(ForStatement::parse(&mut newparser("for"), Scanner::new(), true, true, true), "‘(’ expected", 1, 4);
}
#[test]
fn for_statement_test_err_03() {
    check_err(ForStatement::parse(&mut newparser("for("), Scanner::new(), true, true, true), "‘var’, LexicalDeclaration, or Expression expected", 1, 5);
}
#[test]
fn for_statement_test_err_04() {
    check_err(ForStatement::parse(&mut newparser("for(var"), Scanner::new(), true, true, true), "VariableDeclaration expected", 1, 8);
}
#[test]
fn for_statement_test_err_05() {
    check_err(ForStatement::parse(&mut newparser("for(var a"), Scanner::new(), true, true, true), "‘;’ expected", 1, 10);
}
#[test]
fn for_statement_test_err_06() {
    check_err(ForStatement::parse(&mut newparser("for(var a;"), Scanner::new(), true, true, true), "‘;’ expected", 1, 11);
}
#[test]
fn for_statement_test_err_07() {
    check_err(ForStatement::parse(&mut newparser("for(var a;;"), Scanner::new(), true, true, true), "‘)’ expected", 1, 12);
}
#[test]
fn for_statement_test_err_08() {
    check_err(ForStatement::parse(&mut newparser("for(var a;;)"), Scanner::new(), true, true, true), "Statement expected", 1, 13);
}
#[test]
fn for_statement_test_err_09() {
    check_err(ForStatement::parse(&mut newparser("for(let a;"), Scanner::new(), true, true, true), "‘;’ expected", 1, 11);
}
#[test]
fn for_statement_test_err_10() {
    check_err(ForStatement::parse(&mut newparser("for(let a;;"), Scanner::new(), true, true, true), "‘)’ expected", 1, 12);
}
#[test]
fn for_statement_test_err_11() {
    check_err(ForStatement::parse(&mut newparser("for(let a;;)"), Scanner::new(), true, true, true), "Statement expected", 1, 13);
}
#[test]
fn for_statement_test_err_12() {
    check_err(ForStatement::parse(&mut newparser("for(;"), Scanner::new(), true, true, true), "‘;’ expected", 1, 6);
}
#[test]
fn for_statement_test_err_13() {
    check_err(ForStatement::parse(&mut newparser("for(;;"), Scanner::new(), true, true, true), "‘)’ expected", 1, 7);
}
#[test]
fn for_statement_test_err_14() {
    check_err(ForStatement::parse(&mut newparser("for(;;)"), Scanner::new(), true, true, true), "Statement expected", 1, 8);
}
#[test]
fn for_statement_test_err_15() {
    check_err(ForStatement::parse(&mut newparser("for(let["), Scanner::new(), true, true, true), "BindingElement expected", 1, 9);
}
#[test]
fn for_statement_test_prettyerrors_01() {
    let (item, _) = ForStatement::parse(&mut newparser("for(a;b;c);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_statement_test_prettyerrors_02() {
    let (item, _) = ForStatement::parse(&mut newparser("for(;b;c);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_statement_test_prettyerrors_03() {
    let (item, _) = ForStatement::parse(&mut newparser("for(a;;c);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_statement_test_prettyerrors_04() {
    let (item, _) = ForStatement::parse(&mut newparser("for(a;b;);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_statement_test_prettyerrors_05() {
    let (item, _) = ForStatement::parse(&mut newparser("for(;;c);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_statement_test_prettyerrors_06() {
    let (item, _) = ForStatement::parse(&mut newparser("for(;b;);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_statement_test_prettyerrors_07() {
    let (item, _) = ForStatement::parse(&mut newparser("for(a;;);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_statement_test_prettyerrors_08() {
    let (item, _) = ForStatement::parse(&mut newparser("for(;;);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_statement_test_prettyerrors_09() {
    let (item, _) = ForStatement::parse(&mut newparser("for(var a;b;c);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_statement_test_prettyerrors_10() {
    let (item, _) = ForStatement::parse(&mut newparser("for(var a;;c);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_statement_test_prettyerrors_11() {
    let (item, _) = ForStatement::parse(&mut newparser("for(var a;b;);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_statement_test_prettyerrors_12() {
    let (item, _) = ForStatement::parse(&mut newparser("for(var a;;);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_statement_test_prettyerrors_13() {
    let (item, _) = ForStatement::parse(&mut newparser("for(let a;b;c);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_statement_test_prettyerrors_14() {
    let (item, _) = ForStatement::parse(&mut newparser("for(let a;;c);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_statement_test_prettyerrors_15() {
    let (item, _) = ForStatement::parse(&mut newparser("for(let a;b;);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_statement_test_prettyerrors_16() {
    let (item, _) = ForStatement::parse(&mut newparser("for(let a;;);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_statement_test_conciseerrors_01() {
    let (item, _) = ForStatement::parse(&mut newparser("for(a;b;c);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_statement_test_conciseerrors_02() {
    let (item, _) = ForStatement::parse(&mut newparser("for(;b;c);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_statement_test_conciseerrors_03() {
    let (item, _) = ForStatement::parse(&mut newparser("for(a;;c);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_statement_test_conciseerrors_04() {
    let (item, _) = ForStatement::parse(&mut newparser("for(a;b;);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_statement_test_conciseerrors_05() {
    let (item, _) = ForStatement::parse(&mut newparser("for(;;c);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_statement_test_conciseerrors_06() {
    let (item, _) = ForStatement::parse(&mut newparser("for(;b;);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_statement_test_conciseerrors_07() {
    let (item, _) = ForStatement::parse(&mut newparser("for(a;;);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_statement_test_conciseerrors_08() {
    let (item, _) = ForStatement::parse(&mut newparser("for(;;);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_statement_test_conciseerrors_09() {
    let (item, _) = ForStatement::parse(&mut newparser("for(var a;b;c);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_statement_test_conciseerrors_10() {
    let (item, _) = ForStatement::parse(&mut newparser("for(var a;;c);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_statement_test_conciseerrors_11() {
    let (item, _) = ForStatement::parse(&mut newparser("for(var a;b;);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_statement_test_conciseerrors_12() {
    let (item, _) = ForStatement::parse(&mut newparser("for(var a;;);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_statement_test_conciseerrors_13() {
    let (item, _) = ForStatement::parse(&mut newparser("for(let a;b;c);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_statement_test_conciseerrors_14() {
    let (item, _) = ForStatement::parse(&mut newparser("for(let a;;c);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_statement_test_conciseerrors_15() {
    let (item, _) = ForStatement::parse(&mut newparser("for(let a;b;);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_statement_test_conciseerrors_16() {
    let (item, _) = ForStatement::parse(&mut newparser("for(let a;;);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_statement_test_var_declared_names_01() {
    let (item, _) = ForStatement::parse(&mut newparser("for(;;){var a;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a"]);
}
#[test]
fn for_statement_test_var_declared_names_02() {
    let (item, _) = ForStatement::parse(&mut newparser("for(var a; b; c) { var d; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a", "d"]);
}
#[test]
fn for_statement_test_var_declared_names_03() {
    let (item, _) = ForStatement::parse(&mut newparser("for(let a; b; c) { var d; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["d"]);
}
#[test]
fn for_statement_test_contains_undefined_break_target_01() {
    let (item, _) = ForStatement::parse(&mut newparser("for(;;) { break t; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn for_statement_test_contains_undefined_break_target_02() {
    let (item, _) = ForStatement::parse(&mut newparser("for(var a;;) { break t; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn for_statement_test_contains_undefined_break_target_03() {
    let (item, _) = ForStatement::parse(&mut newparser("for(let a;;) { break t; }"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
    assert_eq!(item.contains_undefined_break_target(&[]), true);
}
#[test]
fn for_statement_test_contains_01() {
    let (item, _) = ForStatement::parse(&mut newparser("for(0;b;c)d;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_statement_test_contains_02() {
    let (item, _) = ForStatement::parse(&mut newparser("for(a;0;c)d;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_statement_test_contains_03() {
    let (item, _) = ForStatement::parse(&mut newparser("for(a;b;0)d;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_statement_test_contains_04() {
    let (item, _) = ForStatement::parse(&mut newparser("for(a;b;c)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_statement_test_contains_05() {
    let (item, _) = ForStatement::parse(&mut newparser("for(a;b;c)d;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn for_statement_test_contains_06() {
    let (item, _) = ForStatement::parse(&mut newparser("for(;;)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_statement_test_contains_07() {
    let (item, _) = ForStatement::parse(&mut newparser("for(var a;0;c)d;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_statement_test_contains_08() {
    let (item, _) = ForStatement::parse(&mut newparser("for(var a;b;0)d;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_statement_test_contains_09() {
    let (item, _) = ForStatement::parse(&mut newparser("for(var a;b;c)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_statement_test_contains_10() {
    let (item, _) = ForStatement::parse(&mut newparser("for(var a;b;c)d;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn for_statement_test_contains_11() {
    let (item, _) = ForStatement::parse(&mut newparser("for(var a;;)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_statement_test_contains_12() {
    let (item, _) = ForStatement::parse(&mut newparser("for(let a;0;c)d;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_statement_test_contains_13() {
    let (item, _) = ForStatement::parse(&mut newparser("for(let a;b;0)d;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_statement_test_contains_14() {
    let (item, _) = ForStatement::parse(&mut newparser("for(let a;b;c)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_statement_test_contains_15() {
    let (item, _) = ForStatement::parse(&mut newparser("for(let a;b;c)d;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn for_statement_test_contains_16() {
    let (item, _) = ForStatement::parse(&mut newparser("for(let a;;)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
fn for_stmt_cdl_check(src: &str) {
    let (item, _) = ForStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
}
#[test]
fn for_statement_test_contains_duplicate_labels() {
    for_stmt_cdl_check("for(;;){t:;}");
    for_stmt_cdl_check("for(var a;;){t:;}");
    for_stmt_cdl_check("for(let a;;){t:;}");
}
#[test_case("for (;;) continue x;" => (false, true); "for (;;) continue x;")]
#[test_case("for (var a;;) continue x;" => (false, true); "for (var a;;) continue x;")]
#[test_case("for (let a;;) continue x;" => (false, true); "for (let a;;) continue x;")]
fn for_statement_test_contains_undefined_continue_target(src: &str) -> (bool, bool) {
    let (item, _) = ForStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (item.contains_undefined_continue_target(&[JSString::from("x")]), item.contains_undefined_continue_target(&[JSString::from("y")]))
}
#[test_case("for (;;) a.#valid;" => true; "xxx valid")]
#[test_case("for (a.#valid;;) stmt;" => true; "Cxx init valid")]
#[test_case("for (c;;) a.#valid;" => true; "Cxx stmt valid")]
#[test_case("for (a.#valid;c2;) stmt;" => true; "CCx init valid")]
#[test_case("for (c1;a.#valid;) stmt;" => true; "CCx cond valid")]
#[test_case("for (c1;c2;) a.#valid;" => true; "CCx stmt valid")]
#[test_case("for (a.#valid;c2;c3) stmt;" => true; "CCC init valid")]
#[test_case("for (c1;a.#valid;c3) stmt;" => true; "CCC cond valid")]
#[test_case("for (c1;c2;a.#valid) stmt;" => true; "CCC inc valid")]
#[test_case("for (c1;c2;c3) a.#valid;" => true; "CCC stmt valid")]
#[test_case("for (;a.#valid;c3) stmt;" => true; "xCC cond valid")]
#[test_case("for (;c2;a.#valid) stmt;" => true; "xCC inc valid")]
#[test_case("for (;c2;c3) a.#valid;" => true; "xCC stmt valid")]
#[test_case("for (;a.#valid;) stmt;" => true; "xCx cond valid")]
#[test_case("for (;c2;) a.#valid;" => true; "xCx stmt valid")]
#[test_case("for (;;a.#valid) stmt;" => true; "xxC inc valid")]
#[test_case("for (;;c3) a.#valid;" => true; "xxC stmt valid")]
#[test_case("for (a.#valid;;c3) stmt;" => true; "CxC init valid")]
#[test_case("for (c1;;a.#valid) stmt;" => true; "CxC inc valid")]
#[test_case("for (c1;;c3) a.#valid;" => true; "CxC stmt valid")]
#[test_case("for (;;) a.#invalid;" => false; "xxx invalid")]
#[test_case("for (a.#invalid;;) stmt;" => false; "Cxx init invalid")]
#[test_case("for (c;;) a.#invalid;" => false; "Cxx stmt invalid")]
#[test_case("for (a.#invalid;c2;) stmt;" => false; "CCx init invalid")]
#[test_case("for (c1;a.#invalid;) stmt;" => false; "CCx cond invalid")]
#[test_case("for (c1;c2;) a.#invalid;" => false; "CCx stmt invalid")]
#[test_case("for (a.#invalid;c2;c3) stmt;" => false; "CCC init invalid")]
#[test_case("for (c1;a.#invalid;c3) stmt;" => false; "CCC cond invalid")]
#[test_case("for (c1;c2;a.#invalid) stmt;" => false; "CCC inc invalid")]
#[test_case("for (c1;c2;c3) a.#invalid;" => false; "CCC stmt invalid")]
#[test_case("for (;a.#invalid;c3) stmt;" => false; "xCC cond invalid")]
#[test_case("for (;c2;a.#invalid) stmt;" => false; "xCC inc invalid")]
#[test_case("for (;c2;c3) a.#invalid;" => false; "xCC stmt invalid")]
#[test_case("for (;a.#invalid;) stmt;" => false; "xCx cond invalid")]
#[test_case("for (;c2;) a.#invalid;" => false; "xCx stmt invalid")]
#[test_case("for (;;a.#invalid) stmt;" => false; "xxC inc invalid")]
#[test_case("for (;;c3) a.#invalid;" => false; "xxC stmt invalid")]
#[test_case("for (a.#invalid;;c3) stmt;" => false; "CxC init invalid")]
#[test_case("for (c1;;a.#invalid) stmt;" => false; "CxC inc invalid")]
#[test_case("for (c1;;c3) a.#invalid;" => false; "CxC stmt invalid")]
#[test_case("for (var i=a.#valid;c;s) stmt;" => true; "var CC init valid")]
#[test_case("for (var i=a;a.#valid;s) stmt;" => true; "var CC cond valid")]
#[test_case("for (var i=a;c;a.#valid) stmt;" => true; "var CC step valid")]
#[test_case("for (var i=a;c;s) a.#valid;" => true; "var CC stmt valid")]
#[test_case("for (var i=a.#valid;;s) stmt;" => true; "var xC init valid")]
#[test_case("for (var i=a;;a.#valid) stmt;" => true; "var xC step valid")]
#[test_case("for (var i=a;;s) a.#valid;" => true; "var xC stmt valid")]
#[test_case("for (var i=a.#valid;c;) stmt;" => true; "var Cx init valid")]
#[test_case("for (var i=a;a.#valid;) stmt;" => true; "var Cx cond valid")]
#[test_case("for (var i=a;c;) a.#valid;" => true; "var Cx stmt valid")]
#[test_case("for (var i=a.#valid;;) stmt;" => true; "var xx init valid")]
#[test_case("for (var i=a;;) a.#valid;" => true; "var xx stmt valid")]
#[test_case("for (var i=a.#invalid;c;s) stmt;" => false; "var CC init invalid")]
#[test_case("for (var i=a;a.#invalid;s) stmt;" => false; "var CC cond invalid")]
#[test_case("for (var i=a;c;a.#invalid) stmt;" => false; "var CC step invalid")]
#[test_case("for (var i=a;c;s) a.#invalid;" => false; "var CC stmt invalid")]
#[test_case("for (var i=a.#invalid;;s) stmt;" => false; "var xC init invalid")]
#[test_case("for (var i=a;;a.#invalid) stmt;" => false; "var xC step invalid")]
#[test_case("for (var i=a;;s) a.#invalid;" => false; "var xC stmt invalid")]
#[test_case("for (var i=a.#invalid;c;) stmt;" => false; "var Cx init invalid")]
#[test_case("for (var i=a;a.#invalid;) stmt;" => false; "var Cx cond invalid")]
#[test_case("for (var i=a;c;) a.#invalid;" => false; "var Cx stmt invalid")]
#[test_case("for (var i=a.#invalid;;) stmt;" => false; "var xx init invalid")]
#[test_case("for (var i=a;;) a.#invalid;" => false; "var xx stmt invalid")]
#[test_case("for (let i=a.#valid;c;s) stmt;" => true; "let CC init valid")]
#[test_case("for (let i=a;a.#valid;s) stmt;" => true; "let CC cond valid")]
#[test_case("for (let i=a;c;a.#valid) stmt;" => true; "let CC step valid")]
#[test_case("for (let i=a;c;s) a.#valid;" => true; "let CC stmt valid")]
#[test_case("for (let i=a.#valid;;s) stmt;" => true; "let xC init valid")]
#[test_case("for (let i=a;;a.#valid) stmt;" => true; "let xC step valid")]
#[test_case("for (let i=a;;s) a.#valid;" => true; "let xC stmt valid")]
#[test_case("for (let i=a.#valid;c;) stmt;" => true; "let Cx init valid")]
#[test_case("for (let i=a;a.#valid;) stmt;" => true; "let Cx cond valid")]
#[test_case("for (let i=a;c;) a.#valid;" => true; "let Cx stmt valid")]
#[test_case("for (let i=a.#valid;;) stmt;" => true; "let xx init valid")]
#[test_case("for (let i=a;;) a.#valid;" => true; "let xx stmt valid")]
#[test_case("for (let i=a.#invalid;c;s) stmt;" => false; "let CC init invalid")]
#[test_case("for (let i=a;a.#invalid;s) stmt;" => false; "let CC cond invalid")]
#[test_case("for (let i=a;c;a.#invalid) stmt;" => false; "let CC step invalid")]
#[test_case("for (let i=a;c;s) a.#invalid;" => false; "let CC stmt invalid")]
#[test_case("for (let i=a.#invalid;;s) stmt;" => false; "let xC init invalid")]
#[test_case("for (let i=a;;a.#invalid) stmt;" => false; "let xC step invalid")]
#[test_case("for (let i=a;;s) a.#invalid;" => false; "let xC stmt invalid")]
#[test_case("for (let i=a.#invalid;c;) stmt;" => false; "let Cx init invalid")]
#[test_case("for (let i=a;a.#invalid;) stmt;" => false; "let Cx cond invalid")]
#[test_case("for (let i=a;c;) a.#invalid;" => false; "let Cx stmt invalid")]
#[test_case("for (let i=a.#invalid;;) stmt;" => false; "let xx init invalid")]
#[test_case("for (let i=a;;) a.#invalid;" => false; "let xx stmt invalid")]
fn for_statement_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ForStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod for_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("for (package; implements; interface) private;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED, PRIVATE_NOT_ALLOWED]); "for ( Expression1 ; Expression2 ; Expression3 ) Statement")]
    #[test_case("for (; implements; interface) private;", true => set(&[ IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED, PRIVATE_NOT_ALLOWED]); "for (; Expression2 ; Expression3 ) Statement")]
    #[test_case("for (package; ; interface) private;", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED, PRIVATE_NOT_ALLOWED]); "for ( Expression1 ;; Expression3 ) Statement")]
    #[test_case("for (package; implements; ) private;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, PRIVATE_NOT_ALLOWED]); "for ( Expression1 ; Expression2 ;) Statement")]
    #[test_case("for (; ; interface) private;", true => set(&[ INTERFACE_NOT_ALLOWED, PRIVATE_NOT_ALLOWED]); "for (;; Expression3 ) Statement")]
    #[test_case("for (package; ; ) private;", true => set(&[PACKAGE_NOT_ALLOWED, PRIVATE_NOT_ALLOWED]); "for ( Expression1 ;;) Statement")]
    #[test_case("for (; implements; ) private;", true => set(&[ IMPLEMENTS_NOT_ALLOWED, PRIVATE_NOT_ALLOWED]); "for (; Expression2 ;) Statement")]
    #[test_case("for (; ; ) private;", true => set(&[ PRIVATE_NOT_ALLOWED]); "for (;;) Statement")]
    #[test_case("for (var package; implements; interface) private;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED, PRIVATE_NOT_ALLOWED]); "for ( VariableDeclarationList ; Expression1 ; Expression2 ) Statement")]
    #[test_case("for (var package; ; interface) private;", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED, PRIVATE_NOT_ALLOWED]); "for ( VariableDeclarationList ;  ; Expression2 ) Statement")]
    #[test_case("for (var package; implements; ) private;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, PRIVATE_NOT_ALLOWED]); "for ( VariableDeclarationList ; Expression1 ;  ) Statement")]
    #[test_case("for (var package; ; ) private;", true => set(&[PACKAGE_NOT_ALLOWED, PRIVATE_NOT_ALLOWED]); "for ( VariableDeclarationList ;  ;  ) Statement")]
    #[test_case("for (let package; implements; interface) private;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED, PRIVATE_NOT_ALLOWED]); "for ( LexicalDeclaration ; Expression1 ; Expression2 ) Statement")]
    #[test_case("for (let package; ; interface) private;", true => set(&[PACKAGE_NOT_ALLOWED, INTERFACE_NOT_ALLOWED, PRIVATE_NOT_ALLOWED]); "for ( LexicalDeclaration ;  ; Expression2 ) Statement")]
    #[test_case("for (let package; implements; ) private;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, PRIVATE_NOT_ALLOWED]); "for ( LexicalDeclaration ; Expression1 ;  ) Statement")]
    #[test_case("for (let package; ; ) private;", true => set(&[PACKAGE_NOT_ALLOWED, PRIVATE_NOT_ALLOWED]); "for ( LexicalDeclaration ;  ;  ) Statement")]
    #[test_case("for (let a;;) { var a; }", false => set(&[A_LEXVARCLASH]); "lex/var clash")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        ForStatement::parse(&mut strictparser(src, strict), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict, false);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// FOR IN-OF STATEMENT
#[test]
fn for_in_of_statement_test_prettyerrors_01() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(a in b);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_prettyerrors_02() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(a of b);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_prettyerrors_03() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(var a in b);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_prettyerrors_04() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(var a of b);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_prettyerrors_05() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(let a in b);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_prettyerrors_06() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(let a of b);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_prettyerrors_07() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(a of b);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_prettyerrors_08() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(var a of b);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_prettyerrors_09() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(let a of b);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_prettyerrors_10() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for ({a} in b);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_prettyerrors_11() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for ({a} of b);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_prettyerrors_12() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await ({a} of b);"), Scanner::new(), true, true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_conciseerrors_01() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(a in b);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_conciseerrors_02() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(a of b);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_conciseerrors_03() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(var a in b);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_conciseerrors_04() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(var a of b);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_conciseerrors_05() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(let a in b);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_conciseerrors_06() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(let a of b);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_conciseerrors_07() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(a of b);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_conciseerrors_08() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(var a of b);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_conciseerrors_09() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(let a of b);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_conciseerrors_10() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for ({a} in b);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_conciseerrors_11() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for ({a} of b);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_conciseerrors_12() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await ({a} of b);"), Scanner::new(), true, true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_in_of_statement_test_var_declared_names_01() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(a in b){var c;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["c"]);
}
#[test]
fn for_in_of_statement_test_var_declared_names_02() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(let a in b){var c;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["c"]);
}
#[test]
fn for_in_of_statement_test_var_declared_names_03() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(a of b){var c;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["c"]);
}
#[test]
fn for_in_of_statement_test_var_declared_names_04() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(let a of b){var c;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["c"]);
}
#[test]
fn for_in_of_statement_test_var_declared_names_05() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(a of b){var c;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["c"]);
}
#[test]
fn for_in_of_statement_test_var_declared_names_06() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(let a of b){var c;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["c"]);
}
#[test]
fn for_in_of_statement_test_var_declared_names_07() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(var a in b){var c;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a", "c"]);
}
#[test]
fn for_in_of_statement_test_var_declared_names_08() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(var a of b){var c;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a", "c"]);
}
#[test]
fn for_in_of_statement_test_var_declared_names_09() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(var a of b){var c;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["a", "c"]);
}
#[test]
fn for_in_of_statement_test_var_declared_names_10() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for ({a} in b){var c;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["c"]);
}
#[test]
fn for_in_of_statement_test_var_declared_names_11() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for ({a} of b){var c;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["c"]);
}
#[test]
fn for_in_of_statement_test_var_declared_names_12() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await({a} of b){var c;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.var_declared_names(), &["c"]);
}
#[test]
fn for_in_of_statement_test_contains_undefined_break_target_01() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(a in b){break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[] as &[JSString]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn for_in_of_statement_test_contains_undefined_break_target_02() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(let a in b){break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[] as &[JSString]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn for_in_of_statement_test_contains_undefined_break_target_03() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(a of b){break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[] as &[JSString]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn for_in_of_statement_test_contains_undefined_break_target_04() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(let a of b){break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[] as &[JSString]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn for_in_of_statement_test_contains_undefined_break_target_05() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(a of b){break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[] as &[JSString]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn for_in_of_statement_test_contains_undefined_break_target_06() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(let a of b){break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[] as &[JSString]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn for_in_of_statement_test_contains_undefined_break_target_07() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(var a in b){break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[] as &[JSString]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn for_in_of_statement_test_contains_undefined_break_target_08() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for(var a of b){break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[] as &[JSString]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn for_in_of_statement_test_contains_undefined_break_target_09() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(var a of b){break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[] as &[JSString]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn for_in_of_statement_test_contains_undefined_break_target_10() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for ({a} in b){break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[] as &[JSString]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn for_in_of_statement_test_contains_undefined_break_target_11() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for ({a} of b){break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[] as &[JSString]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}
#[test]
fn for_in_of_statement_test_contains_undefined_break_target_12() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await({a} of b){break t;}"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_undefined_break_target(&[] as &[JSString]), true);
    assert_eq!(item.contains_undefined_break_target(&[JSString::from("t")]), false);
}

#[test]
fn for_in_of_statement_test_contains_01() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (a[0] in b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_02() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (a in 0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_03() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (a in b)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_04() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (a in b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn for_in_of_statement_test_contains_05() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (var [a=0] in b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_06() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (var a in 0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_07() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (var a in b)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_08() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (var a in b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn for_in_of_statement_test_contains_09() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (let [a=0] in b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_10() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (let a in 0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_11() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (let a in b)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_12() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (let a in b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn for_in_of_statement_test_contains_13() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (a[0] of b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_14() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (a of 0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_15() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (a of b)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_16() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (a of b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn for_in_of_statement_test_contains_17() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(a[0] of b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_18() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(a of 0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_19() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(a of b)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_20() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(a of b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn for_in_of_statement_test_contains_21() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (var [a=0] of b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_22() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (var a of 0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_23() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (var a of b)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_24() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (var a of b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn for_in_of_statement_test_contains_25() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(var [a=0] of b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_26() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(var a of 0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_27() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(var a of b)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_28() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(var a of b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn for_in_of_statement_test_contains_29() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (let [a=0] of b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_30() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (let a of 0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_31() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (let a of b)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_32() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for (let a of b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn for_in_of_statement_test_contains_33() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(let [a=0] of b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_34() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(let a of 0);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_35() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(let a of b)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_36() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await(let a of b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn for_in_of_statement_test_contains_37() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for ({a} of b)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_38() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for ({a} of b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn for_in_of_statement_test_contains_39() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for ({a} in b)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_40() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for ({a} in b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn for_in_of_statement_test_contains_41() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await({a} of b)0;"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_in_of_statement_test_contains_42() {
    let (item, _) = ForInOfStatement::parse(&mut newparser("for await({a} of b);"), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}

fn for_in_of_cdl_check(src: &str) {
    let (item, _) = ForInOfStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
}
#[test]
fn for_in_of_statement_test_contains_duplicate_labels() {
    for_in_of_cdl_check("for(a in b){t:;}");
    for_in_of_cdl_check("for({a} in b){t:;}");
    for_in_of_cdl_check("for(var a in b){t:;}");
    for_in_of_cdl_check("for(let a in b){t:;}");
    for_in_of_cdl_check("for(a of b){t:;}");
    for_in_of_cdl_check("for({a} of b){t:;}");
    for_in_of_cdl_check("for(var a of b){t:;}");
    for_in_of_cdl_check("for(let a of b){t:;}");
    for_in_of_cdl_check("for await(a of b){t:;}");
    for_in_of_cdl_check("for await({a} of b){t:;}");
    for_in_of_cdl_check("for await(var a of b){t:;}");
    for_in_of_cdl_check("for await(let a of b){t:;}");
}

#[allow(clippy::type_complexity)]
mod for_in_of_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("for (a in b) continue x;" => (false, true); "for (a in b) continue x;")]
    #[test_case("for ({a} in b) continue x;" => (false, true); "for ({a} in b) continue x; (dstr)")]
    #[test_case("for (var a in b) continue x;" => (false, true); "for (var a in b) continue x;")]
    #[test_case("for (let a in b) continue x;" => (false, true); "for (let a in b) continue x;")]
    #[test_case("for (a of b) continue x;" => (false, true); "for (a of b) continue x;")]
    #[test_case("for ({a} of b) continue x;" => (false, true); "for ({a} of b) continue x; (dstr)")]
    #[test_case("for (var a of b) continue x;" => (false, true); "for (var a of b) continue x;")]
    #[test_case("for (let a of b) continue x;" => (false, true); "for (let a of b) continue x;")]
    #[test_case("for await (a of b) continue x;" => (false, true); "for await (a of b) continue x;")]
    #[test_case("for await ({a} of b) continue x;" => (false, true); "for await ({a} of b) continue x; (dstr)")]
    #[test_case("for await (var a of b) continue x;" => (false, true); "for await (var a of b) continue x;")]
    #[test_case("for await (let a of b) continue x;" => (false, true); "for await (let a of b) continue x;")]
    fn contains_undefined_continue_target(src: &str) -> (bool, bool) {
        let (item, _) = ForInOfStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
        (item.contains_undefined_continue_target(&[JSString::from("x")]), item.contains_undefined_continue_target(&[JSString::from("y")]))
    }
    #[test_case("for ([a=x.#valid] in c) stmt;" => true; "dstr in init valid")]
    #[test_case("for ([a=b] in x.#valid) stmt;" => true; "dstr in target valid")]
    #[test_case("for ([a=b] in c) x.#valid;" => true; "dstr in stmt valid")]
    #[test_case("for (x.#valid in c) stmt;" => true; "in init valid")]
    #[test_case("for (a in x.#valid) stmt;" => true; "in target valid")]
    #[test_case("for (a in c) x.#valid;" => true; "in stmt valid")]
    #[test_case("for ([a=x.#valid] of c) stmt;" => true; "dstr of init valid")]
    #[test_case("for ([a=b] of x.#valid) stmt;" => true; "dstr of target valid")]
    #[test_case("for ([a=b] of c) x.#valid;" => true; "dstr of stmt valid")]
    #[test_case("for (x.#valid of c) stmt;" => true; "of init valid")]
    #[test_case("for (a of x.#valid) stmt;" => true; "of target valid")]
    #[test_case("for (a of c) x.#valid;" => true; "of stmt valid")]
    #[test_case("for await (x.#valid of c) stmt;" => true; "await init valid")]
    #[test_case("for await (a of x.#valid) stmt;" => true; "await target valid")]
    #[test_case("for await (a of c) x.#valid;" => true; "await stmt valid")]
    #[test_case("for await ([a=x.#valid] of c) stmt;" => true; "dstr await init valid")]
    #[test_case("for await ([a=b] of x.#valid) stmt;" => true; "dstr await target valid")]
    #[test_case("for await ([a=b] of c) x.#valid;" => true; "dstr await stmt valid")]
    #[test_case("for (var [a=x.#valid] in c) stmt;" => true; "varin init valid")]
    #[test_case("for (var [a=b] in x.#valid) stmt;" => true; "varin target valid")]
    #[test_case("for (var [a=b] in c) x.#valid;" => true; "varin stmt valid")]
    #[test_case("for (var [a=x.#valid] of c) stmt;" => true; "varof init valid")]
    #[test_case("for (var [a=b] of x.#valid) stmt;" => true; "varof target valid")]
    #[test_case("for (var [a=b] of c) x.#valid;" => true; "varof stmt valid")]
    #[test_case("for await (var [a=x.#valid] of c) stmt;" => true; "awaitvar init valid")]
    #[test_case("for await (var [a=b] of x.#valid) stmt;" => true; "awaitvar target valid")]
    #[test_case("for await (var [a=b] of c) x.#valid;" => true; "awaitvar stmt valid")]
    #[test_case("for (let [a=x.#valid] in c) stmt;" => true; "letin init valid")]
    #[test_case("for (let [a=b] in x.#valid) stmt;" => true; "letin target valid")]
    #[test_case("for (let [a=b] in c) x.#valid;" => true; "letin stmt valid")]
    #[test_case("for (let [a=x.#valid] of c) stmt;" => true; "letof init valid")]
    #[test_case("for (let [a=b] of x.#valid) stmt;" => true; "letof target valid")]
    #[test_case("for (let [a=b] of c) x.#valid;" => true; "letof stmt valid")]
    #[test_case("for await (let [a=x.#valid] of c) stmt;" => true; "awaitlet init valid")]
    #[test_case("for await (let [a=b] of x.#valid) stmt;" => true; "awaitlet target valid")]
    #[test_case("for await (let [a=b] of c) x.#valid;" => true; "awaitlet stmt valid")]
    #[test_case("for ([a=x.#invalid] in c) stmt;" => false; "dstr in init invalid")]
    #[test_case("for ([a=b] in x.#invalid) stmt;" => false; "dstr in target invalid")]
    #[test_case("for ([a=b] in c) x.#invalid;" => false; "dstr in stmt invalid")]
    #[test_case("for (x.#invalid in c) stmt;" => false; "in init invalid")]
    #[test_case("for (a in x.#invalid) stmt;" => false; "in target invalid")]
    #[test_case("for (a in c) x.#invalid;" => false; "in stmt invalid")]
    #[test_case("for ([a=x.#invalid] of c) stmt;" => false; "dstr of init invalid")]
    #[test_case("for ([a=b] of x.#invalid) stmt;" => false; "dstr of target invalid")]
    #[test_case("for ([a=b] of c) x.#invalid;" => false; "dstr of stmt invalid")]
    #[test_case("for (x.#invalid of c) stmt;" => false; "of init invalid")]
    #[test_case("for (a of x.#invalid) stmt;" => false; "of target invalid")]
    #[test_case("for (a of c) x.#invalid;" => false; "of stmt invalid")]
    #[test_case("for await ([a=x.#invalid] of c) stmt;" => false; "dstr await init invalid")]
    #[test_case("for await ([a=b] of x.#invalid) stmt;" => false; "dstr await target invalid")]
    #[test_case("for await ([a=b] of c) x.#invalid;" => false; "dstr await stmt invalid")]
    #[test_case("for await (x.#invalid of c) stmt;" => false; "await init invalid")]
    #[test_case("for await (a of x.#invalid) stmt;" => false; "await target invalid")]
    #[test_case("for await (a of c) x.#invalid;" => false; "await stmt invalid")]
    #[test_case("for (var [a=x.#invalid] in c) stmt;" => false; "varin init invalid")]
    #[test_case("for (var [a=b] in x.#invalid) stmt;" => false; "varin target invalid")]
    #[test_case("for (var [a=b] in c) x.#invalid;" => false; "varin stmt invalid")]
    #[test_case("for (var [a=x.#invalid] of c) stmt;" => false; "varof init invalid")]
    #[test_case("for (var [a=b] of x.#invalid) stmt;" => false; "varof target invalid")]
    #[test_case("for (var [a=b] of c) x.#invalid;" => false; "varof stmt invalid")]
    #[test_case("for await (var [a=x.#invalid] of c) stmt;" => false; "awaitvar init invalid")]
    #[test_case("for await (var [a=b] of x.#invalid) stmt;" => false; "awaitvar target invalid")]
    #[test_case("for await (var [a=b] of c) x.#invalid;" => false; "awaitvar stmt invalid")]
    #[test_case("for (let [a=x.#invalid] in c) stmt;" => false; "letin init invalid")]
    #[test_case("for (let [a=b] in x.#invalid) stmt;" => false; "letin target invalid")]
    #[test_case("for (let [a=b] in c) x.#invalid;" => false; "letin stmt invalid")]
    #[test_case("for (let [a=x.#invalid] of c) stmt;" => false; "letof init invalid")]
    #[test_case("for (let [a=b] of x.#invalid) stmt;" => false; "letof target invalid")]
    #[test_case("for (let [a=b] of c) x.#invalid;" => false; "letof stmt invalid")]
    #[test_case("for await (let [a=x.#invalid] of c) stmt;" => false; "awaitlet init invalid")]
    #[test_case("for await (let [a=b] of x.#invalid) stmt;" => false; "awaitlet target invalid")]
    #[test_case("for await (let [a=b] of c) x.#invalid;" => false; "awaitlet stmt invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        let (item, _) = ForInOfStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
        item.all_private_identifiers_valid(&[JSString::from("valid")])
    }
    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", ForInOfStatement::parse(&mut newparser("for (x in y);"), Scanner::new(), true, true, true).unwrap().0), "");
    }

    #[test_case("for(x in y);", true => Ok((
        expected_scan(12),
        sv(&["ForInOfStatement: for ( x in y ) ;", "LeftHandSideExpression: x", "Expression: y", "Statement: ;"]),
        sv(&["ForInOfStatement: for ( x in y ) ;", "Keyword: for", "Punctuator: (", "IdentifierName: x", "Keyword: in", "IdentifierName: y", "Punctuator: )", "Punctuator: ;"])
    )); "lhs in")]
    #[test_case("for ({a} in b);", true => Ok((
        expected_scan(15),
        sv(&["ForInOfStatement: for ( { a } in b ) ;", "AssignmentPattern: { a }", "Expression: b", "Statement: ;"]),
        sv(&["ForInOfStatement: for ( { a } in b ) ;", "Keyword: for", "Punctuator: (", "ObjectAssignmentPattern: { a }", "Keyword: in", "IdentifierName: b", "Punctuator: )", "Punctuator: ;"])
    )); "Destructuring in")]
    #[test_case("for(var x in y);", true => Ok((
        expected_scan(16),
        sv(&["ForInOfStatement: for ( var x in y ) ;", "ForBinding: x", "Expression: y", "Statement: ;"]),
        sv(&["ForInOfStatement: for ( var x in y ) ;", "Keyword: for", "Punctuator: (", "Keyword: var", "IdentifierName: x", "Keyword: in", "IdentifierName: y", "Punctuator: )", "Punctuator: ;"])
    )); "var in")]
    #[test_case("for(let x in y);", true => Ok((
        expected_scan(16),
        sv(&["ForInOfStatement: for ( let x in y ) ;", "ForDeclaration: let x", "Expression: y", "Statement: ;"]),
        sv(&["ForInOfStatement: for ( let x in y ) ;", "Keyword: for", "Punctuator: (", "ForDeclaration: let x", "Keyword: in", "IdentifierName: y", "Punctuator: )", "Punctuator: ;"])
    )); "let in")]
    #[test_case("for(x of y);", true => Ok((
        expected_scan(12),
        sv(&["ForInOfStatement: for ( x of y ) ;", "LeftHandSideExpression: x", "AssignmentExpression: y", "Statement: ;"]),
        sv(&["ForInOfStatement: for ( x of y ) ;", "Keyword: for", "Punctuator: (", "IdentifierName: x", "Keyword: of", "IdentifierName: y", "Punctuator: )", "Punctuator: ;"])
    )); "lhs of")]
    #[test_case("for ({a} of b);", true => Ok((
        expected_scan(15),
        sv(&["ForInOfStatement: for ( { a } of b ) ;", "AssignmentPattern: { a }", "AssignmentExpression: b", "Statement: ;"]),
        sv(&["ForInOfStatement: for ( { a } of b ) ;", "Keyword: for", "Punctuator: (", "ObjectAssignmentPattern: { a }", "Keyword: of", "IdentifierName: b", "Punctuator: )", "Punctuator: ;"])
    )); "Destructuring of")]
    #[test_case("for(var x of y);", true => Ok((
        expected_scan(16),
        sv(&["ForInOfStatement: for ( var x of y ) ;", "ForBinding: x", "AssignmentExpression: y", "Statement: ;"]),
        sv(&["ForInOfStatement: for ( var x of y ) ;", "Keyword: for", "Punctuator: (", "Keyword: var", "IdentifierName: x", "Keyword: of", "IdentifierName: y", "Punctuator: )", "Punctuator: ;"])
    )); "var of")]
    #[test_case("for(let x of y);", true => Ok((
        expected_scan(16),
        sv(&["ForInOfStatement: for ( let x of y ) ;", "ForDeclaration: let x", "AssignmentExpression: y", "Statement: ;"]),
        sv(&["ForInOfStatement: for ( let x of y ) ;", "Keyword: for", "Punctuator: (", "ForDeclaration: let x", "Keyword: of", "IdentifierName: y", "Punctuator: )", "Punctuator: ;"])
    )); "let of")]
    #[test_case("for await(x of y);", true => Ok((
        expected_scan(18),
        sv(&["ForInOfStatement: for await ( x of y ) ;", "LeftHandSideExpression: x", "AssignmentExpression: y", "Statement: ;"]),
        sv(&["ForInOfStatement: for await ( x of y ) ;", "Keyword: for", "Keyword: await", "Punctuator: (", "IdentifierName: x", "Keyword: of", "IdentifierName: y", "Punctuator: )", "Punctuator: ;"])
    )); "lhs await of")]
    #[test_case("for await({a} of b);", true => Ok((
        expected_scan(20),
        sv(&["ForInOfStatement: for await ( { a } of b ) ;", "AssignmentPattern: { a }", "AssignmentExpression: b", "Statement: ;"]),
        sv(&["ForInOfStatement: for await ( { a } of b ) ;", "Keyword: for", "Keyword: await", "Punctuator: (", "ObjectAssignmentPattern: { a }", "Keyword: of", "IdentifierName: b", "Punctuator: )", "Punctuator: ;"])
    )); "Destructuring await of")]
    #[test_case("for await(var x of y);", true => Ok((
        expected_scan(22),
        sv(&["ForInOfStatement: for await ( var x of y ) ;", "ForBinding: x", "AssignmentExpression: y", "Statement: ;"]),
        sv(&["ForInOfStatement: for await ( var x of y ) ;", "Keyword: for", "Keyword: await", "Punctuator: (", "Keyword: var", "IdentifierName: x", "Keyword: of", "IdentifierName: y", "Punctuator: )", "Punctuator: ;"])
    )); "var await of")]
    #[test_case("for await(let x of y);", true => Ok((
        expected_scan(22),
        sv(&["ForInOfStatement: for await ( let x of y ) ;", "ForDeclaration: let x", "AssignmentExpression: y", "Statement: ;"]),
        sv(&["ForInOfStatement: for await ( let x of y ) ;", "Keyword: for", "Keyword: await", "Punctuator: (", "ForDeclaration: let x", "Keyword: of", "IdentifierName: y", "Punctuator: )", "Punctuator: ;"])
    )); "let await of")]
    #[test_case("", true => Err((PECode::KeywordExpected(Keyword::For), 0)); "empty")]
    #[test_case("for", true => Err((PECode::PunctuatorExpected(Punctuator::LeftParen), 0)); "_for")]
    #[test_case("for(", true => Err((PECode::ForInOfDefinitionError, 0)); "for(")]
    #[test_case("for(var", true => Err((PECode::ParseNodeExpected(ParseNodeKind::ForBinding), 0)); "for(var")]
    #[test_case("for(var a", true => Err((PECode::OneOfKeywordExpected(vec![Keyword::Of, Keyword::In]), 0)); "for(var a")]
    #[test_case("for(var a of", true => Err((PECode::ParseNodeExpected(ParseNodeKind::AssignmentExpression), 0)); "for(var a of")]
    #[test_case("for(var a of b", true => Err((PECode::PunctuatorExpected(Punctuator::RightParen), 0)); "for(var a of b")]
    #[test_case("for(var a of b)", true => Err((PECode::ParseNodeExpected(ParseNodeKind::Statement), 0)); "for(var a of b)")]
    #[test_case("for await(var a in", true => Err((PECode::KeywordExpected(Keyword::Of), -3)); "for await(var a in")]
    #[test_case("for(var a in b", true => Err((PECode::PunctuatorExpected(Punctuator::RightParen), 0)); "for(var a in b")]
    #[test_case("for(var a in b)", true => Err((PECode::ParseNodeExpected(ParseNodeKind::Statement), 0)); "for(var a in b)")]
    #[test_case("for(let", true => Err((PECode::ParseNodeExpected(ParseNodeKind::ForBinding), 0)); "for(let")]
    #[test_case("for(let a", true => Err((PECode::OneOfKeywordExpected(vec![Keyword::Of, Keyword::In]), 0)); "for(let a")]
    #[test_case("for await(let a", true => Err((PECode::KeywordExpected(Keyword::Of), 0)); "for await(let a")]
    #[test_case("for(let a of", true => Err((PECode::ParseNodeExpected(ParseNodeKind::AssignmentExpression), 0)); "for(let a of")]
    #[test_case("for(let a of b", true => Err((PECode::PunctuatorExpected(Punctuator::RightParen), 0)); "for(let a of b")]
    #[test_case("for(let a of b)", true => Err((PECode::ParseNodeExpected(ParseNodeKind::Statement), 0)); "for(let a of b)")]
    #[test_case("for(let a in", true => Err((PECode::ParseNodeExpected(ParseNodeKind::Expression), 0)); "for(let a in")]
    #[test_case("for(let a in b", true => Err((PECode::PunctuatorExpected(Punctuator::RightParen), 0)); "for(let a in b")]
    #[test_case("for(let a in b)", true => Err((PECode::ParseNodeExpected(ParseNodeKind::Statement), 0)); "for(let a in b)")]
    #[test_case("for await(let", true => Err((PECode::ParseNodeExpected(ParseNodeKind::ForBinding), 0)); "for await(let")]
    #[test_case("for(let[", true => Err((PECode::ParseNodeExpected(ParseNodeKind::BindingElement), 0)); "for(let[")]
    #[test_case("for(a", true => Err((PECode::OneOfKeywordExpected(vec![Keyword::Of, Keyword::In]), 0)); "for(a")]
    #[test_case("for await(a", true => Err((PECode::KeywordExpected(Keyword::Of), 0)); "for await(a")]
    #[test_case("for(a of", true => Err((PECode::ParseNodeExpected(ParseNodeKind::AssignmentExpression), 0)); "for(a of")]
    #[test_case("for(a of b", true => Err((PECode::PunctuatorExpected(Punctuator::RightParen), 0)); "for(a of b")]
    #[test_case("for(a of b)", true => Err((PECode::ParseNodeExpected(ParseNodeKind::Statement), 0)); "for(a of b)")]
    #[test_case("for(a in", true => Err((PECode::ParseNodeExpected(ParseNodeKind::Expression), 0)); "for(a in")]
    #[test_case("for(a in b", true => Err((PECode::PunctuatorExpected(Punctuator::RightParen), 0)); "for(a in b")]
    #[test_case("for(a in b)", true => Err((PECode::ParseNodeExpected(ParseNodeKind::Statement), 0)); "for(a in b)")]
    #[test_case("for await(a in b);", true => Err((PECode::KeywordExpected(Keyword::Of), -7)); "for await(a in b);")]
    #[test_case("for(var a in", true => Err((PECode::ParseNodeExpected(ParseNodeKind::Expression), 0)); "for(var a in")]
    #[test_case("for({a(){}} in b);", true => Err((PECode::OneOfPunctuatorExpected(vec![Punctuator::Comma, Punctuator::RightBrace]), -12)); "bad in destructure")]
    #[test_case("for({a(){}} of b);", true => Err((PECode::OneOfPunctuatorExpected(vec![Punctuator::Comma, Punctuator::RightBrace]), -12)); "bad of destructure")]
    #[test_case("for await(", false => Err((PECode::PunctuatorExpected(Punctuator::LeftParen), -7)); "not await mode")]
    fn parse(src: &str, await_flag: bool) -> Result<(Scanner, Vec<String>, Vec<String>), (PECode, i32)> {
        let after_idx = src.len() as u32 + 1;
        let (node, scanner) = ForInOfStatement::parse(&mut newparser(src), Scanner::new(), true, await_flag, true).map_err(|pe| pe.unpack(after_idx))?;
        let pretty_elements = pretty_data(&*node);
        let concise_elements = concise_data(&*node);
        Ok((scanner, pretty_elements, concise_elements))
    }

    const BAD_LET: &str = "‘let’ is not a valid binding identifier";
    const A_DUPLICATED: &str = "‘a’ already defined";
    const INVALID: &str = "Invalid assignment target";

    #[test_case("for (let let in a);", false => set(&[BAD_LET]); "bad let; in form")]
    #[test_case("for (let let of a);", false => set(&[BAD_LET]); "bad let; of form")]
    #[test_case("for await (let let of a);", false => set(&[BAD_LET]); "bad let; await form")]
    #[test_case("for (let a in b) { var a, x; }", false => set(&[A_LEXVARCLASH]); "var shadow; in form")]
    #[test_case("for (let a of b) { var a, x; }", false => set(&[A_LEXVARCLASH]); "var shadow; of form")]
    #[test_case("for await (let a of b) { var a, x; }", false => set(&[A_LEXVARCLASH]); "var shadow; await form")]
    #[test_case("for (let [a, a, a, a] in b);", false => set(&[A_DUPLICATED]); "duplicate decls - in")]
    #[test_case("for (let [a, a, a, a] of b);", false => set(&[A_DUPLICATED]); "duplicate decls - of")]
    #[test_case("for await (let [a, a, a, a] of b);", false => set(&[A_DUPLICATED]); "duplicate decls - await")]
    #[test_case("for ((3+4) in a);", false => set(&[INVALID]); "invalid lhs - in")]
    #[test_case("for ((3+4) of a);", false => set(&[INVALID]); "invalid lhs - of")]
    #[test_case("for await ((3+4) of a);", false => set(&[INVALID]); "invalid lhs - await")]
    #[test_case("for (package in implements) interface;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "for ( LeftHandSideExpression in Expression ) Statement")]
    #[test_case("for ({package} in implements) interface;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "for ( AssignmentPattern in Expression ) Statement")]
    #[test_case("for (var package in implements) interface;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "for ( var ForBinding in Expression ) Statement")]
    #[test_case("for (let package in implements) interface;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "for ( ForDeclaration in Expression ) Statement")]
    #[test_case("for (package of implements) interface;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "for ( LeftHandSideExpression of AssignmentExpresion ) Statement")]
    #[test_case("for ({package} of implements) interface;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "for ( AssignmentPattern of AssignmentExpresion ) Statement")]
    #[test_case("for (var package of implements) interface;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "for ( var ForBinding of AssignmentExpresion ) Statement")]
    #[test_case("for (let package of implements) interface;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "for ( ForDeclaration of AssignmentExpresion ) Statement")]
    #[test_case("for await (package of implements) interface;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "for await ( LeftHandSideExpression of AssignmentExpresion ) Statement")]
    #[test_case("for await ({package} of implements) interface;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "for await ( AssignmentPattern of AssignmentExpresion ) Statement")]
    #[test_case("for await (var package of implements) interface;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "for await ( var ForBinding of AssignmentExpresion ) Statement")]
    #[test_case("for await (let package of implements) interface;", true => set(&[PACKAGE_NOT_ALLOWED, IMPLEMENTS_NOT_ALLOWED, INTERFACE_NOT_ALLOWED]); "for await ( ForDeclaration of AssignmentExpresion ) Statement")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        ForInOfStatement::parse(&mut strictparser(src, strict), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict, false);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// FOR DECLARATION
#[test]
fn for_declaration_test_01() {
    let (node, scanner) = check(ForDeclaration::parse(&mut newparser("let a"), Scanner::new(), true, true));
    chk_scan(&scanner, 5);
    pretty_check(&*node, "ForDeclaration: let a", vec!["LetOrConst: let", "ForBinding: a"]);
    concise_check(&*node, "ForDeclaration: let a", vec!["Keyword: let", "IdentifierName: a"]);
    format!("{:?}", node);
}
#[test]
fn for_declaration_test_02() {
    let (node, scanner) = check(ForDeclaration::parse(&mut newparser("const a"), Scanner::new(), true, true));
    chk_scan(&scanner, 7);
    pretty_check(&*node, "ForDeclaration: const a", vec!["LetOrConst: const", "ForBinding: a"]);
    concise_check(&*node, "ForDeclaration: const a", vec!["Keyword: const", "IdentifierName: a"]);
    format!("{:?}", node);
}
#[test]
fn for_declaration_test_err_01() {
    check_err(ForDeclaration::parse(&mut newparser(""), Scanner::new(), true, true), "one of [‘let’, ‘const’] expected", 1, 1);
}
#[test]
fn for_declaration_test_err_02() {
    check_err(ForDeclaration::parse(&mut newparser("let"), Scanner::new(), true, true), "ForBinding expected", 1, 4);
}
#[test]
fn for_declaration_test_prettyerrors_1() {
    let (item, _) = ForDeclaration::parse(&mut newparser("let a"), Scanner::new(), true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_declaration_test_prettyerrors_2() {
    let (item, _) = ForDeclaration::parse(&mut newparser("const a"), Scanner::new(), true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_declaration_test_conciseerrors_1() {
    let (item, _) = ForDeclaration::parse(&mut newparser("let a"), Scanner::new(), true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_declaration_test_conciseerrors_2() {
    let (item, _) = ForDeclaration::parse(&mut newparser("const a"), Scanner::new(), true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_declaration_test_contains_01() {
    let (item, _) = ForDeclaration::parse(&mut newparser("let a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn for_declaration_test_contains_02() {
    let (item, _) = ForDeclaration::parse(&mut newparser("const [a=0]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test_case("let [a=b.#valid]" => true; "valid")]
#[test_case("let [a=b.#invalid]" => false; "invalid")]
fn for_declaration_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ForDeclaration::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod for_declaration {
    use super::*;
    use test_case::test_case;

    #[test_case("let a" => vec!["a"]; "LetOrConst ForBinding")]
    fn bound_names(src: &str) -> Vec<String> {
        ForDeclaration::parse(&mut newparser(src), Scanner::new(), true, true).unwrap().0.bound_names().into_iter().map(String::from).collect::<Vec<String>>()
    }

    #[test_case("let package", true => set(&[PACKAGE_NOT_ALLOWED]); "normal")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        ForDeclaration::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

// FOR BINDING
#[test]
fn for_binding_test_01() {
    let (node, scanner) = check(ForBinding::parse(&mut newparser("a"), Scanner::new(), true, true));
    chk_scan(&scanner, 1);
    pretty_check(&*node, "ForBinding: a", vec!["BindingIdentifier: a"]);
    concise_check(&*node, "IdentifierName: a", vec![]);
    format!("{:?}", node);
}
#[test]
fn for_binding_test_02() {
    let (node, scanner) = check(ForBinding::parse(&mut newparser("{a}"), Scanner::new(), true, true));
    chk_scan(&scanner, 3);
    pretty_check(&*node, "ForBinding: { a }", vec!["BindingPattern: { a }"]);
    concise_check(&*node, "ObjectBindingPattern: { a }", vec!["Punctuator: {", "IdentifierName: a", "Punctuator: }"]);
    format!("{:?}", node);
}
#[test]
fn for_binding_test_err_01() {
    check_err(ForBinding::parse(&mut newparser(""), Scanner::new(), true, true), "ForBinding expected", 1, 1);
}
#[test]
fn for_binding_test_prettyerrors_1() {
    let (item, _) = ForBinding::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_binding_test_prettyerrors_2() {
    let (item, _) = ForBinding::parse(&mut newparser("{a}"), Scanner::new(), true, true).unwrap();
    pretty_error_validate(&*item);
}
#[test]
fn for_binding_test_conciseerrors_1() {
    let (item, _) = ForBinding::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_binding_test_conciseerrors_2() {
    let (item, _) = ForBinding::parse(&mut newparser("{a}"), Scanner::new(), true, true).unwrap();
    concise_error_validate(&*item);
}
#[test]
fn for_binding_test_cache_01() {
    let mut parser = newparser("a");
    let (node, scanner) = ForBinding::parse(&mut parser, Scanner::new(), false, false).unwrap();
    let (node2, scanner2) = ForBinding::parse(&mut parser, Scanner::new(), false, false).unwrap();
    assert!(scanner == scanner2);
    assert!(Rc::ptr_eq(&node, &node2));
}
#[test]
fn for_binding_test_bound_names_01() {
    let (item, _) = ForBinding::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn for_binding_test_bound_names_02() {
    let (item, _) = ForBinding::parse(&mut newparser("{a}"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.bound_names(), &["a"]);
}
#[test]
fn for_binding_test_contains_01() {
    let (item, _) = ForBinding::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test]
fn for_binding_test_contains_02() {
    let (item, _) = ForBinding::parse(&mut newparser("[a=0]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), true);
}
#[test]
fn for_binding_test_contains_03() {
    let (item, _) = ForBinding::parse(&mut newparser("[a]"), Scanner::new(), true, true).unwrap();
    assert_eq!(item.contains(ParseNodeKind::Literal), false);
}
#[test_case("a" => true; "Identifier")]
#[test_case("[a=b.#valid]" => true; "pattern valid")]
#[test_case("[a=b.#invalid]" => false; "pattern invalid")]
fn for_binding_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ForBinding::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod for_binding {
    use super::*;
    use test_case::test_case;

    #[test_case("package", true => set(&[PACKAGE_NOT_ALLOWED]); "identifier")]
    #[test_case("[a, package]", true => set(&[PACKAGE_NOT_ALLOWED]); "pattern")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        let mut agent = test_agent();
        let mut errs = vec![];
        ForBinding::parse(&mut strictparser(src, strict), Scanner::new(), true, true).unwrap().0.early_errors(&mut agent, &mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}
