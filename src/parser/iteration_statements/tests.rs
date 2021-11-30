use super::testhelp::{check, check_err, chk_scan, newparser};
use super::*;
use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};
use crate::tests::test_agent;
use test_case::test_case;

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
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        IterationStatement::parse(&mut newparser("do{}while(1);"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
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
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        DoWhileStatement::parse(&mut newparser("do{}while(1);"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
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
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        WhileStatement::parse(&mut newparser("while(1);"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
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
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ForStatement::parse(&mut newparser("for(;;){}"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }
}

// FOR IN-OF STATEMENT
#[test]
fn for_in_of_statement_test_01() {
    let (node, scanner) = check(ForInOfStatement::parse(&mut newparser("for(x in y);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 12);
    pretty_check(&*node, "ForInOfStatement: for ( x in y ) ;", vec!["LeftHandSideExpression: x", "Expression: y", "Statement: ;"]);
    concise_check(
        &*node,
        "ForInOfStatement: for ( x in y ) ;",
        vec!["Keyword: for", "Punctuator: (", "IdentifierName: x", "Keyword: in", "IdentifierName: y", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_in_of_statement_test_02() {
    let (node, scanner) = check(ForInOfStatement::parse(&mut newparser("for(var x in y);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 16);
    pretty_check(&*node, "ForInOfStatement: for ( var x in y ) ;", vec!["ForBinding: x", "Expression: y", "Statement: ;"]);
    concise_check(
        &*node,
        "ForInOfStatement: for ( var x in y ) ;",
        vec!["Keyword: for", "Punctuator: (", "Keyword: var", "IdentifierName: x", "Keyword: in", "IdentifierName: y", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_in_of_statement_test_03() {
    let (node, scanner) = check(ForInOfStatement::parse(&mut newparser("for(let x in y);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 16);
    pretty_check(&*node, "ForInOfStatement: for ( let x in y ) ;", vec!["ForDeclaration: let x", "Expression: y", "Statement: ;"]);
    concise_check(
        &*node,
        "ForInOfStatement: for ( let x in y ) ;",
        vec!["Keyword: for", "Punctuator: (", "ForDeclaration: let x", "Keyword: in", "IdentifierName: y", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_in_of_statement_test_04() {
    let (node, scanner) = check(ForInOfStatement::parse(&mut newparser("for(x of y);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 12);
    pretty_check(&*node, "ForInOfStatement: for ( x of y ) ;", vec!["LeftHandSideExpression: x", "AssignmentExpression: y", "Statement: ;"]);
    concise_check(
        &*node,
        "ForInOfStatement: for ( x of y ) ;",
        vec!["Keyword: for", "Punctuator: (", "IdentifierName: x", "Keyword: of", "IdentifierName: y", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_in_of_statement_test_05() {
    let (node, scanner) = check(ForInOfStatement::parse(&mut newparser("for(var x of y);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 16);
    pretty_check(&*node, "ForInOfStatement: for ( var x of y ) ;", vec!["ForBinding: x", "AssignmentExpression: y", "Statement: ;"]);
    concise_check(
        &*node,
        "ForInOfStatement: for ( var x of y ) ;",
        vec!["Keyword: for", "Punctuator: (", "Keyword: var", "IdentifierName: x", "Keyword: of", "IdentifierName: y", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_in_of_statement_test_06() {
    let (node, scanner) = check(ForInOfStatement::parse(&mut newparser("for(let x of y);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 16);
    pretty_check(&*node, "ForInOfStatement: for ( let x of y ) ;", vec!["ForDeclaration: let x", "AssignmentExpression: y", "Statement: ;"]);
    concise_check(
        &*node,
        "ForInOfStatement: for ( let x of y ) ;",
        vec!["Keyword: for", "Punctuator: (", "ForDeclaration: let x", "Keyword: of", "IdentifierName: y", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_in_of_statement_test_07() {
    let (node, scanner) = check(ForInOfStatement::parse(&mut newparser("for await(x of y);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 18);
    pretty_check(&*node, "ForInOfStatement: for await ( x of y ) ;", vec!["LeftHandSideExpression: x", "AssignmentExpression: y", "Statement: ;"]);
    concise_check(
        &*node,
        "ForInOfStatement: for await ( x of y ) ;",
        vec!["Keyword: for", "Keyword: await", "Punctuator: (", "IdentifierName: x", "Keyword: of", "IdentifierName: y", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_in_of_statement_test_08() {
    let (node, scanner) = check(ForInOfStatement::parse(&mut newparser("for await(var x of y);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 22);
    pretty_check(&*node, "ForInOfStatement: for await ( var x of y ) ;", vec!["ForBinding: x", "AssignmentExpression: y", "Statement: ;"]);
    concise_check(
        &*node,
        "ForInOfStatement: for await ( var x of y ) ;",
        vec!["Keyword: for", "Keyword: await", "Punctuator: (", "Keyword: var", "IdentifierName: x", "Keyword: of", "IdentifierName: y", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_in_of_statement_test_09() {
    let (node, scanner) = check(ForInOfStatement::parse(&mut newparser("for await(let x of y);"), Scanner::new(), true, true, true));
    chk_scan(&scanner, 22);
    pretty_check(&*node, "ForInOfStatement: for await ( let x of y ) ;", vec!["ForDeclaration: let x", "AssignmentExpression: y", "Statement: ;"]);
    concise_check(
        &*node,
        "ForInOfStatement: for await ( let x of y ) ;",
        vec!["Keyword: for", "Keyword: await", "Punctuator: (", "ForDeclaration: let x", "Keyword: of", "IdentifierName: y", "Punctuator: )", "Punctuator: ;"],
    );
    format!("{:?}", node);
}
#[test]
fn for_in_of_statement_test_err_01() {
    check_err(ForInOfStatement::parse(&mut newparser(""), Scanner::new(), true, true, true), "‘for’ expected", 1, 1);
}
#[test]
fn for_in_of_statement_test_err_02() {
    check_err(ForInOfStatement::parse(&mut newparser("for"), Scanner::new(), true, true, true), "‘(’ expected", 1, 4);
}
#[test]
fn for_in_of_statement_test_err_03() {
    check_err(ForInOfStatement::parse(&mut newparser("for("), Scanner::new(), true, true, true), "‘let’, ‘var’, or a LeftHandSideExpression expected", 1, 5);
}
#[test]
fn for_in_of_statement_test_err_04() {
    check_err(ForInOfStatement::parse(&mut newparser("for(var"), Scanner::new(), true, true, true), "ForBinding expected", 1, 8);
}
#[test]
fn for_in_of_statement_test_err_05() {
    check_err(ForInOfStatement::parse(&mut newparser("for(var a"), Scanner::new(), true, true, true), "one of [‘of’, ‘in’] expected", 1, 10);
}
#[test]
fn for_in_of_statement_test_err_06() {
    check_err(ForInOfStatement::parse(&mut newparser("for(var a of"), Scanner::new(), true, true, true), "AssignmentExpression expected", 1, 13);
}
#[test]
fn for_in_of_statement_test_err_07() {
    check_err(ForInOfStatement::parse(&mut newparser("for(var a of b"), Scanner::new(), true, true, true), "‘)’ expected", 1, 15);
}
#[test]
fn for_in_of_statement_test_err_08() {
    check_err(ForInOfStatement::parse(&mut newparser("for(var a of b)"), Scanner::new(), true, true, true), "Statement expected", 1, 16);
}
#[test]
fn for_in_of_statement_test_err_09() {
    check_err(ForInOfStatement::parse(&mut newparser("for await(var a in"), Scanner::new(), true, true, true), "‘of’ expected", 1, 16);
}
#[test]
fn for_in_of_statement_test_err_10() {
    check_err(ForInOfStatement::parse(&mut newparser("for(var a in b"), Scanner::new(), true, true, true), "‘)’ expected", 1, 15);
}
#[test]
fn for_in_of_statement_test_err_11() {
    check_err(ForInOfStatement::parse(&mut newparser("for(var a in b)"), Scanner::new(), true, true, true), "Statement expected", 1, 16);
}
#[test]
fn for_in_of_statement_test_err_12() {
    check_err(ForInOfStatement::parse(&mut newparser("for(let"), Scanner::new(), true, true, true), "ForBinding expected", 1, 8);
}
#[test]
fn for_in_of_statement_test_err_13() {
    check_err(ForInOfStatement::parse(&mut newparser("for(let a"), Scanner::new(), true, true, true), "one of [‘of’, ‘in’] expected", 1, 10);
}
#[test]
fn for_in_of_statement_test_err_14() {
    check_err(ForInOfStatement::parse(&mut newparser("for await(let a"), Scanner::new(), true, true, true), "‘of’ expected", 1, 16);
}
#[test]
fn for_in_of_statement_test_err_15() {
    check_err(ForInOfStatement::parse(&mut newparser("for(let a of"), Scanner::new(), true, true, true), "AssignmentExpression expected", 1, 13);
}
#[test]
fn for_in_of_statement_test_err_16() {
    check_err(ForInOfStatement::parse(&mut newparser("for(let a of b"), Scanner::new(), true, true, true), "‘)’ expected", 1, 15);
}
#[test]
fn for_in_of_statement_test_err_17() {
    check_err(ForInOfStatement::parse(&mut newparser("for(let a of b)"), Scanner::new(), true, true, true), "Statement expected", 1, 16);
}
#[test]
fn for_in_of_statement_test_err_18() {
    check_err(ForInOfStatement::parse(&mut newparser("for(let a in"), Scanner::new(), true, true, true), "Expression expected", 1, 13);
}
#[test]
fn for_in_of_statement_test_err_19() {
    check_err(ForInOfStatement::parse(&mut newparser("for(let a in b"), Scanner::new(), true, true, true), "‘)’ expected", 1, 15);
}
#[test]
fn for_in_of_statement_test_err_20() {
    check_err(ForInOfStatement::parse(&mut newparser("for(let a in b)"), Scanner::new(), true, true, true), "Statement expected", 1, 16);
}
#[test]
fn for_in_of_statement_test_err_21() {
    check_err(ForInOfStatement::parse(&mut newparser("for await(let"), Scanner::new(), true, true, true), "ForBinding expected", 1, 14);
}
#[test]
fn for_in_of_statement_test_err_22() {
    check_err(ForInOfStatement::parse(&mut newparser("for(let["), Scanner::new(), true, true, true), "BindingElement expected", 1, 9);
}
#[test]
fn for_in_of_statement_test_err_23() {
    check_err(ForInOfStatement::parse(&mut newparser("for(a"), Scanner::new(), true, true, true), "one of [‘of’, ‘in’] expected", 1, 6);
}
#[test]
fn for_in_of_statement_test_err_24() {
    check_err(ForInOfStatement::parse(&mut newparser("for await(a"), Scanner::new(), true, true, true), "‘of’ expected", 1, 12);
}
#[test]
fn for_in_of_statement_test_err_25() {
    check_err(ForInOfStatement::parse(&mut newparser("for(a of"), Scanner::new(), true, true, true), "AssignmentExpression expected", 1, 9);
}
#[test]
fn for_in_of_statement_test_err_26() {
    check_err(ForInOfStatement::parse(&mut newparser("for(a of b"), Scanner::new(), true, true, true), "‘)’ expected", 1, 11);
}
#[test]
fn for_in_of_statement_test_err_27() {
    check_err(ForInOfStatement::parse(&mut newparser("for(a of b)"), Scanner::new(), true, true, true), "Statement expected", 1, 12);
}
#[test]
fn for_in_of_statement_test_err_28() {
    check_err(ForInOfStatement::parse(&mut newparser("for(a in"), Scanner::new(), true, true, true), "Expression expected", 1, 9);
}
#[test]
fn for_in_of_statement_test_err_29() {
    check_err(ForInOfStatement::parse(&mut newparser("for(a in b"), Scanner::new(), true, true, true), "‘)’ expected", 1, 11);
}
#[test]
fn for_in_of_statement_test_err_30() {
    check_err(ForInOfStatement::parse(&mut newparser("for(a in b)"), Scanner::new(), true, true, true), "Statement expected", 1, 12);
}
#[test]
fn for_in_of_statement_test_err_31() {
    check_err(ForInOfStatement::parse(&mut newparser("for await(a in b);"), Scanner::new(), false, false, false), "‘(’ expected", 1, 4);
}
#[test]
fn for_in_of_statement_test_err_32() {
    check_err(ForInOfStatement::parse(&mut newparser("for(var a in"), Scanner::new(), true, true, true), "Expression expected", 1, 13);
}
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
fn for_in_of_cdl_check(src: &str) {
    let (item, _) = ForInOfStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    assert_eq!(item.contains_duplicate_labels(&[]), false);
    assert_eq!(item.contains_duplicate_labels(&[JSString::from("t")]), true);
}
#[test]
fn for_in_of_statement_test_contains_duplicate_labels() {
    for_in_of_cdl_check("for(a in b){t:;}");
    for_in_of_cdl_check("for(var a in b){t:;}");
    for_in_of_cdl_check("for(let a in b){t:;}");
    for_in_of_cdl_check("for(a of b){t:;}");
    for_in_of_cdl_check("for(var a of b){t:;}");
    for_in_of_cdl_check("for(let a of b){t:;}");
    for_in_of_cdl_check("for await(a of b){t:;}");
    for_in_of_cdl_check("for await(var a of b){t:;}");
    for_in_of_cdl_check("for await(let a of b){t:;}");
}
#[test_case("for (a in b) continue x;" => (false, true); "for (a in b) continue x;")]
#[test_case("for (var a in b) continue x;" => (false, true); "for (var a in b) continue x;")]
#[test_case("for (let a in b) continue x;" => (false, true); "for (let a in b) continue x;")]
#[test_case("for (a of b) continue x;" => (false, true); "for (a of b) continue x;")]
#[test_case("for (var a of b) continue x;" => (false, true); "for (var a of b) continue x;")]
#[test_case("for (let a of b) continue x;" => (false, true); "for (let a of b) continue x;")]
#[test_case("for await (a of b) continue x;" => (false, true); "for await (a of b) continue x;")]
#[test_case("for await (var a of b) continue x;" => (false, true); "for await (var a of b) continue x;")]
#[test_case("for await (let a of b) continue x;" => (false, true); "for await (let a of b) continue x;")]
fn for_in_of_statement_test_contains_undefined_continue_target(src: &str) -> (bool, bool) {
    let (item, _) = ForInOfStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    (item.contains_undefined_continue_target(&[JSString::from("x")]), item.contains_undefined_continue_target(&[JSString::from("y")]))
}
#[test_case("for ([a=x.#valid] in c) stmt;" => true; "in init valid")]
#[test_case("for ([a=b] in x.#valid) stmt;" => true; "in target valid")]
#[test_case("for ([a=b] in c) x.#valid;" => true; "in stmt valid")]
#[test_case("for ([a=x.#valid] of c) stmt;" => true; "of init valid")]
#[test_case("for ([a=b] of x.#valid) stmt;" => true; "of target valid")]
#[test_case("for ([a=b] of c) x.#valid;" => true; "of stmt valid")]
#[test_case("for await ([a=x.#valid] of c) stmt;" => true; "await init valid")]
#[test_case("for await ([a=b] of x.#valid) stmt;" => true; "await target valid")]
#[test_case("for await ([a=b] of c) x.#valid;" => true; "await stmt valid")]
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
#[test_case("for ([a=x.#invalid] in c) stmt;" => false; "in init invalid")]
#[test_case("for ([a=b] in x.#invalid) stmt;" => false; "in target invalid")]
#[test_case("for ([a=b] in c) x.#invalid;" => false; "in stmt invalid")]
#[test_case("for ([a=x.#invalid] of c) stmt;" => false; "of init invalid")]
#[test_case("for ([a=b] of x.#invalid) stmt;" => false; "of target invalid")]
#[test_case("for ([a=b] of c) x.#invalid;" => false; "of stmt invalid")]
#[test_case("for await ([a=x.#invalid] of c) stmt;" => false; "await init invalid")]
#[test_case("for await ([a=b] of x.#invalid) stmt;" => false; "await target invalid")]
#[test_case("for await ([a=b] of c) x.#invalid;" => false; "await stmt invalid")]
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
fn for_in_of_statement_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ForInOfStatement::parse(&mut newparser(src), Scanner::new(), true, true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("valid")])
}
mod for_in_of_statement {
    use super::*;
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ForInOfStatement::parse(&mut newparser("for(x in y);"), Scanner::new(), true, true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
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
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ForDeclaration::parse(&mut newparser("let a"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
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
    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn early_errors() {
        ForBinding::parse(&mut newparser("a"), Scanner::new(), true, true).unwrap().0.early_errors(&mut test_agent(), &mut vec![], true);
    }
}
