use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use test_case::test_case;

// EXPRESSION STATEMENT
#[test]
fn expression_statement_test_01() {
    let (node, scanner) = check(ExpressionStatement::parse(&mut newparser("a;"), Scanner::new(), false, false));
    chk_scan(&scanner, 2);
    pretty_check(&*node, "ExpressionStatement: a ;", &["Expression: a"]);
    concise_check(&*node, "ExpressionStatement: a ;", &["IdentifierName: a", "Punctuator: ;"]);
    format!("{node:?}");
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn expression_statement_test_02() {
    let (node, scanner) = check(ExpressionStatement::parse(&mut newparser("async;"), Scanner::new(), false, false));
    chk_scan(&scanner, 6);
    pretty_check(&*node, "ExpressionStatement: async ;", &["Expression: async"]);
    concise_check(&*node, "ExpressionStatement: async ;", &["IdentifierName: async", "Punctuator: ;"]);
    format!("{node:?}");
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn expression_statement_test_03() {
    let (node, scanner) =
        check(ExpressionStatement::parse(&mut newparser("async\nfunction a(){}"), Scanner::new(), false, false));
    chk_scan(&scanner, 5);
    pretty_check(&*node, "ExpressionStatement: async ;", &["Expression: async"]);
    concise_check(&*node, "ExpressionStatement: async ;", &["IdentifierName: async", "Punctuator: ;"]);
    format!("{node:?}");
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn expression_statement_test_asi_01() {
    let (node, scanner) = check(ExpressionStatement::parse(&mut newparser("a"), Scanner::new(), false, false));
    chk_scan(&scanner, 1);
    pretty_check(&*node, "ExpressionStatement: a ;", &["Expression: a"]);
    concise_check(&*node, "ExpressionStatement: a ;", &["IdentifierName: a", "Punctuator: ;"]);
    format!("{node:?}");
    pretty_error_validate(&*node);
    concise_error_validate(&*node);
}
#[test]
fn expression_statement_test_err_01() {
    check_err(
        ExpressionStatement::parse(&mut newparser(""), Scanner::new(), false, false),
        "Expression expected",
        1,
        1,
    );
}
#[test]
fn expression_statement_test_err_02() {
    check_err(
        ExpressionStatement::parse(&mut newparser("{"), Scanner::new(), false, false),
        "ExpressionStatement expected",
        1,
        1,
    );
}
#[test]
fn expression_statement_test_err_03() {
    check_err(
        ExpressionStatement::parse(&mut newparser("function"), Scanner::new(), false, false),
        "ExpressionStatement expected",
        1,
        1,
    );
}
#[test]
fn expression_statement_test_err_04() {
    check_err(
        ExpressionStatement::parse(&mut newparser("class"), Scanner::new(), false, false),
        "ExpressionStatement expected",
        1,
        1,
    );
}
#[test]
fn expression_statement_test_err_05() {
    check_err(
        ExpressionStatement::parse(&mut newparser("let ["), Scanner::new(), false, false),
        "ExpressionStatement expected",
        1,
        1,
    );
}
#[test]
fn expression_statement_test_err_06() {
    check_err(
        ExpressionStatement::parse(&mut newparser("async function"), Scanner::new(), false, false),
        "ExpressionStatement expected",
        1,
        1,
    );
}
#[test]
fn expression_statement_test_err_07() {
    check_err(ExpressionStatement::parse(&mut newparser("0 7"), Scanner::new(), false, false), "‘;’ expected", 1, 3);
}
#[test_case("'string';" => Some(JSString::from("string")); "String Token")]
#[test_case("a??b;" => None; "Not token")]
fn expression_statement_test_as_string_literal(src: &str) -> Option<JSString> {
    let (item, _) = ExpressionStatement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.as_string_literal().map(|st| st.value)
}
#[test_case("a.#valid" => true; "valid")]
#[test_case("a.#invalid" => false; "invalid")]
fn expression_statement_test_all_private_identifiers_valid(src: &str) -> bool {
    let (item, _) = ExpressionStatement::parse(&mut newparser(src), Scanner::new(), true, true).unwrap();
    item.all_private_identifiers_valid(&[JSString::from("#valid")])
}
mod expression_statement {
    use super::*;
    use test_case::test_case;

    #[test_case("package;", true => sset(&[PACKAGE_NOT_ALLOWED]); "normal")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).expression_statement().early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&err.clone())))
    }

    #[test_case("arguments;" => true; "yes")]
    #[test_case("a;" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).expression_statement().contains_arguments()
    }

    #[test_case("0;", ParseNodeKind::Literal => true; "literal match")]
    #[test_case("a;", ParseNodeKind::Literal => false; "literal no match")]
    #[test_case("thing;", ParseNodeKind::Expression => true; "just a thing")]
    fn contains(src: &str, target: ParseNodeKind) -> bool {
        Maker::new(src).expression_statement().contains(target)
    }

    #[test_case("   a;" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 2 } })]
    fn location(src: &str) -> Location {
        Maker::new(src).expression_statement().location()
    }
}
