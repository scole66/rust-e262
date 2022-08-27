use super::testhelp::*;
use super::*;
use crate::prettyprint::testhelp::*;
use crate::tests::*;
use ahash::AHashSet;

// THROW STATEMENT
mod throw_statement {
    use super::*;
    use test_case::test_case;

    #[test]
    fn parse() {
        let (node, scanner) = check(ThrowStatement::parse(&mut newparser("throw 0;"), Scanner::new(), false, false));
        chk_scan(&scanner, 8);
        pretty_check(&*node, "ThrowStatement: throw 0 ;", vec!["Expression: 0"]);
        concise_check(&*node, "ThrowStatement: throw 0 ;", vec!["Keyword: throw", "Numeric: 0", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn errs() {
        check_err(ThrowStatement::parse(&mut newparser(""), Scanner::new(), false, false), "‘throw’ expected", 1, 1);
        check_err(
            ThrowStatement::parse(&mut newparser("throw"), Scanner::new(), false, false),
            "Expression expected",
            1,
            6,
        );
        check_err(
            ThrowStatement::parse(&mut newparser("throw\n0;"), Scanner::new(), false, false),
            "newline not allowed here",
            1,
            6,
        );
        check_err(
            ThrowStatement::parse(&mut newparser("throw 0@"), Scanner::new(), false, false),
            "‘;’ expected",
            1,
            8,
        );
    }
    #[test]
    fn printer_errs() {
        let src = "throw 0;";
        let (item, _) = ThrowStatement::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
        concise_error_validate(&*item);
    }
    fn throw_contains_check(src: &str, has_literal: bool) {
        let (item, _) = ThrowStatement::parse(&mut newparser(src), Scanner::new(), false, false).unwrap();
        assert_eq!(item.contains(ParseNodeKind::Literal), has_literal);
    }
    #[test]
    fn contains() {
        throw_contains_check("throw 0;", true);
        throw_contains_check("throw a;", false);
    }
    #[test_case("throw a.#valid;" => true; "valid")]
    #[test_case("throw a.#invalid;" => false; "invalid")]
    fn all_private_identifiers_valid(src: &str) -> bool {
        Maker::new(src).throw_statement().all_private_identifiers_valid(&[JSString::from("#valid")])
    }

    #[test_case("throw package;", true => sset(&[PACKAGE_NOT_ALLOWED]); "normal")]
    fn early_errors(src: &str, strict: bool) -> AHashSet<String> {
        setup_test_agent();
        let mut errs = vec![];
        Maker::new(src).throw_statement().early_errors(&mut errs, strict);
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(err.clone())))
    }

    #[test_case("throw arguments;" => true; "yes")]
    #[test_case("throw a;" => false; "no")]
    fn contains_arguments(src: &str) -> bool {
        Maker::new(src).throw_statement().contains_arguments()
    }

    #[test_case("   throw a;" => Location { starting_line: 1, starting_column: 4, span:Span { starting_index: 3, length: 8 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).throw_statement().location()
    }
}
