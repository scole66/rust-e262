use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;

// EMPTY STATEMENT
mod empty_statement {
    use super::*;
    use test_case::test_case;

    #[test]
    fn parse_01() {
        let (se, scanner) = check(EmptyStatement::parse(&mut newparser(";"), Scanner::new()));
        chk_scan(&scanner, 1);
        pretty_check(&*se, "EmptyStatement: ;", &[]);
        concise_check(&*se, "Punctuator: ;", &[]);
        format!("{se:?}");
    }
    #[test]
    fn parse_02() {
        check_err(EmptyStatement::parse(&mut newparser(""), Scanner::new()), "‘;’ expected", 1, 1);
    }
    #[test]
    fn prettyerrors_1() {
        let (item, _) = EmptyStatement::parse(&mut newparser(";"), Scanner::new()).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_1() {
        let (item, _) = EmptyStatement::parse(&mut newparser(";"), Scanner::new()).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) = EmptyStatement::parse(&mut newparser(";"), Scanner::new()).unwrap();
        assert_eq!(item.contains(ParseNodeKind::Literal), false);
    }

    #[test_case("   ;" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 1 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).empty_statement().location()
    }
}
