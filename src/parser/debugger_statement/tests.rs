use super::testhelp::*;
use super::*;
use crate::prettyprint::pp_testhelp::*;

mod debugger_statement {
    use super::*;
    use test_case::test_case;

    // DEBUGGER STATEMENT
    #[test]
    fn parse() {
        let (se, scanner) = check(DebuggerStatement::parse(&mut newparser("debugger;"), Scanner::new()));
        chk_scan(&scanner, 9);
        pretty_check(&*se, "DebuggerStatement: debugger ;", vec![]);
        concise_check(&*se, "DebuggerStatement: debugger ;", vec!["Keyword: debugger", "Punctuator: ;"]);
        format!("{:?}", se);
    }
    #[test]
    fn asi_01() {
        let (se, scanner) = check(DebuggerStatement::parse(&mut newparser("debugger"), Scanner::new()));
        chk_scan(&scanner, 8);
        pretty_check(&*se, "DebuggerStatement: debugger ;", vec![]);
        concise_check(&*se, "DebuggerStatement: debugger ;", vec!["Keyword: debugger", "Punctuator: ;"]);
        format!("{:?}", se);
    }
    #[test]
    fn err_01() {
        check_err(DebuggerStatement::parse(&mut newparser(""), Scanner::new()), "‘debugger’ expected", 1, 1);
    }
    #[test]
    fn err_02() {
        check_err(DebuggerStatement::parse(&mut newparser("debugger for"), Scanner::new()), "‘;’ expected", 1, 10);
    }
    #[test]
    fn prettyerrors_1() {
        let (item, _) = DebuggerStatement::parse(&mut newparser("debugger;"), Scanner::new()).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn conciseerrors_1() {
        let (item, _) = DebuggerStatement::parse(&mut newparser("debugger;"), Scanner::new()).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn contains_01() {
        let (item, _) = DebuggerStatement::parse(&mut newparser("debugger;"), Scanner::new()).unwrap();
        assert_eq!(item.contains(ParseNodeKind::Literal), false);
    }

    #[test_case("   debugger;" => Location { starting_line: 1, starting_column: 4, span: Span { starting_index: 3, length: 9 } }; "typical")]
    fn location(src: &str) -> Location {
        Maker::new(src).debugger_statement().location()
    }
}
