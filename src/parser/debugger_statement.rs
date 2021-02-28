use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::scanner::{Keyword, Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// DebuggerStatement :
//      debugger ;
#[derive(Debug)]
pub struct DebuggerStatement;

impl fmt::Display for DebuggerStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "debugger ;")
    }
}

impl PrettyPrint for DebuggerStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{}DebuggerStatement: {}", first, self)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}DebuggerStatement: {}", first, self)?;
        pprint_token(writer, "debugger", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl DebuggerStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_deb = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Debugger)?;
        let after_semi = scan_for_punct(after_deb, parser.source, ScanGoal::InputElementDiv, Punctuator::Semicolon)?;
        Ok((Box::new(DebuggerStatement), after_semi))
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // DEBUGGER STATEMENT
    #[test]
    fn debugger_statement_test_01() {
        let (se, scanner) = check(DebuggerStatement::parse(&mut newparser("debugger;"), Scanner::new()));
        chk_scan(&scanner, 9);
        pretty_check(&*se, "DebuggerStatement: debugger ;", vec![]);
        concise_check(&*se, "DebuggerStatement: debugger ;", vec!["Keyword: debugger", "Punctuator: ;"]);
        format!("{:?}", se);
    }
    #[test]
    fn debugger_statement_test_02() {
        check_err(DebuggerStatement::parse(&mut newparser(""), Scanner::new()), "‘debugger’ expected", 1, 1);
    }
    #[test]
    fn debugger_statement_test_03() {
        check_err(DebuggerStatement::parse(&mut newparser("debugger"), Scanner::new()), "‘;’ expected", 1, 9);
    }
    #[test]
    fn debugger_statement_test_prettyerrors_1() {
        let (item, _) = DebuggerStatement::parse(&mut newparser("debugger;"), Scanner::new()).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn debugger_statement_test_conciseerrors_1() {
        let (item, _) = DebuggerStatement::parse(&mut newparser("debugger;"), Scanner::new()).unwrap();
        concise_error_validate(*item);
    }
}
