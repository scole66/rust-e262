use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// EmptyStatement :
//      ;
#[derive(Debug)]
pub struct EmptyStatement;

impl fmt::Display for EmptyStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, ";")
    }
}

impl PrettyPrint for EmptyStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{}EmptyStatement: {}", first, self)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        pprint_token(writer, ";", TokenType::Punctuator, pad, state)
    }
}

impl EmptyStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_semi = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Semicolon)?;
        Ok((Box::new(EmptyStatement), after_semi))
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // EMPTY STATEMENT
    #[test]
    fn empty_statement_test_01() {
        let (se, scanner) = check(EmptyStatement::parse(&mut newparser(";"), Scanner::new()));
        chk_scan(&scanner, 1);
        pretty_check(&*se, "EmptyStatement: ;", vec![]);
        concise_check(&*se, "Punctuator: ;", vec![]);
        format!("{:?}", se);
    }
    #[test]
    fn empty_statement_test_02() {
        check_err(EmptyStatement::parse(&mut newparser(""), Scanner::new()), "‘;’ expected", 1, 1);
    }
    #[test]
    fn empty_statement_test_prettyerrors_1() {
        let (item, _) = EmptyStatement::parse(&mut newparser(";"), Scanner::new()).unwrap();
        pretty_error_validate(*item);
    }
    #[test]
    fn empty_statement_test_conciseerrors_1() {
        let (item, _) = EmptyStatement::parse(&mut newparser(";"), Scanner::new()).unwrap();
        concise_error_validate(*item);
    }
}
