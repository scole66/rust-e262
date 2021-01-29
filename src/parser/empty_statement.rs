use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

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
        self.pprint_with_leftpad(writer, pad, state)
    }
}

impl EmptyStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok, after) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        match tok {
            scanner::Token::Semicolon => Ok(Some((Box::new(EmptyStatement), after))),
            _ => Ok(None),
        }
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}
