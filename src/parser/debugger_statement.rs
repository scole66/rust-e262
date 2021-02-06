use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::scanner::{scan_token, Keyword, Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

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
        pprint_token(writer, "debugger", &successive, Spot::NotFinal)?;
        pprint_token(writer, ";", &successive, Spot::Final)
    }
}

impl DebuggerStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok_deb, after_deb) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        if tok_deb.matches_keyword(Keyword::Debugger) {
            let (semi, after_semi) = scan_token(&after_deb, parser.source, ScanGoal::InputElementDiv);
            if semi.matches_punct(Punctuator::Semicolon) {
                return Ok(Some((Box::new(DebuggerStatement), after_semi)));
            }
        }
        Ok(None)
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}
