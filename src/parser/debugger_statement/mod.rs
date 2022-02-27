use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::scanner::{Keyword, ScanGoal, Scanner};
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
    // no need to cache
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let after_deb = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Debugger)?;
        let after_semi = scan_for_auto_semi(after_deb, parser.source, ScanGoal::InputElementDiv)?;
        Ok((Rc::new(DebuggerStatement), after_semi))
    }

    pub fn contains(&self, _kind: ParseNodeKind) -> bool {
        false
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

#[cfg(test)]
mod tests;
