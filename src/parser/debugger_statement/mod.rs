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

    pub fn all_private_identifiers_valid(&self) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        true
    }
}

#[cfg(test)]
mod tests;
