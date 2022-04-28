use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

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
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let after_semi = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Semicolon)?;
        Ok((Rc::new(EmptyStatement), after_semi))
    }

    pub fn contains(&self, _kind: ParseNodeKind) -> bool {
        false
    }
}

#[cfg(test)]
mod tests;
