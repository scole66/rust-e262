use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::identifiers::LabelIdentifier;
use super::scanner::{Keyword, Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

// ContinueStatement[Yield, Await] :
//      continue ;
//      continue [no LineTerminator here] LabelIdentifier[?Yield, ?Await] ;
#[derive(Debug)]
pub enum ContinueStatement {
    Bare,
    Labelled(Box<LabelIdentifier>),
}

impl fmt::Display for ContinueStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ContinueStatement::Bare => write!(f, "continue ;"),
            ContinueStatement::Labelled(label) => write!(f, "continue {} ;", label),
        }
    }
}

impl PrettyPrint for ContinueStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ContinueStatement: {}", first, self)?;
        match self {
            ContinueStatement::Bare => Ok(()),
            ContinueStatement::Labelled(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ContinueStatement: {}", first, self)?;
        pprint_token(writer, "continue", &successive, Spot::NotFinal)?;
        if let ContinueStatement::Labelled(node) = self {
            node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, ";", &successive, Spot::Final)
    }
}

impl ContinueStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_cont = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Continue)?;
        Err(ParseError::new("Expected label or semicolon", after_cont.line, after_cont.column))
            .otherwise(|| {
                no_line_terminator(after_cont, parser.source)?;
                let (li, after_li) = LabelIdentifier::parse(parser, after_cont, yield_flag, await_flag)?;
                let after_semi = scan_for_punct(after_li, parser.source, ScanGoal::InputElementDiv, Punctuator::Semicolon)?;
                Ok((Box::new(ContinueStatement::Labelled(li)), after_semi))
            })
            .otherwise(|| {
                let after_semi = scan_for_punct(after_cont, parser.source, ScanGoal::InputElementDiv, Punctuator::Semicolon)?;
                Ok((Box::new(ContinueStatement::Bare), after_semi))
            })
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}
