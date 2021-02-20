use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::identifiers::LabelIdentifier;
use super::scanner::{Keyword, Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// BreakStatement[Yield, Await] :
//      break ;
//      break [no LineTerminator here] LabelIdentifier[?Yield, ?Await] ;
#[derive(Debug)]
pub enum BreakStatement {
    Bare,
    Labelled(Box<LabelIdentifier>),
}

impl fmt::Display for BreakStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BreakStatement::Bare => write!(f, "break ;"),
            BreakStatement::Labelled(label) => write!(f, "break {} ;", label),
        }
    }
}

impl PrettyPrint for BreakStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BreakStatement: {}", first, self)?;
        match self {
            BreakStatement::Bare => Ok(()),
            BreakStatement::Labelled(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BreakStatement: {}", first, self)?;
        pprint_token(writer, "break", TokenType::Keyword, &successive, Spot::NotFinal)?;
        if let BreakStatement::Labelled(node) = self {
            node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl BreakStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Break).and_then(|after_break| {
            no_line_terminator(after_break, parser.source)
                .and_then(|()| {
                    LabelIdentifier::parse(parser, after_break, yield_flag, await_flag).and_then(|(li, after_li)| {
                        scan_for_punct(after_li, parser.source, ScanGoal::InputElementDiv, Punctuator::Semicolon).map(|after_semi| (Box::new(BreakStatement::Labelled(li)), after_semi))
                    })
                })
                .otherwise(|| scan_for_punct(after_break, parser.source, ScanGoal::InputElementDiv, Punctuator::Semicolon).map(|after_semi| (Box::new(BreakStatement::Bare), after_semi)))
        })
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}
