use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::identifiers::LabelIdentifier;
use super::scanner::{Keyword, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// ContinueStatement[Yield, Await] :
//      continue ;
//      continue [no LineTerminator here] LabelIdentifier[?Yield, ?Await] ;
#[derive(Debug)]
pub enum ContinueStatement {
    Bare,
    Labelled(Rc<LabelIdentifier>),
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
        pprint_token(writer, "continue", TokenType::Keyword, &successive, Spot::NotFinal)?;
        if let ContinueStatement::Labelled(node) = self {
            node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ContinueStatement {
    // no need to cache
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_cont = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Continue)?;
        scan_for_auto_semi(after_cont, parser.source, ScanGoal::InputElementDiv).map(|after_semi| (Rc::new(ContinueStatement::Bare), after_semi)).otherwise(|| {
            let (li, after_li) = LabelIdentifier::parse(parser, after_cont, yield_flag, await_flag)?;
            let after_semi = scan_for_auto_semi(after_li, parser.source, ScanGoal::InputElementDiv)?;
            Ok((Rc::new(ContinueStatement::Labelled(li)), after_semi))
        })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ContinueStatement::Bare => false,
            ContinueStatement::Labelled(label) => label.contains(kind),
        }
    }
}

#[cfg(test)]
mod tests;
