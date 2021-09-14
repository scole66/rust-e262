use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::identifiers::LabelIdentifier;
use super::scanner::{Keyword, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// BreakStatement[Yield, Await] :
//      break ;
//      break [no LineTerminator here] LabelIdentifier[?Yield, ?Await] ;
#[derive(Debug)]
pub enum BreakStatement {
    Bare,
    Labelled(Rc<LabelIdentifier>),
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
    // no cache needed
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_break = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Break)?;
        scan_for_auto_semi(after_break, parser.source, ScanGoal::InputElementDiv).map(|after_semi| (Rc::new(BreakStatement::Bare), after_semi)).otherwise(|| {
            let (li, after_li) = LabelIdentifier::parse(parser, after_break, yield_flag, await_flag)?;
            let after_semi = scan_for_auto_semi(after_li, parser.source, ScanGoal::InputElementDiv)?;
            Ok((Rc::new(BreakStatement::Labelled(li)), after_semi))
        })
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            BreakStatement::Bare => false,
            BreakStatement::Labelled(label) => !label_set.contains(&label.string_value()),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            BreakStatement::Bare => false,
            BreakStatement::Labelled(label) => label.contains(kind),
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent) -> Vec<Object> {
        // todo!()
        println!("{}:{}: Not yet implemented", file!(), line!());
        Vec::new()
    }
}

#[cfg(test)]
mod tests;
