use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::identifiers::LabelIdentifier;
use super::scanner::{scan_token, Keyword, Punctuator, ScanGoal, Scanner,};
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
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (cont_token, after_cont) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        if cont_token.matches_keyword(Keyword::Continue) {
            let (next_token, after_next) = scan_token(&after_cont, parser.source, ScanGoal::InputElementRegExp);
            if after_next.line == after_cont.line {
                let pot_li = LabelIdentifier::parse(parser, after_cont, yield_flag, await_flag)?;
                if let Some((li, after_li)) = pot_li {
                    let (semi, after_semi) = scan_token(&after_li, parser.source, ScanGoal::InputElementDiv);
                    if semi.matches_punct(Punctuator::Semicolon) {
                        return Ok(Some((Box::new(ContinueStatement::Labelled(li)), after_semi)));
                    }
                }
            }
            let (semi, after_semi) = scan_token(&after_cont, parser.source, ScanGoal::InputElementDiv);
            if semi.matches_punct(Punctuator::Semicolon) {
                return Ok(Some((Box::new(ContinueStatement::Bare), after_semi)));
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
