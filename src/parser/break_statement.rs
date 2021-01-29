use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::identifiers::LabelIdentifier;
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

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
        pprint_token(writer, "break", &successive, Spot::NotFinal)?;
        if let BreakStatement::Labelled(node) = self {
            node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, ";", &successive, Spot::Final)
    }
}

impl BreakStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (break_token, after_break) =
            scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if matches!(break_token, scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Break)) {
            let (next_token, after_next) =
                scanner::scan_token(&after_break, parser.source, scanner::ScanGoal::InputElementRegExp);
            if after_next.line == after_break.line {
                let pot_li = LabelIdentifier::parse(parser, after_break, yield_flag, await_flag)?;
                if let Some((li, after_li)) = pot_li {
                    let (semi, after_semi) =
                        scanner::scan_token(&after_li, parser.source, scanner::ScanGoal::InputElementDiv);
                    if semi == scanner::Token::Semicolon {
                        return Ok(Some((Box::new(BreakStatement::Labelled(li)), after_semi)));
                    }
                }
            }
            let (semi, after_semi) =
                scanner::scan_token(&after_break, parser.source, scanner::ScanGoal::InputElementDiv);
            if semi == scanner::Token::Semicolon {
                return Ok(Some((Box::new(BreakStatement::Bare), after_semi)));
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
