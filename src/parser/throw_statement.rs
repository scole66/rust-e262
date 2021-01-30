use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::comma_operator::Expression;
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

// ThrowStatement[Yield, Await] :
//      throw [no LineTerminator here] Expression[+In, ?Yield, ?Await] ;
#[derive(Debug)]
pub struct ThrowStatement(Box<Expression>);

impl fmt::Display for ThrowStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "throw {} ;", self.0)
    }
}

impl PrettyPrint for ThrowStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ThrowStatement: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ThrowStatement: {}", first, self)?;
        pprint_token(writer, "throw", &successive, Spot::NotFinal)?;
        self.0.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ";", &successive, Spot::Final)
    }
}

impl ThrowStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (throw_tok, after_throw) =
            scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if matches!(throw_tok, scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Throw)) {
            let (next, after_next) =
                scanner::scan_token(&after_throw, parser.source, scanner::ScanGoal::InputElementRegExp);
            if after_throw.line == after_next.line {
                let pot_exp = Expression::parse(parser, after_throw, true, yield_flag, await_flag)?;
                if let Some((exp, after_exp)) = pot_exp {
                    let (semi, after_semi) =
                        scanner::scan_token(&after_exp, parser.source, scanner::ScanGoal::InputElementRegExp);
                    if semi == scanner::Token::Semicolon {
                        return Ok(Some((Box::new(ThrowStatement(exp)), after_semi)));
                    }
                }
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
