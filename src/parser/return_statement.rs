use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::comma_operator::Expression;
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

// ReturnStatement[Yield, Await] :
//      return ;
//      return [no LineTerminator here] Expression[+In, ?Yield, ?Await] ;
#[derive(Debug)]
pub enum ReturnStatement {
    Bare,
    Expression(Box<Expression>),
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ReturnStatement::Bare => write!(f, "return ;"),
            ReturnStatement::Expression(node) => write!(f, "return {} ;", node),
        }
    }
}

impl PrettyPrint for ReturnStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ReturnStatement: {}", first, self)?;
        match self {
            ReturnStatement::Bare => Ok(()),
            ReturnStatement::Expression(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ReturnStatement: {}", first, self)?;
        pprint_token(writer, "return", &successive, Spot::NotFinal)?;
        if let ReturnStatement::Expression(exp) = self {
            exp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, ";", &successive, Spot::Final)
    }
}

impl ReturnStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (ret_tok, after_ret) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if matches!(ret_tok, scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Return)) {
            let (next_tok, after_next) =
                scanner::scan_token(&after_ret, parser.source, scanner::ScanGoal::InputElementRegExp);
            // The following check is broken for literals which span more than one line (strings can do this). Need to come up with a better way.
            if after_ret.line == after_next.line {
                let pot_exp = Expression::parse(parser, after_ret, true, yield_flag, await_flag)?;
                if let Some((exp, after_exp)) = pot_exp {
                    let (semi, after_semi) =
                        scanner::scan_token(&after_exp, parser.source, scanner::ScanGoal::InputElementDiv);
                    if semi == scanner::Token::Semicolon {
                        return Ok(Some((Box::new(ReturnStatement::Expression(exp)), after_semi)));
                    }
                }
            }
            let (semi, after_semi) =
                scanner::scan_token(&after_ret, parser.source, scanner::ScanGoal::InputElementRegExp);
            if semi == scanner::Token::Semicolon {
                return Ok(Some((Box::new(ReturnStatement::Bare), after_semi)));
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
