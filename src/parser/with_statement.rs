use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::comma_operator::Expression;
use super::scanner::Scanner;
use super::statements_and_declarations::Statement;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

// WithStatement[Yield, Await, Return] :
//      with ( Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub struct WithStatement {
    expression: Box<Expression>,
    statement: Box<Statement>,
}

impl fmt::Display for WithStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "with ( {} ) {}", self.expression, self.statement)
    }
}

impl PrettyPrint for WithStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}WithStatement: {}", first, self)?;
        self.expression
            .pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.statement.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}WithStatement: {}", first, self)?;
        pprint_token(writer, "with", &successive, Spot::NotFinal)?;
        pprint_token(writer, "(", &successive, Spot::NotFinal)?;
        self.expression
            .concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", &successive, Spot::NotFinal)?;
        self.statement.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl WithStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (with_tok, after_with) =
            scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if matches!(with_tok, scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::With)) {
            let (open, after_open) =
                scanner::scan_token(&after_with, parser.source, scanner::ScanGoal::InputElementDiv);
            if open == scanner::Token::LeftParen {
                let pot_exp = Expression::parse(parser, after_open, true, yield_flag, await_flag)?;
                if let Some((exp, after_exp)) = pot_exp {
                    let (close, after_close) =
                        scanner::scan_token(&after_exp, parser.source, scanner::ScanGoal::InputElementDiv);
                    if close == scanner::Token::RightParen {
                        let pot_stmt = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                        if let Some((stmt, after_stmt)) = pot_stmt {
                            return Ok(Some((
                                Box::new(WithStatement {
                                    expression: exp,
                                    statement: stmt,
                                }),
                                after_stmt,
                            )));
                        }
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
