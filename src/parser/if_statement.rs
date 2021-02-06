use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::comma_operator::Expression;
use super::scanner::{scan_token, Keyword, Punctuator, ScanGoal, Scanner};
use super::statements_and_declarations::Statement;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

// IfStatement[Yield, Await, Return] :
//      if ( Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return] else Statement[?Yield, ?Await, ?Return]
//      if ( Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return] [lookahead ≠ else]
#[derive(Debug)]
pub enum IfStatement {
    WithElse(Box<Expression>, Box<Statement>, Box<Statement>),
    WithoutElse(Box<Expression>, Box<Statement>),
}

impl fmt::Display for IfStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IfStatement::WithElse(e, s1, s2) => write!(f, "if ( {} ) {} else {}", e, s1, s2),
            IfStatement::WithoutElse(e, s1) => write!(f, "if ( {} ) {}", e, s1),
        }
    }
}

impl PrettyPrint for IfStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}IfStatement: {}", first, self)?;
        match self {
            IfStatement::WithoutElse(e, s1) => {
                e.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                s1.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            IfStatement::WithElse(e, s1, s2) => {
                e.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                s1.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                s2.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}IfStatement: {}", first, self)?;
        pprint_token(writer, "if", &successive, Spot::NotFinal)?;
        pprint_token(writer, "(", &successive, Spot::NotFinal)?;
        let condition = |writer: &mut T, exp: &Box<Expression>| {
            exp.concise_with_leftpad(writer, &successive, Spot::NotFinal)
                .and_then(|_| pprint_token(writer, ")", &successive, Spot::NotFinal))
        };
        match self {
            IfStatement::WithoutElse(e, s1) => {
                condition(writer, e)?;
                s1.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            IfStatement::WithElse(e, s1, s2) => {
                condition(writer, e)?;
                s1.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "else", &successive, Spot::NotFinal)?;
                s2.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IfStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (lead_token, after_lead) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        if lead_token.matches_keyword(Keyword::If) {
            let (open, after_open) = scan_token(&after_lead, parser.source, ScanGoal::InputElementDiv);
            if open.matches_punct(Punctuator::LeftParen) {
                let pot_exp = Expression::parse(parser, after_open, true, yield_flag, await_flag)?;
                if let Some((exp, after_exp)) = pot_exp {
                    let (close, after_close) = scan_token(&after_exp, parser.source, ScanGoal::InputElementDiv);
                    if close.matches_punct(Punctuator::RightParen) {
                        let pot_stmt1 = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                        if let Some((stmt1, after_stmt1)) = pot_stmt1 {
                            let (else_tok, after_else) =
                                scan_token(&after_stmt1, parser.source, ScanGoal::InputElementRegExp);
                            if else_tok.matches_keyword(Keyword::Else) {
                                let pot_stmt2 =
                                    Statement::parse(parser, after_else, yield_flag, await_flag, return_flag)?;
                                if let Some((stmt2, after_stmt2)) = pot_stmt2 {
                                    return Ok(Some((Box::new(IfStatement::WithElse(exp, stmt1, stmt2)), after_stmt2)));
                                }
                            } else {
                                return Ok(Some((Box::new(IfStatement::WithoutElse(exp, stmt1)), after_stmt1)));
                            }
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
