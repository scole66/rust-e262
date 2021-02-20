use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::comma_operator::Expression;
use super::scanner::{Keyword, Punctuator, ScanGoal, Scanner};
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
        let condition =
            |writer: &mut T, exp: &Box<Expression>| exp.concise_with_leftpad(writer, &successive, Spot::NotFinal).and_then(|_| pprint_token(writer, ")", &successive, Spot::NotFinal));
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_lead = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::If)?;
        let after_open = scan_for_punct(after_lead, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (exp, after_exp) = Expression::parse(parser, after_open, true, yield_flag, await_flag)?;
        let after_close = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let (stmt1, after_stmt1) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
        match scan_for_keyword(after_stmt1, parser.source, ScanGoal::InputElementRegExp, Keyword::Else) {
            Err(_) => Ok((Box::new(IfStatement::WithoutElse(exp, stmt1)), after_stmt1)),
            Ok(after_else) => {
                let (stmt2, after_stmt2) = Statement::parse(parser, after_else, yield_flag, await_flag, return_flag)?;
                Ok((Box::new(IfStatement::WithElse(exp, stmt1, stmt2)), after_stmt2))
            }
        }
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}
