use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::comma_operator::Expression;
use super::scanner::{scan_token, Keyword, Punctuator, ScanGoal, Scanner, Token};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

// ExpressionStatement[Yield, Await] :
//      [lookahead ∉ { {, function, async [no LineTerminator here] function, class, let [ }] Expression[+In, ?Yield, ?Await] ;
#[derive(Debug)]
pub enum ExpressionStatement {
    Expression(Box<Expression>),
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ExpressionStatement::Expression(node) = self;
        write!(f, "{} ;", node)
    }
}

impl PrettyPrint for ExpressionStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ExpressionStatement: {}", first, self)?;
        let ExpressionStatement::Expression(node) = self;
        node.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ExpressionStatement: {}", first, self)?;
        let ExpressionStatement::Expression(node) = self;
        node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ";", &successive, Spot::Final)
    }
}

impl ExpressionStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (first_token, after_token) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        let invalid = match first_token {
            Token::Punctuator(Punctuator::LeftBrace) => true,
            Token::Identifier(id) if id.matches(Keyword::Function) => true,
            Token::Identifier(id) if id.matches(Keyword::Class) => true,
            Token::Identifier(id) if id.matches(Keyword::Let) => {
                let (second_token, _) = scan_token(&after_token, parser.source, ScanGoal::InputElementRegExp);
                second_token.matches_punct(Punctuator::LeftBracket)
            }
            Token::Identifier(id) if id.matches(Keyword::Async) => {
                let (second_token, _) = scan_token(&after_token, parser.source, ScanGoal::InputElementRegExp);
                if let Token::Identifier(id2) = second_token {
                    id2.matches(Keyword::Function) && id.line == id2.line
                } else {
                    false
                }
            }
            _ => false,
        };

        if invalid {
            Ok(None)
        } else {
            let pot_exp = Expression::parse(parser, scanner, true, yield_flag, await_flag)?;
            match pot_exp {
                None => Ok(None),
                Some((exp, after_exp)) => {
                    let (pot_semi, after_semi) = scan_token(&after_exp, parser.source, ScanGoal::InputElementRegExp);
                    match pot_semi {
                        Token::Punctuator(Punctuator::Semicolon) => {
                            Ok(Some((Box::new(ExpressionStatement::Expression(exp)), after_semi)))
                        }
                        _ => Ok(None),
                    }
                }
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
