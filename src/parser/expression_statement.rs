use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::comma_operator::Expression;
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

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
}

impl ExpressionStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (first_token, after_token) =
            scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        let invalid = match first_token {
            scanner::Token::LeftBrace => true,
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Function) => true,
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Class) => true,
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Let) => {
                let (second_token, _) =
                    scanner::scan_token(&after_token, parser.source, scanner::ScanGoal::InputElementRegExp);
                matches!(second_token, scanner::Token::LeftBracket)
            }
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Async) => {
                let (second_token, _) =
                    scanner::scan_token(&after_token, parser.source, scanner::ScanGoal::InputElementRegExp);
                if let scanner::Token::Identifier(id2) = second_token {
                    id2.keyword_id == Some(scanner::Keyword::Function) && id.line == id2.line
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
                    let (pot_semi, after_semi) =
                        scanner::scan_token(&after_exp, parser.source, scanner::ScanGoal::InputElementRegExp);
                    match pot_semi {
                        scanner::Token::Semicolon => {
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
