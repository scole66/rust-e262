use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::comma_operator::Expression;
use super::scanner::{scan_token, Keyword, Punctuator, ScanGoal, Scanner, Token};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// ExpressionStatement[Yield, Await] :
//      [lookahead ∉ { {, function, async [no LineTerminator here] function, class, let [ }] Expression[+In, ?Yield, ?Await] ;
#[derive(Debug)]
pub enum ExpressionStatement {
    Expression(Rc<Expression>),
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
        pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ExpressionStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
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
            Err(ParseError::new("ExpressionStatement expected", scanner.line, scanner.column))
        } else {
            let (exp, after_exp) = Expression::parse(parser, scanner, true, yield_flag, await_flag)?;
            let after_semi = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementRegExp, Punctuator::Semicolon)?;
            Ok((Rc::new(ExpressionStatement::Expression(exp)), after_semi))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // EXPRESSION STATEMENT
    #[test]
    fn expression_statement_test_01() {
        let (node, scanner) = check(ExpressionStatement::parse(&mut newparser("a;"), Scanner::new(), false, false));
        chk_scan(&scanner, 2);
        pretty_check(&*node, "ExpressionStatement: a ;", vec!["Expression: a"]);
        concise_check(&*node, "ExpressionStatement: a ;", vec!["IdentifierName: a", "Punctuator: ;"]);
        format!("{:?}", node);
        pretty_error_validate(&*node);
        concise_error_validate(&*node);
    }
    #[test]
    fn expression_statement_test_02() {
        let (node, scanner) = check(ExpressionStatement::parse(&mut newparser("async;"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        pretty_check(&*node, "ExpressionStatement: async ;", vec!["Expression: async"]);
        concise_check(&*node, "ExpressionStatement: async ;", vec!["IdentifierName: async", "Punctuator: ;"]);
        format!("{:?}", node);
        pretty_error_validate(&*node);
        concise_error_validate(&*node);
    }
    #[test]
    fn expression_statement_test_err_01() {
        check_err(ExpressionStatement::parse(&mut newparser(""), Scanner::new(), false, false), "Expression expected", 1, 1);
    }
    #[test]
    fn expression_statement_test_err_02() {
        check_err(ExpressionStatement::parse(&mut newparser("{"), Scanner::new(), false, false), "ExpressionStatement expected", 1, 1);
    }
    #[test]
    fn expression_statement_test_err_03() {
        check_err(ExpressionStatement::parse(&mut newparser("function"), Scanner::new(), false, false), "ExpressionStatement expected", 1, 1);
    }
    #[test]
    fn expression_statement_test_err_04() {
        check_err(ExpressionStatement::parse(&mut newparser("class"), Scanner::new(), false, false), "ExpressionStatement expected", 1, 1);
    }
    #[test]
    fn expression_statement_test_err_05() {
        check_err(ExpressionStatement::parse(&mut newparser("let ["), Scanner::new(), false, false), "ExpressionStatement expected", 1, 1);
    }
    #[test]
    fn expression_statement_test_err_06() {
        check_err(ExpressionStatement::parse(&mut newparser("async function"), Scanner::new(), false, false), "ExpressionStatement expected", 1, 1);
    }
    #[test]
    fn expression_statement_test_err_07() {
        check_err(ExpressionStatement::parse(&mut newparser("0"), Scanner::new(), false, false), "‘;’ expected", 1, 2);
    }
}
