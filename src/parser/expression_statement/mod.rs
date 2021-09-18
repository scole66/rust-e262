use super::comma_operator::Expression;
use super::scanner::{scan_token, Keyword, Punctuator, ScanGoal, Scanner, StringToken, Token};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

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
            let after_semi = scan_for_auto_semi(after_exp, parser.source, ScanGoal::InputElementRegExp)?;
            Ok((Rc::new(ExpressionStatement::Expression(exp)), after_semi))
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let ExpressionStatement::Expression(node) = self;
        node.contains(kind)
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        let ExpressionStatement::Expression(node) = self;
        node.as_string_literal()
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        let ExpressionStatement::Expression(node) = self;
        node.all_private_identifiers_valid(names)
    }
}

#[cfg(test)]
mod tests;
