use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::comma_operator::Expression;
use super::scanner::{scan_token, Keyword, Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

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
        pprint_token(writer, "return", TokenType::Keyword, &successive, Spot::NotFinal)?;
        if let ReturnStatement::Expression(exp) = self {
            exp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ReturnStatement {
    // Given ‘return’
    // See if we can parse ‘[no LineTerminator here] Expression ;’
    fn parse_exp(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (_, after_next) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        // The following check is broken for literals which span more than one line (strings can do this). Need to come up with a better way.
        if scanner.line == after_next.line {
            let (exp, after_exp) = Expression::parse(parser, scanner, true, yield_flag, await_flag)?;
            let after_semi = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::Semicolon)?;
            Ok((Box::new(ReturnStatement::Expression(exp)), after_semi))
        } else {
            Err(ParseError::new("Expression expected", scanner.line, scanner.column))
        }
    }

    // Given ‘return’
    // See if we can parse ‘;’
    fn parse_semi(parser: &mut Parser, scanner: Scanner) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_semi = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Semicolon)?;
        Ok((Box::new(ReturnStatement::Bare), after_semi))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_ret = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Return)?;
        Err(ParseError::new("‘;’ or an Expression expected", after_ret.line, after_ret.column))
            .otherwise(|| Self::parse_exp(parser, after_ret, yield_flag, await_flag))
            .otherwise(|| Self::parse_semi(parser, after_ret))
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}
