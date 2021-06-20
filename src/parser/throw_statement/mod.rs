use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::comma_operator::Expression;
use super::scanner::{Keyword, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// ThrowStatement[Yield, Await] :
//      throw [no LineTerminator here] Expression[+In, ?Yield, ?Await] ;
#[derive(Debug)]
pub struct ThrowStatement(Rc<Expression>);

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
        pprint_token(writer, "throw", TokenType::Keyword, &successive, Spot::NotFinal)?;
        self.0.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ThrowStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_throw = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Throw)?;
        no_line_terminator(after_throw, parser.source)?;
        let (exp, after_exp) = Expression::parse(parser, after_throw, true, yield_flag, await_flag)?;
        let after_semi = scan_for_auto_semi(after_exp, parser.source, ScanGoal::InputElementRegExp)?;
        Ok((Rc::new(ThrowStatement(exp)), after_semi))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.0.contains(kind)
    }
}

#[cfg(test)]
mod tests;
