use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::comma_operator::Expression;
use super::scanner::{Keyword, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// ReturnStatement[Yield, Await] :
//      return ;
//      return [no LineTerminator here] Expression[+In, ?Yield, ?Await] ;
#[derive(Debug)]
pub enum ReturnStatement {
    Bare,
    Expression(Rc<Expression>),
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_ret = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Return)?;
        scan_for_auto_semi(after_ret, parser.source, ScanGoal::InputElementRegExp).map(|after_semi| (Rc::new(ReturnStatement::Bare), after_semi)).otherwise(|| {
            let (exp, after_exp) = Expression::parse(parser, after_ret, true, yield_flag, await_flag)?;
            let after_semi = scan_for_auto_semi(after_exp, parser.source, ScanGoal::InputElementDiv)?;
            Ok((Rc::new(ReturnStatement::Expression(exp)), after_semi))
        })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ReturnStatement::Bare => false,
            ReturnStatement::Expression(node) => node.contains(kind),
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        match self {
            ReturnStatement::Bare => true,
            ReturnStatement::Expression(node) => node.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

#[cfg(test)]
mod tests;
