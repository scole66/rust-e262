use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::function_definitions::FunctionDeclaration;
use super::identifiers::LabelIdentifier;
use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::statements_and_declarations::Statement;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// LabelledStatement[Yield, Await, Return] :
//      LabelIdentifier[?Yield, ?Await] : LabelledItem[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub struct LabelledStatement {
    identifier: Box<LabelIdentifier>,
    item: Box<LabelledItem>,
}

impl fmt::Display for LabelledStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} : {}", self.identifier, self.item)
    }
}

impl PrettyPrint for LabelledStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}LabelledStatement: {}", first, self)?;
        self.identifier.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.item.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}LabelledStatement: {}", first, self)?;
        self.identifier.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.item.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl LabelledStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (li, after_li) = LabelIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
        let after_colon = scan_for_punct(after_li, parser.source, ScanGoal::InputElementDiv, Punctuator::Colon)?;
        let (item, after_item) = LabelledItem::parse(parser, after_colon, yield_flag, await_flag, return_flag)?;
        Ok((Box::new(LabelledStatement { identifier: li, item: item }), after_item))
    }
}

// LabelledItem[Yield, Await, Return] :
//      Statement[?Yield, ?Await, ?Return]
//      FunctionDeclaration[?Yield, ?Await, ~Default]
#[derive(Debug)]
pub enum LabelledItem {
    Statement(Box<Statement>),
    Function(Box<FunctionDeclaration>),
}

impl fmt::Display for LabelledItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LabelledItem::Statement(node) => node.fmt(f),
            LabelledItem::Function(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for LabelledItem {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}LabelledItem: {}", first, self)?;
        match self {
            LabelledItem::Statement(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            LabelledItem::Function(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            LabelledItem::Statement(node) => node.concise_with_leftpad(writer, pad, state),
            LabelledItem::Function(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl LabelledItem {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("LabelledItem expected", scanner.line, scanner.column))
            .otherwise(|| {
                let (stmt, after_stmt) = Statement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Box::new(LabelledItem::Statement(stmt)), after_stmt))
            })
            .otherwise(|| {
                let (fcn, after_fcn) = FunctionDeclaration::parse(parser, scanner, yield_flag, await_flag, false)?;
                Ok((Box::new(LabelledItem::Function(fcn)), after_fcn))
            })
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}
