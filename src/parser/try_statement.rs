use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::block::Block;
use super::declarations_and_variables::BindingPattern;
use super::identifiers::BindingIdentifier;
use super::scanner::{Keyword, Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// TryStatement[Yield, Await, Return] :
//      try Block[?Yield, ?Await, ?Return] Catch[?Yield, ?Await, ?Return]
//      try Block[?Yield, ?Await, ?Return] Finally[?Yield, ?Await, ?Return]
//      try Block[?Yield, ?Await, ?Return] Catch[?Yield, ?Await, ?Return] Finally[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum TryStatement {
    Catch(Box<Block>, Box<Catch>),
    Finally(Box<Block>, Box<Finally>),
    Full(Box<Block>, Box<Catch>, Box<Finally>),
}

impl fmt::Display for TryStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TryStatement::Catch(block, catch) => write!(f, "try {} {}", block, catch),
            TryStatement::Finally(block, finally) => write!(f, "try {} {}", block, finally),
            TryStatement::Full(block, catch, finally) => write!(f, "try {} {} {}", block, catch, finally),
        }
    }
}

impl PrettyPrint for TryStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}TryStatement: {}", first, self)?;
        match self {
            TryStatement::Catch(block, catch) => {
                block.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                catch.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            TryStatement::Finally(block, finally) => {
                block.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                finally.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            TryStatement::Full(block, catch, finally) => {
                block.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                catch.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                finally.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}TryStatement: {}", first, self)?;
        pprint_token(writer, "try", TokenType::Keyword, &successive, Spot::NotFinal)?;
        match self {
            TryStatement::Catch(block, catch) => {
                block.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                catch.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            TryStatement::Finally(block, finally) => {
                block.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                finally.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            TryStatement::Full(block, catch, finally) => {
                block.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                catch.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                finally.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl TryStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_try = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Try)?;
        let (block, after_block) = Block::parse(parser, after_try, yield_flag, await_flag, return_flag)?;
        enum CaseKind {
            Catch(Box<Catch>),
            Finally(Box<Finally>),
            Full(Box<Catch>, Box<Finally>),
        }
        Err(ParseError::new("Catch or Finally block expected", after_block.line, after_block.column))
            .otherwise(|| {
                let (fin, after_fin) = Finally::parse(parser, after_block, yield_flag, await_flag, return_flag)?;
                Ok((CaseKind::Finally(fin), after_fin))
            })
            .otherwise(|| {
                let (catch, after_catch) = Catch::parse(parser, after_block, yield_flag, await_flag, return_flag)?;
                match Finally::parse(parser, after_catch, yield_flag, await_flag, return_flag) {
                    Err(_) => Ok((CaseKind::Catch(catch), after_catch)),
                    Ok((fin, after_fin)) => Ok((CaseKind::Full(catch, fin), after_fin)),
                }
            })
            .map(|(kind, scan)| {
                (
                    Box::new(match kind {
                        CaseKind::Catch(c) => TryStatement::Catch(block, c),
                        CaseKind::Finally(f) => TryStatement::Finally(block, f),
                        CaseKind::Full(c, f) => TryStatement::Full(block, c, f),
                    }),
                    scan,
                )
            })
    }
}

// Catch[Yield, Await, Return] :
//      catch ( CatchParameter[?Yield, ?Await] ) Block[?Yield, ?Await, ?Return]
//      catch Block[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub struct Catch {
    parameter: Option<Box<CatchParameter>>,
    block: Box<Block>,
}

impl fmt::Display for Catch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.parameter {
            None => write!(f, "catch {}", self.block),
            Some(cp) => write!(f, "catch ( {} ) {}", cp, self.block),
        }
    }
}

impl PrettyPrint for Catch {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Catch: {}", first, self)?;
        if let Some(cp) = &self.parameter {
            cp.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        self.block.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Catch: {}", first, self)?;
        pprint_token(writer, "catch", TokenType::Keyword, &successive, Spot::NotFinal)?;
        if let Some(cp) = &self.parameter {
            pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
            cp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        }
        self.block.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl Catch {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_catch = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Catch)?;
        Err(ParseError::new("( or { expected", after_catch.line, after_catch.column))
            .otherwise(|| {
                let (block, after_block) = Block::parse(parser, after_catch, yield_flag, await_flag, return_flag)?;
                Ok((Box::new(Catch { parameter: None, block }), after_block))
            })
            .otherwise(|| {
                let after_open = scan_for_punct(after_catch, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
                let (cp, after_cp) = CatchParameter::parse(parser, after_open, yield_flag, await_flag)?;
                let after_close = scan_for_punct(after_cp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                let (block, after_block) = Block::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                Ok((Box::new(Catch { parameter: Some(cp), block }), after_block))
            })
    }
}

// Finally[Yield, Await, Return] :
//      finally Block[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub struct Finally {
    block: Box<Block>,
}

impl fmt::Display for Finally {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "finally {}", self.block)
    }
}

impl PrettyPrint for Finally {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Finally: {}", first, self)?;
        self.block.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Finally: {}", first, self)?;
        pprint_token(writer, "finally", TokenType::Keyword, &successive, Spot::NotFinal)?;
        self.block.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl Finally {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_fin = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Finally)?;
        let (block, after_block) = Block::parse(parser, after_fin, yield_flag, await_flag, return_flag)?;
        Ok((Box::new(Finally { block }), after_block))
    }
}

// CatchParameter[Yield, Await] :
//      BindingIdentifier[?Yield, ?Await]
//      BindingPattern[?Yield, ?Await]
#[derive(Debug)]
pub enum CatchParameter {
    Ident(Box<BindingIdentifier>),
    Pattern(Box<BindingPattern>),
}

impl fmt::Display for CatchParameter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CatchParameter::Ident(node) => node.fmt(f),
            CatchParameter::Pattern(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for CatchParameter {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CatchParameter: {}", first, self)?;
        match self {
            CatchParameter::Ident(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            CatchParameter::Pattern(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            CatchParameter::Ident(node) => node.concise_with_leftpad(writer, pad, state),
            CatchParameter::Pattern(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl CatchParameter {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("CatchParameter expected", scanner.line, scanner.column))
            .otherwise(|| {
                let (bi, after_bi) = BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Box::new(CatchParameter::Ident(bi)), after_bi))
            })
            .otherwise(|| {
                let (bp, after_bp) = BindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Box::new(CatchParameter::Pattern(bp)), after_bp))
            })
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}
