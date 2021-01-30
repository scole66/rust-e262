use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::block::Block;
use super::declarations_and_variables::BindingPattern;
use super::identifiers::BindingIdentifier;
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

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
        pprint_token(writer, "try", &successive, Spot::NotFinal)?;
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
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (try_tok, after_try) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if matches!(try_tok, scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Try)) {
            let pot_block = Block::parse(parser, after_try, yield_flag, await_flag, return_flag)?;
            if let Some((block, after_block)) = pot_block {
                let pot_catch = Catch::parse(parser, after_block, yield_flag, await_flag, return_flag)?;
                let (catch, after_catch) = match pot_catch {
                    None => (None, after_block),
                    Some((c, s)) => (Some(c), s),
                };
                let pot_finally = Finally::parse(parser, after_catch, yield_flag, await_flag, return_flag)?;
                let (finally, after_finally) = match pot_finally {
                    None => (None, after_catch),
                    Some((f, s)) => (Some(f), s),
                };
                return match (catch, finally) {
                    (None, None) => Ok(None),
                    (Some(c), None) => Ok(Some((Box::new(TryStatement::Catch(block, c)), after_finally))),
                    (None, Some(f)) => Ok(Some((Box::new(TryStatement::Finally(block, f)), after_finally))),
                    (Some(c), Some(f)) => Ok(Some((Box::new(TryStatement::Full(block, c, f)), after_finally))),
                };
            }
        }
        Ok(None)
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
        pprint_token(writer, "catch", &successive, Spot::NotFinal)?;
        if let Some(cp) = &self.parameter {
            pprint_token(writer, "(", &successive, Spot::NotFinal)?;
            cp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            pprint_token(writer, ")", &successive, Spot::NotFinal)?;
        }
        self.block.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl Catch {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (catch_tok, after_catch) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementDiv);
        if matches!(catch_tok, scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Catch)) {
            let (open, after_open) =
                scanner::scan_token(&after_catch, parser.source, scanner::ScanGoal::InputElementDiv);
            if open == scanner::Token::LeftParen {
                let pot_cp = CatchParameter::parse(parser, after_open, yield_flag, await_flag)?;
                if let Some((cp, after_cp)) = pot_cp {
                    let (close, after_close) =
                        scanner::scan_token(&after_cp, parser.source, scanner::ScanGoal::InputElementDiv);
                    if close == scanner::Token::RightParen {
                        let pot_block = Block::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                        if let Some((block, after_block)) = pot_block {
                            return Ok(Some((
                                Box::new(Catch {
                                    parameter: Some(cp),
                                    block,
                                }),
                                after_block,
                            )));
                        }
                    }
                }
            } else {
                let pot_block = Block::parse(parser, after_catch, yield_flag, await_flag, return_flag)?;
                if let Some((block, after_block)) = pot_block {
                    return Ok(Some((Box::new(Catch { parameter: None, block }), after_block)));
                }
            }
        }
        Ok(None)
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
        pprint_token(writer, "finally", &successive, Spot::NotFinal)?;
        self.block.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl Finally {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok_fin, after_fin) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementDiv);
        if matches!(tok_fin, scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Finally)) {
            let pot_block = Block::parse(parser, after_fin, yield_flag, await_flag, return_flag)?;
            if let Some((block, after_block)) = pot_block {
                return Ok(Some((Box::new(Finally { block }), after_block)));
            }
        }
        Ok(None)
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
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_bi = BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((bi, after_bi)) = pot_bi {
            return Ok(Some((Box::new(CatchParameter::Ident(bi)), after_bi)));
        }
        let pot_bp = BindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((bp, after_bp)) = pot_bp {
            return Ok(Some((Box::new(CatchParameter::Pattern(bp)), after_bp)));
        }
        Ok(None)
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}
