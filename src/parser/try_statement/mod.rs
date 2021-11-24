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
    Catch(Rc<Block>, Rc<Catch>),
    Finally(Rc<Block>, Rc<Finally>),
    Full(Rc<Block>, Rc<Catch>, Rc<Finally>),
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let after_try = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Try)?;
        let (block, after_block) = Block::parse(parser, after_try, yield_flag, await_flag, return_flag)?;
        enum CaseKind {
            Catch(Rc<Catch>),
            Finally(Rc<Finally>),
            Full(Rc<Catch>, Rc<Finally>),
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
                    Rc::new(match kind {
                        CaseKind::Catch(c) => TryStatement::Catch(block, c),
                        CaseKind::Finally(f) => TryStatement::Finally(block, f),
                        CaseKind::Full(c, f) => TryStatement::Full(block, c, f),
                    }),
                    scan,
                )
            })
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            TryStatement::Catch(block, catch) => {
                let mut names = block.var_declared_names();
                names.extend(catch.var_declared_names());
                names
            }
            TryStatement::Finally(block, finally) => {
                let mut names = block.var_declared_names();
                names.extend(finally.var_declared_names());
                names
            }
            TryStatement::Full(block, catch, finally) => {
                let mut names = block.var_declared_names();
                names.extend(catch.var_declared_names());
                names.extend(finally.var_declared_names());
                names
            }
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            TryStatement::Catch(block, catch) => block.contains_undefined_break_target(label_set) || catch.contains_undefined_break_target(label_set),
            TryStatement::Finally(block, finally) => block.contains_undefined_break_target(label_set) || finally.contains_undefined_break_target(label_set),
            TryStatement::Full(block, catch, finally) => {
                block.contains_undefined_break_target(label_set) || catch.contains_undefined_break_target(label_set) || finally.contains_undefined_break_target(label_set)
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            TryStatement::Catch(block, catch) => block.contains(kind) || catch.contains(kind),
            TryStatement::Finally(block, finally) => block.contains(kind) || finally.contains(kind),
            TryStatement::Full(block, catch, finally) => block.contains(kind) || catch.contains(kind) || finally.contains(kind),
        }
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self {
            TryStatement::Catch(block, catch) => block.contains_duplicate_labels(label_set) || catch.contains_duplicate_labels(label_set),
            TryStatement::Finally(block, finally) => block.contains_duplicate_labels(label_set) || finally.contains_duplicate_labels(label_set),
            TryStatement::Full(block, catch, finally) => {
                block.contains_duplicate_labels(label_set) || catch.contains_duplicate_labels(label_set) || finally.contains_duplicate_labels(label_set)
            }
        }
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        match self {
            TryStatement::Catch(block, catch) => block.contains_undefined_continue_target(iteration_set, &[]) || catch.contains_undefined_continue_target(iteration_set),
            TryStatement::Finally(block, finally) => block.contains_undefined_continue_target(iteration_set, &[]) || finally.contains_undefined_continue_target(iteration_set),
            TryStatement::Full(block, catch, finally) => {
                block.contains_undefined_continue_target(iteration_set, &[])
                    || catch.contains_undefined_continue_target(iteration_set)
                    || finally.contains_undefined_continue_target(iteration_set)
            }
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
            TryStatement::Catch(block, catch) => block.all_private_identifiers_valid(names) && catch.all_private_identifiers_valid(names),
            TryStatement::Finally(block, finally) => block.all_private_identifiers_valid(names) && finally.all_private_identifiers_valid(names),
            TryStatement::Full(block, catch, finally) => {
                block.all_private_identifiers_valid(names) && catch.all_private_identifiers_valid(names) && finally.all_private_identifiers_valid(names)
            }
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// Catch[Yield, Await, Return] :
//      catch ( CatchParameter[?Yield, ?Await] ) Block[?Yield, ?Await, ?Return]
//      catch Block[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub struct Catch {
    parameter: Option<Rc<CatchParameter>>,
    block: Rc<Block>,
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let after_catch = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Catch)?;
        Err(ParseError::new("( or { expected", after_catch.line, after_catch.column))
            .otherwise(|| {
                let (block, after_block) = Block::parse(parser, after_catch, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(Catch { parameter: None, block }), after_block))
            })
            .otherwise(|| {
                let after_open = scan_for_punct(after_catch, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
                let (cp, after_cp) = CatchParameter::parse(parser, after_open, yield_flag, await_flag)?;
                let after_close = scan_for_punct(after_cp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                let (block, after_block) = Block::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(Catch { parameter: Some(cp), block }), after_block))
            })
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        self.block.var_declared_names()
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        self.block.contains_undefined_break_target(label_set)
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.parameter.as_ref().map_or(false, |n| n.contains(kind)) || self.block.contains(kind)
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        self.block.contains_duplicate_labels(label_set)
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        self.block.contains_undefined_continue_target(iteration_set, &[])
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.parameter.as_ref().map_or(true, |n| n.all_private_identifiers_valid(names)) && self.block.all_private_identifiers_valid(names)
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// Finally[Yield, Await, Return] :
//      finally Block[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub struct Finally {
    block: Rc<Block>,
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let after_fin = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Finally)?;
        let (block, after_block) = Block::parse(parser, after_fin, yield_flag, await_flag, return_flag)?;
        Ok((Rc::new(Finally { block }), after_block))
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        self.block.var_declared_names()
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        self.block.contains_undefined_break_target(label_set)
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.block.contains(kind)
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        self.block.contains_duplicate_labels(label_set)
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        self.block.contains_undefined_continue_target(iteration_set, &[])
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.block.all_private_identifiers_valid(names)
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// CatchParameter[Yield, Await] :
//      BindingIdentifier[?Yield, ?Await]
//      BindingPattern[?Yield, ?Await]
#[derive(Debug)]
pub enum CatchParameter {
    Ident(Rc<BindingIdentifier>),
    Pattern(Rc<BindingPattern>),
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
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("CatchParameter expected", scanner.line, scanner.column))
            .otherwise(|| {
                let (bi, after_bi) = BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(CatchParameter::Ident(bi)), after_bi))
            })
            .otherwise(|| {
                let (bp, after_bp) = BindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(CatchParameter::Pattern(bp)), after_bp))
            })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.catch_parameter_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.catch_parameter_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            CatchParameter::Ident(node) => node.contains(kind),
            CatchParameter::Pattern(node) => node.contains(kind),
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
            CatchParameter::Ident(_) => true,
            CatchParameter::Pattern(node) => node.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

#[cfg(test)]
mod tests;
