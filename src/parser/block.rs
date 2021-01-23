use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::scanner::Scanner;
use super::statements_and_declarations::{Declaration, Statement};
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

// BlockStatement[Yield, Await, Return] :
//      Block[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum BlockStatement {
    Block(Box<Block>),
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let BlockStatement::Block(node) = self;
        node.fmt(f)
    }
}

impl PrettyPrint for BlockStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BlockStatement: {}", first, self)?;
        let BlockStatement::Block(node) = self;
        node.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl BlockStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_block = Block::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        if let Some((block, after_block)) = pot_block {
            return Ok(Some((Box::new(BlockStatement::Block(block)), after_block)));
        }
        Ok(None)
    }
}

// Block[Yield, Await, Return] :
//      { StatementList[?Yield, ?Await, ?Return]opt }
#[derive(Debug)]
pub enum Block {
    Statements(Option<Box<StatementList>>),
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Block::Statements(opt_sl) = self;
        match opt_sl {
            None => write!(f, "{{ }}"),
            Some(node) => write!(f, "{{ {} }}", node),
        }
    }
}

impl PrettyPrint for Block {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Block: {}", first, self)?;
        let Block::Statements(opt_sl) = self;
        match opt_sl {
            None => Ok(()),
            Some(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
}

impl Block {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (token, after_tok) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        match token {
            scanner::Token::LeftBrace => {
                let pot_sl = StatementList::parse(parser, after_tok, yield_flag, await_flag, return_flag)?;
                let (sl, after_sl_scan) = match pot_sl {
                    None => (None, after_tok),
                    Some((node, after)) => (Some(node), after),
                };
                let (closing_tok, after_close) =
                    scanner::scan_token(&after_sl_scan, parser.source, scanner::ScanGoal::InputElementRegExp);
                match closing_tok {
                    scanner::Token::RightBrace => Ok(Some((Box::new(Block::Statements(sl)), after_close))),
                    _ => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }
}

// StatementList[Yield, Await, Return] :
//      StatementListItem[?Yield, ?Await, ?Return]
//      StatementList[?Yield, ?Await, ?Return] StatementListItem[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum StatementList {
    Item(Box<StatementListItem>),
    List(Box<StatementList>, Box<StatementListItem>),
}

impl fmt::Display for StatementList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StatementList::Item(node) => node.fmt(f),
            StatementList::List(lst, item) => write!(f, "{} {}", lst, item),
        }
    }
}

impl PrettyPrint for StatementList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}StatementList: {}", first, self)?;
        match self {
            StatementList::Item(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            StatementList::List(lst, item) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl StatementList {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_item = StatementListItem::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        match pot_item {
            None => Ok(None),
            Some((item, after_item)) => {
                let mut current = Box::new(StatementList::Item(item));
                let mut current_scanner = after_item;
                loop {
                    let next_item =
                        StatementListItem::parse(parser, current_scanner, yield_flag, await_flag, return_flag)?;
                    match next_item {
                        None => {
                            break;
                        }
                        Some((next, after_next)) => {
                            current = Box::new(StatementList::List(current, next));
                            current_scanner = after_next;
                        }
                    }
                }
                Ok(Some((current, current_scanner)))
            }
        }
    }
}

// StatementListItem[Yield, Await, Return] :
//      Statement[?Yield, ?Await, ?Return]
//      Declaration[?Yield, ?Await]
#[derive(Debug)]
pub enum StatementListItem {
    Statement(Box<Statement>),
    Declaration(Box<Declaration>),
}

impl fmt::Display for StatementListItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StatementListItem::Statement(node) => node.fmt(f),
            StatementListItem::Declaration(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for StatementListItem {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}StatementListItem: {}", first, self)?;
        match self {
            StatementListItem::Declaration(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            StatementListItem::Statement(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
}

impl StatementListItem {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_statement = Statement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        if let Some((statement, after_statement)) = pot_statement {
            return Ok(Some((Box::new(StatementListItem::Statement(statement)), after_statement)));
        }

        let pot_decl = Declaration::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((decl, after_decl)) = pot_decl {
            return Ok(Some((Box::new(StatementListItem::Declaration(decl)), after_decl)));
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
