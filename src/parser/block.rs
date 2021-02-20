use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::statements_and_declarations::{Declaration, Statement};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

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

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let BlockStatement::Block(node) = self;
        node.concise_with_leftpad(writer, pad, state)
    }
}

impl BlockStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (block, after_block) = Block::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        Ok((Box::new(BlockStatement::Block(block)), after_block))
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

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Block: {}", first, self)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match self {
            Block::Statements(None) => {}
            Block::Statements(Some(node)) => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
        }
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl Block {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBrace).and_then(|after_tok| {
            let (err, sl, after_sl_scan) = match StatementList::parse(parser, after_tok, yield_flag, await_flag, return_flag) {
                Err(err) => (err, None, after_tok),
                Ok((node, after)) => (ParseError::new("", 1, 1), Some(node), after),
            };
            scan_for_punct(after_sl_scan, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBrace)
                .and_then(|after_close| Ok((Box::new(Block::Statements(sl)), after_close)))
                .otherwise(|| Err(err))
        })
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
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        match self {
            StatementList::Item(node) => node.concise_with_leftpad(writer, pad, state),
            StatementList::List(lst, item) => {
                writeln!(writer, "{}StatementList: {}", first, self)?;
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl StatementList {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        StatementListItem::parse(parser, scanner, yield_flag, await_flag, return_flag).and_then(|(item, after_item)| {
            let mut current = Box::new(StatementList::Item(item));
            let mut current_scanner = after_item;
            loop {
                match StatementListItem::parse(parser, current_scanner, yield_flag, await_flag, return_flag) {
                    Err(_) => {
                        break;
                    }
                    Ok((next, after_next)) => {
                        current = Box::new(StatementList::List(current, next));
                        current_scanner = after_next;
                    }
                }
            }
            Ok((current, current_scanner))
        })
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
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            StatementListItem::Statement(node) => node.concise_with_leftpad(writer, pad, state),
            StatementListItem::Declaration(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl StatementListItem {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("Declaration or Statement expected", scanner.line, scanner.column))
            .otherwise(|| {
                Statement::parse(parser, scanner, yield_flag, await_flag, return_flag)
                    .and_then(|(statement, after_statement)| Ok((Box::new(StatementListItem::Statement(statement)), after_statement)))
            })
            .otherwise(|| Declaration::parse(parser, scanner, yield_flag, await_flag).and_then(|(decl, after_decl)| Ok((Box::new(StatementListItem::Declaration(decl)), after_decl))))
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}
