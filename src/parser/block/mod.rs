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
    Block(Rc<Block>),
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
    // no caching needed
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let (block, after_block) = Block::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        Ok((Rc::new(BlockStatement::Block(block)), after_block))
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        let BlockStatement::Block(node) = self;
        node.var_declared_names()
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        let BlockStatement::Block(node) = self;
        node.contains_undefined_break_target(label_set)
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let BlockStatement::Block(node) = self;
        node.contains(kind)
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        let BlockStatement::Block(node) = self;
        node.contains_duplicate_labels(label_set)
    }
}

// Block[Yield, Await, Return] :
//      { StatementList[?Yield, ?Await, ?Return]opt }
#[derive(Debug)]
pub enum Block {
    Statements(Option<Rc<StatementList>>),
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
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let after_lb = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBrace)?;
        let (sl, after_sl) = match StatementList::parse(parser, after_lb, yield_flag, await_flag, return_flag) {
            Err(_) => (None, after_lb),
            Ok((node, scan)) => (Some(node), scan),
        };
        let after_rb = scan_for_punct(after_sl, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(Block::Statements(sl)), after_rb))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitReturnKey { scanner, yield_flag, await_flag, return_flag };
        match parser.block_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag, return_flag);
                parser.block_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        let Block::Statements(opt_sl) = self;
        match opt_sl {
            None => vec![],
            Some(node) => node.var_declared_names(),
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        let Block::Statements(opt_sl) = self;
        match opt_sl {
            None => false,
            Some(node) => node.contains_undefined_break_target(label_set),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let Block::Statements(node) = self;
        match node {
            None => false,
            Some(n) => n.contains(kind),
        }
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        let Block::Statements(node) = self;
        match node {
            None => false,
            Some(n) => n.contains_duplicate_labels(label_set),
        }
    }
}

// StatementList[Yield, Await, Return] :
//      StatementListItem[?Yield, ?Await, ?Return]
//      StatementList[?Yield, ?Await, ?Return] StatementListItem[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum StatementList {
    Item(Rc<StatementListItem>),
    List(Rc<StatementList>, Rc<StatementListItem>),
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
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let (item, after_item) = StatementListItem::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        let mut current = Rc::new(StatementList::Item(item));
        let mut current_scanner = after_item;
        while let Ok((next, after_next)) = StatementListItem::parse(parser, current_scanner, yield_flag, await_flag, return_flag) {
            current = Rc::new(StatementList::List(current, next));
            current_scanner = after_next;
        }
        Ok((current, current_scanner))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitReturnKey { scanner, yield_flag, await_flag, return_flag };
        match parser.statement_list_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag, return_flag);
                parser.statement_list_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn top_level_lexically_declared_names(&self) -> Vec<JSString> {
        match self {
            StatementList::Item(node) => node.top_level_lexically_declared_names(),
            StatementList::List(list, item) => {
                let mut result = list.top_level_lexically_declared_names();
                result.extend(item.top_level_lexically_declared_names());
                result
            }
        }
    }

    pub fn top_level_var_declared_names(&self) -> Vec<JSString> {
        match self {
            StatementList::Item(node) => node.top_level_var_declared_names(),
            StatementList::List(lst, item) => {
                let mut names = lst.top_level_var_declared_names();
                names.extend(item.top_level_var_declared_names());
                names
            }
        }
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            StatementList::Item(node) => node.var_declared_names(),
            StatementList::List(lst, item) => {
                let mut names = lst.var_declared_names();
                names.extend(item.var_declared_names());
                names
            }
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            StatementList::Item(node) => node.contains_undefined_break_target(label_set),
            StatementList::List(lst, item) => lst.contains_undefined_break_target(label_set) || item.contains_undefined_break_target(label_set),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            StatementList::Item(node) => kind == ParseNodeKind::StatementListItem || node.contains(kind),
            StatementList::List(lst, item) => kind == ParseNodeKind::StatementList || kind == ParseNodeKind::StatementListItem || lst.contains(kind) || item.contains(kind),
        }
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self {
            StatementList::Item(node) => node.contains_duplicate_labels(label_set),
            StatementList::List(lst, item) => lst.contains_duplicate_labels(label_set) || item.contains_duplicate_labels(label_set),
        }
    }
}

// StatementListItem[Yield, Await, Return] :
//      Statement[?Yield, ?Await, ?Return]
//      Declaration[?Yield, ?Await]
#[derive(Debug)]
pub enum StatementListItem {
    Statement(Rc<Statement>),
    Declaration(Rc<Declaration>),
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
    // no caching needed
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("Declaration or Statement expected", scanner.line, scanner.column))
            .otherwise(|| {
                Statement::parse(parser, scanner, yield_flag, await_flag, return_flag).map(|(statement, after_statement)| (Rc::new(StatementListItem::Statement(statement)), after_statement))
            })
            .otherwise(|| Declaration::parse(parser, scanner, yield_flag, await_flag).map(|(decl, after_decl)| (Rc::new(StatementListItem::Declaration(decl)), after_decl)))
    }

    pub fn top_level_lexically_declared_names(&self) -> Vec<JSString> {
        match self {
            StatementListItem::Statement(_) => vec![],
            StatementListItem::Declaration(node) => match **node {
                Declaration::Hoistable(_) => vec![],
                _ => node.bound_names(),
            },
        }
    }

    pub fn top_level_var_declared_names(&self) -> Vec<JSString> {
        match self {
            StatementListItem::Statement(node) => match &**node {
                Statement::Labelled(stmt) => stmt.top_level_var_declared_names(),
                _ => node.var_declared_names(),
            },
            StatementListItem::Declaration(node) => match &**node {
                Declaration::Hoistable(decl) => decl.bound_names(),
                _ => vec![],
            },
        }
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            StatementListItem::Statement(node) => node.var_declared_names(),
            StatementListItem::Declaration(..) => vec![],
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            StatementListItem::Statement(node) => node.contains_undefined_break_target(label_set),
            StatementListItem::Declaration(..) => false,
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            StatementListItem::Statement(node) => kind == ParseNodeKind::Statement || node.contains(kind),
            StatementListItem::Declaration(node) => kind == ParseNodeKind::Declaration || node.contains(kind),
        }
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self {
            StatementListItem::Statement(node) => node.contains_duplicate_labels(label_set),
            StatementListItem::Declaration(_) => false,
        }
    }
}

#[cfg(test)]
mod tests;
