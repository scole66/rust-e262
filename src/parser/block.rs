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
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // BLOCK STATEMENT
    #[test]
    fn block_statement_test_01() {
        let (node, scanner) = check(BlockStatement::parse(&mut newparser("{q;}"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 4);
        pretty_check(&*node, "BlockStatement: { q ; }", vec!["Block: { q ; }"]);
        concise_check(&*node, "Block: { q ; }", vec!["Punctuator: {", "ExpressionStatement: q ;", "Punctuator: }"]);
        format!("{:?}", node);
    }
    #[test]
    fn block_statement_test_err_01() {
        check_err(BlockStatement::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘{’ expected", 1, 1);
    }
    #[test]
    fn block_statement_test_prettyerrors_1() {
        let (item, _) = BlockStatement::parse(&mut newparser("{ statement_list; }"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn block_statement_test_conciseerrors_1() {
        let (item, _) = BlockStatement::parse(&mut newparser("{ statement_list; }"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }

    // BLOCK
    #[test]
    fn block_test_01() {
        let (node, scanner) = check(Block::parse(&mut newparser("{q;}"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 4);
        pretty_check(&*node, "Block: { q ; }", vec!["StatementList: q ;"]);
        concise_check(&*node, "Block: { q ; }", vec!["Punctuator: {", "ExpressionStatement: q ;", "Punctuator: }"]);
        format!("{:?}", node);
    }
    #[test]
    fn block_test_02() {
        let (node, scanner) = check(Block::parse(&mut newparser("{}"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 2);
        pretty_check(&*node, "Block: { }", vec![]);
        concise_check(&*node, "Block: { }", vec!["Punctuator: {", "Punctuator: }"]);
        format!("{:?}", node);
    }
    #[test]
    fn block_test_cache_01() {
        let mut parser = newparser("{ a=1; b=2; c=3; }");
        let (node, scanner) = check(Block::parse(&mut parser, Scanner::new(), false, false, true));
        let (node2, scanner2) = check(Block::parse(&mut parser, Scanner::new(), false, false, true));
        assert!(scanner == scanner2);
        assert!(Rc::ptr_eq(&node, &node2));
    }
    #[test]
    fn block_test_err_01() {
        check_err(Block::parse(&mut newparser(""), Scanner::new(), false, false, true), "‘{’ expected", 1, 1);
    }
    #[test]
    fn block_test_err_02() {
        check_err(Block::parse(&mut newparser("{"), Scanner::new(), false, false, true), "‘}’ expected", 1, 2);
    }
    #[test]
    fn block_test_prettyerrors_1() {
        let (item, _) = Block::parse(&mut newparser("{ statement_list; }"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn block_test_prettyerrors_2() {
        let (item, _) = Block::parse(&mut newparser("{}"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn block_test_conciseerrors_1() {
        let (item, _) = Block::parse(&mut newparser("{ statement_list; }"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn block_test_conciseerrors_2() {
        let (item, _) = Block::parse(&mut newparser("{}"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }

    // STATEMENT LIST
    #[test]
    fn statement_list_test_01() {
        let (node, scanner) = check(StatementList::parse(&mut newparser("a;"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 2);
        pretty_check(&*node, "StatementList: a ;", vec!["StatementListItem: a ;"]);
        concise_check(&*node, "ExpressionStatement: a ;", vec!["IdentifierName: a", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn statement_list_test_02() {
        let (node, scanner) = check(StatementList::parse(&mut newparser("a; b;"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 5);
        pretty_check(&*node, "StatementList: a ; b ;", vec!["StatementList: a ;", "StatementListItem: b ;"]);
        concise_check(&*node, "StatementList: a ; b ;", vec!["ExpressionStatement: a ;", "ExpressionStatement: b ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn statement_list_test_cache_01() {
        let mut parser = newparser("a=1; b=2; c=3;");
        let (node, scanner) = check(StatementList::parse(&mut parser, Scanner::new(), false, false, true));
        let (node2, scanner2) = check(StatementList::parse(&mut parser, Scanner::new(), false, false, true));
        assert!(scanner == scanner2);
        assert!(Rc::ptr_eq(&node, &node2));
    }
    #[test]
    fn statement_list_test_err_01() {
        check_err(StatementList::parse(&mut newparser(""), Scanner::new(), false, false, true), "Declaration or Statement expected", 1, 1);
    }
    #[test]
    fn statement_list_test_prettyerrors_1() {
        let (item, _) = StatementList::parse(&mut newparser("statement_list;"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn statement_list_test_prettyerrors_2() {
        let (item, _) = StatementList::parse(&mut newparser("statement; statement; statement;"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn statement_list_test_conciseerrors_1() {
        let (item, _) = StatementList::parse(&mut newparser("statement_list;"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn statement_list_test_conciseerrors_2() {
        let (item, _) = StatementList::parse(&mut newparser("statement; statement; statement;"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    // STATEMENT LIST ITEM
    #[test]
    fn statement_list_item_test_01() {
        let (node, scanner) = check(StatementListItem::parse(&mut newparser("a;"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 2);
        pretty_check(&*node, "StatementListItem: a ;", vec!["Statement: a ;"]);
        concise_check(&*node, "ExpressionStatement: a ;", vec!["IdentifierName: a", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn statement_list_item_test_02() {
        let (node, scanner) = check(StatementListItem::parse(&mut newparser("let a;"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 6);
        pretty_check(&*node, "StatementListItem: let a ;", vec!["Declaration: let a ;"]);
        concise_check(&*node, "LexicalDeclaration: let a ;", vec!["Keyword: let", "IdentifierName: a", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn statement_list_item_test_err_01() {
        check_err(StatementListItem::parse(&mut newparser(""), Scanner::new(), false, false, true), "Declaration or Statement expected", 1, 1);
    }
    #[test]
    fn statement_list_item_test_prettyerrors_1() {
        let (item, _) = StatementListItem::parse(&mut newparser("statement_list;"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn statement_list_item_test_prettyerrors_2() {
        let (item, _) = StatementListItem::parse(&mut newparser("const declaration = 0;"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn statement_list_item_test_conciseerrors_1() {
        let (item, _) = StatementListItem::parse(&mut newparser("statement_list;"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn statement_list_item_test_conciseerrors_2() {
        let (item, _) = StatementListItem::parse(&mut newparser("const declaration = 0;"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }

    #[test]
    fn really_slow() {
        StatementList::parse(&mut newparser("obj = { blue: 0x0000ff, bluify: function (r, g, b) { return b; } };"), Scanner::new(), false, false, true).expect("test");
    }
    #[test]
    fn probe() {
        StatementList::parse(&mut newparser("obj = { };"), Scanner::new(), false, false, true).expect("test");
    }
}
