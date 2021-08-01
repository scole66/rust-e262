use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::block::StatementList;
use super::comma_operator::Expression;
use super::scanner::{Keyword, Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// SwitchStatement[Yield, Await, Return] :
//      switch ( Expression[+In, ?Yield, ?Await] ) CaseBlock[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub struct SwitchStatement {
    expression: Rc<Expression>,
    case_block: Rc<CaseBlock>,
}

impl fmt::Display for SwitchStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "switch ( {} ) {}", self.expression, self.case_block)
    }
}

impl PrettyPrint for SwitchStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}SwitchStatement: {}", first, self)?;
        self.expression.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.case_block.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}SwitchStatement: {}", first, self)?;
        pprint_token(writer, "switch", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.expression.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.case_block.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl SwitchStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let after_switch = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Switch)?;
        let after_open = scan_for_punct(after_switch, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (exp, after_exp) = Expression::parse(parser, after_open, true, yield_flag, await_flag)?;
        let after_close = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let (cb, after_cases) = CaseBlock::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
        Ok((Rc::new(SwitchStatement { expression: exp, case_block: cb }), after_cases))
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        self.case_block.var_declared_names()
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        self.case_block.contains_undefined_break_target(label_set)
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.expression.contains(kind) || self.case_block.contains(kind)
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        self.case_block.contains_duplicate_labels(label_set)
    }
}

// CaseBlock[Yield, Await, Return] :
//      { CaseClauses[?Yield, ?Await, ?Return]opt }
//      { CaseClauses[?Yield, ?Await, ?Return]opt DefaultClause[?Yield, ?Await, ?Return] CaseClauses[?Yield, ?Await, ?Return]opt }
#[derive(Debug)]
pub enum CaseBlock {
    NoDefault(Option<Rc<CaseClauses>>),
    HasDefault(Option<Rc<CaseClauses>>, Rc<DefaultClause>, Option<Rc<CaseClauses>>),
}

impl fmt::Display for CaseBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CaseBlock::NoDefault(None) => write!(f, "{{ }}"),
            CaseBlock::NoDefault(Some(node)) => write!(f, "{{ {} }}", node),
            CaseBlock::HasDefault(None, def, None) => write!(f, "{{ {} }}", def),
            CaseBlock::HasDefault(Some(pre), def, None) => write!(f, "{{ {} {} }}", pre, def),
            CaseBlock::HasDefault(None, def, Some(post)) => write!(f, "{{ {} {} }}", def, post),
            CaseBlock::HasDefault(Some(pre), def, Some(post)) => write!(f, "{{ {} {} {} }}", pre, def, post),
        }
    }
}

impl PrettyPrint for CaseBlock {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CaseBlock: {}", first, self)?;
        match self {
            CaseBlock::NoDefault(None) => Ok(()),
            CaseBlock::NoDefault(Some(node)) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            CaseBlock::HasDefault(None, def, None) => def.pprint_with_leftpad(writer, &successive, Spot::Final),
            CaseBlock::HasDefault(Some(pre), def, None) => {
                pre.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                def.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CaseBlock::HasDefault(None, def, Some(post)) => {
                def.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                post.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CaseBlock::HasDefault(Some(pre), def, Some(post)) => {
                pre.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                def.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                post.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CaseBlock: {}", first, self)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match self {
            CaseBlock::NoDefault(None) => Ok(()),
            CaseBlock::NoDefault(Some(node)) => node.concise_with_leftpad(writer, &successive, Spot::NotFinal),
            CaseBlock::HasDefault(None, def, None) => def.concise_with_leftpad(writer, &successive, Spot::NotFinal),
            CaseBlock::HasDefault(Some(pre), def, None) => {
                pre.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                def.concise_with_leftpad(writer, &successive, Spot::NotFinal)
            }
            CaseBlock::HasDefault(None, def, Some(post)) => {
                def.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                post.concise_with_leftpad(writer, &successive, Spot::NotFinal)
            }
            CaseBlock::HasDefault(Some(pre), def, Some(post)) => {
                pre.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                def.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                post.concise_with_leftpad(writer, &successive, Spot::NotFinal)
            }
        }?;
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl CaseBlock {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let after_open = scan_for_punct(scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (pre, after_pre) = match CaseClauses::parse(parser, after_open, yield_flag, await_flag, return_flag) {
            Ok((node, scan)) => (Some(node), scan),
            Err(_) => (None, after_open),
        };
        Err(ParseError::new("‘}’, ‘case’, or ‘default’ expected", after_pre.line, after_pre.column))
            .otherwise(|| {
                let after_close = scan_for_punct(after_pre, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                Ok((None, after_close))
            })
            .otherwise(|| {
                let (def, after_def) = DefaultClause::parse(parser, after_pre, yield_flag, await_flag, return_flag)?;
                let (post, after_post) = match CaseClauses::parse(parser, after_def, yield_flag, await_flag, return_flag) {
                    Ok((node, scan)) => (Some(node), scan),
                    Err(_) => (None, after_def),
                };
                let after_close = scan_for_punct(after_post, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                Ok((Some((def, post)), after_close))
            })
            .map(|(post, scan)| {
                (
                    Rc::new(match post {
                        None => CaseBlock::NoDefault(pre),
                        Some((def, after)) => CaseBlock::HasDefault(pre, def, after),
                    }),
                    scan,
                )
            })
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            CaseBlock::NoDefault(None) => vec![],
            CaseBlock::NoDefault(Some(node)) => node.var_declared_names(),
            CaseBlock::HasDefault(pre, def, post) => {
                let mut names = match pre {
                    None => vec![],
                    Some(node) => node.var_declared_names(),
                };
                names.extend(def.var_declared_names());
                if let Some(node) = post {
                    names.extend(node.var_declared_names());
                }
                names
            }
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            CaseBlock::NoDefault(None) => false,
            CaseBlock::NoDefault(Some(node)) => node.contains_undefined_break_target(label_set),
            CaseBlock::HasDefault(pre, def, post) => {
                pre.as_ref().map_or(false, |node| node.contains_undefined_break_target(label_set))
                    || def.contains_undefined_break_target(label_set)
                    || post.as_ref().map_or(false, |node| node.contains_undefined_break_target(label_set))
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            CaseBlock::NoDefault(opt) => opt.as_ref().map_or(false, |n| n.contains(kind)),
            CaseBlock::HasDefault(opt1, def, opt2) => opt1.as_ref().map_or(false, |n| n.contains(kind)) || def.contains(kind) || opt2.as_ref().map_or(false, |n| n.contains(kind)),
        }
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self {
            CaseBlock::NoDefault(None) => false,
            CaseBlock::NoDefault(Some(node)) => node.contains_duplicate_labels(label_set),
            CaseBlock::HasDefault(pre, def, post) => {
                pre.as_ref().map_or(false, |node| node.contains_duplicate_labels(label_set))
                    || def.contains_duplicate_labels(label_set)
                    || post.as_ref().map_or(false, |node| node.contains_duplicate_labels(label_set))
            }
        }
    }
}

// CaseClauses[Yield, Await, Return] :
//      CaseClause[?Yield, ?Await, ?Return]
//      CaseClauses[?Yield, ?Await, ?Return] CaseClause[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum CaseClauses {
    Item(Rc<CaseClause>),
    List(Rc<CaseClauses>, Rc<CaseClause>),
}

impl fmt::Display for CaseClauses {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CaseClauses::Item(node) => node.fmt(f),
            CaseClauses::List(lst, item) => write!(f, "{} {}", lst, item),
        }
    }
}

impl PrettyPrint for CaseClauses {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CaseClauses: {}", first, self)?;
        match self {
            CaseClauses::Item(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            CaseClauses::List(lst, item) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            CaseClauses::Item(node) => node.concise_with_leftpad(writer, pad, state),
            CaseClauses::List(lst, item) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}CaseClauses: {}", first, self)?;
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl CaseClauses {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let (item, after_item) = CaseClause::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        let mut current = Rc::new(CaseClauses::Item(item));
        let mut current_scanner = after_item;
        while let Ok((next, after_next)) = CaseClause::parse(parser, current_scanner, yield_flag, await_flag, return_flag) {
            current = Rc::new(CaseClauses::List(current, next));
            current_scanner = after_next;
        }
        Ok((current, current_scanner))
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            CaseClauses::Item(node) => node.var_declared_names(),
            CaseClauses::List(lst, item) => {
                let mut names = lst.var_declared_names();
                names.extend(item.var_declared_names());
                names
            }
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            CaseClauses::Item(node) => node.contains_undefined_break_target(label_set),
            CaseClauses::List(lst, item) => lst.contains_undefined_break_target(label_set) || item.contains_undefined_break_target(label_set),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            CaseClauses::Item(node) => node.contains(kind),
            CaseClauses::List(lst, item) => lst.contains(kind) || item.contains(kind),
        }
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self {
            CaseClauses::Item(node) => node.contains_duplicate_labels(label_set),
            CaseClauses::List(lst, item) => lst.contains_duplicate_labels(label_set) || item.contains_duplicate_labels(label_set),
        }
    }
}

// CaseClause[Yield, Await, Return] :
//      case Expression[+In, ?Yield, ?Await] : StatementList[?Yield, ?Await, ?Return]opt
#[derive(Debug)]
pub struct CaseClause {
    expression: Rc<Expression>,
    statements: Option<Rc<StatementList>>,
}

impl fmt::Display for CaseClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.statements {
            None => write!(f, "case {} :", self.expression),
            Some(s) => write!(f, "case {} : {}", self.expression, s),
        }
    }
}

impl PrettyPrint for CaseClause {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CaseClause: {}", first, self)?;
        match &self.statements {
            None => self.expression.pprint_with_leftpad(writer, &successive, Spot::Final),
            Some(s) => {
                self.expression.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                s.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CaseClause: {}", first, self)?;
        pprint_token(writer, "case", TokenType::Keyword, &successive, Spot::NotFinal)?;
        self.expression.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        match &self.statements {
            Some(s) => {
                pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                s.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            None => pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::Final),
        }
    }
}

impl CaseClause {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let after_case = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Case)?;
        let (exp, after_exp) = Expression::parse(parser, after_case, true, yield_flag, await_flag)?;
        let after_colon = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::Colon)?;
        let (stmt, after_stmt) = match StatementList::parse(parser, after_colon, yield_flag, await_flag, return_flag) {
            Err(_) => (None, after_colon),
            Ok((stmt, s)) => (Some(stmt), s),
        };
        Ok((Rc::new(CaseClause { expression: exp, statements: stmt }), after_stmt))
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match &self.statements {
            None => vec![],
            Some(s) => s.var_declared_names(),
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match &self.statements {
            None => false,
            Some(s) => s.contains_undefined_break_target(label_set),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.expression.contains(kind) || self.statements.as_ref().map_or(false, |n| n.contains(kind))
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match &self.statements {
            None => false,
            Some(s) => s.contains_duplicate_labels(label_set),
        }
    }
}

// DefaultClause[Yield, Await, Return] :
//      default : StatementList[?Yield, ?Await, ?Return]opt
#[derive(Debug)]
pub struct DefaultClause(Option<Rc<StatementList>>);

impl fmt::Display for DefaultClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DefaultClause(None) => write!(f, "default :"),
            DefaultClause(Some(sl)) => write!(f, "default : {}", sl),
        }
    }
}

impl PrettyPrint for DefaultClause {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}DefaultClause: {}", first, self)?;
        match self {
            DefaultClause(None) => Ok(()),
            DefaultClause(Some(sl)) => sl.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}DefaultClause: {}", first, self)?;
        pprint_token(writer, "default", TokenType::Keyword, &successive, Spot::NotFinal)?;
        match self {
            DefaultClause(None) => pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::Final),
            DefaultClause(Some(sl)) => {
                pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                sl.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl DefaultClause {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let after_def = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Default)?;
        let after_colon = scan_for_punct(after_def, parser.source, ScanGoal::InputElementDiv, Punctuator::Colon)?;
        let (sl, after_sl) = match StatementList::parse(parser, after_colon, yield_flag, await_flag, return_flag) {
            Err(_) => (None, after_colon),
            Ok((lst, scan)) => (Some(lst), scan),
        };
        Ok((Rc::new(DefaultClause(sl)), after_sl))
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            DefaultClause(None) => vec![],
            DefaultClause(Some(sl)) => sl.var_declared_names(),
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            DefaultClause(None) => false,
            DefaultClause(Some(sl)) => sl.contains_undefined_break_target(label_set),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let DefaultClause(opt) = self;
        opt.as_ref().map_or(false, |n| n.contains(kind))
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self {
            DefaultClause(None) => false,
            DefaultClause(Some(sl)) => sl.contains_duplicate_labels(label_set),
        }
    }
}

#[cfg(test)]
mod tests;
