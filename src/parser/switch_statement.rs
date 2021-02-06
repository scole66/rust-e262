use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::block::StatementList;
use super::comma_operator::Expression;
use super::scanner::{scan_token, Keyword, Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

// SwitchStatement[Yield, Await, Return] :
//      switch ( Expression[+In, ?Yield, ?Await] ) CaseBlock[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub struct SwitchStatement {
    expression: Box<Expression>,
    case_block: Box<CaseBlock>,
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
        self.expression
            .pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.case_block.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}SwitchStatement: {}", first, self)?;
        pprint_token(writer, "switch", &successive, Spot::NotFinal)?;
        pprint_token(writer, "(", &successive, Spot::NotFinal)?;
        self.expression
            .concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", &successive, Spot::NotFinal)?;
        self.case_block.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl SwitchStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok_switch, after_switch) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        if tok_switch.matches_keyword(Keyword::Switch) {
            let (open, after_open) = scan_token(&after_switch, parser.source, ScanGoal::InputElementDiv);
            if open.matches_punct(Punctuator::LeftParen) {
                let pot_exp = Expression::parse(parser, after_open, true, yield_flag, await_flag)?;
                if let Some((exp, after_exp)) = pot_exp {
                    let (close, after_close) = scan_token(&after_exp, parser.source, ScanGoal::InputElementDiv);
                    if close.matches_punct(Punctuator::RightParen) {
                        let pot_block = CaseBlock::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
                        if let Some((block, after_block)) = pot_block {
                            return Ok(Some((
                                Box::new(SwitchStatement {
                                    expression: exp,
                                    case_block: block,
                                }),
                                after_block,
                            )));
                        }
                    }
                }
            }
        }
        Ok(None)
    }
}

// CaseBlock[Yield, Await, Return] :
//      { CaseClauses[?Yield, ?Await, ?Return]opt }
//      { CaseClauses[?Yield, ?Await, ?Return]opt DefaultClause[?Yield, ?Await, ?Return] CaseClauses[?Yield, ?Await, ?Return]opt }
#[derive(Debug)]
pub enum CaseBlock {
    NoDefault(Option<Box<CaseClauses>>),
    HasDefault(Option<Box<CaseClauses>>, Box<DefaultClause>, Option<Box<CaseClauses>>),
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
        pprint_token(writer, "{", &successive, Spot::NotFinal)?;
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
        pprint_token(writer, "}", &successive, Spot::Final)
    }
}

impl CaseBlock {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (open, after_open) = scan_token(&scanner, parser.source, ScanGoal::InputElementDiv);
        if open.matches_punct(Punctuator::LeftBrace) {
            let pot_pre = CaseClauses::parse(parser, after_open, yield_flag, await_flag, return_flag)?;
            let (pre, after_pre) = match pot_pre {
                None => (None, after_open),
                Some((p, s)) => (Some(p), s),
            };
            let pot_def = DefaultClause::parse(parser, after_pre, yield_flag, await_flag, return_flag)?;
            let (def, after_def) = match pot_def {
                None => (None, after_pre),
                Some((d, s)) => (Some(d), s),
            };
            let pot_post = CaseClauses::parse(parser, after_def, yield_flag, await_flag, return_flag)?;
            let (post, after_post) = match pot_post {
                None => (None, after_def),
                Some((p, s)) => (Some(p), s),
            };
            let (close, after_close) = scan_token(&after_post, parser.source, ScanGoal::InputElementDiv);
            if close.matches_punct(Punctuator::RightBrace) {
                match def {
                    None => {
                        assert!(post.is_none());
                        return Ok(Some((Box::new(CaseBlock::NoDefault(pre)), after_close)));
                    }
                    Some(dc) => {
                        return Ok(Some((Box::new(CaseBlock::HasDefault(pre, dc, post)), after_close)));
                    }
                }
            }
        }
        Ok(None)
    }
}

// CaseClauses[Yield, Await, Return] :
//      CaseClause[?Yield, ?Await, ?Return]
//      CaseClauses[?Yield, ?Await, ?Return] CaseClause[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum CaseClauses {
    Item(Box<CaseClause>),
    List(Box<CaseClauses>, Box<CaseClause>),
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
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_item = CaseClause::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        if let Some((item, after_item)) = pot_item {
            let mut current = Box::new(CaseClauses::Item(item));
            let mut current_scanner = after_item;
            loop {
                let pot_next = CaseClause::parse(parser, current_scanner, yield_flag, await_flag, return_flag)?;
                match pot_next {
                    Some((next, after_next)) => {
                        current = Box::new(CaseClauses::List(current, next));
                        current_scanner = after_next;
                    }
                    None => {
                        break;
                    }
                }
            }
            return Ok(Some((current, current_scanner)));
        }
        Ok(None)
    }
}

// CaseClause[Yield, Await, Return] :
//      case Expression[+In, ?Yield, ?Await] : StatementList[?Yield, ?Await, ?Return]opt
#[derive(Debug)]
pub struct CaseClause {
    expression: Box<Expression>,
    statements: Option<Box<StatementList>>,
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
                self.expression
                    .pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
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
        pprint_token(writer, "case", &successive, Spot::NotFinal)?;
        self.expression
            .concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        match &self.statements {
            Some(s) => {
                pprint_token(writer, ":", &successive, Spot::NotFinal)?;
                s.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            None => pprint_token(writer, ":", &successive, Spot::Final),
        }
    }
}

impl CaseClause {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (case_tok, after_case) = scan_token(&scanner, parser.source, ScanGoal::InputElementDiv);
        if case_tok.matches_keyword(Keyword::Case) {
            let pot_exp = Expression::parse(parser, after_case, true, yield_flag, await_flag)?;
            if let Some((exp, after_exp)) = pot_exp {
                let (colon, after_colon) = scan_token(&after_exp, parser.source, ScanGoal::InputElementDiv);
                if colon.matches_punct(Punctuator::Colon) {
                    let pot_stmt = StatementList::parse(parser, after_colon, yield_flag, await_flag, return_flag)?;
                    let (stmt, after_stmt) = match pot_stmt {
                        None => (None, after_colon),
                        Some((stmt, s)) => (Some(stmt), s),
                    };
                    return Ok(Some((
                        Box::new(CaseClause {
                            expression: exp,
                            statements: stmt,
                        }),
                        after_stmt,
                    )));
                }
            }
        }
        Ok(None)
    }
}

// DefaultClause[Yield, Await, Return] :
//      default : StatementList[?Yield, ?Await, ?Return]opt
#[derive(Debug)]
pub struct DefaultClause(Option<Box<StatementList>>);

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
        pprint_token(writer, "default", &successive, Spot::NotFinal)?;
        match self {
            DefaultClause(None) => pprint_token(writer, ":", &successive, Spot::Final),
            DefaultClause(Some(sl)) => {
                pprint_token(writer, ":", &successive, Spot::NotFinal)?;
                sl.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl DefaultClause {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (def_tok, after_def) = scan_token(&scanner, parser.source, ScanGoal::InputElementDiv);
        if def_tok.matches_keyword(Keyword::Default) {
            let (colon, after_colon) = scan_token(&after_def, parser.source, ScanGoal::InputElementDiv);
            if colon.matches_punct(Punctuator::Colon) {
                let pot_sl = StatementList::parse(parser, after_colon, yield_flag, await_flag, return_flag)?;
                let (sl, after_sl) = match pot_sl {
                    None => (None, after_colon),
                    Some((lst, scan)) => (Some(lst), scan),
                };
                return Ok(Some((Box::new(DefaultClause(sl)), after_sl)));
            }
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
