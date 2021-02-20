use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::block::StatementList;
use super::identifiers::BindingIdentifier;
use super::parameter_lists::FormalParameters;
use super::scanner::{Keyword, Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// FunctionDeclaration[Yield, Await, Default] :
//      function BindingIdentifier[?Yield, ?Await] ( FormalParameters[~Yield, ~Await] ) { FunctionBody[~Yield, ~Await] }
//      [+Default] function ( FormalParameters[~Yield, ~Await] ) { FunctionBody[~Yield, ~Await] }
#[derive(Debug)]
pub struct FunctionDeclaration {
    ident: Option<Box<BindingIdentifier>>,
    params: Box<FormalParameters>,
    body: Box<FunctionBody>,
}

impl fmt::Display for FunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            None => write!(f, "function ( {} ) {{ {} }}", self.params, self.body),
            Some(id) => write!(f, "function {} ( {} ) {{ {} }}", id, self.params, self.body),
        }
    }
}

impl PrettyPrint for FunctionDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionDeclaration: {}", first, self)?;
        if let Some(id) = &self.ident {
            id.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        self.params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.body.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionDeclaration: {}", first, self)?;
        pprint_token(writer, "function", TokenType::Keyword, &successive, Spot::NotFinal)?;
        if let Some(id) = &self.ident {
            id.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.params.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl FunctionDeclaration {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, default_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_func = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Function)?;
        let (bi, after_bi) = match BindingIdentifier::parse(parser, after_func, yield_flag, await_flag) {
            Ok((node, scan)) => Ok((Some(node), scan)),
            Err(e) => {
                if default_flag {
                    Ok((None, after_func))
                } else {
                    Err(e)
                }
            }
        }?;
        let after_lp = scan_for_punct(after_bi, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (fp, after_fp) = FormalParameters::parse(parser, after_lp, false, false)?;
        let after_rp = scan_for_punct(after_fp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (fb, after_fb) = FunctionBody::parse(parser, after_lb, false, false)?;
        let after_rb = scan_for_punct(after_fb, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Box::new(FunctionDeclaration { ident: bi, params: fp, body: fb }), after_rb))
    }
}

// FunctionExpression :
//      function BindingIdentifier[~Yield, ~Await]opt ( FormalParameters[~Yield, ~Await] ) { FunctionBody[~Yield, ~Await] }
#[derive(Debug)]
pub struct FunctionExpression {
    ident: Option<Box<BindingIdentifier>>,
    params: Box<FormalParameters>,
    body: Box<FunctionBody>,
}

impl fmt::Display for FunctionExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            None => write!(f, "function ( {} ) {{ {} }}", self.params, self.body),
            Some(id) => write!(f, "function {} ( {} ) {{ {} }}", id, self.params, self.body),
        }
    }
}

impl PrettyPrint for FunctionExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionExpression: {}", first, self)?;
        if let Some(id) = &self.ident {
            id.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        self.params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.body.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionExpression: {}", first, self)?;
        pprint_token(writer, "function", TokenType::Keyword, &successive, Spot::NotFinal)?;
        if let Some(id) = &self.ident {
            id.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.params.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl IsFunctionDefinition for FunctionExpression {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl FunctionExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_func = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Function)?;
        let (bi, after_bi) = match BindingIdentifier::parse(parser, after_func, false, false) {
            Ok((node, scan)) => (Some(node), scan),
            Err(_) => (None, after_func),
        };
        let after_lp = scan_for_punct(after_bi, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (fp, after_fp) = FormalParameters::parse(parser, after_lp, false, false)?;
        let after_rp = scan_for_punct(after_fp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (fb, after_fb) = FunctionBody::parse(parser, after_lb, false, false)?;
        let after_rb = scan_for_punct(after_fb, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Box::new(FunctionExpression { ident: bi, params: fp, body: fb }), after_rb))
    }
}

// FunctionBody[Yield, Await] :
//      FunctionStatementList[?Yield, ?Await]
#[derive(Debug)]
pub struct FunctionBody {
    statements: Box<FunctionStatementList>,
}

impl fmt::Display for FunctionBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.statements.fmt(f)
    }
}

impl PrettyPrint for FunctionBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionBody: {}", first, self)?;
        self.statements.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.statements.concise_with_leftpad(writer, pad, state)
    }
}

impl FunctionBody {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (fsl, after_fsl) = FunctionStatementList::parse(parser, scanner, yield_flag, await_flag)?;
        Ok((Box::new(FunctionBody { statements: fsl }), after_fsl))
    }
}

// FunctionStatementList[Yield, Await] :
//      StatementList[?Yield, ?Await, +Return]opt
#[derive(Debug)]
pub struct FunctionStatementList {
    statements: Option<Box<StatementList>>,
}

impl fmt::Display for FunctionStatementList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.statements {
            None => Ok(()),
            Some(s) => s.fmt(f),
        }
    }
}

impl PrettyPrint for FunctionStatementList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionStatementList: {}", first, self)?;
        match &self.statements {
            None => Ok(()),
            Some(s) => s.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match &self.statements {
            None => Ok(()),
            Some(s) => s.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl FunctionStatementList {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (stmts, after_stmts) = match StatementList::parse(parser, scanner, yield_flag, await_flag, true) {
            Err(_) => (None, scanner),
            Ok((st, s)) => (Some(st), s),
        };
        Ok((Box::new(FunctionStatementList { statements: stmts }), after_stmts))
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}
