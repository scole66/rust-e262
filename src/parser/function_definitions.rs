use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::block::StatementList;
use super::identifiers::BindingIdentifier;
use super::parameter_lists::FormalParameters;
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

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
        pprint_token(writer, "function", &successive, Spot::NotFinal)?;
        if let Some(id) = &self.ident {
            id.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, "(", &successive, Spot::NotFinal)?;
        self.params.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", &successive, Spot::NotFinal)?;
        pprint_token(writer, "{", &successive, Spot::NotFinal)?;
        self.body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "}", &successive, Spot::Final)
    }
}

impl FunctionDeclaration {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        default_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok_func, after_func) =
            scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if matches!(tok_func, scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Function)) {
            let pot_bi = BindingIdentifier::parse(parser, after_func, false, false)?;
            let (bi, after_bi) = match pot_bi {
                None => (None, after_func),
                Some((b, s)) => (Some(b), s),
            };
            if !default_flag && bi.is_none() {
                return Ok(None);
            }
            let (popen, after_popen) =
                scanner::scan_token(&after_bi, parser.source, scanner::ScanGoal::InputElementDiv);
            if popen == scanner::Token::LeftParen {
                let pot_fp = FormalParameters::parse(parser, after_popen, false, false)?;
                if let Some((fp, after_fp)) = pot_fp {
                    let (pclose, after_pclose) =
                        scanner::scan_token(&after_fp, parser.source, scanner::ScanGoal::InputElementDiv);
                    if pclose == scanner::Token::RightParen {
                        let (bopen, after_bopen) =
                            scanner::scan_token(&after_pclose, parser.source, scanner::ScanGoal::InputElementDiv);
                        if bopen == scanner::Token::LeftBrace {
                            let pot_fb = FunctionBody::parse(parser, after_bopen, false, false)?;
                            if let Some((fb, after_fb)) = pot_fb {
                                let (bclose, after_bclose) =
                                    scanner::scan_token(&after_fb, parser.source, scanner::ScanGoal::InputElementDiv);
                                if bclose == scanner::Token::RightBrace {
                                    return Ok(Some((
                                        Box::new(FunctionDeclaration {
                                            ident: bi,
                                            params: fp,
                                            body: fb,
                                        }),
                                        after_bclose,
                                    )));
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(None)
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
        pprint_token(writer, "function", &successive, Spot::NotFinal)?;
        if let Some(id) = &self.ident {
            id.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, "(", &successive, Spot::NotFinal)?;
        self.params.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", &successive, Spot::NotFinal)?;
        pprint_token(writer, "{", &successive, Spot::NotFinal)?;
        self.body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "}", &successive, Spot::Final)
    }
}

impl IsFunctionDefinition for FunctionExpression {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl FunctionExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok_func, after_func) =
            scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if matches!(tok_func, scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Function)) {
            let pot_bi = BindingIdentifier::parse(parser, after_func, false, false)?;
            let (bi, after_bi) = match pot_bi {
                None => (None, after_func),
                Some((b, s)) => (Some(b), s),
            };
            let (popen, after_popen) =
                scanner::scan_token(&after_bi, parser.source, scanner::ScanGoal::InputElementDiv);
            if popen == scanner::Token::LeftParen {
                let pot_fp = FormalParameters::parse(parser, after_popen, false, false)?;
                if let Some((fp, after_fp)) = pot_fp {
                    let (pclose, after_pclose) =
                        scanner::scan_token(&after_fp, parser.source, scanner::ScanGoal::InputElementDiv);
                    if pclose == scanner::Token::RightParen {
                        let (bopen, after_bopen) =
                            scanner::scan_token(&after_pclose, parser.source, scanner::ScanGoal::InputElementDiv);
                        if bopen == scanner::Token::LeftBrace {
                            let pot_fb = FunctionBody::parse(parser, after_bopen, false, false)?;
                            if let Some((fb, after_fb)) = pot_fb {
                                let (bclose, after_bclose) =
                                    scanner::scan_token(&after_fb, parser.source, scanner::ScanGoal::InputElementDiv);
                                if bclose == scanner::Token::RightBrace {
                                    return Ok(Some((
                                        Box::new(FunctionExpression {
                                            ident: bi,
                                            params: fp,
                                            body: fb,
                                        }),
                                        after_bclose,
                                    )));
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(None)
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
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_fsl = FunctionStatementList::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((fsl, after_fsl)) = pot_fsl {
            return Ok(Some((Box::new(FunctionBody { statements: fsl }), after_fsl)));
        }
        Ok(None)
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
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_stmts = StatementList::parse(parser, scanner, yield_flag, await_flag, true)?;
        let (stmts, after_stmts) = match pot_stmts {
            None => (None, scanner),
            Some((st, s)) => (Some(st), s),
        };
        Ok(Some((
            Box::new(FunctionStatementList { statements: stmts }),
            after_stmts,
        )))
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}
