use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::class_definitions::ClassElementName;
use super::function_definitions::FunctionBody;
use super::identifiers::BindingIdentifier;
use super::parameter_lists::{FormalParameters, UniqueFormalParameters};
use super::scanner::Scanner;
use super::unary_operators::UnaryExpression;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// AsyncFunctionDeclaration[Yield, Await, Default] :
//      async [no LineTerminator here] function BindingIdentifier[?Yield, ?Await] ( FormalParameters[~Yield, +Await] ) { AsyncFunctionBody }
//      [+Default] async [no LineTerminator here] function ( FormalParameters[~Yield, +Await] ) { AsyncFunctionBody }
#[derive(Debug)]
pub struct AsyncFunctionDeclaration {
    ident: Option<Rc<BindingIdentifier>>,
    params: Rc<FormalParameters>,
    body: Rc<AsyncFunctionBody>,
}

impl fmt::Display for AsyncFunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            None => write!(f, "async function ( {} ) {{ {} }}", self.params, self.body),
            Some(id) => write!(f, "async function {} ( {} ) {{ {} }}", id, self.params, self.body),
        }
    }
}

impl PrettyPrint for AsyncFunctionDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncFunctionDeclaration: {}", first, self)?;
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
        writeln!(writer, "{}AsyncFunctionDeclaration: {}", first, self)?;
        pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
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

impl AsyncFunctionDeclaration {
    // AsyncFunctionDeclaration's only parent is HoistableDeclaration. It doesn't need to be cached.
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, default_flag: bool) -> ParseResult<Self> {
        let after_async = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let after_function = scan_for_keyword(after_async, parser.source, ScanGoal::InputElementDiv, Keyword::Function)?;
        let (ident, after_bi) = match BindingIdentifier::parse(parser, after_function, yield_flag, await_flag) {
            Err(e) => {
                if !default_flag {
                    Err(e)
                } else {
                    Ok((None, after_function))
                }
            }
            Ok((node, scan)) => Ok((Some(node), scan)),
        }?;
        let after_lp = scan_for_punct(after_bi, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_params) = FormalParameters::parse(parser, after_lp, false, true);
        let after_rp = scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = AsyncFunctionBody::parse(parser, after_lb);
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(AsyncFunctionDeclaration { ident, params, body }), after_rb))
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match &self.ident {
            None => vec![JSString::from("*default*")],
            Some(node) => node.bound_names(),
        }
    }

    pub fn contains(&self, _: ParseNodeKind) -> bool {
        false
    }
}

// AsyncFunctionExpression :
//      async [no LineTerminator here] function BindingIdentifier[~Yield, +Await]opt ( FormalParameters[~Yield, +Await] ) { AsyncFunctionBody }
#[derive(Debug)]
pub struct AsyncFunctionExpression {
    ident: Option<Rc<BindingIdentifier>>,
    params: Rc<FormalParameters>,
    body: Rc<AsyncFunctionBody>,
}

impl fmt::Display for AsyncFunctionExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            None => write!(f, "async function ( {} ) {{ {} }}", self.params, self.body),
            Some(id) => write!(f, "async function {} ( {} ) {{ {} }}", id, self.params, self.body),
        }
    }
}

impl PrettyPrint for AsyncFunctionExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncFunctionExpression: {}", first, self)?;
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
        writeln!(writer, "{}AsyncFunctionExpression: {}", first, self)?;
        pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
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

impl IsFunctionDefinition for AsyncFunctionExpression {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl AsyncFunctionExpression {
    // AsyncFunctionExpression's only parent is PrimaryExpression. It doesn't need caching.
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let after_async = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let after_function = scan_for_keyword(after_async, parser.source, ScanGoal::InputElementDiv, Keyword::Function)?;
        let (ident, after_bi) = match BindingIdentifier::parse(parser, after_function, false, true) {
            Err(_) => (None, after_function),
            Ok((node, scan)) => (Some(node), scan),
        };
        let after_lp = scan_for_punct(after_bi, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_params) = FormalParameters::parse(parser, after_lp, false, true);
        let after_rp = scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = AsyncFunctionBody::parse(parser, after_lb);
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(AsyncFunctionExpression { ident, params, body }), after_rb))
    }

    pub fn contains(&self, _: ParseNodeKind) -> bool {
        false
    }
}

// AsyncMethod[Yield, Await] :
//      async [no LineTerminator here] ClassElementName[?Yield, ?Await] ( UniqueFormalParameters[~Yield, +Await] ) { AsyncFunctionBody }
#[derive(Debug)]
pub struct AsyncMethod {
    ident: Rc<ClassElementName>,
    params: Rc<UniqueFormalParameters>,
    body: Rc<AsyncFunctionBody>,
}

impl fmt::Display for AsyncMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "async {} ( {} ) {{ {} }}", self.ident, self.params, self.body)
    }
}

impl PrettyPrint for AsyncMethod {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncMethod: {}", first, self)?;
        self.ident.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.body.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncMethod: {}", first, self)?;
        pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
        self.ident.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.params.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl AsyncMethod {
    // No caching required. Parent: MethodDefinition
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_async = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let (ident, after_ident) = ClassElementName::parse(parser, after_async, yield_flag, await_flag)?;
        let after_lp = scan_for_punct(after_ident, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_params) = UniqueFormalParameters::parse(parser, after_lp, false, true);
        let after_rp = scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = AsyncFunctionBody::parse(parser, after_lb);
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(AsyncMethod { ident, params, body }), after_rb))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.ident.contains(kind) || self.params.contains(kind) || self.body.contains(kind)
    }

    pub fn computed_property_contains(&self, kind: ParseNodeKind) -> bool {
        self.ident.computed_property_contains(kind)
    }
}

// AsyncFunctionBody :
//      FunctionBody[~Yield, +Await]
#[derive(Debug)]
pub struct AsyncFunctionBody(Rc<FunctionBody>);

impl fmt::Display for AsyncFunctionBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PrettyPrint for AsyncFunctionBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncFunctionBody: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.0.concise_with_leftpad(writer, pad, state)
    }
}

impl AsyncFunctionBody {
    fn parse_core(parser: &mut Parser, scanner: Scanner) -> (Rc<Self>, Scanner) {
        let (fb, after_fb) = FunctionBody::parse(parser, scanner, false, true);
        (Rc::new(AsyncFunctionBody(fb)), after_fb)
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner) -> (Rc<Self>, Scanner) {
        match parser.async_function_body_cache.get(&scanner) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner);
                parser.async_function_body_cache.insert(scanner, result.clone());
                result
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.0.contains(kind)
    }
}

// AwaitExpression[Yield] :
//      await UnaryExpression[?Yield, +Await]
#[derive(Debug)]
pub enum AwaitExpression {
    Await(Rc<UnaryExpression>),
}

impl fmt::Display for AwaitExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let AwaitExpression::Await(boxed) = &self;
        write!(f, "await {}", boxed)
    }
}

impl PrettyPrint for AwaitExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AwaitExpression: {}", first, self)?;
        let AwaitExpression::Await(boxed) = &self;
        boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AwaitExpression: {}", first, self)?;
        pprint_token(writer, "await", TokenType::Keyword, &successive, Spot::NotFinal)?;
        let AwaitExpression::Await(ue) = self;
        ue.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl AwaitExpression {
    // No caching required. Parent: UnaryExpression
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool) -> ParseResult<Self> {
        let after_await = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Await)?;
        let (ue, after_ue) = UnaryExpression::parse(parser, after_await, yield_flag, true)?;
        Ok((Rc::new(AwaitExpression::Await(ue)), after_ue))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let AwaitExpression::Await(boxed) = self;
        boxed.contains(kind)
    }
}

#[cfg(test)]
mod tests;
