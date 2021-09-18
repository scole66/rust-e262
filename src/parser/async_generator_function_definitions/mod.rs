use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::class_definitions::ClassElementName;
use super::function_definitions::FunctionBody;
use super::identifiers::BindingIdentifier;
use super::parameter_lists::{FormalParameters, UniqueFormalParameters};
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// AsyncGeneratorMethod[Yield, Await] :
//      async [no LineTerminator here] * ClassElementName[?Yield, ?Await] ( UniqueFormalParameters[+Yield, +Await] ) { AsyncGeneratorBody }
#[derive(Debug)]
pub struct AsyncGeneratorMethod {
    name: Rc<ClassElementName>,
    params: Rc<UniqueFormalParameters>,
    body: Rc<AsyncGeneratorBody>,
}

impl fmt::Display for AsyncGeneratorMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "async * {} ( {} ) {{ {} }}", self.name, self.params, self.body)
    }
}

impl PrettyPrint for AsyncGeneratorMethod {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncGeneratorMethod: {}", first, self)?;
        self.name.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.body.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncGeneratorMethod: {}", first, self)?;
        pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "*", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.name.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.params.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl AsyncGeneratorMethod {
    // AsyncGeneratorMethod: No caching needed. Parent: MethodDefinition
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_async = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        let after_star = scan_for_punct(after_async, parser.source, ScanGoal::InputElementDiv, Punctuator::Star)?;
        let (name, after_name) = ClassElementName::parse(parser, after_star, yield_flag, await_flag)?;
        let after_lp = scan_for_punct(after_name, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_params) = UniqueFormalParameters::parse(parser, after_lp, true, true);
        let after_rp = scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = AsyncGeneratorBody::parse(parser, after_lb);
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(AsyncGeneratorMethod { name, params, body }), after_rb))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.name.contains(kind) || self.params.contains(kind) || self.body.contains(kind)
    }

    pub fn computed_property_contains(&self, kind: ParseNodeKind) -> bool {
        self.name.computed_property_contains(kind)
    }

    pub fn private_bound_identifiers(&self) -> Vec<JSString> {
        // Static Semantics: PrivateBoundIdentifiers
        // AsyncGeneratorMethod : async ClassElementName ( UniqueFormalParameters ) { AsyncGeneratorBody }
        //  1. Return PrivateBoundIdentifiers of ClassElementName.
        self.name.private_bound_identifiers()
    }
    
    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.name.all_private_identifiers_valid(names) && self.params.all_private_identifiers_valid(names) && self.body.all_private_identifiers_valid(names)
    }
}

// AsyncGeneratorDeclaration[Yield, Await, Default] :
//      async [no LineTerminator here] function * BindingIdentifier[?Yield, ?Await] ( FormalParameters[+Yield, +Await] ) { AsyncGeneratorBody }
//      [+Default] async [no LineTerminator here] function * ( FormalParameters[+Yield, +Await] ) { AsyncGeneratorBody }
#[derive(Debug)]
pub struct AsyncGeneratorDeclaration {
    ident: Option<Rc<BindingIdentifier>>,
    params: Rc<FormalParameters>,
    body: Rc<AsyncGeneratorBody>,
}

impl fmt::Display for AsyncGeneratorDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            None => write!(f, "async function * ( {} ) {{ {} }}", self.params, self.body),
            Some(id) => write!(f, "async function * {} ( {} ) {{ {} }}", id, self.params, self.body),
        }
    }
}

impl PrettyPrint for AsyncGeneratorDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncGeneratorDeclaration: {}", first, self)?;
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
        writeln!(writer, "{}AsyncGeneratorDeclaration: {}", first, self)?;
        pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "function", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "*", TokenType::Punctuator, &successive, Spot::NotFinal)?;
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

impl IsFunctionDefinition for AsyncGeneratorDeclaration {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl AsyncGeneratorDeclaration {
    // No caching needed. Parent: HoistableDeclaration
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, default_flag: bool) -> ParseResult<Self> {
        let after_async = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let after_func = scan_for_keyword(after_async, parser.source, ScanGoal::InputElementRegExp, Keyword::Function)?;
        let after_star = scan_for_punct(after_func, parser.source, ScanGoal::InputElementDiv, Punctuator::Star)?;
        let (ident, after_bi) = match BindingIdentifier::parse(parser, after_star, yield_flag, await_flag) {
            Err(err) => {
                if default_flag {
                    Ok((None, after_star))
                } else {
                    Err(err)
                }
            }
            Ok((node, scan)) => Ok((Some(node), scan)),
        }?;
        let after_lp = scan_for_punct(after_bi, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_fp) = FormalParameters::parse(parser, after_lp, true, false);
        let after_rp = scan_for_punct(after_fp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = AsyncGeneratorBody::parse(parser, after_lb);
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(AsyncGeneratorDeclaration { ident, params, body }), after_rb))
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match &self.ident {
            None => vec![JSString::from("*default*")],
            Some(node) => node.bound_names(),
        }
    }

    pub fn contains(&self, _kind: ParseNodeKind) -> bool {
        false
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.ident.as_ref().map_or(true, |n| n.all_private_identifiers_valid(names)) && self.params.all_private_identifiers_valid(names) && self.body.all_private_identifiers_valid(names)
    }
}

// AsyncGeneratorExpression :
//      async [no LineTerminator here] function * BindingIdentifier[+Yield, +Await]opt ( FormalParameters[+Yield, +Await] ) { AsyncGeneratorBody }
#[derive(Debug)]
pub struct AsyncGeneratorExpression {
    ident: Option<Rc<BindingIdentifier>>,
    params: Rc<FormalParameters>,
    body: Rc<AsyncGeneratorBody>,
}

impl fmt::Display for AsyncGeneratorExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            Some(id) => write!(f, "async function * {} ( {} ) {{ {} }}", id, self.params, self.body),
            None => write!(f, "async function * ( {} ) {{ {} }}", self.params, self.body),
        }
    }
}

impl PrettyPrint for AsyncGeneratorExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncGeneratorExpression: {}", first, self)?;
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
        writeln!(writer, "{}AsyncGeneratorExpression: {}", first, self)?;
        pprint_token(writer, "async", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "function", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "*", TokenType::Punctuator, &successive, Spot::NotFinal)?;
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

impl IsFunctionDefinition for AsyncGeneratorExpression {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl AsyncGeneratorExpression {
    // No caching needed. Parent: PrimaryExpression.
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let after_async = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Async)?;
        no_line_terminator(after_async, parser.source)?;
        let after_func = scan_for_keyword(after_async, parser.source, ScanGoal::InputElementDiv, Keyword::Function)?;
        let after_star = scan_for_punct(after_func, parser.source, ScanGoal::InputElementDiv, Punctuator::Star)?;
        let (ident, after_ident) = match BindingIdentifier::parse(parser, after_star, true, true) {
            Err(_) => (None, after_star),
            Ok((node, scan)) => (Some(node), scan),
        };
        let after_lp = scan_for_punct(after_ident, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_params) = FormalParameters::parse(parser, after_lp, true, true);
        let after_rp = scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = AsyncGeneratorBody::parse(parser, after_lb);
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(AsyncGeneratorExpression { ident, params, body }), after_rb))
    }

    pub fn contains(&self, _: ParseNodeKind) -> bool {
        false
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.ident.as_ref().map_or(true, |n| n.all_private_identifiers_valid(names)) && self.params.all_private_identifiers_valid(names) && self.body.all_private_identifiers_valid(names)
    }
}

// AsyncGeneratorBody :
//      FunctionBody[+Yield, +Await]
#[derive(Debug)]
pub struct AsyncGeneratorBody(Rc<FunctionBody>);

impl fmt::Display for AsyncGeneratorBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PrettyPrint for AsyncGeneratorBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AsyncGeneratorBody: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.0.concise_with_leftpad(writer, pad, state)
    }
}

impl AsyncGeneratorBody {
    fn parse_core(parser: &mut Parser, scanner: Scanner) -> (Rc<Self>, Scanner) {
        let (body, after_body) = FunctionBody::parse(parser, scanner, true, true);
        (Rc::new(AsyncGeneratorBody(body)), after_body)
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner) -> (Rc<Self>, Scanner) {
        match parser.async_generator_body_cache.get(&scanner) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner);
                parser.async_generator_body_cache.insert(scanner, result.clone());
                result
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.0.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.0.all_private_identifiers_valid(names)
    }
}

#[cfg(test)]
mod tests;
