use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::assignment_operators::AssignmentExpression;
use super::class_definitions::ClassElementName;
use super::function_definitions::FunctionBody;
use super::identifiers::BindingIdentifier;
use super::parameter_lists::{FormalParameters, UniqueFormalParameters};
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// GeneratorMethod[Yield, Await] :
//      * ClassElementName[?Yield, ?Await] ( UniqueFormalParameters[+Yield, ~Await] ) { GeneratorBody }
#[derive(Debug)]
pub struct GeneratorMethod {
    name: Rc<ClassElementName>,
    params: Rc<UniqueFormalParameters>,
    body: Rc<GeneratorBody>,
}

impl fmt::Display for GeneratorMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "* {} ( {} ) {{ {} }}", self.name, self.params, self.body)
    }
}

impl PrettyPrint for GeneratorMethod {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}GeneratorMethod: {}", first, self)?;
        self.name.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.body.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}GeneratorMethod: {}", first, self)?;
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

impl GeneratorMethod {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_star = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Star)?;
        let (name, after_name) = ClassElementName::parse(parser, after_star, yield_flag, await_flag)?;
        let after_lp = scan_for_punct(after_name, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_params) = UniqueFormalParameters::parse(parser, after_lp, true, false);
        let after_rp = scan_for_punct(after_params, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = GeneratorBody::parse(parser, after_lb);
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(GeneratorMethod { name, params, body }), after_rb))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.name.contains(kind) || self.params.contains(kind) || self.body.contains(kind)
    }

    pub fn computed_property_contains(&self, kind: ParseNodeKind) -> bool {
        self.name.computed_property_contains(kind)
    }

    pub fn private_bound_identifiers(&self) -> Vec<JSString> {
        // Static Semantics: PrivateBoundIdentifiers
        // GeneratorMethod : * ClassElementName ( UniqueFormalParameters ) { GeneratorBody }
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

    pub fn has_direct_super(&self) -> bool {
        // Static Semantics: HasDirectSuper
        //      The syntax-directed operation HasDirectSuper takes no arguments.
        // GeneratorMethod : * ClassElementName ( UniqueFormalParameters ) { GeneratorBody }
        //  1. If UniqueFormalParameters Contains SuperCall is true, return true.
        //  2. Return GeneratorBody Contains SuperCall.
        self.params.contains(ParseNodeKind::SuperCall) || self.body.contains(ParseNodeKind::SuperCall)
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// GeneratorDeclaration[Yield, Await, Default] :
//      function * BindingIdentifier[?Yield, ?Await] ( FormalParameters[+Yield, ~Await] ) { GeneratorBody }
//      [+Default] function * ( FormalParameters[+Yield, ~Await] ) { GeneratorBody }
#[derive(Debug)]
pub struct GeneratorDeclaration {
    ident: Option<Rc<BindingIdentifier>>,
    params: Rc<FormalParameters>,
    body: Rc<GeneratorBody>,
}

impl fmt::Display for GeneratorDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            None => write!(f, "function * ( {} ) {{ {} }}", self.params, self.body),
            Some(id) => write!(f, "function * {} ( {} ) {{ {} }}", id, self.params, self.body),
        }
    }
}

impl PrettyPrint for GeneratorDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}GeneratorDeclaration: {}", first, self)?;
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
        writeln!(writer, "{}GeneratorDeclaration: {}", first, self)?;
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

impl GeneratorDeclaration {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, default_flag: bool) -> ParseResult<Self> {
        let after_func = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Function)?;
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
        let (body, after_body) = GeneratorBody::parse(parser, after_lb);
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(GeneratorDeclaration { ident, params, body }), after_rb))
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
        self.params.all_private_identifiers_valid(names) && self.body.all_private_identifiers_valid(names)
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// GeneratorExpression :
//      function * BindingIdentifier[+Yield, ~Await]opt ( FormalParameters[+Yield, ~Await] ) { GeneratorBody }
#[derive(Debug)]
pub struct GeneratorExpression {
    ident: Option<Rc<BindingIdentifier>>,
    params: Rc<FormalParameters>,
    body: Rc<GeneratorBody>,
}

impl fmt::Display for GeneratorExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            None => write!(f, "function * ( {} ) {{ {} }}", self.params, self.body),
            Some(id) => write!(f, "function * {} ( {} ) {{ {} }}", id, self.params, self.body),
        }
    }
}

impl PrettyPrint for GeneratorExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}GeneratorExpression: {}", first, self)?;
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
        writeln!(writer, "{}GeneratorExpression: {}", first, self)?;
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

impl IsFunctionDefinition for GeneratorExpression {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl GeneratorExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let after_func = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Function)?;
        let after_star = scan_for_punct(after_func, parser.source, ScanGoal::InputElementDiv, Punctuator::Star)?;
        let (ident, after_bi) = match BindingIdentifier::parse(parser, after_star, true, false) {
            Err(_) => (None, after_star),
            Ok((node, scan)) => (Some(node), scan),
        };
        let after_lp = scan_for_punct(after_bi, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_fp) = FormalParameters::parse(parser, after_lp, true, false);
        let after_rp = scan_for_punct(after_fp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let after_lb = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_body) = GeneratorBody::parse(parser, after_lb);
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(GeneratorExpression { ident, params, body }), after_rb))
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
        self.params.all_private_identifiers_valid(names) && self.body.all_private_identifiers_valid(names)
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// GeneratorBody :
//      FunctionBody[+Yield, ~Await]
#[derive(Debug)]
pub struct GeneratorBody(Rc<FunctionBody>);

impl fmt::Display for GeneratorBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PrettyPrint for GeneratorBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}GeneratorBody: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.0.concise_with_leftpad(writer, pad, state)
    }
}

impl GeneratorBody {
    fn parse_core(parser: &mut Parser, scanner: Scanner) -> (Rc<Self>, Scanner) {
        let (fb, after_fb) = FunctionBody::parse(parser, scanner, true, false);
        (Rc::new(GeneratorBody(fb)), after_fb)
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner) -> (Rc<Self>, Scanner) {
        match parser.generator_body_cache.get(&scanner) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner);
                parser.generator_body_cache.insert(scanner, result.clone());
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

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// YieldExpression[In, Await] :
//      yield
//      yield [no LineTerminator here] AssignmentExpression[?In, +Yield, ?Await]
//      yield [no LineTerminator here] * AssignmentExpression[?In, +Yield, ?Await]
#[derive(Debug)]
pub enum YieldExpression {
    Simple,
    Expression(Rc<AssignmentExpression>),
    From(Rc<AssignmentExpression>),
}

impl fmt::Display for YieldExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            YieldExpression::Simple => f.write_str("yield"),
            YieldExpression::Expression(node) => write!(f, "yield {}", node),
            YieldExpression::From(node) => write!(f, "yield * {}", node),
        }
    }
}

impl PrettyPrint for YieldExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}YieldExpression: {}", first, self)?;
        match self {
            YieldExpression::Simple => Ok(()),
            YieldExpression::Expression(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            YieldExpression::From(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let head = |writer: &mut T| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}YieldExpression: {}", first, self).and(Ok(successive))
        };
        match self {
            YieldExpression::Simple => pprint_token(writer, "yield", TokenType::Keyword, pad, state),
            YieldExpression::Expression(node) => {
                let successive = head(writer)?;
                pprint_token(writer, "yield", TokenType::Keyword, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            YieldExpression::From(node) => {
                let successive = head(writer)?;
                pprint_token(writer, "yield", TokenType::Keyword, &successive, Spot::NotFinal)?;
                pprint_token(writer, "*", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl YieldExpression {
    fn parse_after_nlt(parser: &mut Parser, scanner: Scanner, in_flag: bool, await_flag: bool) -> ParseResult<Self> {
        (|| {
            let after_star = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Star)?;
            let (ae, after_ae) = AssignmentExpression::parse(parser, after_star, in_flag, true, await_flag)?;
            Ok((Rc::new(YieldExpression::From(ae)), after_ae))
        })()
        .otherwise(|| {
            let (ae, after_ae) = AssignmentExpression::parse(parser, scanner, in_flag, true, await_flag)?;
            Ok((Rc::new(YieldExpression::Expression(ae)), after_ae))
        })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_yield = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Yield)?;
        no_line_terminator(after_yield, parser.source)
            .and_then(|()| Self::parse_after_nlt(parser, after_yield, in_flag, await_flag))
            .otherwise(|| Ok((Rc::new(YieldExpression::Simple), after_yield)))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            YieldExpression::Simple => false,
            YieldExpression::Expression(node) => node.contains(kind),
            YieldExpression::From(node) => node.contains(kind),
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        match self {
            YieldExpression::Simple => true,
            YieldExpression::Expression(node) => node.all_private_identifiers_valid(names),
            YieldExpression::From(node) => node.all_private_identifiers_valid(names),
        }
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

#[cfg(test)]
mod tests;
