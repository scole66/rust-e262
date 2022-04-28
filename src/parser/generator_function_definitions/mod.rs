use super::assignment_operators::AssignmentExpression;
use super::class_definitions::ClassElementName;
use super::function_definitions::{function_early_errors, FunctionBody};
use super::identifiers::BindingIdentifier;
use super::parameter_lists::{FormalParameters, UniqueFormalParameters};
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

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

    pub fn private_bound_identifier(&self) -> Option<JSString> {
        // Static Semantics: PrivateBoundIdentifiers
        // GeneratorMethod : * ClassElementName ( UniqueFormalParameters ) { GeneratorBody }
        //  1. Return PrivateBoundIdentifiers of ClassElementName.
        self.name.private_bound_identifier()
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

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. Return ContainsArguments of ClassElementName.
        self.name.contains_arguments()
    }

    pub fn has_direct_super(&self) -> bool {
        // Static Semantics: HasDirectSuper
        //      The syntax-directed operation HasDirectSuper takes no arguments.
        // GeneratorMethod : * ClassElementName ( UniqueFormalParameters ) { GeneratorBody }
        //  1. If UniqueFormalParameters Contains SuperCall is true, return true.
        //  2. Return GeneratorBody Contains SuperCall.
        self.params.contains(ParseNodeKind::SuperCall) || self.body.contains(ParseNodeKind::SuperCall)
    }

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Generator Function Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-generator-function-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        //  GeneratorMethod : * ClassElementName ( UniqueFormalParameters ) { GeneratorBody }
        //  * It is a Syntax Error if HasDirectSuper of GeneratorMethod is true.
        //  * It is a Syntax Error if UniqueFormalParameters Contains YieldExpression is true.
        //  * It is a Syntax Error if FunctionBodyContainsUseStrict of GeneratorBody is true and IsSimpleParameterList
        //    of UniqueFormalParameters is false.
        //  * It is a Syntax Error if any element of the BoundNames of UniqueFormalParameters also occurs in the
        //    LexicallyDeclaredNames of GeneratorBody.
        let cus = self.body.function_body_contains_use_strict();
        if self.has_direct_super() {
            errs.push(create_syntax_error_object(agent, "Calls to ‘super’ not allowed here"));
        }
        if self.params.contains(ParseNodeKind::YieldExpression) {
            errs.push(create_syntax_error_object(agent, "Yield expressions can't be parameter initializers in generators"));
        }
        if cus && !self.params.is_simple_parameter_list() {
            errs.push(create_syntax_error_object(agent, "Illegal 'use strict' directive in function with non-simple parameter list"));
        }
        let bn = self.params.bound_names();
        for name in self.body.lexically_declared_names().into_iter().filter(|ldn| bn.contains(ldn)) {
            errs.push(create_syntax_error_object(agent, format!("‘{name}’ already defined")));
        }

        let strict_func = strict || cus;

        self.name.early_errors(agent, errs, strict_func);
        self.params.early_errors(agent, errs, strict_func);
        self.body.early_errors(agent, errs, strict_func);
    }

    pub fn prop_name(&self) -> Option<JSString> {
        // Static Semantics: PropName
        // The syntax-directed operation PropName takes no arguments and returns a String or empty.
        //      GeneratorMethod : * ClassElementName ( UniqueFormalParameters ) { GeneratorBody }
        //  1. Return PropName of ClassElementName.
        self.name.prop_name()
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

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Generator Function Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-generator-function-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        //  GeneratorDeclaration :
        //      function * BindingIdentifier ( FormalParameters ) { GeneratorBody }
        //      function * ( FormalParameters ) { GeneratorBody }
        //  * If the source text matched by FormalParameters is strict mode code, the Early Error rules for
        //    UniqueFormalParameters : FormalParameters are applied.
        //  * If BindingIdentifier is present and the source text matched by BindingIdentifier is strict mode code, it
        //    is a Syntax Error if the StringValue of BindingIdentifier is "eval" or "arguments".
        //  * It is a Syntax Error if FunctionBodyContainsUseStrict of GeneratorBody is true and IsSimpleParameterList
        //    of FormalParameters is false.
        //  * It is a Syntax Error if any element of the BoundNames of FormalParameters also occurs in the
        //    LexicallyDeclaredNames of GeneratorBody.
        //  * It is a Syntax Error if FormalParameters Contains YieldExpression is true.
        //  * It is a Syntax Error if FormalParameters Contains SuperProperty is true.
        //  * It is a Syntax Error if GeneratorBody Contains SuperProperty is true.
        //  * It is a Syntax Error if FormalParameters Contains SuperCall is true.
        //  * It is a Syntax Error if GeneratorBody Contains SuperCall is true.
        if self.params.contains(ParseNodeKind::YieldExpression) {
            errs.push(create_syntax_error_object(agent, "Yield expressions can't be parameter initializers in generators"));
        }
        function_early_errors(agent, errs, strict, self.ident.as_ref(), &self.params, &self.body.0);
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

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Generator Function Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-generator-function-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        //  GeneratorExpression :
        //      function * BindingIdentifier[opt] ( FormalParameters ) { GeneratorBody }
        //  * If the source text matched by FormalParameters is strict mode code, the Early Error rules for
        //    UniqueFormalParameters : FormalParameters are applied.
        //  * If BindingIdentifier is present and the source text matched by BindingIdentifier is strict mode code, it
        //    is a Syntax Error if the StringValue of BindingIdentifier is "eval" or "arguments".
        //  * It is a Syntax Error if FunctionBodyContainsUseStrict of GeneratorBody is true and IsSimpleParameterList
        //    of FormalParameters is false.
        //  * It is a Syntax Error if any element of the BoundNames of FormalParameters also occurs in the
        //    LexicallyDeclaredNames of GeneratorBody.
        //  * It is a Syntax Error if FormalParameters Contains YieldExpression is true.
        //  * It is a Syntax Error if FormalParameters Contains SuperProperty is true.
        //  * It is a Syntax Error if GeneratorBody Contains SuperProperty is true.
        //  * It is a Syntax Error if FormalParameters Contains SuperCall is true.
        //  * It is a Syntax Error if GeneratorBody Contains SuperCall is true.
        if self.params.contains(ParseNodeKind::YieldExpression) {
            errs.push(create_syntax_error_object(agent, "Yield expressions can't be parameter initializers in generators"));
        }
        function_early_errors(agent, errs, strict, self.ident.as_ref(), &self.params, &self.body.0);
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

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Generator Function Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-generator-function-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        self.0.early_errors(agent, errs, strict);
    }

    pub fn function_body_contains_use_strict(&self) -> bool {
        self.0.function_body_contains_use_strict()
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        self.0.lexically_declared_names()
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
        kind == ParseNodeKind::YieldExpression
            || match self {
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

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        match self {
            YieldExpression::Simple => false,
            YieldExpression::Expression(ae) | YieldExpression::From(ae) => ae.contains_arguments(),
        }
    }

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Generator Function Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-generator-function-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            YieldExpression::Expression(ae) | YieldExpression::From(ae) => ae.early_errors(agent, errs, strict),
            YieldExpression::Simple => (),
        }
    }
}

#[cfg(test)]
mod tests;
