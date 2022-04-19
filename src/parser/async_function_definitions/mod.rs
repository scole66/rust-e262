use super::class_definitions::ClassElementName;
use super::function_definitions::FunctionBody;
use super::identifiers::BindingIdentifier;
use super::parameter_lists::{FormalParameters, UniqueFormalParameters};
use super::scanner::Scanner;
use super::unary_operators::UnaryExpression;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};
use ahash::AHashSet;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

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
    /// See [Early Errors for Async Function Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-async-function-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        // AsyncFunctionDeclaration :
        //     async function BindingIdentifier ( FormalParameters ) { AsyncFunctionBody }
        //     async function ( FormalParameters ) { AsyncFunctionBody }
        //
        //  * It is a Syntax Error if FunctionBodyContainsUseStrict of AsyncFunctionBody is true and
        //    IsSimpleParameterList of FormalParameters is false.
        //  * It is a Syntax Error if FormalParameters Contains AwaitExpression is true.
        //  * If the source code matching FormalParameters is strict mode code, the Early Error rules for
        //    UniqueFormalParameters : FormalParameters are applied.
        //  * If BindingIdentifier is present and the source code matching BindingIdentifier is strict mode code, it is
        //    a Syntax Error if the StringValue of BindingIdentifier is "eval" or "arguments". (Superfluous!)
        //  * It is a Syntax Error if any element of the BoundNames of FormalParameters also occurs in the
        //    LexicallyDeclaredNames of AsyncFunctionBody.
        //  * It is a Syntax Error if FormalParameters Contains SuperProperty is true.
        //  * It is a Syntax Error if AsyncFunctionBody Contains SuperProperty is true.
        //  * It is a Syntax Error if FormalParameters Contains SuperCall is true.
        //  * It is a Syntax Error if AsyncFunctionBody Contains SuperCall is true.
        let strict_function = strict || self.body.function_body_contains_use_strict();
        if strict_function && !self.params.is_simple_parameter_list() {
            // FunctionBodyContainsUseStrict of AsyncFunctionBody is true and IsSimpleParameterList of FormalParameters
            // is false
            errs.push(create_syntax_error_object(agent, "Strict functions must also have simple parameter lists"));
        }
        if self.params.contains(ParseNodeKind::AwaitExpression) {
            // FormalParameters Contains AwaitExpression is true.
            errs.push(create_syntax_error_object(agent, "Illegal await-expression in formal parameters of async function"));
        }
        let duplicates_checked = if strict_function {
            // The Early Error rules for UniqueFormalParameters : FormalParameters are applied.
            //      Static Semantics: Early Errors
            //          UniqueFormalParameters : FormalParameters
            //      * It is a Syntax Error if BoundNames of FormalParameters contains any duplicate elements.
            let bn = self.params.bound_names();
            for name in duplicates(&bn) {
                errs.push(create_syntax_error_object(agent, format!("‘{}’ already defined", name)));
            }
            true
        } else {
            false
        };
        // It is a Syntax Error if any element of the BoundNames of FormalParameters also occurs in the
        //    LexicallyDeclaredNames of AsyncFunctionBody
        let bn: AHashSet<JSString> = self.params.bound_names().into_iter().collect();
        let ldn: AHashSet<JSString> = self.body.lexically_declared_names().into_iter().collect();
        if !bn.is_disjoint(&ldn) {
            errs.push(create_syntax_error_object(agent, "Lexical decls in body duplicate parameters"));
        }
        // It is a Syntax Error if FormalParameters Contains SuperProperty is true.
        if self.params.contains(ParseNodeKind::SuperProperty) {
            errs.push(create_syntax_error_object(agent, "Parameters may not include super properties"));
        }
        // It is a Syntax Error if AsyncFunctionBody Contains SuperProperty is true.
        if self.body.contains(ParseNodeKind::SuperProperty) {
            errs.push(create_syntax_error_object(agent, "Body may not contain super properties"));
        }
        // It is a Syntax Error if FormalParameters Contains SuperCall is true.
        if self.params.contains(ParseNodeKind::SuperCall) {
            errs.push(create_syntax_error_object(agent, "Parameters may not include super calls"));
        }
        // It is a Syntax Error if AsyncFunctionBody Contains SuperCall is true.
        if self.body.contains(ParseNodeKind::SuperCall) {
            errs.push(create_syntax_error_object(agent, "Body may not contain super calls"));
        }

        // All the children
        if let Some(binding_identifier) = &self.ident {
            binding_identifier.early_errors(agent, errs, strict_function);
        }
        self.params.early_errors(agent, errs, strict_function, duplicates_checked);
        self.body.early_errors(agent, errs, strict_function);

        // And done.
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
    /// See [Early Errors for Async Function Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-async-function-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        //  AsyncFunctionExpression :
        //      async function BindingIdentifier[opt] ( FormalParameters ) { AsyncFunctionBody }
        //
        //  * It is a Syntax Error if FunctionBodyContainsUseStrict of AsyncFunctionBody is true and
        //    IsSimpleParameterList of FormalParameters is false.
        //  * It is a Syntax Error if FormalParameters Contains AwaitExpression is true.
        //  * If the source code matching FormalParameters is strict mode code, the Early Error rules for
        //    UniqueFormalParameters : FormalParameters are applied.
        //  * If BindingIdentifier is present and the source code matching BindingIdentifier is strict mode code, it is
        //    a Syntax Error if the StringValue of BindingIdentifier is "eval" or "arguments". (Superfluous!)
        //  * It is a Syntax Error if any element of the BoundNames of FormalParameters also occurs in the
        //    LexicallyDeclaredNames of AsyncFunctionBody.
        //  * It is a Syntax Error if FormalParameters Contains SuperProperty is true.
        //  * It is a Syntax Error if AsyncFunctionBody Contains SuperProperty is true.
        //  * It is a Syntax Error if FormalParameters Contains SuperCall is true.
        //  * It is a Syntax Error if AsyncFunctionBody Contains SuperCall is true.
        let strict_function = strict || self.body.function_body_contains_use_strict();
        if strict_function && !self.params.is_simple_parameter_list() {
            // FunctionBodyContainsUseStrict of AsyncFunctionBody is true and IsSimpleParameterList of FormalParameters
            // is false
            errs.push(create_syntax_error_object(agent, "Strict functions must also have simple parameter lists"));
        }
        if self.params.contains(ParseNodeKind::AwaitExpression) {
            // FormalParameters Contains AwaitExpression is true.
            errs.push(create_syntax_error_object(agent, "Illegal await-expression in formal parameters of async function"));
        }
        let duplicates_checked = if strict_function {
            // The Early Error rules for UniqueFormalParameters : FormalParameters are applied.
            //      Static Semantics: Early Errors
            //          UniqueFormalParameters : FormalParameters
            //      * It is a Syntax Error if BoundNames of FormalParameters contains any duplicate elements.
            let bn = self.params.bound_names();
            for name in duplicates(&bn) {
                errs.push(create_syntax_error_object(agent, format!("‘{}’ already defined", name)));
            }
            true
        } else {
            false
        };
        // It is a Syntax Error if any element of the BoundNames of FormalParameters also occurs in the
        //    LexicallyDeclaredNames of AsyncFunctionBody
        let bn: AHashSet<JSString> = self.params.bound_names().into_iter().collect();
        let ldn: AHashSet<JSString> = self.body.lexically_declared_names().into_iter().collect();
        if !bn.is_disjoint(&ldn) {
            errs.push(create_syntax_error_object(agent, "Lexical decls in body duplicate parameters"));
        }
        // It is a Syntax Error if FormalParameters Contains SuperProperty is true.
        if self.params.contains(ParseNodeKind::SuperProperty) {
            errs.push(create_syntax_error_object(agent, "Parameters may not include super properties"));
        }
        // It is a Syntax Error if AsyncFunctionBody Contains SuperProperty is true.
        if self.body.contains(ParseNodeKind::SuperProperty) {
            errs.push(create_syntax_error_object(agent, "Body may not contain super properties"));
        }
        // It is a Syntax Error if FormalParameters Contains SuperCall is true.
        if self.params.contains(ParseNodeKind::SuperCall) {
            errs.push(create_syntax_error_object(agent, "Parameters may not include super calls"));
        }
        // It is a Syntax Error if AsyncFunctionBody Contains SuperCall is true.
        if self.body.contains(ParseNodeKind::SuperCall) {
            errs.push(create_syntax_error_object(agent, "Body may not contain super calls"));
        }

        // All the children
        if let Some(binding_identifier) = &self.ident {
            binding_identifier.early_errors(agent, errs, strict_function);
        }
        self.params.early_errors(agent, errs, strict_function, duplicates_checked);
        self.body.early_errors(agent, errs, strict_function);
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

    pub fn private_bound_identifier(&self) -> Option<JSString> {
        // Static Semantics: PrivateBoundIdentifiers
        // AsyncMethod : async ClassElementName ( UniqueFormalParameters ) { AsyncFunctionBody }
        //  1. Return PrivateBoundIdentifiers of ClassElementName.
        self.ident.private_bound_identifier()
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.ident.all_private_identifiers_valid(names) && self.params.all_private_identifiers_valid(names) && self.body.all_private_identifiers_valid(names)
    }

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. Return ContainsArguments of ClassElementName.
        self.ident.contains_arguments()
    }

    pub fn has_direct_super(&self) -> bool {
        // Static Semantics: HasDirectSuper
        //      The syntax-directed operation HasDirectSuper takes no arguments.
        // AsyncMethod : async ClassElementName ( UniqueFormalParameters ) { AsyncFunctionBody }
        //  1. If UniqueFormalParameters Contains SuperCall is true, return true.
        //  2. Return AsyncFunctionBody Contains SuperCall.
        self.params.contains(ParseNodeKind::SuperCall) || self.body.contains(ParseNodeKind::SuperCall)
    }

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Async Function Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-async-function-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        //  AsyncMethod : async ClassElementName ( UniqueFormalParameters ) { AsyncFunctionBody }
        //  * It is a Syntax Error if FunctionBodyContainsUseStrict of AsyncFunctionBody is true and
        //    IsSimpleParameterList of UniqueFormalParameters is false.
        //  * It is a Syntax Error if HasDirectSuper of AsyncMethod is true.
        //  * It is a Syntax Error if UniqueFormalParameters Contains AwaitExpression is true.
        //  * It is a Syntax Error if any element of the BoundNames of UniqueFormalParameters also occurs in the
        //    LexicallyDeclaredNames of AsyncFunctionBody.
        let cus = self.body.function_body_contains_use_strict();
        if cus && !self.params.is_simple_parameter_list() {
            errs.push(create_syntax_error_object(agent, "Illegal 'use strict' directive in function with non-simple parameter list"));
        }
        if self.has_direct_super() {
            errs.push(create_syntax_error_object(agent, "Calls to ‘super’ not allowed here"));
        }
        if self.params.contains(ParseNodeKind::AwaitExpression) {
            errs.push(create_syntax_error_object(agent, "Illegal await-expression in formal parameters of async function"))
        }
        let bn = self.params.bound_names();
        for name in self.body.lexically_declared_names() {
            if bn.contains(&name) {
                errs.push(create_syntax_error_object(agent, format!("‘{}’ already defined", name)));
            }
        }

        let strict_func = strict || cus;
        self.ident.early_errors(agent, errs, strict_func);
        self.params.early_errors(agent, errs, strict_func);
        self.body.early_errors(agent, errs, strict_func);
    }

    pub fn prop_name(&self) -> Option<JSString> {
        // Static Semantics: PropName
        // The syntax-directed operation PropName takes no arguments and returns a String or empty.
        //      AsyncMethod : async ClassElementName ( UniqueFormalParameters ) { AsyncFunctionBody }
        //  1. Return PropName of ClassElementName.
        self.ident.prop_name()
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

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.0.all_private_identifiers_valid(names)
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
        self.0.contains_arguments()
    }

    pub fn function_body_contains_use_strict(&self) -> bool {
        // Static Semantics: FunctionBodyContainsUseStrict
        // AsyncFunctionBody : FunctionBody
        //  1. Return FunctionBodyContainsUseStrict of FunctionBody.
        self.0.function_body_contains_use_strict()
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        // Static Semantics: LexicallyDeclaredNames
        //      AsyncFunctionBody : FunctionBody
        //  1. Return LexicallyDeclaredNames of FunctionBody.
        self.0.lexically_declared_names()
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        self.0.early_errors(agent, errs, strict);
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
        kind == ParseNodeKind::AwaitExpression || {
            let AwaitExpression::Await(boxed) = self;
            boxed.contains(kind)
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        let AwaitExpression::Await(boxed) = self;
        boxed.all_private_identifiers_valid(names)
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        let AwaitExpression::Await(ue) = self;
        ue.early_errors(agent, errs, strict);
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
        let AwaitExpression::Await(ue) = self;
        ue.contains_arguments()
    }
}

#[cfg(test)]
mod tests;
