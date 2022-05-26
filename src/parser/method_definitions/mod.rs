use super::async_function_definitions::AsyncMethod;
use super::async_generator_function_definitions::AsyncGeneratorMethod;
use super::class_definitions::ClassElementName;
use super::function_definitions::FunctionBody;
use super::generator_function_definitions::GeneratorMethod;
use super::parameter_lists::{FormalParameter, UniqueFormalParameters};
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// MethodDefinition[Yield, Await] :
//      ClassElementName[?Yield, ?Await] ( UniqueFormalParameters[~Yield, ~Await] ) { FunctionBody[~Yield, ~Await] }
//      GeneratorMethod[?Yield, ?Await]
//      AsyncMethod[?Yield, ?Await]
//      AsyncGeneratorMethod[?Yield, ?Await]
//      get ClassElementName[?Yield, ?Await] ( ) { FunctionBody[~Yield, ~Await] }
//      set ClassElementName[?Yield, ?Await] ( PropertySetParameterList ) { FunctionBody[~Yield, ~Await] }
#[derive(Debug)]
pub enum MethodDefinition {
    NamedFunction(Rc<ClassElementName>, Rc<UniqueFormalParameters>, Rc<FunctionBody>),
    Generator(Rc<GeneratorMethod>),
    Async(Rc<AsyncMethod>),
    AsyncGenerator(Rc<AsyncGeneratorMethod>),
    Getter(Rc<ClassElementName>, Rc<FunctionBody>),
    Setter(Rc<ClassElementName>, Rc<PropertySetParameterList>, Rc<FunctionBody>),
}

impl fmt::Display for MethodDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MethodDefinition::NamedFunction(name, params, body) => {
                write!(f, "{} ( {} ) {{ {} }}", name, params, body)
            }
            MethodDefinition::Generator(node) => node.fmt(f),
            MethodDefinition::Async(node) => node.fmt(f),
            MethodDefinition::AsyncGenerator(node) => node.fmt(f),
            MethodDefinition::Getter(name, body) => {
                write!(f, "get {} ( ) {{ {} }}", name, body)
            }
            MethodDefinition::Setter(name, args, body) => {
                write!(f, "set {} ( {} ) {{ {} }}", name, args, body)
            }
        }
    }
}

impl PrettyPrint for MethodDefinition {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}MethodDefinition: {}", first, self)?;
        match self {
            MethodDefinition::NamedFunction(name, args, body) => {
                name.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                args.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                body.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            MethodDefinition::Generator(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            MethodDefinition::Async(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            MethodDefinition::AsyncGenerator(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            MethodDefinition::Getter(name, body) => {
                name.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                body.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            MethodDefinition::Setter(name, args, body) => {
                name.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                args.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                body.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut head = |pad, state| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}MethodDefinition: {}", first, self).and(Ok(successive))
        };
        match self {
            MethodDefinition::NamedFunction(name, args, body) => {
                let successive = head(pad, state)?;
                name.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                args.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
            }
            MethodDefinition::Generator(node) => node.concise_with_leftpad(writer, pad, state),
            MethodDefinition::AsyncGenerator(node) => node.concise_with_leftpad(writer, pad, state),
            MethodDefinition::Async(node) => node.concise_with_leftpad(writer, pad, state),
            MethodDefinition::Getter(name, body) => {
                let successive = head(pad, state)?;
                pprint_token(writer, "get", TokenType::Keyword, &successive, Spot::NotFinal)?;
                name.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
            }
            MethodDefinition::Setter(name, args, body) => {
                let successive = head(pad, state)?;
                pprint_token(writer, "set", TokenType::Keyword, &successive, Spot::NotFinal)?;
                name.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                args.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum MethodType {
    Normal,
    Setter,
    Getter,
}

impl MethodDefinition {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::MethodDefinition), scanner))
            .otherwise(|| {
                let after_get = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Get)?;
                let (pn, after_pn) = ClassElementName::parse(parser, after_get, yield_flag, await_flag)?;
                let after_open =
                    scan_for_punct(after_pn, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
                let after_close =
                    scan_for_punct(after_open, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                let after_lb =
                    scan_for_punct(after_close, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
                let (body, after_body) = FunctionBody::parse(parser, after_lb, false, false);
                let after_rb =
                    scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                Ok((Rc::new(MethodDefinition::Getter(pn, body)), after_rb))
            })
            .otherwise(|| {
                let after_set = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Set)?;
                let (pn, after_pn) = ClassElementName::parse(parser, after_set, yield_flag, await_flag)?;
                let after_open =
                    scan_for_punct(after_pn, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
                let (args, after_args) = PropertySetParameterList::parse(parser, after_open)?;
                let after_close =
                    scan_for_punct(after_args, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                let after_lb =
                    scan_for_punct(after_close, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
                let (body, after_body) = FunctionBody::parse(parser, after_lb, false, false);
                let after_rb =
                    scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                Ok((Rc::new(MethodDefinition::Setter(pn, args, body)), after_rb))
            })
            .otherwise(|| {
                AsyncMethod::parse(parser, scanner, yield_flag, await_flag)
                    .map(|(node, scan)| (Rc::new(MethodDefinition::Async(node)), scan))
            })
            .otherwise(|| {
                AsyncGeneratorMethod::parse(parser, scanner, yield_flag, await_flag)
                    .map(|(node, scan)| (Rc::new(MethodDefinition::AsyncGenerator(node)), scan))
            })
            .otherwise(|| {
                GeneratorMethod::parse(parser, scanner, yield_flag, await_flag)
                    .map(|(node, scan)| (Rc::new(MethodDefinition::Generator(node)), scan))
            })
            .otherwise(|| {
                let (name, after_name) = ClassElementName::parse(parser, scanner, yield_flag, await_flag)?;
                let after_lp =
                    scan_for_punct(after_name, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
                let (ufp, after_ufp) = UniqueFormalParameters::parse(parser, after_lp, false, false);
                let after_rp =
                    scan_for_punct(after_ufp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
                let after_lb =
                    scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
                let (body, after_body) = FunctionBody::parse(parser, after_lb, false, false);
                let after_rb =
                    scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                Ok((Rc::new(MethodDefinition::NamedFunction(name, ufp, body)), after_rb))
            })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.method_definition_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.method_definition_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            MethodDefinition::NamedFunction(name, params, body) => {
                name.contains(kind) || params.contains(kind) || body.contains(kind)
            }
            MethodDefinition::Generator(node) => node.contains(kind),
            MethodDefinition::Async(node) => node.contains(kind),
            MethodDefinition::AsyncGenerator(node) => node.contains(kind),
            MethodDefinition::Getter(name, body) => name.contains(kind) || body.contains(kind),
            MethodDefinition::Setter(name, args, body) => {
                name.contains(kind) || args.contains(kind) || body.contains(kind)
            }
        }
    }

    pub fn computed_property_contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            MethodDefinition::NamedFunction(name, ..) => name.computed_property_contains(kind),
            MethodDefinition::Generator(node) => node.computed_property_contains(kind),
            MethodDefinition::Async(node) => node.computed_property_contains(kind),
            MethodDefinition::AsyncGenerator(node) => node.computed_property_contains(kind),
            MethodDefinition::Getter(name, ..) => name.computed_property_contains(kind),
            MethodDefinition::Setter(name, ..) => name.computed_property_contains(kind),
        }
    }

    pub fn private_bound_identifier(&self) -> Option<(JSString, MethodType)> {
        // Static Semantics: PrivateBoundIdentifiers
        match self {
            // MethodDefinition : ClassElementName ( UniqueFormalParameters ) { FunctionBody }
            // MethodDefinition : get ClassElementName ( ) { FunctionBody }
            // MethodDefinition : set ClassElementName ( PropertySetParameterList ) { FunctionBody }
            //  1. Return PrivateBoundIdentifiers of ClassElementName.
            MethodDefinition::NamedFunction(cen, _, _) => {
                cen.private_bound_identifier().map(|s| (s, MethodType::Normal))
            }
            MethodDefinition::Getter(cen, _) => cen.private_bound_identifier().map(|s| (s, MethodType::Getter)),
            MethodDefinition::Setter(cen, _, _) => cen.private_bound_identifier().map(|s| (s, MethodType::Setter)),

            // MethodDefinition : GeneratorMethod
            //  1. Return PrivateBoundIdentifiers of GeneratorMethod.
            MethodDefinition::Generator(node) => node.private_bound_identifier().map(|s| (s, MethodType::Normal)),

            // MethodDefinition : AsyncMethod
            //  1. Return PrivateBoundIdentifiers of AsyncMethod.
            MethodDefinition::Async(node) => node.private_bound_identifier().map(|s| (s, MethodType::Normal)),

            // MethodDefinition : AsyncGeneratorMethod
            //  1. Return PrivateBoundIdentifiers of AsyncGeneratorMethod.
            MethodDefinition::AsyncGenerator(node) => node.private_bound_identifier().map(|s| (s, MethodType::Normal)),
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
            MethodDefinition::NamedFunction(name, params, body) => {
                name.all_private_identifiers_valid(names)
                    && params.all_private_identifiers_valid(names)
                    && body.all_private_identifiers_valid(names)
            }
            MethodDefinition::Generator(node) => node.all_private_identifiers_valid(names),
            MethodDefinition::Async(node) => node.all_private_identifiers_valid(names),
            MethodDefinition::AsyncGenerator(node) => node.all_private_identifiers_valid(names),
            MethodDefinition::Getter(name, body) => {
                name.all_private_identifiers_valid(names) && body.all_private_identifiers_valid(names)
            }
            MethodDefinition::Setter(name, args, body) => {
                name.all_private_identifiers_valid(names)
                    && args.all_private_identifiers_valid(names)
                    && body.all_private_identifiers_valid(names)
            }
        }
    }

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  MethodDefinition :
        //      ClassElementName ( UniqueFormalParameters ) { FunctionBody }
        //      get ClassElementName ( ) { FunctionBody }
        //      set ClassElementName ( PropertySetParameterList ) { FunctionBody }
        //  1. Return ContainsArguments of ClassElementName.
        match self {
            MethodDefinition::NamedFunction(cen, _, _)
            | MethodDefinition::Getter(cen, _)
            | MethodDefinition::Setter(cen, _, _) => cen.contains_arguments(),
            MethodDefinition::Generator(gm) => gm.contains_arguments(),
            MethodDefinition::Async(am) => am.contains_arguments(),
            MethodDefinition::AsyncGenerator(agm) => agm.contains_arguments(),
        }
    }

    pub fn has_direct_super(&self) -> bool {
        // Static Semantics: HasDirectSuper
        // The syntax-directed operation HasDirectSuper takes no arguments.
        match self {
            MethodDefinition::NamedFunction(_, params, body) => {
                // MethodDefinition : ClassElementName ( UniqueFormalParameters ) { FunctionBody }
                //  1. If UniqueFormalParameters Contains SuperCall is true, return true.
                //  2. Return FunctionBody Contains SuperCall.
                params.contains(ParseNodeKind::SuperCall) || body.contains(ParseNodeKind::SuperCall)
            }
            MethodDefinition::Getter(_, body) => {
                // MethodDefinition : get ClassElementName ( ) { FunctionBody }
                //  1. Return FunctionBody Contains SuperCall.
                body.contains(ParseNodeKind::SuperCall)
            }
            MethodDefinition::Setter(_, params, body) => {
                // MethodDefinition : set ClassElementName ( PropertySetParameterList ) { FunctionBody }
                //  1. If PropertySetParameterList Contains SuperCall is true, return true.
                //  2. Return FunctionBody Contains SuperCall.
                params.contains(ParseNodeKind::SuperCall) || body.contains(ParseNodeKind::SuperCall)
            }
            MethodDefinition::Generator(node) => node.has_direct_super(),
            MethodDefinition::Async(node) => node.has_direct_super(),
            MethodDefinition::AsyncGenerator(node) => node.has_direct_super(),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        match self {
            MethodDefinition::NamedFunction(cen, ufp, fb) => {
                //  MethodDefinition : ClassElementName ( UniqueFormalParameters ) { FunctionBody }
                //      * It is a Syntax Error if FunctionBodyContainsUseStrict of FunctionBody is true and
                //        IsSimpleParameterList of UniqueFormalParameters is false.
                //      * It is a Syntax Error if any element of the BoundNames of UniqueFormalParameters also occurs in
                //        the LexicallyDeclaredNames of FunctionBody.
                if fb.function_body_contains_use_strict() && !ufp.is_simple_parameter_list() {
                    errs.push(create_syntax_error_object(
                        agent,
                        "Illegal 'use strict' directive in function with non-simple parameter list",
                    ));
                }
                let ldn = fb.lexically_declared_names();
                for name in ufp.bound_names().into_iter().filter(|n| ldn.contains(n)) {
                    errs.push(create_syntax_error_object(agent, format!("‘{}’ already defined", name)));
                }
                let strict_function = strict || fb.function_body_contains_use_strict();
                cen.early_errors(agent, errs, strict_function);
                ufp.early_errors(agent, errs, strict_function);
                fb.early_errors(agent, errs, strict_function);
            }
            MethodDefinition::Setter(cen, pspl, fb) => {
                //  MethodDefinition : set ClassElementName ( PropertySetParameterList ) { FunctionBody }
                //      * It is a Syntax Error if BoundNames of PropertySetParameterList contains any duplicate
                //        elements.
                //      * It is a Syntax Error if FunctionBodyContainsUseStrict of FunctionBody is true and
                //        IsSimpleParameterList of PropertySetParameterList is false.
                //      * It is a Syntax Error if any element of the BoundNames of PropertySetParameterList also occurs
                //        in the LexicallyDeclaredNames of FunctionBody.
                let bn = pspl.bound_names();
                for name in duplicates(&bn) {
                    errs.push(create_syntax_error_object(agent, format!("‘{}’ already defined", name)));
                }
                if fb.function_body_contains_use_strict() && !pspl.is_simple_parameter_list() {
                    errs.push(create_syntax_error_object(
                        agent,
                        "Illegal 'use strict' directive in function with non-simple parameter list",
                    ));
                }
                let ldn = fb.lexically_declared_names();
                for name in bn.into_iter().filter(|n| ldn.contains(n)) {
                    errs.push(create_syntax_error_object(agent, format!("‘{}’ already defined", name)));
                }
                let strict_function = strict || fb.function_body_contains_use_strict();
                cen.early_errors(agent, errs, strict_function);
                pspl.early_errors(agent, errs, strict_function);
                fb.early_errors(agent, errs, strict_function);
            }
            MethodDefinition::Getter(cen, fb) => {
                let strict_function = strict || fb.function_body_contains_use_strict();
                cen.early_errors(agent, errs, strict_function);
                fb.early_errors(agent, errs, strict_function);
            }
            MethodDefinition::Generator(g) => g.early_errors(agent, errs, strict),
            MethodDefinition::Async(a) => a.early_errors(agent, errs, strict),
            MethodDefinition::AsyncGenerator(ag) => ag.early_errors(agent, errs, strict),
        }
    }

    pub fn prop_name(&self) -> Option<JSString> {
        // Static Semantics: PropName
        // The syntax-directed operation PropName takes no arguments and returns a String or empty.
        match self {
            MethodDefinition::NamedFunction(cen, _, _)
            | MethodDefinition::Getter(cen, _)
            | MethodDefinition::Setter(cen, _, _) => {
                // MethodDefinition :
                //      ClassElementName ( UniqueFormalParameters ) { FunctionBody }
                //      get ClassElementName ( ) { FunctionBody }
                //      set ClassElementName ( PropertySetParameterList ) { FunctionBody }
                //  1. Return PropName of ClassElementName.
                cen.prop_name()
            }
            MethodDefinition::Generator(node) => node.prop_name(),
            MethodDefinition::Async(node) => node.prop_name(),
            MethodDefinition::AsyncGenerator(node) => node.prop_name(),
        }
    }

    /// Determine whether this node is an ordinary method, or a special one.
    ///
    /// "Special" methods are asychronous functions, generators, or property setters or getters.
    ///
    /// See [SpecialMethod](https://tc39.es/ecma262/#sec-static-semantics-specialmethod) in ECMA-262.
    pub fn special_method(&self) -> bool {
        !matches!(self, MethodDefinition::NamedFunction(..))
    }
}

// PropertySetParameterList :
//      FormalParameter[~Yield, ~Await]
#[derive(Debug)]
pub struct PropertySetParameterList {
    node: Rc<FormalParameter>,
}

impl fmt::Display for PropertySetParameterList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.node.fmt(f)
    }
}

impl PrettyPrint for PropertySetParameterList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}PropertySetParameterList: {}", first, self)?;
        self.node.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.node.concise_with_leftpad(writer, pad, state)
    }
}

impl PropertySetParameterList {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        FormalParameter::parse(parser, scanner, false, false)
            .map(|(node, scanner)| (Rc::new(PropertySetParameterList { node }), scanner))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.node.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.node.all_private_identifiers_valid(names)
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        self.node.bound_names()
    }

    pub fn is_simple_parameter_list(&self) -> bool {
        self.node.is_simple_parameter_list()
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        self.node.early_errors(agent, errs, strict);
    }
}

#[cfg(test)]
mod tests;
