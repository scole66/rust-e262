use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::declarations_and_variables::{BindingElement, BindingRestElement};
use super::scanner::{scan_token, Punctuator, ScanGoal, Scanner, Token};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// UniqueFormalParameters[Yield, Await] :
//      FormalParameters[?Yield, ?Await]
#[derive(Debug)]
pub struct UniqueFormalParameters {
    formals: Rc<FormalParameters>,
}

impl fmt::Display for UniqueFormalParameters {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.formals.fmt(f)
    }
}

impl PrettyPrint for UniqueFormalParameters {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}UniqueFormalParameters: {}", first, self)?;
        self.formals.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.formals.concise_with_leftpad(writer, pad, state)
    }
}

impl UniqueFormalParameters {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> (Rc<Self>, Scanner) {
        let (fp, after_fp) = FormalParameters::parse(parser, scanner, yield_flag, await_flag);
        (Rc::new(UniqueFormalParameters { formals: fp }), after_fp)
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> (Rc<Self>, Scanner) {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.unique_formal_parameters_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.unique_formal_parameters_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.formals.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.formals.all_private_identifiers_valid(names)
    }
}

// FormalParameters[Yield, Await] :
//      [empty]
//      FunctionRestParameter[?Yield, ?Await]
//      FormalParameterList[?Yield, ?Await]
//      FormalParameterList[?Yield, ?Await] ,
//      FormalParameterList[?Yield, ?Await] , FunctionRestParameter[?Yield, ?Await]
#[derive(Debug)]
pub enum FormalParameters {
    Empty,
    Rest(Rc<FunctionRestParameter>),
    List(Rc<FormalParameterList>),
    ListComma(Rc<FormalParameterList>),
    ListRest(Rc<FormalParameterList>, Rc<FunctionRestParameter>),
}

impl fmt::Display for FormalParameters {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FormalParameters::Empty => Ok(()),
            FormalParameters::Rest(node) => node.fmt(f),
            FormalParameters::List(node) => node.fmt(f),
            FormalParameters::ListComma(node) => write!(f, "{} ,", node),
            FormalParameters::ListRest(list, rest) => write!(f, "{} , {}", list, rest),
        }
    }
}

impl PrettyPrint for FormalParameters {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FormalParameters: {}", first, self)?;
        match self {
            FormalParameters::Empty => Ok(()),
            FormalParameters::Rest(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            FormalParameters::List(node) | FormalParameters::ListComma(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            FormalParameters::ListRest(list, rest) => {
                list.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                rest.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let header = |w: &mut T| {
            let (first, successive) = prettypad(pad, state);
            writeln!(w, "{}FormalParameters: {}", first, self).and(Ok(successive))
        };
        match self {
            FormalParameters::Empty => Ok(()),
            FormalParameters::Rest(node) => node.concise_with_leftpad(writer, pad, state),
            FormalParameters::List(node) => node.concise_with_leftpad(writer, pad, state),
            FormalParameters::ListComma(node) => {
                let successive = header(writer)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::Final)
            }
            FormalParameters::ListRest(list, node) => {
                let successive = header(writer)?;
                list.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl FormalParameters {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> (Rc<Self>, Scanner) {
        let pot_fpl = FormalParameterList::parse(parser, scanner, yield_flag, await_flag);
        let (fpl, after_fpl) = match pot_fpl {
            Err(_) => (None, scanner),
            Ok((f, s)) => (Some(f), s),
        };
        let (pot_comma, after_pot) = scan_token(&after_fpl, parser.source, ScanGoal::InputElementDiv);
        let (has_comma, after_comma) = match pot_comma {
            Token::Punctuator(Punctuator::Comma) => (true, after_pot),
            _ => (false, after_fpl),
        };
        let pot_frp = FunctionRestParameter::parse(parser, after_comma, yield_flag, await_flag);
        let (frp, after_frp) = match pot_frp {
            Err(_) => (None, after_comma),
            Ok((f, s)) => (Some(f), s),
        };
        match (fpl, has_comma, frp) {
            (Some(pl), true, Some(rp)) => (Rc::new(FormalParameters::ListRest(pl, rp)), after_frp),
            (Some(pl), true, None) => (Rc::new(FormalParameters::ListComma(pl)), after_comma),
            (Some(pl), false, _) => (Rc::new(FormalParameters::List(pl)), after_fpl),
            (None, false, Some(rp)) => (Rc::new(FormalParameters::Rest(rp)), after_frp),
            (None, false, None) | (None, true, _) => (Rc::new(FormalParameters::Empty), scanner),
        }
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> (Rc<Self>, Scanner) {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.formal_parameters_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.formal_parameters_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            FormalParameters::Empty => false,
            FormalParameters::Rest(node) => node.contains(kind),
            FormalParameters::List(node) => node.contains(kind),
            FormalParameters::ListComma(node) => node.contains(kind),
            FormalParameters::ListRest(list, rest) => list.contains(kind) || rest.contains(kind),
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
            FormalParameters::Empty => true,
            FormalParameters::Rest(node) => node.all_private_identifiers_valid(names),
            FormalParameters::List(node) => node.all_private_identifiers_valid(names),
            FormalParameters::ListComma(node) => node.all_private_identifiers_valid(names),
            FormalParameters::ListRest(list, rest) => list.all_private_identifiers_valid(names) && rest.all_private_identifiers_valid(names),
        }
    }

    pub fn is_simple_parameter_list(&self) -> bool {
        // Static Semantics: IsSimpleParameterList
        match self {
            FormalParameters::Empty => {
                // FormalParameters : [empty]
                //  1. Return true.
                true
            }
            FormalParameters::Rest(_) | FormalParameters::ListRest(..) => {
                // FormalParameters :
                //      FunctionRestParameter
                //      FormalParameterList , FunctionRestParameter
                //  1. Return false.
                false
            }
            FormalParameters::List(formal_parameter_list) | FormalParameters::ListComma(formal_parameter_list) => {
                // FormalParameters :
                //      FormalParameterList
                //      FormalParameterList ,
                //  1. Return IsSimpleParameterList of FormalParameterList
                formal_parameter_list.is_simple_parameter_list()
            }
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        // Static Semantics: BoundNames
        match self {
            FormalParameters::Empty => {
                // FormalParameters : [empty]
                //  1. Return a new empty List.
                vec![]
            }
            FormalParameters::ListRest(formal_parameter_list, function_rest_parameter) => {
                // FormalParameters : FormalParameterList , FunctionRestParameter
                //  1. Let names1 be BoundNames of FormalParameterList.
                //  2. Let names2 be BoundNames of FunctionRestParameter.
                //  3. Return the list-concatenation of names1 and names2.
                let mut names = formal_parameter_list.bound_names();
                let names2 = function_rest_parameter.bound_names();
                names.extend(names2);
                names
            }
            FormalParameters::Rest(function_rest_parameter) => {
                // FormalParameters : FunctionRestParameter
                //  1. Return BoundNames of FunctionRestParameter
                function_rest_parameter.bound_names()
            }
            FormalParameters::List(formal_parameter_list) | FormalParameters::ListComma(formal_parameter_list) => {
                // FormalParameters :
                //      FormalParameterList
                //      FormalParameterList ,
                //  1. Return BoundNames of FormalParameterList
                formal_parameter_list.bound_names()
            }
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent) -> Vec<Object> {
        // todo!()
        println!("{}:{}: Not yet implemented", file!(), line!());
        Vec::new()
    }
}

// FormalParameterList[Yield, Await] :
//      FormalParameter[?Yield, ?Await]
//      FormalParameterList[?Yield, ?Await] , FormalParameter[?Yield, ?Await]
#[derive(Debug)]
pub enum FormalParameterList {
    Item(Rc<FormalParameter>),
    List(Rc<FormalParameterList>, Rc<FormalParameter>),
}

impl fmt::Display for FormalParameterList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FormalParameterList::Item(node) => node.fmt(f),
            FormalParameterList::List(lst, item) => write!(f, "{} , {}", lst, item),
        }
    }
}

impl PrettyPrint for FormalParameterList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FormalParameterList: {}", first, self)?;
        match self {
            FormalParameterList::Item(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            FormalParameterList::List(lst, item) => {
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
            FormalParameterList::Item(node) => node.concise_with_leftpad(writer, pad, state),
            FormalParameterList::List(lst, item) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}FormalParameterList: {}", first, self)?;
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl FormalParameterList {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (fp, after_fp) = FormalParameter::parse(parser, scanner, yield_flag, await_flag)?;
        let mut current = Rc::new(FormalParameterList::Item(fp));
        let mut current_scanner = after_fp;
        while let Ok((next, after_next)) = scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
            .and_then(|after_comma| FormalParameter::parse(parser, after_comma, yield_flag, await_flag))
        {
            current = Rc::new(FormalParameterList::List(current, next));
            current_scanner = after_next;
        }
        Ok((current, current_scanner))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            FormalParameterList::Item(node) => node.contains(kind),
            FormalParameterList::List(lst, item) => lst.contains(kind) || item.contains(kind),
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
            FormalParameterList::Item(node) => node.all_private_identifiers_valid(names),
            FormalParameterList::List(lst, item) => lst.all_private_identifiers_valid(names) && item.all_private_identifiers_valid(names),
        }
    }

    pub fn is_simple_parameter_list(&self) -> bool {
        // Static Semantics: IsSimpleParameterList
        match self {
            FormalParameterList::Item(formal_parameter) => {
                // FormalParameterList : FormalParameter
                //  1. Return IsSimpleParameterList of FormalParameter.
                formal_parameter.is_simple_parameter_list()
            }
            FormalParameterList::List(formal_parameter_list, formal_parameter) => {
                // FormalParameterList : FormalParameterList , FormalParameter
                //  1. If IsSimpleParameterList of FormalParameterList is false, return false.
                //  2. Return IsSimpleParameterList of FormalParameter.
                formal_parameter_list.is_simple_parameter_list() && formal_parameter.is_simple_parameter_list()
            }
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        // Static Semantics: BoundNames
        match self {
            FormalParameterList::Item(formal_parameter) => {
                // FormalParameterList : FormalParameter
                //  1. Return BoundNames of FormalParameter.
                formal_parameter.bound_names()
            }
            FormalParameterList::List(formal_parameter_list, formal_parameter) => {
                // FormalParameterList : FormalParameterList , FormalParameter
                //  1. Let names1 be BoundNames of FormalParameterList.
                //  2. Let names2 be BoundNames of FormalParameter.
                //  3. Return the list-concatenation of names1 and names2.
                let mut names = formal_parameter_list.bound_names();
                let names2 = formal_parameter.bound_names();
                names.extend(names2);
                names
            }
        }
    }
}

// FunctionRestParameter[Yield, Await] :
//      BindingRestElement[?Yield, ?Await]
#[derive(Debug)]
pub struct FunctionRestParameter {
    element: Rc<BindingRestElement>,
}

impl fmt::Display for FunctionRestParameter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.element.fmt(f)
    }
}

impl PrettyPrint for FunctionRestParameter {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionRestParameter: {}", first, self)?;
        self.element.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.element.concise_with_leftpad(writer, pad, state)
    }
}

impl FunctionRestParameter {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (element, after_bre) = BindingRestElement::parse(parser, scanner, yield_flag, await_flag)?;
        Ok((Rc::new(FunctionRestParameter { element }), after_bre))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.element.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.element.all_private_identifiers_valid(names)
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        // Static Semantics: BoundNames
        // FunctionRestParameter : BindingRestElement
        //  1. Return BoundNames of BindingRestElement.
        self.element.bound_names()
    }
}

// FormalParameter[Yield, Await] :
//      BindingElement[?Yield, ?Await]
#[derive(Debug)]
pub struct FormalParameter {
    element: Rc<BindingElement>,
}

impl fmt::Display for FormalParameter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.element.fmt(f)
    }
}

impl PrettyPrint for FormalParameter {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FormalParameter: {}", first, self)?;
        self.element.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.element.concise_with_leftpad(writer, pad, state)
    }
}

impl FormalParameter {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (element, after_be) = BindingElement::parse(parser, scanner, yield_flag, await_flag)?;
        Ok((Rc::new(FormalParameter { element }), after_be))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.formal_parameter_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.formal_parameter_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.element.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.element.all_private_identifiers_valid(names)
    }

    pub fn is_simple_parameter_list(&self) -> bool {
        // Static Semantics: IsSimpleParameterList
        // FormalParameter : BindingElement
        //  1. Return IsSimpleParameterList of BindingElement.
        self.element.is_simple_parameter_list()
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        // Static Semantics: BoundNames
        // FormalParameter : BindingElement
        //  1. Return BoundNames of BindingElement.
        self.element.bound_names()
    }
}

#[cfg(test)]
mod tests;
