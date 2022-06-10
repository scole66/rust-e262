use super::declarations_and_variables::{BindingElement, BindingRestElement};
use super::scanner::{scan_token, Punctuator, ScanGoal, Scanner, Token};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

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

    pub fn location(&self) -> Location {
        self.formals.location()
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
        self.formals.contains_arguments()
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        //  UniqueFormalParameters : FormalParameters
        //  * It is a Syntax Error if BoundNames of FormalParameters contains any duplicate elements.
        let bn = self.formals.bound_names();
        for name in duplicates(&bn) {
            errs.push(create_syntax_error_object(
                agent,
                format!("‘{}’ already defined", name),
                Some(self.formals.location()),
            ));
        }
        self.formals.early_errors(agent, errs, strict, true);
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        self.formals.bound_names()
    }

    pub fn is_simple_parameter_list(&self) -> bool {
        // Static Semantics: IsSimpleParameterList
        // The syntax-directed operation IsSimpleParameterList takes no arguments and returns a Boolean.
        //  UniqueFormalParameters : FormalParameters
        //      1. Return IsSimpleParameterList of FormalParameters.
        self.formals.is_simple_parameter_list()
    }

    pub fn expected_argument_count(&self) -> f64 {
        self.formals.expected_argument_count()
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
    Empty(Location),
    Rest(Rc<FunctionRestParameter>),
    List(Rc<FormalParameterList>),
    ListComma(Rc<FormalParameterList>, Location),
    ListRest(Rc<FormalParameterList>, Rc<FunctionRestParameter>),
}

impl fmt::Display for FormalParameters {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FormalParameters::Empty(_) => Ok(()),
            FormalParameters::Rest(node) => node.fmt(f),
            FormalParameters::List(node) => node.fmt(f),
            FormalParameters::ListComma(node, _) => write!(f, "{} ,", node),
            FormalParameters::ListRest(list, rest) => {
                write!(f, "{} , {}", list, rest)
            }
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
            FormalParameters::Empty(_) => Ok(()),
            FormalParameters::Rest(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            FormalParameters::List(node) | FormalParameters::ListComma(node, _) => {
                node.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
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
            FormalParameters::Empty(_) => Ok(()),
            FormalParameters::Rest(node) => node.concise_with_leftpad(writer, pad, state),
            FormalParameters::List(node) => node.concise_with_leftpad(writer, pad, state),
            FormalParameters::ListComma(node, _) => {
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
        let (pot_comma, pot_loc, after_pot) = scan_token(&after_fpl, parser.source, ScanGoal::InputElementDiv);
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
            (Some(pl), true, None) => {
                let location = pl.location().merge(&pot_loc);
                (Rc::new(FormalParameters::ListComma(pl, location)), after_comma)
            }
            (Some(pl), false, _) => (Rc::new(FormalParameters::List(pl)), after_fpl),
            (None, false, Some(rp)) => (Rc::new(FormalParameters::Rest(rp)), after_frp),
            (None, false, None) | (None, true, _) => {
                (Rc::new(FormalParameters::Empty(Location::from(scanner))), scanner)
            }
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

    pub fn location(&self) -> Location {
        match self {
            FormalParameters::ListComma(_, location) | FormalParameters::Empty(location) => *location,
            FormalParameters::Rest(rest) => rest.location(),
            FormalParameters::List(list) => list.location(),
            FormalParameters::ListRest(list, rest) => list.location().merge(&rest.location()),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            FormalParameters::Empty(_) => false,
            FormalParameters::Rest(node) => node.contains(kind),
            FormalParameters::List(node) => node.contains(kind),
            FormalParameters::ListComma(node, _) => node.contains(kind),
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
            FormalParameters::Empty(_) => true,
            FormalParameters::Rest(node) => node.all_private_identifiers_valid(names),
            FormalParameters::List(node) => node.all_private_identifiers_valid(names),
            FormalParameters::ListComma(node, _) => node.all_private_identifiers_valid(names),
            FormalParameters::ListRest(list, rest) => {
                list.all_private_identifiers_valid(names) && rest.all_private_identifiers_valid(names)
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
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        match self {
            FormalParameters::Empty(_) => false,
            FormalParameters::Rest(frp) => frp.contains_arguments(),
            FormalParameters::List(fpl) | FormalParameters::ListComma(fpl, _) => fpl.contains_arguments(),
            FormalParameters::ListRest(fpl, frp) => fpl.contains_arguments() || frp.contains_arguments(),
        }
    }

    pub fn is_simple_parameter_list(&self) -> bool {
        // Static Semantics: IsSimpleParameterList
        match self {
            FormalParameters::Empty(_) => {
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
            FormalParameters::List(formal_parameter_list) | FormalParameters::ListComma(formal_parameter_list, _) => {
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
            FormalParameters::Empty(_) => {
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
            FormalParameters::List(formal_parameter_list) | FormalParameters::ListComma(formal_parameter_list, _) => {
                // FormalParameters :
                //      FormalParameterList
                //      FormalParameterList ,
                //  1. Return BoundNames of FormalParameterList
                formal_parameter_list.bound_names()
            }
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool, dups_already_checked: bool) {
        // Static Semantics: Early Errors
        //  FormalParameters : FormalParameterList
        //    If BoundNames of FormalParameterList contains any duplicate elements, it is a Syntax Error:
        //      * if IsSimpleParameterList of FormalParameterList is false.
        //      * if this production is executed in strict mode
        //
        // NOTE |   Multiple occurrences of the same BindingIdentifier in a FormalParameterList is only allowed for
        //          functions which have simple parameter lists and which are not defined in strict mode code.
        if !dups_already_checked && (strict || !self.is_simple_parameter_list()) {
            let bn = self.bound_names();
            for name in duplicates(&bn) {
                errs.push(create_syntax_error_object(
                    agent,
                    format!("‘{}’ already defined", name),
                    Some(self.location()),
                ));
            }
        }
        match self {
            FormalParameters::Empty(_) => (),
            FormalParameters::Rest(frp) => frp.early_errors(agent, errs, strict),
            FormalParameters::List(fpl) | FormalParameters::ListComma(fpl, _) => fpl.early_errors(agent, errs, strict),
            FormalParameters::ListRest(fpl, frp) => {
                fpl.early_errors(agent, errs, strict);
                frp.early_errors(agent, errs, strict);
            }
        }
    }

    /// Reports the number of expected arguments for the parameter list.
    ///
    /// The ExpectedArgumentCount of a FormalParameterList is the number of FormalParameters to the left of either the
    /// rest parameter or the first FormalParameter with an Initializer. A FormalParameter without an initializer is
    /// allowed after the first parameter with an initializer but such parameters are considered to be optional with
    /// undefined as their default value.
    ///
    /// See [ExpectedArgumentCount](https://tc39.es/ecma262/#sec-static-semantics-expectedargumentcount) from ECMA-262.
    pub fn expected_argument_count(&self) -> f64 {
        match self {
            FormalParameters::Empty(_) | FormalParameters::Rest(_) => 0.0,
            FormalParameters::List(list)
            | FormalParameters::ListComma(list, _)
            | FormalParameters::ListRest(list, _) => list.expected_argument_count(),
        }
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
        while let Ok((next, after_next)) =
            scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
                .and_then(|(_, after_comma)| FormalParameter::parse(parser, after_comma, yield_flag, await_flag))
        {
            current = Rc::new(FormalParameterList::List(current, next));
            current_scanner = after_next;
        }
        Ok((current, current_scanner))
    }

    pub fn location(&self) -> Location {
        match self {
            FormalParameterList::Item(item) => item.location(),
            FormalParameterList::List(list, item) => list.location().merge(&item.location()),
        }
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
            FormalParameterList::List(lst, item) => {
                lst.all_private_identifiers_valid(names) && item.all_private_identifiers_valid(names)
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
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        match self {
            FormalParameterList::Item(fp) => fp.contains_arguments(),
            FormalParameterList::List(fpl, fp) => fpl.contains_arguments() || fp.contains_arguments(),
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

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            FormalParameterList::Item(fp) => fp.early_errors(agent, errs, strict),
            FormalParameterList::List(fpl, fp) => {
                fpl.early_errors(agent, errs, strict);
                fp.early_errors(agent, errs, strict);
            }
        }
    }

    /// Sub-calculation of expected arguments for parameter lists
    ///
    /// See [ExpectedArgumentCount](https://tc39.es/ecma262/#sec-static-semantics-expectedargumentcount) from ECMA-262.
    fn expected_argument_count(&self) -> f64 {
        match self {
            FormalParameterList::Item(item) => {
                if item.has_initializer() {
                    0.0
                } else {
                    1.0
                }
            }
            FormalParameterList::List(list, item) => {
                let count = list.expected_argument_count();
                if list.has_initializer() || item.has_initializer() {
                    count
                } else {
                    count + 1.0
                }
            }
        }
    }

    /// Report whether this portion of a parameter list contains an intializer
    ///
    /// See [HasInitializer](https://tc39.es/ecma262/#sec-static-semantics-hasinitializer) from ECMA-262.
    fn has_initializer(&self) -> bool {
        match self {
            FormalParameterList::Item(item) => item.has_initializer(),
            FormalParameterList::List(list, item) => list.has_initializer() || item.has_initializer(),
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

    pub fn location(&self) -> Location {
        self.element.location()
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
        self.element.contains_arguments()
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        // Static Semantics: BoundNames
        // FunctionRestParameter : BindingRestElement
        //  1. Return BoundNames of BindingRestElement.
        self.element.bound_names()
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        self.element.early_errors(agent, errs, strict);
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

    pub fn location(&self) -> Location {
        self.element.location()
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
        self.element.contains_arguments()
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

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        self.element.early_errors(agent, errs, strict)
    }

    /// Report whether this parameter contains an intializer
    ///
    /// See [HasInitializer](https://tc39.es/ecma262/#sec-static-semantics-hasinitializer) from ECMA-262.
    fn has_initializer(&self) -> bool {
        self.element.has_initializer()
    }
}

#[cfg(test)]
mod tests;
