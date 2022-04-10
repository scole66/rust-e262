use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::assignment_operators::AssignmentExpression;
use super::binary_logical_operators::ShortCircuitExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner, StringToken};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// ConditionalExpression[In, Yield, Await] :
//      ShortCircuitExpression[?In, ?Yield, ?Await]
//      ShortCircuitExpression[?In, ?Yield, ?Await] ? AssignmentExpression[+In, ?Yield, ?Await] : AssignmentExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum ConditionalExpression {
    FallThru(Rc<ShortCircuitExpression>),
    Conditional(Rc<ShortCircuitExpression>, Rc<AssignmentExpression>, Rc<AssignmentExpression>),
}

impl fmt::Display for ConditionalExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ConditionalExpression::FallThru(node) => node.fmt(f),
            ConditionalExpression::Conditional(condition, thenish, elseish) => {
                write!(f, "{} ? {} : {}", condition, thenish, elseish)
            }
        }
    }
}

impl PrettyPrint for ConditionalExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ConditionalExpression: {}", first, self)?;
        match &self {
            ConditionalExpression::FallThru(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            ConditionalExpression::Conditional(condition, thenish, elseish) => {
                condition.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                thenish.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                elseish.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            ConditionalExpression::FallThru(node) => node.concise_with_leftpad(writer, pad, state),
            ConditionalExpression::Conditional(a, b, c) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}ConditionalExpression: {}", first, self)?;
                a.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "?", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                b.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                c.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for ConditionalExpression {
    fn is_function_definition(&self) -> bool {
        match &self {
            ConditionalExpression::Conditional(_, _, _) => false,
            ConditionalExpression::FallThru(node) => node.is_function_definition(),
        }
    }
}

impl AssignmentTargetType for ConditionalExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match &self {
            ConditionalExpression::Conditional(_, _, _) => ATTKind::Invalid,
            ConditionalExpression::FallThru(node) => node.assignment_target_type(),
        }
    }
}

impl ConditionalExpression {
    // no need to cache
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (left, after_left) = ShortCircuitExpression::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        match scan_for_punct(after_left, parser.source, ScanGoal::InputElementDiv, Punctuator::Question)
            .and_then(|after_q| AssignmentExpression::parse(parser, after_q, true, yield_flag, await_flag))
            .and_then(|(ae1, after_ae1)| {
                scan_for_punct(after_ae1, parser.source, ScanGoal::InputElementDiv, Punctuator::Colon)
                    .and_then(|after_colon| AssignmentExpression::parse(parser, after_colon, in_flag, yield_flag, await_flag))
                    .map(|(ae2, after_ae2)| (ae1, ae2, after_ae2))
            }) {
            Ok((thenish, elseish, after)) => Ok((Rc::new(ConditionalExpression::Conditional(left, thenish, elseish)), after)),
            Err(_) => Ok((Rc::new(ConditionalExpression::FallThru(left)), after_left)),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ConditionalExpression::FallThru(node) => node.contains(kind),
            ConditionalExpression::Conditional(cond, truthy, falsey) => cond.contains(kind) || truthy.contains(kind) || falsey.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            ConditionalExpression::FallThru(node) => node.as_string_literal(),
            _ => None,
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
            ConditionalExpression::FallThru(node) => node.all_private_identifiers_valid(names),
            ConditionalExpression::Conditional(cond, truthy, falsey) => {
                cond.all_private_identifiers_valid(names) && truthy.all_private_identifiers_valid(names) && falsey.all_private_identifiers_valid(names)
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
            ConditionalExpression::FallThru(sce) => sce.contains_arguments(),
            ConditionalExpression::Conditional(sce, ae1, ae2) => sce.contains_arguments() || ae1.contains_arguments() || ae2.contains_arguments(),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ConditionalExpression::FallThru(node) => node.early_errors(agent, errs, strict),
            ConditionalExpression::Conditional(a, b, c) => {
                a.early_errors(agent, errs, strict);
                b.early_errors(agent, errs, strict);
                c.early_errors(agent, errs, strict);
            }
        }
    }

    pub fn is_strictly_deletable(&self) -> bool {
        match self {
            ConditionalExpression::FallThru(node) => node.is_strictly_deletable(),
            _ => true,
        }
    }
}

#[cfg(test)]
mod tests;
