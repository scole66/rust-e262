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
}

#[cfg(test)]
mod tests;
