use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::multiplicative_operators::MultiplicativeExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// AdditiveExpression[Yield, Await] :
//      MultiplicativeExpression[?Yield, ?Await]
//      AdditiveExpression[?Yield, ?Await] + MultiplicativeExpression[?Yield, ?Await]
//      AdditiveExpression[?Yield, ?Await] - MultiplicativeExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum AdditiveExpression {
    MultiplicativeExpression(Rc<MultiplicativeExpression>),
    AdditiveExpressionAdd(Rc<AdditiveExpression>, Rc<MultiplicativeExpression>),
    AdditiveExpressionSubtract(Rc<AdditiveExpression>, Rc<MultiplicativeExpression>),
}

impl fmt::Display for AdditiveExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            AdditiveExpression::MultiplicativeExpression(boxed) => write!(f, "{}", boxed),
            AdditiveExpression::AdditiveExpressionAdd(ae, me) => {
                write!(f, "{} + {}", ae, me)
            }
            AdditiveExpression::AdditiveExpressionSubtract(ae, me) => {
                write!(f, "{} - {}", ae, me)
            }
        }
    }
}

impl PrettyPrint for AdditiveExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}AdditiveExpression: {}", first, self)?;
        match &self {
            AdditiveExpression::MultiplicativeExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            AdditiveExpression::AdditiveExpressionAdd(ae, me) | AdditiveExpression::AdditiveExpressionSubtract(ae, me) => {
                ae.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                me.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut work = |left: &AdditiveExpression, right: &MultiplicativeExpression, op| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}AdditiveExpression: {}", first, self)
                .and_then(|_| left.concise_with_leftpad(writer, &successive, Spot::NotFinal))
                .and_then(|_| pprint_token(writer, op, TokenType::Punctuator, &successive, Spot::NotFinal))
                .and_then(|_| right.concise_with_leftpad(writer, &successive, Spot::Final))
        };

        match self {
            AdditiveExpression::MultiplicativeExpression(node) => node.concise_with_leftpad(writer, pad, state),
            AdditiveExpression::AdditiveExpressionAdd(left, right) => work(left, right, "+"),
            AdditiveExpression::AdditiveExpressionSubtract(left, right) => work(left, right, "-"),
        }
    }
}

impl IsFunctionDefinition for AdditiveExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            AdditiveExpression::AdditiveExpressionAdd(..) | AdditiveExpression::AdditiveExpressionSubtract(..) => false,
            AdditiveExpression::MultiplicativeExpression(me) => me.is_function_definition(),
        }
    }
}

impl AssignmentTargetType for AdditiveExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            AdditiveExpression::AdditiveExpressionAdd(..) | AdditiveExpression::AdditiveExpressionSubtract(..) => ATTKind::Invalid,
            AdditiveExpression::MultiplicativeExpression(me) => me.assignment_target_type(),
        }
    }
}

impl AdditiveExpression {
    // AdditiveExpression's only direct parent is ShiftExpression. It doesn't need to be cached.
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (me, after_me) = MultiplicativeExpression::parse(parser, scanner, yield_flag, await_flag)?;
        let mut current = Rc::new(AdditiveExpression::MultiplicativeExpression(me));
        let mut current_scanner = after_me;
        while let Ok((punct, me, after_me)) = scan_for_punct_set(current_scanner, parser.source, ScanGoal::InputElementDiv, &[Punctuator::Plus, Punctuator::Minus])
            .and_then(|(token, after_op)| MultiplicativeExpression::parse(parser, after_op, yield_flag, await_flag).map(|(node, after_node)| (token, node, after_node)))
        {
            current = Rc::new(match punct {
                Punctuator::Plus => AdditiveExpression::AdditiveExpressionAdd(current, me),
                _ => AdditiveExpression::AdditiveExpressionSubtract(current, me),
            });
            current_scanner = after_me;
        }
        Ok((current, current_scanner))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            AdditiveExpression::MultiplicativeExpression(n) => n.contains(kind),
            AdditiveExpression::AdditiveExpressionAdd(l, r) => l.contains(kind) || r.contains(kind),
            AdditiveExpression::AdditiveExpressionSubtract(l, r) => l.contains(kind) || r.contains(kind),
        }
    }
}

#[cfg(test)]
mod tests;
