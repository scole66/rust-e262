use super::multiplicative_operators::MultiplicativeExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner, StringToken};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// AdditiveExpression[Yield, Await] :
//      MultiplicativeExpression[?Yield, ?Await]
//      AdditiveExpression[?Yield, ?Await] + MultiplicativeExpression[?Yield, ?Await]
//      AdditiveExpression[?Yield, ?Await] - MultiplicativeExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum AdditiveExpression {
    MultiplicativeExpression(Rc<MultiplicativeExpression>),
    Add(Rc<AdditiveExpression>, Rc<MultiplicativeExpression>),
    Subtract(Rc<AdditiveExpression>, Rc<MultiplicativeExpression>),
}

impl fmt::Display for AdditiveExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            AdditiveExpression::MultiplicativeExpression(boxed) => write!(f, "{}", boxed),
            AdditiveExpression::Add(ae, me) => {
                write!(f, "{} + {}", ae, me)
            }
            AdditiveExpression::Subtract(ae, me) => {
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
            AdditiveExpression::Add(ae, me) | AdditiveExpression::Subtract(ae, me) => {
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
            AdditiveExpression::Add(left, right) => work(left, right, "+"),
            AdditiveExpression::Subtract(left, right) => work(left, right, "-"),
        }
    }
}

impl IsFunctionDefinition for AdditiveExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            AdditiveExpression::Add(..) | AdditiveExpression::Subtract(..) => false,
            AdditiveExpression::MultiplicativeExpression(me) => me.is_function_definition(),
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
                Punctuator::Plus => AdditiveExpression::Add(current, me),
                _ => AdditiveExpression::Subtract(current, me),
            });
            current_scanner = after_me;
        }
        Ok((current, current_scanner))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            AdditiveExpression::MultiplicativeExpression(n) => n.contains(kind),
            AdditiveExpression::Add(l, r) => l.contains(kind) || r.contains(kind),
            AdditiveExpression::Subtract(l, r) => l.contains(kind) || r.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            AdditiveExpression::MultiplicativeExpression(n) => n.as_string_literal(),
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
            AdditiveExpression::MultiplicativeExpression(n) => n.all_private_identifiers_valid(names),
            AdditiveExpression::Add(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),
            AdditiveExpression::Subtract(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),
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
            AdditiveExpression::MultiplicativeExpression(me) => me.contains_arguments(),
            AdditiveExpression::Add(ae, me) | AdditiveExpression::Subtract(ae, me) => ae.contains_arguments() || me.contains_arguments(),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            AdditiveExpression::MultiplicativeExpression(n) => n.early_errors(agent, errs, strict),
            AdditiveExpression::Add(l, r) | AdditiveExpression::Subtract(l, r) => {
                l.early_errors(agent, errs, strict);
                r.early_errors(agent, errs, strict);
            }
        }
    }

    pub fn is_strictly_deletable(&self) -> bool {
        match self {
            AdditiveExpression::MultiplicativeExpression(node) => node.is_strictly_deletable(),
            _ => true,
        }
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub fn assignment_target_type(&self, strict: bool) -> ATTKind {
        match self {
            AdditiveExpression::Add(..) | AdditiveExpression::Subtract(..) => ATTKind::Invalid,
            AdditiveExpression::MultiplicativeExpression(me) => me.assignment_target_type(strict),
        }
    }
}

#[cfg(test)]
mod tests;
