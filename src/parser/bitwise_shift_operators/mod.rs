use super::additive_operators::AdditiveExpression;
use super::scanner::{Punctuator, ScanGoal, Scanner, StringToken};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// ShiftExpression[Yield, Await] :
//      AdditiveExpression[?Yield, ?Await]
//      ShiftExpression[?Yield, ?Await] << AdditiveExpression[?Yield, ?Await]
//      ShiftExpression[?Yield, ?Await] >> AdditiveExpression[?Yield, ?Await]
//      ShiftExpression[?Yield, ?Await] >>> AdditiveExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum ShiftExpression {
    AdditiveExpression(Rc<AdditiveExpression>),
    LeftShift(Rc<ShiftExpression>, Rc<AdditiveExpression>),
    SignedRightShift(Rc<ShiftExpression>, Rc<AdditiveExpression>),
    UnsignedRightShift(Rc<ShiftExpression>, Rc<AdditiveExpression>),
}

impl fmt::Display for ShiftExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ShiftExpression::AdditiveExpression(ae) => write!(f, "{}", ae),
            ShiftExpression::LeftShift(se, ae) => write!(f, "{} << {}", se, ae),
            ShiftExpression::SignedRightShift(se, ae) => {
                write!(f, "{} >> {}", se, ae)
            }
            ShiftExpression::UnsignedRightShift(se, ae) => {
                write!(f, "{} >>> {}", se, ae)
            }
        }
    }
}

impl PrettyPrint for ShiftExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ShiftExpression: {}", first, self)?;
        match self {
            ShiftExpression::AdditiveExpression(ae) => ae.pprint_with_leftpad(writer, &successive, Spot::Final),
            ShiftExpression::LeftShift(se, ae)
            | ShiftExpression::SignedRightShift(se, ae)
            | ShiftExpression::UnsignedRightShift(se, ae) => {
                se.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ae.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut work = |left: &ShiftExpression, right: &AdditiveExpression, op| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}ShiftExpression: {}", first, self)
                .and_then(|_| left.concise_with_leftpad(writer, &successive, Spot::NotFinal))
                .and_then(|_| pprint_token(writer, op, TokenType::Punctuator, &successive, Spot::NotFinal))
                .and_then(|_| right.concise_with_leftpad(writer, &successive, Spot::Final))
        };

        match self {
            ShiftExpression::AdditiveExpression(node) => node.concise_with_leftpad(writer, pad, state),
            ShiftExpression::LeftShift(left, right) => work(left, right, "<<"),
            ShiftExpression::SignedRightShift(left, right) => work(left, right, ">>"),
            ShiftExpression::UnsignedRightShift(left, right) => work(left, right, ">>>"),
        }
    }
}

impl IsFunctionDefinition for ShiftExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            ShiftExpression::AdditiveExpression(child) => child.is_function_definition(),
            _ => false,
        }
    }
}

impl ShiftExpression {
    // Only one parent. No need to cache.
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        AdditiveExpression::parse(parser, scanner, yield_flag, await_flag).map(|(ae, after_ae)| {
            let mut current = Rc::new(ShiftExpression::AdditiveExpression(ae));
            let mut current_scan = after_ae;
            while let Ok((make_se, ae2, after_ae2)) = scan_for_punct_set(
                current_scan,
                parser.source,
                ScanGoal::InputElementDiv,
                &[Punctuator::GtGt, Punctuator::GtGtGt, Punctuator::LtLt],
            )
            .and_then(|(shift_op, _, after_op)| {
                let make_se = match shift_op {
                    Punctuator::GtGt => ShiftExpression::SignedRightShift,
                    Punctuator::LtLt => ShiftExpression::LeftShift,
                    _ => ShiftExpression::UnsignedRightShift,
                };
                AdditiveExpression::parse(parser, after_op, yield_flag, await_flag)
                    .map(|(ae2, after_ae2)| (make_se, ae2, after_ae2))
            }) {
                current = Rc::new(make_se(current, ae2));
                current_scan = after_ae2;
            }
            (current, current_scan)
        })
    }

    pub fn location(&self) -> Location {
        match self {
            ShiftExpression::AdditiveExpression(exp) => exp.location(),
            ShiftExpression::LeftShift(left, right)
            | ShiftExpression::SignedRightShift(left, right)
            | ShiftExpression::UnsignedRightShift(left, right) => left.location().merge(&right.location()),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ShiftExpression::AdditiveExpression(n) => n.contains(kind),
            ShiftExpression::LeftShift(l, r) => l.contains(kind) || r.contains(kind),
            ShiftExpression::SignedRightShift(l, r) => l.contains(kind) || r.contains(kind),
            ShiftExpression::UnsignedRightShift(l, r) => l.contains(kind) || r.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            ShiftExpression::AdditiveExpression(n) => n.as_string_literal(),
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
            ShiftExpression::AdditiveExpression(n) => n.all_private_identifiers_valid(names),
            ShiftExpression::LeftShift(l, r) => {
                l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names)
            }
            ShiftExpression::SignedRightShift(l, r) => {
                l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names)
            }
            ShiftExpression::UnsignedRightShift(l, r) => {
                l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names)
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
            ShiftExpression::AdditiveExpression(ae) => ae.contains_arguments(),
            ShiftExpression::LeftShift(se, ae)
            | ShiftExpression::SignedRightShift(se, ae)
            | ShiftExpression::UnsignedRightShift(se, ae) => se.contains_arguments() || ae.contains_arguments(),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ShiftExpression::AdditiveExpression(n) => n.early_errors(agent, errs, strict),
            ShiftExpression::LeftShift(l, r)
            | ShiftExpression::SignedRightShift(l, r)
            | ShiftExpression::UnsignedRightShift(l, r) => {
                l.early_errors(agent, errs, strict);
                r.early_errors(agent, errs, strict);
            }
        }
    }

    pub fn is_strictly_deletable(&self) -> bool {
        match self {
            ShiftExpression::AdditiveExpression(node) => node.is_strictly_deletable(),
            _ => true,
        }
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub fn assignment_target_type(&self, strict: bool) -> ATTKind {
        match self {
            ShiftExpression::AdditiveExpression(child) => child.assignment_target_type(strict),
            _ => ATTKind::Invalid,
        }
    }

    pub fn is_named_function(&self) -> bool {
        match self {
            ShiftExpression::AdditiveExpression(node) => node.is_named_function(),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests;
