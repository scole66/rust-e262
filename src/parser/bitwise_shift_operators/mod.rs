use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// ShiftExpression[Yield, Await] :
//      AdditiveExpression[?Yield, ?Await]
//      ShiftExpression[?Yield, ?Await] << AdditiveExpression[?Yield, ?Await]
//      ShiftExpression[?Yield, ?Await] >> AdditiveExpression[?Yield, ?Await]
//      ShiftExpression[?Yield, ?Await] >>> AdditiveExpression[?Yield, ?Await]
#[derive(Debug)]
pub(crate) enum ShiftExpression {
    AdditiveExpression(Rc<AdditiveExpression>),
    LeftShift(Rc<ShiftExpression>, Rc<AdditiveExpression>),
    SignedRightShift(Rc<ShiftExpression>, Rc<AdditiveExpression>),
    UnsignedRightShift(Rc<ShiftExpression>, Rc<AdditiveExpression>),
}

impl fmt::Display for ShiftExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ShiftExpression::AdditiveExpression(ae) => write!(f, "{ae}"),
            ShiftExpression::LeftShift(se, ae) => write!(f, "{se} << {ae}"),
            ShiftExpression::SignedRightShift(se, ae) => {
                write!(f, "{se} >> {ae}")
            }
            ShiftExpression::UnsignedRightShift(se, ae) => {
                write!(f, "{se} >>> {ae}")
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
        writeln!(writer, "{first}ShiftExpression: {self}")?;
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
            writeln!(writer, "{first}ShiftExpression: {self}")
                .and_then(|()| left.concise_with_leftpad(writer, &successive, Spot::NotFinal))
                .and_then(|()| pprint_token(writer, op, TokenType::Punctuator, &successive, Spot::NotFinal))
                .and_then(|()| right.concise_with_leftpad(writer, &successive, Spot::Final))
        };

        match self {
            ShiftExpression::AdditiveExpression(node) => node.concise_with_leftpad(writer, pad, state),
            ShiftExpression::LeftShift(left, right) => work(left, right, "<<"),
            ShiftExpression::SignedRightShift(left, right) => work(left, right, ">>"),
            ShiftExpression::UnsignedRightShift(left, right) => work(left, right, ">>>"),
        }
    }
}

impl ShiftExpression {
    // Only one parent. No need to cache.
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        AdditiveExpression::parse(parser, scanner, yield_flag, await_flag).map(|(ae, after_ae)| {
            let mut current = Rc::new(ShiftExpression::AdditiveExpression(ae));
            let mut current_scan = after_ae;
            while let Ok((make_se, ae2, after_ae2)) = scan_for_punct_set(
                current_scan,
                parser.source,
                InputElementGoal::Div,
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

    pub(crate) fn location(&self) -> Location {
        match self {
            ShiftExpression::AdditiveExpression(exp) => exp.location(),
            ShiftExpression::LeftShift(left, right)
            | ShiftExpression::SignedRightShift(left, right)
            | ShiftExpression::UnsignedRightShift(left, right) => left.location().merge(&right.location()),
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ShiftExpression::AdditiveExpression(n) => n.contains(kind),
            ShiftExpression::LeftShift(l, r)
            | ShiftExpression::SignedRightShift(l, r)
            | ShiftExpression::UnsignedRightShift(l, r) => l.contains(kind) || r.contains(kind),
        }
    }

    pub(crate) fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            ShiftExpression::AdditiveExpression(n) => n.as_string_literal(),
            _ => None,
        }
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        match self {
            ShiftExpression::AdditiveExpression(n) => n.all_private_identifiers_valid(names),
            ShiftExpression::LeftShift(l, r)
            | ShiftExpression::SignedRightShift(l, r)
            | ShiftExpression::UnsignedRightShift(l, r) => {
                l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names)
            }
        }
    }

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub(crate) fn contains_arguments(&self) -> bool {
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

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ShiftExpression::AdditiveExpression(n) => n.early_errors(errs, strict),
            ShiftExpression::LeftShift(l, r)
            | ShiftExpression::SignedRightShift(l, r)
            | ShiftExpression::UnsignedRightShift(l, r) => {
                l.early_errors(errs, strict);
                r.early_errors(errs, strict);
            }
        }
    }

    pub(crate) fn is_strictly_deletable(&self) -> bool {
        match self {
            ShiftExpression::AdditiveExpression(node) => node.is_strictly_deletable(),
            _ => true,
        }
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub(crate) fn assignment_target_type(&self, strict: bool) -> ATTKind {
        match self {
            ShiftExpression::AdditiveExpression(child) => child.assignment_target_type(strict),
            _ => ATTKind::Invalid,
        }
    }

    //pub(crate) fn is_named_function(&self) -> bool {
    //    match self {
    //        ShiftExpression::AdditiveExpression(node) => node.is_named_function(),
    //        _ => false,
    //    }
    //}

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                ShiftExpression::AdditiveExpression(node) => node.body_containing_location(location),
                ShiftExpression::LeftShift(se, ae)
                | ShiftExpression::SignedRightShift(se, ae)
                | ShiftExpression::UnsignedRightShift(se, ae) => {
                    se.body_containing_location(location).or_else(|| ae.body_containing_location(location))
                }
            }
        } else {
            None
        }
    }

    pub(crate) fn has_call_in_tail_position(&self, location: &Location) -> bool {
        // Static Semantics: HasCallInTailPosition
        // The syntax-directed operation HasCallInTailPosition takes argument call (a CallExpression Parse Node, a
        // MemberExpression Parse Node, or an OptionalChain Parse Node) and returns a Boolean.
        //
        // Note 1: call is a Parse Node that represents a specific range of source text. When the following algorithms
        //         compare call to another Parse Node, it is a test of whether they represent the same source text.
        //
        // Note 2: A potential tail position call that is immediately followed by return GetValue of the call result is
        //         also a possible tail position call. A function call cannot return a Reference Record, so such a
        //         GetValue operation will always return the same value as the actual function call result.
        //
        // ShiftExpression :
        //      AdditiveExpression
        //  1. Return HasCallInTailPosition of AdditiveExpression with argument call.
        // ShiftExpression :
        //      ShiftExpression << AdditiveExpression
        //      ShiftExpression >> AdditiveExpression
        //      ShiftExpression >>> AdditiveExpression
        //  1. Return false.
        match self {
            ShiftExpression::AdditiveExpression(additive_expression) => {
                additive_expression.has_call_in_tail_position(location)
            }
            ShiftExpression::LeftShift(..)
            | ShiftExpression::SignedRightShift(..)
            | ShiftExpression::UnsignedRightShift(..) => false,
        }
    }
}

#[cfg(test)]
mod tests;
