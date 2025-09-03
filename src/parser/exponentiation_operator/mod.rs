use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// ExponentiationExpression[Yield, Await] :
//      UnaryExpression[?Yield, ?Await]
//      UpdateExpression[?Yield, ?Await] ** ExponentiationExpression[?Yield, ?Await]
#[derive(Debug)]
pub(crate) enum ExponentiationExpression {
    UnaryExpression(Rc<UnaryExpression>),
    Exponentiation(Rc<UpdateExpression>, Rc<ExponentiationExpression>),
}

impl fmt::Display for ExponentiationExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ExponentiationExpression::UnaryExpression(boxed) => {
                write!(f, "{boxed}")
            }
            ExponentiationExpression::Exponentiation(ue, ee) => {
                write!(f, "{ue} ** {ee}")
            }
        }
    }
}

impl PrettyPrint for ExponentiationExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ExponentiationExpression: {self}")?;
        match &self {
            ExponentiationExpression::UnaryExpression(boxed) => {
                boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ExponentiationExpression::Exponentiation(ue, ee) => {
                ue.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ee.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            ExponentiationExpression::UnaryExpression(node) => node.concise_with_leftpad(writer, pad, state),
            ExponentiationExpression::Exponentiation(left, right) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{first}ExponentiationExpression: {self}")?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "**", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl ExponentiationExpression {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::ExponentiationExpression), scanner))
            .otherwise(|| {
                let (ue, after_ue) = UpdateExpression::parse(parser, scanner, yield_flag, await_flag)?;
                let (_, after_op) =
                    scan_for_punct(after_ue, parser.source, InputElementGoal::Div, Punctuator::StarStar)?;
                let (ee, after_ee) = ExponentiationExpression::parse(parser, after_op, yield_flag, await_flag)?;
                Ok((Rc::new(ExponentiationExpression::Exponentiation(ue, ee)), after_ee))
            })
            .otherwise(|| {
                let (unary, after_unary) = UnaryExpression::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(ExponentiationExpression::UnaryExpression(unary)), after_unary))
            })
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            ExponentiationExpression::UnaryExpression(exp) => exp.location(),
            ExponentiationExpression::Exponentiation(left, right) => left.location().merge(&right.location()),
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ExponentiationExpression::UnaryExpression(n) => n.contains(kind),
            ExponentiationExpression::Exponentiation(l, r) => l.contains(kind) || r.contains(kind),
        }
    }

    pub(crate) fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            ExponentiationExpression::UnaryExpression(n) => n.as_string_literal(),
            ExponentiationExpression::Exponentiation(..) => None,
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
            ExponentiationExpression::UnaryExpression(n) => n.all_private_identifiers_valid(names),
            ExponentiationExpression::Exponentiation(l, r) => {
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
            ExponentiationExpression::UnaryExpression(ue) => ue.contains_arguments(),
            ExponentiationExpression::Exponentiation(ue, ee) => ue.contains_arguments() || ee.contains_arguments(),
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ExponentiationExpression::UnaryExpression(n) => n.early_errors(errs, strict),
            ExponentiationExpression::Exponentiation(l, r) => {
                l.early_errors(errs, strict);
                r.early_errors(errs, strict);
            }
        }
    }

    pub(crate) fn is_strictly_deletable(&self) -> bool {
        match self {
            ExponentiationExpression::UnaryExpression(node) => node.is_strictly_deletable(),
            ExponentiationExpression::Exponentiation(..) => true,
        }
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub(crate) fn assignment_target_type(&self, strict: bool) -> ATTKind {
        match self {
            ExponentiationExpression::Exponentiation(..) => ATTKind::Invalid,
            ExponentiationExpression::UnaryExpression(ue) => ue.assignment_target_type(strict),
        }
    }

    //pub(crate) fn is_named_function(&self) -> bool {
    //    match self {
    //        ExponentiationExpression::UnaryExpression(node) => node.is_named_function(),
    //        ExponentiationExpression::Exponentiation(..) => false,
    //    }
    //}

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                ExponentiationExpression::UnaryExpression(node) => node.body_containing_location(location),
                ExponentiationExpression::Exponentiation(left, right) => {
                    left.body_containing_location(location).or_else(|| right.body_containing_location(location))
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
        // ExponentiationExpression :
        //      UnaryExpression
        //  1. Return HasCallInTailPosition for UnaryExpression with argument call.
        // ExponentiationExpression :
        //      UpdateExpression ** ExponentiationExpression
        //  1. Return false.
        match self {
            ExponentiationExpression::UnaryExpression(unary_expression) => {
                unary_expression.has_call_in_tail_position(location)
            }
            ExponentiationExpression::Exponentiation(..) => false,
        }
    }
}

#[cfg(test)]
mod tests;
