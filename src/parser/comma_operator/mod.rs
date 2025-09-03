use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// Expression[In, Yield, Await] :
//      AssignmentExpression[?In, ?Yield, ?Await]
//      Expression[?In, ?Yield, ?Await] , AssignmentExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub(crate) enum Expression {
    FallThru(Rc<AssignmentExpression>),
    Comma(Rc<Expression>, Rc<AssignmentExpression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Expression::FallThru(node) => node.fmt(f),
            Expression::Comma(left, right) => write!(f, "{left} , {right}"),
        }
    }
}

impl PrettyPrint for Expression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}Expression: {self}")?;
        match &self {
            Expression::FallThru(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Expression::Comma(left, right) => {
                left.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                right.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            Expression::FallThru(node) => node.concise_with_leftpad(writer, pad, state),
            Expression::Comma(left, right) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{first}Expression: {self}")?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl Expression {
    fn parse_core(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::Expression), scanner)).otherwise(|| {
            AssignmentExpression::parse(parser, scanner, in_flag, yield_flag, await_flag).map(|(left, after_left)| {
                let mut current = Rc::new(Expression::FallThru(left));
                let mut current_scanner = after_left;
                while let Ok((right, after_right)) =
                    scan_for_punct(current_scanner, parser.source, InputElementGoal::Div, Punctuator::Comma).and_then(
                        |(_, after_token)| {
                            AssignmentExpression::parse(parser, after_token, in_flag, yield_flag, await_flag)
                        },
                    )
                {
                    current = Rc::new(Expression::Comma(current, right));
                    current_scanner = after_right;
                }
                (current, current_scanner)
            })
        })
    }

    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let key = InYieldAwaitKey { scanner, in_flag, yield_flag, await_flag };
        match parser.expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag, yield_flag, await_flag);
                parser.expression_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            Expression::FallThru(exp) => exp.location(),
            Expression::Comma(left, right) => left.location().merge(&right.location()),
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            Expression::FallThru(node) => kind == ParseNodeKind::AssignmentExpression || node.contains(kind),
            Expression::Comma(left, right) => {
                [ParseNodeKind::Expression, ParseNodeKind::AssignmentExpression].contains(&kind)
                    || left.contains(kind)
                    || right.contains(kind)
            }
        }
    }

    pub(crate) fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            Expression::FallThru(node) => node.as_string_literal(),
            Expression::Comma(..) => None,
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
            Expression::FallThru(node) => node.all_private_identifiers_valid(names),
            Expression::Comma(left, right) => {
                left.all_private_identifiers_valid(names) && right.all_private_identifiers_valid(names)
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
            Expression::FallThru(ae) => ae.contains_arguments(),
            Expression::Comma(e, ae) => e.contains_arguments() || ae.contains_arguments(),
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            Expression::FallThru(node) => node.early_errors(errs, strict),
            Expression::Comma(left, right) => {
                left.early_errors(errs, strict);
                right.early_errors(errs, strict);
            }
        }
    }

    pub(crate) fn is_strictly_deletable(&self) -> bool {
        match self {
            Expression::FallThru(node) => node.is_strictly_deletable(),
            Expression::Comma(..) => true,
        }
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub(crate) fn assignment_target_type(&self, strict: bool) -> ATTKind {
        match &self {
            Expression::FallThru(node) => node.assignment_target_type(strict),
            Expression::Comma(_, _) => ATTKind::Invalid,
        }
    }

    //pub(crate) fn is_named_function(&self) -> bool {
    //    match self {
    //        Expression::FallThru(node) => node.is_named_function(),
    //        Expression::Comma(..) => false,
    //    }
    //}

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                Expression::FallThru(node) => node.body_containing_location(location),
                Expression::Comma(left, right) => {
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
        // Expression :
        //      AssignmentExpression
        //      Expression , AssignmentExpression
        //  1. Return HasCallInTailPosition of AssignmentExpression with argument call.
        match self {
            Expression::FallThru(ae) | Expression::Comma(_, ae) => ae.has_call_in_tail_position(location),
        }
    }
}

#[cfg(test)]
mod tests;
