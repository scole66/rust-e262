use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// UpdateExpression[Yield, Await] :
//      LeftHandSideExpression[?Yield, ?Await]
//      LeftHandSideExpression[?Yield, ?Await] [no LineTerminator here] ++
//      LeftHandSideExpression[?Yield, ?Await] [no LineTerminator here] --
//      ++ UnaryExpression[?Yield, ?Await]
//      -- UnaryExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum UpdateExpression {
    LeftHandSideExpression(Rc<LeftHandSideExpression>),
    PostIncrement { lhs: Rc<LeftHandSideExpression>, location: Location },
    PostDecrement { lhs: Rc<LeftHandSideExpression>, location: Location },
    PreIncrement { ue: Rc<UnaryExpression>, location: Location },
    PreDecrement { ue: Rc<UnaryExpression>, location: Location },
}

impl fmt::Display for UpdateExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            UpdateExpression::LeftHandSideExpression(boxed) => boxed.fmt(f),
            UpdateExpression::PostIncrement { lhs: boxed, .. } => write!(f, "{boxed} ++"),
            UpdateExpression::PostDecrement { lhs: boxed, .. } => write!(f, "{boxed} --"),
            UpdateExpression::PreIncrement { ue: boxed, .. } => write!(f, "++ {boxed}"),
            UpdateExpression::PreDecrement { ue: boxed, .. } => write!(f, "-- {boxed}"),
        }
    }
}

impl PrettyPrint for UpdateExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}UpdateExpression: {self}")?;
        match &self {
            UpdateExpression::LeftHandSideExpression(boxed)
            | UpdateExpression::PostIncrement { lhs: boxed, .. }
            | UpdateExpression::PostDecrement { lhs: boxed, .. } => {
                boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            UpdateExpression::PreIncrement { ue: boxed, .. } | UpdateExpression::PreDecrement { ue: boxed, .. } => {
                boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let head = |writer: &mut T| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{first}UpdateExpression: {self}").and(Ok(successive))
        };
        let workafter = |writer: &mut T, node: &LeftHandSideExpression, op: &str| {
            head(writer).and_then(|successive| {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)
                    .and_then(|()| pprint_token(writer, op, TokenType::Punctuator, &successive, Spot::Final))
            })
        };
        let workbefore = |writer: &mut T, node: &UnaryExpression, op: &str| {
            head(writer).and_then(|successive| {
                pprint_token(writer, op, TokenType::Punctuator, &successive, Spot::NotFinal)
                    .and_then(|()| node.concise_with_leftpad(writer, &successive, Spot::Final))
            })
        };
        match self {
            UpdateExpression::LeftHandSideExpression(node) => node.concise_with_leftpad(writer, pad, state),
            UpdateExpression::PostIncrement { lhs: node, .. } => workafter(writer, node, "++"),
            UpdateExpression::PostDecrement { lhs: node, .. } => workafter(writer, node, "--"),
            UpdateExpression::PreIncrement { ue: node, .. } => workbefore(writer, node, "++"),
            UpdateExpression::PreDecrement { ue: node, .. } => workbefore(writer, node, "--"),
        }
    }
}

impl IsFunctionDefinition for UpdateExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            UpdateExpression::LeftHandSideExpression(boxed) => boxed.is_function_definition(),
            UpdateExpression::PostIncrement { .. }
            | UpdateExpression::PostDecrement { .. }
            | UpdateExpression::PreIncrement { .. }
            | UpdateExpression::PreDecrement { .. } => false,
        }
    }
}

impl UpdateExpression {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::UpdateExpression), scanner))
            .otherwise(|| {
                let (plusses_loc, after_plusses) =
                    scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::PlusPlus)?;
                let (ue, after_ue) = UnaryExpression::parse(parser, after_plusses, yield_flag, await_flag)?;
                Ok((
                    Rc::new({
                        let location = plusses_loc.merge(&ue.location());
                        UpdateExpression::PreIncrement { ue, location }
                    }),
                    after_ue,
                ))
            })
            .otherwise(|| {
                let (minuses_loc, after_minuses) =
                    scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::MinusMinus)?;
                let (ue, after_ue) = UnaryExpression::parse(parser, after_minuses, yield_flag, await_flag)?;
                Ok((
                    Rc::new({
                        let location = minuses_loc.merge(&ue.location());
                        UpdateExpression::PreDecrement { ue, location }
                    }),
                    after_ue,
                ))
            })
            .otherwise(|| {
                enum AftLHS {
                    Nothing,
                    Inc(Location),
                    Dec(Location),
                }
                let (lhs, after_lhs) = LeftHandSideExpression::parse(parser, scanner, yield_flag, await_flag)?;
                no_line_terminator(after_lhs, parser.source)
                    .and_then(|()| {
                        let (punct, punct_loc, after_punct) = scan_for_punct_set(
                            after_lhs,
                            parser.source,
                            ScanGoal::InputElementDiv,
                            &[Punctuator::PlusPlus, Punctuator::MinusMinus],
                        )?;
                        let location = lhs.location().merge(&punct_loc);
                        match punct {
                            Punctuator::PlusPlus => Ok((AftLHS::Inc(location), after_punct)),
                            _ => Ok((AftLHS::Dec(location), after_punct)),
                        }
                    })
                    .otherwise(|| Ok((AftLHS::Nothing, after_lhs)))
                    .map(|(aft, scan)| {
                        (
                            Rc::new(match aft {
                                AftLHS::Nothing => UpdateExpression::LeftHandSideExpression(lhs),
                                AftLHS::Inc(location) => UpdateExpression::PostIncrement { lhs, location },
                                AftLHS::Dec(location) => UpdateExpression::PostDecrement { lhs, location },
                            }),
                            scan,
                        )
                    })
            })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.update_expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.update_expression_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn location(&self) -> Location {
        match self {
            UpdateExpression::LeftHandSideExpression(lhs) => lhs.location(),
            UpdateExpression::PostIncrement { location, .. }
            | UpdateExpression::PostDecrement { location, .. }
            | UpdateExpression::PreIncrement { location, .. }
            | UpdateExpression::PreDecrement { location, .. } => *location,
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            UpdateExpression::LeftHandSideExpression(lhs)
            | UpdateExpression::PostIncrement { lhs, .. }
            | UpdateExpression::PostDecrement { lhs, .. } => lhs.contains(kind),
            UpdateExpression::PreIncrement { ue, .. } | UpdateExpression::PreDecrement { ue, .. } => ue.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            UpdateExpression::LeftHandSideExpression(n) => n.as_string_literal(),
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
            UpdateExpression::LeftHandSideExpression(lhs)
            | UpdateExpression::PostIncrement { lhs, .. }
            | UpdateExpression::PostDecrement { lhs, .. } => lhs.all_private_identifiers_valid(names),
            UpdateExpression::PreIncrement { ue, .. } | UpdateExpression::PreDecrement { ue, .. } => {
                ue.all_private_identifiers_valid(names)
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
            UpdateExpression::LeftHandSideExpression(lhs)
            | UpdateExpression::PostIncrement { lhs, .. }
            | UpdateExpression::PostDecrement { lhs, .. } => lhs.contains_arguments(),
            UpdateExpression::PreIncrement { ue, .. } | UpdateExpression::PreDecrement { ue, .. } => {
                ue.contains_arguments()
            }
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        match self {
            UpdateExpression::LeftHandSideExpression(n) => n.early_errors(errs, strict),
            UpdateExpression::PostIncrement { lhs: n, .. } | UpdateExpression::PostDecrement { lhs: n, .. } => {
                //  UpdateExpression :
                //      LeftHandSideExpression ++
                //      LeftHandSideExpression --
                // * It is an early Syntax Error if AssignmentTargetType of LeftHandSideExpression is not simple.
                if n.assignment_target_type(strict) != ATTKind::Simple {
                    errs.push(create_syntax_error_object("Invalid target for update", Some(n.location())));
                }
                n.early_errors(errs, strict);
            }
            UpdateExpression::PreIncrement { ue: n, .. } | UpdateExpression::PreDecrement { ue: n, .. } => {
                //  UpdateExpression :
                //      ++ UnaryExpression
                //      -- UnaryExpression
                // * It is an early Syntax Error if AssignmentTargetType of UnaryExpression is not simple.
                if n.assignment_target_type(strict) != ATTKind::Simple {
                    errs.push(create_syntax_error_object("Invalid target for update", Some(n.location())));
                }
                n.early_errors(errs, strict);
            }
        }
    }

    pub fn is_strictly_deletable(&self) -> bool {
        match self {
            UpdateExpression::LeftHandSideExpression(x) => x.is_strictly_deletable(),
            _ => true,
        }
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub fn assignment_target_type(&self, strict: bool) -> ATTKind {
        match self {
            UpdateExpression::LeftHandSideExpression(boxed) => boxed.assignment_target_type(strict),
            UpdateExpression::PostIncrement { .. }
            | UpdateExpression::PostDecrement { .. }
            | UpdateExpression::PreIncrement { .. }
            | UpdateExpression::PreDecrement { .. } => ATTKind::Invalid,
        }
    }

    pub fn is_named_function(&self) -> bool {
        match self {
            UpdateExpression::LeftHandSideExpression(node) => node.is_named_function(),
            _ => false,
        }
    }

    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                UpdateExpression::LeftHandSideExpression(node) => node.body_containing_location(location),
                UpdateExpression::PostIncrement { lhs, .. } | UpdateExpression::PostDecrement { lhs, .. } => {
                    lhs.body_containing_location(location)
                }
                UpdateExpression::PreIncrement { ue, .. } | UpdateExpression::PreDecrement { ue, .. } => {
                    ue.body_containing_location(location)
                }
            }
        } else {
            None
        }
    }

    pub fn has_call_in_tail_position(&self, location: &Location) -> bool {
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
        // UpdateExpression :
        //      LeftHandSideExpression
        //  1. Return HasCallInTailPosition of LeftHandSideExpression with argument call.
        // UpdateExpression :
        //      LeftHandSideExpression  ++
        //      LeftHandSideExpression  --
        //      ++ UnaryExpression
        //      -- UnaryExpression
        //  1. Return false.
        match self {
            UpdateExpression::LeftHandSideExpression(left_hand_side_expression) => {
                left_hand_side_expression.has_call_in_tail_position(location)
            }
            UpdateExpression::PostIncrement { .. }
            | UpdateExpression::PostDecrement { .. }
            | UpdateExpression::PreIncrement { .. }
            | UpdateExpression::PreDecrement { .. } => false,
        }
    }
}

#[cfg(test)]
mod tests;
