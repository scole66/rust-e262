use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// AssignmentExpression[In, Yield, Await] :
//      ConditionalExpression[?In, ?Yield, ?Await]
//      [+Yield]YieldExpression[?In, ?Await]
//      ArrowFunction[?In, ?Yield, ?Await]
//      AsyncArrowFunction[?In, ?Yield, ?Await]
//      LeftHandSideExpression[?Yield, ?Await] = AssignmentExpression[?In, ?Yield, ?Await]
//      LeftHandSideExpression[?Yield, ?Await] AssignmentOperator AssignmentExpression[?In, ?Yield, ?Await]
//      LeftHandSideExpression[?Yield, ?Await] &&= AssignmentExpression[?In, ?Yield, ?Await]
//      LeftHandSideExpression[?Yield, ?Await] ||= AssignmentExpression[?In, ?Yield, ?Await]
//      LeftHandSideExpression[?Yield, ?Await] ??= AssignmentExpression[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum AssignmentExpression {
    FallThru(Rc<ConditionalExpression>),
    Yield(Rc<YieldExpression>),
    Arrow(Rc<ArrowFunction>),
    AsyncArrow(Rc<AsyncArrowFunction>),
    Assignment(Rc<LeftHandSideExpression>, Rc<AssignmentExpression>),
    OpAssignment(Rc<LeftHandSideExpression>, AssignmentOperator, Rc<AssignmentExpression>),
    LandAssignment(Rc<LeftHandSideExpression>, Rc<AssignmentExpression>),
    LorAssignment(Rc<LeftHandSideExpression>, Rc<AssignmentExpression>),
    CoalAssignment(Rc<LeftHandSideExpression>, Rc<AssignmentExpression>),
    Destructuring(Rc<AssignmentPattern>, Rc<AssignmentExpression>),
}

impl fmt::Display for AssignmentExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            AssignmentExpression::FallThru(node) => node.fmt(f),
            AssignmentExpression::Yield(node) => node.fmt(f),
            AssignmentExpression::Arrow(node) => node.fmt(f),
            AssignmentExpression::AsyncArrow(node) => node.fmt(f),
            AssignmentExpression::Assignment(left, right) => {
                write!(f, "{left} = {right}")
            }
            AssignmentExpression::OpAssignment(left, op, right) => {
                write!(f, "{left} {op} {right}")
            }
            AssignmentExpression::LandAssignment(left, right) => {
                write!(f, "{left} &&= {right}")
            }
            AssignmentExpression::LorAssignment(left, right) => {
                write!(f, "{left} ||= {right}")
            }
            AssignmentExpression::CoalAssignment(left, right) => {
                write!(f, "{left} ??= {right}")
            }
            AssignmentExpression::Destructuring(left, right) => {
                write!(f, "{left} = {right}")
            }
        }
    }
}

impl PrettyPrint for AssignmentExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AssignmentExpression: {self}")?;
        match self {
            AssignmentExpression::FallThru(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentExpression::Yield(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentExpression::Arrow(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentExpression::AsyncArrow(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentExpression::Assignment(left, right)
            | AssignmentExpression::LandAssignment(left, right)
            | AssignmentExpression::LorAssignment(left, right)
            | AssignmentExpression::CoalAssignment(left, right) => {
                left.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                right.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::OpAssignment(left, op, right) => {
                left.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                op.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                right.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::Destructuring(pat, exp) => {
                pat.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                exp.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        match self {
            AssignmentExpression::FallThru(node) => node.concise_with_leftpad(writer, pad, state),
            AssignmentExpression::Yield(node) => node.concise_with_leftpad(writer, pad, state),
            AssignmentExpression::Arrow(node) => node.concise_with_leftpad(writer, pad, state),
            AssignmentExpression::AsyncArrow(node) => node.concise_with_leftpad(writer, pad, state),
            AssignmentExpression::Assignment(left, right) => {
                writeln!(writer, "{first}AssignmentExpression: {self}")?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "=", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::OpAssignment(left, op, right) => {
                writeln!(writer, "{first}AssignmentExpression: {self}")?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                op.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::LandAssignment(left, right) => {
                writeln!(writer, "{first}AssignmentExpression: {self}")?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "&&=", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::LorAssignment(left, right) => {
                writeln!(writer, "{first}AssignmentExpression: {self}")?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "||=", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::CoalAssignment(left, right) => {
                writeln!(writer, "{first}AssignmentExpression: {self}")?;
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "??=", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentExpression::Destructuring(pat, exp) => {
                writeln!(writer, "{first}AssignmentExpression: {self}")?;
                pat.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "=", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                exp.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for AssignmentExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            AssignmentExpression::Yield(_)
            | AssignmentExpression::Assignment(..)
            | AssignmentExpression::OpAssignment(..)
            | AssignmentExpression::LandAssignment(..)
            | AssignmentExpression::LorAssignment(..)
            | AssignmentExpression::CoalAssignment(..)
            | AssignmentExpression::Destructuring(..) => false,
            AssignmentExpression::Arrow(_) | AssignmentExpression::AsyncArrow(_) => true,
            AssignmentExpression::FallThru(node) => node.is_function_definition(),
        }
    }
}

impl AssignmentExpression {
    fn parse_core(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let result = Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::AssignmentExpression), scanner))
            .otherwise(|| {
                if yield_flag {
                    YieldExpression::parse(parser, scanner, in_flag, await_flag).map(|(yieldexp, after_yield)| {
                        (Rc::new(AssignmentExpression::Yield(yieldexp)), after_yield, Scanner::new())
                    })
                } else {
                    Err(ParseError::new(PECode::Generic, scanner))
                }
            })
            .otherwise(|| {
                ArrowFunction::parse(parser, scanner, in_flag, yield_flag, await_flag)
                    .map(|(af, after_af)| (Rc::new(AssignmentExpression::Arrow(af)), after_af, Scanner::new()))
            })
            .otherwise(|| {
                AsyncArrowFunction::parse(parser, scanner, in_flag, yield_flag, await_flag)
                    .map(|(aaf, after_aaf)| (Rc::new(AssignmentExpression::AsyncArrow(aaf)), after_aaf, Scanner::new()))
            })
            .otherwise(|| {
                LeftHandSideExpression::parse(parser, scanner, yield_flag, await_flag).and_then(|(lhs, after_lhs)| {
                    scan_for_punct_set(
                        after_lhs,
                        parser.source,
                        ScanGoal::InputElementDiv,
                        &[
                            Punctuator::Eq,
                            Punctuator::AmpAmpEq,
                            Punctuator::PipePipeEq,
                            Punctuator::QQEq,
                            Punctuator::StarEq,
                            Punctuator::SlashEq,
                            Punctuator::PercentEq,
                            Punctuator::PlusEq,
                            Punctuator::MinusEq,
                            Punctuator::LtLtEq,
                            Punctuator::GtGtEq,
                            Punctuator::GtGtGtEq,
                            Punctuator::AmpEq,
                            Punctuator::PipeEq,
                            Punctuator::StarStarEq,
                            Punctuator::CaretEq,
                        ],
                    )
                    .and_then(|(op, _, after_op)| {
                        let make_ae = match op {
                            Punctuator::Eq => AssignmentExpression::Assignment,
                            Punctuator::AmpAmpEq => AssignmentExpression::LandAssignment,
                            Punctuator::PipePipeEq => AssignmentExpression::LorAssignment,
                            Punctuator::QQEq => AssignmentExpression::CoalAssignment,
                            Punctuator::StarEq => {
                                |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Multiply, ae)
                            }
                            Punctuator::SlashEq => {
                                |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Divide, ae)
                            }
                            Punctuator::PercentEq => {
                                |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Modulo, ae)
                            }
                            Punctuator::PlusEq => {
                                |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Add, ae)
                            }
                            Punctuator::MinusEq => {
                                |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Subtract, ae)
                            }
                            Punctuator::LtLtEq => {
                                |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::LeftShift, ae)
                            }
                            Punctuator::GtGtEq => |lhs, ae| {
                                AssignmentExpression::OpAssignment(lhs, AssignmentOperator::SignedRightShift, ae)
                            },
                            Punctuator::GtGtGtEq => |lhs, ae| {
                                AssignmentExpression::OpAssignment(lhs, AssignmentOperator::UnsignedRightShift, ae)
                            },
                            Punctuator::AmpEq => {
                                |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::BitwiseAnd, ae)
                            }
                            Punctuator::PipeEq => {
                                |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::BitwiseOr, ae)
                            }
                            Punctuator::StarStarEq => {
                                |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::Exponentiate, ae)
                            }
                            _ => |lhs, ae| AssignmentExpression::OpAssignment(lhs, AssignmentOperator::BitwiseXor, ae),
                        };
                        AssignmentExpression::parse(parser, after_op, in_flag, yield_flag, await_flag)
                            .map(|(ae, after_ae)| (Rc::new(make_ae(lhs, ae)), after_ae, after_lhs))
                    })
                })
            })
            .otherwise(|| {
                ConditionalExpression::parse(parser, scanner, in_flag, yield_flag, await_flag)
                    .map(|(ce, after_ce)| (Rc::new(AssignmentExpression::FallThru(ce)), after_ce, Scanner::new()))
            })?;

        if let AssignmentExpression::Assignment(lhs, ae) = &*result.0 {
            if lhs.is_object_or_array_literal() {
                // Re-parse the LHS as an AssignmentPattern.
                let (ap, after_ap) = AssignmentPattern::parse(parser, scanner, yield_flag, await_flag)?;
                // Note: because the object/array literals require proper nested brackets/braces, and so do the
                // assignment patterns, we're guaranteed the the text we just parsed as an AssignmentPattern was the
                // same text that was parsed as a literal. There won't ever be an error where this new parse didn't
                // consume all the characters. (No need to write a test to cover this won't-ever-happen error
                // condition.)
                assert_eq!(after_ap, result.2);
                return Ok((Rc::new(AssignmentExpression::Destructuring(ap, ae.clone())), result.1));
            }
        }
        Ok((result.0, result.1))
    }

    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let key = InYieldAwaitKey { scanner, in_flag, yield_flag, await_flag };
        match parser.assignment_expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag, yield_flag, await_flag);
                parser.assignment_expression_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn location(&self) -> Location {
        match self {
            AssignmentExpression::FallThru(exp) => exp.location(),
            AssignmentExpression::Yield(exp) => exp.location(),
            AssignmentExpression::Arrow(exp) => exp.location(),
            AssignmentExpression::AsyncArrow(exp) => exp.location(),
            AssignmentExpression::Assignment(lhs, ae)
            | AssignmentExpression::OpAssignment(lhs, _, ae)
            | AssignmentExpression::LandAssignment(lhs, ae)
            | AssignmentExpression::LorAssignment(lhs, ae)
            | AssignmentExpression::CoalAssignment(lhs, ae) => lhs.location().merge(&ae.location()),
            AssignmentExpression::Destructuring(destructure, ae) => destructure.location().merge(&ae.location()),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            AssignmentExpression::FallThru(node) => kind == ParseNodeKind::ConditionalExpression || node.contains(kind),
            AssignmentExpression::Yield(node) => kind == ParseNodeKind::YieldExpression || node.contains(kind),
            AssignmentExpression::Arrow(node) => kind == ParseNodeKind::ArrowFunction || node.contains(kind),
            AssignmentExpression::AsyncArrow(node) => kind == ParseNodeKind::AsyncArrowFunction || node.contains(kind),
            AssignmentExpression::Assignment(left, right)
            | AssignmentExpression::LandAssignment(left, right)
            | AssignmentExpression::LorAssignment(left, right)
            | AssignmentExpression::CoalAssignment(left, right) => {
                [ParseNodeKind::LeftHandSideExpression, ParseNodeKind::AssignmentExpression].contains(&kind)
                    || left.contains(kind)
                    || right.contains(kind)
            }
            AssignmentExpression::OpAssignment(left, op, right) => {
                [
                    ParseNodeKind::LeftHandSideExpression,
                    ParseNodeKind::AssignmentOperator,
                    ParseNodeKind::AssignmentExpression,
                ]
                .contains(&kind)
                    || left.contains(kind)
                    || op.contains(kind)
                    || right.contains(kind)
            }
            AssignmentExpression::Destructuring(pat, exp) => {
                [ParseNodeKind::AssignmentPattern, ParseNodeKind::AssignmentExpression].contains(&kind)
                    || pat.contains(kind)
                    || exp.contains(kind)
            }
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            AssignmentExpression::FallThru(node) => node.as_string_literal(),
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
            AssignmentExpression::FallThru(node) => node.all_private_identifiers_valid(names),
            AssignmentExpression::Yield(node) => node.all_private_identifiers_valid(names),
            AssignmentExpression::Arrow(node) => node.all_private_identifiers_valid(names),
            AssignmentExpression::AsyncArrow(node) => node.all_private_identifiers_valid(names),
            AssignmentExpression::Assignment(left, right)
            | AssignmentExpression::OpAssignment(left, _, right)
            | AssignmentExpression::LandAssignment(left, right)
            | AssignmentExpression::LorAssignment(left, right)
            | AssignmentExpression::CoalAssignment(left, right) => {
                left.all_private_identifiers_valid(names) && right.all_private_identifiers_valid(names)
            }
            AssignmentExpression::Destructuring(pat, exp) => {
                pat.all_private_identifiers_valid(names) && exp.all_private_identifiers_valid(names)
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
            AssignmentExpression::FallThru(ce) => ce.contains_arguments(),
            AssignmentExpression::Yield(ye) => ye.contains_arguments(),
            AssignmentExpression::Arrow(af) => af.contains_arguments(),
            AssignmentExpression::AsyncArrow(aaf) => aaf.contains_arguments(),
            AssignmentExpression::Assignment(lhse, ae)
            | AssignmentExpression::OpAssignment(lhse, _, ae)
            | AssignmentExpression::LandAssignment(lhse, ae)
            | AssignmentExpression::LorAssignment(lhse, ae)
            | AssignmentExpression::CoalAssignment(lhse, ae) => lhse.contains_arguments() || ae.contains_arguments(),
            AssignmentExpression::Destructuring(ap, ae) => ap.contains_arguments() || ae.contains_arguments(),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        // AssignmentExpression :
        // LeftHandSideExpression AssignmentOperator AssignmentExpression
        // LeftHandSideExpression &&= AssignmentExpression
        // LeftHandSideExpression ||= AssignmentExpression
        // LeftHandSideExpression ??= AssignmentExpression
        // It is a Syntax Error if AssignmentTargetType of LeftHandSideExpression is not simple.
        match self {
            AssignmentExpression::FallThru(node) => node.early_errors(errs, strict),
            AssignmentExpression::Yield(node) => node.early_errors(errs, strict),
            AssignmentExpression::Arrow(node) => node.early_errors(errs, strict),
            AssignmentExpression::AsyncArrow(node) => node.early_errors(errs, strict),
            AssignmentExpression::Assignment(left, right)
            | AssignmentExpression::OpAssignment(left, _, right)
            | AssignmentExpression::LandAssignment(left, right)
            | AssignmentExpression::LorAssignment(left, right)
            | AssignmentExpression::CoalAssignment(left, right) => {
                // AssignmentExpression :
                //      LeftHandSideExpression = AssignmentExpression
                //      LeftHandSideExpression AssignmentOperator AssignmentExpression
                //      LeftHandSideExpression &&= AssignmentExpression
                //      LeftHandSideExpression ||= AssignmentExpression
                //      LeftHandSideExpression ??= AssignmentExpression
                //
                //  * It is a Syntax Error if AssignmentTargetType of LeftHandSideExpression is not simple.
                if left.assignment_target_type(strict) != ATTKind::Simple {
                    errs.push(create_syntax_error_object(
                        "Invalid left-hand side in assignment",
                        Some(left.location()),
                    ));
                }
                left.early_errors(errs, strict);
                right.early_errors(errs, strict);
            }
            AssignmentExpression::Destructuring(pat, exp) => {
                pat.early_errors(errs, strict);
                exp.early_errors(errs, strict);
            }
        }
    }

    pub fn is_strictly_deletable(&self) -> bool {
        match self {
            AssignmentExpression::FallThru(node) => node.is_strictly_deletable(),
            _ => true,
        }
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub fn assignment_target_type(&self, strict: bool) -> ATTKind {
        match self {
            AssignmentExpression::Yield(_)
            | AssignmentExpression::Arrow(_)
            | AssignmentExpression::AsyncArrow(_)
            | AssignmentExpression::Assignment(_, _)
            | AssignmentExpression::OpAssignment(_, _, _)
            | AssignmentExpression::LandAssignment(_, _)
            | AssignmentExpression::LorAssignment(_, _)
            | AssignmentExpression::CoalAssignment(_, _)
            | AssignmentExpression::Destructuring(..) => ATTKind::Invalid,
            AssignmentExpression::FallThru(node) => node.assignment_target_type(strict),
        }
    }

    /// Determine if this parse node is an anonymous function
    ///
    /// See [IsAnonymousFunctionDefinition](https://tc39.es/ecma262/#sec-isanonymousfunctiondefinition) in ECMA-262.
    pub fn is_anonymous_function_definition(&self) -> bool {
        self.is_function_definition() && !self.is_named_function()
    }

    pub fn anonymous_function_definition(&self) -> Option<NameableProduction> {
        match self.function_definition() {
            Some(fd) if !fd.is_named_function() => Some(fd),
            Some(_) | None => None,
        }
    }

    pub fn function_definition(&self) -> Option<NameableProduction> {
        match self {
            AssignmentExpression::FallThru(node) => NameableProduction::try_from(node.clone()).ok(),
            AssignmentExpression::Arrow(node) => Some(NameableProduction::Arrow(node.clone())),
            AssignmentExpression::AsyncArrow(node) => Some(NameableProduction::AsyncArrow(node.clone())),
            AssignmentExpression::Yield(_)
            | AssignmentExpression::Assignment(_, _)
            | AssignmentExpression::OpAssignment(_, _, _)
            | AssignmentExpression::LandAssignment(_, _)
            | AssignmentExpression::LorAssignment(_, _)
            | AssignmentExpression::CoalAssignment(_, _)
            | AssignmentExpression::Destructuring(_, _) => None,
        }
    }

    pub fn is_named_function(&self) -> bool {
        match self {
            AssignmentExpression::Yield(_)
            | AssignmentExpression::Assignment(..)
            | AssignmentExpression::OpAssignment(..)
            | AssignmentExpression::LandAssignment(..)
            | AssignmentExpression::LorAssignment(..)
            | AssignmentExpression::CoalAssignment(..)
            | AssignmentExpression::Destructuring(..)
            | AssignmentExpression::Arrow(..)
            | AssignmentExpression::AsyncArrow(..) => false,
            AssignmentExpression::FallThru(node) => node.is_named_function(),
        }
    }

    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                AssignmentExpression::FallThru(node) => node.body_containing_location(location),
                AssignmentExpression::Yield(node) => node.body_containing_location(location),
                AssignmentExpression::Arrow(node) => node.body_containing_location(location),
                AssignmentExpression::AsyncArrow(node) => node.body_containing_location(location),
                AssignmentExpression::Assignment(lhs, ae) => {
                    lhs.body_containing_location(location).or_else(|| ae.body_containing_location(location))
                }
                AssignmentExpression::OpAssignment(lhs, _, ae) => {
                    lhs.body_containing_location(location).or_else(|| ae.body_containing_location(location))
                }
                AssignmentExpression::LandAssignment(lhs, ae) => {
                    lhs.body_containing_location(location).or_else(|| ae.body_containing_location(location))
                }
                AssignmentExpression::LorAssignment(left_hand_side_expression, assignment_expression) => {
                    left_hand_side_expression
                        .body_containing_location(location)
                        .or_else(|| assignment_expression.body_containing_location(location))
                }
                AssignmentExpression::CoalAssignment(left_hand_side_expression, assignment_expression) => {
                    left_hand_side_expression
                        .body_containing_location(location)
                        .or_else(|| assignment_expression.body_containing_location(location))
                }
                AssignmentExpression::Destructuring(assignment_pattern, assignment_expression) => assignment_pattern
                    .body_containing_location(location)
                    .or_else(|| assignment_expression.body_containing_location(location)),
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
        // AssignmentExpression :
        //      YieldExpression
        //      ArrowFunction
        //      AsyncArrowFunction
        //      LeftHandSideExpression = AssignmentExpression
        //      LeftHandSideExpression AssignmentOperator AssignmentExpression
        //      LeftHandSideExpression &&= AssignmentExpression
        //      LeftHandSideExpression ||= AssignmentExpression
        //      LeftHandSideExpression ??= AssignmentExpression
        //  1. Return false.
        // AssignementExpression :
        //      ConditionalExpression
        //  1. Return HasCallInTailPosition of ConditionalExpression with argument call.
        match self {
            AssignmentExpression::FallThru(node) => node.has_call_in_tail_position(location),
            AssignmentExpression::Yield(_)
            | AssignmentExpression::Arrow(_)
            | AssignmentExpression::AsyncArrow(_)
            | AssignmentExpression::Assignment(_, _)
            | AssignmentExpression::OpAssignment(_, _, _)
            | AssignmentExpression::LandAssignment(_, _)
            | AssignmentExpression::LorAssignment(_, _)
            | AssignmentExpression::CoalAssignment(_, _)
            | AssignmentExpression::Destructuring(_, _) => false,
        }
    }
}

// AssignmentOperator : one of
//      *= /= %= += -= <<= >>= >>>= &= ^= |= **=
#[derive(Debug, Copy, Clone)]
pub enum AssignmentOperator {
    Multiply,
    Divide,
    Modulo,
    Add,
    Subtract,
    LeftShift,
    SignedRightShift,
    UnsignedRightShift,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    Exponentiate,
}

impl fmt::Display for AssignmentOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            AssignmentOperator::Multiply => write!(f, "*="),
            AssignmentOperator::Divide => write!(f, "/="),
            AssignmentOperator::Modulo => write!(f, "%="),
            AssignmentOperator::Add => write!(f, "+="),
            AssignmentOperator::Subtract => write!(f, "-="),
            AssignmentOperator::LeftShift => write!(f, "<<="),
            AssignmentOperator::SignedRightShift => write!(f, ">>="),
            AssignmentOperator::UnsignedRightShift => write!(f, ">>>="),
            AssignmentOperator::BitwiseAnd => write!(f, "&="),
            AssignmentOperator::BitwiseXor => write!(f, "^="),
            AssignmentOperator::BitwiseOr => write!(f, "|="),
            AssignmentOperator::Exponentiate => write!(f, "**="),
        }
    }
}

impl PrettyPrint for AssignmentOperator {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{first}AssignmentOperator: {self}")
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        pprint_token(writer, self, TokenType::Punctuator, pad, state)
    }
}

impl AssignmentOperator {
    pub fn contains(&self, _kind: ParseNodeKind) -> bool {
        false
    }
}

// AssignmentPattern[Yield, Await] :
//      ObjectAssignmentPattern[?Yield, ?Await]
//      ArrayAssignmentPattern[?Yield, ?Await]
#[derive(Debug)]
pub enum AssignmentPattern {
    Object(Rc<ObjectAssignmentPattern>),
    Array(Rc<ArrayAssignmentPattern>),
}

impl fmt::Display for AssignmentPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AssignmentPattern::Object(obj) => obj.fmt(f),
            AssignmentPattern::Array(ary) => ary.fmt(f),
        }
    }
}

impl PrettyPrint for AssignmentPattern {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AssignmentPattern: {self}")?;
        match self {
            AssignmentPattern::Object(obj) => obj.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentPattern::Array(ary) => ary.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            AssignmentPattern::Object(obj) => obj.concise_with_leftpad(writer, pad, state),
            AssignmentPattern::Array(ary) => ary.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl AssignmentPattern {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::AssignmentPattern), scanner))
            .otherwise(|| {
                ObjectAssignmentPattern::parse(parser, scanner, yield_flag, await_flag)
                    .map(|(oap, after_oap)| (Rc::new(AssignmentPattern::Object(oap)), after_oap))
            })
            .otherwise(|| {
                ArrayAssignmentPattern::parse(parser, scanner, yield_flag, await_flag)
                    .map(|(aap, after_aap)| (Rc::new(AssignmentPattern::Array(aap)), after_aap))
            })
    }

    pub fn location(&self) -> Location {
        match self {
            AssignmentPattern::Object(pattern) => pattern.location(),
            AssignmentPattern::Array(pattern) => pattern.location(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            AssignmentPattern::Object(obj) => obj.contains(kind),
            AssignmentPattern::Array(ary) => ary.contains(kind),
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
            AssignmentPattern::Object(obj) => obj.all_private_identifiers_valid(names),
            AssignmentPattern::Array(ary) => ary.all_private_identifiers_valid(names),
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
            AssignmentPattern::Object(oap) => oap.contains_arguments(),
            AssignmentPattern::Array(aap) => aap.contains_arguments(),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            AssignmentPattern::Object(obj) => obj.early_errors(errs, strict),
            AssignmentPattern::Array(ary) => ary.early_errors(errs, strict),
        }
    }

    pub fn is_destructuring(&self) -> bool {
        true
    }

    #[expect(unused_variables)]
    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        todo!()
    }
}

/// A parse node for _[ObjectAssignmentPattern][1]_
///
/// It encapsulates this production:
/// ```plain
/// ObjectAssignmentPattern[Yield, Await] :
///      { }
///      { AssignmentRestProperty[?Yield, ?Await] }
///      { AssignmentPropertyList[?Yield, ?Await] }
///      { AssignmentPropertyList[?Yield, ?Await] , AssignmentRestProperty[?Yield, ?Await]opt }
/// ```
///
/// [1]: https://tc39.es/ecma262/#prod-ObjectAssignmentPattern
#[derive(Debug)]
pub enum ObjectAssignmentPattern {
    Empty { location: Location },
    RestOnly { arp: Rc<AssignmentRestProperty>, location: Location },
    ListOnly { apl: Rc<AssignmentPropertyList>, location: Location },
    ListRest { apl: Rc<AssignmentPropertyList>, arp: Option<Rc<AssignmentRestProperty>>, location: Location },
}

impl fmt::Display for ObjectAssignmentPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ObjectAssignmentPattern::Empty { .. } => write!(f, "{{ }}"),
            ObjectAssignmentPattern::RestOnly { arp, .. } => write!(f, "{{ {arp} }}"),
            ObjectAssignmentPattern::ListOnly { apl, .. } => write!(f, "{{ {apl} }}"),
            ObjectAssignmentPattern::ListRest { apl, arp: None, .. } => {
                write!(f, "{{ {apl} , }}")
            }
            ObjectAssignmentPattern::ListRest { apl, arp: Some(arp), .. } => {
                write!(f, "{{ {apl} , {arp} }}")
            }
        }
    }
}

impl PrettyPrint for ObjectAssignmentPattern {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ObjectAssignmentPattern: {self}")?;
        match self {
            ObjectAssignmentPattern::Empty { .. } => Ok(()),
            ObjectAssignmentPattern::RestOnly { arp, .. } => arp.pprint_with_leftpad(writer, &successive, Spot::Final),
            ObjectAssignmentPattern::ListOnly { apl, .. }
            | ObjectAssignmentPattern::ListRest { apl, arp: None, .. } => {
                apl.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ObjectAssignmentPattern::ListRest { apl, arp: Some(arp), .. } => {
                apl.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                arp.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ObjectAssignmentPattern: {self}")?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        if let ObjectAssignmentPattern::ListOnly { apl, .. } | ObjectAssignmentPattern::ListRest { apl, .. } = self {
            apl.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        if matches!(self, ObjectAssignmentPattern::ListRest { .. }) {
            pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        }
        if let ObjectAssignmentPattern::RestOnly { arp, .. }
        | ObjectAssignmentPattern::ListRest { arp: Some(arp), .. } = self
        {
            arp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ObjectAssignmentPattern {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (open_loc, after_brace) =
            scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBrace)?;
        Err(ParseError::new(PECode::ObjectAssignmentPatternEndFailure, after_brace))
            .otherwise(|| {
                let (apl, after_apl) = AssignmentPropertyList::parse(parser, after_brace, yield_flag, await_flag)?;
                let (punct, punct_loc, after_punct) = scan_for_punct_set(
                    after_apl,
                    parser.source,
                    ScanGoal::InputElementDiv,
                    &[Punctuator::Comma, Punctuator::RightBrace],
                )?;
                match punct {
                    Punctuator::RightBrace => Ok((
                        Rc::new(ObjectAssignmentPattern::ListOnly { apl, location: open_loc.merge(&punct_loc) }),
                        after_punct,
                    )),
                    _ => {
                        let (arp, after_arp) =
                            match AssignmentRestProperty::parse(parser, after_punct, yield_flag, await_flag) {
                                Ok((node, scan)) => (Some(node), scan),
                                Err(_) => (None, after_punct),
                            };
                        let (rb_loc, after_close) = scan_for_punct(
                            after_arp,
                            parser.source,
                            ScanGoal::InputElementDiv,
                            Punctuator::RightBrace,
                        )?;
                        Ok((
                            Rc::new(ObjectAssignmentPattern::ListRest { apl, arp, location: open_loc.merge(&rb_loc) }),
                            after_close,
                        ))
                    }
                }
            })
            .otherwise(|| {
                let (arp, after_arp) = AssignmentRestProperty::parse(parser, after_brace, yield_flag, await_flag)?;
                let (rb_loc, after_close) =
                    scan_for_punct(after_arp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                Ok((Rc::new(ObjectAssignmentPattern::RestOnly { arp, location: open_loc.merge(&rb_loc) }), after_close))
            })
            .otherwise(|| {
                let (rb_loc, after_close) =
                    scan_for_punct(after_brace, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                Ok((Rc::new(ObjectAssignmentPattern::Empty { location: open_loc.merge(&rb_loc) }), after_close))
            })
    }

    pub fn location(&self) -> Location {
        match self {
            ObjectAssignmentPattern::Empty { location }
            | ObjectAssignmentPattern::RestOnly { location, .. }
            | ObjectAssignmentPattern::ListOnly { location, .. }
            | ObjectAssignmentPattern::ListRest { location, .. } => *location,
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ObjectAssignmentPattern::Empty { .. } => false,
            ObjectAssignmentPattern::RestOnly { arp, .. } => arp.contains(kind),
            ObjectAssignmentPattern::ListOnly { apl, .. }
            | ObjectAssignmentPattern::ListRest { apl, arp: None, .. } => apl.contains(kind),
            ObjectAssignmentPattern::ListRest { apl, arp: Some(arp), .. } => apl.contains(kind) || arp.contains(kind),
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
            ObjectAssignmentPattern::Empty { .. } => true,
            ObjectAssignmentPattern::RestOnly { arp, .. } => arp.all_private_identifiers_valid(names),
            ObjectAssignmentPattern::ListOnly { apl, .. }
            | ObjectAssignmentPattern::ListRest { apl, arp: None, .. } => apl.all_private_identifiers_valid(names),
            ObjectAssignmentPattern::ListRest { apl, arp: Some(apr), .. } => {
                apl.all_private_identifiers_valid(names) && apr.all_private_identifiers_valid(names)
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
            ObjectAssignmentPattern::Empty { .. } => false,
            ObjectAssignmentPattern::RestOnly { arp, .. } => arp.contains_arguments(),
            ObjectAssignmentPattern::ListOnly { apl, .. }
            | ObjectAssignmentPattern::ListRest { apl, arp: None, .. } => apl.contains_arguments(),
            ObjectAssignmentPattern::ListRest { apl, arp: Some(arp), .. } => {
                apl.contains_arguments() || arp.contains_arguments()
            }
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ObjectAssignmentPattern::Empty { .. } => (),
            ObjectAssignmentPattern::RestOnly { arp, .. } => arp.early_errors(errs, strict),
            ObjectAssignmentPattern::ListOnly { apl, .. }
            | ObjectAssignmentPattern::ListRest { apl, arp: None, .. } => apl.early_errors(errs, strict),
            ObjectAssignmentPattern::ListRest { apl, arp: Some(apr), .. } => {
                apl.early_errors(errs, strict);
                apr.early_errors(errs, strict);
            }
        }
    }

    #[expect(unused_variables)]
    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        todo!()
    }
}

// ArrayAssignmentPattern[Yield, Await] :
//      [ Elision_opt AssignmentRestElement[?Yield, ?Await]opt ]
//      [ AssignmentElementList[?Yield, ?Await] ]
//      [ AssignmentElementList[?Yield, ?Await] , Elision_opt AssignmentRestElement[?Yield, ?Await]opt ]
#[derive(Debug)]
pub enum ArrayAssignmentPattern {
    RestOnly {
        elision: Option<Rc<Elisions>>,
        are: Option<Rc<AssignmentRestElement>>,
        location: Location,
    },
    ListOnly {
        ael: Rc<AssignmentElementList>,
        location: Location,
    },
    ListRest {
        ael: Rc<AssignmentElementList>,
        elision: Option<Rc<Elisions>>,
        are: Option<Rc<AssignmentRestElement>>,
        location: Location,
    },
}

impl fmt::Display for ArrayAssignmentPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArrayAssignmentPattern::RestOnly { elision: None, are: None, .. } => write!(f, "[ ]"),
            ArrayAssignmentPattern::RestOnly { elision: Some(elisions), are: None, .. } => {
                write!(f, "[ {elisions} ]")
            }
            ArrayAssignmentPattern::RestOnly { elision: None, are: Some(are), .. } => {
                write!(f, "[ {are} ]")
            }
            ArrayAssignmentPattern::RestOnly { elision: Some(elisions), are: Some(are), .. } => {
                write!(f, "[ {elisions} {are} ]")
            }
            ArrayAssignmentPattern::ListOnly { ael, .. } => write!(f, "[ {ael} ]"),
            ArrayAssignmentPattern::ListRest { ael, elision: None, are: None, .. } => write!(f, "[ {ael} , ]"),
            ArrayAssignmentPattern::ListRest { ael, elision: Some(elisions), are: None, .. } => {
                write!(f, "[ {ael} , {elisions} ]")
            }
            ArrayAssignmentPattern::ListRest { ael, elision: None, are: Some(are), .. } => {
                write!(f, "[ {ael} , {are} ]")
            }
            ArrayAssignmentPattern::ListRest { ael, elision: Some(elisions), are: Some(are), .. } => {
                write!(f, "[ {ael} , {elisions} {are} ]")
            }
        }
    }
}

impl PrettyPrint for ArrayAssignmentPattern {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ArrayAssignmentPattern: {self}")?;
        match self {
            ArrayAssignmentPattern::RestOnly { elision: None, are: None, .. } => Ok(()),
            ArrayAssignmentPattern::RestOnly { elision: Some(elisions), are: None, .. } => {
                elisions.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayAssignmentPattern::RestOnly { elision: None, are: Some(are), .. } => {
                are.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayAssignmentPattern::RestOnly { elision: Some(elisions), are: Some(are), .. } => {
                elisions.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                are.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayAssignmentPattern::ListOnly { ael, .. }
            | ArrayAssignmentPattern::ListRest { ael, elision: None, are: None, .. } => {
                ael.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayAssignmentPattern::ListRest { ael, elision: Some(elisions), are: None, .. } => {
                ael.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                elisions.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayAssignmentPattern::ListRest { ael, elision: None, are: Some(are), .. } => {
                ael.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                are.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayAssignmentPattern::ListRest { ael, elision: Some(elisions), are: Some(are), .. } => {
                ael.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                elisions.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                are.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ArrayAssignmentPattern: {self}")?;
        pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        if let ArrayAssignmentPattern::ListOnly { ael, .. } | ArrayAssignmentPattern::ListRest { ael, .. } = self {
            ael.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        if matches!(self, ArrayAssignmentPattern::ListRest { .. }) {
            pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        }
        if let ArrayAssignmentPattern::RestOnly { elision: Some(e), .. }
        | ArrayAssignmentPattern::ListRest { elision: Some(e), .. } = self
        {
            e.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        if let ArrayAssignmentPattern::RestOnly { are: Some(are), .. }
        | ArrayAssignmentPattern::ListRest { are: Some(are), .. } = self
        {
            are.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ArrayAssignmentPattern {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (open_loc, after_open) =
            scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBracket)?;
        Err(ParseError::new(PECode::ArrayAssignmentPatternEndFailure, after_open))
            .otherwise(|| {
                let (ael, after_el) = AssignmentElementList::parse(parser, after_open, yield_flag, await_flag)?;
                let (punct, punct_loc, after_punct) = scan_for_punct_set(
                    after_el,
                    parser.source,
                    ScanGoal::InputElementDiv,
                    &[Punctuator::Comma, Punctuator::RightBracket],
                )?;
                match punct {
                    Punctuator::RightBracket => Ok((
                        Rc::new({
                            let location = open_loc.merge(&punct_loc);
                            ArrayAssignmentPattern::ListOnly { ael, location }
                        }),
                        after_punct,
                    )),
                    _ => {
                        let (elision, after_elisions) = match Elisions::parse(parser, after_punct) {
                            Ok((node, scan)) => (Some(node), scan),
                            Err(_) => (None, after_punct),
                        };
                        let (are, after_are) =
                            match AssignmentRestElement::parse(parser, after_elisions, yield_flag, await_flag) {
                                Ok((node, scan)) => (Some(node), scan),
                                Err(_) => (None, after_elisions),
                            };
                        let (close_loc, after_close) = scan_for_punct(
                            after_are,
                            parser.source,
                            ScanGoal::InputElementDiv,
                            Punctuator::RightBracket,
                        )?;
                        let location = open_loc.merge(&close_loc);
                        Ok((Rc::new(ArrayAssignmentPattern::ListRest { ael, elision, are, location }), after_close))
                    }
                }
            })
            .otherwise(|| {
                let (elision, after_elisions) = match Elisions::parse(parser, after_open) {
                    Ok((node, scan)) => (Some(node), scan),
                    Err(_) => (None, after_open),
                };
                let (are, after_are) =
                    match AssignmentRestElement::parse(parser, after_elisions, yield_flag, await_flag) {
                        Ok((node, scan)) => (Some(node), scan),
                        Err(_) => (None, after_elisions),
                    };
                let (close_loc, after_close) =
                    scan_for_punct(after_are, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBracket)?;
                let location = open_loc.merge(&close_loc);
                Ok((Rc::new(ArrayAssignmentPattern::RestOnly { elision, are, location }), after_close))
            })
    }

    pub fn location(&self) -> Location {
        match self {
            ArrayAssignmentPattern::RestOnly { location, .. }
            | ArrayAssignmentPattern::ListOnly { location, .. }
            | ArrayAssignmentPattern::ListRest { location, .. } => *location,
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ArrayAssignmentPattern::RestOnly { are: None, .. } => false,
            ArrayAssignmentPattern::RestOnly { are: Some(are), .. } => are.contains(kind),
            ArrayAssignmentPattern::ListOnly { ael, .. } | ArrayAssignmentPattern::ListRest { ael, are: None, .. } => {
                ael.contains(kind)
            }
            ArrayAssignmentPattern::ListRest { ael, are: Some(are), .. } => ael.contains(kind) || are.contains(kind),
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
            ArrayAssignmentPattern::RestOnly { are: None, .. } => true,
            ArrayAssignmentPattern::RestOnly { are: Some(are), .. } => are.all_private_identifiers_valid(names),
            ArrayAssignmentPattern::ListOnly { ael, .. } | ArrayAssignmentPattern::ListRest { ael, are: None, .. } => {
                ael.all_private_identifiers_valid(names)
            }
            ArrayAssignmentPattern::ListRest { ael, are: Some(are), .. } => {
                ael.all_private_identifiers_valid(names) && are.all_private_identifiers_valid(names)
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
            ArrayAssignmentPattern::RestOnly { are: None, .. } => false,
            ArrayAssignmentPattern::RestOnly { are: Some(are), .. } => are.contains_arguments(),
            ArrayAssignmentPattern::ListOnly { ael, .. } | ArrayAssignmentPattern::ListRest { ael, are: None, .. } => {
                ael.contains_arguments()
            }
            ArrayAssignmentPattern::ListRest { ael, are: Some(are), .. } => {
                ael.contains_arguments() || are.contains_arguments()
            }
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ArrayAssignmentPattern::RestOnly { are: None, .. } => (),
            ArrayAssignmentPattern::RestOnly { are: Some(are), .. } => are.early_errors(errs, strict),
            ArrayAssignmentPattern::ListOnly { ael, .. } | ArrayAssignmentPattern::ListRest { ael, are: None, .. } => {
                ael.early_errors(errs, strict);
            }
            ArrayAssignmentPattern::ListRest { ael, are: Some(are), .. } => {
                ael.early_errors(errs, strict);
                are.early_errors(errs, strict);
            }
        }
    }

    #[expect(unused_variables)]
    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        todo!()
    }
}

// AssignmentRestProperty[Yield, Await] :
//      ... DestructuringAssignmentTarget[?Yield, ?Await]
#[derive(Debug)]
pub struct AssignmentRestProperty(pub Rc<DestructuringAssignmentTarget>);

impl fmt::Display for AssignmentRestProperty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "... {}", self.0)
    }
}

impl PrettyPrint for AssignmentRestProperty {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AssignmentRestProperty: {self}")?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AssignmentRestProperty: {self}")?;
        pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.0.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl AssignmentRestProperty {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (_, after_dots) = scan_for_punct(scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Ellipsis)?;
        let (dat, after_dat) = DestructuringAssignmentTarget::parse(parser, after_dots, yield_flag, await_flag)?;
        Ok((Rc::new(AssignmentRestProperty(dat)), after_dat))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.0.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.0.all_private_identifiers_valid(names)
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
        self.0.contains_arguments()
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        // AssignmentRestProperty : ... DestructuringAssignmentTarget
        //  * It is a Syntax Error if DestructuringAssignmentTarget is an ArrayLiteral or an ObjectLiteral.
        if let DestructuringAssignmentTarget::AssignmentPattern(pat) = &*self.0 {
            // e.g.: ({...{a}}=b)
            errs.push(create_syntax_error_object(
                "`...` must be followed by an assignable reference in assignment contexts",
                Some(pat.location()),
            ));
        }
        self.0.early_errors(errs, strict);
    }

    #[expect(unused_variables)]
    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        todo!()
    }
}

// AssignmentPropertyList[Yield, Await] :
//      AssignmentProperty[?Yield, ?Await]
//      AssignmentPropertyList[?Yield, ?Await] , AssignmentProperty[?Yield, ?Await]
#[derive(Debug)]
pub enum AssignmentPropertyList {
    Item(Rc<AssignmentProperty>),
    List(Rc<AssignmentPropertyList>, Rc<AssignmentProperty>),
}

impl fmt::Display for AssignmentPropertyList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AssignmentPropertyList::Item(item) => item.fmt(f),
            AssignmentPropertyList::List(lst, item) => {
                write!(f, "{lst} , {item}")
            }
        }
    }
}

impl PrettyPrint for AssignmentPropertyList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AssignmentPropertyList: {self}")?;
        match self {
            AssignmentPropertyList::Item(item) => item.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentPropertyList::List(lst, item) => {
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
            AssignmentPropertyList::Item(item) => item.concise_with_leftpad(writer, pad, state),
            AssignmentPropertyList::List(lst, item) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{first}AssignmentPropertyList: {self}")?;
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl AssignmentPropertyList {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (item, after_item) = AssignmentProperty::parse(parser, scanner, yield_flag, await_flag)?;
        let mut current_production = Rc::new(AssignmentPropertyList::Item(item));
        let mut current_scanner = after_item;
        while let Ok((next_item, after_next)) =
            scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
                .and_then(|(_, after_comma)| AssignmentProperty::parse(parser, after_comma, yield_flag, await_flag))
        {
            current_production = Rc::new(AssignmentPropertyList::List(current_production, next_item));
            current_scanner = after_next;
        }
        Ok((current_production, current_scanner))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            AssignmentPropertyList::Item(item) => item.contains(kind),
            AssignmentPropertyList::List(lst, item) => lst.contains(kind) || item.contains(kind),
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
            AssignmentPropertyList::Item(item) => item.all_private_identifiers_valid(names),
            AssignmentPropertyList::List(list, item) => {
                list.all_private_identifiers_valid(names) && item.all_private_identifiers_valid(names)
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
            AssignmentPropertyList::Item(ap) => ap.contains_arguments(),
            AssignmentPropertyList::List(apl, ap) => apl.contains_arguments() || ap.contains_arguments(),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            AssignmentPropertyList::Item(item) => item.early_errors(errs, strict),
            AssignmentPropertyList::List(list, item) => {
                list.early_errors(errs, strict);
                item.early_errors(errs, strict);
            }
        }
    }

    #[expect(unused_variables)]
    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        todo!()
    }
}

// AssignmentElementList[Yield, Await] :
//      AssignmentElisionElement[?Yield, ?Await]
//      AssignmentElementList[?Yield, ?Await] , AssignmentElisionElement[?Yield, ?Await]
#[derive(Debug)]
pub enum AssignmentElementList {
    Item(Rc<AssignmentElisionElement>),
    List(Rc<AssignmentElementList>, Rc<AssignmentElisionElement>),
}

impl fmt::Display for AssignmentElementList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AssignmentElementList::Item(item) => item.fmt(f),
            AssignmentElementList::List(list, item) => {
                write!(f, "{list} , {item}")
            }
        }
    }
}

impl PrettyPrint for AssignmentElementList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AssignmentElementList: {self}")?;
        match self {
            AssignmentElementList::Item(item) => item.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentElementList::List(list, item) => {
                list.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            AssignmentElementList::Item(item) => item.concise_with_leftpad(writer, pad, state),
            AssignmentElementList::List(list, item) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{first}AssignmentElementList: {self}")?;
                list.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl AssignmentElementList {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (item, after_item) = AssignmentElisionElement::parse(parser, scanner, yield_flag, await_flag)?;
        let mut current_production = Rc::new(AssignmentElementList::Item(item));
        let mut current_scanner = after_item;
        while let Ok((next_item, after_next)) =
            scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma).and_then(
                |(_, after_comma)| AssignmentElisionElement::parse(parser, after_comma, yield_flag, await_flag),
            )
        {
            current_production = Rc::new(AssignmentElementList::List(current_production, next_item));
            current_scanner = after_next;
        }
        Ok((current_production, current_scanner))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            AssignmentElementList::Item(item) => item.contains(kind),
            AssignmentElementList::List(list, item) => list.contains(kind) || item.contains(kind),
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
            AssignmentElementList::Item(item) => item.all_private_identifiers_valid(names),
            AssignmentElementList::List(list, item) => {
                list.all_private_identifiers_valid(names) && item.all_private_identifiers_valid(names)
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
            AssignmentElementList::Item(aee) => aee.contains_arguments(),
            AssignmentElementList::List(ael, aee) => ael.contains_arguments() || aee.contains_arguments(),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            AssignmentElementList::Item(item) => item.early_errors(errs, strict),
            AssignmentElementList::List(list, item) => {
                list.early_errors(errs, strict);
                item.early_errors(errs, strict);
            }
        }
    }

    #[expect(unused_variables)]
    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        todo!()
    }
}

// AssignmentElisionElement[Yield, Await] :
//      Elision_opt AssignmentElement[?Yield, ?Await]
#[derive(Debug)]
pub struct AssignmentElisionElement {
    pub elisions: Option<Rc<Elisions>>,
    pub element: Rc<AssignmentElement>,
}

impl fmt::Display for AssignmentElisionElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.elisions {
            Some(elisions) => write!(f, "{} {}", elisions, self.element),
            None => self.element.fmt(f),
        }
    }
}

impl PrettyPrint for AssignmentElisionElement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AssignmentElisionElement: {self}")?;
        if let Some(elisions) = &self.elisions {
            elisions.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        self.element.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match &self.elisions {
            Some(elisions) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{first}AssignmentElisionElement: {self}")?;
                elisions.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                self.element.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            None => self.element.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl AssignmentElisionElement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (elisions, after_elision) = match Elisions::parse(parser, scanner) {
            Ok((node, scan)) => (Some(node), scan),
            Err(_) => (None, scanner),
        };
        let (element, after_ae) = AssignmentElement::parse(parser, after_elision, yield_flag, await_flag)?;
        Ok((Rc::new(AssignmentElisionElement { elisions, element }), after_ae))
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

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.element.early_errors(errs, strict);
    }

    #[expect(unused_variables)]
    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        todo!()
    }
}

// AssignmentProperty[Yield, Await] :
//      IdentifierReference[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]opt
//      PropertyName[?Yield, ?Await] : AssignmentElement[?Yield, ?Await]
#[derive(Debug)]
pub enum AssignmentProperty {
    Ident(Rc<IdentifierReference>, Option<Rc<Initializer>>),
    Property(Rc<PropertyName>, Rc<AssignmentElement>),
}

impl fmt::Display for AssignmentProperty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AssignmentProperty::Ident(id, None) => id.fmt(f),
            AssignmentProperty::Ident(id, Some(init)) => write!(f, "{id} {init}"),
            AssignmentProperty::Property(name, ae) => write!(f, "{name} : {ae}"),
        }
    }
}

impl PrettyPrint for AssignmentProperty {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AssignmentProperty: {self}")?;
        match self {
            AssignmentProperty::Ident(id, None) => id.pprint_with_leftpad(writer, &successive, Spot::Final),
            AssignmentProperty::Ident(id, Some(init)) => {
                id.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                init.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentProperty::Property(name, ae) => {
                name.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ae.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let write_head = |writer: &mut T, pad, state| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{first}AssignmentProperty: {self}")?;
            IoResult::<String>::Ok(successive)
        };

        match self {
            AssignmentProperty::Ident(id, None) => id.concise_with_leftpad(writer, pad, state),
            AssignmentProperty::Ident(id, Some(init)) => {
                let successive = write_head(writer, pad, state)?;
                id.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                init.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            AssignmentProperty::Property(name, ae) => {
                let successive = write_head(writer, pad, state)?;
                name.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                ae.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl AssignmentProperty {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::IdRefOrPropertyNameExpected, scanner))
            .otherwise(|| {
                let (name, after_name) = PropertyName::parse(parser, scanner, yield_flag, await_flag)?;
                let (_, after_colon) =
                    scan_for_punct(after_name, parser.source, ScanGoal::InputElementDiv, Punctuator::Colon)?;
                let (element, after_element) = AssignmentElement::parse(parser, after_colon, yield_flag, await_flag)?;
                Ok((Rc::new(AssignmentProperty::Property(name, element)), after_element))
            })
            .otherwise(|| {
                let (idref, after_id) = IdentifierReference::parse(parser, scanner, yield_flag, await_flag)?;
                let (init, after_init) = match Initializer::parse(parser, after_id, true, yield_flag, await_flag) {
                    Ok((node, scan)) => (Some(node), scan),
                    Err(_) => (None, after_id),
                };
                Ok((Rc::new(AssignmentProperty::Ident(idref, init)), after_init))
            })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            AssignmentProperty::Ident(id, None) => id.contains(kind),
            AssignmentProperty::Ident(id, Some(init)) => id.contains(kind) || init.contains(kind),
            AssignmentProperty::Property(name, element) => name.contains(kind) || element.contains(kind),
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
            AssignmentProperty::Ident(_, None) => true,
            AssignmentProperty::Ident(_, Some(init)) => init.all_private_identifiers_valid(names),
            AssignmentProperty::Property(name, element) => {
                name.all_private_identifiers_valid(names) && element.all_private_identifiers_valid(names)
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
            AssignmentProperty::Ident(ir, None) => ir.contains_arguments(),
            AssignmentProperty::Ident(ir, Some(izer)) => ir.contains_arguments() || izer.contains_arguments(),
            AssignmentProperty::Property(pn, ae) => pn.contains_arguments() || ae.contains_arguments(),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        // AssignmentProperty : IdentifierReference Initializeropt
        //  * It is a Syntax Error if AssignmentTargetType of IdentifierReference is not simple.
        match self {
            AssignmentProperty::Ident(idref, izer) => {
                if idref.assignment_target_type(strict) != ATTKind::Simple {
                    // node.js reports:
                    //     "Unexpected eval or arguments in strict mode"
                    // But that feels like it knows too much about the cause for !Simple. Which might mean that we
                    // should have a different API for the "simple or not" query. Something like validate_simple_target
                    // that returns a Result<(), String>, where the error case is a description of why things aren't
                    // simple.
                    errs.push(create_syntax_error_object(
                        format!("Identifier {} is an invalid left-hand-side", idref.string_value()),
                        Some(idref.location()),
                    ));
                }
                idref.early_errors(errs, strict);
                if let Some(i) = izer {
                    i.early_errors(errs, strict);
                }
            }
            AssignmentProperty::Property(pn, ae) => {
                pn.early_errors(errs, strict);
                ae.early_errors(errs, strict);
            }
        }
    }

    #[expect(unused_variables)]
    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        todo!()
    }
}

// AssignmentElement[Yield, Await] :
//      DestructuringAssignmentTarget[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]opt
#[derive(Debug)]
pub struct AssignmentElement {
    pub target: Rc<DestructuringAssignmentTarget>,
    pub initializer: Option<Rc<Initializer>>,
}

impl fmt::Display for AssignmentElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.initializer {
            None => self.target.fmt(f),
            Some(init) => write!(f, "{} {}", self.target, init),
        }
    }
}

impl PrettyPrint for AssignmentElement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AssignmentElement: {self}")?;
        match &self.initializer {
            None => self.target.pprint_with_leftpad(writer, &successive, Spot::Final),
            Some(init) => {
                self.target.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                init.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match &self.initializer {
            None => self.target.concise_with_leftpad(writer, pad, state),
            Some(init) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{first}AssignmentElement: {self}")?;
                self.target.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                init.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl AssignmentElement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (target, after_target) = DestructuringAssignmentTarget::parse(parser, scanner, yield_flag, await_flag)?;
        let (initializer, after_init) = match Initializer::parse(parser, after_target, true, yield_flag, await_flag) {
            Ok((node, scan)) => (Some(node), scan),
            Err(_) => (None, after_target),
        };
        Ok((Rc::new(AssignmentElement { target, initializer }), after_init))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match &self.initializer {
            Some(init) => self.target.contains(kind) || init.contains(kind),
            None => self.target.contains(kind),
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        match &self.initializer {
            Some(init) => self.target.all_private_identifiers_valid(names) && init.all_private_identifiers_valid(names),
            None => self.target.all_private_identifiers_valid(names),
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
        self.target.contains_arguments() || self.initializer.as_ref().is_some_and(|izer| izer.contains_arguments())
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.target.early_errors(errs, strict);
        if let Some(izer) = &self.initializer {
            izer.early_errors(errs, strict);
        }
    }

    #[expect(unused_variables)]
    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        todo!()
    }
}

// AssignmentRestElement[Yield, Await] :
//      ... DestructuringAssignmentTarget[?Yield, ?Await]
#[derive(Debug)]
pub struct AssignmentRestElement(pub Rc<DestructuringAssignmentTarget>);

impl fmt::Display for AssignmentRestElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "... {}", self.0)
    }
}

impl PrettyPrint for AssignmentRestElement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AssignmentRestElement: {self}")?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}AssignmentRestElement: {self}")?;
        pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.0.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl AssignmentRestElement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (_, after_dots) =
            scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Ellipsis)?;
        let (target, after_target) = DestructuringAssignmentTarget::parse(parser, after_dots, yield_flag, await_flag)?;
        Ok((Rc::new(AssignmentRestElement(target)), after_target))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.0.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.0.all_private_identifiers_valid(names)
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
        self.0.contains_arguments()
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.0.early_errors(errs, strict);
    }

    #[expect(unused_variables)]
    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        todo!()
    }
}

// DestructuringAssignmentTarget[Yield, Await] :
//      LeftHandSideExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum DestructuringAssignmentTarget {
    LeftHandSideExpression(Rc<LeftHandSideExpression>),
    AssignmentPattern(Rc<AssignmentPattern>),
}

impl fmt::Display for DestructuringAssignmentTarget {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DestructuringAssignmentTarget::LeftHandSideExpression(lhs) => lhs.fmt(f),
            DestructuringAssignmentTarget::AssignmentPattern(pat) => pat.fmt(f),
        }
    }
}

impl PrettyPrint for DestructuringAssignmentTarget {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}DestructuringAssignmentTarget: {self}")?;
        match self {
            DestructuringAssignmentTarget::LeftHandSideExpression(lhs) => {
                lhs.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            DestructuringAssignmentTarget::AssignmentPattern(pat) => {
                pat.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            DestructuringAssignmentTarget::LeftHandSideExpression(lhs) => lhs.concise_with_leftpad(writer, pad, state),
            DestructuringAssignmentTarget::AssignmentPattern(pat) => pat.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl DestructuringAssignmentTarget {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (lhs, after_lhs) = LeftHandSideExpression::parse(parser, scanner, yield_flag, await_flag)?;

        if lhs.is_object_or_array_literal() {
            // Re-parse the LHS as an AssignmentPattern.
            let (ap, after_ap) = AssignmentPattern::parse(parser, scanner, yield_flag, await_flag)?;
            assert_eq!(after_ap, after_lhs);
            Ok((Rc::new(DestructuringAssignmentTarget::AssignmentPattern(ap)), after_ap))
        } else {
            Ok((Rc::new(DestructuringAssignmentTarget::LeftHandSideExpression(lhs)), after_lhs))
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            DestructuringAssignmentTarget::AssignmentPattern(pat) => pat.contains(kind),
            DestructuringAssignmentTarget::LeftHandSideExpression(lhs) => lhs.contains(kind),
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
            DestructuringAssignmentTarget::LeftHandSideExpression(lhs) => lhs.all_private_identifiers_valid(names),
            DestructuringAssignmentTarget::AssignmentPattern(pat) => pat.all_private_identifiers_valid(names),
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
            DestructuringAssignmentTarget::AssignmentPattern(ap) => ap.contains_arguments(),
            DestructuringAssignmentTarget::LeftHandSideExpression(lhse) => lhse.contains_arguments(),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        // DestructuringAssignmentTarget : LeftHandSideExpression
        //  * If LeftHandSideExpression is an ObjectLiteral or an ArrayLiteral, the following Early Error rules are applied:
        //      * LeftHandSideExpression must cover an AssignmentPattern.
        //  * If LeftHandSideExpression is neither an ObjectLiteral nor an ArrayLiteral, the following Early Error rule is applied:
        //      * It is a Syntax Error if AssignmentTargetType of LeftHandSideExpression is not simple.
        match self {
            DestructuringAssignmentTarget::AssignmentPattern(pat) => pat.early_errors(errs, strict),
            DestructuringAssignmentTarget::LeftHandSideExpression(lhs) => {
                if lhs.assignment_target_type(strict) != ATTKind::Simple {
                    errs.push(create_syntax_error_object("Invalid left-hand side in assignment", Some(lhs.location())));
                }
                lhs.early_errors(errs, strict);
            }
        }
    }

    pub fn identifier_ref(&self) -> Option<Rc<IdentifierReference>> {
        match self {
            DestructuringAssignmentTarget::LeftHandSideExpression(lhse) => lhse.identifier_ref(),
            DestructuringAssignmentTarget::AssignmentPattern(_) => None,
        }
    }

    #[expect(unused_variables)]
    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        todo!()
    }
}

#[cfg(test)]
mod tests;
