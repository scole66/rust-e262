use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

//////// 12.3 Left-Hand-Side Expressions

// MemberExpression[Yield, Await] :
//      PrimaryExpression[?Yield, ?Await]
//      MemberExpression[?Yield, ?Await] [ Expression[+In, ?Yield, ?Await] ]
//      MemberExpression[?Yield, ?Await] . IdentifierName
//      MemberExpression[?Yield, ?Await] TemplateLiteral[?Yield, ?Await, +Tagged]
//      SuperProperty[?Yield, ?Await]
//      MetaProperty
//      new MemberExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
//      MemberExpression[?Yield, ?Await] . PrivateIdentifier

// How to parse:
// if PrimaryExpression, SuperProperty, or MetaProperty is detected,
//      make a MemberExpression node.
// if a "new" token is detected, make a MemberExpression node.
// if neither of those, return None.
// Check for the "after member expression" tokens, "[", ".", or a TemplateLiteral.
// If they're there, build up one of the interior productions and loop.

#[derive(Debug)]
pub(crate) enum MemberExpression {
    PrimaryExpression(Rc<PrimaryExpression>),
    Expression(Rc<MemberExpression>, Rc<Expression>, Location),
    IdentifierName(Rc<MemberExpression>, IdentifierData, Location),
    TemplateLiteral(Rc<MemberExpression>, Rc<TemplateLiteral>),
    SuperProperty(Rc<SuperProperty>),
    MetaProperty(Rc<MetaProperty>),
    NewArguments(Rc<MemberExpression>, Rc<Arguments>, Location),
    PrivateId(Rc<MemberExpression>, IdentifierData, Location),
}

impl fmt::Display for MemberExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MemberExpression::PrimaryExpression(boxed) => write!(f, "{boxed}"),
            MemberExpression::Expression(me, exp, _) => {
                write!(f, "{me} [ {exp} ]")
            }
            MemberExpression::IdentifierName(me, id, _) | MemberExpression::PrivateId(me, id, _) => {
                write!(f, "{me} . {id}")
            }
            MemberExpression::TemplateLiteral(me, tl) => write!(f, "{me} {tl}"),
            MemberExpression::SuperProperty(boxed) => write!(f, "{boxed}"),
            MemberExpression::MetaProperty(boxed) => write!(f, "{boxed}"),
            MemberExpression::NewArguments(me, args, _) => {
                write!(f, "new {me} {args}")
            }
        }
    }
}

impl PrettyPrint for MemberExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}MemberExpression: {self}")?;
        match self {
            MemberExpression::PrimaryExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            MemberExpression::Expression(me, exp, _) => {
                me.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                exp.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            MemberExpression::IdentifierName(me, ..) | MemberExpression::PrivateId(me, ..) => {
                me.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            MemberExpression::TemplateLiteral(me, tl) => {
                me.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                tl.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            MemberExpression::SuperProperty(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            MemberExpression::MetaProperty(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            MemberExpression::NewArguments(me, args, _) => {
                me.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                args.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut head = |pad, state| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{first}MemberExpression: {self}").and(Ok(successive))
        };
        match self {
            MemberExpression::PrimaryExpression(node) => node.concise_with_leftpad(writer, pad, state),
            MemberExpression::SuperProperty(node) => node.concise_with_leftpad(writer, pad, state),
            MemberExpression::MetaProperty(node) => node.concise_with_leftpad(writer, pad, state),
            MemberExpression::Expression(me, exp, _) => {
                let successive = head(pad, state)?;
                me.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                exp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            MemberExpression::IdentifierName(me, id, _) => {
                let successive = head(pad, state)?;
                me.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, id, TokenType::IdentifierName, &successive, Spot::Final)
            }
            MemberExpression::PrivateId(me, id, _) => {
                let successive = head(pad, state)?;
                me.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, id, TokenType::PrivateIdentifier, &successive, Spot::Final)
            }
            MemberExpression::TemplateLiteral(me, tl) => {
                let successive = head(pad, state)?;
                me.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                tl.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            MemberExpression::NewArguments(me, args, _) => {
                let successive = head(pad, state)?;
                pprint_token(writer, "new", TokenType::Keyword, &successive, Spot::NotFinal)?;
                me.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                args.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl From<Rc<PrimaryExpression>> for MemberExpression {
    fn from(node: Rc<PrimaryExpression>) -> Self {
        Self::PrimaryExpression(node)
    }
}

impl From<Rc<SuperProperty>> for MemberExpression {
    fn from(node: Rc<SuperProperty>) -> Self {
        Self::SuperProperty(node)
    }
}

impl From<Rc<MetaProperty>> for MemberExpression {
    fn from(node: Rc<MetaProperty>) -> Self {
        Self::MetaProperty(node)
    }
}

fn me_boxer<T>(pair: (T, Scanner)) -> (Rc<MemberExpression>, Scanner)
where
    T: Into<MemberExpression>,
{
    let (node, scanner) = pair;
    (Rc::new(node.into()), scanner)
}

fn member_expression_head_recursive(
    parser: &mut Parser,
    yield_flag: bool,
    await_flag: bool,
    me: Rc<MemberExpression>,
    scan: Scanner,
) -> (Rc<MemberExpression>, Scanner) {
    enum After {
        Exp(Rc<Expression>, Location),
        Id(IdentifierData, Location),
        TLit(Rc<TemplateLiteral>),
        Pid(IdentifierData, Location),
    }
    let mut current_me = me;
    let mut after_scan = scan;
    while let Ok((parts, after_production)) = TemplateLiteral::parse(parser, after_scan, yield_flag, await_flag, true)
        .map(|(tl, after_tl)| (After::TLit(tl), after_tl))
        .otherwise(|| {
            scan_for_punct_set(
                after_scan,
                parser.source,
                InputElementGoal::RegExp,
                &[Punctuator::Dot, Punctuator::LeftBracket],
            )
            .and_then(|(punct, _, after)| match punct {
                Punctuator::Dot => scan_for_identifiername(after, parser.source, InputElementGoal::RegExp)
                    .map(|(id, id_loc, after_id)| (After::Id(id, id_loc), after_id))
                    .otherwise(|| {
                        scan_for_private_identifier(after, parser.source, InputElementGoal::RegExp)
                            .map(|(id, id_loc, after_id)| (After::Pid(id, id_loc), after_id))
                    }),
                _ => Expression::parse(parser, after, true, yield_flag, await_flag).and_then(
                    |(expression, after_exp)| {
                        scan_for_punct(after_exp, parser.source, InputElementGoal::RegExp, Punctuator::RightBracket)
                            .map(|(bracket_loc, after_bracket)| (After::Exp(expression, bracket_loc), after_bracket))
                    },
                ),
            })
        })
    {
        current_me = match parts {
            After::TLit(tl) => Rc::new(MemberExpression::TemplateLiteral(current_me, tl)),
            After::Exp(exp, tail_loc) => {
                let location = current_me.location().merge(&tail_loc);
                Rc::new(MemberExpression::Expression(current_me, exp, location))
            }
            After::Id(id, tail_loc) => {
                let location = current_me.location().merge(&tail_loc);
                Rc::new(MemberExpression::IdentifierName(current_me, id, location))
            }
            After::Pid(id, tail_loc) => {
                let location = current_me.location().merge(&tail_loc);
                Rc::new(MemberExpression::PrivateId(current_me, id, location))
            }
        };
        after_scan = after_production;
    }
    (current_me, after_scan)
}

impl MemberExpression {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.member_expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.member_expression_cache.insert(key, result.clone());
                result
            }
        }
    }

    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::MemberExpression), scanner))
            // First: All the non-head-recursive productions
            .otherwise(|| PrimaryExpression::parse(parser, scanner, yield_flag, await_flag).map(me_boxer))
            .otherwise(|| SuperProperty::parse(parser, scanner, yield_flag, await_flag).map(me_boxer))
            .otherwise(|| MetaProperty::parse(parser, scanner).map(me_boxer))
            .otherwise(|| {
                Self::new_memberexpression_arguments(parser, scanner, yield_flag, await_flag).map(
                    |(me, args, new_loc, after)| {
                        let location = new_loc.merge(&args.location());
                        (Rc::new(MemberExpression::NewArguments(me, args, location)), after)
                    },
                )
            })
            // And then all the head-recursive productions.
            .map(|(me, scan)| member_expression_head_recursive(parser, yield_flag, await_flag, me, scan))
    }
    fn new_memberexpression_arguments(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<(Rc<MemberExpression>, Rc<Arguments>, Location, Scanner), ParseError> {
        let (new_loc, after_new) = scan_for_keyword(scanner, parser.source, InputElementGoal::RegExp, Keyword::New)?;
        let (me, after_me) = MemberExpression::parse(parser, after_new, yield_flag, await_flag)?;
        let (args, after_args) = Arguments::parse(parser, after_me, yield_flag, await_flag)?;
        Ok((me, args, new_loc, after_args))
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            MemberExpression::PrimaryExpression(exp) => exp.location(),
            MemberExpression::TemplateLiteral(right, left) => right.location().merge(&left.location()),
            MemberExpression::SuperProperty(exp) => exp.location(),
            MemberExpression::MetaProperty(exp) => exp.location(),
            MemberExpression::Expression(_, _, location)
            | MemberExpression::IdentifierName(_, _, location)
            | MemberExpression::NewArguments(_, _, location)
            | MemberExpression::PrivateId(_, _, location) => *location,
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            MemberExpression::PrimaryExpression(n) => n.contains(kind),
            MemberExpression::Expression(l, r, ..) => l.contains(kind) || r.contains(kind),
            MemberExpression::IdentifierName(n, ..) | MemberExpression::PrivateId(n, ..) => n.contains(kind),
            MemberExpression::TemplateLiteral(l, r) => l.contains(kind) || r.contains(kind),
            MemberExpression::SuperProperty(n) => kind == ParseNodeKind::SuperProperty || n.contains(kind),
            MemberExpression::MetaProperty(n) => n.contains(kind),
            MemberExpression::NewArguments(l, r, ..) => l.contains(kind) || r.contains(kind),
        }
    }

    pub(crate) fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            MemberExpression::PrimaryExpression(n) => n.as_string_literal(),
            _ => None,
        }
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        match self {
            //  1. For each child node child of this Parse Node, do
            //      a. If child is an instance of a nonterminal, then
            //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
            //  2. Return true.
            MemberExpression::PrimaryExpression(n) => n.all_private_identifiers_valid(names),
            MemberExpression::Expression(l, r, ..) => {
                l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names)
            }
            MemberExpression::IdentifierName(n, ..) => n.all_private_identifiers_valid(names),
            MemberExpression::TemplateLiteral(l, r) => {
                l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names)
            }
            MemberExpression::SuperProperty(n) => n.all_private_identifiers_valid(names),
            MemberExpression::MetaProperty(_) => true,
            MemberExpression::NewArguments(l, r, ..) => {
                l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names)
            }

            // MemberExpression : MemberExpression . PrivateIdentifier
            //  1. If names contains the StringValue of PrivateIdentifier, then
            //      a. Return AllPrivateIdentifiersValid of MemberExpression with argument names.
            //  2. Return false.
            MemberExpression::PrivateId(n, id, ..) => {
                names.contains(&id.string_value) && n.all_private_identifiers_valid(names)
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
            MemberExpression::PrimaryExpression(pe) => pe.contains_arguments(),
            MemberExpression::Expression(me, e, ..) => me.contains_arguments() || e.contains_arguments(),
            MemberExpression::IdentifierName(me, ..) | MemberExpression::PrivateId(me, ..) => me.contains_arguments(),
            MemberExpression::TemplateLiteral(me, tl) => me.contains_arguments() || tl.contains_arguments(),
            MemberExpression::SuperProperty(sp) => sp.contains_arguments(),
            MemberExpression::MetaProperty(_) => false,
            MemberExpression::NewArguments(me, a, ..) => me.contains_arguments() || a.contains_arguments(),
        }
    }

    pub(crate) fn is_object_or_array_literal(&self) -> bool {
        match self {
            MemberExpression::PrimaryExpression(n) => n.is_object_or_array_literal(),
            MemberExpression::Expression(..)
            | MemberExpression::IdentifierName(..)
            | MemberExpression::TemplateLiteral(..)
            | MemberExpression::SuperProperty(..)
            | MemberExpression::MetaProperty(..)
            | MemberExpression::NewArguments(..)
            | MemberExpression::PrivateId(..) => false,
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            MemberExpression::PrimaryExpression(n) => n.early_errors(errs, strict),
            MemberExpression::Expression(l, r, ..) => {
                l.early_errors(errs, strict);
                r.early_errors(errs, strict);
            }
            MemberExpression::IdentifierName(n, ..) | MemberExpression::PrivateId(n, ..) => {
                n.early_errors(errs, strict);
            }
            MemberExpression::TemplateLiteral(l, r) => {
                l.early_errors(errs, strict);
                r.early_errors(errs, strict, 0xffff_ffff);
            }
            MemberExpression::SuperProperty(n) => n.early_errors(errs, strict),
            MemberExpression::MetaProperty(meta) => meta.early_errors(errs),
            MemberExpression::NewArguments(l, r, ..) => {
                l.early_errors(errs, strict);
                r.early_errors(errs, strict);
            }
        }
    }

    pub(crate) fn is_strictly_deletable(&self) -> bool {
        match self {
            MemberExpression::PrimaryExpression(node) => node.is_strictly_deletable(),
            MemberExpression::Expression(..)
            | MemberExpression::IdentifierName(..)
            | MemberExpression::TemplateLiteral(..)
            | MemberExpression::SuperProperty(..)
            | MemberExpression::MetaProperty(..)
            | MemberExpression::NewArguments(..) => true,
            MemberExpression::PrivateId(..) => false,
        }
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub(crate) fn assignment_target_type(&self, strict: bool) -> ATTKind {
        match self {
            MemberExpression::PrimaryExpression(boxed) => boxed.assignment_target_type(strict),
            MemberExpression::Expression(..)
            | MemberExpression::IdentifierName(..)
            | MemberExpression::PrivateId(..)
            | MemberExpression::SuperProperty(..) => ATTKind::Simple,
            MemberExpression::TemplateLiteral(..)
            | MemberExpression::MetaProperty(..)
            | MemberExpression::NewArguments(..) => ATTKind::Invalid,
        }
    }

    /// True if this production winds up being an IdentifierRef
    ///
    /// See [IsIdentifierRef](https://tc39.es/ecma262/#sec-static-semantics-isidentifierref) from ECMA-262.
    //#[cfg(test)]
    //pub(crate) fn is_identifier_ref(&self) -> bool {
    //    match self {
    //        MemberExpression::PrimaryExpression(x) => x.is_identifier_ref(),
    //        MemberExpression::Expression(..)
    //        | MemberExpression::IdentifierName(..)
    //        | MemberExpression::TemplateLiteral(..)
    //        | MemberExpression::SuperProperty(..)
    //        | MemberExpression::MetaProperty(..)
    //        | MemberExpression::NewArguments(..)
    //        | MemberExpression::PrivateId(..) => false,
    //    }
    //}
    pub(crate) fn identifier_ref(&self) -> Option<Rc<IdentifierReference>> {
        match self {
            MemberExpression::PrimaryExpression(x) => x.identifier_ref(),
            MemberExpression::Expression(..)
            | MemberExpression::IdentifierName(..)
            | MemberExpression::TemplateLiteral(..)
            | MemberExpression::SuperProperty(..)
            | MemberExpression::MetaProperty(..)
            | MemberExpression::NewArguments(..)
            | MemberExpression::PrivateId(..) => None,
        }
    }

    //pub(crate) fn is_named_function(&self) -> bool {
    //    match self {
    //        MemberExpression::PrimaryExpression(node) => node.is_named_function(),
    //        _ => false,
    //    }
    //}

    pub(crate) fn is_destructuring(&self) -> bool {
        // Static Semantics: IsDestructuring
        // The syntax-directed operation IsDestructuring takes no arguments and returns a Boolean. It is
        // defined piecewise over the following productions:
        match self {
            MemberExpression::PrimaryExpression(pe) => {
                // MemberExpression : PrimaryExpression
                //  1. If PrimaryExpression is either an ObjectLiteral or an ArrayLiteral, return true.
                //  2. Return false.
                matches!(pe.as_ref(), PrimaryExpression::ArrayLiteral { .. } | PrimaryExpression::ObjectLiteral { .. })
            }
            MemberExpression::Expression(_, _, _)
            | MemberExpression::IdentifierName(_, _, _)
            | MemberExpression::TemplateLiteral(_, _)
            | MemberExpression::SuperProperty(_)
            | MemberExpression::MetaProperty(_)
            | MemberExpression::NewArguments(_, _, _)
            | MemberExpression::PrivateId(_, _, _) => {
                // MemberExpression :
                //      MemberExpression [ Expression ]
                //      MemberExpression . IdentifierName
                //      MemberExpression TemplateLiteral
                //      SuperProperty
                //      MetaProperty
                //      new MemberExpression Arguments
                //      MemberExpression . PrivateIdentifier
                //  1. Return false.
                false
            }
        }
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                MemberExpression::PrimaryExpression(node) => node.body_containing_location(location),
                MemberExpression::Expression(left, right, _) => {
                    left.body_containing_location(location).or_else(|| right.body_containing_location(location))
                }
                MemberExpression::IdentifierName(node, ..) | MemberExpression::PrivateId(node, ..) => {
                    node.body_containing_location(location)
                }
                MemberExpression::TemplateLiteral(left, right) => {
                    left.body_containing_location(location).or_else(|| right.body_containing_location(location))
                }
                MemberExpression::SuperProperty(node) => node.body_containing_location(location),
                MemberExpression::MetaProperty(node) => node.body_containing_location(location),
                MemberExpression::NewArguments(left, right, _) => {
                    left.body_containing_location(location).or_else(|| right.body_containing_location(location))
                }
            }
        } else {
            None
        }
    }

    #[expect(unused_variables)]
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
        // MemberExpression[Yield, Await] :
        //      MemberExpression[?Yield, ?Await] [ Expression[+In, ?Yield, ?Await] ]
        //      MemberExpression[?Yield, ?Await] . IdentifierName
        //      SuperProperty[?Yield, ?Await]
        //      MetaProperty
        //      new MemberExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
        //      MemberExpression[?Yield, ?Await] . PrivateIdentifier
        //  1. Return false.
        // MemberExpression[Yield, Await] :
        //      PrimaryExpression[?Yield, ?Await]
        //  1. Return HasCallInTailPosition of PrimaryExpression with argument call.
        // MemberExpression[Yield, Await] :
        //      MemberExpression[?Yield, ?Await] TemplateLiteral[?Yield, ?Await, +Tagged]
        //  1. If this MemberExpression is call, return true.
        //  2. Return false.
        match self {
            MemberExpression::PrimaryExpression(primary_expression) => {
                primary_expression.has_call_in_tail_position(location)
            }
            MemberExpression::TemplateLiteral(member_expression, template_literal) => self.location() == *location,
            MemberExpression::Expression(..)
            | MemberExpression::IdentifierName(..)
            | MemberExpression::SuperProperty(..)
            | MemberExpression::MetaProperty(..)
            | MemberExpression::NewArguments(..)
            | MemberExpression::PrivateId(..) => false,
        }
    }
}

// SuperProperty[Yield, Await] :
//      super [ Expression[+In, ?Yield, ?Await] ]
//      super . IdentifierName
#[derive(Debug)]
pub(crate) enum SuperProperty {
    Expression { exp: Rc<Expression>, location: Location },
    IdentifierName { id: IdentifierData, location: Location },
}

impl fmt::Display for SuperProperty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SuperProperty::Expression { exp: boxed, .. } => write!(f, "super [ {boxed} ]"),
            SuperProperty::IdentifierName { id: boxed, .. } => write!(f, "super . {boxed}"),
        }
    }
}

impl PrettyPrint for SuperProperty {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}SuperProperty: {self}")?;
        match self {
            SuperProperty::Expression { exp: boxed, .. } => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            SuperProperty::IdentifierName { .. } => Ok(()),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}SuperProperty: {self}")?;
        match self {
            SuperProperty::Expression { exp: node, .. } => {
                pprint_token(writer, "super", TokenType::Keyword, &successive, Spot::NotFinal)?;
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            SuperProperty::IdentifierName { id, .. } => {
                pprint_token(writer, "super", TokenType::Keyword, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, id, TokenType::IdentifierName, &successive, Spot::Final)
            }
        }
    }
}

impl SuperProperty {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (super_loc, after_super) =
            scan_for_keyword(scanner, parser.source, InputElementGoal::RegExp, Keyword::Super)?;
        let (punct, _, after_punct) = scan_for_punct_set(
            after_super,
            parser.source,
            InputElementGoal::RegExp,
            &[Punctuator::Dot, Punctuator::LeftBracket],
        )?;
        match punct {
            Punctuator::LeftBracket => {
                let (exp, after_exp) = Expression::parse(parser, after_punct, true, yield_flag, await_flag)?;
                let (rb_loc, after_rb) =
                    scan_for_punct(after_exp, parser.source, InputElementGoal::Div, Punctuator::RightBracket)?;
                Ok((Rc::new(SuperProperty::Expression { exp, location: super_loc.merge(&rb_loc) }), after_rb))
            }
            _ => {
                let (id, id_loc, after_id) =
                    scan_for_identifiername(after_punct, parser.source, InputElementGoal::RegExp)?;
                Ok((Rc::new(SuperProperty::IdentifierName { id, location: super_loc.merge(&id_loc) }), after_id))
            }
        }
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            SuperProperty::Expression { location, .. } | SuperProperty::IdentifierName { location, .. } => *location,
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            SuperProperty::Expression { exp: n, .. } => kind == ParseNodeKind::Super || n.contains(kind),
            SuperProperty::IdentifierName { .. } => kind == ParseNodeKind::Super,
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
            SuperProperty::Expression { exp: n, .. } => n.all_private_identifiers_valid(names),
            SuperProperty::IdentifierName { .. } => true,
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
            SuperProperty::Expression { exp: e, .. } => e.contains_arguments(),
            SuperProperty::IdentifierName { .. } => false,
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        match self {
            SuperProperty::Expression { exp, .. } => exp.early_errors(errs, strict),
            SuperProperty::IdentifierName { .. } => {}
        }
    }

    #[expect(unused_variables)]
    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        todo!()
    }
}

// MetaProperty :
//      NewTarget
//      ImportMeta
// NewTarget :
//      new . target
// ImportMeta :
//      import . meta
#[derive(Debug)]
pub(crate) enum MetaProperty {
    NewTarget { location: Location },
    ImportMeta { goal: ParseGoal, location: Location },
}

impl fmt::Display for MetaProperty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MetaProperty::NewTarget { .. } => write!(f, "new . target"),
            MetaProperty::ImportMeta { .. } => write!(f, "import . meta"),
        }
    }
}

impl PrettyPrint for MetaProperty {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{first}MetaProperty: {self}")
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}MetaProperty: {self}")?;
        match self {
            MetaProperty::NewTarget { .. } => {
                pprint_token(writer, "new", TokenType::Keyword, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "target", TokenType::Keyword, &successive, Spot::Final)
            }
            MetaProperty::ImportMeta { .. } => {
                pprint_token(writer, "import", TokenType::Keyword, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "meta", TokenType::Keyword, &successive, Spot::Final)
            }
        }
    }
}

#[derive(Copy, Clone)]
enum MetaHelper {
    NewTarget,
    ImportMeta(ParseGoal),
}

impl MetaProperty {
    fn dot_token(
        parser: &mut Parser,
        scanner: Scanner,
        kwd: Keyword,
        starting_loc: Location,
        kind: MetaHelper,
    ) -> ParseResult<Self> {
        let (_, after_dot) = scan_for_punct(scanner, parser.source, InputElementGoal::Div, Punctuator::Dot)?;
        let (kwd_loc, after_kwd) = scan_for_keyword(after_dot, parser.source, InputElementGoal::RegExp, kwd)?;
        let location = starting_loc.merge(&kwd_loc);
        let production = match kind {
            MetaHelper::NewTarget => MetaProperty::NewTarget { location },
            MetaHelper::ImportMeta(goal) => MetaProperty::ImportMeta { goal, location },
        };
        Ok((Rc::new(production), after_kwd))
    }

    pub(crate) fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (kwd, kwd_loc, after_kwd) =
            scan_for_keywords(scanner, parser.source, InputElementGoal::RegExp, &[Keyword::New, Keyword::Import])?;
        match kwd {
            Keyword::New => Self::dot_token(parser, after_kwd, Keyword::Target, kwd_loc, MetaHelper::NewTarget),
            _ => Self::dot_token(parser, after_kwd, Keyword::Meta, kwd_loc, MetaHelper::ImportMeta(parser.goal)),
        }
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            MetaProperty::NewTarget { location } | MetaProperty::ImportMeta { location, .. } => *location,
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            MetaProperty::NewTarget { .. } => kind == ParseNodeKind::NewTarget,
            MetaProperty::ImportMeta { .. } => false,
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>) {
        match self {
            MetaProperty::NewTarget { .. } => {}
            MetaProperty::ImportMeta { goal, .. } => {
                // ImportMeta :
                //  import . meta
                //  * It is a Syntax Error if the syntactic goal symbol is not Module.
                if *goal != ParseGoal::Module {
                    errs.push(create_syntax_error_object(
                        "import.meta allowed only in Module code",
                        Some(self.location()),
                    ));
                }
            }
        }
    }

    #[expect(unused_variables)]
    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        todo!()
    }
}

// Arguments[Yield, Await] :
//      ( )
//      ( ArgumentList[?Yield, ?Await] )
//      ( ArgumentList[?Yield, ?Await] , )
#[derive(Debug)]
pub(crate) enum Arguments {
    Empty { location: Location },
    ArgumentList(Rc<ArgumentList>, Location),
    ArgumentListComma(Rc<ArgumentList>, Location),
}

impl fmt::Display for Arguments {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Arguments::Empty { .. } => write!(f, "( )"),
            Arguments::ArgumentList(boxed, _) => write!(f, "( {boxed} )"),
            Arguments::ArgumentListComma(boxed, _) => write!(f, "( {boxed} , )"),
        }
    }
}

impl PrettyPrint for Arguments {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}Arguments: {self}")?;
        match self {
            Arguments::Empty { .. } => Ok(()),
            Arguments::ArgumentList(boxed, _) | Arguments::ArgumentListComma(boxed, _) => {
                boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}Arguments: {self}")?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match self {
            Arguments::Empty { .. } => {}
            Arguments::ArgumentList(node, _) => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            Arguments::ArgumentListComma(node, _) => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
            }
        }
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl Arguments {
    // Arguments has many parents. It needs caching.
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (lp_loc, after_lp) =
            scan_for_punct(scanner, parser.source, InputElementGoal::RegExp, Punctuator::LeftParen)?;
        scan_for_punct(after_lp, parser.source, InputElementGoal::RegExp, Punctuator::RightParen)
            .map(|(rp_loc, after_rp)| (Rc::new(Arguments::Empty { location: lp_loc.merge(&rp_loc) }), after_rp))
            .otherwise(|| {
                let (args, after_args) = ArgumentList::parse(parser, after_lp, yield_flag, await_flag)?;
                let (punct, punct_loc, after_punct) = scan_for_punct_set(
                    after_args,
                    parser.source,
                    InputElementGoal::Div,
                    &[Punctuator::Comma, Punctuator::RightParen],
                )?;
                match punct {
                    Punctuator::RightParen => {
                        let location = lp_loc.merge(&punct_loc);
                        Ok((Rc::new(Arguments::ArgumentList(args, location)), after_punct))
                    }
                    _ => {
                        let (rp_loc, after_rp) = scan_for_punct(
                            after_punct,
                            parser.source,
                            InputElementGoal::RegExp,
                            Punctuator::RightParen,
                        )?;
                        let location = lp_loc.merge(&rp_loc);
                        Ok((Rc::new(Arguments::ArgumentListComma(args, location)), after_rp))
                    }
                }
            })
    }

    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.arguments_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.arguments_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            Arguments::Empty { location }
            | Arguments::ArgumentList(_, location)
            | Arguments::ArgumentListComma(_, location) => *location,
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            Arguments::Empty { .. } => false,
            Arguments::ArgumentList(n, _) | Arguments::ArgumentListComma(n, _) => n.contains(kind),
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
            Arguments::Empty { .. } => true,
            Arguments::ArgumentList(n, _) | Arguments::ArgumentListComma(n, _) => {
                n.all_private_identifiers_valid(names)
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
            Arguments::Empty { .. } => false,
            Arguments::ArgumentList(al, _) | Arguments::ArgumentListComma(al, _) => al.contains_arguments(),
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        match self {
            Arguments::Empty { .. } => {}
            Arguments::ArgumentList(n, _) | Arguments::ArgumentListComma(n, _) => n.early_errors(errs, strict),
        }
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                Arguments::Empty { .. } => None,
                Arguments::ArgumentList(n, _) | Arguments::ArgumentListComma(n, _) => {
                    n.body_containing_location(location)
                }
            }
        } else {
            None
        }
    }
}

// ArgumentList[Yield, Await] :
//      AssignmentExpression[+In, ?Yield, ?Await]
//      ... AssignmentExpression[+In, ?Yield, ?Await]
//      ArgumentList[?Yield, ?Await] , AssignmentExpression[+In, ?Yield, ?Await]
//      ArgumentList[?Yield, ?Await] , ... AssignmentExpression[+In, ?Yield, ?Await]
#[derive(Debug)]
pub(crate) enum ArgumentList {
    FallThru(Rc<AssignmentExpression>),
    Dots(Rc<AssignmentExpression>, Location),
    List(Rc<ArgumentList>, Rc<AssignmentExpression>),
    ListDots(Rc<ArgumentList>, Rc<AssignmentExpression>),
}

impl ArgumentList {
    // Package the results of a successful assignment_expression into an ArgumentList::FallThru.
    fn ae_bundle(pair: (Rc<AssignmentExpression>, Scanner)) -> (Self, Scanner) {
        let (ae_boxed, scanner) = pair;
        (Self::FallThru(ae_boxed), scanner)
    }

    // Package the results of assignment_expression into an ArgumentList (or pass along a None)
    //fn ae_package(opt: Option<(Rc<AssignmentExpression>, Scanner)>) -> Result<Option<(Self, Scanner)>, String> {
    //    opt.map_or(Ok(None), Self::ae_bundle)
    //}

    // Parse the production
    //      ArgumentList : AssignmentExpression
    // returning one of:
    //    * an ArgumentList that contains all the relevant info
    //    * an Err with a human readable message about what went wrong
    pub(crate) fn parse_assignment_expression(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<(Self, Scanner), ParseError> {
        AssignmentExpression::parse(parser, scanner, true, yield_flag, await_flag).map(Self::ae_bundle)
    }

    // Parse the production
    //      ArgumentList : ... AssignmentExpression
    // returning one of:
    //    * an ArgumentList that contains all the relevant info
    //    * an Err with a human readable message about what went wrong
    // Note: It is an error for ... to appear during an ArgumentList parse without being followed by an AssignmentExpression.
    pub(crate) fn parse_dots_assignment_expression(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<(Self, Scanner), ParseError> {
        let (loc, after_ellipsis) =
            scan_for_punct(scanner, parser.source, InputElementGoal::RegExp, Punctuator::Ellipsis)?;
        let (ae, after_ae) = AssignmentExpression::parse(parser, after_ellipsis, true, yield_flag, await_flag)?;
        let location = loc.merge(&ae.location());
        Ok((Self::Dots(ae, location), after_ae))
    }

    // Parse the production
    //      ArgumentList : ArgumentList , AssignmentExpression
    // ASSUMING: that the first ArgumentList has already been parsed. (I.e: just do the part starting with the comma.)
    // returning one of:
    //    * a pair: (Rc<AssignmentExpression>, Scanner)
    //    * an Err with a human readable message about what went wrong
    fn parse_al_ae(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<AssignmentExpression> {
        let (_, after_comma) = scan_for_punct(scanner, parser.source, InputElementGoal::Div, Punctuator::Comma)?;
        AssignmentExpression::parse(parser, after_comma, true, yield_flag, await_flag)
    }

    // Parse the production
    //      ArgumentList : ArgumentList , ... AssignmentExpression
    // ASSUMING: that the first ArgumentList has already been parsed. (I.e: just do the part starting with the comma.)
    // returning one of:
    //    * a pair: (Rc<AssignmentExpression>, Scanner)
    //    * an Err with a human readable message about what went wrong
    fn parse_al_dots_ae(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<AssignmentExpression> {
        let (_, after_comma) = scan_for_punct(scanner, parser.source, InputElementGoal::Div, Punctuator::Comma)?;
        let (_, after_ellipsis) =
            scan_for_punct(after_comma, parser.source, InputElementGoal::RegExp, Punctuator::Ellipsis)?;
        AssignmentExpression::parse(parser, after_ellipsis, true, yield_flag, await_flag)
    }
}

impl fmt::Display for ArgumentList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArgumentList::FallThru(boxed) => write!(f, "{boxed}"),
            ArgumentList::Dots(boxed, _) => write!(f, "... {boxed}"),
            ArgumentList::List(list, exp) => write!(f, "{list} , {exp}"),
            ArgumentList::ListDots(list, exp) => {
                write!(f, "{list} , ... {exp}")
            }
        }
    }
}

impl PrettyPrint for ArgumentList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ArgumentList: {self}")?;
        match self {
            ArgumentList::FallThru(boxed) | ArgumentList::Dots(boxed, _) => {
                boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArgumentList::List(list, exp) | ArgumentList::ListDots(list, exp) => {
                list.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                exp.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let mut head = |pad, state| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{first}ArgumentList: {self}").and(Ok(successive))
        };
        match self {
            ArgumentList::FallThru(node) => node.concise_with_leftpad(writer, pad, state),
            ArgumentList::Dots(node, _) => {
                let successive = head(pad, state)?;
                pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ArgumentList::List(list, exp) => {
                let successive = head(pad, state)?;
                list.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                exp.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ArgumentList::ListDots(list, exp) => {
                let successive = head(pad, state)?;
                list.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                exp.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl ArgumentList {
    // ArgumentList's only direct parent is Arguments; it doesn't need to be cached.
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        enum Dots {
            Dots,
            NoDots,
        }

        ArgumentList::parse_assignment_expression(parser, scanner, yield_flag, await_flag)
            .otherwise(|| ArgumentList::parse_dots_assignment_expression(parser, scanner, yield_flag, await_flag))
            .map(|(kind, after)| {
                let mut top_scanner = after;
                let mut top_box = Rc::new(kind);
                while let Ok((ae, scan, dotstate)) =
                    ArgumentList::parse_al_ae(parser, top_scanner, yield_flag, await_flag)
                        .map(|(ae, after_ae)| (ae, after_ae, Dots::NoDots))
                        .otherwise(|| {
                            ArgumentList::parse_al_dots_ae(parser, top_scanner, yield_flag, await_flag)
                                .map(|(ae, after_ae)| (ae, after_ae, Dots::Dots))
                        })
                {
                    top_box = Rc::new(match dotstate {
                        Dots::Dots => ArgumentList::ListDots(top_box, ae),
                        Dots::NoDots => ArgumentList::List(top_box, ae),
                    });
                    top_scanner = scan;
                }
                (top_box, top_scanner)
            })
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ArgumentList::FallThru(boxed) | ArgumentList::Dots(boxed, _) => boxed.contains(kind),
            ArgumentList::List(list, exp) | ArgumentList::ListDots(list, exp) => {
                list.contains(kind) || exp.contains(kind)
            }
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
            ArgumentList::FallThru(boxed) | ArgumentList::Dots(boxed, _) => boxed.all_private_identifiers_valid(names),
            ArgumentList::List(list, exp) | ArgumentList::ListDots(list, exp) => {
                list.all_private_identifiers_valid(names) && exp.all_private_identifiers_valid(names)
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
            ArgumentList::FallThru(ae) | ArgumentList::Dots(ae, _) => ae.contains_arguments(),
            ArgumentList::List(al, ae) | ArgumentList::ListDots(al, ae) => {
                al.contains_arguments() || ae.contains_arguments()
            }
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        match self {
            ArgumentList::FallThru(boxed) | ArgumentList::Dots(boxed, _) => boxed.early_errors(errs, strict),
            ArgumentList::List(list, exp) | ArgumentList::ListDots(list, exp) => {
                list.early_errors(errs, strict);
                exp.early_errors(errs, strict);
            }
        }
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                ArgumentList::FallThru(n) | ArgumentList::Dots(n, _) => n.body_containing_location(location),
                ArgumentList::List(n, _) | ArgumentList::ListDots(n, _) => n.body_containing_location(location),
            }
        } else {
            None
        }
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            ArgumentList::FallThru(boxed) => boxed.location(),
            ArgumentList::Dots(_, location) => *location,
            ArgumentList::List(al, ae) | ArgumentList::ListDots(al, ae) => {
                let al_loc = al.location();
                let ae_loc = ae.location();
                al_loc.merge(&ae_loc)
            }
        }
    }
}

// NewExpression[Yield, Await] :
//      MemberExpression[?Yield, ?Await]
//      new NewExpression[?Yield, ?Await]
#[derive(Debug)]
pub(crate) enum NewExpression {
    MemberExpression(Rc<MemberExpression>),
    NewExpression(Rc<NewExpression>, Location),
}

impl fmt::Display for NewExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NewExpression::MemberExpression(boxed) => write!(f, "{boxed}"),
            NewExpression::NewExpression(boxed, _) => write!(f, "new {boxed}"),
        }
    }
}

impl PrettyPrint for NewExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}NewExpression: {self}")?;
        match self {
            NewExpression::MemberExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            NewExpression::NewExpression(boxed, _) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            NewExpression::MemberExpression(node) => node.concise_with_leftpad(writer, pad, state),
            NewExpression::NewExpression(node, _) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{first}NewExpression: {self}")?;
                pprint_token(writer, "new", TokenType::Keyword, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl NewExpression {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        Err(ParseError::new(PECode::NewOrMEExpected, scanner))
            .otherwise(|| {
                let (me, after_me) = MemberExpression::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(NewExpression::MemberExpression(me)), after_me))
            })
            .otherwise(|| {
                let (new_loc, after_new) =
                    scan_for_keyword(scanner, parser.source, InputElementGoal::RegExp, Keyword::New)?;
                let (ne, after_ne) = Self::parse(parser, after_new, yield_flag, await_flag)?;
                let location = new_loc.merge(&ne.location());
                Ok((Rc::new(NewExpression::NewExpression(ne, location)), after_ne))
            })
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            NewExpression::MemberExpression(node) => node.location(),
            NewExpression::NewExpression(_, location) => *location,
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            NewExpression::MemberExpression(boxed) => boxed.contains(kind),
            NewExpression::NewExpression(boxed, ..) => boxed.contains(kind),
        }
    }

    pub(crate) fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            NewExpression::MemberExpression(n) => n.as_string_literal(),
            NewExpression::NewExpression(..) => None,
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
            NewExpression::MemberExpression(boxed) => boxed.all_private_identifiers_valid(names),
            NewExpression::NewExpression(boxed, ..) => boxed.all_private_identifiers_valid(names),
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
            NewExpression::MemberExpression(me) => me.contains_arguments(),
            NewExpression::NewExpression(ne, ..) => ne.contains_arguments(),
        }
    }

    pub(crate) fn is_object_or_array_literal(&self) -> bool {
        match self {
            NewExpression::MemberExpression(boxed) => boxed.is_object_or_array_literal(),
            NewExpression::NewExpression(..) => false,
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            NewExpression::MemberExpression(boxed) => boxed.early_errors(errs, strict),
            NewExpression::NewExpression(boxed, ..) => boxed.early_errors(errs, strict),
        }
    }

    pub(crate) fn is_strictly_deletable(&self) -> bool {
        match self {
            NewExpression::NewExpression(..) => true,
            NewExpression::MemberExpression(node) => node.is_strictly_deletable(),
        }
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub(crate) fn assignment_target_type(&self, strict: bool) -> ATTKind {
        match self {
            NewExpression::MemberExpression(boxed) => boxed.assignment_target_type(strict),
            NewExpression::NewExpression(..) => ATTKind::Invalid,
        }
    }

    /// True if this production winds up being an IdentifierRef
    ///
    /// See [IsIdentifierRef](https://tc39.es/ecma262/#sec-static-semantics-isidentifierref) from ECMA-262.
    //#[cfg(test)]
    //pub(crate) fn is_identifier_ref(&self) -> bool {
    //    match self {
    //        NewExpression::NewExpression(..) => false,
    //        NewExpression::MemberExpression(x) => x.is_identifier_ref(),
    //    }
    //}
    pub(crate) fn identifier_ref(&self) -> Option<Rc<IdentifierReference>> {
        match self {
            NewExpression::NewExpression(..) => None,
            NewExpression::MemberExpression(x) => x.identifier_ref(),
        }
    }

    //pub(crate) fn is_named_function(&self) -> bool {
    //    match self {
    //        NewExpression::MemberExpression(node) => node.is_named_function(),
    //        NewExpression::NewExpression(..) => false,
    //    }
    //}

    pub(crate) fn is_destructuring(&self) -> bool {
        // Static Semantics: IsDestructuring
        // The syntax-directed operation IsDestructuring takes no arguments and returns a Boolean. It is
        // defined piecewise over the following productions:
        match self {
            NewExpression::MemberExpression(me) => {
                // NewExpression : MemberExpression
                //  1. Return IsDestructuring of MemberExpression.
                me.is_destructuring()
            }
            NewExpression::NewExpression(_, _) => {
                // NewExpression : new NewExpression
                //  1. Return false.
                false
            }
        }
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                NewExpression::MemberExpression(n) => n.body_containing_location(location),
                NewExpression::NewExpression(n, _) => n.body_containing_location(location),
            }
        } else {
            None
        }
    }

    #[expect(unused_variables)]
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
        // NewExpression[Yield, Await] :
        //      MemberExpression[?Yield, ?Await]
        //  1. Return HasCallInTailPosition of MemberExpression with argument call.
        // NewExpression[Yield, Await] :
        //      new NewExpression[?Yield, ?Await]
        //  1. Return false.
        match self {
            NewExpression::MemberExpression(member_expression) => member_expression.has_call_in_tail_position(location),
            NewExpression::NewExpression(new_expression, location) => false,
        }
    }
}

// CallMemberExpression[Yield, Await] :
//      MemberExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
#[derive(Debug)]
pub(crate) struct CallMemberExpression {
    pub(crate) member_expression: Rc<MemberExpression>,
    pub(crate) arguments: Rc<Arguments>,
}

impl fmt::Display for CallMemberExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.member_expression, self.arguments)
    }
}

impl PrettyPrint for CallMemberExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}CallMemberExpression: {self}")?;
        self.member_expression.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.arguments.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}CallMemberExpression: {self}")?;
        self.member_expression.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.arguments.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl CallMemberExpression {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (me, after_me) = MemberExpression::parse(parser, scanner, yield_flag, await_flag)?;
        let (args, after_args) = Arguments::parse(parser, after_me, yield_flag, await_flag)?;
        Ok((Rc::new(CallMemberExpression { member_expression: me, arguments: args }), after_args))
    }

    pub(crate) fn location(&self) -> Location {
        self.member_expression.location().merge(&self.arguments.location())
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        self.member_expression.contains(kind) || self.arguments.contains(kind)
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.member_expression.all_private_identifiers_valid(names)
            && self.arguments.all_private_identifiers_valid(names)
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
        self.member_expression.contains_arguments() || self.arguments.contains_arguments()
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.member_expression.early_errors(errs, strict);
        self.arguments.early_errors(errs, strict);
    }

    pub(crate) fn is_in_tail_position(&self, top: &ParsedText, strict: bool) -> bool {
        // Static Semantics: IsInTailPosition ( call )
        //
        // The abstract operation IsInTailPosition takes argument call (a CallExpression Parse Node, a MemberExpression
        // Parse Node, or an OptionalChain Parse Node) and returns a Boolean. It performs the following steps when
        // called:
        //
        //  1. If IsStrict(call) is false, return false.
        //  2. If call is not contained within a FunctionBody, a ConciseBody, or an AsyncConciseBody, return false.
        //  3. Let body be the FunctionBody, ConciseBody, or AsyncConciseBody that most closely contains call.
        //  4. If body is the FunctionBody of a GeneratorBody, return false.
        //  5. If body is the FunctionBody of an AsyncFunctionBody, return false.
        //  6. If body is the FunctionBody of an AsyncGeneratorBody, return false.
        //  7. If body is an AsyncConciseBody, return false.
        //  8. Return the result of HasCallInTailPosition of body with argument call.
        //
        // Note: Tail Position calls are only defined in strict mode code because of a common non-standard language
        //       extension (see 10.2.4) that enables observation of the chain of caller contexts.
        is_in_tail_position(&self.location(), top, strict)
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            self.member_expression
                .body_containing_location(location)
                .or_else(|| self.arguments.body_containing_location(location))
        } else {
            None
        }
    }
}

// SuperCall[Yield, Await] :
//      super Arguments[?Yield, ?Await]
#[derive(Debug)]
pub(crate) struct SuperCall {
    pub(crate) arguments: Rc<Arguments>,
    location: Location,
}

impl fmt::Display for SuperCall {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "super {}", self.arguments)
    }
}

impl PrettyPrint for SuperCall {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}SuperCall: {self}")?;
        self.arguments.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}SuperCall: {self}")?;
        pprint_token(writer, "super", TokenType::Keyword, &successive, Spot::NotFinal)?;
        self.arguments.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl SuperCall {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (super_loc, after_super) =
            scan_for_keyword(scanner, parser.source, InputElementGoal::RegExp, Keyword::Super)?;
        let (args, after_args) = Arguments::parse(parser, after_super, yield_flag, await_flag)?;
        let location = super_loc.merge(&args.location());
        Ok((Rc::new(Self { arguments: args, location }), after_args))
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        kind == ParseNodeKind::Super || self.arguments.contains(kind)
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.arguments.all_private_identifiers_valid(names)
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
        self.arguments.contains_arguments()
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.arguments.early_errors(errs, strict);
    }

    #[expect(unused_variables)]
    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        todo!()
    }
}

// ImportCall[Yield, Await] :
//      import ( AssignmentExpression[+In, ?Yield, ?Await] )
#[derive(Debug)]
pub(crate) struct ImportCall {
    assignment_expression: Rc<AssignmentExpression>,
    location: Location,
}

impl fmt::Display for ImportCall {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "import ( {} )", self.assignment_expression)
    }
}

impl PrettyPrint for ImportCall {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ImportCall: {self}")?;
        self.assignment_expression.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ImportCall: {self}")?;
        pprint_token(writer, "import", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.assignment_expression.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ImportCall {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (import_loc, after_import) =
            scan_for_keyword(scanner, parser.source, InputElementGoal::RegExp, Keyword::Import)?;
        let (_, after_lp) =
            scan_for_punct(after_import, parser.source, InputElementGoal::RegExp, Punctuator::LeftParen)?;
        let (ae, after_ae) = AssignmentExpression::parse(parser, after_lp, true, yield_flag, await_flag)?;
        let (rp_loc, after_rp) =
            scan_for_punct(after_ae, parser.source, InputElementGoal::RegExp, Punctuator::RightParen)?;
        let location = import_loc.merge(&rp_loc);
        Ok((Rc::new(Self { assignment_expression: ae, location }), after_rp))
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        self.assignment_expression.contains(kind)
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.assignment_expression.all_private_identifiers_valid(names)
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
        self.assignment_expression.contains_arguments()
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.assignment_expression.early_errors(errs, strict);
    }

    #[expect(unused_variables)]
    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        todo!()
    }
}

// CallExpression[Yield, Await] :
//      CoverCallExpressionAndAsyncArrowHead[?Yield, ?Await]
//      SuperCall[?Yield, ?Await]
//      ImportCall[?Yield, ?Await]
//      CallExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
//      CallExpression[?Yield, ?Await] [ Expression[+In, ?Yield, ?Await] ]
//      CallExpression[?Yield, ?Await] . IdentifierName
//      CallExpression[?Yield, ?Await] TemplateLiteral[?Yield, ?Await, +Tagged]
//      CallExpression[?Yield, ?Await] . PrivateIdentifier
#[derive(Debug)]
pub(crate) enum CallExpression {
    CallMemberExpression(Rc<CallMemberExpression>),
    SuperCall(Rc<SuperCall>),
    ImportCall(Rc<ImportCall>),
    CallThenArguments(Rc<CallExpression>, Rc<Arguments>),
    CallThenExpression(Rc<CallExpression>, Rc<Expression>, Location),
    CallThenName(Rc<CallExpression>, IdentifierData, Location),
    CallThenTemplateLiteral(Rc<CallExpression>, Rc<TemplateLiteral>),
    CallThenPrivateId(Rc<CallExpression>, IdentifierData, Location),
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CallExpression::CallMemberExpression(boxed) => write!(f, "{boxed}"),
            CallExpression::SuperCall(boxed) => write!(f, "{boxed}"),
            CallExpression::ImportCall(boxed) => write!(f, "{boxed}"),
            CallExpression::CallThenArguments(ce, args) => write!(f, "{ce} {args}"),
            CallExpression::CallThenExpression(ce, exp, _) => write!(f, "{ce} [ {exp} ]"),
            CallExpression::CallThenName(ce, id, _) | CallExpression::CallThenPrivateId(ce, id, _) => {
                write!(f, "{ce} . {id}")
            }
            CallExpression::CallThenTemplateLiteral(ce, tl) => write!(f, "{ce} {tl}"),
        }
    }
}

impl PrettyPrint for CallExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}CallExpression: {self}")?;
        match self {
            CallExpression::CallMemberExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            CallExpression::SuperCall(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            CallExpression::ImportCall(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            CallExpression::CallThenArguments(ce, args) => {
                ce.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                args.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CallExpression::CallThenExpression(ce, exp, _) => {
                ce.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                exp.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CallExpression::CallThenName(ce, _, _) | CallExpression::CallThenPrivateId(ce, _, _) => {
                ce.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CallExpression::CallThenTemplateLiteral(ce, tl) => {
                ce.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                tl.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let head = |writer: &mut T, node: &CallExpression| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{first}CallExpression: {self}")?;
            node.concise_with_leftpad(writer, &successive, Spot::NotFinal).and(Ok(successive))
        };
        match self {
            CallExpression::CallMemberExpression(node) => node.concise_with_leftpad(writer, pad, state),
            CallExpression::SuperCall(node) => node.concise_with_leftpad(writer, pad, state),
            CallExpression::ImportCall(node) => node.concise_with_leftpad(writer, pad, state),
            CallExpression::CallThenArguments(ce, right) => {
                let successive = head(writer, ce)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            CallExpression::CallThenExpression(ce, right, _) => {
                let successive = head(writer, ce)?;
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            CallExpression::CallThenName(ce, right, _) => {
                let successive = head(writer, ce)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, right, TokenType::IdentifierName, &successive, Spot::Final)
            }
            CallExpression::CallThenTemplateLiteral(ce, right) => {
                let successive = head(writer, ce)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            CallExpression::CallThenPrivateId(ce, id, _) => {
                let successive = head(writer, ce)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, id, TokenType::PrivateIdentifier, &successive, Spot::Final)
            }
        }
    }
}

impl CallExpression {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_arg: bool, await_arg: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::CallExpression), scanner))
            .otherwise(|| {
                CallMemberExpression::parse(parser, scanner, yield_arg, await_arg)
                    .map(|(cme, after_cme)| (Rc::new(CallExpression::CallMemberExpression(cme)), after_cme))
            })
            .otherwise(|| {
                SuperCall::parse(parser, scanner, yield_arg, await_arg)
                    .map(|(sc, after_sc)| (Rc::new(CallExpression::SuperCall(sc)), after_sc))
            })
            .otherwise(|| {
                ImportCall::parse(parser, scanner, yield_arg, await_arg)
                    .map(|(ic, after_ic)| (Rc::new(CallExpression::ImportCall(ic)), after_ic))
            })
            .map(|(ce, after_ce)| {
                enum Follow {
                    Args(Rc<Arguments>),
                    Exp(Rc<Expression>, Location),
                    Id(IdentifierData, Location),
                    TLit(Rc<TemplateLiteral>),
                    Pid(IdentifierData, Location),
                }
                let mut top_box = ce;
                let mut top_scanner = after_ce;
                while let Ok((follow, scan)) = Arguments::parse(parser, top_scanner, yield_arg, await_arg)
                    .map(|(args, after_args)| (Follow::Args(args), after_args))
                    .otherwise(|| {
                        TemplateLiteral::parse(parser, top_scanner, yield_arg, await_arg, true)
                            .map(|(tl, after_tl)| (Follow::TLit(tl), after_tl))
                    })
                    .otherwise(|| {
                        let (punct, _, after_punct) = scan_for_punct_set(
                            top_scanner,
                            parser.source,
                            InputElementGoal::Div,
                            &[Punctuator::Dot, Punctuator::LeftBracket],
                        )?;
                        match punct {
                            Punctuator::Dot => {
                                scan_for_identifiername(after_punct, parser.source, InputElementGoal::Div)
                                    .map(|(id, id_loc, after_id)| (Follow::Id(id, id_loc), after_id))
                                    .otherwise(|| {
                                        scan_for_private_identifier(after_punct, parser.source, InputElementGoal::Div)
                                            .map(|(pid, pid_loc, after_pid)| (Follow::Pid(pid, pid_loc), after_pid))
                                    })
                            }
                            _ => {
                                let (exp, after_exp) =
                                    Expression::parse(parser, after_punct, true, yield_arg, await_arg)?;
                                let (rb_loc, after_rb) = scan_for_punct(
                                    after_exp,
                                    parser.source,
                                    InputElementGoal::Div,
                                    Punctuator::RightBracket,
                                )?;
                                Ok((Follow::Exp(exp, rb_loc), after_rb))
                            }
                        }
                    })
                {
                    top_box = Rc::new(match follow {
                        Follow::Exp(exp, tok_loc) => {
                            let location = top_box.location().merge(&tok_loc);
                            CallExpression::CallThenExpression(top_box, exp, location)
                        }
                        Follow::Id(id, id_loc) => {
                            let location = top_box.location().merge(&id_loc);
                            CallExpression::CallThenName(top_box, id, location)
                        }
                        Follow::TLit(tl) => CallExpression::CallThenTemplateLiteral(top_box, tl),
                        Follow::Args(args) => CallExpression::CallThenArguments(top_box, args),
                        Follow::Pid(id, id_loc) => {
                            let location = top_box.location().merge(&id_loc);
                            CallExpression::CallThenPrivateId(top_box, id, location)
                        }
                    });
                    top_scanner = scan;
                }
                (top_box, top_scanner)
            })
    }

    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.call_expression_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.call_expression_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            CallExpression::CallThenExpression(_, _, location)
            | CallExpression::CallThenName(_, _, location)
            | CallExpression::CallThenPrivateId(_, _, location) => *location,
            CallExpression::CallMemberExpression(node) => node.location(),
            CallExpression::SuperCall(node) => node.location(),
            CallExpression::ImportCall(node) => node.location(),
            CallExpression::CallThenArguments(chain, node) => chain.location().merge(&node.location()),
            CallExpression::CallThenTemplateLiteral(chain, node) => chain.location().merge(&node.location()),
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            CallExpression::CallMemberExpression(boxed) => boxed.contains(kind),
            CallExpression::SuperCall(boxed) => kind == ParseNodeKind::SuperCall || boxed.contains(kind),
            CallExpression::ImportCall(boxed) => boxed.contains(kind),
            CallExpression::CallThenArguments(ce, args) => ce.contains(kind) || args.contains(kind),
            CallExpression::CallThenExpression(ce, exp, _) => ce.contains(kind) || exp.contains(kind),
            CallExpression::CallThenName(ce, _, _) | CallExpression::CallThenPrivateId(ce, _, _) => ce.contains(kind),
            CallExpression::CallThenTemplateLiteral(ce, tl) => ce.contains(kind) || tl.contains(kind),
        }
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        match self {
            //  1. For each child node child of this Parse Node, do
            //      a. If child is an instance of a nonterminal, then
            //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
            //  2. Return true.
            CallExpression::CallMemberExpression(boxed) => boxed.all_private_identifiers_valid(names),
            CallExpression::SuperCall(boxed) => boxed.all_private_identifiers_valid(names),
            CallExpression::ImportCall(boxed) => boxed.all_private_identifiers_valid(names),
            CallExpression::CallThenArguments(ce, args) => {
                ce.all_private_identifiers_valid(names) && args.all_private_identifiers_valid(names)
            }
            CallExpression::CallThenExpression(ce, exp, _) => {
                ce.all_private_identifiers_valid(names) && exp.all_private_identifiers_valid(names)
            }
            CallExpression::CallThenName(ce, _, _) => ce.all_private_identifiers_valid(names),
            CallExpression::CallThenTemplateLiteral(ce, tl) => {
                ce.all_private_identifiers_valid(names) && tl.all_private_identifiers_valid(names)
            }

            // CallExpression : CallExpression . PrivateIdentifier
            //  1. If names contains the StringValue of PrivateIdentifier, then
            //      a. Return AllPrivateIdentifiersValid of CallExpression with argument names.
            //  2. Return false.
            CallExpression::CallThenPrivateId(ce, id, _) => {
                names.contains(&id.string_value) && ce.all_private_identifiers_valid(names)
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
            CallExpression::CallMemberExpression(cme) => cme.contains_arguments(),
            CallExpression::SuperCall(sc) => sc.contains_arguments(),
            CallExpression::ImportCall(ic) => ic.contains_arguments(),
            CallExpression::CallThenArguments(ce, a) => ce.contains_arguments() || a.contains_arguments(),
            CallExpression::CallThenExpression(ce, e, _) => ce.contains_arguments() || e.contains_arguments(),
            CallExpression::CallThenName(ce, _, _) | CallExpression::CallThenPrivateId(ce, _, _) => {
                ce.contains_arguments()
            }
            CallExpression::CallThenTemplateLiteral(ce, tl) => ce.contains_arguments() | tl.contains_arguments(),
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            CallExpression::CallMemberExpression(node) => node.early_errors(errs, strict),
            CallExpression::SuperCall(node) => node.early_errors(errs, strict),
            CallExpression::ImportCall(node) => node.early_errors(errs, strict),
            CallExpression::CallThenArguments(node, args) => {
                node.early_errors(errs, strict);
                args.early_errors(errs, strict);
            }
            CallExpression::CallThenExpression(node, exp, _) => {
                node.early_errors(errs, strict);
                exp.early_errors(errs, strict);
            }
            CallExpression::CallThenName(node, _, _) | CallExpression::CallThenPrivateId(node, _, _) => {
                node.early_errors(errs, strict);
            }
            CallExpression::CallThenTemplateLiteral(node, tl) => {
                node.early_errors(errs, strict);
                tl.early_errors(errs, strict, 0xffff_ffff);
            }
        }
    }

    pub(crate) fn is_strictly_deletable(&self) -> bool {
        !matches!(self, CallExpression::CallThenPrivateId(..))
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub(crate) fn assignment_target_type(&self) -> ATTKind {
        match self {
            CallExpression::CallMemberExpression(_)
            | CallExpression::SuperCall(_)
            | CallExpression::ImportCall(_)
            | CallExpression::CallThenArguments(..)
            | CallExpression::CallThenTemplateLiteral(..) => ATTKind::Invalid,
            CallExpression::CallThenExpression(..)
            | CallExpression::CallThenName(..)
            | CallExpression::CallThenPrivateId(..) => ATTKind::Simple,
        }
    }

    pub(crate) fn is_in_tail_position(&self, top: &ParsedText, strict: bool) -> bool {
        is_in_tail_position(&self.location(), top, strict)
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                CallExpression::CallMemberExpression(boxed) => boxed.body_containing_location(location),
                CallExpression::SuperCall(boxed) => boxed.body_containing_location(location),
                CallExpression::ImportCall(boxed) => boxed.body_containing_location(location),
                CallExpression::CallThenArguments(ce, args) => {
                    ce.body_containing_location(location).or_else(|| args.body_containing_location(location))
                }
                CallExpression::CallThenExpression(ce, exp, _) => {
                    ce.body_containing_location(location).or_else(|| exp.body_containing_location(location))
                }
                CallExpression::CallThenName(ce, _, _) | CallExpression::CallThenPrivateId(ce, _, _) => {
                    ce.body_containing_location(location)
                }
                CallExpression::CallThenTemplateLiteral(ce, tl) => {
                    ce.body_containing_location(location).or_else(|| tl.body_containing_location(location))
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
        // CallExpression :
        //      CoverCallExpressionAndAsyncArrowHead
        //      CallExpression Arguments
        //      CallExpression TemplateLiteral
        //  1. If this CallExpression is call, return true.
        //  2. Return false.
        // CallExpression :
        //      SuperCall
        //      ImportCall
        //      CallExpression [ Expression ]
        //      CallExpression . IdentifierName
        //      CallExpression . PrivateIdentifier
        //  1. Return false.
        match self {
            CallExpression::CallMemberExpression(_)
            | CallExpression::CallThenArguments(..)
            | CallExpression::CallThenTemplateLiteral(..) => self.location() == *location,
            CallExpression::SuperCall(_)
            | CallExpression::ImportCall(_)
            | CallExpression::CallThenExpression(..)
            | CallExpression::CallThenName(..)
            | CallExpression::CallThenPrivateId(..) => false,
        }
    }
}

// LeftHandSideExpression[Yield, Await] :
//      NewExpression[?Yield, ?Await]
//      CallExpression[?Yield, ?Await]
//      OptionalExpression[?Yield, ?Await]
#[derive(Debug)]
pub(crate) enum LeftHandSideExpression {
    New(Rc<NewExpression>),
    Call(Rc<CallExpression>),
    Optional(Rc<OptionalExpression>),
}

impl fmt::Display for LeftHandSideExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LeftHandSideExpression::New(boxed) => write!(f, "{boxed}"),
            LeftHandSideExpression::Call(boxed) => write!(f, "{boxed}"),
            LeftHandSideExpression::Optional(boxed) => write!(f, "{boxed}"),
        }
    }
}

impl PrettyPrint for LeftHandSideExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}LeftHandSideExpression: {self}")?;
        match &self {
            LeftHandSideExpression::New(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            LeftHandSideExpression::Call(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            LeftHandSideExpression::Optional(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            LeftHandSideExpression::New(node) => node.concise_with_leftpad(writer, pad, state),
            LeftHandSideExpression::Call(node) => node.concise_with_leftpad(writer, pad, state),
            LeftHandSideExpression::Optional(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl LeftHandSideExpression {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_arg: bool, await_arg: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::LeftHandSideExpression), scanner))
            .otherwise(|| {
                OptionalExpression::parse(parser, scanner, yield_arg, await_arg)
                    .map(|(opt, after_opt)| (Rc::new(Self::Optional(opt)), after_opt))
            })
            .otherwise(|| {
                CallExpression::parse(parser, scanner, yield_arg, await_arg)
                    .map(|(ce, after_ce)| (Rc::new(Self::Call(ce)), after_ce))
            })
            .otherwise(|| {
                NewExpression::parse(parser, scanner, yield_arg, await_arg)
                    .map(|(ne, after_ne)| (Rc::new(Self::New(ne)), after_ne))
            })
    }

    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.lhs_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.lhs_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            LeftHandSideExpression::New(node) => node.location(),
            LeftHandSideExpression::Call(node) => node.location(),
            LeftHandSideExpression::Optional(node) => node.location(),
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            LeftHandSideExpression::New(boxed) => boxed.contains(kind),
            LeftHandSideExpression::Call(boxed) => boxed.contains(kind),
            LeftHandSideExpression::Optional(boxed) => boxed.contains(kind),
        }
    }

    pub(crate) fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            LeftHandSideExpression::New(n) => n.as_string_literal(),
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
            LeftHandSideExpression::New(boxed) => boxed.all_private_identifiers_valid(names),
            LeftHandSideExpression::Call(boxed) => boxed.all_private_identifiers_valid(names),
            LeftHandSideExpression::Optional(boxed) => boxed.all_private_identifiers_valid(names),
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
            LeftHandSideExpression::New(ne) => ne.contains_arguments(),
            LeftHandSideExpression::Call(ce) => ce.contains_arguments(),
            LeftHandSideExpression::Optional(oe) => oe.contains_arguments(),
        }
    }

    pub(crate) fn is_object_or_array_literal(&self) -> bool {
        match self {
            LeftHandSideExpression::New(boxed) => boxed.is_object_or_array_literal(),
            LeftHandSideExpression::Call(_) | LeftHandSideExpression::Optional(_) => false,
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            LeftHandSideExpression::New(boxed) => boxed.early_errors(errs, strict),
            LeftHandSideExpression::Call(boxed) => boxed.early_errors(errs, strict),
            LeftHandSideExpression::Optional(boxed) => boxed.early_errors(errs, strict),
        }
    }

    pub(crate) fn is_strictly_deletable(&self) -> bool {
        match self {
            LeftHandSideExpression::New(node) => node.is_strictly_deletable(),
            LeftHandSideExpression::Call(node) => node.is_strictly_deletable(),
            LeftHandSideExpression::Optional(node) => node.is_strictly_deletable(),
        }
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub(crate) fn assignment_target_type(&self, strict: bool) -> ATTKind {
        match self {
            LeftHandSideExpression::New(boxed) => boxed.assignment_target_type(strict),
            LeftHandSideExpression::Call(boxed) => boxed.assignment_target_type(),
            LeftHandSideExpression::Optional(_) => ATTKind::Invalid,
        }
    }

    /// True if this production winds up being an IdentifierRef
    ///
    /// See [IsIdentifierRef](https://tc39.es/ecma262/#sec-static-semantics-isidentifierref) from ECMA-262.
    //#[cfg(test)]
    //pub(crate) fn is_identifier_ref(&self) -> bool {
    //    match self {
    //        LeftHandSideExpression::Call(_) | LeftHandSideExpression::Optional(_) => false,
    //        LeftHandSideExpression::New(x) => x.is_identifier_ref(),
    //    }
    //}
    pub(crate) fn identifier_ref(&self) -> Option<Rc<IdentifierReference>> {
        match self {
            LeftHandSideExpression::Call(_) | LeftHandSideExpression::Optional(_) => None,
            LeftHandSideExpression::New(x) => x.identifier_ref(),
        }
    }

    //pub(crate) fn is_named_function(&self) -> bool {
    //    match self {
    //        LeftHandSideExpression::New(node) => node.is_named_function(),
    //        _ => false,
    //    }
    //}

    pub(crate) fn is_destructuring(&self) -> bool {
        // Static Semantics: IsDestructuring
        // The syntax-directed operation IsDestructuring takes no arguments and returns a Boolean. It is
        // defined piecewise over the following productions:
        match self {
            LeftHandSideExpression::New(ne) => {
                // LeftHandSideExpression : NewExpression
                //  1. Return IsDestructuring of NewExpression.
                ne.is_destructuring()
            }
            LeftHandSideExpression::Call(_) | LeftHandSideExpression::Optional(_) => {
                // LeftHandSideExpression :
                //      CallExpression
                //      OptionalExpression
                //  1. Return false.
                false
            }
        }
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                LeftHandSideExpression::New(node) => node.body_containing_location(location),
                LeftHandSideExpression::Call(node) => node.body_containing_location(location),
                LeftHandSideExpression::Optional(node) => node.body_containing_location(location),
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
        // LeftHandSideExpression :
        //      NewExpression
        //      CallExpression
        //      OptionalExpression
        match self {
            LeftHandSideExpression::New(new_expression) => new_expression.has_call_in_tail_position(location),
            LeftHandSideExpression::Call(call_expression) => call_expression.has_call_in_tail_position(location),
            LeftHandSideExpression::Optional(optional_expression) => {
                optional_expression.has_call_in_tail_position(location)
            }
        }
    }
}

// OptionalExpression[Yield, Await] :
//      MemberExpression[?Yield, ?Await] OptionalChain[?Yield, ?Await]
//      CallExpression[?Yield, ?Await] OptionalChain[?Yield, ?Await]
//      OptionalExpression[?Yield, ?Await] OptionalChain[?Yield, ?Await]
#[derive(Debug)]
pub(crate) enum OptionalExpression {
    Member(Rc<MemberExpression>, Rc<OptionalChain>),
    Call(Rc<CallExpression>, Rc<OptionalChain>),
    Opt(Rc<OptionalExpression>, Rc<OptionalChain>),
}

impl fmt::Display for OptionalExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OptionalExpression::Member(left, right) => {
                write!(f, "{left} {right}")
            }
            OptionalExpression::Call(left, right) => write!(f, "{left} {right}"),
            OptionalExpression::Opt(left, right) => write!(f, "{left} {right}"),
        }
    }
}

impl PrettyPrint for OptionalExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}OptionalExpression: {self}")?;
        match self {
            OptionalExpression::Member(left, right) => {
                left.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                right.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalExpression::Call(left, right) => {
                left.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                right.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalExpression::Opt(left, right) => {
                left.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                right.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}OptionalExpression: {self}")?;
        match self {
            OptionalExpression::Member(left, right) => {
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalExpression::Call(left, right) => {
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalExpression::Opt(left, right) => {
                left.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl OptionalExpression {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::OptionalExpression), scanner))
            .otherwise(|| {
                MemberExpression::parse(parser, scanner, yield_flag, await_flag).and_then(|(me, after_me)| {
                    let (oc, after_oc) = OptionalChain::parse(parser, after_me, yield_flag, await_flag)?;
                    Ok((Rc::new(OptionalExpression::Member(me, oc)), after_oc))
                })
            })
            .otherwise(|| {
                CallExpression::parse(parser, scanner, yield_flag, await_flag).and_then(|(ce, after_ce)| {
                    let (oc, after_oc) = OptionalChain::parse(parser, after_ce, yield_flag, await_flag)?;
                    Ok((Rc::new(OptionalExpression::Call(ce, oc)), after_oc))
                })
            })
            .map(|(opt, after_opt)| {
                let mut current = opt;
                let mut current_scanner = after_opt;
                while let Ok((oc, after_oc)) = OptionalChain::parse(parser, current_scanner, yield_flag, await_flag) {
                    current = Rc::new(OptionalExpression::Opt(current, oc));
                    current_scanner = after_oc;
                }
                (current, current_scanner)
            })
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            OptionalExpression::Member(exp, chain) => exp.location().merge(&chain.location()),
            OptionalExpression::Call(exp, chain) => exp.location().merge(&chain.location()),
            OptionalExpression::Opt(exp, chain) => exp.location().merge(&chain.location()),
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            OptionalExpression::Member(left, right) => left.contains(kind) || right.contains(kind),
            OptionalExpression::Call(left, right) => left.contains(kind) || right.contains(kind),
            OptionalExpression::Opt(left, right) => left.contains(kind) || right.contains(kind),
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
            OptionalExpression::Member(left, right) => {
                left.all_private_identifiers_valid(names) && right.all_private_identifiers_valid(names)
            }
            OptionalExpression::Call(left, right) => {
                left.all_private_identifiers_valid(names) && right.all_private_identifiers_valid(names)
            }
            OptionalExpression::Opt(left, right) => {
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
            OptionalExpression::Member(me, oc) => me.contains_arguments() || oc.contains_arguments(),
            OptionalExpression::Call(ce, oc) => ce.contains_arguments() || oc.contains_arguments(),
            OptionalExpression::Opt(oe, oc) => oe.contains_arguments() || oc.contains_arguments(),
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            OptionalExpression::Member(left, right) => {
                left.early_errors(errs, strict);
                right.early_errors(errs, strict);
            }
            OptionalExpression::Call(left, right) => {
                left.early_errors(errs, strict);
                right.early_errors(errs, strict);
            }
            OptionalExpression::Opt(left, right) => {
                left.early_errors(errs, strict);
                right.early_errors(errs, strict);
            }
        }
    }

    pub(crate) fn is_strictly_deletable(&self) -> bool {
        match self {
            OptionalExpression::Member(_, chain)
            | OptionalExpression::Call(_, chain)
            | OptionalExpression::Opt(_, chain) => chain.is_strictly_deletable(),
        }
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                OptionalExpression::Member(me, oc) => {
                    me.body_containing_location(location).or_else(|| oc.body_containing_location(location))
                }
                OptionalExpression::Call(ce, oc) => {
                    ce.body_containing_location(location).or_else(|| oc.body_containing_location(location))
                }
                OptionalExpression::Opt(oe, oc) => {
                    oe.body_containing_location(location).or_else(|| oc.body_containing_location(location))
                }
            }
        } else {
            None
        }
    }

    #[expect(unused_variables)]
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
        todo!()
    }
}

// OptionalChain[Yield, Await] :
//      ?. Arguments[?Yield, ?Await]
//      ?. [ Expression[+In, ?Yield, ?Await] ]
//      ?. IdentifierName
//      ?. TemplateLiteral[?Yield, ?Await, +Tagged]
//      ?. PrivateIdentifier
//      OptionalChain[?Yield, ?Await] Arguments[?Yield, ?Await]
//      OptionalChain[?Yield, ?Await] [ Expression[+In, ?Yield, ?Await] ]
//      OptionalChain[?Yield, ?Await] . IdentifierName
//      OptionalChain[?Yield, ?Await] TemplateLiteral[?Yield, ?Await, +Tagged]
//      OptionalChain[?Yield, ?Await] . PrivateIdentifier
#[derive(Debug)]
pub(crate) enum OptionalChain {
    Args(Rc<Arguments>, Location),
    Exp(Rc<Expression>, Location),
    Ident(IdentifierData, Location),
    Template(Rc<TemplateLiteral>, Location),
    PrivateId(IdentifierData, Location),
    PlusArgs(Rc<OptionalChain>, Rc<Arguments>),
    PlusExp(Rc<OptionalChain>, Rc<Expression>, Location),
    PlusIdent(Rc<OptionalChain>, IdentifierData, Location),
    PlusTemplate(Rc<OptionalChain>, Rc<TemplateLiteral>),
    PlusPrivateId(Rc<OptionalChain>, IdentifierData, Location),
}

impl fmt::Display for OptionalChain {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OptionalChain::Args(node, _) => write!(f, "?. {node}"),
            OptionalChain::Exp(node, _) => write!(f, "?. [ {node} ]"),
            OptionalChain::Ident(node, _) | OptionalChain::PrivateId(node, _) => {
                write!(f, "?. {node}")
            }
            OptionalChain::Template(node, _) => write!(f, "?. {node}"),
            OptionalChain::PlusArgs(lst, item) => write!(f, "{lst} {item}"),
            OptionalChain::PlusExp(lst, item, _) => write!(f, "{lst} [ {item} ]"),
            OptionalChain::PlusIdent(lst, item, _) | OptionalChain::PlusPrivateId(lst, item, _) => {
                write!(f, "{lst} . {item}")
            }
            OptionalChain::PlusTemplate(lst, item) => write!(f, "{lst} {item}"),
        }
    }
}

impl PrettyPrint for OptionalChain {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}OptionalChain: {self}")?;
        match self {
            OptionalChain::Args(node, _) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            OptionalChain::Exp(node, _) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            OptionalChain::Ident(node, _) => {
                pprint_token(writer, node, TokenType::IdentifierName, &successive, Spot::Final)
            }
            OptionalChain::Template(node, _) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            OptionalChain::PrivateId(node, _) => {
                pprint_token(writer, node, TokenType::PrivateIdentifier, &successive, Spot::Final)
            }
            OptionalChain::PlusArgs(lst, item) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalChain::PlusExp(lst, item, _) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalChain::PlusIdent(lst, item, _) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, item, TokenType::IdentifierName, &successive, Spot::Final)
            }
            OptionalChain::PlusTemplate(lst, item) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalChain::PlusPrivateId(lst, item, _) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, item, TokenType::PrivateIdentifier, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}OptionalChain: {self}")?;
        match self {
            OptionalChain::Args(node, _) => {
                pprint_token(writer, "?.", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalChain::Exp(node, _) => {
                pprint_token(writer, "?.", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            OptionalChain::Ident(node, _) => {
                pprint_token(writer, "?.", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, node, TokenType::IdentifierName, &successive, Spot::Final)
            }
            OptionalChain::Template(node, _) => {
                pprint_token(writer, "?.", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalChain::PrivateId(node, _) => {
                pprint_token(writer, "?.", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, node, TokenType::PrivateIdentifier, &successive, Spot::Final)
            }
            OptionalChain::PlusArgs(lst, item) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalChain::PlusExp(lst, item, _) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            OptionalChain::PlusIdent(lst, item, _) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, item, TokenType::IdentifierName, &successive, Spot::Final)
            }
            OptionalChain::PlusTemplate(lst, item) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalChain::PlusPrivateId(lst, item, _) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, item, TokenType::PrivateIdentifier, &successive, Spot::Final)
            }
        }
    }
}

impl OptionalChain {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        enum Follow {
            Args(Rc<Arguments>),
            TLit(Rc<TemplateLiteral>),
            Exp(Rc<Expression>, Location),
            Id(IdentifierData, Location),
            Pid(IdentifierData, Location),
        }

        let (opt_loc, after_opt) = scan_for_punct(scanner, parser.source, InputElementGoal::Div, Punctuator::QDot)?;
        let (mut current, mut current_scan) = Err(ParseError::new(PECode::ChainFailed, after_opt))
            .otherwise(|| {
                let (args, after_args) = Arguments::parse(parser, after_opt, yield_flag, await_flag)?;
                let location = opt_loc.merge(&args.location());
                Ok((Rc::new(OptionalChain::Args(args, location)), after_args))
            })
            .otherwise(|| {
                let (tl, after_tl) = TemplateLiteral::parse(parser, after_opt, yield_flag, await_flag, true)?;
                let location = opt_loc.merge(&tl.location());
                Ok((Rc::new(OptionalChain::Template(tl, location)), after_tl))
            })
            .otherwise(|| {
                let (_, after_lb) =
                    scan_for_punct(after_opt, parser.source, InputElementGoal::Div, Punctuator::LeftBracket)?;
                let (exp, after_exp) = Expression::parse(parser, after_lb, true, yield_flag, await_flag)?;
                let (rb_loc, after_rb) =
                    scan_for_punct(after_exp, parser.source, InputElementGoal::Div, Punctuator::RightBracket)?;
                let location = opt_loc.merge(&rb_loc);
                Ok((Rc::new(OptionalChain::Exp(exp, location)), after_rb))
            })
            .otherwise(|| {
                let (id, id_loc, after_id) = scan_for_identifiername(after_opt, parser.source, InputElementGoal::Div)?;
                let location = opt_loc.merge(&id_loc);
                Ok((Rc::new(OptionalChain::Ident(id, location)), after_id))
            })
            .otherwise(|| {
                scan_for_private_identifier(after_opt, parser.source, InputElementGoal::Div).map(
                    |(id, id_loc, after_id)| {
                        let location = opt_loc.merge(&id_loc);
                        (Rc::new(OptionalChain::PrivateId(id, location)), after_id)
                    },
                )
            })?;

        while let Ok((follow, scan)) = Err(ParseError::new(PECode::Generic, current_scan))
            .otherwise(|| {
                let (args, after_args) = Arguments::parse(parser, current_scan, yield_flag, await_flag)?;
                Ok((Follow::Args(args), after_args))
            })
            .otherwise(|| {
                let (tl, after_tl) = TemplateLiteral::parse(parser, current_scan, yield_flag, await_flag, true)?;
                Ok((Follow::TLit(tl), after_tl))
            })
            .otherwise(|| {
                let (punct, _, after_punct) = scan_for_punct_set(
                    current_scan,
                    parser.source,
                    InputElementGoal::Div,
                    &[Punctuator::Dot, Punctuator::LeftBracket],
                )?;
                match punct {
                    Punctuator::Dot => scan_for_identifiername(after_punct, parser.source, InputElementGoal::Div)
                        .map(|(id, id_loc, after_id)| (Follow::Id(id, id_loc), after_id))
                        .otherwise(|| {
                            scan_for_private_identifier(after_punct, parser.source, InputElementGoal::Div)
                                .map(|(id, id_loc, after_id)| (Follow::Pid(id, id_loc), after_id))
                        }),
                    _ => {
                        let (exp, after_exp) = Expression::parse(parser, after_punct, true, yield_flag, await_flag)?;
                        let (rb_loc, after_rb) =
                            scan_for_punct(after_exp, parser.source, InputElementGoal::Div, Punctuator::RightBracket)?;
                        Ok((Follow::Exp(exp, rb_loc), after_rb))
                    }
                }
            })
        {
            current = Rc::new(match follow {
                Follow::Args(args) => OptionalChain::PlusArgs(current, args),
                Follow::Id(id, id_loc) => {
                    let location = current.location().merge(&id_loc);
                    OptionalChain::PlusIdent(current, id, location)
                }
                Follow::Exp(exp, end_loc) => {
                    let location = current.location().merge(&end_loc);
                    OptionalChain::PlusExp(current, exp, location)
                }
                Follow::TLit(tl) => OptionalChain::PlusTemplate(current, tl),
                Follow::Pid(id, id_loc) => {
                    let location = current.location().merge(&id_loc);
                    OptionalChain::PlusPrivateId(current, id, location)
                }
            });
            current_scan = scan;
        }
        Ok((current, current_scan))
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            OptionalChain::Args(_, location)
            | OptionalChain::Exp(_, location)
            | OptionalChain::Ident(_, location)
            | OptionalChain::Template(_, location)
            | OptionalChain::PrivateId(_, location)
            | OptionalChain::PlusExp(_, _, location)
            | OptionalChain::PlusIdent(_, _, location)
            | OptionalChain::PlusPrivateId(_, _, location) => *location,
            OptionalChain::PlusArgs(chain, args) => chain.location().merge(&args.location()),
            OptionalChain::PlusTemplate(chain, templ) => chain.location().merge(&templ.location()),
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            OptionalChain::Args(node, _) => node.contains(kind),
            OptionalChain::Exp(node, _) => node.contains(kind),
            OptionalChain::Ident(_, _) | OptionalChain::PrivateId(_, _) => false,
            OptionalChain::Template(node, _) => node.contains(kind),
            OptionalChain::PlusArgs(lst, item) => lst.contains(kind) || item.contains(kind),
            OptionalChain::PlusExp(lst, item, _) => lst.contains(kind) || item.contains(kind),
            OptionalChain::PlusIdent(lst, _, _) | OptionalChain::PlusPrivateId(lst, _, _) => lst.contains(kind),
            OptionalChain::PlusTemplate(lst, item) => lst.contains(kind) || item.contains(kind),
        }
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        match self {
            //  1. For each child node child of this Parse Node, do
            //      a. If child is an instance of a nonterminal, then
            //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
            //  2. Return true.
            OptionalChain::Args(node, _) => node.all_private_identifiers_valid(names),
            OptionalChain::Exp(node, _) => node.all_private_identifiers_valid(names),
            OptionalChain::Ident(_, _) => true,
            OptionalChain::Template(node, _) => node.all_private_identifiers_valid(names),
            OptionalChain::PlusArgs(lst, item) => {
                lst.all_private_identifiers_valid(names) && item.all_private_identifiers_valid(names)
            }
            OptionalChain::PlusExp(lst, item, _) => {
                lst.all_private_identifiers_valid(names) && item.all_private_identifiers_valid(names)
            }
            OptionalChain::PlusIdent(lst, _, _) => lst.all_private_identifiers_valid(names),
            OptionalChain::PlusTemplate(lst, item) => {
                lst.all_private_identifiers_valid(names) && item.all_private_identifiers_valid(names)
            }

            // OptionalChain : ?. PrivateIdentifier
            //  1. If names contains the StringValue of PrivateIdentifier, return true.
            //  2. Return false.
            OptionalChain::PrivateId(pid, _) => names.contains(&pid.string_value),
            // OptionalChain : OptionalChain . PrivateIdentifier
            //  1. If names contains the StringValue of PrivateIdentifier, then
            //      a. Return AllPrivateIdentifiersValid of OptionalChain with argument names.
            //  2. Return false.
            OptionalChain::PlusPrivateId(lst, pid, _) => {
                names.contains(&pid.string_value) && lst.all_private_identifiers_valid(names)
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
            OptionalChain::Args(a, _) => a.contains_arguments(),
            OptionalChain::Exp(e, _) => e.contains_arguments(),
            OptionalChain::Ident(_, _) | OptionalChain::PrivateId(_, _) => false,
            OptionalChain::Template(tl, _) => tl.contains_arguments(),
            OptionalChain::PlusArgs(oc, a) => oc.contains_arguments() || a.contains_arguments(),
            OptionalChain::PlusExp(oc, e, _) => oc.contains_arguments() || e.contains_arguments(),
            OptionalChain::PlusIdent(oc, _, _) | OptionalChain::PlusPrivateId(oc, _, _) => oc.contains_arguments(),
            OptionalChain::PlusTemplate(oc, tl) => oc.contains_arguments() || tl.contains_arguments(),
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        //  OptionalChain :
        //      ?. TemplateLiteral
        //      OptionalChain TemplateLiteral
        //  * It is a Syntax Error if any source text is matched by this production.
        //
        // NOTE | This production exists in order to prevent automatic semicolon insertion rules (12.9) from being
        //      | applied to the following code:
        //      |       a?.b
        //      |       `c`
        //      | so that it would be interpreted as two valid statements. The purpose is to maintain consistency with
        //      | similar code without optional chaining:
        //      |       a.b
        //      |       `c`
        //      | which is a valid statement and where automatic semicolon insertion does not apply.
        match self {
            OptionalChain::Template(tl, _) => {
                errs.push(create_syntax_error_object("Template literal not allowed here", Some(tl.location())));
                tl.early_errors(errs, strict, 0xffff_ffff);
            }
            OptionalChain::PlusTemplate(node, tl) => {
                node.early_errors(errs, strict);
                errs.push(create_syntax_error_object("Template literal not allowed here", Some(tl.location())));
                tl.early_errors(errs, strict, 0xffff_ffff);
            }
            OptionalChain::Args(node, _) => node.early_errors(errs, strict),
            OptionalChain::Exp(node, _) => node.early_errors(errs, strict),
            OptionalChain::Ident(_, _) | OptionalChain::PrivateId(_, _) => {}
            OptionalChain::PlusArgs(node, args) => {
                node.early_errors(errs, strict);
                args.early_errors(errs, strict);
            }
            OptionalChain::PlusExp(node, exp, _) => {
                node.early_errors(errs, strict);
                exp.early_errors(errs, strict);
            }
            OptionalChain::PlusIdent(node, _, _) | OptionalChain::PlusPrivateId(node, _, _) => {
                node.early_errors(errs, strict);
            }
        }
    }

    pub(crate) fn is_strictly_deletable(&self) -> bool {
        !matches!(self, OptionalChain::PrivateId(..) | OptionalChain::PlusPrivateId(..))
    }

    pub(crate) fn is_in_tail_position(&self, top: &ParsedText, strict: bool) -> bool {
        is_in_tail_position(&self.location(), top, strict)
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                OptionalChain::Args(node, _) => node.body_containing_location(location),
                OptionalChain::Exp(node, _) => node.body_containing_location(location),
                OptionalChain::Ident(_, _) | OptionalChain::PrivateId(_, _) => None,
                OptionalChain::Template(node, _) => node.body_containing_location(location),
                OptionalChain::PlusArgs(lst, args) => {
                    lst.body_containing_location(location).or_else(|| args.body_containing_location(location))
                }
                OptionalChain::PlusExp(lst, exp, _) => {
                    lst.body_containing_location(location).or_else(|| exp.body_containing_location(location))
                }
                OptionalChain::PlusIdent(lst, _, _) | OptionalChain::PlusPrivateId(lst, _, _) => {
                    lst.body_containing_location(location)
                }
                OptionalChain::PlusTemplate(lst, tl) => {
                    lst.body_containing_location(location).or_else(|| tl.body_containing_location(location))
                }
            }
        } else {
            None
        }
    }
}

pub(crate) fn is_in_tail_position(location: &Location, top: &ParsedText, strict: bool) -> bool {
    // Static Semantics: IsInTailPosition ( call )
    //
    // The abstract operation IsInTailPosition takes argument call (a CallExpression Parse Node, a MemberExpression
    // Parse Node, or an OptionalChain Parse Node) and returns a Boolean. It performs the following steps when
    // called:
    //
    //  1. If IsStrict(call) is false, return false.
    //  2. If call is not contained within a FunctionBody, a ConciseBody, or an AsyncConciseBody, return false.
    //  3. Let body be the FunctionBody, ConciseBody, or AsyncConciseBody that most closely contains call.
    //  4. If body is the FunctionBody of a GeneratorBody, return false.
    //  5. If body is the FunctionBody of an AsyncFunctionBody, return false.
    //  6. If body is the FunctionBody of an AsyncGeneratorBody, return false.
    //  7. If body is an AsyncConciseBody, return false.
    //  8. Return the result of HasCallInTailPosition of body with argument call.
    //
    // Note: Tail Position calls are only defined in strict mode code because of a common non-standard language
    //       extension (see 10.2.4) that enables observation of the chain of caller contexts.
    if !strict {
        return false;
    }
    let body = top.body_containing_location(location);
    match &body {
        None /*| Some(ContainingBody::AsyncConciseBody(_))*/ => false,
        Some(ContainingBody::ConciseBody(body)) => {
            // Otherwise, HasCallInTailPosition of body with argument call.
            body.has_call_in_tail_position(location)
        }
        Some(ContainingBody::FunctionBody(body)) => {
            // If body is the FunctionBody of a GeneratorBody, return false.
            if body.is_generator_body() {
                return false;
            }
            // If body is the FunctionBody of an AsyncFunctionBody, return false.
            if body.is_async_function_body() {
                return false;
            }
            // If body is the FunctionBody of an AsyncGeneratorBody, return false.
            if body.is_async_generator_body() {
                return false;
            }
            // Otherwise, HasCallInTailPosition of body with argument call.
            body.has_call_in_tail_position(location)
        }
    }
}

#[cfg(test)]
mod tests;
