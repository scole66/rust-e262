use super::assignment_operators::AssignmentExpression;
use super::comma_operator::Expression;
use super::primary_expressions::PrimaryExpression;
use super::primary_expressions::TemplateLiteral;
use super::scanner::{IdentifierData, Keyword, Punctuator, ScanGoal, Scanner, StringToken};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};
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
pub enum MemberExpression {
    PrimaryExpression(Rc<PrimaryExpression>),
    Expression(Rc<MemberExpression>, Rc<Expression>),
    IdentifierName(Rc<MemberExpression>, IdentifierData),
    TemplateLiteral(Rc<MemberExpression>, Rc<TemplateLiteral>),
    SuperProperty(Rc<SuperProperty>),
    MetaProperty(Rc<MetaProperty>),
    NewArguments(Rc<MemberExpression>, Rc<Arguments>),
    PrivateId(Rc<MemberExpression>, IdentifierData),
}

impl fmt::Display for MemberExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MemberExpression::PrimaryExpression(boxed) => write!(f, "{}", boxed),
            MemberExpression::Expression(me, exp) => {
                write!(f, "{} [ {} ]", me, exp)
            }
            MemberExpression::IdentifierName(me, id) | MemberExpression::PrivateId(me, id) => {
                write!(f, "{} . {}", me, id)
            }
            MemberExpression::TemplateLiteral(me, tl) => write!(f, "{} {}", me, tl),
            MemberExpression::SuperProperty(boxed) => write!(f, "{}", boxed),
            MemberExpression::MetaProperty(boxed) => write!(f, "{}", boxed),
            MemberExpression::NewArguments(me, args) => write!(f, "new {} {}", me, args),
        }
    }
}

impl PrettyPrint for MemberExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}MemberExpression: {}", first, self)?;
        match self {
            MemberExpression::PrimaryExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            MemberExpression::Expression(me, exp) => {
                me.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                exp.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            MemberExpression::IdentifierName(me, _) | MemberExpression::PrivateId(me, _) => me.pprint_with_leftpad(writer, &successive, Spot::Final),
            MemberExpression::TemplateLiteral(me, tl) => {
                me.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                tl.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            MemberExpression::SuperProperty(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            MemberExpression::MetaProperty(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            MemberExpression::NewArguments(me, args) => {
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
            writeln!(writer, "{}MemberExpression: {}", first, self).and(Ok(successive))
        };
        match self {
            MemberExpression::PrimaryExpression(node) => node.concise_with_leftpad(writer, pad, state),
            MemberExpression::SuperProperty(node) => node.concise_with_leftpad(writer, pad, state),
            MemberExpression::MetaProperty(node) => node.concise_with_leftpad(writer, pad, state),
            MemberExpression::Expression(me, exp) => {
                let successive = head(pad, state)?;
                me.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                exp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            MemberExpression::IdentifierName(me, id) => {
                let successive = head(pad, state)?;
                me.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, id, TokenType::IdentifierName, &successive, Spot::Final)
            }
            MemberExpression::PrivateId(me, id) => {
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
            MemberExpression::NewArguments(me, args) => {
                let successive = head(pad, state)?;
                pprint_token(writer, "new", TokenType::Keyword, &successive, Spot::NotFinal)?;
                me.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                args.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for MemberExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            MemberExpression::PrimaryExpression(boxed) => boxed.is_function_definition(),
            MemberExpression::Expression(..)
            | MemberExpression::IdentifierName(..)
            | MemberExpression::TemplateLiteral(..)
            | MemberExpression::SuperProperty(_)
            | MemberExpression::MetaProperty(_)
            | MemberExpression::NewArguments(..)
            | MemberExpression::PrivateId(..) => false,
        }
    }
}

pub trait ToMemberExpression {
    fn to_member_expression_kind(node: Rc<Self>) -> MemberExpression;
}

impl ToMemberExpression for PrimaryExpression {
    fn to_member_expression_kind(node: Rc<Self>) -> MemberExpression {
        MemberExpression::PrimaryExpression(node)
    }
}

impl ToMemberExpression for SuperProperty {
    fn to_member_expression_kind(node: Rc<Self>) -> MemberExpression {
        MemberExpression::SuperProperty(node)
    }
}

impl ToMemberExpression for MetaProperty {
    fn to_member_expression_kind(node: Rc<Self>) -> MemberExpression {
        MemberExpression::MetaProperty(node)
    }
}

fn me_boxer<T>(pair: (Rc<T>, Scanner)) -> ParseResult<MemberExpression>
where
    T: ToMemberExpression,
{
    let (node, scanner) = pair;
    Ok((Rc::new(T::to_member_expression_kind(node)), scanner))
}

fn member_expression_head_recursive(parser: &mut Parser, yield_flag: bool, await_flag: bool, me: Rc<MemberExpression>, scan: Scanner) -> Result<(Rc<MemberExpression>, Scanner), ParseError> {
    enum After {
        Exp(Rc<Expression>),
        Id(IdentifierData),
        TLit(Rc<TemplateLiteral>),
        Pid(IdentifierData),
    }
    let mut current_me = me;
    let mut after_scan = scan;
    while let Ok((parts, after_production)) = TemplateLiteral::parse(parser, after_scan, yield_flag, await_flag, true).map(|(tl, after_tl)| (After::TLit(tl), after_tl)).otherwise(|| {
        scan_for_punct_set(after_scan, parser.source, ScanGoal::InputElementRegExp, &[Punctuator::Dot, Punctuator::LeftBracket]).and_then(|(punct, after)| match punct {
            Punctuator::Dot => scan_for_identifiername(after, parser.source, ScanGoal::InputElementRegExp)
                .map(|(id, after_id)| (After::Id(id), after_id))
                .otherwise(|| scan_for_private_identifier(after, parser.source, ScanGoal::InputElementRegExp).map(|(id, after_id)| (After::Pid(id), after_id))),
            _ => Expression::parse(parser, after, true, yield_flag, await_flag).and_then(|(expression, after_exp)| {
                scan_for_punct(after_exp, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBracket).map(|after_bracket| (After::Exp(expression), after_bracket))
            }),
        })
    }) {
        current_me = match parts {
            After::TLit(tl) => Rc::new(MemberExpression::TemplateLiteral(current_me, tl)),
            After::Exp(exp) => Rc::new(MemberExpression::Expression(current_me, exp)),
            After::Id(id) => Rc::new(MemberExpression::IdentifierName(current_me, id)),
            After::Pid(id) => Rc::new(MemberExpression::PrivateId(current_me, id)),
        };
        after_scan = after_production;
    }
    Ok((current_me, after_scan))
}

impl MemberExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
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
            .otherwise(|| PrimaryExpression::parse(parser, scanner, yield_flag, await_flag).and_then(me_boxer))
            .otherwise(|| SuperProperty::parse(parser, scanner, yield_flag, await_flag).and_then(me_boxer))
            .otherwise(|| MetaProperty::parse(parser, scanner).and_then(me_boxer))
            .otherwise(|| Self::new_memberexpression_arguments(parser, scanner, yield_flag, await_flag).map(|(me, args, after)| (Rc::new(MemberExpression::NewArguments(me, args)), after)))
            // And then all the head-recursive productions.
            .and_then(|(me, scan)| member_expression_head_recursive(parser, yield_flag, await_flag, me, scan))
    }
    fn new_memberexpression_arguments(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Rc<MemberExpression>, Rc<Arguments>, Scanner), ParseError> {
        let after_new = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::New)?;
        let (me, after_me) = MemberExpression::parse(parser, after_new, yield_flag, await_flag)?;
        let (args, after_args) = Arguments::parse(parser, after_me, yield_flag, await_flag)?;
        Ok((me, args, after_args))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            MemberExpression::PrimaryExpression(n) => n.contains(kind),
            MemberExpression::Expression(l, r) => l.contains(kind) || r.contains(kind),
            MemberExpression::IdentifierName(n, _) | MemberExpression::PrivateId(n, _) => n.contains(kind),
            MemberExpression::TemplateLiteral(l, r) => l.contains(kind) || r.contains(kind),
            MemberExpression::SuperProperty(n) => kind == ParseNodeKind::SuperProperty || n.contains(kind),
            MemberExpression::MetaProperty(n) => n.contains(kind),
            MemberExpression::NewArguments(l, r) => l.contains(kind) || r.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            MemberExpression::PrimaryExpression(n) => n.as_string_literal(),
            _ => None,
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        match self {
            //  1. For each child node child of this Parse Node, do
            //      a. If child is an instance of a nonterminal, then
            //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
            //  2. Return true.
            MemberExpression::PrimaryExpression(n) => n.all_private_identifiers_valid(names),
            MemberExpression::Expression(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),
            MemberExpression::IdentifierName(n, _) => n.all_private_identifiers_valid(names),
            MemberExpression::TemplateLiteral(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),
            MemberExpression::SuperProperty(n) => n.all_private_identifiers_valid(names),
            MemberExpression::MetaProperty(_) => true,
            MemberExpression::NewArguments(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),

            // MemberExpression : MemberExpression . PrivateIdentifier
            //  1. If names contains the StringValue of PrivateIdentifier, then
            //      a. Return AllPrivateIdentifiersValid of MemberExpression with argument names.
            //  2. Return false.
            MemberExpression::PrivateId(n, id) => names.contains(&id.string_value) && n.all_private_identifiers_valid(names),
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
            MemberExpression::PrimaryExpression(pe) => pe.contains_arguments(),
            MemberExpression::Expression(me, e) => me.contains_arguments() || e.contains_arguments(),
            MemberExpression::IdentifierName(me, _) | MemberExpression::PrivateId(me, _) => me.contains_arguments(),
            MemberExpression::TemplateLiteral(me, tl) => me.contains_arguments() || tl.contains_arguments(),
            MemberExpression::SuperProperty(sp) => sp.contains_arguments(),
            MemberExpression::MetaProperty(_) => false,
            MemberExpression::NewArguments(me, a) => me.contains_arguments() || a.contains_arguments(),
        }
    }

    pub fn is_object_or_array_literal(&self) -> bool {
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

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            MemberExpression::PrimaryExpression(n) => n.early_errors(agent, errs, strict),
            MemberExpression::Expression(l, r) => {
                l.early_errors(agent, errs, strict);
                r.early_errors(agent, errs, strict);
            }
            MemberExpression::IdentifierName(n, _) => n.early_errors(agent, errs, strict),
            MemberExpression::TemplateLiteral(l, r) => {
                l.early_errors(agent, errs, strict);
                r.early_errors(agent, errs, strict, 0xffff_ffff);
            }
            MemberExpression::SuperProperty(n) => n.early_errors(agent, errs, strict),
            MemberExpression::MetaProperty(meta) => meta.early_errors(agent, errs),
            MemberExpression::NewArguments(l, r) => {
                l.early_errors(agent, errs, strict);
                r.early_errors(agent, errs, strict);
            }
            MemberExpression::PrivateId(n, _) => n.early_errors(agent, errs, strict),
        }
    }

    pub fn is_strictly_deletable(&self) -> bool {
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
    pub fn assignment_target_type(&self, strict: bool) -> ATTKind {
        match self {
            MemberExpression::PrimaryExpression(boxed) => boxed.assignment_target_type(strict),
            MemberExpression::Expression(..) => ATTKind::Simple,
            MemberExpression::IdentifierName(..) | MemberExpression::PrivateId(..) => ATTKind::Simple,
            MemberExpression::TemplateLiteral(..) => ATTKind::Invalid,
            MemberExpression::SuperProperty(..) => ATTKind::Simple,
            MemberExpression::MetaProperty(..) => ATTKind::Invalid,
            MemberExpression::NewArguments(..) => ATTKind::Invalid,
        }
    }

    /// True if this production winds up being an IdentifierRef
    ///
    /// See [IsIdentifierRef](https://tc39.es/ecma262/#sec-static-semantics-isidentifierref) from ECMA-262.
    pub fn is_identifier_ref(&self) -> bool {
        match self {
            MemberExpression::PrimaryExpression(x) => x.is_identifier_ref(),
            MemberExpression::Expression(..)
            | MemberExpression::IdentifierName(..)
            | MemberExpression::TemplateLiteral(..)
            | MemberExpression::SuperProperty(..)
            | MemberExpression::MetaProperty(..)
            | MemberExpression::NewArguments(..)
            | MemberExpression::PrivateId(..) => false,
        }
    }

    pub fn is_named_function(&self) -> bool {
        match self {
            MemberExpression::PrimaryExpression(node) => node.is_named_function(),
            _ => false,
        }
    }
}

// SuperProperty[Yield, Await] :
//      super [ Expression[+In, ?Yield, ?Await] ]
//      super . IdentifierName
#[derive(Debug)]
pub enum SuperProperty {
    Expression(Rc<Expression>),
    IdentifierName(IdentifierData),
}

impl fmt::Display for SuperProperty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SuperProperty::Expression(boxed) => write!(f, "super [ {} ]", boxed),
            SuperProperty::IdentifierName(boxed) => write!(f, "super . {}", boxed),
        }
    }
}

impl PrettyPrint for SuperProperty {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}SuperProperty: {}", first, self)?;
        match self {
            SuperProperty::Expression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            SuperProperty::IdentifierName(_) => Ok(()),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}SuperProperty: {}", first, self)?;
        match self {
            SuperProperty::Expression(node) => {
                pprint_token(writer, "super", TokenType::Keyword, &successive, Spot::NotFinal)?;
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            SuperProperty::IdentifierName(id) => {
                pprint_token(writer, "super", TokenType::Keyword, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, id, TokenType::IdentifierName, &successive, Spot::Final)
            }
        }
    }
}

impl SuperProperty {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_super = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Super)?;
        let (punct, after_punct) = scan_for_punct_set(after_super, parser.source, ScanGoal::InputElementRegExp, &[Punctuator::Dot, Punctuator::LeftBracket])?;
        match punct {
            Punctuator::LeftBracket => {
                let (exp, after_exp) = Expression::parse(parser, after_punct, true, yield_flag, await_flag)?;
                let after_rb = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBracket)?;
                Ok((Rc::new(SuperProperty::Expression(exp)), after_rb))
            }
            _ => {
                let (id, after_id) = scan_for_identifiername(after_punct, parser.source, ScanGoal::InputElementRegExp)?;
                Ok((Rc::new(SuperProperty::IdentifierName(id)), after_id))
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            SuperProperty::Expression(n) => kind == ParseNodeKind::Super || n.contains(kind),
            SuperProperty::IdentifierName(_) => kind == ParseNodeKind::Super,
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
            SuperProperty::Expression(n) => n.all_private_identifiers_valid(names),
            SuperProperty::IdentifierName(_) => true,
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
            SuperProperty::Expression(e) => e.contains_arguments(),
            SuperProperty::IdentifierName(_) => false,
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        match self {
            SuperProperty::Expression(exp) => exp.early_errors(agent, errs, strict),
            SuperProperty::IdentifierName(_) => {}
        }
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
pub enum MetaProperty {
    NewTarget,
    ImportMeta(ParseGoal),
}

impl fmt::Display for MetaProperty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MetaProperty::NewTarget => write!(f, "new . target"),
            MetaProperty::ImportMeta(_) => write!(f, "import . meta"),
        }
    }
}

impl PrettyPrint for MetaProperty {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{}MetaProperty: {}", first, self)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}MetaProperty: {}", first, self)?;
        match self {
            MetaProperty::NewTarget => {
                pprint_token(writer, "new", TokenType::Keyword, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "target", TokenType::Keyword, &successive, Spot::Final)
            }
            MetaProperty::ImportMeta(_) => {
                pprint_token(writer, "import", TokenType::Keyword, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "meta", TokenType::Keyword, &successive, Spot::Final)
            }
        }
    }
}

impl MetaProperty {
    fn dot_token(parser: &mut Parser, scanner: Scanner, kwd: Keyword, kind: MetaProperty) -> ParseResult<Self> {
        let after_dot = scan_for_punct(scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Dot)?;
        let after_kwd = scan_for_keyword(after_dot, parser.source, ScanGoal::InputElementRegExp, kwd)?;
        Ok((Rc::new(kind), after_kwd))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (kwd, after_kwd) = scan_for_keywords(scanner, parser.source, ScanGoal::InputElementRegExp, &[Keyword::New, Keyword::Import])?;
        match kwd {
            Keyword::New => Self::dot_token(parser, after_kwd, Keyword::Target, MetaProperty::NewTarget),
            _ => Self::dot_token(parser, after_kwd, Keyword::Meta, MetaProperty::ImportMeta(parser.goal)),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            MetaProperty::NewTarget => kind == ParseNodeKind::NewTarget,
            MetaProperty::ImportMeta(_) => false,
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>) {
        match self {
            MetaProperty::NewTarget => {}
            MetaProperty::ImportMeta(goal) => {
                // ImportMeta :
                //  import . meta
                //  * It is a Syntax Error if the syntactic goal symbol is not Module.
                if *goal != ParseGoal::Module {
                    errs.push(create_syntax_error_object(agent, "import.meta allowed only in Module code"));
                }
            }
        }
    }
}

// Arguments[Yield, Await] :
//      ( )
//      ( ArgumentList[?Yield, ?Await] )
//      ( ArgumentList[?Yield, ?Await] , )
#[derive(Debug)]
pub enum Arguments {
    Empty,
    ArgumentList(Rc<ArgumentList>),
    ArgumentListComma(Rc<ArgumentList>),
}

impl fmt::Display for Arguments {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Arguments::Empty => write!(f, "( )"),
            Arguments::ArgumentList(boxed) => write!(f, "( {} )", boxed),
            Arguments::ArgumentListComma(boxed) => write!(f, "( {} , )", boxed),
        }
    }
}

impl PrettyPrint for Arguments {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Arguments: {}", first, self)?;
        match self {
            Arguments::Empty => Ok(()),
            Arguments::ArgumentList(boxed) | Arguments::ArgumentListComma(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Arguments: {}", first, self)?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match self {
            Arguments::Empty => {}
            Arguments::ArgumentList(node) => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            Arguments::ArgumentListComma(node) => {
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
        let after_lp = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftParen)?;
        scan_for_punct(after_lp, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightParen).map(|after_rp| (Rc::new(Arguments::Empty), after_rp)).otherwise(|| {
            let (args, after_args) = ArgumentList::parse(parser, after_lp, yield_flag, await_flag)?;
            let (punct, after_punct) = scan_for_punct_set(after_args, parser.source, ScanGoal::InputElementDiv, &[Punctuator::Comma, Punctuator::RightParen])?;
            match punct {
                Punctuator::RightParen => Ok((Rc::new(Arguments::ArgumentList(args)), after_punct)),
                _ => {
                    let after_rp = scan_for_punct(after_punct, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightParen)?;
                    Ok((Rc::new(Arguments::ArgumentListComma(args)), after_rp))
                }
            }
        })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
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

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            Arguments::Empty => false,
            Arguments::ArgumentList(n) => n.contains(kind),
            Arguments::ArgumentListComma(n) => n.contains(kind),
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
            Arguments::Empty => true,
            Arguments::ArgumentList(n) => n.all_private_identifiers_valid(names),
            Arguments::ArgumentListComma(n) => n.all_private_identifiers_valid(names),
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
            Arguments::Empty => false,
            Arguments::ArgumentList(al) | Arguments::ArgumentListComma(al) => al.contains_arguments(),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        match self {
            Arguments::Empty => {}
            Arguments::ArgumentList(n) | Arguments::ArgumentListComma(n) => n.early_errors(agent, errs, strict),
        }
    }
}

// ArgumentList[Yield, Await] :
//      AssignmentExpression[+In, ?Yield, ?Await]
//      ... AssignmentExpression[+In, ?Yield, ?Await]
//      ArgumentList[?Yield, ?Await] , AssignmentExpression[+In, ?Yield, ?Await]
//      ArgumentList[?Yield, ?Await] , ... AssignmentExpression[+In, ?Yield, ?Await]
#[derive(Debug)]
pub enum ArgumentList {
    FallThru(Rc<AssignmentExpression>),
    Dots(Rc<AssignmentExpression>),
    ArgumentList(Rc<ArgumentList>, Rc<AssignmentExpression>),
    ArgumentListDots(Rc<ArgumentList>, Rc<AssignmentExpression>),
}

impl ArgumentList {
    // Package the results of a successful assignment_expression into an ArgumentList::FallThru.
    fn ae_bundle(pair: (Rc<AssignmentExpression>, Scanner)) -> Result<(Self, Scanner), ParseError> {
        let (ae_boxed, scanner) = pair;
        Ok((Self::FallThru(ae_boxed), scanner))
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
    pub fn parse_assignment_expression(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Self, Scanner), ParseError> {
        AssignmentExpression::parse(parser, scanner, true, yield_flag, await_flag).and_then(Self::ae_bundle)
    }

    // Parse the production
    //      ArgumentList : ... AssignmentExpression
    // returning one of:
    //    * an ArgumentList that contains all the relevant info
    //    * an Err with a human readable message about what went wrong
    // Note: It is an error for ... to appear during an ArgumentList parse without being followed by an AssignmentExpression.
    pub fn parse_dots_assignment_expression(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Self, Scanner), ParseError> {
        let after_ellipsis = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Ellipsis)?;
        let (ae, after_ae) = AssignmentExpression::parse(parser, after_ellipsis, true, yield_flag, await_flag)?;
        Ok((Self::Dots(ae), after_ae))
    }

    // Parse the production
    //      ArgumentList : ArgumentList , AssignmentExpression
    // ASSUMING: that the first ArgumentList has already been parsed. (I.e: just do the part starting with the comma.)
    // returning one of:
    //    * a pair: (Rc<AssignmentExpression>, Scanner)
    //    * an Err with a human readable message about what went wrong
    fn parse_al_ae(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<AssignmentExpression> {
        let after_comma = scan_for_punct(scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)?;
        AssignmentExpression::parse(parser, after_comma, true, yield_flag, await_flag)
    }

    // Parse the production
    //      ArgumentList : ArgumentList , ... AssignmentExpression
    // ASSUMING: that the first ArgumentList has already been parsed. (I.e: just do the part starting with the comma.)
    // returning one of:
    //    * a pair: (Rc<AssignmentExpression>, Scanner)
    //    * an Err with a human readable message about what went wrong
    fn parse_al_dots_ae(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<AssignmentExpression> {
        let after_comma = scan_for_punct(scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)?;
        let after_ellipsis = scan_for_punct(after_comma, parser.source, ScanGoal::InputElementRegExp, Punctuator::Ellipsis)?;
        AssignmentExpression::parse(parser, after_ellipsis, true, yield_flag, await_flag)
    }
}

impl fmt::Display for ArgumentList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArgumentList::FallThru(boxed) => write!(f, "{}", boxed),
            ArgumentList::Dots(boxed) => write!(f, "... {}", boxed),
            ArgumentList::ArgumentList(list, exp) => write!(f, "{} , {}", list, exp),
            ArgumentList::ArgumentListDots(list, exp) => write!(f, "{} , ... {}", list, exp),
        }
    }
}

impl PrettyPrint for ArgumentList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ArgumentList: {}", first, self)?;
        match self {
            ArgumentList::FallThru(boxed) | ArgumentList::Dots(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            ArgumentList::ArgumentList(list, exp) | ArgumentList::ArgumentListDots(list, exp) => {
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
            writeln!(writer, "{}ArgumentList: {}", first, self).and(Ok(successive))
        };
        match self {
            ArgumentList::FallThru(node) => node.concise_with_leftpad(writer, pad, state),
            ArgumentList::Dots(node) => {
                let successive = head(pad, state)?;
                pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ArgumentList::ArgumentList(list, exp) => {
                let successive = head(pad, state)?;
                list.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                exp.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ArgumentList::ArgumentListDots(list, exp) => {
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        ArgumentList::parse_assignment_expression(parser, scanner, yield_flag, await_flag)
            .otherwise(|| ArgumentList::parse_dots_assignment_expression(parser, scanner, yield_flag, await_flag))
            .map(|(kind, after)| {
                let mut top_scanner = after;
                let mut top_box = Rc::new(kind);
                enum Dots {
                    Dots,
                    NoDots,
                }
                while let Ok((ae, scan, dotstate)) = ArgumentList::parse_al_ae(parser, top_scanner, yield_flag, await_flag)
                    .map(|(ae, after_ae)| (ae, after_ae, Dots::NoDots))
                    .otherwise(|| ArgumentList::parse_al_dots_ae(parser, top_scanner, yield_flag, await_flag).map(|(ae, after_ae)| (ae, after_ae, Dots::Dots)))
                {
                    top_box = Rc::new(match dotstate {
                        Dots::Dots => ArgumentList::ArgumentListDots(top_box, ae),
                        Dots::NoDots => ArgumentList::ArgumentList(top_box, ae),
                    });
                    top_scanner = scan;
                }
                (top_box, top_scanner)
            })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ArgumentList::FallThru(boxed) => boxed.contains(kind),
            ArgumentList::Dots(boxed) => boxed.contains(kind),
            ArgumentList::ArgumentList(list, exp) => list.contains(kind) || exp.contains(kind),
            ArgumentList::ArgumentListDots(list, exp) => list.contains(kind) || exp.contains(kind),
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
            ArgumentList::FallThru(boxed) => boxed.all_private_identifiers_valid(names),
            ArgumentList::Dots(boxed) => boxed.all_private_identifiers_valid(names),
            ArgumentList::ArgumentList(list, exp) => list.all_private_identifiers_valid(names) && exp.all_private_identifiers_valid(names),
            ArgumentList::ArgumentListDots(list, exp) => list.all_private_identifiers_valid(names) && exp.all_private_identifiers_valid(names),
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
            ArgumentList::FallThru(ae) | ArgumentList::Dots(ae) => ae.contains_arguments(),
            ArgumentList::ArgumentList(al, ae) | ArgumentList::ArgumentListDots(al, ae) => al.contains_arguments() || ae.contains_arguments(),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        match self {
            ArgumentList::FallThru(boxed) | ArgumentList::Dots(boxed) => boxed.early_errors(agent, errs, strict),
            ArgumentList::ArgumentList(list, exp) | ArgumentList::ArgumentListDots(list, exp) => {
                list.early_errors(agent, errs, strict);
                exp.early_errors(agent, errs, strict);
            }
        }
    }
}

// NewExpression[Yield, Await] :
//      MemberExpression[?Yield, ?Await]
//      new NewExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum NewExpression {
    MemberExpression(Rc<MemberExpression>),
    NewExpression(Rc<NewExpression>),
}

impl fmt::Display for NewExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NewExpression::MemberExpression(boxed) => write!(f, "{}", boxed),
            NewExpression::NewExpression(boxed) => write!(f, "new {}", boxed),
        }
    }
}

impl PrettyPrint for NewExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}NewExpression: {}", first, self)?;
        match self {
            NewExpression::MemberExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            NewExpression::NewExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            NewExpression::MemberExpression(node) => node.concise_with_leftpad(writer, pad, state),
            NewExpression::NewExpression(node) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}NewExpression: {}", first, self)?;
                pprint_token(writer, "new", TokenType::Keyword, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for NewExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            NewExpression::MemberExpression(boxed) => boxed.is_function_definition(),
            NewExpression::NewExpression(_) => false,
        }
    }
}

impl NewExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::NewOrMEExpected, scanner))
            .otherwise(|| {
                let (me, after_me) = MemberExpression::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(NewExpression::MemberExpression(me)), after_me))
            })
            .otherwise(|| {
                let after_new = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::New)?;
                let (ne, after_ne) = Self::parse(parser, after_new, yield_flag, await_flag)?;
                Ok((Rc::new(NewExpression::NewExpression(ne)), after_ne))
            })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            NewExpression::MemberExpression(boxed) => boxed.contains(kind),
            NewExpression::NewExpression(boxed) => boxed.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            NewExpression::MemberExpression(n) => n.as_string_literal(),
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
            NewExpression::MemberExpression(boxed) => boxed.all_private_identifiers_valid(names),
            NewExpression::NewExpression(boxed) => boxed.all_private_identifiers_valid(names),
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
            NewExpression::MemberExpression(me) => me.contains_arguments(),
            NewExpression::NewExpression(ne) => ne.contains_arguments(),
        }
    }

    pub fn is_object_or_array_literal(&self) -> bool {
        match self {
            NewExpression::MemberExpression(boxed) => boxed.is_object_or_array_literal(),
            NewExpression::NewExpression(_) => false,
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            NewExpression::MemberExpression(boxed) => boxed.early_errors(agent, errs, strict),
            NewExpression::NewExpression(boxed) => boxed.early_errors(agent, errs, strict),
        }
    }

    pub fn is_strictly_deletable(&self) -> bool {
        match self {
            NewExpression::NewExpression(_) => true,
            NewExpression::MemberExpression(node) => node.is_strictly_deletable(),
        }
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub fn assignment_target_type(&self, strict: bool) -> ATTKind {
        match self {
            NewExpression::MemberExpression(boxed) => boxed.assignment_target_type(strict),
            NewExpression::NewExpression(_) => ATTKind::Invalid,
        }
    }

    /// True if this production winds up being an IdentifierRef
    ///
    /// See [IsIdentifierRef](https://tc39.es/ecma262/#sec-static-semantics-isidentifierref) from ECMA-262.
    pub fn is_identifier_ref(&self) -> bool {
        match self {
            NewExpression::NewExpression(_) => false,
            NewExpression::MemberExpression(x) => x.is_identifier_ref(),
        }
    }

    pub fn is_named_function(&self) -> bool {
        match self {
            NewExpression::MemberExpression(node) => node.is_named_function(),
            _ => false,
        }
    }
}

// CallMemberExpression[Yield, Await] :
//      MemberExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
#[derive(Debug)]
pub struct CallMemberExpression {
    pub member_expression: Rc<MemberExpression>,
    pub arguments: Rc<Arguments>,
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
        writeln!(writer, "{}CallMemberExpression: {}", first, self)?;
        self.member_expression.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.arguments.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CallMemberExpression: {}", first, self)?;
        self.member_expression.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.arguments.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl CallMemberExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (me, after_me) = MemberExpression::parse(parser, scanner, yield_flag, await_flag)?;
        let (args, after_args) = Arguments::parse(parser, after_me, yield_flag, await_flag)?;
        Ok((Rc::new(CallMemberExpression { member_expression: me, arguments: args }), after_args))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.member_expression.contains(kind) || self.arguments.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.member_expression.all_private_identifiers_valid(names) && self.arguments.all_private_identifiers_valid(names)
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
        self.member_expression.contains_arguments() || self.arguments.contains_arguments()
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        self.member_expression.early_errors(agent, errs, strict);
        self.arguments.early_errors(agent, errs, strict);
    }
}

// SuperCall[Yield, Await] :
//      super Arguments[?Yield, ?Await]
#[derive(Debug)]
pub struct SuperCall {
    arguments: Rc<Arguments>,
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
        writeln!(writer, "{}SuperCall: {}", first, self)?;
        self.arguments.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}SuperCall: {}", first, self)?;
        pprint_token(writer, "super", TokenType::Keyword, &successive, Spot::NotFinal)?;
        self.arguments.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl SuperCall {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_super = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Super)?;
        let (args, after_args) = Arguments::parse(parser, after_super, yield_flag, await_flag)?;
        Ok((Rc::new(Self { arguments: args }), after_args))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        kind == ParseNodeKind::Super || self.arguments.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
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
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.arguments.contains_arguments()
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        self.arguments.early_errors(agent, errs, strict);
    }
}

// ImportCall[Yield, Await] :
//      import ( AssignmentExpression[+In, ?Yield, ?Await] )
#[derive(Debug)]
pub struct ImportCall {
    assignment_expression: Rc<AssignmentExpression>,
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
        writeln!(writer, "{}ImportCall: {}", first, self)?;
        self.assignment_expression.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ImportCall: {}", first, self)?;
        pprint_token(writer, "import", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.assignment_expression.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ImportCall {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_import = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Import)?;
        let after_lp = scan_for_punct(after_import, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftParen)?;
        let (ae, after_ae) = AssignmentExpression::parse(parser, after_lp, true, yield_flag, await_flag)?;
        let after_rp = scan_for_punct(after_ae, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightParen)?;
        Ok((Rc::new(Self { assignment_expression: ae }), after_rp))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.assignment_expression.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
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
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.assignment_expression.contains_arguments()
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        self.assignment_expression.early_errors(agent, errs, strict);
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
pub enum CallExpression {
    CallMemberExpression(Rc<CallMemberExpression>),
    SuperCall(Rc<SuperCall>),
    ImportCall(Rc<ImportCall>),
    CallExpressionArguments(Rc<CallExpression>, Rc<Arguments>),
    CallExpressionExpression(Rc<CallExpression>, Rc<Expression>),
    CallExpressionIdentifierName(Rc<CallExpression>, IdentifierData),
    CallExpressionTemplateLiteral(Rc<CallExpression>, Rc<TemplateLiteral>),
    CallExpressionPrivateId(Rc<CallExpression>, IdentifierData),
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CallExpression::CallMemberExpression(boxed) => write!(f, "{}", boxed),
            CallExpression::SuperCall(boxed) => write!(f, "{}", boxed),
            CallExpression::ImportCall(boxed) => write!(f, "{}", boxed),
            CallExpression::CallExpressionArguments(ce, args) => write!(f, "{} {}", ce, args),
            CallExpression::CallExpressionExpression(ce, exp) => write!(f, "{} [ {} ]", ce, exp),
            CallExpression::CallExpressionIdentifierName(ce, id) | CallExpression::CallExpressionPrivateId(ce, id) => write!(f, "{} . {}", ce, id),
            CallExpression::CallExpressionTemplateLiteral(ce, tl) => write!(f, "{} {}", ce, tl),
        }
    }
}

impl PrettyPrint for CallExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CallExpression: {}", first, self)?;
        match self {
            CallExpression::CallMemberExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            CallExpression::SuperCall(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            CallExpression::ImportCall(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            CallExpression::CallExpressionArguments(ce, args) => {
                ce.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                args.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CallExpression::CallExpressionExpression(ce, exp) => {
                ce.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                exp.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CallExpression::CallExpressionIdentifierName(ce, _) => ce.pprint_with_leftpad(writer, &successive, Spot::Final),
            CallExpression::CallExpressionTemplateLiteral(ce, tl) => {
                ce.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                tl.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CallExpression::CallExpressionPrivateId(ce, _) => ce.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let head = |writer: &mut T, node: &CallExpression| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}CallExpression: {}", first, self)?;
            node.concise_with_leftpad(writer, &successive, Spot::NotFinal).and(Ok(successive))
        };
        match self {
            CallExpression::CallMemberExpression(node) => node.concise_with_leftpad(writer, pad, state),
            CallExpression::SuperCall(node) => node.concise_with_leftpad(writer, pad, state),
            CallExpression::ImportCall(node) => node.concise_with_leftpad(writer, pad, state),
            CallExpression::CallExpressionArguments(ce, right) => {
                let successive = head(writer, ce)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            CallExpression::CallExpressionExpression(ce, right) => {
                let successive = head(writer, ce)?;
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            CallExpression::CallExpressionIdentifierName(ce, right) => {
                let successive = head(writer, ce)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, right, TokenType::IdentifierName, &successive, Spot::NotFinal)
            }
            CallExpression::CallExpressionTemplateLiteral(ce, right) => {
                let successive = head(writer, ce)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            CallExpression::CallExpressionPrivateId(ce, id) => {
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
            .otherwise(|| CallMemberExpression::parse(parser, scanner, yield_arg, await_arg).map(|(cme, after_cme)| (Rc::new(CallExpression::CallMemberExpression(cme)), after_cme)))
            .otherwise(|| SuperCall::parse(parser, scanner, yield_arg, await_arg).map(|(sc, after_sc)| (Rc::new(CallExpression::SuperCall(sc)), after_sc)))
            .otherwise(|| ImportCall::parse(parser, scanner, yield_arg, await_arg).map(|(ic, after_ic)| (Rc::new(CallExpression::ImportCall(ic)), after_ic)))
            .map(|(ce, after_ce)| {
                enum Follow {
                    Args(Rc<Arguments>),
                    Exp(Rc<Expression>),
                    Id(IdentifierData),
                    TLit(Rc<TemplateLiteral>),
                    Pid(IdentifierData),
                }
                let mut top_box = ce;
                let mut top_scanner = after_ce;
                while let Ok((follow, scan)) = Arguments::parse(parser, top_scanner, yield_arg, await_arg)
                    .map(|(args, after_args)| (Follow::Args(args), after_args))
                    .otherwise(|| TemplateLiteral::parse(parser, top_scanner, yield_arg, await_arg, true).map(|(tl, after_tl)| (Follow::TLit(tl), after_tl)))
                    .otherwise(|| {
                        let (punct, after_punct) = scan_for_punct_set(top_scanner, parser.source, ScanGoal::InputElementDiv, &[Punctuator::Dot, Punctuator::LeftBracket])?;
                        match punct {
                            Punctuator::Dot => scan_for_identifiername(after_punct, parser.source, ScanGoal::InputElementDiv)
                                .map(|(id, after_id)| (Follow::Id(id), after_id))
                                .otherwise(|| scan_for_private_identifier(after_punct, parser.source, ScanGoal::InputElementDiv).map(|(pid, after_pid)| (Follow::Pid(pid), after_pid))),
                            _ => {
                                let (exp, after_exp) = Expression::parse(parser, after_punct, true, yield_arg, await_arg)?;
                                let after_rb = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBracket)?;
                                Ok((Follow::Exp(exp), after_rb))
                            }
                        }
                    })
                {
                    top_box = Rc::new(match follow {
                        Follow::Exp(exp) => CallExpression::CallExpressionExpression(top_box, exp),
                        Follow::Id(id) => CallExpression::CallExpressionIdentifierName(top_box, id),
                        Follow::TLit(tl) => CallExpression::CallExpressionTemplateLiteral(top_box, tl),
                        Follow::Args(args) => CallExpression::CallExpressionArguments(top_box, args),
                        Follow::Pid(id) => CallExpression::CallExpressionPrivateId(top_box, id),
                    });
                    top_scanner = scan;
                }
                (top_box, top_scanner)
            })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
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

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            CallExpression::CallMemberExpression(boxed) => boxed.contains(kind),
            CallExpression::SuperCall(boxed) => kind == ParseNodeKind::SuperCall || boxed.contains(kind),
            CallExpression::ImportCall(boxed) => boxed.contains(kind),
            CallExpression::CallExpressionArguments(ce, args) => ce.contains(kind) || args.contains(kind),
            CallExpression::CallExpressionExpression(ce, exp) => ce.contains(kind) || exp.contains(kind),
            CallExpression::CallExpressionIdentifierName(ce, _) | CallExpression::CallExpressionPrivateId(ce, _) => ce.contains(kind),
            CallExpression::CallExpressionTemplateLiteral(ce, tl) => ce.contains(kind) || tl.contains(kind),
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
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
            CallExpression::CallExpressionArguments(ce, args) => ce.all_private_identifiers_valid(names) && args.all_private_identifiers_valid(names),
            CallExpression::CallExpressionExpression(ce, exp) => ce.all_private_identifiers_valid(names) && exp.all_private_identifiers_valid(names),
            CallExpression::CallExpressionIdentifierName(ce, _) => ce.all_private_identifiers_valid(names),
            CallExpression::CallExpressionTemplateLiteral(ce, tl) => ce.all_private_identifiers_valid(names) && tl.all_private_identifiers_valid(names),

            // CallExpression : CallExpression . PrivateIdentifier
            //  1. If names contains the StringValue of PrivateIdentifier, then
            //      a. Return AllPrivateIdentifiersValid of CallExpression with argument names.
            //  2. Return false.
            CallExpression::CallExpressionPrivateId(ce, id) => names.contains(&id.string_value) && ce.all_private_identifiers_valid(names),
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
            CallExpression::CallMemberExpression(cme) => cme.contains_arguments(),
            CallExpression::SuperCall(sc) => sc.contains_arguments(),
            CallExpression::ImportCall(ic) => ic.contains_arguments(),
            CallExpression::CallExpressionArguments(ce, a) => ce.contains_arguments() || a.contains_arguments(),
            CallExpression::CallExpressionExpression(ce, e) => ce.contains_arguments() || e.contains_arguments(),
            CallExpression::CallExpressionIdentifierName(ce, _) | CallExpression::CallExpressionPrivateId(ce, _) => ce.contains_arguments(),
            CallExpression::CallExpressionTemplateLiteral(ce, tl) => ce.contains_arguments() | tl.contains_arguments(),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            CallExpression::CallMemberExpression(node) => node.early_errors(agent, errs, strict),
            CallExpression::SuperCall(node) => node.early_errors(agent, errs, strict),
            CallExpression::ImportCall(node) => node.early_errors(agent, errs, strict),
            CallExpression::CallExpressionArguments(node, args) => {
                node.early_errors(agent, errs, strict);
                args.early_errors(agent, errs, strict);
            }
            CallExpression::CallExpressionExpression(node, exp) => {
                node.early_errors(agent, errs, strict);
                exp.early_errors(agent, errs, strict);
            }
            CallExpression::CallExpressionIdentifierName(node, _) => node.early_errors(agent, errs, strict),
            CallExpression::CallExpressionTemplateLiteral(node, tl) => {
                node.early_errors(agent, errs, strict);
                tl.early_errors(agent, errs, strict, 0xffff_ffff);
            }
            CallExpression::CallExpressionPrivateId(node, _) => node.early_errors(agent, errs, strict),
        }
    }

    pub fn is_strictly_deletable(&self) -> bool {
        !matches!(self, CallExpression::CallExpressionPrivateId(..))
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub fn assignment_target_type(&self) -> ATTKind {
        match self {
            CallExpression::CallMemberExpression(_)
            | CallExpression::SuperCall(_)
            | CallExpression::ImportCall(_)
            | CallExpression::CallExpressionArguments(..)
            | CallExpression::CallExpressionTemplateLiteral(..) => ATTKind::Invalid,
            CallExpression::CallExpressionExpression(..) | CallExpression::CallExpressionIdentifierName(..) | CallExpression::CallExpressionPrivateId(..) => ATTKind::Simple,
        }
    }
}

// LeftHandSideExpression[Yield, Await] :
//      NewExpression[?Yield, ?Await]
//      CallExpression[?Yield, ?Await]
//      OptionalExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum LeftHandSideExpression {
    New(Rc<NewExpression>),
    Call(Rc<CallExpression>),
    Optional(Rc<OptionalExpression>),
}

impl fmt::Display for LeftHandSideExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LeftHandSideExpression::New(boxed) => write!(f, "{}", boxed),
            LeftHandSideExpression::Call(boxed) => write!(f, "{}", boxed),
            LeftHandSideExpression::Optional(boxed) => write!(f, "{}", boxed),
        }
    }
}

impl PrettyPrint for LeftHandSideExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}LeftHandSideExpression: {}", first, self)?;
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

impl IsFunctionDefinition for LeftHandSideExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            LeftHandSideExpression::New(boxed) => boxed.is_function_definition(),
            LeftHandSideExpression::Optional(_) | LeftHandSideExpression::Call(_) => false,
        }
    }
}

impl LeftHandSideExpression {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_arg: bool, await_arg: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::LeftHandSideExpression), scanner))
            .otherwise(|| OptionalExpression::parse(parser, scanner, yield_arg, await_arg).map(|(opt, after_opt)| (Rc::new(Self::Optional(opt)), after_opt)))
            .otherwise(|| CallExpression::parse(parser, scanner, yield_arg, await_arg).map(|(ce, after_ce)| (Rc::new(Self::Call(ce)), after_ce)))
            .otherwise(|| NewExpression::parse(parser, scanner, yield_arg, await_arg).map(|(ne, after_ne)| (Rc::new(Self::New(ne)), after_ne)))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
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

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            LeftHandSideExpression::New(boxed) => boxed.contains(kind),
            LeftHandSideExpression::Call(boxed) => boxed.contains(kind),
            LeftHandSideExpression::Optional(boxed) => boxed.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            LeftHandSideExpression::New(n) => n.as_string_literal(),
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
            LeftHandSideExpression::New(boxed) => boxed.all_private_identifiers_valid(names),
            LeftHandSideExpression::Call(boxed) => boxed.all_private_identifiers_valid(names),
            LeftHandSideExpression::Optional(boxed) => boxed.all_private_identifiers_valid(names),
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
            LeftHandSideExpression::New(ne) => ne.contains_arguments(),
            LeftHandSideExpression::Call(ce) => ce.contains_arguments(),
            LeftHandSideExpression::Optional(oe) => oe.contains_arguments(),
        }
    }

    pub fn is_object_or_array_literal(&self) -> bool {
        match self {
            LeftHandSideExpression::New(boxed) => boxed.is_object_or_array_literal(),
            LeftHandSideExpression::Call(_) | LeftHandSideExpression::Optional(_) => false,
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            LeftHandSideExpression::New(boxed) => boxed.early_errors(agent, errs, strict),
            LeftHandSideExpression::Call(boxed) => boxed.early_errors(agent, errs, strict),
            LeftHandSideExpression::Optional(boxed) => boxed.early_errors(agent, errs, strict),
        }
    }

    pub fn is_strictly_deletable(&self) -> bool {
        match self {
            LeftHandSideExpression::New(node) => node.is_strictly_deletable(),
            LeftHandSideExpression::Call(node) => node.is_strictly_deletable(),
            LeftHandSideExpression::Optional(node) => node.is_strictly_deletable(),
        }
    }

    /// Whether an expression can be assigned to. `Simple` or `Invalid`.
    ///
    /// See [AssignmentTargetType](https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype) from ECMA-262.
    pub fn assignment_target_type(&self, strict: bool) -> ATTKind {
        match self {
            LeftHandSideExpression::New(boxed) => boxed.assignment_target_type(strict),
            LeftHandSideExpression::Call(boxed) => boxed.assignment_target_type(),
            LeftHandSideExpression::Optional(_) => ATTKind::Invalid,
        }
    }

    /// True if this production winds up being an IdentifierRef
    ///
    /// See [IsIdentifierRef](https://tc39.es/ecma262/#sec-static-semantics-isidentifierref) from ECMA-262.
    pub fn is_identifier_ref(&self) -> bool {
        match self {
            LeftHandSideExpression::Call(_) | LeftHandSideExpression::Optional(_) => false,
            LeftHandSideExpression::New(x) => x.is_identifier_ref(),
        }
    }

    pub fn is_named_function(&self) -> bool {
        match self {
            LeftHandSideExpression::New(node) => node.is_named_function(),
            _ => false,
        }
    }
}

// OptionalExpression[Yield, Await] :
//      MemberExpression[?Yield, ?Await] OptionalChain[?Yield, ?Await]
//      CallExpression[?Yield, ?Await] OptionalChain[?Yield, ?Await]
//      OptionalExpression[?Yield, ?Await] OptionalChain[?Yield, ?Await]
#[derive(Debug)]
pub enum OptionalExpression {
    Member(Rc<MemberExpression>, Rc<OptionalChain>),
    Call(Rc<CallExpression>, Rc<OptionalChain>),
    Opt(Rc<OptionalExpression>, Rc<OptionalChain>),
}

impl fmt::Display for OptionalExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OptionalExpression::Member(left, right) => write!(f, "{} {}", left, right),
            OptionalExpression::Call(left, right) => write!(f, "{} {}", left, right),
            OptionalExpression::Opt(left, right) => write!(f, "{} {}", left, right),
        }
    }
}

impl PrettyPrint for OptionalExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}OptionalExpression: {}", first, self)?;
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
        writeln!(writer, "{}OptionalExpression: {}", first, self)?;
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
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

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            OptionalExpression::Member(left, right) => left.contains(kind) || right.contains(kind),
            OptionalExpression::Call(left, right) => left.contains(kind) || right.contains(kind),
            OptionalExpression::Opt(left, right) => left.contains(kind) || right.contains(kind),
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
            OptionalExpression::Member(left, right) => left.all_private_identifiers_valid(names) && right.all_private_identifiers_valid(names),
            OptionalExpression::Call(left, right) => left.all_private_identifiers_valid(names) && right.all_private_identifiers_valid(names),
            OptionalExpression::Opt(left, right) => left.all_private_identifiers_valid(names) && right.all_private_identifiers_valid(names),
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
            OptionalExpression::Member(me, oc) => me.contains_arguments() || oc.contains_arguments(),
            OptionalExpression::Call(ce, oc) => ce.contains_arguments() || oc.contains_arguments(),
            OptionalExpression::Opt(oe, oc) => oe.contains_arguments() || oc.contains_arguments(),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            OptionalExpression::Member(left, right) => {
                left.early_errors(agent, errs, strict);
                right.early_errors(agent, errs, strict);
            }
            OptionalExpression::Call(left, right) => {
                left.early_errors(agent, errs, strict);
                right.early_errors(agent, errs, strict);
            }
            OptionalExpression::Opt(left, right) => {
                left.early_errors(agent, errs, strict);
                right.early_errors(agent, errs, strict);
            }
        }
    }

    pub fn is_strictly_deletable(&self) -> bool {
        match self {
            OptionalExpression::Member(_, chain) | OptionalExpression::Call(_, chain) | OptionalExpression::Opt(_, chain) => chain.is_strictly_deletable(),
        }
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
pub enum OptionalChain {
    Args(Rc<Arguments>),
    Exp(Rc<Expression>),
    Ident(IdentifierData),
    Template(Rc<TemplateLiteral>),
    PrivateId(IdentifierData),
    PlusArgs(Rc<OptionalChain>, Rc<Arguments>),
    PlusExp(Rc<OptionalChain>, Rc<Expression>),
    PlusIdent(Rc<OptionalChain>, IdentifierData),
    PlusTemplate(Rc<OptionalChain>, Rc<TemplateLiteral>),
    PlusPrivateId(Rc<OptionalChain>, IdentifierData),
}

impl fmt::Display for OptionalChain {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OptionalChain::Args(node) => write!(f, "?. {}", node),
            OptionalChain::Exp(node) => write!(f, "?. [ {} ]", node),
            OptionalChain::Ident(node) | OptionalChain::PrivateId(node) => write!(f, "?. {}", node),
            OptionalChain::Template(node) => write!(f, "?. {}", node),
            OptionalChain::PlusArgs(lst, item) => write!(f, "{} {}", lst, item),
            OptionalChain::PlusExp(lst, item) => write!(f, "{} [ {} ]", lst, item),
            OptionalChain::PlusIdent(lst, item) | OptionalChain::PlusPrivateId(lst, item) => write!(f, "{} . {}", lst, item),
            OptionalChain::PlusTemplate(lst, item) => write!(f, "{} {}", lst, item),
        }
    }
}

impl PrettyPrint for OptionalChain {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}OptionalChain: {}", first, self)?;
        match self {
            OptionalChain::Args(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            OptionalChain::Exp(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            OptionalChain::Ident(node) => pprint_token(writer, node, TokenType::IdentifierName, &successive, Spot::Final),
            OptionalChain::Template(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            OptionalChain::PrivateId(node) => pprint_token(writer, node, TokenType::PrivateIdentifier, &successive, Spot::Final),
            OptionalChain::PlusArgs(lst, item) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalChain::PlusExp(lst, item) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalChain::PlusIdent(lst, item) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, item, TokenType::IdentifierName, &successive, Spot::Final)
            }
            OptionalChain::PlusTemplate(lst, item) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalChain::PlusPrivateId(lst, item) => {
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
        writeln!(writer, "{}OptionalChain: {}", first, self)?;
        match self {
            OptionalChain::Args(node) => {
                pprint_token(writer, "?.", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalChain::Exp(node) => {
                pprint_token(writer, "?.", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            OptionalChain::Ident(node) => {
                pprint_token(writer, "?.", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, node, TokenType::IdentifierName, &successive, Spot::Final)
            }
            OptionalChain::Template(node) => {
                pprint_token(writer, "?.", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalChain::PrivateId(node) => {
                pprint_token(writer, "?.", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, node, TokenType::PrivateIdentifier, &successive, Spot::Final)
            }
            OptionalChain::PlusArgs(lst, item) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalChain::PlusExp(lst, item) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            OptionalChain::PlusIdent(lst, item) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, item, TokenType::IdentifierName, &successive, Spot::Final)
            }
            OptionalChain::PlusTemplate(lst, item) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalChain::PlusPrivateId(lst, item) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, item, TokenType::PrivateIdentifier, &successive, Spot::Final)
            }
        }
    }
}

impl OptionalChain {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_opt = scan_for_punct(scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::QDot)?;
        let (mut current, mut current_scan) = Err(ParseError::new(PECode::ChainFailed, after_opt))
            .otherwise(|| {
                let (args, after_args) = Arguments::parse(parser, after_opt, yield_flag, await_flag)?;
                Ok((Rc::new(OptionalChain::Args(args)), after_args))
            })
            .otherwise(|| {
                let (tl, after_tl) = TemplateLiteral::parse(parser, after_opt, yield_flag, await_flag, true)?;
                Ok((Rc::new(OptionalChain::Template(tl)), after_tl))
            })
            .otherwise(|| {
                let after_lb = scan_for_punct(after_opt, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBracket)?;
                let (exp, after_exp) = Expression::parse(parser, after_lb, true, yield_flag, await_flag)?;
                let after_rb = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBracket)?;
                Ok((Rc::new(OptionalChain::Exp(exp)), after_rb))
            })
            .otherwise(|| {
                let (id, after_id) = scan_for_identifiername(after_opt, parser.source, ScanGoal::InputElementDiv)?;
                Ok((Rc::new(OptionalChain::Ident(id)), after_id))
            })
            .otherwise(|| scan_for_private_identifier(after_opt, parser.source, ScanGoal::InputElementDiv).map(|(id, after_id)| (Rc::new(OptionalChain::PrivateId(id)), after_id)))?;

        enum Follow {
            Args(Rc<Arguments>),
            TLit(Rc<TemplateLiteral>),
            Exp(Rc<Expression>),
            Id(IdentifierData),
            Pid(IdentifierData),
        }
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
                let (punct, after_punct) = scan_for_punct_set(current_scan, parser.source, ScanGoal::InputElementDiv, &[Punctuator::Dot, Punctuator::LeftBracket])?;
                match punct {
                    Punctuator::Dot => scan_for_identifiername(after_punct, parser.source, ScanGoal::InputElementDiv)
                        .map(|(id, after_id)| (Follow::Id(id), after_id))
                        .otherwise(|| scan_for_private_identifier(after_punct, parser.source, ScanGoal::InputElementDiv).map(|(id, after_id)| (Follow::Pid(id), after_id))),
                    _ => {
                        let (exp, after_exp) = Expression::parse(parser, after_punct, true, yield_flag, await_flag)?;
                        let after_rb = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBracket)?;
                        Ok((Follow::Exp(exp), after_rb))
                    }
                }
            })
        {
            current = Rc::new(match follow {
                Follow::Args(args) => OptionalChain::PlusArgs(current, args),
                Follow::Id(id) => OptionalChain::PlusIdent(current, id),
                Follow::Exp(exp) => OptionalChain::PlusExp(current, exp),
                Follow::TLit(tl) => OptionalChain::PlusTemplate(current, tl),
                Follow::Pid(id) => OptionalChain::PlusPrivateId(current, id),
            });
            current_scan = scan;
        }
        Ok((current, current_scan))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            OptionalChain::Args(node) => node.contains(kind),
            OptionalChain::Exp(node) => node.contains(kind),
            OptionalChain::Ident(_) => false,
            OptionalChain::Template(node) => node.contains(kind),
            OptionalChain::PrivateId(_) => false,
            OptionalChain::PlusArgs(lst, item) => lst.contains(kind) || item.contains(kind),
            OptionalChain::PlusExp(lst, item) => lst.contains(kind) || item.contains(kind),
            OptionalChain::PlusIdent(lst, _) => lst.contains(kind),
            OptionalChain::PlusTemplate(lst, item) => lst.contains(kind) || item.contains(kind),
            OptionalChain::PlusPrivateId(lst, _) => lst.contains(kind),
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        match self {
            //  1. For each child node child of this Parse Node, do
            //      a. If child is an instance of a nonterminal, then
            //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
            //  2. Return true.
            OptionalChain::Args(node) => node.all_private_identifiers_valid(names),
            OptionalChain::Exp(node) => node.all_private_identifiers_valid(names),
            OptionalChain::Ident(_) => true,
            OptionalChain::Template(node) => node.all_private_identifiers_valid(names),
            OptionalChain::PlusArgs(lst, item) => lst.all_private_identifiers_valid(names) && item.all_private_identifiers_valid(names),
            OptionalChain::PlusExp(lst, item) => lst.all_private_identifiers_valid(names) && item.all_private_identifiers_valid(names),
            OptionalChain::PlusIdent(lst, _) => lst.all_private_identifiers_valid(names),
            OptionalChain::PlusTemplate(lst, item) => lst.all_private_identifiers_valid(names) && item.all_private_identifiers_valid(names),

            // OptionalChain : ?. PrivateIdentifier
            //  1. If names contains the StringValue of PrivateIdentifier, return true.
            //  2. Return false.
            OptionalChain::PrivateId(pid) => names.contains(&pid.string_value),
            // OptionalChain : OptionalChain . PrivateIdentifier
            //  1. If names contains the StringValue of PrivateIdentifier, then
            //      a. Return AllPrivateIdentifiersValid of OptionalChain with argument names.
            //  2. Return false.
            OptionalChain::PlusPrivateId(lst, pid) => names.contains(&pid.string_value) && lst.all_private_identifiers_valid(names),
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
            OptionalChain::Args(a) => a.contains_arguments(),
            OptionalChain::Exp(e) => e.contains_arguments(),
            OptionalChain::Ident(_) | OptionalChain::PrivateId(_) => false,
            OptionalChain::Template(tl) => tl.contains_arguments(),
            OptionalChain::PlusArgs(oc, a) => oc.contains_arguments() || a.contains_arguments(),
            OptionalChain::PlusExp(oc, e) => oc.contains_arguments() || e.contains_arguments(),
            OptionalChain::PlusIdent(oc, _) | OptionalChain::PlusPrivateId(oc, _) => oc.contains_arguments(),
            OptionalChain::PlusTemplate(oc, tl) => oc.contains_arguments() || tl.contains_arguments(),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
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
            OptionalChain::Template(tl) => {
                errs.push(create_syntax_error_object(agent, "Template literal not allowed here"));
                tl.early_errors(agent, errs, strict, 0xffff_ffff);
            }
            OptionalChain::PlusTemplate(node, tl) => {
                node.early_errors(agent, errs, strict);
                errs.push(create_syntax_error_object(agent, "Template literal not allowed here"));
                tl.early_errors(agent, errs, strict, 0xffff_ffff);
            }
            OptionalChain::Args(node) => node.early_errors(agent, errs, strict),
            OptionalChain::Exp(node) => node.early_errors(agent, errs, strict),
            OptionalChain::Ident(_) | OptionalChain::PrivateId(_) => {}
            OptionalChain::PlusArgs(node, args) => {
                node.early_errors(agent, errs, strict);
                args.early_errors(agent, errs, strict);
            }
            OptionalChain::PlusExp(node, exp) => {
                node.early_errors(agent, errs, strict);
                exp.early_errors(agent, errs, strict);
            }
            OptionalChain::PlusIdent(node, _) | OptionalChain::PlusPrivateId(node, _) => node.early_errors(agent, errs, strict),
        }
    }

    pub fn is_strictly_deletable(&self) -> bool {
        !matches!(self, OptionalChain::PrivateId(..) | OptionalChain::PlusPrivateId(..))
    }
}

#[cfg(test)]
mod tests;
