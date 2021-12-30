use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::assignment_operators::AssignmentExpression;
use super::comma_operator::Expression;
use super::primary_expressions::PrimaryExpression;
use super::primary_expressions::TemplateLiteral;
use super::scanner::{IdentifierData, Keyword, Punctuator, ScanGoal, Scanner, StringToken};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

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
pub enum MemberExpressionKind {
    PrimaryExpression(Rc<PrimaryExpression>),
    Expression(Rc<MemberExpression>, Rc<Expression>),
    IdentifierName(Rc<MemberExpression>, IdentifierData),
    TemplateLiteral(Rc<MemberExpression>, Rc<TemplateLiteral>),
    SuperProperty(Rc<SuperProperty>),
    MetaProperty(Rc<MetaProperty>),
    NewArguments(Rc<MemberExpression>, Rc<Arguments>),
    PrivateId(Rc<MemberExpression>, IdentifierData),
}

#[derive(Debug)]
pub struct MemberExpression {
    kind: MemberExpressionKind,
}

impl fmt::Display for MemberExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            MemberExpressionKind::PrimaryExpression(boxed) => write!(f, "{}", boxed),
            MemberExpressionKind::Expression(me, exp) => {
                write!(f, "{} [ {} ]", me, exp)
            }
            MemberExpressionKind::IdentifierName(me, id) => {
                write!(f, "{} . {}", me, id)
            }
            MemberExpressionKind::TemplateLiteral(me, tl) => write!(f, "{} {}", me, tl),
            MemberExpressionKind::SuperProperty(boxed) => write!(f, "{}", boxed),
            MemberExpressionKind::MetaProperty(boxed) => write!(f, "{}", boxed),
            MemberExpressionKind::NewArguments(me, args) => write!(f, "new {} {}", me, args),
            MemberExpressionKind::PrivateId(me, id) => write!(f, "{} . #{}", me, id),
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
        match &self.kind {
            MemberExpressionKind::PrimaryExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            MemberExpressionKind::Expression(me, exp) => {
                me.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                exp.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            MemberExpressionKind::IdentifierName(me, _) | MemberExpressionKind::PrivateId(me, _) => me.pprint_with_leftpad(writer, &successive, Spot::Final),
            MemberExpressionKind::TemplateLiteral(me, tl) => {
                me.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                tl.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            MemberExpressionKind::SuperProperty(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            MemberExpressionKind::MetaProperty(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            MemberExpressionKind::NewArguments(me, args) => {
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
        match &self.kind {
            MemberExpressionKind::PrimaryExpression(node) => node.concise_with_leftpad(writer, pad, state),
            MemberExpressionKind::SuperProperty(node) => node.concise_with_leftpad(writer, pad, state),
            MemberExpressionKind::MetaProperty(node) => node.concise_with_leftpad(writer, pad, state),
            MemberExpressionKind::Expression(me, exp) => {
                let successive = head(pad, state)?;
                me.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                exp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            MemberExpressionKind::IdentifierName(me, id) => {
                let successive = head(pad, state)?;
                me.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, id, TokenType::IdentifierName, &successive, Spot::Final)
            }
            MemberExpressionKind::PrivateId(me, id) => {
                let successive = head(pad, state)?;
                me.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, format!("#{}", id), TokenType::PrivateIdentifier, &successive, Spot::Final)
            }
            MemberExpressionKind::TemplateLiteral(me, tl) => {
                let successive = head(pad, state)?;
                me.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                tl.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            MemberExpressionKind::NewArguments(me, args) => {
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
        match &self.kind {
            MemberExpressionKind::PrimaryExpression(boxed) => boxed.is_function_definition(),
            MemberExpressionKind::Expression(..)
            | MemberExpressionKind::IdentifierName(..)
            | MemberExpressionKind::TemplateLiteral(..)
            | MemberExpressionKind::SuperProperty(_)
            | MemberExpressionKind::MetaProperty(_)
            | MemberExpressionKind::NewArguments(..)
            | MemberExpressionKind::PrivateId(..) => false,
        }
    }
}

impl AssignmentTargetType for MemberExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match &self.kind {
            MemberExpressionKind::PrimaryExpression(boxed) => boxed.assignment_target_type(),
            MemberExpressionKind::Expression(..) => ATTKind::Simple,
            MemberExpressionKind::IdentifierName(..) | MemberExpressionKind::PrivateId(..) => ATTKind::Simple,
            MemberExpressionKind::TemplateLiteral(..) => ATTKind::Invalid,
            MemberExpressionKind::SuperProperty(..) => ATTKind::Simple,
            MemberExpressionKind::MetaProperty(boxed) => boxed.assignment_target_type(),
            MemberExpressionKind::NewArguments(..) => ATTKind::Invalid,
        }
    }
}

pub trait ToMemberExpressionKind {
    fn to_member_expression_kind(node: Rc<Self>) -> MemberExpressionKind;
}

impl ToMemberExpressionKind for PrimaryExpression {
    fn to_member_expression_kind(node: Rc<Self>) -> MemberExpressionKind {
        MemberExpressionKind::PrimaryExpression(node)
    }
}

impl ToMemberExpressionKind for SuperProperty {
    fn to_member_expression_kind(node: Rc<Self>) -> MemberExpressionKind {
        MemberExpressionKind::SuperProperty(node)
    }
}

impl ToMemberExpressionKind for MetaProperty {
    fn to_member_expression_kind(node: Rc<Self>) -> MemberExpressionKind {
        MemberExpressionKind::MetaProperty(node)
    }
}

fn me_boxer<T>(pair: (Rc<T>, Scanner)) -> ParseResult<MemberExpression>
where
    T: ToMemberExpressionKind,
{
    let (node, scanner) = pair;
    Ok((Rc::new(MemberExpression { kind: T::to_member_expression_kind(node) }), scanner))
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
            After::TLit(tl) => Rc::new(MemberExpression { kind: MemberExpressionKind::TemplateLiteral(current_me, tl) }),
            After::Exp(exp) => Rc::new(MemberExpression { kind: MemberExpressionKind::Expression(current_me, exp) }),
            After::Id(id) => Rc::new(MemberExpression { kind: MemberExpressionKind::IdentifierName(current_me, id) }),
            After::Pid(id) => Rc::new(MemberExpression { kind: MemberExpressionKind::PrivateId(current_me, id) }),
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
            .otherwise(|| {
                Self::new_memberexpression_arguments(parser, scanner, yield_flag, await_flag)
                    .map(|(me, args, after)| (Rc::new(MemberExpression { kind: MemberExpressionKind::NewArguments(me, args) }), after))
            })
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
        match &self.kind {
            MemberExpressionKind::PrimaryExpression(n) => n.contains(kind),
            MemberExpressionKind::Expression(l, r) => l.contains(kind) || r.contains(kind),
            MemberExpressionKind::IdentifierName(n, _) | MemberExpressionKind::PrivateId(n, _) => n.contains(kind),
            MemberExpressionKind::TemplateLiteral(l, r) => l.contains(kind) || r.contains(kind),
            MemberExpressionKind::SuperProperty(n) => kind == ParseNodeKind::SuperProperty || n.contains(kind),
            MemberExpressionKind::MetaProperty(n) => n.contains(kind),
            MemberExpressionKind::NewArguments(l, r) => l.contains(kind) || r.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match &self.kind {
            MemberExpressionKind::PrimaryExpression(n) => n.as_string_literal(),
            _ => None,
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        match &self.kind {
            //  1. For each child node child of this Parse Node, do
            //      a. If child is an instance of a nonterminal, then
            //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
            //  2. Return true.
            MemberExpressionKind::PrimaryExpression(n) => n.all_private_identifiers_valid(names),
            MemberExpressionKind::Expression(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),
            MemberExpressionKind::IdentifierName(n, _) => n.all_private_identifiers_valid(names),
            MemberExpressionKind::TemplateLiteral(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),
            MemberExpressionKind::SuperProperty(n) => n.all_private_identifiers_valid(names),
            MemberExpressionKind::MetaProperty(_) => true,
            MemberExpressionKind::NewArguments(l, r) => l.all_private_identifiers_valid(names) && r.all_private_identifiers_valid(names),

            // MemberExpression : MemberExpression . PrivateIdentifier
            //  1. If names contains the StringValue of PrivateIdentifier, then
            //      a. Return AllPrivateIdentifiersValid of MemberExpression with argument names.
            //  2. Return false.
            MemberExpressionKind::PrivateId(n, id) => names.contains(&id.string_value) && n.all_private_identifiers_valid(names),
        }
    }

    pub fn is_object_or_array_literal(&self) -> bool {
        match &self.kind {
            MemberExpressionKind::PrimaryExpression(n) => n.is_object_or_array_literal(),
            MemberExpressionKind::Expression(..)
            | MemberExpressionKind::IdentifierName(..)
            | MemberExpressionKind::TemplateLiteral(..)
            | MemberExpressionKind::SuperProperty(..)
            | MemberExpressionKind::MetaProperty(..)
            | MemberExpressionKind::NewArguments(..)
            | MemberExpressionKind::PrivateId(..) => false,
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// SuperProperty[Yield, Await] :
//      super [ Expression[+In, ?Yield, ?Await] ]
//      super . IdentifierName
#[derive(Debug)]
pub enum SuperPropertyKind {
    Expression(Rc<Expression>),
    IdentifierName(IdentifierData),
}
#[derive(Debug)]
pub struct SuperProperty {
    kind: SuperPropertyKind,
}

impl fmt::Display for SuperProperty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            SuperPropertyKind::Expression(boxed) => write!(f, "super [ {} ]", boxed),
            SuperPropertyKind::IdentifierName(boxed) => write!(f, "super . {}", boxed),
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
        match &self.kind {
            SuperPropertyKind::Expression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            SuperPropertyKind::IdentifierName(_) => Ok(()),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}SuperProperty: {}", first, self)?;
        match &self.kind {
            SuperPropertyKind::Expression(node) => {
                pprint_token(writer, "super", TokenType::Keyword, &successive, Spot::NotFinal)?;
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            SuperPropertyKind::IdentifierName(id) => {
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
                Ok((Rc::new(SuperProperty { kind: SuperPropertyKind::Expression(exp) }), after_rb))
            }
            _ => {
                let (id, after_id) = scan_for_identifiername(after_punct, parser.source, ScanGoal::InputElementRegExp)?;
                Ok((Rc::new(SuperProperty { kind: SuperPropertyKind::IdentifierName(id) }), after_id))
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match &self.kind {
            SuperPropertyKind::Expression(n) => kind == ParseNodeKind::Super || n.contains(kind),
            SuperPropertyKind::IdentifierName(_) => kind == ParseNodeKind::Super,
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        match &self.kind {
            SuperPropertyKind::Expression(n) => n.all_private_identifiers_valid(names),
            SuperPropertyKind::IdentifierName(_) => true,
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
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
pub enum MetaPropertyKind {
    NewTarget,
    ImportMeta,
}
#[derive(Debug)]
pub struct MetaProperty {
    kind: MetaPropertyKind,
}

impl fmt::Display for MetaProperty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            MetaPropertyKind::NewTarget => write!(f, "new . target"),
            MetaPropertyKind::ImportMeta => write!(f, "import . meta"),
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
        match &self.kind {
            MetaPropertyKind::NewTarget => {
                pprint_token(writer, "new", TokenType::Keyword, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "target", TokenType::Keyword, &successive, Spot::Final)
            }
            MetaPropertyKind::ImportMeta => {
                pprint_token(writer, "import", TokenType::Keyword, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, "meta", TokenType::Keyword, &successive, Spot::Final)
            }
        }
    }
}

impl AssignmentTargetType for MetaProperty {
    fn assignment_target_type(&self) -> ATTKind {
        ATTKind::Invalid
    }
}

impl MetaProperty {
    fn dot_token(parser: &mut Parser, scanner: Scanner, kwd: Keyword, kind: MetaPropertyKind) -> ParseResult<Self> {
        let after_dot = scan_for_punct(scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Dot)?;
        let after_kwd = scan_for_keyword(after_dot, parser.source, ScanGoal::InputElementRegExp, kwd)?;
        Ok((Rc::new(MetaProperty { kind }), after_kwd))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (kwd, after_kwd) = scan_for_keywords(scanner, parser.source, ScanGoal::InputElementRegExp, &[Keyword::New, Keyword::Import])?;
        match kwd {
            Keyword::New => Self::dot_token(parser, after_kwd, Keyword::Target, MetaPropertyKind::NewTarget),
            _ => Self::dot_token(parser, after_kwd, Keyword::Meta, MetaPropertyKind::ImportMeta),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match &self.kind {
            MetaPropertyKind::NewTarget => kind == ParseNodeKind::NewTarget,
            MetaPropertyKind::ImportMeta => false,
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// Arguments[Yield, Await] :
//      ( )
//      ( ArgumentList[?Yield, ?Await] )
//      ( ArgumentList[?Yield, ?Await] , )
#[derive(Debug)]
pub enum ArgumentsKind {
    Empty,
    ArgumentList(Rc<ArgumentList>),
    ArgumentListComma(Rc<ArgumentList>),
}
#[derive(Debug)]
pub struct Arguments {
    kind: ArgumentsKind,
}

impl fmt::Display for Arguments {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ArgumentsKind::Empty => write!(f, "( )"),
            ArgumentsKind::ArgumentList(boxed) => write!(f, "( {} )", boxed),
            ArgumentsKind::ArgumentListComma(boxed) => write!(f, "( {} , )", boxed),
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
        match &self.kind {
            ArgumentsKind::Empty => Ok(()),
            ArgumentsKind::ArgumentList(boxed) | ArgumentsKind::ArgumentListComma(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Arguments: {}", first, self)?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match &self.kind {
            ArgumentsKind::Empty => {}
            ArgumentsKind::ArgumentList(node) => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ArgumentsKind::ArgumentListComma(node) => {
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
        scan_for_punct(after_lp, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightParen).map(|after_rp| (Rc::new(Arguments { kind: ArgumentsKind::Empty }), after_rp)).otherwise(
            || {
                let (args, after_args) = ArgumentList::parse(parser, after_lp, yield_flag, await_flag)?;
                let (punct, after_punct) = scan_for_punct_set(after_args, parser.source, ScanGoal::InputElementDiv, &[Punctuator::Comma, Punctuator::RightParen])?;
                match punct {
                    Punctuator::RightParen => Ok((Rc::new(Arguments { kind: ArgumentsKind::ArgumentList(args) }), after_punct)),
                    _ => {
                        let after_rp = scan_for_punct(after_punct, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightParen)?;
                        Ok((Rc::new(Arguments { kind: ArgumentsKind::ArgumentListComma(args) }), after_rp))
                    }
                }
            },
        )
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
        match &self.kind {
            ArgumentsKind::Empty => false,
            ArgumentsKind::ArgumentList(n) => n.contains(kind),
            ArgumentsKind::ArgumentListComma(n) => n.contains(kind),
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        match &self.kind {
            ArgumentsKind::Empty => true,
            ArgumentsKind::ArgumentList(n) => n.all_private_identifiers_valid(names),
            ArgumentsKind::ArgumentListComma(n) => n.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// ArgumentList[Yield, Await] :
//      AssignmentExpression[+In, ?Yield, ?Await]
//      ... AssignmentExpression[+In, ?Yield, ?Await]
//      ArgumentList[?Yield, ?Await] , AssignmentExpression[+In, ?Yield, ?Await]
//      ArgumentList[?Yield, ?Await] , ... AssignmentExpression[+In, ?Yield, ?Await]
#[derive(Debug)]
pub enum ArgumentListKind {
    FallThru(Rc<AssignmentExpression>),
    Dots(Rc<AssignmentExpression>),
    ArgumentList(Rc<ArgumentList>, Rc<AssignmentExpression>),
    ArgumentListDots(Rc<ArgumentList>, Rc<AssignmentExpression>),
}

impl ArgumentListKind {
    // Package the results of a successful assignment_expression into an ArgumentListKind::FallThru.
    fn ae_bundle(pair: (Rc<AssignmentExpression>, Scanner)) -> Result<(Self, Scanner), ParseError> {
        let (ae_boxed, scanner) = pair;
        Ok((Self::FallThru(ae_boxed), scanner))
    }

    // Package the results of assignment_expression into an ArgumentListKind (or pass along a None)
    //fn ae_package(opt: Option<(Rc<AssignmentExpression>, Scanner)>) -> Result<Option<(Self, Scanner)>, String> {
    //    opt.map_or(Ok(None), Self::ae_bundle)
    //}

    // Parse the production
    //      ArgumentList : AssignmentExpression
    // returning one of:
    //    * an ArgumentListKind that contains all the relevant info
    //    * an Err with a human readable message about what went wrong
    pub fn parse_assignment_expression(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Self, Scanner), ParseError> {
        AssignmentExpression::parse(parser, scanner, true, yield_flag, await_flag).and_then(Self::ae_bundle)
    }

    // Parse the production
    //      ArgumentList : ... AssignmentExpression
    // returning one of:
    //    * an ArgumentListKind that contains all the relevant info
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

#[derive(Debug)]
pub struct ArgumentList {
    kind: ArgumentListKind,
}

impl fmt::Display for ArgumentList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ArgumentListKind::FallThru(boxed) => write!(f, "{}", boxed),
            ArgumentListKind::Dots(boxed) => write!(f, "... {}", boxed),
            ArgumentListKind::ArgumentList(list, exp) => write!(f, "{} , {}", list, exp),
            ArgumentListKind::ArgumentListDots(list, exp) => write!(f, "{} , ... {}", list, exp),
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
        match &self.kind {
            ArgumentListKind::FallThru(boxed) | ArgumentListKind::Dots(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            ArgumentListKind::ArgumentList(list, exp) | ArgumentListKind::ArgumentListDots(list, exp) => {
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
        match &self.kind {
            ArgumentListKind::FallThru(node) => node.concise_with_leftpad(writer, pad, state),
            ArgumentListKind::Dots(node) => {
                let successive = head(pad, state)?;
                pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ArgumentListKind::ArgumentList(list, exp) => {
                let successive = head(pad, state)?;
                list.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                exp.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ArgumentListKind::ArgumentListDots(list, exp) => {
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
        ArgumentListKind::parse_assignment_expression(parser, scanner, yield_flag, await_flag)
            .otherwise(|| ArgumentListKind::parse_dots_assignment_expression(parser, scanner, yield_flag, await_flag))
            .map(|(kind, after)| {
                let mut top_scanner = after;
                let mut top_box = Rc::new(Self { kind });
                enum Dots {
                    Dots,
                    NoDots,
                }
                while let Ok((ae, scan, dotstate)) = ArgumentListKind::parse_al_ae(parser, top_scanner, yield_flag, await_flag)
                    .map(|(ae, after_ae)| (ae, after_ae, Dots::NoDots))
                    .otherwise(|| ArgumentListKind::parse_al_dots_ae(parser, top_scanner, yield_flag, await_flag).map(|(ae, after_ae)| (ae, after_ae, Dots::Dots)))
                {
                    top_box = Rc::new(ArgumentList {
                        kind: match dotstate {
                            Dots::Dots => ArgumentListKind::ArgumentListDots(top_box, ae),
                            Dots::NoDots => ArgumentListKind::ArgumentList(top_box, ae),
                        },
                    });
                    top_scanner = scan;
                }
                (top_box, top_scanner)
            })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match &self.kind {
            ArgumentListKind::FallThru(boxed) => boxed.contains(kind),
            ArgumentListKind::Dots(boxed) => boxed.contains(kind),
            ArgumentListKind::ArgumentList(list, exp) => list.contains(kind) || exp.contains(kind),
            ArgumentListKind::ArgumentListDots(list, exp) => list.contains(kind) || exp.contains(kind),
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        match &self.kind {
            ArgumentListKind::FallThru(boxed) => boxed.all_private_identifiers_valid(names),
            ArgumentListKind::Dots(boxed) => boxed.all_private_identifiers_valid(names),
            ArgumentListKind::ArgumentList(list, exp) => list.all_private_identifiers_valid(names) && exp.all_private_identifiers_valid(names),
            ArgumentListKind::ArgumentListDots(list, exp) => list.all_private_identifiers_valid(names) && exp.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// NewExpression[Yield, Await] :
//      MemberExpression[?Yield, ?Await]
//      new NewExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum NewExpressionKind {
    MemberExpression(Rc<MemberExpression>),
    NewExpression(Rc<NewExpression>),
}

#[derive(Debug)]
pub struct NewExpression {
    kind: NewExpressionKind,
}

impl fmt::Display for NewExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            NewExpressionKind::MemberExpression(boxed) => write!(f, "{}", boxed),
            NewExpressionKind::NewExpression(boxed) => write!(f, "new {}", boxed),
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
        match &self.kind {
            NewExpressionKind::MemberExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            NewExpressionKind::NewExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match &self.kind {
            NewExpressionKind::MemberExpression(node) => node.concise_with_leftpad(writer, pad, state),
            NewExpressionKind::NewExpression(node) => {
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
        match &self.kind {
            NewExpressionKind::MemberExpression(boxed) => boxed.is_function_definition(),
            NewExpressionKind::NewExpression(_) => false,
        }
    }
}

impl AssignmentTargetType for NewExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match &self.kind {
            NewExpressionKind::MemberExpression(boxed) => boxed.assignment_target_type(),
            NewExpressionKind::NewExpression(_) => ATTKind::Invalid,
        }
    }
}

impl NewExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::NewOrMEExpected, scanner))
            .otherwise(|| {
                let (me, after_me) = MemberExpression::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(NewExpression { kind: NewExpressionKind::MemberExpression(me) }), after_me))
            })
            .otherwise(|| {
                let after_new = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::New)?;
                let (ne, after_ne) = Self::parse(parser, after_new, yield_flag, await_flag)?;
                Ok((Rc::new(NewExpression { kind: NewExpressionKind::NewExpression(ne) }), after_ne))
            })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match &self.kind {
            NewExpressionKind::MemberExpression(boxed) => boxed.contains(kind),
            NewExpressionKind::NewExpression(boxed) => boxed.contains(kind),
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match &self.kind {
            NewExpressionKind::MemberExpression(n) => n.as_string_literal(),
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
        match &self.kind {
            NewExpressionKind::MemberExpression(boxed) => boxed.all_private_identifiers_valid(names),
            NewExpressionKind::NewExpression(boxed) => boxed.all_private_identifiers_valid(names),
        }
    }

    pub fn is_object_or_array_literal(&self) -> bool {
        match &self.kind {
            NewExpressionKind::MemberExpression(boxed) => boxed.is_object_or_array_literal(),
            NewExpressionKind::NewExpression(_) => false,
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// CallMemberExpression[Yield, Await] :
//      MemberExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
#[derive(Debug)]
pub struct CallMemberExpression {
    member_expression: Rc<MemberExpression>,
    arguments: Rc<Arguments>,
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

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
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

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
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

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
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
pub enum CallExpressionKind {
    CallMemberExpression(Rc<CallMemberExpression>),
    SuperCall(Rc<SuperCall>),
    ImportCall(Rc<ImportCall>),
    CallExpressionArguments(Rc<CallExpression>, Rc<Arguments>),
    CallExpressionExpression(Rc<CallExpression>, Rc<Expression>),
    CallExpressionIdentifierName(Rc<CallExpression>, IdentifierData),
    CallExpressionTemplateLiteral(Rc<CallExpression>, Rc<TemplateLiteral>),
    CallExpressionPrivateId(Rc<CallExpression>, IdentifierData),
}

#[derive(Debug)]
pub struct CallExpression {
    kind: CallExpressionKind,
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            CallExpressionKind::CallMemberExpression(boxed) => write!(f, "{}", boxed),
            CallExpressionKind::SuperCall(boxed) => write!(f, "{}", boxed),
            CallExpressionKind::ImportCall(boxed) => write!(f, "{}", boxed),
            CallExpressionKind::CallExpressionArguments(ce, args) => write!(f, "{} {}", ce, args),
            CallExpressionKind::CallExpressionExpression(ce, exp) => write!(f, "{} [ {} ]", ce, exp),
            CallExpressionKind::CallExpressionIdentifierName(ce, int) => write!(f, "{} . {}", ce, int),
            CallExpressionKind::CallExpressionTemplateLiteral(ce, tl) => write!(f, "{} {}", ce, tl),
            CallExpressionKind::CallExpressionPrivateId(ce, id) => write!(f, "{} . #{}", ce, id),
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
        match &self.kind {
            CallExpressionKind::CallMemberExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            CallExpressionKind::SuperCall(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            CallExpressionKind::ImportCall(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            CallExpressionKind::CallExpressionArguments(ce, args) => {
                ce.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                args.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CallExpressionKind::CallExpressionExpression(ce, exp) => {
                ce.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                exp.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CallExpressionKind::CallExpressionIdentifierName(ce, _) => ce.pprint_with_leftpad(writer, &successive, Spot::Final),
            CallExpressionKind::CallExpressionTemplateLiteral(ce, tl) => {
                ce.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                tl.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CallExpressionKind::CallExpressionPrivateId(ce, _) => ce.pprint_with_leftpad(writer, &successive, Spot::Final),
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
        match &self.kind {
            CallExpressionKind::CallMemberExpression(node) => node.concise_with_leftpad(writer, pad, state),
            CallExpressionKind::SuperCall(node) => node.concise_with_leftpad(writer, pad, state),
            CallExpressionKind::ImportCall(node) => node.concise_with_leftpad(writer, pad, state),
            CallExpressionKind::CallExpressionArguments(ce, right) => {
                let successive = head(writer, ce)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            CallExpressionKind::CallExpressionExpression(ce, right) => {
                let successive = head(writer, ce)?;
                pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
            }
            CallExpressionKind::CallExpressionIdentifierName(ce, right) => {
                let successive = head(writer, ce)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, right, TokenType::IdentifierName, &successive, Spot::NotFinal)
            }
            CallExpressionKind::CallExpressionTemplateLiteral(ce, right) => {
                let successive = head(writer, ce)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            CallExpressionKind::CallExpressionPrivateId(ce, id) => {
                let successive = head(writer, ce)?;
                pprint_token(writer, ".", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                pprint_token(writer, format!("#{}", id), TokenType::PrivateIdentifier, &successive, Spot::Final)
            }
        }
    }
}

impl AssignmentTargetType for CallExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match &self.kind {
            CallExpressionKind::CallMemberExpression(_)
            | CallExpressionKind::SuperCall(_)
            | CallExpressionKind::ImportCall(_)
            | CallExpressionKind::CallExpressionArguments(..)
            | CallExpressionKind::CallExpressionTemplateLiteral(..) => ATTKind::Invalid,
            CallExpressionKind::CallExpressionExpression(..) | CallExpressionKind::CallExpressionIdentifierName(..) | CallExpressionKind::CallExpressionPrivateId(..) => ATTKind::Simple,
        }
    }
}

impl CallExpression {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_arg: bool, await_arg: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::CallExpression), scanner))
            .otherwise(|| {
                CallMemberExpression::parse(parser, scanner, yield_arg, await_arg).map(|(cme, after_cme)| (Rc::new(Self { kind: CallExpressionKind::CallMemberExpression(cme) }), after_cme))
            })
            .otherwise(|| SuperCall::parse(parser, scanner, yield_arg, await_arg).map(|(sc, after_sc)| (Rc::new(Self { kind: CallExpressionKind::SuperCall(sc) }), after_sc)))
            .otherwise(|| ImportCall::parse(parser, scanner, yield_arg, await_arg).map(|(ic, after_ic)| (Rc::new(Self { kind: CallExpressionKind::ImportCall(ic) }), after_ic)))
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
                    top_box = Rc::new(CallExpression {
                        kind: match follow {
                            Follow::Exp(exp) => CallExpressionKind::CallExpressionExpression(top_box, exp),
                            Follow::Id(id) => CallExpressionKind::CallExpressionIdentifierName(top_box, id),
                            Follow::TLit(tl) => CallExpressionKind::CallExpressionTemplateLiteral(top_box, tl),
                            Follow::Args(args) => CallExpressionKind::CallExpressionArguments(top_box, args),
                            Follow::Pid(id) => CallExpressionKind::CallExpressionPrivateId(top_box, id),
                        },
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
        match &self.kind {
            CallExpressionKind::CallMemberExpression(boxed) => boxed.contains(kind),
            CallExpressionKind::SuperCall(boxed) => kind == ParseNodeKind::SuperCall || boxed.contains(kind),
            CallExpressionKind::ImportCall(boxed) => boxed.contains(kind),
            CallExpressionKind::CallExpressionArguments(ce, args) => ce.contains(kind) || args.contains(kind),
            CallExpressionKind::CallExpressionExpression(ce, exp) => ce.contains(kind) || exp.contains(kind),
            CallExpressionKind::CallExpressionIdentifierName(ce, _) | CallExpressionKind::CallExpressionPrivateId(ce, _) => ce.contains(kind),
            CallExpressionKind::CallExpressionTemplateLiteral(ce, tl) => ce.contains(kind) || tl.contains(kind),
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        match &self.kind {
            //  1. For each child node child of this Parse Node, do
            //      a. If child is an instance of a nonterminal, then
            //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
            //  2. Return true.
            CallExpressionKind::CallMemberExpression(boxed) => boxed.all_private_identifiers_valid(names),
            CallExpressionKind::SuperCall(boxed) => boxed.all_private_identifiers_valid(names),
            CallExpressionKind::ImportCall(boxed) => boxed.all_private_identifiers_valid(names),
            CallExpressionKind::CallExpressionArguments(ce, args) => ce.all_private_identifiers_valid(names) && args.all_private_identifiers_valid(names),
            CallExpressionKind::CallExpressionExpression(ce, exp) => ce.all_private_identifiers_valid(names) && exp.all_private_identifiers_valid(names),
            CallExpressionKind::CallExpressionIdentifierName(ce, _) => ce.all_private_identifiers_valid(names),
            CallExpressionKind::CallExpressionTemplateLiteral(ce, tl) => ce.all_private_identifiers_valid(names) && tl.all_private_identifiers_valid(names),

            // CallExpression : CallExpression . PrivateIdentifier
            //  1. If names contains the StringValue of PrivateIdentifier, then
            //      a. Return AllPrivateIdentifiersValid of CallExpression with argument names.
            //  2. Return false.
            CallExpressionKind::CallExpressionPrivateId(ce, id) => names.contains(&id.string_value) && ce.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
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

impl AssignmentTargetType for LeftHandSideExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            LeftHandSideExpression::New(boxed) => boxed.assignment_target_type(),
            LeftHandSideExpression::Call(boxed) => boxed.assignment_target_type(),
            LeftHandSideExpression::Optional(_) => ATTKind::Invalid,
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

    pub fn is_object_or_array_literal(&self) -> bool {
        match self {
            LeftHandSideExpression::New(boxed) => boxed.is_object_or_array_literal(),
            LeftHandSideExpression::Call(_) | LeftHandSideExpression::Optional(_) => false,
        }
    }

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
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

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
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
            OptionalChain::Ident(node) => write!(f, "?. {}", node),
            OptionalChain::Template(node) => write!(f, "?. {}", node),
            OptionalChain::PrivateId(node) => write!(f, "?. #{}", node),
            OptionalChain::PlusArgs(lst, item) => write!(f, "{} {}", lst, item),
            OptionalChain::PlusExp(lst, item) => write!(f, "{} [ {} ]", lst, item),
            OptionalChain::PlusIdent(lst, item) => write!(f, "{} . {}", lst, item),
            OptionalChain::PlusTemplate(lst, item) => write!(f, "{} {}", lst, item),
            OptionalChain::PlusPrivateId(lst, item) => write!(f, "{} . #{}", lst, item),
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
            OptionalChain::PrivateId(node) => pprint_token(writer, format!("#{}", node), TokenType::PrivateIdentifier, &successive, Spot::Final),
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
                pprint_token(writer, format!("#{}", item), TokenType::PrivateIdentifier, &successive, Spot::Final)
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
                pprint_token(writer, format!("#{}", node), TokenType::PrivateIdentifier, &successive, Spot::Final)
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
                pprint_token(writer, format!("#{}", item), TokenType::PrivateIdentifier, &successive, Spot::Final)
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

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

#[cfg(test)]
mod tests;
