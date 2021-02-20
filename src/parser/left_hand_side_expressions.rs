use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::assignment_operators::AssignmentExpression;
use super::comma_operator::Expression;
use super::primary_expressions::PrimaryExpression;
use super::primary_expressions::TemplateLiteral;
use super::scanner::{IdentifierData, Keyword, Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot};

//////// 12.3 Left-Hand-Side Expressions

// MemberExpression[Yield, Await] :
//      PrimaryExpression[?Yield, ?Await]
//      MemberExpression[?Yield, ?Await] [ Expression[+In, ?Yield, ?Await] ]
//      MemberExpression[?Yield, ?Await] . IdentifierName
//      MemberExpression[?Yield, ?Await] TemplateLiteral[?Yield, ?Await, +Tagged]
//      SuperProperty[?Yield, ?Await]
//      MetaProperty
//      new MemberExpression[?Yield, ?Await] Arguments[?Yield, ?Await]

// How to parse:
// if PrimaryExpression, SuperProperty, or MetaProperty is detected,
//      make a MemberExpression node.
// if a "new" token is detected, make a MemberExpression node.
// if neither of those, return None.
// Check for the "after member expression" tokens, "[", ".", or a TemplateLiteral.
// If they're there, build up one of the interior productions and loop.

#[derive(Debug)]
pub enum MemberExpressionKind {
    PrimaryExpression(Box<PrimaryExpression>),
    Expression(Box<MemberExpression>, Box<Expression>),
    IdentifierName(Box<MemberExpression>, IdentifierData),
    TemplateLiteral(Box<MemberExpression>, Box<TemplateLiteral>),
    SuperProperty(Box<SuperProperty>),
    MetaProperty(Box<MetaProperty>),
    NewArguments(Box<MemberExpression>, Box<Arguments>),
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
            MemberExpressionKind::IdentifierName(me, _) => me.pprint_with_leftpad(writer, &successive, Spot::Final),
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
                pprint_token(writer, "[", &successive, Spot::NotFinal)?;
                exp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", &successive, Spot::Final)
            }
            MemberExpressionKind::IdentifierName(me, id) => {
                let successive = head(pad, state)?;
                me.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", &successive, Spot::NotFinal)?;
                pprint_token(writer, &format!("{}", id), &successive, Spot::Final)
            }
            MemberExpressionKind::TemplateLiteral(me, tl) => {
                let successive = head(pad, state)?;
                me.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                tl.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            MemberExpressionKind::NewArguments(me, args) => {
                let successive = head(pad, state)?;
                pprint_token(writer, "new", &successive, Spot::NotFinal)?;
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
            | MemberExpressionKind::NewArguments(..) => false,
        }
    }
}

impl AssignmentTargetType for MemberExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match &self.kind {
            MemberExpressionKind::PrimaryExpression(boxed) => boxed.assignment_target_type(),
            MemberExpressionKind::Expression(..) => ATTKind::Simple,
            MemberExpressionKind::IdentifierName(..) => ATTKind::Simple,
            MemberExpressionKind::TemplateLiteral(..) => ATTKind::Invalid,
            MemberExpressionKind::SuperProperty(..) => ATTKind::Simple,
            MemberExpressionKind::MetaProperty(boxed) => boxed.assignment_target_type(),
            MemberExpressionKind::NewArguments(..) => ATTKind::Invalid,
        }
    }
}

pub trait ToMemberExpressionKind {
    fn to_member_expression_kind(node: Box<Self>) -> MemberExpressionKind;
}

impl ToMemberExpressionKind for PrimaryExpression {
    fn to_member_expression_kind(node: Box<Self>) -> MemberExpressionKind {
        MemberExpressionKind::PrimaryExpression(node)
    }
}

impl ToMemberExpressionKind for SuperProperty {
    fn to_member_expression_kind(node: Box<Self>) -> MemberExpressionKind {
        MemberExpressionKind::SuperProperty(node)
    }
}

impl ToMemberExpressionKind for MetaProperty {
    fn to_member_expression_kind(node: Box<Self>) -> MemberExpressionKind {
        MemberExpressionKind::MetaProperty(node)
    }
}

// impl ToMemberExpressionKind for NewMemberExpressionArguments {
//     fn to_member_expression_kind(node: Box<Self>) -> MemberExpressionKind {
//         MemberExpressionKind::NewArguments(*node)
//     }
// }

fn me_boxer<T>(pair: (Box<T>, Scanner)) -> Result<(Box<MemberExpression>, Scanner), ParseError>
where
    T: ToMemberExpressionKind,
{
    let (node, scanner) = pair;
    Ok((Box::new(MemberExpression { kind: T::to_member_expression_kind(node) }), scanner))
}

//fn or_me_kind<F, T>(opt: Option<(Box<MemberExpression>, Scanner)>, parser: &mut Parser, parse_func: F) -> Result<Option<(Box<MemberExpression>, Scanner)>, String>
//where
//    F: FnOnce(&mut Parser) -> Result<Option<(Box<T>, Scanner)>, String>,
//    T: ToMemberExpressionKind,
//{
//    opt.map_or_else(|| parse_func(parser).and_then(me_boxer), rewrap)
//}

fn member_expression_head_recursive(
    parser: &mut Parser,
    yield_flag: bool,
    await_flag: bool,
    me: Box<MemberExpression>,
    scan: Scanner,
) -> Result<(Box<MemberExpression>, Scanner), ParseError> {
    enum After {
        Exp(Box<Expression>),
        Id(IdentifierData),
        TLit(Box<TemplateLiteral>),
    }
    let mut current_me = me;
    let mut after_scan = scan;
    loop {
        match TemplateLiteral::parse(parser, after_scan, yield_flag, await_flag, true).map(|(tl, after_tl)| (After::TLit(tl), after_tl)).otherwise(|| {
            scan_for_punct_set(after_scan, parser.source, ScanGoal::InputElementRegExp, &[Punctuator::Dot, Punctuator::LeftBracket]).and_then(|(punct, after)| match punct {
                Punctuator::Dot => scan_for_identifiername(after, parser.source, ScanGoal::InputElementRegExp).map(|(id, after_id)| (After::Id(id), after_id)),
                Punctuator::LeftBracket | _ => Expression::parse(parser, after, true, yield_flag, await_flag).and_then(|(expression, after_exp)| {
                    scan_for_punct(after_exp, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBracket).map(|after_bracket| (After::Exp(expression), after_bracket))
                }),
            })
        }) {
            Err(_) => {
                break;
            }
            Ok((parts, after_production)) => {
                current_me = match parts {
                    After::TLit(tl) => Box::new(MemberExpression { kind: MemberExpressionKind::TemplateLiteral(current_me, tl) }),
                    After::Exp(exp) => Box::new(MemberExpression { kind: MemberExpressionKind::Expression(current_me, exp) }),
                    After::Id(id) => Box::new(MemberExpression { kind: MemberExpressionKind::IdentifierName(current_me, id) }),
                };
                after_scan = after_production;
            }
        }
        //let (tok, after) = scan_token(&after_scan, parser.source, ScanGoal::InputElementRegExp);
        //match tok {
        //    Token::Punctuator(Punctuator::Dot) => {
        //        let token_after_dot = scan_token(&after, parser.source, ScanGoal::InputElementRegExp);
        //        if let (Token::Identifier(id), after_id) = token_after_dot {
        //            let me = Box::new(MemberExpression {
        //                kind: MemberExpressionKind::IdentifierName(MemberExpressionIdentifierName {
        //                    member_expression: current_me,
        //                    identifier_name: Box::new(IdentifierNameToken { value: id }),
        //                }),
        //            });
        //            current_me = me;
        //            after_scan = after_id;
        //        } else {
        //            return Err(format!("Expected IdentifierName after ‘.’."));
        //        }
        //    }
        //    Token::Punctuator(Punctuator::LeftBracket) => {
        //        let potential_expression = Expression::parse(parser, after, true, yield_flag, await_flag)?;
        //        match potential_expression {
        //            None => {
        //                return Err(format!("Expect Expression after ‘[’."));
        //            }
        //            Some((expression, after_exp)) => {
        //                let after_exp = scan_token(&after_exp, parser.source, ScanGoal::InputElementRegExp);
        //                match after_exp {
        //                    (Token::Punctuator(Punctuator::RightBracket), scanner) => {
        //                        let me = Box::new(MemberExpression {
        //                            kind: MemberExpressionKind::Expression(MemberExpressionExpression { member_expression: current_me, expression: expression }),
        //                        });
        //                        current_me = me;
        //                        after_scan = scanner;
        //                    }
        //                    _ => {
        //                        return Err(format!("Expect ‘]’ after expression."));
        //                    }
        //                }
        //            }
        //        }
        //    }
        //    _ => {
        //        break;
        //    }
        //}
    }
    Ok((current_me, after_scan))
}

impl MemberExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("MemberExpression expected", scanner.line, scanner.column))
            // First: All the non-head-recursive productions
            .otherwise(|| PrimaryExpression::parse(parser, scanner, yield_flag, await_flag).and_then(me_boxer))
            .otherwise(|| SuperProperty::parse(parser, scanner, yield_flag, await_flag).and_then(me_boxer))
            .otherwise(|| MetaProperty::parse(parser, scanner).and_then(me_boxer))
            .otherwise(|| {
                Self::new_memberexpression_arguments(parser, scanner, yield_flag, await_flag)
                    .map(|(me, args, after)| (Box::new(MemberExpression { kind: MemberExpressionKind::NewArguments(me, args) }), after))
            })
            // And then all the head-recursive productions.
            .and_then(|(me, scan)| member_expression_head_recursive(parser, yield_flag, await_flag, me, scan))
    }
    fn new_memberexpression_arguments(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<MemberExpression>, Box<Arguments>, Scanner), ParseError> {
        let after_new = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::New)?;
        let (me, after_me) = MemberExpression::parse(parser, after_new, yield_flag, await_flag)?;
        let (args, after_args) = Arguments::parse(parser, after_me, yield_flag, await_flag)?;
        Ok((me, args, after_args))
    }
}

// SuperProperty[Yield, Await] :
//      super [ Expression[+In, ?Yield, ?Await] ]
//      super . IdentifierName
#[derive(Debug)]
pub enum SuperPropertyKind {
    Expression(Box<Expression>),
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
                pprint_token(writer, "super", &successive, Spot::NotFinal)?;
                pprint_token(writer, "[", &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", &successive, Spot::Final)
            }
            SuperPropertyKind::IdentifierName(id) => {
                pprint_token(writer, "super", &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", &successive, Spot::NotFinal)?;
                pprint_token(writer, &format!("{}", id), &successive, Spot::Final)
            }
        }
    }
}

impl SuperProperty {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<SuperProperty>, Scanner), ParseError> {
        let after_super = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Super)?;
        let (punct, after_punct) = scan_for_punct_set(after_super, parser.source, ScanGoal::InputElementRegExp, &[Punctuator::Dot, Punctuator::LeftBracket])?;
        match punct {
            Punctuator::LeftBracket => {
                let (exp, after_exp) = Expression::parse(parser, after_punct, true, yield_flag, await_flag)?;
                let after_rb = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBracket)?;
                Ok((Box::new(SuperProperty { kind: SuperPropertyKind::Expression(exp) }), after_rb))
            }
            Punctuator::Dot | _ => {
                let (id, after_id) = scan_for_identifiername(after_punct, parser.source, ScanGoal::InputElementRegExp)?;
                Ok((Box::new(SuperProperty { kind: SuperPropertyKind::IdentifierName(id) }), after_id))
            }
        }

        //Ok(scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp)).and_then(|(token, scanner)| match token {
        //    Token::Identifier(id) if id.matches(Keyword::Super) => Ok(scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp)).and_then(|(token, scanner)| match token {
        //        Token::Punctuator(Punctuator::LeftBracket) => Expression::parse(parser, scanner, true, yield_flag, await_flag).and_then(|opt| {
        //            opt.map_or_else(
        //                || Err(String::from("‘super[’ must be followed by an Expression")),
        //                |(exp_boxed, scanner)| {
        //                    Ok(scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp)).and_then(|(token, scanner)| match token {
        //                        Token::Punctuator(Punctuator::RightBracket) => Ok(Some((Box::new(SuperProperty { kind: SuperPropertyKind::Expression(exp_boxed) }), scanner))),
        //                        _ => Err(String::from("‘super[ Expression’ must be closed by a ‘]’.")),
        //                    })
        //                },
        //            )
        //        }),
        //        Token::Punctuator(Punctuator::Dot) => Ok(scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp)).and_then(|(token, scanner)| match token {
        //            Token::Identifier(id) => Ok(Some((Box::new(SuperProperty { kind: SuperPropertyKind::IdentifierName(Box::new(IdentifierNameToken { value: id })) }), scanner))),
        //            _ => Err(String::from("‘super.’ must be followed by an IdentifierName")),
        //        }),
        //        _ => Ok(None),
        //    }),
        //    _ => Ok(None),
        //})
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
                pprint_token(writer, "new", &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", &successive, Spot::NotFinal)?;
                pprint_token(writer, "target", &successive, Spot::Final)
            }
            MetaPropertyKind::ImportMeta => {
                pprint_token(writer, "import", &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", &successive, Spot::NotFinal)?;
                pprint_token(writer, "meta", &successive, Spot::Final)
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
    fn dot_token(parser: &mut Parser, scanner: Scanner, kwd: Keyword, kind: MetaPropertyKind) -> Result<(Box<MetaProperty>, Scanner), ParseError> {
        let after_dot = scan_for_punct(scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Dot)?;
        let after_kwd = scan_for_keyword(after_dot, parser.source, ScanGoal::InputElementRegExp, kwd)?;
        Ok((Box::new(MetaProperty { kind }), after_kwd))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner) -> Result<(Box<MetaProperty>, Scanner), ParseError> {
        let (kwd, after_kwd) = scan_for_keywords(scanner, parser.source, ScanGoal::InputElementRegExp, &[Keyword::New, Keyword::Import])?;
        match kwd {
            Keyword::New => Self::dot_token(parser, after_kwd, Keyword::Target, MetaPropertyKind::NewTarget),
            Keyword::Import | _ => Self::dot_token(parser, after_kwd, Keyword::Meta, MetaPropertyKind::ImportMeta),
        }
    }
}

// Arguments[Yield, Await] :
//      ( )
//      ( ArgumentList[?Yield, ?Await] )
//      ( ArgumentList[?Yield, ?Await] , )
#[derive(Debug)]
pub enum ArgumentsKind {
    Empty,
    ArgumentList(Box<ArgumentList>),
    ArgumentListComma(Box<ArgumentList>),
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
        pprint_token(writer, "(", &successive, Spot::NotFinal)?;
        match &self.kind {
            ArgumentsKind::Empty => {}
            ArgumentsKind::ArgumentList(node) => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ArgumentsKind::ArgumentListComma(node) => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", &successive, Spot::NotFinal)?;
            }
        }
        pprint_token(writer, ")", &successive, Spot::Final)
    }
}

impl Arguments {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Arguments>, Scanner), ParseError> {
        let after_lp = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftParen)?;
        scan_for_punct(after_lp, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightParen)
            .and_then(|after_rp| Ok((Box::new(Arguments { kind: ArgumentsKind::Empty }), after_rp)))
            .otherwise(|| {
                let (args, after_args) = ArgumentList::parse(parser, after_lp, yield_flag, await_flag)?;
                let (punct, after_punct) = scan_for_punct_set(after_args, parser.source, ScanGoal::InputElementDiv, &[Punctuator::Comma, Punctuator::RightParen])?;
                match punct {
                    Punctuator::RightParen => Ok((Box::new(Arguments { kind: ArgumentsKind::ArgumentList(args) }), after_punct)),
                    Punctuator::Comma | _ => {
                        let after_rp = scan_for_punct(after_punct, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightParen)?;
                        Ok((Box::new(Arguments { kind: ArgumentsKind::ArgumentListComma(args) }), after_rp))
                    }
                }
            })

        //Ok(scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp)).and_then(|(token, scanner)| match token {
        //    Token::Punctuator(Punctuator::LeftParen) => ArgumentList::parse(parser, scanner, yield_flag, await_flag).and_then(|opt| {
        //        opt.map_or_else(
        //            || {
        //                Ok(scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp)).and_then(|(token, scanner)| match token {
        //                    Token::Punctuator(Punctuator::RightParen) => Ok(Some((Box::new(Arguments { kind: ArgumentsKind::Empty }), scanner))),
        //                    _ => Ok(None),
        //                })
        //            },
        //            |(arglist_boxed, scanner)| {
        //                Ok(scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp)).and_then(|(token, scanner)| match token {
        //                    Token::Punctuator(Punctuator::Comma) => Ok(scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp)).and_then(|(token, scanner)| match token {
        //                        Token::Punctuator(Punctuator::RightParen) => Ok(Some((Box::new(Arguments { kind: ArgumentsKind::ArgumentListComma(arglist_boxed) }), scanner))),
        //                        _ => Ok(None),
        //                    }),
        //                    Token::Punctuator(Punctuator::RightParen) => Ok(Some((Box::new(Arguments { kind: ArgumentsKind::ArgumentList(arglist_boxed) }), scanner))),
        //                    _ => Ok(None),
        //                })
        //            },
        //        )
        //    }),
        //    _ => Ok(None),
        //})
    }
}

// ArgumentList[Yield, Await] :
//      AssignmentExpression[+In, ?Yield, ?Await]
//      ... AssignmentExpression[+In, ?Yield, ?Await]
//      ArgumentList[?Yield, ?Await] , AssignmentExpression[+In, ?Yield, ?Await]
//      ArgumentList[?Yield, ?Await] , ... AssignmentExpression[+In, ?Yield, ?Await]
#[derive(Debug)]
pub enum ArgumentListKind {
    AssignmentExpression(Box<AssignmentExpression>),
    DotsAssignmentExpression(Box<AssignmentExpression>),
    ArgumentListAssignmentExpression(Box<ArgumentList>, Box<AssignmentExpression>),
    ArgumentListDotsAssignmentExpression(Box<ArgumentList>, Box<AssignmentExpression>),
}

impl ArgumentListKind {
    // Package the results of a successful assignment_expression into an ArgumentListKind::AssignmentExpression.
    fn ae_bundle(pair: (Box<AssignmentExpression>, Scanner)) -> Result<(Self, Scanner), ParseError> {
        let (ae_boxed, scanner) = pair;
        Ok((Self::AssignmentExpression(ae_boxed), scanner))
    }

    // Package the results of assignment_expression into an ArgumentListKind (or pass along a None)
    //fn ae_package(opt: Option<(Box<AssignmentExpression>, Scanner)>) -> Result<Option<(Self, Scanner)>, String> {
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
        Ok((Self::DotsAssignmentExpression(ae), after_ae))
    }

    // Parse the production
    //      ArgumentList : ArgumentList , AssignmentExpression
    // ASSUMING: that the first ArgumentList has already been parsed. (I.e: just do the part starting with the comma.)
    // returning one of:
    //    * a pair: (Box<AssignmentExpression>, Scanner)
    //    * an Err with a human readable message about what went wrong
    fn parse_al_ae(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<AssignmentExpression>, Scanner), ParseError> {
        let after_comma = scan_for_punct(scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)?;
        AssignmentExpression::parse(parser, after_comma, true, yield_flag, await_flag)
    }

    // Parse the production
    //      ArgumentList : ArgumentList , ... AssignmentExpression
    // ASSUMING: that the first ArgumentList has already been parsed. (I.e: just do the part starting with the comma.)
    // returning one of:
    //    * a pair: (Box<AssignmentExpression>, Scanner)
    //    * an Err with a human readable message about what went wrong
    fn parse_al_dots_ae(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<AssignmentExpression>, Scanner), ParseError> {
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
            ArgumentListKind::AssignmentExpression(boxed) => write!(f, "{}", boxed),
            ArgumentListKind::DotsAssignmentExpression(boxed) => write!(f, "... {}", boxed),
            ArgumentListKind::ArgumentListAssignmentExpression(list, exp) => write!(f, "{} , {}", list, exp),
            ArgumentListKind::ArgumentListDotsAssignmentExpression(list, exp) => write!(f, "{} , ... {}", list, exp),
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
            ArgumentListKind::AssignmentExpression(boxed) | ArgumentListKind::DotsAssignmentExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            ArgumentListKind::ArgumentListAssignmentExpression(list, exp) | ArgumentListKind::ArgumentListDotsAssignmentExpression(list, exp) => {
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
            ArgumentListKind::AssignmentExpression(node) => node.concise_with_leftpad(writer, pad, state),
            ArgumentListKind::DotsAssignmentExpression(node) => {
                let successive = head(pad, state)?;
                pprint_token(writer, "...", &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ArgumentListKind::ArgumentListAssignmentExpression(list, exp) => {
                let successive = head(pad, state)?;
                list.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", &successive, Spot::NotFinal)?;
                exp.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ArgumentListKind::ArgumentListDotsAssignmentExpression(list, exp) => {
                let successive = head(pad, state)?;
                list.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", &successive, Spot::NotFinal)?;
                pprint_token(writer, "...", &successive, Spot::NotFinal)?;
                exp.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl ArgumentList {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        ArgumentListKind::parse_assignment_expression(parser, scanner, yield_flag, await_flag)
            .otherwise(|| ArgumentListKind::parse_dots_assignment_expression(parser, scanner, yield_flag, await_flag))
            .and_then(|(kind, after)| {
                let mut top_scanner = after;
                let mut top_box = Box::new(Self { kind });
                loop {
                    enum Dots {
                        Dots,
                        NoDots,
                    }
                    match ArgumentListKind::parse_al_ae(parser, top_scanner, yield_flag, await_flag)
                        .map(|(ae, after_ae)| (ae, after_ae, Dots::NoDots))
                        .otherwise(|| ArgumentListKind::parse_al_dots_ae(parser, top_scanner, yield_flag, await_flag).map(|(ae, after_ae)| (ae, after_ae, Dots::Dots)))
                    {
                        Ok((ae, scan, dotstate)) => {
                            top_box = Box::new(ArgumentList {
                                kind: match dotstate {
                                    Dots::Dots => ArgumentListKind::ArgumentListDotsAssignmentExpression(top_box, ae),
                                    Dots::NoDots => ArgumentListKind::ArgumentListAssignmentExpression(top_box, ae),
                                },
                            });
                            top_scanner = scan;
                        }
                        Err(_) => {
                            break;
                        }
                    }
                }
                Ok((top_box, top_scanner))
            })

        //let mut k = ArgumentListKind::parse_assignment_expression(parser, scanner, yield_flag, await_flag);
        //if k.is_none() {
        //    k = ArgumentListKind::parse_dots_assignment_expression(parser, scanner, yield_flag, await_flag)?;
        //    if k.is_none() {
        //        return Ok(None);
        //    }
        //}
        //let (kind, mut top_scanner) = k.unwrap();
        //let mut top_box = Box::new(Self { kind });
        //loop {
        //    let pot_alae = ArgumentListKind::parse_al_ae(parser, top_scanner, yield_flag, await_flag)?;
        //    if let Some((boxed_ae, scanner)) = pot_alae {
        //        top_box = Self::alae_boxer(top_box, boxed_ae);
        //        top_scanner = scanner;
        //    } else {
        //        let pot_al_dots_ae = ArgumentListKind::parse_al_dots_ae(parser, top_scanner, yield_flag, await_flag)?;
        //        if let Some((boxed_ae, scanner)) = pot_al_dots_ae {
        //            top_box = Self::aldotsae_boxer(top_box, boxed_ae);
        //            top_scanner = scanner;
        //        } else {
        //            break;
        //        }
        //    }
        //}
        //Ok(Some((top_box, top_scanner)))
    }
}

// NewExpression[Yield, Await] :
//      MemberExpression[?Yield, ?Await]
//      new NewExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum NewExpressionKind {
    MemberExpression(Box<MemberExpression>),
    NewExpression(Box<NewExpression>),
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
                pprint_token(writer, "new", &successive, Spot::NotFinal)?;
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<NewExpression>, Scanner), ParseError> {
        Err(ParseError::new("‘new’ or MemberExpression expected", scanner.line, scanner.column))
            .otherwise(|| {
                let after_new = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::New)?;
                let (ne, after_ne) = Self::parse(parser, after_new, yield_flag, await_flag)?;
                Ok((Box::new(NewExpression { kind: NewExpressionKind::NewExpression(ne) }), after_ne))
            })
            .otherwise(|| {
                let (me, after_me) = MemberExpression::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Box::new(NewExpression { kind: NewExpressionKind::MemberExpression(me) }), after_me))
            })
    }
}

// CallMemberExpression[Yield, Await] :
//      MemberExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
#[derive(Debug)]
pub struct CallMemberExpression {
    member_expression: Box<MemberExpression>,
    arguments: Box<Arguments>,
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<CallMemberExpression>, Scanner), ParseError> {
        let (me, after_me) = MemberExpression::parse(parser, scanner, yield_flag, await_flag)?;
        let (args, after_args) = Arguments::parse(parser, after_me, yield_flag, await_flag)?;
        Ok((Box::new(CallMemberExpression { member_expression: me, arguments: args }), after_args))
    }
}

// SuperCall[Yield, Await] :
//      super Arguments[?Yield, ?Await]
#[derive(Debug)]
pub struct SuperCall {
    arguments: Box<Arguments>,
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
        pprint_token(writer, "super", &successive, Spot::NotFinal)?;
        self.arguments.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl SuperCall {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_super = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Super)?;
        let (args, after_args) = Arguments::parse(parser, after_super, yield_flag, await_flag)?;
        Ok((Box::new(Self { arguments: args }), after_args))
    }
}

// ImportCall[Yield, Await] :
//      import ( AssignmentExpression[+In, ?Yield, ?Await] )
#[derive(Debug)]
pub struct ImportCall {
    assignment_expression: Box<AssignmentExpression>,
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
        pprint_token(writer, "import", &successive, Spot::NotFinal)?;
        pprint_token(writer, "(", &successive, Spot::NotFinal)?;
        self.assignment_expression.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", &successive, Spot::Final)
    }
}

impl ImportCall {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_import = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Import)?;
        let after_lp = scan_for_punct(after_import, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftParen)?;
        let (ae, after_ae) = AssignmentExpression::parse(parser, after_lp, true, yield_flag, await_flag)?;
        let after_rp = scan_for_punct(after_ae, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightParen)?;
        Ok((Box::new(Self { assignment_expression: ae }), after_rp))
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
#[derive(Debug)]
pub enum CallExpressionKind {
    CallMemberExpression(Box<CallMemberExpression>),
    SuperCall(Box<SuperCall>),
    ImportCall(Box<ImportCall>),
    CallExpressionArguments(Box<CallExpression>, Box<Arguments>),
    CallExpressionExpression(Box<CallExpression>, Box<Expression>),
    CallExpressionIdentifierName(Box<CallExpression>, IdentifierData),
    CallExpressionTemplateLiteral(Box<CallExpression>, Box<TemplateLiteral>),
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
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let head = |writer: &mut T, node: &Box<CallExpression>| {
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
                pprint_token(writer, "[", &successive, Spot::NotFinal)?;
                right.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", &successive, Spot::Final)
            }
            CallExpressionKind::CallExpressionIdentifierName(ce, right) => {
                let successive = head(writer, ce)?;
                pprint_token(writer, ".", &successive, Spot::NotFinal)?;
                pprint_token(writer, &format!("{}", right), &successive, Spot::NotFinal)
            }
            CallExpressionKind::CallExpressionTemplateLiteral(ce, right) => {
                let successive = head(writer, ce)?;
                right.concise_with_leftpad(writer, &successive, Spot::Final)
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
            CallExpressionKind::CallExpressionExpression(..) | CallExpressionKind::CallExpressionIdentifierName(..) => ATTKind::Simple,
        }
    }
}

impl CallExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_arg: bool, await_arg: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("CallExpression expected", scanner.line, scanner.column))
            .otherwise(|| {
                CallMemberExpression::parse(parser, scanner, yield_arg, await_arg)
                    .and_then(|(cme, after_cme)| Ok((Box::new(Self { kind: CallExpressionKind::CallMemberExpression(cme) }), after_cme)))
            })
            .otherwise(|| SuperCall::parse(parser, scanner, yield_arg, await_arg).and_then(|(sc, after_sc)| Ok((Box::new(Self { kind: CallExpressionKind::SuperCall(sc) }), after_sc))))
            .otherwise(|| ImportCall::parse(parser, scanner, yield_arg, await_arg).and_then(|(ic, after_ic)| Ok((Box::new(Self { kind: CallExpressionKind::ImportCall(ic) }), after_ic))))
            .and_then(|(ce, after_ce)| {
                enum Follow {
                    Args(Box<Arguments>),
                    Exp(Box<Expression>),
                    Id(IdentifierData),
                    TLit(Box<TemplateLiteral>),
                }
                let mut top_box = ce;
                let mut top_scanner = after_ce;
                loop {
                    match Arguments::parse(parser, top_scanner, yield_arg, await_arg)
                        .map(|(args, after_args)| (Follow::Args(args), after_args))
                        .otherwise(|| TemplateLiteral::parse(parser, top_scanner, yield_arg, await_arg, true).map(|(tl, after_tl)| (Follow::TLit(tl), after_tl)))
                        .otherwise(|| {
                            let (punct, after_punct) = scan_for_punct_set(top_scanner, parser.source, ScanGoal::InputElementDiv, &[Punctuator::Dot, Punctuator::LeftBracket])?;
                            match punct {
                                Punctuator::Dot => {
                                    let (id, after_id) = scan_for_identifiername(after_punct, parser.source, ScanGoal::InputElementDiv)?;
                                    Ok((Follow::Id(id), after_id))
                                }
                                Punctuator::LeftBracket | _ => {
                                    let (exp, after_exp) = Expression::parse(parser, after_punct, true, yield_arg, await_arg)?;
                                    let after_rb = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBracket)?;
                                    Ok((Follow::Exp(exp), after_rb))
                                }
                            }
                        }) {
                        Ok((follow, scan)) => {
                            top_box = Box::new(CallExpression {
                                kind: match follow {
                                    Follow::Exp(exp) => CallExpressionKind::CallExpressionExpression(top_box, exp),
                                    Follow::Id(id) => CallExpressionKind::CallExpressionIdentifierName(top_box, id),
                                    Follow::TLit(tl) => CallExpressionKind::CallExpressionTemplateLiteral(top_box, tl),
                                    Follow::Args(args) => CallExpressionKind::CallExpressionArguments(top_box, args),
                                },
                            });
                            top_scanner = scan;
                        }
                        Err(_) => {
                            break;
                        }
                    }
                }
                Ok((top_box, top_scanner))
            })
    }
}

// LeftHandSideExpression[Yield, Await] :
//      NewExpression[?Yield, ?Await]
//      CallExpression[?Yield, ?Await]
//      OptionalExpression[?Yield, ?Await]
#[derive(Debug)]
pub enum LeftHandSideExpression {
    NewExpression(Box<NewExpression>),
    CallExpression(Box<CallExpression>),
    OptionalExpression(Box<OptionalExpression>),
}

impl fmt::Display for LeftHandSideExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            LeftHandSideExpression::NewExpression(boxed) => write!(f, "{}", boxed),
            LeftHandSideExpression::CallExpression(boxed) => write!(f, "{}", boxed),
            LeftHandSideExpression::OptionalExpression(boxed) => write!(f, "{}", boxed),
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
            LeftHandSideExpression::NewExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            LeftHandSideExpression::CallExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            LeftHandSideExpression::OptionalExpression(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            LeftHandSideExpression::NewExpression(node) => node.concise_with_leftpad(writer, pad, state),
            LeftHandSideExpression::CallExpression(node) => node.concise_with_leftpad(writer, pad, state),
            LeftHandSideExpression::OptionalExpression(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl IsFunctionDefinition for LeftHandSideExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            LeftHandSideExpression::NewExpression(boxed) => boxed.is_function_definition(),
            LeftHandSideExpression::OptionalExpression(_) | LeftHandSideExpression::CallExpression(_) => false,
        }
    }
}

impl AssignmentTargetType for LeftHandSideExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            LeftHandSideExpression::NewExpression(boxed) => boxed.assignment_target_type(),
            LeftHandSideExpression::CallExpression(boxed) => boxed.assignment_target_type(),
            LeftHandSideExpression::OptionalExpression(_) => ATTKind::Invalid,
        }
    }
}

impl LeftHandSideExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_arg: bool, await_arg: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("LeftHandSideExpression expected", scanner.line, scanner.column))
            .otherwise(|| OptionalExpression::parse(parser, scanner, yield_arg, await_arg).and_then(|(opt, after_opt)| Ok((Box::new(Self::OptionalExpression(opt)), after_opt))))
            .otherwise(|| CallExpression::parse(parser, scanner, yield_arg, await_arg).and_then(|(ce, after_ce)| Ok((Box::new(Self::CallExpression(ce)), after_ce))))
            .otherwise(|| NewExpression::parse(parser, scanner, yield_arg, await_arg).and_then(|(ne, after_ne)| Ok((Box::new(Self::NewExpression(ne)), after_ne))))
    }
}

// OptionalExpression[Yield, Await] :
//      MemberExpression[?Yield, ?Await] OptionalChain[?Yield, ?Await]
//      CallExpression[?Yield, ?Await] OptionalChain[?Yield, ?Await]
//      OptionalExpression[?Yield, ?Await] OptionalChain[?Yield, ?Await]
#[derive(Debug)]
pub enum OptionalExpression {
    Member(Box<MemberExpression>, Box<OptionalChain>),
    Call(Box<CallExpression>, Box<OptionalChain>),
    Opt(Box<OptionalExpression>, Box<OptionalChain>),
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("OptionalExpression expected", scanner.line, scanner.column))
            .otherwise(|| {
                MemberExpression::parse(parser, scanner, yield_flag, await_flag).and_then(|(me, after_me)| {
                    let (oc, after_oc) = OptionalChain::parse(parser, after_me, yield_flag, await_flag)?;
                    Ok((Box::new(OptionalExpression::Member(me, oc)), after_oc))
                })
            })
            .otherwise(|| {
                CallExpression::parse(parser, scanner, yield_flag, await_flag).and_then(|(ce, after_ce)| {
                    let (oc, after_oc) = OptionalChain::parse(parser, after_ce, yield_flag, await_flag)?;
                    Ok((Box::new(OptionalExpression::Call(ce, oc)), after_oc))
                })
            })
            .and_then(|(opt, after_opt)| {
                let mut current = opt;
                let mut current_scanner = after_opt;
                loop {
                    match OptionalChain::parse(parser, current_scanner, yield_flag, await_flag) {
                        Ok((oc, after_oc)) => {
                            current = Box::new(OptionalExpression::Opt(current, oc));
                            current_scanner = after_oc;
                        }
                        Err(_) => {
                            break;
                        }
                    }
                }
                Ok((current, current_scanner))
            })
    }
}

// OptionalChain[Yield, Await] :
//      ?. Arguments[?Yield, ?Await]
//      ?. [ Expression[+In, ?Yield, ?Await] ]
//      ?. IdentifierName
//      ?. TemplateLiteral[?Yield, ?Await, +Tagged]
//      OptionalChain[?Yield, ?Await] Arguments[?Yield, ?Await]
//      OptionalChain[?Yield, ?Await] [ Expression[+In, ?Yield, ?Await] ]
//      OptionalChain[?Yield, ?Await] . IdentifierName
//      OptionalChain[?Yield, ?Await] TemplateLiteral[?Yield, ?Await, +Tagged]
#[derive(Debug)]
pub enum OptionalChain {
    Args(Box<Arguments>),
    Exp(Box<Expression>),
    Ident(IdentifierData),
    Template(Box<TemplateLiteral>),
    PlusArgs(Box<OptionalChain>, Box<Arguments>),
    PlusExp(Box<OptionalChain>, Box<Expression>),
    PlusIdent(Box<OptionalChain>, IdentifierData),
    PlusTemplate(Box<OptionalChain>, Box<TemplateLiteral>),
}

impl fmt::Display for OptionalChain {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OptionalChain::Args(node) => write!(f, "?. {}", node),
            OptionalChain::Exp(node) => write!(f, "?. [ {} ]", node),
            OptionalChain::Ident(node) => write!(f, "?. {}", node),
            OptionalChain::Template(node) => write!(f, "?. {}", node),
            OptionalChain::PlusArgs(lst, item) => write!(f, "{} {}", lst, item),
            OptionalChain::PlusExp(lst, item) => write!(f, "{} [ {} ]", lst, item),
            OptionalChain::PlusIdent(lst, item) => write!(f, "{} . {}", lst, item),
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
            OptionalChain::Ident(node) => pprint_token(writer, &format!("{}", node), &successive, Spot::Final),
            OptionalChain::Template(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
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
                pprint_token(writer, &format!("{}", item), &successive, Spot::Final)
            }
            OptionalChain::PlusTemplate(lst, item) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.pprint_with_leftpad(writer, &successive, Spot::Final)
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
                pprint_token(writer, "?.", &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalChain::Exp(node) => {
                pprint_token(writer, "?.", &successive, Spot::NotFinal)?;
                pprint_token(writer, "[", &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", &successive, Spot::Final)
            }
            OptionalChain::Ident(node) => {
                pprint_token(writer, "?.", &successive, Spot::NotFinal)?;
                pprint_token(writer, &format!("{}", node), &successive, Spot::Final)
            }
            OptionalChain::Template(node) => {
                pprint_token(writer, "?.", &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalChain::PlusArgs(lst, item) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            OptionalChain::PlusExp(lst, item) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "[", &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "]", &successive, Spot::Final)
            }
            OptionalChain::PlusIdent(lst, item) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ".", &successive, Spot::NotFinal)?;
                pprint_token(writer, &format!("{}", item), &successive, Spot::Final)
            }
            OptionalChain::PlusTemplate(lst, item) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl OptionalChain {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_opt = scan_for_punct(scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::QDot)?;
        let (mut current, mut current_scan) = Err(ParseError::new("‘(’, ‘[’, ‘`’, or an identifier name was expected (optional chaining failed)", after_opt.line, after_opt.column))
            .otherwise(|| {
                let (args, after_args) = Arguments::parse(parser, after_opt, yield_flag, await_flag)?;
                Ok((Box::new(OptionalChain::Args(args)), after_args))
            })
            .otherwise(|| {
                let (tl, after_tl) = TemplateLiteral::parse(parser, after_opt, yield_flag, await_flag, true)?;
                Ok((Box::new(OptionalChain::Template(tl)), after_tl))
            })
            .otherwise(|| {
                let after_lb = scan_for_punct(after_opt, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
                let (exp, after_exp) = Expression::parse(parser, after_lb, true, yield_flag, await_flag)?;
                let after_rb = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBracket)?;
                Ok((Box::new(OptionalChain::Exp(exp)), after_rb))
            })
            .otherwise(|| {
                let (id, after_id) = scan_for_identifiername(after_opt, parser.source, ScanGoal::InputElementDiv)?;
                Ok((Box::new(OptionalChain::Ident(id)), after_id))
            })?;

        loop {
            enum Follow {
                Args(Box<Arguments>),
                TLit(Box<TemplateLiteral>),
                Exp(Box<Expression>),
                Id(IdentifierData),
            }
            match Err(ParseError::new(String::new(), current_scan.line, current_scan.column))
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
                        Punctuator::Dot => {
                            let (id, after_id) = scan_for_identifiername(after_punct, parser.source, ScanGoal::InputElementDiv)?;
                            Ok((Follow::Id(id), after_id))
                        }
                        Punctuator::LeftBracket | _ => {
                            let (exp, after_exp) = Expression::parse(parser, after_punct, true, yield_flag, await_flag)?;
                            let after_rb = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBracket)?;
                            Ok((Follow::Exp(exp), after_rb))
                        }
                    }
                }) {
                Ok((follow, scan)) => {
                    current = Box::new(match follow {
                        Follow::Args(args) => OptionalChain::PlusArgs(current, args),
                        Follow::Id(id) => OptionalChain::PlusIdent(current, id),
                        Follow::Exp(exp) => OptionalChain::PlusExp(current, exp),
                        Follow::TLit(tl) => OptionalChain::PlusTemplate(current, tl),
                    });
                    current_scan = scan;
                }
                Err(_) => {
                    break;
                }
            }
        }
        Ok((current, current_scan))
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::pretty_check;

    // MEMBER EXPRESSION
    #[test]
    fn member_expression_test_primary_expression() {
        let (me, scanner) = check(MemberExpression::parse(&mut newparser("a"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(me.kind, MemberExpressionKind::PrimaryExpression(_)));
        // Excersize the Debug formatter, for code coverage
        format!("{:?}", me);
        pretty_check(&*me, "MemberExpression: a", vec!["PrimaryExpression: a"]);
        assert_eq!(me.is_function_definition(), false);
        assert_eq!(me.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn member_expression_test_meta_property() {
        let (me, scanner) = check(MemberExpression::parse(&mut newparser("new.target"), Scanner::new(), false, false));
        chk_scan(&scanner, 10);
        assert!(matches!(me.kind, MemberExpressionKind::MetaProperty(_)));
        // Excersize the Debug formatter, for code coverage
        format!("{:?}", me);
        pretty_check(&*me, "MemberExpression: new . target", vec!["MetaProperty: new . target"]);
        assert_eq!(me.is_function_definition(), false);
        assert_eq!(me.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn member_expression_test_super_property() {
        let (me, scanner) = check(MemberExpression::parse(&mut newparser("super.ior"), Scanner::new(), false, false));
        chk_scan(&scanner, 9);
        assert!(matches!(me.kind, MemberExpressionKind::SuperProperty(_)));
        // Excersize the Debug formatter, for code coverage
        format!("{:?}", me);
        pretty_check(&*me, "MemberExpression: super . ior", vec!["SuperProperty: super . ior"]);
        assert_eq!(me.is_function_definition(), false);
        assert_eq!(me.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn member_expression_test_new_me_args() {
        let (me, scanner) = check(MemberExpression::parse(&mut newparser("new shoes('red', 'leather')"), Scanner::new(), false, false));
        chk_scan(&scanner, 27);
        assert!(matches!(me.kind, MemberExpressionKind::NewArguments(..)));
        // Excersize the Debug formatter, for code coverage
        format!("{:?}", me);
        pretty_check(&*me, "MemberExpression: new shoes ( \"red\" , \"leather\" )", vec!["MemberExpression: shoes", "Arguments: ( \"red\" , \"leather\" )"]);
        assert_eq!(me.is_function_definition(), false);
        assert_eq!(me.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn member_expression_test_me_expression() {
        let (me, scanner) = check(MemberExpression::parse(&mut newparser("bill[a]"), Scanner::new(), false, false));
        chk_scan(&scanner, 7);
        assert!(matches!(me.kind, MemberExpressionKind::Expression(..)));
        // Excersize the Debug formatter, for code coverage
        format!("{:?}", me);
        pretty_check(&*me, "MemberExpression: bill [ a ]", vec!["MemberExpression: bill", "Expression: a"]);
        assert_eq!(me.is_function_definition(), false);
        assert_eq!(me.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn member_expression_test_me_ident() {
        let (me, scanner) = check(MemberExpression::parse(&mut newparser("alice.name"), Scanner::new(), false, false));
        chk_scan(&scanner, 10);
        assert!(matches!(me.kind, MemberExpressionKind::IdentifierName(..)));
        // Excersize the Debug formatter, for code coverage
        format!("{:?}", me);
        pretty_check(&*me, "MemberExpression: alice . name", vec!["MemberExpression: alice"]);
        assert_eq!(me.is_function_definition(), false);
        assert_eq!(me.assignment_target_type(), ATTKind::Simple);
    }

    // SUPER PROPERTY
    #[test]
    fn super_property_test_expression() {
        let (sp, scanner) = check(SuperProperty::parse(&mut newparser("super[a]"), Scanner::new(), false, false));
        chk_scan(&scanner, 8);
        assert!(matches!(sp.kind, SuperPropertyKind::Expression(_)));
        // Excersize the Debug formatter, for code coverage
        format!("{:?}", sp);
        pretty_check(&*sp, "SuperProperty: super [ a ]", vec!["Expression: a"]);
    }
    #[test]
    fn super_property_test_ident() {
        let (sp, scanner) = check(SuperProperty::parse(&mut newparser("super.bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 9);
        assert!(matches!(sp.kind, SuperPropertyKind::IdentifierName(_)));
        // Excersize the Debug formatter, for code coverage
        format!("{:?}", sp);
        pretty_check(&*sp, "SuperProperty: super . bob", vec![]);
    }
    #[test]
    fn super_property_test_nomatch() {
        check_err(SuperProperty::parse(&mut newparser("silly"), Scanner::new(), false, false), "‘super’ expected", 1, 1);
    }
    #[test]
    fn super_property_test_bad_ident() {
        let r = SuperProperty::parse(&mut newparser("super.**"), Scanner::new(), false, false);
        check_err(r, "IdentifierName expected", 1, 7);
    }
    #[test]
    fn super_property_test_bad_expression() {
        let r = SuperProperty::parse(&mut newparser("super[while]"), Scanner::new(), false, false);
        check_err(r, "Expression expected", 1, 7);
    }
    #[test]
    fn super_property_test_incomplete_expression() {
        let r = SuperProperty::parse(&mut newparser("super[99"), Scanner::new(), false, false);
        check_err(r, "‘]’ expected", 1, 9);
    }
    #[test]
    fn super_property_test_bad_following_token() {
        check_err(SuperProperty::parse(&mut newparser("super duper"), Scanner::new(), false, false), "One of [‘.’, ‘[’] expected", 1, 6);
    }

    // META PROPERTY
    #[test]
    fn meta_property_test_newtarget() {
        let (mp, scanner) = check(MetaProperty::parse(&mut newparser("new.target"), Scanner::new()));
        chk_scan(&scanner, 10);
        assert!(matches!(mp.kind, MetaPropertyKind::NewTarget));
        format!("{:?}", mp);
        pretty_check(&*mp, "MetaProperty: new . target", vec![]);
    }
    #[test]
    fn meta_property_test_importmeta() {
        let (mp, scanner) = check(MetaProperty::parse(&mut newparser("import.meta"), Scanner::new()));
        chk_scan(&scanner, 11);
        assert!(matches!(mp.kind, MetaPropertyKind::ImportMeta));
        format!("{:?}", mp);
        pretty_check(&*mp, "MetaProperty: import . meta", vec![]);
    }
    #[test]
    fn meta_property_test_nomatch_01() {
        check_err(MetaProperty::parse(&mut newparser("silly"), Scanner::new()), "One of [‘new’, ‘import’] expected", 1, 1);
    }
    #[test]
    fn meta_property_test_nomatch_02() {
        check_err(MetaProperty::parse(&mut newparser("new silly"), Scanner::new()), "‘.’ expected", 1, 4);
    }
    #[test]
    fn meta_property_test_nomatch_03() {
        check_err(MetaProperty::parse(&mut newparser("new.silly"), Scanner::new()), "‘target’ expected", 1, 5);
    }
    #[test]
    fn meta_property_test_nomatch_04() {
        check_err(MetaProperty::parse(&mut newparser("import silly"), Scanner::new()), "‘.’ expected", 1, 7);
    }
    #[test]
    fn meta_property_test_nomatch_05() {
        check_err(MetaProperty::parse(&mut newparser("import.silly"), Scanner::new()), "‘meta’ expected", 1, 8);
    }

    // ARGUMENTS
    #[test]
    fn arguments_test_onlyparens() {
        let (args, scanner) = check(Arguments::parse(&mut newparser("()"), Scanner::new(), false, false));
        chk_scan(&scanner, 2);
        assert!(matches!(args.kind, ArgumentsKind::Empty));
        format!("{:?}", args);
        pretty_check(&*args, "Arguments: ( )", vec![]);
    }
    #[test]
    fn arguments_test_trailing_comma() {
        let (args, scanner) = check(Arguments::parse(&mut newparser("(a,)"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(args.kind, ArgumentsKind::ArgumentListComma(_)));
        format!("{:?}", args);
        pretty_check(&*args, "Arguments: ( a , )", vec!["ArgumentList: a"]);
    }
    #[test]
    fn arguments_test_arglist() {
        let (args, scanner) = check(Arguments::parse(&mut newparser("(a,b)"), Scanner::new(), false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(args.kind, ArgumentsKind::ArgumentList(_)));
        format!("{:?}", args);
        pretty_check(&*args, "Arguments: ( a , b )", vec!["ArgumentList: a , b"]);
    }
    #[test]
    fn arguments_test_nomatch() {
        check_err(Arguments::parse(&mut newparser("**"), Scanner::new(), false, false), "‘(’ expected", 1, 1);
    }
    #[test]
    fn arguments_test_unclosed_01() {
        check_err(Arguments::parse(&mut newparser("("), Scanner::new(), false, false), "‘)’ expected", 1, 2);
    }
    #[test]
    fn arguments_test_unclosed_02() {
        check_err(Arguments::parse(&mut newparser("(88"), Scanner::new(), false, false), "One of [‘,’, ‘)’] expected", 1, 4);
    }
    #[test]
    fn arguments_test_unclosed_03() {
        check_err(Arguments::parse(&mut newparser("(91,"), Scanner::new(), false, false), "‘)’ expected", 1, 5);
    }

    // ARGUMENT LIST
    #[test]
    fn argument_list_test_ae() {
        let (al, scanner) = check(ArgumentList::parse(&mut newparser("aba"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(al.kind, ArgumentListKind::AssignmentExpression(_)));
        format!("{:?}", al);
        pretty_check(&*al, "ArgumentList: aba", vec!["AssignmentExpression: aba"]);
    }
    #[test]
    fn argument_list_test_dots_ae() {
        let (al, scanner) = check(ArgumentList::parse(&mut newparser("...aba"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(al.kind, ArgumentListKind::DotsAssignmentExpression(_)));
        format!("{:?}", al);
        pretty_check(&*al, "ArgumentList: ... aba", vec!["AssignmentExpression: aba"]);
    }
    #[test]
    fn argument_list_test_al_ae() {
        let (al, scanner) = check(ArgumentList::parse(&mut newparser("ab,aba"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(al.kind, ArgumentListKind::ArgumentListAssignmentExpression(..)));
        format!("{:?}", al);
        pretty_check(&*al, "ArgumentList: ab , aba", vec!["ArgumentList: ab", "AssignmentExpression: aba"]);
    }
    #[test]
    fn argument_list_test_al_dots_ae() {
        let (al, scanner) = check(ArgumentList::parse(&mut newparser("ab,...aba"), Scanner::new(), false, false));
        chk_scan(&scanner, 9);
        assert!(matches!(al.kind, ArgumentListKind::ArgumentListDotsAssignmentExpression(..)));
        format!("{:?}", al);
        pretty_check(&*al, "ArgumentList: ab , ... aba", vec!["ArgumentList: ab", "AssignmentExpression: aba"]);
    }
    #[test]
    fn argument_list_test_nomatch() {
        check_err(ArgumentList::parse(&mut newparser("**"), Scanner::new(), false, false), "AssignmentExpression expected", 1, 1);
    }
    #[test]
    fn argument_list_test_dotsonly() {
        check_err(ArgumentList::parse(&mut newparser("..."), Scanner::new(), false, false), "AssignmentExpression expected", 1, 4);
    }
    #[test]
    fn argument_list_test_dots_term() {
        let (al, scanner) = check(ArgumentList::parse(&mut newparser("10,..."), Scanner::new(), false, false));
        chk_scan(&scanner, 2);
        assert!(matches!(al.kind, ArgumentListKind::AssignmentExpression(_)));
    }
    #[test]
    fn argument_list_test_commas() {
        let (al, scanner) = check(ArgumentList::parse(&mut newparser("10,,10"), Scanner::new(), false, false));
        chk_scan(&scanner, 2);
        assert!(matches!(al.kind, ArgumentListKind::AssignmentExpression(_)));
    }

    // NEW EXPRESSION
    #[test]
    fn new_expression_test_me() {
        let (ne, scanner) = check(NewExpression::parse(&mut newparser("true"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(ne.kind, NewExpressionKind::MemberExpression(_)));
        format!("{:?}", ne);
        pretty_check(&*ne, "NewExpression: true", vec!["MemberExpression: true"]);
        assert_eq!(ne.is_function_definition(), false);
        assert_eq!(ne.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn new_expression_test_new() {
        let (ne, scanner) = check(NewExpression::parse(&mut newparser("new bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 7);
        assert!(matches!(ne.kind, NewExpressionKind::NewExpression(_)));
        format!("{:?}", ne);
        pretty_check(&*ne, "NewExpression: new bob", vec!["NewExpression: bob"]);
        assert_eq!(ne.is_function_definition(), false);
        assert_eq!(ne.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn new_expression_test_nomatch() {
        check_err(NewExpression::parse(&mut newparser("**"), Scanner::new(), false, false), "‘new’ or MemberExpression expected", 1, 1);
    }
    #[test]
    fn new_expression_test_chopped() {
        check_err(NewExpression::parse(&mut newparser("new"), Scanner::new(), false, false), "‘new’ or MemberExpression expected", 1, 4);
    }

    // CALL MEMBER EXPRESSION
    #[test]
    fn call_member_expression_test_me_args() {
        let (cme, scanner) = check(CallMemberExpression::parse(&mut newparser("a()"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        format!("{:?}", cme);
        pretty_check(&*cme, "CallMemberExpression: a ( )", vec!["MemberExpression: a", "Arguments: ( )"]);
    }
    #[test]
    fn call_member_expression_test_nomatch() {
        check_err(CallMemberExpression::parse(&mut newparser("++"), Scanner::new(), false, false), "MemberExpression expected", 1, 1);
    }
    #[test]
    fn call_member_expression_test_incomplete() {
        check_err(CallMemberExpression::parse(&mut newparser("pop"), Scanner::new(), false, false), "‘(’ expected", 1, 4);
    }

    // SUPER CALL
    #[test]
    fn super_call_test_args() {
        let (sc, scanner) = check(SuperCall::parse(&mut newparser("super()"), Scanner::new(), false, false));
        chk_scan(&scanner, 7);
        assert!(matches!(sc.arguments.kind, ArgumentsKind::Empty));
        format!("{:?}", sc);
        pretty_check(&*sc, "SuperCall: super ( )", vec!["Arguments: ( )"]);
    }
    #[test]
    fn super_call_test_nomatch() {
        check_err(SuperCall::parse(&mut newparser("++"), Scanner::new(), false, false), "‘super’ expected", 1, 1);
    }
    #[test]
    fn super_call_test_incomplete() {
        check_err(SuperCall::parse(&mut newparser("super"), Scanner::new(), false, false), "‘(’ expected", 1, 6);
    }

    // IMPORT CALL
    #[test]
    fn import_call_test_ae() {
        let (ic, scanner) = check(ImportCall::parse(&mut newparser("import(bob)"), Scanner::new(), false, false));
        chk_scan(&scanner, 11);
        format!("{:?}", ic);
        pretty_check(&*ic, "ImportCall: import ( bob )", vec!["AssignmentExpression: bob"]);
    }
    #[test]
    fn import_call_test_nomatch() {
        check_err(ImportCall::parse(&mut newparser("++"), Scanner::new(), false, false), "‘import’ expected", 1, 1);
    }
    #[test]
    fn import_call_test_incomplete() {
        check_err(ImportCall::parse(&mut newparser("import"), Scanner::new(), false, false), "‘(’ expected", 1, 7);
    }
    #[test]
    fn import_call_test_incomplete2() {
        check_err(ImportCall::parse(&mut newparser("import("), Scanner::new(), false, false), "AssignmentExpression expected", 1, 8);
    }
    #[test]
    fn import_call_test_incomplete3() {
        check_err(ImportCall::parse(&mut newparser("import(bob"), Scanner::new(), false, false), "‘)’ expected", 1, 11);
    }

    // CALL EXPRESSION
    #[test]
    fn call_expression_test_me_args() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("a()"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(ce.kind, CallExpressionKind::CallMemberExpression(_)));
        format!("{:?}", ce);
        pretty_check(&*ce, "CallExpression: a ( )", vec!["CallMemberExpression: a ( )"]);
        assert_eq!(ce.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn call_expression_test_super() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("super()"), Scanner::new(), false, false));
        chk_scan(&scanner, 7);
        assert!(matches!(ce.kind, CallExpressionKind::SuperCall(_)));
        format!("{:?}", ce);
        pretty_check(&*ce, "CallExpression: super ( )", vec!["SuperCall: super ( )"]);
        assert_eq!(ce.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn call_expression_test_import() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("import(pop)"), Scanner::new(), false, false));
        chk_scan(&scanner, 11);
        assert!(matches!(ce.kind, CallExpressionKind::ImportCall(_)));
        format!("{:?}", ce);
        pretty_check(&*ce, "CallExpression: import ( pop )", vec!["ImportCall: import ( pop )"]);
        assert_eq!(ce.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn call_expression_test_ce_args() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)(snap)(10)(20)"), Scanner::new(), false, false));
        chk_scan(&scanner, 23);
        assert!(matches!(ce.kind, CallExpressionKind::CallExpressionArguments(..)));
        format!("{:?}", ce);
        pretty_check(&*ce, "CallExpression: blue ( pop ) ( snap ) ( 10 ) ( 20 )", vec!["CallExpression: blue ( pop ) ( snap ) ( 10 )", "Arguments: ( 20 )"]);
        assert_eq!(ce.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn call_expression_test_ce_args2() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)(snap)(10)(++)"), Scanner::new(), false, false));
        chk_scan(&scanner, 19);
        assert!(matches!(ce.kind, CallExpressionKind::CallExpressionArguments(..)));
        format!("{:?}", ce);
    }
    #[test]
    fn call_expression_test_ce_exp() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)[snap]"), Scanner::new(), false, false));
        chk_scan(&scanner, 15);
        assert!(matches!(ce.kind, CallExpressionKind::CallExpressionExpression(..)));
        format!("{:?}", ce);
        pretty_check(&*ce, "CallExpression: blue ( pop ) [ snap ]", vec!["CallExpression: blue ( pop )", "Expression: snap"]);
        assert_eq!(ce.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn call_expression_test_ce_ident() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop).snap"), Scanner::new(), false, false));
        chk_scan(&scanner, 14);
        assert!(matches!(ce.kind, CallExpressionKind::CallExpressionIdentifierName(..)));
        format!("{:?}", ce);
        pretty_check(&*ce, "CallExpression: blue ( pop ) . snap", vec!["CallExpression: blue ( pop )"]);
        assert_eq!(ce.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn call_expression_test_nomatch() {
        check_err(CallExpression::parse(&mut newparser(""), Scanner::new(), false, false), "CallExpression expected", 1, 1);
    }
    #[test]
    fn call_expression_test_incomplete_01() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)["), Scanner::new(), false, false));
        chk_scan(&scanner, 9);
        assert!(matches!(ce.kind, CallExpressionKind::CallMemberExpression(_)));
    }
    #[test]
    fn call_expression_test_incomplete_02() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)[99"), Scanner::new(), false, false));
        chk_scan(&scanner, 9);
        assert!(matches!(ce.kind, CallExpressionKind::CallMemberExpression(_)));
    }
    #[test]
    fn call_expression_test_incomplete_03() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)."), Scanner::new(), false, false));
        chk_scan(&scanner, 9);
        assert!(matches!(ce.kind, CallExpressionKind::CallMemberExpression(_)));
    }

    // LEFT-HAND-SIDE EXPRESSION
    #[test]
    fn left_hand_side_expression_test_01() {
        let (lhs, scanner) = check(LeftHandSideExpression::parse(&mut newparser("a"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(*lhs, LeftHandSideExpression::NewExpression(_)));
        format!("{:?}", lhs);
        pretty_check(&*lhs, "LeftHandSideExpression: a", vec!["NewExpression: a"]);
        assert_eq!(lhs.is_function_definition(), false);
        assert_eq!(lhs.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn left_hand_side_expression_test_02() {
        let (lhs, scanner) = check(LeftHandSideExpression::parse(&mut newparser("a()"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*lhs, LeftHandSideExpression::CallExpression(_)));
        pretty_check(&*lhs, "LeftHandSideExpression: a ( )", vec!["CallExpression: a ( )"]);
        assert_eq!(lhs.is_function_definition(), false);
        assert_eq!(lhs.assignment_target_type(), ATTKind::Invalid);
    }
}
