use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::assignment_operators::AssignmentExpression;
use super::identifiers::IdentifierNameToken;
use super::primary_expressions::PrimaryExpression;
use super::primary_expressions::TemplateLiteral;
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};
use crate::Expression;

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
pub struct MemberExpressionExpression {
    member_expression: Box<MemberExpression>,
    expression: Box<Expression>,
}

#[derive(Debug)]
pub struct MemberExpressionIdentifierName {
    member_expression: Box<MemberExpression>,
    identifier_name: Box<IdentifierNameToken>,
}

#[derive(Debug)]
pub struct MemberExpressionTemplateLiteral {
    member_expression: Box<MemberExpression>,
    template_literal: Box<TemplateLiteral>,
}

#[derive(Debug)]
pub struct NewMemberExpressionArguments {
    member_expression: Box<MemberExpression>,
    arguments: Box<Arguments>,
}

#[derive(Debug)]
pub enum MemberExpressionKind {
    PrimaryExpression(Box<PrimaryExpression>),
    Expression(MemberExpressionExpression),
    IdentifierName(MemberExpressionIdentifierName),
    TemplateLiteral(MemberExpressionTemplateLiteral),
    SuperProperty(Box<SuperProperty>),
    MetaProperty(Box<MetaProperty>),
    NewArguments(NewMemberExpressionArguments),
}

#[derive(Debug)]
pub struct MemberExpression {
    kind: MemberExpressionKind,
}

impl fmt::Display for MemberExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            MemberExpressionKind::PrimaryExpression(boxed) => write!(f, "{}", boxed),
            MemberExpressionKind::Expression(mee) => {
                write!(f, "{} [ {} ]", mee.member_expression, mee.expression)
            }
            MemberExpressionKind::IdentifierName(mein) => write!(f, "{} . {}", mein.member_expression, mein.identifier_name),
            MemberExpressionKind::TemplateLiteral(metl) => write!(f, "{} {}", metl.member_expression, metl.template_literal),
            MemberExpressionKind::SuperProperty(boxed) => write!(f, "{}", boxed),
            MemberExpressionKind::MetaProperty(boxed) => write!(f, "{}", boxed),
            MemberExpressionKind::NewArguments(nmea) => write!(f, "new {} {}", nmea.member_expression, nmea.arguments),
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
            MemberExpressionKind::Expression(mee) => {
                mee.member_expression.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                mee.expression.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            MemberExpressionKind::IdentifierName(mein) => mein.member_expression.pprint_with_leftpad(writer, &successive, Spot::Final),
            MemberExpressionKind::TemplateLiteral(metl) => {
                metl.member_expression.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                metl.template_literal.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            MemberExpressionKind::SuperProperty(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            MemberExpressionKind::MetaProperty(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            MemberExpressionKind::NewArguments(nmea) => {
                nmea.member_expression.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                nmea.arguments.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IsFunctionDefinition for MemberExpression {
    fn is_function_definition(&self) -> bool {
        match &self.kind {
            MemberExpressionKind::PrimaryExpression(boxed) => boxed.is_function_definition(),
            MemberExpressionKind::Expression(_)
            | MemberExpressionKind::IdentifierName(_)
            | MemberExpressionKind::TemplateLiteral(_)
            | MemberExpressionKind::SuperProperty(_)
            | MemberExpressionKind::MetaProperty(_)
            | MemberExpressionKind::NewArguments(_) => false,
        }
    }
}

impl AssignmentTargetType for MemberExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match &self.kind {
            MemberExpressionKind::PrimaryExpression(boxed) => boxed.assignment_target_type(),
            MemberExpressionKind::Expression(_) => ATTKind::Simple,
            MemberExpressionKind::IdentifierName(_) => ATTKind::Simple,
            MemberExpressionKind::TemplateLiteral(_) => ATTKind::Invalid,
            MemberExpressionKind::SuperProperty(_) => ATTKind::Simple,
            MemberExpressionKind::MetaProperty(boxed) => boxed.assignment_target_type(),
            MemberExpressionKind::NewArguments(_) => ATTKind::Invalid,
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

impl ToMemberExpressionKind for NewMemberExpressionArguments {
    fn to_member_expression_kind(node: Box<Self>) -> MemberExpressionKind {
        MemberExpressionKind::NewArguments(*node)
    }
}

fn me_boxer<T>(opt: Option<(Box<T>, Scanner)>) -> Result<Option<(Box<MemberExpression>, Scanner)>, String>
where
    T: ToMemberExpressionKind,
{
    Ok(opt.map(|(node, scanner)| {
        (
            Box::new(MemberExpression {
                kind: T::to_member_expression_kind(node),
            }),
            scanner,
        )
    }))
}

fn or_me_kind<F, T>(opt: Option<(Box<MemberExpression>, Scanner)>, parser: &mut Parser, parse_func: F) -> Result<Option<(Box<MemberExpression>, Scanner)>, String>
where
    F: FnOnce(&mut Parser) -> Result<Option<(Box<T>, Scanner)>, String>,
    T: ToMemberExpressionKind,
{
    opt.map_or_else(|| parse_func(parser).and_then(me_boxer), rewrap)
}

fn member_expression_head_recursive(parser: &mut Parser, yield_flag: bool, await_flag: bool, pair: (Box<MemberExpression>, Scanner)) -> Result<Option<(Box<MemberExpression>, Scanner)>, String> {
    let (mut current_me, mut after_scan) = pair;
    loop {
        let (tok, after) = scanner::scan_token(&after_scan, parser.source, scanner::ScanGoal::InputElementRegExp);
        match tok {
            scanner::Token::Dot => {
                let token_after_dot = scanner::scan_token(&after, parser.source, scanner::ScanGoal::InputElementRegExp);
                if let (scanner::Token::Identifier(id), after_id) = token_after_dot {
                    let me = Box::new(MemberExpression {
                        kind: MemberExpressionKind::IdentifierName(MemberExpressionIdentifierName {
                            member_expression: current_me,
                            identifier_name: Box::new(IdentifierNameToken {
                                value: scanner::Token::Identifier(id),
                            }),
                        }),
                    });
                    current_me = me;
                    after_scan = after_id;
                } else {
                    return Err(format!("Expected IdentifierName after ‘.’."));
                }
            }
            scanner::Token::LeftBracket => {
                let potential_expression = Expression::parse(parser, after, true, yield_flag, await_flag)?;
                match potential_expression {
                    None => {
                        return Err(format!("Expect Expression after ‘[’."));
                    }
                    Some((expression, after_exp)) => {
                        let after_exp = scanner::scan_token(&after_exp, parser.source, scanner::ScanGoal::InputElementRegExp);
                        match after_exp {
                            (scanner::Token::RightBracket, scanner) => {
                                let me = Box::new(MemberExpression {
                                    kind: MemberExpressionKind::Expression(MemberExpressionExpression {
                                        member_expression: current_me,
                                        expression: expression,
                                    }),
                                });
                                current_me = me;
                                after_scan = scanner;
                            }
                            _ => {
                                return Err(format!("Expect ‘]’ after expression."));
                            }
                        }
                    }
                }
            }
            _ => {
                break;
            }
        }
    }
    Ok(Some((current_me, after_scan)))
}

impl MemberExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        Ok(None)
            // First: All the non-head-recursive productions
            .and_then(|opt| or_me_kind(opt, parser, |p| PrimaryExpression::parse(p, scanner, yield_flag, await_flag)))
            .and_then(|opt| or_me_kind(opt, parser, |p| SuperProperty::parse(p, scanner, yield_flag, await_flag)))
            .and_then(|opt| or_me_kind(opt, parser, |p| MetaProperty::parse(p, scanner)))
            .and_then(|opt| or_me_kind(opt, parser, |p| Self::new_memberexpression_arguments(p, scanner, yield_flag, await_flag)))
            // And then all the head-recursive productions.
            .and_then(|opt| opt.map_or(Ok(None), |x| member_expression_head_recursive(parser, yield_flag, await_flag, x)))
    }
    fn new_memberexpression_arguments(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<NewMemberExpressionArguments>, Scanner)>, String> {
        let (token, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        match token {
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::New) => MemberExpression::parse(parser, scanner, yield_flag, await_flag).and_then(|opt| {
                opt.map_or(Ok(None), |(me, scan)| {
                    Arguments::parse(parser, scan, yield_flag, await_flag).and_then(|opt| {
                        opt.map_or(Ok(None), |(args, scan)| {
                            Ok(Some((
                                Box::new(NewMemberExpressionArguments {
                                    member_expression: me,
                                    arguments: args,
                                }),
                                scan,
                            )))
                        })
                    })
                })
            }),
            _ => Ok(None),
        }
    }
}

#[derive(Debug)]
pub enum SuperPropertyKind {
    Expression(Box<Expression>),
    IdentifierName(Box<IdentifierNameToken>),
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
}

impl SuperProperty {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<SuperProperty>, Scanner)>, String> {
        Ok(scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)).and_then(|(token, scanner)| match token {
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Super) => {
                Ok(scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)).and_then(|(token, scanner)| match token {
                    scanner::Token::LeftBracket => Expression::parse(parser, scanner, true, yield_flag, await_flag).and_then(|opt| {
                        opt.map_or_else(
                            || Err(String::from("‘super[’ must be followed by an Expression")),
                            |(exp_boxed, scanner)| {
                                Ok(scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)).and_then(|(token, scanner)| match token {
                                    scanner::Token::RightBracket => Ok(Some((
                                        Box::new(SuperProperty {
                                            kind: SuperPropertyKind::Expression(exp_boxed),
                                        }),
                                        scanner,
                                    ))),
                                    _ => Err(String::from("‘super[ Expression’ must be closed by a ‘]’.")),
                                })
                            },
                        )
                    }),
                    scanner::Token::Dot => Ok(scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)).and_then(|(token, scanner)| match token {
                        scanner::Token::Identifier(id) => Ok(Some((
                            Box::new(SuperProperty {
                                kind: SuperPropertyKind::IdentifierName(Box::new(IdentifierNameToken {
                                    value: scanner::Token::Identifier(id),
                                })),
                            }),
                            scanner,
                        ))),
                        _ => Err(String::from("‘super.’ must be followed by an IdentifierName")),
                    }),
                    _ => Ok(None),
                })
            }
            _ => Ok(None),
        })
    }
}

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
}

impl AssignmentTargetType for MetaProperty {
    fn assignment_target_type(&self) -> ATTKind {
        ATTKind::Invalid
    }
}

fn dot_token(parser: &mut Parser, scanner: Scanner, kwd: scanner::Keyword, kind: MetaPropertyKind) -> Result<Option<(Box<MetaProperty>, Scanner)>, String> {
    let (token, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
    match token {
        scanner::Token::Dot => {
            let (token2, scanner2) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
            match token2 {
                scanner::Token::Identifier(id) if id.keyword_id == Some(kwd) => Ok(Some((Box::new(MetaProperty { kind }), scanner2))),
                _ => Ok(None),
            }
        }
        _ => Ok(None),
    }
}

impl MetaProperty {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> Result<Option<(Box<MetaProperty>, Scanner)>, String> {
        Ok(scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)).and_then(|(token, scanner)| match token {
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::New) => dot_token(parser, scanner, scanner::Keyword::Target, MetaPropertyKind::NewTarget),
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Import) => dot_token(parser, scanner, scanner::Keyword::Meta, MetaPropertyKind::ImportMeta),
            _ => Ok(None),
        })
    }
}

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
}

impl Arguments {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Arguments>, Scanner)>, String> {
        Ok(scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)).and_then(|(token, scanner)| match token {
            scanner::Token::LeftParen => ArgumentList::parse(parser, scanner, yield_flag, await_flag).and_then(|opt| {
                opt.map_or_else(
                    || {
                        Ok(scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)).and_then(|(token, scanner)| match token {
                            scanner::Token::RightParen => Ok(Some((Box::new(Arguments { kind: ArgumentsKind::Empty }), scanner))),
                            _ => Ok(None),
                        })
                    },
                    |(arglist_boxed, scanner)| {
                        Ok(scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)).and_then(|(token, scanner)| match token {
                            scanner::Token::Comma => Ok(scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)).and_then(|(token, scanner)| match token {
                                scanner::Token::RightParen => Ok(Some((
                                    Box::new(Arguments {
                                        kind: ArgumentsKind::ArgumentListComma(arglist_boxed),
                                    }),
                                    scanner,
                                ))),
                                _ => Ok(None),
                            }),
                            scanner::Token::RightParen => Ok(Some((
                                Box::new(Arguments {
                                    kind: ArgumentsKind::ArgumentList(arglist_boxed),
                                }),
                                scanner,
                            ))),
                            _ => Ok(None),
                        })
                    },
                )
            }),
            _ => Ok(None),
        })
    }
}

#[derive(Debug)]
pub struct ArgumentListAssignmentExpression {
    argument_list: Box<ArgumentList>,
    assignment_expression: Box<AssignmentExpression>,
}

#[derive(Debug)]
pub enum ArgumentListKind {
    AssignmentExpression(Box<AssignmentExpression>),
    DotsAssignmentExpression(Box<AssignmentExpression>),
    ArgumentListAssignmentExpression(ArgumentListAssignmentExpression),
    ArgumentListDotsAssignmentExpression(ArgumentListAssignmentExpression),
}

impl ArgumentListKind {
    // Package the results of a successful assignment_expression into an ArgumentListKind::AssignmentExpression.
    fn ae_bundle(pair: (Box<AssignmentExpression>, Scanner)) -> Result<Option<(Self, Scanner)>, String> {
        let (ae_boxed, scanner) = pair;
        Ok(Some((Self::AssignmentExpression(ae_boxed), scanner)))
    }

    // Package the results of assignment_expression into an ArgumentListKind (or pass along a None)
    fn ae_package(opt: Option<(Box<AssignmentExpression>, Scanner)>) -> Result<Option<(Self, Scanner)>, String> {
        opt.map_or(Ok(None), Self::ae_bundle)
    }

    // Parse the production
    //      ArgumentList : AssignmentExpression
    // returning one of:
    //    * an ArgumentListKind that contains all the relevant info
    //    * None, indicating that no AssignmentExpression was detected
    //    * an Err with a human readable message about what went wrong
    pub fn parse_assignment_expression(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Self, Scanner)>, String> {
        AssignmentExpression::parse(parser, scanner, true, yield_flag, await_flag).and_then(Self::ae_package)
    }

    // Parse the production
    //      ArgumentList : ... AssignmentExpression
    // returning one of:
    //    * an ArgumentListKind that contains all the relevant info
    //    * None, indicating that no ... was detected
    //    * an Err with a human readable message about what went wrong
    // Note: It is an error for ... to appear during an ArgumentList parse without being followed by an AssignmentExpression.
    pub fn parse_dots_assignment_expression(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Self, Scanner)>, String> {
        // Get the next token
        let (token, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        match token {
            scanner::Token::Ellipsis => {
                // It was an ellipsis, so now try to get the AssignmentExpression
                let potential_ae = AssignmentExpression::parse(parser, scanner, true, yield_flag, await_flag)?;
                match potential_ae {
                    None => {
                        // No AssignmentExpression after an ellipsis is an error.
                        Err(String::from("... must be followed by an AssignmentExpression."))
                    }
                    Some((boxed_ae, scanner)) => {
                        // Successful parsing of ... AssignmentExpression
                        Ok(Some((Self::DotsAssignmentExpression(boxed_ae), scanner)))
                    }
                }
            }
            _ => {
                // No ellipsis, so return None to indicate this production was not detected
                Ok(None)
            }
        }
    }

    // Parse the production
    //      ArgumentList : ArgumentList , AssignmentExpression
    // ASSUMING: that the first ArgumentList has already been parsed. (I.e: just do the part starting with the comma.)
    // returning one of:
    //    * a pair: (Box<AssignmentExpression>, Scanner)
    //    * None, indicating that no ',' was detected
    //    * an Err with a human readable message about what went wrong
    fn parse_al_ae(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<AssignmentExpression>, Scanner)>, String> {
        let (token, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        match token {
            scanner::Token::Comma => AssignmentExpression::parse(parser, scanner, true, yield_flag, await_flag),
            _ => Ok(None),
        }
    }

    // Parse the production
    //      ArgumentList : ArgumentList , ... AssignmentExpression
    // ASSUMING: that the first ArgumentList has already been parsed. (I.e: just do the part starting with the comma.)
    // returning one of:
    //    * a pair: (Box<AssignmentExpression>, Scanner)
    //    * None, indicating that neither a ',' nor a '...' was detected
    //    * an Err with a human readable message about what went wrong
    fn parse_al_dots_ae(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<AssignmentExpression>, Scanner)>, String> {
        let (token, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        match token {
            scanner::Token::Comma => {
                let (token, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
                match token {
                    scanner::Token::Ellipsis => AssignmentExpression::parse(parser, scanner, true, yield_flag, await_flag),
                    _ => Ok(None),
                }
            }
            _ => Ok(None),
        }
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
            ArgumentListKind::ArgumentListAssignmentExpression(alae) => write!(f, "{} , {}", alae.argument_list, alae.assignment_expression),
            ArgumentListKind::ArgumentListDotsAssignmentExpression(aldae) => write!(f, "{} , ... {}", aldae.argument_list, aldae.assignment_expression),
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
            ArgumentListKind::ArgumentListAssignmentExpression(alae) | ArgumentListKind::ArgumentListDotsAssignmentExpression(alae) => {
                alae.argument_list.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                alae.assignment_expression.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl ArgumentList {
    fn boxer(kind: ArgumentListKind) -> Box<Self> {
        Box::new(Self { kind })
    }
    fn alae_boxer(arglist: Box<Self>, ae: Box<AssignmentExpression>) -> Box<Self> {
        Self::boxer(ArgumentListKind::ArgumentListAssignmentExpression(ArgumentListAssignmentExpression {
            argument_list: arglist,
            assignment_expression: ae,
        }))
    }
    fn aldotsae_boxer(arglist: Box<Self>, ae: Box<AssignmentExpression>) -> Box<Self> {
        Self::boxer(ArgumentListKind::ArgumentListDotsAssignmentExpression(ArgumentListAssignmentExpression {
            argument_list: arglist,
            assignment_expression: ae,
        }))
    }
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let mut k = ArgumentListKind::parse_assignment_expression(parser, scanner, yield_flag, await_flag)?;
        if k.is_none() {
            k = ArgumentListKind::parse_dots_assignment_expression(parser, scanner, yield_flag, await_flag)?;
            if k.is_none() {
                return Ok(None);
            }
        }
        let (kind, mut top_scanner) = k.unwrap();
        let mut top_box = Box::new(Self { kind });
        loop {
            let pot_alae = ArgumentListKind::parse_al_ae(parser, top_scanner, yield_flag, await_flag)?;
            if let Some((boxed_ae, scanner)) = pot_alae {
                top_box = Self::alae_boxer(top_box, boxed_ae);
                top_scanner = scanner;
            } else {
                let pot_al_dots_ae = ArgumentListKind::parse_al_dots_ae(parser, top_scanner, yield_flag, await_flag)?;
                if let Some((boxed_ae, scanner)) = pot_al_dots_ae {
                    top_box = Self::aldotsae_boxer(top_box, boxed_ae);
                    top_scanner = scanner;
                } else {
                    break;
                }
            }
        }
        Ok(Some((top_box, top_scanner)))
    }
}

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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<NewExpression>, Scanner)>, String> {
        let pot_me = MemberExpression::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_me {
            None => {
                let (token, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
                match token {
                    scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::New) => {
                        let pot_ne = Self::parse(parser, scanner, yield_flag, await_flag)?;
                        match pot_ne {
                            None => Ok(None),
                            Some((ne_boxed, scanner)) => Ok(Some((
                                Box::new(NewExpression {
                                    kind: NewExpressionKind::NewExpression(ne_boxed),
                                }),
                                scanner,
                            ))),
                        }
                    }
                    _ => Ok(None),
                }
            }
            Some((me_boxed, scanner)) => Ok(Some((
                Box::new(NewExpression {
                    kind: NewExpressionKind::MemberExpression(me_boxed),
                }),
                scanner,
            ))),
        }
    }
}

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
}

impl CallMemberExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<CallMemberExpression>, Scanner)>, String> {
        let pot_me = MemberExpression::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_me {
            None => Ok(None),
            Some((boxed_me, scanner)) => {
                let pot_args = Arguments::parse(parser, scanner, yield_flag, await_flag)?;
                match pot_args {
                    None => Ok(None),
                    Some((boxed_args, scanner)) => Ok(Some((
                        Box::new(CallMemberExpression {
                            member_expression: boxed_me,
                            arguments: boxed_args,
                        }),
                        scanner,
                    ))),
                }
            }
        }
    }
}

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
}

impl SuperCall {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        match tok {
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Super) => {
                let pot_args = Arguments::parse(parser, scanner, yield_flag, await_flag)?;
                match pot_args {
                    None => Ok(None),
                    Some((boxed_args, scanner)) => Ok(Some((Box::new(Self { arguments: boxed_args }), scanner))),
                }
            }
            _ => Ok(None),
        }
    }
}

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
}

impl ImportCall {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        match tok {
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Import) => {
                // Got "import"
                let (tok, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
                match tok {
                    scanner::Token::LeftParen => {
                        // Got "import ("
                        let pot_ae = AssignmentExpression::parse(parser, scanner, true, yield_flag, await_flag)?;
                        match pot_ae {
                            None => Ok(None),
                            Some((ae_boxed, scanner)) => {
                                // Got "import ( AssignmentExpression"
                                let (tok, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
                                match tok {
                                    scanner::Token::RightParen => {
                                        // Got "import ( AssignmentExpression )"
                                        Ok(Some((Box::new(Self { assignment_expression: ae_boxed }), scanner)))
                                    }
                                    _ => Ok(None),
                                }
                            }
                        }
                    }
                    _ => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug)]
pub enum CallExpressionKind {
    CallMemberExpression(Box<CallMemberExpression>),
    SuperCall(Box<SuperCall>),
    ImportCall(Box<ImportCall>),
    CallExpressionArguments((Box<CallExpression>, Box<Arguments>)),
    CallExpressionExpression((Box<CallExpression>, Box<Expression>)),
    CallExpressionIdentifierName((Box<CallExpression>, Box<IdentifierNameToken>)),
    // CallExpressionTemplateLiteral
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
            CallExpressionKind::CallExpressionArguments((ce, args)) => write!(f, "{} {}", ce, args),
            CallExpressionKind::CallExpressionExpression((ce, exp)) => write!(f, "{} [ {} ]", ce, exp),
            CallExpressionKind::CallExpressionIdentifierName((ce, int)) => write!(f, "{} . {}", ce, int),
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
            CallExpressionKind::CallExpressionArguments((ce, args)) => {
                ce.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                args.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CallExpressionKind::CallExpressionExpression((ce, exp)) => {
                ce.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                exp.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CallExpressionKind::CallExpressionIdentifierName((ce, _)) => ce.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
}

impl AssignmentTargetType for CallExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match &self.kind {
            CallExpressionKind::CallMemberExpression(_) | CallExpressionKind::SuperCall(_) | CallExpressionKind::ImportCall(_) | CallExpressionKind::CallExpressionArguments(_) => ATTKind::Invalid,
            CallExpressionKind::CallExpressionExpression(_) | CallExpressionKind::CallExpressionIdentifierName(_) => ATTKind::Simple,
        }
    }
}

impl CallExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_arg: bool, await_arg: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let first_parse = {
            let pot_cme = CallMemberExpression::parse(parser, scanner, yield_arg, await_arg)?;
            match pot_cme {
                None => {
                    let pot_sc = SuperCall::parse(parser, scanner, yield_arg, await_arg)?;
                    match pot_sc {
                        None => {
                            let pot_ic = ImportCall::parse(parser, scanner, yield_arg, await_arg)?;
                            match pot_ic {
                                None => None,
                                Some((ic_boxed, scanner)) => Some((
                                    Box::new(Self {
                                        kind: CallExpressionKind::ImportCall(ic_boxed),
                                    }),
                                    scanner,
                                )),
                            }
                        }
                        Some((sc_boxed, scanner)) => Some((
                            Box::new(Self {
                                kind: CallExpressionKind::SuperCall(sc_boxed),
                            }),
                            scanner,
                        )),
                    }
                }
                Some((cme_boxed, scanner)) => Some((
                    Box::new(Self {
                        kind: CallExpressionKind::CallMemberExpression(cme_boxed),
                    }),
                    scanner,
                )),
            }
        };
        if first_parse.is_none() {
            return Ok(None);
        }
        let (mut top_box, mut top_scanner) = first_parse.unwrap();
        loop {
            let (tok, scanner) = scanner::scan_token(&top_scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
            match tok {
                scanner::Token::LeftParen => {
                    // Don't consume the token in this particular case.
                    let pot_args = Arguments::parse(parser, top_scanner, yield_arg, await_arg)?;
                    match pot_args {
                        None => {
                            break;
                        }
                        Some((args_boxed, scanner)) => {
                            top_box = Box::new(Self {
                                kind: CallExpressionKind::CallExpressionArguments((top_box, args_boxed)),
                            });
                            top_scanner = scanner;
                        }
                    }
                }
                scanner::Token::LeftBracket => {
                    let pot_exp = Expression::parse(parser, scanner, true, yield_arg, await_arg)?;
                    match pot_exp {
                        None => {
                            break;
                        }
                        Some((exp_boxed, scanner)) => {
                            let (tok, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
                            match tok {
                                scanner::Token::RightBracket => {
                                    top_box = Box::new(Self {
                                        kind: CallExpressionKind::CallExpressionExpression((top_box, exp_boxed)),
                                    });
                                    top_scanner = scanner;
                                }
                                _ => {
                                    break;
                                }
                            }
                        }
                    }
                }
                scanner::Token::Dot => {
                    let (tok, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
                    match tok {
                        scanner::Token::Identifier(_) => {
                            top_box = Box::new(Self {
                                kind: CallExpressionKind::CallExpressionIdentifierName((top_box, Box::new(IdentifierNameToken { value: tok }))),
                            });
                            top_scanner = scanner;
                        }
                        _ => {
                            break;
                        }
                    }
                }
                _ => {
                    break;
                }
            }
        }
        Ok(Some((top_box, top_scanner)))
    }
}

#[derive(Debug)]
pub enum LeftHandSideExpression {
    NewExpression(Box<NewExpression>),
    CallExpression(Box<CallExpression>),
    // OptionalExpression(Box<OptionalExpression>),
}

impl fmt::Display for LeftHandSideExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            LeftHandSideExpression::NewExpression(boxed) => write!(f, "{}", boxed),
            LeftHandSideExpression::CallExpression(boxed) => write!(f, "{}", boxed),
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
        }
    }
}

impl IsFunctionDefinition for LeftHandSideExpression {
    fn is_function_definition(&self) -> bool {
        match self {
            LeftHandSideExpression::NewExpression(boxed) => boxed.is_function_definition(),
            LeftHandSideExpression::CallExpression(_) => false,
        }
    }
}

impl AssignmentTargetType for LeftHandSideExpression {
    fn assignment_target_type(&self) -> ATTKind {
        match self {
            LeftHandSideExpression::NewExpression(boxed) => boxed.assignment_target_type(),
            LeftHandSideExpression::CallExpression(boxed) => boxed.assignment_target_type(),
        }
    }
}

impl LeftHandSideExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_arg: bool, await_arg: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_ce = CallExpression::parse(parser, scanner, yield_arg, await_arg)?;
        match pot_ce {
            Some((ce_boxed, scanner)) => Ok(Some((Box::new(Self::CallExpression(ce_boxed)), scanner))),
            _ => {
                let pot_ne = NewExpression::parse(parser, scanner, yield_arg, await_arg)?;
                match pot_ne {
                    Some((ne_boxed, scanner)) => Ok(Some((Box::new(Self::NewExpression(ne_boxed)), scanner))),
                    None => Ok(None),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::assignment_operators::AssignmentExpressionKind;
    use super::testhelp::{check, check_none, chk_scan, newparser};
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
        assert!(matches!(me.kind, MemberExpressionKind::NewArguments(_)));
        // Excersize the Debug formatter, for code coverage
        format!("{:?}", me);
        pretty_check(
            &*me,
            "MemberExpression: new shoes ( \"red\" , \"leather\" )",
            vec!["MemberExpression: shoes", "Arguments: ( \"red\" , \"leather\" )"],
        );
        assert_eq!(me.is_function_definition(), false);
        assert_eq!(me.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn member_expression_test_me_expression() {
        let (me, scanner) = check(MemberExpression::parse(&mut newparser("bill[a]"), Scanner::new(), false, false));
        chk_scan(&scanner, 7);
        assert!(matches!(me.kind, MemberExpressionKind::Expression(_)));
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
        assert!(matches!(me.kind, MemberExpressionKind::IdentifierName(_)));
        // Excersize the Debug formatter, for code coverage
        format!("{:?}", me);
        pretty_check(&*me, "MemberExpression: alice . name", vec!["MemberExpression: alice"]);
        assert_eq!(me.is_function_definition(), false);
        assert_eq!(me.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn member_expression_test_bad_ident() {
        let r = MemberExpression::parse(&mut newparser("alice.'pool'"), Scanner::new(), false, false);
        assert!(r.is_err());
    }
    #[test]
    fn member_expression_test_bad_expr() {
        let r = MemberExpression::parse(&mut newparser("alice[while]"), Scanner::new(), false, false);
        assert!(r.is_err());
    }
    #[test]
    fn member_expression_test_bad_expr_close() {
        let r = MemberExpression::parse(&mut newparser("alice[73"), Scanner::new(), false, false);
        assert!(r.is_err());
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
        check_none(SuperProperty::parse(&mut newparser("silly"), Scanner::new(), false, false));
    }
    #[test]
    fn super_property_test_bad_ident() {
        let r = SuperProperty::parse(&mut newparser("super.**"), Scanner::new(), false, false);
        assert!(r.is_err());
    }
    #[test]
    fn super_property_test_bad_expression() {
        let r = SuperProperty::parse(&mut newparser("super[while]"), Scanner::new(), false, false);
        assert!(r.is_err());
    }
    #[test]
    fn super_property_test_incomplete_expression() {
        let r = SuperProperty::parse(&mut newparser("super[99"), Scanner::new(), false, false);
        assert!(r.is_err());
    }
    #[test]
    fn super_property_test_bad_following_token() {
        check_none(SuperProperty::parse(&mut newparser("super duper"), Scanner::new(), false, false));
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
        check_none(MetaProperty::parse(&mut newparser("silly"), Scanner::new()));
    }
    #[test]
    fn meta_property_test_nomatch_02() {
        check_none(MetaProperty::parse(&mut newparser("new silly"), Scanner::new()));
    }
    #[test]
    fn meta_property_test_nomatch_03() {
        check_none(MetaProperty::parse(&mut newparser("new.silly"), Scanner::new()));
    }
    #[test]
    fn meta_property_test_nomatch_04() {
        check_none(MetaProperty::parse(&mut newparser("import silly"), Scanner::new()));
    }
    #[test]
    fn meta_property_test_nomatch_05() {
        check_none(MetaProperty::parse(&mut newparser("import.silly"), Scanner::new()));
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
        check_none(Arguments::parse(&mut newparser("**"), Scanner::new(), false, false));
    }
    #[test]
    fn arguments_test_unclosed_01() {
        check_none(Arguments::parse(&mut newparser("("), Scanner::new(), false, false));
    }
    #[test]
    fn arguments_test_unclosed_02() {
        check_none(Arguments::parse(&mut newparser("(88"), Scanner::new(), false, false));
    }
    #[test]
    fn arguments_test_unclosed_03() {
        check_none(Arguments::parse(&mut newparser("(91,"), Scanner::new(), false, false));
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
        assert!(matches!(al.kind, ArgumentListKind::ArgumentListAssignmentExpression(_)));
        format!("{:?}", al);
        pretty_check(&*al, "ArgumentList: ab , aba", vec!["ArgumentList: ab", "AssignmentExpression: aba"]);
    }
    #[test]
    fn argument_list_test_al_dots_ae() {
        let (al, scanner) = check(ArgumentList::parse(&mut newparser("ab,...aba"), Scanner::new(), false, false));
        chk_scan(&scanner, 9);
        assert!(matches!(al.kind, ArgumentListKind::ArgumentListDotsAssignmentExpression(_)));
        format!("{:?}", al);
        pretty_check(&*al, "ArgumentList: ab , ... aba", vec!["ArgumentList: ab", "AssignmentExpression: aba"]);
    }
    #[test]
    fn argument_list_test_nomatch() {
        check_none(ArgumentList::parse(&mut newparser("**"), Scanner::new(), false, false));
    }
    #[test]
    fn argument_list_test_dotsonly() {
        assert!(ArgumentList::parse(&mut newparser("..."), Scanner::new(), false, false).is_err());
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
        check_none(NewExpression::parse(&mut newparser("**"), Scanner::new(), false, false));
    }
    #[test]
    fn new_expression_test_chopped() {
        check_none(NewExpression::parse(&mut newparser("new"), Scanner::new(), false, false));
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
        check_none(CallMemberExpression::parse(&mut newparser("++"), Scanner::new(), false, false));
    }
    #[test]
    fn call_member_expression_test_incomplete() {
        check_none(CallMemberExpression::parse(&mut newparser("pop"), Scanner::new(), false, false));
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
        check_none(SuperCall::parse(&mut newparser("++"), Scanner::new(), false, false));
    }
    #[test]
    fn super_call_test_incomplete() {
        check_none(SuperCall::parse(&mut newparser("super"), Scanner::new(), false, false));
    }

    // IMPORT CALL
    #[test]
    fn import_call_test_ae() {
        let (ic, scanner) = check(ImportCall::parse(&mut newparser("import(bob)"), Scanner::new(), false, false));
        chk_scan(&scanner, 11);
        assert!(matches!(ic.assignment_expression.kind, AssignmentExpressionKind::Temp(_)));
        format!("{:?}", ic);
        pretty_check(&*ic, "ImportCall: import ( bob )", vec!["AssignmentExpression: bob"]);
    }
    #[test]
    fn import_call_test_nomatch() {
        check_none(ImportCall::parse(&mut newparser("++"), Scanner::new(), false, false));
    }
    #[test]
    fn import_call_test_incomplete() {
        check_none(ImportCall::parse(&mut newparser("import"), Scanner::new(), false, false));
    }
    #[test]
    fn import_call_test_incomplete2() {
        check_none(ImportCall::parse(&mut newparser("import("), Scanner::new(), false, false));
    }
    #[test]
    fn import_call_test_incomplete3() {
        check_none(ImportCall::parse(&mut newparser("import(bob"), Scanner::new(), false, false));
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
        assert!(matches!(ce.kind, CallExpressionKind::CallExpressionArguments(_)));
        format!("{:?}", ce);
        pretty_check(
            &*ce,
            "CallExpression: blue ( pop ) ( snap ) ( 10 ) ( 20 )",
            vec!["CallExpression: blue ( pop ) ( snap ) ( 10 )", "Arguments: ( 20 )"],
        );
        assert_eq!(ce.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn call_expression_test_ce_args2() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)(snap)(10)(++)"), Scanner::new(), false, false));
        chk_scan(&scanner, 19);
        assert!(matches!(ce.kind, CallExpressionKind::CallExpressionArguments(_)));
        format!("{:?}", ce);
    }
    #[test]
    fn call_expression_test_ce_exp() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)[snap]"), Scanner::new(), false, false));
        chk_scan(&scanner, 15);
        assert!(matches!(ce.kind, CallExpressionKind::CallExpressionExpression(_)));
        format!("{:?}", ce);
        pretty_check(&*ce, "CallExpression: blue ( pop ) [ snap ]", vec!["CallExpression: blue ( pop )", "Expression: snap"]);
        assert_eq!(ce.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn call_expression_test_ce_ident() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop).snap"), Scanner::new(), false, false));
        chk_scan(&scanner, 14);
        assert!(matches!(ce.kind, CallExpressionKind::CallExpressionIdentifierName(_)));
        format!("{:?}", ce);
        pretty_check(&*ce, "CallExpression: blue ( pop ) . snap", vec!["CallExpression: blue ( pop )"]);
        assert_eq!(ce.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn call_expression_test_nomatch() {
        check_none(CallExpression::parse(&mut newparser(""), Scanner::new(), false, false));
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
