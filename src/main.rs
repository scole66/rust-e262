#![allow(dead_code, unused_variables)]

use num::bigint::BigInt;
use std::env;
use std::fmt;
use std::io::{self, Write};

mod scanner;

#[derive(Debug, PartialEq)]
pub enum ParseGoal {
    Script,
    Module,
}

pub struct Parser<'a> {
    source: &'a str,
    scanner: Scanner,
    strict: bool,
    goal: ParseGoal,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, strict: bool, goal: ParseGoal) -> Self {
        Self {
            source,
            scanner: Scanner::new(),
            strict,
            goal,
        }
    }
}

pub trait StringValue {
    fn string_value(&self) -> scanner::JSString;
}

pub trait BoundNames {
    fn bound_names(&self) -> Vec<scanner::JSString>;
}

pub trait HasName {
    fn has_name(&self) -> bool;
}

pub trait IsFunctionDefinition {
    fn is_function_definition(&self) -> bool;
}

pub trait IsIdentifierReference {
    fn is_identifier_reference(&self) -> bool;
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ATTKind {
    Invalid,
    Simple,
}
pub trait AssignmentTargetType {
    fn assignment_target_type(&self) -> ATTKind;
}

pub enum Spot {
    Initial,
    NotFinal,
    Final,
}

fn prettypad(pad: &str, state: Spot) -> (String, String) {
    let mut first = String::from(pad);
    let mut successive = String::from(pad);
    match state {
        Spot::Initial => {}
        Spot::NotFinal => {
            first.push_str("├── ");
            successive.push_str("│   ");
        }
        Spot::Final => {
            first.push_str("└── ");
            successive.push_str("    ");
        }
    }
    (first, successive)
}

pub trait PrettyPrint {
    fn pprint(&self);
    fn pprint_with_leftpad(&self, pad: &str, state: Spot);
}

//////// 12.1 Identifiers

// Identifier:
//      IdentifierName but not ReservedWord
#[derive(Debug)]
pub enum Identifier {
    IdentifierName(scanner::IdentifierData),
}

impl StringValue for Identifier {
    fn string_value(&self) -> scanner::JSString {
        let Identifier::IdentifierName(identifier_name) = self;
        identifier_name.string_value.clone()
    }
}

impl PrettyPrint for Identifier {
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, _) = prettypad(pad, state);
        println!("{}Identifier: {}", first, self.string_value());
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.string_value())
    }
}

impl Identifier {
    fn parse(parser: &mut Parser, scanner: Scanner) -> Result<Option<(Box<Identifier>, Scanner)>, String> {
        let (tok, after_tok) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
        match tok {
            scanner::Token::Identifier(id) => match id.keyword_id {
                Some(scanner::Keyword::Await)
                | Some(scanner::Keyword::Break)
                | Some(scanner::Keyword::Case)
                | Some(scanner::Keyword::Catch)
                | Some(scanner::Keyword::Class)
                | Some(scanner::Keyword::Const)
                | Some(scanner::Keyword::Continue)
                | Some(scanner::Keyword::Debugger)
                | Some(scanner::Keyword::Default)
                | Some(scanner::Keyword::Delete)
                | Some(scanner::Keyword::Do)
                | Some(scanner::Keyword::Else)
                | Some(scanner::Keyword::Enum)
                | Some(scanner::Keyword::Export)
                | Some(scanner::Keyword::Extends)
                | Some(scanner::Keyword::False)
                | Some(scanner::Keyword::Finally)
                | Some(scanner::Keyword::For)
                | Some(scanner::Keyword::Function)
                | Some(scanner::Keyword::If)
                | Some(scanner::Keyword::Import)
                | Some(scanner::Keyword::In)
                | Some(scanner::Keyword::Instanceof)
                | Some(scanner::Keyword::New)
                | Some(scanner::Keyword::Null)
                | Some(scanner::Keyword::Return)
                | Some(scanner::Keyword::Super)
                | Some(scanner::Keyword::Switch)
                | Some(scanner::Keyword::This)
                | Some(scanner::Keyword::Throw)
                | Some(scanner::Keyword::True)
                | Some(scanner::Keyword::Try)
                | Some(scanner::Keyword::Typeof)
                | Some(scanner::Keyword::Var)
                | Some(scanner::Keyword::Void)
                | Some(scanner::Keyword::While)
                | Some(scanner::Keyword::With)
                | Some(scanner::Keyword::Yield) => Ok(None),
                _ => {
                    if parser.strict
                        && (id.string_value == "implements"
                            || id.string_value == "interface"
                            || id.string_value == "let"
                            || id.string_value == "package"
                            || id.string_value == "private"
                            || id.string_value == "protected"
                            || id.string_value == "public"
                            || id.string_value == "static"
                            || id.string_value == "yield")
                    {
                        Err(format!("{}:{}: ‘{}’ not allowed as an identifier in strict mode", id.line, id.column, id.string_value))
                    } else if parser.goal == ParseGoal::Module && id.string_value == "await" {
                        Err(format!("{}:{}: ‘await’ not allowed as an identifier in modules", id.line, id.column))
                    } else if id.string_value == "break"
                        || id.string_value == "case"
                        || id.string_value == "catch"
                        || id.string_value == "class"
                        || id.string_value == "const"
                        || id.string_value == "continue"
                        || id.string_value == "debugger"
                        || id.string_value == "default"
                        || id.string_value == "delete"
                        || id.string_value == "do"
                        || id.string_value == "else"
                        || id.string_value == "enum"
                        || id.string_value == "export"
                        || id.string_value == "extends"
                        || id.string_value == "false"
                        || id.string_value == "finally"
                        || id.string_value == "for"
                        || id.string_value == "function"
                        || id.string_value == "if"
                        || id.string_value == "import"
                        || id.string_value == "in"
                        || id.string_value == "instanceof"
                        || id.string_value == "new"
                        || id.string_value == "null"
                        || id.string_value == "return"
                        || id.string_value == "super"
                        || id.string_value == "switch"
                        || id.string_value == "this"
                        || id.string_value == "throw"
                        || id.string_value == "true"
                        || id.string_value == "try"
                        || id.string_value == "typeof"
                        || id.string_value == "var"
                        || id.string_value == "void"
                        || id.string_value == "while"
                        || id.string_value == "with"
                    {
                        Err(format!("{}:{}: ‘{}’ is a reserved word and may not be used as an identifier", id.line, id.column, id.string_value))
                    } else {
                        let node = Identifier::IdentifierName(id);
                        let boxed = Box::new(node);
                        Ok(Some((boxed, after_tok)))
                    }
                }
            },
            _ => Ok(None),
        }
    }
}

// IdentifierReference[Yield, Await]:
//      Identifier
//      [~Yield]yield
//      [~Await]await

#[derive(Debug)]
enum IdentifierReferenceKind {
    Identifier(Box<Identifier>),
    Yield,
    Await,
}

#[derive(Debug)]
pub struct IdentifierReference {
    kind: IdentifierReferenceKind,
    strict: bool,
}

impl StringValue for IdentifierReference {
    fn string_value(&self) -> scanner::JSString {
        use IdentifierReferenceKind::*;
        match &self.kind {
            Identifier(id) => id.string_value(),
            Yield => scanner::JSString::from("yield"),
            Await => scanner::JSString::from("await"),
        }
    }
}

impl AssignmentTargetType for IdentifierReference {
    fn assignment_target_type(&self) -> ATTKind {
        use ATTKind::*;
        use IdentifierReferenceKind::*;
        match &self.kind {
            Identifier(id) => {
                if self.strict {
                    let sv = id.string_value();
                    if sv == "eval" || sv == "arguments" {
                        Invalid
                    } else {
                        Simple
                    }
                } else {
                    Simple
                }
            }
            Await | Yield => Simple,
        }
    }
}

impl PrettyPrint for IdentifierReference {
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}IdentifierReference: {}", first, self);
        if let IdentifierReferenceKind::Identifier(boxed) = &self.kind {
            boxed.pprint_with_leftpad(&successive, Spot::Final);
        }
    }
}

impl fmt::Display for IdentifierReference {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            IdentifierReferenceKind::Identifier(boxed) => write!(f, "{}", *boxed),
            IdentifierReferenceKind::Yield => write!(f, "yield"),
            IdentifierReferenceKind::Await => write!(f, "await"),
        }
    }
}

impl IdentifierReference {
    fn parse(parser: &mut Parser, initial_scanner: Scanner, arg_yield: bool, arg_await: bool) -> Result<Option<(Box<IdentifierReference>, Scanner)>, String> {
        let production = Identifier::parse(parser, initial_scanner)?;
        match production {
            Some((ident, scanner)) => {
                let node = IdentifierReference {
                    kind: IdentifierReferenceKind::Identifier(ident),
                    strict: parser.strict,
                };
                let boxed = Box::new(node);
                Ok(Some((boxed, scanner)))
            }
            None => {
                let (token, scan) = scanner::scan_token(&parser.scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
                match token {
                    scanner::Token::Identifier(id) if !arg_await && id.keyword_id == Some(scanner::Keyword::Await) => Ok(Some((
                        Box::new(IdentifierReference {
                            kind: IdentifierReferenceKind::Await,
                            strict: parser.strict,
                        }),
                        scan,
                    ))),
                    scanner::Token::Identifier(id) if !arg_yield && id.keyword_id == Some(scanner::Keyword::Yield) => Ok(Some((
                        Box::new(IdentifierReference {
                            kind: IdentifierReferenceKind::Yield,
                            strict: parser.strict,
                        }),
                        scan,
                    ))),
                    _ => Ok(None),
                }
            }
        }
    }
}

// BindingIdentifier[Yield, Await] :
//    Identifier
//    yield
//    await
#[derive(Debug)]
enum BindingIdentifierKind {
    Identifier(Box<Identifier>),
    Yield,
    Await,
}

#[derive(Debug)]
struct BindingIdentifier {
    kind: BindingIdentifierKind,
    yield_flag: bool,
    await_flag: bool,
}

impl StringValue for BindingIdentifier {
    fn string_value(&self) -> scanner::JSString {
        use BindingIdentifierKind::*;
        match &self.kind {
            Identifier(id) => id.string_value(),
            Yield => scanner::JSString::from("yield"),
            Await => scanner::JSString::from("await"),
        }
    }
}

impl BoundNames for BindingIdentifier {
    fn bound_names(&self) -> Vec<scanner::JSString> {
        use BindingIdentifierKind::*;
        match &self.kind {
            Identifier(id) => vec![id.string_value()],
            Yield => vec![scanner::JSString::from("yield")],
            Await => vec![scanner::JSString::from("await")],
        }
    }
}

impl fmt::Display for BindingIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            BindingIdentifierKind::Await => write!(f, "await"),
            BindingIdentifierKind::Identifier(boxed) => write!(f, "{}", boxed),
            BindingIdentifierKind::Yield => write!(f, "yield"),
        }
    }
}

impl PrettyPrint for BindingIdentifier {
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}BindingIdentifier: {}", first, self);
        if let BindingIdentifierKind::Identifier(boxed) = &self.kind {
            boxed.pprint_with_leftpad(&successive, Spot::Final);
        }
    }
}

fn binding_identifier(parser: &mut Parser, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<BindingIdentifier>, Scanner)>, String> {
    let production = Identifier::parse(parser, parser.scanner)?;
    match production {
        Some((ident, scanner)) => {
            let node = BindingIdentifier {
                kind: BindingIdentifierKind::Identifier(ident),
                yield_flag,
                await_flag,
            };
            let boxed = Box::new(node);
            Ok(Some((boxed, scanner)))
        }
        None => {
            let (token, scan) = scanner::scan_token(&parser.scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
            match token {
                scanner::Token::Identifier(id) => match id.keyword_id {
                    Some(scanner::Keyword::Await) => Ok(Some((
                        Box::new(BindingIdentifier {
                            kind: BindingIdentifierKind::Await,
                            yield_flag,
                            await_flag,
                        }),
                        scan,
                    ))),
                    Some(scanner::Keyword::Yield) => Ok(Some((
                        Box::new(BindingIdentifier {
                            kind: BindingIdentifierKind::Yield,
                            yield_flag,
                            await_flag,
                        }),
                        scan,
                    ))),
                    _ => Ok(None),
                },
                _ => Ok(None),
            }
        }
    }
}

//////// 12.2 Primary Expression
// PrimaryExpression[Yield, Await] :
//      this
//      IdentifierReference[?Yield, ?Await]
//      Literal
//      ArrayLiteral[?Yield, ?Await]
//      ObjectLiteral[?Yield, ?Await]
//      FunctionExpression
//      ClassExpression[?Yield, ?Await]
//      GeneratorExpression
//      AsyncFunctionExpression
//      AsyncGeneratorExpression
//      RegularExpressionLiteral
//      TemplateLiteral[?Yield, ?Await, ~Tagged]
//      CoverParenthesizedExpressionAndArrowParameterList[?Yield, ?Await]

#[derive(Debug)]
pub enum PrimaryExpressionKind {
    This,
    IdentifierReference(Box<IdentifierReference>),
    Literal(Box<Literal>),
    // More to come
}

#[derive(Debug)]
pub struct PrimaryExpression {
    kind: PrimaryExpressionKind,
}

impl fmt::Display for PrimaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            PrimaryExpressionKind::This => write!(f, "this"),
            PrimaryExpressionKind::IdentifierReference(boxed) => write!(f, "{}", boxed),
            PrimaryExpressionKind::Literal(boxed) => write!(f, "{}", boxed),
        }
    }
}

impl PrettyPrint for PrimaryExpression {
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}PrimaryExpression: {}", first, self);
        match &self.kind {
            PrimaryExpressionKind::This => {}
            PrimaryExpressionKind::IdentifierReference(boxed) => {
                boxed.pprint_with_leftpad(&successive, Spot::Final);
            }
            PrimaryExpressionKind::Literal(boxed) => {
                boxed.pprint_with_leftpad(&successive, Spot::Final);
            }
        }
    }
}

impl IsFunctionDefinition for PrimaryExpression {
    fn is_function_definition(&self) -> bool {
        use PrimaryExpressionKind::*;
        match self.kind {
            This | IdentifierReference(_) | Literal(_) => false,
        }
    }
}

impl IsIdentifierReference for PrimaryExpression {
    fn is_identifier_reference(&self) -> bool {
        use PrimaryExpressionKind::*;
        match &self.kind {
            This | Literal(_) => false,
            IdentifierReference(_) => true,
        }
    }
}

impl AssignmentTargetType for PrimaryExpression {
    fn assignment_target_type(&self) -> ATTKind {
        use PrimaryExpressionKind::*;
        match &self.kind {
            This | Literal(_) => ATTKind::Invalid,
            IdentifierReference(id) => id.assignment_target_type(),
        }
    }
}

pub trait ToPrimaryExpressionKind {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind;
}

impl ToPrimaryExpressionKind for IdentifierReference {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::IdentifierReference(node)
    }
}
impl ToPrimaryExpressionKind for Literal {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::Literal(node)
    }
}

fn pe_boxer<T>(opt: Option<(Box<T>, Scanner)>) -> Result<Option<(Box<PrimaryExpression>, Scanner)>, String>
where
    T: ToPrimaryExpressionKind,
{
    Ok(opt.map(|(node, scanner)| {
        (
            Box::new(PrimaryExpression {
                kind: T::to_primary_expression_kind(node),
            }),
            scanner,
        )
    }))
}

fn rewrap<T, E>(value: T) -> Result<Option<T>, E> {
    Ok(Some(value))
}

fn or_pe_kind<F, T>(opt: Option<(Box<PrimaryExpression>, Scanner)>, parser: &mut Parser, parse_func: F) -> Result<Option<(Box<PrimaryExpression>, Scanner)>, String>
where
    F: FnOnce(&mut Parser) -> Result<Option<(Box<T>, Scanner)>, String>,
    T: ToPrimaryExpressionKind,
{
    opt.map_or_else(|| parse_func(parser).and_then(pe_boxer), rewrap)
}

fn primary_expression(parser: &mut Parser, arg_yield: bool, arg_await: bool) -> Result<Option<(Box<PrimaryExpression>, Scanner)>, String> {
    Ok(None)
        .and_then(|opt| or_pe_kind(opt, parser, |p| IdentifierReference::parse(p, p.scanner, arg_yield, arg_await)))
        .and_then(|opt| or_pe_kind(opt, parser, literal))
        .and_then(|opt| or_pe_kind(opt, parser, this_token))
}

#[derive(Debug)]
pub struct ThisToken {}

impl ToPrimaryExpressionKind for ThisToken {
    fn to_primary_expression_kind(_node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::This
    }
}

fn this_token(parser: &mut Parser) -> Result<Option<(Box<ThisToken>, Scanner)>, String> {
    let (tok, scanner) = scanner::scan_token(&parser.scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
    Ok(match tok {
        scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::This) => Some((Box::new(ThisToken {}), scanner)),
        _ => None,
    })
}

//////// 12.2.4 Literals
// Literal :
//      NullLiteral
//      BooleanLiteral
//      NumericLiteral
//      StringLiteral
#[derive(Debug, PartialEq)]
pub enum Numeric {
    Number(f64),
    BigInt(BigInt),
}
#[derive(Debug, PartialEq)]
pub enum LiteralKind {
    NullLiteral,
    BooleanLiteral(bool),
    NumericLiteral(Numeric),
    StringLiteral(scanner::JSString),
}
#[derive(Debug, PartialEq)]
pub struct Literal {
    kind: LiteralKind,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            LiteralKind::NullLiteral => write!(f, "null"),
            LiteralKind::BooleanLiteral(b) => {
                if *b {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                }
            }
            LiteralKind::NumericLiteral(n) => write!(f, "{:?}", *n),
            LiteralKind::StringLiteral(s) => write!(f, "{:?}", *s),
        }
    }
}

impl PrettyPrint for Literal {
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}Literal: {}", first, self);
    }
}

fn literal(parser: &mut Parser) -> Result<Option<(Box<Literal>, Scanner)>, String> {
    let scan_result = scanner::scan_token(&parser.scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
    let (token, newscanner) = scan_result;
    match token {
        scanner::Token::Identifier(id) => match id.keyword_id {
            Some(scanner::Keyword::Null) => {
                let node = Literal { kind: LiteralKind::NullLiteral };
                let boxed = Box::new(node);
                return Ok(Some((boxed, newscanner)));
            }
            Some(scanner::Keyword::True) => {
                let node = Literal {
                    kind: LiteralKind::BooleanLiteral(true),
                };
                let boxed = Box::new(node);
                return Ok(Some((boxed, newscanner)));
            }
            Some(scanner::Keyword::False) => {
                let node = Literal {
                    kind: LiteralKind::BooleanLiteral(false),
                };
                let boxed = Box::new(node);
                return Ok(Some((boxed, newscanner)));
            }
            _ => return Ok(None),
        },
        scanner::Token::Number(num) => {
            let node = Literal {
                kind: LiteralKind::NumericLiteral(Numeric::Number(num)),
            };
            let boxed = Box::new(node);
            return Ok(Some((boxed, newscanner)));
        }
        scanner::Token::BigInt(bi) => {
            let node = Literal {
                kind: LiteralKind::NumericLiteral(Numeric::BigInt(bi)),
            };
            let boxed = Box::new(node);
            return Ok(Some((boxed, newscanner)));
        }
        scanner::Token::String(s) => {
            let node = Literal { kind: LiteralKind::StringLiteral(s) };
            let boxed = Box::new(node);
            return Ok(Some((boxed, newscanner)));
        }
        _ => return Ok(None),
    }
}

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
pub struct IdentifierNameToken {
    value: scanner::Token,
}
impl fmt::Display for IdentifierNameToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let scanner::Token::Identifier(id) = &self.value {
            write!(f, "{}", id.string_value)
        } else {
            unreachable!()
        }
    }
}

#[derive(Debug)]
pub struct TemplateLiteral {}
impl fmt::Display for TemplateLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "`unimplemented`")
    }
}

impl PrettyPrint for TemplateLiteral {
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}TemplateLiteral: {}", first, self);
    }
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
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}MemberExpression: {}", first, self);
        match &self.kind {
            MemberExpressionKind::PrimaryExpression(boxed) => boxed.pprint_with_leftpad(&successive, Spot::Final),
            MemberExpressionKind::Expression(mee) => {
                mee.member_expression.pprint_with_leftpad(&successive, Spot::NotFinal);
                mee.expression.pprint_with_leftpad(&successive, Spot::Final);
            }
            MemberExpressionKind::IdentifierName(mein) => mein.member_expression.pprint_with_leftpad(&successive, Spot::Final),
            MemberExpressionKind::TemplateLiteral(metl) => {
                metl.member_expression.pprint_with_leftpad(&successive, Spot::NotFinal);
                metl.template_literal.pprint_with_leftpad(&successive, Spot::Final);
            }
            MemberExpressionKind::SuperProperty(boxed) => boxed.pprint_with_leftpad(&successive, Spot::Final),
            MemberExpressionKind::MetaProperty(boxed) => boxed.pprint_with_leftpad(&successive, Spot::Final),
            MemberExpressionKind::NewArguments(nmea) => {
                nmea.member_expression.pprint_with_leftpad(&successive, Spot::NotFinal);
                nmea.arguments.pprint_with_leftpad(&successive, Spot::Final);
            }
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
        let (tok, after) = scanner::scan_token(&after_scan, parser.source, scanner::ScanGoal::InputElementRegExp)?;
        match tok {
            scanner::Token::Dot => {
                let token_after_dot = scanner::scan_token(&after, parser.source, scanner::ScanGoal::InputElementRegExp)?;
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
                parser.scanner = after;
                let potential_expression = expression(parser, true, yield_flag, await_flag)?;
                match potential_expression {
                    None => {
                        return Err(format!("Expect Expression after ‘[’."));
                    }
                    Some((expression, after_exp)) => {
                        let after_exp = scanner::scan_token(&after_exp, parser.source, scanner::ScanGoal::InputElementRegExp)?;
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

pub fn member_expression(parser: &mut Parser, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<MemberExpression>, Scanner)>, String> {
    Ok(None)
        // First: All the non-head-recursive productions
        .and_then(|opt| or_me_kind(opt, parser, |p| primary_expression(p, yield_flag, await_flag)))
        .and_then(|opt| or_me_kind(opt, parser, |p| super_property(p, yield_flag, await_flag)))
        .and_then(|opt| or_me_kind(opt, parser, meta_property))
        .and_then(|opt| or_me_kind(opt, parser, |p| new_memberexpression_arguments(p, yield_flag, await_flag)))
        // And then all the head-recursive productions.
        .and_then(|opt| opt.map_or(Ok(None), |x| member_expression_head_recursive(parser, yield_flag, await_flag, x)))
}

pub fn new_memberexpression_arguments(parser: &mut Parser, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<NewMemberExpressionArguments>, Scanner)>, String> {
    scanner::scan_token(&parser.scanner, parser.source, scanner::ScanGoal::InputElementRegExp).and_then(|(token, scanner)| match token {
        scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::New) => {
            parser.scanner = scanner;
            member_expression(parser, yield_flag, await_flag).and_then(|opt| {
                opt.map_or(Ok(None), |(me, scan)| {
                    parser.scanner = scan;
                    arguments(parser, yield_flag, await_flag).and_then(|opt| {
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
            })
        }
        _ => Ok(None),
    })
}

#[derive(Debug)]
pub enum Expression {
    // todo!
    Temp(Box<AssignmentExpression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self::Temp(boxed) = &self;
        write!(f, "{}", boxed)
    }
}

impl PrettyPrint for Expression {
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}Expression: {}", first, self);
        let Expression::Temp(boxed) = &self;
        boxed.pprint_with_leftpad(&successive, Spot::Final);
    }
}

pub fn expression(parser: &mut Parser, in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Expression>, Scanner)>, String> {
    // todo!
    let potential = assignment_expression(parser, in_flag, yield_flag, await_flag)?;
    match potential {
        None => Ok(None),
        Some((boxed, scanner)) => Ok(Some((Box::new(Expression::Temp(boxed)), scanner))),
    }
}

#[derive(Debug)]
pub enum AssignmentExpressionKind {
    Temp(Box<ExponentiationExpression>),
}
#[derive(Debug)]
pub struct AssignmentExpression {
    kind: AssignmentExpressionKind,
}

impl fmt::Display for AssignmentExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let AssignmentExpressionKind::Temp(boxed) = &self.kind;
        write!(f, "{}", boxed)
    }
}

impl PrettyPrint for AssignmentExpression {
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}AssignmentExpression: {}", first, self);
        let AssignmentExpressionKind::Temp(boxed) = &self.kind;
        boxed.pprint_with_leftpad(&successive, Spot::Final);
    }
}

pub fn assignment_expression(parser: &mut Parser, _in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<AssignmentExpression>, Scanner)>, String> {
    let potential = ExponentiationExpression::parse(parser, parser.scanner, yield_flag, await_flag)?;
    match potential {
        None => Ok(None),
        Some((boxed, scanner)) => Ok(Some((
            Box::new(AssignmentExpression {
                kind: AssignmentExpressionKind::Temp(boxed),
            }),
            scanner,
        ))),
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
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}SuperProperty: {}", first, self);
        match &self.kind {
            SuperPropertyKind::Expression(boxed) => boxed.pprint_with_leftpad(&successive, Spot::Final),
            SuperPropertyKind::IdentifierName(boxed) => {}
        }
    }
}

pub fn super_property(parser: &mut Parser, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<SuperProperty>, Scanner)>, String> {
    scanner::scan_token(&parser.scanner, parser.source, scanner::ScanGoal::InputElementRegExp).and_then(|(token, scanner)| match token {
        scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Super) => {
            scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp).and_then(|(token, scanner)| match token {
                scanner::Token::LeftBracket => {
                    parser.scanner = scanner;
                    expression(parser, true, yield_flag, await_flag).and_then(|opt| {
                        opt.map_or_else(
                            || Err(String::from("‘super[’ must be followed by an Expression")),
                            |(exp_boxed, scanner)| {
                                scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp).and_then(|(token, scanner)| match token {
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
                    })
                }
                scanner::Token::Dot => scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp).and_then(|(token, scanner)| match token {
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
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}MetaProperty: {}", first, self);
    }
}

fn dot_token(parser: &mut Parser, scanner: Scanner, kwd: scanner::Keyword, kind: MetaPropertyKind) -> Result<Option<(Box<MetaProperty>, Scanner)>, String> {
    scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp).and_then(|(token, scanner)| match token {
        scanner::Token::Dot => scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp).and_then(|(token, scanner)| match token {
            scanner::Token::Identifier(id) if id.keyword_id == Some(kwd) => Ok(Some((Box::new(MetaProperty { kind }), scanner))),
            _ => Ok(None),
        }),
        _ => Ok(None),
    })
}

pub fn meta_property(parser: &mut Parser) -> Result<Option<(Box<MetaProperty>, Scanner)>, String> {
    scanner::scan_token(&parser.scanner, parser.source, scanner::ScanGoal::InputElementRegExp).and_then(|(token, scanner)| match token {
        scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::New) => dot_token(parser, scanner, scanner::Keyword::Target, MetaPropertyKind::NewTarget),
        scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Import) => dot_token(parser, scanner, scanner::Keyword::Meta, MetaPropertyKind::ImportMeta),
        _ => Ok(None),
    })
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
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}Arguments: {}", first, self);
        match &self.kind {
            ArgumentsKind::Empty => {}
            ArgumentsKind::ArgumentList(boxed) | ArgumentsKind::ArgumentListComma(boxed) => boxed.pprint_with_leftpad(&successive, Spot::Final),
        }
    }
}

pub fn arguments(parser: &mut Parser, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Arguments>, Scanner)>, String> {
    scanner::scan_token(&parser.scanner, parser.source, scanner::ScanGoal::InputElementRegExp).and_then(|(token, scanner)| match token {
        scanner::Token::LeftParen => {
            parser.scanner = scanner;
            argument_list(parser, yield_flag, await_flag).and_then(|opt| {
                opt.map_or_else(
                    || {
                        scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp).and_then(|(token, scanner)| match token {
                            scanner::Token::RightParen => Ok(Some((Box::new(Arguments { kind: ArgumentsKind::Empty }), scanner))),
                            _ => Ok(None),
                        })
                    },
                    |(arglist_boxed, scanner)| {
                        scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp).and_then(|(token, scanner)| match token {
                            scanner::Token::Comma => scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp).and_then(|(token, scanner)| match token {
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
            })
        }
        _ => Ok(None),
    })
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
        parser.scanner = scanner;
        assignment_expression(parser, true, yield_flag, await_flag).and_then(Self::ae_package)
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
        let (token, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
        match token {
            scanner::Token::Ellipsis => {
                // It was an ellipsis, so now try to get the AssignmentExpression
                parser.scanner = scanner;
                let potential_ae = assignment_expression(parser, true, yield_flag, await_flag)?;
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
        let (token, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
        match token {
            scanner::Token::Comma => {
                parser.scanner = scanner;
                assignment_expression(parser, true, yield_flag, await_flag)
            }
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
        let (token, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
        match token {
            scanner::Token::Comma => {
                let (token, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
                match token {
                    scanner::Token::Ellipsis => {
                        parser.scanner = scanner;
                        assignment_expression(parser, true, yield_flag, await_flag)
                    }
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
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}ArgumentList: {}", first, self);
        match &self.kind {
            ArgumentListKind::AssignmentExpression(boxed) | ArgumentListKind::DotsAssignmentExpression(boxed) => boxed.pprint_with_leftpad(&successive, Spot::Final),
            ArgumentListKind::ArgumentListAssignmentExpression(alae) | ArgumentListKind::ArgumentListDotsAssignmentExpression(alae) => {
                alae.argument_list.pprint_with_leftpad(&successive, Spot::NotFinal);
                alae.assignment_expression.pprint_with_leftpad(&successive, Spot::Final);
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
    pub fn parse(parser: &mut Parser, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let mut k = ArgumentListKind::parse_assignment_expression(parser, parser.scanner, yield_flag, await_flag)?;
        if k.is_none() {
            k = ArgumentListKind::parse_dots_assignment_expression(parser, parser.scanner, yield_flag, await_flag)?;
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

pub fn argument_list(parser: &mut Parser, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<ArgumentList>, Scanner)>, String> {
    ArgumentList::parse(parser, yield_flag, await_flag)
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
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}NewExpression: {}", first, self);
        match &self.kind {
            NewExpressionKind::MemberExpression(boxed) => boxed.pprint_with_leftpad(&successive, Spot::Final),
            NewExpressionKind::NewExpression(boxed) => boxed.pprint_with_leftpad(&successive, Spot::Final),
        }
    }
}

impl NewExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<NewExpression>, Scanner)>, String> {
        parser.scanner = scanner;
        let pot_me = member_expression(parser, yield_flag, await_flag)?;
        match pot_me {
            None => {
                let (token, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
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
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}CallMemberExpression: {}", first, self);
        self.member_expression.pprint_with_leftpad(&successive, Spot::NotFinal);
        self.arguments.pprint_with_leftpad(&successive, Spot::Final);
    }
}

impl CallMemberExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<CallMemberExpression>, Scanner)>, String> {
        parser.scanner = scanner;
        let pot_me = member_expression(parser, yield_flag, await_flag)?;
        match pot_me {
            None => Ok(None),
            Some((boxed_me, scanner)) => {
                parser.scanner = scanner;
                let pot_args = arguments(parser, yield_flag, await_flag)?;
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
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}SuperCall: {}", first, self);
        self.arguments.pprint_with_leftpad(&successive, Spot::Final);
    }
}

impl SuperCall {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
        match tok {
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Super) => {
                parser.scanner = scanner;
                let pot_args = arguments(parser, yield_flag, await_flag)?;
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
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}ImportCall: {}", first, self);
        self.assignment_expression.pprint_with_leftpad(&successive, Spot::Final);
    }
}

impl ImportCall {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
        match tok {
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Import) => {
                // Got "import"
                let (tok, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
                match tok {
                    scanner::Token::LeftParen => {
                        // Got "import ("
                        parser.scanner = scanner;
                        let pot_ae = assignment_expression(parser, true, yield_flag, await_flag)?;
                        match pot_ae {
                            None => Ok(None),
                            Some((ae_boxed, scanner)) => {
                                // Got "import ( AssignmentExpression"
                                let (tok, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
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
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}CallExpression: {}", first, self);
        match &self.kind {
            CallExpressionKind::CallMemberExpression(boxed) => boxed.pprint_with_leftpad(&successive, Spot::Final),
            CallExpressionKind::SuperCall(boxed) => boxed.pprint_with_leftpad(&successive, Spot::Final),
            CallExpressionKind::ImportCall(boxed) => boxed.pprint_with_leftpad(&successive, Spot::Final),
            CallExpressionKind::CallExpressionArguments((ce, args)) => {
                ce.pprint_with_leftpad(&successive, Spot::NotFinal);
                args.pprint_with_leftpad(&successive, Spot::Final);
            }
            CallExpressionKind::CallExpressionExpression((ce, exp)) => {
                ce.pprint_with_leftpad(&successive, Spot::NotFinal);
                exp.pprint_with_leftpad(&successive, Spot::Final);
            }
            CallExpressionKind::CallExpressionIdentifierName((ce, int)) => {
                ce.pprint_with_leftpad(&successive, Spot::Final);
            }
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
            let (tok, scanner) = scanner::scan_token(&top_scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
            match tok {
                scanner::Token::LeftParen => {
                    // Don't consume the token in this particular case.
                    parser.scanner = top_scanner;
                    let pot_args = arguments(parser, yield_arg, await_arg)?;
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
                    parser.scanner = scanner;
                    let pot_exp = expression(parser, true, yield_arg, await_arg)?;
                    match pot_exp {
                        None => {
                            break;
                        }
                        Some((exp_boxed, scanner)) => {
                            let (tok, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
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
                    let (tok, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
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
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}LeftHandSideExpression: {}", first, self);
        match &self {
            LeftHandSideExpression::NewExpression(boxed) => {
                boxed.pprint_with_leftpad(&successive, Spot::Final);
            }
            LeftHandSideExpression::CallExpression(boxed) => {
                boxed.pprint_with_leftpad(&successive, Spot::Final);
            }
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

#[derive(Debug)]
pub enum UpdateExpression {
    LeftHandSideExpression(Box<LeftHandSideExpression>),
    PostIncrement(Box<LeftHandSideExpression>),
    PostDecrement(Box<LeftHandSideExpression>),
    PreIncrement(Box<UnaryExpression>),
    PreDecrement(Box<UnaryExpression>),
}

impl fmt::Display for UpdateExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            UpdateExpression::LeftHandSideExpression(boxed) => write!(f, "{}", boxed),
            UpdateExpression::PostIncrement(boxed) => write!(f, "{} ++", boxed),
            UpdateExpression::PostDecrement(boxed) => write!(f, "{} --", boxed),
            UpdateExpression::PreIncrement(boxed) => write!(f, "++ {}", boxed),
            UpdateExpression::PreDecrement(boxed) => write!(f, "-- {}", boxed),
        }
    }
}

impl PrettyPrint for UpdateExpression {
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}UpdateExpression: {}", first, self);
        match &self {
            UpdateExpression::LeftHandSideExpression(boxed) | UpdateExpression::PostIncrement(boxed) | UpdateExpression::PostDecrement(boxed) => {
                boxed.pprint_with_leftpad(&successive, Spot::Final);
            }
            UpdateExpression::PreIncrement(boxed) | UpdateExpression::PreDecrement(boxed) => {
                boxed.pprint_with_leftpad(&successive, Spot::Final);
            }
        }
    }
}

impl UpdateExpression {
    fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (token, after_token) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
        match token {
            scanner::Token::PlusPlus => {
                // Seen ++ ...
                let pot_ue = UnaryExpression::parse(parser, after_token, yield_flag, await_flag)?;
                match pot_ue {
                    Some((boxed, after_exp)) => {
                        // Seen ++ UnaryExpression
                        Ok(Some((Box::new(Self::PreIncrement(boxed)), after_exp)))
                    }
                    None => Ok(None),
                }
            }
            scanner::Token::MinusMinus => {
                // Seen -- ...
                let pot_ue = UnaryExpression::parse(parser, after_token, yield_flag, await_flag)?;
                match pot_ue {
                    Some((boxed, after_exp)) => {
                        // Seen -- UnaryExpression
                        Ok(Some((Box::new(Self::PreDecrement(boxed)), after_exp)))
                    }
                    None => Ok(None),
                }
            }
            _ => {
                let pot_lhs = LeftHandSideExpression::parse(parser, scanner, yield_flag, await_flag)?;
                match pot_lhs {
                    Some((boxed, after_lhs)) => {
                        let (token, after_token) = scanner::scan_token(&after_lhs, parser.source, scanner::ScanGoal::InputElementRegExp)?;
                        if after_token.line != after_lhs.line {
                            Ok(Some((Box::new(UpdateExpression::LeftHandSideExpression(boxed)), after_lhs)))
                        } else {
                            match token {
                                scanner::Token::PlusPlus => Ok(Some((Box::new(UpdateExpression::PostIncrement(boxed)), after_token))),
                                scanner::Token::MinusMinus => Ok(Some((Box::new(UpdateExpression::PostDecrement(boxed)), after_token))),
                                _ => Ok(Some((Box::new(UpdateExpression::LeftHandSideExpression(boxed)), after_lhs))),
                            }
                        }
                    }
                    None => Ok(None),
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum UnaryExpression {
    UpdateExpression(Box<UpdateExpression>),
    Delete(Box<UnaryExpression>),
    Void(Box<UnaryExpression>),
    Typeof(Box<UnaryExpression>),
    NoOp(Box<UnaryExpression>),
    Negate(Box<UnaryExpression>),
    Complement(Box<UnaryExpression>),
    Not(Box<UnaryExpression>),
    Await(Box<AwaitExpression>),
}

impl fmt::Display for UnaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            UnaryExpression::UpdateExpression(boxed) => write!(f, "{}", boxed),
            UnaryExpression::Delete(boxed) => write!(f, "delete {}", boxed),
            UnaryExpression::Void(boxed) => write!(f, "void {}", boxed),
            UnaryExpression::Typeof(boxed) => write!(f, "typeof {}", boxed),
            UnaryExpression::NoOp(boxed) => write!(f, "+ {}", boxed),
            UnaryExpression::Negate(boxed) => write!(f, "- {}", boxed),
            UnaryExpression::Complement(boxed) => write!(f, "~ {}", boxed),
            UnaryExpression::Not(boxed) => write!(f, "! {}", boxed),
            UnaryExpression::Await(boxed) => write!(f, "{}", boxed),
        }
    }
}

impl PrettyPrint for UnaryExpression {
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}UnaryExpression: {}", first, self);
        match &self {
            UnaryExpression::UpdateExpression(boxed) => {
                boxed.pprint_with_leftpad(&successive, Spot::Final);
            }
            UnaryExpression::Delete(boxed)
            | UnaryExpression::Void(boxed)
            | UnaryExpression::Typeof(boxed)
            | UnaryExpression::NoOp(boxed)
            | UnaryExpression::Negate(boxed)
            | UnaryExpression::Complement(boxed)
            | UnaryExpression::Not(boxed) => {
                boxed.pprint_with_leftpad(&successive, Spot::Final);
            }
            UnaryExpression::Await(boxed) => {
                boxed.pprint_with_leftpad(&successive, Spot::Final);
            }
        }
    }
}

impl UnaryExpression {
    fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (token, after_token) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
        let mut unary_helper =
            |f: fn(Box<Self>) -> Self| UnaryExpression::parse(parser, after_token, yield_flag, await_flag).and_then(|opt| opt.map_or(Ok(None), |(boxed, after)| Ok(Some((Box::new(f(boxed)), after)))));
        match token {
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Delete) => unary_helper(|boxed| UnaryExpression::Delete(boxed)),
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Void) => unary_helper(|boxed| UnaryExpression::Void(boxed)),
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Typeof) => unary_helper(|boxed| UnaryExpression::Typeof(boxed)),
            scanner::Token::Plus => unary_helper(|boxed| UnaryExpression::NoOp(boxed)),
            scanner::Token::Minus => unary_helper(|boxed| UnaryExpression::Negate(boxed)),
            scanner::Token::Tilde => unary_helper(|boxed| UnaryExpression::Complement(boxed)),
            scanner::Token::Bang => unary_helper(|boxed| UnaryExpression::Not(boxed)),
            _ => {
                let mut production: Option<(Box<Self>, Scanner)> = None;
                if await_flag {
                    let pot_ae = AwaitExpression::parse(parser, scanner, yield_flag)?;
                    match pot_ae {
                        Some((boxed, scanner)) => {
                            production = Some((Box::new(UnaryExpression::Await(boxed)), scanner));
                        }
                        None => {}
                    }
                }
                if production.is_none() {
                    production = {
                        let pot_ue = UpdateExpression::parse(parser, scanner, yield_flag, await_flag)?;
                        match pot_ue {
                            Some((boxed, scanner)) => Some((Box::new(UnaryExpression::UpdateExpression(boxed)), scanner)),
                            None => None,
                        }
                    };
                }
                Ok(production)
            }
        }
    }
}

#[derive(Debug)]
pub enum ExponentiationExpression {
    UnaryExpression(Box<UnaryExpression>),
    Exponentiation((Box<UpdateExpression>, Box<ExponentiationExpression>)),
}

impl fmt::Display for ExponentiationExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ExponentiationExpression::UnaryExpression(boxed) => write!(f, "{}", boxed),
            ExponentiationExpression::Exponentiation((ue, ee)) => write!(f, "{} ** {}", ue, ee),
        }
    }
}

impl PrettyPrint for ExponentiationExpression {
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}ExponentiationExpression: {}", first, self);
        match &self {
            ExponentiationExpression::UnaryExpression(boxed) => {
                boxed.pprint_with_leftpad(&successive, Spot::Final);
            }
            ExponentiationExpression::Exponentiation((ue, ee)) => {
                ue.pprint_with_leftpad(&successive, Spot::NotFinal);
                ee.pprint_with_leftpad(&successive, Spot::Final);
            }
        }
    }
}

impl ExponentiationExpression {
    fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_ue = UpdateExpression::parse(parser, scanner, yield_flag, await_flag)?;
        let mut result = match pot_ue {
            Some((boxed_ue, after_ue)) => {
                let (token, scanner) = scanner::scan_token(&after_ue, parser.source, scanner::ScanGoal::InputElementRegExp)?;
                match token {
                    scanner::Token::StarStar => {
                        let pot_ee = ExponentiationExpression::parse(parser, scanner, yield_flag, await_flag)?;
                        match pot_ee {
                            Some((boxed_ee, after_ee)) => Some((Box::new(ExponentiationExpression::Exponentiation((boxed_ue, boxed_ee))), after_ee)),
                            None => None,
                        }
                    }
                    _ => None,
                }
            }
            None => None,
        };
        if result.is_none() {
            let pot_unary = UnaryExpression::parse(parser, scanner, yield_flag, await_flag)?;
            result = match pot_unary {
                Some((boxed_unary, after_unary)) => Some((Box::new(ExponentiationExpression::UnaryExpression(boxed_unary)), after_unary)),
                None => None,
            }
        }
        Ok(result)
    }
}

#[derive(Debug)]
pub enum AwaitExpression {
    Await(Box<UnaryExpression>),
}

impl fmt::Display for AwaitExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let AwaitExpression::Await(boxed) = &self;
        write!(f, "await {}", boxed)
    }
}

impl PrettyPrint for AwaitExpression {
    fn pprint(&self) {
        self.pprint_with_leftpad("", Spot::Initial);
    }

    fn pprint_with_leftpad(&self, pad: &str, state: Spot) {
        let (first, successive) = prettypad(pad, state);
        println!("{}AwaitExpression: {}", first, self);
        let AwaitExpression::Await(boxed) = &self;
        boxed.pprint_with_leftpad(&successive, Spot::Final);
    }
}

impl AwaitExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        Ok(None)
    }
}
//////// 13.2 Block

// StatementList[Yield, Await, Return]:
//    StatementListItem[?Yield, ?Await, ?Return]
//    StatementList[?Yield, ?Await, ?Return] StatementListItem[?Yield, ?Await, ?Return]
#[derive(Debug)]
struct StatementlistStatementlistitem {
    statement_list_item: Box<StatementListItem>,
}
#[derive(Debug)]
struct StatementlistStatementlistStatementlistitem {
    statement_list: Box<StatementList>,
    statement_list_item: Box<StatementListItem>,
}
#[derive(Debug)]
enum StatementlistProduction {
    StatementListItem(StatementlistStatementlistitem),
    StatementListStatmentListItem(StatementlistStatementlistStatementlistitem),
}
#[derive(Debug)]
struct StatementList {
    yield_arg: bool,
    await_arg: bool,
    return_arg: bool,
    production: Box<StatementlistProduction>,
}

// StatementListItem[Yield, Await, Return]:
//    Statement[?Yield, ?Await, ?Return]
//    Declaration[?Yield, ?Await]
#[derive(Debug)]
struct StatementlistitemStatement {
    statement: Box<Statement>,
}
#[derive(Debug)]
struct StatementlistitemDeclaration {
    declaration: Box<Declaration>,
}
#[derive(Debug)]
enum StatementlistitemProduction {
    Statement(StatementlistitemStatement),
    Declaration(StatementlistitemDeclaration),
}
#[derive(Debug)]
struct StatementListItem {
    yield_arg: bool,
    await_arg: bool,
    return_arg: bool,
    production: StatementlistitemProduction,
}

// tbd
#[derive(Debug)]
struct Statement {
    faux: String,
}
#[derive(Debug)]
struct Declaration {
    faux: String,
}

//////// 15.1 Scripts

// Script:
//    ScriptBody[opt]
#[derive(Debug)]
struct Script {
    script_body: Option<Box<ScriptBody>>,
}

// ScriptBody:
//    StatementList[~Yield, ~Await, ~Return]
#[derive(Debug)]
struct ScriptBody {
    statement_list: Box<StatementList>,
}

#[derive(Debug)]
struct VM {
    // Holds the state for the virtual machine. Anything shared between execution contexts winds up here.
}

impl VM {
    fn new() -> VM {
        VM {}
    }

    fn compile(&mut self, _ast: &AST) -> Result<i32, String> {
        Ok(0)
    }

    fn run(&mut self) -> Result<i32, String> {
        Ok(0)
    }
}

use crate::scanner::Scanner;

fn script<'a>(_scanner: &'a mut Scanner) -> Result<Box<Script>, String> {
    //let ch = scanner.scan_token(ScanGoal::InputElementRegExp);
    Ok(Box::new(Script { script_body: None }))
}

#[derive(Debug)]
struct AST {
    script: Box<Script>,
}
impl AST {
    fn generate(_source: &str) -> Result<Box<AST>, String> {
        let mut scanner = Scanner::new();
        script(&mut scanner).and_then(|s| Ok(Box::new(AST { script: s })))
    }
}

fn generate_ast<'a>(_vm: &'a VM, source: &'a str) -> Result<Box<AST>, String> {
    AST::generate(source)
}

fn interpret(_vm: &mut VM, source: &str) -> Result<i32, String> {
    //generate_ast(vm, source).and_then(|ast| vm.compile(&ast)).and_then(|_| vm.run())
    // let result = scanner::scan_token(
    //     &Scanner::new(),
    //     source,
    //     scanner::ScanGoal::InputElementRegExp,
    // );
    let mut parser = Parser::new(source, false, ParseGoal::Script);
    let result = expression(&mut parser, true, false, false);
    match result {
        Ok(Some((node, scanner))) => {
            node.pprint();
            Ok(0)
        }
        Ok(None) => Ok(0),
        Err(msg) => Err(msg),
    }
}

fn repl(vm: &mut VM) {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        io::stdin().read_line(&mut line).expect("Failed to read line");
        let linelen = line.len();
        if linelen == 0 {
            println!("");
            break;
        }

        println!("You entered the string {:?}", line);
        match interpret(vm, &line) {
            Ok(value) => println!("{}", value),
            Err(err) => println!("{}", err),
        }
    }
}

fn run_file(_vm: &mut VM, fname: &str) {
    println!("Running from the file {}", fname);
}

fn run_app() -> Result<(), i32> {
    let mut vm: VM = VM::new();
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => repl(&mut vm),
        2 => run_file(&mut vm, &args[1]),
        _ => {
            eprintln!("Usage: {} [path]", &args[0]);
            return Err(2);
        }
    }

    Ok(())
}

fn main() {
    std::process::exit(match run_app() {
        Ok(_) => 0,
        Err(err) => err,
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    fn id_kwd_test(kwd: &str) {
        let result = Identifier::parse(&mut super::Parser::new(kwd, false, super::ParseGoal::Script), Scanner::new());
        assert!(result.is_ok());
        assert!(result.unwrap().is_none());
    }
    #[test]
    fn identifier_test_await() {
        id_kwd_test("await")
    }
    #[test]
    fn identifier_test_break() {
        id_kwd_test("break")
    }
    #[test]
    fn identifier_test_case() {
        id_kwd_test("case")
    }
    #[test]
    fn identifier_test_catch() {
        id_kwd_test("catch")
    }
    #[test]
    fn identifier_test_class() {
        id_kwd_test("class")
    }
    #[test]
    fn identifier_test_const() {
        id_kwd_test("const")
    }
    #[test]
    fn identifier_test_continue() {
        id_kwd_test("continue")
    }
    #[test]
    fn identifier_test_debugger() {
        id_kwd_test("debugger")
    }
    #[test]
    fn identifier_test_default() {
        id_kwd_test("default")
    }
    #[test]
    fn identifier_test_delete() {
        id_kwd_test("delete")
    }
    #[test]
    fn identifier_test_do() {
        id_kwd_test("do")
    }
    #[test]
    fn identifier_test_else() {
        id_kwd_test("else")
    }
    #[test]
    fn identifier_test_enum() {
        id_kwd_test("enum")
    }
    #[test]
    fn identifier_test_export() {
        id_kwd_test("export")
    }
    #[test]
    fn identifier_test_extends() {
        id_kwd_test("extends")
    }
    #[test]
    fn identifier_test_false() {
        id_kwd_test("false")
    }
    #[test]
    fn identifier_test_finally() {
        id_kwd_test("finally")
    }
    #[test]
    fn identifier_test_for() {
        id_kwd_test("for")
    }
    #[test]
    fn identifier_test_function() {
        id_kwd_test("function")
    }
    #[test]
    fn identifier_test_if() {
        id_kwd_test("if")
    }
    #[test]
    fn identifier_test_import() {
        id_kwd_test("import")
    }
    #[test]
    fn identifier_test_in() {
        id_kwd_test("in")
    }
    #[test]
    fn identifier_test_instanceof() {
        id_kwd_test("instanceof")
    }
    #[test]
    fn identifier_test_new() {
        id_kwd_test("new")
    }
    #[test]
    fn identifier_test_null() {
        id_kwd_test("null")
    }
    #[test]
    fn identifier_test_return() {
        id_kwd_test("return")
    }
    #[test]
    fn identifier_test_super() {
        id_kwd_test("super")
    }
    #[test]
    fn identifier_test_switch() {
        id_kwd_test("switch")
    }
    #[test]
    fn identifier_test_this() {
        id_kwd_test("this")
    }
    #[test]
    fn identifier_test_throw() {
        id_kwd_test("throw")
    }
    #[test]
    fn identifier_test_true() {
        id_kwd_test("true")
    }
    #[test]
    fn identifier_test_try() {
        id_kwd_test("try")
    }
    #[test]
    fn identifier_test_typeof() {
        id_kwd_test("typeof")
    }
    #[test]
    fn identifier_test_var() {
        id_kwd_test("var")
    }
    #[test]
    fn identifier_test_void() {
        id_kwd_test("void")
    }
    #[test]
    fn identifier_test_while() {
        id_kwd_test("while")
    }
    #[test]
    fn identifier_test_with() {
        id_kwd_test("with")
    }
    #[test]
    fn identifier_test_yield() {
        id_kwd_test("yield")
    }
    #[test]
    fn identifier_test_err() {
        let result = Identifier::parse(&mut super::Parser::new("iden\\u{20}tifier", false, super::ParseGoal::Script), Scanner::new());
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "1:5: Invalid Identifier Continuation Character ' '")
    }
    fn identifier_test_strict(kwd: &str) {
        let result = Identifier::parse(&mut super::Parser::new(kwd, true, super::ParseGoal::Script), Scanner::new());
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), format!("1:1: ‘{}’ not allowed as an identifier in strict mode", kwd));
    }
    #[test]
    fn identifier_test_strict_implements() {
        identifier_test_strict("implements")
    }
    #[test]
    fn identifier_test_strict_interface() {
        identifier_test_strict("interface")
    }
    #[test]
    fn identifier_test_strict_let() {
        identifier_test_strict("let")
    }
    #[test]
    fn identifier_test_strict_package() {
        identifier_test_strict("package")
    }
    #[test]
    fn identifier_test_strict_private() {
        identifier_test_strict("private")
    }
    #[test]
    fn identifier_test_strict_protected() {
        identifier_test_strict("protected")
    }
    #[test]
    fn identifier_test_strict_public() {
        identifier_test_strict("public")
    }
    #[test]
    fn identifier_test_strict_static() {
        identifier_test_strict("static")
    }
    #[test]
    fn identifier_test_await_module() {
        let result = Identifier::parse(&mut Parser::new("aw\\u0061it", false, ParseGoal::Module), Scanner::new());
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "1:1: ‘await’ not allowed as an identifier in modules");
    }
    #[test]
    fn identifier_test_nothing() {
        let result = Identifier::parse(&mut Parser::new(".", false, ParseGoal::Script), Scanner::new());
        assert!(result.is_ok());
        assert!(result.unwrap().is_none());
    }
    fn identifier_test_keyword(kwd: &str) {
        let firstch = kwd.chars().next().unwrap();
        let id_src = format!("\\u{{{:x}}}{}", firstch as u32, &kwd[firstch.len_utf8()..]);
        let result = Identifier::parse(&mut super::Parser::new(&id_src, false, super::ParseGoal::Script), Scanner::new());
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), format!("1:1: ‘{}’ is a reserved word and may not be used as an identifier", kwd));
    }
    #[test]
    fn identifier_test_keyword_break() {
        identifier_test_keyword("break")
    }
    #[test]
    fn identifier_test_keyword_case() {
        identifier_test_keyword("case")
    }
    #[test]
    fn identifier_test_keyword_catch() {
        identifier_test_keyword("catch")
    }
    #[test]
    fn identifier_test_keyword_class() {
        identifier_test_keyword("class")
    }
    #[test]
    fn identifier_test_keyword_const() {
        identifier_test_keyword("const")
    }
    #[test]
    fn identifier_test_keyword_continue() {
        identifier_test_keyword("continue")
    }
    #[test]
    fn identifier_test_keyword_debugger() {
        identifier_test_keyword("debugger")
    }
    #[test]
    fn identifier_test_keyword_default() {
        identifier_test_keyword("default")
    }
    #[test]
    fn identifier_test_keyword_delete() {
        identifier_test_keyword("delete")
    }
    #[test]
    fn identifier_test_keyword_do() {
        identifier_test_keyword("do")
    }
    #[test]
    fn identifier_test_keyword_else() {
        identifier_test_keyword("else")
    }
    #[test]
    fn identifier_test_keyword_enum() {
        identifier_test_keyword("enum")
    }
    #[test]
    fn identifier_test_keyword_export() {
        identifier_test_keyword("export")
    }
    #[test]
    fn identifier_test_keyword_extends() {
        identifier_test_keyword("extends")
    }
    #[test]
    fn identifier_test_keyword_false() {
        identifier_test_keyword("false")
    }
    #[test]
    fn identifier_test_keyword_finally() {
        identifier_test_keyword("finally")
    }
    #[test]
    fn identifier_test_keyword_for() {
        identifier_test_keyword("for")
    }
    #[test]
    fn identifier_test_keyword_function() {
        identifier_test_keyword("function")
    }
    #[test]
    fn identifier_test_keyword_if() {
        identifier_test_keyword("if")
    }
    #[test]
    fn identifier_test_keyword_import() {
        identifier_test_keyword("import")
    }
    #[test]
    fn identifier_test_keyword_in() {
        identifier_test_keyword("in")
    }
    #[test]
    fn identifier_test_keyword_instanceof() {
        identifier_test_keyword("instanceof")
    }
    #[test]
    fn identifier_test_keyword_new() {
        identifier_test_keyword("new")
    }
    #[test]
    fn identifier_test_keyword_null() {
        identifier_test_keyword("null")
    }
    #[test]
    fn identifier_test_keyword_return() {
        identifier_test_keyword("return")
    }
    #[test]
    fn identifier_test_keyword_super() {
        identifier_test_keyword("super")
    }
    #[test]
    fn identifier_test_keyword_switch() {
        identifier_test_keyword("switch")
    }
    #[test]
    fn identifier_test_keyword_this() {
        identifier_test_keyword("this")
    }
    #[test]
    fn identifier_test_keyword_throw() {
        identifier_test_keyword("throw")
    }
    #[test]
    fn identifier_test_keyword_true() {
        identifier_test_keyword("true")
    }
    #[test]
    fn identifier_test_keyword_try() {
        identifier_test_keyword("try")
    }
    #[test]
    fn identifier_test_keyword_typeof() {
        identifier_test_keyword("typeof")
    }
    #[test]
    fn identifier_test_keyword_var() {
        identifier_test_keyword("var")
    }
    #[test]
    fn identifier_test_keyword_void() {
        identifier_test_keyword("void")
    }
    #[test]
    fn identifier_test_keyword_while() {
        identifier_test_keyword("while")
    }
    #[test]
    fn identifier_test_keyword_with() {
        identifier_test_keyword("with")
    }
    #[test]
    fn identifier_test_successful_bob() {
        let result = Identifier::parse(&mut super::Parser::new("bob", true, super::ParseGoal::Script), Scanner::new());
        assert!(result.is_ok());
        let optional_id = result.unwrap();
        assert!(optional_id.is_some());
        let (identifier, scanner) = optional_id.unwrap();
        assert_eq!(scanner, super::Scanner { line: 1, column: 4, start_idx: 3 });
        let Identifier::IdentifierName(data) = *identifier;
        assert_eq!(data.string_value, "bob");
        assert_eq!(data.keyword_id, None);
        assert_eq!(data.line, 1);
        assert_eq!(data.column, 1);
    }
    #[test]
    fn identifier_test_successful_japanese() {
        let text = "手がける黒田征太郎さんです";
        let result = Identifier::parse(&mut super::Parser::new(text, true, super::ParseGoal::Script), Scanner::new());
        assert!(result.is_ok());
        let optional_id = result.unwrap();
        assert!(optional_id.is_some());
        let (identifier, scanner) = optional_id.unwrap();
        assert_eq!(scanner, super::Scanner { line: 1, column: 14, start_idx: 39 });
        let Identifier::IdentifierName(data) = *identifier;
        assert_eq!(data.string_value, "手がける黒田征太郎さんです");
        assert_eq!(data.keyword_id, None);
        assert_eq!(data.line, 1);
        assert_eq!(data.column, 1);
    }

    #[test]
    fn identifier_reference_test_debug() {
        assert_eq!(
            format!(
                "{:?}",
                IdentifierReference {
                    kind: IdentifierReferenceKind::Yield,
                    strict: false
                }
            ),
            "IdentifierReference { kind: Yield, strict: false }"
        );
    }
    fn idref_create(text: &str, strict: bool) -> Box<IdentifierReference> {
        let yield_syntax = false;
        let await_syntax = false;
        let result = IdentifierReference::parse(&mut Parser::new(text, strict, ParseGoal::Script), Scanner::new(), yield_syntax, await_syntax);
        assert!(result.is_ok());
        let optional_idref = result.unwrap();
        assert!(optional_idref.is_some());
        let (idref, scanner) = optional_idref.unwrap();
        assert_eq!(
            scanner,
            Scanner {
                line: 1,
                column: text.len() as u32 + 1,
                start_idx: text.len()
            }
        );
        idref
    }

    #[test]
    fn identifier_reference_test_simple_success() {
        let idref = idref_create("identifier", false);
        assert_eq!(idref.strict, false);
        use IdentifierReferenceKind::*;
        match &idref.kind {
            Yield | Await => assert!(false, "Wrong IdentifierReference Kind (expected Identifier)"),
            Identifier(_) => (),
        }

        assert_eq!(idref.string_value(), "identifier");
        assert_eq!(idref.assignment_target_type(), ATTKind::Simple)
    }
    #[test]
    fn identifier_reference_test_yield() {
        let idref = idref_create("yield", false);
        assert_eq!(idref.strict, false);
        assert!(matches!(idref.kind, IdentifierReferenceKind::Yield));
        assert_eq!(idref.string_value(), "yield");
        assert_eq!(idref.assignment_target_type(), ATTKind::Simple)
    }
    #[test]
    fn identifier_reference_test_yield_02() {
        let idref = IdentifierReference::parse(&mut Parser::new("yield", false, ParseGoal::Script), Scanner::new(), true, true);
        assert!(idref.is_ok());
        assert!(idref.unwrap().is_none());
    }
    #[test]
    fn identifier_reference_test_await() {
        let idref = idref_create("await", false);
        assert_eq!(idref.strict, false);
        assert!(matches!(idref.kind, IdentifierReferenceKind::Await));
        assert_eq!(idref.string_value(), "await");
        assert_eq!(idref.assignment_target_type(), ATTKind::Simple)
    }
    #[test]
    fn identifier_reference_test_await_02() {
        let idref = IdentifierReference::parse(&mut Parser::new("await", false, ParseGoal::Script), Scanner::new(), true, true);
        assert!(idref.is_ok());
        assert!(idref.unwrap().is_none());
    }
    #[test]
    fn identifier_reference_test_kwd() {
        let idref = IdentifierReference::parse(&mut Parser::new("new", false, ParseGoal::Script), Scanner::new(), true, true);
        assert!(idref.is_ok());
        assert!(idref.unwrap().is_none());
    }
    #[test]
    fn identifier_reference_test_punct() {
        let idref = IdentifierReference::parse(&mut Parser::new("**", false, ParseGoal::Script), Scanner::new(), true, true);
        assert!(idref.is_ok());
        assert!(idref.unwrap().is_none());
    }
    #[test]
    fn identifier_reference_test_att_strict() {
        let idref = idref_create("abcd", true);
        assert_eq!(idref.string_value(), "abcd");
        assert_eq!(idref.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn identifier_reference_test_eval_strict() {
        let idref = idref_create("eval", true);
        assert_eq!(idref.string_value(), "eval");
        assert_eq!(idref.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn identifier_reference_test_eval_loose() {
        let idref = idref_create("eval", false);
        assert_eq!(idref.string_value(), "eval");
        assert_eq!(idref.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn identifier_reference_test_arguments_strict() {
        let idref = idref_create("arguments", true);
        assert_eq!(idref.string_value(), "arguments");
        assert_eq!(idref.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn identifier_reference_test_arguments_loose() {
        let idref = idref_create("arguments", false);
        assert_eq!(idref.string_value(), "arguments");
        assert_eq!(idref.assignment_target_type(), ATTKind::Simple);
    }

    fn bindingid_create(text: &str, y: bool, a: bool) -> Box<BindingIdentifier> {
        let yield_syntax = y;
        let await_syntax = a;
        let strict = false;
        let result = binding_identifier(&mut Parser::new(text, strict, ParseGoal::Script), yield_syntax, await_syntax);
        assert!(result.is_ok());
        let optional_bid = result.unwrap();
        assert!(optional_bid.is_some());
        let (bid, scanner) = optional_bid.unwrap();
        assert_eq!(
            scanner,
            Scanner {
                line: 1,
                column: text.len() as u32 + 1,
                start_idx: text.len()
            }
        );
        bid
    }

    fn bid_allflags(text: &str) {
        for yflag in [false, true].iter() {
            for aflag in [false, true].iter() {
                let bid = bindingid_create(text, *yflag, *aflag);
                assert_eq!(bid.string_value(), text);
                assert_eq!(bid.bound_names(), [text]);
                assert_eq!(bid.yield_flag, *yflag);
                assert_eq!(bid.await_flag, *aflag);
            }
        }
    }

    #[test]
    fn binding_identifier_test_normal() {
        bid_allflags("green");
    }
    #[test]
    fn binding_identifier_test_yield() {
        bid_allflags("yield");
    }
    #[test]
    fn binding_identifier_test_await() {
        bid_allflags("await");
    }
    #[test]
    fn binding_identifier_test_debug() {
        format!("{:?}", bindingid_create("abcd", true, true));
    }
    #[test]
    fn binding_identifier_test_non_matches() {
        let mut p1 = Parser::new("function", false, ParseGoal::Script);
        let r1 = binding_identifier(&mut p1, false, false);
        assert!(r1.is_ok());
        assert!(r1.unwrap().is_none());
        let mut p2 = Parser::new("**", false, ParseGoal::Script);
        let r2 = binding_identifier(&mut p2, false, false);
        assert!(r2.is_ok());
        assert!(r2.unwrap().is_none());
    }

    fn check<T>(res: Result<Option<(Box<T>, Scanner)>, String>) -> (Box<T>, Scanner) {
        assert!(res.is_ok());
        let potential = res.unwrap();
        assert!(potential.is_some());
        potential.unwrap()
    }
    fn check_none<T>(res: Result<Option<(Box<T>, Scanner)>, String>) {
        assert!(res.is_ok());
        let potential = res.unwrap();
        assert!(potential.is_none());
    }
    fn chk_scan(scanner: &Scanner, count: u32) {
        assert_eq!(
            *scanner,
            Scanner {
                line: 1,
                column: count + 1,
                start_idx: count as usize
            }
        );
    }
    fn newparser(text: &str) -> Parser {
        Parser::new(text, false, ParseGoal::Script)
    }
    // PRIMARY EXPRESSION
    #[test]
    fn primary_expression_test_debug() {
        let pe = primary_expression(&mut newparser("this"), false, false);
        let (exp, _) = check(pe);
        assert_eq!(format!("{:?}", exp), "PrimaryExpression { kind: This }");
    }
    #[test]
    fn primary_expression_test_idref() {
        let pe_res = primary_expression(&mut newparser("blue"), false, false);
        let (boxed_pe, scanner) = check(pe_res);
        chk_scan(&scanner, 4);
        assert!(matches!(boxed_pe.kind, PrimaryExpressionKind::IdentifierReference(_)));
        assert_eq!(boxed_pe.is_function_definition(), false);
        assert_eq!(boxed_pe.is_identifier_reference(), true);
        assert_eq!(boxed_pe.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn primary_expression_test_literal() {
        let (node, scanner) = check(primary_expression(&mut newparser("371"), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(node.kind, PrimaryExpressionKind::Literal(_)));
        assert_eq!(node.is_function_definition(), false);
        assert_eq!(node.is_identifier_reference(), false);
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn primary_expression_test_this() {
        let (node, scanner) = check(primary_expression(&mut newparser("this"), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(node.kind, PrimaryExpressionKind::This));
        assert_eq!(node.is_function_definition(), false);
        assert_eq!(node.is_identifier_reference(), false);
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }

    #[test]
    fn this_token_test_debug() {
        assert_eq!(format!("{:?}", ThisToken {}), "ThisToken");
    }
    #[test]
    fn this_token_test_01() {
        let (_, scanner) = check(this_token(&mut newparser("this")));
        chk_scan(&scanner, 4);
    }
    #[test]
    fn this_token_test_02() {
        check_none(this_token(&mut newparser("**")));
    }

    // LITERAL
    #[test]
    fn literal_test_debug() {
        assert_eq!(format!("{:?}", Literal { kind: LiteralKind::NullLiteral }), "Literal { kind: NullLiteral }");
    }
    #[test]
    fn literal_test_null() {
        let (lit, scanner) = check(literal(&mut newparser("null")));
        chk_scan(&scanner, 4);
        assert!(matches!(lit.kind, LiteralKind::NullLiteral));
    }
    #[test]
    fn literal_test_boolean_01() {
        let (lit, scanner) = check(literal(&mut newparser("true")));
        chk_scan(&scanner, 4);
        assert!(matches!(lit.kind, LiteralKind::BooleanLiteral(true)));
    }
    #[test]
    fn literal_test_boolean_02() {
        let (lit, scanner) = check(literal(&mut newparser("false")));
        chk_scan(&scanner, 5);
        assert!(matches!(lit.kind, LiteralKind::BooleanLiteral(false)));
    }
    #[test]
    fn literal_test_leading_dot() {
        let (lit, scanner) = check(literal(&mut newparser(".25")));
        chk_scan(&scanner, 3);
        assert_eq!(lit.kind, LiteralKind::NumericLiteral(Numeric::Number(0.25)))
    }
    #[test]
    fn literal_test_bigint() {
        let (lit, scanner) = check(literal(&mut newparser("7173n")));
        chk_scan(&scanner, 5);
        assert!(matches!(lit.kind, LiteralKind::NumericLiteral(Numeric::BigInt(_))));
    }
    #[test]
    fn literal_test_string() {
        let (lit, scanner) = check(literal(&mut newparser("'string'")));
        chk_scan(&scanner, 8);
        assert!(matches!(lit.kind, LiteralKind::StringLiteral(_)));
    }
    #[test]
    fn literal_test_keyword() {
        check_none(literal(&mut newparser("function")));
    }
    #[test]
    fn literal_test_punct() {
        check_none(literal(&mut newparser("**")));
    }

    // MEMBER EXPRESSION
    #[test]
    fn member_expression_test_primary_expression() {
        let (me, scanner) = check(member_expression(&mut newparser("6"), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(me.kind, MemberExpressionKind::PrimaryExpression(_)));
        // Excersize the Debug formatter, for code coverage
        format!("{:?}", me);
    }
    #[test]
    fn member_expression_test_meta_property() {
        let (me, scanner) = check(member_expression(&mut newparser("new.target"), false, false));
        chk_scan(&scanner, 10);
        assert!(matches!(me.kind, MemberExpressionKind::MetaProperty(_)));
        // Excersize the Debug formatter, for code coverage
        format!("{:?}", me);
    }
    #[test]
    fn member_expression_test_super_property() {
        let (me, scanner) = check(member_expression(&mut newparser("super.ior"), false, false));
        chk_scan(&scanner, 9);
        assert!(matches!(me.kind, MemberExpressionKind::SuperProperty(_)));
        // Excersize the Debug formatter, for code coverage
        format!("{:?}", me);
    }
    #[test]
    fn member_expression_test_new_me_args() {
        let (me, scanner) = check(member_expression(&mut newparser("new shoes('red', 'leather')"), false, false));
        chk_scan(&scanner, 27);
        assert!(matches!(me.kind, MemberExpressionKind::NewArguments(_)));
        // Excersize the Debug formatter, for code coverage
        format!("{:?}", me);
    }
    #[test]
    fn member_expression_test_me_expression() {
        let (me, scanner) = check(member_expression(&mut newparser("bill[3]"), false, false));
        chk_scan(&scanner, 7);
        assert!(matches!(me.kind, MemberExpressionKind::Expression(_)));
        // Excersize the Debug formatter, for code coverage
        format!("{:?}", me);
    }
    #[test]
    fn member_expression_test_me_ident() {
        let (me, scanner) = check(member_expression(&mut newparser("alice.name"), false, false));
        chk_scan(&scanner, 10);
        assert!(matches!(me.kind, MemberExpressionKind::IdentifierName(_)));
        // Excersize the Debug formatter, for code coverage
        format!("{:?}", me);
    }
    #[test]
    fn member_expression_test_bad_ident() {
        let r = member_expression(&mut newparser("alice.'pool'"), false, false);
        assert!(r.is_err());
    }
    #[test]
    fn member_expression_test_bad_expr() {
        let r = member_expression(&mut newparser("alice[while]"), false, false);
        assert!(r.is_err());
    }
    #[test]
    fn member_expression_test_bad_expr_close() {
        let r = member_expression(&mut newparser("alice[73"), false, false);
        assert!(r.is_err());
    }

    // SUPER PROPERTY
    #[test]
    fn super_property_test_expression() {
        let (sp, scanner) = check(super_property(&mut newparser("super[3]"), false, false));
        chk_scan(&scanner, 8);
        assert!(matches!(sp.kind, SuperPropertyKind::Expression(_)));
        // Excersize the Debug formatter, for code coverage
        format!("{:?}", sp);
    }
    #[test]
    fn super_property_test_ident() {
        let (sp, scanner) = check(super_property(&mut newparser("super.bob"), false, false));
        chk_scan(&scanner, 9);
        assert!(matches!(sp.kind, SuperPropertyKind::IdentifierName(_)));
        // Excersize the Debug formatter, for code coverage
        format!("{:?}", sp);
    }
    #[test]
    fn super_property_test_nomatch() {
        check_none(super_property(&mut newparser("silly"), false, false));
    }
    #[test]
    fn super_property_test_bad_ident() {
        let r = super_property(&mut newparser("super.**"), false, false);
        assert!(r.is_err());
    }
    #[test]
    fn super_property_test_bad_expression() {
        let r = super_property(&mut newparser("super[while]"), false, false);
        assert!(r.is_err());
    }
    #[test]
    fn super_property_test_incomplete_expression() {
        let r = super_property(&mut newparser("super[99"), false, false);
        assert!(r.is_err());
    }
    #[test]
    fn super_property_test_bad_following_token() {
        check_none(super_property(&mut newparser("super duper"), false, false));
    }

    // META PROPERTY
    #[test]
    fn meta_property_test_newtarget() {
        let (mp, scanner) = check(meta_property(&mut newparser("new.target")));
        chk_scan(&scanner, 10);
        assert!(matches!(mp.kind, MetaPropertyKind::NewTarget));
        format!("{:?}", mp);
    }
    #[test]
    fn meta_property_test_importmeta() {
        let (mp, scanner) = check(meta_property(&mut newparser("import.meta")));
        chk_scan(&scanner, 11);
        assert!(matches!(mp.kind, MetaPropertyKind::ImportMeta));
        format!("{:?}", mp);
    }
    #[test]
    fn meta_property_test_nomatch_01() {
        check_none(meta_property(&mut newparser("silly")));
    }
    #[test]
    fn meta_property_test_nomatch_02() {
        check_none(meta_property(&mut newparser("new silly")));
    }
    #[test]
    fn meta_property_test_nomatch_03() {
        check_none(meta_property(&mut newparser("new.silly")));
    }
    #[test]
    fn meta_property_test_nomatch_04() {
        check_none(meta_property(&mut newparser("import silly")));
    }
    #[test]
    fn meta_property_test_nomatch_05() {
        check_none(meta_property(&mut newparser("import.silly")));
    }

    // ARGUMENTS
    #[test]
    fn arguments_test_onlyparens() {
        let (args, scanner) = check(arguments(&mut newparser("()"), false, false));
        chk_scan(&scanner, 2);
        assert!(matches!(args.kind, ArgumentsKind::Empty));
        format!("{:?}", args);
    }
    #[test]
    fn arguments_test_trailing_comma() {
        let (args, scanner) = check(arguments(&mut newparser("(3,)"), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(args.kind, ArgumentsKind::ArgumentListComma(_)));
        format!("{:?}", args);
    }
    #[test]
    fn arguments_test_arglist() {
        let (args, scanner) = check(arguments(&mut newparser("(4,5)"), false, false));
        chk_scan(&scanner, 5);
        assert!(matches!(args.kind, ArgumentsKind::ArgumentList(_)));
        format!("{:?}", args);
    }
    #[test]
    fn arguments_test_nomatch() {
        check_none(arguments(&mut newparser("**"), false, false));
    }
    #[test]
    fn arguments_test_unclosed_01() {
        check_none(arguments(&mut newparser("("), false, false));
    }
    #[test]
    fn arguments_test_unclosed_02() {
        check_none(arguments(&mut newparser("(88"), false, false));
    }
    #[test]
    fn arguments_test_unclosed_03() {
        check_none(arguments(&mut newparser("(91,"), false, false));
    }

    // ARGUMENT LIST
    #[test]
    fn argument_list_test_ae() {
        let (al, scanner) = check(ArgumentList::parse(&mut newparser("101"), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(al.kind, ArgumentListKind::AssignmentExpression(_)));
        format!("{:?}", al);
    }
    #[test]
    fn argument_list_test_dots_ae() {
        let (al, scanner) = check(ArgumentList::parse(&mut newparser("...101"), false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(al.kind, ArgumentListKind::DotsAssignmentExpression(_)));
        format!("{:?}", al);
    }
    #[test]
    fn argument_list_test_al_ae() {
        let (al, scanner) = check(ArgumentList::parse(&mut newparser("10,101"), false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(al.kind, ArgumentListKind::ArgumentListAssignmentExpression(_)));
        format!("{:?}", al);
    }
    #[test]
    fn argument_list_test_al_dots_ae() {
        let (al, scanner) = check(ArgumentList::parse(&mut newparser("10,...101"), false, false));
        chk_scan(&scanner, 9);
        assert!(matches!(al.kind, ArgumentListKind::ArgumentListDotsAssignmentExpression(_)));
        format!("{:?}", al);
    }
    #[test]
    fn argument_list_test_nomatch() {
        check_none(ArgumentList::parse(&mut newparser("**"), false, false));
    }
    #[test]
    fn argument_list_test_dotsonly() {
        assert!(ArgumentList::parse(&mut newparser("..."), false, false).is_err());
    }
    #[test]
    fn argument_list_test_dots_term() {
        let (al, scanner) = check(ArgumentList::parse(&mut newparser("10,..."), false, false));
        chk_scan(&scanner, 2);
        assert!(matches!(al.kind, ArgumentListKind::AssignmentExpression(_)));
    }
    #[test]
    fn argument_list_test_commas() {
        let (al, scanner) = check(ArgumentList::parse(&mut newparser("10,,10"), false, false));
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
    }
    #[test]
    fn new_expression_test_new() {
        let (ne, scanner) = check(NewExpression::parse(&mut newparser("new bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 7);
        assert!(matches!(ne.kind, NewExpressionKind::NewExpression(_)));
        format!("{:?}", ne);
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
    }
    #[test]
    fn call_expression_test_super() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("super()"), Scanner::new(), false, false));
        chk_scan(&scanner, 7);
        assert!(matches!(ce.kind, CallExpressionKind::SuperCall(_)));
        format!("{:?}", ce);
    }
    #[test]
    fn call_expression_test_import() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("import(pop)"), Scanner::new(), false, false));
        chk_scan(&scanner, 11);
        assert!(matches!(ce.kind, CallExpressionKind::ImportCall(_)));
        format!("{:?}", ce);
    }
    #[test]
    fn call_expression_test_ce_args() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop)(snap)(10)(20)"), Scanner::new(), false, false));
        chk_scan(&scanner, 23);
        assert!(matches!(ce.kind, CallExpressionKind::CallExpressionArguments(_)));
        format!("{:?}", ce);
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
    }
    #[test]
    fn call_expression_test_ce_ident() {
        let (ce, scanner) = check(CallExpression::parse(&mut newparser("blue(pop).snap"), Scanner::new(), false, false));
        chk_scan(&scanner, 14);
        assert!(matches!(ce.kind, CallExpressionKind::CallExpressionIdentifierName(_)));
        format!("{:?}", ce);
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

    // UPDATE EXPRESSION
    #[test]
    fn update_expression_lhs() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("78"), Scanner::new(), false, false));
        chk_scan(&scanner, 2);
        assert!(matches!(*ue, UpdateExpression::LeftHandSideExpression(_)));
        format!("{:?}", ue);
    }
    #[test]
    fn update_expression_test_preinc() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("++a"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UpdateExpression::PreIncrement(_)));
        format!("{:?}", ue);
    }
    #[test]
    fn update_expression_test_predec() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("--a"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UpdateExpression::PreDecrement(_)));
        format!("{:?}", ue);
    }
    #[test]
    fn update_expression_test_postinc() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("a++"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UpdateExpression::PostIncrement(_)));
        format!("{:?}", ue);
    }
    #[test]
    fn update_expression_test_postdec() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("a--"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UpdateExpression::PostDecrement(_)));
        format!("{:?}", ue);
    }
    #[test]
    fn update_expression_test_newline() {
        let (ue, scanner) = check(UpdateExpression::parse(&mut newparser("a\n++"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(*ue, UpdateExpression::LeftHandSideExpression(_)));
    }
    #[test]
    fn update_expression_test_nomatch() {
        check_none(UpdateExpression::parse(&mut newparser("**"), Scanner::new(), false, false));
    }
    #[test]
    fn update_expression_test_syntax_error_01() {
        check_none(UpdateExpression::parse(&mut newparser("++ ++"), Scanner::new(), false, false));
    }
    #[test]
    fn update_expression_test_syntax_error_02() {
        check_none(UpdateExpression::parse(&mut newparser("-- ++"), Scanner::new(), false, false));
    }

    // UNARY EXPRESSION
    #[test]
    fn unary_expression_test_update_expression() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("900"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(*ue, UnaryExpression::UpdateExpression(_)));
    }
    #[test]
    fn unary_expression_test_delete() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("delete bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 10);
        assert!(matches!(*ue, UnaryExpression::Delete(_)));
    }
    #[test]
    fn unary_expression_test_void() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("void bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 8);
        assert!(matches!(*ue, UnaryExpression::Void(_)));
    }
    #[test]
    fn unary_expression_test_typeof() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("typeof bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 10);
        assert!(matches!(*ue, UnaryExpression::Typeof(_)));
    }
    #[test]
    fn unary_expression_test_numberify() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("+bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*ue, UnaryExpression::NoOp(_)));
    }
    #[test]
    fn unary_expression_test_negate() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("-bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*ue, UnaryExpression::Negate(_)));
    }
    #[test]
    fn unary_expression_test_complement() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("~bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*ue, UnaryExpression::Complement(_)));
    }
    #[test]
    fn unary_expression_test_not() {
        let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("!bob"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*ue, UnaryExpression::Not(_)));
    }
    #[test]
    fn unary_expression_test_await() {
        //let (ue, scanner) = check(UnaryExpression::parse(&mut newparser("await bob"), Scanner::new(), false, true));
        //chk_scan(&scanner, 9);
        //assert!(matches!(*ue, UnaryExpression::Await(_)));

        // Use the prior lines when AwaitExpression gets implemented.
        check_none(UnaryExpression::parse(&mut newparser("await bob"), Scanner::new(), false, true));
    }
    #[test]
    fn unary_expression_test_nomatch() {
        check_none(UnaryExpression::parse(&mut newparser(""), Scanner::new(), false, false));
    }
    #[test]
    fn unary_expression_test_incomplete() {
        check_none(UnaryExpression::parse(&mut newparser("delete"), Scanner::new(), false, false));
        check_none(UnaryExpression::parse(&mut newparser("void"), Scanner::new(), false, false));
        check_none(UnaryExpression::parse(&mut newparser("typeof"), Scanner::new(), false, false));
        check_none(UnaryExpression::parse(&mut newparser("+"), Scanner::new(), false, false));
        check_none(UnaryExpression::parse(&mut newparser("-"), Scanner::new(), false, false));
        check_none(UnaryExpression::parse(&mut newparser("~"), Scanner::new(), false, false));
        check_none(UnaryExpression::parse(&mut newparser("!"), Scanner::new(), false, false));
    }
}
