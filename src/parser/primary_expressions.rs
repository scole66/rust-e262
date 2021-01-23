use num::bigint::BigInt;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::assignment_operators::AssignmentExpression;
use super::async_function_definitions::AsyncFunctionExpression;
use super::async_generator_function_definitions::AsyncGeneratorExpression;
use super::class_definitions::ClassExpression;
use super::comma_operator::Expression;
use super::function_definitions::FunctionExpression;
use super::generator_function_definitions::GeneratorExpression;
use super::identifiers::{IdentifierNameToken, IdentifierReference};
use super::method_definitions::MethodDefinition;
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};
use crate::values::number_to_string;

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
    ArrayLiteral(Box<ArrayLiteral>),
    ObjectLiteral(Box<ObjectLiteral>),
    Parenthesized(Box<ParenthesizedExpression>),
    TemplateLiteral(Box<TemplateLiteral>),
    Function(Box<FunctionExpression>),
    Class(Box<ClassExpression>),
    Generator(Box<GeneratorExpression>),
    AsyncFunction(Box<AsyncFunctionExpression>),
    AsyncGenerator(Box<AsyncGeneratorExpression>),
    RegularExpression(Box<scanner::RegularExpressionData>),
}

#[derive(Debug)]
pub struct PrimaryExpression {
    kind: PrimaryExpressionKind,
}

impl fmt::Display for PrimaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            PrimaryExpressionKind::This => write!(f, "this"),
            PrimaryExpressionKind::IdentifierReference(boxed) => boxed.fmt(f),
            PrimaryExpressionKind::Literal(boxed) => boxed.fmt(f),
            PrimaryExpressionKind::ArrayLiteral(boxed) => boxed.fmt(f),
            PrimaryExpressionKind::ObjectLiteral(boxed) => boxed.fmt(f),
            PrimaryExpressionKind::Parenthesized(boxed) => boxed.fmt(f),
            PrimaryExpressionKind::TemplateLiteral(boxed) => boxed.fmt(f),
            PrimaryExpressionKind::Function(node) => node.fmt(f),
            PrimaryExpressionKind::Class(node) => node.fmt(f),
            PrimaryExpressionKind::Generator(node) => node.fmt(f),
            PrimaryExpressionKind::AsyncFunction(node) => node.fmt(f),
            PrimaryExpressionKind::AsyncGenerator(node) => node.fmt(f),
            PrimaryExpressionKind::RegularExpression(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for PrimaryExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}PrimaryExpression: {}", first, self)?;
        match &self.kind {
            PrimaryExpressionKind::This => Ok(()),
            PrimaryExpressionKind::IdentifierReference(boxed) => {
                boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            PrimaryExpressionKind::Literal(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::ArrayLiteral(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::ObjectLiteral(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::Parenthesized(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::TemplateLiteral(boxed) => {
                boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            PrimaryExpressionKind::Function(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::Class(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::Generator(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::AsyncFunction(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::AsyncGenerator(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::RegularExpression(node) => Ok(()),
        }
    }
}

impl IsFunctionDefinition for PrimaryExpression {
    fn is_function_definition(&self) -> bool {
        use PrimaryExpressionKind::*;
        match &self.kind {
            This
            | IdentifierReference(_)
            | Literal(_)
            | ArrayLiteral(_)
            | ObjectLiteral(_)
            | TemplateLiteral(_)
            | RegularExpression(_) => false,
            Parenthesized(exp) => exp.is_function_definition(),
            Function(node) => node.is_function_definition(),
            Class(node) => node.is_function_definition(),
            Generator(node) => node.is_function_definition(),
            AsyncFunction(node) => node.is_function_definition(),
            AsyncGenerator(node) => node.is_function_definition(),
        }
    }
}

impl IsIdentifierReference for PrimaryExpression {
    fn is_identifier_reference(&self) -> bool {
        use PrimaryExpressionKind::*;
        match &self.kind {
            This | Literal(_) | ArrayLiteral(_) | ObjectLiteral(_) | Parenthesized(_) | TemplateLiteral(_)
            | RegularExpression(_) | Function(_) | Class(_) | Generator(_) | AsyncFunction(_) | AsyncGenerator(_) => {
                false
            }
            IdentifierReference(_) => true,
        }
    }
}

impl AssignmentTargetType for PrimaryExpression {
    fn assignment_target_type(&self) -> ATTKind {
        use PrimaryExpressionKind::*;
        match &self.kind {
            This | Literal(_) | ArrayLiteral(_) | ObjectLiteral(_) | TemplateLiteral(_) | RegularExpression(_)
            | Function(_) | Class(_) | Generator(_) | AsyncFunction(_) | AsyncGenerator(_) => ATTKind::Invalid,
            IdentifierReference(id) => id.assignment_target_type(),
            Parenthesized(expr) => expr.assignment_target_type(),
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

impl ToPrimaryExpressionKind for ArrayLiteral {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::ArrayLiteral(node)
    }
}

impl ToPrimaryExpressionKind for ObjectLiteral {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::ObjectLiteral(node)
    }
}

impl ToPrimaryExpressionKind for ParenthesizedExpression {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::Parenthesized(node)
    }
}

impl ToPrimaryExpressionKind for TemplateLiteral {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::TemplateLiteral(node)
    }
}

impl ToPrimaryExpressionKind for FunctionExpression {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::Function(node)
    }
}

impl ToPrimaryExpressionKind for ClassExpression {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::Class(node)
    }
}

impl ToPrimaryExpressionKind for GeneratorExpression {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::Generator(node)
    }
}

impl ToPrimaryExpressionKind for AsyncFunctionExpression {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::AsyncFunction(node)
    }
}

impl ToPrimaryExpressionKind for AsyncGeneratorExpression {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::AsyncGenerator(node)
    }
}

impl ToPrimaryExpressionKind for scanner::RegularExpressionData {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::RegularExpression(node)
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

fn or_pe_kind<F, T>(
    opt: Option<(Box<PrimaryExpression>, Scanner)>,
    parser: &mut Parser,
    parse_func: F,
) -> Result<Option<(Box<PrimaryExpression>, Scanner)>, String>
where
    F: FnOnce(&mut Parser) -> Result<Option<(Box<T>, Scanner)>, String>,
    T: ToPrimaryExpressionKind,
{
    opt.map_or_else(|| parse_func(parser).and_then(pe_boxer), rewrap)
}

impl PrimaryExpression {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        arg_yield: bool,
        arg_await: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        Ok(None)
            .and_then(|opt| {
                or_pe_kind(opt, parser, |p| {
                    IdentifierReference::parse(p, scanner, arg_yield, arg_await)
                })
            })
            .and_then(|opt| or_pe_kind(opt, parser, |p| Literal::parse(p, scanner)))
            .and_then(|opt| or_pe_kind(opt, parser, |p| this_token(p, scanner)))
            .and_then(|opt| or_pe_kind(opt, parser, |p| ArrayLiteral::parse(p, scanner, arg_yield, arg_await)))
            .and_then(|opt| or_pe_kind(opt, parser, |p| ObjectLiteral::parse(p, scanner, arg_yield, arg_await)))
            .and_then(|opt| {
                or_pe_kind(opt, parser, |p| {
                    ParenthesizedExpression::parse(p, scanner, arg_yield, arg_await)
                })
            })
            .and_then(|opt| {
                or_pe_kind(opt, parser, |p| {
                    TemplateLiteral::parse(p, scanner, arg_yield, arg_await, false)
                })
            })
            .and_then(|opt| or_pe_kind(opt, parser, |p| FunctionExpression::parse(p, scanner)))
            .and_then(|opt| {
                or_pe_kind(opt, parser, |p| {
                    ClassExpression::parse(p, scanner, arg_yield, arg_await)
                })
            })
            .and_then(|opt| or_pe_kind(opt, parser, |p| GeneratorExpression::parse(p, scanner)))
            .and_then(|opt| or_pe_kind(opt, parser, |p| AsyncFunctionExpression::parse(p, scanner)))
            .and_then(|opt| or_pe_kind(opt, parser, |p| AsyncGeneratorExpression::parse(p, scanner)))
            .and_then(|opt| or_pe_kind(opt, parser, |p| regex_token(p, scanner)))
    }
}

#[derive(Debug)]
pub struct ThisToken {}

impl ToPrimaryExpressionKind for ThisToken {
    fn to_primary_expression_kind(_node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::This
    }
}

fn this_token(parser: &mut Parser, scanner: Scanner) -> Result<Option<(Box<ThisToken>, Scanner)>, String> {
    let (tok, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
    Ok(match tok {
        scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::This) => {
            Some((Box::new(ThisToken {}), scanner))
        }
        _ => None,
    })
}

fn regex_token(
    parser: &mut Parser,
    scanner: Scanner,
) -> Result<Option<(Box<scanner::RegularExpressionData>, Scanner)>, String> {
    let (tok, after_tok) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
    match tok {
        scanner::Token::RegularExpression(rd) => Ok(Some((Box::new(rd), after_tok))),
        _ => Ok(None),
    }
}

#[derive(Debug)]
pub struct Elisions {
    count: usize,
}

impl fmt::Display for Elisions {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        assert!(self.count > 0);
        write!(f, ",")?;
        for _ in 1..self.count {
            write!(f, " ,")?;
        }
        Ok(())
    }
}

impl PrettyPrint for Elisions {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{}Elisions: {}", first, self)
    }
}

impl Elisions {
    fn parse(parser: &mut Parser, scanner: Scanner) -> Result<Option<(Box<Elisions>, Scanner)>, String> {
        let mut comma_count: usize = 0;
        let mut current_scanner = scanner;
        loop {
            let (token, after_comma) =
                scanner::scan_token(&current_scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
            if token != scanner::Token::Comma {
                return if comma_count == 0 {
                    Ok(None)
                } else {
                    Ok(Some((Box::new(Elisions { count: comma_count }), current_scanner)))
                };
            }
            comma_count += 1;
            current_scanner = after_comma;
        }
    }
}

#[derive(Debug)]
pub enum SpreadElement {
    AssignmentExpression(Box<AssignmentExpression>),
}

impl fmt::Display for SpreadElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let SpreadElement::AssignmentExpression(boxed) = self;
        write!(f, "... {}", boxed)
    }
}

impl PrettyPrint for SpreadElement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}SpreadElement: {}", first, self)?;
        let SpreadElement::AssignmentExpression(boxed) = self;
        boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl SpreadElement {
    fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<SpreadElement>, Scanner)>, String> {
        let (token, after_ellipsis) =
            scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if token != scanner::Token::Ellipsis {
            Ok(None)
        } else {
            let pot_ae = AssignmentExpression::parse(parser, after_ellipsis, true, yield_flag, await_flag)?;
            match pot_ae {
                None => Ok(None),
                Some((ae, after_ae)) => Ok(Some((Box::new(SpreadElement::AssignmentExpression(ae)), after_ae))),
            }
        }
    }
}

#[derive(Debug)]
pub enum ElementList {
    AssignmentExpression((Option<Box<Elisions>>, Box<AssignmentExpression>)),
    SpreadElement((Option<Box<Elisions>>, Box<SpreadElement>)),
    ElementListAssignmentExpression((Box<ElementList>, Option<Box<Elisions>>, Box<AssignmentExpression>)),
    ElementListSpreadElement((Box<ElementList>, Option<Box<Elisions>>, Box<SpreadElement>)),
}

impl fmt::Display for ElementList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ElementList::AssignmentExpression((elisions, ae)) => match elisions {
                None => write!(f, "{}", ae),
                Some(commas) => write!(f, "{} {}", commas, ae),
            },
            ElementList::SpreadElement((elisions, se)) => match elisions {
                None => write!(f, "{}", se),
                Some(commas) => write!(f, "{} {}", commas, se),
            },
            ElementList::ElementListAssignmentExpression((el, elisions, ae)) => match elisions {
                None => write!(f, "{} , {}", el, ae),
                Some(commas) => write!(f, "{} , {} {}", el, commas, ae),
            },
            ElementList::ElementListSpreadElement((el, elisions, se)) => match elisions {
                None => write!(f, "{} , {}", el, se),
                Some(commas) => write!(f, "{} , {} {}", el, commas, se),
            },
        }
    }
}

impl PrettyPrint for ElementList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ElementList: {}", first, self)?;
        match self {
            ElementList::AssignmentExpression((elisions, ae)) => match elisions {
                None => ae.pprint_with_leftpad(writer, &successive, Spot::Final),
                Some(commas) => {
                    commas.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    ae.pprint_with_leftpad(writer, &successive, Spot::Final)
                }
            },
            ElementList::SpreadElement((elisions, boxed)) => match elisions {
                None => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
                Some(commas) => {
                    commas.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
                }
            },
            ElementList::ElementListAssignmentExpression((right, elisions, left)) => match elisions {
                None => {
                    right.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    left.pprint_with_leftpad(writer, &successive, Spot::Final)
                }
                Some(commas) => {
                    right.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    commas.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    left.pprint_with_leftpad(writer, &successive, Spot::Final)
                }
            },
            ElementList::ElementListSpreadElement((right, elisions, left)) => match elisions {
                None => {
                    right.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    left.pprint_with_leftpad(writer, &successive, Spot::Final)
                }
                Some(commas) => {
                    right.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    commas.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    left.pprint_with_leftpad(writer, &successive, Spot::Final)
                }
            },
        }
    }
}

impl ElementList {
    fn non_recursive_part(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<
        (
            Option<Box<Elisions>>,
            Option<Box<AssignmentExpression>>,
            Option<Box<SpreadElement>>,
            Scanner,
        ),
        String,
    > {
        let pot_elision = Elisions::parse(parser, scanner)?;
        let elision: Option<Box<Elisions>>;
        let after_e_scanner: Scanner;
        match pot_elision {
            None => {
                elision = None;
                after_e_scanner = scanner;
            }
            Some((boxed, after_elision)) => {
                elision = Some(boxed);
                after_e_scanner = after_elision;
            }
        }
        let pot_ae = AssignmentExpression::parse(parser, after_e_scanner, true, yield_flag, await_flag)?;
        match pot_ae {
            Some((boxed, after_ae_scanner)) => Ok((elision, Some(boxed), None, after_ae_scanner)),
            None => {
                let pot_se = SpreadElement::parse(parser, after_e_scanner, yield_flag, await_flag)?;
                match pot_se {
                    Some((boxed, after_se_scanner)) => Ok((elision, None, Some(boxed), after_se_scanner)),
                    None => Ok((None, None, None, scanner)),
                }
            }
        }
    }

    fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<ElementList>, Scanner)>, String> {
        let mut current_production: Box<ElementList>;
        let mut current_scanner: Scanner;

        let (elision, boxed_ae, boxed_se, after) = Self::non_recursive_part(parser, scanner, yield_flag, await_flag)?;
        if boxed_ae.is_some() {
            current_production = Box::new(ElementList::AssignmentExpression((elision, boxed_ae.unwrap())));
            current_scanner = after;
        } else if boxed_se.is_some() {
            current_production = Box::new(ElementList::SpreadElement((elision, boxed_se.unwrap())));
            current_scanner = after;
        } else {
            return Ok(None);
        }

        loop {
            let (token, after_tok) =
                scanner::scan_token(&current_scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
            match token {
                scanner::Token::Comma => {
                    let (elision, boxed_ae, boxed_se, after) =
                        Self::non_recursive_part(parser, after_tok, yield_flag, await_flag)?;
                    if boxed_ae.is_some() {
                        current_production = Box::new(ElementList::ElementListAssignmentExpression((
                            current_production,
                            elision,
                            boxed_ae.unwrap(),
                        )));
                        current_scanner = after;
                    } else if boxed_se.is_some() {
                        current_production = Box::new(ElementList::ElementListSpreadElement((
                            current_production,
                            elision,
                            boxed_se.unwrap(),
                        )));
                        current_scanner = after;
                    } else {
                        break;
                    }
                }
                _ => {
                    break;
                }
            }
        }
        Ok(Some((current_production, current_scanner)))
    }
}

#[derive(Debug)]
pub enum ArrayLiteral {
    Empty(Option<Box<Elisions>>),
    ElementList(Box<ElementList>),
    ElementListElision(Box<ElementList>, Option<Box<Elisions>>),
}

impl fmt::Display for ArrayLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArrayLiteral::Empty(pot_elision) => match pot_elision {
                None => write!(f, "[ ]"),
                Some(elision) => write!(f, "[ {} ]", elision),
            },
            ArrayLiteral::ElementList(boxed) => write!(f, "[ {} ]", boxed),
            ArrayLiteral::ElementListElision(boxed, pot_elision) => match pot_elision {
                None => write!(f, "[ {} , ]", boxed),
                Some(elision) => write!(f, "[ {} , {} ]", boxed, elision),
            },
        }
    }
}

impl PrettyPrint for ArrayLiteral {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ArrayLiteral: {}", first, self)?;
        match self {
            ArrayLiteral::Empty(None) => Ok(()),
            ArrayLiteral::Empty(Some(elision)) => elision.pprint_with_leftpad(writer, &successive, Spot::Final),
            ArrayLiteral::ElementList(boxed) | ArrayLiteral::ElementListElision(boxed, None) => {
                boxed.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayLiteral::ElementListElision(boxed, Some(elision)) => {
                boxed.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                elision.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl ArrayLiteral {
    fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (token, after) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if token != scanner::Token::LeftBracket {
            return Ok(None);
        }
        let pot_el = ElementList::parse(parser, after, yield_flag, await_flag)?;
        match pot_el {
            None => {
                let pot_elision = Elisions::parse(parser, after)?;
                match pot_elision {
                    None => {
                        let (closing, end_scan) =
                            scanner::scan_token(&after, parser.source, scanner::ScanGoal::InputElementRegExp);
                        if closing == scanner::Token::RightBracket {
                            return Ok(Some((Box::new(ArrayLiteral::Empty(None)), end_scan)));
                        }
                        return Ok(None);
                    }
                    Some((elisions, after_2)) => {
                        let (closing, end_scan) =
                            scanner::scan_token(&after_2, parser.source, scanner::ScanGoal::InputElementRegExp);
                        if closing == scanner::Token::RightBracket {
                            return Ok(Some((Box::new(ArrayLiteral::Empty(Some(elisions))), end_scan)));
                        }
                        return Ok(None);
                    }
                }
            }
            Some((boxed_el, after_el_scan)) => {
                let (comma_or_bracket, after_cb) =
                    scanner::scan_token(&after_el_scan, parser.source, scanner::ScanGoal::InputElementRegExp);
                match comma_or_bracket {
                    scanner::Token::RightBracket => {
                        return Ok(Some((Box::new(ArrayLiteral::ElementList(boxed_el)), after_cb)));
                    }
                    scanner::Token::Comma => {
                        let pot_elision = Elisions::parse(parser, after_cb)?;
                        match pot_elision {
                            None => {
                                let (closing, end_scan) = scanner::scan_token(
                                    &after_cb,
                                    parser.source,
                                    scanner::ScanGoal::InputElementRegExp,
                                );
                                if closing == scanner::Token::RightBracket {
                                    return Ok(Some((
                                        Box::new(ArrayLiteral::ElementListElision(boxed_el, None)),
                                        end_scan,
                                    )));
                                }
                                return Ok(None);
                            }
                            Some((elisions, after_2)) => {
                                let (closing, end_scan) =
                                    scanner::scan_token(&after_2, parser.source, scanner::ScanGoal::InputElementRegExp);
                                if closing == scanner::Token::RightBracket {
                                    return Ok(Some((
                                        Box::new(ArrayLiteral::ElementListElision(boxed_el, Some(elisions))),
                                        end_scan,
                                    )));
                                }
                                return Ok(None);
                            }
                        }
                    }
                    _ => {
                        return Ok(None);
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum Initializer {
    AssignmentExpression(Box<AssignmentExpression>),
}

impl fmt::Display for Initializer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Initializer::AssignmentExpression(boxed_ae) = self;
        write!(f, "= {}", *boxed_ae)
    }
}

impl PrettyPrint for Initializer {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Initializer: {}", first, self)?;
        let Initializer::AssignmentExpression(boxed_ae) = self;
        boxed_ae.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl Initializer {
    fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Initializer>, Scanner)>, String> {
        let (token, after_tok) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if token != scanner::Token::Eq {
            return Ok(None);
        }
        let pot_ae = AssignmentExpression::parse(parser, after_tok, in_flag, yield_flag, await_flag)?;
        match pot_ae {
            None => Ok(None),
            Some((boxed_ae, after_ae)) => Ok(Some((Box::new(Initializer::AssignmentExpression(boxed_ae)), after_ae))),
        }
    }
}

#[derive(Debug)]
pub enum CoverInitializedName {
    InitializedName(Box<IdentifierReference>, Box<Initializer>),
}

impl fmt::Display for CoverInitializedName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let CoverInitializedName::InitializedName(idref, izer) = self;
        write!(f, "{} {}", idref, izer)
    }
}

impl PrettyPrint for CoverInitializedName {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}CoverInitializedName: {}", first, self)?;
        let CoverInitializedName::InitializedName(idref, izer) = self;
        idref.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        izer.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl CoverInitializedName {
    fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_idref = IdentifierReference::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_idref {
            None => Ok(None),
            Some((idref, after_idref)) => {
                let pot_init = Initializer::parse(parser, after_idref, true, yield_flag, await_flag)?;
                match pot_init {
                    None => Ok(None),
                    Some((izer, after_izer)) => Ok(Some((
                        Box::new(CoverInitializedName::InitializedName(idref, izer)),
                        after_izer,
                    ))),
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum ComputedPropertyName {
    AssignmentExpression(Box<AssignmentExpression>),
}

impl fmt::Display for ComputedPropertyName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ComputedPropertyName::AssignmentExpression(ae) = self;
        write!(f, "[ {} ]", ae)
    }
}

impl PrettyPrint for ComputedPropertyName {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ComputedPropertyName: {}", first, self)?;
        let ComputedPropertyName::AssignmentExpression(ae) = self;
        ae.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl ComputedPropertyName {
    fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok, after_tok) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        match tok {
            scanner::Token::LeftBracket => {
                let pot_ae = AssignmentExpression::parse(parser, after_tok, true, yield_flag, await_flag)?;
                match pot_ae {
                    None => Ok(None),
                    Some((ae, after_ae)) => {
                        let (tok2, after_rb) =
                            scanner::scan_token(&after_ae, parser.source, scanner::ScanGoal::InputElementRegExp);
                        match tok2 {
                            scanner::Token::RightBracket => Ok(Some((
                                Box::new(ComputedPropertyName::AssignmentExpression(ae)),
                                after_rb,
                            ))),
                            _ => Ok(None),
                        }
                    }
                }
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug)]
pub enum LiteralPropertyName {
    IdentifierName(Box<IdentifierNameToken>),
    StringLiteral(scanner::JSString),
    NumericLiteral(Numeric),
}

impl fmt::Display for LiteralPropertyName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LiteralPropertyName::IdentifierName(id) => write!(f, "{}", id),
            LiteralPropertyName::StringLiteral(s) => write!(f, "{:?}", s),
            LiteralPropertyName::NumericLiteral(Numeric::Number(n)) => {
                let mut s = Vec::new();
                number_to_string(&mut s, *n).unwrap();
                write!(f, "{}", String::from_utf8(s).unwrap())
            }
            LiteralPropertyName::NumericLiteral(Numeric::BigInt(b)) => write!(f, "{}", b),
        }
    }
}

impl PrettyPrint for LiteralPropertyName {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{}LiteralPropertyName: {}", first, self)
    }
}

impl LiteralPropertyName {
    fn parse(parser: &mut Parser, scanner: Scanner) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok, after_tok) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        match tok {
            scanner::Token::Identifier(id) => Ok(Some((
                Box::new(LiteralPropertyName::IdentifierName(Box::new(IdentifierNameToken {
                    value: scanner::Token::Identifier(id),
                }))),
                after_tok,
            ))),
            scanner::Token::String(s) => Ok(Some((Box::new(LiteralPropertyName::StringLiteral(s)), after_tok))),
            scanner::Token::Number(n) => Ok(Some((
                Box::new(LiteralPropertyName::NumericLiteral(Numeric::Number(n))),
                after_tok,
            ))),
            scanner::Token::BigInt(b) => Ok(Some((
                Box::new(LiteralPropertyName::NumericLiteral(Numeric::BigInt(b))),
                after_tok,
            ))),
            _ => Ok(None),
        }
    }
}

#[derive(Debug)]
pub enum PropertyName {
    LiteralPropertyName(Box<LiteralPropertyName>),
    ComputedPropertyName(Box<ComputedPropertyName>),
}

impl fmt::Display for PropertyName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PropertyName::LiteralPropertyName(lpn) => write!(f, "{}", lpn),
            PropertyName::ComputedPropertyName(cpn) => write!(f, "{}", cpn),
        }
    }
}

impl PrettyPrint for PropertyName {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}PropertyName: {}", first, self)?;
        match &self {
            PropertyName::LiteralPropertyName(lpn) => lpn.pprint_with_leftpad(writer, &successive, Spot::Final),
            PropertyName::ComputedPropertyName(cpn) => cpn.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
}

impl PropertyName {
    fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_lpn = LiteralPropertyName::parse(parser, scanner)?;
        match pot_lpn {
            None => {
                let pot_cpn = ComputedPropertyName::parse(parser, scanner, yield_flag, await_flag)?;
                match pot_cpn {
                    None => Ok(None),
                    Some((cpn, after_cpn)) => Ok(Some((Box::new(PropertyName::ComputedPropertyName(cpn)), after_cpn))),
                }
            }
            Some((lpn, after_lpn)) => Ok(Some((Box::new(PropertyName::LiteralPropertyName(lpn)), after_lpn))),
        }
    }
}

// PropertyDefinition[Yield, Await] :
//      IdentifierReference[?Yield, ?Await]
//      CoverInitializedName[?Yield, ?Await]
//      PropertyName[?Yield, ?Await] : AssignmentExpression[+In, ?Yield, ?Await]
//      MethodDefinition[?Yield, ?Await]
//      ... AssignmentExpression[+In, ?Yield, ?Await]
#[derive(Debug)]
pub enum PropertyDefinition {
    IdentifierReference(Box<IdentifierReference>),
    CoverInitializedName(Box<CoverInitializedName>),
    PropertyNameAssignmentExpression(Box<PropertyName>, Box<AssignmentExpression>),
    MethodDefinition(Box<MethodDefinition>),
    AssignmentExpression(Box<AssignmentExpression>),
}

impl fmt::Display for PropertyDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PropertyDefinition::IdentifierReference(idref) => write!(f, "{}", idref),
            PropertyDefinition::CoverInitializedName(cin) => write!(f, "{}", cin),
            PropertyDefinition::PropertyNameAssignmentExpression(pn, ae) => write!(f, "{} : {}", pn, ae),
            PropertyDefinition::MethodDefinition(md) => write!(f, "{}", md),
            PropertyDefinition::AssignmentExpression(ae) => write!(f, "... {}", ae),
        }
    }
}

impl PrettyPrint for PropertyDefinition {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}PropertyDefinition: {}", first, self)?;
        match self {
            PropertyDefinition::IdentifierReference(idref) => {
                idref.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            PropertyDefinition::CoverInitializedName(cin) => cin.pprint_with_leftpad(writer, &successive, Spot::Final),
            PropertyDefinition::PropertyNameAssignmentExpression(pn, ae) => {
                pn.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ae.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            PropertyDefinition::MethodDefinition(md) => md.pprint_with_leftpad(writer, &successive, Spot::Final),
            PropertyDefinition::AssignmentExpression(ae) => ae.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
}

impl PropertyDefinition {
    fn parse_pn_ae(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_pn = PropertyName::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_pn {
            Some((pn, after_pn)) => {
                let (tok, after_tok) =
                    scanner::scan_token(&after_pn, parser.source, scanner::ScanGoal::InputElementRegExp);
                match tok {
                    scanner::Token::Colon => {
                        let pot_ae = AssignmentExpression::parse(parser, after_tok, true, yield_flag, await_flag)?;
                        match pot_ae {
                            Some((ae, after_ae)) => Ok(Some((
                                Box::new(PropertyDefinition::PropertyNameAssignmentExpression(pn, ae)),
                                after_ae,
                            ))),
                            None => Ok(None),
                        }
                    }
                    _ => Ok(None),
                }
            }
            None => Ok(None),
        }
    }

    fn parse_cin(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_cin = CoverInitializedName::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_cin {
            Some((cin, after_cin)) => Ok(Some((
                Box::new(PropertyDefinition::CoverInitializedName(cin)),
                after_cin,
            ))),
            None => Ok(None),
        }
    }

    fn parse_md(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_md = MethodDefinition::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_md {
            Some((md, after_md)) => Ok(Some((Box::new(PropertyDefinition::MethodDefinition(md)), after_md))),
            None => Ok(None),
        }
    }

    fn parse_idref(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_idref = IdentifierReference::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_idref {
            Some((idref, after_idref)) => Ok(Some((
                Box::new(PropertyDefinition::IdentifierReference(idref)),
                after_idref,
            ))),
            None => Ok(None),
        }
    }

    fn parse_ae(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok, after_tok) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        match tok {
            scanner::Token::Ellipsis => {
                let pot_ae = AssignmentExpression::parse(parser, after_tok, true, yield_flag, await_flag)?;
                match pot_ae {
                    Some((ae, after_ae)) => {
                        Ok(Some((Box::new(PropertyDefinition::AssignmentExpression(ae)), after_ae)))
                    }
                    None => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }

    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        Self::parse_pn_ae(parser, scanner, yield_flag, await_flag)
            .and_then(|opt| opt.map_or_else(|| Self::parse_cin(parser, scanner, yield_flag, await_flag), rewrap))
            .and_then(|opt| opt.map_or_else(|| Self::parse_md(parser, scanner, yield_flag, await_flag), rewrap))
            .and_then(|opt| opt.map_or_else(|| Self::parse_idref(parser, scanner, yield_flag, await_flag), rewrap))
            .and_then(|opt| opt.map_or_else(|| Self::parse_ae(parser, scanner, yield_flag, await_flag), rewrap))
    }
}

// PropertyDefinitionList[Yield, Await] :
//      PropertyDefinition[?Yield, ?Await]
//      PropertyDefinitionList[?Yield, ?Await] , PropertyDefinition[?Yield, ?Await]
#[derive(Debug)]
pub enum PropertyDefinitionList {
    OneDef(Box<PropertyDefinition>),
    ManyDefs(Box<PropertyDefinitionList>, Box<PropertyDefinition>),
}

impl fmt::Display for PropertyDefinitionList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PropertyDefinitionList::OneDef(pd) => write!(f, "{}", pd),
            PropertyDefinitionList::ManyDefs(pdl, pd) => write!(f, "{} , {}", pdl, pd),
        }
    }
}

impl PrettyPrint for PropertyDefinitionList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}PropertyDefinitionList: {}", first, self)?;
        match self {
            PropertyDefinitionList::OneDef(pd) => pd.pprint_with_leftpad(writer, &successive, Spot::Final),
            PropertyDefinitionList::ManyDefs(pdl, pd) => {
                pdl.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pd.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl PropertyDefinitionList {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_pd = PropertyDefinition::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_pd {
            None => Ok(None),
            Some((pd, after_pd)) => {
                let mut current_production = Box::new(PropertyDefinitionList::OneDef(pd));
                let mut current_scanner = after_pd;
                loop {
                    let (comma, after_comma) =
                        scanner::scan_token(&current_scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
                    match comma {
                        scanner::Token::Comma => {
                            let pot_pd2 = PropertyDefinition::parse(parser, after_comma, yield_flag, await_flag)?;
                            match pot_pd2 {
                                None => {
                                    break;
                                }
                                Some((pd2, after_pd2)) => {
                                    current_production =
                                        Box::new(PropertyDefinitionList::ManyDefs(current_production, pd2));
                                    current_scanner = after_pd2;
                                }
                            }
                        }
                        _ => {
                            break;
                        }
                    }
                }
                Ok(Some((current_production, current_scanner)))
            }
        }
    }
}

// ObjectLiteral[Yield, Await] :
//      { }
//      { PropertyDefinitionList[?Yield, ?Await] }
//      { PropertyDefinitionList[?Yield, ?Await] , }
#[derive(Debug)]
pub enum ObjectLiteral {
    Empty,
    Normal(Box<PropertyDefinitionList>),
    TrailingComma(Box<PropertyDefinitionList>),
}

impl fmt::Display for ObjectLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ObjectLiteral::Empty => write!(f, "{{ }}"),
            ObjectLiteral::Normal(pdl) => write!(f, "{{ {} }}", pdl),
            ObjectLiteral::TrailingComma(pdl) => write!(f, "{{ {} , }}", pdl),
        }
    }
}

impl PrettyPrint for ObjectLiteral {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ObjectLiteral: {}", first, self)?;
        match self {
            ObjectLiteral::Empty => Ok(()),
            ObjectLiteral::Normal(pdl) | ObjectLiteral::TrailingComma(pdl) => {
                pdl.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl ObjectLiteral {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (lb, after_brace) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        match lb {
            scanner::Token::LeftBrace => {
                let pot_pdl = PropertyDefinitionList::parse(parser, after_brace, yield_flag, await_flag)?;
                match pot_pdl {
                    None => {
                        let (rb, after_brace2) =
                            scanner::scan_token(&after_brace, parser.source, scanner::ScanGoal::InputElementRegExp);
                        match rb {
                            scanner::Token::RightBrace => Ok(Some((Box::new(ObjectLiteral::Empty), after_brace2))),
                            _ => Ok(None),
                        }
                    }
                    Some((pdl, after_pdl)) => {
                        let (comma_or_brace, after_punct) =
                            scanner::scan_token(&after_pdl, parser.source, scanner::ScanGoal::InputElementRegExp);
                        match comma_or_brace {
                            scanner::Token::RightBrace => Ok(Some((Box::new(ObjectLiteral::Normal(pdl)), after_punct))),
                            scanner::Token::Comma => {
                                let (rb2, after_brace3) = scanner::scan_token(
                                    &after_punct,
                                    parser.source,
                                    scanner::ScanGoal::InputElementRegExp,
                                );
                                match rb2 {
                                    scanner::Token::RightBrace => {
                                        Ok(Some((Box::new(ObjectLiteral::TrailingComma(pdl)), after_brace3)))
                                    }
                                    _ => Ok(None),
                                }
                            }
                            _ => Ok(None),
                        }
                    }
                }
            }
            _ => Ok(None),
        }
    }
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
            LiteralKind::NumericLiteral(Numeric::Number(n)) => {
                let mut s = Vec::new();
                number_to_string(&mut s, *n).unwrap();
                write!(f, "{}", String::from_utf8(s).unwrap())
            }
            LiteralKind::NumericLiteral(Numeric::BigInt(b)) => write!(f, "{}", *b),
            LiteralKind::StringLiteral(s) => write!(f, "{:?}", *s),
        }
    }
}

impl PrettyPrint for Literal {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{}Literal: {}", first, self)
    }
}

impl Literal {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> Result<Option<(Box<Literal>, Scanner)>, String> {
        let scan_result = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        let (token, newscanner) = scan_result;
        match token {
            scanner::Token::Identifier(id) => match id.keyword_id {
                Some(scanner::Keyword::Null) => {
                    let node = Literal {
                        kind: LiteralKind::NullLiteral,
                    };
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
                let node = Literal {
                    kind: LiteralKind::StringLiteral(s),
                };
                let boxed = Box::new(node);
                return Ok(Some((boxed, newscanner)));
            }
            _ => return Ok(None),
        }
    }
}

// TemplateLiteral[Yield, Await, Tagged] :
//      NoSubstitutionTemplate
//      SubstitutionTemplate[?Yield, ?Await, ?Tagged]
#[derive(Debug)]
pub enum TemplateLiteral {
    NoSubstitutionTemplate(scanner::TemplateData, bool),
    SubstitutionTemplate(Box<SubstitutionTemplate>),
}

impl fmt::Display for TemplateLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TemplateLiteral::NoSubstitutionTemplate(td, _) => match &td.tv {
                None => write!(f, "`{}` (TV undefined)", td.trv),
                Some(s) => {
                    let printable = format!("{}", s).replace(char::is_control, "\u{2426}");
                    write!(f, "`{}` ({})", td.trv, printable)
                }
            },
            TemplateLiteral::SubstitutionTemplate(boxed) => write!(f, "{}", boxed),
        }
    }
}

impl PrettyPrint for TemplateLiteral {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}TemplateLiteral: {}", first, self)?;
        match self {
            TemplateLiteral::NoSubstitutionTemplate(_, _) => Ok(()),
            TemplateLiteral::SubstitutionTemplate(st) => st.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
}

impl TemplateLiteral {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        tagged_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok, after_nst) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        match tok {
            scanner::Token::NoSubstitutionTemplate(td) => Ok(Some((
                Box::new(TemplateLiteral::NoSubstitutionTemplate(td, tagged_flag)),
                after_nst,
            ))),
            scanner::Token::TemplateHead(_) => {
                let pot_st = SubstitutionTemplate::parse(parser, scanner, yield_flag, await_flag, tagged_flag)?;
                match pot_st {
                    Some((boxed, after_scan)) => Ok(Some((
                        Box::new(TemplateLiteral::SubstitutionTemplate(boxed)),
                        after_scan,
                    ))),
                    None => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }
}

// SubstitutionTemplate[Yield, Await, Tagged] :
//      TemplateHead Expression[+In, ?Yield, ?Await] TemplateSpans[?Yield, ?Await, ?Tagged]
#[derive(Debug)]
pub struct SubstitutionTemplate {
    template_head: scanner::TemplateData,
    tagged: bool,
    expression: Box<Expression>,
    template_spans: Box<TemplateSpans>,
}

impl fmt::Display for SubstitutionTemplate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "`{}${{ {} {}",
            format!("{}", self.template_head.trv).replace(char::is_control, "\u{2426}"),
            self.expression,
            self.template_spans
        )
    }
}

impl PrettyPrint for SubstitutionTemplate {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}SubstitutionTemplate: {}", first, self)?;
        self.expression
            .pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.template_spans
            .pprint_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl SubstitutionTemplate {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        tagged_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (head, after_head) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        match head {
            scanner::Token::TemplateHead(td) => {
                let pot_exp = Expression::parse(parser, after_head, true, yield_flag, await_flag)?;
                match pot_exp {
                    None => Ok(None),
                    Some((exp_boxed, after_exp)) => {
                        let pot_spans = TemplateSpans::parse(parser, after_exp, yield_flag, await_flag, tagged_flag)?;
                        match pot_spans {
                            None => Ok(None),
                            Some((spans_boxed, after_spans)) => Ok(Some((
                                Box::new(SubstitutionTemplate {
                                    template_head: td,
                                    tagged: tagged_flag,
                                    expression: exp_boxed,
                                    template_spans: spans_boxed,
                                }),
                                after_spans,
                            ))),
                        }
                    }
                }
            }
            _ => Ok(None),
        }
    }
}

// TemplateSpans[Yield, Await, Tagged] :
//      TemplateTail
//      TemplateMiddleList[?Yield, ?Await, ?Tagged] TemplateTail
#[derive(Debug)]
pub enum TemplateSpans {
    Tail(scanner::TemplateData, bool),
    List(Box<TemplateMiddleList>, scanner::TemplateData, bool),
}

impl fmt::Display for TemplateSpans {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TemplateSpans::Tail(td, _) => {
                write!(f, "}}{}`", format!("{}", td.trv).replace(char::is_control, "\u{2426}"))
            }
            TemplateSpans::List(tml, td, _) => write!(
                f,
                "{} }}{}`",
                tml,
                format!("{}", td.trv).replace(char::is_control, "\u{2426}")
            ),
        }
    }
}

impl PrettyPrint for TemplateSpans {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}TemplateSpans: {}", first, self)?;
        match self {
            TemplateSpans::Tail(_, _) => Ok(()),
            TemplateSpans::List(tml, _, _) => tml.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
}

impl TemplateSpans {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        tagged_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (token, after_tmplt) =
            scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementTemplateTail);
        match token {
            scanner::Token::TemplateTail(td) => Ok(Some((Box::new(TemplateSpans::Tail(td, tagged_flag)), after_tmplt))),
            scanner::Token::TemplateMiddle(td) => {
                let pot_tml = TemplateMiddleList::parse(parser, scanner, yield_flag, await_flag, tagged_flag)?;
                match pot_tml {
                    None => Ok(None),
                    Some((boxed_tml, after_tml)) => {
                        let (tail, after_tail) =
                            scanner::scan_token(&after_tml, parser.source, scanner::ScanGoal::InputElementTemplateTail);
                        match tail {
                            scanner::Token::TemplateTail(td) => Ok(Some((
                                Box::new(TemplateSpans::List(boxed_tml, td, tagged_flag)),
                                after_tail,
                            ))),
                            _ => Ok(None),
                        }
                    }
                }
            }
            _ => Ok(None),
        }
    }
}

// TemplateMiddleList[Yield, Await, Tagged] :
//      TemplateMiddle Expression[+In, ?Yield, ?Await]
//      TemplateMiddleList[?Yield, ?Await, ?Tagged] TemplateMiddle Expression[+In, ?Yield, ?Await]
#[derive(Debug)]
pub enum TemplateMiddleList {
    ListEnd(scanner::TemplateData, Box<Expression>, bool),
    ListMid(Box<TemplateMiddleList>, scanner::TemplateData, Box<Expression>, bool),
}

impl fmt::Display for TemplateMiddleList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TemplateMiddleList::ListEnd(td, exp, _) => write!(
                f,
                "}}{}${{ {}",
                format!("{}", td.trv).replace(char::is_control, "\u{2426}"),
                exp
            ),
            TemplateMiddleList::ListMid(tml, td, exp, _) => write!(
                f,
                "{} }}{}${{ {}",
                tml,
                format!("{}", td.trv).replace(char::is_control, "\u{2426}"),
                exp
            ),
        }
    }
}

impl PrettyPrint for TemplateMiddleList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}TemplateMiddleList: {}", first, self)?;
        match self {
            TemplateMiddleList::ListEnd(_, exp, _) => exp.pprint_with_leftpad(writer, &successive, Spot::Final),
            TemplateMiddleList::ListMid(tml, _, exp, _) => {
                tml.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                exp.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl TemplateMiddleList {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        tagged_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let mut current_node = None;
        let mut current_scanner = scanner;
        loop {
            let (middle, after_mid) = scanner::scan_token(
                &current_scanner,
                parser.source,
                scanner::ScanGoal::InputElementTemplateTail,
            );
            match middle {
                scanner::Token::TemplateMiddle(td) => {
                    let pot_exp = Expression::parse(parser, after_mid, true, yield_flag, await_flag)?;
                    match pot_exp {
                        None => {
                            return match current_node {
                                None => Ok(None),
                                Some(boxed) => Ok(Some((boxed, current_scanner))),
                            };
                        }
                        Some((boxed_exp, after_exp)) => {
                            let node = Some(Box::new(match current_node {
                                None => TemplateMiddleList::ListEnd(td, boxed_exp, tagged_flag),
                                Some(previous) => TemplateMiddleList::ListMid(previous, td, boxed_exp, tagged_flag),
                            }));
                            current_node = node;
                            current_scanner = after_exp;
                        }
                    }
                }
                _ => {
                    return match current_node {
                        None => Ok(None),
                        Some(boxed) => Ok(Some((boxed, current_scanner))),
                    };
                }
            }
        }
    }
}

// ParenthesizedExpression[Yield, Await] :
//      ( Expression[+In, ?Yield, ?Await] )
#[derive(Debug)]
pub enum ParenthesizedExpression {
    Expression(Box<Expression>),
}

impl fmt::Display for ParenthesizedExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ParenthesizedExpression::Expression(e) = self;
        write!(f, "( {} )", e)
    }
}

impl PrettyPrint for ParenthesizedExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ParenthesizedExpression: {}", first, self)?;
        let ParenthesizedExpression::Expression(e) = self;
        e.pprint_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl IsFunctionDefinition for ParenthesizedExpression {
    fn is_function_definition(&self) -> bool {
        let ParenthesizedExpression::Expression(e) = self;
        e.is_function_definition()
    }
}

impl AssignmentTargetType for ParenthesizedExpression {
    fn assignment_target_type(&self) -> ATTKind {
        let ParenthesizedExpression::Expression(e) = self;
        e.assignment_target_type()
    }
}

impl ParenthesizedExpression {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (left_paren, after_lp) =
            scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        match left_paren {
            scanner::Token::LeftParen => {
                let pot_exp = Expression::parse(parser, after_lp, true, yield_flag, await_flag)?;
                match pot_exp {
                    Some((exp, after_exp)) => {
                        let (right_paren, after_rp) =
                            scanner::scan_token(&after_exp, parser.source, scanner::ScanGoal::InputElementRegExp);
                        match right_paren {
                            scanner::Token::RightParen => {
                                Ok(Some((Box::new(ParenthesizedExpression::Expression(exp)), after_rp)))
                            }
                            _ => Ok(None),
                        }
                    }
                    None => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_none, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::pretty_check;

    // PRIMARY EXPRESSION
    #[test]
    fn primary_expression_test_debug() {
        let pe = PrimaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false);
        let (exp, _) = check(pe);
        assert_eq!(format!("{:?}", exp), "PrimaryExpression { kind: This }");
    }
    #[test]
    fn primary_expression_test_pprint() {
        let (pe1, _) = check(PrimaryExpression::parse(
            &mut newparser("this"),
            Scanner::new(),
            false,
            false,
        ));
        pretty_check(&*pe1, "PrimaryExpression: this", vec![]);
        let (pe2, _) = check(PrimaryExpression::parse(
            &mut newparser("1"),
            Scanner::new(),
            false,
            false,
        ));
        pretty_check(&*pe2, "PrimaryExpression: 1", vec!["Literal: 1"]);
        let (pe3, _) = check(PrimaryExpression::parse(
            &mut newparser("i"),
            Scanner::new(),
            false,
            false,
        ));
        pretty_check(&*pe3, "PrimaryExpression: i", vec!["IdentifierReference: i"]);
        let (pe4, _) = check(PrimaryExpression::parse(
            &mut newparser("[]"),
            Scanner::new(),
            false,
            false,
        ));
        pretty_check(&*pe4, "PrimaryExpression: [ ]", vec!["ArrayLiteral: [ ]"]);
        let (pe5, _) = check(PrimaryExpression::parse(
            &mut newparser("{}"),
            Scanner::new(),
            false,
            false,
        ));
        pretty_check(&*pe5, "PrimaryExpression: { }", vec!["ObjectLiteral: { }"]);
        let (pe6, _) = check(PrimaryExpression::parse(
            &mut newparser("(a)"),
            Scanner::new(),
            false,
            false,
        ));
        pretty_check(
            &*pe6,
            "PrimaryExpression: ( a )",
            vec!["ParenthesizedExpression: ( a )"],
        );
        let (pe7, _) = check(PrimaryExpression::parse(
            &mut newparser("`rust`"),
            Scanner::new(),
            false,
            false,
        ));
        pretty_check(
            &*pe7,
            "PrimaryExpression: `rust` (rust)",
            vec!["TemplateLiteral: `rust` (rust)"],
        );
    }
    #[test]
    fn primary_expression_test_idref() {
        let pe_res = PrimaryExpression::parse(&mut newparser("blue"), Scanner::new(), false, false);
        let (boxed_pe, scanner) = check(pe_res);
        chk_scan(&scanner, 4);
        assert!(matches!(boxed_pe.kind, PrimaryExpressionKind::IdentifierReference(_)));
        assert_eq!(boxed_pe.is_function_definition(), false);
        assert_eq!(boxed_pe.is_identifier_reference(), true);
        assert_eq!(boxed_pe.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn primary_expression_test_literal() {
        let (node, scanner) = check(PrimaryExpression::parse(
            &mut newparser("371"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 3);
        assert!(matches!(node.kind, PrimaryExpressionKind::Literal(_)));
        assert_eq!(node.is_function_definition(), false);
        assert_eq!(node.is_identifier_reference(), false);
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn primary_expression_test_this() {
        let (node, scanner) = check(PrimaryExpression::parse(
            &mut newparser("this"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 4);
        assert!(matches!(node.kind, PrimaryExpressionKind::This));
        assert_eq!(node.is_function_definition(), false);
        assert_eq!(node.is_identifier_reference(), false);
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn primary_expression_test_arraylit() {
        let (node, scanner) = check(PrimaryExpression::parse(
            &mut newparser("[]"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 2);
        assert!(matches!(node.kind, PrimaryExpressionKind::ArrayLiteral(_)));
        assert_eq!(node.is_function_definition(), false);
        assert_eq!(node.is_identifier_reference(), false);
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn primary_expression_test_objlit() {
        let (node, scanner) = check(PrimaryExpression::parse(
            &mut newparser("{}"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 2);
        assert!(matches!(node.kind, PrimaryExpressionKind::ObjectLiteral(_)));
        assert_eq!(node.is_function_definition(), false);
        assert_eq!(node.is_identifier_reference(), false);
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn primary_expression_test_group() {
        let (node, scanner) = check(PrimaryExpression::parse(
            &mut newparser("(a)"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 3);
        assert!(matches!(node.kind, PrimaryExpressionKind::Parenthesized(_)));
        assert_eq!(node.is_function_definition(), false);
        assert_eq!(node.is_identifier_reference(), false);
        assert_eq!(node.assignment_target_type(), ATTKind::Simple);
    }

    #[test]
    fn this_token_test_debug() {
        assert_eq!(format!("{:?}", ThisToken {}), "ThisToken");
    }
    #[test]
    fn this_token_test_01() {
        let (_, scanner) = check(this_token(&mut newparser("this"), Scanner::new()));
        chk_scan(&scanner, 4);
    }
    #[test]
    fn this_token_test_02() {
        check_none(this_token(&mut newparser("**"), Scanner::new()));
    }

    // LITERAL
    #[test]
    fn literal_test_debug() {
        assert_eq!(
            format!(
                "{:?}",
                Literal {
                    kind: LiteralKind::NullLiteral
                }
            ),
            "Literal { kind: NullLiteral }"
        );
    }
    #[test]
    fn literal_test_null() {
        let (lit, scanner) = check(Literal::parse(&mut newparser("null"), Scanner::new()));
        chk_scan(&scanner, 4);
        assert!(matches!(lit.kind, LiteralKind::NullLiteral));
        pretty_check(&*lit, "Literal: null", vec![]);
    }
    #[test]
    fn literal_test_boolean_01() {
        let (lit, scanner) = check(Literal::parse(&mut newparser("true"), Scanner::new()));
        chk_scan(&scanner, 4);
        assert!(matches!(lit.kind, LiteralKind::BooleanLiteral(true)));
        pretty_check(&*lit, "Literal: true", vec![]);
    }
    #[test]
    fn literal_test_boolean_02() {
        let (lit, scanner) = check(Literal::parse(&mut newparser("false"), Scanner::new()));
        chk_scan(&scanner, 5);
        assert!(matches!(lit.kind, LiteralKind::BooleanLiteral(false)));
        pretty_check(&*lit, "Literal: false", vec![]);
    }
    #[test]
    fn literal_test_leading_dot() {
        let (lit, scanner) = check(Literal::parse(&mut newparser(".25"), Scanner::new()));
        chk_scan(&scanner, 3);
        assert_eq!(lit.kind, LiteralKind::NumericLiteral(Numeric::Number(0.25)));
        pretty_check(&*lit, "Literal: 0.25", vec![]);
    }
    #[test]
    fn literal_test_bigint() {
        let (lit, scanner) = check(Literal::parse(&mut newparser("7173n"), Scanner::new()));
        chk_scan(&scanner, 5);
        assert!(matches!(lit.kind, LiteralKind::NumericLiteral(Numeric::BigInt(_))));
    }
    #[test]
    fn literal_test_string() {
        let (lit, scanner) = check(Literal::parse(&mut newparser("'string'"), Scanner::new()));
        chk_scan(&scanner, 8);
        assert!(matches!(lit.kind, LiteralKind::StringLiteral(_)));
        pretty_check(&*lit, "Literal: \"string\"", vec![]);
    }
    #[test]
    fn literal_test_keyword() {
        check_none(Literal::parse(&mut newparser("function"), Scanner::new()));
    }
    #[test]
    fn literal_test_punct() {
        check_none(Literal::parse(&mut newparser("**"), Scanner::new()));
    }
    #[test]
    fn elision_test_01() {
        check_none(Elisions::parse(&mut newparser(""), Scanner::new()));
    }
    #[test]
    fn elision_test_02() {
        let (e, scanner) = check(Elisions::parse(&mut newparser(",,"), Scanner::new()));
        chk_scan(&scanner, 2);
        assert!(matches!(*e, Elisions { count: 2 }));
    }
    #[test]
    fn elision_test_03() {
        let (e, scanner) = check(Elisions::parse(&mut newparser(",,,]"), Scanner::new()));
        chk_scan(&scanner, 3);
        assert!(matches!(*e, Elisions { count: 3 }));
    }
    #[test]
    fn elision_test_pprint() {
        let (e1, _) = check(Elisions::parse(&mut newparser(","), Scanner::new()));
        pretty_check(&*e1, "Elisions: ,", vec![]);
        let (e2, _) = check(Elisions::parse(&mut newparser(",,,,,,"), Scanner::new()));
        pretty_check(&*e2, "Elisions: , , , , , ,", vec![]);
        format!("{:?}", e1);
    }

    #[test]
    fn spread_element_test_empty() {
        check_none(SpreadElement::parse(&mut newparser(""), Scanner::new(), false, false));
        check_none(SpreadElement::parse(
            &mut newparser("..."),
            Scanner::new(),
            false,
            false,
        ));
    }
    #[test]
    fn spread_element_test_assignment_expression() {
        let (se, scanner) = check(SpreadElement::parse(
            &mut newparser("...1"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 4);
        assert!(matches!(*se, SpreadElement::AssignmentExpression(_)));
    }
    #[test]
    fn spread_element_test_pretty() {
        let (se, _) = check(SpreadElement::parse(
            &mut newparser("...1"),
            Scanner::new(),
            false,
            false,
        ));
        pretty_check(&*se, "SpreadElement: ... 1", vec!["AssignmentExpression: 1"]);
        format!("{:?}", se);
    }

    #[test]
    fn element_list_test_01() {
        check_none(ElementList::parse(&mut newparser(""), Scanner::new(), false, false));
    }
    #[test]
    fn element_list_test_02() {
        let (el, scanner) = check(ElementList::parse(&mut newparser("3"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(*el, ElementList::AssignmentExpression((None, _))));
        pretty_check(&*el, "ElementList: 3", vec!["AssignmentExpression: 3"]);
        format!("{:?}", *el);
    }
    #[test]
    fn element_list_test_03() {
        let (el, scanner) = check(ElementList::parse(&mut newparser(",,3"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*el, ElementList::AssignmentExpression((Some(be), _)) if be.count == 2));
        pretty_check(
            &*el,
            "ElementList: , , 3",
            vec!["Elisions: , ,", "AssignmentExpression: 3"],
        );
    }
    #[test]
    fn element_list_test_05() {
        let (el, scanner) = check(ElementList::parse(&mut newparser("...a"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*el, ElementList::SpreadElement((None, _))));
        pretty_check(&*el, "ElementList: ... a", vec!["SpreadElement: ... a"]);
    }
    #[test]
    fn element_list_test_06() {
        let (el, scanner) = check(ElementList::parse(
            &mut newparser(",,...a"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 6);
        assert!(matches!(&*el, ElementList::SpreadElement((Some(be), _)) if be.count == 2));
        pretty_check(
            &*el,
            "ElementList: , , ... a",
            vec!["Elisions: , ,", "SpreadElement: ... a"],
        );
    }
    #[test]
    fn element_list_test_07() {
        let (el, scanner) = check(ElementList::parse(&mut newparser("a,b"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(
            &*el,
            ElementList::ElementListAssignmentExpression((_, None, _))
        ));
        pretty_check(
            &*el,
            "ElementList: a , b",
            vec!["ElementList: a", "AssignmentExpression: b"],
        );
    }
    #[test]
    fn element_list_test_08() {
        let (el, scanner) = check(ElementList::parse(&mut newparser("a,,b"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*el, ElementList::ElementListAssignmentExpression((_, Some(be), _)) if be.count == 1));
        pretty_check(
            &*el,
            "ElementList: a , , b",
            vec!["ElementList: a", "Elisions: ,", "AssignmentExpression: b"],
        );
    }
    #[test]
    fn element_list_test_09() {
        let (el, scanner) = check(ElementList::parse(
            &mut newparser("a,...b"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 6);
        assert!(matches!(&*el, ElementList::ElementListSpreadElement((_, None, _))));
        pretty_check(
            &*el,
            "ElementList: a , ... b",
            vec!["ElementList: a", "SpreadElement: ... b"],
        );
    }
    #[test]
    fn element_list_test_10() {
        let (el, scanner) = check(ElementList::parse(
            &mut newparser("a,,...b"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 7);
        assert!(matches!(&*el, ElementList::ElementListSpreadElement((_, Some(be), _)) if be.count == 1));
        pretty_check(
            &*el,
            "ElementList: a , , ... b",
            vec!["ElementList: a", "Elisions: ,", "SpreadElement: ... b"],
        );
    }
    #[test]
    fn element_list_test_04() {
        let (el, scanner) = check(ElementList::parse(&mut newparser("0,"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(*el, ElementList::AssignmentExpression((None, _))));
    }

    #[test]
    fn array_literal_test_01() {
        let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[]"), Scanner::new(), false, false));
        chk_scan(&scanner, 2);
        assert!(matches!(&*al, ArrayLiteral::Empty(None)));
        pretty_check(&*al, "ArrayLiteral: [ ]", vec![]);
        format!("{:?}", &*al);
    }
    #[test]
    fn array_literal_test_02() {
        let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[,]"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*al, ArrayLiteral::Empty(Some(be)) if be.count == 1));
        pretty_check(&*al, "ArrayLiteral: [ , ]", vec!["Elisions: ,"]);
    }
    #[test]
    fn array_literal_test_03() {
        let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[a]"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*al, ArrayLiteral::ElementList(_)));
        pretty_check(&*al, "ArrayLiteral: [ a ]", vec!["ElementList: a"]);
    }
    #[test]
    fn array_literal_test_04() {
        let (al, scanner) = check(ArrayLiteral::parse(
            &mut newparser("[a,]"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 4);
        assert!(matches!(*al, ArrayLiteral::ElementListElision(_, None)));
        pretty_check(&*al, "ArrayLiteral: [ a , ]", vec!["ElementList: a"]);
    }
    #[test]
    fn array_literal_test_05() {
        let (al, scanner) = check(ArrayLiteral::parse(
            &mut newparser("[a,,]"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 5);
        assert!(matches!(&*al, ArrayLiteral::ElementListElision(_, Some(be)) if be.count == 1));
        pretty_check(&*al, "ArrayLiteral: [ a , , ]", vec!["ElementList: a", "Elisions: ,"]);
    }
    #[test]
    fn array_literal_test_nones() {
        check_none(ArrayLiteral::parse(&mut newparser(""), Scanner::new(), false, false));
        check_none(ArrayLiteral::parse(&mut newparser("["), Scanner::new(), false, false));
        check_none(ArrayLiteral::parse(&mut newparser("[,,"), Scanner::new(), false, false));
        check_none(ArrayLiteral::parse(&mut newparser("[a"), Scanner::new(), false, false));
        check_none(ArrayLiteral::parse(&mut newparser("[a,"), Scanner::new(), false, false));
        check_none(ArrayLiteral::parse(
            &mut newparser("[a,,"),
            Scanner::new(),
            false,
            false,
        ));
    }

    // INITIALIZER
    #[test]
    fn initializer_test_nomatch() {
        check_none(Initializer::parse(
            &mut newparser(""),
            Scanner::new(),
            false,
            false,
            false,
        ));
        check_none(Initializer::parse(
            &mut newparser("="),
            Scanner::new(),
            false,
            false,
            false,
        ));
    }
    #[test]
    fn initializer_test_01() {
        let (izer, scanner) = check(Initializer::parse(
            &mut newparser("=a"),
            Scanner::new(),
            false,
            false,
            false,
        ));
        chk_scan(&scanner, 2);
        assert!(matches!(&*izer, Initializer::AssignmentExpression(_)));
        pretty_check(&*izer, "Initializer: = a", vec!["AssignmentExpression: a"]);
        format!("{:?}", *izer);
    }

    // COVER INITIALIZED NAME
    #[test]
    fn cover_initialized_name_test_nomatch() {
        check_none(CoverInitializedName::parse(
            &mut newparser(""),
            Scanner::new(),
            false,
            false,
        ));
        check_none(CoverInitializedName::parse(
            &mut newparser("a"),
            Scanner::new(),
            false,
            false,
        ));
    }
    #[test]
    fn cover_initialized_name_test_01() {
        let (cin, scanner) = check(CoverInitializedName::parse(
            &mut newparser("a=b"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 3);
        assert!(matches!(&*cin, CoverInitializedName::InitializedName(_, _)));
        pretty_check(
            &*cin,
            "CoverInitializedName: a = b",
            vec!["IdentifierReference: a", "Initializer: = b"],
        );
        format!("{:?}", *cin);
    }

    // COMPUTED PROPERTY NAME
    #[test]
    fn computed_property_name_test_nomatch() {
        check_none(ComputedPropertyName::parse(
            &mut newparser(""),
            Scanner::new(),
            false,
            false,
        ));
        check_none(ComputedPropertyName::parse(
            &mut newparser("["),
            Scanner::new(),
            false,
            false,
        ));
        check_none(ComputedPropertyName::parse(
            &mut newparser("[a"),
            Scanner::new(),
            false,
            false,
        ));
    }
    #[test]
    fn computed_property_name_test_01() {
        let (cpn, scanner) = check(ComputedPropertyName::parse(
            &mut newparser("[a]"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 3);
        assert!(matches!(&*cpn, ComputedPropertyName::AssignmentExpression(_)));
        pretty_check(&*cpn, "ComputedPropertyName: [ a ]", vec!["AssignmentExpression: a"]);
        format!("{:?}", &*cpn);
    }

    // LITERAL PROPERTY NAME
    #[test]
    fn literal_property_name_test_none() {
        check_none(LiteralPropertyName::parse(&mut newparser(""), Scanner::new()));
    }
    #[test]
    fn literal_property_name_test_01() {
        let (lpn, scanner) = check(LiteralPropertyName::parse(&mut newparser("b"), Scanner::new()));
        chk_scan(&scanner, 1);
        assert!(matches!(&*lpn, LiteralPropertyName::IdentifierName(_)));
        pretty_check(&*lpn, "LiteralPropertyName: b", vec![]);
        format!("{:?}", *lpn);
    }
    #[test]
    fn literal_property_name_test_02() {
        let (lpn, scanner) = check(LiteralPropertyName::parse(&mut newparser("'b'"), Scanner::new()));
        chk_scan(&scanner, 3);
        assert!(matches!(&*lpn, LiteralPropertyName::StringLiteral(_)));
        pretty_check(&*lpn, "LiteralPropertyName: \"b\"", vec![]);
    }
    #[test]
    fn literal_property_name_test_03() {
        let (lpn, scanner) = check(LiteralPropertyName::parse(&mut newparser("0"), Scanner::new()));
        chk_scan(&scanner, 1);
        assert!(matches!(&*lpn, LiteralPropertyName::NumericLiteral(_)));
        pretty_check(&*lpn, "LiteralPropertyName: 0", vec![]);
    }
    #[test]
    fn literal_property_name_test_04() {
        let (lpn, scanner) = check(LiteralPropertyName::parse(&mut newparser("1n"), Scanner::new()));
        chk_scan(&scanner, 2);
        assert!(matches!(&*lpn, LiteralPropertyName::NumericLiteral(_)));
        pretty_check(&*lpn, "LiteralPropertyName: 1", vec![]);
    }

    // PROPERTY NAME
    #[test]
    fn property_name_test_nomatch() {
        check_none(PropertyName::parse(&mut newparser(""), Scanner::new(), false, false));
    }
    #[test]
    fn property_name_test_01() {
        let (pn, scanner) = check(PropertyName::parse(&mut newparser("a"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pn, PropertyName::LiteralPropertyName(_)));
        pretty_check(&*pn, "PropertyName: a", vec!["LiteralPropertyName: a"]);
        format!("{:?}", *pn);
    }
    #[test]
    fn property_name_test_02() {
        let (pn, scanner) = check(PropertyName::parse(&mut newparser("[a]"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*pn, PropertyName::ComputedPropertyName(_)));
        pretty_check(&*pn, "PropertyName: [ a ]", vec!["ComputedPropertyName: [ a ]"]);
        format!("{:?}", *pn);
    }

    // PROPERTY DEFINITION
    #[test]
    fn property_definition_test_01() {
        let (pd, scanner) = check(PropertyDefinition::parse(
            &mut newparser("a"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pd, PropertyDefinition::IdentifierReference(_)));
        pretty_check(&*pd, "PropertyDefinition: a", vec!["IdentifierReference: a"]);
        format!("{:?}", *pd);
    }
    #[test]
    fn property_definition_test_02() {
        let (pd, scanner) = check(PropertyDefinition::parse(
            &mut newparser("a=b"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 3);
        assert!(matches!(&*pd, PropertyDefinition::CoverInitializedName(_)));
        pretty_check(&*pd, "PropertyDefinition: a = b", vec!["CoverInitializedName: a = b"]);
    }
    #[test]
    fn property_definition_test_03() {
        let (pd, scanner) = check(PropertyDefinition::parse(
            &mut newparser("a:b"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 3);
        assert!(matches!(
            &*pd,
            PropertyDefinition::PropertyNameAssignmentExpression(_, _)
        ));
        pretty_check(
            &*pd,
            "PropertyDefinition: a : b",
            vec!["PropertyName: a", "AssignmentExpression: b"],
        );
    }
    #[test]
    fn property_definition_test_04() {
        let (pd, scanner) = check(PropertyDefinition::parse(
            &mut newparser("...a"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 4);
        assert!(matches!(&*pd, PropertyDefinition::AssignmentExpression(_)));
        pretty_check(&*pd, "PropertyDefinition: ... a", vec!["AssignmentExpression: a"]);
    }
    #[test]
    fn property_definition_test_nomatch() {
        check_none(PropertyDefinition::parse(
            &mut newparser(""),
            Scanner::new(),
            false,
            false,
        ));
        check_none(PropertyDefinition::parse(
            &mut newparser("..."),
            Scanner::new(),
            false,
            false,
        ));
        check_none(PropertyDefinition::parse(
            &mut newparser("3"),
            Scanner::new(),
            false,
            false,
        ));
        check_none(PropertyDefinition::parse(
            &mut newparser("3:"),
            Scanner::new(),
            false,
            false,
        ));
    }

    #[test]
    fn property_definition_list_test_01() {
        let (pdl, scanner) = check(PropertyDefinitionList::parse(
            &mut newparser("a"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pdl, PropertyDefinitionList::OneDef(_)));
        pretty_check(&*pdl, "PropertyDefinitionList: a", vec!["PropertyDefinition: a"]);
        format!("{:?}", *pdl);
    }
    #[test]
    fn property_definition_list_test_02() {
        let (pdl, scanner) = check(PropertyDefinitionList::parse(
            &mut newparser("a,"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pdl, PropertyDefinitionList::OneDef(_)));
        pretty_check(&*pdl, "PropertyDefinitionList: a", vec!["PropertyDefinition: a"]);
    }
    #[test]
    fn property_definition_list_test_03() {
        let (pdl, scanner) = check(PropertyDefinitionList::parse(
            &mut newparser("a,b"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 3);
        assert!(matches!(&*pdl, PropertyDefinitionList::ManyDefs(_, _)));
        pretty_check(
            &*pdl,
            "PropertyDefinitionList: a , b",
            vec!["PropertyDefinitionList: a", "PropertyDefinition: b"],
        );
    }
    #[test]
    fn property_definition_list_test_04() {
        check_none(PropertyDefinitionList::parse(
            &mut newparser(""),
            Scanner::new(),
            false,
            false,
        ));
    }

    // OBJECT LITERAL
    #[test]
    fn object_literal_test_01() {
        let (ol, scanner) = check(ObjectLiteral::parse(&mut newparser("{}"), Scanner::new(), false, false));
        chk_scan(&scanner, 2);
        assert!(matches!(&*ol, ObjectLiteral::Empty));
        pretty_check(&*ol, "ObjectLiteral: { }", vec![]);
        format!("{:?}", *ol);
    }
    #[test]
    fn object_literal_test_02() {
        let (ol, scanner) = check(ObjectLiteral::parse(
            &mut newparser("{a:b}"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 5);
        assert!(matches!(&*ol, ObjectLiteral::Normal(_)));
        pretty_check(&*ol, "ObjectLiteral: { a : b }", vec!["PropertyDefinitionList: a : b"]);
    }
    #[test]
    fn object_literal_test_03() {
        let (ol, scanner) = check(ObjectLiteral::parse(
            &mut newparser("{a:b,}"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 6);
        assert!(matches!(&*ol, ObjectLiteral::TrailingComma(_)));
        pretty_check(
            &*ol,
            "ObjectLiteral: { a : b , }",
            vec!["PropertyDefinitionList: a : b"],
        );
    }
    #[test]
    fn object_literal_test_04() {
        check_none(ObjectLiteral::parse(&mut newparser(""), Scanner::new(), false, false));
        check_none(ObjectLiteral::parse(&mut newparser("{"), Scanner::new(), false, false));
        check_none(ObjectLiteral::parse(
            &mut newparser("{a:b"),
            Scanner::new(),
            false,
            false,
        ));
        check_none(ObjectLiteral::parse(
            &mut newparser("{a:b,"),
            Scanner::new(),
            false,
            false,
        ));
    }

    // PARENTHESIZED EXPRESSION
    #[test]
    fn parenthesized_expression_test_01() {
        let (pe, scanner) = check(ParenthesizedExpression::parse(
            &mut newparser("(a)"),
            Scanner::new(),
            false,
            false,
        ));
        chk_scan(&scanner, 3);
        assert!(matches!(&*pe, ParenthesizedExpression::Expression(_)));
        pretty_check(&*pe, "ParenthesizedExpression: ( a )", vec!["Expression: a"]);
        format!("{:?}", pe);
        assert_eq!(pe.is_function_definition(), false);
        assert_eq!(pe.assignment_target_type(), ATTKind::Simple);
    }
    #[test]
    fn parenthesized_expression_test_02() {
        check_none(ParenthesizedExpression::parse(
            &mut newparser(""),
            Scanner::new(),
            false,
            false,
        ));
        check_none(ParenthesizedExpression::parse(
            &mut newparser("("),
            Scanner::new(),
            false,
            false,
        ));
        check_none(ParenthesizedExpression::parse(
            &mut newparser("(0"),
            Scanner::new(),
            false,
            false,
        ));
    }

    // TEMPLATE MIDDLE LIST
    #[test]
    fn template_middle_list_test_01() {
        let (tml, scanner) = check(TemplateMiddleList::parse(
            &mut newparser("}a${0"),
            Scanner::new(),
            false,
            false,
            false,
        ));
        chk_scan(&scanner, 5);
        assert!(matches!(&*tml, TemplateMiddleList::ListEnd(_, _, _)));
        pretty_check(&*tml, "TemplateMiddleList: }a${ 0", vec!["Expression: 0"]);
        format!("{:?}", tml);
    }
    #[test]
    fn template_middle_list_test_02() {
        let (tml, scanner) = check(TemplateMiddleList::parse(
            &mut newparser("}${a}${b}"),
            Scanner::new(),
            false,
            false,
            false,
        ));
        chk_scan(&scanner, 8);
        assert!(matches!(&*tml, TemplateMiddleList::ListMid(_, _, _, _)));
        pretty_check(
            &*tml,
            "TemplateMiddleList: }${ a }${ b",
            vec!["TemplateMiddleList: }${ a", "Expression: b"],
        );
        format!("{:?}", tml);
    }
    #[test]
    fn template_middle_list_test_03() {
        check_none(TemplateMiddleList::parse(
            &mut newparser(""),
            Scanner::new(),
            false,
            false,
            false,
        ));
        check_none(TemplateMiddleList::parse(
            &mut newparser("}abc${@"),
            Scanner::new(),
            false,
            false,
            false,
        ));
    }
    #[test]
    fn template_middle_list_test_04() {
        let (tml, scanner) = check(TemplateMiddleList::parse(
            &mut newparser("}${a}${@}"),
            Scanner::new(),
            false,
            false,
            false,
        ));
        chk_scan(&scanner, 4);
        assert!(matches!(&*tml, TemplateMiddleList::ListEnd(_, _, _)));
        pretty_check(&*tml, "TemplateMiddleList: }${ a", vec!["Expression: a"]);
        format!("{:?}", tml);
    }

    // TEMPLATE SPANS
    #[test]
    fn template_spans_test_01() {
        let (ts, scanner) = check(TemplateSpans::parse(
            &mut newparser("}done`"),
            Scanner::new(),
            false,
            false,
            false,
        ));
        chk_scan(&scanner, 6);
        assert!(matches!(&*ts, TemplateSpans::Tail(_, _)));
        pretty_check(&*ts, "TemplateSpans: }done`", vec![]);
        format!("{:?}", ts);
    }
    #[test]
    fn template_spans_test_02() {
        let (ts, scanner) = check(TemplateSpans::parse(
            &mut newparser("}${a}done`"),
            Scanner::new(),
            false,
            false,
            false,
        ));
        chk_scan(&scanner, 10);
        assert!(matches!(&*ts, TemplateSpans::List(_, _, _)));
        pretty_check(&*ts, "TemplateSpans: }${ a }done`", vec!["TemplateMiddleList: }${ a"]);
        format!("{:?}", ts);
    }
    #[test]
    fn template_spans_test_03() {
        check_none(TemplateSpans::parse(
            &mut newparser(""),
            Scanner::new(),
            false,
            false,
            false,
        ));
        check_none(TemplateSpans::parse(
            &mut newparser("}${blue"),
            Scanner::new(),
            false,
            false,
            false,
        ));
    }

    // SUBSTITUTION TEMPLATE
    #[test]
    fn substitution_template_test_01() {
        let (st, scanner) = check(SubstitutionTemplate::parse(
            &mut newparser("`${a}`"),
            Scanner::new(),
            false,
            false,
            false,
        ));
        chk_scan(&scanner, 6);
        assert_eq!(st.tagged, false);
        pretty_check(
            &*st,
            "SubstitutionTemplate: `${ a }`",
            vec!["Expression: a", "TemplateSpans: }`"],
        );
        format!("{:?}", st);
    }
    #[test]
    fn substitution_template_test_02() {
        check_none(SubstitutionTemplate::parse(
            &mut newparser(""),
            Scanner::new(),
            false,
            false,
            false,
        ));
        check_none(SubstitutionTemplate::parse(
            &mut newparser("`${"),
            Scanner::new(),
            false,
            false,
            false,
        ));
        check_none(SubstitutionTemplate::parse(
            &mut newparser("`${a"),
            Scanner::new(),
            false,
            false,
            false,
        ));
    }

    // TEMPLATE LITERAL
    #[test]
    fn template_literal_test_01() {
        let (tl, scanner) = check(TemplateLiteral::parse(
            &mut newparser("`rust`"),
            Scanner::new(),
            false,
            false,
            false,
        ));
        chk_scan(&scanner, 6);
        assert!(matches!(&*tl, TemplateLiteral::NoSubstitutionTemplate(_, _)));
        if let TemplateLiteral::NoSubstitutionTemplate(_, tagged) = &*tl {
            assert_eq!(*tagged, false);
        }
        pretty_check(&*tl, "TemplateLiteral: `rust` (rust)", vec![]);
        format!("{:?}", tl);
    }
    #[test]
    fn template_literal_test_02() {
        let (tl, scanner) = check(TemplateLiteral::parse(
            &mut newparser("`${a}`"),
            Scanner::new(),
            false,
            false,
            false,
        ));
        chk_scan(&scanner, 6);
        assert!(matches!(&*tl, TemplateLiteral::SubstitutionTemplate(_)));
        pretty_check(
            &*tl,
            "TemplateLiteral: `${ a }`",
            vec!["SubstitutionTemplate: `${ a }`"],
        );
        format!("{:?}", tl);
    }
    #[test]
    fn template_literal_test_03() {
        check_none(TemplateLiteral::parse(
            &mut newparser(""),
            Scanner::new(),
            false,
            false,
            false,
        ));
        check_none(TemplateLiteral::parse(
            &mut newparser("`${"),
            Scanner::new(),
            false,
            false,
            false,
        ));
    }
}
