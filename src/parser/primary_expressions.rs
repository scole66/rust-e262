use num::bigint::BigInt;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::assignment_operators::AssignmentExpression;
use super::identifiers::{IdentifierNameToken, IdentifierReference};
use super::method_definitions::MethodDefinition;
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

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
            PrimaryExpressionKind::ArrayLiteral(boxed) => write!(f, "{}", boxed),
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
            PrimaryExpressionKind::IdentifierReference(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::Literal(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            PrimaryExpressionKind::ArrayLiteral(boxed) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
}

impl IsFunctionDefinition for PrimaryExpression {
    fn is_function_definition(&self) -> bool {
        use PrimaryExpressionKind::*;
        match self.kind {
            This | IdentifierReference(_) | Literal(_) | ArrayLiteral(_) => false,
        }
    }
}

impl IsIdentifierReference for PrimaryExpression {
    fn is_identifier_reference(&self) -> bool {
        use PrimaryExpressionKind::*;
        match &self.kind {
            This | Literal(_) | ArrayLiteral(_) => false,
            IdentifierReference(_) => true,
        }
    }
}

impl AssignmentTargetType for PrimaryExpression {
    fn assignment_target_type(&self) -> ATTKind {
        use PrimaryExpressionKind::*;
        match &self.kind {
            This | Literal(_) | ArrayLiteral(_) => ATTKind::Invalid,
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

impl ToPrimaryExpressionKind for ArrayLiteral {
    fn to_primary_expression_kind(node: Box<Self>) -> PrimaryExpressionKind {
        PrimaryExpressionKind::ArrayLiteral(node)
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

fn or_pe_kind<F, T>(opt: Option<(Box<PrimaryExpression>, Scanner)>, parser: &mut Parser, parse_func: F) -> Result<Option<(Box<PrimaryExpression>, Scanner)>, String>
where
    F: FnOnce(&mut Parser) -> Result<Option<(Box<T>, Scanner)>, String>,
    T: ToPrimaryExpressionKind,
{
    opt.map_or_else(|| parse_func(parser).and_then(pe_boxer), rewrap)
}

impl PrimaryExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, arg_yield: bool, arg_await: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        Ok(None)
            .and_then(|opt| or_pe_kind(opt, parser, |p| IdentifierReference::parse(p, scanner, arg_yield, arg_await)))
            .and_then(|opt| or_pe_kind(opt, parser, |p| Literal::parse(p, scanner)))
            .and_then(|opt| or_pe_kind(opt, parser, |p| this_token(p, scanner)))
            .and_then(|opt| or_pe_kind(opt, parser, |p| ArrayLiteral::parse(p, scanner, arg_yield, arg_await)))
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
    let (tok, scanner) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
    Ok(match tok {
        scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::This) => Some((Box::new(ThisToken {}), scanner)),
        _ => None,
    })
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
            let (token, after_comma) = scanner::scan_token(&current_scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
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
    fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<SpreadElement>, Scanner)>, String> {
        let (token, after_ellipsis) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
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
    ) -> Result<(Option<Box<Elisions>>, Option<Box<AssignmentExpression>>, Option<Box<SpreadElement>>, Scanner), String> {
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

    fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<ElementList>, Scanner)>, String> {
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
            let (token, after_tok) = scanner::scan_token(&current_scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
            match token {
                scanner::Token::Comma => {
                    let (elision, boxed_ae, boxed_se, after) = Self::non_recursive_part(parser, after_tok, yield_flag, await_flag)?;
                    if boxed_ae.is_some() {
                        current_production = Box::new(ElementList::ElementListAssignmentExpression((current_production, elision, boxed_ae.unwrap())));
                        current_scanner = after;
                    } else if boxed_se.is_some() {
                        current_production = Box::new(ElementList::ElementListSpreadElement((current_production, elision, boxed_se.unwrap())));
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
            ArrayLiteral::ElementList(boxed) | ArrayLiteral::ElementListElision(boxed, None) => boxed.pprint_with_leftpad(writer, &successive, Spot::Final),
            ArrayLiteral::ElementListElision(boxed, Some(elision)) => {
                boxed.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                elision.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl ArrayLiteral {
    fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (token, after) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
        if token != scanner::Token::LeftBracket {
            return Ok(None);
        }
        let pot_el = ElementList::parse(parser, after, yield_flag, await_flag)?;
        match pot_el {
            None => {
                let pot_elision = Elisions::parse(parser, after)?;
                match pot_elision {
                    None => {
                        let (closing, end_scan) = scanner::scan_token(&after, parser.source, scanner::ScanGoal::InputElementRegExp)?;
                        if closing == scanner::Token::RightBracket {
                            return Ok(Some((Box::new(ArrayLiteral::Empty(None)), end_scan)));
                        }
                        return Ok(None);
                    }
                    Some((elisions, after_2)) => {
                        let (closing, end_scan) = scanner::scan_token(&after_2, parser.source, scanner::ScanGoal::InputElementRegExp)?;
                        if closing == scanner::Token::RightBracket {
                            return Ok(Some((Box::new(ArrayLiteral::Empty(Some(elisions))), end_scan)));
                        }
                        return Ok(None);
                    }
                }
            }
            Some((boxed_el, after_el_scan)) => {
                let (comma_or_bracket, after_cb) = scanner::scan_token(&after_el_scan, parser.source, scanner::ScanGoal::InputElementRegExp)?;
                match comma_or_bracket {
                    scanner::Token::RightBracket => {
                        return Ok(Some((Box::new(ArrayLiteral::ElementList(boxed_el)), after_cb)));
                    }
                    scanner::Token::Comma => {
                        let pot_elision = Elisions::parse(parser, after_cb)?;
                        match pot_elision {
                            None => {
                                let (closing, end_scan) = scanner::scan_token(&after_cb, parser.source, scanner::ScanGoal::InputElementRegExp)?;
                                if closing == scanner::Token::RightBracket {
                                    return Ok(Some((Box::new(ArrayLiteral::ElementListElision(boxed_el, None)), end_scan)));
                                }
                                return Ok(None);
                            }
                            Some((elisions, after_2)) => {
                                let (closing, end_scan) = scanner::scan_token(&after_2, parser.source, scanner::ScanGoal::InputElementRegExp)?;
                                if closing == scanner::Token::RightBracket {
                                    return Ok(Some((Box::new(ArrayLiteral::ElementListElision(boxed_el, Some(elisions))), end_scan)));
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
    fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Initializer>, Scanner)>, String> {
        let (token, after_tok) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
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
    fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_idref = IdentifierReference::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_idref {
            None => Ok(None),
            Some((idref, after_idref)) => {
                let pot_init = Initializer::parse(parser, after_idref, true, yield_flag, await_flag)?;
                match pot_init {
                    None => Ok(None),
                    Some((izer, after_izer)) => Ok(Some((Box::new(CoverInitializedName::InitializedName(idref, izer)), after_izer))),
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
    fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok, after_tok) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
        match tok {
            scanner::Token::LeftBracket => {
                let pot_ae = AssignmentExpression::parse(parser, after_tok, true, yield_flag, await_flag)?;
                match pot_ae {
                    None => Ok(None),
                    Some((ae, after_ae)) => {
                        let (tok2, after_rb) = scanner::scan_token(&after_ae, parser.source, scanner::ScanGoal::InputElementRegExp)?;
                        match tok2 {
                            scanner::Token::RightBracket => Ok(Some((Box::new(ComputedPropertyName::AssignmentExpression(ae)), after_rb))),
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
            LiteralPropertyName::NumericLiteral(n) => write!(f, "{:?}", n),
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
        let (tok, after_tok) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
        match tok {
            scanner::Token::Identifier(id) => Ok(Some((
                Box::new(LiteralPropertyName::IdentifierName(Box::new(IdentifierNameToken {
                    value: scanner::Token::Identifier(id),
                }))),
                after_tok,
            ))),
            scanner::Token::String(s) => Ok(Some((Box::new(LiteralPropertyName::StringLiteral(s)), after_tok))),
            scanner::Token::Number(n) => Ok(Some((Box::new(LiteralPropertyName::NumericLiteral(Numeric::Number(n))), after_tok))),
            scanner::Token::BigInt(b) => Ok(Some((Box::new(LiteralPropertyName::NumericLiteral(Numeric::BigInt(b))), after_tok))),
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
    fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
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
            PropertyDefinition::IdentifierReference(idref) => idref.pprint_with_leftpad(writer, &successive, Spot::Final),
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
    fn parse_pn_ae(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_pn = PropertyName::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_pn {
            Some((pn, after_pn)) => {
                let (tok, after_tok) = scanner::scan_token(&after_pn, parser.source, scanner::ScanGoal::InputElementRegExp)?;
                match tok {
                    scanner::Token::Colon => {
                        let pot_ae = AssignmentExpression::parse(parser, after_tok, true, yield_flag, await_flag)?;
                        match pot_ae {
                            Some((ae, after_ae)) => Ok(Some((Box::new(PropertyDefinition::PropertyNameAssignmentExpression(pn, ae)), after_ae))),
                            None => Ok(None),
                        }
                    }
                    _ => Ok(None),
                }
            }
            None => Ok(None),
        }
    }

    fn parse_cin(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_cin = CoverInitializedName::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_cin {
            Some((cin, after_cin)) => Ok(Some((Box::new(PropertyDefinition::CoverInitializedName(cin)), after_cin))),
            None => Ok(None),
        }
    }

    fn parse_md(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_md = MethodDefinition::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_md {
            Some((md, after_md)) => Ok(Some((Box::new(PropertyDefinition::MethodDefinition(md)), after_md))),
            None => Ok(None),
        }
    }

    fn parse_idref(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_idref = IdentifierReference::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_idref {
            Some((idref, after_idref)) => Ok(Some((Box::new(PropertyDefinition::IdentifierReference(idref)), after_idref))),
            None => Ok(None),
        }
    }

    fn parse_ae(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok, after_tok) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
        match tok {
            scanner::Token::Ellipsis => {
                let pot_ae = AssignmentExpression::parse(parser, after_tok, true, yield_flag, await_flag)?;
                match pot_ae {
                    Some((ae, after_ae)) => Ok(Some((Box::new(PropertyDefinition::AssignmentExpression(ae)), after_ae))),
                    None => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<Option<(Box<Self>, Scanner)>, String> {
        Self::parse_pn_ae(parser, scanner, yield_flag, await_flag)
            .and_then(|opt| opt.map_or_else(|| Self::parse_cin(parser, scanner, yield_flag, await_flag), rewrap))
            .and_then(|opt| opt.map_or_else(|| Self::parse_md(parser, scanner, yield_flag, await_flag), rewrap))
            .and_then(|opt| opt.map_or_else(|| Self::parse_idref(parser, scanner, yield_flag, await_flag), rewrap))
            .and_then(|opt| opt.map_or_else(|| Self::parse_ae(parser, scanner, yield_flag, await_flag), rewrap))
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
            LiteralKind::NumericLiteral(n) => write!(f, "{:?}", *n),
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
        let scan_result = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp)?;
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
}

#[derive(Debug)]
pub struct TemplateLiteral {}
impl fmt::Display for TemplateLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "`unimplemented`")
    }
}

impl PrettyPrint for TemplateLiteral {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{}TemplateLiteral: {}", first, self)
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
        let (pe1, _) = check(PrimaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false));
        pretty_check(&*pe1, "PrimaryExpression: this", vec![]);
        let (pe2, _) = check(PrimaryExpression::parse(&mut newparser("1"), Scanner::new(), false, false));
        pretty_check(&*pe2, "PrimaryExpression: Number(1.0)", vec!["Literal: Number(1.0)"]);
        let (pe3, _) = check(PrimaryExpression::parse(&mut newparser("i"), Scanner::new(), false, false));
        pretty_check(&*pe3, "PrimaryExpression: i", vec!["IdentifierReference: i"]);
        let (pe4, _) = check(PrimaryExpression::parse(&mut newparser("[]"), Scanner::new(), false, false));
        pretty_check(&*pe4, "PrimaryExpression: [ ]", vec!["ArrayLiteral: [ ]"]);
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
        let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("371"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(node.kind, PrimaryExpressionKind::Literal(_)));
        assert_eq!(node.is_function_definition(), false);
        assert_eq!(node.is_identifier_reference(), false);
        assert_eq!(node.assignment_target_type(), ATTKind::Invalid);
    }
    #[test]
    fn primary_expression_test_this() {
        let (node, scanner) = check(PrimaryExpression::parse(&mut newparser("this"), Scanner::new(), false, false));
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
        assert_eq!(format!("{:?}", Literal { kind: LiteralKind::NullLiteral }), "Literal { kind: NullLiteral }");
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
        pretty_check(&*lit, "Literal: Number(0.25)", vec![]);
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
        check_none(SpreadElement::parse(&mut newparser("..."), Scanner::new(), false, false));
    }
    #[test]
    fn spread_element_test_assignment_expression() {
        let (se, scanner) = check(SpreadElement::parse(&mut newparser("...1"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*se, SpreadElement::AssignmentExpression(_)));
    }
    #[test]
    fn spread_element_test_pretty() {
        let (se, _) = check(SpreadElement::parse(&mut newparser("...1"), Scanner::new(), false, false));
        pretty_check(&*se, "SpreadElement: ... Number(1.0)", vec!["AssignmentExpression: Number(1.0)"]);
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
        pretty_check(&*el, "ElementList: Number(3.0)", vec!["AssignmentExpression: Number(3.0)"]);
    }
    #[test]
    fn element_list_test_03() {
        let (el, scanner) = check(ElementList::parse(&mut newparser(",,3"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*el, ElementList::AssignmentExpression((Some(be), _)) if be.count == 2));
        pretty_check(&*el, "ElementList: , , Number(3.0)", vec!["Elisions: , ,", "AssignmentExpression: Number(3.0)"]);
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
        let (el, scanner) = check(ElementList::parse(&mut newparser(",,...a"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(&*el, ElementList::SpreadElement((Some(be), _)) if be.count == 2));
        pretty_check(&*el, "ElementList: , , ... a", vec!["Elisions: , ,", "SpreadElement: ... a"]);
    }
    #[test]
    fn element_list_test_07() {
        let (el, scanner) = check(ElementList::parse(&mut newparser("a,b"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*el, ElementList::ElementListAssignmentExpression((_, None, _))));
        pretty_check(&*el, "ElementList: a , b", vec!["ElementList: a", "AssignmentExpression: b"]);
    }
    #[test]
    fn element_list_test_08() {
        let (el, scanner) = check(ElementList::parse(&mut newparser("a,,b"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*el, ElementList::ElementListAssignmentExpression((_, Some(be), _)) if be.count == 1));
        pretty_check(&*el, "ElementList: a , , b", vec!["ElementList: a", "Elisions: ,", "AssignmentExpression: b"]);
    }
    #[test]
    fn element_list_test_09() {
        let (el, scanner) = check(ElementList::parse(&mut newparser("a,...b"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        assert!(matches!(&*el, ElementList::ElementListSpreadElement((_, None, _))));
        pretty_check(&*el, "ElementList: a , ... b", vec!["ElementList: a", "SpreadElement: ... b"]);
    }
    #[test]
    fn element_list_test_10() {
        let (el, scanner) = check(ElementList::parse(&mut newparser("a,,...b"), Scanner::new(), false, false));
        chk_scan(&scanner, 7);
        assert!(matches!(&*el, ElementList::ElementListSpreadElement((_, Some(be), _)) if be.count == 1));
        pretty_check(&*el, "ElementList: a , , ... b", vec!["ElementList: a", "Elisions: ,", "SpreadElement: ... b"]);
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
        let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[a,]"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(*al, ArrayLiteral::ElementListElision(_, None)));
        pretty_check(&*al, "ArrayLiteral: [ a , ]", vec!["ElementList: a"]);
    }
    #[test]
    fn array_literal_test_05() {
        let (al, scanner) = check(ArrayLiteral::parse(&mut newparser("[a,,]"), Scanner::new(), false, false));
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
        check_none(ArrayLiteral::parse(&mut newparser("[a,,"), Scanner::new(), false, false));
    }

    // INITIALIZER
    #[test]
    fn initializer_test_nomatch() {
        check_none(Initializer::parse(&mut newparser(""), Scanner::new(), false, false, false));
        check_none(Initializer::parse(&mut newparser("="), Scanner::new(), false, false, false));
    }
    #[test]
    fn initializer_test_01() {
        let (izer, scanner) = check(Initializer::parse(&mut newparser("=a"), Scanner::new(), false, false, false));
        chk_scan(&scanner, 2);
        assert!(matches!(&*izer, Initializer::AssignmentExpression(_)));
        pretty_check(&*izer, "Initializer: = a", vec!["AssignmentExpression: a"]);
        format!("{:?}", *izer);
    }

    // COVER INITIALIZED NAME
    #[test]
    fn cover_initialized_name_test_nomatch() {
        check_none(CoverInitializedName::parse(&mut newparser(""), Scanner::new(), false, false));
        check_none(CoverInitializedName::parse(&mut newparser("a"), Scanner::new(), false, false));
    }
    #[test]
    fn cover_initialized_name_test_01() {
        let (cin, scanner) = check(CoverInitializedName::parse(&mut newparser("a=b"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*cin, CoverInitializedName::InitializedName(_, _)));
        pretty_check(&*cin, "CoverInitializedName: a = b", vec!["IdentifierReference: a", "Initializer: = b"]);
        format!("{:?}", *cin);
    }

    // COMPUTED PROPERTY NAME
    #[test]
    fn computed_property_name_test_nomatch() {
        check_none(ComputedPropertyName::parse(&mut newparser(""), Scanner::new(), false, false));
        check_none(ComputedPropertyName::parse(&mut newparser("["), Scanner::new(), false, false));
        check_none(ComputedPropertyName::parse(&mut newparser("[a"), Scanner::new(), false, false));
    }
    #[test]
    fn computed_property_name_test_01() {
        let (cpn, scanner) = check(ComputedPropertyName::parse(&mut newparser("[a]"), Scanner::new(), false, false));
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
        pretty_check(&*lpn, "LiteralPropertyName: Number(0.0)", vec![]);
    }
    #[test]
    fn literal_property_name_test_04() {
        let (lpn, scanner) = check(LiteralPropertyName::parse(&mut newparser("1n"), Scanner::new()));
        chk_scan(&scanner, 2);
        assert!(matches!(&*lpn, LiteralPropertyName::NumericLiteral(_)));
        pretty_check(&*lpn, "LiteralPropertyName: BigInt(BigInt { sign: Plus, data: BigUint { data: [1] } })", vec![]);
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
        let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("a"), Scanner::new(), false, false));
        chk_scan(&scanner, 1);
        assert!(matches!(&*pd, PropertyDefinition::IdentifierReference(_)));
        pretty_check(&*pd, "PropertyDefinition: a", vec!["IdentifierReference: a"]);
        format!("{:?}", *pd);
    }
    #[test]
    fn property_definition_test_02() {
        let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("a=b"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*pd, PropertyDefinition::CoverInitializedName(_)));
        pretty_check(&*pd, "PropertyDefinition: a = b", vec!["CoverInitializedName: a = b"]);
    }
    #[test]
    fn property_definition_test_03() {
        let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("a:b"), Scanner::new(), false, false));
        chk_scan(&scanner, 3);
        assert!(matches!(&*pd, PropertyDefinition::PropertyNameAssignmentExpression(_, _)));
        pretty_check(&*pd, "PropertyDefinition: a : b", vec!["PropertyName: a", "AssignmentExpression: b"]);
    }
    #[test]
    fn property_definition_test_04() {
        let (pd, scanner) = check(PropertyDefinition::parse(&mut newparser("...a"), Scanner::new(), false, false));
        chk_scan(&scanner, 4);
        assert!(matches!(&*pd, PropertyDefinition::AssignmentExpression(_)));
        pretty_check(&*pd, "PropertyDefinition: ... a", vec!["AssignmentExpression: a"]);
    }
    #[test]
    fn property_definition_test_nomatch() {
        check_none(PropertyDefinition::parse(&mut newparser(""), Scanner::new(), false, false));
        check_none(PropertyDefinition::parse(&mut newparser("..."), Scanner::new(), false, false));
        check_none(PropertyDefinition::parse(&mut newparser("3"), Scanner::new(), false, false));
        check_none(PropertyDefinition::parse(&mut newparser("3:"), Scanner::new(), false, false));
    }
}
