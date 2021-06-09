use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;
use std::rc::Rc;

use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// ClassDeclaration[Yield, Await, Default] :
//      class BindingIdentifier[?Yield, ?Await] ClassTail[?Yield, ?Await]
//      [+Default] class ClassTail[?Yield, ?Await]
#[derive(Debug)]
pub enum ClassDeclaration {
    Named(Rc<BindingIdentifier>, Rc<ClassTail>),
    Unnamed(Rc<ClassTail>),
}

impl fmt::Display for ClassDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ClassDeclaration::Named(bi, ct) => write!(f, "class {} {}", bi, ct),
            ClassDeclaration::Unnamed(ct) => write!(f, "class {}", ct),
        }
    }
}

impl PrettyPrint for ClassDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ClassDeclaration: {}", first, self)?;
        match self {
            ClassDeclaration::Named(bi, ct) => {
                bi.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ct.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ClassDeclaration::Unnamed(ct) => ct.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ClassDeclaration: {}", first, self)?;
        pprint_token(writer, "class", TokenType::Keyword, &successive, Spot::NotFinal)?;
        match self {
            ClassDeclaration::Named(bi, ct) => {
                bi.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                ct.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ClassDeclaration::Unnamed(ct) => ct.concise_with_leftpad(writer, &successive, Spot::Final),
        }
    }
}

impl ClassDeclaration {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, default_flag: bool) -> ParseResult<Self> {
        let after_class = scan_for_keyword(scanner, &parser.source, ScanGoal::InputElementDiv, Keyword::Class)?;
        let pot_bi = BindingIdentifier::parse(parser, after_class, yield_flag, await_flag);
        let (bi, after_bi) = match pot_bi {
            Ok((node, scanner)) => (Some(node), scanner),
            Err(e) => {
                if default_flag {
                    (None, after_class)
                } else {
                    return Err(e);
                }
            }
        };
        let (ct, after_ct) = ClassTail::parse(parser, after_bi, yield_flag, await_flag)?;
        match bi {
            Some(ident) => Ok((Rc::new(ClassDeclaration::Named(ident, ct)), after_ct)),
            None => Ok((Rc::new(ClassDeclaration::Unnamed(ct)), after_ct)),
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            ClassDeclaration::Named(ident, _) => ident.bound_names(),
            ClassDeclaration::Unnamed(_) => vec![JSString::from("*default*")],
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ClassDeclaration::Named(bi, ct) => bi.contains(kind) || ct.contains(kind),
            ClassDeclaration::Unnamed(ct) => ct.contains(kind),
        }
    }
}

// ClassExpression[Yield, Await] :
//      class BindingIdentifier[?Yield, ?Await]opt ClassTail[?Yield, ?Await]
#[derive(Debug)]
pub struct ClassExpression {
    ident: Option<Rc<BindingIdentifier>>,
    tail: Rc<ClassTail>,
}

impl fmt::Display for ClassExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            None => write!(f, "class {}", self.tail),
            Some(id) => write!(f, "class {} {}", id, self.tail),
        }
    }
}

impl PrettyPrint for ClassExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ClassExpression: {}", first, self)?;
        if let Some(ident) = &self.ident {
            ident.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        self.tail.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ClassExpression: {}", first, self)?;
        pprint_token(writer, "class", TokenType::Keyword, &successive, Spot::NotFinal)?;
        if let Some(ident) = &self.ident {
            ident.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        self.tail.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl IsFunctionDefinition for ClassExpression {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl ClassExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_class = scan_for_keyword(scanner, &parser.source, ScanGoal::InputElementDiv, Keyword::Class)?;
        let pot_bi = BindingIdentifier::parse(parser, after_class, yield_flag, await_flag);
        let (bi, after_bi) = match pot_bi {
            Ok((node, scanner)) => (Some(node), scanner),
            Err(_) => (None, after_class),
        };
        let (ct, after_ct) = ClassTail::parse(parser, after_bi, yield_flag, await_flag)?;
        Ok((Rc::new(ClassExpression { ident: bi, tail: ct }), after_ct))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.ident.as_ref().map_or(false, |n| n.contains(kind)) || self.tail.contains(kind)
    }
}

// ClassTail[Yield, Await] :
//      ClassHeritage[?Yield, ?Await]opt { ClassBody[?Yield, ?Await]opt }
#[derive(Debug)]
pub struct ClassTail {
    heritage: Option<Rc<ClassHeritage>>,
    body: Option<Rc<ClassBody>>,
}

impl fmt::Display for ClassTail {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (&self.heritage, &self.body) {
            (Some(h), Some(b)) => write!(f, "{} {{ {} }}", h, b),
            (None, Some(b)) => write!(f, "{{ {} }}", b),
            (Some(h), None) => write!(f, "{} {{ }}", h),
            (None, None) => write!(f, "{{ }}"),
        }
    }
}

impl PrettyPrint for ClassTail {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ClassTail: {}", first, self)?;
        match &self.body {
            Some(b) => {
                if let Some(h) = &self.heritage {
                    h.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                }
                b.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            None => {
                if let Some(h) = &self.heritage {
                    h.pprint_with_leftpad(writer, &successive, Spot::Final)?;
                }
                Ok(())
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ClassTail: {}", first, self)?;
        if let Some(h) = &self.heritage {
            h.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        if let Some(b) = &self.body {
            b.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ClassTail {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let pot_heritage = ClassHeritage::parse(parser, scanner, yield_flag, await_flag);
        let (heritage, after_heritage) = match pot_heritage {
            Ok((n, s)) => (Some(n), s),
            Err(_) => (None, scanner),
        };
        let after_lb = scan_for_punct(after_heritage, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let pot_body = ClassBody::parse(parser, after_lb, yield_flag, await_flag);
        let (body, after_body) = match pot_body {
            Ok((n, s)) => (Some(n), s),
            Err(_) => (None, after_lb),
        };
        let after_rb = scan_for_punct(after_body, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(ClassTail { heritage, body }), after_rb))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.class_tail_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.class_tail_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match kind {
            ParseNodeKind::ClassBody => self.body.is_some(),
            ParseNodeKind::ClassHeritage => self.heritage.is_some(),
            _ => self.heritage.as_ref().map_or(false, |n| n.contains(kind)) || self.body.as_ref().map_or(false, |n| n.computed_property_contains(kind)),
        }
    }
}

// ClassHeritage[Yield, Await] :
//      extends LeftHandSideExpression[?Yield, ?Await]
#[derive(Debug)]
pub struct ClassHeritage(Rc<LeftHandSideExpression>);

impl fmt::Display for ClassHeritage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "extends {}", self.0)
    }
}

impl PrettyPrint for ClassHeritage {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ClassHeritage: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ClassHeritage: {}", first, self)?;
        pprint_token(writer, "extends", TokenType::Keyword, &successive, Spot::NotFinal)?;
        self.0.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl ClassHeritage {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_extends = scan_for_keyword(scanner, &parser.source, ScanGoal::InputElementDiv, Keyword::Extends)?;
        let (lhs, after_lhs) = LeftHandSideExpression::parse(parser, after_extends, yield_flag, await_flag)?;
        Ok((Rc::new(ClassHeritage(lhs)), after_lhs))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.0.contains(kind)
    }
}

// ClassBody[Yield, Await] :
//      ClassElementList[?Yield, ?Await]
#[derive(Debug)]
pub struct ClassBody(Rc<ClassElementList>);

impl fmt::Display for ClassBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PrettyPrint for ClassBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ClassBody: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.0.concise_with_leftpad(writer, pad, state)
    }
}

impl ClassBody {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (el, after_el) = ClassElementList::parse(parser, scanner, yield_flag, await_flag)?;
        Ok((Rc::new(ClassBody(el)), after_el))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.0.contains(kind)
    }

    pub fn computed_property_contains(&self, kind: ParseNodeKind) -> bool {
        self.0.computed_property_contains(kind)
    }
}

// ClassElementList[Yield, Await] :
//      ClassElement[?Yield, ?Await]
//      ClassElementList[?Yield, ?Await] ClassElement[?Yield, ?Await]
#[derive(Debug)]
pub enum ClassElementList {
    Item(Rc<ClassElement>),
    List(Rc<ClassElementList>, Rc<ClassElement>),
}

impl fmt::Display for ClassElementList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ClassElementList::Item(item) => item.fmt(f),
            ClassElementList::List(list, item) => {
                write!(f, "{} {}", list, item)
            }
        }
    }
}

impl PrettyPrint for ClassElementList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ClassElementList: {}", first, self)?;
        match self {
            ClassElementList::Item(item) => item.pprint_with_leftpad(writer, &successive, Spot::Final),
            ClassElementList::List(list, item) => {
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
            ClassElementList::Item(item) => item.concise_with_leftpad(writer, pad, state),
            ClassElementList::List(list, item) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}ClassElementList: {}", first, self)?;
                list.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl ClassElementList {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (ce, mut current_scanner) = ClassElement::parse(parser, scanner, yield_flag, await_flag)?;
        let mut current = Rc::new(ClassElementList::Item(ce));
        while let Ok((node, after)) = ClassElement::parse(parser, current_scanner, yield_flag, await_flag) {
            current = Rc::new(ClassElementList::List(current, node));
            current_scanner = after;
        }
        Ok((current, current_scanner))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ClassElementList::Item(item) => item.contains(kind),
            ClassElementList::List(list, item) => list.contains(kind) || item.contains(kind),
        }
    }

    pub fn computed_property_contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ClassElementList::Item(item) => item.computed_property_contains(kind),
            ClassElementList::List(list, item) => list.computed_property_contains(kind) || item.computed_property_contains(kind),
        }
    }
}

// ClassElement[Yield, Await] :
//      MethodDefinition[?Yield, ?Await]
//      static MethodDefinition[?Yield, ?Await]
//      ;
#[derive(Debug)]
pub enum ClassElement {
    Standard(Rc<MethodDefinition>),
    Static(Rc<MethodDefinition>),
    Empty,
}

impl fmt::Display for ClassElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ClassElement::Standard(n) => n.fmt(f),
            ClassElement::Static(n) => write!(f, "static {}", n),
            ClassElement::Empty => f.write_str(";"),
        }
    }
}

impl PrettyPrint for ClassElement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ClassElement: {}", first, self)?;
        match self {
            ClassElement::Standard(n) => n.pprint_with_leftpad(writer, &successive, Spot::Final),
            ClassElement::Static(n) => n.pprint_with_leftpad(writer, &successive, Spot::Final),
            ClassElement::Empty => Ok(()),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            ClassElement::Static(n) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}ClassElement: {}", first, self)?;
                pprint_token(writer, "static", TokenType::Keyword, &successive, Spot::NotFinal)?;
                n.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ClassElement::Standard(n) => n.concise_with_leftpad(writer, pad, state),
            ClassElement::Empty => pprint_token(writer, ";", TokenType::Punctuator, pad, state),
        }
    }
}

impl ClassElement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("ClassElement expected", scanner.line, scanner.column)).otherwise(|| {
            scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Static)
                .and_then(|after_static| MethodDefinition::parse(parser, after_static, yield_flag, await_flag).map(|(md, after_md)| (Rc::new(ClassElement::Static(md)), after_md)))
                .otherwise(|| scan_for_punct(scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Semicolon).map(|after_semi| (Rc::new(ClassElement::Empty), after_semi)))
                .otherwise(|| MethodDefinition::parse(parser, scanner, yield_flag, await_flag).map(|(md, after_md)| (Rc::new(ClassElement::Standard(md)), after_md)))
        })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ClassElement::Standard(n) | ClassElement::Static(n) => kind == ParseNodeKind::MethodDefinition || n.contains(kind),
            ClassElement::Empty => false,
        }
    }

    pub fn computed_property_contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ClassElement::Standard(n) | ClassElement::Static(n) => n.computed_property_contains(kind),
            ClassElement::Empty => false,
        }
    }
}

#[cfg(test)]
mod tests;