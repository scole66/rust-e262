use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::identifiers::BindingIdentifier;
use super::primary_expressions::{Elisions, Initializer, PropertyName};
use super::scanner::{Keyword, Punctuator, ScanGoal, Scanner};
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// LexicalDeclaration[In, Yield, Await] :
//      LetOrConst BindingList[?In, ?Yield, ?Await] ;
#[derive(Debug)]
pub enum LexicalDeclaration {
    List(LetOrConst, Box<BindingList>),
}

impl fmt::Display for LexicalDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let LexicalDeclaration::List(loc, bl) = self;
        write!(f, "{} {} ;", *loc, bl)
    }
}

impl PrettyPrint for LexicalDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}LexicalDeclaration: {}", first, self)?;
        let LexicalDeclaration::List(loc, bl) = self;
        loc.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        bl.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}LexicalDeclaration: {}", first, self)?;
        let LexicalDeclaration::List(loc, bl) = self;
        loc.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        bl.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl LexicalDeclaration {
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        scan_for_keywords(scanner, parser.source, ScanGoal::InputElementRegExp, &[Keyword::Let, Keyword::Const]).and_then(|(kwd, after_tok)| {
            let loc = match kwd {
                Keyword::Let => LetOrConst::Let,
                Keyword::Const | _ => LetOrConst::Const,
            };
            BindingList::parse(parser, after_tok, in_flag, yield_flag, await_flag).and_then(|(bl, after_bl)| {
                scan_for_punct(after_bl, parser.source, ScanGoal::InputElementRegExp, Punctuator::Semicolon)
                    .and_then(|after_semi| Ok((Box::new(LexicalDeclaration::List(loc, bl)), after_semi)))
            })
        })

        //let (tok, after_tok) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        //let loc = match tok {
        //    Token::Identifier(id) if id.matches(Keyword::Let) => LetOrConst::Let,
        //    Token::Identifier(id) if id.matches(Keyword::Const) => LetOrConst::Const,
        //    _ => {
        //        return Ok(None);
        //    }
        //};
        //
        //let pot_bl = BindingList::parse(parser, after_tok, in_flag, yield_flag, await_flag)?;
        //let (bl, after_bl) = match pot_bl {
        //    None => {
        //        return Ok(None);
        //    }
        //    Some(x) => x,
        //};
        //
        //let (semi, after_semi) = scan_token(&after_bl, parser.source, ScanGoal::InputElementRegExp);
        //match semi {
        //    Token::Punctuator(Punctuator::Semicolon) => Ok(Some((Box::new(LexicalDeclaration::List(loc, bl)), after_semi))),
        //    _ => Ok(None),
        //}
    }
}

// LetOrConst :
//      let
//      const
#[derive(Debug, PartialEq)]
pub enum LetOrConst {
    Let,
    Const,
}

impl fmt::Display for LetOrConst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LetOrConst::Let => write!(f, "let"),
            LetOrConst::Const => write!(f, "const"),
        }
    }
}

impl PrettyPrint for LetOrConst {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, _) = prettypad(pad, state);
        writeln!(writer, "{}LetOrConst: {}", first, self)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        pprint_token(writer, self, TokenType::Keyword, pad, state)
    }
}

// BindingList[In, Yield, Await] :
//      LexicalBinding[?In, ?Yield, ?Await]
//      BindingList[?In, ?Yield, ?Await] , LexicalBinding[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum BindingList {
    Item(Box<LexicalBinding>),
    List(Box<BindingList>, Box<LexicalBinding>),
}

impl fmt::Display for BindingList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BindingList::Item(node) => node.fmt(f),
            BindingList::List(lst, item) => write!(f, "{} , {}", lst, item),
        }
    }
}

impl PrettyPrint for BindingList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BindingList: {}", first, self)?;
        match self {
            BindingList::Item(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            BindingList::List(lst, item) => {
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
            BindingList::Item(node) => node.concise_with_leftpad(writer, pad, state),
            BindingList::List(lst, item) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}BindingList: {}", first, self)?;
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl BindingList {
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (lb, after_lb) = LexicalBinding::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        let mut current = Box::new(BindingList::Item(lb));
        let mut current_scanner = after_lb;
        loop {
            match scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
                .and_then(|after_tok| LexicalBinding::parse(parser, after_tok, in_flag, yield_flag, await_flag))
            {
                Err(_) => {
                    break;
                }
                Ok((lb2, after_lb2)) => {
                    current = Box::new(BindingList::List(current, lb2));
                    current_scanner = after_lb2;
                }
            }
        }
        Ok((current, current_scanner))

        //let pot_lb = LexicalBinding::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        //match pot_lb {
        //    None => Ok(None),
        //    Some((lb, after_lb)) => {
        //        let mut current = Box::new(BindingList::Item(lb));
        //        let mut current_scanner = after_lb;
        //        loop {
        //            let (tok, after_tok) = scan_token(&current_scanner, parser.source, ScanGoal::InputElementDiv);
        //            match tok {
        //                Token::Punctuator(Punctuator::Comma) => {
        //                    let pot_lb2 = LexicalBinding::parse(parser, after_tok, in_flag, yield_flag, await_flag)?;
        //                    match pot_lb2 {
        //                        None => {
        //                            break;
        //                        }
        //                        Some((lb2, after_lb2)) => {
        //                            current = Box::new(BindingList::List(current, lb2));
        //                            current_scanner = after_lb2;
        //                        }
        //                    }
        //                }
        //                _ => {
        //                    break;
        //                }
        //            }
        //        }
        //        Ok(Some((current, current_scanner)))
        //    }
        //}
    }
}

// LexicalBinding[In, Yield, Await] :
//      BindingIdentifier[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]opt
//      BindingPattern[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum LexicalBinding {
    Identifier(Box<BindingIdentifier>, Option<Box<Initializer>>),
    Pattern(Box<BindingPattern>, Box<Initializer>),
}

impl fmt::Display for LexicalBinding {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexicalBinding::Identifier(bi, Some(i)) => write!(f, "{} {}", bi, i),
            LexicalBinding::Identifier(bi, None) => bi.fmt(f),
            LexicalBinding::Pattern(bp, i) => write!(f, "{} {}", bp, i),
        }
    }
}

impl PrettyPrint for LexicalBinding {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}LexicalBinding: {}", first, self)?;
        match self {
            LexicalBinding::Identifier(bi, Some(i)) => {
                bi.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                i.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            LexicalBinding::Identifier(bi, None) => bi.pprint_with_leftpad(writer, &successive, Spot::Final),
            LexicalBinding::Pattern(bp, i) => {
                bp.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                i.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let head = |writer: &mut T| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}LexicalBinding: {}", first, self).and(Ok(successive))
        };
        match self {
            LexicalBinding::Identifier(bi, None) => bi.concise_with_leftpad(writer, pad, state),
            LexicalBinding::Identifier(bi, Some(i)) => {
                let successive = head(writer)?;
                bi.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                i.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            LexicalBinding::Pattern(bp, i) => {
                let successive = head(writer)?;
                bp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                i.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl LexicalBinding {
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)
            .and_then(|(bi, after_bi)| {
                let (init, after_init) = match Initializer::parse(parser, after_bi, in_flag, yield_flag, await_flag) {
                    Err(_) => (None, after_bi),
                    Ok((i, after_i)) => (Some(i), after_i),
                };
                Ok((Box::new(LexicalBinding::Identifier(bi, init)), after_init))
            })
            //let pot_bi = BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
            //if let Some((bi, after_bi)) = pot_bi {
            //    let pot_init = Initializer::parse(parser, after_bi, in_flag, yield_flag, await_flag)?;
            //    let (init, after_init) = match pot_init {
            //        None => (None, after_bi),
            //        Some((i, after_i)) => (Some(i), after_i),
            //    };
            //    return Ok(Some((Box::new(LexicalBinding::Identifier(bi, init)), after_init)));
            //}
            .otherwise(|| {
                BindingPattern::parse(parser, scanner, yield_flag, await_flag).and_then(|(bp, after_bp)| {
                    Initializer::parse(parser, after_bp, in_flag, yield_flag, await_flag).and_then(|(init, after_init)| Ok((Box::new(LexicalBinding::Pattern(bp, init)), after_init)))
                })
            })

        //let pot_bp = BindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
        //if let Some((bp, after_bp)) = pot_bp {
        //    let pot_init = Initializer::parse(parser, after_bp, in_flag, yield_flag, await_flag)?;
        //    if let Some((init, after_init)) = pot_init {
        //        return Ok(Some((Box::new(LexicalBinding::Pattern(bp, init)), after_init)));
        //    }
        //}
        //
        //Ok(None)
    }
}

// VariableStatement[Yield, Await] :
//      var VariableDeclarationList[+In, ?Yield, ?Await] ;
#[derive(Debug)]
pub enum VariableStatement {
    Var(Box<VariableDeclarationList>),
}

impl fmt::Display for VariableStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let VariableStatement::Var(node) = self;
        write!(f, "var {} ;", node)
    }
}

impl PrettyPrint for VariableStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}VariableStatement: {}", first, self)?;
        let VariableStatement::Var(node) = self;
        node.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}VariableStatement: {}", first, self)?;
        pprint_token(writer, "var", TokenType::Keyword, &successive, Spot::NotFinal)?;
        let VariableStatement::Var(node) = self;
        node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl VariableStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_var = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Var)?;
        let (vdl, after_vdl) = VariableDeclarationList::parse(parser, after_var, true, yield_flag, await_flag)?;
        let after_semi = scan_for_punct(after_vdl, parser.source, ScanGoal::InputElementRegExp, Punctuator::Semicolon)?;
        Ok((Box::new(VariableStatement::Var(vdl)), after_semi))
    }
}

// VariableDeclarationList[In, Yield, Await] :
//      VariableDeclaration[?In, ?Yield, ?Await]
//      VariableDeclarationList[?In, ?Yield, ?Await] , VariableDeclaration[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum VariableDeclarationList {
    Item(Box<VariableDeclaration>),
    List(Box<VariableDeclarationList>, Box<VariableDeclaration>),
}

impl fmt::Display for VariableDeclarationList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VariableDeclarationList::Item(node) => node.fmt(f),
            VariableDeclarationList::List(lst, item) => write!(f, "{} , {}", lst, item),
        }
    }
}

impl PrettyPrint for VariableDeclarationList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}VariableDeclarationList: {}", first, self)?;
        match self {
            VariableDeclarationList::Item(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            VariableDeclarationList::List(lst, item) => {
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
            VariableDeclarationList::Item(node) => node.concise_with_leftpad(writer, pad, state),
            VariableDeclarationList::List(lst, item) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}VariableDeclarationList: {}", first, self)?;
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl VariableDeclarationList {
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (decl, after_dcl) = VariableDeclaration::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        let mut current = Box::new(VariableDeclarationList::Item(decl));
        let mut current_scanner = after_dcl;
        loop {
            match scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
                .and_then(|after_comma| VariableDeclaration::parse(parser, after_comma, in_flag, yield_flag, await_flag))
            {
                Err(_) => {
                    break;
                }
                Ok((next, after_next)) => {
                    current = Box::new(VariableDeclarationList::List(current, next));
                    current_scanner = after_next;
                }
            }
        }
        Ok((current, current_scanner))
    }
}

// VariableDeclaration[In, Yield, Await] :
//      BindingIdentifier[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]opt
//      BindingPattern[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum VariableDeclaration {
    Identifier(Box<BindingIdentifier>, Option<Box<Initializer>>),
    Pattern(Box<BindingPattern>, Box<Initializer>),
}

impl fmt::Display for VariableDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VariableDeclaration::Identifier(bi, Some(i)) => write!(f, "{} {}", bi, i),
            VariableDeclaration::Identifier(bi, None) => bi.fmt(f),
            VariableDeclaration::Pattern(bp, i) => write!(f, "{} {}", bp, i),
        }
    }
}

impl PrettyPrint for VariableDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}VariableDeclaration: {}", first, self)?;
        match self {
            VariableDeclaration::Identifier(bi, Some(i)) => {
                bi.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                i.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            VariableDeclaration::Identifier(bi, None) => bi.pprint_with_leftpad(writer, &successive, Spot::Final),
            VariableDeclaration::Pattern(bp, i) => {
                bp.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                i.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let head = |writer: &mut T| {
            let (first, successive) = prettypad(pad, state);
            writeln!(writer, "{}VariableDeclaration: {}", first, self).and(Ok(successive))
        };
        match self {
            VariableDeclaration::Identifier(bi, None) => bi.concise_with_leftpad(writer, pad, state),
            VariableDeclaration::Identifier(bi, Some(i)) => {
                let successive = head(writer)?;
                bi.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                i.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            VariableDeclaration::Pattern(bp, i) => {
                let successive = head(writer)?;
                bp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                i.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl VariableDeclaration {
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("VariableDeclaration expected", scanner.line, scanner.column))
            .otherwise(|| {
                let (bi, after_bi) = BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
                let pot_init = Initializer::parse(parser, after_bi, in_flag, yield_flag, await_flag);
                let (init, after_init) = match pot_init {
                    Err(_) => (None, after_bi),
                    Ok((i, after_i)) => (Some(i), after_i),
                };
                Ok((Box::new(VariableDeclaration::Identifier(bi, init)), after_init))
            })
            .otherwise(|| {
                let (bp, after_bp) = BindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
                let (init, after_init) = Initializer::parse(parser, after_bp, in_flag, yield_flag, await_flag)?;
                Ok((Box::new(VariableDeclaration::Pattern(bp, init)), after_init))
            })
    }
}

// BindingPattern[Yield, Await] :
//      ObjectBindingPattern[?Yield, ?Await]
//      ArrayBindingPattern[?Yield, ?Await]
#[derive(Debug)]
pub enum BindingPattern {
    Object(Box<ObjectBindingPattern>),
    Array(Box<ArrayBindingPattern>),
}

impl fmt::Display for BindingPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BindingPattern::Object(node) => node.fmt(f),
            BindingPattern::Array(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for BindingPattern {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BindingPattern: {}", first, self)?;
        match self {
            BindingPattern::Object(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            BindingPattern::Array(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            BindingPattern::Object(node) => node.concise_with_leftpad(writer, pad, state),
            BindingPattern::Array(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl BindingPattern {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("BindingPattern expected", scanner.line, scanner.column))
            .otherwise(|| ObjectBindingPattern::parse(parser, scanner, yield_flag, await_flag).and_then(|(obp, after_obp)| Ok((Box::new(BindingPattern::Object(obp)), after_obp))))
            .otherwise(|| ArrayBindingPattern::parse(parser, scanner, yield_flag, await_flag).and_then(|(abp, after_abp)| Ok((Box::new(BindingPattern::Array(abp)), after_abp))))
    }
}

// ObjectBindingPattern[Yield, Await] :
//      { }
//      { BindingRestProperty[?Yield, ?Await] }
//      { BindingPropertyList[?Yield, ?Await] }
//      { BindingPropertyList[?Yield, ?Await] , BindingRestProperty[?Yield, ?Await]opt }
#[derive(Debug)]
pub enum ObjectBindingPattern {
    Empty,
    RestOnly(Box<BindingRestProperty>),
    ListOnly(Box<BindingPropertyList>),
    ListRest(Box<BindingPropertyList>, Option<Box<BindingRestProperty>>),
}

impl fmt::Display for ObjectBindingPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ObjectBindingPattern::Empty => write!(f, "{{ }}"),
            ObjectBindingPattern::RestOnly(node) => write!(f, "{{ {} }}", node),
            ObjectBindingPattern::ListOnly(node) => write!(f, "{{ {} }}", node),
            ObjectBindingPattern::ListRest(lst, Some(rst)) => write!(f, "{{ {} , {} }}", lst, rst),
            ObjectBindingPattern::ListRest(lst, None) => write!(f, "{{ {} , }}", lst),
        }
    }
}

impl PrettyPrint for ObjectBindingPattern {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ObjectBindingPattern: {}", first, self)?;
        match self {
            ObjectBindingPattern::Empty => Ok(()),
            ObjectBindingPattern::RestOnly(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            ObjectBindingPattern::ListOnly(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            ObjectBindingPattern::ListRest(lst, Some(rst)) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                rst.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ObjectBindingPattern::ListRest(lst, None) => lst.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ObjectBindingPattern: {}", first, self)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match self {
            ObjectBindingPattern::Empty => {}
            ObjectBindingPattern::RestOnly(node) => {
                node.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ObjectBindingPattern::ListOnly(node) => {
                node.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ObjectBindingPattern::ListRest(lst, Some(rst)) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                rst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ObjectBindingPattern::ListRest(lst, None) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
            }
        }
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ObjectBindingPattern {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("ObjectBindingPattern expected", scanner.line, scanner.column)).otherwise(|| {
            scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBrace).and_then(|after_open| {
                scan_for_punct(after_open, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBrace)
                    .and_then(|after_close| Ok((Box::new(ObjectBindingPattern::Empty), after_close)))
                    .otherwise(|| {
                        BindingRestProperty::parse(parser, after_open, yield_flag, await_flag).and_then(|(brp, after_brp)| {
                            scan_for_punct(after_brp, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBrace)
                                .and_then(|after_close| Ok((Box::new(ObjectBindingPattern::RestOnly(brp)), after_close)))
                        })
                    })
                    .otherwise(|| {
                        BindingPropertyList::parse(parser, after_open, yield_flag, await_flag).and_then(|(bpl, after_bpl)| {
                            match scan_for_punct(after_bpl, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBrace).and_then(|after_close| Ok((None, after_close))).otherwise(
                                || {
                                    scan_for_punct(after_bpl, parser.source, ScanGoal::InputElementRegExp, Punctuator::Comma).and_then(|after_comma| {
                                        let (brp, after_brp) = match BindingRestProperty::parse(parser, after_comma, yield_flag, await_flag) {
                                            Err(_) => (None, after_comma),
                                            Ok((node, s)) => (Some(node), s),
                                        };
                                        scan_for_punct(after_brp, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBrace).and_then(|after_final| Ok((Some(brp), after_final)))
                                    })
                                },
                            ) {
                                Ok((None, after)) => Ok((Box::new(ObjectBindingPattern::ListOnly(bpl)), after)),
                                Ok((Some(brp), after)) => Ok((Box::new(ObjectBindingPattern::ListRest(bpl, brp)), after)),
                                Err(e) => Err(e),
                            }
                        })
                    })
            })
        })

        // let (tok_open, after_open) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        // if tok_open.matches_punct(Punctuator::LeftBrace) {
        //     let (tok_close, after_close) = scan_token(&after_open, parser.source, ScanGoal::InputElementRegExp);
        //     if tok_close.matches_punct(Punctuator::RightBrace) {
        //         return Ok(Some((Box::new(ObjectBindingPattern::Empty), after_close)));
        //     }
        //     let pot_brp = BindingRestProperty::parse(parser, after_open, yield_flag, await_flag)?;
        //     if let Some((brp, after_brp)) = pot_brp {
        //         let (tok_close, after_close) = scan_token(&after_brp, parser.source, ScanGoal::InputElementRegExp);
        //         if tok_close.matches_punct(Punctuator::RightBrace) {
        //             return Ok(Some((Box::new(ObjectBindingPattern::RestOnly(brp)), after_close)));
        //         }
        //     }
        //
        //     let pot_bpl = BindingPropertyList::parse(parser, after_open, yield_flag, await_flag)?;
        //     if let Some((bpl, after_bpl)) = pot_bpl {
        //         let (tok_after, after_tok) = scan_token(&after_bpl, parser.source, ScanGoal::InputElementRegExp);
        //         if tok_after.matches_punct(Punctuator::RightBrace) {
        //             return Ok(Some((Box::new(ObjectBindingPattern::ListOnly(bpl)), after_tok)));
        //         }
        //         if tok_after.matches_punct(Punctuator::Comma) {
        //             let pot_brp = BindingRestProperty::parse(parser, after_tok, yield_flag, await_flag)?;
        //             let (brp, after_brp) = match pot_brp {
        //                 None => (None, after_tok),
        //                 Some((node, s)) => (Some(node), s),
        //             };
        //             let (tok_final, after_final) = scan_token(&after_brp, parser.source, ScanGoal::InputElementRegExp);
        //             if tok_final.matches_punct(Punctuator::RightBrace) {
        //                 return Ok(Some((Box::new(ObjectBindingPattern::ListRest(bpl, brp)), after_final)));
        //             }
        //         }
        //     }
        // }
        // Ok(None)
    }
}

// ArrayBindingPattern[Yield, Await] :
//      [ Elisionopt BindingRestElement[?Yield, ?Await]opt ]
//      [ BindingElementList[?Yield, ?Await] ]
//      [ BindingElementList[?Yield, ?Await] , Elisionopt BindingRestElement[?Yield, ?Await]opt ]
#[derive(Debug)]
pub enum ArrayBindingPattern {
    RestOnly(Option<Box<Elisions>>, Option<Box<BindingRestElement>>),
    ListOnly(Box<BindingElementList>),
    ListRest(Box<BindingElementList>, Option<Box<Elisions>>, Option<Box<BindingRestElement>>),
}

impl fmt::Display for ArrayBindingPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArrayBindingPattern::RestOnly(Some(elisions), Some(node)) => write!(f, "[ {} {} ]", elisions, node),
            ArrayBindingPattern::RestOnly(Some(elisions), None) => write!(f, "[ {} ]", elisions),
            ArrayBindingPattern::RestOnly(None, Some(node)) => write!(f, "[ {} ]", node),
            ArrayBindingPattern::RestOnly(None, None) => write!(f, "[ ]"),
            ArrayBindingPattern::ListOnly(node) => write!(f, "[ {} ]", node),
            ArrayBindingPattern::ListRest(lst, Some(elisions), Some(rst)) => {
                write!(f, "[ {} , {} {} ]", lst, elisions, rst)
            }
            ArrayBindingPattern::ListRest(lst, None, Some(rst)) => write!(f, "[ {} , {} ]", lst, rst),
            ArrayBindingPattern::ListRest(lst, Some(elisions), None) => write!(f, "[ {} , {} ]", lst, elisions),
            ArrayBindingPattern::ListRest(lst, None, None) => write!(f, "[ {} , ]", lst),
        }
    }
}

impl PrettyPrint for ArrayBindingPattern {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ArrayBindingPattern: {}", first, self)?;
        match self {
            ArrayBindingPattern::RestOnly(Some(elisions), Some(node)) => {
                elisions.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                node.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayBindingPattern::RestOnly(Some(elisions), None) => elisions.pprint_with_leftpad(writer, &successive, Spot::Final),
            ArrayBindingPattern::RestOnly(None, Some(node)) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            ArrayBindingPattern::RestOnly(None, None) => Ok(()),
            ArrayBindingPattern::ListOnly(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            ArrayBindingPattern::ListRest(lst, Some(elisions), Some(rst)) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                elisions.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                rst.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayBindingPattern::ListRest(lst, None, Some(rst)) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                rst.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayBindingPattern::ListRest(lst, Some(elisions), None) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                elisions.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayBindingPattern::ListRest(lst, None, None) => lst.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ArrayBindingPattern: {}", first, self)?;
        pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match self {
            ArrayBindingPattern::RestOnly(Some(elisions), Some(node)) => {
                elisions.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ArrayBindingPattern::RestOnly(Some(elisions), None) => {
                elisions.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ArrayBindingPattern::RestOnly(None, Some(node)) => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ArrayBindingPattern::RestOnly(None, None) => {}
            ArrayBindingPattern::ListOnly(node) => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ArrayBindingPattern::ListRest(lst, Some(elisions), Some(rst)) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                elisions.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                rst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ArrayBindingPattern::ListRest(lst, None, Some(rst)) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                rst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ArrayBindingPattern::ListRest(lst, Some(elisions), None) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                elisions.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ArrayBindingPattern::ListRest(lst, None, None) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
            }
        }
        pprint_token(writer, "]", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ArrayBindingPattern {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_first = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBracket)?;
        BindingElementList::parse(parser, after_first, yield_flag, await_flag)
            .and_then(|(bel, after_bel)| {
                scan_for_punct_set(after_bel, parser.source, ScanGoal::InputElementRegExp, &[Punctuator::RightBracket, Punctuator::Comma]).and_then(|(punct_next, after_next)| {
                    match punct_next {
                        Punctuator::RightBracket => Ok((Box::new(ArrayBindingPattern::ListOnly(bel)), after_next)),
                        Punctuator::Comma | _ => {
                            let (elisions, after_elisions, err_elisions) = match Elisions::parse(parser, after_next) {
                                Err(err) => (None, after_next, Some(err)),
                                Ok((e, s)) => (Some(e), s, None),
                            };
                            let (bre, after_bre, err_bre) = match BindingRestElement::parse(parser, after_elisions, yield_flag, await_flag) {
                                Err(err) => (None, after_elisions, Some(err)),
                                Ok((b, s)) => (Some(b), s, None),
                            };
                            match scan_for_punct(after_bre, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBracket) {
                                Ok(after_close) => Ok((Box::new(ArrayBindingPattern::ListRest(bel, elisions, bre)), after_close)),
                                Err(pe) => {
                                    let mut err = Some(pe);
                                    if ParseError::compare_option(&err_elisions, &err) == Ordering::Greater {
                                        err = err_elisions;
                                    }
                                    if ParseError::compare_option(&err_bre, &err) == Ordering::Greater {
                                        err = err_bre;
                                    }
                                    Err(err.unwrap())
                                }
                            }
                        }
                    }
                })
            })
            .otherwise(|| {
                let (elisions, after_elisions, err_elisions) = match Elisions::parse(parser, after_first) {
                    Err(err) => (None, after_first, Some(err)),
                    Ok((e, s)) => (Some(e), s, None),
                };
                let (bre, after_bre, err_bre) = match BindingRestElement::parse(parser, after_elisions, yield_flag, await_flag) {
                    Err(err) => (None, after_elisions, Some(err)),
                    Ok((b, s)) => (Some(b), s, None),
                };
                match scan_for_punct(after_bre, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBracket) {
                    Ok(after_close) => Ok((Box::new(ArrayBindingPattern::RestOnly(elisions, bre)), after_close)),
                    Err(pe) => {
                        let mut err = Some(pe);
                        if ParseError::compare_option(&err_elisions, &err) == Ordering::Greater {
                            err = err_elisions;
                        }
                        if ParseError::compare_option(&err_bre, &err) == Ordering::Greater {
                            err = err_bre;
                        }
                        Err(err.unwrap())
                    }
                }
            })

        //    if let Some((bel, after_bel)) = pot_bel {
        //        let (tok_next, after_next) = scan_token(&after_bel, parser.source, ScanGoal::InputElementRegExp);
        //        if tok_next.matches_punct(Punctuator::RightBracket) {
        //            return Ok(Some((Box::new(ArrayBindingPattern::ListOnly(bel)), after_next)));
        //        }
        //        if tok_next.matches_punct(Punctuator::Comma) {
        //            let pot_elisions = Elisions::parse(parser, after_next)?;
        //            let (elisions, after_elisions) = match pot_elisions {
        //                None => (None, after_next),
        //                Some((e, s)) => (Some(e), s),
        //            };
        //            let pot_bre = BindingRestElement::parse(parser, after_elisions, yield_flag, await_flag)?;
        //            let (bre, after_bre) = match pot_bre {
        //                None => (None, after_elisions),
        //                Some((b, s)) => (Some(b), s),
        //            };
        //            let (tok_close, after_close) = scan_token(&after_bre, parser.source, ScanGoal::InputElementRegExp);
        //            if tok_close.matches_punct(Punctuator::RightBracket) {
        //                return Ok(Some((Box::new(ArrayBindingPattern::ListRest(bel, elisions, bre)), after_close)));
        //            }
        //        }
        //    }

        //    let pot_elisions = Elisions::parse(parser, after_first)?;
        //    let (elisions, after_elisions) = match pot_elisions {
        //        None => (None, after_first),
        //        Some((e, s)) => (Some(e), s),
        //    };
        //    let pot_bre = BindingRestElement::parse(parser, after_elisions, yield_flag, await_flag)?;
        //    let (bre, after_bre) = match pot_bre {
        //        None => (None, after_elisions),
        //        Some((b, s)) => (Some(b), s),
        //    };
        //    let (tok_close, after_close) = scan_token(&after_bre, parser.source, ScanGoal::InputElementRegExp);
        //    if tok_close.matches_punct(Punctuator::RightBracket) {
        //        return Ok(Some((Box::new(ArrayBindingPattern::RestOnly(elisions, bre)), after_close)));
        //    }
        //}
        //Ok(None)
    }
}

// BindingRestProperty[Yield, Await] :
//      ... BindingIdentifier[?Yield, ?Await]
#[derive(Debug)]
pub enum BindingRestProperty {
    Id(Box<BindingIdentifier>),
}

impl fmt::Display for BindingRestProperty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let BindingRestProperty::Id(node) = self;
        write!(f, "... {}", node)
    }
}

impl PrettyPrint for BindingRestProperty {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BindingRestProperty: {}", first, self)?;
        let BindingRestProperty::Id(node) = self;
        node.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BindingRestProperty: {}", first, self)?;
        pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        let BindingRestProperty::Id(node) = self;
        node.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl BindingRestProperty {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Ellipsis)
            .and_then(|after_dots| BindingIdentifier::parse(parser, after_dots, yield_flag, await_flag))
            .and_then(|(id, after_id)| Ok((Box::new(BindingRestProperty::Id(id)), after_id)))

        // let (tok_dots, after_dots) = scan_token(&scanner, parser.source, ScanGoal::InputElementRegExp);
        // if tok_dots.matches_punct(Punctuator::Ellipsis) {
        //     let pot_id = BindingIdentifier::parse(parser, after_dots, yield_flag, await_flag)?;
        //     if let Some((id, after_id)) = pot_id {
        //         return Ok(Some((Box::new(BindingRestProperty::Id(id)), after_id)));
        //     }
        // }
        // Ok(None)
    }
}

// BindingPropertyList[Yield, Await] :
//      BindingProperty[?Yield, ?Await]
//      BindingPropertyList[?Yield, ?Await] , BindingProperty[?Yield, ?Await]
#[derive(Debug)]
pub enum BindingPropertyList {
    Item(Box<BindingProperty>),
    List(Box<BindingPropertyList>, Box<BindingProperty>),
}

impl fmt::Display for BindingPropertyList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BindingPropertyList::Item(node) => node.fmt(f),
            BindingPropertyList::List(lst, item) => write!(f, "{} , {}", lst, item),
        }
    }
}

impl PrettyPrint for BindingPropertyList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BindingPropertyList: {}", first, self)?;
        match self {
            BindingPropertyList::Item(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            BindingPropertyList::List(lst, item) => {
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
            BindingPropertyList::Item(node) => node.concise_with_leftpad(writer, pad, state),
            BindingPropertyList::List(lst, item) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}BindingPropertyList: {}", first, self)?;
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl BindingPropertyList {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (bp, after_bp) = BindingProperty::parse(parser, scanner, yield_flag, await_flag)?;
        let mut current = Box::new(BindingPropertyList::Item(bp));
        let mut current_scan = after_bp;
        loop {
            match scan_for_punct(current_scan, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
                .and_then(|after_token| BindingProperty::parse(parser, after_token, yield_flag, await_flag))
            {
                Err(_) => {
                    break;
                }
                Ok((bp2, after_bp2)) => {
                    current = Box::new(BindingPropertyList::List(current, bp2));
                    current_scan = after_bp2;
                }
            }
        }
        Ok((current, current_scan))
    }
}

// BindingElementList[Yield, Await] :
//      BindingElisionElement[?Yield, ?Await]
//      BindingElementList[?Yield, ?Await] , BindingElisionElement[?Yield, ?Await]
#[derive(Debug)]
pub enum BindingElementList {
    Item(Box<BindingElisionElement>),
    List(Box<BindingElementList>, Box<BindingElisionElement>),
}

impl fmt::Display for BindingElementList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BindingElementList::Item(node) => node.fmt(f),
            BindingElementList::List(lst, item) => write!(f, "{} , {}", lst, item),
        }
    }
}

impl PrettyPrint for BindingElementList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BindingElementList: {}", first, self)?;
        match self {
            BindingElementList::Item(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            BindingElementList::List(lst, item) => {
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
            BindingElementList::Item(node) => node.concise_with_leftpad(writer, pad, state),
            BindingElementList::List(lst, item) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}BindingElementList: {}", first, self)?;
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl BindingElementList {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (elem, after_elem) = BindingElisionElement::parse(parser, scanner, yield_flag, await_flag)?;
        let mut current = Box::new(BindingElementList::Item(elem));
        let mut current_scanner = after_elem;
        loop {
            match scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
                .and_then(|after_tok| BindingElisionElement::parse(parser, after_tok, yield_flag, await_flag))
            {
                Err(_) => {
                    break;
                }
                Ok((next, after_next)) => {
                    current = Box::new(BindingElementList::List(current, next));
                    current_scanner = after_next;
                }
            }
        }
        Ok((current, current_scanner))
    }
}

// BindingElisionElement[Yield, Await] :
//      Elisionopt BindingElement[?Yield, ?Await]
#[derive(Debug)]
pub enum BindingElisionElement {
    Element(Option<Box<Elisions>>, Box<BindingElement>),
}

impl fmt::Display for BindingElisionElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BindingElisionElement::Element(None, elem) => elem.fmt(f),
            BindingElisionElement::Element(Some(elision), elem) => write!(f, "{} {}", elision, elem),
        }
    }
}

impl PrettyPrint for BindingElisionElement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BindingElisionElement: {}", first, self)?;
        match self {
            BindingElisionElement::Element(None, elem) => elem.pprint_with_leftpad(writer, &successive, Spot::Final),
            BindingElisionElement::Element(Some(elision), elem) => {
                elision.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                elem.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            BindingElisionElement::Element(None, node) => node.concise_with_leftpad(writer, pad, state),
            BindingElisionElement::Element(Some(elision), node) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}BindingElisionElement: {}", first, self)?;
                elision.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl BindingElisionElement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (elision, after_elision, err_elision) = match Elisions::parse(parser, scanner) {
            Err(err) => (None, scanner, Some(err)),
            Ok((e, s)) => (Some(e), s, None),
        };
        match BindingElement::parse(parser, after_elision, yield_flag, await_flag) {
            Ok((be, after_be)) => Ok((Box::new(BindingElisionElement::Element(elision, be)), after_be)),
            Err(err_be) => {
                let mut err = Some(err_be);
                if ParseError::compare_option(&err_elision, &err) == Ordering::Greater {
                    err = err_elision;
                }
                Err(err.unwrap())
            }
        }
    }
}

// BindingProperty[Yield, Await] :
//      SingleNameBinding[?Yield, ?Await]
//      PropertyName[?Yield, ?Await] : BindingElement[?Yield, ?Await]
#[derive(Debug)]
pub enum BindingProperty {
    Single(Box<SingleNameBinding>),
    Property(Box<PropertyName>, Box<BindingElement>),
}

impl fmt::Display for BindingProperty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BindingProperty::Single(node) => node.fmt(f),
            BindingProperty::Property(name, elem) => write!(f, "{} : {}", name, elem),
        }
    }
}

impl PrettyPrint for BindingProperty {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BindingProperty: {}", first, self)?;
        match self {
            BindingProperty::Single(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            BindingProperty::Property(name, elem) => {
                name.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                elem.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            BindingProperty::Single(node) => node.concise_with_leftpad(writer, pad, state),
            BindingProperty::Property(name, elem) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}BindingProperty: {}", first, self)?;
                name.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                elem.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl BindingProperty {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("BindingProperty expected", scanner.line, scanner.column))
            .otherwise(|| {
                let (pn, after_pn) = PropertyName::parse(parser, scanner, yield_flag, await_flag)?;
                let after_token = scan_for_punct(after_pn, parser.source, ScanGoal::InputElementDiv, Punctuator::Colon)?;
                let (be, after_be) = BindingElement::parse(parser, after_token, yield_flag, await_flag)?;
                Ok((Box::new(BindingProperty::Property(pn, be)), after_be))
            })
            .otherwise(|| {
                let (snb, after_snb) = SingleNameBinding::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Box::new(BindingProperty::Single(snb)), after_snb))
            })
    }
}

// BindingElement[Yield, Await] :
//      SingleNameBinding[?Yield, ?Await]
//      BindingPattern[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]opt
#[derive(Debug)]
pub enum BindingElement {
    Single(Box<SingleNameBinding>),
    Pattern(Box<BindingPattern>, Option<Box<Initializer>>),
}

impl fmt::Display for BindingElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BindingElement::Single(node) => node.fmt(f),
            BindingElement::Pattern(node, None) => node.fmt(f),
            BindingElement::Pattern(node, Some(init)) => write!(f, "{} {}", node, init),
        }
    }
}

impl PrettyPrint for BindingElement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BindingElement: {}", first, self)?;
        match self {
            BindingElement::Single(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            BindingElement::Pattern(node, None) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            BindingElement::Pattern(node, Some(init)) => {
                node.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                init.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            BindingElement::Single(node) => node.concise_with_leftpad(writer, pad, state),
            BindingElement::Pattern(node, None) => node.concise_with_leftpad(writer, pad, state),
            BindingElement::Pattern(node, Some(init)) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}BindingElement: {}", first, self)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                init.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl BindingElement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        Err(ParseError::new("BindingElement expected", scanner.line, scanner.column))
            .otherwise(|| {
                let (bp, after_bp) = BindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
                let (init, after_init) = match Initializer::parse(parser, after_bp, true, yield_flag, await_flag) {
                    Err(_) => (None, after_bp),
                    Ok((i, s)) => (Some(i), s),
                };
                Ok((Box::new(BindingElement::Pattern(bp, init)), after_init))
            })
            .otherwise(|| {
                let (snb, after_snb) = SingleNameBinding::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Box::new(BindingElement::Single(snb)), after_snb))
            })
    }
}

// SingleNameBinding[Yield, Await] :
//      BindingIdentifier[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]opt
#[derive(Debug)]
pub enum SingleNameBinding {
    Id(Box<BindingIdentifier>, Option<Box<Initializer>>),
}

impl fmt::Display for SingleNameBinding {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SingleNameBinding::Id(id, Some(init)) => write!(f, "{} {}", id, init),
            SingleNameBinding::Id(id, None) => id.fmt(f),
        }
    }
}

impl PrettyPrint for SingleNameBinding {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}SingleNameBinding: {}", first, self)?;
        match self {
            SingleNameBinding::Id(id, Some(init)) => {
                id.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                init.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            SingleNameBinding::Id(id, None) => id.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            SingleNameBinding::Id(id, None) => id.concise_with_leftpad(writer, pad, state),
            SingleNameBinding::Id(id, Some(init)) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}SingleNameBinding: {}", first, self)?;
                id.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                init.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl SingleNameBinding {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let (bi, after_bi) = BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
        let (init, after_init) = match Initializer::parse(parser, after_bi, true, yield_flag, await_flag) {
            Err(_) => (None, after_bi),
            Ok((i, s)) => (Some(i), s),
        };
        Ok((Box::new(SingleNameBinding::Id(bi, init)), after_init))
    }
}

// BindingRestElement[Yield, Await] :
//      ... BindingIdentifier[?Yield, ?Await]
//      ... BindingPattern[?Yield, ?Await]
#[derive(Debug)]
pub enum BindingRestElement {
    Identifier(Box<BindingIdentifier>),
    Pattern(Box<BindingPattern>),
}

impl fmt::Display for BindingRestElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BindingRestElement::Identifier(node) => write!(f, "... {}", node),
            BindingRestElement::Pattern(node) => write!(f, "... {}", node),
        }
    }
}

impl PrettyPrint for BindingRestElement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BindingRestElement: {}", first, self)?;
        match self {
            BindingRestElement::Identifier(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            BindingRestElement::Pattern(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BindingRestElement: {}", first, self)?;
        pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match self {
            BindingRestElement::Identifier(node) => node.concise_with_leftpad(writer, &successive, Spot::Final),
            BindingRestElement::Pattern(node) => node.concise_with_leftpad(writer, &successive, Spot::Final),
        }
    }
}

impl BindingRestElement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> Result<(Box<Self>, Scanner), ParseError> {
        let after_tok = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Ellipsis)?;
        BindingPattern::parse(parser, after_tok, yield_flag, await_flag)
            .and_then(|(bp, after_bp)| Ok((Box::new(BindingRestElement::Pattern(bp)), after_bp)))
            .otherwise(|| BindingIdentifier::parse(parser, after_tok, yield_flag, await_flag).and_then(|(bi, after_bi)| Ok((Box::new(BindingRestElement::Identifier(bi)), after_bi))))
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}
