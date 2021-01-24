use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::identifiers::BindingIdentifier;
use super::primary_expressions::{Elisions, Initializer, PropertyName};
use super::scanner::Scanner;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

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
}

impl LexicalDeclaration {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok, after_tok) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        let loc = match tok {
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Let) => LetOrConst::Let,
            scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Const) => LetOrConst::Const,
            _ => {
                return Ok(None);
            }
        };

        let pot_bl = BindingList::parse(parser, after_tok, in_flag, yield_flag, await_flag)?;
        let (bl, after_bl) = match pot_bl {
            None => {
                return Ok(None);
            }
            Some(x) => x,
        };

        let (semi, after_semi) = scanner::scan_token(&after_bl, parser.source, scanner::ScanGoal::InputElementRegExp);
        match semi {
            scanner::Token::Semicolon => Ok(Some((Box::new(LexicalDeclaration::List(loc, bl)), after_semi))),
            _ => Ok(None),
        }
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
}

impl BindingList {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_lb = LexicalBinding::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        match pot_lb {
            None => Ok(None),
            Some((lb, after_lb)) => {
                let mut current = Box::new(BindingList::Item(lb));
                let mut current_scanner = after_lb;
                loop {
                    let (tok, after_tok) =
                        scanner::scan_token(&current_scanner, parser.source, scanner::ScanGoal::InputElementDiv);
                    match tok {
                        scanner::Token::Comma => {
                            let pot_lb2 = LexicalBinding::parse(parser, after_tok, in_flag, yield_flag, await_flag)?;
                            match pot_lb2 {
                                None => {
                                    break;
                                }
                                Some((lb2, after_lb2)) => {
                                    current = Box::new(BindingList::List(current, lb2));
                                    current_scanner = after_lb2;
                                }
                            }
                        }
                        _ => {
                            break;
                        }
                    }
                }
                Ok(Some((current, current_scanner)))
            }
        }
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
}

impl LexicalBinding {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_bi = BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((bi, after_bi)) = pot_bi {
            let pot_init = Initializer::parse(parser, after_bi, in_flag, yield_flag, await_flag)?;
            let (init, after_init) = match pot_init {
                None => (None, after_bi),
                Some((i, after_i)) => (Some(i), after_i),
            };
            return Ok(Some((Box::new(LexicalBinding::Identifier(bi, init)), after_init)));
        }

        let pot_bp = BindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((bp, after_bp)) = pot_bp {
            let pot_init = Initializer::parse(parser, after_bp, in_flag, yield_flag, await_flag)?;
            if let Some((init, after_init)) = pot_init {
                return Ok(Some((Box::new(LexicalBinding::Pattern(bp, init)), after_init)));
            }
        }

        Ok(None)
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
}

impl VariableStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (var_tok, after_var) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if matches!(var_tok, scanner::Token::Identifier(id) if id.keyword_id == Some(scanner::Keyword::Var)) {
            let pot_vdl = VariableDeclarationList::parse(parser, after_var, true, yield_flag, await_flag)?;
            if let Some((vdl, after_vdl)) = pot_vdl {
                let (semi_tok, after_semi) =
                    scanner::scan_token(&after_vdl, parser.source, scanner::ScanGoal::InputElementRegExp);
                if semi_tok == scanner::Token::Semicolon {
                    return Ok(Some((Box::new(VariableStatement::Var(vdl)), after_semi)));
                }
            }
        }

        Ok(None)
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
}

impl VariableDeclarationList {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_decl = VariableDeclaration::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        match pot_decl {
            None => Ok(None),
            Some((decl, after_dcl)) => {
                let mut current = Box::new(VariableDeclarationList::Item(decl));
                let mut current_scanner = after_dcl;
                loop {
                    let (tok_comma, after_comma) =
                        scanner::scan_token(&current_scanner, parser.source, scanner::ScanGoal::InputElementDiv);
                    match tok_comma {
                        scanner::Token::Comma => {
                            let pot_next =
                                VariableDeclaration::parse(parser, after_comma, in_flag, yield_flag, await_flag)?;
                            match pot_next {
                                None => {
                                    break;
                                }
                                Some((next, after_next)) => {
                                    current = Box::new(VariableDeclarationList::List(current, next));
                                    current_scanner = after_next;
                                }
                            }
                        }
                        _ => {
                            break;
                        }
                    }
                }
                Ok(Some((current, current_scanner)))
            }
        }
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
}

impl VariableDeclaration {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_bi = BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((bi, after_bi)) = pot_bi {
            let pot_init = Initializer::parse(parser, after_bi, in_flag, yield_flag, await_flag)?;
            let (init, after_init) = match pot_init {
                None => (None, after_bi),
                Some((i, after_i)) => (Some(i), after_i),
            };
            return Ok(Some((Box::new(VariableDeclaration::Identifier(bi, init)), after_init)));
        }

        let pot_bp = BindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((bp, after_bp)) = pot_bp {
            let pot_init = Initializer::parse(parser, after_bp, in_flag, yield_flag, await_flag)?;
            if let Some((init, after_init)) = pot_init {
                return Ok(Some((Box::new(VariableDeclaration::Pattern(bp, init)), after_init)));
            }
        }

        Ok(None)
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
}

impl BindingPattern {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_obp = ObjectBindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((obp, after_obp)) = pot_obp {
            return Ok(Some((Box::new(BindingPattern::Object(obp)), after_obp)));
        }

        let pot_abp = ArrayBindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((abp, after_abp)) = pot_abp {
            return Ok(Some((Box::new(BindingPattern::Array(abp)), after_abp)));
        }
        Ok(None)
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
}

impl ObjectBindingPattern {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok_open, after_open) =
            scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if tok_open == scanner::Token::LeftBrace {
            let (tok_close, after_close) =
                scanner::scan_token(&after_open, parser.source, scanner::ScanGoal::InputElementRegExp);
            if tok_close == scanner::Token::RightBrace {
                return Ok(Some((Box::new(ObjectBindingPattern::Empty), after_close)));
            }
            let pot_brp = BindingRestProperty::parse(parser, after_open, yield_flag, await_flag)?;
            if let Some((brp, after_brp)) = pot_brp {
                let (tok_close, after_close) =
                    scanner::scan_token(&after_brp, parser.source, scanner::ScanGoal::InputElementRegExp);
                if tok_close == scanner::Token::RightBrace {
                    return Ok(Some((Box::new(ObjectBindingPattern::RestOnly(brp)), after_close)));
                }
            }

            let pot_bpl = BindingPropertyList::parse(parser, after_open, yield_flag, await_flag)?;
            if let Some((bpl, after_bpl)) = pot_bpl {
                let (tok_after, after_tok) =
                    scanner::scan_token(&after_bpl, parser.source, scanner::ScanGoal::InputElementRegExp);
                if tok_after == scanner::Token::RightBrace {
                    return Ok(Some((Box::new(ObjectBindingPattern::ListOnly(bpl)), after_tok)));
                }
                if tok_after == scanner::Token::Comma {
                    let pot_brp = BindingRestProperty::parse(parser, after_tok, yield_flag, await_flag)?;
                    let (brp, after_brp) = match pot_brp {
                        None => (None, after_tok),
                        Some((node, s)) => (Some(node), s),
                    };
                    let (tok_final, after_final) =
                        scanner::scan_token(&after_brp, parser.source, scanner::ScanGoal::InputElementRegExp);
                    if tok_final == scanner::Token::RightBrace {
                        return Ok(Some((Box::new(ObjectBindingPattern::ListRest(bpl, brp)), after_final)));
                    }
                }
            }
        }
        Ok(None)
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
    ListRest(
        Box<BindingElementList>,
        Option<Box<Elisions>>,
        Option<Box<BindingRestElement>>,
    ),
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
            ArrayBindingPattern::RestOnly(Some(elisions), None) => {
                elisions.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayBindingPattern::RestOnly(None, Some(node)) => {
                node.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
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
}

impl ArrayBindingPattern {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok_first, after_first) =
            scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if tok_first == scanner::Token::LeftBracket {
            let pot_bel = BindingElementList::parse(parser, after_first, yield_flag, await_flag)?;
            if let Some((bel, after_bel)) = pot_bel {
                let (tok_next, after_next) =
                    scanner::scan_token(&after_bel, parser.source, scanner::ScanGoal::InputElementRegExp);
                if tok_next == scanner::Token::RightBracket {
                    return Ok(Some((Box::new(ArrayBindingPattern::ListOnly(bel)), after_next)));
                }
                if tok_next == scanner::Token::Comma {
                    let pot_elisions = Elisions::parse(parser, after_next)?;
                    let (elisions, after_elisions) = match pot_elisions {
                        None => (None, after_next),
                        Some((e, s)) => (Some(e), s),
                    };
                    let pot_bre = BindingRestElement::parse(parser, after_elisions, yield_flag, await_flag)?;
                    let (bre, after_bre) = match pot_bre {
                        None => (None, after_elisions),
                        Some((b, s)) => (Some(b), s),
                    };
                    let (tok_close, after_close) =
                        scanner::scan_token(&after_bre, parser.source, scanner::ScanGoal::InputElementRegExp);
                    if tok_close == scanner::Token::RightBracket {
                        return Ok(Some((
                            Box::new(ArrayBindingPattern::ListRest(bel, elisions, bre)),
                            after_close,
                        )));
                    }
                }
            }

            let pot_elisions = Elisions::parse(parser, after_first)?;
            let (elisions, after_elisions) = match pot_elisions {
                None => (None, after_first),
                Some((e, s)) => (Some(e), s),
            };
            let pot_bre = BindingRestElement::parse(parser, after_elisions, yield_flag, await_flag)?;
            let (bre, after_bre) = match pot_bre {
                None => (None, after_elisions),
                Some((b, s)) => (Some(b), s),
            };
            let (tok_close, after_close) =
                scanner::scan_token(&after_bre, parser.source, scanner::ScanGoal::InputElementRegExp);
            if tok_close == scanner::Token::RightBracket {
                return Ok(Some((
                    Box::new(ArrayBindingPattern::RestOnly(elisions, bre)),
                    after_close,
                )));
            }
        }
        Ok(None)
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
}

impl BindingRestProperty {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok_dots, after_dots) =
            scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if tok_dots == scanner::Token::Ellipsis {
            let pot_id = BindingIdentifier::parse(parser, after_dots, yield_flag, await_flag)?;
            if let Some((id, after_id)) = pot_id {
                return Ok(Some((Box::new(BindingRestProperty::Id(id)), after_id)));
            }
        }
        Ok(None)
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
}

impl BindingPropertyList {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_bp = BindingProperty::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_bp {
            None => Ok(None),
            Some((bp, after_bp)) => {
                let mut current = Box::new(BindingPropertyList::Item(bp));
                let mut current_scan = after_bp;
                loop {
                    let (token, after_token) =
                        scanner::scan_token(&current_scan, parser.source, scanner::ScanGoal::InputElementDiv);
                    match token {
                        scanner::Token::Comma => {
                            let pot_bp2 = BindingProperty::parse(parser, after_token, yield_flag, await_flag)?;
                            match pot_bp2 {
                                None => {
                                    break;
                                }
                                Some((bp2, after_bp2)) => {
                                    current = Box::new(BindingPropertyList::List(current, bp2));
                                    current_scan = after_bp2;
                                }
                            }
                        }
                        _ => {
                            break;
                        }
                    }
                }
                Ok(Some((current, current_scan)))
            }
        }
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
}

impl BindingElementList {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_elem = BindingElisionElement::parse(parser, scanner, yield_flag, await_flag)?;
        match pot_elem {
            None => Ok(None),
            Some((elem, after_elem)) => {
                let mut current = Box::new(BindingElementList::Item(elem));
                let mut current_scanner = after_elem;
                loop {
                    let (tok, after_tok) =
                        scanner::scan_token(&current_scanner, parser.source, scanner::ScanGoal::InputElementDiv);
                    match tok {
                        scanner::Token::Comma => {
                            let pot_next = BindingElisionElement::parse(parser, after_tok, yield_flag, await_flag)?;
                            match pot_next {
                                None => {
                                    break;
                                }
                                Some((next, after_next)) => {
                                    current = Box::new(BindingElementList::List(current, next));
                                    current_scanner = after_next;
                                }
                            }
                        }
                        _ => {
                            break;
                        }
                    }
                }
                Ok(Some((current, current_scanner)))
            }
        }
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
}

impl BindingElisionElement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_elision = Elisions::parse(parser, scanner)?;
        let (elision, after_elision) = match pot_elision {
            None => (None, scanner),
            Some((e, s)) => (Some(e), s),
        };
        let pot_be = BindingElement::parse(parser, after_elision, yield_flag, await_flag)?;
        if let Some((be, after_be)) = pot_be {
            return Ok(Some((Box::new(BindingElisionElement::Element(elision, be)), after_be)));
        }
        Ok(None)
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
}

impl BindingProperty {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_pn = PropertyName::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((pn, after_pn)) = pot_pn {
            let (token, after_token) =
                scanner::scan_token(&after_pn, parser.source, scanner::ScanGoal::InputElementDiv);
            if token == scanner::Token::Colon {
                let pot_be = BindingElement::parse(parser, after_token, yield_flag, await_flag)?;
                if let Some((be, after_be)) = pot_be {
                    return Ok(Some((Box::new(BindingProperty::Property(pn, be)), after_be)));
                }
            }
        }

        let pot_snb = SingleNameBinding::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((snb, after_snb)) = pot_snb {
            return Ok(Some((Box::new(BindingProperty::Single(snb)), after_snb)));
        }
        Ok(None)
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
}

impl BindingElement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_bp = BindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((bp, after_bp)) = pot_bp {
            let pot_init = Initializer::parse(parser, after_bp, true, yield_flag, await_flag)?;
            let (init, after_init) = match pot_init {
                None => (None, after_bp),
                Some((i, s)) => (Some(i), s),
            };
            return Ok(Some((Box::new(BindingElement::Pattern(bp, init)), after_init)));
        }

        let pot_snb = SingleNameBinding::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((snb, after_snb)) = pot_snb {
            return Ok(Some((Box::new(BindingElement::Single(snb)), after_snb)));
        }

        Ok(None)
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
}

impl SingleNameBinding {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_bi = BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((bi, after_bi)) = pot_bi {
            let pot_init = Initializer::parse(parser, after_bi, true, yield_flag, await_flag)?;
            let (init, after_init) = match pot_init {
                None => (None, after_bi),
                Some((i, s)) => (Some(i), s),
            };
            return Ok(Some((Box::new(SingleNameBinding::Id(bi, init)), after_init)));
        }
        Ok(None)
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
}
impl BindingRestElement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let (tok, after_tok) = scanner::scan_token(&scanner, parser.source, scanner::ScanGoal::InputElementRegExp);
        if tok == scanner::Token::Ellipsis {
            let pot_bp = BindingPattern::parse(parser, after_tok, yield_flag, await_flag)?;
            if let Some((bp, after_bp)) = pot_bp {
                return Ok(Some((Box::new(BindingRestElement::Pattern(bp)), after_bp)));
            }

            let pot_bi = BindingIdentifier::parse(parser, after_tok, yield_flag, await_flag)?;
            if let Some((bi, after_bi)) = pot_bi {
                return Ok(Some((Box::new(BindingRestElement::Identifier(bi)), after_bi)));
            }
        }
        Ok(None)
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}
