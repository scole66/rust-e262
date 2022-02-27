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
    List(LetOrConst, Rc<BindingList>),
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
    fn parse_core(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (kwd, after_tok) = scan_for_keywords(scanner, parser.source, ScanGoal::InputElementRegExp, &[Keyword::Let, Keyword::Const])?;
        let loc = match kwd {
            Keyword::Let => LetOrConst::Let,
            _ => LetOrConst::Const,
        };
        let (bl, after_bl) = BindingList::parse(parser, after_tok, in_flag, yield_flag, await_flag)?;
        let after_semi = scan_for_auto_semi(after_bl, parser.source, ScanGoal::InputElementRegExp)?;
        Ok((Rc::new(LexicalDeclaration::List(loc, bl)), after_semi))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = InYieldAwaitKey { scanner, in_flag, yield_flag, await_flag };
        match parser.lexical_declaration_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag, yield_flag, await_flag);
                parser.lexical_declaration_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            LexicalDeclaration::List(_, l) => l.bound_names(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let LexicalDeclaration::List(loc, bl) = self;
        loc.contains(kind) || bl.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        let LexicalDeclaration::List(_, node) = self;
        node.all_private_identifiers_valid(names)
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// LetOrConst :
//      let
//      const
#[derive(Debug)]
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

impl LetOrConst {
    pub fn contains(&self, _kind: ParseNodeKind) -> bool {
        false
    }
}

// BindingList[In, Yield, Await] :
//      LexicalBinding[?In, ?Yield, ?Await]
//      BindingList[?In, ?Yield, ?Await] , LexicalBinding[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum BindingList {
    Item(Rc<LexicalBinding>),
    List(Rc<BindingList>, Rc<LexicalBinding>),
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
    // no cache
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (lb, after_lb) = LexicalBinding::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        let mut current = Rc::new(BindingList::Item(lb));
        let mut current_scanner = after_lb;
        while let Ok((lb2, after_lb2)) = scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
            .and_then(|after_tok| LexicalBinding::parse(parser, after_tok, in_flag, yield_flag, await_flag))
        {
            current = Rc::new(BindingList::List(current, lb2));
            current_scanner = after_lb2;
        }
        Ok((current, current_scanner))
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            BindingList::Item(node) => node.bound_names(),
            BindingList::List(lst, node) => {
                let mut names = lst.bound_names();
                names.extend(node.bound_names());
                names
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            BindingList::Item(node) => node.contains(kind),
            BindingList::List(lst, item) => lst.contains(kind) || item.contains(kind),
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
            BindingList::Item(node) => node.all_private_identifiers_valid(names),
            BindingList::List(node1, node2) => node1.all_private_identifiers_valid(names) && node2.all_private_identifiers_valid(names),
        }
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// LexicalBinding[In, Yield, Await] :
//      BindingIdentifier[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]opt
//      BindingPattern[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum LexicalBinding {
    Identifier(Rc<BindingIdentifier>, Option<Rc<Initializer>>),
    Pattern(Rc<BindingPattern>, Rc<Initializer>),
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
    // no cache
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::LexicalBinding), scanner))
            .otherwise(|| {
                let (bi, after_bi) = BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
                let (init, after_init) = match Initializer::parse(parser, after_bi, in_flag, yield_flag, await_flag) {
                    Err(_) => (None, after_bi),
                    Ok((i, after_i)) => (Some(i), after_i),
                };
                Ok((Rc::new(LexicalBinding::Identifier(bi, init)), after_init))
            })
            .otherwise(|| {
                let (bp, after_bp) = BindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
                let (init, after_init) = Initializer::parse(parser, after_bp, in_flag, yield_flag, await_flag)?;
                Ok((Rc::new(LexicalBinding::Pattern(bp, init)), after_init))
            })
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            LexicalBinding::Identifier(bi, _) => bi.bound_names(),
            LexicalBinding::Pattern(bp, _) => bp.bound_names(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            LexicalBinding::Identifier(bi, opt) => bi.contains(kind) || opt.as_ref().map_or(false, |n| n.contains(kind)),
            LexicalBinding::Pattern(bp, i) => bp.contains(kind) || i.contains(kind),
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
            LexicalBinding::Identifier(_, Some(node)) => node.all_private_identifiers_valid(names),
            LexicalBinding::Identifier(_, None) => true,
            LexicalBinding::Pattern(node1, node2) => node1.all_private_identifiers_valid(names) && node2.all_private_identifiers_valid(names),
        }
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// VariableStatement[Yield, Await] :
//      var VariableDeclarationList[+In, ?Yield, ?Await] ;
#[derive(Debug)]
pub enum VariableStatement {
    Var(Rc<VariableDeclarationList>),
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
    // no cache
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_var = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Var)?;
        let (vdl, after_vdl) = VariableDeclarationList::parse(parser, after_var, true, yield_flag, await_flag)?;
        let after_semi = scan_for_auto_semi(after_vdl, parser.source, ScanGoal::InputElementRegExp)?;
        Ok((Rc::new(VariableStatement::Var(vdl)), after_semi))
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        let VariableStatement::Var(node) = self;
        node.bound_names()
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let VariableStatement::Var(node) = self;
        node.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        let VariableStatement::Var(node) = self;
        node.all_private_identifiers_valid(names)
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// VariableDeclarationList[In, Yield, Await] :
//      VariableDeclaration[?In, ?Yield, ?Await]
//      VariableDeclarationList[?In, ?Yield, ?Await] , VariableDeclaration[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum VariableDeclarationList {
    Item(Rc<VariableDeclaration>),
    List(Rc<VariableDeclarationList>, Rc<VariableDeclaration>),
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
    fn parse_core(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (decl, after_dcl) = VariableDeclaration::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        let mut current = Rc::new(VariableDeclarationList::Item(decl));
        let mut current_scanner = after_dcl;
        while let Ok((next, after_next)) = scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
            .and_then(|after_comma| VariableDeclaration::parse(parser, after_comma, in_flag, yield_flag, await_flag))
        {
            current = Rc::new(VariableDeclarationList::List(current, next));
            current_scanner = after_next;
        }
        Ok((current, current_scanner))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = InYieldAwaitKey { scanner, in_flag, yield_flag, await_flag };
        match parser.variable_declaration_list_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, in_flag, yield_flag, await_flag);
                parser.variable_declaration_list_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            VariableDeclarationList::Item(node) => node.bound_names(),
            VariableDeclarationList::List(lst, item) => {
                let mut names = lst.bound_names();
                names.extend(item.bound_names());
                names
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            VariableDeclarationList::Item(node) => node.contains(kind),
            VariableDeclarationList::List(lst, item) => lst.contains(kind) || item.contains(kind),
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
            VariableDeclarationList::Item(node) => node.all_private_identifiers_valid(names),
            VariableDeclarationList::List(lst, item) => lst.all_private_identifiers_valid(names) && item.all_private_identifiers_valid(names),
        }
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// VariableDeclaration[In, Yield, Await] :
//      BindingIdentifier[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]opt
//      BindingPattern[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]
#[derive(Debug)]
pub enum VariableDeclaration {
    Identifier(Rc<BindingIdentifier>, Option<Rc<Initializer>>),
    Pattern(Rc<BindingPattern>, Rc<Initializer>),
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
    // no cache
    pub fn parse(parser: &mut Parser, scanner: Scanner, in_flag: bool, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::VariableDeclaration), scanner))
            .otherwise(|| {
                let (bi, after_bi) = BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
                let pot_init = Initializer::parse(parser, after_bi, in_flag, yield_flag, await_flag);
                let (init, after_init) = match pot_init {
                    Err(_) => (None, after_bi),
                    Ok((i, after_i)) => (Some(i), after_i),
                };
                Ok((Rc::new(VariableDeclaration::Identifier(bi, init)), after_init))
            })
            .otherwise(|| {
                let (bp, after_bp) = BindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
                let (init, after_init) = Initializer::parse(parser, after_bp, in_flag, yield_flag, await_flag)?;
                Ok((Rc::new(VariableDeclaration::Pattern(bp, init)), after_init))
            })
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            VariableDeclaration::Identifier(bi, _) => bi.bound_names(),
            VariableDeclaration::Pattern(bp, _) => bp.bound_names(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            VariableDeclaration::Identifier(bi, None) => bi.contains(kind),
            VariableDeclaration::Identifier(bi, Some(init)) => bi.contains(kind) || init.contains(kind),
            VariableDeclaration::Pattern(bp, init) => bp.contains(kind) || init.contains(kind),
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
            VariableDeclaration::Identifier(_, None) => true,
            VariableDeclaration::Identifier(_, Some(node)) => node.all_private_identifiers_valid(names),
            VariableDeclaration::Pattern(node1, node2) => node1.all_private_identifiers_valid(names) && node2.all_private_identifiers_valid(names),
        }
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// BindingPattern[Yield, Await] :
//      ObjectBindingPattern[?Yield, ?Await]
//      ArrayBindingPattern[?Yield, ?Await]
#[derive(Debug)]
pub enum BindingPattern {
    Object(Rc<ObjectBindingPattern>),
    Array(Rc<ArrayBindingPattern>),
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
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::BindingPattern), scanner))
            .otherwise(|| ObjectBindingPattern::parse(parser, scanner, yield_flag, await_flag).map(|(obp, after_obp)| (Rc::new(BindingPattern::Object(obp)), after_obp)))
            .otherwise(|| ArrayBindingPattern::parse(parser, scanner, yield_flag, await_flag).map(|(abp, after_abp)| (Rc::new(BindingPattern::Array(abp)), after_abp)))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.binding_pattern_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.binding_pattern_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            BindingPattern::Object(node) => node.bound_names(),
            BindingPattern::Array(node) => node.bound_names(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            BindingPattern::Object(node) => node.contains(kind),
            BindingPattern::Array(node) => node.contains(kind),
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
            BindingPattern::Object(node) => node.all_private_identifiers_valid(names),
            BindingPattern::Array(node) => node.all_private_identifiers_valid(names),
        }
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
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
    RestOnly(Rc<BindingRestProperty>),
    ListOnly(Rc<BindingPropertyList>),
    ListRest(Rc<BindingPropertyList>, Option<Rc<BindingRestProperty>>),
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
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ObjectBindingPattern::ListOnly(node) => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ObjectBindingPattern::ListRest(lst, Some(rst)) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                rst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ObjectBindingPattern::ListRest(lst, None) => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
            }
        }
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ObjectBindingPattern {
    // no cache
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::ObjectBindingPattern), scanner)).otherwise(|| {
            scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBrace).and_then(|after_open| {
                scan_for_punct(after_open, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBrace)
                    .map(|after_close| (Rc::new(ObjectBindingPattern::Empty), after_close))
                    .otherwise(|| {
                        BindingRestProperty::parse(parser, after_open, yield_flag, await_flag).and_then(|(brp, after_brp)| {
                            scan_for_punct(after_brp, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBrace)
                                .map(|after_close| (Rc::new(ObjectBindingPattern::RestOnly(brp)), after_close))
                        })
                    })
                    .otherwise(|| {
                        BindingPropertyList::parse(parser, after_open, yield_flag, await_flag).and_then(|(bpl, after_bpl)| {
                            match scan_for_punct(after_bpl, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBrace).map(|after_close| (None, after_close)).otherwise(|| {
                                scan_for_punct(after_bpl, parser.source, ScanGoal::InputElementRegExp, Punctuator::Comma).and_then(|after_comma| {
                                    let (brp, after_brp) = match BindingRestProperty::parse(parser, after_comma, yield_flag, await_flag) {
                                        Err(_) => (None, after_comma),
                                        Ok((node, s)) => (Some(node), s),
                                    };
                                    scan_for_punct(after_brp, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBrace).map(|after_final| (Some(brp), after_final))
                                })
                            }) {
                                Ok((None, after)) => Ok((Rc::new(ObjectBindingPattern::ListOnly(bpl)), after)),
                                Ok((Some(brp), after)) => Ok((Rc::new(ObjectBindingPattern::ListRest(bpl, brp)), after)),
                                Err(e) => Err(e),
                            }
                        })
                    })
            })
        })
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            ObjectBindingPattern::Empty => vec![],
            ObjectBindingPattern::RestOnly(node) => node.bound_names(),
            ObjectBindingPattern::ListOnly(node) => node.bound_names(),
            ObjectBindingPattern::ListRest(lst, Some(rst)) => {
                let mut names = lst.bound_names();
                names.extend(rst.bound_names());
                names
            }
            ObjectBindingPattern::ListRest(lst, None) => lst.bound_names(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ObjectBindingPattern::Empty => false,
            ObjectBindingPattern::RestOnly(node) => node.contains(kind),
            ObjectBindingPattern::ListOnly(node) => node.contains(kind),
            ObjectBindingPattern::ListRest(list, None) => list.contains(kind),
            ObjectBindingPattern::ListRest(list, Some(n)) => list.contains(kind) || n.contains(kind),
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
            ObjectBindingPattern::Empty | ObjectBindingPattern::RestOnly(_) => true,
            ObjectBindingPattern::ListOnly(node) | ObjectBindingPattern::ListRest(node, _) => node.all_private_identifiers_valid(names),
        }
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// ArrayBindingPattern[Yield, Await] :
//      [ Elisionopt BindingRestElement[?Yield, ?Await]opt ]
//      [ BindingElementList[?Yield, ?Await] ]
//      [ BindingElementList[?Yield, ?Await] , Elisionopt BindingRestElement[?Yield, ?Await]opt ]
#[derive(Debug)]
pub enum ArrayBindingPattern {
    RestOnly(Option<Rc<Elisions>>, Option<Rc<BindingRestElement>>),
    ListOnly(Rc<BindingElementList>),
    ListRest(Rc<BindingElementList>, Option<Rc<Elisions>>, Option<Rc<BindingRestElement>>),
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
    // ArrayBindingPattern's only parent is BindingPattern. It doesn't need to be cached.
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_first = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBracket)?;
        BindingElementList::parse(parser, after_first, yield_flag, await_flag)
            .and_then(|(bel, after_bel)| {
                scan_for_punct_set(after_bel, parser.source, ScanGoal::InputElementRegExp, &[Punctuator::RightBracket, Punctuator::Comma]).and_then(|(punct_next, after_next)| {
                    match punct_next {
                        Punctuator::RightBracket => Ok((Rc::new(ArrayBindingPattern::ListOnly(bel)), after_next)),
                        _ => {
                            let (elisions, after_elisions) = match Elisions::parse(parser, after_next) {
                                Err(_) => (None, after_next),
                                Ok((e, s)) => (Some(e), s),
                            };
                            let (bre, after_bre, err_bre) = match BindingRestElement::parse(parser, after_elisions, yield_flag, await_flag) {
                                Err(err) => (None, after_elisions, Some(err)),
                                Ok((b, s)) => (Some(b), s, None),
                            };
                            match scan_for_punct(after_bre, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBracket) {
                                Ok(after_close) => Ok((Rc::new(ArrayBindingPattern::ListRest(bel, elisions, bre)), after_close)),
                                Err(pe) => {
                                    let mut err = Some(pe);
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
                let (elisions, after_elisions) = match Elisions::parse(parser, after_first) {
                    Err(_) => (None, after_first),
                    Ok((e, s)) => (Some(e), s),
                };
                let (bre, after_bre, err_bre) = match BindingRestElement::parse(parser, after_elisions, yield_flag, await_flag) {
                    Err(err) => (None, after_elisions, Some(err)),
                    Ok((b, s)) => (Some(b), s, None),
                };
                match scan_for_punct(after_bre, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBracket) {
                    Ok(after_close) => Ok((Rc::new(ArrayBindingPattern::RestOnly(elisions, bre)), after_close)),
                    Err(pe) => {
                        let mut err = Some(pe);
                        if ParseError::compare_option(&err_bre, &err) == Ordering::Greater {
                            err = err_bre;
                        }
                        Err(err.unwrap())
                    }
                }
            })
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            ArrayBindingPattern::RestOnly(_, Some(node)) => node.bound_names(),
            ArrayBindingPattern::RestOnly(_, None) => vec![],
            ArrayBindingPattern::ListOnly(node) => node.bound_names(),
            ArrayBindingPattern::ListRest(lst, _, Some(rst)) => {
                let mut names = lst.bound_names();
                names.extend(rst.bound_names());
                names
            }
            ArrayBindingPattern::ListRest(lst, _, None) => lst.bound_names(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ArrayBindingPattern::RestOnly(onode_a, onode_b) => onode_a.as_ref().map_or(false, |node| node.contains(kind)) || onode_b.as_ref().map_or(false, |node| node.contains(kind)),
            ArrayBindingPattern::ListOnly(node) => node.contains(kind),
            ArrayBindingPattern::ListRest(node, onode_a, onode_b) => {
                node.contains(kind) || onode_a.as_ref().map_or(false, |node| node.contains(kind)) || onode_b.as_ref().map_or(false, |node| node.contains(kind))
            }
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
            ArrayBindingPattern::RestOnly(_, onode) => onode.as_ref().map_or(true, |node| node.all_private_identifiers_valid(names)),
            ArrayBindingPattern::ListOnly(node) => node.all_private_identifiers_valid(names),
            ArrayBindingPattern::ListRest(node, _, onode) => node.all_private_identifiers_valid(names) && onode.as_ref().map_or(true, |node| node.all_private_identifiers_valid(names)),
        }
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// BindingRestProperty[Yield, Await] :
//      ... BindingIdentifier[?Yield, ?Await]
#[derive(Debug)]
pub enum BindingRestProperty {
    Id(Rc<BindingIdentifier>),
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
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Ellipsis)
            .and_then(|after_dots| BindingIdentifier::parse(parser, after_dots, yield_flag, await_flag))
            .map(|(id, after_id)| (Rc::new(BindingRestProperty::Id(id)), after_id))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.binding_rest_property_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.binding_rest_property_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        let BindingRestProperty::Id(node) = self;
        node.bound_names()
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let BindingRestProperty::Id(node) = self;
        node.contains(kind)
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// BindingPropertyList[Yield, Await] :
//      BindingProperty[?Yield, ?Await]
//      BindingPropertyList[?Yield, ?Await] , BindingProperty[?Yield, ?Await]
#[derive(Debug)]
pub enum BindingPropertyList {
    Item(Rc<BindingProperty>),
    List(Rc<BindingPropertyList>, Rc<BindingProperty>),
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
    // no cache
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (bp, after_bp) = BindingProperty::parse(parser, scanner, yield_flag, await_flag)?;
        let mut current = Rc::new(BindingPropertyList::Item(bp));
        let mut current_scan = after_bp;
        while let Ok((bp2, after_bp2)) = scan_for_punct(current_scan, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
            .and_then(|after_token| BindingProperty::parse(parser, after_token, yield_flag, await_flag))
        {
            current = Rc::new(BindingPropertyList::List(current, bp2));
            current_scan = after_bp2;
        }
        Ok((current, current_scan))
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            BindingPropertyList::Item(node) => node.bound_names(),
            BindingPropertyList::List(lst, item) => {
                let mut names = lst.bound_names();
                names.extend(item.bound_names());
                names
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            BindingPropertyList::Item(node) => node.contains(kind),
            BindingPropertyList::List(lst, item) => lst.contains(kind) || item.contains(kind),
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
            BindingPropertyList::Item(node) => node.all_private_identifiers_valid(names),
            BindingPropertyList::List(lst, item) => lst.all_private_identifiers_valid(names) && item.all_private_identifiers_valid(names),
        }
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// BindingElementList[Yield, Await] :
//      BindingElisionElement[?Yield, ?Await]
//      BindingElementList[?Yield, ?Await] , BindingElisionElement[?Yield, ?Await]
#[derive(Debug)]
pub enum BindingElementList {
    Item(Rc<BindingElisionElement>),
    List(Rc<BindingElementList>, Rc<BindingElisionElement>),
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
    // no cache
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (elem, after_elem) = BindingElisionElement::parse(parser, scanner, yield_flag, await_flag)?;
        let mut current = Rc::new(BindingElementList::Item(elem));
        let mut current_scanner = after_elem;
        loop {
            match scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
                .and_then(|after_tok| BindingElisionElement::parse(parser, after_tok, yield_flag, await_flag))
            {
                Err(_) => {
                    break;
                }
                Ok((next, after_next)) => {
                    current = Rc::new(BindingElementList::List(current, next));
                    current_scanner = after_next;
                }
            }
        }
        Ok((current, current_scanner))
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            BindingElementList::Item(node) => node.bound_names(),
            BindingElementList::List(lst, item) => {
                let mut names = lst.bound_names();
                names.extend(item.bound_names());
                names
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            BindingElementList::Item(node) => node.contains(kind),
            BindingElementList::List(l, i) => l.contains(kind) || i.contains(kind),
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
            BindingElementList::Item(node) => node.all_private_identifiers_valid(names),
            BindingElementList::List(l, i) => l.all_private_identifiers_valid(names) && i.all_private_identifiers_valid(names),
        }
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// BindingElisionElement[Yield, Await] :
//      Elisionopt BindingElement[?Yield, ?Await]
#[derive(Debug)]
pub enum BindingElisionElement {
    Element(Option<Rc<Elisions>>, Rc<BindingElement>),
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
    // no cache
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (elision, after_elision) = match Elisions::parse(parser, scanner) {
            Err(_) => (None, scanner),
            Ok((e, s)) => (Some(e), s),
        };
        let (be, after_be) = BindingElement::parse(parser, after_elision, yield_flag, await_flag)?;
        Ok((Rc::new(BindingElisionElement::Element(elision, be)), after_be))
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        let BindingElisionElement::Element(_, elem) = self;
        elem.bound_names()
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let BindingElisionElement::Element(opt, n) = self;
        opt.as_ref().map_or(false, |n| n.contains(kind)) || n.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        let BindingElisionElement::Element(_, n) = self;
        n.all_private_identifiers_valid(names)
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// BindingProperty[Yield, Await] :
//      SingleNameBinding[?Yield, ?Await]
//      PropertyName[?Yield, ?Await] : BindingElement[?Yield, ?Await]
#[derive(Debug)]
pub enum BindingProperty {
    Single(Rc<SingleNameBinding>),
    Property(Rc<PropertyName>, Rc<BindingElement>),
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
    // no cache
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::BindingProperty), scanner))
            .otherwise(|| {
                let (pn, after_pn) = PropertyName::parse(parser, scanner, yield_flag, await_flag)?;
                let after_token = scan_for_punct(after_pn, parser.source, ScanGoal::InputElementDiv, Punctuator::Colon)?;
                let (be, after_be) = BindingElement::parse(parser, after_token, yield_flag, await_flag)?;
                Ok((Rc::new(BindingProperty::Property(pn, be)), after_be))
            })
            .otherwise(|| {
                let (snb, after_snb) = SingleNameBinding::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(BindingProperty::Single(snb)), after_snb))
            })
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            BindingProperty::Single(node) => node.bound_names(),
            BindingProperty::Property(_, elem) => elem.bound_names(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            BindingProperty::Single(node) => node.contains(kind),
            BindingProperty::Property(node1, node2) => node1.contains(kind) || node2.contains(kind),
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
            BindingProperty::Single(node) => node.all_private_identifiers_valid(names),
            BindingProperty::Property(node1, node2) => node1.all_private_identifiers_valid(names) && node2.all_private_identifiers_valid(names),
        }
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// BindingElement[Yield, Await] :
//      SingleNameBinding[?Yield, ?Await]
//      BindingPattern[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]opt
#[derive(Debug)]
pub enum BindingElement {
    Single(Rc<SingleNameBinding>),
    Pattern(Rc<BindingPattern>, Option<Rc<Initializer>>),
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
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::BindingElement), scanner))
            .otherwise(|| {
                let (bp, after_bp) = BindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
                let (init, after_init) = match Initializer::parse(parser, after_bp, true, yield_flag, await_flag) {
                    Err(_) => (None, after_bp),
                    Ok((i, s)) => (Some(i), s),
                };
                Ok((Rc::new(BindingElement::Pattern(bp, init)), after_init))
            })
            .otherwise(|| {
                let (snb, after_snb) = SingleNameBinding::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(BindingElement::Single(snb)), after_snb))
            })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.binding_element_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.binding_element_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            BindingElement::Single(node) => node.bound_names(),
            BindingElement::Pattern(node, _) => node.bound_names(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            BindingElement::Single(n) => n.contains(kind),
            BindingElement::Pattern(n, opt) => n.contains(kind) || opt.as_ref().map_or(false, |n| n.contains(kind)),
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
            BindingElement::Single(n) => n.all_private_identifiers_valid(names),
            BindingElement::Pattern(n, opt) => n.all_private_identifiers_valid(names) && opt.as_ref().map_or(true, |n| n.all_private_identifiers_valid(names)),
        }
    }

    pub fn is_simple_parameter_list(&self) -> bool {
        // Static Semantics: IsSimpleParameterList
        match self {
            BindingElement::Single(single_name_binding) => {
                // BindingElement : SingleNameBinding
                //  1. Return IsSimpleParameterList of SingleNameBinding
                single_name_binding.is_simple_parameter_list()
            }
            BindingElement::Pattern(..) => {
                // BindingElement : BindingPattern
                // BindingElement : BindingPattern Initializer
                //  1. Return false.
                false
            }
        }
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// SingleNameBinding[Yield, Await] :
//      BindingIdentifier[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]opt
#[derive(Debug)]
pub enum SingleNameBinding {
    Id(Rc<BindingIdentifier>, Option<Rc<Initializer>>),
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
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (bi, after_bi) = BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
        let (init, after_init) = match Initializer::parse(parser, after_bi, true, yield_flag, await_flag) {
            Err(_) => (None, after_bi),
            Ok((i, s)) => (Some(i), s),
        };
        Ok((Rc::new(SingleNameBinding::Id(bi, init)), after_init))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.single_name_binding_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.single_name_binding_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        let SingleNameBinding::Id(ident, _) = self;
        ident.bound_names()
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let SingleNameBinding::Id(ident, opt) = self;
        ident.contains(kind) || opt.as_ref().map_or(false, |n| n.contains(kind))
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        let SingleNameBinding::Id(_, opt) = self;
        opt.as_ref().map_or(true, |n| n.all_private_identifiers_valid(names))
    }

    pub fn is_simple_parameter_list(&self) -> bool {
        // Static Semantics: IsSimpleParameterList
        // SingleNameBinding : BindingIdentifier
        //  1. Return true.
        // SingleNameBinding : BindingIdentifier Initializer
        //  1. Return false.
        let SingleNameBinding::Id(_, initializer) = self;
        initializer.is_none()
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// BindingRestElement[Yield, Await] :
//      ... BindingIdentifier[?Yield, ?Await]
//      ... BindingPattern[?Yield, ?Await]
#[derive(Debug)]
pub enum BindingRestElement {
    Identifier(Rc<BindingIdentifier>),
    Pattern(Rc<BindingPattern>),
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
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let after_tok = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Ellipsis)?;
        Err(ParseError::new(PECode::OpenOrIdentExpected, after_tok))
            .otherwise(|| BindingPattern::parse(parser, after_tok, yield_flag, await_flag).map(|(bp, after_bp)| (Rc::new(BindingRestElement::Pattern(bp)), after_bp)))
            .otherwise(|| BindingIdentifier::parse(parser, after_tok, yield_flag, await_flag).map(|(bi, after_bi)| (Rc::new(BindingRestElement::Identifier(bi)), after_bi)))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.binding_rest_element_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.binding_rest_element_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            BindingRestElement::Identifier(node) => node.bound_names(),
            BindingRestElement::Pattern(node) => node.bound_names(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            BindingRestElement::Identifier(node) => node.contains(kind),
            BindingRestElement::Pattern(node) => node.contains(kind),
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
            BindingRestElement::Identifier(_) => true,
            BindingRestElement::Pattern(node) => node.all_private_identifiers_valid(names),
        }
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

#[cfg(test)]
mod tests;
