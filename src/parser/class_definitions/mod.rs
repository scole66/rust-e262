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
        let after_class = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Class)?;
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

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        match self {
            ClassDeclaration::Named(_, node) | ClassDeclaration::Unnamed(node) => node.all_private_identifiers_valid(names),
        }
    }

    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        match self {
            ClassDeclaration::Named(_, ct) | ClassDeclaration::Unnamed(ct) => ct.contains_arguments(),
        }
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
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
        let after_class = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Class)?;
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

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.tail.all_private_identifiers_valid(names)
    }

    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.tail.contains_arguments()
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
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

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.heritage.as_ref().map_or(true, |node| node.all_private_identifiers_valid(names)) && self.body.as_ref().map_or(true, |node| node.all_private_identifiers_valid(names))
    }

    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.heritage.as_ref().map_or(false, |ch| ch.contains_arguments()) || self.body.as_ref().map_or(false, |cb| cb.contains_arguments())
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
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
        let after_extends = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Extends)?;
        let (lhs, after_lhs) = LeftHandSideExpression::parse(parser, after_extends, yield_flag, await_flag)?;
        Ok((Rc::new(ClassHeritage(lhs)), after_lhs))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.0.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.0.all_private_identifiers_valid(names)
    }

    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.0.contains_arguments()
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
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

    pub fn private_bound_identifiers(&self) -> Vec<JSString> {
        // Static Semantics: PrivateBoundIdentifiers
        // ClassBody : ClassElementList
        //  1. Return PrivateBoundIdentifiers of ClassElementList.
        self.0.private_bound_identifiers()
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        // ClassBody : ClassElementList
        //  1. Let newNames be the list-concatenation of names and PrivateBoundIdentifiers of ClassBody.
        //  2. Return AllPrivateIdentifiersValid of ClassElementList with argument newNames.
        let mut new_names = Vec::<JSString>::new();
        new_names.extend_from_slice(names);
        new_names.extend(self.private_bound_identifiers());
        self.0.all_private_identifiers_valid(&new_names)
    }

    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.0.contains_arguments()
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
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

    pub fn private_bound_identifiers(&self) -> Vec<JSString> {
        // Static Semantics: PrivateBoundIdentifiers
        match self {
            ClassElementList::List(lst, elem) => {
                // ClassElementList : ClassElementList ClassElement
                //  1. Let names1 be PrivateBoundIdentifiers of ClassElementList.
                //  2. Let names2 be PrivateBoundIdentifiers of ClassElement.
                //  3. Return the list-concatenation of names1 and names2.
                let mut ids = lst.private_bound_identifiers();
                ids.extend(elem.private_bound_identifiers());
                ids
            }
            ClassElementList::Item(node) => {
                // ClassElementList : ClassElement
                //  1. Return PrivateBoundIdentifiers of ClassElement.
                node.private_bound_identifiers()
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
            ClassElementList::Item(node) => node.all_private_identifiers_valid(names),
            ClassElementList::List(node1, node2) => node1.all_private_identifiers_valid(names) && node2.all_private_identifiers_valid(names),
        }
    }

    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        match self {
            ClassElementList::Item(ce) => ce.contains_arguments(),
            ClassElementList::List(cel, ce) => cel.contains_arguments() || ce.contains_arguments(),
        }
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// ClassElement[Yield, Await] :
//      MethodDefinition[?Yield, ?Await]
//      static MethodDefinition[?Yield, ?Await]
//      FieldDefinition[?Yield, ?Await] ;
//      static FieldDefinition[?Yield, ?Await] ;
//      ClassStaticBlock
//      ;
#[derive(Debug)]
pub enum ClassElement {
    Standard(Rc<MethodDefinition>),
    Static(Rc<MethodDefinition>),
    Field(Rc<FieldDefinition>),
    StaticField(Rc<FieldDefinition>),
    StaticBlock(Rc<ClassStaticBlock>),
    Empty,
}

impl fmt::Display for ClassElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ClassElement::Standard(n) => n.fmt(f),
            ClassElement::Static(n) => write!(f, "static {}", n),
            ClassElement::Field(n) => write!(f, "{} ;", n),
            ClassElement::StaticField(n) => write!(f, "static {} ;", n),
            ClassElement::StaticBlock(n) => n.fmt(f),
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
            ClassElement::Field(n) => n.pprint_with_leftpad(writer, &successive, Spot::Final),
            ClassElement::StaticField(n) => n.pprint_with_leftpad(writer, &successive, Spot::Final),
            ClassElement::StaticBlock(n) => n.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let head = |f: &mut T| {
            let (first, successive) = prettypad(pad, state);
            writeln!(f, "{}ClassElement: {}", first, self)?;
            Ok(successive) as IoResult<String>
        };
        let head_n_static = |f: &mut T| {
            let successive = head(f)?;
            pprint_token(f, "static", TokenType::Keyword, &successive, Spot::NotFinal)?;
            Ok(successive) as IoResult<String>
        };
        match self {
            ClassElement::Static(n) => {
                let successive = head_n_static(writer)?;
                n.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            ClassElement::Standard(n) => n.concise_with_leftpad(writer, pad, state),
            ClassElement::Empty => pprint_token(writer, ";", TokenType::Punctuator, pad, state),
            ClassElement::StaticField(n) => {
                let successive = head_n_static(writer)?;
                n.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
            }
            ClassElement::Field(n) => {
                let successive = head(writer)?;
                n.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
            }
            ClassElement::StaticBlock(n) => n.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl ClassElement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::ClassElement), scanner))
            .otherwise(|| ClassStaticBlock::parse(parser, scanner).map(|(sb, after_sb)| (Rc::new(ClassElement::StaticBlock(sb)), after_sb)))
            .otherwise(|| {
                scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Static)
                    .and_then(|after_static| {
                        MethodDefinition::parse(parser, after_static, yield_flag, await_flag).map(|(md, after_md)| (Rc::new(ClassElement::Static(md)), after_md)).otherwise(|| {
                            FieldDefinition::parse(parser, after_static, yield_flag, await_flag).and_then(|(fd, after_fd)| {
                                scan_for_auto_semi(after_fd, parser.source, ScanGoal::InputElementDiv).map(|after_semi| (Rc::new(ClassElement::StaticField(fd)), after_semi))
                            })
                        })
                    })
                    .otherwise(|| scan_for_punct(scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Semicolon).map(|after_semi| (Rc::new(ClassElement::Empty), after_semi)))
                    .otherwise(|| MethodDefinition::parse(parser, scanner, yield_flag, await_flag).map(|(md, after_md)| (Rc::new(ClassElement::Standard(md)), after_md)))
                    .otherwise(|| {
                        FieldDefinition::parse(parser, scanner, yield_flag, await_flag).and_then(|(fd, after_fd)| {
                            scan_for_auto_semi(after_fd, parser.source, ScanGoal::InputElementDiv).map(|after_semi| (Rc::new(ClassElement::Field(fd)), after_semi))
                        })
                    })
            })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ClassElement::Standard(n) | ClassElement::Static(n) => kind == ParseNodeKind::MethodDefinition || n.contains(kind),
            ClassElement::Empty => false,
            ClassElement::Field(n) | ClassElement::StaticField(n) => n.contains(kind),
            ClassElement::StaticBlock(sb) => sb.contains(),
        }
    }

    pub fn computed_property_contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ClassElement::Standard(n) | ClassElement::Static(n) => n.computed_property_contains(kind),
            ClassElement::Empty => false,
            ClassElement::Field(n) | ClassElement::StaticField(n) => n.computed_property_contains(kind),
            ClassElement::StaticBlock(_) => false,
        }
    }

    pub fn private_bound_identifiers(&self) -> Vec<JSString> {
        // Static Semantics: PrivateBoundIdentifiers
        match self {
            // ClassElement : ClassStaticBlock
            // ClassElement : ;
            //  1. Return a new empty List.
            ClassElement::Empty | ClassElement::StaticBlock(_) => Vec::new(),

            // ClassElement : MethodDefinition
            // ClassElement : static MethodDefinition
            //  1. Return PrivateBoundIdentifiers of MethodDefinition.
            ClassElement::Standard(md) | ClassElement::Static(md) => md.private_bound_identifiers(),

            // ClassElement : FieldDefinition
            // ClassElement : static FieldDefinition
            //  1. Return PrivateBoundIdentifiers of FieldDefinition.
            ClassElement::Field(fd) | ClassElement::StaticField(fd) => fd.private_bound_identifiers(),
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
            ClassElement::Standard(md) | ClassElement::Static(md) => md.all_private_identifiers_valid(names),
            ClassElement::Field(fd) | ClassElement::StaticField(fd) => fd.all_private_identifiers_valid(names),
            ClassElement::StaticBlock(sb) => sb.all_private_identifiers_valid(names),
            ClassElement::Empty => true,
        }
    }

    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        match self {
            ClassElement::Standard(md) | ClassElement::Static(md) => md.contains_arguments(),
            ClassElement::Field(fd) | ClassElement::StaticField(fd) => fd.contains_arguments(),
            ClassElement::StaticBlock(sb) => sb.contains_arguments(),
            ClassElement::Empty => false,
        }
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// FieldDefinition[Yield, Await] :
//      ClassElementName[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]opt
#[derive(Debug)]
pub struct FieldDefinition {
    name: Rc<ClassElementName>,
    init: Option<Rc<Initializer>>,
}

impl fmt::Display for FieldDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.init {
            None => self.name.fmt(f),
            Some(init) => write!(f, "{} {}", self.name, init),
        }
    }
}

impl PrettyPrint for FieldDefinition {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FieldDefinition: {}", first, self)?;
        match &self.init {
            None => self.name.pprint_with_leftpad(writer, &successive, Spot::Final),
            Some(init) => {
                self.name.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                init.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match &self.init {
            None => self.name.concise_with_leftpad(writer, pad, state),
            Some(init) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{}FieldDefinition: {}", first, self)?;
                self.name.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                init.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl FieldDefinition {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        ClassElementName::parse(parser, scanner, yield_flag, await_flag).map(|(cen, after_cen)| {
            let (init, after_init) = match Initializer::parse(parser, after_cen, true, yield_flag, await_flag) {
                Err(_) => (None, after_cen),
                Ok((id, after_id)) => (Some(id), after_id),
            };
            (Rc::new(FieldDefinition { name: cen, init }), after_init)
        })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.name.contains(kind) || self.init.as_ref().map_or(false, |n| n.contains(kind))
    }

    pub fn computed_property_contains(&self, kind: ParseNodeKind) -> bool {
        self.name.computed_property_contains(kind)
    }

    pub fn private_bound_identifiers(&self) -> Vec<JSString> {
        // Static Semantics: PrivateBoundIdentifiers
        // FieldDefinition : ClassElementName Initializer [opt]
        //  1. Return PrivateBoundIdentifiers of ClassElementName.
        self.name.private_bound_identifiers()
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.name.all_private_identifiers_valid(names) && self.init.as_ref().map_or(true, |init| init.all_private_identifiers_valid(names))
    }

    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.name.contains_arguments() || self.init.as_ref().map_or(false, |izer| izer.contains_arguments())
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// ClassElementName[Yield, Await] :
//      PropertyName[?Yield, ?Await]
//      PrivateIdentifier
#[derive(Debug)]
pub enum ClassElementName {
    PropertyName(Rc<PropertyName>),
    PrivateIdentifier(IdentifierData),
}

impl fmt::Display for ClassElementName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ClassElementName::PropertyName(n) => n.fmt(f),
            ClassElementName::PrivateIdentifier(n) => n.fmt(f),
        }
    }
}

impl PrettyPrint for ClassElementName {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ClassElementName: {}", first, self)?;
        match self {
            ClassElementName::PropertyName(n) => n.pprint_with_leftpad(writer, &successive, Spot::Final),
            ClassElementName::PrivateIdentifier(n) => pprint_token(writer, n, TokenType::PrivateIdentifier, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            ClassElementName::PropertyName(n) => n.concise_with_leftpad(writer, pad, state),
            ClassElementName::PrivateIdentifier(id) => pprint_token(writer, id, TokenType::PrivateIdentifier, pad, state),
        }
    }
}

impl ClassElementName {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::ClassElementName), scanner)).otherwise(|| {
            PropertyName::parse(parser, scanner, yield_flag, await_flag)
                .map(|(item, scan)| (Rc::new(ClassElementName::PropertyName(item)), scan))
                .otherwise(|| scan_for_private_identifier(scanner, parser.source, ScanGoal::InputElementDiv).map(|(item, scan)| (Rc::new(ClassElementName::PrivateIdentifier(item)), scan)))
        })
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ClassElementName::PropertyName(n) => n.contains(kind),
            ClassElementName::PrivateIdentifier(_) => false,
        }
    }

    pub fn computed_property_contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ClassElementName::PropertyName(n) => n.computed_property_contains(kind),
            ClassElementName::PrivateIdentifier(_) => false,
        }
    }

    pub fn private_bound_identifiers(&self) -> Vec<JSString> {
        // Static Semantics: PrivateBoundIdentifiers
        match self {
            // ClassElementName : PropertyName
            //  1. Return a new empty List.
            ClassElementName::PropertyName(_) => Vec::new(),

            // ClassElementName : PrivateIdentifier
            //  1. Return a List whose sole element is the StringValue of PrivateIdentifier.
            ClassElementName::PrivateIdentifier(pid) => vec![pid.string_value.clone()],
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
            ClassElementName::PropertyName(node) => node.all_private_identifiers_valid(names),
            ClassElementName::PrivateIdentifier(_) => true,
        }
    }

    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        match self {
            ClassElementName::PropertyName(pn) => pn.contains_arguments(),
            ClassElementName::PrivateIdentifier(_) => false,
        }
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }

    pub fn prop_name(&self) -> Option<JSString> {
        // Static Semantics: PropName
        // The syntax-directed operation PropName takes no arguments and returns a String or empty.
        match self {
            ClassElementName::PropertyName(node) => node.prop_name(),
            ClassElementName::PrivateIdentifier(_) => {
                // ClassElementName : PrivateIdentifier
                //  1. Return empty.
                None
            }
        }
    }
}

// ClassStaticBlock :
//      static { ClassStaticBlockBody }
#[derive(Debug)]
pub struct ClassStaticBlock(Rc<ClassStaticBlockBody>);

impl fmt::Display for ClassStaticBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "static {{ {} }}", self.0)
    }
}

impl PrettyPrint for ClassStaticBlock {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ClassStaticBlock: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ClassStaticBlock: {}", first, self)?;
        pprint_token(writer, "static", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.0.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl ClassStaticBlock {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let after_static = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Static)?;
        let after_lb = scan_for_punct(after_static, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBrace)?;
        let (block, after_block) = ClassStaticBlockBody::parse(parser, after_lb);
        let after_close = scan_for_punct(after_block, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(ClassStaticBlock(block)), after_close))
    }

    pub fn contains(&self) -> bool {
        false
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.0.all_private_identifiers_valid(names)
    }

    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.0.contains_arguments()
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// ClassStaticBlockBody :
//      ClassStaticBlockStatementList
#[derive(Debug)]
pub struct ClassStaticBlockBody(Rc<ClassStaticBlockStatementList>);

impl fmt::Display for ClassStaticBlockBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PrettyPrint for ClassStaticBlockBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ClassStaticBlockBody: {}", first, self)?;
        self.0.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.0.concise_with_leftpad(writer, pad, state)
    }
}

impl ClassStaticBlockBody {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> (Rc<Self>, Scanner) {
        let (sl, after_sl) = ClassStaticBlockStatementList::parse(parser, scanner);
        (Rc::new(ClassStaticBlockBody(sl)), after_sl)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.0.all_private_identifiers_valid(names)
    }

    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.0.contains_arguments()
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

// ClassStaticBlockStatementList :
//      StatementList[~Yield, +Await, ~Return]opt
#[derive(Debug)]
pub struct ClassStaticBlockStatementList(Option<Rc<StatementList>>);

impl fmt::Display for ClassStaticBlockStatementList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.as_ref().map_or(Ok(()), |node| node.fmt(f))
    }
}

impl PrettyPrint for ClassStaticBlockStatementList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}ClassStaticBlockStatementList: {}", first, self)?;
        self.0.as_ref().map_or(Ok(()), |node| node.pprint_with_leftpad(writer, &successive, Spot::Final))
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.0.as_ref().map_or(Ok(()), |node| node.concise_with_leftpad(writer, pad, state))
    }
}

impl ClassStaticBlockStatementList {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> (Rc<Self>, Scanner) {
        match StatementList::parse(parser, scanner, false, true, false) {
            Ok((sl, after)) => (Rc::new(ClassStaticBlockStatementList(Some(sl))), after),
            Err(_) => (Rc::new(ClassStaticBlockStatementList(None)), scanner),
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.0.as_ref().map_or(true, |sl| sl.all_private_identifiers_valid(names))
    }

    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.0.as_ref().map_or(false, |sl| sl.contains_arguments())
    }

    #[allow(clippy::ptr_arg)]
    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut Vec<Object>, _strict: bool) {
        todo!()
    }
}

#[cfg(test)]
mod tests;
