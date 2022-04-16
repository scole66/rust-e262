use ahash::AHashMap;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;
use std::rc::Rc;

use super::method_definitions::MethodType;
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

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
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

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Class Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>) {
        if let ClassDeclaration::Named(name, _) = self {
            name.early_errors(agent, errs, true);
        }
        let tail = match self {
            ClassDeclaration::Named(_, tail) => tail,
            ClassDeclaration::Unnamed(tail) => tail,
        };
        tail.early_errors(agent, errs, true);
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

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.tail.contains_arguments()
    }

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Class Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>) {
        if let Some(name) = &self.ident {
            name.early_errors(agent, errs, true);
        }
        self.tail.early_errors(agent, errs, true);
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

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.heritage.as_ref().map_or(false, |ch| ch.contains_arguments()) || self.body.as_ref().map_or(false, |cb| cb.contains_arguments())
    }

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Class Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // ClassTail : ClassHeritageopt { ClassBody }
        //  * It is a Syntax Error if ClassHeritage is not present and the following algorithm returns true:
        //
        //      1. Let constructor be ConstructorMethod of ClassBody.
        //      2. If constructor is empty, return false.
        //      3. Return HasDirectSuper of constructor.
        if self.heritage.is_none() {
            if let Some(body) = &self.body {
                if let Some(constructor) = body.constructor_method() {
                    if constructor.has_direct_super() {
                        errs.push(create_syntax_error_object(agent, "Cannot use super in a constructor with no parent class"));
                    }
                }
            }
        }

        if let Some(heritage) = &self.heritage {
            heritage.early_errors(agent, errs, strict);
        }
        if let Some(body) = &self.body {
            body.early_errors(agent, errs, strict);
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

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.0.contains_arguments()
    }

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Class Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        self.0.early_errors(agent, errs, strict);
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

    pub fn private_bound_identifiers(&self) -> Vec<PrivateIdInfo> {
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
        new_names.extend(self.private_bound_identifiers().into_iter().map(|info| info.name));
        self.0.all_private_identifiers_valid(&new_names)
    }

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.0.contains_arguments()
    }

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Class Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // ClassBody : ClassElementList
        //  * It is a Syntax Error if PrototypePropertyNameList of ClassElementList contains more than one occurrence
        //    of "constructor".
        //  * It is a Syntax Error if PrivateBoundIdentifiers of ClassElementList contains any duplicate entries,
        //    unless the name is used once for a getter and once for a setter and in no other entries, and the getter
        //    and setter are either both static or both non-static.
        if self.0.prototype_property_name_list().into_iter().filter(|x| x == &"constructor").count() > 1 {
            errs.push(create_syntax_error_object(agent, "Classes may have only one constructor"));
        }
        #[derive(PartialEq, Copy, Clone)]
        enum HowSeen {
            Completely,   // Any further use triggers "duplicate" error
            Getter,       // Saw as a non-static getter
            Setter,       // Saw as a non-static setter
            StaticGetter, // Saw as a static getter
            StaticSetter, // Saw as a static setter
        }
        let mut private_ids = AHashMap::<JSString, HowSeen>::new();
        for pid in self.0.private_bound_identifiers() {
            match private_ids.get(&pid.name) {
                Some(&HowSeen::Completely) => {
                    errs.push(create_syntax_error_object(agent, format!("{} already defined", pid.name)));
                }
                Some(&HowSeen::Getter) if pid.usage != IdUsage::Setter => {
                    errs.push(create_syntax_error_object(agent, format!("{} was previously defined as a getter method.", pid.name)));
                }
                Some(&HowSeen::Getter) => {
                    private_ids.insert(pid.name, HowSeen::Completely);
                }
                Some(&HowSeen::Setter) if pid.usage != IdUsage::Getter => {
                    errs.push(create_syntax_error_object(agent, format!("{} was previously defined as a setter method.", pid.name)));
                }
                Some(&HowSeen::Setter) => {
                    private_ids.insert(pid.name, HowSeen::Completely);
                }
                Some(&HowSeen::StaticGetter) if pid.usage != IdUsage::StaticSetter => {
                    errs.push(create_syntax_error_object(agent, format!("{} was previously defined as a static getter method.", pid.name)));
                }
                Some(&HowSeen::StaticGetter) => {
                    private_ids.insert(pid.name, HowSeen::Completely);
                }
                Some(&HowSeen::StaticSetter) if pid.usage != IdUsage::StaticGetter => {
                    errs.push(create_syntax_error_object(agent, format!("{} was previously defined as a static setter method.", pid.name)));
                }
                Some(&HowSeen::StaticSetter) => {
                    private_ids.insert(pid.name, HowSeen::Completely);
                }
                None => {
                    private_ids.insert(
                        pid.name,
                        match pid.usage {
                            IdUsage::Getter => HowSeen::Getter,
                            IdUsage::Setter => HowSeen::Setter,
                            IdUsage::StaticGetter => HowSeen::StaticGetter,
                            IdUsage::StaticSetter => HowSeen::StaticSetter,
                            IdUsage::Public => HowSeen::Completely,
                            IdUsage::Static => HowSeen::Completely,
                        },
                    );
                }
            }
        }
        self.0.early_errors(agent, errs, strict);
    }

    pub fn constructor_method(&self) -> Option<&Rc<ClassElement>> {
        self.0.constructor_method()
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IdUsage {
    Public,
    Static,
    Getter,
    Setter,
    StaticGetter,
    StaticSetter,
}
#[derive(Debug, Clone)]
pub struct PrivateIdInfo {
    name: JSString,
    usage: IdUsage,
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

    pub fn private_bound_identifiers(&self) -> Vec<PrivateIdInfo> {
        // Static Semantics: PrivateBoundIdentifiers
        match self {
            ClassElementList::List(lst, elem) => {
                // ClassElementList : ClassElementList ClassElement
                //  1. Let names1 be PrivateBoundIdentifiers of ClassElementList.
                //  2. Let names2 be PrivateBoundIdentifiers of ClassElement.
                //  3. Return the list-concatenation of names1 and names2.
                let mut ids = lst.private_bound_identifiers();
                if let Some(pbi) = elem.private_bound_identifier() {
                    ids.push(pbi);
                }
                ids
            }
            ClassElementList::Item(node) => {
                // ClassElementList : ClassElement
                //  1. Return PrivateBoundIdentifiers of ClassElement.
                node.private_bound_identifier().into_iter().collect::<Vec<_>>()
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

    pub fn constructor_method(&self) -> Option<&Rc<ClassElement>> {
        // Static Semantics: ConstructorMethod
        // The syntax-directed operation ConstructorMethod takes no arguments and returns a ClassElement Parse Node or empty.
        //
        //  ClassElementList : ClassElement
        //      1. If ClassElementKind of ClassElement is ConstructorMethod, return ClassElement.
        //      2. Return empty.
        //  ClassElementList : ClassElementList ClassElement
        //      1. Let head be ConstructorMethod of ClassElementList.
        //      2. If head is not empty, return head.
        //      3. If ClassElementKind of ClassElement is ConstructorMethod, return ClassElement.
        //      4. Return empty.
        fn from_item(item: &Rc<ClassElement>) -> Option<&Rc<ClassElement>> {
            match item.class_element_kind() {
                Some(CEKind::ConstructorMethod) => Some(item),
                _ => None,
            }
        }
        match self {
            ClassElementList::Item(element) => from_item(element),
            ClassElementList::List(lst, element) => lst.constructor_method().or_else(|| from_item(element)),
        }
    }

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
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

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Class Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ClassElementList::Item(ce) => ce.early_errors(agent, errs, strict),
            ClassElementList::List(cel, ce) => {
                cel.early_errors(agent, errs, strict);
                ce.early_errors(agent, errs, strict);
            }
        }
    }

    pub fn prototype_property_name_list(&self) -> Vec<JSString> {
        match self {
            ClassElementList::Item(ce) => {
                if !ce.is_static() {
                    if let Some(pn) = ce.prop_name() {
                        return vec![pn];
                    }
                }
                vec![]
            }
            ClassElementList::List(cel, ce) => {
                let mut list = cel.prototype_property_name_list();
                if !ce.is_static() {
                    if let Some(pn) = ce.prop_name() {
                        list.push(pn);
                    }
                }
                list
            }
        }
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

    pub fn private_bound_identifier(&self) -> Option<PrivateIdInfo> {
        // Static Semantics: PrivateBoundIdentifiers
        match self {
            // ClassElement : ClassStaticBlock
            // ClassElement : ;
            //  1. Return a new empty List.
            ClassElement::Empty | ClassElement::StaticBlock(_) => None,

            // ClassElement : MethodDefinition
            // ClassElement : static MethodDefinition
            //  1. Return PrivateBoundIdentifiers of MethodDefinition.
            ClassElement::Standard(md) => md.private_bound_identifier().map(|(name, mtype)| PrivateIdInfo {
                name,
                usage: match mtype {
                    MethodType::Normal => IdUsage::Public,
                    MethodType::Setter => IdUsage::Setter,
                    MethodType::Getter => IdUsage::Getter,
                },
            }),
            ClassElement::Static(md) => md.private_bound_identifier().map(|(name, mtype)| PrivateIdInfo {
                name,
                usage: match mtype {
                    MethodType::Normal => IdUsage::Static,
                    MethodType::Setter => IdUsage::StaticSetter,
                    MethodType::Getter => IdUsage::StaticGetter,
                },
            }),

            // ClassElement : FieldDefinition
            // ClassElement : static FieldDefinition
            //  1. Return PrivateBoundIdentifiers of FieldDefinition.
            ClassElement::Field(fd) => fd.private_bound_identifier().map(|name| PrivateIdInfo { name, usage: IdUsage::Public }),
            ClassElement::StaticField(fd) => fd.private_bound_identifier().map(|name| PrivateIdInfo { name, usage: IdUsage::Static }),
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

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
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

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Class Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ClassElement::Standard(md) => {
                // ClassElement : MethodDefinition
                //  * It is a Syntax Error if PropName of MethodDefinition is not "constructor" and HasDirectSuper of
                //    MethodDefinition is true.
                //  * It is a Syntax Error if PropName of MethodDefinition is "constructor" and SpecialMethod of MethodDefinition is true.
                match md.prop_name() {
                    Some(s) if s == "constructor" => {
                        if md.special_method() {
                            errs.push(create_syntax_error_object(agent, "special methods not allowed for constructors"));
                        }
                    }
                    Some(_) | None => {
                        if md.has_direct_super() {
                            errs.push(create_syntax_error_object(agent, "super only allowed for constructors"));
                        }
                    }
                }
                md.early_errors(agent, errs, strict);
            }
            ClassElement::Static(md) => {
                // ClassElement : static MethodDefinition
                //  * It is a Syntax Error if HasDirectSuper of MethodDefinition is true.
                //  * It is a Syntax Error if PropName of MethodDefinition is "prototype".
                if md.has_direct_super() {
                    errs.push(create_syntax_error_object(agent, "super only allowed for constructors"));
                }
                if matches!(md.prop_name(), Some(s) if s == "prototype") {
                    errs.push(create_syntax_error_object(agent, "prototypes cannot be static"));
                }
                md.early_errors(agent, errs, strict);
            }
            ClassElement::Field(fd) => {
                // ClassElement : FieldDefinition ;
                //  * It is a Syntax Error if PropName of FieldDefinition is "constructor".
                if matches!(fd.prop_name(), Some(s) if s == "constructor") {
                    errs.push(create_syntax_error_object(agent, "constructors may not be defined as class fields"));
                }
                fd.early_errors(agent, errs, strict);
            }
            ClassElement::StaticField(fd) => {
                // ClassElement : static FieldDefinition ;
                //  * It is a Syntax Error if PropName of FieldDefinition is "prototype" or "constructor".
                let pn = fd.prop_name();
                match pn {
                    Some(s) if s == "prototype" => errs.push(create_syntax_error_object(agent, "prototypes cannot be static")),
                    Some(s) if s == "constructor" => errs.push(create_syntax_error_object(agent, "constructors may not be defined as class fields")),
                    _ => (),
                }
                fd.early_errors(agent, errs, strict);
            }
            ClassElement::StaticBlock(sb) => sb.early_errors(agent, errs, strict),
            ClassElement::Empty => (),
        }
    }

    pub fn class_element_kind(&self) -> Option<CEKind> {
        // Static Semantics: ClassElementKind
        // The syntax-directed operation ClassElementKind takes no arguments and returns ConstructorMethod, NonConstructorMethod, or empty.
        //  ClassElement : MethodDefinition
        //      1. If PropName of MethodDefinition is "constructor", return ConstructorMethod.
        //      2. Return NonConstructorMethod.
        //  ClassElement : static MethodDefinition
        //  ClassElement : FieldDefinition ;
        //  ClassElement : static FieldDefinition ;
        //  ClassElement : ClassStaticBlock
        //      1. Return NonConstructorMethod.
        //  ClassElement : ;
        //      1. Return empty.
        match self {
            ClassElement::Standard(md) if md.prop_name() == Some("constructor".into()) => Some(CEKind::ConstructorMethod),
            ClassElement::Standard(_) | ClassElement::Field(_) | ClassElement::Static(_) | ClassElement::StaticField(_) | ClassElement::StaticBlock(_) => Some(CEKind::NonConstructorMethod),
            ClassElement::Empty => None,
        }
    }

    pub fn prop_name(&self) -> Option<JSString> {
        match self {
            ClassElement::Standard(md) | ClassElement::Static(md) => md.prop_name(),
            ClassElement::Field(fd) | ClassElement::StaticField(fd) => fd.prop_name(),
            ClassElement::StaticBlock(_) | ClassElement::Empty => None,
        }
    }

    pub fn is_static(&self) -> bool {
        matches!(self, ClassElement::Static(..) | ClassElement::StaticField(..) | ClassElement::StaticBlock(..))
    }

    pub fn has_direct_super(&self) -> bool {
        match self {
            ClassElement::Empty | ClassElement::Field(_) | ClassElement::StaticField(_) | ClassElement::StaticBlock(_) => false,
            ClassElement::Standard(md) | ClassElement::Static(md) => md.has_direct_super(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum CEKind {
    ConstructorMethod,
    NonConstructorMethod,
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

    pub fn private_bound_identifier(&self) -> Option<JSString> {
        // Static Semantics: PrivateBoundIdentifiers
        // FieldDefinition : ClassElementName Initializer [opt]
        //  1. Return PrivateBoundIdentifiers of ClassElementName.
        self.name.private_bound_identifier()
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

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.name.contains_arguments() || self.init.as_ref().map_or(false, |izer| izer.contains_arguments())
    }

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Class Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // FieldDefinition :
        //  ClassElementName Initializer[opt]
        //  * It is a Syntax Error if Initializer is present and ContainsArguments of Initializer is true.
        //  * It is a Syntax Error if Initializer is present and Initializer Contains SuperCall is true.
        self.name.early_errors(agent, errs, strict);
        if let Some(izer) = self.init.as_ref() {
            if izer.contains_arguments() {
                errs.push(create_syntax_error_object(agent, "‘arguments’ not expected here"));
            }
            if izer.contains(ParseNodeKind::SuperCall) {
                errs.push(create_syntax_error_object(agent, "Calls to ‘super’ not allowed here"));
            }
            izer.early_errors(agent, errs, strict);
        }
    }

    /// Returns the property name (if it exists) for this node.
    ///
    /// See [PropName](https://tc39.es/ecma262/#sec-static-semantics-propname) in ECMA-262.
    pub fn prop_name(&self) -> Option<JSString> {
        self.name.prop_name()
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

    pub fn private_bound_identifier(&self) -> Option<JSString> {
        // Static Semantics: PrivateBoundIdentifiers
        match self {
            // ClassElementName : PropertyName
            //  1. Return a new empty List.
            ClassElementName::PropertyName(_) => None,

            // ClassElementName : PrivateIdentifier
            //  1. Return a List whose sole element is the StringValue of PrivateIdentifier.
            ClassElementName::PrivateIdentifier(pid) => Some(pid.string_value.clone()),
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

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
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

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Class Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ClassElementName::PrivateIdentifier(pid) => {
                // ClassElementName : PrivateIdentifier
                //  * It is a Syntax Error if StringValue of PrivateIdentifier is "#constructor".
                if pid.string_value == "#constructor" {
                    errs.push(create_syntax_error_object(agent, "#constructor is an invalid private id"));
                }
            }
            ClassElementName::PropertyName(pn) => pn.early_errors(agent, errs, strict),
        }
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

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.0.contains_arguments()
    }

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Class Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        self.0.early_errors(agent, errs, strict);
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

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Class Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        //  ClassStaticBlockBody : ClassStaticBlockStatementList
        //  * It is a Syntax Error if the LexicallyDeclaredNames of ClassStaticBlockStatementList contains any duplicate
        //    entries.
        //  * It is a Syntax Error if any element of the LexicallyDeclaredNames of ClassStaticBlockStatementList also
        //    occurs in the VarDeclaredNames of ClassStaticBlockStatementList.
        //  * It is a Syntax Error if ContainsDuplicateLabels of ClassStaticBlockStatementList with argument « » is
        //    true.
        //  * It is a Syntax Error if ContainsUndefinedBreakTarget of ClassStaticBlockStatementList with argument « » is
        //    true.
        //  * It is a Syntax Error if ContainsUndefinedContinueTarget of ClassStaticBlockStatementList with arguments «
        //    » and « » is true.
        //  * It is a Syntax Error if ContainsArguments of ClassStaticBlockStatementList is true.
        //  * It is a Syntax Error if ClassStaticBlockStatementList Contains SuperCall is true.
        //  * It is a Syntax Error if ClassStaticBlockStatementList Contains await is true.
        let ldn = self.0.lexically_declared_names();
        for name in duplicates(&ldn) {
            errs.push(create_syntax_error_object(agent, format!("‘{}’ already defined", name)));
        }
        let vdn = self.0.var_declared_names();
        for name in ldn.iter().filter(|n| vdn.contains(n)) {
            errs.push(create_syntax_error_object(agent, format!("‘{}’ already defined", name)));
        }
        if self.0.contains_duplicate_labels(&[]) {
            errs.push(create_syntax_error_object(agent, "duplicate labels detected"));
        }
        if self.0.contains_undefined_break_target(&[]) {
            errs.push(create_syntax_error_object(agent, "undefined break target detected"));
        }
        if self.0.contains_undefined_continue_target(&[], &[]) {
            errs.push(create_syntax_error_object(agent, "undefined continue target detected"));
        }
        if self.0.contains_arguments() {
            errs.push(create_syntax_error_object(agent, "‘arguments’ not expected here"))
        }
        if self.0.contains(ParseNodeKind::SuperCall) {
            errs.push(create_syntax_error_object(agent, "Calls to ‘super’ not allowed here"))
        }
        if self.0.contains(ParseNodeKind::AwaitExpression) {
            errs.push(create_syntax_error_object(agent, "await expressions not expected here"))
        }
        self.0.early_errors(agent, errs, strict);
    }

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.0.contains_arguments()
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

    /// Add the early errors of this node and its children to the error list.
    ///
    /// This calculates all the early errors of this parse node, and then follows them up with the early errors of all
    /// the children's nodes, placing them in the `errs` vector as ECMAScript SyntaxError objects. `strict` is used to
    /// indicate whether this node was parsed in strict mode. `agent` is the Evaluation Agent under which the objects
    /// are created.
    ///
    /// See [Early Errors for Class Definitions][1] from ECMA-262.
    ///
    /// [1]: https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool) {
        if let Some(sl) = self.0.as_ref() {
            sl.early_errors(agent, errs, strict, false, false);
        }
    }

    /// Return a list of identifiers defined lexically for this node.
    ///
    /// Note that class static blocks are treated like top-level code in that top-level function identifiers are _not_ included in this list.
    ///
    /// See [LexicallyDeclaredNames](https://tc39.es/ecma262/#sec-static-semantics-lexicallydeclarednames) in ECMA-262.
    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        // Static Semantics: LexicallyDeclaredNames
        // The syntax-directed operation LexicallyDeclaredNames takes no arguments and returns a List of Strings.
        //  ClassStaticBlockStatementList : [empty]
        //      1. Return a new empty List.
        //  ClassStaticBlockStatementList : StatementList
        //      1. Return the TopLevelLexicallyDeclaredNames of StatementList.
        match self.0.as_ref() {
            Some(sl) => sl.top_level_lexically_declared_names(),
            None => vec![],
        }
    }

    /// Return a list of identifiers defined by the `var` statement for this node.
    ///
    /// Note that class static blocks are treated like top-level code in that top-level functions identifiers are part
    /// of the var-declared list.
    ///
    /// See [VarDeclaredNames](https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames) from ECMA-262.
    pub fn var_declared_names(&self) -> Vec<JSString> {
        // Static Semantics: VarDeclaredNames
        // The syntax-directed operation VarDeclaredNames takes no arguments and returns a List of Strings.
        //  ClassStaticBlockStatementList : [empty]
        //      1. Return a new empty List.
        //  ClassStaticBlockStatementList : StatementList
        //      1. Return the TopLevelVarDeclaredNames of StatementList.
        match self.0.as_ref() {
            Some(sl) => sl.top_level_var_declared_names(),
            None => vec![],
        }
    }

    /// Detect whether this node contains any duplicate labels.
    ///
    /// A "duplicate label" occurs when one labelled statement contains another labelled statement and they share the
    /// same label. For the purposes of this function, return `true` if this node:
    /// * Contains a labelled statement whose label is also contained in `label_set`, or
    /// * Contains a labelled statement which itself contains a labelled statement and whose labels match (regardless of
    ///   the parameters to this function).
    ///
    /// See [ContainsDuplicateLabels](https://tc39.es/ecma262/#sec-static-semantics-containsduplicatelabels) from ECMA-262.
    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self.0.as_ref() {
            Some(sl) => sl.contains_duplicate_labels(label_set),
            None => false,
        }
    }

    /// Detect whether this node contains an undefined break target
    ///
    /// * If this node contains a `break` statement with a label contained within `label_set`, then this is not an
    ///   undefined break target.
    /// * If this node contains a labelled breakable statement that contains a break statement with the matching label,
    ///   then this is not an undefined break target.
    ///
    /// Any targeted break statement that does not meet one of the above conditions has an "undefined break target".
    ///
    /// See [ContainsUndefinedBreakTarget](https://tc39.es/ecma262/#sec-static-semantics-containsundefinedbreaktarget)
    /// from ECMA-262.
    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self.0.as_ref() {
            Some(sl) => sl.contains_undefined_break_target(label_set),
            None => false,
        }
    }

    /// Detect whether this node contains an undefined continue target
    ///
    /// * If this node contains a `continue` statement with a label contained within `iteration_set`, then this is not
    ///   an undefined continue target.
    /// * If this node contains an iteration statement that contains a `continue` statement with a label contained
    ///   within `label_set`, then this is not an undefined continue target (the label from the label set applies to the
    ///   iteration statement).
    /// * If this node contains a labelled iteration statement with a matching continue, then that's also not an
    ///   undefined target (independent of the input arguments)
    ///
    /// Any targeted continue statement that does not meet one of the above conditions has an "undefined continue
    /// target".
    ///
    /// See
    /// [ContainsUndefinedContinueTarget](https://tc39.es/ecma262/#sec-static-semantics-containsundefinedcontinuetarget)
    /// from ECMA-262.
    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString], label_set: &[JSString]) -> bool {
        match self.0.as_ref() {
            Some(sl) => sl.contains_undefined_continue_target(iteration_set, label_set),
            None => false,
        }
    }

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.0.as_ref().map_or(false, |sl| sl.contains_arguments())
    }

    /// Return `true` if the current node has, as one if its children (without looking into function bodies) the
    /// production specified by `kind`.
    ///
    /// See [Contains](https://tc39.es/ecma262/#sec-syntax-directed-operations-contains) from ECMA-262.
    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.0.as_ref().map_or(false, |sl| kind == ParseNodeKind::StatementList || sl.contains(kind))
    }
}

#[cfg(test)]
mod tests;
