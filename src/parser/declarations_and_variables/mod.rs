use super::*;
use counter::Counter;
use non_empty_vec::{ne_vec, NonEmpty};
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// LexicalDeclaration[In, Yield, Await] :
//      LetOrConst BindingList[?In, ?Yield, ?Await] ;
#[derive(Debug)]
pub struct LexicalDeclaration {
    pub style: LetOrConst,
    pub list: Rc<BindingList>,
    location: Location,
}

impl fmt::Display for LexicalDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} ;", self.style, self.list)
    }
}

impl PrettyPrint for LexicalDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}LexicalDeclaration: {self}")?;
        self.style.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.list.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}LexicalDeclaration: {self}")?;
        self.style.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.list.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl LexicalDeclaration {
    fn parse_core(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (kwd, kwd_loc, after_tok) =
            scan_for_keywords(scanner, parser.source, ScanGoal::InputElementRegExp, &[Keyword::Let, Keyword::Const])?;
        let style = match kwd {
            Keyword::Let => LetOrConst::Let,
            _ => LetOrConst::Const,
        };
        let (list, after_bl) = BindingList::parse(parser, after_tok, in_flag, yield_flag, await_flag)?;
        let (semi_loc, after_semi) = scan_for_auto_semi(after_bl, parser.source, ScanGoal::InputElementRegExp)?;
        Ok((Rc::new(LexicalDeclaration { style, list, location: kwd_loc.merge(&semi_loc) }), after_semi))
    }

    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
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

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        self.list.bound_names()
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.style.contains(kind) || self.list.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.list.all_private_identifiers_valid(names)
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
        self.list.contains_arguments()
    }

    pub fn is_constant_declaration(&self) -> bool {
        self.style.is_constant_declaration()
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        // Static Semantics: Early Errors
        //  LexicalDeclaration : LetOrConst BindingList ;
        //  * It is a Syntax Error if the BoundNames of BindingList contains "let".
        //  * It is a Syntax Error if the BoundNames of BindingList contains any duplicate entries.
        let bn = self.list.bound_names();

        let let_string = JSString::from("let");
        let counts = bn.into_iter().collect::<Counter<_>>();
        if counts[&let_string] > 0 {
            errs.push(create_syntax_error_object(
                "‘let’ is not a valid binding identifier",
                Some(self.list.location()),
            ));
        }
        let mut dup_ids = counts.into_iter().filter(|&(_, n)| n > 1).map(|(s, _)| String::from(s)).collect::<Vec<_>>();
        if !dup_ids.is_empty() {
            dup_ids.sort_unstable();
            errs.push(create_syntax_error_object(
                format!("Duplicate binding identifiers: ‘{}’", dup_ids.join("’, ‘")),
                Some(self.list.location()),
            ));
        }

        self.list.early_errors(errs, strict, self.is_constant_declaration());
    }
}

// LetOrConst :
//      let
//      const
#[derive(Debug, Copy, Clone)]
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
        writeln!(writer, "{first}LetOrConst: {self}")
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

    pub fn is_constant_declaration(&self) -> bool {
        matches!(self, LetOrConst::Const)
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
            BindingList::List(lst, item) => write!(f, "{lst} , {item}"),
        }
    }
}

impl PrettyPrint for BindingList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}BindingList: {self}")?;
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
                writeln!(writer, "{first}BindingList: {self}")?;
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl BindingList {
    // no cache
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (lb, after_lb) = LexicalBinding::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        let mut current = Rc::new(BindingList::Item(lb));
        let mut current_scanner = after_lb;
        while let Ok((lb2, after_lb2)) =
            scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
                .and_then(|(_, after_tok)| LexicalBinding::parse(parser, after_tok, in_flag, yield_flag, await_flag))
        {
            current = Rc::new(BindingList::List(current, lb2));
            current_scanner = after_lb2;
        }
        Ok((current, current_scanner))
    }

    pub fn location(&self) -> Location {
        match self {
            BindingList::Item(item) => item.location(),
            BindingList::List(list, item) => list.location().merge(&item.location()),
        }
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
            BindingList::List(node1, node2) => {
                node1.all_private_identifiers_valid(names) && node2.all_private_identifiers_valid(names)
            }
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
            BindingList::Item(lb) => lb.contains_arguments(),
            BindingList::List(bl, lb) => bl.contains_arguments() || lb.contains_arguments(),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool, is_constant_declaration: bool) {
        match self {
            BindingList::Item(node) => node.early_errors(errs, strict, is_constant_declaration),
            BindingList::List(lst, tail) => {
                lst.early_errors(errs, strict, is_constant_declaration);
                tail.early_errors(errs, strict, is_constant_declaration);
            }
        }
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
            LexicalBinding::Identifier(bi, Some(i)) => write!(f, "{bi} {i}"),
            LexicalBinding::Identifier(bi, None) => bi.fmt(f),
            LexicalBinding::Pattern(bp, i) => write!(f, "{bp} {i}"),
        }
    }
}

impl PrettyPrint for LexicalBinding {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}LexicalBinding: {self}")?;
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
            writeln!(writer, "{first}LexicalBinding: {self}").and(Ok(successive))
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
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
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

    pub fn location(&self) -> Location {
        match self {
            LexicalBinding::Identifier(id, None) => id.location(),
            LexicalBinding::Identifier(id, Some(izer)) => id.location().merge(&izer.location()),
            LexicalBinding::Pattern(pat, izer) => pat.location().merge(&izer.location()),
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            LexicalBinding::Identifier(bi, _) => bi.bound_names(),
            LexicalBinding::Pattern(bp, _) => bp.bound_names(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            LexicalBinding::Identifier(bi, opt) => {
                bi.contains(kind) || opt.as_ref().is_some_and(|n| n.contains(kind))
            }
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
            LexicalBinding::Pattern(node1, node2) => {
                node1.all_private_identifiers_valid(names) && node2.all_private_identifiers_valid(names)
            }
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
            LexicalBinding::Identifier(_, Some(izer)) => izer.contains_arguments(),
            LexicalBinding::Identifier(_, None) => false,
            LexicalBinding::Pattern(bp, izer) => bp.contains_arguments() || izer.contains_arguments(),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool, is_constant_declaration: bool) {
        // Static Semantics: Early Errors
        //  LexicalBinding : BindingIdentifier Initializer[opt]
        //  * It is a Syntax Error if Initializer is not present and IsConstantDeclaration of the LexicalDeclaration containing this LexicalBinding is true.
        match self {
            LexicalBinding::Identifier(idref, Some(node)) => {
                idref.early_errors(errs, strict);
                node.early_errors(errs, strict);
            }
            LexicalBinding::Identifier(idref, None) => {
                if is_constant_declaration {
                    errs.push(create_syntax_error_object(
                        "Missing initializer in const declaration",
                        Some(idref.location()),
                    ));
                }
                idref.early_errors(errs, strict);
            }
            LexicalBinding::Pattern(pat, izer) => {
                pat.early_errors(errs, strict);
                izer.early_errors(errs, strict);
            }
        }
    }
}

// VariableStatement[Yield, Await] :
//      var VariableDeclarationList[+In, ?Yield, ?Await] ;
#[derive(Debug)]
pub struct VariableStatement {
    pub list: Rc<VariableDeclarationList>,
    location: Location,
}

impl fmt::Display for VariableStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "var {} ;", self.list)
    }
}

impl PrettyPrint for VariableStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}VariableStatement: {self}")?;
        self.list.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}VariableStatement: {self}")?;
        pprint_token(writer, "var", TokenType::Keyword, &successive, Spot::NotFinal)?;
        self.list.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ";", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl VariableStatement {
    // no cache
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (var_loc, after_var) =
            scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Var)?;
        let (list, after_vdl) = VariableDeclarationList::parse(parser, after_var, true, yield_flag, await_flag)?;
        let (semi_loc, after_semi) = scan_for_auto_semi(after_vdl, parser.source, ScanGoal::InputElementRegExp)?;
        Ok((Rc::new(VariableStatement { list, location: var_loc.merge(&semi_loc) }), after_semi))
    }

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        self.list.bound_names()
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.list.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.list.all_private_identifiers_valid(names)
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
        self.list.contains_arguments()
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        self.list.early_errors(errs, strict);
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        self.list.var_scoped_declarations()
    }
}

// VariableDeclarationList[In, Yield, Await] :
//      VariableDeclaration[?In, ?Yield, ?Await]
//      VariableDeclarationList[?In, ?Yield, ?Await] , VariableDeclaration[?In, ?Yield, ?Await]
#[derive(Debug)]
pub struct VariableDeclarationList {
    pub list: NonEmpty<Rc<VariableDeclaration>>,
}

impl fmt::Display for VariableDeclarationList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.list[0].fmt(f)?;
        for item in &self.list[1..] {
            write!(f, " , {item}")?;
        }
        Ok(())
    }
}

impl PrettyPrint for VariableDeclarationList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}VariableDeclarationList: {self}")?;
        let last_item_index = usize::from(self.list.len()) - 1;
        for item in &self.list[0..last_item_index] {
            item.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        self.list[last_item_index].pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match usize::from(self.list.len()) {
            1 => self.list[0].concise_with_leftpad(writer, pad, state),
            _ => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{first}VariableDeclarationList: {self}")?;
                for item in &self.list[0..usize::from(self.list.len()) - 1] {
                    item.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                    pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                }
                self.list[usize::from(self.list.len()) - 1].concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl VariableDeclarationList {
    fn parse_core(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let (decl, after_dcl) = VariableDeclaration::parse(parser, scanner, in_flag, yield_flag, await_flag)?;
        let mut items = ne_vec![decl];
        let mut current_scanner = after_dcl;
        while let Ok((next, after_next)) =
            scan_for_punct(current_scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma).and_then(
                |(_, after_comma)| VariableDeclaration::parse(parser, after_comma, in_flag, yield_flag, await_flag),
            )
        {
            items.push(next);
            current_scanner = after_next;
        }
        Ok((Rc::new(VariableDeclarationList { list: items }), current_scanner))
    }

    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
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
        self.list.iter().flat_map(|item| item.bound_names()).collect()
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.list.iter().any(|item| item.contains(kind))
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.list.iter().all(|item| item.all_private_identifiers_valid(names))
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
        self.list.iter().any(|item| item.contains_arguments())
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        for item in &self.list {
            item.early_errors(errs, strict);
        }
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        self.list.iter().map(|item| VarScopeDecl::VariableDeclaration(item.clone())).collect()
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
            VariableDeclaration::Identifier(bi, Some(i)) => write!(f, "{bi} {i}"),
            VariableDeclaration::Identifier(bi, None) => bi.fmt(f),
            VariableDeclaration::Pattern(bp, i) => write!(f, "{bp} {i}"),
        }
    }
}

impl PrettyPrint for VariableDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}VariableDeclaration: {self}")?;
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
            writeln!(writer, "{first}VariableDeclaration: {self}").and(Ok(successive))
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
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        in_flag: bool,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
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
            VariableDeclaration::Pattern(node1, node2) => {
                node1.all_private_identifiers_valid(names) && node2.all_private_identifiers_valid(names)
            }
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
            VariableDeclaration::Identifier(_, None) => false,
            VariableDeclaration::Identifier(_, Some(izer)) => izer.contains_arguments(),
            VariableDeclaration::Pattern(bp, izer) => bp.contains_arguments() || izer.contains_arguments(),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            VariableDeclaration::Identifier(bi, Some(i)) => {
                bi.early_errors(errs, strict);
                i.early_errors(errs, strict);
            }
            VariableDeclaration::Identifier(bi, None) => bi.early_errors(errs, strict),
            VariableDeclaration::Pattern(bp, i) => {
                bp.early_errors(errs, strict);
                i.early_errors(errs, strict);
            }
        }
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
        writeln!(writer, "{first}BindingPattern: {self}")?;
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
            .otherwise(|| {
                ObjectBindingPattern::parse(parser, scanner, yield_flag, await_flag)
                    .map(|(obp, after_obp)| (Rc::new(BindingPattern::Object(obp)), after_obp))
            })
            .otherwise(|| {
                ArrayBindingPattern::parse(parser, scanner, yield_flag, await_flag)
                    .map(|(abp, after_abp)| (Rc::new(BindingPattern::Array(abp)), after_abp))
            })
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

    pub fn location(&self) -> Location {
        match self {
            BindingPattern::Object(obp) => obp.location(),
            BindingPattern::Array(abp) => abp.location(),
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
            BindingPattern::Object(obp) => obp.contains_arguments(),
            BindingPattern::Array(abp) => abp.contains_arguments(),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            BindingPattern::Object(node) => node.early_errors(errs, strict),
            BindingPattern::Array(node) => node.early_errors(errs, strict),
        }
    }

    /// Report whether this portion of a parameter list contains an expression
    ///
    /// See [ContainsExpression](https://tc39.es/ecma262/#sec-static-semantics-containsexpression) in ECMA-262.
    pub fn contains_expression(&self) -> bool {
        match self {
            BindingPattern::Object(o) => o.contains_expression(),
            BindingPattern::Array(a) => a.contains_expression(),
        }
    }
}

// ObjectBindingPattern[Yield, Await] :
//      { }
//      { BindingRestProperty[?Yield, ?Await] }
//      { BindingPropertyList[?Yield, ?Await] }
//      { BindingPropertyList[?Yield, ?Await] , BindingRestProperty[?Yield, ?Await]opt }
#[derive(Debug)]
pub enum ObjectBindingPattern {
    Empty { location: Location },
    RestOnly { brp: Rc<BindingRestProperty>, location: Location },
    ListOnly { bpl: Rc<BindingPropertyList>, location: Location },
    ListRest { bpl: Rc<BindingPropertyList>, brp: Option<Rc<BindingRestProperty>>, location: Location },
}

impl fmt::Display for ObjectBindingPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ObjectBindingPattern::Empty { .. } => write!(f, "{{ }}"),
            ObjectBindingPattern::RestOnly { brp, .. } => write!(f, "{{ {brp} }}"),
            ObjectBindingPattern::ListOnly { bpl, .. } => write!(f, "{{ {bpl} }}"),
            ObjectBindingPattern::ListRest { bpl, brp: Some(rst), .. } => {
                write!(f, "{{ {bpl} , {rst} }}")
            }
            ObjectBindingPattern::ListRest { bpl, brp: None, .. } => write!(f, "{{ {bpl} , }}"),
        }
    }
}

impl PrettyPrint for ObjectBindingPattern {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ObjectBindingPattern: {self}")?;
        match self {
            ObjectBindingPattern::Empty { .. } => Ok(()),
            ObjectBindingPattern::RestOnly { brp, .. } => brp.pprint_with_leftpad(writer, &successive, Spot::Final),
            ObjectBindingPattern::ListOnly { bpl, .. } => bpl.pprint_with_leftpad(writer, &successive, Spot::Final),
            ObjectBindingPattern::ListRest { bpl, brp: Some(rst), .. } => {
                bpl.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                rst.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ObjectBindingPattern::ListRest { bpl, brp: None, .. } => {
                bpl.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ObjectBindingPattern: {self}")?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match self {
            ObjectBindingPattern::Empty { .. } => {}
            ObjectBindingPattern::RestOnly { brp, .. } => {
                brp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ObjectBindingPattern::ListOnly { bpl, .. } => {
                bpl.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ObjectBindingPattern::ListRest { bpl, brp: Some(rst), .. } => {
                bpl.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                rst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ObjectBindingPattern::ListRest { bpl, brp: None, .. } => {
                bpl.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
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
            scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBrace).and_then(
                |(open_loc, after_open)| {
                    scan_for_punct(after_open, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBrace)
                        .map(|(close_loc, after_close)| {
                            (Rc::new(ObjectBindingPattern::Empty { location: open_loc.merge(&close_loc) }), after_close)
                        })
                        .otherwise(|| {
                            BindingRestProperty::parse(parser, after_open, yield_flag, await_flag).and_then(
                                |(brp, after_brp)| {
                                    scan_for_punct(
                                        after_brp,
                                        parser.source,
                                        ScanGoal::InputElementRegExp,
                                        Punctuator::RightBrace,
                                    )
                                    .map(|(close_loc, after_close)| {
                                        (
                                            Rc::new(ObjectBindingPattern::RestOnly {
                                                brp,
                                                location: open_loc.merge(&close_loc),
                                            }),
                                            after_close,
                                        )
                                    })
                                },
                            )
                        })
                        .otherwise(|| {
                            BindingPropertyList::parse(parser, after_open, yield_flag, await_flag).and_then(
                                |(bpl, after_bpl)| match scan_for_punct(
                                    after_bpl,
                                    parser.source,
                                    ScanGoal::InputElementRegExp,
                                    Punctuator::RightBrace,
                                )
                                .map(|(close_loc, after_close)| (None, close_loc, after_close))
                                .otherwise(|| {
                                    scan_for_punct(
                                        after_bpl,
                                        parser.source,
                                        ScanGoal::InputElementRegExp,
                                        Punctuator::Comma,
                                    )
                                    .and_then(|(_, after_comma)| {
                                        let (brp, after_brp) = match BindingRestProperty::parse(
                                            parser,
                                            after_comma,
                                            yield_flag,
                                            await_flag,
                                        ) {
                                            Err(_) => (None, after_comma),
                                            Ok((node, s)) => (Some(node), s),
                                        };
                                        scan_for_punct(
                                            after_brp,
                                            parser.source,
                                            ScanGoal::InputElementRegExp,
                                            Punctuator::RightBrace,
                                        )
                                        .map(|(final_loc, after_final)| (Some(brp), final_loc, after_final))
                                    })
                                }) {
                                    Ok((None, final_loc, after)) => Ok((
                                        Rc::new(ObjectBindingPattern::ListOnly {
                                            bpl,
                                            location: open_loc.merge(&final_loc),
                                        }),
                                        after,
                                    )),
                                    Ok((Some(brp), final_loc, after)) => Ok((
                                        Rc::new(ObjectBindingPattern::ListRest {
                                            bpl,
                                            brp,
                                            location: open_loc.merge(&final_loc),
                                        }),
                                        after,
                                    )),
                                    Err(e) => Err(e),
                                },
                            )
                        })
                },
            )
        })
    }

    pub fn location(&self) -> Location {
        match self {
            ObjectBindingPattern::Empty { location }
            | ObjectBindingPattern::RestOnly { location, .. }
            | ObjectBindingPattern::ListOnly { location, .. }
            | ObjectBindingPattern::ListRest { location, .. } => *location,
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            ObjectBindingPattern::Empty { .. } => vec![],
            ObjectBindingPattern::RestOnly { brp, .. } => brp.bound_names(),
            ObjectBindingPattern::ListOnly { bpl, .. } | ObjectBindingPattern::ListRest { bpl, brp: None, .. } => {
                bpl.bound_names()
            }
            ObjectBindingPattern::ListRest { bpl, brp: Some(rst), .. } => {
                let mut names = bpl.bound_names();
                names.extend(rst.bound_names());
                names
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ObjectBindingPattern::Empty { .. } => false,
            ObjectBindingPattern::RestOnly { brp, .. } => brp.contains(kind),
            ObjectBindingPattern::ListOnly { bpl, .. } | ObjectBindingPattern::ListRest { bpl, brp: None, .. } => {
                bpl.contains(kind)
            }
            ObjectBindingPattern::ListRest { bpl, brp: Some(n), .. } => bpl.contains(kind) || n.contains(kind),
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
            ObjectBindingPattern::Empty { .. } | ObjectBindingPattern::RestOnly { .. } => true,
            ObjectBindingPattern::ListOnly { bpl, .. } | ObjectBindingPattern::ListRest { bpl, .. } => {
                bpl.all_private_identifiers_valid(names)
            }
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
            ObjectBindingPattern::Empty { .. } | ObjectBindingPattern::RestOnly { .. } => false,
            ObjectBindingPattern::ListOnly { bpl, .. } | ObjectBindingPattern::ListRest { bpl, .. } => {
                bpl.contains_arguments()
            }
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ObjectBindingPattern::Empty { .. } => (),
            ObjectBindingPattern::RestOnly { brp, .. } => brp.early_errors(errs, strict),
            ObjectBindingPattern::ListOnly { bpl, .. } | ObjectBindingPattern::ListRest { bpl, brp: None, .. } => {
                bpl.early_errors(errs, strict);
            }
            ObjectBindingPattern::ListRest { bpl, brp: Some(rst), .. } => {
                bpl.early_errors(errs, strict);
                rst.early_errors(errs, strict);
            }
        }
    }

    /// Report whether this portion of a parameter list contains an expression
    ///
    /// See [ContainsExpression](https://tc39.es/ecma262/#sec-static-semantics-containsexpression) in ECMA-262.
    pub fn contains_expression(&self) -> bool {
        match self {
            ObjectBindingPattern::Empty { .. } | ObjectBindingPattern::RestOnly { .. } => false,
            ObjectBindingPattern::ListOnly { bpl, .. } | ObjectBindingPattern::ListRest { bpl, .. } => {
                bpl.contains_expression()
            }
        }
    }
}

// ArrayBindingPattern[Yield, Await] :
//      [ Elisionopt BindingRestElement[?Yield, ?Await]opt ]
//      [ BindingElementList[?Yield, ?Await] ]
//      [ BindingElementList[?Yield, ?Await] , Elisionopt BindingRestElement[?Yield, ?Await]opt ]
#[derive(Debug)]
pub enum ArrayBindingPattern {
    RestOnly {
        elision: Option<Rc<Elisions>>,
        bre: Option<Rc<BindingRestElement>>,
        location: Location,
    },
    ListOnly {
        bel: Rc<BindingElementList>,
        location: Location,
    },
    ListRest {
        bel: Rc<BindingElementList>,
        elision: Option<Rc<Elisions>>,
        bre: Option<Rc<BindingRestElement>>,
        location: Location,
    },
}

impl fmt::Display for ArrayBindingPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArrayBindingPattern::RestOnly { elision: Some(elisions), bre: Some(node), .. } => {
                write!(f, "[ {elisions} {node} ]")
            }
            ArrayBindingPattern::RestOnly { elision: Some(elisions), bre: None, .. } => {
                write!(f, "[ {elisions} ]")
            }
            ArrayBindingPattern::RestOnly { elision: None, bre: Some(node), .. } => {
                write!(f, "[ {node} ]")
            }
            ArrayBindingPattern::RestOnly { elision: None, bre: None, .. } => write!(f, "[ ]"),
            ArrayBindingPattern::ListOnly { bel: node, .. } => write!(f, "[ {node} ]"),
            ArrayBindingPattern::ListRest { bel: lst, elision: Some(elisions), bre: Some(rst), .. } => {
                write!(f, "[ {lst} , {elisions} {rst} ]")
            }
            ArrayBindingPattern::ListRest { bel: lst, elision: None, bre: Some(rst), .. } => {
                write!(f, "[ {lst} , {rst} ]")
            }
            ArrayBindingPattern::ListRest { bel: lst, elision: Some(elisions), bre: None, .. } => {
                write!(f, "[ {lst} , {elisions} ]")
            }
            ArrayBindingPattern::ListRest { bel: lst, elision: None, bre: None, .. } => {
                write!(f, "[ {lst} , ]")
            }
        }
    }
}

impl PrettyPrint for ArrayBindingPattern {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ArrayBindingPattern: {self}")?;
        match self {
            ArrayBindingPattern::RestOnly { elision: Some(elisions), bre: Some(node), .. } => {
                elisions.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                node.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayBindingPattern::RestOnly { elision: Some(elisions), bre: None, .. } => {
                elisions.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayBindingPattern::RestOnly { elision: None, bre: Some(node), .. } => {
                node.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayBindingPattern::RestOnly { elision: None, bre: None, .. } => Ok(()),
            ArrayBindingPattern::ListOnly { bel: node, .. } => {
                node.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayBindingPattern::ListRest { bel: lst, elision: Some(elisions), bre: Some(rst), .. } => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                elisions.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                rst.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayBindingPattern::ListRest { bel: lst, elision: None, bre: Some(rst), .. } => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                rst.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayBindingPattern::ListRest { bel: lst, elision: Some(elisions), bre: None, .. } => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                elisions.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            ArrayBindingPattern::ListRest { bel: lst, elision: None, bre: None, .. } => {
                lst.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ArrayBindingPattern: {self}")?;
        pprint_token(writer, "[", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match self {
            ArrayBindingPattern::RestOnly { elision: Some(elisions), bre: Some(node), .. } => {
                elisions.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ArrayBindingPattern::RestOnly { elision: Some(elisions), bre: None, .. } => {
                elisions.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ArrayBindingPattern::RestOnly { elision: None, bre: Some(node), .. } => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ArrayBindingPattern::RestOnly { elision: None, bre: None, .. } => {}
            ArrayBindingPattern::ListOnly { bel: node, .. } => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ArrayBindingPattern::ListRest { bel: lst, elision: Some(elisions), bre: Some(rst), .. } => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                elisions.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                rst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ArrayBindingPattern::ListRest { bel: lst, elision: None, bre: Some(rst), .. } => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                rst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ArrayBindingPattern::ListRest { bel: lst, elision: Some(elisions), bre: None, .. } => {
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, ",", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                elisions.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            ArrayBindingPattern::ListRest { bel: lst, elision: None, bre: None, .. } => {
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
        let (first_loc, after_first) =
            scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBracket)?;
        BindingElementList::parse(parser, after_first, yield_flag, await_flag)
            .and_then(|(bel, after_bel)| {
                scan_for_punct_set(
                    after_bel,
                    parser.source,
                    ScanGoal::InputElementRegExp,
                    &[Punctuator::RightBracket, Punctuator::Comma],
                )
                .and_then(|(punct_next, next_loc, after_next)| match punct_next {
                    Punctuator::RightBracket => Ok((
                        Rc::new(ArrayBindingPattern::ListOnly { bel, location: first_loc.merge(&next_loc) }),
                        after_next,
                    )),
                    _ => {
                        let (elision, after_elisions) = match Elisions::parse(parser, after_next) {
                            Err(_) => (None, after_next),
                            Ok((e, s)) => (Some(e), s),
                        };
                        let (bre, after_bre, err_bre) =
                            match BindingRestElement::parse(parser, after_elisions, yield_flag, await_flag) {
                                Err(err) => (None, after_elisions, Some(err)),
                                Ok((b, s)) => (Some(b), s, None),
                            };
                        match scan_for_punct(
                            after_bre,
                            parser.source,
                            ScanGoal::InputElementRegExp,
                            Punctuator::RightBracket,
                        ) {
                            Ok((close_loc, after_close)) => Ok((
                                Rc::new(ArrayBindingPattern::ListRest {
                                    bel,
                                    elision,
                                    bre,
                                    location: first_loc.merge(&close_loc),
                                }),
                                after_close,
                            )),
                            Err(pe) => {
                                let mut err = Some(pe);
                                if ParseError::compare_option(err_bre.as_ref(), err.as_ref()) == Ordering::Greater {
                                    err = err_bre;
                                }
                                Err(err.unwrap())
                            }
                        }
                    }
                })
            })
            .otherwise(|| {
                let (elision, after_elisions) = match Elisions::parse(parser, after_first) {
                    Err(_) => (None, after_first),
                    Ok((e, s)) => (Some(e), s),
                };
                let (bre, after_bre, err_bre) =
                    match BindingRestElement::parse(parser, after_elisions, yield_flag, await_flag) {
                        Err(err) => (None, after_elisions, Some(err)),
                        Ok((b, s)) => (Some(b), s, None),
                    };
                match scan_for_punct(after_bre, parser.source, ScanGoal::InputElementRegExp, Punctuator::RightBracket) {
                    Ok((close_loc, after_close)) => Ok((
                        Rc::new(ArrayBindingPattern::RestOnly { elision, bre, location: first_loc.merge(&close_loc) }),
                        after_close,
                    )),
                    Err(pe) => {
                        let mut err = Some(pe);
                        if ParseError::compare_option(err_bre.as_ref(), err.as_ref()) == Ordering::Greater {
                            err = err_bre;
                        }
                        Err(err.unwrap())
                    }
                }
            })
    }

    pub fn location(&self) -> Location {
        match self {
            ArrayBindingPattern::RestOnly { location, .. }
            | ArrayBindingPattern::ListOnly { location, .. }
            | ArrayBindingPattern::ListRest { location, .. } => *location,
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            ArrayBindingPattern::RestOnly { bre: Some(node), .. } => node.bound_names(),
            ArrayBindingPattern::RestOnly { bre: None, .. } => vec![],
            ArrayBindingPattern::ListOnly { bel: node, .. } => node.bound_names(),
            ArrayBindingPattern::ListRest { bel: lst, bre: Some(rst), .. } => {
                let mut names = lst.bound_names();
                names.extend(rst.bound_names());
                names
            }
            ArrayBindingPattern::ListRest { bel: lst, bre: None, .. } => lst.bound_names(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            ArrayBindingPattern::RestOnly { elision: onode_a, bre: onode_b, .. } => {
                onode_a.as_ref().is_some_and(|node| node.contains(kind))
                    || onode_b.as_ref().is_some_and(|node| node.contains(kind))
            }
            ArrayBindingPattern::ListOnly { bel: node, .. } => node.contains(kind),
            ArrayBindingPattern::ListRest { bel: node, elision: onode_a, bre: onode_b, .. } => {
                node.contains(kind)
                    || onode_a.as_ref().is_some_and(|node| node.contains(kind))
                    || onode_b.as_ref().is_some_and(|node| node.contains(kind))
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
            ArrayBindingPattern::RestOnly { bre: onode, .. } => {
                onode.as_ref().map_or(true, |node| node.all_private_identifiers_valid(names))
            }
            ArrayBindingPattern::ListOnly { bel: node, .. } => node.all_private_identifiers_valid(names),
            ArrayBindingPattern::ListRest { bel: node, bre: onode, .. } => {
                node.all_private_identifiers_valid(names)
                    && onode.as_ref().map_or(true, |node| node.all_private_identifiers_valid(names))
            }
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
            ArrayBindingPattern::RestOnly { bre: obre, .. } => {
                obre.as_ref().is_some_and(|bre| bre.contains_arguments())
            }
            ArrayBindingPattern::ListOnly { bel, .. } => bel.contains_arguments(),
            ArrayBindingPattern::ListRest { bel, bre: obre, .. } => {
                bel.contains_arguments() || obre.as_ref().is_some_and(|bre| bre.contains_arguments())
            }
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            ArrayBindingPattern::RestOnly { bre: Some(node), .. } => node.early_errors(errs, strict),
            ArrayBindingPattern::RestOnly { bre: None, .. } => (),
            ArrayBindingPattern::ListRest { bel, bre: None, .. } | ArrayBindingPattern::ListOnly { bel, .. } => {
                bel.early_errors(errs, strict);
            }
            ArrayBindingPattern::ListRest { bel: lst, bre: Some(rst), .. } => {
                lst.early_errors(errs, strict);
                rst.early_errors(errs, strict);
            }
        }
    }

    /// Report whether this portion of a parameter list contains an expression
    ///
    /// See [ContainsExpression](https://tc39.es/ecma262/#sec-static-semantics-containsexpression) in ECMA-262.
    pub fn contains_expression(&self) -> bool {
        match self {
            ArrayBindingPattern::RestOnly { bre: None, .. } => false,
            ArrayBindingPattern::RestOnly { bre: Some(bre), .. } => bre.contains_expression(),
            ArrayBindingPattern::ListRest { bel, bre: None, .. } | ArrayBindingPattern::ListOnly { bel, .. } => {
                bel.contains_expression()
            }
            ArrayBindingPattern::ListRest { bel, bre: Some(bre), .. } => {
                bel.contains_expression() || bre.contains_expression()
            }
        }
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
        write!(f, "... {node}")
    }
}

impl PrettyPrint for BindingRestProperty {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}BindingRestProperty: {self}")?;
        let BindingRestProperty::Id(node) = self;
        node.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}BindingRestProperty: {self}")?;
        pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        let BindingRestProperty::Id(node) = self;
        node.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl BindingRestProperty {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Ellipsis)
            .and_then(|(_, after_dots)| BindingIdentifier::parse(parser, after_dots, yield_flag, await_flag))
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

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        let BindingRestProperty::Id(node) = self;
        node.early_errors(errs, strict);
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
            BindingPropertyList::List(lst, item) => write!(f, "{lst} , {item}"),
        }
    }
}

impl PrettyPrint for BindingPropertyList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}BindingPropertyList: {self}")?;
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
                writeln!(writer, "{first}BindingPropertyList: {self}")?;
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
        while let Ok((bp2, after_bp2)) =
            scan_for_punct(current_scan, parser.source, ScanGoal::InputElementDiv, Punctuator::Comma)
                .and_then(|(_, after_token)| BindingProperty::parse(parser, after_token, yield_flag, await_flag))
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
            BindingPropertyList::List(lst, item) => {
                lst.all_private_identifiers_valid(names) && item.all_private_identifiers_valid(names)
            }
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
            BindingPropertyList::Item(bp) => bp.contains_arguments(),
            BindingPropertyList::List(bpl, bp) => bpl.contains_arguments() || bp.contains_arguments(),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            BindingPropertyList::Item(node) => node.early_errors(errs, strict),
            BindingPropertyList::List(lst, item) => {
                lst.early_errors(errs, strict);
                item.early_errors(errs, strict);
            }
        }
    }

    /// Report whether this portion of a parameter list contains an expression
    ///
    /// See [ContainsExpression](https://tc39.es/ecma262/#sec-static-semantics-containsexpression) in ECMA-262.
    pub fn contains_expression(&self) -> bool {
        match self {
            BindingPropertyList::Item(node) => node.contains_expression(),
            BindingPropertyList::List(lst, item) => lst.contains_expression() || item.contains_expression(),
        }
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
            BindingElementList::List(lst, item) => write!(f, "{lst} , {item}"),
        }
    }
}

impl PrettyPrint for BindingElementList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}BindingElementList: {self}")?;
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
                writeln!(writer, "{first}BindingElementList: {self}")?;
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
                .and_then(|(_, after_tok)| BindingElisionElement::parse(parser, after_tok, yield_flag, await_flag))
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
            BindingElementList::List(l, i) => {
                l.all_private_identifiers_valid(names) && i.all_private_identifiers_valid(names)
            }
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
            BindingElementList::Item(bee) => bee.contains_arguments(),
            BindingElementList::List(bel, bee) => bel.contains_arguments() || bee.contains_arguments(),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            BindingElementList::Item(node) => node.early_errors(errs, strict),
            BindingElementList::List(lst, item) => {
                lst.early_errors(errs, strict);
                item.early_errors(errs, strict);
            }
        }
    }

    /// Report whether this portion of a parameter list contains an expression
    ///
    /// See [ContainsExpression](https://tc39.es/ecma262/#sec-static-semantics-containsexpression) in ECMA-262.
    pub fn contains_expression(&self) -> bool {
        match self {
            BindingElementList::Item(node) => node.contains_expression(),
            BindingElementList::List(lst, item) => lst.contains_expression() || item.contains_expression(),
        }
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
            BindingElisionElement::Element(Some(elision), elem) => {
                write!(f, "{elision} {elem}")
            }
        }
    }
}

impl PrettyPrint for BindingElisionElement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}BindingElisionElement: {self}")?;
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
                writeln!(writer, "{first}BindingElisionElement: {self}")?;
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
        opt.as_ref().is_some_and(|n| n.contains(kind)) || n.contains(kind)
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
        let BindingElisionElement::Element(_, be) = self;
        be.contains_arguments()
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        let BindingElisionElement::Element(_, elem) = self;
        elem.early_errors(errs, strict);
    }

    /// Report whether this portion of a parameter list contains an expression
    ///
    /// See [ContainsExpression](https://tc39.es/ecma262/#sec-static-semantics-containsexpression) in ECMA-262.
    pub fn contains_expression(&self) -> bool {
        let BindingElisionElement::Element(_, elem) = self;
        elem.contains_expression()
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
            BindingProperty::Property(name, elem) => write!(f, "{name} : {elem}"),
        }
    }
}

impl PrettyPrint for BindingProperty {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}BindingProperty: {self}")?;
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
                writeln!(writer, "{first}BindingProperty: {self}")?;
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
                let (_, after_token) =
                    scan_for_punct(after_pn, parser.source, ScanGoal::InputElementDiv, Punctuator::Colon)?;
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
            BindingProperty::Property(node1, node2) => {
                node1.all_private_identifiers_valid(names) && node2.all_private_identifiers_valid(names)
            }
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
            BindingProperty::Single(snb) => snb.contains_arguments(),
            BindingProperty::Property(pn, be) => pn.contains_arguments() || be.contains_arguments(),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            BindingProperty::Single(node) => node.early_errors(errs, strict),
            BindingProperty::Property(name, elem) => {
                name.early_errors(errs, strict);
                elem.early_errors(errs, strict);
            }
        }
    }

    /// Report whether this portion of a parameter list contains an expression
    ///
    /// See [ContainsExpression](https://tc39.es/ecma262/#sec-static-semantics-containsexpression) in ECMA-262.
    pub fn contains_expression(&self) -> bool {
        match self {
            BindingProperty::Single(single) => single.contains_expression(),
            BindingProperty::Property(name, elem) => name.is_computed_property_key() || elem.contains_expression(),
        }
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
            BindingElement::Pattern(node, Some(init)) => {
                write!(f, "{node} {init}")
            }
        }
    }
}

impl PrettyPrint for BindingElement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}BindingElement: {self}")?;
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
                writeln!(writer, "{first}BindingElement: {self}")?;
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

    pub fn location(&self) -> Location {
        match self {
            BindingElement::Single(single) => single.location(),
            BindingElement::Pattern(pat, Some(izer)) => pat.location().merge(&izer.location()),
            BindingElement::Pattern(pat, None) => pat.location(),
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
            BindingElement::Pattern(n, opt) => n.contains(kind) || opt.as_ref().is_some_and(|n| n.contains(kind)),
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
            BindingElement::Pattern(n, opt) => {
                n.all_private_identifiers_valid(names)
                    && opt.as_ref().map_or(true, |n| n.all_private_identifiers_valid(names))
            }
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
            BindingElement::Single(snb) => snb.contains_arguments(),
            BindingElement::Pattern(bp, oizer) => {
                bp.contains_arguments() || oizer.as_ref().is_some_and(|izer| izer.contains_arguments())
            }
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

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            BindingElement::Single(node) => node.early_errors(errs, strict),
            BindingElement::Pattern(node, None) => node.early_errors(errs, strict),
            BindingElement::Pattern(node, Some(init)) => {
                node.early_errors(errs, strict);
                init.early_errors(errs, strict);
            }
        }
    }

    /// Report whether this binding id contains an intializer
    ///
    /// See [HasInitializer](https://tc39.es/ecma262/#sec-static-semantics-hasinitializer) from ECMA-262.
    pub fn has_initializer(&self) -> bool {
        match self {
            BindingElement::Single(sing) => sing.has_initializer(),
            BindingElement::Pattern(_, izer) => izer.is_some(),
        }
    }

    /// Report whether this portion of a parameter list contains an expression
    ///
    /// See [ContainsExpression](https://tc39.es/ecma262/#sec-static-semantics-containsexpression) in ECMA-262.
    pub fn contains_expression(&self) -> bool {
        match self {
            BindingElement::Single(sing) => sing.contains_expression(),
            BindingElement::Pattern(_, Some(_)) => true,
            BindingElement::Pattern(pat, None) => pat.contains_expression(),
        }
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
            SingleNameBinding::Id(id, Some(init)) => write!(f, "{id} {init}"),
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
        writeln!(writer, "{first}SingleNameBinding: {self}")?;
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
                writeln!(writer, "{first}SingleNameBinding: {self}")?;
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

    pub fn location(&self) -> Location {
        match self {
            SingleNameBinding::Id(ident, None) => ident.location(),
            SingleNameBinding::Id(ident, Some(izer)) => ident.location().merge(&izer.location()),
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        let SingleNameBinding::Id(ident, _) = self;
        ident.bound_names()
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let SingleNameBinding::Id(ident, opt) = self;
        ident.contains(kind) || opt.as_ref().is_some_and(|n| n.contains(kind))
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
        let SingleNameBinding::Id(_, oizer) = self;
        oizer.as_ref().is_some_and(|izer| izer.contains_arguments())
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

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            SingleNameBinding::Id(id, Some(init)) => {
                id.early_errors(errs, strict);
                init.early_errors(errs, strict);
            }
            SingleNameBinding::Id(id, None) => id.early_errors(errs, strict),
        }
    }

    /// Report whether this binding id contains an intializer
    ///
    /// See [HasInitializer](https://tc39.es/ecma262/#sec-static-semantics-hasinitializer) from ECMA-262.
    pub fn has_initializer(&self) -> bool {
        let SingleNameBinding::Id(_, izer) = self;
        izer.is_some()
    }

    /// Report whether this portion of a parameter list contains an expression
    ///
    /// See [ContainsExpression](https://tc39.es/ecma262/#sec-static-semantics-containsexpression) in ECMA-262.
    pub fn contains_expression(&self) -> bool {
        self.has_initializer()
    }
}

// BindingRestElement[Yield, Await] :
//      ... BindingIdentifier[?Yield, ?Await]
//      ... BindingPattern[?Yield, ?Await]
#[derive(Debug)]
pub enum BindingRestElement {
    Identifier(Rc<BindingIdentifier>, Location),
    Pattern(Rc<BindingPattern>, Location),
}

impl fmt::Display for BindingRestElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BindingRestElement::Identifier(node, ..) => write!(f, "... {node}"),
            BindingRestElement::Pattern(node, ..) => write!(f, "... {node}"),
        }
    }
}

impl PrettyPrint for BindingRestElement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}BindingRestElement: {self}")?;
        match self {
            BindingRestElement::Identifier(node, ..) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            BindingRestElement::Pattern(node, ..) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}BindingRestElement: {self}")?;
        pprint_token(writer, "...", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match self {
            BindingRestElement::Identifier(node, ..) => node.concise_with_leftpad(writer, &successive, Spot::Final),
            BindingRestElement::Pattern(node, ..) => node.concise_with_leftpad(writer, &successive, Spot::Final),
        }
    }
}

impl BindingRestElement {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        let (tok_loc, after_tok) =
            scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::Ellipsis)?;
        Err(ParseError::new(PECode::OpenOrIdentExpected, after_tok))
            .otherwise(|| {
                BindingPattern::parse(parser, after_tok, yield_flag, await_flag).map(|(bp, after_bp)| {
                    let location = tok_loc.merge(&bp.location());
                    (Rc::new(BindingRestElement::Pattern(bp, location)), after_bp)
                })
            })
            .otherwise(|| {
                BindingIdentifier::parse(parser, after_tok, yield_flag, await_flag).map(|(bi, after_bi)| {
                    let location = tok_loc.merge(&bi.location());
                    (Rc::new(BindingRestElement::Identifier(bi, location)), after_bi)
                })
            })
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

    pub fn location(&self) -> Location {
        match self {
            BindingRestElement::Identifier(_, location) | BindingRestElement::Pattern(_, location) => *location,
        }
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            BindingRestElement::Identifier(node, ..) => node.bound_names(),
            BindingRestElement::Pattern(node, ..) => node.bound_names(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            BindingRestElement::Identifier(node, ..) => node.contains(kind),
            BindingRestElement::Pattern(node, ..) => node.contains(kind),
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
            BindingRestElement::Identifier(..) => true,
            BindingRestElement::Pattern(node, ..) => node.all_private_identifiers_valid(names),
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
            BindingRestElement::Identifier(..) => false,
            BindingRestElement::Pattern(bp, ..) => bp.contains_arguments(),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            BindingRestElement::Identifier(node, ..) => node.early_errors(errs, strict),
            BindingRestElement::Pattern(node, ..) => node.early_errors(errs, strict),
        }
    }

    /// Report whether this portion of a parameter list contains an expression
    ///
    /// See [ContainsExpression](https://tc39.es/ecma262/#sec-static-semantics-containsexpression) in ECMA-262.
    pub fn contains_expression(&self) -> bool {
        match self {
            BindingRestElement::Identifier(..) => false,
            BindingRestElement::Pattern(node, ..) => node.contains_expression(),
        }
    }
}

#[cfg(test)]
mod tests;
