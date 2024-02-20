use super::*;
use ahash::AHashSet;
use non_empty_vec::{ne_vec, NonEmpty};
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// BlockStatement[Yield, Await, Return] :
//      Block[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum BlockStatement {
    Block(Rc<Block>),
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let BlockStatement::Block(node) = self;
        node.fmt(f)
    }
}

impl PrettyPrint for BlockStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}BlockStatement: {self}")?;
        let BlockStatement::Block(node) = self;
        node.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let BlockStatement::Block(node) = self;
        node.concise_with_leftpad(writer, pad, state)
    }
}

impl BlockStatement {
    // no caching needed
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> ParseResult<Self> {
        let (block, after_block) = Block::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        Ok((Rc::new(BlockStatement::Block(block)), after_block))
    }

    pub fn location(&self) -> Location {
        let BlockStatement::Block(node) = self;
        node.location()
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        let BlockStatement::Block(node) = self;
        node.var_declared_names()
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        let BlockStatement::Block(node) = self;
        node.contains_undefined_break_target(label_set)
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let BlockStatement::Block(node) = self;
        node.contains(kind)
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        let BlockStatement::Block(node) = self;
        node.contains_duplicate_labels(label_set)
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString], label_set: &[JSString]) -> bool {
        let BlockStatement::Block(node) = self;
        node.contains_undefined_continue_target(iteration_set, label_set)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        let BlockStatement::Block(node) = self;
        node.all_private_identifiers_valid(names)
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool, within_iteration: bool, within_switch: bool) {
        let BlockStatement::Block(node) = self;
        node.early_errors(errs, strict, within_iteration, within_switch);
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
        let BlockStatement::Block(block) = self;
        block.contains_arguments()
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        let BlockStatement::Block(block) = self;
        block.var_scoped_declarations()
    }
}

// Block[Yield, Await, Return] :
//      { StatementList[?Yield, ?Await, ?Return]opt }
#[derive(Debug)]
pub struct Block {
    pub statements: Option<Rc<StatementList>>,
    location: Location,
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let opt_sl = &self.statements;
        match opt_sl {
            None => write!(f, "{{ }}"),
            Some(node) => write!(f, "{{ {node} }}"),
        }
    }
}

impl PrettyPrint for Block {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}Block: {self}")?;
        let opt_sl = &self.statements;
        match opt_sl {
            None => Ok(()),
            Some(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}Block: {self}")?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match &self.statements {
            None => {}
            Some(node) => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
        }
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl Block {
    fn parse_core(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> ParseResult<Self> {
        let (lb_loc, after_lb) =
            scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBrace)?;
        let (sl, after_sl) = match StatementList::parse(parser, after_lb, yield_flag, await_flag, return_flag) {
            Err(_) => (None, after_lb),
            Ok((node, scan)) => (Some(node), scan),
        };
        let (rb_loc, after_rb) =
            scan_for_punct(after_sl, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(Block { statements: sl, location: lb_loc.merge(&rb_loc) }), after_rb))
    }

    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> ParseResult<Self> {
        let key = YieldAwaitReturnKey { scanner, yield_flag, await_flag, return_flag };
        match parser.block_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag, return_flag);
                parser.block_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        let opt_sl = &self.statements;
        match opt_sl {
            None => vec![],
            Some(node) => node.var_declared_names(),
        }
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        let opt_sl = &self.statements;
        match opt_sl {
            None => vec![],
            Some(node) => node.lexically_declared_names(),
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        let opt_sl = &self.statements;
        match opt_sl {
            None => false,
            Some(node) => node.contains_undefined_break_target(label_set),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match &self.statements {
            None => false,
            Some(n) => n.contains(kind),
        }
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match &self.statements {
            None => false,
            Some(n) => n.contains_duplicate_labels(label_set),
        }
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString], label_set: &[JSString]) -> bool {
        let opt_sl = &self.statements;
        opt_sl.as_ref().map_or(false, |node| node.contains_undefined_continue_target(iteration_set, label_set))
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        if let Some(node) = &self.statements {
            node.all_private_identifiers_valid(names)
        } else {
            true
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
        self.statements.as_ref().map_or(false, |sl| sl.contains_arguments())
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool, within_iteration: bool, within_switch: bool) {
        if let Some(sl) = &self.statements {
            // Static Semantics: Early Errors
            // Block : { StatementList }
            //  * It is a Syntax Error if the LexicallyDeclaredNames of StatementList contains any duplicate entries.
            //  * It is a Syntax Error if any element of the LexicallyDeclaredNames of StatementList also occurs in
            //    the VarDeclaredNames of StatementList.
            let ldn = sl.lexically_declared_names();
            let lexname_count = ldn.len();
            let lex_names_set: AHashSet<JSString> = ldn.into_iter().collect();
            let unique_lexname_count = lex_names_set.len();
            if lexname_count != unique_lexname_count {
                errs.push(create_syntax_error_object("Duplicate lexically declared names", Some(sl.location())));
            }
            let vdn = sl.var_declared_names();
            let var_names_set: AHashSet<JSString> = vdn.into_iter().collect();
            if !lex_names_set.is_disjoint(&var_names_set) {
                errs.push(create_syntax_error_object("Name defined both lexically and var-style", Some(sl.location())));
            }
            sl.early_errors(errs, strict, within_iteration, within_switch);
        }
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        match &self.statements {
            None => vec![],
            Some(sl) => sl.var_scoped_declarations(),
        }
    }
}

// StatementList[Yield, Await, Return] :
//      StatementListItem[?Yield, ?Await, ?Return]
//      StatementList[?Yield, ?Await, ?Return] StatementListItem[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub struct StatementList {
    pub list: NonEmpty<Rc<StatementListItem>>,
}

impl fmt::Display for StatementList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.list[0].fmt(f)?;
        for item in &self.list[1..] {
            write!(f, " {item}")?;
        }
        Ok(())
    }
}

impl PrettyPrint for StatementList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}StatementList: {self}")?;
        let not_final_length = usize::from(self.list.len()) - 1;
        for item in &self.list[0..not_final_length] {
            item.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        self.list[not_final_length].pprint_with_leftpad(writer, &successive, Spot::Final)
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        if usize::from(self.list.len()) == 1 {
            self.list[0].concise_with_leftpad(writer, pad, state)
        } else {
            writeln!(writer, "{first}StatementList: {self}")?;
            let not_final_length = usize::from(self.list.len()) - 1;
            for item in &self.list[0..not_final_length] {
                item.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
            self.list[not_final_length].concise_with_leftpad(writer, &successive, Spot::Final)
        }
    }
}

impl StatementList {
    fn parse_core(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> ParseResult<Self> {
        let (item, after_item) = StatementListItem::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        let mut items = ne_vec![item];
        let mut current_scanner = after_item;
        while let Ok((next, after_next)) =
            StatementListItem::parse(parser, current_scanner, yield_flag, await_flag, return_flag)
        {
            items.push(next);
            current_scanner = after_next;
        }
        Ok((Rc::new(StatementList { list: items }), current_scanner))
    }

    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> ParseResult<Self> {
        let key = YieldAwaitReturnKey { scanner, yield_flag, await_flag, return_flag };
        match parser.statement_list_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag, return_flag);
                parser.statement_list_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn location(&self) -> Location {
        match usize::from(self.list.len()) {
            1 => self.list[0].location(),
            n => self.list[0].location().merge(&self.list[n - 1].location()),
        }
    }

    pub fn top_level_lexically_declared_names(&self) -> Vec<JSString> {
        let mut result = vec![];
        for item in &self.list {
            result.extend(item.top_level_lexically_declared_names());
        }
        result
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        // Static Semantics: LexicallyDeclaredNames
        //
        // StatementList : StatementListItem
        //  1. Return LexicallyDeclaredNames of StatementListItem
        //
        // StatementList : StatementList StatementListItem
        //  1. Let names1 be LexicallyDeclaredNames of StatementList.
        //  2. Let names2 be LexicallyDeclaredNames of StatementListItem.
        //  3. Return the list-concatenation of names1 and names2.
        self.list.iter().flat_map(|item| item.lexically_declared_names()).collect()
    }

    pub fn top_level_var_declared_names(&self) -> Vec<JSString> {
        self.list.iter().flat_map(|item| item.top_level_var_declared_names()).collect()
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        self.list.iter().flat_map(|item| item.var_declared_names()).collect()
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        self.list.iter().any(|item| item.contains_undefined_break_target(label_set))
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        kind == ParseNodeKind::StatementListItem
            || (usize::from(self.list.len()) > 1 && kind == ParseNodeKind::StatementList)
            || self.list.iter().any(|item| item.contains(kind))
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        self.list.iter().any(|item| item.contains_duplicate_labels(label_set))
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString], label_set: &[JSString]) -> bool {
        match usize::from(self.list.len()) {
            1 => self.list[0].contains_undefined_continue_target(iteration_set, label_set),
            _ => self.list.iter().any(|item| item.contains_undefined_continue_target(iteration_set, &[])),
        }
    }

    // Returns the list of string tokens which comprise the first expression statements of a statement list.
    pub fn initial_string_tokens(&self) -> Vec<StringToken> {
        self.list.iter().map_while(|item| item.as_string_literal()).collect()
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

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool, within_iteration: bool, within_switch: bool) {
        for item in &self.list {
            item.early_errors(errs, strict, within_iteration, within_switch);
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
        self.list.iter().any(|item| item.contains_arguments())
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// This is the top-level form; in this form, function definitions that exist lexically at global scope are treated
    /// as though they are declared var-style.
    ///
    /// See [TopLevelVarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-toplevelvarscopeddeclarations) in ECMA-262.
    pub fn top_level_var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        self.list.iter().flat_map(|item| item.top_level_var_scoped_declarations()).collect()
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        self.list.iter().flat_map(|item| item.var_scoped_declarations()).collect()
    }

    /// Returns the lexically-scoped declarations of this node (as if this node was at global scope)
    ///
    /// See [TopLevelLexicallyScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-toplevellexicallyscopeddeclarations) in ECMA-262.
    pub fn top_level_lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        self.list.iter().flat_map(|item| item.top_level_lexically_scoped_declarations()).collect()
    }

    pub fn lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        self.list.iter().flat_map(|item| item.lexically_scoped_declarations()).collect()
    }
}

// StatementListItem[Yield, Await, Return] :
//      Statement[?Yield, ?Await, ?Return]
//      Declaration[?Yield, ?Await]
#[derive(Debug)]
pub enum StatementListItem {
    Statement(Rc<Statement>),
    Declaration(Rc<Declaration>),
}

impl fmt::Display for StatementListItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StatementListItem::Statement(node) => node.fmt(f),
            StatementListItem::Declaration(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for StatementListItem {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}StatementListItem: {self}")?;
        match self {
            StatementListItem::Declaration(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            StatementListItem::Statement(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            StatementListItem::Statement(node) => node.concise_with_leftpad(writer, pad, state),
            StatementListItem::Declaration(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl StatementListItem {
    // no caching needed
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> ParseResult<Self> {
        Err(ParseError::new(PECode::DeclarationOrStatementExpected, scanner))
            .otherwise(|| {
                Statement::parse(parser, scanner, yield_flag, await_flag, return_flag).map(
                    |(statement, after_statement)| (Rc::new(StatementListItem::Statement(statement)), after_statement),
                )
            })
            .otherwise(|| {
                Declaration::parse(parser, scanner, yield_flag, await_flag)
                    .map(|(decl, after_decl)| (Rc::new(StatementListItem::Declaration(decl)), after_decl))
            })
    }

    pub fn location(&self) -> Location {
        match self {
            StatementListItem::Statement(stmt) => stmt.location(),
            StatementListItem::Declaration(decl) => decl.location(),
        }
    }

    pub fn top_level_lexically_declared_names(&self) -> Vec<JSString> {
        match self {
            StatementListItem::Statement(_) => vec![],
            StatementListItem::Declaration(node) => match **node {
                Declaration::Hoistable(_) => vec![],
                _ => node.bound_names(),
            },
        }
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        // Static Semantics: LexicallyDeclaredNames
        match self {
            StatementListItem::Statement(node) => {
                // StatementListItem : Statement
                //  1. If Statement is Statement : LabelledStatement , return LexicallyDeclaredNames of LabelledStatement.
                //  2. Return a new empty List.
                match &**node {
                    Statement::Labelled(node) => node.lexically_declared_names(),
                    _ => vec![],
                }
            }
            StatementListItem::Declaration(node) => {
                // StatementListItem : Declaration
                //  1. Return the BoundNames of Declaration.
                node.bound_names()
            }
        }
    }

    pub fn top_level_var_declared_names(&self) -> Vec<JSString> {
        match self {
            StatementListItem::Statement(node) => match &**node {
                Statement::Labelled(stmt) => stmt.top_level_var_declared_names(),
                _ => node.var_declared_names(),
            },
            StatementListItem::Declaration(node) => match &**node {
                Declaration::Hoistable(decl) => decl.bound_names(),
                _ => vec![],
            },
        }
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            StatementListItem::Statement(node) => node.var_declared_names(),
            StatementListItem::Declaration(..) => vec![],
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            StatementListItem::Statement(node) => node.contains_undefined_break_target(label_set),
            StatementListItem::Declaration(..) => false,
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            StatementListItem::Statement(node) => kind == ParseNodeKind::Statement || node.contains(kind),
            StatementListItem::Declaration(node) => kind == ParseNodeKind::Declaration || node.contains(kind),
        }
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self {
            StatementListItem::Statement(node) => node.contains_duplicate_labels(label_set),
            StatementListItem::Declaration(_) => false,
        }
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString], label_set: &[JSString]) -> bool {
        match self {
            StatementListItem::Statement(node) => node.contains_undefined_continue_target(iteration_set, label_set),
            StatementListItem::Declaration(_) => false,
        }
    }

    pub fn as_string_literal(&self) -> Option<StringToken> {
        match self {
            StatementListItem::Statement(node) => node.as_string_literal(),
            StatementListItem::Declaration(_) => None,
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
            StatementListItem::Statement(node) => node.all_private_identifiers_valid(names),
            StatementListItem::Declaration(node) => node.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool, within_iteration: bool, within_switch: bool) {
        match self {
            StatementListItem::Statement(node) => node.early_errors(errs, strict, within_iteration, within_switch),
            StatementListItem::Declaration(node) => node.early_errors(errs, strict),
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
            StatementListItem::Statement(stmt) => stmt.contains_arguments(),
            StatementListItem::Declaration(decl) => decl.contains_arguments(),
        }
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// This is the top-level form; in this form, function definitions that exist lexically at global scope are treated
    /// as though they are declared var-style.
    ///
    /// See [TopLevelVarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-toplevelvarscopeddeclarations) in ECMA-262.
    pub fn top_level_var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        match self {
            StatementListItem::Statement(node) => match &**node {
                Statement::Labelled(stmt) => stmt.top_level_var_scoped_declarations(),
                _ => node.var_scoped_declarations(),
            },
            StatementListItem::Declaration(node) => match &**node {
                Declaration::Hoistable(decl) => vec![decl.declaration_part().into()],
                _ => vec![],
            },
        }
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        match self {
            StatementListItem::Declaration(_) => vec![],
            StatementListItem::Statement(stmt) => stmt.var_scoped_declarations(),
        }
    }

    /// Returns the lexically-scoped declarations of this node (as if this node was at global scope)
    ///
    /// See [TopLevelLexicallyScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-toplevellexicallyscopeddeclarations) in ECMA-262.
    pub fn top_level_lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        match self {
            StatementListItem::Statement(_) => vec![],
            StatementListItem::Declaration(d) => d.top_level_lexically_scoped_declarations(),
        }
    }

    pub fn lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        match self {
            StatementListItem::Statement(stmt) => match &**stmt {
                Statement::Labelled(l) => l.lexically_scoped_declarations(),
                _ => vec![],
            },
            StatementListItem::Declaration(d) => vec![d.declaration_part()],
        }
    }
}

#[cfg(test)]
mod tests;
