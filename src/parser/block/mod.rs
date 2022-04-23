use super::scanner::{Punctuator, ScanGoal, Scanner, StringToken};
use super::scripts::VarScopeDecl;
use super::statements_and_declarations::{DeclPart, Declaration, Statement};
use super::*;
use crate::chunk::Chunk;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};
use ahash::AHashSet;
use anyhow;
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
        writeln!(writer, "{}BlockStatement: {}", first, self)?;
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let (block, after_block) = Block::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        Ok((Rc::new(BlockStatement::Block(block)), after_block))
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

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool, within_iteration: bool, within_switch: bool) {
        let BlockStatement::Block(node) = self;
        node.early_errors(agent, errs, strict, within_iteration, within_switch);
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
pub enum Block {
    Statements(Option<Rc<StatementList>>),
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Block::Statements(opt_sl) = self;
        match opt_sl {
            None => write!(f, "{{ }}"),
            Some(node) => write!(f, "{{ {} }}", node),
        }
    }
}

impl PrettyPrint for Block {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Block: {}", first, self)?;
        let Block::Statements(opt_sl) = self;
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
        writeln!(writer, "{}Block: {}", first, self)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match self {
            Block::Statements(None) => {}
            Block::Statements(Some(node)) => {
                node.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            }
        }
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl Block {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let after_lb = scan_for_punct(scanner, parser.source, ScanGoal::InputElementRegExp, Punctuator::LeftBrace)?;
        let (sl, after_sl) = match StatementList::parse(parser, after_lb, yield_flag, await_flag, return_flag) {
            Err(_) => (None, after_lb),
            Ok((node, scan)) => (Some(node), scan),
        };
        let after_rb = scan_for_punct(after_sl, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((Rc::new(Block::Statements(sl)), after_rb))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
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

    pub fn var_declared_names(&self) -> Vec<JSString> {
        let Block::Statements(opt_sl) = self;
        match opt_sl {
            None => vec![],
            Some(node) => node.var_declared_names(),
        }
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        let Block::Statements(opt_sl) = self;
        match opt_sl {
            None => vec![],
            Some(node) => node.lexically_declared_names(),
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        let Block::Statements(opt_sl) = self;
        match opt_sl {
            None => false,
            Some(node) => node.contains_undefined_break_target(label_set),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        let Block::Statements(node) = self;
        match node {
            None => false,
            Some(n) => n.contains(kind),
        }
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        let Block::Statements(node) = self;
        match node {
            None => false,
            Some(n) => n.contains_duplicate_labels(label_set),
        }
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString], label_set: &[JSString]) -> bool {
        let Block::Statements(opt_sl) = self;
        opt_sl.as_ref().map_or(false, |node| node.contains_undefined_continue_target(iteration_set, label_set))
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        if let Block::Statements(Some(node)) = self {
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
        let Block::Statements(sl) = self;
        sl.as_ref().map_or(false, |sl| sl.contains_arguments())
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool, within_iteration: bool, within_switch: bool) {
        if let Block::Statements(Some(sl)) = self {
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
                errs.push(create_syntax_error_object(agent, "Duplicate lexically declared names"));
            }
            let vdn = sl.var_declared_names();
            let var_names_set: AHashSet<JSString> = vdn.into_iter().collect();
            if !lex_names_set.is_disjoint(&var_names_set) {
                errs.push(create_syntax_error_object(agent, "Name defined both lexically and var-style"));
            }
            sl.early_errors(agent, errs, strict, within_iteration, within_switch);
        }
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        let Block::Statements(sl) = self;
        match sl {
            None => vec![],
            Some(sl) => sl.var_scoped_declarations(),
        }
    }
}

// StatementList[Yield, Await, Return] :
//      StatementListItem[?Yield, ?Await, ?Return]
//      StatementList[?Yield, ?Await, ?Return] StatementListItem[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum StatementList {
    Item(Rc<StatementListItem>),
    List(Rc<StatementList>, Rc<StatementListItem>),
}

impl fmt::Display for StatementList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StatementList::Item(node) => node.fmt(f),
            StatementList::List(lst, item) => write!(f, "{} {}", lst, item),
        }
    }
}

impl PrettyPrint for StatementList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}StatementList: {}", first, self)?;
        match self {
            StatementList::Item(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            StatementList::List(lst, item) => {
                lst.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        match self {
            StatementList::Item(node) => node.concise_with_leftpad(writer, pad, state),
            StatementList::List(lst, item) => {
                writeln!(writer, "{}StatementList: {}", first, self)?;
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl StatementList {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let (item, after_item) = StatementListItem::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        let mut current = Rc::new(StatementList::Item(item));
        let mut current_scanner = after_item;
        while let Ok((next, after_next)) = StatementListItem::parse(parser, current_scanner, yield_flag, await_flag, return_flag) {
            current = Rc::new(StatementList::List(current, next));
            current_scanner = after_next;
        }
        Ok((current, current_scanner))
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
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

    pub fn top_level_lexically_declared_names(&self) -> Vec<JSString> {
        match self {
            StatementList::Item(node) => node.top_level_lexically_declared_names(),
            StatementList::List(list, item) => {
                let mut result = list.top_level_lexically_declared_names();
                result.extend(item.top_level_lexically_declared_names());
                result
            }
        }
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        // Static Semantics: LexicallyDeclaredNames
        match self {
            StatementList::Item(node) => {
                // StatementList : StatementListItem
                //  1. Return LexicallyDeclaredNames of StatementListItem
                node.lexically_declared_names()
            }
            StatementList::List(list, item) => {
                // StatementList : StatementList StatementListItem
                //  1. Let names1 be LexicallyDeclaredNames of StatementList.
                //  2. Let names2 be LexicallyDeclaredNames of StatementListItem.
                //  3. Return the list-concatenation of names1 and names2.
                let mut result = list.lexically_declared_names();
                result.extend(item.lexically_declared_names());
                result
            }
        }
    }

    pub fn top_level_var_declared_names(&self) -> Vec<JSString> {
        match self {
            StatementList::Item(node) => node.top_level_var_declared_names(),
            StatementList::List(lst, item) => {
                let mut names = lst.top_level_var_declared_names();
                names.extend(item.top_level_var_declared_names());
                names
            }
        }
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            StatementList::Item(node) => node.var_declared_names(),
            StatementList::List(lst, item) => {
                let mut names = lst.var_declared_names();
                names.extend(item.var_declared_names());
                names
            }
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            StatementList::Item(node) => node.contains_undefined_break_target(label_set),
            StatementList::List(lst, item) => lst.contains_undefined_break_target(label_set) || item.contains_undefined_break_target(label_set),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            StatementList::Item(node) => kind == ParseNodeKind::StatementListItem || node.contains(kind),
            StatementList::List(lst, item) => kind == ParseNodeKind::StatementList || kind == ParseNodeKind::StatementListItem || lst.contains(kind) || item.contains(kind),
        }
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self {
            StatementList::Item(node) => node.contains_duplicate_labels(label_set),
            StatementList::List(lst, item) => lst.contains_duplicate_labels(label_set) || item.contains_duplicate_labels(label_set),
        }
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString], label_set: &[JSString]) -> bool {
        match self {
            StatementList::Item(node) => node.contains_undefined_continue_target(iteration_set, label_set),
            StatementList::List(lst, item) => lst.contains_undefined_continue_target(iteration_set, &[]) || item.contains_undefined_continue_target(iteration_set, &[]),
        }
    }

    // Returns the list of string tokens which comprise the first expression statements of a statement list, along with
    // a boolean value which is true if all of the items in the statement list were string literal expressions.
    fn initial_string_tokens_internal(&self) -> (Vec<StringToken>, bool) {
        match self {
            StatementList::Item(node) => node.as_string_literal().map_or((Vec::new(), false), |token| (vec![token], true)),
            StatementList::List(lst, item) => {
                let (mut head, all) = lst.initial_string_tokens_internal();
                if all {
                    let next = item.as_string_literal();
                    match next {
                        None => (head, false),
                        Some(token) => {
                            head.push(token);
                            (head, true)
                        }
                    }
                } else {
                    (head, false)
                }
            }
        }
    }
    pub fn initial_string_tokens(&self) -> Vec<StringToken> {
        self.initial_string_tokens_internal().0
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        match self {
            StatementList::Item(node) => node.all_private_identifiers_valid(names),
            StatementList::List(lst, item) => lst.all_private_identifiers_valid(names) && item.all_private_identifiers_valid(names),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool, within_iteration: bool, within_switch: bool) {
        match self {
            StatementList::Item(node) => node.early_errors(agent, errs, strict, within_iteration, within_switch),
            StatementList::List(lst, item) => {
                lst.early_errors(agent, errs, strict, within_iteration, within_switch);
                item.early_errors(agent, errs, strict, within_iteration, within_switch);
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
            StatementList::Item(sli) => sli.contains_arguments(),
            StatementList::List(sl, sli) => sl.contains_arguments() || sli.contains_arguments(),
        }
    }

    #[allow(unused_variables)]
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            StatementList::Item(sli) => sli.compile(chunk, strict),
            StatementList::List(sl, sli) => todo!(),
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
            StatementList::Item(sli) => sli.top_level_var_scoped_declarations(),
            StatementList::List(sl, sli) => {
                let mut list = sl.top_level_var_scoped_declarations();
                list.extend(sli.top_level_var_scoped_declarations());
                list
            }
        }
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        match self {
            StatementList::Item(sli) => sli.var_scoped_declarations(),
            StatementList::List(sl, sli) => {
                let mut list = sl.var_scoped_declarations();
                list.extend(sli.var_scoped_declarations());
                list
            }
        }
    }

    /// Returns the lexically-scoped declarations of this node (as if this node was at global scope)
    ///
    /// See [TopLevelLexicallyScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-toplevellexicallyscopeddeclarations) in ECMA-262.
    pub fn top_level_lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        match self {
            StatementList::Item(sli) => sli.top_level_lexically_scoped_declarations(),
            StatementList::List(sl, sli) => {
                let mut list = sl.top_level_lexically_scoped_declarations();
                list.extend(sli.top_level_lexically_scoped_declarations());
                list
            }
        }
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
        writeln!(writer, "{}StatementListItem: {}", first, self)?;
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::DeclarationOrStatementExpected, scanner))
            .otherwise(|| {
                Statement::parse(parser, scanner, yield_flag, await_flag, return_flag).map(|(statement, after_statement)| (Rc::new(StatementListItem::Statement(statement)), after_statement))
            })
            .otherwise(|| Declaration::parse(parser, scanner, yield_flag, await_flag).map(|(decl, after_decl)| (Rc::new(StatementListItem::Declaration(decl)), after_decl)))
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

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool, within_iteration: bool, within_switch: bool) {
        match self {
            StatementListItem::Statement(node) => node.early_errors(agent, errs, strict, within_iteration, within_switch),
            StatementListItem::Declaration(node) => node.early_errors(agent, errs, strict),
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

    #[allow(unused_variables)]
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            StatementListItem::Statement(stmt) => stmt.compile(chunk, strict),
            StatementListItem::Declaration(decl) => todo!(),
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
}

#[cfg(test)]
mod tests;
