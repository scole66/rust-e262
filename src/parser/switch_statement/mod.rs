use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// SwitchStatement[Yield, Await, Return] :
//      switch ( Expression[+In, ?Yield, ?Await] ) CaseBlock[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub struct SwitchStatement {
    pub expression: Rc<Expression>,
    pub case_block: Rc<CaseBlock>,
    location: Location,
}

impl fmt::Display for SwitchStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "switch ( {} ) {}", self.expression, self.case_block)
    }
}

impl PrettyPrint for SwitchStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}SwitchStatement: {self}")?;
        self.expression.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.case_block.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}SwitchStatement: {self}")?;
        pprint_token(writer, "switch", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.expression.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.case_block.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl SwitchStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> ParseResult<Self> {
        let (switch_loc, after_switch) =
            scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Switch)?;
        let (_, after_open) =
            scan_for_punct(after_switch, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (exp, after_exp) = Expression::parse(parser, after_open, true, yield_flag, await_flag)?;
        let (_, after_close) =
            scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let (cb, after_cases) = CaseBlock::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
        let location = switch_loc.merge(&cb.location());
        Ok((Rc::new(SwitchStatement { expression: exp, case_block: cb, location }), after_cases))
    }

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        self.case_block.var_declared_names()
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        self.case_block.contains_undefined_break_target(label_set)
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.expression.contains(kind) || self.case_block.contains(kind)
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        self.case_block.contains_duplicate_labels(label_set)
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        self.case_block.contains_undefined_continue_target(iteration_set)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.expression.all_private_identifiers_valid(names) && self.case_block.all_private_identifiers_valid(names)
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
        self.expression.contains_arguments() || self.case_block.contains_arguments()
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool, within_iteration: bool) {
        // Static Semantics: Early Errors
        //  SwitchStatement : switch ( Expression ) CaseBlock
        //  * It is a Syntax Error if the LexicallyDeclaredNames of CaseBlock contains any duplicate entries.
        //  * It is a Syntax Error if any element of the LexicallyDeclaredNames of CaseBlock also occurs in the
        //    VarDeclaredNames of CaseBlock.
        let ldn = self.case_block.lexically_declared_names();
        let vdn = self.case_block.var_declared_names();
        for name in duplicates(&ldn) {
            errs.push(create_syntax_error_object(
                format!("‘{name}’ already defined"),
                Some(self.case_block.location()),
            ));
        }
        for name in ldn.iter().filter(|&s| vdn.contains(s)) {
            errs.push(create_syntax_error_object(
                format!("‘{name}’ may not be declared both lexically and var-style"),
                Some(self.case_block.location()),
            ));
        }

        self.expression.early_errors(errs, strict);
        self.case_block.early_errors(errs, strict, within_iteration);
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        self.case_block.var_scoped_declarations()
    }

    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            self.expression
                .body_containing_location(location)
                .or_else(|| self.case_block.body_containing_location(location))
        } else {
            None
        }
    }

    pub fn has_call_in_tail_position(&self, location: &Location) -> bool {
        // Static Semantics: HasCallInTailPosition
        // The syntax-directed operation HasCallInTailPosition takes argument call (a CallExpression Parse Node, a
        // MemberExpression Parse Node, or an OptionalChain Parse Node) and returns a Boolean.
        //
        // Note 1: call is a Parse Node that represents a specific range of source text. When the following algorithms
        //         compare call to another Parse Node, it is a test of whether they represent the same source text.
        //
        // Note 2: A potential tail position call that is immediately followed by return GetValue of the call result is
        //         also a possible tail position call. A function call cannot return a Reference Record, so such a
        //         GetValue operation will always return the same value as the actual function call result.
        //
        self.case_block.has_call_in_tail_position(location)
    }
}

// CaseBlock[Yield, Await, Return] :
//      { CaseClauses[?Yield, ?Await, ?Return]opt }
//      { CaseClauses[?Yield, ?Await, ?Return]opt DefaultClause[?Yield, ?Await, ?Return] CaseClauses[?Yield, ?Await, ?Return]opt }
#[derive(Debug)]
pub enum CaseBlock {
    NoDefault(Option<Rc<CaseClauses>>, Location),
    HasDefault(Option<Rc<CaseClauses>>, Rc<DefaultClause>, Option<Rc<CaseClauses>>, Location),
}

impl fmt::Display for CaseBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CaseBlock::NoDefault(None, _) => write!(f, "{{ }}"),
            CaseBlock::NoDefault(Some(node), _) => write!(f, "{{ {node} }}"),
            CaseBlock::HasDefault(None, def, None, _) => write!(f, "{{ {def} }}"),
            CaseBlock::HasDefault(Some(pre), def, None, _) => {
                write!(f, "{{ {pre} {def} }}")
            }
            CaseBlock::HasDefault(None, def, Some(post), _) => {
                write!(f, "{{ {def} {post} }}")
            }
            CaseBlock::HasDefault(Some(pre), def, Some(post), _) => {
                write!(f, "{{ {pre} {def} {post} }}")
            }
        }
    }
}

impl PrettyPrint for CaseBlock {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}CaseBlock: {self}")?;
        match self {
            CaseBlock::NoDefault(None, _) => Ok(()),
            CaseBlock::NoDefault(Some(node), _) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            CaseBlock::HasDefault(None, def, None, _) => def.pprint_with_leftpad(writer, &successive, Spot::Final),
            CaseBlock::HasDefault(Some(pre), def, None, _) => {
                pre.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                def.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CaseBlock::HasDefault(None, def, Some(post), _) => {
                def.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                post.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            CaseBlock::HasDefault(Some(pre), def, Some(post), _) => {
                pre.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                def.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                post.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}CaseBlock: {self}")?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        match self {
            CaseBlock::NoDefault(None, _) => Ok(()),
            CaseBlock::NoDefault(Some(node), _) => node.concise_with_leftpad(writer, &successive, Spot::NotFinal),
            CaseBlock::HasDefault(None, def, None, _) => def.concise_with_leftpad(writer, &successive, Spot::NotFinal),
            CaseBlock::HasDefault(Some(pre), def, None, _) => {
                pre.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                def.concise_with_leftpad(writer, &successive, Spot::NotFinal)
            }
            CaseBlock::HasDefault(None, def, Some(post), _) => {
                def.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                post.concise_with_leftpad(writer, &successive, Spot::NotFinal)
            }
            CaseBlock::HasDefault(Some(pre), def, Some(post), _) => {
                pre.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                def.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                post.concise_with_leftpad(writer, &successive, Spot::NotFinal)
            }
        }?;
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl CaseBlock {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> ParseResult<Self> {
        let (open_loc, after_open) =
            scan_for_punct(scanner, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (pre, after_pre) = match CaseClauses::parse(parser, after_open, yield_flag, await_flag, return_flag) {
            Ok((node, scan)) => (Some(node), scan),
            Err(_) => (None, after_open),
        };
        Err(ParseError::new(PECode::CaseBlockCloseExpected, after_pre))
            .otherwise(|| {
                let (close_loc, after_close) =
                    scan_for_punct(after_pre, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                Ok((None, close_loc, after_close))
            })
            .otherwise(|| {
                let (def, after_def) = DefaultClause::parse(parser, after_pre, yield_flag, await_flag, return_flag)?;
                let (post, after_post) =
                    match CaseClauses::parse(parser, after_def, yield_flag, await_flag, return_flag) {
                        Ok((node, scan)) => (Some(node), scan),
                        Err(_) => (None, after_def),
                    };
                let (close_loc, after_close) =
                    scan_for_punct(after_post, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
                Ok((Some((def, post)), close_loc, after_close))
            })
            .map(|(post, close_loc, scan)| {
                (
                    {
                        let location = open_loc.merge(&close_loc);
                        Rc::new(match post {
                            None => CaseBlock::NoDefault(pre, location),
                            Some((def, after)) => CaseBlock::HasDefault(pre, def, after, location),
                        })
                    },
                    scan,
                )
            })
    }

    pub fn location(&self) -> Location {
        match self {
            CaseBlock::NoDefault(_, location) | CaseBlock::HasDefault(_, _, _, location) => *location,
        }
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            CaseBlock::NoDefault(None, _) => vec![],
            CaseBlock::NoDefault(Some(node), _) => node.var_declared_names(),
            CaseBlock::HasDefault(pre, def, post, _) => {
                let mut names = match pre {
                    None => vec![],
                    Some(node) => node.var_declared_names(),
                };
                names.extend(def.var_declared_names());
                if let Some(node) = post {
                    names.extend(node.var_declared_names());
                }
                names
            }
        }
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        let (c1, dflt, c2) = match self {
            CaseBlock::NoDefault(c, _) => (c.as_ref(), None, None),
            CaseBlock::HasDefault(pre, def, post, _) => (pre.as_ref(), Some(def), post.as_ref()),
        };
        let mut result = vec![];
        if let Some(caseclauses) = c1 {
            result.extend(caseclauses.lexically_declared_names());
        }
        if let Some(defaultclause) = dflt {
            result.extend(defaultclause.lexically_declared_names());
        }
        if let Some(caseclauses) = c2 {
            result.extend(caseclauses.lexically_declared_names());
        }

        result
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            CaseBlock::NoDefault(None, _) => false,
            CaseBlock::NoDefault(Some(node), _) => node.contains_undefined_break_target(label_set),
            CaseBlock::HasDefault(pre, def, post, _) => {
                pre.as_ref().is_some_and(|node| node.contains_undefined_break_target(label_set))
                    || def.contains_undefined_break_target(label_set)
                    || post.as_ref().is_some_and(|node| node.contains_undefined_break_target(label_set))
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            CaseBlock::NoDefault(opt, _) => opt.as_ref().is_some_and(|n| n.contains(kind)),
            CaseBlock::HasDefault(opt1, def, opt2, _) => {
                opt1.as_ref().is_some_and(|n| n.contains(kind))
                    || def.contains(kind)
                    || opt2.as_ref().is_some_and(|n| n.contains(kind))
            }
        }
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self {
            CaseBlock::NoDefault(None, _) => false,
            CaseBlock::NoDefault(Some(node), _) => node.contains_duplicate_labels(label_set),
            CaseBlock::HasDefault(pre, def, post, _) => {
                pre.as_ref().is_some_and(|node| node.contains_duplicate_labels(label_set))
                    || def.contains_duplicate_labels(label_set)
                    || post.as_ref().is_some_and(|node| node.contains_duplicate_labels(label_set))
            }
        }
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        match self {
            CaseBlock::NoDefault(None, _) => false,
            CaseBlock::NoDefault(Some(node), _) => node.contains_undefined_continue_target(iteration_set),
            CaseBlock::HasDefault(pre, def, post, _) => {
                pre.as_ref().is_some_and(|node| node.contains_undefined_continue_target(iteration_set))
                    || def.contains_undefined_continue_target(iteration_set)
                    || post.as_ref().is_some_and(|node| node.contains_undefined_continue_target(iteration_set))
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
            CaseBlock::NoDefault(Some(node), _) => node.all_private_identifiers_valid(names),
            CaseBlock::HasDefault(None, node, None, _) => node.all_private_identifiers_valid(names),
            CaseBlock::HasDefault(Some(node1), node2, None, _) => {
                node1.all_private_identifiers_valid(names) && node2.all_private_identifiers_valid(names)
            }
            CaseBlock::HasDefault(None, node1, Some(node2), _) => {
                node1.all_private_identifiers_valid(names) && node2.all_private_identifiers_valid(names)
            }
            CaseBlock::HasDefault(Some(node1), node2, Some(node3), _) => {
                node1.all_private_identifiers_valid(names)
                    && node2.all_private_identifiers_valid(names)
                    && node3.all_private_identifiers_valid(names)
            }
            _ => true,
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
            CaseBlock::NoDefault(occ, _) => occ.as_ref().is_some_and(|cc| cc.contains_arguments()),
            CaseBlock::HasDefault(occ1, dc, occ2, _) => {
                occ1.as_ref().is_some_and(|cc| cc.contains_arguments())
                    || dc.contains_arguments()
                    || occ2.as_ref().is_some_and(|cc| cc.contains_arguments())
            }
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool, within_iteration: bool) {
        let (before, default, after) = match self {
            CaseBlock::NoDefault(cc, _) => (cc.as_ref(), None, None),
            CaseBlock::HasDefault(cc1, def, cc2, _) => (cc1.as_ref(), Some(def), cc2.as_ref()),
        };
        if let Some(cc) = before {
            cc.early_errors(errs, strict, within_iteration);
        }
        if let Some(def) = default {
            def.early_errors(errs, strict, within_iteration);
        }
        if let Some(cc) = after {
            cc.early_errors(errs, strict, within_iteration);
        }
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        let (before, default, after) = match self {
            CaseBlock::NoDefault(cc, _) => (cc.as_ref(), None, None),
            CaseBlock::HasDefault(cc1, def, cc2, _) => (cc1.as_ref(), Some(def), cc2.as_ref()),
        };
        let mut list = vec![];
        if let Some(before) = before {
            list.extend(before.var_scoped_declarations());
        }
        if let Some(def) = default {
            list.extend(def.var_scoped_declarations());
        }
        if let Some(after) = after {
            list.extend(after.var_scoped_declarations());
        }
        list
    }

    pub fn lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        let (before, default, after) = match self {
            CaseBlock::NoDefault(cc, _) => (cc.as_ref(), None, None),
            CaseBlock::HasDefault(before, def, after, _) => (before.as_ref(), Some(def), after.as_ref()),
        };
        let mut list = vec![];
        if let Some(before) = before {
            list.extend(before.lexically_scoped_declarations());
        }
        if let Some(def) = default {
            list.extend(def.lexically_scoped_declarations());
        }
        if let Some(after) = after {
            list.extend(after.lexically_scoped_declarations());
        }
        list
    }

    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                CaseBlock::NoDefault(case_clauses, ..) => {
                    case_clauses.as_ref().and_then(|cc| cc.body_containing_location(location))
                }
                CaseBlock::HasDefault(case_clauses, default_clause, case_clauses1, ..) => case_clauses
                    .as_ref()
                    .and_then(|cc| cc.body_containing_location(location))
                    .or_else(|| default_clause.body_containing_location(location))
                    .or_else(|| case_clauses1.as_ref().and_then(|cc| cc.body_containing_location(location))),
            }
        } else {
            None
        }
    }

    pub fn has_call_in_tail_position(&self, location: &Location) -> bool {
        // Static Semantics: HasCallInTailPosition
        // The syntax-directed operation HasCallInTailPosition takes argument call (a CallExpression Parse Node, a
        // MemberExpression Parse Node, or an OptionalChain Parse Node) and returns a Boolean.
        //
        // Note 1: call is a Parse Node that represents a specific range of source text. When the following algorithms
        //         compare call to another Parse Node, it is a test of whether they represent the same source text.
        //
        // Note 2: A potential tail position call that is immediately followed by return GetValue of the call result is
        //         also a possible tail position call. A function call cannot return a Reference Record, so such a
        //         GetValue operation will always return the same value as the actual function call result.
        //
        match self {
            CaseBlock::NoDefault(case_clauses, ..) => {
                case_clauses.as_ref().is_some_and(|cc| cc.has_call_in_tail_position(location))
            }
            CaseBlock::HasDefault(case_clauses, default_clause, case_clauses1, ..) => {
                case_clauses.as_ref().is_some_and(|cc| cc.has_call_in_tail_position(location))
                    || default_clause.has_call_in_tail_position(location)
                    || case_clauses1.as_ref().is_some_and(|cc| cc.has_call_in_tail_position(location))
            }
        }
    }
}

// CaseClauses[Yield, Await, Return] :
//      CaseClause[?Yield, ?Await, ?Return]
//      CaseClauses[?Yield, ?Await, ?Return] CaseClause[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum CaseClauses {
    Item(Rc<CaseClause>),
    List(Rc<CaseClauses>, Rc<CaseClause>),
}

impl fmt::Display for CaseClauses {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CaseClauses::Item(node) => node.fmt(f),
            CaseClauses::List(lst, item) => write!(f, "{lst} {item}"),
        }
    }
}

impl PrettyPrint for CaseClauses {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}CaseClauses: {self}")?;
        match self {
            CaseClauses::Item(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            CaseClauses::List(lst, item) => {
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
            CaseClauses::Item(node) => node.concise_with_leftpad(writer, pad, state),
            CaseClauses::List(lst, item) => {
                let (first, successive) = prettypad(pad, state);
                writeln!(writer, "{first}CaseClauses: {self}")?;
                lst.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                item.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl CaseClauses {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> ParseResult<Self> {
        let (item, after_item) = CaseClause::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        let mut current = Rc::new(CaseClauses::Item(item));
        let mut current_scanner = after_item;
        while let Ok((next, after_next)) =
            CaseClause::parse(parser, current_scanner, yield_flag, await_flag, return_flag)
        {
            current = Rc::new(CaseClauses::List(current, next));
            current_scanner = after_next;
        }
        Ok((current, current_scanner))
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            CaseClauses::Item(node) => node.var_declared_names(),
            CaseClauses::List(lst, item) => {
                let mut names = lst.var_declared_names();
                names.extend(item.var_declared_names());
                names
            }
        }
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        let (list, item) = match self {
            CaseClauses::Item(item) => (None, item),
            CaseClauses::List(lst, item) => (Some(lst), item),
        };
        let mut result = if let Some(list) = list { list.lexically_declared_names() } else { vec![] };
        result.extend(item.lexically_declared_names());
        result
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            CaseClauses::Item(node) => node.contains_undefined_break_target(label_set),
            CaseClauses::List(lst, item) => {
                lst.contains_undefined_break_target(label_set) || item.contains_undefined_break_target(label_set)
            }
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            CaseClauses::Item(node) => node.contains(kind),
            CaseClauses::List(lst, item) => lst.contains(kind) || item.contains(kind),
        }
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self {
            CaseClauses::Item(node) => node.contains_duplicate_labels(label_set),
            CaseClauses::List(lst, item) => {
                lst.contains_duplicate_labels(label_set) || item.contains_duplicate_labels(label_set)
            }
        }
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        match self {
            CaseClauses::Item(node) => node.contains_undefined_continue_target(iteration_set),
            CaseClauses::List(lst, item) => {
                lst.contains_undefined_continue_target(iteration_set)
                    || item.contains_undefined_continue_target(iteration_set)
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
            CaseClauses::Item(node) => node.all_private_identifiers_valid(names),
            CaseClauses::List(node1, node2) => {
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
            CaseClauses::Item(cc) => cc.contains_arguments(),
            CaseClauses::List(ccs, cc) => ccs.contains_arguments() || cc.contains_arguments(),
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool, within_iteration: bool) {
        let (list, item) = match self {
            CaseClauses::Item(node) => (None, node),
            CaseClauses::List(list, node) => (Some(list), node),
        };
        if let Some(list) = list {
            list.early_errors(errs, strict, within_iteration);
        }
        item.early_errors(errs, strict, within_iteration);
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        match self {
            CaseClauses::Item(cc) => cc.var_scoped_declarations(),
            CaseClauses::List(ccl, cc) => {
                let mut list = ccl.var_scoped_declarations();
                list.extend(cc.var_scoped_declarations());
                list
            }
        }
    }

    pub fn lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        match self {
            CaseClauses::Item(cc) => cc.lexically_scoped_declarations(),
            CaseClauses::List(clauses, clause) => {
                let mut list = clauses.lexically_scoped_declarations();
                list.extend(clause.lexically_scoped_declarations());
                list
            }
        }
    }

    pub fn to_vec(&self) -> Vec<Rc<CaseClause>> {
        match self {
            CaseClauses::Item(item) => vec![item.clone()],
            CaseClauses::List(list, item) => {
                let mut result = list.to_vec();
                result.push(item.clone());
                result
            }
        }
    }

    pub fn location(&self) -> Location {
        match self {
            CaseClauses::Item(case_clause) => case_clause.location(),
            CaseClauses::List(case_clauses, case_clause) => case_clauses.location().merge(&case_clause.location()),
        }
    }

    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                CaseClauses::Item(case_clause) => case_clause.body_containing_location(location),
                CaseClauses::List(case_clauses, case_clause) => case_clauses
                    .body_containing_location(location)
                    .or_else(|| case_clause.body_containing_location(location)),
            }
        } else {
            None
        }
    }

    pub fn has_call_in_tail_position(&self, location: &Location) -> bool {
        // Static Semantics: HasCallInTailPosition
        // The syntax-directed operation HasCallInTailPosition takes argument call (a CallExpression Parse Node, a
        // MemberExpression Parse Node, or an OptionalChain Parse Node) and returns a Boolean.
        //
        // Note 1: call is a Parse Node that represents a specific range of source text. When the following algorithms
        //         compare call to another Parse Node, it is a test of whether they represent the same source text.
        //
        // Note 2: A potential tail position call that is immediately followed by return GetValue of the call result is
        //         also a possible tail position call. A function call cannot return a Reference Record, so such a
        //         GetValue operation will always return the same value as the actual function call result.
        //
        match self {
            CaseClauses::Item(case_clause) => case_clause.has_call_in_tail_position(location),
            CaseClauses::List(case_clauses, case_clause) => {
                case_clauses.has_call_in_tail_position(location) || case_clause.has_call_in_tail_position(location)
            }
        }
    }
}

// CaseClause[Yield, Await, Return] :
//      case Expression[+In, ?Yield, ?Await] : StatementList[?Yield, ?Await, ?Return]opt
#[derive(Debug)]
pub struct CaseClause {
    pub expression: Rc<Expression>,
    pub statements: Option<Rc<StatementList>>,
    pub location: Location,
}

impl fmt::Display for CaseClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.statements {
            None => write!(f, "case {} :", self.expression),
            Some(s) => write!(f, "case {} : {}", self.expression, s),
        }
    }
}

impl PrettyPrint for CaseClause {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}CaseClause: {self}")?;
        match &self.statements {
            None => self.expression.pprint_with_leftpad(writer, &successive, Spot::Final),
            Some(s) => {
                self.expression.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                s.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}CaseClause: {self}")?;
        pprint_token(writer, "case", TokenType::Keyword, &successive, Spot::NotFinal)?;
        self.expression.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        match &self.statements {
            Some(s) => {
                pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                s.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            None => pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::Final),
        }
    }
}

impl CaseClause {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> ParseResult<Self> {
        let (loc_start, after_case) =
            scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Case)?;
        let (exp, after_exp) = Expression::parse(parser, after_case, true, yield_flag, await_flag)?;
        let (loc_colon, after_colon) =
            scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::Colon)?;
        let (stmt, after_stmt) = match StatementList::parse(parser, after_colon, yield_flag, await_flag, return_flag) {
            Err(_) => (None, after_colon),
            Ok((stmt, s)) => (Some(stmt), s),
        };
        let after_loc = stmt.as_ref().map_or(loc_colon, |s| s.location());
        let location = loc_start.merge(&after_loc);
        Ok((Rc::new(CaseClause { expression: exp, statements: stmt, location }), after_stmt))
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match &self.statements {
            None => vec![],
            Some(s) => s.var_declared_names(),
        }
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        if let Some(stmt) = &self.statements { stmt.lexically_declared_names() } else { vec![] }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match &self.statements {
            None => false,
            Some(s) => s.contains_undefined_break_target(label_set),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.expression.contains(kind) || self.statements.as_ref().is_some_and(|n| n.contains(kind))
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match &self.statements {
            None => false,
            Some(s) => s.contains_duplicate_labels(label_set),
        }
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        match &self.statements {
            None => false,
            Some(s) => s.contains_undefined_continue_target(iteration_set, &[]),
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.expression.all_private_identifiers_valid(names)
            && match &self.statements {
                None => true,
                Some(node) => node.all_private_identifiers_valid(names),
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
        self.expression.contains_arguments() || self.statements.as_ref().is_some_and(|s| s.contains_arguments())
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool, within_iteration: bool) {
        self.expression.early_errors(errs, strict);
        if let Some(stmt) = &self.statements {
            stmt.early_errors(errs, strict, within_iteration, true);
        }
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        if let Some(sl) = &self.statements { sl.var_scoped_declarations() } else { vec![] }
    }

    pub fn lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        match &self.statements {
            Some(sl) => sl.lexically_scoped_declarations(),
            None => vec![],
        }
    }

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            if let Some(statements) = &self.statements { statements.body_containing_location(location) } else { None }
        } else {
            None
        }
    }

    pub fn has_call_in_tail_position(&self, location: &Location) -> bool {
        // Static Semantics: HasCallInTailPosition
        // The syntax-directed operation HasCallInTailPosition takes argument call (a CallExpression Parse Node, a
        // MemberExpression Parse Node, or an OptionalChain Parse Node) and returns a Boolean.
        //
        // Note 1: call is a Parse Node that represents a specific range of source text. When the following algorithms
        //         compare call to another Parse Node, it is a test of whether they represent the same source text.
        //
        // Note 2: A potential tail position call that is immediately followed by return GetValue of the call result is
        //         also a possible tail position call. A function call cannot return a Reference Record, so such a
        //         GetValue operation will always return the same value as the actual function call result.
        //
        self.statements.as_ref().is_some_and(|sl| sl.has_call_in_tail_position(location))
    }
}

// DefaultClause[Yield, Await, Return] :
//      default : StatementList[?Yield, ?Await, ?Return]opt
#[derive(Debug)]
pub struct DefaultClause {
    pub list: Option<Rc<StatementList>>,
    location: Location,
}

impl fmt::Display for DefaultClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DefaultClause { list: None, .. } => write!(f, "default :"),
            DefaultClause { list: Some(sl), .. } => write!(f, "default : {sl}"),
        }
    }
}

impl PrettyPrint for DefaultClause {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}DefaultClause: {self}")?;
        match self {
            DefaultClause { list: None, .. } => Ok(()),
            DefaultClause { list: Some(sl), .. } => sl.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}DefaultClause: {self}")?;
        pprint_token(writer, "default", TokenType::Keyword, &successive, Spot::NotFinal)?;
        match self {
            DefaultClause { list: None, .. } => {
                pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::Final)
            }
            DefaultClause { list: Some(sl), .. } => {
                pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::NotFinal)?;
                sl.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl DefaultClause {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> ParseResult<Self> {
        let (start_loc, after_def) =
            scan_for_keyword(scanner, parser.source, ScanGoal::InputElementDiv, Keyword::Default)?;
        let (colon_loc, after_colon) =
            scan_for_punct(after_def, parser.source, ScanGoal::InputElementDiv, Punctuator::Colon)?;
        let (sl, after_sl) = match StatementList::parse(parser, after_colon, yield_flag, await_flag, return_flag) {
            Err(_) => (None, after_colon),
            Ok((lst, scan)) => (Some(lst), scan),
        };
        let location = start_loc.merge(&sl.as_ref().map_or(colon_loc, |sl| sl.location()));
        Ok((Rc::new(DefaultClause { list: sl, location }), after_sl))
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            DefaultClause { list: None, .. } => vec![],
            DefaultClause { list: Some(sl), .. } => sl.var_declared_names(),
        }
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        match &self.list {
            None => vec![],
            Some(stmt) => stmt.lexically_declared_names(),
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            DefaultClause { list: None, .. } => false,
            DefaultClause { list: Some(sl), .. } => sl.contains_undefined_break_target(label_set),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.list.as_ref().is_some_and(|n| n.contains(kind))
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self {
            DefaultClause { list: None, .. } => false,
            DefaultClause { list: Some(sl), .. } => sl.contains_duplicate_labels(label_set),
        }
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        match self {
            DefaultClause { list: None, .. } => false,
            DefaultClause { list: Some(sl), .. } => sl.contains_undefined_continue_target(iteration_set, &[]),
        }
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        if let Some(node) = &self.list { node.all_private_identifiers_valid(names) } else { true }
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
        self.list.as_ref().is_some_and(|sl| sl.contains_arguments())
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool, within_iteration: bool) {
        if let Some(stmt) = &self.list {
            stmt.early_errors(errs, strict, within_iteration, true);
        }
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        if let Some(stmt) = &self.list { stmt.var_scoped_declarations() } else { vec![] }
    }

    pub fn lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        match &self.list {
            Some(stmt) => stmt.lexically_scoped_declarations(),
            None => vec![],
        }
    }

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            self.list.as_ref().and_then(|item| item.body_containing_location(location))
        } else {
            None
        }
    }

    pub fn has_call_in_tail_position(&self, location: &Location) -> bool {
        // Static Semantics: HasCallInTailPosition
        // The syntax-directed operation HasCallInTailPosition takes argument call (a CallExpression Parse Node, a
        // MemberExpression Parse Node, or an OptionalChain Parse Node) and returns a Boolean.
        //
        // Note 1: call is a Parse Node that represents a specific range of source text. When the following algorithms
        //         compare call to another Parse Node, it is a test of whether they represent the same source text.
        //
        // Note 2: A potential tail position call that is immediately followed by return GetValue of the call result is
        //         also a possible tail position call. A function call cannot return a Reference Record, so such a
        //         GetValue operation will always return the same value as the actual function call result.
        //
        self.list.as_ref().is_some_and(|sl| sl.has_call_in_tail_position(location))
    }
}

#[cfg(test)]
mod tests;
