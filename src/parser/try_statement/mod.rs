use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// TryStatement[Yield, Await, Return] :
//      try Block[?Yield, ?Await, ?Return] Catch[?Yield, ?Await, ?Return]
//      try Block[?Yield, ?Await, ?Return] Finally[?Yield, ?Await, ?Return]
//      try Block[?Yield, ?Await, ?Return] Catch[?Yield, ?Await, ?Return] Finally[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub(crate) enum TryStatement {
    Catch { block: Rc<Block>, catch: Rc<Catch>, location: Location },
    Finally { block: Rc<Block>, finally: Rc<Finally>, location: Location },
    Full { block: Rc<Block>, catch: Rc<Catch>, finally: Rc<Finally>, location: Location },
}

impl fmt::Display for TryStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TryStatement::Catch { block, catch, .. } => write!(f, "try {block} {catch}"),
            TryStatement::Finally { block, finally, .. } => {
                write!(f, "try {block} {finally}")
            }
            TryStatement::Full { block, catch, finally, .. } => {
                write!(f, "try {block} {catch} {finally}")
            }
        }
    }
}

impl PrettyPrint for TryStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}TryStatement: {self}")?;
        match self {
            TryStatement::Catch { block, catch, .. } => {
                block.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                catch.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            TryStatement::Finally { block, finally, .. } => {
                block.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                finally.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            TryStatement::Full { block, catch, finally, .. } => {
                block.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                catch.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                finally.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}TryStatement: {self}")?;
        pprint_token(writer, "try", TokenType::Keyword, &successive, Spot::NotFinal)?;
        match self {
            TryStatement::Catch { block, catch, .. } => {
                block.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                catch.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            TryStatement::Finally { block, finally, .. } => {
                block.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                finally.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            TryStatement::Full { block, catch, finally, .. } => {
                block.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                catch.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                finally.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl TryStatement {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> ParseResult<Self> {
        enum CaseKind {
            Catch(Rc<Catch>),
            Finally(Rc<Finally>),
            Full(Rc<Catch>, Rc<Finally>),
        }

        let (try_loc, after_try) = scan_for_keyword(scanner, parser.source, InputElementGoal::RegExp, Keyword::Try)?;
        let (block, after_block) = Block::parse(parser, after_try, yield_flag, await_flag, return_flag)?;
        Err(ParseError::new(PECode::TryBlockError, after_block))
            .otherwise(|| {
                let (fin, after_fin) = Finally::parse(parser, after_block, yield_flag, await_flag, return_flag)?;
                Ok((CaseKind::Finally(fin), after_fin))
            })
            .otherwise(|| {
                let (catch, after_catch) = Catch::parse(parser, after_block, yield_flag, await_flag, return_flag)?;
                match Finally::parse(parser, after_catch, yield_flag, await_flag, return_flag) {
                    Err(_) => Ok((CaseKind::Catch(catch), after_catch)),
                    Ok((fin, after_fin)) => Ok((CaseKind::Full(catch, fin), after_fin)),
                }
            })
            .map(|(kind, scan)| {
                (
                    Rc::new(match kind {
                        CaseKind::Catch(catch) => {
                            let location = try_loc.merge(&catch.location());
                            TryStatement::Catch { block, catch, location }
                        }
                        CaseKind::Finally(finally) => {
                            let location = try_loc.merge(&finally.location());
                            TryStatement::Finally { block, finally, location }
                        }
                        CaseKind::Full(catch, finally) => {
                            let location = try_loc.merge(&finally.location());
                            TryStatement::Full { block, catch, finally, location }
                        }
                    }),
                    scan,
                )
            })
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            TryStatement::Catch { location, .. }
            | TryStatement::Finally { location, .. }
            | TryStatement::Full { location, .. } => *location,
        }
    }

    pub(crate) fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            TryStatement::Catch { block, catch, .. } => {
                let mut names = block.var_declared_names();
                names.extend(catch.var_declared_names());
                names
            }
            TryStatement::Finally { block, finally, .. } => {
                let mut names = block.var_declared_names();
                names.extend(finally.var_declared_names());
                names
            }
            TryStatement::Full { block, catch, finally, .. } => {
                let mut names = block.var_declared_names();
                names.extend(catch.var_declared_names());
                names.extend(finally.var_declared_names());
                names
            }
        }
    }

    pub(crate) fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            TryStatement::Catch { block, catch, .. } => {
                block.contains_undefined_break_target(label_set) || catch.contains_undefined_break_target(label_set)
            }
            TryStatement::Finally { block, finally, .. } => {
                block.contains_undefined_break_target(label_set) || finally.contains_undefined_break_target(label_set)
            }
            TryStatement::Full { block, catch, finally, .. } => {
                block.contains_undefined_break_target(label_set)
                    || catch.contains_undefined_break_target(label_set)
                    || finally.contains_undefined_break_target(label_set)
            }
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            TryStatement::Catch { block, catch, .. } => block.contains(kind) || catch.contains(kind),
            TryStatement::Finally { block, finally, .. } => block.contains(kind) || finally.contains(kind),
            TryStatement::Full { block, catch, finally, .. } => {
                block.contains(kind) || catch.contains(kind) || finally.contains(kind)
            }
        }
    }

    pub(crate) fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self {
            TryStatement::Catch { block, catch, .. } => {
                block.contains_duplicate_labels(label_set) || catch.contains_duplicate_labels(label_set)
            }
            TryStatement::Finally { block, finally, .. } => {
                block.contains_duplicate_labels(label_set) || finally.contains_duplicate_labels(label_set)
            }
            TryStatement::Full { block, catch, finally, .. } => {
                block.contains_duplicate_labels(label_set)
                    || catch.contains_duplicate_labels(label_set)
                    || finally.contains_duplicate_labels(label_set)
            }
        }
    }

    pub(crate) fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        match self {
            TryStatement::Catch { block, catch, .. } => {
                block.contains_undefined_continue_target(iteration_set, &[])
                    || catch.contains_undefined_continue_target(iteration_set)
            }
            TryStatement::Finally { block, finally, .. } => {
                block.contains_undefined_continue_target(iteration_set, &[])
                    || finally.contains_undefined_continue_target(iteration_set)
            }
            TryStatement::Full { block, catch, finally, .. } => {
                block.contains_undefined_continue_target(iteration_set, &[])
                    || catch.contains_undefined_continue_target(iteration_set)
                    || finally.contains_undefined_continue_target(iteration_set)
            }
        }
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        match self {
            TryStatement::Catch { block, catch, .. } => {
                block.all_private_identifiers_valid(names) && catch.all_private_identifiers_valid(names)
            }
            TryStatement::Finally { block, finally, .. } => {
                block.all_private_identifiers_valid(names) && finally.all_private_identifiers_valid(names)
            }
            TryStatement::Full { block, catch, finally, .. } => {
                block.all_private_identifiers_valid(names)
                    && catch.all_private_identifiers_valid(names)
                    && finally.all_private_identifiers_valid(names)
            }
        }
    }

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub(crate) fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        match self {
            TryStatement::Catch { block, catch, .. } => block.contains_arguments() || catch.contains_arguments(),
            TryStatement::Finally { block, finally, .. } => block.contains_arguments() || finally.contains_arguments(),
            TryStatement::Full { block, catch, finally, .. } => {
                block.contains_arguments() || catch.contains_arguments() || finally.contains_arguments()
            }
        }
    }

    pub(crate) fn early_errors(
        &self,
        errs: &mut Vec<Object>,
        strict: bool,
        within_iteration: bool,
        within_switch: bool,
    ) {
        let (block, catch, finally) = match self {
            TryStatement::Catch { block, catch, .. } => (block, Some(catch), None),
            TryStatement::Finally { block, finally, .. } => (block, None, Some(finally)),
            TryStatement::Full { block, catch, finally, .. } => (block, Some(catch), Some(finally)),
        };
        block.early_errors(errs, strict, within_iteration, within_switch);
        if let Some(catch) = catch {
            catch.early_errors(errs, strict, within_iteration, within_switch);
        }
        if let Some(finally) = finally {
            finally.early_errors(errs, strict, within_iteration, within_switch);
        }
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub(crate) fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        let (block, catch, finally) = match self {
            TryStatement::Catch { block, catch, .. } => (block, Some(catch), None),
            TryStatement::Finally { block, finally, .. } => (block, None, Some(finally)),
            TryStatement::Full { block, catch, finally, .. } => (block, Some(catch), Some(finally)),
        };
        let mut list = block.var_scoped_declarations();
        if let Some(catch) = catch {
            list.extend(catch.var_scoped_declarations());
        }
        if let Some(finally) = finally {
            list.extend(finally.var_scoped_declarations());
        }
        list
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                TryStatement::Catch { block, catch, .. } => {
                    block.body_containing_location(location).or_else(|| catch.body_containing_location(location))
                }
                TryStatement::Finally { block, finally, .. } => {
                    block.body_containing_location(location).or_else(|| finally.body_containing_location(location))
                }
                TryStatement::Full { block, catch, finally, .. } => block
                    .body_containing_location(location)
                    .or_else(|| catch.body_containing_location(location))
                    .or_else(|| finally.body_containing_location(location)),
            }
        } else {
            None
        }
    }

    pub(crate) fn has_call_in_tail_position(&self, location: &Location) -> bool {
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
            TryStatement::Catch { catch, .. } => catch.has_call_in_tail_position(location),
            TryStatement::Finally { finally, .. } | TryStatement::Full { finally, .. } => {
                finally.has_call_in_tail_position(location)
            }
        }
    }
}

// Catch[Yield, Await, Return] :
//      catch ( CatchParameter[?Yield, ?Await] ) Block[?Yield, ?Await, ?Return]
//      catch Block[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub(crate) struct Catch {
    pub(crate) parameter: Option<Rc<CatchParameter>>,
    pub(crate) block: Rc<Block>,
    location: Location,
}

impl fmt::Display for Catch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.parameter {
            None => write!(f, "catch {}", self.block),
            Some(cp) => write!(f, "catch ( {} ) {}", cp, self.block),
        }
    }
}

impl PrettyPrint for Catch {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}Catch: {self}")?;
        if let Some(cp) = &self.parameter {
            cp.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        self.block.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}Catch: {self}")?;
        pprint_token(writer, "catch", TokenType::Keyword, &successive, Spot::NotFinal)?;
        if let Some(cp) = &self.parameter {
            pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
            cp.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
            pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        }
        self.block.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl Catch {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> ParseResult<Self> {
        let (catch_loc, after_catch) = scan_for_keyword(scanner, parser.source, InputElementGoal::Div, Keyword::Catch)?;
        Err(ParseError::new(
            PECode::OneOfPunctuatorExpected(vec![Punctuator::LeftParen, Punctuator::LeftBrace]),
            after_catch,
        ))
        .otherwise(|| {
            let (block, after_block) = Block::parse(parser, after_catch, yield_flag, await_flag, return_flag)?;
            let location = catch_loc.merge(&block.location());
            Ok((Rc::new(Catch { parameter: None, block, location }), after_block))
        })
        .otherwise(|| {
            let (_, after_open) =
                scan_for_punct(after_catch, parser.source, InputElementGoal::Div, Punctuator::LeftParen)?;
            let (cp, after_cp) = CatchParameter::parse(parser, after_open, yield_flag, await_flag)?;
            let (_, after_close) =
                scan_for_punct(after_cp, parser.source, InputElementGoal::Div, Punctuator::RightParen)?;
            let (block, after_block) = Block::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
            let location = catch_loc.merge(&block.location());
            Ok((Rc::new(Catch { parameter: Some(cp), block, location }), after_block))
        })
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }

    pub(crate) fn var_declared_names(&self) -> Vec<JSString> {
        self.block.var_declared_names()
    }

    pub(crate) fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        self.block.contains_undefined_break_target(label_set)
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        self.parameter.as_ref().is_some_and(|n| n.contains(kind)) || self.block.contains(kind)
    }

    pub(crate) fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        self.block.contains_duplicate_labels(label_set)
    }

    pub(crate) fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        self.block.contains_undefined_continue_target(iteration_set, &[])
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.parameter.as_ref().is_none_or(|n| n.all_private_identifiers_valid(names))
            && self.block.all_private_identifiers_valid(names)
    }

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub(crate) fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.parameter.as_ref().is_some_and(|cp| cp.contains_arguments()) || self.block.contains_arguments()
    }

    pub(crate) fn early_errors(
        &self,
        errs: &mut Vec<Object>,
        strict: bool,
        within_iteration: bool,
        within_switch: bool,
    ) {
        // Static Semantics: Early Errors
        //  Catch : catch ( CatchParameter ) Block
        //  * It is a Syntax Error if BoundNames of CatchParameter contains any duplicate elements.
        //  * It is a Syntax Error if any element of the BoundNames of CatchParameter also occurs in the LexicallyDeclaredNames of Block.
        //  * It is a Syntax Error if any element of the BoundNames of CatchParameter also occurs in the VarDeclaredNames of Block.

        if let Some(cp) = &self.parameter {
            let bn = cp.bound_names();
            let ldn = self.block.lexically_declared_names();
            let vdn = self.block.var_declared_names();
            for name in duplicates(&bn) {
                errs.push(create_syntax_error_object(format!("‘{name}’ already defined"), Some(self.block.location())));
            }
            for name in &bn {
                if ldn.contains(name) || vdn.contains(name) {
                    errs.push(create_syntax_error_object(
                        format!("‘{name}’ already defined"),
                        Some(self.block.location()),
                    ));
                }
            }
            cp.early_errors(errs, strict);
        }
        self.block.early_errors(errs, strict, within_iteration, within_switch);
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub(crate) fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        self.block.var_scoped_declarations()
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) { self.block.body_containing_location(location) } else { None }
    }

    pub(crate) fn has_call_in_tail_position(&self, location: &Location) -> bool {
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
        self.block.has_call_in_tail_position(location)
    }
}

// Finally[Yield, Await, Return] :
//      finally Block[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub(crate) struct Finally {
    pub(crate) block: Rc<Block>,
    location: Location,
}

impl fmt::Display for Finally {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "finally {}", self.block)
    }
}

impl PrettyPrint for Finally {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}Finally: {self}")?;
        self.block.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}Finally: {self}")?;
        pprint_token(writer, "finally", TokenType::Keyword, &successive, Spot::NotFinal)?;
        self.block.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl Finally {
    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> ParseResult<Self> {
        let (fin_loc, after_fin) = scan_for_keyword(scanner, parser.source, InputElementGoal::Div, Keyword::Finally)?;
        let (block, after_block) = Block::parse(parser, after_fin, yield_flag, await_flag, return_flag)?;
        let location = fin_loc.merge(&block.location());
        Ok((Rc::new(Finally { block, location }), after_block))
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }

    pub(crate) fn var_declared_names(&self) -> Vec<JSString> {
        self.block.var_declared_names()
    }

    pub(crate) fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        self.block.contains_undefined_break_target(label_set)
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        self.block.contains(kind)
    }

    pub(crate) fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        self.block.contains_duplicate_labels(label_set)
    }

    pub(crate) fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        self.block.contains_undefined_continue_target(iteration_set, &[])
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.block.all_private_identifiers_valid(names)
    }

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub(crate) fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        self.block.contains_arguments()
    }

    pub(crate) fn early_errors(
        &self,
        errs: &mut Vec<Object>,
        strict: bool,
        within_iteration: bool,
        within_switch: bool,
    ) {
        self.block.early_errors(errs, strict, within_iteration, within_switch);
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub(crate) fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        self.block.var_scoped_declarations()
    }

    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) { self.block.body_containing_location(location) } else { None }
    }

    pub(crate) fn has_call_in_tail_position(&self, location: &Location) -> bool {
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
        self.block.has_call_in_tail_position(location)
    }
}

// CatchParameter[Yield, Await] :
//      BindingIdentifier[?Yield, ?Await]
//      BindingPattern[?Yield, ?Await]
#[derive(Debug)]
pub(crate) enum CatchParameter {
    Ident(Rc<BindingIdentifier>),
    Pattern(Rc<BindingPattern>),
}

impl fmt::Display for CatchParameter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CatchParameter::Ident(node) => node.fmt(f),
            CatchParameter::Pattern(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for CatchParameter {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}CatchParameter: {self}")?;
        match self {
            CatchParameter::Ident(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            CatchParameter::Pattern(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            CatchParameter::Ident(node) => node.concise_with_leftpad(writer, pad, state),
            CatchParameter::Pattern(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl CatchParameter {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::CatchParameter), scanner))
            .otherwise(|| {
                let (bi, after_bi) = BindingIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(CatchParameter::Ident(bi)), after_bi))
            })
            .otherwise(|| {
                let (bp, after_bp) = BindingPattern::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(CatchParameter::Pattern(bp)), after_bp))
            })
    }

    pub(crate) fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> ParseResult<Self> {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.catch_parameter_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.catch_parameter_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub(crate) fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            CatchParameter::Ident(_) => false,
            CatchParameter::Pattern(node) => node.contains(kind),
        }
    }

    pub(crate) fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        match self {
            CatchParameter::Ident(_) => true,
            CatchParameter::Pattern(node) => node.all_private_identifiers_valid(names),
        }
    }

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub(crate) fn contains_arguments(&self) -> bool {
        // Static Semantics: ContainsArguments
        // The syntax-directed operation ContainsArguments takes no arguments and returns a Boolean.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If ContainsArguments of child is true, return true.
        //  2. Return false.
        match self {
            CatchParameter::Ident(_) => false,
            CatchParameter::Pattern(bp) => bp.contains_arguments(),
        }
    }

    pub(crate) fn bound_names(&self) -> Vec<JSString> {
        match self {
            CatchParameter::Ident(id) => id.bound_names(),
            CatchParameter::Pattern(pat) => pat.bound_names(),
        }
    }

    pub(crate) fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        match self {
            CatchParameter::Ident(id) => id.early_errors(errs, strict),
            CatchParameter::Pattern(pat) => pat.early_errors(errs, strict),
        }
    }

    #[cfg(test)]
    pub(crate) fn location(&self) -> Location {
        match self {
            CatchParameter::Ident(node) => node.location(),
            CatchParameter::Pattern(node) => node.location(),
        }
    }

    #[cfg(test)]
    pub(crate) fn body_containing_location(&self, location: &Location) -> Option<ContainingBody> {
        // Finds the FunctionBody, ConciseBody, or AsyncConciseBody that contains location most closely.
        if self.location().contains(location) {
            match self {
                CatchParameter::Ident(..) => None,
                CatchParameter::Pattern(bp) => bp.body_containing_location(location),
            }
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests;
