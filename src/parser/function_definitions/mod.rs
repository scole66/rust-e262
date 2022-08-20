use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// FunctionDeclaration[Yield, Await, Default] :
//      function BindingIdentifier[?Yield, ?Await] ( FormalParameters[~Yield, ~Await] ) { FunctionBody[~Yield, ~Await] }
//      [+Default] function ( FormalParameters[~Yield, ~Await] ) { FunctionBody[~Yield, ~Await] }
#[derive(Debug)]
pub struct FunctionDeclaration {
    pub ident: Option<Rc<BindingIdentifier>>,
    pub params: Rc<FormalParameters>,
    pub body: Rc<FunctionBody>,
    location: Location,
}

impl fmt::Display for FunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            None => write!(f, "function ( {} ) {{ {} }}", self.params, self.body),
            Some(id) => {
                write!(f, "function {} ( {} ) {{ {} }}", id, self.params, self.body)
            }
        }
    }
}

impl PrettyPrint for FunctionDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionDeclaration: {}", first, self)?;
        if let Some(id) = &self.ident {
            id.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        self.params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.body.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionDeclaration: {}", first, self)?;
        pprint_token(writer, "function", TokenType::Keyword, &successive, Spot::NotFinal)?;
        if let Some(id) = &self.ident {
            id.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.params.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl FunctionDeclaration {
    fn parse_core(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        default_flag: bool,
    ) -> ParseResult<Self> {
        let (func_loc, after_func) =
            scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Function)?;
        let (ident, after_bi) = match BindingIdentifier::parse(parser, after_func, yield_flag, await_flag) {
            Ok((node, scan)) => Ok((Some(node), scan)),
            Err(e) => {
                if default_flag {
                    Ok((None, after_func))
                } else {
                    Err(e)
                }
            }
        }?;
        let (_, after_lp) = scan_for_punct(after_bi, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (params, after_fp) = FormalParameters::parse(parser, after_lp, false, false);
        let (_, after_rp) = scan_for_punct(after_fp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let (_, after_lb) = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (body, after_fb) = FunctionBody::parse(parser, after_lb, false, false);
        let (rb_loc, after_rb) =
            scan_for_punct(after_fb, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        let location = func_loc.merge(&rb_loc);
        Ok((Rc::new(FunctionDeclaration { ident, params, body, location }), after_rb))
    }

    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        default_flag: bool,
    ) -> ParseResult<Self> {
        let key = YieldAwaitDefaultKey { scanner, yield_flag, await_flag, default_flag };
        match parser.function_declaration_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag, default_flag);
                parser.function_declaration_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        vec![self.bound_name()]
    }

    pub fn bound_name(&self) -> JSString {
        match &self.ident {
            None => JSString::from("*default*"),
            Some(node) => node.bound_name(),
        }
    }

    pub fn contains(&self, _kind: ParseNodeKind) -> bool {
        false
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.params.all_private_identifiers_valid(names) && self.body.all_private_identifiers_valid(names)
    }

    pub fn early_errors(&self, agent: &Agent, errs: &mut Vec<Object>, strict: bool) {
        function_early_errors(agent, errs, strict, self.ident.as_ref(), &self.params, &self.body);
    }

    pub fn is_constant_declaration(&self) -> bool {
        false
    }
}

pub fn function_early_errors(
    agent: &Agent,
    errs: &mut Vec<Object>,
    strict: bool,
    ident: Option<&Rc<BindingIdentifier>>,
    params: &Rc<FormalParameters>,
    body: &Rc<FunctionBody>,
) {
    // Static Semantics: Early Errors
    //  FunctionDeclaration :
    //      function BindingIdentifier ( FormalParameters ) { FunctionBody }
    //      function ( FormalParameters ) { FunctionBody }
    //  FunctionExpression :
    //      function BindingIdentifier ( FormalParameters ) { FunctionBody }
    //      function ( FormalParameters ) { FunctionBody }
    //
    //  * If the source text matched by FormalParameters is strict mode code, the Early Error rules for
    //    UniqueFormalParameters : FormalParameters are applied.
    //  * If BindingIdentifier is present and the source text matched by BindingIdentifier is strict mode code, it
    //    is a Syntax Error if the StringValue of BindingIdentifier is "eval" or "arguments". [Redundant check]
    //  * It is a Syntax Error if FunctionBodyContainsUseStrict of FunctionBody is true and IsSimpleParameterList
    //    of FormalParameters is false.
    //  * It is a Syntax Error if any element of the BoundNames of FormalParameters also occurs in the
    //    LexicallyDeclaredNames of FunctionBody.
    //  * It is a Syntax Error if FormalParameters Contains SuperProperty is true.
    //  * It is a Syntax Error if FunctionBody Contains SuperProperty is true.
    //  * It is a Syntax Error if FormalParameters Contains SuperCall is true.
    //  * It is a Syntax Error if FunctionBody Contains SuperCall is true.

    let strict_function = strict || body.function_body_contains_use_strict();

    let bn = params.bound_names();

    if strict_function {
        for name in duplicates(&bn) {
            errs.push(create_syntax_error_object(
                agent,
                format!("‘{}’ already defined", name),
                Some(params.location()),
            ));
        }
    }

    if body.function_body_contains_use_strict() && !params.is_simple_parameter_list() {
        errs.push(create_syntax_error_object(
            agent,
            "Illegal 'use strict' directive in function with non-simple parameter list",
            Some(body.location()),
        ));
    }

    let lexnames = body.lexically_declared_names();
    for lexname in lexnames {
        if bn.contains(&lexname) {
            errs.push(create_syntax_error_object(
                agent,
                format!("‘{}’ already defined", lexname),
                Some(body.location()),
            ));
        }
    }

    if params.contains(ParseNodeKind::SuperProperty)
        || params.contains(ParseNodeKind::SuperCall)
        || body.contains(ParseNodeKind::SuperProperty)
        || body.contains(ParseNodeKind::SuperCall)
    {
        errs.push(create_syntax_error_object(agent, "‘super’ not allowed here", Some(params.location())));
    }

    if let Some(ident) = ident {
        ident.early_errors(agent, errs, strict_function);
    }
    params.early_errors(agent, errs, strict_function, strict_function);
    body.early_errors(agent, errs, strict_function);
}

// FunctionExpression :
//      function BindingIdentifier[~Yield, ~Await]opt ( FormalParameters[~Yield, ~Await] ) { FunctionBody[~Yield, ~Await] }
#[derive(Debug)]
pub struct FunctionExpression {
    pub ident: Option<Rc<BindingIdentifier>>,
    pub params: Rc<FormalParameters>,
    pub body: Rc<FunctionBody>,
    location: Location,
}

impl fmt::Display for FunctionExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ident {
            None => write!(f, "function ( {} ) {{ {} }}", self.params, self.body),
            Some(id) => {
                write!(f, "function {} ( {} ) {{ {} }}", id, self.params, self.body)
            }
        }
    }
}

impl PrettyPrint for FunctionExpression {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionExpression: {}", first, self)?;
        if let Some(id) = &self.ident {
            id.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        self.params.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.body.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionExpression: {}", first, self)?;
        pprint_token(writer, "function", TokenType::Keyword, &successive, Spot::NotFinal)?;
        if let Some(id) = &self.ident {
            id.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        }
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.params.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        pprint_token(writer, "{", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.body.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, "}", TokenType::Punctuator, &successive, Spot::Final)
    }
}

impl IsFunctionDefinition for FunctionExpression {
    fn is_function_definition(&self) -> bool {
        true
    }
}

impl FunctionExpression {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (func_loc, after_func) =
            scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::Function)?;
        let (bi, after_bi) = match BindingIdentifier::parse(parser, after_func, false, false) {
            Ok((node, scan)) => (Some(node), scan),
            Err(_) => (None, after_func),
        };
        let (_, after_lp) = scan_for_punct(after_bi, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (fp, after_fp) = FormalParameters::parse(parser, after_lp, false, false);
        let (_, after_rp) = scan_for_punct(after_fp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let (_, after_lb) = scan_for_punct(after_rp, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftBrace)?;
        let (fb, after_fb) = FunctionBody::parse(parser, after_lb, false, false);
        let (rb_loc, after_rb) =
            scan_for_punct(after_fb, parser.source, ScanGoal::InputElementDiv, Punctuator::RightBrace)?;
        Ok((
            Rc::new(FunctionExpression { ident: bi, params: fp, body: fb, location: func_loc.merge(&rb_loc) }),
            after_rb,
        ))
    }

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn contains(&self, _kind: ParseNodeKind) -> bool {
        false
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.params.all_private_identifiers_valid(names) && self.body.all_private_identifiers_valid(names)
    }

    pub fn early_errors(&self, agent: &Agent, errs: &mut Vec<Object>, strict: bool) {
        function_early_errors(agent, errs, strict, self.ident.as_ref(), &self.params, &self.body);
    }

    pub fn is_named_function(&self) -> bool {
        self.ident.is_some()
    }
}

// FunctionBody[Yield, Await] :
//      FunctionStatementList[?Yield, ?Await]
#[derive(Debug)]
pub struct FunctionBody {
    pub statements: Rc<FunctionStatementList>,
}

impl fmt::Display for FunctionBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.statements.fmt(f)
    }
}

impl PrettyPrint for FunctionBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionBody: {}", first, self)?;
        self.statements.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.statements.concise_with_leftpad(writer, pad, state)
    }
}

impl FunctionBody {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> (Rc<Self>, Scanner) {
        // Can never return an error
        let (fsl, after_fsl) = FunctionStatementList::parse(parser, scanner, yield_flag, await_flag);
        (Rc::new(FunctionBody { statements: fsl }), after_fsl)
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> (Rc<Self>, Scanner) {
        let key = YieldAwaitKey { scanner, yield_flag, await_flag };
        match parser.function_body_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag);
                parser.function_body_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn location(&self) -> Location {
        self.statements.location()
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.statements.contains(kind)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.statements.all_private_identifiers_valid(names)
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
        self.statements.contains_arguments()
    }

    pub fn directive_prologue(&self) -> Vec<StringToken> {
        self.statements.initial_string_tokens()
    }

    pub fn function_body_contains_use_strict(&self) -> bool {
        // Static Semantics: FunctionBodyContainsUseStrict
        // FunctionBody : FunctionStatementList
        //  1. If the Directive Prologue of FunctionBody contains a Use Strict Directive, return true; otherwise, return false.
        let prologue = self.directive_prologue();
        let needle = JSString::from("use strict");
        prologue.iter().any(|string_tok| string_tok.raw.is_none() && string_tok.value == needle)
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        // Static Semantics: LexicallyDeclaredNames
        //      FunctionBody : FunctionStatementList
        //  1. Return LexicallyDeclaredNames of FunctionStatementList.
        self.statements.lexically_declared_names()
    }

    pub fn early_errors(&self, agent: &Agent, errs: &mut Vec<Object>, strict: bool) {
        // FunctionBody : FunctionStatementList
        //  * It is a Syntax Error if the LexicallyDeclaredNames of FunctionStatementList contains any duplicate
        //    entries.
        //  * It is a Syntax Error if any element of the LexicallyDeclaredNames of FunctionStatementList also occurs in
        //    the VarDeclaredNames of FunctionStatementList.
        //  * It is a Syntax Error if ContainsDuplicateLabels of FunctionStatementList with argument « » is true.
        //  * It is a Syntax Error if ContainsUndefinedBreakTarget of FunctionStatementList with argument « » is true.
        //  * It is a Syntax Error if ContainsUndefinedContinueTarget of FunctionStatementList with arguments « » and
        //    « » is true.
        let ldn = self.statements.lexically_declared_names();
        for name in duplicates(&ldn) {
            errs.push(create_syntax_error_object(
                agent,
                format!("‘{}’ already defined", name),
                Some(self.statements.location()),
            ));
        }
        let vdn = self.statements.var_declared_names();
        for name in vdn {
            if ldn.contains(&name) {
                errs.push(create_syntax_error_object(
                    agent,
                    format!("‘{}’ cannot be used in a var statement, as it is also lexically declared", name),
                    Some(self.statements.location()),
                ));
            }
        }
        if self.statements.contains_duplicate_labels(&[]) {
            errs.push(create_syntax_error_object(agent, "duplicate labels detected", Some(self.statements.location())));
        }
        if self.statements.contains_undefined_break_target(&[]) {
            errs.push(create_syntax_error_object(
                agent,
                "undefined break target detected",
                Some(self.statements.location()),
            ));
        }
        if self.statements.contains_undefined_continue_target(&[], &[]) {
            errs.push(create_syntax_error_object(
                agent,
                "undefined continue target detected",
                Some(self.statements.location()),
            ));
        }

        self.statements.early_errors(agent, errs, strict);
    }

    /// Return a list of identifiers defined by the `var` statement for this node.
    ///
    /// Note that function bodies are treated like top-level code in that top-level function identifiers are part
    /// of the var-declared list.
    ///
    /// See [VarDeclaredNames](https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames) from ECMA-262.
    pub fn var_declared_names(&self) -> Vec<JSString> {
        self.statements.var_declared_names()
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        self.statements.var_scoped_declarations()
    }

    pub fn lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        self.statements.lexically_scoped_declarations()
    }
}

// FunctionStatementList[Yield, Await] :
//      StatementList[?Yield, ?Await, +Return]opt
#[derive(Debug)]
pub enum FunctionStatementList {
    Statements(Rc<StatementList>),
    Empty(Location),
}

impl fmt::Display for FunctionStatementList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FunctionStatementList::Empty(_) => Ok(()),
            FunctionStatementList::Statements(s) => s.fmt(f),
        }
    }
}

impl PrettyPrint for FunctionStatementList {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}FunctionStatementList: {}", first, self)?;
        match self {
            FunctionStatementList::Empty(_) => Ok(()),
            FunctionStatementList::Statements(s) => s.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            FunctionStatementList::Empty(_) => Ok(()),
            FunctionStatementList::Statements(s) => s.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl FunctionStatementList {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> (Rc<Self>, Scanner) {
        // Can never return an error.
        let (stmts, after_stmts) = match StatementList::parse(parser, scanner, yield_flag, await_flag, true) {
            Err(_) => (FunctionStatementList::Empty(Location::from(scanner)), scanner),
            Ok((st, s)) => (FunctionStatementList::Statements(st), s),
        };
        (Rc::new(stmts), after_stmts)
    }

    pub fn location(&self) -> Location {
        match self {
            FunctionStatementList::Statements(s) => s.location(),
            FunctionStatementList::Empty(loc) => *loc,
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            FunctionStatementList::Statements(n) => n.contains(kind),
            FunctionStatementList::Empty(_) => false,
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
            FunctionStatementList::Statements(n) => n.all_private_identifiers_valid(names),
            FunctionStatementList::Empty(_) => true,
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
            FunctionStatementList::Statements(sl) => sl.contains_arguments(),
            FunctionStatementList::Empty(_) => false,
        }
    }

    pub fn initial_string_tokens(&self) -> Vec<StringToken> {
        match self {
            FunctionStatementList::Statements(statement_list) => statement_list.initial_string_tokens(),
            FunctionStatementList::Empty(_) => vec![],
        }
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        // Static Semantics: LexicallyDeclaredNames
        match self {
            FunctionStatementList::Statements(statement_list) => {
                // FunctionStatementList : StatementList
                //  1. Return TopLevelLexicallyDeclaredNames of StatementList.
                statement_list.top_level_lexically_declared_names()
            }
            FunctionStatementList::Empty(_) => {
                // FunctionStatementList : [empty]
                //  1. Return a new empty List.
                vec![]
            }
        }
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        // Static Semantics: VarDeclaredNames
        match self {
            FunctionStatementList::Statements(statement_list) => {
                // FunctionStatementList : StatementList
                //  1. Return TopLevelVarDeclaredNames of StatementList.
                statement_list.top_level_var_declared_names()
            }
            FunctionStatementList::Empty(_) => {
                // FunctionStatementList : [empty]
                //  1. Return a new empty List.
                vec![]
            }
        }
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self {
            FunctionStatementList::Statements(sl) => sl.contains_duplicate_labels(label_set),
            FunctionStatementList::Empty(_) => false,
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            FunctionStatementList::Statements(sl) => sl.contains_undefined_break_target(label_set),
            FunctionStatementList::Empty(_) => false,
        }
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString], label_set: &[JSString]) -> bool {
        match self {
            FunctionStatementList::Statements(sl) => sl.contains_undefined_continue_target(iteration_set, label_set),
            FunctionStatementList::Empty(_) => false,
        }
    }

    pub fn early_errors(&self, agent: &Agent, errs: &mut Vec<Object>, strict: bool) {
        if let FunctionStatementList::Statements(sl) = self {
            sl.early_errors(agent, errs, strict, false, false);
        }
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        match self {
            FunctionStatementList::Statements(s) => s.top_level_var_scoped_declarations(),
            FunctionStatementList::Empty(_) => vec![],
        }
    }

    pub fn lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        match self {
            FunctionStatementList::Statements(s) => s.top_level_lexically_scoped_declarations(),
            FunctionStatementList::Empty(_) => vec![],
        }
    }
}

#[cfg(test)]
mod tests;
