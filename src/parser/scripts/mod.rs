use super::*;
use ahash::AHashSet;
use std::fmt;
use std::hash::Hash;
use std::io::Result as IoResult;
use std::io::Write;

// Script :
//      ScriptBody opt
#[derive(Debug)]
pub struct Script {
    pub body: Option<Rc<ScriptBody>>,
    location: Location,
}

impl fmt::Display for Script {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.body {
            None => Ok(()),
            Some(n) => n.fmt(f),
        }
    }
}

impl PrettyPrint for Script {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}Script: {self}")?;
        if let Some(body) = &self.body { body.pprint_with_leftpad(writer, &successive, Spot::Final) } else { Ok(()) }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match &self.body {
            None => {
                let (first, _) = prettypad(pad, state);
                writeln!(writer, "{first}Script:")
            }
            Some(body) => body.concise_with_leftpad(writer, pad, state),
        }
    }
}

pub fn has_unique_elements<T>(iter: T) -> bool
where
    T: IntoIterator,
    T::Item: Eq + Hash,
{
    let mut uniq = AHashSet::new();
    iter.into_iter().all(move |x| uniq.insert(x))
}

#[derive(Debug, Clone)]
pub enum VarScopeDecl {
    VariableDeclaration(Rc<VariableDeclaration>),
    ForBinding(Rc<ForBinding>),
    FunctionDeclaration(Rc<FunctionDeclaration>),
    GeneratorDeclaration(Rc<GeneratorDeclaration>),
    AsyncFunctionDeclaration(Rc<AsyncFunctionDeclaration>),
    AsyncGeneratorDeclaration(Rc<AsyncGeneratorDeclaration>),
}
impl From<HoistableDeclPart> for VarScopeDecl {
    fn from(src: HoistableDeclPart) -> Self {
        match src {
            HoistableDeclPart::FunctionDeclaration(fd) => VarScopeDecl::FunctionDeclaration(fd),
            HoistableDeclPart::GeneratorDeclaration(gd) => VarScopeDecl::GeneratorDeclaration(gd),
            HoistableDeclPart::AsyncFunctionDeclaration(afd) => VarScopeDecl::AsyncFunctionDeclaration(afd),
            HoistableDeclPart::AsyncGeneratorDeclaration(agd) => VarScopeDecl::AsyncGeneratorDeclaration(agd),
        }
    }
}
impl fmt::Display for VarScopeDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VarScopeDecl::FunctionDeclaration(fd) => fd.fmt(f),
            VarScopeDecl::GeneratorDeclaration(gd) => gd.fmt(f),
            VarScopeDecl::AsyncFunctionDeclaration(afd) => afd.fmt(f),
            VarScopeDecl::AsyncGeneratorDeclaration(agd) => agd.fmt(f),
            VarScopeDecl::VariableDeclaration(vd) => vd.fmt(f),
            VarScopeDecl::ForBinding(fb) => fb.fmt(f),
        }
    }
}
impl From<&VarScopeDecl> for String {
    fn from(src: &VarScopeDecl) -> String {
        src.to_string()
    }
}
impl VarScopeDecl {
    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            VarScopeDecl::VariableDeclaration(node) => node.bound_names(),
            VarScopeDecl::ForBinding(node) => node.bound_names(),
            VarScopeDecl::FunctionDeclaration(node) => node.bound_names(),
            VarScopeDecl::GeneratorDeclaration(node) => node.bound_names(),
            VarScopeDecl::AsyncFunctionDeclaration(node) => node.bound_names(),
            VarScopeDecl::AsyncGeneratorDeclaration(node) => node.bound_names(),
        }
    }
}

impl Script {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let starting_location = Location::from(scanner);
        let (script, after_script, err) = match ScriptBody::parse(parser, scanner) {
            Ok((node, scan)) => (Some(node), scan, None),
            Err(err) => (None, scanner, Some(err)),
        };
        let end = scan_for_eof(after_script, parser.source);
        match end {
            Ok((eof_loc, scan)) => {
                let location = starting_location.merge(&eof_loc);
                Ok((Rc::new(Script { body: script, location }), scan))
            }
            Err(e) => match err {
                Some(x) => Err(x),
                None => Err(e),
            },
        }
    }

    pub fn location(&self) -> Location {
        self.location
    }

    // Static Semantics: Early Errors
    //      Script : ScriptBody
    // * It is a Syntax Error if the LexicallyDeclaredNames of ScriptBody contains any duplicate entries.
    // * It is a Syntax Error if any element of the LexicallyDeclaredNames of ScriptBody also occurs in the
    //   VarDeclaredNames of ScriptBody.
    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        if let Some(body) = &self.body {
            let lex_names = body.lexically_declared_names();
            let var_names = body.var_declared_names();
            if !has_unique_elements(lex_names.clone()) {
                errs.push(create_syntax_error_object("Duplicate lexically declared names", Some(body.location())));
            }
            let lex_names_set: AHashSet<JSString> = lex_names.into_iter().collect();
            let var_names_set: AHashSet<JSString> = var_names.into_iter().collect();
            if !lex_names_set.is_disjoint(&var_names_set) {
                errs.push(create_syntax_error_object(
                    "Name defined both lexically and var-style",
                    Some(body.location()),
                ));
            }
            body.early_errors(errs, strict);
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match &self.body {
            None => false,
            Some(n) => kind == ParseNodeKind::ScriptBody || n.contains(kind),
        }
    }

    /// Return a list of the lexically declared names for this script.
    ///
    /// See [LexicallyDeclaredNames](https://tc39.es/ecma262/#sec-static-semantics-lexicallydeclarednames) from ECMA-262.
    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        match &self.body {
            None => vec![],
            Some(sb) => sb.lexically_declared_names(),
        }
    }

    /// Return a list of the var delcared names for this script.
    ///
    /// See [VarDeclaredNames](https://tc39.es/ecma262/#sec-static-semantics-vardeclarednames) from ECMA-262.
    pub fn var_declared_names(&self) -> Vec<JSString> {
        match &self.body {
            None => vec![],
            Some(sb) => sb.var_declared_names(),
        }
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// For Scripts and ScriptBodies, function definitions that exist lexically at global scope are treated as though
    /// they are declared var-style.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        match &self.body {
            None => vec![],
            Some(sb) => sb.var_scoped_declarations(),
        }
    }

    /// Return a list of parse nodes for the lexically declared identifiers contained within the children of this node.
    ///
    /// For Scripts and ScriptBodies, function definitions that exist lexically at global scope are treated as though
    /// they are declared var-style, and as such won't be reflected here.
    ///
    /// See [LexicallyScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-lexicallyscopeddeclarations) in ECMA-262.
    pub fn lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        match &self.body {
            None => vec![],
            Some(sb) => sb.lexically_scoped_declarations(),
        }
    }
}

// ScriptBody :
//      StatementList[~Yield, ~Await, ~Return]
#[derive(Debug)]
pub struct ScriptBody {
    pub statement_list: Rc<StatementList>,
    pub direct: bool,
}

impl fmt::Display for ScriptBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.statement_list.fmt(f)
    }
}

impl PrettyPrint for ScriptBody {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}ScriptBody: {self}")?;
        self.statement_list.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        self.statement_list.concise_with_leftpad(writer, pad, state)
    }
}

impl ScriptBody {
    pub fn parse(parser: &mut Parser, scanner: Scanner) -> ParseResult<Self> {
        let (sl, after_sl) = StatementList::parse(parser, scanner, false, false, false)?;
        Ok((Rc::new(ScriptBody { statement_list: sl, direct: parser.direct }), after_sl))
    }

    pub fn location(&self) -> Location {
        self.statement_list.location()
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        self.statement_list.top_level_lexically_declared_names()
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        self.statement_list.top_level_var_declared_names()
    }

    // ScriptBody : StatementList
    //  * It is a Syntax Error if StatementList Contains super unless the source code containing super is eval code that
    //    is being processed by a direct eval. Additional early error rules for super within direct eval are defined in
    //    19.2.1.1.
    //  * It is a Syntax Error if StatementList Contains NewTarget unless the source code containing NewTarget is eval
    //    code that is being processed by a direct eval. Additional early error rules for NewTarget in direct eval are
    //    defined in 19.2.1.1.
    //  * It is a Syntax Error if ContainsDuplicateLabels of StatementList with argument « » is true.
    //  * It is a Syntax Error if ContainsUndefinedBreakTarget of StatementList with argument « » is true.
    //  * It is a Syntax Error if ContainsUndefinedContinueTarget of StatementList with arguments « » and « » is true.
    //  * It is a Syntax Error if AllPrivateIdentifiersValid of StatementList with argument « » is false unless the
    //    source code containing ScriptBody is eval code that is being processed by a direct eval.
    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool) {
        if !self.direct {
            if self.statement_list.contains(ParseNodeKind::Super) {
                errs.push(create_syntax_error_object(
                    "`super' not allowed in top-level code",
                    Some(self.statement_list.location()),
                ));
            }
            if self.statement_list.contains(ParseNodeKind::NewTarget) {
                errs.push(create_syntax_error_object(
                    "`new.target` not allowed in top-level code",
                    Some(self.statement_list.location()),
                ));
            }
        }
        if self.statement_list.contains_duplicate_labels(&[]) {
            errs.push(create_syntax_error_object("duplicate labels detected", Some(self.statement_list.location())));
        }
        if self.statement_list.contains_undefined_break_target(&[]) {
            errs.push(create_syntax_error_object(
                "undefined break target detected",
                Some(self.statement_list.location()),
            ));
        }
        if self.statement_list.contains_undefined_continue_target(&[], &[]) {
            errs.push(create_syntax_error_object(
                "undefined continue target detected",
                Some(self.statement_list.location()),
            ));
        }
        if !self.direct && !self.statement_list.all_private_identifiers_valid(&[]) {
            errs.push(create_syntax_error_object(
                "invalid private identifier detected",
                Some(self.statement_list.location()),
            ));
        }
        self.statement_list.early_errors(errs, strict || self.contains_use_strict(), false, false);
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        kind == ParseNodeKind::StatementList || self.statement_list.contains(kind)
    }

    pub fn directive_prologue(&self) -> Vec<StringToken> {
        self.statement_list.initial_string_tokens()
    }

    pub fn contains_use_strict(&self) -> bool {
        let prologue = self.directive_prologue();
        let needle = JSString::from("use strict");
        prologue.iter().any(|string_tok| string_tok.raw.is_none() && string_tok.value == needle)
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// For Scripts and ScriptBodies, function definitions that exist lexically at global scope are treated as though
    /// they are declared var-style.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        self.statement_list.top_level_var_scoped_declarations()
    }

    /// Return a list of parse nodes for the lexically declared identifiers contained within the children of this node.
    ///
    /// For Scripts and ScriptBodies, function definitions that exist lexically at global scope are treated as though
    /// they are declared var-style, and as such won't be reflected here.
    ///
    /// See [LexicallyScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-lexicallyscopeddeclarations) in ECMA-262.
    pub fn lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        self.statement_list.top_level_lexically_scoped_declarations()
    }

    /// Returns `true` if any subexpression starting from here (but not crossing function boundaries) contains an
    /// [`IdentifierReference`] with string value `"arguments"`.
    ///
    /// See [ContainsArguments](https://tc39.es/ecma262/#sec-static-semantics-containsarguments) from ECMA-262.
    pub fn contains_arguments(&self) -> bool {
        self.statement_list.contains_arguments()
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.statement_list.all_private_identifiers_valid(names)
    }
}

#[cfg(test)]
mod tests;
