use super::*;
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// LabelledStatement[Yield, Await, Return] :
//      LabelIdentifier[?Yield, ?Await] : LabelledItem[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub struct LabelledStatement {
    pub identifier: Rc<LabelIdentifier>,
    pub item: Rc<LabelledItem>,
}

impl fmt::Display for LabelledStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} : {}", self.identifier, self.item)
    }
}

impl PrettyPrint for LabelledStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}LabelledStatement: {self}")?;
        self.identifier.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.item.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}LabelledStatement: {self}")?;
        self.identifier.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.item.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl LabelledStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> ParseResult<Self> {
        let (identifier, after_li) = LabelIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
        let (_, after_colon) = scan_for_punct(after_li, parser.source, ScanGoal::InputElementDiv, Punctuator::Colon)?;
        let (item, after_item) = LabelledItem::parse(parser, after_colon, yield_flag, await_flag, return_flag)?;
        Ok((Rc::new(LabelledStatement { identifier, item }), after_item))
    }

    pub fn location(&self) -> Location {
        self.identifier.location().merge(&self.item.location())
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        // LabelledStatement : LabelIdentifier : LabelledItem
        //  1. Return the LexicallyDeclaredNames of LabelledItem.
        self.item.lexically_declared_names()
    }

    pub fn top_level_var_declared_names(&self) -> Vec<JSString> {
        self.item.top_level_var_declared_names()
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        self.item.var_declared_names()
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        let label = self.identifier.string_value();
        let mut new_label_set: Vec<JSString> = Vec::new();
        new_label_set.extend_from_slice(label_set);
        new_label_set.push(label);
        self.item.contains_undefined_break_target(&new_label_set)
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.identifier.contains(kind) || self.item.contains(kind)
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        let label = self.identifier.string_value();
        label_set.contains(&label) || {
            let mut new_label_set: Vec<JSString> = Vec::new();
            new_label_set.extend_from_slice(label_set);
            new_label_set.push(label);
            self.item.contains_duplicate_labels(&new_label_set)
        }
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString], label_set: &[JSString]) -> bool {
        let label = self.identifier.string_value();
        let mut new_label_set: Vec<JSString> = Vec::new();
        new_label_set.extend_from_slice(label_set);
        new_label_set.push(label);
        self.item.contains_undefined_continue_target(iteration_set, &new_label_set)
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.item.all_private_identifiers_valid(names)
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
        self.item.contains_arguments()
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool, within_iteration: bool, within_switch: bool) {
        self.identifier.early_errors(errs, strict);
        self.item.early_errors(errs, strict, within_iteration, within_switch);
    }

    pub fn is_labelled_function(&self) -> bool {
        // Static Semantics: IsLabelledFunction ( stmt )
        //
        // The abstract operation IsLabelledFunction takes argument stmt and returns a Boolean. It performs the
        // following steps when called:
        //
        //  1. If stmt is not a LabelledStatement, return false.
        //  2. Let item be the LabelledItem of stmt.
        //  3. If item is LabelledItem : FunctionDeclaration , return true.
        //  4. Let subStmt be the Statement of item.
        //  5. Return IsLabelledFunction(subStmt).
        self.item.is_labelled_function()
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// This is the top-level form; in this form, function definitions that exist lexically at global scope are treated
    /// as though they are declared var-style.
    ///
    /// See [TopLevelVarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-toplevelvarscopeddeclarations) in ECMA-262.
    pub fn top_level_var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        self.item.top_level_var_scoped_declarations()
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        self.item.var_scoped_declarations()
    }

    pub fn lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        self.item.lexically_scoped_declarations()
    }
}

// LabelledItem[Yield, Await, Return] :
//      Statement[?Yield, ?Await, ?Return]
//      FunctionDeclaration[?Yield, ?Await, ~Default]
#[derive(Debug)]
pub enum LabelledItem {
    Statement(Rc<Statement>),
    Function(Rc<FunctionDeclaration>),
}

impl fmt::Display for LabelledItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LabelledItem::Statement(node) => node.fmt(f),
            LabelledItem::Function(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for LabelledItem {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{first}LabelledItem: {self}")?;
        match self {
            LabelledItem::Statement(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            LabelledItem::Function(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            LabelledItem::Statement(node) => node.concise_with_leftpad(writer, pad, state),
            LabelledItem::Function(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl LabelledItem {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> ParseResult<Self> {
        Err(ParseError::new(PECode::ParseNodeExpected(ParseNodeKind::LabelledItem), scanner))
            .otherwise(|| {
                let (stmt, after_stmt) = Statement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(LabelledItem::Statement(stmt)), after_stmt))
            })
            .otherwise(|| {
                let (fcn, after_fcn) = FunctionDeclaration::parse(parser, scanner, yield_flag, await_flag, false)?;
                Ok((Rc::new(LabelledItem::Function(fcn)), after_fcn))
            })
    }

    pub fn location(&self) -> Location {
        match self {
            LabelledItem::Statement(node) => node.location(),
            LabelledItem::Function(node) => node.location(),
        }
    }

    pub fn lexically_declared_names(&self) -> Vec<JSString> {
        match self {
            LabelledItem::Statement(_) => {
                // LabelledItem : Statement
                //  1. Return a new empty List.
                vec![]
            }
            LabelledItem::Function(node) => {
                // LabelledItem : FunctionDeclaration
                //  1. Return BoundNames of FunctionDeclaration.
                node.bound_names()
            }
        }
    }

    pub fn top_level_var_declared_names(&self) -> Vec<JSString> {
        match self {
            LabelledItem::Statement(node) => match &**node {
                Statement::Labelled(stmt) => stmt.top_level_var_declared_names(),
                _ => node.var_declared_names(),
            },
            LabelledItem::Function(node) => node.bound_names(),
        }
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            LabelledItem::Statement(node) => node.var_declared_names(),
            LabelledItem::Function(..) => vec![],
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            LabelledItem::Statement(node) => node.contains_undefined_break_target(label_set),
            LabelledItem::Function(..) => false,
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            LabelledItem::Statement(node) => node.contains(kind),
            LabelledItem::Function(node) => node.contains(kind),
        }
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self {
            LabelledItem::Statement(node) => node.contains_duplicate_labels(label_set),
            LabelledItem::Function(..) => false,
        }
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString], label_set: &[JSString]) -> bool {
        match self {
            LabelledItem::Statement(node) => node.contains_undefined_continue_target(iteration_set, label_set),
            LabelledItem::Function(..) => false,
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
            LabelledItem::Statement(node) => node.all_private_identifiers_valid(names),
            LabelledItem::Function(node) => node.all_private_identifiers_valid(names),
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
            LabelledItem::Statement(stmt) => stmt.contains_arguments(),
            LabelledItem::Function(_) => false,
        }
    }

    pub fn early_errors(&self, errs: &mut Vec<Object>, strict: bool, within_iteration: bool, within_switch: bool) {
        // Static Semantics: Early Errors
        //  LabelledItem : FunctionDeclaration
        //      * It is a Syntax Error if any source text is matched by this production.
        if matches!(self, LabelledItem::Function(_)) {
            errs.push(create_syntax_error_object(
                "Labelled functions not allowed in modern ECMAScript code",
                Some(self.location()),
            ));
        }
        match self {
            LabelledItem::Statement(stmt) => stmt.early_errors(errs, strict, within_iteration, within_switch),
            LabelledItem::Function(fcn) => fcn.early_errors(errs, strict),
        }
    }

    pub fn is_labelled_function(&self) -> bool {
        // Static Semantics: IsLabelledFunction ( stmt )
        //
        // The abstract operation IsLabelledFunction takes argument stmt and returns a Boolean. It performs the
        // following steps when called:
        //
        //  1. If stmt is not a LabelledStatement, return false.
        //  2. Let item be the LabelledItem of stmt.
        //  3. If item is LabelledItem : FunctionDeclaration , return true.
        //  4. Let subStmt be the Statement of item.
        //  5. Return IsLabelledFunction(subStmt).
        match self {
            LabelledItem::Function(_) => true,
            LabelledItem::Statement(sub_stmt) => sub_stmt.is_labelled_function(),
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
            LabelledItem::Function(fd) => {
                vec![VarScopeDecl::FunctionDeclaration(Rc::clone(fd))]
            }
            LabelledItem::Statement(stmt) => match &**stmt {
                Statement::Labelled(ls) => ls.top_level_var_scoped_declarations(),
                _ => stmt.var_scoped_declarations(),
            },
        }
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        match self {
            LabelledItem::Function(_) => vec![],
            LabelledItem::Statement(stmt) => stmt.var_scoped_declarations(),
        }
    }

    pub fn lexically_scoped_declarations(&self) -> Vec<DeclPart> {
        match self {
            LabelledItem::Statement(_) => vec![],
            LabelledItem::Function(f) => vec![Rc::clone(f).into()],
        }
    }
}

#[cfg(test)]
mod tests;
