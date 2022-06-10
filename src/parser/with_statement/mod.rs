use super::comma_operator::Expression;
use super::scanner::{Keyword, Punctuator, ScanGoal, Scanner};
use super::scripts::VarScopeDecl;
use super::statements_and_declarations::Statement;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// WithStatement[Yield, Await, Return] :
//      with ( Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub struct WithStatement {
    expression: Rc<Expression>,
    statement: Rc<Statement>,
    location: Location,
}

impl fmt::Display for WithStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "with ( {} ) {}", self.expression, self.statement)
    }
}

impl PrettyPrint for WithStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}WithStatement: {}", first, self)?;
        self.expression.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.statement.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}WithStatement: {}", first, self)?;
        pprint_token(writer, "with", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.expression.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.statement.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl WithStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> ParseResult<Self> {
        let (with_loc, after_with) =
            scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::With)?;
        let (_, after_open) =
            scan_for_punct(after_with, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (expression, after_exp) = Expression::parse(parser, after_open, true, yield_flag, await_flag)?;
        let (_, after_close) =
            scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let (statement, after_stmt) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
        Ok((
            Rc::new({
                let location = with_loc.merge(&statement.location());
                WithStatement { expression, statement, location }
            }),
            after_stmt,
        ))
    }

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        self.statement.var_declared_names()
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        self.statement.contains_undefined_break_target(label_set)
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        self.expression.contains(kind) || self.statement.contains(kind)
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        self.statement.contains_duplicate_labels(label_set)
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        self.statement.contains_undefined_continue_target(iteration_set, &[])
    }

    pub fn all_private_identifiers_valid(&self, names: &[JSString]) -> bool {
        // Static Semantics: AllPrivateIdentifiersValid
        // With parameter names.
        //  1. For each child node child of this Parse Node, do
        //      a. If child is an instance of a nonterminal, then
        //          i. If AllPrivateIdentifiersValid of child with argument names is false, return false.
        //  2. Return true.
        self.expression.all_private_identifiers_valid(names) && self.statement.all_private_identifiers_valid(names)
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
        self.expression.contains_arguments() || self.statement.contains_arguments()
    }

    pub fn early_errors(
        &self,
        agent: &mut Agent,
        errs: &mut Vec<Object>,
        strict: bool,
        within_iteration: bool,
        within_switch: bool,
    ) {
        // Static Semantics: Early Errors
        //  WithStatement : with ( Expression ) Statement
        //  * It is a Syntax Error if the source text matched by this production is contained in strict mode code.
        if strict {
            errs.push(create_syntax_error_object(
                agent,
                "'with' statements not allowed in strict mode",
                Some(self.location()),
            ));
        }
        self.expression.early_errors(agent, errs, strict);
        self.statement.early_errors(agent, errs, strict, within_iteration, within_switch);
    }

    /// Return a list of parse nodes for the var-style declarations contained within the children of this node.
    ///
    /// See [VarScopedDeclarations](https://tc39.es/ecma262/#sec-static-semantics-varscopeddeclarations) in ECMA-262.
    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        self.statement.var_scoped_declarations()
    }
}

#[cfg(test)]
mod tests;
