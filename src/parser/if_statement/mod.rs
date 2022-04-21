use super::comma_operator::Expression;
use super::scanner::{Keyword, Punctuator, ScanGoal, Scanner};
use super::scripts::VarScopeDecl;
use super::statements_and_declarations::Statement;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};
use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

// IfStatement[Yield, Await, Return] :
//      if ( Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return] else Statement[?Yield, ?Await, ?Return]
//      if ( Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return] [lookahead ≠ else]
#[derive(Debug)]
pub enum IfStatement {
    WithElse(Rc<Expression>, Rc<Statement>, Rc<Statement>),
    WithoutElse(Rc<Expression>, Rc<Statement>),
}

impl fmt::Display for IfStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IfStatement::WithElse(e, s1, s2) => write!(f, "if ( {} ) {} else {}", e, s1, s2),
            IfStatement::WithoutElse(e, s1) => write!(f, "if ( {} ) {}", e, s1),
        }
    }
}

impl PrettyPrint for IfStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}IfStatement: {}", first, self)?;
        match self {
            IfStatement::WithoutElse(e, s1) => {
                e.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                s1.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
            IfStatement::WithElse(e, s1, s2) => {
                e.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                s1.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
                s2.pprint_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}IfStatement: {}", first, self)?;
        pprint_token(writer, "if", TokenType::Keyword, &successive, Spot::NotFinal)?;
        pprint_token(writer, "(", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        let condition = |writer: &mut T, exp: &Expression| {
            exp.concise_with_leftpad(writer, &successive, Spot::NotFinal).and_then(|_| pprint_token(writer, ")", TokenType::Punctuator, &successive, Spot::NotFinal))
        };
        match self {
            IfStatement::WithoutElse(e, s1) => {
                condition(writer, e)?;
                s1.concise_with_leftpad(writer, &successive, Spot::Final)
            }
            IfStatement::WithElse(e, s1, s2) => {
                condition(writer, e)?;
                s1.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
                pprint_token(writer, "else", TokenType::Keyword, &successive, Spot::NotFinal)?;
                s2.concise_with_leftpad(writer, &successive, Spot::Final)
            }
        }
    }
}

impl IfStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let after_lead = scan_for_keyword(scanner, parser.source, ScanGoal::InputElementRegExp, Keyword::If)?;
        let after_open = scan_for_punct(after_lead, parser.source, ScanGoal::InputElementDiv, Punctuator::LeftParen)?;
        let (exp, after_exp) = Expression::parse(parser, after_open, true, yield_flag, await_flag)?;
        let after_close = scan_for_punct(after_exp, parser.source, ScanGoal::InputElementDiv, Punctuator::RightParen)?;
        let (stmt1, after_stmt1) = Statement::parse(parser, after_close, yield_flag, await_flag, return_flag)?;
        match scan_for_keyword(after_stmt1, parser.source, ScanGoal::InputElementRegExp, Keyword::Else) {
            Err(_) => Ok((Rc::new(IfStatement::WithoutElse(exp, stmt1)), after_stmt1)),
            Ok(after_else) => {
                let (stmt2, after_stmt2) = Statement::parse(parser, after_else, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(IfStatement::WithElse(exp, stmt1, stmt2)), after_stmt2))
            }
        }
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            IfStatement::WithElse(_, s1, s2) => {
                let mut names = s1.var_declared_names();
                names.extend(s2.var_declared_names());
                names
            }
            IfStatement::WithoutElse(_, s1) => s1.var_declared_names(),
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            IfStatement::WithElse(_, s1, s2) => s1.contains_undefined_break_target(label_set) || s2.contains_undefined_break_target(label_set),
            IfStatement::WithoutElse(_, s1) => s1.contains_undefined_break_target(label_set),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            IfStatement::WithElse(e, s1, s2) => e.contains(kind) || s1.contains(kind) || s2.contains(kind),
            IfStatement::WithoutElse(e, s1) => e.contains(kind) || s1.contains(kind),
        }
    }

    pub fn contains_duplicate_labels(&self, label_set: &[JSString]) -> bool {
        match self {
            IfStatement::WithElse(_, s1, s2) => s1.contains_duplicate_labels(label_set) || s2.contains_duplicate_labels(label_set),
            IfStatement::WithoutElse(_, s1) => s1.contains_duplicate_labels(label_set),
        }
    }

    pub fn contains_undefined_continue_target(&self, iteration_set: &[JSString]) -> bool {
        match self {
            IfStatement::WithElse(_, s1, s2) => s1.contains_undefined_continue_target(iteration_set, &[]) || s2.contains_undefined_continue_target(iteration_set, &[]),
            IfStatement::WithoutElse(_, s1) => s1.contains_undefined_continue_target(iteration_set, &[]),
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
            IfStatement::WithElse(e, s1, s2) => e.all_private_identifiers_valid(names) && s1.all_private_identifiers_valid(names) && s2.all_private_identifiers_valid(names),
            IfStatement::WithoutElse(e, s1) => e.all_private_identifiers_valid(names) && s1.all_private_identifiers_valid(names),
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
            IfStatement::WithElse(e, s1, s2) => e.contains_arguments() || s1.contains_arguments() || s2.contains_arguments(),
            IfStatement::WithoutElse(e, stmt) => e.contains_arguments() || stmt.contains_arguments(),
        }
    }

    pub fn early_errors(&self, agent: &mut Agent, errs: &mut Vec<Object>, strict: bool, within_iteration: bool, within_switch: bool) {
        let (e, s1, s2) = match self {
            IfStatement::WithElse(e, s1, s2) => (e, s1, Some(s2)),
            IfStatement::WithoutElse(e, s1) => (e, s1, None),
        };
        e.early_errors(agent, errs, strict);
        s1.early_errors(agent, errs, strict, within_iteration, within_switch);
        if let Some(s) = s2 {
            s.early_errors(agent, errs, strict, within_iteration, within_switch);
        }
    }

    pub fn var_scoped_declarations(&self) -> Vec<VarScopeDecl> {
        match self {
            IfStatement::WithElse(_, s1, s2) => {
                let mut list = s1.var_scoped_declarations();
                list.extend(s2.var_scoped_declarations());
                list
            }
            IfStatement::WithoutElse(_, s1) => s1.var_scoped_declarations(),
        }
    }
}

#[cfg(test)]
mod tests;
