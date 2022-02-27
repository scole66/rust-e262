use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::function_definitions::FunctionDeclaration;
use super::identifiers::LabelIdentifier;
use super::scanner::{Punctuator, ScanGoal, Scanner};
use super::statements_and_declarations::Statement;
use super::*;
use crate::prettyprint::{pprint_token, prettypad, PrettyPrint, Spot, TokenType};

// LabelledStatement[Yield, Await, Return] :
//      LabelIdentifier[?Yield, ?Await] : LabelledItem[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub struct LabelledStatement {
    identifier: Rc<LabelIdentifier>,
    item: Rc<LabelledItem>,
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
        writeln!(writer, "{}LabelledStatement: {}", first, self)?;
        self.identifier.pprint_with_leftpad(writer, &successive, Spot::NotFinal)?;
        self.item.pprint_with_leftpad(writer, &successive, Spot::Final)
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}LabelledStatement: {}", first, self)?;
        self.identifier.concise_with_leftpad(writer, &successive, Spot::NotFinal)?;
        pprint_token(writer, ":", TokenType::Punctuator, &successive, Spot::NotFinal)?;
        self.item.concise_with_leftpad(writer, &successive, Spot::Final)
    }
}

impl LabelledStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let (identifier, after_li) = LabelIdentifier::parse(parser, scanner, yield_flag, await_flag)?;
        let after_colon = scan_for_punct(after_li, parser.source, ScanGoal::InputElementDiv, Punctuator::Colon)?;
        let (item, after_item) = LabelledItem::parse(parser, after_colon, yield_flag, await_flag, return_flag)?;
        Ok((Rc::new(LabelledStatement { identifier, item }), after_item))
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

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut [Object], _strict: bool) {
        todo!()
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
        writeln!(writer, "{}LabelledItem: {}", first, self)?;
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
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
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

    pub fn early_errors(&self, _agent: &mut Agent, _errs: &mut [Object], _strict: bool) {
        todo!()
    }
}

#[cfg(test)]
mod tests;
