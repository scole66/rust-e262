use std::fmt;
use std::io::Result as IoResult;
use std::io::Write;

use super::async_function_definitions::AsyncFunctionDeclaration;
use super::async_generator_function_definitions::AsyncGeneratorDeclaration;
use super::block::BlockStatement;
use super::break_statement::BreakStatement;
use super::class_definitions::ClassDeclaration;
use super::continue_statement::ContinueStatement;
use super::debugger_statement::DebuggerStatement;
use super::declarations_and_variables::{LexicalDeclaration, VariableStatement};
use super::empty_statement::EmptyStatement;
use super::expression_statement::ExpressionStatement;
use super::function_definitions::FunctionDeclaration;
use super::generator_function_definitions::GeneratorDeclaration;
use super::if_statement::IfStatement;
use super::iteration_statements::IterationStatement;
use super::labelled_statements::LabelledStatement;
use super::return_statement::ReturnStatement;
use super::scanner::Scanner;
use super::switch_statement::SwitchStatement;
use super::throw_statement::ThrowStatement;
use super::try_statement::TryStatement;
use super::with_statement::WithStatement;
use super::*;
use crate::prettyprint::{prettypad, PrettyPrint, Spot};

// Statement[Yield, Await, Return] :
//      BlockStatement[?Yield, ?Await, ?Return]
//      VariableStatement[?Yield, ?Await]
//      EmptyStatement
//      ExpressionStatement[?Yield, ?Await]
//      IfStatement[?Yield, ?Await, ?Return]
//      BreakableStatement[?Yield, ?Await, ?Return]
//      ContinueStatement[?Yield, ?Await]
//      BreakStatement[?Yield, ?Await]
//      [+Return]ReturnStatement[?Yield, ?Await]
//      WithStatement[?Yield, ?Await, ?Return]
//      LabelledStatement[?Yield, ?Await, ?Return]
//      ThrowStatement[?Yield, ?Await]
//      TryStatement[?Yield, ?Await, ?Return]
//      DebuggerStatement
#[derive(Debug)]
pub enum Statement {
    Block(Rc<BlockStatement>),
    Variable(Rc<VariableStatement>),
    Empty(Rc<EmptyStatement>),
    Expression(Rc<ExpressionStatement>),
    If(Rc<IfStatement>),
    Breakable(Rc<BreakableStatement>),
    Continue(Rc<ContinueStatement>),
    Break(Rc<BreakStatement>),
    Return(Rc<ReturnStatement>),
    With(Rc<WithStatement>),
    Labelled(Rc<LabelledStatement>),
    Throw(Rc<ThrowStatement>),
    Try(Rc<TryStatement>),
    Debugger(Rc<DebuggerStatement>),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Statement::Block(node) => node.fmt(f),
            Statement::Variable(node) => node.fmt(f),
            Statement::Empty(node) => node.fmt(f),
            Statement::Expression(node) => node.fmt(f),
            Statement::If(node) => node.fmt(f),
            Statement::Breakable(node) => node.fmt(f),
            Statement::Continue(node) => node.fmt(f),
            Statement::Break(node) => node.fmt(f),
            Statement::Return(node) => node.fmt(f),
            Statement::With(node) => node.fmt(f),
            Statement::Labelled(node) => node.fmt(f),
            Statement::Throw(node) => node.fmt(f),
            Statement::Try(node) => node.fmt(f),
            Statement::Debugger(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for Statement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Statement: {}", first, self)?;
        match &self {
            Statement::Block(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Statement::Variable(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Statement::Empty(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Statement::Expression(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Statement::If(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Statement::Breakable(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Statement::Continue(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Statement::Break(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Statement::Return(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Statement::With(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Statement::Labelled(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Statement::Throw(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Statement::Try(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Statement::Debugger(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            Statement::Block(node) => node.concise_with_leftpad(writer, pad, state),
            Statement::Variable(node) => node.concise_with_leftpad(writer, pad, state),
            Statement::Empty(node) => node.concise_with_leftpad(writer, pad, state),
            Statement::Expression(node) => node.concise_with_leftpad(writer, pad, state),
            Statement::If(node) => node.concise_with_leftpad(writer, pad, state),
            Statement::Breakable(node) => node.concise_with_leftpad(writer, pad, state),
            Statement::Continue(node) => node.concise_with_leftpad(writer, pad, state),
            Statement::Break(node) => node.concise_with_leftpad(writer, pad, state),
            Statement::Return(node) => node.concise_with_leftpad(writer, pad, state),
            Statement::With(node) => node.concise_with_leftpad(writer, pad, state),
            Statement::Labelled(node) => node.concise_with_leftpad(writer, pad, state),
            Statement::Throw(node) => node.concise_with_leftpad(writer, pad, state),
            Statement::Try(node) => node.concise_with_leftpad(writer, pad, state),
            Statement::Debugger(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl Statement {
    fn parse_core(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("Statement expected", scanner.line, scanner.column))
            .otherwise(|| {
                let (block, after_block) = BlockStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(Statement::Block(block)), after_block))
            })
            .otherwise(|| {
                let (var, after_var) = VariableStatement::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(Statement::Variable(var)), after_var))
            })
            .otherwise(|| {
                let (empty, after_empty) = EmptyStatement::parse(parser, scanner)?;
                Ok((Rc::new(Statement::Empty(empty)), after_empty))
            })
            .otherwise(|| {
                let (exp, after_exp) = ExpressionStatement::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(Statement::Expression(exp)), after_exp))
            })
            .otherwise(|| {
                let (if_node, after_if) = IfStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(Statement::If(if_node)), after_if))
            })
            .otherwise(|| {
                let (bable_node, after_bable) = BreakableStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(Statement::Breakable(bable_node)), after_bable))
            })
            .otherwise(|| {
                let (cont_node, after_cont) = ContinueStatement::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(Statement::Continue(cont_node)), after_cont))
            })
            .otherwise(|| {
                let (break_node, after_break) = BreakStatement::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(Statement::Break(break_node)), after_break))
            })
            .otherwise(|| {
                if return_flag {
                    let (return_node, after_return) = ReturnStatement::parse(parser, scanner, yield_flag, await_flag)?;
                    Ok((Rc::new(Statement::Return(return_node)), after_return))
                } else {
                    Err(ParseError::new(String::new(), scanner.line, scanner.column))
                }
            })
            .otherwise(|| {
                let (with_node, after_with) = WithStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(Statement::With(with_node)), after_with))
            })
            .otherwise(|| {
                let (lbl_node, after_lbl) = LabelledStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(Statement::Labelled(lbl_node)), after_lbl))
            })
            .otherwise(|| {
                let (throw_node, after_throw) = ThrowStatement::parse(parser, scanner, yield_flag, await_flag)?;
                Ok((Rc::new(Statement::Throw(throw_node)), after_throw))
            })
            .otherwise(|| {
                let (try_node, after_try) = TryStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(Statement::Try(try_node)), after_try))
            })
            .otherwise(|| {
                let (dbg_node, after_dbg) = DebuggerStatement::parse(parser, scanner)?;
                Ok((Rc::new(Statement::Debugger(dbg_node)), after_dbg))
            })
    }

    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        let key = YieldAwaitReturnKey { scanner, yield_flag, await_flag, return_flag };
        match parser.statement_cache.get(&key) {
            Some(result) => result.clone(),
            None => {
                let result = Self::parse_core(parser, scanner, yield_flag, await_flag, return_flag);
                parser.statement_cache.insert(key, result.clone());
                result
            }
        }
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match &self {
            Statement::Block(node) => node.var_declared_names(),
            Statement::Variable(node) => node.var_declared_names(),
            Statement::Empty(_) => vec![],
            Statement::Expression(_) => vec![],
            Statement::If(node) => node.var_declared_names(),
            Statement::Breakable(node) => node.var_declared_names(),
            Statement::Continue(_) => vec![],
            Statement::Break(_) => vec![],
            Statement::Return(_) => vec![],
            Statement::With(node) => node.var_declared_names(),
            Statement::Labelled(node) => node.var_declared_names(),
            Statement::Throw(_) => vec![],
            Statement::Try(node) => node.var_declared_names(),
            Statement::Debugger(_) => vec![],
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            Statement::Variable(_) | Statement::Empty(_) | Statement::Expression(_) | Statement::Continue(_) | Statement::Return(_) | Statement::Throw(_) | Statement::Debugger(_) => false,
            Statement::Block(node) => node.contains_undefined_break_target(label_set),
            Statement::If(node) => node.contains_undefined_break_target(label_set),
            Statement::Breakable(node) => node.contains_undefined_break_target(label_set),
            Statement::Break(node) => node.contains_undefined_break_target(label_set),
            Statement::With(node) => node.contains_undefined_break_target(label_set),
            Statement::Labelled(node) => node.contains_undefined_break_target(label_set),
            Statement::Try(node) => node.contains_undefined_break_target(label_set),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            Statement::Block(n) => kind == ParseNodeKind::BlockStatement || n.contains(kind),
            Statement::Variable(n) => kind == ParseNodeKind::VariableStatement || n.contains(kind),
            Statement::Empty(n) => kind == ParseNodeKind::EmptyStatement || n.contains(kind),
            Statement::Expression(n) => kind == ParseNodeKind::ExpressionStatement || n.contains(kind),
            Statement::If(n) => kind == ParseNodeKind::IfStatement || n.contains(kind),
            Statement::Breakable(n) => kind == ParseNodeKind::BreakableStatement || n.contains(kind),
            Statement::Continue(n) => kind == ParseNodeKind::ContinueStatement || n.contains(kind),
            Statement::Break(n) => kind == ParseNodeKind::BreakStatement || n.contains(kind),
            Statement::With(n) => kind == ParseNodeKind::WithStatement || n.contains(kind),
            Statement::Labelled(n) => kind == ParseNodeKind::LabelledStatement || n.contains(kind),
            Statement::Throw(n) => kind == ParseNodeKind::ThrowStatement || n.contains(kind),
            Statement::Try(n) => kind == ParseNodeKind::TryStatement || n.contains(kind),
            Statement::Debugger(n) => kind == ParseNodeKind::DebuggerStatement || n.contains(kind),
            Statement::Return(n) => kind == ParseNodeKind::ReturnStatement || n.contains(kind),
        }
    }
}

// Declaration[Yield, Await] :
//      HoistableDeclaration[?Yield, ?Await, ~Default]
//      ClassDeclaration[?Yield, ?Await, ~Default]
//      LexicalDeclaration[+In, ?Yield, ?Await]
#[derive(Debug)]
pub enum Declaration {
    Hoistable(Rc<HoistableDeclaration>),
    Class(Rc<ClassDeclaration>),
    Lexical(Rc<LexicalDeclaration>),
}

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Declaration::Hoistable(node) => node.fmt(f),
            Declaration::Class(node) => node.fmt(f),
            Declaration::Lexical(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for Declaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}Declaration: {}", first, self)?;
        match &self {
            Declaration::Hoistable(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Declaration::Class(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            Declaration::Lexical(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            Declaration::Hoistable(node) => node.concise_with_leftpad(writer, pad, state),
            Declaration::Class(node) => node.concise_with_leftpad(writer, pad, state),
            Declaration::Lexical(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl Declaration {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("Declaration expected", scanner.line, scanner.column))
            .otherwise(|| {
                let (hoist, after_hoist) = HoistableDeclaration::parse(parser, scanner, yield_flag, await_flag, false)?;
                Ok((Rc::new(Declaration::Hoistable(hoist)), after_hoist))
            })
            .otherwise(|| {
                let (cls, after_cls) = ClassDeclaration::parse(parser, scanner, yield_flag, await_flag, false)?;
                Ok((Rc::new(Declaration::Class(cls)), after_cls))
            })
            .otherwise(|| {
                let (lex, after_lex) = LexicalDeclaration::parse(parser, scanner, true, yield_flag, await_flag)?;
                Ok((Rc::new(Declaration::Lexical(lex)), after_lex))
            })
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            Declaration::Hoistable(node) => node.bound_names(),
            Declaration::Class(node) => node.bound_names(),
            Declaration::Lexical(node) => node.bound_names(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            Declaration::Hoistable(node) => node.contains(kind),
            Declaration::Class(node) => node.contains(kind),
            Declaration::Lexical(node) => node.contains(kind),
        }
    }
}

// HoistableDeclaration[Yield, Await, Default] :
//      FunctionDeclaration[?Yield, ?Await, ?Default]
//      GeneratorDeclaration[?Yield, ?Await, ?Default]
//      AsyncFunctionDeclaration[?Yield, ?Await, ?Default]
//      AsyncGeneratorDeclaration[?Yield, ?Await, ?Default]
#[derive(Debug)]
pub enum HoistableDeclaration {
    Function(Rc<FunctionDeclaration>),
    Generator(Rc<GeneratorDeclaration>),
    AsyncFunction(Rc<AsyncFunctionDeclaration>),
    AsyncGenerator(Rc<AsyncGeneratorDeclaration>),
}

impl fmt::Display for HoistableDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            HoistableDeclaration::Function(node) => node.fmt(f),
            HoistableDeclaration::Generator(node) => node.fmt(f),
            HoistableDeclaration::AsyncFunction(node) => node.fmt(f),
            HoistableDeclaration::AsyncGenerator(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for HoistableDeclaration {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}HoistableDeclaration: {}", first, self)?;
        match &self {
            HoistableDeclaration::Function(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            HoistableDeclaration::Generator(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            HoistableDeclaration::AsyncFunction(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            HoistableDeclaration::AsyncGenerator(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            HoistableDeclaration::Function(node) => node.concise_with_leftpad(writer, pad, state),
            HoistableDeclaration::Generator(node) => node.concise_with_leftpad(writer, pad, state),
            HoistableDeclaration::AsyncFunction(node) => node.concise_with_leftpad(writer, pad, state),
            HoistableDeclaration::AsyncGenerator(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl HoistableDeclaration {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, default_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("HoistableDeclaration expected", scanner.line, scanner.column))
            .otherwise(|| {
                let (func, after_func) = FunctionDeclaration::parse(parser, scanner, yield_flag, await_flag, default_flag)?;
                Ok((Rc::new(HoistableDeclaration::Function(func)), after_func))
            })
            .otherwise(|| {
                let (gen, after_gen) = GeneratorDeclaration::parse(parser, scanner, yield_flag, await_flag, default_flag)?;
                Ok((Rc::new(HoistableDeclaration::Generator(gen)), after_gen))
            })
            .otherwise(|| {
                let (afun, after_afun) = AsyncFunctionDeclaration::parse(parser, scanner, yield_flag, await_flag, default_flag)?;
                Ok((Rc::new(HoistableDeclaration::AsyncFunction(afun)), after_afun))
            })
            .otherwise(|| {
                let (agen, after_agen) = AsyncGeneratorDeclaration::parse(parser, scanner, yield_flag, await_flag, default_flag)?;
                Ok((Rc::new(HoistableDeclaration::AsyncGenerator(agen)), after_agen))
            })
    }

    pub fn bound_names(&self) -> Vec<JSString> {
        match self {
            HoistableDeclaration::Function(node) => node.bound_names(),
            HoistableDeclaration::Generator(node) => node.bound_names(),
            HoistableDeclaration::AsyncFunction(node) => node.bound_names(),
            HoistableDeclaration::AsyncGenerator(node) => node.bound_names(),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            HoistableDeclaration::Function(node) => node.contains(kind),
            HoistableDeclaration::Generator(node) => node.contains(kind),
            HoistableDeclaration::AsyncFunction(node) => node.contains(kind),
            HoistableDeclaration::AsyncGenerator(node) => node.contains(kind),
        }
    }
}

// BreakableStatement[Yield, Await, Return] :
//      IterationStatement[?Yield, ?Await, ?Return]
//      SwitchStatement[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum BreakableStatement {
    Iteration(Rc<IterationStatement>),
    Switch(Rc<SwitchStatement>),
}

impl fmt::Display for BreakableStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            BreakableStatement::Iteration(node) => node.fmt(f),
            BreakableStatement::Switch(node) => node.fmt(f),
        }
    }
}

impl PrettyPrint for BreakableStatement {
    fn pprint_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        let (first, successive) = prettypad(pad, state);
        writeln!(writer, "{}BreakableStatement: {}", first, self)?;
        match &self {
            BreakableStatement::Iteration(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            BreakableStatement::Switch(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }

    fn concise_with_leftpad<T>(&self, writer: &mut T, pad: &str, state: Spot) -> IoResult<()>
    where
        T: Write,
    {
        match self {
            BreakableStatement::Iteration(node) => node.concise_with_leftpad(writer, pad, state),
            BreakableStatement::Switch(node) => node.concise_with_leftpad(writer, pad, state),
        }
    }
}

impl BreakableStatement {
    pub fn parse(parser: &mut Parser, scanner: Scanner, yield_flag: bool, await_flag: bool, return_flag: bool) -> ParseResult<Self> {
        Err(ParseError::new("BreakableStatement expected", scanner.line, scanner.column))
            .otherwise(|| {
                let (iter, after_iter) = IterationStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(BreakableStatement::Iteration(iter)), after_iter))
            })
            .otherwise(|| {
                let (switch, after_switch) = SwitchStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
                Ok((Rc::new(BreakableStatement::Switch(switch)), after_switch))
            })
    }

    pub fn var_declared_names(&self) -> Vec<JSString> {
        match self {
            BreakableStatement::Iteration(node) => node.var_declared_names(),
            BreakableStatement::Switch(node) => node.var_declared_names(),
        }
    }

    pub fn contains_undefined_break_target(&self, label_set: &[JSString]) -> bool {
        match self {
            BreakableStatement::Iteration(node) => node.contains_undefined_break_target(label_set),
            BreakableStatement::Switch(node) => node.contains_undefined_break_target(label_set),
        }
    }

    pub fn contains(&self, kind: ParseNodeKind) -> bool {
        match self {
            BreakableStatement::Iteration(node) => node.contains(kind),
            BreakableStatement::Switch(node) => node.contains(kind),
        }
    }
}

#[cfg(test)]
mod tests;
