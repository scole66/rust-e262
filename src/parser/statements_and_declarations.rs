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
    Block(Box<BlockStatement>),
    Variable(Box<VariableStatement>),
    Empty(Box<EmptyStatement>),
    Expression(Box<ExpressionStatement>),
    If(Box<IfStatement>),
    Breakable(Box<BreakableStatement>),
    Continue(Box<ContinueStatement>),
    Break(Box<BreakStatement>),
    Return(Box<ReturnStatement>),
    With(Box<WithStatement>),
    Labelled(Box<LabelledStatement>),
    Throw(Box<ThrowStatement>),
    Try(Box<TryStatement>),
    Debugger(Box<DebuggerStatement>),
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
}

impl Statement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_block = BlockStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        if let Some((block, after_block)) = pot_block {
            return Ok(Some((Box::new(Statement::Block(block)), after_block)));
        }

        let pot_var = VariableStatement::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((var, after_var)) = pot_var {
            return Ok(Some((Box::new(Statement::Variable(var)), after_var)));
        }

        let pot_empty = EmptyStatement::parse(parser, scanner)?;
        if let Some((empty, after_empty)) = pot_empty {
            return Ok(Some((Box::new(Statement::Empty(empty)), after_empty)));
        }

        let pot_exp = ExpressionStatement::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((exp, after_exp)) = pot_exp {
            return Ok(Some((Box::new(Statement::Expression(exp)), after_exp)));
        }

        let pot_if = IfStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        if let Some((if_node, after_if)) = pot_if {
            return Ok(Some((Box::new(Statement::If(if_node)), after_if)));
        }

        let pot_bable = BreakableStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        if let Some((bable_node, after_bable)) = pot_bable {
            return Ok(Some((Box::new(Statement::Breakable(bable_node)), after_bable)));
        }

        let pot_cont = ContinueStatement::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((cont_node, after_cont)) = pot_cont {
            return Ok(Some((Box::new(Statement::Continue(cont_node)), after_cont)));
        }

        let pot_break = BreakStatement::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((break_node, after_break)) = pot_break {
            return Ok(Some((Box::new(Statement::Break(break_node)), after_break)));
        }

        let pot_return = ReturnStatement::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((return_node, after_return)) = pot_return {
            return Ok(Some((Box::new(Statement::Return(return_node)), after_return)));
        }

        let pot_with = WithStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        if let Some((with_node, after_with)) = pot_with {
            return Ok(Some((Box::new(Statement::With(with_node)), after_with)));
        }

        let pot_lbl = LabelledStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        if let Some((lbl_node, after_lbl)) = pot_lbl {
            return Ok(Some((Box::new(Statement::Labelled(lbl_node)), after_lbl)));
        }

        let pot_throw = ThrowStatement::parse(parser, scanner, yield_flag, await_flag)?;
        if let Some((throw_node, after_throw)) = pot_throw {
            return Ok(Some((Box::new(Statement::Throw(throw_node)), after_throw)));
        }

        let pot_try = TryStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        if let Some((try_node, after_try)) = pot_try {
            return Ok(Some((Box::new(Statement::Try(try_node)), after_try)));
        }

        let pot_dbg = DebuggerStatement::parse(parser, scanner)?;
        if let Some((dbg_node, after_dbg)) = pot_dbg {
            return Ok(Some((Box::new(Statement::Debugger(dbg_node)), after_dbg)));
        }

        Ok(None)
    }
}

// Declaration[Yield, Await] :
//      HoistableDeclaration[?Yield, ?Await, ~Default]
//      ClassDeclaration[?Yield, ?Await, ~Default]
//      LexicalDeclaration[+In, ?Yield, ?Await]
#[derive(Debug)]
pub enum Declaration {
    Hoistable(Box<HoistableDeclaration>),
    Class(Box<ClassDeclaration>),
    Lexical(Box<LexicalDeclaration>),
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
}

impl Declaration {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_hoist = HoistableDeclaration::parse(parser, scanner, yield_flag, await_flag, false)?;
        if let Some((hoist, after_hoist)) = pot_hoist {
            return Ok(Some((Box::new(Declaration::Hoistable(hoist)), after_hoist)));
        }
        let pot_cls = ClassDeclaration::parse(parser, scanner, yield_flag, await_flag, false)?;
        if let Some((cls, after_cls)) = pot_cls {
            return Ok(Some((Box::new(Declaration::Class(cls)), after_cls)));
        }
        let pot_lex = LexicalDeclaration::parse(parser, scanner, true, yield_flag, await_flag)?;
        if let Some((lex, after_lex)) = pot_lex {
            return Ok(Some((Box::new(Declaration::Lexical(lex)), after_lex)));
        }

        Ok(None)
    }
}

// HoistableDeclaration[Yield, Await, Default] :
//      FunctionDeclaration[?Yield, ?Await, ?Default]
//      GeneratorDeclaration[?Yield, ?Await, ?Default]
//      AsyncFunctionDeclaration[?Yield, ?Await, ?Default]
//      AsyncGeneratorDeclaration[?Yield, ?Await, ?Default]
#[derive(Debug)]
pub enum HoistableDeclaration {
    Function(Box<FunctionDeclaration>),
    Generator(Box<GeneratorDeclaration>),
    AsyncFunction(Box<AsyncFunctionDeclaration>),
    AsyncGenerator(Box<AsyncGeneratorDeclaration>),
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
        writeln!(writer, "{}Statement: {}", first, self)?;
        match &self {
            HoistableDeclaration::Function(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            HoistableDeclaration::Generator(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            HoistableDeclaration::AsyncFunction(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            HoistableDeclaration::AsyncGenerator(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
}

impl HoistableDeclaration {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        default_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_func = FunctionDeclaration::parse(parser, scanner, yield_flag, await_flag, default_flag)?;
        if let Some((func, after_func)) = pot_func {
            return Ok(Some((Box::new(HoistableDeclaration::Function(func)), after_func)));
        }
        let pot_gen = GeneratorDeclaration::parse(parser, scanner, yield_flag, await_flag, default_flag)?;
        if let Some((gen, after_gen)) = pot_gen {
            return Ok(Some((Box::new(HoistableDeclaration::Generator(gen)), after_gen)));
        }
        let pot_afun = AsyncFunctionDeclaration::parse(parser, scanner, yield_flag, await_flag, default_flag)?;
        if let Some((afun, after_afun)) = pot_afun {
            return Ok(Some((Box::new(HoistableDeclaration::AsyncFunction(afun)), after_afun)));
        }
        let pot_agen = AsyncGeneratorDeclaration::parse(parser, scanner, yield_flag, await_flag, default_flag)?;
        if let Some((agen, after_agen)) = pot_agen {
            return Ok(Some((Box::new(HoistableDeclaration::AsyncGenerator(agen)), after_agen)));
        }
        Ok(None)
    }
}

// BreakableStatement[Yield, Await, Return] :
//      IterationStatement[?Yield, ?Await, ?Return]
//      SwitchStatement[?Yield, ?Await, ?Return]
#[derive(Debug)]
pub enum BreakableStatement {
    Iteration(Box<IterationStatement>),
    Switch(Box<SwitchStatement>),
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
        writeln!(writer, "{}Statement: {}", first, self)?;
        match &self {
            BreakableStatement::Iteration(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
            BreakableStatement::Switch(node) => node.pprint_with_leftpad(writer, &successive, Spot::Final),
        }
    }
}

impl BreakableStatement {
    pub fn parse(
        parser: &mut Parser,
        scanner: Scanner,
        yield_flag: bool,
        await_flag: bool,
        return_flag: bool,
    ) -> Result<Option<(Box<Self>, Scanner)>, String> {
        let pot_iter = IterationStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        if let Some((iter, after_iter)) = pot_iter {
            return Ok(Some((Box::new(BreakableStatement::Iteration(iter)), after_iter)));
        }

        let pot_switch = SwitchStatement::parse(parser, scanner, yield_flag, await_flag, return_flag)?;
        if let Some((switch, after_switch)) = pot_switch {
            return Ok(Some((Box::new(BreakableStatement::Switch(switch)), after_switch)));
        }

        Ok(None)
    }
}

//#[cfg(test)]
//mod tests {
//    use super::testhelp::{check, check_none, chk_scan, newparser};
//    use super::*;
//    use crate::prettyprint::testhelp::{pretty_check, pretty_error_validate};
//}
