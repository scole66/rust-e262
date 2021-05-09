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
}

#[cfg(test)]
mod tests {
    use super::testhelp::{check, check_err, chk_scan, newparser};
    use super::*;
    use crate::prettyprint::testhelp::{concise_check, concise_error_validate, pretty_check, pretty_error_validate};

    // STATEMENT
    #[test]
    fn statement_test_01() {
        let (node, scanner) = check(Statement::parse(&mut newparser("{ a=0; }"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 8);
        pretty_check(&*node, "Statement: { a = 0 ; }", vec!["BlockStatement: { a = 0 ; }"]);
        concise_check(&*node, "Block: { a = 0 ; }", vec!["Punctuator: {", "ExpressionStatement: a = 0 ;", "Punctuator: }"]);
        format!("{:?}", node);
    }
    #[test]
    fn statement_test_02() {
        let (node, scanner) = check(Statement::parse(&mut newparser("var a=0;"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 8);
        pretty_check(&*node, "Statement: var a = 0 ;", vec!["VariableStatement: var a = 0 ;"]);
        concise_check(&*node, "VariableStatement: var a = 0 ;", vec!["Keyword: var", "VariableDeclaration: a = 0", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn statement_test_03() {
        let (node, scanner) = check(Statement::parse(&mut newparser(";"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 1);
        pretty_check(&*node, "Statement: ;", vec!["EmptyStatement: ;"]);
        concise_check(&*node, "Punctuator: ;", vec![]);
        format!("{:?}", node);
    }
    #[test]
    fn statement_test_04() {
        let (node, scanner) = check(Statement::parse(&mut newparser("a();"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 4);
        pretty_check(&*node, "Statement: a ( ) ;", vec!["ExpressionStatement: a ( ) ;"]);
        concise_check(&*node, "ExpressionStatement: a ( ) ;", vec!["CallMemberExpression: a ( )", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn statement_test_05() {
        let (node, scanner) = check(Statement::parse(&mut newparser("if (a) {}"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 9);
        pretty_check(&*node, "Statement: if ( a ) { }", vec!["IfStatement: if ( a ) { }"]);
        concise_check(&*node, "IfStatement: if ( a ) { }", vec!["Keyword: if", "Punctuator: (", "IdentifierName: a", "Punctuator: )", "Block: { }"]);
        format!("{:?}", node);
    }
    #[test]
    fn statement_test_06() {
        let (node, scanner) = check(Statement::parse(&mut newparser("switch (a) {}"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 13);
        pretty_check(&*node, "Statement: switch ( a ) { }", vec!["BreakableStatement: switch ( a ) { }"]);
        concise_check(&*node, "SwitchStatement: switch ( a ) { }", vec!["Keyword: switch", "Punctuator: (", "IdentifierName: a", "Punctuator: )", "CaseBlock: { }"]);
        format!("{:?}", node);
    }
    #[test]
    fn statement_test_07() {
        let (node, scanner) = check(Statement::parse(&mut newparser("continue;"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 9);
        pretty_check(&*node, "Statement: continue ;", vec!["ContinueStatement: continue ;"]);
        concise_check(&*node, "ContinueStatement: continue ;", vec!["Keyword: continue", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn statement_test_08() {
        let (node, scanner) = check(Statement::parse(&mut newparser("break;"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 6);
        pretty_check(&*node, "Statement: break ;", vec!["BreakStatement: break ;"]);
        concise_check(&*node, "BreakStatement: break ;", vec!["Keyword: break", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn statement_test_09() {
        let (node, scanner) = check(Statement::parse(&mut newparser("return;"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 7);
        pretty_check(&*node, "Statement: return ;", vec!["ReturnStatement: return ;"]);
        concise_check(&*node, "ReturnStatement: return ;", vec!["Keyword: return", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn statement_test_10() {
        let (node, scanner) = check(Statement::parse(&mut newparser("with(a)b;"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 9);
        pretty_check(&*node, "Statement: with ( a ) b ;", vec!["WithStatement: with ( a ) b ;"]);
        concise_check(&*node, "WithStatement: with ( a ) b ;", vec!["Keyword: with", "Punctuator: (", "IdentifierName: a", "Punctuator: )", "ExpressionStatement: b ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn statement_test_11() {
        let (node, scanner) = check(Statement::parse(&mut newparser("a:b;"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 4);
        pretty_check(&*node, "Statement: a : b ;", vec!["LabelledStatement: a : b ;"]);
        concise_check(&*node, "LabelledStatement: a : b ;", vec!["IdentifierName: a", "Punctuator: :", "ExpressionStatement: b ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn statement_test_12() {
        let (node, scanner) = check(Statement::parse(&mut newparser("throw a;"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 8);
        pretty_check(&*node, "Statement: throw a ;", vec!["ThrowStatement: throw a ;"]);
        concise_check(&*node, "ThrowStatement: throw a ;", vec!["Keyword: throw", "IdentifierName: a", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn statement_test_13() {
        let (node, scanner) = check(Statement::parse(&mut newparser("try {} catch {}"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 15);
        pretty_check(&*node, "Statement: try { } catch { }", vec!["TryStatement: try { } catch { }"]);
        concise_check(&*node, "TryStatement: try { } catch { }", vec!["Keyword: try", "Block: { }", "Catch: catch { }"]);
        format!("{:?}", node);
    }
    #[test]
    fn statement_test_14() {
        let (node, scanner) = check(Statement::parse(&mut newparser("debugger;"), Scanner::new(), false, false, true));
        chk_scan(&scanner, 9);
        pretty_check(&*node, "Statement: debugger ;", vec!["DebuggerStatement: debugger ;"]);
        concise_check(&*node, "DebuggerStatement: debugger ;", vec!["Keyword: debugger", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn statement_test_err_01() {
        check_err(Statement::parse(&mut newparser(""), Scanner::new(), false, false, true), "Statement expected", 1, 1);
    }
    #[test]
    fn statement_test_err_02() {
        check_err(Statement::parse(&mut newparser("return;"), Scanner::new(), false, false, false), "Statement expected", 1, 1);
    }
    #[test]
    fn statement_test_prettyerrors_1() {
        let (item, _) = Statement::parse(&mut newparser("{}"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn statement_test_prettyerrors_2() {
        let (item, _) = Statement::parse(&mut newparser("var a;"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn statement_test_prettyerrors_3() {
        let (item, _) = Statement::parse(&mut newparser(";"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn statement_test_prettyerrors_4() {
        let (item, _) = Statement::parse(&mut newparser("if(a)b;"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn statement_test_prettyerrors_5() {
        let (item, _) = Statement::parse(&mut newparser("switch(a){}"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn statement_test_prettyerrors_6() {
        let (item, _) = Statement::parse(&mut newparser("continue;"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn statement_test_prettyerrors_7() {
        let (item, _) = Statement::parse(&mut newparser("break;"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn statement_test_prettyerrors_8() {
        let (item, _) = Statement::parse(&mut newparser("return;"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn statement_test_prettyerrors_9() {
        let (item, _) = Statement::parse(&mut newparser("with(a){};"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn statement_test_prettyerrors_10() {
        let (item, _) = Statement::parse(&mut newparser("a:b;"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn statement_test_prettyerrors_11() {
        let (item, _) = Statement::parse(&mut newparser("throw a;"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn statement_test_prettyerrors_12() {
        let (item, _) = Statement::parse(&mut newparser("try{}catch{}"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn statement_test_prettyerrors_13() {
        let (item, _) = Statement::parse(&mut newparser("debugger;"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn statement_test_prettyerrors_14() {
        let (item, _) = Statement::parse(&mut newparser("a();"), Scanner::new(), false, false, true).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn statement_test_conciseerrors_1() {
        let (item, _) = Statement::parse(&mut newparser("{}"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn statement_test_conciseerrors_2() {
        let (item, _) = Statement::parse(&mut newparser("var a;"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn statement_test_conciseerrors_3() {
        let (item, _) = Statement::parse(&mut newparser(";"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn statement_test_conciseerrors_4() {
        let (item, _) = Statement::parse(&mut newparser("if(a)b;"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn statement_test_conciseerrors_5() {
        let (item, _) = Statement::parse(&mut newparser("switch(a){}"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn statement_test_conciseerrors_6() {
        let (item, _) = Statement::parse(&mut newparser("continue;"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn statement_test_conciseerrors_7() {
        let (item, _) = Statement::parse(&mut newparser("break;"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn statement_test_conciseerrors_8() {
        let (item, _) = Statement::parse(&mut newparser("return;"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn statement_test_conciseerrors_9() {
        let (item, _) = Statement::parse(&mut newparser("with(a){};"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn statement_test_conciseerrors_10() {
        let (item, _) = Statement::parse(&mut newparser("a:b;"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn statement_test_conciseerrors_11() {
        let (item, _) = Statement::parse(&mut newparser("throw a;"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn statement_test_conciseerrors_12() {
        let (item, _) = Statement::parse(&mut newparser("try{}catch{}"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn statement_test_conciseerrors_13() {
        let (item, _) = Statement::parse(&mut newparser("debugger;"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn statement_test_conciseerrors_14() {
        let (item, _) = Statement::parse(&mut newparser("a();"), Scanner::new(), false, false, true).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn statement_test_cache_01() {
        let mut parser = newparser("A += 34;");
        let (node, scanner) = check(Statement::parse(&mut parser, Scanner::new(), true, false, false));
        let (node2, scanner2) = check(Statement::parse(&mut parser, Scanner::new(), true, false, false));
        assert!(scanner == scanner2);
        assert!(Rc::ptr_eq(&node, &node2));
    }

    // DECLARATION
    #[test]
    fn declaration_test_01() {
        let (node, scanner) = check(Declaration::parse(&mut newparser("function a(){}"), Scanner::new(), false, false));
        chk_scan(&scanner, 14);
        pretty_check(&*node, "Declaration: function a (  ) {  }", vec!["HoistableDeclaration: function a (  ) {  }"]);
        concise_check(
            &*node,
            "FunctionDeclaration: function a (  ) {  }",
            vec!["Keyword: function", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
        );
        format!("{:?}", node);
    }
    #[test]
    fn declaration_test_02() {
        let (node, scanner) = check(Declaration::parse(&mut newparser("class a{}"), Scanner::new(), false, false));
        chk_scan(&scanner, 9);
        pretty_check(&*node, "Declaration: class a { }", vec!["ClassDeclaration: class a { }"]);
        concise_check(&*node, "ClassDeclaration: class a { }", vec!["Keyword: class", "IdentifierName: a", "ClassTail: { }"]);
        format!("{:?}", node);
    }
    #[test]
    fn declaration_test_03() {
        let (node, scanner) = check(Declaration::parse(&mut newparser("let a;"), Scanner::new(), false, false));
        chk_scan(&scanner, 6);
        pretty_check(&*node, "Declaration: let a ;", vec!["LexicalDeclaration: let a ;"]);
        concise_check(&*node, "LexicalDeclaration: let a ;", vec!["Keyword: let", "IdentifierName: a", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn declaration_test_err_01() {
        check_err(Declaration::parse(&mut newparser(""), Scanner::new(), false, false), "Declaration expected", 1, 1);
    }
    #[test]
    fn declaration_test_prettyerrors_1() {
        let (item, _) = Declaration::parse(&mut newparser("function a(){}"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn declaration_test_prettyerrors_2() {
        let (item, _) = Declaration::parse(&mut newparser("class a{}"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn declaration_test_prettyerrors_3() {
        let (item, _) = Declaration::parse(&mut newparser("let a;"), Scanner::new(), false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn declaration_test_conciseerrors_1() {
        let (item, _) = Declaration::parse(&mut newparser("function a(){}"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn declaration_test_conciseerrors_2() {
        let (item, _) = Declaration::parse(&mut newparser("class a{}"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn declaration_test_conciseerrors_3() {
        let (item, _) = Declaration::parse(&mut newparser("let a;"), Scanner::new(), false, false).unwrap();
        concise_error_validate(&*item);
    }

    // HOISTABLE DECLARATION
    #[test]
    fn hoistable_declaration_test_01() {
        let (node, scanner) = check(HoistableDeclaration::parse(&mut newparser("function a(){}"), Scanner::new(), false, false, false));
        chk_scan(&scanner, 14);
        pretty_check(&*node, "HoistableDeclaration: function a (  ) {  }", vec!["FunctionDeclaration: function a (  ) {  }"]);
        concise_check(
            &*node,
            "FunctionDeclaration: function a (  ) {  }",
            vec!["Keyword: function", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
        );
        format!("{:?}", node);
    }
    #[test]
    fn hoistable_declaration_test_02() {
        let (node, scanner) = check(HoistableDeclaration::parse(&mut newparser("function *a(){}"), Scanner::new(), false, false, false));
        chk_scan(&scanner, 15);
        pretty_check(&*node, "HoistableDeclaration: function * a (  ) {  }", vec!["GeneratorDeclaration: function * a (  ) {  }"]);
        concise_check(
            &*node,
            "GeneratorDeclaration: function * a (  ) {  }",
            vec!["Keyword: function", "Punctuator: *", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
        );
        format!("{:?}", node);
    }
    #[test]
    fn hoistable_declaration_test_03() {
        let (node, scanner) = check(HoistableDeclaration::parse(&mut newparser("async function a(){}"), Scanner::new(), false, false, false));
        chk_scan(&scanner, 20);
        pretty_check(&*node, "HoistableDeclaration: async function a (  ) {  }", vec!["AsyncFunctionDeclaration: async function a (  ) {  }"]);
        concise_check(
            &*node,
            "AsyncFunctionDeclaration: async function a (  ) {  }",
            vec!["Keyword: async", "Keyword: function", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
        );
        format!("{:?}", node);
    }
    #[test]
    fn hoistable_declaration_test_04() {
        let (node, scanner) = check(HoistableDeclaration::parse(&mut newparser("async function *a(){}"), Scanner::new(), false, false, false));
        chk_scan(&scanner, 21);
        pretty_check(&*node, "HoistableDeclaration: async function * a (  ) {  }", vec!["AsyncGeneratorDeclaration: async function * a (  ) {  }"]);
        concise_check(
            &*node,
            "AsyncGeneratorDeclaration: async function * a (  ) {  }",
            vec!["Keyword: async", "Keyword: function", "Punctuator: *", "IdentifierName: a", "Punctuator: (", "Punctuator: )", "Punctuator: {", "Punctuator: }"],
        );
        format!("{:?}", node);
    }
    #[test]
    fn hoistable_declaration_test_err_01() {
        check_err(HoistableDeclaration::parse(&mut newparser(""), Scanner::new(), false, false, false), "HoistableDeclaration expected", 1, 1);
    }
    #[test]
    fn hoistable_declaration_test_prettyerrors_1() {
        let (item, _) = HoistableDeclaration::parse(&mut newparser("function a(){}"), Scanner::new(), false, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn hoistable_declaration_test_prettyerrors_2() {
        let (item, _) = HoistableDeclaration::parse(&mut newparser("function *a(){}"), Scanner::new(), false, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn hoistable_declaration_test_prettyerrors_3() {
        let (item, _) = HoistableDeclaration::parse(&mut newparser("async function a(){}"), Scanner::new(), false, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn hoistable_declaration_test_prettyerrors_4() {
        let (item, _) = HoistableDeclaration::parse(&mut newparser("async function *a(){}"), Scanner::new(), false, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn hoistable_declaration_test_conciseerrors_1() {
        let (item, _) = HoistableDeclaration::parse(&mut newparser("function a(){}"), Scanner::new(), false, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn hoistable_declaration_test_conciseerrors_2() {
        let (item, _) = HoistableDeclaration::parse(&mut newparser("function *a(){}"), Scanner::new(), false, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn hoistable_declaration_test_conciseerrors_3() {
        let (item, _) = HoistableDeclaration::parse(&mut newparser("async function a(){}"), Scanner::new(), false, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn hoistable_declaration_test_conciseerrors_4() {
        let (item, _) = HoistableDeclaration::parse(&mut newparser("async function *a(){}"), Scanner::new(), false, false, false).unwrap();
        concise_error_validate(&*item);
    }

    // BREAKABLE STATEMENT
    #[test]
    fn breakable_statement_test_01() {
        let (node, scanner) = check(BreakableStatement::parse(&mut newparser("while (a);"), Scanner::new(), false, false, false));
        chk_scan(&scanner, 10);
        pretty_check(&*node, "BreakableStatement: while ( a ) ;", vec!["IterationStatement: while ( a ) ;"]);
        concise_check(&*node, "WhileStatement: while ( a ) ;", vec!["Keyword: while", "Punctuator: (", "IdentifierName: a", "Punctuator: )", "Punctuator: ;"]);
        format!("{:?}", node);
    }
    #[test]
    fn breakable_statement_test_02() {
        let (node, scanner) = check(BreakableStatement::parse(&mut newparser("switch(a){}"), Scanner::new(), false, false, false));
        chk_scan(&scanner, 11);
        pretty_check(&*node, "BreakableStatement: switch ( a ) { }", vec!["SwitchStatement: switch ( a ) { }"]);
        concise_check(&*node, "SwitchStatement: switch ( a ) { }", vec!["Keyword: switch", "Punctuator: (", "IdentifierName: a", "Punctuator: )", "CaseBlock: { }"]);
        format!("{:?}", node);
    }
    #[test]
    fn breable_statement_test_err_01() {
        check_err(BreakableStatement::parse(&mut newparser(""), Scanner::new(), false, false, false), "BreakableStatement expected", 1, 1);
    }
    #[test]
    fn breakable_statement_test_prettyerrors_1() {
        let (item, _) = BreakableStatement::parse(&mut newparser("while(a);"), Scanner::new(), false, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn breakable_statement_test_prettyerrors_2() {
        let (item, _) = BreakableStatement::parse(&mut newparser("switch(a){}"), Scanner::new(), false, false, false).unwrap();
        pretty_error_validate(&*item);
    }
    #[test]
    fn breakable_statement_test_conciseerrors_1() {
        let (item, _) = BreakableStatement::parse(&mut newparser("while(a);"), Scanner::new(), false, false, false).unwrap();
        concise_error_validate(&*item);
    }
    #[test]
    fn breakable_statement_test_conciseerrors_2() {
        let (item, _) = BreakableStatement::parse(&mut newparser("switch(a){}"), Scanner::new(), false, false, false).unwrap();
        concise_error_validate(&*item);
    }
}
