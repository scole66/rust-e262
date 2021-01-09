#![allow(dead_code, unused_variables)]

use std::env;
use std::io::{self, Write};

mod parser;
mod prettyprint;
mod scanner;

use parser::comma_operator::Expression;
use parser::unary_operators::UnaryExpression;
use parser::Parser;
use prettyprint::PrettyPrint;
use scanner::Scanner;

//////// 13.2 Block

// StatementList[Yield, Await, Return]:
//    StatementListItem[?Yield, ?Await, ?Return]
//    StatementList[?Yield, ?Await, ?Return] StatementListItem[?Yield, ?Await, ?Return]
#[derive(Debug)]
struct StatementlistStatementlistitem {
    statement_list_item: Box<StatementListItem>,
}
#[derive(Debug)]
struct StatementlistStatementlistStatementlistitem {
    statement_list: Box<StatementList>,
    statement_list_item: Box<StatementListItem>,
}
#[derive(Debug)]
enum StatementlistProduction {
    StatementListItem(StatementlistStatementlistitem),
    StatementListStatmentListItem(StatementlistStatementlistStatementlistitem),
}
#[derive(Debug)]
struct StatementList {
    yield_arg: bool,
    await_arg: bool,
    return_arg: bool,
    production: Box<StatementlistProduction>,
}

// StatementListItem[Yield, Await, Return]:
//    Statement[?Yield, ?Await, ?Return]
//    Declaration[?Yield, ?Await]
#[derive(Debug)]
struct StatementlistitemStatement {
    statement: Box<Statement>,
}
#[derive(Debug)]
struct StatementlistitemDeclaration {
    declaration: Box<Declaration>,
}
#[derive(Debug)]
enum StatementlistitemProduction {
    Statement(StatementlistitemStatement),
    Declaration(StatementlistitemDeclaration),
}
#[derive(Debug)]
struct StatementListItem {
    yield_arg: bool,
    await_arg: bool,
    return_arg: bool,
    production: StatementlistitemProduction,
}

// tbd
#[derive(Debug)]
struct Statement {
    faux: String,
}
#[derive(Debug)]
struct Declaration {
    faux: String,
}

//////// 15.1 Scripts

// Script:
//    ScriptBody[opt]
#[derive(Debug)]
struct Script {
    script_body: Option<Box<ScriptBody>>,
}

// ScriptBody:
//    StatementList[~Yield, ~Await, ~Return]
#[derive(Debug)]
struct ScriptBody {
    statement_list: Box<StatementList>,
}

#[derive(Debug)]
struct VM {
    // Holds the state for the virtual machine. Anything shared between execution contexts winds up here.
}

impl VM {
    fn new() -> VM {
        VM {}
    }

    fn compile(&mut self, _ast: &AST) -> Result<i32, String> {
        Ok(0)
    }

    fn run(&mut self) -> Result<i32, String> {
        Ok(0)
    }
}

fn script<'a>(_scanner: &'a mut Scanner) -> Result<Box<Script>, String> {
    //let ch = scanner.scan_token(ScanGoal::InputElementRegExp);
    Ok(Box::new(Script { script_body: None }))
}

#[derive(Debug)]
struct AST {
    script: Box<Script>,
}
impl AST {
    fn generate(_source: &str) -> Result<Box<AST>, String> {
        let mut scanner = Scanner::new();
        script(&mut scanner).and_then(|s| Ok(Box::new(AST { script: s })))
    }
}

fn generate_ast<'a>(_vm: &'a VM, source: &'a str) -> Result<Box<AST>, String> {
    AST::generate(source)
}

fn interpret(_vm: &mut VM, source: &str) -> Result<i32, String> {
    //generate_ast(vm, source).and_then(|ast| vm.compile(&ast)).and_then(|_| vm.run())
    // let result = scanner::scan_token(
    //     &Scanner::new(),
    //     source,
    //     scanner::ScanGoal::InputElementRegExp,
    // );
    let mut parser = Parser::new(source, false, parser::ParseGoal::Script);
    let result = Expression::parse(&mut parser, Scanner::new(), true, false, false);
    match result {
        Ok(Some((node, _))) => {
            node.pprint(&mut io::stdout()).expect("Output Error");
            Ok(0)
        }
        Ok(None) => Ok(0),
        Err(msg) => Err(msg),
    }
}

fn repl(vm: &mut VM) {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        io::stdin().read_line(&mut line).expect("Failed to read line");
        let linelen = line.len();
        if linelen == 0 {
            println!("");
            break;
        }

        println!("You entered the string {:?}", line);
        match interpret(vm, &line) {
            Ok(value) => println!("{}", value),
            Err(err) => println!("{}", err),
        }
    }
}

fn run_file(_vm: &mut VM, fname: &str) {
    println!("Running from the file {}", fname);
}

fn run_app() -> Result<(), i32> {
    let mut vm: VM = VM::new();
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => repl(&mut vm),
        2 => run_file(&mut vm, &args[1]),
        _ => {
            eprintln!("Usage: {} [path]", &args[0]);
            return Err(2);
        }
    }

    Ok(())
}

fn main() {
    std::process::exit(match run_app() {
        Ok(_) => 0,
        Err(err) => err,
    });
}

//#[cfg(test)]
//mod tests {
//    use super::*;
//    use crate::prettyprint::testhelp::pretty_check;
//    use parser::testhelp::{check, check_none, chk_scan, newparser};
//
//}
