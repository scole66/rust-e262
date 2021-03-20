#![allow(dead_code, unused_variables)]
#![feature(cmp_min_max_by)]

use std::env;
use std::io::{self, Write};

mod agent;
mod cr;
mod dtoa_r;
mod errors;
mod object;
mod parser;
mod prettyprint;
mod scanner;
mod strings;
mod values;

use parser::block::StatementList;
use parser::Parser;
use prettyprint::PrettyPrint;
use scanner::Scanner;

#[derive(Debug)]
struct VM {
    // Holds the state for the virtual machine. Anything shared between execution contexts winds up here.
}

impl VM {
    fn new() -> VM {
        VM {}
    }

    //fn compile(&mut self, _ast: &AST) -> Result<i32, String> {
    //    Ok(0)
    //}

    fn run(&mut self) -> Result<i32, String> {
        Ok(0)
    }
}

fn interpret(_vm: &mut VM, source: &str) -> Result<i32, String> {
    let mut parser = Parser::new(source, false, parser::ParseGoal::Script);
    let result = StatementList::parse(&mut parser, Scanner::new(), false, false, false);
    match result {
        Ok((node, _)) => {
            node.pprint_concise(&mut io::stdout()).expect("Output Error");
            Ok(0)
        }
        Err(err) => Err(format!("{}:{}: {}", err.line, err.column, err.msg)),
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
            println!();
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
