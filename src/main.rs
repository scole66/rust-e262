#![feature(ptr_metadata)]
#![allow(dead_code)]
#![allow(clippy::bool_assert_comparison)]
#![allow(clippy::enum_variant_names)]

use std::env;
use std::fs;
use std::io::{self, Write};

mod agent;
mod arrays;
mod bigint_object;
mod boolean_object;
mod chunk;
mod comparison;
mod compiler;
mod cr;
mod dtoa_r;
mod environment_record;
mod errors;
mod execution_context;
mod function_object;
mod number_object;
mod object;
mod object_object;
mod parser;
mod prettyprint;
mod realm;
mod reference;
mod scanner;
mod string_object;
mod strings;
mod symbol_object;
mod values;

use agent::{process_ecmascript, Agent};
use parser::{parse_text, ParseGoal, ParsedText};
use prettyprint::PrettyPrint;
use values::to_string;

#[derive(Debug)]
struct VM {
    // Holds the state for the virtual machine. Anything shared between execution contexts winds up here.
    pub agent: Agent,
}

impl VM {
    fn new() -> VM {
        let mut agent = Agent::new();
        agent.initialize_host_defined_realm(false);
        VM { agent }
    }

    //fn compile(&mut self, _ast: &AST) -> Result<i32, String> {
    //    Ok(0)
    //}

    fn run(&mut self) -> Result<i32, String> {
        Ok(0)
    }
}

fn interpret(vm: &mut VM, source: &str) -> Result<i32, String> {
    let parsed = parse_text(&mut vm.agent, source, ParseGoal::Script);
    match parsed {
        ParsedText::Errors(errs) => {
            for err in errs {
                println!("{}", to_string(&mut vm.agent, err).unwrap());
            }
            Err("See above".to_string())
        }
        ParsedText::Script(node) => {
            node.pprint_concise(&mut io::stdout()).expect("Output Error");
            Ok(0)
        }
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

fn run_file(vm: &mut VM, fname: &str) {
    println!("Running from the file {}", fname);
    let potential_file_content = fs::read(fname);
    match potential_file_content {
        Err(e) => println!("{}", e),
        Ok(file_content) => {
            let script_source = String::from_utf8_lossy(&file_content);
            match process_ecmascript(&mut vm.agent, &script_source) {
                Ok(value) => println!("{:?}", value),
                Err(err) => println!("{}", err),
            }
            //match interpret(vm, &script_source) {
            //    Ok(value) => println!("{}", value),
            //    Err(err) => println!("{}", err),
            //}
        }
    }
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

#[cfg(test)]
mod tests;
