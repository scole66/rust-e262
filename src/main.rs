#![allow(dead_code)]
#![allow(clippy::bool_assert_comparison)]
#![allow(clippy::enum_variant_names)]
phooey
use std::env;
use std::fs;
use std::io::{self, Write};

mod agent;
mod arrays;
mod bigint_object;
mod boolean_object;
mod comparison;
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

use parser::scripts::Script;
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
    let mut parser = Parser::new(source, false, false, parser::ParseGoal::Script);
    let result = Script::parse(&mut parser, Scanner::new());
    match result {
        Ok((node, _)) => {
            node.pprint_concise(&mut io::stdout()).expect("Output Error");
            Ok(0)
        }
        Err(err) => Err(format!("{}:{}: {}", err.line, err.column, err.msg)),
    }
}

use agent::Agent;
use errors::create_syntax_error_object;
use object::get;
use realm::IntrinsicId;
use values::{to_object, PropertyKey};

fn repl(vm: &mut VM) {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let e = create_syntax_error_object(&mut agent, "Test Message");
    println!("e: {:#?}", e);
    let syntax_error_proto = e.o.get_prototype_of().unwrap().unwrap();
    println!("e.[[Prototype]]: {:#?}", syntax_error_proto);
    if syntax_error_proto == agent.intrinsic(IntrinsicId::SyntaxErrorPrototype) {
        println!("    (aka %SyntaxError.Prototype%)")
    }
    let syntax_error_constructor_val = get(&mut agent, &syntax_error_proto, &PropertyKey::from("constructor")).unwrap();
    let syntax_error_constructor = to_object(&mut agent, syntax_error_constructor_val).unwrap();
    println!("e.[[Prototype]].constructor: {:#?}", syntax_error_constructor);
    if syntax_error_constructor == agent.intrinsic(IntrinsicId::SyntaxError) {
        println!("    (aka %SyntaxError%)");
    }
    let syntax_error_other_proto_val = get(&mut agent, &syntax_error_constructor, &PropertyKey::from("prototype")).unwrap();
    let syntax_error_other_proto = to_object(&mut agent, syntax_error_other_proto_val).unwrap();
    println!("e.[[Prototype]].constructor.prototype: {:#?}", syntax_error_other_proto);
    if syntax_error_other_proto == agent.intrinsic(IntrinsicId::SyntaxErrorPrototype) {
        println!("    (aka %SyntaxError.Prototype%)");
    }

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
            match interpret(vm, &script_source) {
                Ok(value) => println!("{}", value),
                Err(err) => println!("{}", err),
            }
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
