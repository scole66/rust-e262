#![allow(clippy::wildcard_imports)]
#![expect(clippy::doc_markdown)]
#![expect(clippy::float_cmp)]
#![expect(clippy::similar_names)]
#![expect(clippy::single_match_else)]
#![expect(clippy::too_many_lines)]
// nursery denies.
#![deny(clippy::empty_line_after_doc_comments)]

mod agent;
mod arguments_object;
mod arrays;
mod bigint_object;
mod boolean_object;
mod bound_function;
mod chunk;
mod comparison;
mod compiler;
mod control_abstraction;
mod cr;
mod date_object;
mod dtoa_r;
mod environment_record;
mod errors;
mod execution_context;
mod function_object;
mod map;
mod math;
mod number_object;
mod object;
mod object_object;
mod parser;
mod prettyprint;
mod proxy_object;
mod realm;
mod reference;
mod reflect;
mod regexp;
mod scanner;
mod string_object;
mod strings;
mod symbol_object;
mod values;

pub(crate) use crate::agent::*;
pub(crate) use crate::arguments_object::*;
pub(crate) use crate::arrays::*;
pub(crate) use crate::bigint_object::*;
pub(crate) use crate::boolean_object::*;
pub(crate) use crate::bound_function::*;
pub(crate) use crate::chunk::*;
pub(crate) use crate::comparison::*;
pub(crate) use crate::compiler::*;
pub(crate) use crate::control_abstraction::*;
pub(crate) use crate::cr::*;
pub(crate) use crate::date_object::*;
pub(crate) use crate::dtoa_r::*;
pub(crate) use crate::environment_record::*;
pub(crate) use crate::errors::*;
pub(crate) use crate::execution_context::*;
pub(crate) use crate::function_object::*;
pub(crate) use crate::map::*;
pub(crate) use crate::math::*;
pub(crate) use crate::number_object::*;
pub(crate) use crate::object::*;
pub(crate) use crate::object_object::*;
pub(crate) use crate::parser::*;
pub(crate) use crate::prettyprint::*;
pub(crate) use crate::proxy_object::*;
pub(crate) use crate::realm::*;
pub(crate) use crate::reference::*;
pub(crate) use crate::reflect::*;
pub(crate) use crate::regexp::*;
pub(crate) use crate::scanner::*;
pub(crate) use crate::string_object::*;
pub(crate) use crate::strings::*;
pub(crate) use crate::symbol_object::*;
pub(crate) use crate::values::*;

use std::cell::RefCell;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::rc::Rc;

const GOODOBJ: &str = "algorithmically created objects should not have strange behaviors";
const GOODCSTR: &str = "built-in contructors should not have strange behaviors";

#[derive(Debug)]
#[expect(dead_code)]
struct VM {
    // Holds the state for the virtual machine. Anything shared between agents winds up here.
    symbols: Rc<RefCell<SymbolRegistry>>,
}

impl VM {
    fn new() -> Self {
        let sym_registry = Rc::new(RefCell::new(SymbolRegistry::new()));
        AGENT.with(|agent| {
            agent.set_global_symbol_registry(sym_registry.clone());
            initialize_host_defined_realm(0, true);
        });
        VM { symbols: sym_registry }
    }

    //fn compile(&mut self, _ast: &AST) -> Result<i32, String> {
    //    Ok(0)
    //}

    //fn run(&mut self) -> Result<i32, String> {
    //    Ok(0)
    //}
}

fn interpret(source: &str) -> Result<i32, String> {
    let parsed = parse_text(source, ParseGoal::Script, false, false);
    match parsed {
        ParsedText::Errors(errs) => {
            for err in errs {
                println!("{}", err.to_string().unwrap());
            }
            Err("See above".to_string())
        }
        ParsedText::Script(node) => {
            node.pprint_concise(&mut io::stdout()).expect("Output Error");
            Ok(0)
        }
        _ => unreachable!(),
    }
}

fn repl() {
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

        println!("You entered the string {line:?}");
        match interpret(&line) {
            Ok(value) => println!("{value}"),
            Err(err) => println!("{err}"),
        }
    }
}

fn run_file(fname: &str) {
    let potential_file_content = fs::read(fname);
    match potential_file_content {
        Err(e) => println!("{e}"),
        Ok(file_content) => {
            let script_source = String::from_utf8_lossy(&file_content);
            match process_ecmascript(&script_source) {
                Ok(value) => println!("{value:?}"),
                Err(err) => println!("{err}"),
            }
        }
    }
}

fn run_app() -> Result<(), i32> {
    VM::new();
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => repl(),
        2 => run_file(&args[1]),
        _ => {
            eprintln!("Usage: {} [path]", &args[0]);
            return Err(2);
        }
    }

    Ok(())
}

fn main() {
    std::process::exit(match run_app() {
        Ok(()) => 0,
        Err(err) => err,
    });
}

#[cfg(test)]
//#[expect(hidden_glob_reexports)]
mod tests;
