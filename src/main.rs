#![allow(clippy::wildcard_imports)]
#![expect(clippy::bool_assert_comparison)]
#![expect(clippy::doc_markdown)]
#![expect(clippy::enum_variant_names)]
#![expect(clippy::float_cmp)]
#![expect(clippy::missing_errors_doc)]
#![expect(clippy::missing_panics_doc)]
#![expect(clippy::module_name_repetitions)]
#![expect(clippy::must_use_candidate)]
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
mod chunk;
mod comparison;
mod compiler;
mod control_abstraction;
mod cr;
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
mod scanner;
mod string_object;
mod strings;
mod symbol_object;
mod values;

pub use crate::agent::*;
pub use crate::arguments_object::*;
pub use crate::arrays::*;
pub use crate::bigint_object::*;
pub use crate::boolean_object::*;
pub use crate::chunk::*;
pub use crate::comparison::*;
pub use crate::compiler::*;
pub use crate::control_abstraction::*;
pub use crate::cr::*;
pub use crate::dtoa_r::*;
pub use crate::environment_record::*;
pub use crate::errors::*;
pub use crate::execution_context::*;
pub use crate::function_object::*;
pub use crate::map::*;
pub use crate::math::*;
pub use crate::number_object::*;
pub use crate::object::*;
pub use crate::object_object::*;
pub use crate::parser::*;
pub use crate::prettyprint::*;
pub use crate::proxy_object::*;
pub use crate::realm::*;
pub use crate::reference::*;
pub use crate::reflect::*;
pub use crate::scanner::*;
pub use crate::string_object::*;
pub use crate::strings::*;
pub use crate::symbol_object::*;
pub use crate::values::*;

use std::cell::RefCell;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::rc::Rc;

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
            initialize_host_defined_realm(0, false);
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
#[expect(hidden_glob_reexports)]
mod tests;
