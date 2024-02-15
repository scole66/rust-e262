#![allow(dead_code)]
#![allow(clippy::bool_assert_comparison)]
#![allow(clippy::enum_variant_names)]
#![allow(clippy::similar_names)]
#![allow(clippy::wildcard_imports)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::single_match_else)]
#![allow(clippy::float_cmp)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::doc_markdown)]
// pedantic denies. (When we turn on pedantic, these denys can be removed)
#![deny(clippy::uninlined_format_args)]
#![deny(clippy::unnested_or_patterns)]
#![deny(clippy::unreadable_literal)]
#![deny(clippy::ignored_unit_patterns)]
#![deny(clippy::unnecessary_wraps)]
#![deny(clippy::semicolon_if_nothing_returned)]
#![deny(clippy::redundant_else)]
#![deny(clippy::match_same_arms)]
#![deny(clippy::manual_assert)]
#![deny(clippy::cast_possible_truncation)]
#![deny(clippy::cast_precision_loss)]
#![deny(clippy::needless_pass_by_value)]
#![deny(clippy::explicit_iter_loop)]
#![deny(clippy::map_unwrap_or)]
#![deny(clippy::redundant_closure_for_method_calls)]
#![deny(clippy::if_not_else)]
#![deny(clippy::missing_fields_in_debug)]
#![deny(clippy::from_iter_instead_of_collect)]
#![deny(clippy::struct_excessive_bools)]
#![deny(clippy::cast_lossless)]
#![deny(clippy::default_trait_access)]
#![deny(clippy::cast_sign_loss)]
#![deny(clippy::manual_string_new)]
#![deny(clippy::range_plus_one)]
#![deny(clippy::return_self_not_must_use)]
#![deny(clippy::borrow_as_ptr)]
#![deny(clippy::ptr_as_ptr)]
#![deny(clippy::match_bool)]
#![deny(clippy::cloned_instead_of_copied)]
#![deny(clippy::items_after_statements)]
#![deny(clippy::inconsistent_struct_constructor)]
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
                println!("{}", to_string(err).unwrap());
            }
            Err("See above".to_string())
        }
        ParsedText::Script(node) => {
            node.pprint_concise(&mut io::stdout()).expect("Output Error");
            Ok(0)
        }
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
#[allow(hidden_glob_reexports)]
mod tests;
