use super::*;
use crate::parser::testhelp::*;
use crate::tests::*;
use ahash::AHashSet;
use lazy_static::lazy_static;
use regex::Regex;
use std::cell::RefCell;
use std::rc::Rc;

mod agent {
    use super::*;

    #[test]
    fn new() {
        let agent = Agent::new(Rc::new(RefCell::new(SymbolRegistry::new())));

        // New agent; no realm initialized.
        assert!(agent.execution_context_stack.is_empty());

        // All well-known symbols initialized, and different from one another.
        let symbols = vec![
            agent.wks(WksId::AsyncIterator),
            agent.wks(WksId::HasInstance),
            agent.wks(WksId::IsConcatSpreadable),
            agent.wks(WksId::Iterator),
            agent.wks(WksId::Match),
            agent.wks(WksId::MatchAll),
            agent.wks(WksId::Replace),
            agent.wks(WksId::Search),
            agent.wks(WksId::Species),
            agent.wks(WksId::Split),
            agent.wks(WksId::ToPrimitive),
            agent.wks(WksId::ToStringTag),
            agent.wks(WksId::Unscopables),
        ];
        let num_symbols = symbols.len();
        let mut symbol_set = AHashSet::new();
        for sym in symbols.iter() {
            symbol_set.insert(sym);
        }
        assert_eq!(num_symbols, symbol_set.len());

        // ID trackers at reasonable spots
        assert_eq!(agent.obj_id, 1);
        assert_eq!(agent.symbol_id, num_symbols + 1);
    }

    #[test]
    fn pop_execution_context() {
        let mut agent = test_agent();
        let realm_ref = agent.current_realm_record().unwrap();
        // build a new EC, and add it to the EC stack
        let sr = ScriptRecord {
            realm: realm_ref.clone(),
            ecmascript_code: Maker::new("").script(),
            compiled: Rc::new(Chunk::new("test")),
        };
        let test_ec = ExecutionContext::new(None, realm_ref, Some(ScriptOrModule::Script(Rc::new(sr))));
        agent.push_execution_context(test_ec);
        // now pop it.
        agent.pop_execution_context();
        // And verify the one on top has no script_or_module value
        let r = &agent.execution_context_stack[agent.execution_context_stack.len() - 1];
        assert!(r.script_or_module.is_none());
    }
    #[test]
    fn active_function_object() {
        let mut agent = Agent::new(Rc::new(RefCell::new(SymbolRegistry::new())));
        // no Running Execution Context, so this should be None.
        let afo = agent.active_function_object();
        assert!(afo.is_none());

        agent.initialize_host_defined_realm(true);
        // Now there's an execution context, but still no active function, so this should still be None.
        let afo = agent.active_function_object();
        assert!(afo.is_none());

        // Create a new EC that _does_ have a function object; push it, and then check the active function.
        let fo = agent.intrinsic(IntrinsicId::ThrowTypeError);
        let realm = agent.current_realm_record().unwrap();
        let function_ec = ExecutionContext::new(Some(fo.clone()), realm, None);
        agent.push_execution_context(function_ec);

        let afo = agent.active_function_object().unwrap();
        assert_eq!(afo, fo);
    }
    #[test]
    fn next_object_id() {
        let mut agent = Agent::new(Rc::new(RefCell::new(SymbolRegistry::new())));
        // Starts at something, and then increases monotonically.
        let first = agent.next_object_id();
        for x in 1..10 {
            assert_eq!(agent.next_object_id(), x + first);
        }
    }
    #[test]
    fn next_symbol_id() {
        let mut agent = Agent::new(Rc::new(RefCell::new(SymbolRegistry::new())));
        // Starts at something, and then increases monotonically.
        let first = agent.next_symbol_id();
        for x in 1..10 {
            assert_eq!(agent.next_symbol_id(), x + first);
        }
    }
    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", Agent::new(Rc::new(RefCell::new(SymbolRegistry::new())))), "");
    }
}

#[test]
fn wksid_debug() {
    assert_ne!(format!("{:?}", WksId::ToStringTag), "");
}
#[test]
fn wksid_eq() {
    let w1 = WksId::Match;
    let w2 = WksId::Search;
    let w3 = WksId::Match;

    assert_eq!(w1 == w2, false);
    assert_eq!(w1 == w3, true);
    assert_eq!(w2 == w3, false);
}
#[test]
#[allow(clippy::clone_on_copy)]
fn wksid_clone() {
    let w1 = WksId::ToPrimitive;
    let w2 = w1.clone();

    assert_eq!(w1, w2);
}

#[test]
fn wks_descriptions() {
    let agent = test_agent();
    let symbols = vec![
        WksId::AsyncIterator,
        WksId::HasInstance,
        WksId::IsConcatSpreadable,
        WksId::Iterator,
        WksId::Match,
        WksId::MatchAll,
        WksId::Replace,
        WksId::Search,
        WksId::Species,
        WksId::Split,
        WksId::ToPrimitive,
        WksId::ToStringTag,
        WksId::Unscopables,
    ];
    let descriptions = vec![
        "Symbol.asyncIterator",
        "Symbol.hasInstance",
        "Symbol.isConcatSpreadable",
        "Symbol.iterator",
        "Symbol.match",
        "Symbol.matchAll",
        "Symbol.replace",
        "Symbol.search",
        "Symbol.species",
        "Symbol.split",
        "Symbol.toPrimitive",
        "Symbol.toStringTag",
        "Symbol.unscopables",
    ];
    for (id, expected) in symbols.iter().zip(descriptions) {
        let desc = agent.wks(*id).description().unwrap();
        assert_eq!(desc, JSString::from(expected));
    }
}

mod current_realm_record {
    use super::*;

    #[test]
    fn empty() {
        let a = Agent::new(Rc::new(RefCell::new(SymbolRegistry::new())));
        let realm = a.current_realm_record();

        assert!(realm.is_none());
    }
    #[test]
    fn stacked() {
        let mut a = Agent::new(Rc::new(RefCell::new(SymbolRegistry::new())));
        let first_realm = create_named_realm(&mut a, "first");
        let first_context = ExecutionContext::new(None, first_realm, None);
        a.push_execution_context(first_context);

        let second_realm = create_named_realm(&mut a, "second");
        let second_context = ExecutionContext::new(None, second_realm, None);
        a.push_execution_context(second_context);

        let current = a.current_realm_record().unwrap();
        assert_eq!(get_realm_name(&mut a, &*current.borrow()), "second");

        a.pop_execution_context();

        let current = a.current_realm_record().unwrap();
        assert_eq!(get_realm_name(&mut a, &*current.borrow()), "first");
    }
}

mod well_known_symbols {
    use super::*;

    #[test]
    fn debug() {
        let agent = test_agent();
        let s = format!("{:?}", agent.symbols);
        assert_ne!(s, "");
    }
}

mod parse_script {
    use super::*;
    use test_case::test_case;

    #[test]
    fn happy() {
        let mut agent = test_agent();
        let src = "'hello world';";
        let starting_realm = agent.current_realm_record().unwrap();
        let ScriptRecord { realm, ecmascript_code, compiled } =
            super::parse_script(&mut agent, src, starting_realm.clone()).unwrap();
        assert!(Rc::ptr_eq(&realm, &starting_realm));
        assert_eq!(format!("{}", ecmascript_code), "'hello world' ;");
        assert_eq!(compiled.name, "top level script");
        assert_eq!(
            compiled.disassemble().into_iter().filter_map(disasm_filt).collect::<Vec<_>>(),
            svec(&["STRING 0 (hello world)",])
        );
    }

    #[test_case("for [i=0, i<10, i++] {}" => set(&["1:5: ‘(’ expected"]); "parse time syntax")]
    #[test_case("break lbl;" => set(&["undefined break target detected"]); "early error syntax")]
    fn parse_error(src: &str) -> AHashSet<String> {
        let mut agent = test_agent();
        let starting_realm = agent.current_realm_record().unwrap();
        let errs = super::parse_script(&mut agent, src, starting_realm.clone()).unwrap_err();
        AHashSet::from_iter(errs.iter().map(|err| unwind_syntax_error_object(&mut agent, err.clone())))
    }
}

mod process_error {
    use super::*;
    use std::io::Write;
    use test_case::test_case;

    #[test]
    fn debug() {
        let s = format!("{:?}", ProcessError::InternalError { reason: "random reason".into() });
        assert_ne!(s, "");
    }

    fn internal_err(_: &mut Agent) -> ProcessError {
        ProcessError::InternalError { reason: "blue".into() }
    }

    fn runtime_err_obj(agent: &mut Agent) -> ProcessError {
        let err = create_type_error_object(agent, "test sentinel");
        ProcessError::RuntimeError { error: err.into() }
    }
    fn runtime_err_value(_: &mut Agent) -> ProcessError {
        let error = "test sentinel".into();
        ProcessError::RuntimeError { error }
    }
    fn runtime_err_non_err_obj(agent: &mut Agent) -> ProcessError {
        let error = ordinary_object_create(agent, None, &[]).into();
        ProcessError::RuntimeError { error }
    }
    fn matches_object(s: String) {
        lazy_static! {
            static ref MATCH: Regex = Regex::new("^Thrown: <Object [0-9]+>$").expect("Valid regex");
        }
        assert!(MATCH.is_match(&s));
    }
    fn compiler_objs(agent: &mut Agent) -> ProcessError {
        ProcessError::CompileErrors {
            values: vec![
                create_syntax_error_object(agent, "Trouble in Paradise", None),
                create_reference_error_object(agent, "yeah, compiler errs are only syntax..."),
            ],
        }
    }
    #[test_case(internal_err => "blue"; "internal error")]
    #[test_case(runtime_err_obj => "Thrown: TypeError: test sentinel"; "error obj runtime")]
    #[test_case(runtime_err_value => "Thrown: test sentinel"; "error value runtime")]
    #[test_case(runtime_err_non_err_obj => using matches_object; "error obj but not error")]
    #[test_case(compiler_objs => "During compilation:\nSyntaxError: Trouble in Paradise\nReferenceError: yeah, compiler errs are only syntax...\n"; "compiler err list")]
    fn display(make_error: fn(&mut Agent) -> ProcessError) -> String {
        let mut agent = test_agent();
        let err = make_error(&mut agent);
        format!("{err}")
    }

    #[test]
    fn display_err() {
        let mut agent = test_agent();
        let err = compiler_objs(&mut agent);
        let mut target = 1;
        loop {
            let mut writer = MockWriter::new(Vec::new(), target);
            let result = write!(&mut writer, "{err}");
            assert!(result.is_err() || !writer.error_generated);
            if !writer.error_generated {
                break;
            }
            target += 1;
        }
    }
}

mod process_ecmascript {
    use super::*;
    use test_case::test_case;

    #[test_case("1;" => Ok(ECMAScriptValue::from(1)); "normal result")]
    #[test_case("void" => serr("During compilation:\nSyntaxError: 1:5: UnaryExpression expected\n"); "syntax error")]
    #[test_case("a;" => serr("Thrown: ReferenceError: Unresolvable Reference"); "runtime error")]
    fn process_ecmascript(src: &str) -> Result<ECMAScriptValue, String> {
        let mut agent = test_agent();
        let result = super::process_ecmascript(&mut agent, src);
        result.map_err(|e| format!("{e}"))
    }
}
