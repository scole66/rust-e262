use super::*;
use crate::parser::testhelp::Maker;
use crate::tests::test_agent;
use crate::tests::{create_named_realm, get_realm_name};
use ahash::AHashSet;

#[test]
fn agent_new() {
    let agent = Agent::new();

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
fn agent_pop_execution_context() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm(true);
    let realm_ref = agent.current_realm_record().unwrap();
    // build a new EC, and add it to the EC stack
    let sr = ScriptRecord { realm: realm_ref.clone(), ecmascript_code: Maker::new("").script(), compiled: Rc::new(Chunk::new("test")) };
    let test_ec = ExecutionContext::new(None, realm_ref, Some(ScriptOrModule::Script(Rc::new(sr))));
    agent.push_execution_context(test_ec);
    // now pop it.
    agent.pop_execution_context();
    // And verify the one on top has no script_or_module value
    let r = &agent.execution_context_stack[agent.execution_context_stack.len() - 1];
    assert!(r.script_or_module.is_none());
}
#[test]
fn agent_active_function_object() {
    let mut agent = Agent::new();
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
fn agent_next_object_id() {
    let mut agent = Agent::new();
    // Starts at something, and then increases monotonically.
    let first = agent.next_object_id();
    for x in 1..10 {
        assert_eq!(agent.next_object_id(), x + first);
    }
}
#[test]
fn agent_next_symbol_id() {
    let mut agent = Agent::new();
    // Starts at something, and then increases monotonically.
    let first = agent.next_symbol_id();
    for x in 1..10 {
        assert_eq!(agent.next_symbol_id(), x + first);
    }
}
#[test]
fn agent_debug() {
    assert_ne!(format!("{:?}", Agent::new()), "");
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
        let a = Agent::new();
        let realm = a.current_realm_record();

        assert!(realm.is_none());
    }
    #[test]
    fn stacked() {
        let mut a = Agent::new();
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
