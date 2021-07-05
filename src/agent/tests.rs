use super::*;
use crate::execution_context::{ScriptOrModule, ScriptRecord};
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
fn agent_running_execution_context() {
    let mut agent = Agent::new();

    let r1 = agent.running_execution_context();
    assert!(r1.is_none());

    agent.initialize_host_defined_realm();

    let r2 = agent.running_execution_context().unwrap();
    // the initial context has no "script_or_module" value...
    assert!(r2.script_or_module.is_none());

    // build a new EC, and add it to the EC stack
    let test_ec = ExecutionContext::new(None, r2.realm.clone(), Some(ScriptOrModule::Script(Rc::new(ScriptRecord {}))));
    agent.push_execution_context(test_ec);

    // Then get it back and check its script_or_module to ensure we got the new one.
    let r3 = agent.running_execution_context().unwrap();
    assert!(r3.script_or_module.is_some());
}
#[test]
fn agent_running_execution_context_mut() {
    let mut agent = Agent::new();

    let r1 = agent.running_execution_context_mut();
    assert!(r1.is_none());

    agent.initialize_host_defined_realm();

    let r2 = agent.running_execution_context_mut().unwrap();
    // the initial context has no "script_or_module" value...
    assert!(r2.script_or_module.is_none());

    // build a new EC, and add it to the EC stack
    let test_ec = ExecutionContext::new(None, r2.realm.clone(), Some(ScriptOrModule::Script(Rc::new(ScriptRecord {}))));
    agent.push_execution_context(test_ec);

    // Then get it back and check its script_or_module to ensure we got the new one.
    let r3 = agent.running_execution_context_mut().unwrap();
    assert!(r3.script_or_module.is_some());
}
#[test]
fn agent_pop_execution_context() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let r1 = agent.running_execution_context().unwrap();
    // build a new EC, and add it to the EC stack
    let test_ec = ExecutionContext::new(None, r1.realm.clone(), Some(ScriptOrModule::Script(Rc::new(ScriptRecord {}))));
    agent.push_execution_context(test_ec);
    // now pop it.
    agent.pop_execution_context();
    // And verify the one on top has no script_or_module value
    let r = agent.running_execution_context().unwrap();
    assert!(r.script_or_module.is_none());
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
