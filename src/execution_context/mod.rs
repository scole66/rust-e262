use super::agent::Agent;
use super::object::Object;
use super::realm::Realm;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct ScriptRecord {}
#[derive(Debug)]
pub struct ModuleRecord {}

#[derive(Debug)]
pub enum ScriptOrModule {
    Script(Rc<ScriptRecord>),
    Module(Rc<ModuleRecord>),
}

#[derive(Debug)]
pub struct ExecutionContext {
    pub realm: Rc<RefCell<Realm>>,
    pub function: Option<Object>,
    pub script_or_module: Option<ScriptOrModule>,
}

impl ExecutionContext {
    pub fn new(function: Option<Object>, realm: Rc<RefCell<Realm>>, script_or_module: Option<ScriptOrModule>) -> Self {
        ExecutionContext { realm, function, script_or_module }
    }

    pub fn suspend(&mut self) {}
    pub fn resume(&mut self) {}
}

// GetGlobalObject ( )
//
// The abstract operation GetGlobalObject takes no arguments. It returns the global object used by the currently
// running execution context. It performs the following steps when called:
//
//  1. Let currentRealm be the current Realm Record.
//  2. Return currentRealm.[[GlobalObject]].
pub fn get_global_object(agent: &Agent) -> Option<Object> {
    agent.current_realm_record()?.borrow().global_object.clone()
}
