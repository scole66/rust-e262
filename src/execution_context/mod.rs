use super::agent::Agent;
use super::chunk::Chunk;
use super::object::Object;
use super::parser::scripts::Script;
use super::realm::Realm;
use super::reference::SuperValue;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ScriptRecord {
    pub realm: Rc<RefCell<Realm>>,
    pub ecmascript_code: Rc<Script>,
    pub compiled: Rc<Chunk>,
}

impl ScriptRecord {
    #[cfg(test)]
    pub fn new_empty(realm: Rc<RefCell<Realm>>) -> Self {
        ScriptRecord { realm, ecmascript_code: Rc::new(Script(None)), compiled: Rc::new(Chunk::new("empty")) }
    }
}

#[derive(Debug)]
pub struct ModuleRecord {}

#[derive(Debug)]
pub enum ScriptOrModule {
    Script(Rc<ScriptRecord>),
    Module(Rc<ModuleRecord>),
}

use super::environment_record::{EnvironmentRecord, PrivateEnvironmentRecord};

#[derive(Debug)]
pub struct ExecutionContext {
    pub realm: Rc<RefCell<Realm>>,
    pub function: Option<Object>,
    pub script_or_module: Option<ScriptOrModule>,

    // for code contexts
    pub lexical_environment: Option<Rc<RefCell<dyn EnvironmentRecord>>>,
    pub variable_environment: Option<Rc<RefCell<dyn EnvironmentRecord>>>,
    pub private_environment: Option<Rc<RefCell<PrivateEnvironmentRecord>>>,

    // code evaluation state
    pub stack: Vec<SuperValue>,
    pub chunk: Option<Rc<Chunk>>, // This might change. It might be easier to have an empty chunk than a None.
    pub pc: usize,
}

impl ExecutionContext {
    #[allow(unused_variables)]
    pub fn new(function: Option<Object>, realm: Rc<RefCell<Realm>>, script_or_module: Option<ScriptOrModule>) -> Self {
        let chunk = match &script_or_module {
            None => None,
            Some(ScriptOrModule::Script(sr)) => Some(Rc::clone(&sr.compiled)),
            Some(ScriptOrModule::Module(mr)) => todo!(),
        };
        ExecutionContext { realm, function, script_or_module, stack: vec![], chunk, pc: 0, lexical_environment: None, variable_environment: None, private_environment: None }
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
pub fn get_global_object(agent: &mut Agent) -> Option<Object> {
    let ec = agent.running_execution_context();
    ec.and_then(|ec| {
        let g = &ec.realm.borrow().global_object;
        g.as_ref().cloned()
    })
}
