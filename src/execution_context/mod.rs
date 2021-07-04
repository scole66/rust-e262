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

pub fn get_global_object(_agent: &mut Agent) -> Object {
    todo!()
}
