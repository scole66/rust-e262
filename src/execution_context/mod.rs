use super::agent::Agent;
use super::chunk::Chunk;
use super::cr::{AltCompletion, Completion};
use super::errors::create_type_error;
use super::object::Object;
use super::opcodes::Insn;
use super::parser::scripts::Script;
use super::realm::Realm;
use super::reference::SuperValue;
use super::values::ECMAScriptValue;
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
    pub lexical_environment: Option<Rc<dyn EnvironmentRecord>>,
    pub variable_environment: Option<Rc<dyn EnvironmentRecord>>,
    pub private_environment: Option<Rc<PrivateEnvironmentRecord>>,

    // code evaluation state
    pub stack: Vec<AltCompletion<SuperValue>>,
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

    pub fn prepare_for_execution(&mut self, chunk: Rc<Chunk>) {
        self.stack = vec![];
        self.chunk = Some(chunk);
        self.pc = 0;
    }

    pub fn suspend(&mut self) {}
    pub fn resume(&mut self) {}
    pub fn execute(&mut self, agent: &mut Agent) -> Completion {
        let result = ECMAScriptValue::Empty;
        let chunk = self.chunk.as_ref().ok_or_else(|| create_type_error(agent, "No compiled units!"))?;
        while self.pc < chunk.opcodes.len() {
            let icode = chunk.opcodes[self.pc as usize]; // in range due to while condition
            let instruction = Insn::try_from(icode).unwrap(); // failure is a coding error (the compiler broke)
            self.pc += 1;
            match instruction {
                Insn::String => {
                    let string_index = chunk.opcodes[self.pc as usize]; // failure is a coding error (the compiler broke)
                    let string = &chunk.strings[string_index as usize];
                    self.stack.push(Ok(string.into()));
                }
                Insn::Null => self.stack.push(Ok(ECMAScriptValue::Null.into())),
                Insn::True => self.stack.push(Ok(true.into())),
                Insn::False => self.stack.push(Ok(false.into())),
                Insn::This => self.stack.push(resolve_this_binding(agent).map(SuperValue::from)),
                Insn::Resolve => todo!(),
                Insn::StrictResolve => todo!(),
                Insn::Float => todo!(),
                Insn::Bigint => todo!(),
                Insn::GetValue => todo!(),
            }
        }
        Ok(result)
    }
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

/// Finds the Environment Record that currently supplies the binding of the keyword this.
///
/// See [GetThisEnvironment](https://tc39.es/ecma262/#sec-getthisenvironment) in ECMA-262.
pub fn get_this_environment(agent: &mut Agent) -> Rc<dyn EnvironmentRecord> {
    // The abstract operation GetThisEnvironment takes no arguments and returns an Environment Record. It finds the
    // Environment Record that currently supplies the binding of the keyword this. It performs the following steps when
    // called:
    //
    //  1. Let env be the running execution context's LexicalEnvironment.
    //  2. Repeat,
    //      a. Let exists be env.HasThisBinding().
    //      b. If exists is true, return env.
    //      c. Let outer be env.[[OuterEnv]].
    //      d. Assert: outer is not null.
    //      e. Set env to outer.
    // NOTE |   The loop in step 2 will always terminate because the list of environments always ends with the global
    //      |   environment which has a this binding.
    let ec = agent.running_execution_context().unwrap();
    let mut env = ec.lexical_environment.as_ref().unwrap().clone();
    loop {
        let exists = env.has_this_binding();
        if exists {
            return env;
        }
        let outer = env.get_outer_env().unwrap().clone();
        env = outer;
    }
}

/// Determine the binding of the "this" keyword (and return it)
///
/// See [ResolveThisBinding](https://tc39.es/ecma262/#sec-resolvethisbinding) in ECMA-262.
pub fn resolve_this_binding(agent: &mut Agent) -> Completion {
    // ResolveThisBinding ( )
    //
    // The abstract operation ResolveThisBinding takes no arguments and returns either a normal completion containing an
    // ECMAScript language value or an abrupt completion. It determines the binding of the keyword this using the
    // LexicalEnvironment of the running execution context. It performs the following steps when called:
    //
    // 1. Let envRec be GetThisEnvironment().
    // 2. Return ? envRec.GetThisBinding().
    let env_rec = get_this_environment(agent);
    env_rec.get_this_binding(agent)
}
