use super::agent::Agent;
use super::chunk::Chunk;
use super::cr::{Completion, FullCompletion, NormalCompletion};
use super::environment_record::{get_identifier_reference, EnvironmentRecord, PrivateEnvironmentRecord};
use super::object::Object;
use super::parser::scripts::Script;
use super::realm::Realm;
use super::strings::JSString;
use super::values::ECMAScriptValue;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ScriptRecord {
    pub realm: Rc<RefCell<Realm>>,
    pub ecmascript_code: Rc<Script>,
    pub compiled: Rc<Chunk>,
}

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

    // for code contexts
    pub lexical_environment: Option<Rc<dyn EnvironmentRecord>>,
    pub variable_environment: Option<Rc<dyn EnvironmentRecord>>,
    pub private_environment: Option<Rc<PrivateEnvironmentRecord>>,

    // code evaluation state
    pub stack: Vec<FullCompletion>,
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
        ExecutionContext {
            realm,
            function,
            script_or_module,
            stack: vec![],
            chunk,
            pc: 0,
            lexical_environment: None,
            variable_environment: None,
            private_environment: None,
        }
    }
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

impl Agent {
    /// Finds the Environment Record that currently supplies the binding of the keyword this.
    ///
    /// See [GetThisEnvironment](https://tc39.es/ecma262/#sec-getthisenvironment) in ECMA-262.
    pub fn get_this_environment(&self) -> Rc<dyn EnvironmentRecord> {
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
        let mut env = self.current_lexical_environment().unwrap();
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
    pub fn resolve_this_binding(&mut self) -> Completion<ECMAScriptValue> {
        // ResolveThisBinding ( )
        //
        // The abstract operation ResolveThisBinding takes no arguments and returns either a normal completion containing an
        // ECMAScript language value or an abrupt completion. It determines the binding of the keyword this using the
        // LexicalEnvironment of the running execution context. It performs the following steps when called:
        //
        // 1. Let envRec be GetThisEnvironment().
        // 2. Return ? envRec.GetThisBinding().
        let env_rec = self.get_this_environment();
        env_rec.get_this_binding(self)
    }

    /// Constructs a Reference for the given name (and, potentially, environment)
    ///
    /// See [ResolveBinding](https://tc39.es/ecma262/#sec-resolvebinding) in ECMA-262.
    pub fn resolve_binding(
        &mut self,
        name: &JSString,
        env: Option<Rc<dyn EnvironmentRecord>>,
        strict: bool,
    ) -> FullCompletion {
        // ResolveBinding ( name [ , env ] )
        // The abstract operation ResolveBinding takes argument name (a String) and optional argument env (an
        // Environment Record or undefined) and returns either a normal completion containing a Reference Record or an
        // abrupt completion. It is used to determine the binding of name. env can be used to explicitly provide the
        // Environment Record that is to be searched for the binding. It performs the following steps when called:
        //
        //  1. If env is not present or if env is undefined, then
        //      a. Set env to the running execution context's LexicalEnvironment.
        //  2. Assert: env is an Environment Record.
        //  3. If the source text matched by the syntactic production that is being evaluated is contained in strict
        //     mode code, let strict be true; else let strict be false.
        //  4. Return ? GetIdentifierReference(env, name, strict).
        // NOTE |   The result of ResolveBinding is always a Reference Record whose [[ReferencedName]] field is name.
        let env = match env {
            Some(e) => Some(e),
            None => self.current_lexical_environment(),
        };
        get_identifier_reference(self, env, name.clone(), strict).map(NormalCompletion::from)
    }
}

#[cfg(test)]
mod tests;
