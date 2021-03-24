use super::realm::{create_realm, Realm};
use super::values::Symbol;
use std::default::Default;

// Agents
//
// An agent comprises a set of ECMAScript execution contexts, an execution context stack, a running execution context,
// an Agent Record, and an executing thread. Except for the executing thread, the constituents of an agent belong
// exclusively to that agent.
//
// An agent's executing thread executes a job on the agent's execution contexts independently of other agents, except
// that an executing thread may be used as the executing thread by multiple agents, provided none of the agents sharing
// the thread have an Agent Record whose [[CanBlock]] property is true.
//
// NOTE 1   | Some web browsers share a single executing thread across multiple unrelated tabs of a browser window, for
//          | example.
//
// While an agent's executing thread executes jobs, the agent is the surrounding agent for the code in those jobs. The
// code uses the surrounding agent to access the specification level execution objects held within the agent: the
// running execution context, the execution context stack, and the Agent Record's fields.

#[derive(Default)]
pub struct Agent {
    execution_context_stack: Vec<ExecutionContext>,
    pub symbols: WellKnownSymbols,
    pub obj_id: usize,
}

impl Agent {
    pub fn new() -> Self {
        Agent { ..Default::default() }
    }

    pub fn running_execution_context(&self) -> Option<&ExecutionContext> {
        let len = self.execution_context_stack.len();
        if len > 0 {
            Some(&self.execution_context_stack[len - 1])
        } else {
            None
        }
    }

    pub fn next_object_id(&mut self) -> usize {
        assert!(self.obj_id < usize::MAX);
        let result = self.obj_id;
        self.obj_id += 1;
        result
    }

    // InitializeHostDefinedRealm ( )
    //
    // The abstract operation InitializeHostDefinedRealm takes no arguments. It performs the following steps when
    // called:
    //
    //  1. Let realm be CreateRealm().
    //  2. Let newContext be a new execution context.
    //  3. Set the Function of newContext to null.
    //  4. Set the Realm of newContext to realm.
    //  5. Set the ScriptOrModule of newContext to null.
    //  6. Push newContext onto the execution context stack; newContext is now the running execution context.
    //  7. If the host requires use of an exotic object to serve as realm's global object, let global be such an object
    //     created in a host-defined manner. Otherwise, let global be undefined, indicating that an ordinary object
    //     should be created as the global object.
    //  8. If the host requires that the this binding in realm's global scope return an object other than the global
    //     object, let thisValue be such an object created in a host-defined manner. Otherwise, let thisValue be
    //     undefined, indicating that realm's global this binding should be the global object.
    //  9. Perform SetRealmGlobalObject(realm, global, thisValue).
    //  10. Let globalObj be ? SetDefaultGlobalBindings(realm).
    //  11. Create any host-defined global object properties on globalObj.
    //  12. Return NormalCompletion(empty).
    pub fn initialize_host_defined_realm(&mut self) {
        let realm = create_realm(self);
        let new_context = ExecutionContext { realm };
        self.execution_context_stack.push(new_context);
    }
}

pub struct ExecutionContext {
    pub realm: Realm,
}

pub struct WellKnownSymbols {
    pub to_primitive: Symbol,
}

impl Default for WellKnownSymbols {
    fn default() -> Self {
        WellKnownSymbols { to_primitive: Symbol::ToPrimitive }
    }
}
