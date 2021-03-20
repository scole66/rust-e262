use crate::values::Symbol;
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
}

pub struct ExecutionContext {}

pub struct WellKnownSymbols {
    pub to_primitive: Symbol,
}

impl Default for WellKnownSymbols {
    fn default() -> Self {
        WellKnownSymbols { to_primitive: Symbol::ToPrimitive }
    }
}
