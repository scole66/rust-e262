use super::*;
use anyhow::anyhow;
use itertools::Itertools;
use num::pow::Pow;
use num::{BigInt, BigUint, ToPrimitive, Zero};
use std::cell::{Cell, RefCell};
use std::convert::TryFrom;
use std::convert::TryInto;
use std::error;
use std::fmt;
use std::rc::Rc;

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

#[derive(Debug)]
pub struct Agent {
    execution_context_stack: RefCell<Vec<ExecutionContext>>,
    symbols: WellKnownSymbols,
    obj_id: Cell<usize>,
    symbol_id: Cell<usize>,
    gsr: RefCell<Option<Rc<RefCell<SymbolRegistry>>>>,
}

thread_local! {
    pub static AGENT: Agent = Agent::new();
}

impl Default for Agent {
    fn default() -> Self {
        Agent::new()
    }
}

impl Agent {
    pub fn new() -> Self {
        Agent {
            obj_id: Cell::new(1),
            execution_context_stack: RefCell::new(vec![]),
            symbols: WellKnownSymbols {
                async_iterator_: Symbol(Rc::new(SymbolInternals {
                    id: 1,
                    description: Some(JSString::from("Symbol.asyncIterator")),
                })),
                has_instance_: Symbol(Rc::new(SymbolInternals {
                    id: 2,
                    description: Some(JSString::from("Symbol.hasInstance")),
                })),
                is_concat_spreadable_: Symbol(Rc::new(SymbolInternals {
                    id: 3,
                    description: Some(JSString::from("Symbol.isConcatSpreadable")),
                })),
                iterator_: Symbol(Rc::new(SymbolInternals {
                    id: 4,
                    description: Some(JSString::from("Symbol.iterator")),
                })),
                match_: Symbol(Rc::new(SymbolInternals { id: 5, description: Some(JSString::from("Symbol.match")) })),
                match_all_: Symbol(Rc::new(SymbolInternals {
                    id: 6,
                    description: Some(JSString::from("Symbol.matchAll")),
                })),
                replace_: Symbol(Rc::new(SymbolInternals {
                    id: 7,
                    description: Some(JSString::from("Symbol.replace")),
                })),
                search_: Symbol(Rc::new(SymbolInternals { id: 8, description: Some(JSString::from("Symbol.search")) })),
                species_: Symbol(Rc::new(SymbolInternals {
                    id: 9,
                    description: Some(JSString::from("Symbol.species")),
                })),
                split_: Symbol(Rc::new(SymbolInternals { id: 10, description: Some(JSString::from("Symbol.split")) })),
                to_primitive_: Symbol(Rc::new(SymbolInternals {
                    id: 11,
                    description: Some(JSString::from("Symbol.toPrimitive")),
                })),
                to_string_tag_: Symbol(Rc::new(SymbolInternals {
                    id: 12,
                    description: Some(JSString::from("Symbol.toStringTag")),
                })),
                unscopables_: Symbol(Rc::new(SymbolInternals {
                    id: 13,
                    description: Some(JSString::from("Symbol.unscopables")),
                })),
            },
            symbol_id: Cell::new(14),
            gsr: RefCell::new(None),
        }
    }

    pub fn reset(&self) {
        self.obj_id.set(1);
        self.execution_context_stack.borrow_mut().clear();
        self.symbol_id.set(14);
        self.gsr.borrow_mut().take();
    }
}

pub fn active_function_object() -> Option<Object> {
    AGENT.with(|agent| {
        let stack = agent.execution_context_stack.borrow();
        match stack.len() {
            0 => None,
            n => stack[n - 1].function.clone(),
        }
    })
}

/// Return the active script or module record associated with the current execution
///
/// See [GetActiveScriptOrModule](https://tc39.es/ecma262/#sec-getactivescriptormodule) from ECMA-262.
pub fn get_active_script_or_module() -> Option<ScriptOrModule> {
    // GetActiveScriptOrModule ( )
    //
    // The abstract operation GetActiveScriptOrModule takes no arguments and returns a Script Record, a Module
    // Record, or null. It is used to determine the running script or module, based on the running execution
    // context. It performs the following steps when called:
    //
    //  1. If the execution context stack is empty, return null.
    //  2. Let ec be the topmost execution context on the execution context stack whose ScriptOrModule component is not null.
    //  3. If no such execution context exists, return null. Otherwise, return ec's ScriptOrModule.
    AGENT.with(|agent| {
        let stack = agent.execution_context_stack.borrow();
        for ec in stack.iter() {
            if let Some(script_or_module) = &ec.script_or_module {
                return Some(script_or_module.clone());
            }
        }
        None
    })
}

pub fn next_object_id() -> usize {
    AGENT.with(|agent| {
        // Note: single threaded, so no worries about read-then-write trouble.
        let result = agent.obj_id.get();
        assert!(result < usize::MAX);
        agent.obj_id.set(result + 1);
        result
    })
}

pub fn next_symbol_id() -> usize {
    AGENT.with(|agent| {
        let result = agent.symbol_id.get();
        assert!(result < usize::MAX);
        agent.symbol_id.set(result + 1);
        result
    })
}

pub fn push_execution_context(context: ExecutionContext) {
    AGENT.with(|agent| agent.execution_context_stack.borrow_mut().push(context));
}

pub fn pop_execution_context() {
    AGENT.with(|agent| agent.execution_context_stack.borrow_mut().pop());
}

pub fn ec_push(val: FullCompletion) {
    AGENT.with(|agent| {
        let mut ec_stack = agent.execution_context_stack.borrow_mut();
        let len = ec_stack.len();
        assert!(len > 0, "EC Push called with no active EC");
        let ec = &mut ec_stack[len - 1];
        ec.stack.push(val);
    })
}

pub fn ec_pop() -> Option<FullCompletion> {
    AGENT.with(|agent| {
        let mut execution_context_stack = agent.execution_context_stack.borrow_mut();
        let len = execution_context_stack.len();
        match len {
            0 => None,
            _ => {
                let ec = &mut execution_context_stack[len - 1];
                ec.stack.pop()
            }
        }
    })
}

pub fn ec_stack_len() -> usize {
    AGENT.with(|agent| {
        let execution_context_stack = agent.execution_context_stack.borrow();
        let len = execution_context_stack.len();
        match len {
            0 => 0,
            _ => {
                let ec = &execution_context_stack[len - 1];
                ec.stack.len()
            }
        }
    })
}

pub fn wks(sym_id: WksId) -> Symbol {
    AGENT.with(|agent| {
        match sym_id {
            WksId::AsyncIterator => &agent.symbols.async_iterator_,
            WksId::HasInstance => &agent.symbols.has_instance_,
            WksId::IsConcatSpreadable => &agent.symbols.is_concat_spreadable_,
            WksId::Iterator => &agent.symbols.iterator_,
            WksId::Match => &agent.symbols.match_,
            WksId::MatchAll => &agent.symbols.match_all_,
            WksId::Replace => &agent.symbols.replace_,
            WksId::Search => &agent.symbols.search_,
            WksId::Species => &agent.symbols.species_,
            WksId::Split => &agent.symbols.split_,
            WksId::ToPrimitive => &agent.symbols.to_primitive_,
            WksId::ToStringTag => &agent.symbols.to_string_tag_,
            WksId::Unscopables => &agent.symbols.unscopables_,
        }
        .clone()
    })
}

pub fn intrinsic(id: IntrinsicId) -> Object {
    let realm_ref = current_realm_record().unwrap();
    let realm = realm_ref.borrow();
    realm.intrinsics.get(id)
}

pub fn current_realm_record() -> Option<Rc<RefCell<Realm>>> {
    AGENT.with(|agent| {
        let execution_context_stack = agent.execution_context_stack.borrow();
        match execution_context_stack.len() {
            0 => None,
            n => Some(execution_context_stack[n - 1].realm.clone()),
        }
    })
}

pub fn current_lexical_environment() -> Option<Rc<dyn EnvironmentRecord>> {
    AGENT.with(|agent| {
        let execution_context_stack = agent.execution_context_stack.borrow();
        match execution_context_stack.len() {
            0 => None,
            n => execution_context_stack[n - 1].lexical_environment.clone(),
        }
    })
}

pub fn current_variable_environment() -> Option<Rc<dyn EnvironmentRecord>> {
    AGENT.with(|agent| {
        let execution_context_stack = agent.execution_context_stack.borrow();
        match execution_context_stack.len() {
            0 => None,
            n => execution_context_stack[n - 1].variable_environment.clone(),
        }
    })
}

pub fn current_private_environment() -> Option<Rc<RefCell<PrivateEnvironmentRecord>>> {
    AGENT.with(|agent| {
        let execution_context_stack = agent.execution_context_stack.borrow();
        match execution_context_stack.len() {
            0 => None,
            n => execution_context_stack[n - 1].private_environment.clone(),
        }
    })
}

pub fn set_lexical_environment(env: Option<Rc<dyn EnvironmentRecord>>) {
    AGENT.with(|agent| {
        let mut execution_context_stack = agent.execution_context_stack.borrow_mut();
        match execution_context_stack.len() {
            0 => (),
            n => {
                execution_context_stack[n - 1].lexical_environment = env;
            }
        }
    })
}

pub fn set_variable_environment(env: Option<Rc<dyn EnvironmentRecord>>) {
    AGENT.with(|agent| {
        let mut execution_context_stack = agent.execution_context_stack.borrow_mut();
        match execution_context_stack.len() {
            0 => (),
            n => {
                execution_context_stack[n - 1].variable_environment = env;
            }
        }
    })
}

// SetRealmGlobalObject ( realmRec, globalObj, thisValue )
//
// The abstract operation SetRealmGlobalObject takes arguments realmRec, globalObj (an Object or undefined), and
// thisValue. It performs the following steps when called:
//
//  1. If globalObj is undefined, then
//      a. Let intrinsics be realmRec.[[Intrinsics]].
//      b. Set globalObj to ! OrdinaryObjectCreate(intrinsics.[[%Object.prototype%]]).
//  2. Assert: Type(globalObj) is Object.
//  3. If thisValue is undefined, set thisValue to globalObj.
//  4. Set realmRec.[[GlobalObject]] to globalObj.
//  5. Let newGlobalEnv be NewGlobalEnvironment(globalObj, thisValue).
//  6. Set realmRec.[[GlobalEnv]] to newGlobalEnv.
//  7. Return realmRec.
pub fn set_realm_global_object(global_obj: Option<Object>, this_value: Option<Object>) {
    let go = global_obj.unwrap_or_else(|| {
        let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
        ordinary_object_create(Some(object_proto), &[])
    });
    let tv = this_value.unwrap_or_else(|| go.clone());
    let realm_ref = current_realm_record().unwrap();
    let mut realm = realm_ref.borrow_mut();

    realm.global_object = Some(go.clone());
    let new_global_env = GlobalEnvironmentRecord::new(go, tv, "realm-global");
    realm.global_env = Some(Rc::new(new_global_env));
}

// SetDefaultGlobalBindings ( realmRec )
//
// The abstract operation SetDefaultGlobalBindings takes argument realmRec. It performs the following steps when
// called:
//
//  1. Let global be realmRec.[[GlobalObject]].
//  2. For each property of the Global Object specified in clause 19, do
//      a. Let name be the String value of the property name.
//      b. Let desc be the fully populated data Property Descriptor for the property, containing the specified
//         attributes for the property. For properties listed in 19.2, 19.3, or 19.4 the value of the [[Value]]
//         attribute is the corresponding intrinsic object from realmRec.
//      c. Perform ? DefinePropertyOrThrow(global, name, desc).
//  3. Return global.
pub fn set_default_global_bindings() {
    let global = get_global_object().unwrap();

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    /////////       Value Properties of the Global Object
    //////////////////////////////////////////////////////////////////////////////////////////////////////
    macro_rules! global_data {
        ( $name:expr, $value:expr, $writable:expr, $enumerable:expr, $configurable:expr ) => {
            define_property_or_throw(
                &global,
                $name,
                PotentialPropertyDescriptor::new()
                    .value(ECMAScriptValue::from($value))
                    .writable($writable)
                    .enumerable($enumerable)
                    .configurable($configurable),
            )
            .unwrap();
        };
    }
    // globalThis
    //
    // The initial value of the "globalThis" property of the global object in a Realm Record realm is
    // realm.[[GlobalEnv]].[[GlobalThisValue]].
    //
    // This property has the attributes { [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true }.
    let gtv = {
        let rc_realm = current_realm_record().unwrap();
        let realm_ref = rc_realm.borrow();
        realm_ref.global_env.as_ref().unwrap().get_this_binding().unwrap()
    };
    global_data!("globalThis", gtv, true, false, true);

    // Infinity
    //
    // The value of Infinity is +∞𝔽 (see 6.1.6.1). This property has the attributes { [[Writable]]: false,
    // [[Enumerable]]: false, [[Configurable]]: false }.
    global_data!("Infinity", f64::INFINITY, false, false, false);

    // NaN
    //
    // The value of NaN is NaN (see 6.1.6.1). This property has the attributes { [[Writable]]: false,
    // [[Enumerable]]: false, [[Configurable]]: false }.
    global_data!("NaN", f64::NAN, false, false, false);

    // undefined
    //
    // The value of undefined is undefined (see 6.1.1). This property has the attributes { [[Writable]]: false,
    // [[Enumerable]]: false, [[Configurable]]: false }.
    global_data!("undefined", ECMAScriptValue::Undefined, false, false, false);

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    /////////       Function Properties of the Global Object
    //////////////////////////////////////////////////////////////////////////////////////////////////////
    // eval ( x )
    // isFinite ( number )
    // isNaN ( number )
    // parseFloat ( string )
    // parseInt ( string, radix )
    // decodeURI ( encodedURI )
    // decodeURIComponent ( encodedURIComponent )
    // encodeURI ( uri )
    // encodeURIComponent ( uriComponent )

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    /////////       Constructor Properties of the Global Object
    //////////////////////////////////////////////////////////////////////////////////////////////////////
    macro_rules! constructor_property {
        ( $name:ident ) => {
            global_data!(stringify!($name), intrinsic(IntrinsicId::$name), true, false, true);
        };
    }
    // AggregateError ( . . . )
    // Array ( . . . )
    // ArrayBuffer ( . . . )
    // BigInt ( . . . )
    // BigInt64Array ( . . . )
    // BigUint64Array ( . . . )
    // Boolean ( . . . )
    constructor_property!(Boolean);
    // DataView ( . . . )
    // Date ( . . . )
    // Error ( . . . )
    constructor_property!(Error);
    // EvalError ( . . . )
    constructor_property!(EvalError);
    // FinalizationRegistry ( . . . )
    // Float32Array ( . . . )
    // Float64Array ( . . . )
    // Function ( . . . )
    // Int8Array ( . . . )
    // Int16Array ( . . . )
    // Int32Array ( . . . )
    // Map ( . . . )
    // Number ( . . . )
    constructor_property!(Number);
    // Object ( . . . )
    //constructor_property!(Object);
    // Promise ( . . . )
    // Proxy ( . . . )
    // RangeError ( . . . )
    constructor_property!(RangeError);
    // ReferenceError ( . . . )
    constructor_property!(ReferenceError);
    // RegExp ( . . . )
    // Set ( . . . )
    // SharedArrayBuffer ( . . . )
    // String ( . . . )
    constructor_property!(String);
    // Symbol ( . . . )
    constructor_property!(Symbol);
    // SyntaxError ( . . . )
    constructor_property!(SyntaxError);
    // TypeError ( . . . )
    constructor_property!(TypeError);
    // Uint8Array ( . . . )
    // Uint8ClampedArray ( . . . )
    // Uint16Array ( . . . )
    // Uint32Array ( . . . )
    // URIError ( . . . )
    constructor_property!(URIError);
    // WeakMap ( . . . )
    // WeakRef ( . . . )
    // WeakSet ( . . . )

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    /////////       Other Properties of the Global Object
    //////////////////////////////////////////////////////////////////////////////////////////////////////
    // Atomics
    // JSON
    // Math
    // Reflect
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
pub fn initialize_host_defined_realm(install_test_hooks: bool) {
    let realm = create_realm();
    let new_context = ExecutionContext::new(None, realm, None);
    push_execution_context(new_context);
    set_realm_global_object(None, None);
    set_default_global_bindings();
    if install_test_hooks {
        let global = get_global_object().unwrap();
        macro_rules! global_data {
            ( $name:expr, $value:expr, $writable:expr, $enumerable:expr, $configurable:expr ) => {
                define_property_or_throw(
                    &global,
                    $name,
                    PotentialPropertyDescriptor::new()
                        .value(ECMAScriptValue::from($value))
                        .writable($writable)
                        .enumerable($enumerable)
                        .configurable($configurable),
                )
                .unwrap();
            };
        }
        global_data!("debug_token", "present", true, true, true);
    }
}

pub fn global_symbol_registry() -> Rc<RefCell<SymbolRegistry>> {
    AGENT.with(|agent| agent.gsr.borrow().as_ref().unwrap().clone())
}
impl Agent {
    pub fn set_global_symbol_registry(&self, gsr: Rc<RefCell<SymbolRegistry>>) {
        assert!(self.gsr.borrow().is_none(), "GSR: Attempted change after having already been set");
        *self.gsr.borrow_mut() = Some(gsr);
    }
}

pub fn evaluate(chunk: Rc<Chunk>, text: &str) -> Completion<ECMAScriptValue> {
    AGENT.with(|agent| {
        if agent.execution_context_stack.borrow().is_empty() {
            return Err(create_type_error("No active execution context"));
        }

        prepare_running_ec_for_execution(chunk);
        let result = execute(text);

        {
            let execution_context_stack = agent.execution_context_stack.borrow();
            let ec_idx = execution_context_stack.len() - 1;
            assert!(execution_context_stack[ec_idx].stack.is_empty());
        }

        result
    })
}

pub fn prepare_running_ec_for_execution(chunk: Rc<Chunk>) {
    AGENT.with(|agent| {
        assert!(!agent.execution_context_stack.borrow().is_empty());
        let index = agent.execution_context_stack.borrow().len() - 1;
        prepare_for_execution(index, chunk);
    })
}

pub fn prepare_for_execution(index: usize, chunk: Rc<Chunk>) {
    AGENT.with(|agent| {
        let mut execution_context_stack = agent.execution_context_stack.borrow_mut();
        execution_context_stack[index].chunk = Some(chunk);
        execution_context_stack[index].pc = 0;
    })
}

pub fn execute(text: &str) -> Completion<ECMAScriptValue> {
    AGENT.with(|agent| {
        // If our ec index drops below this, we exit.
        let initial_context_index = agent.execution_context_stack.borrow().len() - 1;
        loop {
            let index = agent.execution_context_stack.borrow().len() - 1;
            /* Diagnostics */
            print!("Stack: [ ");
            print!(
                "{}",
                agent.execution_context_stack.borrow()[index]
                    .stack
                    .iter()
                    .rev()
                    .map(|fc| match fc {
                        Ok(nc) => format!("{nc}"),
                        Err(ac) => format!("{ac}"),
                    })
                    .join(" ] [ ")
            );
            println!(" ]");

            if index < initial_context_index {
                break;
            }

            let chunk = match agent.execution_context_stack.borrow()[index].chunk.clone() {
                Some(r) => Ok(r),
                None => Err(create_type_error("No compiled units!")),
            }?;

            if agent.execution_context_stack.borrow()[index].pc >= chunk.opcodes.len() {
                break;
            }
            let (_, repr) = chunk.insn_repr_at(agent.execution_context_stack.borrow()[index].pc as usize);
            println!("{:04}{}", agent.execution_context_stack.borrow()[index].pc, repr);

            /* Real work */
            let icode = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize]; // in range due to while condition
            let instruction = Insn::try_from(icode).unwrap(); // failure is a coding error (the compiler broke)
            agent.execution_context_stack.borrow_mut()[index].pc += 1;
            match instruction {
                Insn::Nop => {
                    // Do nothing
                }
                Insn::ToDo => {
                    // Panic with a todo message
                    todo!()
                }
                Insn::String => {
                    let string_index = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize]; // failure is a coding error (the compiler broke)
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let string = &chunk.strings[string_index as usize];
                    agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(string.into()));
                }
                Insn::Null => {
                    agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(ECMAScriptValue::Null.into()))
                }
                Insn::True => agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(true.into())),
                Insn::False => agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(false.into())),
                Insn::Empty => {
                    agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(NormalCompletion::Empty))
                }
                Insn::Undefined => {
                    agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(ECMAScriptValue::Undefined.into()))
                }
                Insn::This => {
                    let this_resolved = resolve_this_binding().map(NormalCompletion::from);
                    agent.execution_context_stack.borrow_mut()[index].stack.push(this_resolved);
                }
                Insn::Resolve => {
                    let name = match agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap() {
                        NormalCompletion::Value(ECMAScriptValue::String(s)) => s,
                        _ => unreachable!(),
                    };
                    let resolved = resolve_binding(&name, None, false);
                    agent.execution_context_stack.borrow_mut()[index].stack.push(resolved);
                }
                Insn::StrictResolve => {
                    let name = match agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap() {
                        NormalCompletion::Value(ECMAScriptValue::String(s)) => s,
                        _ => unreachable!(),
                    };
                    let resolved = resolve_binding(&name, None, true);
                    agent.execution_context_stack.borrow_mut()[index].stack.push(resolved);
                }
                Insn::Float => {
                    let float_index = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize];
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let number = chunk.floats[float_index as usize];
                    agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(number.into()));
                }
                Insn::Bigint => {
                    let bigint_index = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize];
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let number = Rc::clone(&chunk.bigints[bigint_index as usize]);
                    agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(number.into()));
                }
                Insn::GetValue => {
                    let reference = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap();
                    let value = get_value(reference);
                    agent.execution_context_stack.borrow_mut()[index].stack.push(value.map(NormalCompletion::from));
                }
                Insn::PutValue => {
                    let w = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap();
                    let v = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap();
                    let result = put_value(v, w.map(|v| v.try_into().unwrap()));
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result.map(NormalCompletion::from));
                }
                Insn::JumpIfAbrupt => {
                    let jump = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize] as i16;
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let stack_idx = agent.execution_context_stack.borrow()[index].stack.len() - 1;
                    if agent.execution_context_stack.borrow()[index].stack[stack_idx].is_err() {
                        if jump >= 0 {
                            agent.execution_context_stack.borrow_mut()[index].pc += jump as usize;
                        } else {
                            agent.execution_context_stack.borrow_mut()[index].pc -= (-jump) as usize;
                        }
                    }
                }
                Insn::JumpIfNormal => {
                    let jump = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize] as i16;
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let stack_idx = agent.execution_context_stack.borrow()[index].stack.len() - 1;
                    if agent.execution_context_stack.borrow()[index].stack[stack_idx].is_ok() {
                        if jump >= 0 {
                            agent.execution_context_stack.borrow_mut()[index].pc += jump as usize;
                        } else {
                            agent.execution_context_stack.borrow_mut()[index].pc -= (-jump) as usize;
                        }
                    }
                }
                Insn::JumpIfFalse | Insn::JumpIfTrue => {
                    let mut execution_context = &mut agent.execution_context_stack.borrow_mut()[index];
                    let jump = chunk.opcodes[execution_context.pc as usize] as i16;
                    execution_context.pc += 1;
                    let stack_idx = execution_context.stack.len() - 1;
                    let bool_val = bool::from(
                        ECMAScriptValue::try_from(
                            execution_context.stack[stack_idx]
                                .clone()
                                .expect("Boolean Jumps may only be used with Normal completions"),
                        )
                        .expect("Boolean Jumps may only be used with Values"),
                    );
                    if (instruction == Insn::JumpIfFalse && !bool_val) || (instruction == Insn::JumpIfTrue && bool_val)
                    {
                        if jump >= 0 {
                            execution_context.pc += jump as usize;
                        } else {
                            execution_context.pc -= (-jump) as usize;
                        }
                    }
                }
                Insn::JumpPopIfFalse | Insn::JumpPopIfTrue => {
                    let mut ec = &mut agent.execution_context_stack.borrow_mut()[index];
                    let jump = chunk.opcodes[ec.pc as usize] as i16;
                    ec.pc += 1;
                    let bool_val = bool::from(
                        ECMAScriptValue::try_from(
                            ec.stack
                                .pop()
                                .expect("JumpPop must have an argument")
                                .expect("Boolean Jumps may only be used with Normal completions"),
                        )
                        .expect("Boolean Jumps may only be used with Values"),
                    );
                    if (instruction == Insn::JumpPopIfFalse && !bool_val)
                        || (instruction == Insn::JumpPopIfTrue && bool_val)
                    {
                        if jump >= 0 {
                            ec.pc += jump as usize;
                        } else {
                            ec.pc -= (-jump) as usize;
                        }
                    }
                }
                Insn::JumpIfNotNullish => {
                    let mut ec = &mut agent.execution_context_stack.borrow_mut()[index];
                    let jump = chunk.opcodes[ec.pc as usize] as i16;
                    ec.pc += 1;
                    let stack_idx = ec.stack.len() - 1;
                    let val = ECMAScriptValue::try_from(
                        ec.stack[stack_idx].clone().expect("Nullish Jumps may only be used with Normal completions"),
                    )
                    .expect("Nullish Jumps may only be used with Values");
                    if val != ECMAScriptValue::Undefined && val != ECMAScriptValue::Null {
                        if jump >= 0 {
                            ec.pc += jump as usize;
                        } else {
                            ec.pc -= (-jump) as usize;
                        }
                    }
                }
                Insn::JumpIfNotUndef => {
                    let mut ec = &mut agent.execution_context_stack.borrow_mut()[index];
                    let jump = chunk.opcodes[ec.pc as usize] as i16;
                    ec.pc += 1;
                    let stack_idx = ec.stack.len() - 1;
                    let val = ECMAScriptValue::try_from(
                        ec.stack[stack_idx].clone().expect("Undef Jumps may only be used with Normal completions"),
                    )
                    .expect("Undef Jumps may only be used with Values");
                    if val != ECMAScriptValue::Undefined {
                        if jump >= 0 {
                            ec.pc += jump as usize;
                        } else {
                            ec.pc -= (-jump) as usize;
                        }
                    }
                }
                Insn::JumpNotThrow => {
                    let mut ec = &mut agent.execution_context_stack.borrow_mut()[index];
                    let jump = chunk.opcodes[ec.pc as usize] as i16;
                    ec.pc += 1;
                    let stack_idx = ec.stack.len() - 1;
                    let completion = &ec.stack[stack_idx];

                    if !matches!(completion, Err(AbruptCompletion::Throw { .. })) {
                        if jump >= 0 {
                            ec.pc += jump as usize;
                        } else {
                            ec.pc -= (-jump) as usize;
                        }
                    }
                }
                Insn::Jump => {
                    let mut ec = &mut agent.execution_context_stack.borrow_mut()[index];
                    let jump = chunk.opcodes[ec.pc as usize] as i16;
                    ec.pc += 1;
                    if jump >= 0 {
                        ec.pc += jump as usize;
                    } else {
                        ec.pc -= (-jump) as usize;
                    }
                }
                Insn::UpdateEmpty => {
                    let ec = &mut agent.execution_context_stack.borrow_mut()[index];
                    let newer = ec.stack.pop().unwrap();
                    let older = ec.stack.pop().unwrap().unwrap();
                    ec.stack.push(update_empty(newer, older));
                }
                Insn::Pop2Push3 => {
                    let ec = &mut agent.execution_context_stack.borrow_mut()[index];
                    // Stack: top lower ====> top lower top
                    let top = ec.stack.pop().unwrap();
                    let lower = ec.stack.pop().unwrap();
                    let bottom = top.clone();
                    ec.stack.push(bottom);
                    ec.stack.push(lower);
                    ec.stack.push(top);
                }
                Insn::Ref | Insn::StrictRef => {
                    let ec = &mut agent.execution_context_stack.borrow_mut()[index];
                    let strict = instruction == Insn::StrictRef;
                    // Stack: name base ...
                    let name = {
                        let result: Result<ECMAScriptValue, _> = ec.stack.pop().unwrap().unwrap().try_into();
                        let value: Result<PropertyKey, _> = result.unwrap().try_into();
                        value.unwrap()
                    };
                    // Stack: base ...
                    let base = {
                        let result: Result<ECMAScriptValue, _> = ec.stack.pop().unwrap().unwrap().try_into();
                        result.unwrap()
                    };
                    // Stack: ...
                    let reference = Reference::new(Base::Value(base), name, strict, None);
                    let result = Ok(NormalCompletion::from(reference));
                    ec.stack.push(result);
                    // Stack: ref ...
                }
                Insn::Pop => {
                    let ec = &mut agent.execution_context_stack.borrow_mut()[index];
                    let stack_size = ec.stack.len();
                    assert!(stack_size > 0);
                    ec.stack.truncate(stack_size - 1);
                }
                Insn::Swap => {
                    let ec = &mut agent.execution_context_stack.borrow_mut()[index];
                    let stack_size = ec.stack.len();
                    assert!(stack_size >= 2);
                    ec.stack.swap(stack_size - 1, stack_size - 2);
                }
                Insn::SwapList => {
                    let ec = &mut agent.execution_context_stack.borrow_mut()[index];
                    let stack_size = ec.stack.len();
                    assert!(stack_size >= 2);
                    let list_len = f64::try_from(
                        ECMAScriptValue::try_from(
                            ec.stack[stack_size - 1].clone().expect("Top of stack must contain a list"),
                        )
                        .expect("Top of stack must contain a list"),
                    )
                    .expect("Top of stack must contain a list") as usize;
                    let item = ec.stack.remove(stack_size - list_len - 2);
                    ec.stack.push(item);
                }
                Insn::InitializeReferencedBinding => {
                    let (value, lhs) = {
                        let ec = &mut agent.execution_context_stack.borrow_mut()[index];
                        let stack_size = ec.stack.len();
                        assert!(stack_size >= 2);
                        let value = ec.stack.pop().unwrap();
                        let lhs = ec.stack.pop().unwrap();
                        (value, lhs)
                    };
                    let result = initialize_referenced_binding(lhs, value.map(|nc| nc.try_into().unwrap()))
                        .map(NormalCompletion::from);
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::PushNewLexEnv => {
                    let current_env = current_lexical_environment();
                    let new_env = DeclarativeEnvironmentRecord::new(current_env, "inner block");
                    set_lexical_environment(Some(Rc::new(new_env)));
                }
                Insn::PopLexEnv => {
                    let current_env = current_lexical_environment().expect("lex environment must exist");
                    let outer_env = current_env.get_outer_env();
                    set_lexical_environment(outer_env);
                }
                Insn::PushNewVarEnvFromLex => {
                    let current_env = current_lexical_environment();
                    let new_env = DeclarativeEnvironmentRecord::new(current_env, "new var env");
                    set_variable_environment(Some(Rc::new(new_env)));
                }
                Insn::PushNewLexEnvFromVar => {
                    let current_env = current_variable_environment();
                    let new_env = DeclarativeEnvironmentRecord::new(current_env, "new var env");
                    set_lexical_environment(Some(Rc::new(new_env)));
                }
                Insn::SetLexEnvToVarEnv => {
                    let current_env = current_variable_environment();
                    set_lexical_environment(current_env);
                }

                Insn::CreateStrictImmutableLexBinding | Insn::CreateNonStrictImmutableLexBinding => {
                    let env = current_lexical_environment().expect("lex environment must exist");
                    let string_idx = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize] as usize;
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let name = chunk.strings[string_idx].clone();

                    env.create_immutable_binding(name, instruction == Insn::CreateStrictImmutableLexBinding)
                        .expect("binding should not already exist");
                }
                Insn::CreatePermanentMutableLexBinding | Insn::CreatePermanentMutableVarBinding => {
                    let env = if instruction == Insn::CreatePermanentMutableLexBinding {
                        current_lexical_environment().expect("lex environment must exist")
                    } else {
                        current_variable_environment().expect("var environment must exist")
                    };
                    let string_idx = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize] as usize;
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let name = chunk.strings[string_idx].clone();

                    env.create_mutable_binding(name, false).expect("binding should not already exist");
                }
                Insn::CreatePermanentMutableLexIfMissing => {
                    //  1. Let alreadyDeclared be ! env.HasBinding(paramName).
                    //  2. If alreadyDeclared is false, then
                    //        a. Perform ! env.CreateMutableBinding(paramName, false).
                    let env = current_lexical_environment().expect("lex environment must exist");
                    let string_idx = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize] as usize;
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let name = &chunk.strings[string_idx];

                    let already_declared = env.has_binding(name).expect("basic environments can't fail this");
                    if !already_declared {
                        env.create_mutable_binding(name.clone(), false).expect("binding does not already exist");
                    }
                }
                Insn::CreateInitializedPermanentMutableLexIfMissing => {
                    //  1. Let alreadyDeclared be ! env.HasBinding(paramName).
                    //  2. If alreadyDeclared is false, then
                    //      a. Perform ! env.CreateMutableBinding(paramName, false).
                    //      b. Perform ! env.InitializeBinding(paramName, undefined).
                    let env = current_lexical_environment().expect("lex environment must exist");
                    let string_idx = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize] as usize;
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let name = &chunk.strings[string_idx];

                    let already_declared = env.has_binding(name).expect("basic environments can't fail this");
                    if !already_declared {
                        env.create_mutable_binding(name.clone(), false).expect("binding does not already exist");
                        env.initialize_binding(name, ECMAScriptValue::Undefined)
                            .expect("binding is not previously initialized");
                    }
                }
                Insn::InitializeLexBinding | Insn::InitializeVarBinding => {
                    let env = if instruction == Insn::InitializeLexBinding {
                        current_lexical_environment().expect("lex environment must exist")
                    } else {
                        current_variable_environment().expect("var environment must exist")
                    };
                    let string_idx = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize] as usize;
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let name = &chunk.strings[string_idx];

                    let value = ECMAScriptValue::try_from(
                        agent.execution_context_stack.borrow_mut()[index]
                            .stack
                            .pop()
                            .expect("InitializeLexBinding must have a stack arg")
                            .expect("InitializeLexBinding's stack arg must be a normal completion"),
                    )
                    .expect("InitializeLexBinding's stack arg must be a value");
                    env.initialize_binding(name, value).expect("binding should not already have a value");
                }
                Insn::GetLexBinding => {
                    let env = current_lexical_environment().expect("lex environment must exist");
                    let string_idx = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize] as usize;
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let name = &chunk.strings[string_idx];
                    let value = env.get_binding_value(name, false).expect("Binding will be there");
                    agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(NormalCompletion::from(value)));
                }
                Insn::SetMutableVarBinding => {
                    let env = current_variable_environment().expect("var environment must exist");
                    let string_idx = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize] as usize;
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let name = &chunk.strings[string_idx];
                    let value = ECMAScriptValue::try_from(
                        agent.execution_context_stack.borrow_mut()[index]
                            .stack
                            .pop()
                            .expect("SetMutableVarBinding must have a stack arg")
                            .expect("SetMutableVarBinding's stack arg must be a normal completion"),
                    )
                    .expect("SetMutableVarBinding's stack arg must be a value");
                    env.set_mutable_binding(name.clone(), value, false).expect("error free execution");
                }
                Insn::ExtractThrownValue => {
                    let stack_idx = agent.execution_context_stack.borrow()[index].stack.len() - 1;
                    let completion = &agent.execution_context_stack.borrow()[index].stack[stack_idx];
                    match completion.as_ref().unwrap_err() {
                        AbruptCompletion::Throw { value } => {
                            agent.execution_context_stack.borrow_mut()[index].stack[stack_idx] =
                                Ok(value.clone().into())
                        }
                        _ => panic!("Bad error type for ExtractThrownValue"),
                    }
                }
                Insn::ExtractArg => {
                    let ec = &mut agent.execution_context_stack.borrow_mut()[index];
                    // Stack: N arg[N-1] arg[N-2] ... arg[1] arg[0] (when N >= 1)
                    // Out: arg[0] N-1 arg[N-1] arg[N-2] ... arg[1]
                    //   --or, if N == 0 --
                    // Stack: 0
                    // Out: Undefined 0
                    let stack_len = ec.stack.len();
                    assert!(stack_len > 0, "ExtractArg must have an argument list on the stack");
                    let arg_count = f64::try_from(
                        ECMAScriptValue::try_from(
                            ec.stack[stack_len - 1].clone().expect("ExtractArg must have a 'count' argument"),
                        )
                        .expect("ExtractArg must have a 'count' argument"),
                    )
                    .expect("ExtractArg 'count' arg must be a number");
                    if arg_count < 0.5 {
                        ec.stack.push(Ok(NormalCompletion::from(ECMAScriptValue::Undefined)));
                    } else {
                        let arg_count = arg_count as usize;
                        assert!(stack_len > arg_count, "Stack must contain an argument list");
                        let arg0 = ec.stack.remove(stack_len - arg_count - 1);
                        ec.stack[stack_len - 2] = Ok(NormalCompletion::from((arg_count - 1) as u32));
                        ec.stack.push(arg0);
                    }
                }
                Insn::FinishArgs => {
                    let ec = &mut agent.execution_context_stack.borrow_mut()[index];
                    // Stack: N arg[N-1] ... arg[0]
                    // Out:
                    // Remove any remaining arguments from the stack (we're at zero, or the caller gave us too much)
                    let arg_count = f64::try_from(
                        ECMAScriptValue::try_from(
                            ec.stack
                                .pop()
                                .expect("FinishArgs must have a 'count' argument")
                                .expect("FinishArgs must have a 'count' argument"),
                        )
                        .expect("FinishArgs must have a 'count' argument"),
                    )
                    .expect("FinishArgs 'count' arg must be a number");
                    let to_retain = ec.stack.len() - arg_count as usize;
                    ec.stack.truncate(to_retain);
                }
                Insn::Object => {
                    let obj_proto = intrinsic(IntrinsicId::ObjectPrototype);
                    let o = ordinary_object_create(Some(obj_proto), &[]);
                    agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(ECMAScriptValue::from(o).into()));
                }
                Insn::CreateDataProperty => {
                    let nc_value = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap();
                    let nc_name = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap();
                    let nc_obj = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap();
                    let obj = Object::try_from(nc_obj).unwrap();
                    let name = PropertyKey::try_from(nc_name).unwrap();
                    let value = ECMAScriptValue::try_from(nc_value).unwrap();
                    create_data_property_or_throw(&obj, name, value).unwrap();
                    agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(NormalCompletion::from(obj)));
                }
                Insn::SetPrototype => {
                    let nc_value = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap();
                    let nc_obj = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap();
                    let obj = Object::try_from(nc_obj).unwrap();
                    let value = ECMAScriptValue::try_from(nc_value).unwrap();
                    let val_obj_res: anyhow::Result<Option<Object>> = value.try_into();
                    if let Ok(new_proto) = val_obj_res {
                        obj.o.set_prototype_of(new_proto).unwrap();
                    }
                    agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(NormalCompletion::from(obj)));
                }
                Insn::ToPropertyKey => {
                    let nc_name = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap();
                    let value_name = ECMAScriptValue::try_from(nc_name).unwrap();
                    let key = to_property_key(value_name);
                    let fc = key.map(|pk| NormalCompletion::from(ECMAScriptValue::from(pk)));
                    agent.execution_context_stack.borrow_mut()[index].stack.push(fc);
                }
                Insn::CopyDataProps => {
                    let nc_value = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap();
                    let nc_obj = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap();
                    let obj = Object::try_from(nc_obj).unwrap();
                    let value = ECMAScriptValue::try_from(nc_value).unwrap();
                    let result = copy_data_properties(&obj, value, &[]);
                    let fc = match result {
                        Ok(_) => Ok(NormalCompletion::from(obj)),
                        Err(e) => Err(e),
                    };
                    agent.execution_context_stack.borrow_mut()[index].stack.push(fc);
                }
                Insn::Dup => {
                    let idx = agent.execution_context_stack.borrow()[index].stack.len() - 1;
                    let fc = agent.execution_context_stack.borrow()[index].stack[idx].clone();
                    agent.execution_context_stack.borrow_mut()[index].stack.push(fc);
                }
                Insn::ToNumeric => {
                    let nc_val = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap();
                    let val: ECMAScriptValue = nc_val.try_into().unwrap();
                    let result = to_numeric(val).map(NormalCompletion::from);
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::Increment => {
                    let nc_val = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap();
                    let num: Numeric = nc_val.try_into().unwrap();
                    let result_val: ECMAScriptValue = match num {
                        Numeric::Number(n) => (n + 1.0).into(),
                        Numeric::BigInt(bi) => ECMAScriptValue::BigInt(Rc::new(&*bi + 1)),
                    };
                    let fc = Ok(NormalCompletion::from(result_val));
                    agent.execution_context_stack.borrow_mut()[index].stack.push(fc);
                }
                Insn::Decrement => {
                    let nc_val = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap();
                    let num: Numeric = nc_val.try_into().unwrap();
                    let result_val: ECMAScriptValue = match num {
                        Numeric::Number(n) => (n - 1.0).into(),
                        Numeric::BigInt(bi) => ECMAScriptValue::BigInt(Rc::new(&*bi - 1)),
                    };
                    let fc = Ok(NormalCompletion::from(result_val));
                    agent.execution_context_stack.borrow_mut()[index].stack.push(fc);
                }
                Insn::PreIncrement => {
                    let fc = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap();
                    let result = prefix_increment(fc);
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::PreDecrement => {
                    let fc = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap();
                    let result = prefix_decrement(fc);
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::Delete => {
                    let fc = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap();
                    let result = delete_ref(fc);
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::Void => {
                    let fc = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap();
                    let result = void_operator(fc);
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::TypeOf => {
                    let fc = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap();
                    let result = typeof_operator(fc);
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::Unwind => {
                    let vals_to_remove =
                        chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize] as usize;
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    assert!(vals_to_remove < agent.execution_context_stack.borrow()[index].stack.len());
                    if vals_to_remove > 0 {
                        let old_index_of_err = agent.execution_context_stack.borrow()[index].stack.len() - 1;
                        let new_index_of_err = old_index_of_err - vals_to_remove;
                        agent.execution_context_stack.borrow_mut()[index]
                            .stack
                            .swap(new_index_of_err, old_index_of_err);
                        agent.execution_context_stack.borrow_mut()[index].stack.truncate(new_index_of_err + 1);
                    }
                }
                Insn::UnwindList => {
                    let err_to_keep = agent.execution_context_stack.borrow_mut()[index]
                        .stack
                        .pop()
                        .expect("UnwindList has two stack args");
                    let vals_to_remove = f64::try_from(
                        ECMAScriptValue::try_from(
                            agent.execution_context_stack.borrow_mut()[index]
                                .stack
                                .pop()
                                .expect("UnwindList has a stack argument")
                                .expect("UnwindList expects a normal completion"),
                        )
                        .expect("UnwindList expects a value"),
                    )
                    .expect("UnwindList expects a number") as usize;
                    if vals_to_remove > 0 {
                        let old_stack_size = agent.execution_context_stack.borrow()[index].stack.len();
                        assert!(vals_to_remove <= old_stack_size);
                        let new_stack_size = old_stack_size - vals_to_remove;
                        agent.execution_context_stack.borrow_mut()[index].stack.truncate(new_stack_size);
                    }
                    agent.execution_context_stack.borrow_mut()[index].stack.push(err_to_keep);
                }
                Insn::Call => {
                    let arg_count_nc = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap();
                    let arg_count_val = ECMAScriptValue::try_from(arg_count_nc).unwrap();
                    let arg_count: usize = (f64::try_from(arg_count_val).unwrap().round() as i64).try_into().unwrap();
                    let mut arguments = Vec::with_capacity(arg_count);
                    for _ in 1..=arg_count {
                        let nc = ECMAScriptValue::try_from(
                            agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap(),
                        )
                        .unwrap();
                        arguments.push(nc);
                    }
                    arguments.reverse();
                    let func_nc = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap();
                    let func_val = ECMAScriptValue::try_from(func_nc).unwrap();
                    let ref_nc = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap();

                    begin_call_evaluation(func_val, ref_nc, &arguments);
                }
                Insn::EndFunction => {
                    let stack_len = agent.execution_context_stack.borrow()[index].stack.len();
                    assert!(stack_len >= 2);
                    let result = agent.execution_context_stack.borrow_mut()[index]
                        .stack
                        .pop()
                        .expect("Stack is at least 2 elements");
                    let f_obj = Object::try_from(
                        ECMAScriptValue::try_from(
                            agent.execution_context_stack.borrow_mut()[index]
                                .stack
                                .pop()
                                .expect("Stack is at least 2 elements")
                                .expect("function obj argument must be a function obj"),
                        )
                        .expect("function obj argument must be a function obj"),
                    )
                    .expect("function obj argument must be a function obj");
                    let callable = f_obj.o.to_callable_obj().expect("function obj argument must be a function obj");
                    callable.end_evaluation(result);
                }
                Insn::Construct => {
                    // Stack: N arg[n-1] arg[n-2] ... arg[0] newtgt cstr
                    let arg_count_nc = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap();
                    let arg_count_val = ECMAScriptValue::try_from(arg_count_nc).unwrap();
                    let arg_count: usize = (f64::try_from(arg_count_val).unwrap().round() as i64).try_into().unwrap();
                    let mut arguments = Vec::with_capacity(arg_count);
                    for _ in 1..=arg_count {
                        let nc = ECMAScriptValue::try_from(
                            agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap(),
                        )
                        .unwrap();
                        arguments.push(nc);
                    }
                    arguments.reverse();
                    let newtgt = ECMAScriptValue::try_from(
                        agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap(),
                    )
                    .expect("new target must be value");
                    let cstr_nc = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap();
                    let cstr_val = ECMAScriptValue::try_from(cstr_nc).unwrap();

                    begin_constructor_evaluation(cstr_val, newtgt, &arguments);
                }
                Insn::RequireConstructor => {
                    let x = ec_pop()
                        .ok_or(())
                        .and_then(|fc| fc.map_err(|_| ()))
                        .and_then(|nc| ECMAScriptValue::try_from(nc).map_err(|_| ()))
                        .and_then(|val| if is_constructor(&val) { Ok(()) } else { Err(()) })
                        .map_err(|_| create_type_error("Constructor required"));
                    ec_push(x.map(NormalCompletion::from));
                }

                Insn::Return => {
                    let value = ECMAScriptValue::try_from(
                        ec_pop().expect("Return needs an argument").expect("Return needs a normal completion"),
                    )
                    .expect("Return needs a value");
                    ec_push(Err(AbruptCompletion::Return { value }));
                }
                Insn::UnaryPlus => {
                    let exp = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap();
                    let val = get_value(exp);
                    let result = match val {
                        Ok(ev) => to_number(ev).map(NormalCompletion::from),
                        Err(ac) => Err(ac),
                    };
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::UnaryMinus => {
                    let exp = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap();
                    let val = get_value(exp);
                    let old_val = match val {
                        Ok(val) => to_numeric(val),
                        Err(ac) => Err(ac),
                    };
                    let result = match old_val {
                        Err(ac) => Err(ac),
                        Ok(Numeric::Number(n)) => Ok(NormalCompletion::from(-n)),
                        Ok(Numeric::BigInt(bi)) => Ok(NormalCompletion::from(Rc::new(-&*bi))),
                    };
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::UnaryComplement => {
                    let exp = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap();
                    let val = get_value(exp);
                    let old_val = match val {
                        Ok(val) => to_numeric(val),
                        Err(ac) => Err(ac),
                    };
                    let result = match old_val {
                        Err(ac) => Err(ac),
                        Ok(Numeric::Number(n)) => Ok(NormalCompletion::from(!to_int32(n).unwrap())),
                        Ok(Numeric::BigInt(bi)) => Ok(NormalCompletion::from(Rc::new(!&*bi))),
                    };
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::UnaryNot => {
                    let exp = agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap();
                    let val = get_value(exp);
                    let result = match val {
                        Ok(val) => Ok(NormalCompletion::from(!to_boolean(val))),
                        Err(ac) => Err(ac),
                    };
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::Exponentiate => agent.binary_operation(index, BinOp::Exponentiate),
                Insn::Multiply => agent.binary_operation(index, BinOp::Multiply),
                Insn::Divide => agent.binary_operation(index, BinOp::Divide),
                Insn::Modulo => agent.binary_operation(index, BinOp::Remainder),
                Insn::Add => agent.binary_operation(index, BinOp::Add),
                Insn::Subtract => agent.binary_operation(index, BinOp::Subtract),

                Insn::InstantiateIdFreeFunctionExpression => {
                    let id = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize]; // failure is a coding error (the compiler broke)
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let info = &chunk.function_object_data[id as usize];
                    instantiate_ordinary_function_expression_without_binding_id(index, text, info)
                }
                Insn::InstantiateOrdinaryFunctionExpression => {
                    let id = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize]; // failure is a coding error (the compiler broke)
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let info = &chunk.function_object_data[id as usize];
                    instantiate_ordinary_function_expression_with_binding_id(index, text, info)
                }

                Insn::InstantiateArrowFunctionExpression => {
                    let id = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize]; // failure is a coding error (the compiler broke)
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let info = &chunk.function_object_data[id as usize];
                    instantiate_arrow_function_expression(Some(index), text, info)
                }
                Insn::InstantiateOrdinaryFunctionObject => {
                    let string_index = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize]; // failure is a coding error (the compiler broke)
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let string = &chunk.strings[string_index as usize];
                    let func_index = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize] as usize;
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let info = &chunk.function_object_data[func_index as usize];
                    instantiate_ordinary_function_object(Some(index), text, string, info)
                }
                Insn::LeftShift => agent.binary_operation(index, BinOp::LeftShift),
                Insn::SignedRightShift => agent.binary_operation(index, BinOp::SignedRightShift),
                Insn::UnsignedRightShift => agent.binary_operation(index, BinOp::UnsignedRightShift),
                Insn::Throw => {
                    // Convert the NormalCompletion::Value on top of the stack into a ThrowCompletion with a matching value
                    let exp: ECMAScriptValue = agent.execution_context_stack.borrow_mut()[index]
                        .stack
                        .pop()
                        .expect("Throw requires an argument")
                        .expect("Throw requires a NormalCompletion")
                        .try_into()
                        .expect("Throw requires a value");
                    agent.execution_context_stack.borrow_mut()[index]
                        .stack
                        .push(Err(AbruptCompletion::Throw { value: exp }));
                }
                Insn::Less => {
                    let (lval, rval) = agent.two_values(index);
                    let result =
                        is_less_than(lval, rval, true).map(|optb| NormalCompletion::from(optb.unwrap_or(false)));
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::Greater => {
                    let (lval, rval) = agent.two_values(index);
                    let result =
                        is_less_than(rval, lval, false).map(|optb| NormalCompletion::from(optb.unwrap_or(false)));
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::LessEqual => {
                    let (lval, rval) = agent.two_values(index);
                    let result =
                        is_less_than(rval, lval, false).map(|optb| NormalCompletion::from(!optb.unwrap_or(true)));
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::GreaterEqual => {
                    let (lval, rval) = agent.two_values(index);
                    let result =
                        is_less_than(lval, rval, true).map(|optb| NormalCompletion::from(!optb.unwrap_or(true)));
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::InstanceOf => {
                    let (lval, rval) = agent.two_values(index);
                    let result = instanceof_operator(lval, rval);
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::In => {
                    let (lval, rval) = agent.two_values(index);
                    let result = match rval {
                        ECMAScriptValue::Object(obj) => {
                            let key = to_property_key(lval)?;
                            has_property(&obj, &key).map(NormalCompletion::from)
                        }
                        _ => Err(create_type_error("Right-hand side of 'in' must be an object")),
                    };
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::Equal => {
                    let (lval, rval) = agent.two_values(index);
                    let result = is_loosely_equal(&lval, &rval).map(NormalCompletion::from);
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::NotEqual => {
                    let (lval, rval) = agent.two_values(index);
                    let result = is_loosely_equal(&lval, &rval).map(|val| NormalCompletion::from(!val));
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::StrictEqual => {
                    let (lval, rval) = agent.two_values(index);
                    let result = Ok(NormalCompletion::from(lval.is_strictly_equal(&rval)));
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::StrictNotEqual => {
                    let (lval, rval) = agent.two_values(index);
                    let result = Ok(NormalCompletion::from(!lval.is_strictly_equal(&rval)));
                    agent.execution_context_stack.borrow_mut()[index].stack.push(result);
                }
                Insn::BitwiseAnd => agent.binary_operation(index, BinOp::BitwiseAnd),
                Insn::BitwiseOr => agent.binary_operation(index, BinOp::BitwiseOr),
                Insn::BitwiseXor => agent.binary_operation(index, BinOp::BitwiseXor),
                Insn::CreateUnmappedArguments => create_unmapped_arguments_object(index),
                Insn::CreateMappedArguments => create_mapped_arguments_object(index),
                Insn::AddMappedArgument => {
                    let string_index = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize]; // failure is a coding error (the compiler broke)
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let string = &chunk.strings[string_index as usize];
                    let argument_index =
                        chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize] as usize;
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    attach_mapped_arg(index, string, argument_index);
                }
                Insn::HandleEmptyBreak => {
                    let prior_result = agent.execution_context_stack.borrow_mut()[index]
                        .stack
                        .pop()
                        .expect("HandleEmptyBreak requires an argument");
                    let new_result = if let Err(AbruptCompletion::Break { value, target: None }) = prior_result {
                        match value {
                            NormalCompletion::Empty => Ok(NormalCompletion::from(ECMAScriptValue::Undefined)),
                            value => Ok(value),
                        }
                    } else {
                        prior_result
                    };
                    agent.execution_context_stack.borrow_mut()[index].stack.push(new_result);
                }
                Insn::HandleTargetedBreak => {
                    let str_idx = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize] as usize;
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let label = &chunk.strings[str_idx];
                    let prior_result = agent.execution_context_stack.borrow_mut()[index]
                        .stack
                        .pop()
                        .expect("HandleTargetedBreak requires an argument");
                    let new_result = match prior_result {
                        Err(AbruptCompletion::Break { value, target: Some(target) }) if &target == label => Ok(value),
                        _ => prior_result,
                    };
                    agent.execution_context_stack.borrow_mut()[index].stack.push(new_result);
                }
                Insn::CoalesceValue => {
                    // Stack: stmtResult V ...
                    // If stmtResult.[[Value]] is not empty, set V to stmtResult.[[Value]].
                    let stmt_result = agent.execution_context_stack.borrow_mut()[index]
                        .stack
                        .pop()
                        .expect("CoalesceValue requires two arguments");
                    let v = ECMAScriptValue::try_from(
                        agent.execution_context_stack.borrow_mut()[index]
                            .stack
                            .pop()
                            .expect("CoalesceValue requires two arguments")
                            .expect("argument V must be  normal completion"),
                    )
                    .expect("argument V must be a value");
                    agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(match stmt_result {
                        Ok(NormalCompletion::Value(value))
                        | Err(AbruptCompletion::Throw { value })
                        | Err(AbruptCompletion::Return { value })
                        | Err(AbruptCompletion::Continue { value: NormalCompletion::Value(value), .. })
                        | Err(AbruptCompletion::Break { value: NormalCompletion::Value(value), .. }) => {
                            NormalCompletion::from(value)
                        }
                        _ => NormalCompletion::from(v),
                    }));
                }
                Insn::LoopContinues => {
                    let ec = &mut agent.execution_context_stack.borrow_mut()[index];
                    let set_idx = chunk.opcodes[ec.pc as usize] as usize;
                    ec.pc += 1;
                    let label_set = &chunk.label_sets[set_idx];
                    // 1. If completion.[[Type]] is normal, return true.
                    // 2. If completion.[[Type]] is not continue, return false.
                    // 3. If completion.[[Target]] is empty, return true.
                    // 4. If completion.[[Target]] is an element of labelSet, return true.
                    // 5. Return false.
                    let idx = ec.stack.len() - 1;
                    let completion = &ec.stack[idx];
                    let result = match completion {
                        Ok(_) => true,
                        Err(AbruptCompletion::Continue { value: _, target: None }) => true,
                        Err(AbruptCompletion::Continue { value: _, target: Some(label) }) => label_set.contains(label),
                        _ => false,
                    };
                    ec.stack.push(Ok(NormalCompletion::from(result)));
                }
                Insn::Continue => {
                    agent.execution_context_stack.borrow_mut()[index]
                        .stack
                        .push(Err(AbruptCompletion::Continue { value: NormalCompletion::Empty, target: None }));
                }
                Insn::TargetedContinue => {
                    let str_idx = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize] as usize;
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let label = chunk.strings[str_idx].clone();
                    agent.execution_context_stack.borrow_mut()[index]
                        .stack
                        .push(Err(AbruptCompletion::Continue { value: NormalCompletion::Empty, target: Some(label) }));
                }
                Insn::Break => {
                    agent.execution_context_stack.borrow_mut()[index]
                        .stack
                        .push(Err(AbruptCompletion::Break { value: NormalCompletion::Empty, target: None }));
                }
                Insn::TargetedBreak => {
                    let str_idx = chunk.opcodes[agent.execution_context_stack.borrow()[index].pc as usize] as usize;
                    agent.execution_context_stack.borrow_mut()[index].pc += 1;
                    let label = chunk.strings[str_idx].clone();
                    agent.execution_context_stack.borrow_mut()[index]
                        .stack
                        .push(Err(AbruptCompletion::Break { value: NormalCompletion::Empty, target: Some(label) }));
                }
            }
        }
        let index = agent.execution_context_stack.borrow().len() - 1;
        agent.execution_context_stack.borrow_mut()[index]
            .stack
            .pop()
            .map(|svr| {
                svr.map(|sv| match sv {
                    NormalCompletion::Reference(_) | NormalCompletion::Empty => ECMAScriptValue::Undefined,
                    NormalCompletion::Value(v) => v,
                    NormalCompletion::Environment(..) => unreachable!(),
                })
            })
            .unwrap_or(Ok(ECMAScriptValue::Undefined))
    })
}

fn begin_call_evaluation(func: ECMAScriptValue, reference: NormalCompletion, arguments: &[ECMAScriptValue]) {
    let this_value = match &reference {
        NormalCompletion::Empty | NormalCompletion::Environment(..) => unreachable!(),
        NormalCompletion::Value(_) => ECMAScriptValue::Undefined,
        NormalCompletion::Reference(r) => match &r.base {
            Base::Unresolvable => unreachable!(),
            Base::Environment(e) => {
                e.with_base_object().map(ECMAScriptValue::from).unwrap_or(ECMAScriptValue::Undefined)
            }
            Base::Value(_) => r.get_this_value(),
        },
    };
    if !func.is_object() {
        let err = Err(create_type_error("not an object"));
        ec_push(err);
        return;
    }
    if !is_callable(&func) {
        let err = Err(create_type_error("not a function"));
        ec_push(err);
        return;
    }
    initiate_call(&func, &this_value, arguments);
}

fn begin_constructor_evaluation(cstr: ECMAScriptValue, newtgt: ECMAScriptValue, args: &[ECMAScriptValue]) {
    assert!(is_constructor(&cstr));
    let cstr = Object::try_from(cstr).expect("Must be a constructor");
    let newtgt = Object::try_from(newtgt).expect("Must be an object");
    initiate_construct(&cstr, args, Some(&newtgt));
}

fn prefix_increment(expr: FullCompletion) -> FullCompletion {
    let value = get_value(expr.clone())?;
    let old_value = to_numeric(value)?;
    let new_value: ECMAScriptValue = match old_value {
        Numeric::Number(n) => (n + 1.0).into(),
        Numeric::BigInt(bi) => ECMAScriptValue::from(&*bi + 1),
    };
    put_value(expr, Ok(new_value.clone()))?;
    Ok(NormalCompletion::from(new_value))
}

fn prefix_decrement(expr: FullCompletion) -> FullCompletion {
    let value = get_value(expr.clone())?;
    let old_value = to_numeric(value)?;
    let new_value: ECMAScriptValue = match old_value {
        Numeric::Number(n) => (n - 1.0).into(),
        Numeric::BigInt(bi) => ECMAScriptValue::from(&*bi - 1),
    };
    put_value(expr, Ok(new_value.clone()))?;
    Ok(NormalCompletion::from(new_value))
}

fn delete_ref(expr: FullCompletion) -> FullCompletion {
    let reference = expr?;
    match reference {
        NormalCompletion::Environment(..) => unreachable!(),
        NormalCompletion::Empty | NormalCompletion::Value(_) => Ok(true.into()),
        NormalCompletion::Reference(r) => match *r {
            Reference { base: Base::Unresolvable, .. } => Ok(true.into()),
            Reference { base: Base::Value(_), this_value: Some(_), .. } => {
                Err(create_reference_error("super properties not deletable"))
            }
            Reference { base: Base::Value(val), referenced_name, strict, this_value: None } => {
                let base_obj = to_object(val)?;
                let delete_status =
                    base_obj.o.delete(&referenced_name.try_into().expect("Property name will never be private"))?;
                if !delete_status && strict {
                    Err(create_type_error("property not deletable"))
                } else {
                    Ok(delete_status.into())
                }
            }
            Reference { base: Base::Environment(base), referenced_name, .. } => {
                let delete_status =
                    base.delete_binding(&referenced_name.try_into().expect("Property name will never be private"))?;
                Ok(delete_status.into())
            }
        },
    }
}

fn void_operator(expr: FullCompletion) -> FullCompletion {
    get_value(expr)?;
    Ok(ECMAScriptValue::Undefined.into())
}

fn typeof_operator(expr: FullCompletion) -> FullCompletion {
    if let Ok(NormalCompletion::Reference(r)) = &expr {
        if r.is_unresolvable_reference() {
            return Ok(NormalCompletion::from("undefined"));
        }
    }

    let val = get_value(expr)?;
    let type_string = match val {
        ECMAScriptValue::Undefined => "undefined",
        ECMAScriptValue::Null => "object",
        ECMAScriptValue::Boolean(_) => "boolean",
        ECMAScriptValue::String(_) => "string",
        ECMAScriptValue::Number(_) => "number",
        ECMAScriptValue::BigInt(_) => "bigint",
        ECMAScriptValue::Symbol(_) => "symbol",
        ECMAScriptValue::Object(obj) => {
            if obj.o.is_callable_obj() {
                "function"
            } else {
                "object"
            }
        }
    };
    Ok(NormalCompletion::from(type_string))
}

impl Agent {
    fn two_values(&self, index: usize) -> (ECMAScriptValue, ECMAScriptValue) {
        let (right, left) = {
            let stack = &mut self.execution_context_stack.borrow_mut()[index].stack;
            (stack.pop(), stack.pop())
        };
        let rval: ECMAScriptValue = right
            .expect("Operation requires an argument")
            .expect("Right must be a NormalCompletion")
            .try_into()
            .expect("Right must be a value");
        let lval: ECMAScriptValue = left
            .expect("Operation requires two arguments")
            .expect("Left must be a NormalCompletion")
            .try_into()
            .expect("Left must be a value");
        (lval, rval)
    }

    fn binary_operation(&self, index: usize, op: BinOp) {
        let (lval, rval) = self.two_values(index);
        let result = apply_string_or_numeric_binary_operator(lval, rval, op);
        self.execution_context_stack.borrow_mut()[index].stack.push(result);
    }
}

fn apply_string_or_numeric_binary_operator(lval: ECMAScriptValue, rval: ECMAScriptValue, op: BinOp) -> FullCompletion {
    let (lval, rval) = if op == BinOp::Add {
        let lprim = to_primitive(lval, None)?;
        let rprim = to_primitive(rval, None)?;
        if lprim.is_string() || rprim.is_string() {
            let lstr = to_string(lprim)?;
            let rstr = to_string(rprim)?;
            return Ok(NormalCompletion::from(lstr.concat(rstr)));
        }
        (lprim, rprim)
    } else {
        (lval, rval)
    };
    let lnum = to_numeric(lval)?;
    let rnum = to_numeric(rval)?;
    match (lnum, rnum, op) {
        (Numeric::Number(left), Numeric::Number(right), BinOp::Exponentiate) => {
            Ok(NormalCompletion::from(left.powf(right)))
        }
        (Numeric::Number(left), Numeric::Number(right), BinOp::Multiply) => Ok(NormalCompletion::from(left * right)),
        (Numeric::Number(left), Numeric::Number(right), BinOp::Divide) => Ok(NormalCompletion::from(left / right)),
        (Numeric::Number(left), Numeric::Number(right), BinOp::Remainder) => Ok(NormalCompletion::from(left % right)),
        (Numeric::Number(left), Numeric::Number(right), BinOp::Add) => Ok(NormalCompletion::from(left + right)),
        (Numeric::Number(left), Numeric::Number(right), BinOp::Subtract) => Ok(NormalCompletion::from(left - right)),
        (Numeric::Number(left), Numeric::Number(right), BinOp::LeftShift) => {
            let lnum = to_int32(left).expect("Numbers are always convertable to Int32");
            let rnum = to_uint32(right).expect("Numbers are always convertable to Uint32");
            let shift_count = rnum % 32;
            Ok(NormalCompletion::from(lnum << shift_count))
        }
        (Numeric::Number(left), Numeric::Number(right), BinOp::SignedRightShift) => {
            let lnum = to_int32(left).expect("Numbers are always convertable to Int32");
            let rnum = to_uint32(right).expect("Numbers are always convertable to Uint32");
            let shift_count = rnum % 32;
            Ok(NormalCompletion::from(lnum >> shift_count))
        }
        (Numeric::Number(left), Numeric::Number(right), BinOp::UnsignedRightShift) => {
            let lnum = to_uint32(left).expect("Numbers are always convertable to Uint32");
            let rnum = to_uint32(right).expect("Numbers are always convertable to Uint32");
            let shift_count = rnum % 32;
            Ok(NormalCompletion::from(lnum >> shift_count))
        }
        (Numeric::Number(left), Numeric::Number(right), BinOp::BitwiseAnd) => {
            let lnum = to_int32(left).expect("Numbers are always convertable to int32");
            let rnum = to_int32(right).expect("Numbers are always convertable to int32");
            Ok(NormalCompletion::from(lnum & rnum))
        }
        (Numeric::Number(left), Numeric::Number(right), BinOp::BitwiseOr) => {
            let lnum = to_int32(left).expect("Numbers are always convertable to int32");
            let rnum = to_int32(right).expect("Numbers are always convertable to int32");
            Ok(NormalCompletion::from(lnum | rnum))
        }
        (Numeric::Number(left), Numeric::Number(right), BinOp::BitwiseXor) => {
            let lnum = to_int32(left).expect("Numbers are always convertable to int32");
            let rnum = to_int32(right).expect("Numbers are always convertable to int32");
            Ok(NormalCompletion::from(lnum ^ rnum))
        }
        (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::Exponentiate) => {
            let exponent = BigUint::try_from(&*right).map_err(|_| create_range_error("Exponent must be positive"))?;
            let base = (*left).clone();
            Ok(NormalCompletion::from(Rc::new(base.pow(exponent))))
        }
        (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::Multiply) => {
            Ok(NormalCompletion::from(Rc::new(&*left * &*right)))
        }
        (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::Divide) => left
            .checked_div(&right)
            .map(NormalCompletion::from)
            .map(Ok)
            .unwrap_or_else(|| Err(create_range_error("Division by zero"))),
        (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::Remainder) => {
            if right.is_zero() {
                Err(create_range_error("Division by zero"))
            } else {
                Ok(NormalCompletion::from(&*left % &*right))
            }
        }
        (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::Add) => Ok(NormalCompletion::from(&*left + &*right)),
        (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::Subtract) => {
            Ok(NormalCompletion::from(&*left - &*right))
        }
        (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::LeftShift) => bigint_leftshift(&left, &right)
            .map_err(|err| create_range_error(err.to_string()))
            .map(NormalCompletion::from),
        (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::SignedRightShift) => bigint_rightshift(&left, &right)
            .map_err(|err| create_range_error(err.to_string()))
            .map(NormalCompletion::from),
        (Numeric::BigInt(_), Numeric::BigInt(_), BinOp::UnsignedRightShift) => {
            Err(create_type_error("BigInts have no unsigned right shift, use >> instead"))
        }
        (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::BitwiseAnd) => {
            Ok(NormalCompletion::from(&*left & &*right))
        }
        (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::BitwiseOr) => {
            Ok(NormalCompletion::from(&*left | &*right))
        }
        (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::BitwiseXor) => {
            Ok(NormalCompletion::from(&*left ^ &*right))
        }
        (Numeric::BigInt(_), Numeric::Number(_), _) | (Numeric::Number(_), Numeric::BigInt(_), _) => {
            Err(create_type_error("Cannot mix BigInt and other types, use explicit conversions"))
        }
    }
}

fn instantiate_ordinary_function_expression_without_binding_id(index: usize, text: &str, info: &StashedFunctionData) {
    // The syntax-directed operation InstantiateOrdinaryFunctionExpression takes optional argument name and
    // returns a function object. It is defined piecewise over the following productions:
    //
    //  FunctionExpression : function ( FormalParameters ) { FunctionBody }
    //      1. If name is not present, set name to "".
    //      2. Let env be the LexicalEnvironment of the running execution context.
    //      3. Let privateEnv be the running execution context's PrivateEnvironment.
    //      4. Let sourceText be the source text matched by FunctionExpression.
    //      5. Let closure be OrdinaryFunctionCreate(%Function.prototype%, sourceText, FormalParameters,
    //         FunctionBody, non-lexical-this, env, privateEnv).
    //      6. Perform SetFunctionName(closure, name).
    //      7. Perform MakeConstructor(closure).
    //      8. Return closure.

    let to_compile: Rc<FunctionExpression> =
        info.to_compile.clone().try_into().expect("This routine only used with FunctionExpressions");
    let name = nameify(&info.source_text, 50);
    let mut compiled = Chunk::new(name);
    let compilation_status = to_compile.body.compile_body(&mut compiled, text, info);
    if let Err(err) = compilation_status {
        AGENT.with(|agent| {
            let typeerror = create_type_error(err.to_string());
            let stack = &mut agent.execution_context_stack.borrow_mut()[index].stack;
            let l = stack.len();
            stack[l - 1] = Err(typeerror); // pop then push
        });
        return;
    }
    for line in compiled.disassemble() {
        println!("{line}");
    }

    // Name is on the stack.
    // env/privateenv come from the agent.
    // sourceText is on the stack? Or with the productions?
    // FormalParameters/FunctionBody are... In a registry someplace maybe? With a registry ID in the instruction?
    // strict is ... in that registry? (Needs to be not part of the current chunk).
    let env = current_lexical_environment().expect("Lexical environment must exist if code is running");
    let priv_env = current_private_environment();

    let name = AGENT.with(|agent| {
        PropertyKey::try_from(
            ECMAScriptValue::try_from(
                agent.execution_context_stack.borrow_mut()[index]
                    .stack
                    .pop()
                    .expect("Insn only used with argument on stack")
                    .expect("Argument must not be an AbruptCompletion"),
            )
            .expect("Argument must be a value"),
        )
        .expect("Argument must be a property key")
    });

    let function_prototype = intrinsic(IntrinsicId::FunctionPrototype);

    let closure = ordinary_function_create(
        function_prototype,
        info.source_text.as_str(),
        info.params.clone(),
        info.body.clone(),
        ThisLexicality::NonLexicalThis,
        env,
        priv_env,
        info.strict,
        Rc::new(compiled),
    );

    set_function_name(&closure, name.into(), None);
    make_constructor(&closure, None);

    AGENT.with(|agent| agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(closure.into())));
}

fn instantiate_ordinary_function_expression_with_binding_id(index: usize, text: &str, info: &StashedFunctionData) {
    // The syntax-directed operation InstantiateOrdinaryFunctionExpression takes optional argument name
    // and returns a function object. It is defined piecewise over the following productions:
    //
    // FunctionExpression : function BindingIdentifier ( FormalParameters ) { FunctionBody }
    //
    //   1. Assert: name is not present.
    //   2. Set name to StringValue of BindingIdentifier.
    //   3. Let outerEnv be the running execution context's LexicalEnvironment.
    //   4. Let funcEnv be NewDeclarativeEnvironment(outerEnv).
    //   5. Perform ! funcEnv.CreateImmutableBinding(name, false).
    //   6. Let privateEnv be the running execution context's PrivateEnvironment.
    //   7. Let sourceText be the source text matched by FunctionExpression.
    //   8. Let closure be OrdinaryFunctionCreate(%Function.prototype%, sourceText, FormalParameters,
    //      FunctionBody, non-lexical-this, funcEnv, privateEnv).
    //   9. Perform SetFunctionName(closure, name).
    //  10. Perform MakeConstructor(closure).
    //  11. Perform ! funcEnv.InitializeBinding(name, closure).
    //  12. Return closure.
    //
    // NOTE: The BindingIdentifier in a FunctionExpression can be referenced from inside the
    // FunctionExpression's FunctionBody to allow the function to call itself recursively. However, unlike
    // in a FunctionDeclaration, the BindingIdentifier in a FunctionExpression cannot be referenced from
    // and does not affect the scope enclosing the FunctionExpression.

    // First: compile the function.
    let to_compile: Rc<FunctionExpression> =
        info.to_compile.clone().try_into().expect("This routine only used with FunctionExpressions");
    let chunk_name = nameify(&info.source_text, 50);
    let mut compiled = Chunk::new(chunk_name);
    let compilation_status = to_compile.body.compile_body(&mut compiled, text, info);
    if let Err(err) = compilation_status {
        AGENT.with(|agent| {
            let typeerror = create_type_error(err.to_string());
            let mut execution_context_stack = agent.execution_context_stack.borrow_mut();
            let stack = &mut execution_context_stack[index].stack;
            let l = stack.len();
            stack[l - 1] = Err(typeerror); // pop then push
        });
        return;
    }
    for line in compiled.disassemble() {
        println!("{line}");
    }

    let name = AGENT.with(|agent| {
        JSString::try_from(
            ECMAScriptValue::try_from(
                agent.execution_context_stack.borrow_mut()[index]
                    .stack
                    .pop()
                    .expect("Insn only used with argument on stack")
                    .expect("Argument must not be an AbruptCompletion"),
            )
            .expect("Argument must be a value"),
        )
        .expect("Argument must be a string")
    });

    let outer_env = current_lexical_environment().expect("Lexical environment must exist if code is running");
    let func_env = Rc::new(DeclarativeEnvironmentRecord::new(Some(outer_env), name.clone()));
    let priv_env = current_private_environment();

    func_env.create_immutable_binding(name.clone(), false).expect("Fresh environment won't fail");
    let function_prototype = intrinsic(IntrinsicId::FunctionPrototype);

    let closure = ordinary_function_create(
        function_prototype,
        info.source_text.as_str(),
        info.params.clone(),
        info.body.clone(),
        ThisLexicality::NonLexicalThis,
        func_env.clone(),
        priv_env,
        info.strict,
        Rc::new(compiled),
    );

    set_function_name(&closure, name.clone().into(), None);
    make_constructor(&closure, None);
    func_env.initialize_binding(&name, closure.clone().into()).expect("binding has been created");

    AGENT.with(|agent| agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(closure.into())));
}

pub fn instantiate_arrow_function_expression(index: Option<usize>, text: &str, info: &StashedFunctionData) {
    let index = index.unwrap_or(AGENT.with(|agent| agent.execution_context_stack.borrow().len()) - 1);
    let env = current_lexical_environment().unwrap();
    let priv_env = current_private_environment();

    let name = AGENT.with(|agent| {
        JSString::try_from(
            ECMAScriptValue::try_from(agent.execution_context_stack.borrow_mut()[index].stack.pop().unwrap().unwrap())
                .unwrap(),
        )
        .unwrap()
    });

    let to_compile: Rc<ArrowFunction> =
        info.to_compile.clone().try_into().expect("This routine only used with Arrow Functions");
    let chunk_name = nameify(&info.source_text, 50);
    let mut compiled = Chunk::new(chunk_name);
    let compilation_status = to_compile.body.compile_body(&mut compiled, text, info);
    if let Err(err) = compilation_status {
        AGENT.with(|agent| {
            let typeerror = create_type_error(err.to_string());
            let mut execution_context_stack = agent.execution_context_stack.borrow_mut();
            let l = execution_context_stack[index].stack.len();
            execution_context_stack[index].stack[l - 1] = Err(typeerror); // pop then push
        });
        return;
    }
    for line in compiled.disassemble() {
        println!("{line}");
    }

    let function_prototype = intrinsic(IntrinsicId::FunctionPrototype);

    let closure = ordinary_function_create(
        function_prototype,
        info.source_text.as_str(),
        info.params.clone(),
        info.body.clone(),
        ThisLexicality::LexicalThis,
        env,
        priv_env,
        info.strict,
        Rc::new(compiled),
    );
    set_function_name(&closure, name.into(), None);

    AGENT.with(|agent| agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(closure.into())));
}

pub fn instantiate_ordinary_function_object(
    index: Option<usize>,
    text: &str,
    name: &JSString,
    info: &StashedFunctionData,
) {
    let index = index.unwrap_or(AGENT.with(|agent| agent.execution_context_stack.borrow().len()) - 1);
    let to_compile: Rc<FunctionDeclaration> =
        info.to_compile.clone().try_into().expect("This routine only used with Function Declarations");
    let chunk_name = nameify(&info.source_text, 50);
    let mut compiled = Chunk::new(chunk_name);
    let compilation_status = to_compile.body.compile_body(&mut compiled, text, info);
    if let Err(err) = compilation_status {
        let typeerror = create_type_error(err.to_string());
        AGENT.with(|agent| {
            let mut execution_context_stack = agent.execution_context_stack.borrow_mut();
            let l = execution_context_stack[index].stack.len();
            execution_context_stack[index].stack[l - 1] = Err(typeerror); // pop then push
        });
        return;
    }
    for line in compiled.disassemble() {
        println!("{line}");
    }

    let env = current_lexical_environment().unwrap();
    let priv_env = current_private_environment();
    let function_prototype = intrinsic(IntrinsicId::FunctionPrototype);

    let closure = ordinary_function_create(
        function_prototype,
        info.source_text.as_str(),
        info.params.clone(),
        info.body.clone(),
        ThisLexicality::NonLexicalThis,
        env,
        priv_env,
        info.strict,
        Rc::new(compiled),
    );
    set_function_name(&closure, name.clone().into(), None);
    make_constructor(&closure, None);

    AGENT.with(|agent| agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(closure.into())));
}

fn is_less_than(x: ECMAScriptValue, y: ECMAScriptValue, left_first: bool) -> Completion<Option<bool>> {
    let (px, py) = if left_first {
        let px = to_primitive(x, None)?;
        let py = to_primitive(y, None)?;
        (px, py)
    } else {
        let py = to_primitive(y, None)?;
        let px = to_primitive(x, None)?;
        (px, py)
    };
    if px.is_string() && py.is_string() {
        let sx = JSString::try_from(px).expect("String values must be strings");
        let sy = JSString::try_from(py).expect("String values must be strings");
        return Ok(Some(sx < sy));
    }
    if px.is_string() && py.is_bigint() {
        let nx = String::from(JSString::try_from(px).expect("String values must be strings")).parse::<BigInt>().ok();
        let ny: Rc<BigInt> = py.try_into().expect("Bigint values must be bigints");
        return match nx {
            None => Ok(None),
            Some(nx) => Ok(Some(nx < *ny)),
        };
    }
    if px.is_bigint() && py.is_string() {
        let nx: Rc<BigInt> = px.try_into().expect("Bigint values must be bigints");
        let ny = String::from(JSString::try_from(py).expect("String values must be strings")).parse::<BigInt>().ok();
        return match ny {
            None => Ok(None),
            Some(ny) => Ok(Some(*nx < ny)),
        };
    }
    let nx = to_numeric(px)?;
    let ny = to_numeric(py)?;
    match (nx, ny) {
        (Numeric::Number(nx), Numeric::Number(ny)) => {
            if nx.is_nan() || ny.is_nan() {
                Ok(None)
            } else {
                Ok(Some(nx < ny))
            }
        }
        (Numeric::Number(nx), Numeric::BigInt(by)) => {
            if nx.is_nan() {
                Ok(None)
            } else if nx == f64::NEG_INFINITY {
                Ok(Some(true))
            } else if nx == f64::INFINITY {
                Ok(Some(false))
            } else {
                Ok(Some(nx < by.to_f64().unwrap()))
            }
        }
        (Numeric::BigInt(bx), Numeric::Number(ny)) => {
            if ny.is_nan() {
                Ok(None)
            } else if ny == f64::NEG_INFINITY {
                Ok(Some(false))
            } else if ny == f64::INFINITY {
                Ok(Some(true))
            } else {
                Ok(Some(bx.to_f64().unwrap() < ny))
            }
        }
        (Numeric::BigInt(bx), Numeric::BigInt(by)) => Ok(Some(*bx < *by)),
    }
}

fn instanceof_operator(v: ECMAScriptValue, target: ECMAScriptValue) -> FullCompletion {
    // InstanceofOperator ( V, target )
    //
    // The abstract operation InstanceofOperator takes arguments V (an ECMAScript language value) and target (an
    // ECMAScript language value) and returns either a normal completion containing a Boolean or a throw completion.
    // It implements the generic algorithm for determining if V is an instance of target either by consulting
    // target's @@hasInstance method or, if absent, determining whether the value of target's "prototype" property
    // is present in V's prototype chain. It performs the following steps when called:
    //
    //  1. If Type(target) is not Object, throw a TypeError exception.
    //  2. Let instOfHandler be ? GetMethod(target, @@hasInstance).
    //  3. If instOfHandler is not undefined, then
    //      a. Return ToBoolean(? Call(instOfHandler, target, « V »)).
    //  4. If IsCallable(target) is false, throw a TypeError exception.
    //  5. Return ? OrdinaryHasInstance(target, V).
    //
    //
    // NOTE    | Steps 4 and 5 provide compatibility with previous editions of ECMAScript that did not use a
    //         | @@hasInstance method to define the instanceof operator semantics. If an object does not define or
    //         | inherit @@hasInstance it uses the default instanceof semantics.
    match &target {
        ECMAScriptValue::Object(_) => {
            let hi = wks(WksId::HasInstance);
            let instof_handler = get_method(&target, &hi.into())?;
            match &instof_handler {
                ECMAScriptValue::Undefined => {
                    if !is_callable(&target) {
                        Err(create_type_error("Right-hand side of 'instanceof' is not callable"))
                    } else {
                        ordinary_has_instance(&target, &v).map(NormalCompletion::from)
                    }
                }
                _ => {
                    let res = call(&instof_handler, &target, &[v])?;
                    Ok(NormalCompletion::from(to_boolean(res)))
                }
            }
        }
        _ => Err(create_type_error("Right-hand side of 'instanceof' is not an object")),
    }
}

pub fn create_unmapped_arguments_object(index: usize) {
    // Stack should have n arg[n-1] arg[n-2] ... arg[0] ...
    // Those values are NOT consumed; this function assumes they'll be used again.

    let stack_len = AGENT.with(|agent| agent.execution_context_stack.borrow()[index].stack.len());
    assert!(stack_len > 0, "Stack must not be empty");
    let length = f64::try_from(
        ECMAScriptValue::try_from(AGENT.with(|agent| {
            agent.execution_context_stack.borrow()[index].stack[stack_len - 1]
                .clone()
                .expect("Non-error arguments needed")
        }))
        .expect("Value arguments needed"),
    )
    .expect("Numeric arguments needed") as u32;
    assert!(stack_len > length as usize, "Stack too short to fit all the arguments");

    let obj = ArgumentsObject::object(None);
    define_property_or_throw(
        &obj,
        "length",
        PotentialPropertyDescriptor::new().value(length).writable(true).enumerable(false).configurable(true),
    )
    .expect("Normal Object");

    let first_arg_index = stack_len - length as usize - 1;
    let arguments = AGENT.with(|agent| {
        agent.execution_context_stack.borrow()[index].stack[first_arg_index..first_arg_index + length as usize].to_vec()
    });

    for (arg_number, item) in arguments.into_iter().enumerate() {
        let value =
            ECMAScriptValue::try_from(item.expect("Non-error arguments needed")).expect("Value arguments needed");
        create_data_property_or_throw(&obj, arg_number, value).expect("Normal Object");
    }

    let iterator = wks(WksId::Iterator);
    let array_values = intrinsic(IntrinsicId::ArrayPrototypeValues);
    let throw_type_error = intrinsic(IntrinsicId::ThrowTypeError);
    define_property_or_throw(
        &obj,
        iterator,
        PotentialPropertyDescriptor::new().value(array_values).writable(true).enumerable(false).configurable(true),
    )
    .expect("Normal Object");
    define_property_or_throw(
        &obj,
        "callee",
        PotentialPropertyDescriptor::new()
            .get(throw_type_error.clone())
            .set(throw_type_error)
            .enumerable(false)
            .configurable(false),
    )
    .expect("Normal Object");

    AGENT.with(|agent| agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(NormalCompletion::from(obj))));
    // Stack at exit: AObj N arg[N-1] ... arg[0] ...
}

pub fn create_mapped_arguments_object(index: usize) {
    // Stack should have n arg[n-1] arg[n-2] ... arg[0] func ...

    let stack_len = AGENT.with(|agent| agent.execution_context_stack.borrow()[index].stack.len());
    assert!(stack_len > 0, "Stack must not be empty");
    let length = f64::try_from(
        ECMAScriptValue::try_from(
            AGENT
                .with(|agent| agent.execution_context_stack.borrow()[index].stack[stack_len - 1].clone())
                .expect("Non-error arguments needed"),
        )
        .expect("Value arguments needed"),
    )
    .expect("Numeric arguments needed") as u32;
    assert!(stack_len > length as usize + 1, "Stack too short to fit all the arguments plus the function obj");

    let first_arg_index = stack_len - length as usize - 1;
    let arguments = AGENT.with(|agent| {
        agent.execution_context_stack.borrow()[index].stack[first_arg_index..first_arg_index + length as usize].to_vec()
    });

    let env = current_lexical_environment().expect("A lex env must exist");
    let map = ParameterMap::new(env);
    let ao = ArgumentsObject::object(Some(map));

    for (idx, item) in arguments.into_iter().enumerate() {
        let val = ECMAScriptValue::try_from(item.expect("arguments must be values")).expect("arguments must be values");
        create_data_property_or_throw(&ao, idx, val).expect("ArgumentObject won't throw");
    }

    define_property_or_throw(
        &ao,
        "length",
        PotentialPropertyDescriptor::new().value(length).writable(true).enumerable(false).configurable(true),
    )
    .expect("ArgumentObject won't throw");
    let iterator = wks(WksId::Iterator);
    let array_values = intrinsic(IntrinsicId::ArrayPrototypeValues);
    define_property_or_throw(
        &ao,
        iterator,
        PotentialPropertyDescriptor::new().value(array_values).writable(true).enumerable(false).configurable(true),
    )
    .expect("ArgumentObject won't throw");
    let func = ECMAScriptValue::try_from(
        AGENT
            .with(|agent| agent.execution_context_stack.borrow()[index].stack[first_arg_index - 1].clone())
            .expect("Function object type error"),
    )
    .expect("Function object type error");
    define_property_or_throw(
        &ao,
        "callee",
        PotentialPropertyDescriptor::new().value(func).writable(true).enumerable(false).configurable(true),
    )
    .expect("ArgumentObject won't throw");

    AGENT.with(|agent| agent.execution_context_stack.borrow_mut()[index].stack.push(Ok(NormalCompletion::from(ao))));
    // Stack at exit: AObj N arg[N-1] ... arg[0] func ...
}

pub fn attach_mapped_arg(index: usize, name: &JSString, idx: usize) {
    // Stack: AObj ...
    let top = AGENT.with(|agent| agent.execution_context_stack.borrow()[index].stack.len());
    assert!(top > 0, "stack must not be empty");
    let obj = Object::try_from(
        ECMAScriptValue::try_from(
            AGENT
                .with(|agent| agent.execution_context_stack.borrow()[index].stack[top - 1].clone())
                .expect("arguments must be values"),
        )
        .expect("arguments must be values"),
    )
    .expect("argument must be an object");
    let ao = obj.o.to_arguments_object().expect("argument must be an ArgumentsObject");
    let pmap = ao.parameter_map.as_ref().expect("argument must be a mapped arguments object");
    let mut pmap = pmap.borrow_mut();
    pmap.add_mapped_name(name.clone(), idx);
    // Stack: AObj ...
}

#[derive(PartialEq, Eq)]
enum BinOp {
    Exponentiate,
    Multiply,
    Divide,
    Remainder,
    Add,
    Subtract,
    LeftShift,
    SignedRightShift,
    UnsignedRightShift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum WksId {
    AsyncIterator,
    HasInstance,
    IsConcatSpreadable,
    Iterator,
    Match,
    MatchAll,
    Replace,
    Search,
    Species,
    Split,
    ToPrimitive,
    ToStringTag,
    Unscopables,
}

#[derive(Debug)]
pub struct WellKnownSymbols {
    pub async_iterator_: Symbol,
    pub has_instance_: Symbol,
    pub is_concat_spreadable_: Symbol,
    pub iterator_: Symbol,
    pub match_: Symbol,
    pub match_all_: Symbol,
    pub replace_: Symbol,
    pub search_: Symbol,
    pub species_: Symbol,
    pub split_: Symbol,
    pub to_primitive_: Symbol,
    pub to_string_tag_: Symbol,
    pub unscopables_: Symbol,
}

pub fn parse_script(source_text: &str, realm: Rc<RefCell<Realm>>) -> Result<ScriptRecord, Vec<Object>> {
    let script = parse_text(source_text, ParseGoal::Script);
    match script {
        ParsedText::Errors(errs) => Err(errs),
        ParsedText::Script(script) => {
            let mut chunk = Chunk::new("top level script");
            script.compile(&mut chunk, source_text).unwrap();
            for line in chunk.disassemble() {
                println!("{line}");
            }
            Ok(ScriptRecord { realm, ecmascript_code: script, compiled: Rc::new(chunk), text: source_text.into() })
        }
    }
}

enum TopLevelLexDecl {
    Class(Rc<ClassDeclaration>),
    Lex(Rc<LexicalDeclaration>),
}
impl TryFrom<DeclPart> for TopLevelLexDecl {
    type Error = anyhow::Error;
    fn try_from(src: DeclPart) -> anyhow::Result<Self> {
        match src {
            DeclPart::ClassDeclaration(cd) => Ok(Self::Class(cd)),
            DeclPart::LexicalDeclaration(ld) => Ok(Self::Lex(ld)),
            _ => Err(anyhow!("Not a top-level lexical decl")),
        }
    }
}
#[derive(Debug, Clone)]
pub enum FcnDef {
    Function(Rc<FunctionDeclaration>),
    Generator(Rc<GeneratorDeclaration>),
    AsyncFun(Rc<AsyncFunctionDeclaration>),
    AsyncGen(Rc<AsyncGeneratorDeclaration>),
}
impl TryFrom<VarScopeDecl> for FcnDef {
    type Error = anyhow::Error;
    fn try_from(src: VarScopeDecl) -> anyhow::Result<Self> {
        match src {
            VarScopeDecl::FunctionDeclaration(fd) => Ok(Self::Function(fd)),
            VarScopeDecl::GeneratorDeclaration(gd) => Ok(Self::Generator(gd)),
            VarScopeDecl::AsyncFunctionDeclaration(afd) => Ok(Self::AsyncFun(afd)),
            VarScopeDecl::AsyncGeneratorDeclaration(agd) => Ok(Self::AsyncGen(agd)),
            _ => Err(anyhow!("Not a function def")),
        }
    }
}

impl TryFrom<DeclPart> for FcnDef {
    type Error = anyhow::Error;
    fn try_from(value: DeclPart) -> anyhow::Result<Self> {
        match value {
            DeclPart::FunctionDeclaration(fd) => Ok(Self::Function(fd)),
            DeclPart::GeneratorDeclaration(gd) => Ok(Self::Generator(gd)),
            DeclPart::AsyncFunctionDeclaration(afd) => Ok(Self::AsyncFun(afd)),
            DeclPart::AsyncGeneratorDeclaration(agd) => Ok(Self::AsyncGen(agd)),
            DeclPart::ClassDeclaration(_) | DeclPart::LexicalDeclaration(_) => Err(anyhow!("Not a function def")),
        }
    }
}
impl FcnDef {
    pub fn bound_name(&self) -> JSString {
        match self {
            FcnDef::Function(x) => x.bound_name(),
            FcnDef::Generator(x) => x.bound_name(),
            FcnDef::AsyncFun(x) => x.bound_name(),
            FcnDef::AsyncGen(x) => x.bound_name(),
        }
    }
    pub fn instantiate_function_object(
        &self,
        env: Rc<dyn EnvironmentRecord>,
        private_env: Option<Rc<RefCell<PrivateEnvironmentRecord>>>,
        strict: bool,
        text: &str,
    ) -> Completion<ECMAScriptValue> {
        match self {
            FcnDef::Function(x) => x.instantiate_function_object(env, private_env, strict, text, x.clone()),
            FcnDef::Generator(x) => x.instantiate_function_object(env, private_env, strict, text, x.clone()),
            FcnDef::AsyncFun(x) => x.instantiate_function_object(env, private_env, strict, text, x.clone()),
            FcnDef::AsyncGen(x) => x.instantiate_function_object(env, private_env, strict, text, x.clone()),
        }
    }
}

enum TopLevelVarDecl {
    VarDecl(Rc<VariableDeclaration>),
    ForBinding(Rc<ForBinding>),
}
impl TryFrom<VarScopeDecl> for TopLevelVarDecl {
    type Error = anyhow::Error;
    fn try_from(value: VarScopeDecl) -> Result<Self, Self::Error> {
        match value {
            VarScopeDecl::VariableDeclaration(vd) => Ok(Self::VarDecl(vd)),
            VarScopeDecl::ForBinding(fb) => Ok(Self::ForBinding(fb)),
            VarScopeDecl::FunctionDeclaration(_) => {
                Err(anyhow!("FunctionDeclaration seen when top-level var decl expected"))
            }
            VarScopeDecl::GeneratorDeclaration(_) => {
                Err(anyhow!("GeneratorDeclaration seen when top-level var decl expected"))
            }
            VarScopeDecl::AsyncFunctionDeclaration(_) => {
                Err(anyhow!("AsyncFunctionDeclaration seen when top-level var decl expected"))
            }
            VarScopeDecl::AsyncGeneratorDeclaration(_) => {
                Err(anyhow!("AsyncGeneratorDeclaration seen when top-level var decl expected"))
            }
        }
    }
}

pub fn global_declaration_instantiation(
    script: Rc<Script>,
    env: Rc<GlobalEnvironmentRecord>,
    strict: bool,
    text: &str,
) -> Completion<()> {
    println!("Creating Globals...");
    let lex_names = script.lexically_declared_names();
    let var_names = script.var_declared_names();
    for name in lex_names {
        if env.has_var_declaration(&name) {
            return Err(create_syntax_error(format!("{name}: already defined"), None));
        }
        if env.has_lexical_declaration(&name) {
            return Err(create_syntax_error(format!("{name}: already defined"), None));
        }
        let has_restricted_global = env.has_restricted_global_property(&name)?;
        if has_restricted_global {
            return Err(create_syntax_error(format!("{name} is restricted and may not be used"), None));
        }
    }
    for name in var_names {
        if env.has_lexical_declaration(&name) {
            return Err(create_syntax_error(format!("{name}: already defined"), None));
        }
    }
    let var_declarations = script.var_scoped_declarations();
    let mut functions_to_initialize = vec![];
    let mut declared_function_names = vec![];
    for d in var_declarations.iter().rev().cloned().filter_map(|decl| FcnDef::try_from(decl).ok()) {
        let func_name = d.bound_name();
        if !declared_function_names.contains(&func_name) {
            let fn_definable = env.can_declare_global_function(&func_name)?;
            if !fn_definable {
                return Err(create_type_error(format!("Cannot create global function {func_name}")));
            }
            declared_function_names.push(func_name);
            functions_to_initialize.insert(0, d);
        }
    }
    let mut declared_var_names = vec![];
    for d in var_declarations.into_iter().filter_map(|pn| TopLevelVarDecl::try_from(pn).ok()) {
        for vn in match d {
            TopLevelVarDecl::VarDecl(vd) => vd.bound_names(),
            TopLevelVarDecl::ForBinding(fb) => fb.bound_names(),
        } {
            if !declared_function_names.contains(&vn) {
                let vn_definable = env.can_declare_global_var(&vn)?;
                if !vn_definable {
                    return Err(create_type_error(format!("Cannot create global variable {vn}")));
                }
                if !declared_var_names.contains(&vn) {
                    declared_var_names.push(vn);
                }
            }
        }
    }
    let lex_declarations =
        script.lexically_scoped_declarations().into_iter().filter_map(|d| TopLevelLexDecl::try_from(d).ok());
    let private_env = None;
    for d in lex_declarations {
        let (names, is_constant) = match &d {
            TopLevelLexDecl::Class(cd) => (cd.bound_names(), false),
            TopLevelLexDecl::Lex(ld) => (ld.bound_names(), ld.is_constant_declaration()),
        };
        for dn in names {
            if is_constant {
                println!("   immutable: {dn}");
                env.create_immutable_binding(dn, true)?;
            } else {
                println!("   mutable:   {dn}");
                env.create_mutable_binding(dn, false)?;
            }
        }
    }
    for f in functions_to_initialize {
        let name = f.bound_name();
        let func_obj =
            f.instantiate_function_object(env.clone() as Rc<dyn EnvironmentRecord>, private_env.clone(), strict, text)?;
        println!("   function:  {name}");
        env.create_global_function_binding(name, func_obj, false)?;
    }
    for vn in declared_var_names {
        println!("   var:       {vn}");
        env.create_global_var_binding(vn, false)?;
    }
    println!("..done");

    Ok(())
}

pub fn script_evaluation(sr: ScriptRecord) -> Completion<ECMAScriptValue> {
    let global_env = sr.realm.borrow().global_env.clone();
    let mut script_context =
        ExecutionContext::new(None, Rc::clone(&sr.realm), Some(ScriptOrModule::Script(Rc::new(sr.clone()))));
    script_context.lexical_environment = global_env.clone().map(|g| g as Rc<dyn EnvironmentRecord>);
    script_context.variable_environment = global_env.clone().map(|g| g as Rc<dyn EnvironmentRecord>);

    push_execution_context(script_context);

    let script = sr.ecmascript_code.clone();

    let strict = script.body.as_ref().map(|b| b.contains_use_strict()).unwrap_or(false);

    let result = global_declaration_instantiation(script, global_env.unwrap(), strict, &sr.text)
        .and_then(|_| evaluate(sr.compiled, &sr.text));

    pop_execution_context();

    result
}

#[derive(Debug)]
pub enum ProcessError {
    RuntimeError { error: ECMAScriptValue },
    CompileErrors { values: Vec<Object> },
    InternalError { reason: String },
}

impl error::Error for ProcessError {}

impl fmt::Display for ProcessError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ProcessError::RuntimeError { error } => {
                if let ECMAScriptValue::Object(o) = error {
                    if o.o.is_error_object() {
                        return write!(f, "Thrown: {}", unwind_any_error_object(o));
                    }
                }
                write!(f, "Thrown: {error}")
            }
            ProcessError::CompileErrors { values } => {
                writeln!(f, "During compilation:")?;
                for err_obj in values {
                    writeln!(f, "{}", unwind_any_error_object(err_obj))?;
                }
                Ok(())
            }
            ProcessError::InternalError { reason } => write!(f, "{reason}"),
        }
    }
}

pub fn process_ecmascript(source_text: &str) -> Result<ECMAScriptValue, ProcessError> {
    let realm = current_realm_record().unwrap();
    let x = parse_script(source_text, realm).map_err(|errs| ProcessError::CompileErrors { values: errs })?;

    let result = script_evaluation(x);
    match result {
        Ok(val) => Ok(val),
        Err(e) => Err(ProcessError::RuntimeError {
            error: ThrowValue::try_from(e).expect("Only ThrowCompletions come from script executions").into(),
        }),
    }
}

pub fn bigint_leftshift(left: &BigInt, right: &BigInt) -> Result<BigInt, anyhow::Error> {
    if right < &BigInt::zero() {
        bigint_rightshift(left, &-right)
    } else {
        Ok(left << u32::try_from(right)?)
    }
}

pub fn bigint_rightshift(left: &BigInt, right: &BigInt) -> Result<BigInt, anyhow::Error> {
    if right < &BigInt::zero() {
        bigint_leftshift(left, &-right)
    } else {
        Ok(match u32::try_from(right) {
            Ok(shift_amt) => left >> shift_amt,
            Err(_) => BigInt::zero(),
        })
    }
}

#[cfg(test)]
mod tests;
