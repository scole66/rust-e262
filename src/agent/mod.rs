use super::*;
use ahash::AHashSet;
use anyhow::{anyhow, Context};
use genawaiter::rc::{Co, Gen};
use itertools::Itertools;
use num::pow::Pow;
use num::{BigInt, BigUint, ToPrimitive, Zero};
use std::cell::{Cell, RefCell};
use std::convert::identity;
use std::convert::TryFrom;
use std::convert::TryInto;
use std::error;
use std::fmt;
use std::ops::Not;
use std::rc::Rc;
use thiserror::Error;

// The tail-call optimization tests "work" if they can go 100,000 deep. (Because TCO means that we don't
// actually consume context stack space.) So we put in an artificial limit for less than that so that TCO
// tests will fail until TCO actually gets implemented.
const RECURSION_LIMIT: usize = 90000;

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
    pub execution_context_stack: RefCell<Vec<ExecutionContext>>,
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

pub fn current_script_or_module() -> Option<ScriptOrModule> {
    AGENT.with(|agent| {
        let execution_context_stack = agent.execution_context_stack.borrow();
        let ec = &execution_context_stack[execution_context_stack.len() - 1];
        ec.script_or_module.clone()
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
    });
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

pub fn ec_peek(from_end: usize) -> Option<FullCompletion> {
    AGENT.with(|agent| {
        let execution_context_stack = agent.execution_context_stack.borrow();
        let len = execution_context_stack.len();
        match len {
            0 => None,
            _ => {
                let ec = &execution_context_stack[len - 1];
                let stack_len = ec.stack.len();
                match stack_len {
                    n if n > from_end => Some(ec.stack[n - from_end - 1].clone()),
                    _ => None,
                }
            }
        }
    })
}

pub fn ec_pop_list() -> anyhow::Result<Vec<ECMAScriptValue>> {
    AGENT.with(|agent| {
        let mut ec_stack = agent.execution_context_stack.borrow_mut();
        let ec = ec_stack.last_mut().ok_or_else(|| anyhow!("no execution context"))?;
        let len = to_usize(f64::try_from(ECMAScriptValue::try_from(
            ec.stack
                .pop()
                .ok_or_else(|| anyhow!("empty application stack"))?
                .map_err(|_| anyhow!("Unexpected abrupt completion"))?,
        )?)?)?;
        let mut result = Vec::with_capacity(len);
        for _ in 0..len {
            result.push(ECMAScriptValue::try_from(
                ec.stack
                    .pop()
                    .ok_or_else(|| anyhow!("empty application stack"))?
                    .map_err(|_| anyhow!("Unexpected abrupt completion:"))?,
            )?);
        }
        Ok(result)
    })
}

pub fn execution_context_stack_len() -> usize {
    AGENT.with(|agent| {
        let execution_context_stack = agent.execution_context_stack.borrow();
        execution_context_stack.len()
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
        execution_context_stack.last().and_then(|context| context.private_environment.clone())
    })
}

pub fn set_lexical_environment(env: Option<Rc<dyn EnvironmentRecord>>) {
    AGENT.with(|agent| {
        let mut execution_context_stack = agent.execution_context_stack.borrow_mut();
        if let Some(context) = execution_context_stack.last_mut() {
            context.lexical_environment = env;
        }
    });
}

pub fn set_variable_environment(env: Option<Rc<dyn EnvironmentRecord>>) {
    AGENT.with(|agent| {
        let mut execution_context_stack = agent.execution_context_stack.borrow_mut();
        if let Some(context) = execution_context_stack.last_mut() {
            context.variable_environment = env;
        }
    });
}

pub fn set_private_environment(env: Option<Rc<RefCell<PrivateEnvironmentRecord>>>) {
    AGENT.with(|agent| {
        let mut execution_context_stack = agent.execution_context_stack.borrow_mut();
        if let Some(context) = execution_context_stack.last_mut() {
            context.private_environment = env;
        }
    });
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
        ordinary_object_create(Some(object_proto))
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
    macro_rules! function_property {
        ( $jsname:literal, $intrinsic:ident ) => {
            global_data!($jsname, intrinsic(IntrinsicId::$intrinsic), true, false, true);
        };
    }
    // eval ( x )
    function_property!("eval", Eval);
    // isFinite ( number )
    function_property!("isFinite", IsFinite);
    // isNaN ( number )
    function_property!("isNaN", IsNaN);
    // parseFloat ( string )
    function_property!("parseFloat", ParseFloat);
    // parseInt ( string, radix )
    function_property!("parseInt", ParseInt);
    // decodeURI ( encodedURI )
    function_property!("decodeURI", DecodeURI);
    // decodeURIComponent ( encodedURIComponent )
    function_property!("decodeURIComponent", DecodeURIComponent);
    // encodeURI ( uri )
    function_property!("encodeURI", EncodeURI);
    // encodeURIComponent ( uriComponent )
    function_property!("encodeURIComponent", EncodeURIComponent);

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
    constructor_property!(Array);
    // ArrayBuffer ( . . . )
    // BigInt ( . . . )
    constructor_property!(BigInt);
    // BigInt64Array ( . . . )
    // BigUint64Array ( . . . )
    // Boolean ( . . . )
    constructor_property!(Boolean);
    // DataView ( . . . )
    // Date ( . . . )
    constructor_property!(Date);
    // Error ( . . . )
    constructor_property!(Error);
    // EvalError ( . . . )
    constructor_property!(EvalError);
    // FinalizationRegistry ( . . . )
    // Float32Array ( . . . )
    // Float64Array ( . . . )
    // Function ( . . . )
    constructor_property!(Function);
    // Int8Array ( . . . )
    // Int16Array ( . . . )
    // Int32Array ( . . . )
    // Map ( . . . )
    // Number ( . . . )
    constructor_property!(Number);
    // Object ( . . . )
    constructor_property!(Object);
    // Promise ( . . . )
    // Proxy ( . . . )
    constructor_property!(Proxy);
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
    global_data!("Math", intrinsic(IntrinsicId::Math), true, false, true);
    // Reflect
    global_data!("Reflect", intrinsic(IntrinsicId::Reflect), true, false, true);
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
pub fn initialize_host_defined_realm(id: RealmId, install_test_hooks: bool) {
    let realm = create_realm(id);
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
        global_data!("$262", testrunner_helper(), true, true, true);
        let print_function = create_builtin_function(
            Box::new(print_262),
            None,
            f64::from(1),
            PropertyKey::from("print"),
            BUILTIN_FUNCTION_SLOTS,
            Some(current_realm_record().expect("realm should exist by now")),
            Some(intrinsic(IntrinsicId::FunctionPrototype)),
            None,
        );
        global_data!("print", print_function, true, false, true);
    }
}

fn print_262(_this: &ECMAScriptValue, _nt: Option<&Object>, args: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
    // print A function that exposes the string value of its first argument to the test runner. This is used
    // as a communication mechanism for asynchronous tests (via the async flag, described below).
    let mut a = FuncArgs::from(args);
    let message = a.next_arg();
    let message = to_string(message)?.to_string();
    for line in message.lines() {
        println!("LOGGER: {line}");
    }
    Ok(ECMAScriptValue::Undefined)
}
fn testrunner_helper() -> Object {
    let two62 = ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)));
    let global = get_global_object().unwrap();
    macro_rules! data {
        ( $name:expr, $value:expr, $writable:expr, $enumerable:expr, $configurable:expr ) => {
            define_property_or_throw(
                &two62,
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
    data!("global", global, true, true, true);
    let createrealm = create_builtin_function(
        Box::new(testrunner_createrealm),
        None,
        f64::from(0),
        PropertyKey::from("createRealm"),
        BUILTIN_FUNCTION_SLOTS,
        Some(current_realm_record().expect("realm should exist by now")),
        Some(intrinsic(IntrinsicId::FunctionPrototype)),
        None,
    );
    data!("createRealm", createrealm, true, false, true);

    two62
}

fn testrunner_createrealm(
    _this: &ECMAScriptValue,
    _nt: Option<&Object>,
    _args: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    initialize_host_defined_realm(9939, true);
    let global = get_global_object().unwrap();
    let twosixtytwo = global.get(&PropertyKey::from("$262"))?;
    pop_execution_context();
    Ok(twosixtytwo)
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
        let result = execute_synchronously(text);

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
    });
}

pub fn prepare_for_execution(index: usize, chunk: Rc<Chunk>) {
    AGENT.with(|agent| {
        let mut execution_context_stack = agent.execution_context_stack.borrow_mut();
        execution_context_stack[index].chunk = Some(chunk);
        execution_context_stack[index].pc = 0;
    });
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Error)]
pub enum InternalRuntimeError {
    #[error("no active execution context")]
    NoContext,
    #[error("runtime stack is empty")]
    EmptyStack,
    #[error("non-error completion expected")]
    NonErrorExpected,
    #[error("string value expected")]
    StringExpected,
    #[error("number value expected")]
    NumberExpected,
    #[error("value expected")]
    ValueExpected,
    #[error("code terminated early")]
    CodeEndedEarly,
    #[error("string index out of bounds")]
    StringIndexOOB,
    #[error("string set index out of bounds")]
    StringSetIndexOOB,
    #[error("float index out of bounds")]
    FloatIndexOOB,
    #[error("bigint index out of bounds")]
    BigintIndexOOB,
    #[error("stashed function data index out of bounds")]
    SFDIndexOOB,
    #[error("no current lexical environment")]
    NoLexicalEnvironment,
    #[error("no current variable environment")]
    NoVarEnvironment,
    #[error("binding already exists")]
    BindingAlreadyExists,
    #[error("an environment's HasBinding method failed")]
    HasBindingFailure,
    #[error("an environment's CreateMutableBinding method failed")]
    CreateMutableBindingFailure,
    #[error("an environment's InitializeBinding method failed")]
    InitializeBindingFailure,
    #[error("an environment's GetBindingValue method failed")]
    GetBindingValueFailure,
    #[error("an environment's SetMutableBinding method failed")]
    SetMutableBindingFailure,
    #[error("an object's CreateDataProperty method failed")]
    ObjectCDPFailure,
    #[error("an object's SetPrototypeOf method failed")]
    SetPrototypeOfFailure,
    #[error("throw completion expected")]
    ThrowExpected,
    #[error("function object expected")]
    FunctionExpected,
    #[error("value or reference expected")]
    ValueOrReferenceExpected,
    #[error("reference must be resolvable")]
    UnresolvableReference,
    #[error("arguments object expected")]
    ArgumentsObjectExpected,
    #[error("mapped arguments object expected")]
    MappedArgumentsObjectExpected,
    #[error("no active private environment")]
    NoPrivateEnv,
    #[error("private name missing from private environment")]
    MissingPrivateName,
    #[error("method definition expected")]
    ExpectedMethod,
    #[error("getter method production expected")]
    GetterMethodExpected,
    #[error("setter method production expected")]
    SetterMethodExpected,
    #[error("generator expected")]
    GeneratorExpected,
    #[error("Normal, Throw, or Return completion expected")]
    OkThrowOrReturnExpected,
    #[error("parent expected for current lexical environment")]
    NoParentLexicalEnvironment,
    #[error("PrivateElement accessor expected")]
    AccessorElementExpected,
    #[error("PrivateElement improperly constructed")]
    ImproperPrivateElement,
    #[error("function environment expected")]
    FunctionEnvironmentExpected,
    #[error("[[GetPrototypeOf]] failed when the spec says it shouldn't")]
    GetPrototypeOfFailed,
    #[error("String, Symbol, or PrivateName expected")]
    ClassNameExpected,
    #[error("current environment has no super")]
    MissingSuperEnvironnment,
    #[error("property key (string or symbol) expected")]
    ImproperPropertyKey,
    #[error("Code point value out of range")]
    CodePointOutOfRange,
    #[error("unknown time zone id")]
    UnknownTimeZoneIdentifier,
    #[error("time value out of range")]
    TimeValueOutOfRange,
}
mod insn_impl {
    use super::*;

    const PUSHABLE: &str = "If we could pop a value, we should be able to push a value";
    const POPPABLE: &str = "Previously pushed values should be present";

    fn push_completion(cmpl: FullCompletion) -> anyhow::Result<()> {
        // Input: nothing
        // Output: Completion on the stack
        AGENT.with(|agent| -> anyhow::Result<()> {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            ec.stack.push(cmpl);
            Ok(())
        })
    }
    fn push_value(val: ECMAScriptValue) -> anyhow::Result<()> {
        // Input: nothing
        // Output: ECMA value on the stack
        push_completion(Ok(val.into()))
    }
    fn peek_completion(from_end: usize) -> anyhow::Result<FullCompletion> {
        AGENT.with(|agent| {
            let ec_stack_ref = agent.execution_context_stack.borrow();
            let ec = ec_stack_ref.last().ok_or(InternalRuntimeError::NoContext)?;
            let stack_len = ec.stack.len();
            if stack_len < from_end + 1 {
                Err(InternalRuntimeError::EmptyStack)?;
            }
            let spot = stack_len - 1 - from_end;
            let completion = &ec.stack[spot];
            Ok(completion.clone())
        })
    }
    fn peek_value(from_end: usize) -> anyhow::Result<ECMAScriptValue> {
        ECMAScriptValue::try_from(peek_completion(from_end)?.or(Err(InternalRuntimeError::NonErrorExpected))?)
    }
    fn peek_obj(from_end: usize) -> anyhow::Result<Object> {
        Object::try_from(peek_value(from_end)?)
    }
    fn peek_function_name(from_end: usize) -> anyhow::Result<FunctionName> {
        FunctionName::try_from(peek_completion(from_end)?.or(Err(InternalRuntimeError::NonErrorExpected))?)
    }
    fn peek_usize(from_end: usize) -> anyhow::Result<usize> {
        let value = peek_value(from_end)?;
        let number = f64::try_from(value)?;
        let integer = to_usize(number)?;
        Ok(integer)
    }
    fn peek_list(from_end: usize, length: usize) -> anyhow::Result<Vec<FullCompletion>> {
        AGENT.with(|agent| {
            let ec_stack_ref = agent.execution_context_stack.borrow();
            let ec = ec_stack_ref.last().ok_or(InternalRuntimeError::NoContext)?;

            let stack_len = ec.stack.len();
            if stack_len >= from_end + length {
                let starting_index = stack_len - from_end - length;
                let ending_index = starting_index + length;
                Ok(ec.stack[starting_index..ending_index].to_vec())
            } else {
                Err(InternalRuntimeError::EmptyStack.into())
            }
        })
    }
    fn pop_completion() -> anyhow::Result<FullCompletion> {
        AGENT.with(|agent| {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            let completion = ec.stack.pop().ok_or(InternalRuntimeError::EmptyStack)?;
            Ok(completion)
        })
    }
    fn pop_value() -> anyhow::Result<ECMAScriptValue> {
        ECMAScriptValue::try_from(pop_completion()?.or(Err(InternalRuntimeError::NonErrorExpected))?)
    }
    fn pop_string() -> anyhow::Result<JSString> {
        let value = pop_value()?;
        if let ECMAScriptValue::String(s) = value {
            Ok(s)
        } else {
            Err(InternalRuntimeError::StringExpected.into())
        }
    }
    fn pop_usize() -> anyhow::Result<usize> {
        let value = pop_value()?;
        let number = f64::try_from(value)?;
        let integer = to_usize(number)?;
        Ok(integer)
    }
    fn pop_obj() -> anyhow::Result<Object> {
        Object::try_from(pop_value()?)
    }
    fn pop_numeric() -> anyhow::Result<Numeric> {
        Numeric::try_from(pop_value()?)
    }
    fn pop_ir() -> anyhow::Result<Rc<IteratorRecord>> {
        Rc::<IteratorRecord>::try_from(pop_completion()?.or(Err(InternalRuntimeError::NonErrorExpected))?)
    }
    fn pop_classname() -> anyhow::Result<Option<ClassName>> {
        TryFrom::try_from(pop_completion()?.or(Err(InternalRuntimeError::NonErrorExpected))?)
    }
    fn pop_functionname() -> anyhow::Result<FunctionName> {
        FunctionName::try_from(pop_completion()?.or(Err(InternalRuntimeError::NonErrorExpected))?)
    }
    fn pop_key() -> anyhow::Result<PropertyKey> {
        PropertyKey::try_from(pop_value()?)
    }
    //fn pop_ref() -> anyhow::Result<Box<Reference>> {
    //    pop_completion()?.or(Err(InternalRuntimeError::NonErrorExpected))?.try_into()
    //}
    fn pop_key_list() -> anyhow::Result<Vec<PropertyKey>> {
        let list_len = pop_usize()?;
        AGENT.with(|agent| {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            let stack_size = ec.stack.len();
            if stack_size < list_len {
                Err(InternalRuntimeError::EmptyStack)?;
            }
            ec.stack
                .drain(stack_size - list_len..stack_size)
                .rev()
                .map(|fc| {
                    let nc = fc.or(Err(InternalRuntimeError::NonErrorExpected))?;
                    let val = ECMAScriptValue::try_from(nc)?;
                    PropertyKey::try_from(val)
                })
                .collect::<Result<Vec<_>, _>>()
        })
    }
    fn usize_at(from_end: usize) -> anyhow::Result<usize> {
        AGENT.with(|agent| {
            let ec_stack_ref = agent.execution_context_stack.borrow();
            let ec = ec_stack_ref.last().ok_or(InternalRuntimeError::NoContext)?;
            let stack_size = ec.stack.len();
            if stack_size < 1 + from_end {
                Err(InternalRuntimeError::EmptyStack.into())
            } else {
                let fc = ec.stack[stack_size - 1 - from_end].clone();
                let nc = fc.or(Err(InternalRuntimeError::NonErrorExpected))?;
                let value = ECMAScriptValue::try_from(nc)?;
                let number = f64::try_from(value)?;
                let length = to_usize(number)?;
                Ok(length)
            }
        })
    }
    fn operand(chunk: &Rc<Chunk>) -> anyhow::Result<u16> {
        AGENT.with(|agent| {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec: &mut ExecutionContext = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            let operand = chunk.opcodes.get(ec.pc).ok_or(InternalRuntimeError::CodeEndedEarly)?;
            ec.pc += 1;
            Ok(*operand)
        })
    }
    fn string_operand(chunk: &Rc<Chunk>) -> anyhow::Result<&JSString> {
        let string_index = operand(chunk)?;
        let string = chunk.strings.get(string_index as usize).ok_or(InternalRuntimeError::StringIndexOOB)?;
        Ok(string)
    }
    fn string_set_operand(chunk: &Rc<Chunk>) -> anyhow::Result<&AHashSet<JSString>> {
        let set_index = operand(chunk)?;
        let string_set = chunk.string_sets.get(set_index as usize).ok_or(InternalRuntimeError::StringSetIndexOOB)?;
        Ok(string_set)
    }
    fn usize_operand(chunk: &Rc<Chunk>) -> anyhow::Result<usize> {
        operand(chunk).map(|short| short as usize)
    }
    fn sfd_operand(chunk: &Rc<Chunk>) -> anyhow::Result<&StashedFunctionData> {
        let sfd_index = usize_operand(chunk)?;
        let sfd = chunk.function_object_data.get(sfd_index).ok_or(InternalRuntimeError::SFDIndexOOB)?;
        Ok(sfd)
    }
    fn bump_pc(jump: i16) -> anyhow::Result<()> {
        AGENT.with(|agent| {
            let mut ec_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            if jump >= 0 {
                let offset: usize = usize::try_from(jump).expect("jump is positive");
                if ec.pc + offset > ec.chunk.as_ref().map_or(0, |c| c.opcodes.len()) {
                    return Err(InternalRuntimeError::CodeEndedEarly.into());
                }
                ec.pc += offset;
            } else {
                let offset: usize = usize::try_from(-jump).expect("jump is negative");
                if offset > ec.pc {
                    return Err(InternalRuntimeError::CodeEndedEarly.into());
                }
                ec.pc -= offset;
            }
            Ok(())
        })
    }
    pub fn nop() {
        // surprise. Do Nothing.
    }
    pub fn todo() -> ! {
        // emit a todo error
        todo!()
    }
    pub fn pop() -> anyhow::Result<()> {
        // Input:  Stack: item
        // Output: Stack:
        let _ = pop_completion()?;
        Ok(())
    }
    pub fn string(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input: argument: index;
        //        stack: no input
        // Output: ECMA String value added to the stack
        let string = string_operand(chunk)?;
        push_value(ECMAScriptValue::String(string.clone()))
    }
    pub fn float(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input: argument: index;
        //        stack: no input
        // Output: ECMA Number value added to the stack
        let float_index = operand(chunk)?;
        let float = chunk.floats.get(float_index as usize).ok_or(InternalRuntimeError::FloatIndexOOB)?;
        push_value(ECMAScriptValue::Number(*float))
    }
    pub fn bigint(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input: argument: index;
        //        stack: no input
        // Output: ECMA BigInt value added to the stack
        let bigint_index = operand(chunk)?;
        let bigint = chunk.bigints.get(bigint_index as usize).ok_or(InternalRuntimeError::BigintIndexOOB)?;
        push_value(ECMAScriptValue::BigInt(bigint.clone()))
    }
    pub fn null() -> anyhow::Result<()> {
        // Input: nothing
        // Output: ECMA value NULL on the stack
        push_value(ECMAScriptValue::Null)
    }
    pub fn true_val() -> anyhow::Result<()> {
        // Input: Nothing
        // Output: ECMA value TRUE on the stack
        push_value(ECMAScriptValue::Boolean(true))
    }
    pub fn false_val() -> anyhow::Result<()> {
        // Input: Nothing
        // Output: ECMA value FALSE on the stack
        push_value(ECMAScriptValue::Boolean(false))
    }
    pub fn zero() -> anyhow::Result<()> {
        // Input: Nothing
        // Output: ECMA value 0.0 on the stack
        push_value(ECMAScriptValue::Number(0.0))
    }
    pub fn empty() -> anyhow::Result<()> {
        // Input: Nothing
        // Output: [ empty ] on the stack
        push_completion(Ok(NormalCompletion::Empty))
    }
    pub fn empty_if_not_error() -> anyhow::Result<()> {
        // Input: Stack one item.
        // Output: On stack: item if item is an error, else [ empty ]
        let completion = pop_completion()?;
        push_completion(match completion {
            Ok(_) => Ok(NormalCompletion::Empty),
            Err(_) => completion,
        })
    }
    pub fn undefined() -> anyhow::Result<()> {
        // Input: Nothing
        // Output: Undefined on the stack
        push_value(ECMAScriptValue::Undefined)
    }
    pub fn function_prototype() -> anyhow::Result<()> {
        // Input: None
        // Output: Stack: Current realm's %Function.prototype%
        push_value(ECMAScriptValue::Object(intrinsic(IntrinsicId::FunctionPrototype)))
    }
    pub fn object_prototype() -> anyhow::Result<()> {
        // Input: None
        // Output:: Stack: Current realm's %Object.prototype%
        push_value(ECMAScriptValue::Object(intrinsic(IntrinsicId::ObjectPrototype)))
    }
    pub fn this() -> anyhow::Result<()> {
        // Input: Nothing
        // Output: the result of ResolveThisBinding on the stack.
        let this_resolved = resolve_this_binding().map(NormalCompletion::from);
        push_completion(this_resolved)
    }
    pub fn resolve(strict: bool) -> anyhow::Result<()> {
        // Input: A string item on the stack
        // Output: A resolved binding for that string on the stack
        let name = pop_string()?;
        let resolved = resolve_binding(&name, None, strict);
        push_completion(resolved).expect(PUSHABLE);
        Ok(())
    }
    pub fn get_value() -> anyhow::Result<()> {
        // Input:  Stack: A reference, value, or error.
        // Output: Stack: The error, the value, or a value from dereferencing the reference
        let reference = pop_completion()?;
        let value = super::get_value(reference);
        push_completion(value.map(NormalCompletion::from)).expect(PUSHABLE);
        Ok(())
    }
    pub fn put_value() -> anyhow::Result<()> {
        // Input: Stack, 2 args: the value to store, then the reference to store it in.
        // Output Stack: [empty] or error
        let wc = pop_completion()?;
        let w = match wc.map(ECMAScriptValue::try_from) {
            Ok(Err(err)) => Err(err),
            Ok(Ok(val)) => Ok(Ok(val)),
            Err(err) => Ok(Err(err)),
        }?;
        let v = pop_completion()?;
        let result = super::put_value(v, w).map(NormalCompletion::from);
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    #[expect(clippy::cast_possible_wrap)]
    pub fn jump_if_abrupt(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input: operand: i16 instruction offset. Stack: any completion (NOT CONSUMED)
        // Output: nothing
        let jump = operand(chunk)? as i16;
        let tos = peek_completion(0)?;
        if tos.is_err() {
            bump_pc(jump)?;
        }
        Ok(())
    }
    #[expect(clippy::cast_possible_wrap)]
    pub fn jump_if_normal(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input: operand: i16 instruction offset. Stack: any completion (NOT CONSUMED)
        // Output: nothing
        let jump = operand(chunk)? as i16;
        let tos = peek_completion(0)?;
        if tos.is_ok() {
            bump_pc(jump)?;
        }
        Ok(())
    }
    #[expect(clippy::cast_possible_wrap)]
    pub fn jump_referencing_bool(bool_match: bool, chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input: operand: i16 instruction offset. Stack: any value (NOT CONSUMED)
        // Output: nothing
        let jump = operand(chunk)? as i16;
        let tos = peek_value(0)?;
        let bool_val = bool::from(tos);
        if bool_match == bool_val {
            bump_pc(jump)?;
        }
        Ok(())
    }
    #[expect(clippy::cast_possible_wrap)]
    pub fn jump_taking_bool(bool_match: bool, chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input: operand: i16 instruction offset. Stack: any value
        // Output: nothing
        let jump = operand(chunk)? as i16;
        let tos = pop_value()?;
        let bool_val = bool::from(tos);
        if bool_match == bool_val {
            bump_pc(jump)?;
        }
        Ok(())
    }
    #[expect(clippy::cast_possible_wrap)]
    pub fn jump_nullishness(nullish: bool, chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input: operand: i16 instruction offset. Stack: any value (NOT CONSUMED)
        // Output: nothing
        let jump = operand(chunk)? as i16;
        let tos = peek_value(0)?;
        let is_nullish = tos == ECMAScriptValue::Undefined || tos == ECMAScriptValue::Null;
        if nullish == is_nullish {
            bump_pc(jump)?;
        }
        Ok(())
    }
    #[expect(clippy::cast_possible_wrap)]
    pub fn jump_undefness(undef: bool, chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input: operand: i16 instruction offset. Stack: any value (NOT CONSUMED)
        // Output: nothing
        let jump = operand(chunk)? as i16;
        let tos = peek_value(0)?;
        let is_undef = tos == ECMAScriptValue::Undefined;
        if undef == is_undef {
            bump_pc(jump)?;
        }
        Ok(())
    }
    #[expect(clippy::cast_possible_wrap)]
    pub fn jump_throwyness(throw: bool, chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input: operand: i16 instruction offset. Stack: any value (NOT CONSUMED)
        // Output: nothing
        let jump = operand(chunk)? as i16;
        let tos = peek_completion(0)?;
        let is_throw = matches!(tos, Err(AbruptCompletion::Throw { .. }));
        if throw == is_throw {
            bump_pc(jump)?;
        }
        Ok(())
    }
    #[expect(clippy::cast_possible_wrap)]
    pub fn jump(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input: operand: i16 instruction offset. Stack: nothing
        // Output: nothing
        let jump = operand(chunk)? as i16;
        bump_pc(jump)
    }
    pub fn update_empty() -> anyhow::Result<()> {
        // Input: on stack: newer (any completion)
        //                  older (any normal completion)
        // Output: on stack: the result of UpdateEmpty
        let newer = pop_completion()?;
        let older = pop_completion()?.or(Err(InternalRuntimeError::NonErrorExpected))?;
        push_completion(super::update_empty(newer, older)).expect(PUSHABLE);
        Ok(())
    }
    pub fn pop2push3() -> anyhow::Result<()> {
        // Input:  Stack: top lower
        // Output: Stack: top lower top
        let top = pop_completion()?;
        let lower = pop_completion()?;
        let bottom = top.clone();
        push_completion(bottom).expect(PUSHABLE);
        push_completion(lower).expect(PUSHABLE);
        push_completion(top).expect(PUSHABLE);
        Ok(())
    }
    pub fn dup() -> anyhow::Result<()> {
        // Input:  Stack: item
        // Output: Stack: item item
        let item = peek_completion(0)?;
        push_completion(item).expect(PUSHABLE);
        Ok(())
    }
    pub fn dup_after_list() -> anyhow::Result<()> {
        // stack has a list followed by a value: N item(n-1) ... item(0) value
        // output dups that value behind the list: N item(n-1) ... item(0) value value
        let list_len = peek_usize(0)?;
        AGENT.with(|agent| -> anyhow::Result<()> {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            let stack = &mut ec.stack;
            let stack_len = stack.len();
            if stack_len < list_len + 2 {
                Err(InternalRuntimeError::EmptyStack)?;
            }
            let value_index = stack_len - (list_len + 2);
            let value = stack[value_index].clone();
            stack.push(value);
            stack[value_index..].rotate_right(1);
            Ok(())
        })
    }
    pub fn swap() -> anyhow::Result<()> {
        // Input:  Stack: A B
        // Output: Stack: B A
        AGENT.with(|agent| -> anyhow::Result<()> {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            let stack_len = ec.stack.len();
            if stack_len < 2 {
                Err(InternalRuntimeError::EmptyStack.into())
            } else {
                ec.stack.swap(stack_len - 1, stack_len - 2);
                Ok(())
            }
        })
    }
    pub fn rotate_up(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // a 1-arg instruction: take the n-th item in the stack and move it to the top, sliding the others
        // down. E.g.: with A, B, C, D on the stack (top on the left), "RotateUp 3" will produce C, A, B, D.
        let amt = operand(chunk)? as usize;
        AGENT.with(|agent| {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            let stack = &mut ec.stack;
            let len = stack.len();
            if len >= amt {
                stack[len - amt..len].rotate_left(1);
                Ok(())
            } else {
                Err(InternalRuntimeError::EmptyStack.into())
            }
        })
    }
    pub fn rotate_down(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // a 1-arg instruction: take the top item of the stack and move it down until it becomes the n-th item in the
        // stack, sliding the others up. E.g.: with A, B, C, D on the stack (top on the left), "RotateDown 3" will
        // produce B, C, A, D.
        let amt = operand(chunk)? as usize;
        AGENT.with(|agent| {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            let stack = &mut ec.stack;
            let len = stack.len();
            if len >= amt {
                stack[len - amt..len].rotate_right(1);
                Ok(())
            } else {
                Err(InternalRuntimeError::EmptyStack.into())
            }
        })
    }
    pub fn rotate_down_list(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // a 1-arg instruction (N)
        // Given a stack, ITEM LIST B C D, move ITEM down past the list plus N more items.
        let amt = operand(chunk)? as usize;
        let list_len = usize_at(1)?;
        AGENT.with(|agent| {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            let stack = &mut ec.stack;
            let len = stack.len();
            if list_len + amt + 2 <= len {
                stack[len - list_len - amt - 2..len].rotate_right(1);
                Ok(())
            } else {
                Err(InternalRuntimeError::EmptyStack.into())
            }
        })
    }
    pub fn rotate_list_down(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input: Operand N
        // Input: Stack: LIST item1 item2 ... itemN
        // Output Stack: item1 item2 ... itemN LIST
        let amt = operand(chunk)? as usize;
        let list_len = usize_at(0)?;
        AGENT.with(|agent| {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            let stack = &mut ec.stack;
            let len = stack.len();
            if list_len + 1 + amt <= len {
                stack[len - list_len - amt - 1..len].rotate_left(amt);
                Ok(())
            } else {
                Err(InternalRuntimeError::EmptyStack.into())
            }
        })
    }
    pub fn rotate_list_up(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input: Operand: N
        // Input: Stack: item1 item2 ... itemN LIST
        // Output Stack: LIST item1 item2 ... itemN
        let amt = operand(chunk)? as usize;
        let list_len = usize_at(amt)?;
        AGENT.with(|agent| {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            let stack = &mut ec.stack;
            let len = stack.len();
            if list_len + 1 + amt <= len {
                stack[len - list_len - amt - 1..len].rotate_right(amt);
                Ok(())
            } else {
                Err(InternalRuntimeError::EmptyStack.into())
            }
        })
    }
    pub fn make_ref(strict: bool) -> anyhow::Result<()> {
        // Input:  Stack: name base
        // Output: Stack: reference
        let name = pop_value()?;
        let base = pop_value()?;
        let reference = Reference::new(Base::Value(base), name, strict, None);
        let result = Ok(NormalCompletion::from(reference));
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn make_super_property_ref(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input: operand: 0 == not-strict; 1 == strict
        // Input: Stack: key this
        // Output: Stack: err/ref
        let strict = operand(chunk)? != 0;
        let key = pop_value()?;
        let this = pop_value()?;
        let super_ref = Reference::make_super_property_reference(this, key, strict)?.map(NormalCompletion::from);
        push_completion(super_ref).expect(PUSHABLE);
        Ok(())
    }
    pub fn pop_or_panic() -> anyhow::Result<()> {
        let val = pop_completion()?;
        if let Err(e) = val {
            panic!("Unhandled, unexpected error {e}");
        }
        Ok(())
    }
    pub fn swap_list() -> anyhow::Result<()> {
        // Input: LIST ITEM
        // Output: ITEM LIST
        let list_len = usize_at(0)?;
        let item = AGENT.with(|agent| {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            let stack = &mut ec.stack;
            let stack_size = stack.len();
            if stack_size < list_len + 2 {
                return Err(InternalRuntimeError::EmptyStack);
            }
            let item = stack.remove(stack_size - list_len - 2);
            Ok(item)
        })?;
        push_completion(item).expect(PUSHABLE);
        Ok(())
    }
    pub fn pop_list() -> anyhow::Result<()> {
        // Input: LIST
        // Output:
        let length = pop_usize()?;
        AGENT.with(|agent| {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            let stack = &mut ec.stack;
            let stack_size = stack.len();
            if stack_size >= length {
                stack.truncate(stack_size - length);
                Ok(())
            } else {
                Err(InternalRuntimeError::EmptyStack.into())
            }
        })
    }
    pub fn pop_out_list(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input: operand: "N"
        // Input:  Stack: item1 ... item(n-1) LIST
        // Output: Stack: item1 ... item(n-1)
        let skip = operand(chunk)? as usize;
        let list_len = usize_at(skip)?;
        AGENT.with(|agent| {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            let stack = &mut ec.stack;
            let stack_size = stack.len();
            if stack_size < list_len + skip + 1 {
                Err(InternalRuntimeError::EmptyStack.into())
            } else {
                //   SL-1   SL-2   SL-3  SL-4   SL-5   SL-6
                //   item1  item2   3    listx  listy  listz
                //   skip = 2;
                //   list_len = 3;
                stack[stack_size - list_len - skip - 1..stack_size].rotate_left(list_len + 1);
                // truncate is: stack_size - list_len - 1
                stack.truncate(stack_size - list_len - 1);
                Ok(())
            }
        })
    }
    pub fn swap_deep_list() -> anyhow::Result<()> {
        // Input: LIST_A item LIST_B
        // Output: LIST_A LIST_B item
        let a_len = usize_at(0)?;
        let b_len = usize_at(a_len + 2)?;
        AGENT.with(|agent| {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            let stack = &mut ec.stack;
            let stack_size = stack.len();
            // 3 l2 l1 l0 item 6 n5 n4 n3 n2 n1 n0
            // =>
            // 3 l2 l1 l0 6 n5 n4 n3 n2 n1 n0 item
            // that's a rotate in the (item list_b) sub-slice
            stack[stack_size - a_len - b_len - 3..stack_size - a_len - 1].rotate_right(1);
            Ok(())
        })
    }
    pub fn list_to_array() -> anyhow::Result<()> {
        // On the stack at the start: N list[N-1] ... list[0]
        // Afterward: A
        let array_len = pop_usize()?;
        let elements = AGENT.with(|agent| {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            let stack = &mut ec.stack;
            let stack_size = stack.len();
            stack
                .drain(stack_size - array_len..stack_size)
                .map(|completion| {
                    let nc = completion.or(Err(InternalRuntimeError::NonErrorExpected))?;
                    let value = ECMAScriptValue::try_from(nc)?;
                    Ok(value)
                })
                .collect::<Result<Vec<_>, anyhow::Error>>()
        })?;
        let a = create_array_from_list(&elements);
        push_value(a.into()).expect(PUSHABLE);
        Ok(())
    }
    fn unwind_internal(amt: usize) -> anyhow::Result<()> {
        AGENT.with(|agent| {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            let stack = &mut ec.stack;
            let stack_size = stack.len();
            if stack_size <= amt {
                Err(InternalRuntimeError::EmptyStack)?;
            }
            if amt > 0 {
                let old_index_of_err = stack_size - 1;
                let new_index_of_err = old_index_of_err - amt;
                stack.swap(new_index_of_err, old_index_of_err);
                stack.truncate(new_index_of_err + 1);
            }
            Ok(())
        })
    }
    pub fn unwind(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        let vals_to_remove = usize_operand(chunk)?;
        unwind_internal(vals_to_remove)
    }
    pub fn unwind_if_abrupt(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        let vals_to_remove = usize_operand(chunk)?;
        let top_completion = peek_completion(0)?;
        if top_completion.is_err() && vals_to_remove > 0 {
            unwind_internal(vals_to_remove)
        } else {
            Ok(())
        }
    }
    pub fn unwind_list() -> anyhow::Result<()> {
        let err_to_keep = pop_completion()?;
        let vals_to_remove = pop_usize()?;
        if vals_to_remove > 0 {
            AGENT.with(|agent| -> Result<_, InternalRuntimeError> {
                let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
                let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
                let stack = &mut ec.stack;
                let old_stack_size = stack.len();
                if vals_to_remove > old_stack_size {
                    Err(InternalRuntimeError::EmptyStack)?;
                }
                let new_stack_size = old_stack_size - vals_to_remove;
                stack.truncate(new_stack_size);
                Ok(())
            })?;
        }
        push_completion(err_to_keep).expect(PUSHABLE);
        Ok(())
    }
    pub fn initialize_referenced_binding() -> anyhow::Result<()> {
        // Input:  Stack: (value to store) (place to store)
        // Output: Stack: [empty] or error
        let value = match pop_completion().context("InitializeReferencedBinding takes two arguments (got none)")?.map(ECMAScriptValue::try_from) {
            Ok(Ok(item)) => Ok(Ok(item)),
            Ok(Err(err)) => Err(err),
            Err(ac) => Ok(Err(ac)),
        }.context("the first argument of InitializeReferencedBinding must be a ECMAScript language value or an Abrupt Completion")?;
        let lhs = pop_completion().context("InitializeReferencedBinding takes two arguments (got only one)")?;
        let result = super::initialize_referenced_binding(lhs, value).map(NormalCompletion::from);
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn push_new_lexical_environment() {
        let current_env = current_lexical_environment();
        let new_env = DeclarativeEnvironmentRecord::new(current_env, "inner block");
        set_lexical_environment(Some(Rc::new(new_env)));
    }
    pub fn pop_lexical_environment() -> anyhow::Result<()> {
        let current_env = current_lexical_environment().ok_or(InternalRuntimeError::NoLexicalEnvironment)?;
        let outer_env = current_env.get_outer_env();
        set_lexical_environment(outer_env);
        Ok(())
    }
    pub fn push_new_var_env_from_lex() {
        let current_env = current_lexical_environment();
        let new_env = DeclarativeEnvironmentRecord::new(current_env, "new var env");
        set_variable_environment(Some(Rc::new(new_env)));
    }
    pub fn push_new_lex_env_from_var() {
        let current_env = current_variable_environment();
        let new_env = DeclarativeEnvironmentRecord::new(current_env, "new var env");
        set_lexical_environment(Some(Rc::new(new_env)));
    }
    pub fn set_lex_env_to_var_env() {
        let current_env = current_variable_environment();
        set_lexical_environment(current_env);
    }
    pub fn set_aside_lex_env() -> anyhow::Result<()> {
        // Input: None
        // Output on Stack: the top lexical environment (which is then popped)
        let current_env = current_lexical_environment().ok_or(InternalRuntimeError::NoLexicalEnvironment)?;
        let outer_env = current_env.get_outer_env().ok_or(InternalRuntimeError::NoParentLexicalEnvironment)?;
        set_lexical_environment(Some(outer_env));
        push_completion(Ok(NormalCompletion::Environment(current_env)))
    }
    pub fn restore_lex_env() -> anyhow::Result<()> {
        // Input on stack: Env
        // Output: None
        let completion = pop_completion()?.or(Err(InternalRuntimeError::NonErrorExpected))?;
        let env: Rc<dyn EnvironmentRecord> = completion.try_into()?;
        set_lexical_environment(Some(env));
        Ok(())
    }
    pub fn push_new_private_env() {
        // Input: None; Output: None
        let outer_private_environment = current_private_environment();
        let new_private_environment = PrivateEnvironmentRecord::new(outer_private_environment);
        set_private_environment(Some(Rc::new(RefCell::new(new_private_environment))));
    }
    pub fn pop_private_env() -> anyhow::Result<()> {
        // Input: None; Output: None
        let current_private_environment = current_private_environment().ok_or(InternalRuntimeError::NoPrivateEnv)?;
        let parent = current_private_environment.borrow().outer_private_environment.clone();
        set_private_environment(parent);
        Ok(())
    }
    pub fn create_immutable_lex_binding(strict: bool, chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        let env = current_lexical_environment().ok_or(InternalRuntimeError::NoLexicalEnvironment)?;
        let name = string_operand(chunk)?;

        env.create_immutable_binding(name.clone(), strict).or(Err(InternalRuntimeError::BindingAlreadyExists))?;
        Ok(())
    }
    fn create_mutable_binding(
        env: &Rc<dyn EnvironmentRecord>,
        deletable: bool,
        chunk: &Rc<Chunk>,
    ) -> anyhow::Result<()> {
        let name = string_operand(chunk)?;
        env.create_mutable_binding(name.clone(), deletable).or(Err(InternalRuntimeError::BindingAlreadyExists))?;
        Ok(())
    }
    pub fn create_mutable_lexical_binding(deletable: bool, chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        let env = current_lexical_environment().ok_or(InternalRuntimeError::NoLexicalEnvironment)?;
        create_mutable_binding(&env, deletable, chunk)
    }
    pub fn create_mutable_var_binding(deletable: bool, chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        let env = current_variable_environment().ok_or(InternalRuntimeError::NoVarEnvironment)?;
        create_mutable_binding(&env, deletable, chunk)
    }
    pub fn create_mutable_lexical_binding_if_missing(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        //  1. Let alreadyDeclared be ! env.HasBinding(paramName).
        //  2. If alreadyDeclared is false, then
        //        a. Perform ! env.CreateMutableBinding(paramName, false).
        let env = current_lexical_environment().ok_or(InternalRuntimeError::NoLexicalEnvironment)?;
        let name = string_operand(chunk)?;

        let already_declared = env.has_binding(name).or(Err(InternalRuntimeError::HasBindingFailure))?;
        if !already_declared {
            env.create_mutable_binding(name.clone(), false)
                .or(Err(InternalRuntimeError::CreateMutableBindingFailure))?;
        }
        Ok(())
    }
    pub fn create_initialized_permanent_mutable_lexical_binding_if_missing(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        //  1. Let alreadyDeclared be ! env.HasBinding(paramName).
        //  2. If alreadyDeclared is false, then
        //      a. Perform ! env.CreateMutableBinding(paramName, false).
        //      b. Perform ! env.InitializeBinding(paramName, undefined).
        let env = current_lexical_environment().ok_or(InternalRuntimeError::NoLexicalEnvironment)?;
        let name = string_operand(chunk)?;

        let already_declared = env.has_binding(name).or(Err(InternalRuntimeError::HasBindingFailure))?;
        if !already_declared {
            env.create_mutable_binding(name.clone(), false)
                .or(Err(InternalRuntimeError::CreateMutableBindingFailure))?;
            env.initialize_binding(name, ECMAScriptValue::Undefined)
                .or(Err(InternalRuntimeError::InitializeBindingFailure))?;
        }
        Ok(())
    }
    fn initialize_binding(env: &Rc<dyn EnvironmentRecord>, chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        let name = string_operand(chunk)?;
        let value = pop_value()?;
        env.initialize_binding(name, value).or(Err(InternalRuntimeError::InitializeBindingFailure))?;
        Ok(())
    }
    pub fn initialize_lex_binding(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        let env = current_lexical_environment().ok_or(InternalRuntimeError::NoLexicalEnvironment)?;
        initialize_binding(&env, chunk)
    }
    pub fn initialize_var_binding(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        let env = current_variable_environment().ok_or(InternalRuntimeError::NoVarEnvironment)?;
        initialize_binding(&env, chunk)
    }
    pub fn get_lex_binding(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        let env = current_lexical_environment().ok_or(InternalRuntimeError::NoLexicalEnvironment)?;
        let name = string_operand(chunk)?;
        let value = env.get_binding_value(name, false).or(Err(InternalRuntimeError::GetBindingValueFailure))?;
        push_value(value)
    }
    pub fn set_mutable_var_binding(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        let env = current_variable_environment().ok_or(InternalRuntimeError::NoVarEnvironment)?;
        let name = string_operand(chunk)?;
        let value = pop_value()?;
        env.set_mutable_binding(name.clone(), value, false).or(Err(InternalRuntimeError::SetMutableBindingFailure))?;
        Ok(())
    }
    pub fn create_private_name_if_missing(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input operand: string Index
        let name = string_operand(chunk)?;
        let pe_holder = current_private_environment().ok_or(InternalRuntimeError::NoPrivateEnv)?;
        let mut pe = pe_holder.borrow_mut();
        if !pe.names.iter().map(|pn| &pn.description).any(|description| description == name) {
            let pn = PrivateName::new(name.clone());
            pe.names.push(pn);
        }
        Ok(())
    }
    pub fn create_per_iteration_environment(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // A 1-operand instruction that takes nothing from the stack as input, and produces one
        // stack item ([empty]/error) on exit.
        let string_set = string_set_operand(chunk)?;
        let res = super::create_per_iteration_environment(string_set).map(NormalCompletion::from);
        push_completion(res)?;
        Ok(())
    }
    pub fn extract_thrown_value() -> anyhow::Result<()> {
        let item = pop_completion()?;
        match item {
            Err(AbruptCompletion::Throw { value }) => {
                push_value(value).expect(PUSHABLE);
            }
            _ => Err(InternalRuntimeError::ThrowExpected)?,
        }
        Ok(())
    }
    pub fn extract_arg() -> anyhow::Result<()> {
        // Stack: N arg[N-1] arg[N-2] ... arg[1] arg[0] (when N >= 1)
        // Out: arg[0] N-1 arg[N-1] arg[N-2] ... arg[1]
        //   --or, if N == 0 --
        // Stack: 0
        // Out: Undefined 0
        let arg_count = usize_at(0)?;
        if arg_count == 0 {
            push_value(ECMAScriptValue::Undefined).expect(PUSHABLE);
            Ok(())
        } else {
            AGENT.with(|agent| {
                let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
                let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
                let stack = &mut ec.stack;
                let stack_len = stack.len();
                if stack_len <= arg_count {
                    Err(InternalRuntimeError::EmptyStack)?;
                } else {
                    let arg0 = stack.remove(stack_len - arg_count - 1);
                    stack[stack_len - 2] = Ok(NormalCompletion::from(arg_count - 1));
                    stack.push(arg0);
                }
                Ok(())
            })
        }
    }
    pub fn finish_args() -> anyhow::Result<()> {
        // Stack: N arg[N-1] ... arg[0]
        // Out:
        // Remove any remaining arguments from the stack (we're at zero, or the caller gave us too much)
        let arg_count = usize_at(0)?;
        AGENT.with(|agent| {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            let stack = &mut ec.stack;
            let stack_len = stack.len();
            if stack_len <= arg_count {
                Err(InternalRuntimeError::EmptyStack)?;
            }
            let to_retain = stack_len - arg_count - 1;
            stack.truncate(to_retain);
            Ok(())
        })
    }
    pub fn object() -> anyhow::Result<()> {
        let obj_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let o = ordinary_object_create(Some(obj_proto));
        push_value(ECMAScriptValue::Object(o))
    }
    pub fn object_with_proto() -> anyhow::Result<()> {
        // Input on stack: proto
        // Output on stack: object
        let proto: Option<Object> = pop_value()?.try_into()?;
        let o = ordinary_object_create(proto);
        push_value(ECMAScriptValue::Object(o))
    }
    pub fn array() -> anyhow::Result<()> {
        let array = array_create(0, None).expect("Arrays of length zero are not too large");
        push_value(ECMAScriptValue::Object(array))
    }
    pub fn create_data_property() -> anyhow::Result<()> {
        let value = pop_value()?;
        let nc_name = pop_completion()?.or(Err(InternalRuntimeError::NonErrorExpected))?;
        let nc_obj = pop_completion()?.or(Err(InternalRuntimeError::NonErrorExpected))?;
        let obj = Object::try_from(nc_obj)?;
        let name = PropertyKey::try_from(nc_name)?;
        obj.create_data_property_or_throw(name, value).or(Err(InternalRuntimeError::ObjectCDPFailure))?;
        push_value(ECMAScriptValue::Object(obj)).expect(PUSHABLE);
        Ok(())
    }
    pub fn set_prototype() -> anyhow::Result<()> {
        let value = pop_value()?;
        let nc_obj = pop_completion()?.or(Err(InternalRuntimeError::NonErrorExpected))?;
        let obj = Object::try_from(nc_obj)?;
        let val_obj_res: anyhow::Result<Option<Object>> = value.try_into();
        if let Ok(new_proto) = val_obj_res {
            obj.o.set_prototype_of(new_proto).or(Err(InternalRuntimeError::SetPrototypeOfFailure))?;
        }
        push_value(ECMAScriptValue::Object(obj)).expect(PUSHABLE);
        Ok(())
    }
    pub fn to_property_key() -> anyhow::Result<()> {
        let value_name = pop_value()?;
        let key = super::to_property_key(value_name);
        let fc = key.map(|pk| NormalCompletion::from(ECMAScriptValue::from(pk)));
        push_completion(fc).expect(PUSHABLE);
        Ok(())
    }
    pub fn copy_data_props(exclusions: &[PropertyKey]) -> anyhow::Result<()> {
        let value = pop_value()?;
        let obj = pop_obj()?;
        let fc = obj.copy_data_properties(value, exclusions).map(|()| NormalCompletion::from(obj));
        push_completion(fc).expect(PUSHABLE);
        Ok(())
    }
    pub fn copy_data_props_with_exclusions() -> anyhow::Result<()> {
        let exclusions = pop_key_list()?;
        copy_data_props(&exclusions)
    }
    pub fn to_string() -> anyhow::Result<()> {
        let val = pop_value()?;
        let result = super::to_string(val).map(NormalCompletion::from);
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn to_numeric() -> anyhow::Result<()> {
        let val = pop_value()?;
        let result = super::to_numeric(val).map(NormalCompletion::from);
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn to_object() -> anyhow::Result<()> {
        let val = pop_value()?;
        let result = super::to_object(val).map(NormalCompletion::from);
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn increment() -> anyhow::Result<()> {
        let num = pop_numeric()?;
        let result_val = match num {
            Numeric::Number(n) => ECMAScriptValue::Number(n + 1.0),
            Numeric::BigInt(bi) => ECMAScriptValue::BigInt(Rc::new(&*bi + 1)),
        };
        push_value(result_val).expect(PUSHABLE);
        Ok(())
    }
    pub fn decrement() -> anyhow::Result<()> {
        let num = pop_numeric()?;
        let result_val = match num {
            Numeric::Number(n) => ECMAScriptValue::Number(n - 1.0),
            Numeric::BigInt(bi) => ECMAScriptValue::BigInt(Rc::new(&*bi - 1)),
        };
        push_value(result_val).expect(PUSHABLE);
        Ok(())
    }
    pub fn pre_increment() -> anyhow::Result<()> {
        let fc = pop_completion()?;
        let result = prefix_increment(fc);
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn pre_decrement() -> anyhow::Result<()> {
        let fc = pop_completion()?;
        let result = prefix_decrement(fc);
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn delete() -> anyhow::Result<()> {
        let fc = pop_completion()?;
        let result = delete_ref(fc);
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn void() -> anyhow::Result<()> {
        let fc = pop_completion()?;
        let result = void_operator(fc);
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn type_of() -> anyhow::Result<()> {
        let fc = pop_completion()?;
        let result = typeof_operator(fc);
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn append_list() -> anyhow::Result<()> {
        // stack has 2 lists (N itemA(n-1) itemA(n-2) ... itemA(0)) (M itemB(m-1) ... itemB(0))
        // This routine combines them into (N+M itemA(n-1) ... itemA(0) itemB(m-1) ... itemB(0))
        // call them ListA and ListB ...
        let len_a = pop_usize()?;
        AGENT.with(|agent| -> Result<(), InternalRuntimeError> {
            let mut ec_stack_ref = agent.execution_context_stack.borrow_mut();
            let ec = ec_stack_ref.last_mut().ok_or(InternalRuntimeError::NoContext)?;
            let stack = &mut ec.stack;
            let len = stack.len();
            if len < len_a + 1 {
                Err(InternalRuntimeError::EmptyStack)
            } else {
                stack[len - (len_a + 1)..len].rotate_left(1);
                Ok(())
            }
        })?;
        let len_b = pop_usize()?;
        let new_len = len_a + len_b;
        assert!(new_len < 1 << 53);
        #[expect(clippy::cast_precision_loss)]
        push_completion(Ok((new_len as f64).into())).expect(PUSHABLE);
        Ok(())
    }
    pub fn require_constructor() -> anyhow::Result<()> {
        let val = pop_value()?;
        push_completion(if val.is_constructor() {
            Ok(NormalCompletion::Empty)
        } else {
            Err(create_type_error("Constructor required"))
        })
        .expect(PUSHABLE);
        Ok(())
    }
    pub fn call(strict: bool) -> anyhow::Result<()> {
        let arg_count = pop_usize()?;
        let mut arguments = Vec::with_capacity(arg_count);
        for _ in 1..=arg_count {
            let val = pop_value()?;
            arguments.push(val);
        }
        arguments.reverse();
        let func_val = pop_value()?;
        let ref_nc = pop_completion()?.or(Err(InternalRuntimeError::NonErrorExpected))?;

        let mut was_direct_eval = false;
        if let NormalCompletion::Reference(evalref) = &ref_nc {
            if !evalref.is_property_reference() {
                if let ReferencedName::Value(ECMAScriptValue::String(name)) = &evalref.referenced_name {
                    if name == &JSString::from("eval")
                        && super::to_object(func_val.clone()).unwrap() == intrinsic(IntrinsicId::Eval)
                    {
                        // A direct eval
                        if arg_count == 0 {
                            push_value(ECMAScriptValue::Undefined).expect(PUSHABLE);
                        } else {
                            let result = perform_eval(
                                arguments[0].clone(),
                                if strict {
                                    EvalCallStatus::DirectWithStrictCaller
                                } else {
                                    EvalCallStatus::DirectWithNonStrictCaller
                                },
                            );
                            push_completion(result.map(NormalCompletion::from)).expect(PUSHABLE);
                        }
                        was_direct_eval = true;
                    }
                }
            }
        }

        if !was_direct_eval {
            begin_call_evaluation(&func_val, &ref_nc, &arguments)?;
        }

        Ok(())
    }
    pub fn end_function() -> anyhow::Result<()> {
        let result = pop_completion()?;
        let f_obj = pop_obj()?;
        let callable = f_obj.o.to_callable_obj().ok_or(InternalRuntimeError::FunctionExpected)?;
        callable.end_evaluation(result);
        Ok(())
    }
    pub fn construct() -> anyhow::Result<()> {
        // Stack: N arg[n-1] arg[n-2] ... arg[0] newtgt cstr
        let arg_count = pop_usize()?;
        let mut arguments = Vec::with_capacity(arg_count);
        for _ in 1..=arg_count {
            let val = pop_value()?;
            arguments.push(val);
        }
        arguments.reverse();
        let newtgt = pop_value()?;
        let cstr_val = pop_value()?;

        begin_constructor_evaluation(cstr_val, newtgt, &arguments)?;

        Ok(())
    }
    pub fn return_insn() -> anyhow::Result<()> {
        let value = pop_value()?;
        push_completion(Err(AbruptCompletion::Return { value })).expect(PUSHABLE);
        Ok(())
    }
    pub fn unary_plus() -> anyhow::Result<()> {
        let exp = pop_completion()?;
        let val = super::get_value(exp);
        let result = match val {
            Ok(ev) => ev.to_number().map(NormalCompletion::from),
            Err(ac) => Err(ac),
        };
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn unary_minus() -> anyhow::Result<()> {
        let exp = pop_completion()?;
        let val = super::get_value(exp);
        let old_val = match val {
            Ok(val) => super::to_numeric(val),
            Err(ac) => Err(ac),
        };
        let result = match old_val {
            Err(ac) => Err(ac),
            Ok(Numeric::Number(n)) => Ok(NormalCompletion::from(-n)),
            Ok(Numeric::BigInt(bi)) => Ok(NormalCompletion::from(Rc::new(-&*bi))),
        };
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn unary_complement() -> anyhow::Result<()> {
        let exp = pop_completion()?;
        let val = super::get_value(exp);
        let old_val = match val {
            Ok(val) => super::to_numeric(val),
            Err(ac) => Err(ac),
        };
        let result = match old_val {
            Err(ac) => Err(ac),
            Ok(Numeric::Number(n)) => Ok(NormalCompletion::from(!to_int32_f64(n))),
            Ok(Numeric::BigInt(bi)) => Ok(NormalCompletion::from(Rc::new(!&*bi))),
        };
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn unary_not() -> anyhow::Result<()> {
        let exp = pop_completion()?;
        let val = super::get_value(exp);
        let result = match val {
            Ok(val) => Ok(NormalCompletion::from(!super::to_boolean(val))),
            Err(ac) => Err(ac),
        };
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn binary_operation(op: BinOp) -> anyhow::Result<()> {
        let right = pop_value()?;
        let left = pop_value()?;
        let result = apply_string_or_numeric_binary_operator(left, right, op);
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn instantiate_id_free_function_expression(chunk: &Rc<Chunk>, text: &str) -> anyhow::Result<()> {
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
        let info = sfd_operand(chunk)?;
        let to_compile: Rc<FunctionExpression> = info.to_compile.clone().try_into()?;
        let name = nameify(&info.source_text, 50);
        let mut compiled = Chunk::new(name);
        let compilation_status = to_compile.body.compile_body(&mut compiled, text, info);
        if let Err(err) = compilation_status {
            let typeerror = create_type_error(err.to_string());
            let _ = pop_completion()?;
            push_completion(Err(typeerror)).expect(PUSHABLE);
            return Ok(());
        }
        for line in compiled.disassemble() {
            println!("{line}");
        }

        // Name is on the stack.
        // env/privateenv come from the agent.
        // sourceText is on the stack? Or with the productions?
        // FormalParameters/FunctionBody are... In a registry someplace maybe? With a registry ID in the instruction?
        // strict is ... in that registry? (Needs to be not part of the current chunk).
        let outer_env = current_lexical_environment().ok_or(InternalRuntimeError::NoLexicalEnvironment)?;
        let priv_env = current_private_environment();

        let name = pop_key()?;
        let function_prototype = intrinsic(IntrinsicId::FunctionPrototype);

        let closure = ordinary_function_create(
            function_prototype,
            info.source_text.as_str(),
            info.params.clone(),
            info.body.clone(),
            ThisLexicality::NonLexicalThis,
            outer_env,
            priv_env,
            info.strict,
            Rc::new(compiled),
        );

        super::set_function_name(&closure, name.into(), None);
        super::make_constructor(&closure, None);

        push_value(closure.into()).expect(PUSHABLE);
        Ok(())
    }

    pub fn instantiate_ordinary_function_expression(chunk: &Rc<Chunk>, text: &str) -> anyhow::Result<()> {
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
        let info = sfd_operand(chunk)?;

        // First: compile the function.
        let to_compile: Rc<FunctionExpression> = info.to_compile.clone().try_into()?;
        let chunk_name = nameify(&info.source_text, 50);
        let mut compiled = Chunk::new(chunk_name);
        let compilation_status = to_compile.body.compile_body(&mut compiled, text, info);
        if let Err(err) = compilation_status {
            let _ = pop_completion()?;
            let typeerror = create_type_error(err.to_string());
            push_completion(Err(typeerror)).expect(PUSHABLE);
            return Ok(());
        }
        for line in compiled.disassemble() {
            println!("{line}");
        }

        let name = pop_string()?;

        let outer_env = current_lexical_environment().ok_or(InternalRuntimeError::NoLexicalEnvironment)?;
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

        super::set_function_name(&closure, name.clone().into(), None);
        super::make_constructor(&closure, None);
        func_env.initialize_binding(&name, closure.clone().into()).expect("binding has been created");

        push_value(closure.into()).expect(PUSHABLE);
        Ok(())
    }

    pub fn instantiate_arrow_function_expression(chunk: &Rc<Chunk>, text: &str) -> anyhow::Result<()> {
        let info = sfd_operand(chunk)?;
        let env = current_lexical_environment().unwrap();
        let priv_env = current_private_environment();

        let name_val = pop_value()?;
        let name = FunctionName::try_from(name_val)?;

        let to_compile: Rc<ArrowFunction> = info.to_compile.clone().try_into()?;
        let chunk_name = nameify(&info.source_text, 50);
        let mut compiled = Chunk::new(chunk_name);
        let compilation_status = to_compile.body.compile_body(&mut compiled, text, info);
        if let Err(err) = compilation_status {
            let typeerror = create_type_error(err.to_string());
            let _ = pop_completion()?;
            push_completion(Err(typeerror)).expect(PUSHABLE);
            return Ok(());
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
        super::set_function_name(&closure, name, None);

        push_value(closure.into()).expect(PUSHABLE);
        Ok(())
    }
    pub fn instantiate_ordinary_function_object(chunk: &Rc<Chunk>, text: &str) -> anyhow::Result<()> {
        let name = string_operand(chunk)?;
        let info = sfd_operand(chunk)?;

        let to_compile: Rc<FunctionDeclaration> =
            info.to_compile.clone().try_into().context("finding function compilation source")?;
        let chunk_name = nameify(&info.source_text, 50);
        let mut compiled = Chunk::new(chunk_name);
        let compilation_status = to_compile.body.compile_body(&mut compiled, text, info);
        if let Err(err) = compilation_status {
            let typeerror = create_type_error(err.to_string());
            let _ = pop_completion()?;
            push_completion(Err(typeerror)).expect(PUSHABLE);
            return Ok(());
        }
        for line in compiled.disassemble() {
            println!("{line}");
        }

        let env = current_lexical_environment().ok_or(InternalRuntimeError::NoLexicalEnvironment)?;
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
        super::set_function_name(&closure, name.clone().into(), None);
        super::make_constructor(&closure, None);

        push_value(closure.into()).expect(PUSHABLE);
        Ok(())
    }
    pub fn instantiate_generator_function_object(chunk: &Rc<Chunk>, text: &str) -> anyhow::Result<()> {
        // GeneratorDeclaration : function * BindingIdentifier ( FormalParameters ) { GeneratorBody }
        //  1. Let name be the StringValue of BindingIdentifier.
        //  2. Let sourceText be the source text matched by GeneratorDeclaration.
        //  3. Let F be OrdinaryFunctionCreate(%GeneratorFunction.prototype%, sourceText, FormalParameters,
        //     GeneratorBody, NON-LEXICAL-THIS, env, privateEnv).
        //  4. Perform SetFunctionName(F, name).
        //  5. Let prototype be OrdinaryObjectCreate(%GeneratorFunction.prototype.prototype%).
        //  6. Perform ! DefinePropertyOrThrow(F, "prototype", PropertyDescriptor { [[Value]]: prototype, [[Writable]]:
        //     true, [[Enumerable]]: false, [[Configurable]]: false }).
        //  7. Return F.
        let name = string_operand(chunk)?;
        let info = sfd_operand(chunk)?;

        let to_compile: Rc<GeneratorDeclaration> =
            info.to_compile.clone().try_into().context("finding function compilation source")?;
        let chunk_name = nameify(&info.source_text, 50);
        let mut compiled = Chunk::new(chunk_name);
        let compilation_status = to_compile.body.evaluate_generator_body(&mut compiled, text, info);
        if let Err(err) = compilation_status {
            let typeerror = create_type_error(err.to_string());
            let _ = pop_completion()?;
            push_completion(Err(typeerror)).expect(PUSHABLE);
            return Ok(());
        }
        for line in compiled.disassemble() {
            println!("{line}");
        }

        let env = current_lexical_environment().ok_or(InternalRuntimeError::NoLexicalEnvironment)?;
        let priv_env = current_private_environment();
        let generator_function_prototype = intrinsic(IntrinsicId::GeneratorFunctionPrototype);

        let closure = ordinary_function_create(
            generator_function_prototype,
            info.source_text.as_str(),
            info.params.clone(),
            info.body.clone(),
            ThisLexicality::NonLexicalThis,
            env,
            priv_env,
            info.strict,
            Rc::new(compiled),
        );
        super::set_function_name(&closure, name.clone().into(), None);
        let generator_function_prototype_prototype = intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototype);
        let prototype = ordinary_object_create(Some(generator_function_prototype_prototype));
        define_property_or_throw(
            &closure,
            "prototype",
            PotentialPropertyDescriptor::new().value(prototype).writable(true).enumerable(false).configurable(false),
        )
        .expect("defining properties on an algorithmically generated object should work");
        push_value(closure.into()).expect(PUSHABLE);
        Ok(())
    }
    pub fn instantiate_generator_method(chunk: &Rc<Chunk>, text: &str) -> anyhow::Result<()> {
        // Input: Operand: function chunk id
        // Input: Operand: enumerable boolean
        // Input: Stack: propKey object
        // Output: empty/private_element

        // From MethodDefinitionEvaluation for GeneratorMethod
        //  2. Let env be the running execution context's LexicalEnvironment.
        //  3. Let privateEnv be the running execution context's PrivateEnvironment.
        //  4. Let sourceText be the source text matched by GeneratorMethod.
        //  5. Let closure be OrdinaryFunctionCreate(%GeneratorFunction.prototype%, sourceText, UniqueFormalParameters,
        //     GeneratorBody, non-lexical-this, env, privateEnv).
        //  6. Perform MakeMethod(closure, object).
        //  7. Perform SetFunctionName(closure, propKey).
        //  8. Let prototype be OrdinaryObjectCreate(%GeneratorFunction.prototype.prototype%).
        //  9. Perform ! DefinePropertyOrThrow(closure, "prototype", PropertyDescriptor { [[Value]]: prototype,
        //     [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false }).
        //  10. Return ? DefineMethodProperty(object, propKey, closure, enumerable).

        let info = sfd_operand(chunk)?;
        let enumerable = operand(chunk)? != 0;
        let prop_key = pop_classname()?.ok_or(InternalRuntimeError::ClassNameExpected)?;
        let object = pop_obj()?;

        let to_compile: Rc<GeneratorMethod> =
            info.to_compile.clone().try_into().context("finding generator method compilation source")?;
        let chunk_name = nameify(&info.source_text, 50);
        let mut compiled = Chunk::new(chunk_name);
        let compilation_status = to_compile.body.evaluate_generator_body(&mut compiled, text, info);
        if let Err(err) = compilation_status {
            let typeerror = create_type_error(err.to_string());
            push_completion(Err(typeerror)).expect(PUSHABLE);
            return Ok(());
        }
        for line in compiled.disassemble() {
            println!("{line}");
        }

        let env = current_lexical_environment().ok_or(InternalRuntimeError::NoLexicalEnvironment)?;
        let priv_env = current_private_environment();
        let generator_function_prototype = intrinsic(IntrinsicId::GeneratorFunctionPrototype);

        let closure = ordinary_function_create(
            generator_function_prototype,
            info.source_text.as_str(),
            info.params.clone(),
            info.body.clone(),
            ThisLexicality::NonLexicalThis,
            env,
            priv_env,
            info.strict,
            Rc::new(compiled),
        );

        make_method(
            closure.o.to_function_obj().expect("a closure we just created should be a function object"),
            object.clone(),
        );
        super::set_function_name(&closure, FunctionName::from(prop_key.clone()), None);
        let generator_function_prototype_prototype: Object =
            intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototype);
        let prototype = ordinary_object_create(Some(generator_function_prototype_prototype));
        define_property_or_throw(
            &closure,
            "prototype",
            PotentialPropertyDescriptor::new().value(prototype).writable(true).enumerable(false).configurable(false),
        )
        .expect("defining properties on an algorithmically generated object should work");

        let result = super::define_method_property(&object, FunctionName::from(prop_key), closure, enumerable)
            .map(NormalCompletion::from);

        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn throw() -> anyhow::Result<()> {
        // Convert the NormalCompletion::Value on top of the stack into a ThrowCompletion with a matching value
        let value = pop_value()?;
        push_completion(Err(AbruptCompletion::Throw { value })).expect(PUSHABLE);
        Ok(())
    }
    pub fn compare(swap_args: bool, invert: bool) -> anyhow::Result<()> {
        fn compare_invert(optb: Option<bool>) -> NormalCompletion {
            NormalCompletion::from(!optb.unwrap_or(true))
        }
        fn compare_pass(optb: Option<bool>) -> NormalCompletion {
            NormalCompletion::from(optb.unwrap_or(false))
        }

        let right_operand = pop_value()?;
        let left_operand = pop_value()?;
        let (x, y) = if swap_args { (right_operand, left_operand) } else { (left_operand, right_operand) };
        let handler = if invert { compare_invert } else { compare_pass };
        let result = is_less_than(x, y, !swap_args).map(handler);
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn instance_of() -> anyhow::Result<()> {
        let right = pop_value()?;
        let left = pop_value()?;
        let result = instanceof_operator(left, &right).map(NormalCompletion::from);
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn in_op() -> anyhow::Result<()> {
        let right = pop_value()?;
        let left = pop_value()?;
        let result = match right {
            ECMAScriptValue::Object(obj) => {
                super::to_property_key(left).and_then(|key| super::has_property(&obj, &key)).map(NormalCompletion::from)
            }
            _ => Err(create_type_error("Right-hand side of 'in' must be an object")),
        };
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn equal(invert: bool) -> anyhow::Result<()> {
        let compute = if invert { bool::not } else { identity };
        let right = pop_value()?;
        let left = pop_value()?;
        let result = is_loosely_equal(&left, &right).map(compute).map(NormalCompletion::from);
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn strict_equal(invert: bool) -> anyhow::Result<()> {
        let compute = if invert { bool::not } else { identity };
        let right = pop_value()?;
        let left = pop_value()?;
        let result = Ok(NormalCompletion::from(compute(left.is_strictly_equal(&right))));
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn create_unmapped_arguments_object() -> anyhow::Result<()> {
        // Stack should have n arg[n-1] arg[n-2] ... arg[0] ...
        // Those values are NOT consumed; this function assumes they'll be used again.

        let length = peek_usize(0)?;
        let obj = ArgumentsObject::object(None);
        define_property_or_throw(
            &obj,
            "length",
            PotentialPropertyDescriptor::new().value(length).writable(true).enumerable(false).configurable(true),
        )
        .expect("Normal Object");

        let arguments = peek_list(1, length)?;

        for (arg_number, item) in arguments.into_iter().enumerate() {
            let value = ECMAScriptValue::try_from(item.map_err(|_| InternalRuntimeError::NonErrorExpected)?)?;
            obj.create_data_property_or_throw(arg_number, value).expect("Normal Object");
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

        push_value(obj.into()).expect(PUSHABLE);
        Ok(())
        // Stack at exit: AObj N arg[N-1] ... arg[0] ...
    }
    pub fn create_mapped_arguments_object() -> anyhow::Result<()> {
        // Input: M arg(m-1) ... arg(0) func
        // Output: Obj M arg(m-1) ... arg(0) func
        let length = peek_usize(0)?;
        let arguments = peek_list(1, length)?;

        let env = current_lexical_environment().ok_or(InternalRuntimeError::NoContext)?;
        let map = ParameterMap::new(env);
        let ao = ArgumentsObject::object(Some(map));

        for (idx, item) in arguments.into_iter().enumerate() {
            let val =
                ECMAScriptValue::try_from(item.expect("arguments must be values")).expect("arguments must be values");
            ao.create_data_property_or_throw(idx, val).expect("ArgumentObject won't throw");
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
        let func = peek_value(length + 1)?;
        define_property_or_throw(
            &ao,
            "callee",
            PotentialPropertyDescriptor::new().value(func).writable(true).enumerable(false).configurable(true),
        )
        .expect("ArgumentObject won't throw");

        push_value(ao.into()).expect(PUSHABLE);
        Ok(())
        // Stack at exit: AObj N arg[N-1] ... arg[0] func ...
    }
    pub fn add_mapped_argument(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        let name = string_operand(chunk)?;
        let argument_index = usize_operand(chunk)?;
        // Stack: AObj len ...
        let obj = peek_obj(0)?;
        let len = peek_usize(1)?;
        if argument_index < len {
            let ao = obj.o.to_arguments_object().ok_or(InternalRuntimeError::ArgumentsObjectExpected)?;
            let pmap = ao.parameter_map.as_ref().ok_or(InternalRuntimeError::MappedArgumentsObjectExpected)?;
            let mut pmap = pmap.borrow_mut();
            pmap.add_mapped_name(name.clone(), argument_index);
        }
        // Stack: AObj len ...
        Ok(())
    }
    pub fn handle_empty_break() -> anyhow::Result<()> {
        let prior_result = pop_completion()?;
        let new_result = if let Err(AbruptCompletion::Break { value, target: None }) = prior_result {
            match value {
                NormalCompletion::Empty => Ok(NormalCompletion::from(ECMAScriptValue::Undefined)),
                value => Ok(value),
            }
        } else {
            prior_result
        };
        push_completion(new_result).expect(PUSHABLE);
        Ok(())
    }
    pub fn handle_targeted_break(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        let label = string_operand(chunk)?;
        let prior_result = pop_completion()?;
        let new_result = match prior_result {
            Err(AbruptCompletion::Break { value, target: Some(target) }) if &target == label => Ok(value),
            _ => prior_result,
        };
        push_completion(new_result).expect(PUSHABLE);
        Ok(())
    }
    pub fn coalesce_value() -> anyhow::Result<()> {
        // Stack: stmtResult V ...
        // If stmtResult.[[Value]] is not empty, set V to stmtResult.[[Value]].
        let stmt_result = pop_completion()?;
        let v = pop_value()?;
        push_completion(Ok(match stmt_result {
            Ok(NormalCompletion::Value(value))
            | Err(
                AbruptCompletion::Throw { value }
                | AbruptCompletion::Return { value }
                | AbruptCompletion::Continue { value: NormalCompletion::Value(value), .. }
                | AbruptCompletion::Break { value: NormalCompletion::Value(value), .. },
            ) => NormalCompletion::from(value),
            _ => NormalCompletion::from(v),
        }))
        .expect(PUSHABLE);
        Ok(())
    }
    pub fn loop_continues(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // 1-argument opcode; takes nothing on stack (but does look at the top item); output is
        // true or false on the stack.
        let label_set = string_set_operand(chunk)?;
        // 1. If completion.[[Type]] is normal, return true.
        // 2. If completion.[[Type]] is not continue, return false.
        // 3. If completion.[[Target]] is empty, return true.
        // 4. If completion.[[Target]] is an element of labelSet, return true.
        // 5. Return false.
        let completion = peek_completion(0)?;
        let result = match &completion {
            Ok(_) | Err(AbruptCompletion::Continue { value: _, target: None }) => true,
            Err(AbruptCompletion::Continue { value: _, target: Some(label) }) => label_set.contains(label),
            _ => false,
        };
        push_value(result.into()).expect(PUSHABLE);
        Ok(())
    }
    pub fn continue_insn() -> anyhow::Result<()> {
        push_completion(Err(AbruptCompletion::Continue { value: NormalCompletion::Empty, target: None }))
    }
    pub fn targeted_continue(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        let label = string_operand(chunk)?.clone();
        push_completion(Err(AbruptCompletion::Continue { value: NormalCompletion::Empty, target: Some(label) }))
    }
    pub fn break_insn() -> anyhow::Result<()> {
        push_completion(Err(AbruptCompletion::Break { value: NormalCompletion::Empty, target: None }))
    }
    pub fn targeted_break(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        let label = string_operand(chunk)?.clone();
        push_completion(Err(AbruptCompletion::Break { value: NormalCompletion::Empty, target: Some(label) }))
    }
    pub fn iterator_accumulate() -> anyhow::Result<()> {
        let iterable = pop_value()?;
        let starting_index = pop_value()?;
        let array = pop_obj()?;
        let mut next_index = to_index(starting_index).map_err(|_| InternalRuntimeError::NumberExpected)?;
        match get_iterator(&iterable, IteratorKind::Sync).and_then(|ir| loop {
            match ir.step() {
                Ok(next_opt_obj) => match next_opt_obj {
                    None => break Ok((next_index, array)),
                    Some(next_obj) => {
                        let next_value = iterator_value(&next_obj);
                        match next_value {
                            Err(e) => break Err(e),
                            Ok(next_value) => {
                                array
                                    .create_data_property_or_throw(JSString::from(next_index), next_value)
                                    .expect("props should store ok");
                                next_index += 1;
                            }
                        }
                    }
                },
                Err(e) => break Err(e),
            }
        }) {
            Ok((next_index, array)) => {
                push_value(array.into()).expect(PUSHABLE);
                push_value(next_index.into()).expect(PUSHABLE);
            }
            Err(e) => {
                push_completion(Err(e)).expect(PUSHABLE);
            }
        }
        Ok(())
    }
    pub fn iterate_arguments() -> anyhow::Result<()> {
        // Starts with one item on the stack (an ecmascript value), returns either an error or a stack-based
        // representation of an argument list (N arg(n-1) ... arg(0)). See
        // [ArgumentListEvaluation](https://tc39.es/ecma262/#sec-runtime-semantics-argumentlistevaluation)
        // (Especially the _ArgumentList : ... AssignmentExpression_ production.)
        //
        //  4. Let iteratorRecord be ? GetIterator(spreadObj, sync).
        //  5. Repeat,
        //      a. Let next be ? IteratorStep(iteratorRecord).
        //      b. If next is false, return list.
        //      c. Let nextArg be ? IteratorValue(next).
        //      d. Append nextArg to list.
        let spread_obj = pop_value()?;

        let iterator_result = get_iterator(&spread_obj, IteratorKind::Sync);
        let steps_result = if let Ok(iterator_record) = iterator_result {
            let mut count = 0;
            let res = loop {
                match iterator_step(&iterator_record) {
                    Ok(next_opt_obj) => match next_opt_obj {
                        None => {
                            push_value(count.into()).expect(PUSHABLE);
                            break Ok(());
                        }
                        Some(obj) => match iterator_value(&obj) {
                            Ok(next_arg) => {
                                count += 1;
                                push_value(next_arg).expect(PUSHABLE);
                            }
                            Err(e) => {
                                break Err(e);
                            }
                        },
                    },
                    Err(e) => {
                        break Err(e);
                    }
                }
            };
            if res.is_err() {
                // unwind
                for _ in 0..count {
                    let _ = pop_completion().expect(POPPABLE);
                }
            }
            res
        } else {
            Err(iterator_result.unwrap_err())
        };
        if let Err(e) = steps_result {
            push_completion(Err(e)).expect(PUSHABLE);
        }
        Ok(())
    }
    pub fn elision_idae() -> anyhow::Result<()> {
        // The Elision form of IteratorDestructuringAssignmentEvaluation
        // Input on stack: <elision count as a float value> <iterator record>
        // Ouptut on stack: <iterator record>/err
        let mut count = pop_usize()?;
        let ir = pop_ir()?;
        // Elision : ,
        //  1. If iteratorRecord.[[Done]] is false, then
        //      a. Let next be Completion(IteratorStep(iteratorRecord)).
        //      b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
        //      c. ReturnIfAbrupt(next).
        //      d. If next is false, set iteratorRecord.[[Done]] to true.
        //  2. Return unused.
        // Elision : Elision ,
        //  1. Perform ? IteratorDestructuringAssignmentEvaluation of Elision with argument iteratorRecord.
        //  2. If iteratorRecord.[[Done]] is false, then
        //      a. Let next be Completion(IteratorStep(iteratorRecord)).
        //      b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
        //      c. ReturnIfAbrupt(next).
        //      d. If next is false, set iteratorRecord.[[Done]] to true.
        //  3. Return unused.
        let retval = loop {
            if count == 0 || ir.done.get() {
                break Ok(());
            }
            let next_result = ir.step();
            match next_result {
                Ok(None) => {
                    ir.done.set(true);
                }
                Ok(Some(_)) => (),
                Err(e) => {
                    ir.done.set(true);
                    break Err(e);
                }
            }
            count -= 1;
        }
        .map(|()| NormalCompletion::from(ir));
        push_completion(retval).expect(PUSHABLE);
        Ok(())
    }
    pub fn embellished_iterator_step() -> anyhow::Result<()> {
        // Input: on stack: an iterator record (ir)
        // Output: on stack: an output value (v), and the input ir, -or- one error
        //  1. Let v be undefined.
        //  2. If iteratorRecord.[[Done]] is false, then
        //      a. Let next be Completion(IteratorStep(iteratorRecord)).
        //      b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
        //      c. ReturnIfAbrupt(next).
        //      d. If next is false, set iteratorRecord.[[Done]] to true.
        //      e. Else,
        //          i. Set v to Completion(IteratorValue(next)).
        //          ii. If v is an abrupt completion, set iteratorRecord.[[Done]] to true.
        //          iii. ReturnIfAbrupt(v).
        let ir = pop_ir()?;
        if ir.done.get() {
            push_completion(Ok(NormalCompletion::from(ir))).expect(PUSHABLE);
            push_value(ECMAScriptValue::Undefined).expect(PUSHABLE);
        } else {
            let next_result = ir.step();
            match next_result {
                Err(e) => {
                    ir.done.set(true);
                    push_completion(Err(e)).expect(PUSHABLE);
                }
                Ok(None) => {
                    ir.done.set(true);
                    push_completion(Ok(NormalCompletion::from(ir))).expect(PUSHABLE);
                    push_value(ECMAScriptValue::Undefined).expect(PUSHABLE);
                }
                Ok(Some(next_obj)) => {
                    let v = iterator_value(&next_obj);
                    match v {
                        Err(e) => {
                            ir.done.set(true);
                            push_completion(Err(e)).expect(PUSHABLE);
                        }
                        Ok(v) => {
                            push_completion(Ok(NormalCompletion::from(ir))).expect(PUSHABLE);
                            push_value(v).expect(PUSHABLE);
                        }
                    }
                }
            }
        }
        Ok(())
    }
    pub fn iterator_rest() -> anyhow::Result<()> {
        // Input: one stack argument, an iterator record
        // Output: two stack elements: an array object, followed by the iterator record
        //     or: one stack element: an error

        //  2. Let A be ! ArrayCreate(0).
        //  3. Let n be 0.
        //  4. Repeat,
        //      a. If iteratorRecord.[[Done]] is false, then
        //          i. Let next be Completion(IteratorStep(iteratorRecord)).
        //          ii. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
        //          iii. ReturnIfAbrupt(next).
        //          iv. If next is false, set iteratorRecord.[[Done]] to true.
        //      b. If iteratorRecord.[[Done]] is true, then
        //          i. Return (A, iteratorRecord).
        //      c. Let nextValue be Completion(IteratorValue(next)).
        //      d. If nextValue is an abrupt completion, set iteratorRecord.[[Done]] to true.
        //      e. ReturnIfAbrupt(nextValue).
        //      f. Perform ! CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), nextValue).
        //      g. Set n to n + 1.
        let ir = pop_ir()?;
        let a = array_create(0, None).expect("0 should fit in ram");
        let mut n = 0;
        loop {
            if ir.done.get() {
                push_completion(Ok(NormalCompletion::from(ir))).expect(PUSHABLE);
                push_value(ECMAScriptValue::Object(a)).expect(PUSHABLE);
                break;
            }
            match ir.step() {
                Ok(None) => {
                    ir.done.set(true);
                    push_completion(Ok(NormalCompletion::from(ir))).expect(PUSHABLE);
                    push_value(ECMAScriptValue::Object(a)).expect(PUSHABLE);
                    break;
                }
                Ok(Some(next_obj)) => {
                    match iterator_value(&next_obj) {
                        Ok(next_value) => {
                            a.create_data_property_or_throw(format!("{n}"), next_value)
                                .expect("array property set should work");
                            n += 1;
                        }
                        Err(e) => {
                            ir.done.set(true);
                            push_completion(Err(e)).expect(PUSHABLE);
                            break;
                        }
                    };
                }
                Err(e) => {
                    ir.done.set(true);
                    push_completion(Err(e)).expect(PUSHABLE);
                    break;
                }
            };
        }
        Ok(())
    }
    pub fn require_coercible() -> anyhow::Result<()> {
        // Input: one stack argument, an ECMAScriptValue
        // Output: one stack completion: either the same input value, or an error.
        let val = pop_value()?;
        let result = require_object_coercible(&val);
        push_completion(match result {
            Ok(()) => Ok(val.into()),
            Err(e) => Err(e),
        })
        .expect(PUSHABLE);
        Ok(())
    }
    pub fn get_sync_iterator() -> anyhow::Result<()> {
        // This instruction handles the step that looks like:
        //  1. Let iteratorRecord be ? GetIterator(value, sync).
        //
        // Input on stack: that value.
        // Output on stack: the iterator record/abrupt completion.

        let value = pop_value()?;
        let result = get_iterator(&value, IteratorKind::Sync).map(NormalCompletion::from);
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn iterator_close() -> anyhow::Result<()> {
        // This instruction handles the steps that look like:
        //  1. Return ? IteratorClose(iteratorRecord, status).

        // Input on the stack: status iteratorRecord
        // Ouptut on stack: the operation's result (which might be the input status)
        let status = pop_completion()?;
        let ir = pop_ir()?;
        let result = super::iterator_close(&ir, status);
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn iterator_close_if_not_done() -> anyhow::Result<()> {
        // This instruction handles the step that looks like:
        //  3. If iteratorRecord.[[Done]] is false, return ? IteratorClose(iteratorRecord, result).
        //  4. Return ? result.
        //
        // Input on stack: result iteratorRecord
        // Output result/err
        let result = pop_completion()?;
        let ir = pop_ir()?;
        let done = ir.done.get();
        let right = if done { result } else { super::iterator_close(&ir, result) };
        push_completion(right).expect(PUSHABLE);
        Ok(())
    }
    pub fn iterator_next() -> anyhow::Result<()> {
        // This instruction handles the steps that look like:
        //  1. Let nextResult be ? Call(iteratorRecord.[[NextMethod]], iteratorRecord.[[Iterator]]).
        //  2. If nextResult is not an Object, throw a TypeError exception.
        //
        // Input on stack: iteratorRecord
        // Output:         nextResult/err iteratorRecord
        let ir = pop_ir()?;
        let next_method = ir.next_method.clone();
        let iterator = ECMAScriptValue::from(ir.iterator.clone());
        let next_result = super::call(&next_method, &iterator, &[]);

        let result = match next_result {
            Ok(val) => match val {
                ECMAScriptValue::Object(_) => Ok(val),
                _ => Err(create_type_error("Iterator Result must be an object")),
            },
            Err(_) => next_result,
        };
        push_completion(Ok(NormalCompletion::from(ir))).expect(PUSHABLE);
        push_completion(result.map(NormalCompletion::from)).expect(PUSHABLE);
        Ok(())
    }
    pub fn iterator_result_complete() -> anyhow::Result<()> {
        // This instruction handles the steps that look like:
        //  1. Let done be ? IteratorComplete(nextResult).
        //
        // Input on stack: nextResult (an iterator result object)
        // Output: done/err nextResult (the result is not consumed)

        let next_result = pop_obj()?;
        let done = iterator_complete(&next_result);
        push_value(next_result.into()).expect(PUSHABLE);
        push_completion(done.map(NormalCompletion::from)).expect(PUSHABLE);
        Ok(())
    }
    pub fn iterator_result_to_value() -> anyhow::Result<()> {
        // This instruction handles the steps that look like:
        //  1. Let nextValue be ? IteratorValue(nextResult).
        //
        // Input on stack: nextResult (an iterator result object)
        // Output: value/err (the nextResult is consumed)
        let next_result = pop_obj()?;
        let value = iterator_value(&next_result);
        push_completion(value.map(NormalCompletion::from)).expect(PUSHABLE);
        Ok(())
    }
    pub fn getv() -> anyhow::Result<()> {
        // input: key, base
        let prop_name = pop_key()?;
        let base_val = pop_value()?;
        let result = base_val.get(&prop_name).map(NormalCompletion::from);
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn enumerate_object_properties() -> anyhow::Result<()> {
        // input: an object
        // output: an iterator record set up to iterate over the properties of that object
        let obj = pop_obj()?;
        let iterator = create_for_in_iterator(obj);
        let next_obj = iterator.get(&"next".into()).expect("next method should exist");
        let ir = IteratorRecord { iterator, next_method: next_obj, done: Cell::new(false) };
        push_completion(Ok(NormalCompletion::from(ir))).expect(PUSHABLE);
        Ok(())
    }
    pub fn private_id_lookup(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Expect string id in the opcode; it refers to "privateIdentifier" in the following steps:
        // Input on the stack: nothing
        // Output on the stack: the sought-after PrivateId

        // (These are from the evalution steps for the production: ClassElementName : PrivateIdentifier)
        // 2. Let privateEnvRec be the running execution context's PrivateEnvironment.
        // 3. Let names be privateEnvRec.[[Names]].
        // 4. Assert: Exactly one element of names is a Private Name whose [[Description]] is privateIdentifier.
        // 5. Let privateName be the Private Name in names whose [[Description]] is privateIdentifier.
        // 6. Return privateName.
        let private_identifier = string_operand(chunk)?;
        let priv_env = current_private_environment().ok_or(InternalRuntimeError::NoPrivateEnv)?;
        let names = &priv_env.borrow().names;
        let private_name = names
            .iter()
            .find(|&item| item.description == *private_identifier)
            .ok_or(InternalRuntimeError::MissingPrivateName)?
            .clone();
        push_completion(Ok(NormalCompletion::from(private_name)))
    }
    pub fn evaluate_initialized_class_field_def(
        chunk: &Rc<Chunk>,
        text: &str,
        staticness: Static,
    ) -> anyhow::Result<()> {
        // Input: Operand: function stash
        // Input: Stack: name homeObject
        // Output: FieldRecord homeObject
        let info = sfd_operand(chunk)?;
        let name = pop_classname()?.ok_or(InternalRuntimeError::StringExpected)?;
        let home_object = pop_obj()?;
        let initializer =
            evaluate_initialized_class_field_definition(info, home_object.clone(), Some(name.clone()), text)?;
        let cstr = if staticness == Static::Yes {
            ClassItem::StaticClassFieldDefinition
        } else {
            ClassItem::ClassFieldDefinition
        };
        let cfdr = ClassFieldDefinitionRecord { name, initializer: Some(initializer) };
        let completion = NormalCompletion::ClassItem(Box::new(cstr(cfdr)));

        push_value(ECMAScriptValue::Object(home_object)).expect(PUSHABLE);
        push_completion(Ok(completion)).expect(PUSHABLE);
        Ok(())
    }
    pub fn evaluate_class_static_block_def(chunk: &Rc<Chunk>, text: &str) -> anyhow::Result<()> {
        // This does the runtime parts of ClassStaticBlockDefinitionEvaluation
        // Input Operand: sfd_index
        // Input Stack: homeObject
        // Output Stack: block homeObject
        let info = sfd_operand(chunk)?;
        let home_object = pop_obj()?;
        let block_body = evaluate_class_static_block_definition(info, home_object.clone(), text)?;
        let block = Box::new(ClassItem::ClassStaticBlockDefinition(ClassStaticBlockDefinitionRecord {
            body_function: block_body,
        }));

        push_value(ECMAScriptValue::Object(home_object)).expect(PUSHABLE);
        push_completion(Ok(NormalCompletion::ClassItem(block))).expect(PUSHABLE);
        Ok(())
    }
    pub fn define_method(chunk: &Rc<Chunk>, text: &str) -> anyhow::Result<()> {
        // on the stack: object prototype
        let info = sfd_operand(chunk)?;
        let obj = pop_obj()?;
        let prototype = pop_obj()?;

        let to_compile: Rc<MethodDefinition> = info.to_compile.clone().try_into()?;
        if let MethodDefinition::NamedFunction(_, _, body, _) = to_compile.as_ref() {
            let name = nameify(&info.source_text, 50);
            let mut compiled = Chunk::new(name);
            let compilation_status = body.compile_body(&mut compiled, text, info);
            if let Err(err) = compilation_status {
                let typeerror = create_type_error(err.to_string());
                push_completion(Err(typeerror)).expect(PUSHABLE);
                return Ok(());
            }
            for line in compiled.disassemble() {
                println!("{line}");
            }

            let env = current_lexical_environment().ok_or(InternalRuntimeError::NoLexicalEnvironment)?;
            let private_env = current_private_environment();
            let source_text = &info.source_text;
            let strict = info.strict;
            let closure = ordinary_function_create(
                prototype,
                source_text,
                info.params.clone(),
                info.body.clone(),
                ThisLexicality::NonLexicalThis,
                env,
                private_env,
                strict,
                Rc::new(compiled),
            );

            make_method(
                closure.o.to_function_obj().expect("a closure we just created should be a function object"),
                obj,
            );

            push_value(ECMAScriptValue::Object(closure)).expect(PUSHABLE);
            Ok(())
        } else {
            Err(InternalRuntimeError::ExpectedMethod)?
        }
    }
    pub fn set_function_name() -> anyhow::Result<()> {
        // on stack: propertykey closure
        // but at the exit, we want them to stay there, so we just peek them from the end of the stack.
        let property_key = peek_function_name(0)?;
        let closure = peek_obj(1)?;
        super::set_function_name(&closure, property_key, None);
        Ok(())
    }
    pub fn define_method_property(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Takes one arg. 0 => enumerable:false; 1 => enumerable:true
        // Stack: propertykey closure object
        let enumerable = usize_operand(chunk)? != 0;
        let name = pop_functionname()?;
        let closure = pop_obj()?;
        let home_object = pop_obj()?;
        let result = super::define_method_property(&home_object, name, closure, enumerable).map(NormalCompletion::from);
        push_completion(result).expect(PUSHABLE);
        Ok(())
    }
    pub fn define_getter(chunk: &Rc<Chunk>, text: &str) -> anyhow::Result<()> {
        // Takes two args: idx into function stash and enumerable flag
        // stack input: propkey object
        // output: err/empty/PrivateElement
        let info = sfd_operand(chunk)?;
        let enumerable = usize_operand(chunk)? != 0;
        let prop_key = pop_functionname()?;
        let base_object = pop_obj()?;

        //  2. Let env be the running execution context's LexicalEnvironment.
        //  3. Let privateEnv be the running execution context's PrivateEnvironment.
        //  6. Let closure be OrdinaryFunctionCreate(%Function.prototype%, sourceText, formalParameterList,
        //     FunctionBody, NON-LEXICAL-THIS, env, privateEnv).
        //  7. Perform MakeMethod(closure, object).
        //  8. Perform SetFunctionName(closure, propKey, "get").
        //  9. If propKey is a Private Name, then
        //      a. Return PrivateElement { [[Key]]: propKey, [[Kind]]: ACCESSOR, [[Get]]: closure, [[Set]]:
        //         undefined }.
        //  10. Else,
        //      a. Let desc be the PropertyDescriptor { [[Get]]: closure, [[Enumerable]]: enumerable,
        //         [[Configurable]]: true }.
        //      b. Perform ? DefinePropertyOrThrow(object, propKey, desc).
        //      c. Return UNUSED.
        let to_compile: Rc<MethodDefinition> = info.to_compile.clone().try_into()?;
        let MethodDefinition::Getter(_, fb, _) = to_compile.as_ref() else {
            Err(InternalRuntimeError::GetterMethodExpected)?
        };
        let prod_text_loc = to_compile.location().span;
        let prod_text = &text[prod_text_loc.starting_index..prod_text_loc.starting_index + prod_text_loc.length];
        let chunk_name = nameify(prod_text, 50);
        let mut compiled = Chunk::new(chunk_name);
        let compilation_status = fb.compile_body(&mut compiled, text, info);
        if let Err(err) = compilation_status {
            let typeerror = create_type_error(err.to_string());
            push_completion(Err(typeerror)).expect(PUSHABLE);
            return Ok(());
        }

        for line in compiled.disassemble() {
            println!("{line}");
        }

        let env = current_lexical_environment().ok_or(InternalRuntimeError::NoLexicalEnvironment)?;
        let private_env = current_private_environment();
        let prototype = intrinsic(IntrinsicId::FunctionPrototype);
        let closure = ordinary_function_create(
            prototype,
            &info.source_text,
            info.params.clone(),
            info.body.clone(),
            info.this_mode,
            env,
            private_env,
            info.strict,
            Rc::new(compiled),
        );
        make_method(closure.o.to_function_obj().unwrap(), base_object.clone());
        super::set_function_name(&closure, prop_key.clone(), Some("get".into()));

        let result = match prop_key {
            FunctionName::String(_) | FunctionName::Symbol(_) => {
                let desc = PotentialPropertyDescriptor::new().get(closure).enumerable(enumerable).configurable(true);
                let result = define_property_or_throw(&base_object, PropertyKey::try_from(prop_key).unwrap(), desc);
                result.map(|()| None)
            }
            FunctionName::PrivateName(pn) => Ok(Some(PrivateElement {
                key: pn,
                kind: PrivateElementKind::Accessor { get: Some(closure), set: None },
            })),
        };
        push_completion(result.map(NormalCompletion::from)).expect(PUSHABLE);
        Ok(())
    }
    pub fn define_setter(chunk: &Rc<Chunk>, text: &str) -> anyhow::Result<()> {
        // Takes two args: idx into function stash and enumerable flag
        // stack input: propkey object
        // output: err/empty/PrivateElement
        let info = sfd_operand(chunk)?;
        let enumerable = usize_operand(chunk)? != 0;
        let prop_key = pop_functionname()?;
        let base_object = pop_obj()?;

        //  2. Let env be the running execution context's LexicalEnvironment.
        //  3. Let privateEnv be the running execution context's PrivateEnvironment.
        //  5. Let closure be OrdinaryFunctionCreate(%Function.prototype%, sourceText,
        //     PropertySetParameterList, FunctionBody, NON-LEXICAL-THIS, env, privateEnv).
        //  6. Perform MakeMethod(closure, object).
        //  7. Perform SetFunctionName(closure, propKey, "set").
        //  8. If propKey is a Private Name, then
        //      a. Return PrivateElement { [[Key]]: propKey, [[Kind]]: ACCESSOR, [[Get]]: undefined, [[Set]]:
        //         closure }.
        //  9. Else,
        //      a. Let desc be the PropertyDescriptor { [[Set]]: closure, [[Enumerable]]: enumerable,
        //         [[Configurable]]: true }.
        //      b. Perform ? DefinePropertyOrThrow(object, propKey, desc).
        //      c. Return UNUSED.
        let to_compile: Rc<MethodDefinition> = info.to_compile.clone().try_into()?;
        let MethodDefinition::Setter(_, _, fb, _) = to_compile.as_ref() else {
            Err(InternalRuntimeError::SetterMethodExpected)?
        };
        let prod_text_loc = to_compile.location().span;
        let prod_text = &text[prod_text_loc.starting_index..prod_text_loc.starting_index + prod_text_loc.length];
        let chunk_name = nameify(prod_text, 50);
        let mut compiled = Chunk::new(chunk_name);
        let compilation_status = fb.compile_body(&mut compiled, text, info);
        if let Err(err) = compilation_status {
            let typeerror = create_type_error(err.to_string());
            push_completion(Err(typeerror)).expect(PUSHABLE);
            return Ok(());
        }

        for line in compiled.disassemble() {
            println!("{line}");
        }

        let env = current_lexical_environment().ok_or(InternalRuntimeError::NoLexicalEnvironment)?;
        let private_env = current_private_environment();
        let prototype = intrinsic(IntrinsicId::FunctionPrototype);
        let closure = ordinary_function_create(
            prototype,
            &info.source_text,
            info.params.clone(),
            info.body.clone(),
            info.this_mode,
            env,
            private_env,
            info.strict,
            Rc::new(compiled),
        );
        make_method(closure.o.to_function_obj().unwrap(), base_object.clone());
        super::set_function_name(&closure, prop_key.clone(), Some("set".into()));

        let result = match prop_key {
            FunctionName::String(_) | FunctionName::Symbol(_) => {
                let desc = PotentialPropertyDescriptor::new().set(closure).enumerable(enumerable).configurable(true);
                let result = define_property_or_throw(&base_object, PropertyKey::try_from(prop_key).unwrap(), desc);
                result.map(|()| None)
            }
            FunctionName::PrivateName(pn) => Ok(Some(PrivateElement {
                key: pn,
                kind: PrivateElementKind::Accessor { set: Some(closure), get: None },
            })),
        };
        push_completion(result.map(NormalCompletion::from)).expect(PUSHABLE);
        Ok(())
    }
    pub fn generator_start_from_function(text: &str) -> anyhow::Result<()> {
        //  2. Let G be ? OrdinaryCreateFromConstructor(functionObject,
        //     "%GeneratorFunction.prototype.prototype%", « [[GeneratorState]], [[GeneratorContext]],
        //     [[GeneratorBrand]] »).
        //  3. Set G.[[GeneratorBrand]] to EMPTY.
        //  4. Perform GeneratorStart(G, FunctionBody).
        let func = pop_obj()?;
        let g_res = func.ordinary_create_from_constructor(IntrinsicId::GeneratorFunctionPrototypePrototype, |proto| {
            GeneratorObject::object(proto, GeneratorState::Undefined, "")
        });
        push_completion(g_res.clone().map(NormalCompletion::from)).expect(PUSHABLE);
        if let Ok(g_obj) = g_res.as_ref() {
            let g = g_obj.o.to_generator_object().ok_or(InternalRuntimeError::GeneratorExpected)?;
            g.generator_data.borrow_mut().generator_brand = String::new();
            generator_start_from_function_body(
                g_obj,
                func.o.to_function_obj().ok_or(InternalRuntimeError::FunctionExpected)?,
                text,
            );
        }
        Ok(())
    }
    pub fn instantiate_generator_function_expression(chunk: &Rc<Chunk>, text: &str) -> anyhow::Result<()> {
        let info = sfd_operand(chunk)?;
        // Runtime Semantics: InstantiateGeneratorFunctionExpression
        // The syntax-directed operation InstantiateGeneratorFunctionExpression takes optional argument name (a property
        // key or a Private Name) and returns a function object. It is defined piecewise over the following productions:
        //
        // GeneratorExpression : function * ( FormalParameters ) { GeneratorBody }
        //  1. If name is not present, set name to "".
        //  2. Let env be the LexicalEnvironment of the running execution context.
        //  3. Let privateEnv be the running execution context's PrivateEnvironment.
        //  4. Let sourceText be the source text matched by GeneratorExpression.
        //  5. Let closure be OrdinaryFunctionCreate(%GeneratorFunction.prototype%, sourceText, FormalParameters,
        //     GeneratorBody, non-lexical-this, env, privateEnv).
        //  6. Perform SetFunctionName(closure, name).
        //  7. Let prototype be OrdinaryObjectCreate(%GeneratorFunction.prototype.prototype%).
        //  8. Perform ! DefinePropertyOrThrow(closure, "prototype", PropertyDescriptor { [[Value]]: prototype,
        //     [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false }).
        //  9. Return closure.
        let env = current_lexical_environment().ok_or(InternalRuntimeError::NoLexicalEnvironment)?;
        let priv_env = current_private_environment();
        let name = pop_string()?;

        let to_compile: Rc<GeneratorExpression> = info.to_compile.clone().try_into()?;
        let chunk_name = nameify(&info.source_text, 50);
        let mut compiled = Chunk::new(chunk_name);
        let compilation_status = to_compile.body.evaluate_generator_body(&mut compiled, text, info);
        if let Err(err) = compilation_status {
            let typeerror = create_type_error(err.to_string());
            let _ = pop_completion()?;
            push_completion(Err(typeerror)).expect(PUSHABLE);
            return Ok(());
        }
        for line in compiled.disassemble() {
            println!("{line}");
        }

        let generator_function_prototype = intrinsic(IntrinsicId::GeneratorFunctionPrototype);

        let closure = ordinary_function_create(
            generator_function_prototype,
            info.source_text.as_str(),
            info.params.clone(),
            info.body.clone(),
            ThisLexicality::NonLexicalThis,
            env,
            priv_env,
            info.strict,
            Rc::new(compiled),
        );
        super::set_function_name(&closure, name.into(), None);

        let gfpp = intrinsic(IntrinsicId::GeneratorFunctionPrototypePrototype);
        let prototype = ordinary_object_create(Some(gfpp));
        let desc =
            PotentialPropertyDescriptor::new().value(prototype).writable(true).enumerable(false).configurable(false);
        define_property_or_throw(&closure, "prototype", desc).expect("simple property definition works");
        push_value(closure.into()).expect(PUSHABLE);
        Ok(())
    }
    pub async fn yield_insn(co: &Co<ECMAScriptValue, Completion<ECMAScriptValue>>) -> anyhow::Result<()> {
        // Yield ( value )
        // The abstract operation Yield takes argument value (an ECMAScript language value) and returns either a normal
        // completion containing an ECMAScript language value or an abrupt completion. It performs the following steps
        // when called:
        //
        //  1. Let generatorKind be GetGeneratorKind().
        //  2. If generatorKind is ASYNC, return ? AsyncGeneratorYield(? Await(value)).
        //  3. Otherwise, return ? GeneratorYield(CreateIterResultObject(value, false)).
        let value = pop_value()?;
        let resobj = create_iter_result_object(value, false);
        let yielded_result = generator_yield(co, ECMAScriptValue::Object(resobj)).await;
        push_completion(yielded_result.map(NormalCompletion::from)).expect(PUSHABLE);
        Ok(())
    }
    pub fn get_parents_from_superclass() -> anyhow::Result<()> {
        // Input Stack:   err/superclass-reference
        // Output Stack:  err/(proto-parent constructor-parent)

        //      e. Let superclass be ? GetValue(? superclassRef).
        //      f. If superclass is null, then
        //          i. Let protoParent be null.
        //          ii. Let constructorParent be %Function.prototype%.
        //      g. Else if IsConstructor(superclass) is false, then
        //          i. Throw a TypeError exception.
        //      h. Else,
        //          i. Let protoParent be ? Get(superclass, "prototype").
        //          ii. If protoParent is not an Object and protoParent is not null, throw a TypeError exception.
        //          iii. Let constructorParent be superclass.

        let item = pop_completion()?;
        let result = (|item| {
            let superclass = super::get_value(item)?;
            if superclass == ECMAScriptValue::Null {
                Ok((ECMAScriptValue::Null, ECMAScriptValue::Object(intrinsic(IntrinsicId::FunctionPrototype))))
            } else if !superclass.is_constructor() {
                Err(create_type_error("Constructor expected"))
            } else {
                let proto_parent = superclass.get(&"prototype".into())?;
                if matches!(proto_parent, ECMAScriptValue::Null | ECMAScriptValue::Object(_)) {
                    Ok((proto_parent, superclass))
                } else {
                    Err(create_type_error("Constructor has bad prototype property"))
                }
            }
        })(item);
        match result {
            Ok((proto_parent, constructor_parent)) => {
                push_value(constructor_parent)?;
                push_value(proto_parent)?;
            }
            Err(err) => {
                push_completion(Err(err))?;
            }
        }
        Ok(())
    }

    pub fn create_default_constructor() -> anyhow::Result<()> {
        // Input: Stack: constructorParent className
        // Output: Stack: constructorFunctionObject
        let constructor_parent = pop_value()?.try_into()?;
        let class_name = pop_value()?.try_into()?;
        let realm = current_realm_record();

        // a. Let defaultConstructor be a new Abstract Closure with no parameters that captures nothing and
        //    performs the following steps when called:
        //    [ See default_constructor, below ]
        // b. Let F be CreateBuiltinFunction(defaultConstructor, 0, className, « [[ConstructorKind]],
        //    [[SourceText]] », the current Realm Record, constructorParent).
        let f = create_builtin_function(
            Box::new(default_constructor),
            Some(ConstructorKind::Base),
            0.0,
            class_name,
            &[InternalSlotName::ConstructorKind, InternalSlotName::SourceText],
            realm,
            constructor_parent,
            None,
        );
        push_value(ECMAScriptValue::Object(f)).expect(PUSHABLE);
        Ok(())
    }

    pub fn make_class_constructor_and_set_name() -> anyhow::Result<()> {
        // Input Stack: functionObject className
        // Output Stack: functionObject
        let function_object = pop_value()?.try_into()?;
        let class_name = pop_value()?.try_into()?;

        make_class_constructor(&function_object);
        super::set_function_name(&function_object, class_name, None);

        push_value(ECMAScriptValue::Object(function_object)).expect(PUSHABLE);
        Ok(())
    }

    pub fn make_constructor() -> anyhow::Result<()> {
        // Input Stack: func-object
        // Output Stack: func-object
        let function_object = pop_value()?.try_into()?;
        super::make_constructor(&function_object, None);
        push_value(ECMAScriptValue::Object(function_object)).expect(PUSHABLE);
        Ok(())
    }

    pub fn make_constructor_with_proto() -> anyhow::Result<()> {
        // Input Stack: func-object proto
        // Output Stack: func-object proto
        let function_object = pop_obj()?;
        let proto = pop_obj()?;
        super::make_constructor(&function_object, Some((false, proto.clone())));
        push_value(ECMAScriptValue::Object(proto)).expect(PUSHABLE);
        push_value(ECMAScriptValue::Object(function_object)).expect(PUSHABLE);
        Ok(())
    }

    pub fn set_derived() -> anyhow::Result<()> {
        // Input Stack: func-object
        // Output Stack: func-object
        let function_object: Object = pop_obj()?;
        match function_object.o.to_function_obj() {
            Some(fobj) => {
                let mut data_ref = fobj.function_data().borrow_mut();
                data_ref.constructor_kind = ConstructorKind::Derived;
            }
            None => {
                let builtin =
                    function_object.o.to_builtin_function_obj().ok_or(InternalRuntimeError::FunctionExpected)?;
                let mut data_ref = builtin.builtin_function_data().borrow_mut();
                data_ref.constructor_kind = Some(ConstructorKind::Derived);
            }
        }
        push_value(ECMAScriptValue::Object(function_object)).expect(PUSHABLE);
        Ok(())
    }

    pub fn attach_elements(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input: Operand: count: the number of elements to process; count can be zero
        // Input: Stack: FunctionObject element1 element2 ... elementN
        // Output: Stack: err/FunctionObject

        fn private_element_insert(
            container: &mut Vec<PrivateElement>,
            pe: PrivateElement,
        ) -> Result<(), InternalRuntimeError> {
            if let Some(element) = container.iter_mut().find(|element| element.key == pe.key) {
                let (pe_get, pe_set) = match pe.kind {
                    PrivateElementKind::Field { .. } | PrivateElementKind::Method { .. } => {
                        return Err(InternalRuntimeError::AccessorElementExpected);
                    }
                    PrivateElementKind::Accessor { get, set } => (get, set),
                };
                if pe_get.is_some() == pe_set.is_some() {
                    Err(InternalRuntimeError::ImproperPrivateElement)
                } else if let PrivateElementKind::Accessor { get, set } = &mut element.kind {
                    if set.is_none() && pe_set.is_some() {
                        *set = pe_set;
                        Ok(())
                    } else if get.is_none() && pe_get.is_some() {
                        *get = pe_get;
                        Ok(())
                    } else {
                        Err(InternalRuntimeError::ImproperPrivateElement)
                    }
                } else {
                    Err(InternalRuntimeError::AccessorElementExpected)
                }
            } else {
                container.push(pe);
                Ok(())
            }
        }

        let count = operand(chunk)?;
        let func_obj = pop_obj()?;

        let mut items = vec![];
        for _ in 0..count {
            let item = pop_completion()?.or(Err(InternalRuntimeError::NonErrorExpected))?;
            items.push(item);
        }

        let mut instance_private_methods = vec![];
        let mut static_private_methods = vec![];
        let mut instance_fields = vec![];
        let mut static_elements = vec![];

        for item in items.into_iter().rev() {
            // Item may be a
            //   * static PrivateElement
            //   * nonstatic PrivateElement
            //   * static ClassFieldDefinition
            //   * nonstatic ClassFieldDefinition
            //   * ClassStaticBlockDefinition
            //   * Empty
            let field: Option<ClassItem> = item.try_into()?;
            if let Some(field) = field {
                match field {
                    ClassItem::StaticPrivateElement(pe) => private_element_insert(&mut static_private_methods, pe)?,
                    ClassItem::PrivateElement(pe) => private_element_insert(&mut instance_private_methods, pe)?,
                    ClassItem::StaticClassFieldDefinition(fd) => static_elements.push(StaticElement::from(fd)),
                    ClassItem::ClassFieldDefinition(fd) => instance_fields.push(fd),
                    ClassItem::ClassStaticBlockDefinition(bd) => static_elements.push(StaticElement::from(bd)),
                }
            }
        }

        {
            match func_obj.o.to_function_obj() {
                Some(func) => {
                    let mut func_data = func.function_data().borrow_mut();
                    func_data.private_methods = instance_private_methods;
                    func_data.fields = instance_fields;
                }
                None => match func_obj.o.to_builtin_function_obj() {
                    Some(builtin) => {
                        let mut func_data = builtin.builtin_function_data().borrow_mut();
                        func_data.private_methods = instance_private_methods;
                        func_data.fields = instance_fields;
                    }
                    None => Err(InternalRuntimeError::FunctionExpected)?,
                },
            }
        }
        for method in static_private_methods {
            private_method_or_accessor_add(&func_obj, method)
                .map_err(|_| InternalRuntimeError::ImproperPrivateElement)?;
        }
        for element_record in static_elements {
            let result = match element_record {
                StaticElement::Field(element_record) => define_field(&func_obj, &element_record),
                StaticElement::Block(element_record) => super::call(
                    &ECMAScriptValue::Object(element_record.body_function),
                    &ECMAScriptValue::Object(func_obj.clone()),
                    &[],
                )
                .map(|_| ()),
            };
            if let Err(ac) = result {
                push_completion(Err(ac)).expect(PUSHABLE);
                return Ok(());
            }
        }

        push_value(ECMAScriptValue::Object(func_obj)).expect(PUSHABLE);
        Ok(())
    }

    #[derive(Debug, Clone)]
    enum StaticElement {
        Field(ClassFieldDefinitionRecord),
        Block(ClassStaticBlockDefinitionRecord),
    }
    impl From<ClassFieldDefinitionRecord> for StaticElement {
        fn from(value: ClassFieldDefinitionRecord) -> Self {
            Self::Field(value)
        }
    }
    impl From<ClassStaticBlockDefinitionRecord> for StaticElement {
        fn from(value: ClassStaticBlockDefinitionRecord) -> Self {
            Self::Block(value)
        }
    }

    pub fn attach_source_text(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input: operand: string
        // Input stack: obj
        // Output stack: obj
        let src_text = string_operand(chunk)?;
        let func_obj = pop_obj()?;
        match func_obj.o.to_function_obj() {
            Some(func) => {
                let mut data_ref = func.function_data().borrow_mut();
                data_ref.source_text = String::from(src_text);
            }
            None => match func_obj.o.to_builtin_function_obj() {
                Some(builtin) => {
                    let mut data_ref = builtin.builtin_function_data().borrow_mut();
                    data_ref.source_text = Some(String::from(src_text));
                }
                None => Err(InternalRuntimeError::FunctionExpected)?,
            },
        }
        push_value(ECMAScriptValue::Object(func_obj)).expect(PUSHABLE);
        Ok(())
    }

    pub fn name_only_field_record(staticness: Static) -> anyhow::Result<()> {
        // Input: Stack: name (which is a string, symbol, or private name)
        // Output: Stack: fieldRecord
        let name = pop_classname()?.ok_or(InternalRuntimeError::ClassNameExpected)?;
        let cfdr = ClassFieldDefinitionRecord { name, initializer: None };
        let cstr = if staticness == Static::Yes {
            ClassItem::StaticClassFieldDefinition
        } else {
            ClassItem::ClassFieldDefinition
        };
        push_completion(Ok(NormalCompletion::ClassItem(Box::new(cstr(cfdr))))).expect(PUSHABLE);
        Ok(())
    }

    pub fn make_private_reference(chunk: &Rc<Chunk>) -> anyhow::Result<()> {
        // Input: operand : string index
        // Input: Stack: baseValue
        // Output: Stack: privateReference

        let name = string_operand(chunk)?;
        let base = pop_value()?;
        let reference = super::make_private_reference(base, name)?;
        push_completion(Ok(NormalCompletion::from(reference))).expect(PUSHABLE);
        Ok(())
    }

    pub fn get_new_target() -> anyhow::Result<()> {
        // Input: nothing
        // Output: Stack: newTarget
        let nt = super::get_new_target()?;
        let val = if let Some(obj) = nt { ECMAScriptValue::Object(obj) } else { ECMAScriptValue::Undefined };
        push_value(val)
    }

    pub fn get_super_constructor() -> anyhow::Result<()> {
        // Input: nothing
        // Output Stack: func
        push_value(super::get_super_constructor()?)
    }

    pub fn constructor_check() -> anyhow::Result<()> {
        // Input: value
        // Output: err/value
        let value = pop_value()?;
        if value.is_constructor() {
            push_value(value).expect(PUSHABLE);
        } else {
            push_completion(Err(create_type_error("constructor required"))).expect(PUSHABLE);
        }
        Ok(())
    }

    pub fn bind_this_and_init() -> anyhow::Result<()> {
        //  7. Let thisER be GetThisEnvironment().
        //  8. Perform ? thisER.BindThisValue(result).
        //  9. Let F be thisER.[[FunctionObject]].
        //  10. Assert: F is an ECMAScript function object.
        //  11. Perform ? InitializeInstanceElements(result, F).
        //  12. Return result.

        // Input Stack: result
        // Output Stack: err/result

        let result = pop_value()?;
        let this_er = get_this_environment();
        let bind_status = this_er.bind_this_value(result.clone());
        let init_status = match bind_status {
            Ok(_val) => {
                let f = this_er.get_function_object().ok_or(InternalRuntimeError::FunctionExpected)?;
                let this_argument = Object::try_from(result.clone())?;
                initialize_instance_elements(&this_argument, &f)
            }
            Err(err) => Err(err),
        };
        let to_push = init_status.map(|()| NormalCompletion::from(result));
        push_completion(to_push).expect(PUSHABLE);
        Ok(())
    }

    pub fn static_class_item() -> anyhow::Result<()> {
        // Take what's on top of the stack and transform it to a static class item

        // Input: empty/classitem/privatelement/err
        // output: empty/classitem/err

        let completion = pop_completion()?;
        match completion {
            Ok(nc) => {
                let item: Option<ClassItem> = nc.try_into()?;
                match item {
                    None => {
                        push_completion(Ok(NormalCompletion::Empty)).expect(PUSHABLE);
                    }
                    Some(ci) => {
                        let transformed = match ci {
                            ClassItem::StaticPrivateElement(..)
                            | ClassItem::ClassStaticBlockDefinition(..)
                            | ClassItem::StaticClassFieldDefinition(..) => ci,
                            ClassItem::PrivateElement(private_element) => {
                                ClassItem::StaticPrivateElement(private_element)
                            }
                            ClassItem::ClassFieldDefinition(class_field_definition_record) => {
                                ClassItem::StaticClassFieldDefinition(class_field_definition_record)
                            }
                        };
                        push_completion(Ok(NormalCompletion::ClassItem(Box::new(transformed)))).expect(PUSHABLE);
                    }
                }
            }
            Err(err) => {
                push_completion(Err(err)).expect(PUSHABLE);
            }
        }
        Ok(())
    }
}

pub fn execute_synchronously(text: &str) -> Completion<ECMAScriptValue> {
    match Gen::new(|co| execute(co, text.to_owned())).resume_with(Ok(ECMAScriptValue::Undefined)) {
        genawaiter::GeneratorState::Yielded(val) => panic!("Yielded from synchronous context with value {val}"),
        genawaiter::GeneratorState::Complete(val) => val,
    }
}

pub async fn execute(
    co: Co<ECMAScriptValue, Completion<ECMAScriptValue>>,
    text: String,
) -> Completion<ECMAScriptValue> {
    const GOODCODE: &str = "code should have been properly compiled";

    // If our ec index drops below this, we exit.
    let initial_context_index = AGENT.with(|agent| agent.execution_context_stack.borrow().len() - 1);

    loop {
        let index = AGENT.with(|agent| agent.execution_context_stack.borrow().len() - 1);
        assert!(index <= RECURSION_LIMIT, "Recursion limit exceeded");
        /* Diagnostics */
        print!("[{index:2}] Stack: [ ");
        AGENT.with(|agent| {
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
        });
        println!(" ]");

        if index < initial_context_index {
            break;
        }

        let chunk = AGENT
            .with(|agent| agent.execution_context_stack.borrow()[index].chunk.clone())
            .expect("A running execution context should have code");

        if AGENT.with(|agent| agent.execution_context_stack.borrow()[index].pc) >= chunk.opcodes.len() {
            break;
        }
        let (_, repr) = chunk.insn_repr_at(AGENT.with(|agent| agent.execution_context_stack.borrow()[index].pc));
        AGENT.with(|agent| {
            println!("{:04}{}", agent.execution_context_stack.borrow()[index].pc, repr);
        });

        /* Real work */
        let icode = chunk.opcodes[AGENT.with(|agent| agent.execution_context_stack.borrow()[index].pc)]; // in range due to while condition
        let instruction = Insn::try_from(icode).unwrap(); // failure is a coding error (the compiler broke)
        AGENT.with(|agent| {
            agent.execution_context_stack.borrow_mut()[index].pc += 1;
        });
        match instruction {
            Insn::Nop => insn_impl::nop(),
            Insn::ToDo => insn_impl::todo(),
            Insn::String => insn_impl::string(&chunk).expect(GOODCODE),
            Insn::Null => insn_impl::null().expect(GOODCODE),
            Insn::True => insn_impl::true_val().expect(GOODCODE),
            Insn::False => insn_impl::false_val().expect(GOODCODE),
            Insn::Zero => insn_impl::zero().expect(GOODCODE),
            Insn::Empty => insn_impl::empty().expect(GOODCODE),
            Insn::EmptyIfNotError => insn_impl::empty_if_not_error().expect(GOODCODE),
            Insn::Undefined => insn_impl::undefined().expect(GOODCODE),
            Insn::This => insn_impl::this().expect(GOODCODE),
            Insn::Resolve => insn_impl::resolve(false).expect(GOODCODE),
            Insn::StrictResolve => insn_impl::resolve(true).expect(GOODCODE),
            Insn::Float => insn_impl::float(&chunk).expect(GOODCODE),
            Insn::Bigint => insn_impl::bigint(&chunk).expect(GOODCODE),
            Insn::GetValue => insn_impl::get_value().expect(GOODCODE),
            Insn::PutValue => insn_impl::put_value().expect(GOODCODE),
            Insn::FunctionPrototype => insn_impl::function_prototype().expect(GOODCODE),
            Insn::ObjectPrototype => insn_impl::object_prototype().expect(GOODCODE),
            Insn::JumpIfAbrupt => insn_impl::jump_if_abrupt(&chunk).expect(GOODCODE),
            Insn::JumpIfNormal => insn_impl::jump_if_normal(&chunk).expect(GOODCODE),
            Insn::JumpIfFalse => insn_impl::jump_referencing_bool(false, &chunk).expect(GOODCODE),
            Insn::JumpIfTrue => insn_impl::jump_referencing_bool(true, &chunk).expect(GOODCODE),
            Insn::JumpPopIfFalse => insn_impl::jump_taking_bool(false, &chunk).expect(GOODCODE),
            Insn::JumpPopIfTrue => insn_impl::jump_taking_bool(true, &chunk).expect(GOODCODE),
            Insn::JumpIfNotNullish => insn_impl::jump_nullishness(false, &chunk).expect(GOODCODE),
            Insn::JumpIfNullish => insn_impl::jump_nullishness(true, &chunk).expect(GOODCODE),
            Insn::JumpIfNotUndef => insn_impl::jump_undefness(false, &chunk).expect(GOODCODE),
            Insn::JumpNotThrow => insn_impl::jump_throwyness(false, &chunk).expect(GOODCODE),
            Insn::Jump => insn_impl::jump(&chunk).expect(GOODCODE),
            Insn::UpdateEmpty => insn_impl::update_empty().expect(GOODCODE),
            Insn::Pop2Push3 => insn_impl::pop2push3().expect(GOODCODE),
            Insn::RotateUp => insn_impl::rotate_up(&chunk).expect(GOODCODE),
            Insn::RotateDown => insn_impl::rotate_down(&chunk).expect(GOODCODE),
            Insn::RotateDownList => insn_impl::rotate_down_list(&chunk).expect(GOODCODE),
            Insn::RotateListDown => insn_impl::rotate_list_down(&chunk).expect(GOODCODE),
            Insn::RotateListUp => insn_impl::rotate_list_up(&chunk).expect(GOODCODE),
            Insn::Ref => insn_impl::make_ref(false).expect(GOODCODE),
            Insn::StrictRef => insn_impl::make_ref(true).expect(GOODCODE),
            Insn::MakeSuperPropertyReference => insn_impl::make_super_property_ref(&chunk).expect(GOODCODE),
            Insn::Pop => insn_impl::pop().expect(GOODCODE),
            Insn::PopOrPanic => insn_impl::pop_or_panic().expect(GOODCODE),
            Insn::Swap => insn_impl::swap().expect(GOODCODE),
            Insn::SwapList => insn_impl::swap_list().expect(GOODCODE),
            Insn::PopList => insn_impl::pop_list().expect(GOODCODE),
            Insn::PopOutList => insn_impl::pop_out_list(&chunk).expect(GOODCODE),
            Insn::SwapDeepList => insn_impl::swap_deep_list().expect(GOODCODE),
            Insn::ListToArray => insn_impl::list_to_array().expect(GOODCODE),
            Insn::InitializeReferencedBinding => insn_impl::initialize_referenced_binding().expect(GOODCODE),
            Insn::PushNewLexEnv => insn_impl::push_new_lexical_environment(),
            Insn::PopLexEnv => insn_impl::pop_lexical_environment().expect(GOODCODE),
            Insn::PushNewVarEnvFromLex => insn_impl::push_new_var_env_from_lex(),
            Insn::PushNewLexEnvFromVar => insn_impl::push_new_lex_env_from_var(),
            Insn::SetLexEnvToVarEnv => insn_impl::set_lex_env_to_var_env(),
            Insn::SetAsideLexEnv => insn_impl::set_aside_lex_env().expect(GOODCODE),
            Insn::RestoreLexEnv => insn_impl::restore_lex_env().expect(GOODCODE),
            Insn::PushNewPrivateEnv => insn_impl::push_new_private_env(),
            Insn::PopPrivateEnv => insn_impl::pop_private_env().expect(GOODCODE),
            Insn::CreateStrictImmutableLexBinding => {
                insn_impl::create_immutable_lex_binding(true, &chunk).expect(GOODCODE);
            }
            Insn::CreateNonStrictImmutableLexBinding => {
                insn_impl::create_immutable_lex_binding(false, &chunk).expect(GOODCODE);
            }
            Insn::CreatePermanentMutableLexBinding => {
                insn_impl::create_mutable_lexical_binding(false, &chunk).expect(GOODCODE);
            }
            Insn::CreatePermanentMutableVarBinding => {
                insn_impl::create_mutable_var_binding(false, &chunk).expect(GOODCODE);
            }
            Insn::CreatePermanentMutableLexIfMissing => {
                insn_impl::create_mutable_lexical_binding_if_missing(&chunk).expect(GOODCODE);
            }
            Insn::CreateInitializedPermanentMutableLexIfMissing => {
                insn_impl::create_initialized_permanent_mutable_lexical_binding_if_missing(&chunk).expect(GOODCODE);
            }
            Insn::InitializeLexBinding => insn_impl::initialize_lex_binding(&chunk).expect(GOODCODE),
            Insn::InitializeVarBinding => insn_impl::initialize_var_binding(&chunk).expect(GOODCODE),
            Insn::GetLexBinding => insn_impl::get_lex_binding(&chunk).expect(GOODCODE),
            Insn::SetMutableVarBinding => insn_impl::set_mutable_var_binding(&chunk).expect(GOODCODE),
            Insn::CreatePrivateNameIfMissing => insn_impl::create_private_name_if_missing(&chunk).expect(GOODCODE),
            Insn::CreatePerIterationEnvironment => {
                insn_impl::create_per_iteration_environment(&chunk).expect(GOODCODE);
            }
            Insn::ExtractThrownValue => insn_impl::extract_thrown_value().expect(GOODCODE),
            Insn::ExtractArg => insn_impl::extract_arg().expect(GOODCODE),
            Insn::FinishArgs => insn_impl::finish_args().expect(GOODCODE),
            Insn::Object => insn_impl::object().expect(GOODCODE),
            Insn::ObjectWithProto => insn_impl::object_with_proto().expect(GOODCODE),
            Insn::Array => insn_impl::array().expect(GOODCODE),
            Insn::CreateDataProperty => insn_impl::create_data_property().expect(GOODCODE),
            Insn::SetPrototype => insn_impl::set_prototype().expect(GOODCODE),
            Insn::ToPropertyKey => insn_impl::to_property_key().expect(GOODCODE),
            Insn::CopyDataProps => insn_impl::copy_data_props(&[]).expect(GOODCODE),
            Insn::CopyDataPropsWithExclusions => insn_impl::copy_data_props_with_exclusions().expect(GOODCODE),
            Insn::Dup => insn_impl::dup().expect(GOODCODE),
            Insn::DupAfterList => insn_impl::dup_after_list().expect(GOODCODE),
            Insn::ToString => insn_impl::to_string().expect(GOODCODE),
            Insn::ToNumeric => insn_impl::to_numeric().expect(GOODCODE),
            Insn::ToObject => insn_impl::to_object().expect(GOODCODE),
            Insn::Increment => insn_impl::increment().expect(GOODCODE),
            Insn::Decrement => insn_impl::decrement().expect(GOODCODE),
            Insn::PreIncrement => insn_impl::pre_increment().expect(GOODCODE),
            Insn::PreDecrement => insn_impl::pre_decrement().expect(GOODCODE),
            Insn::Delete => insn_impl::delete().expect(GOODCODE),
            Insn::Void => insn_impl::void().expect(GOODCODE),
            Insn::TypeOf => insn_impl::type_of().expect(GOODCODE),
            Insn::Unwind => insn_impl::unwind(&chunk).expect(GOODCODE),
            Insn::UnwindIfAbrupt => insn_impl::unwind_if_abrupt(&chunk).expect(GOODCODE),
            Insn::UnwindList => insn_impl::unwind_list().expect(GOODCODE),
            Insn::AppendList => insn_impl::append_list().expect(GOODCODE),
            Insn::Call => insn_impl::call(false).expect(GOODCODE),
            Insn::StrictCall => insn_impl::call(true).expect(GOODCODE),
            Insn::EndFunction => insn_impl::end_function().expect(GOODCODE),
            Insn::Construct => insn_impl::construct().expect(GOODCODE),
            Insn::RequireConstructor => insn_impl::require_constructor().expect(GOODCODE),
            Insn::Return => insn_impl::return_insn().expect(GOODCODE),
            Insn::UnaryPlus => insn_impl::unary_plus().expect(GOODCODE),
            Insn::UnaryMinus => insn_impl::unary_minus().expect(GOODCODE),
            Insn::UnaryComplement => insn_impl::unary_complement().expect(GOODCODE),
            Insn::UnaryNot => insn_impl::unary_not().expect(GOODCODE),
            Insn::Exponentiate => insn_impl::binary_operation(BinOp::Exponentiate).expect(GOODCODE),
            Insn::Multiply => insn_impl::binary_operation(BinOp::Multiply).expect(GOODCODE),
            Insn::Divide => insn_impl::binary_operation(BinOp::Divide).expect(GOODCODE),
            Insn::Modulo => insn_impl::binary_operation(BinOp::Remainder).expect(GOODCODE),
            Insn::Add => insn_impl::binary_operation(BinOp::Add).expect(GOODCODE),
            Insn::Subtract => insn_impl::binary_operation(BinOp::Subtract).expect(GOODCODE),
            Insn::InstantiateIdFreeFunctionExpression => {
                insn_impl::instantiate_id_free_function_expression(&chunk, &text).expect(GOODCODE);
            }
            Insn::InstantiateOrdinaryFunctionExpression => {
                insn_impl::instantiate_ordinary_function_expression(&chunk, &text).expect(GOODCODE);
            }
            Insn::InstantiateArrowFunctionExpression => {
                insn_impl::instantiate_arrow_function_expression(&chunk, &text).expect(GOODCODE);
            }
            Insn::InstantiateGeneratorFunctionExpression => {
                insn_impl::instantiate_generator_function_expression(&chunk, &text).expect(GOODCODE);
            }
            Insn::InstantiateOrdinaryFunctionObject => {
                insn_impl::instantiate_ordinary_function_object(&chunk, &text).expect(GOODCODE);
            }
            Insn::InstantiateGeneratorFunctionObject => {
                insn_impl::instantiate_generator_function_object(&chunk, &text).expect(GOODCODE);
            }
            Insn::InstantiateGeneratorMethod => insn_impl::instantiate_generator_method(&chunk, &text).expect(GOODCODE),
            Insn::LeftShift => insn_impl::binary_operation(BinOp::LeftShift).expect(GOODCODE),
            Insn::SignedRightShift => insn_impl::binary_operation(BinOp::SignedRightShift).expect(GOODCODE),
            Insn::UnsignedRightShift => insn_impl::binary_operation(BinOp::UnsignedRightShift).expect(GOODCODE),
            Insn::Throw => insn_impl::throw().expect(GOODCODE),
            Insn::Less => insn_impl::compare(false, false).expect(GOODCODE),
            Insn::Greater => insn_impl::compare(true, false).expect(GOODCODE),
            Insn::LessEqual => insn_impl::compare(true, true).expect(GOODCODE),
            Insn::GreaterEqual => insn_impl::compare(false, true).expect(GOODCODE),
            Insn::InstanceOf => insn_impl::instance_of().expect(GOODCODE),
            Insn::In => insn_impl::in_op().expect(GOODCODE),
            Insn::Equal => insn_impl::equal(false).expect(GOODCODE),
            Insn::NotEqual => insn_impl::equal(true).expect(GOODCODE),
            Insn::StrictEqual => insn_impl::strict_equal(false).expect(GOODCODE),
            Insn::StrictNotEqual => insn_impl::strict_equal(true).expect(GOODCODE),
            Insn::BitwiseAnd => insn_impl::binary_operation(BinOp::BitwiseAnd).expect(GOODCODE),
            Insn::BitwiseOr => insn_impl::binary_operation(BinOp::BitwiseOr).expect(GOODCODE),
            Insn::BitwiseXor => insn_impl::binary_operation(BinOp::BitwiseXor).expect(GOODCODE),
            Insn::CreateUnmappedArguments => insn_impl::create_unmapped_arguments_object().expect(GOODCODE),
            Insn::CreateMappedArguments => insn_impl::create_mapped_arguments_object().expect(GOODCODE),
            Insn::AddMappedArgument => insn_impl::add_mapped_argument(&chunk).expect(GOODCODE),
            Insn::HandleEmptyBreak => insn_impl::handle_empty_break().expect(GOODCODE),
            Insn::HandleTargetedBreak => insn_impl::handle_targeted_break(&chunk).expect(GOODCODE),
            Insn::CoalesceValue => insn_impl::coalesce_value().expect(GOODCODE),
            Insn::LoopContinues => insn_impl::loop_continues(&chunk).expect(GOODCODE),
            Insn::Continue => insn_impl::continue_insn().expect(GOODCODE),
            Insn::TargetedContinue => insn_impl::targeted_continue(&chunk).expect(GOODCODE),
            Insn::Break => insn_impl::break_insn().expect(GOODCODE),
            Insn::TargetedBreak => insn_impl::targeted_break(&chunk).expect(GOODCODE),
            Insn::IteratorAccumulate => insn_impl::iterator_accumulate().expect(GOODCODE),
            Insn::IterateArguments => insn_impl::iterate_arguments().expect(GOODCODE),
            Insn::IteratorDAEElision => insn_impl::elision_idae().expect(GOODCODE),
            Insn::EmbellishedIteratorStep => insn_impl::embellished_iterator_step().expect(GOODCODE),
            Insn::IteratorRest => insn_impl::iterator_rest().expect(GOODCODE),
            Insn::RequireCoercible => insn_impl::require_coercible().expect(GOODCODE),
            Insn::GetSyncIterator => insn_impl::get_sync_iterator().expect(GOODCODE),
            Insn::IteratorClose => insn_impl::iterator_close().expect(GOODCODE),
            Insn::IteratorCloseIfNotDone => insn_impl::iterator_close_if_not_done().expect(GOODCODE),
            Insn::IteratorNext => insn_impl::iterator_next().expect(GOODCODE),
            Insn::IteratorResultComplete => insn_impl::iterator_result_complete().expect(GOODCODE),
            Insn::IteratorResultToValue => insn_impl::iterator_result_to_value().expect(GOODCODE),
            Insn::GetV => insn_impl::getv().expect(GOODCODE),
            Insn::EnumerateObjectProperties => insn_impl::enumerate_object_properties().expect(GOODCODE),
            Insn::PrivateIdLookup => insn_impl::private_id_lookup(&chunk).expect(GOODCODE),
            Insn::EvaluateInitializedClassFieldDefinition => {
                insn_impl::evaluate_initialized_class_field_def(&chunk, &text, Static::No).expect(GOODCODE);
            }
            Insn::EvaluateInitializedClassStaticFieldDefinition => {
                insn_impl::evaluate_initialized_class_field_def(&chunk, &text, Static::Yes).expect(GOODCODE);
            }
            Insn::EvaluateClassStaticBlockDefinition => {
                insn_impl::evaluate_class_static_block_def(&chunk, &text).expect(GOODCODE);
            }
            Insn::DefineMethod => insn_impl::define_method(&chunk, &text).expect(GOODCODE),
            Insn::SetFunctionName => insn_impl::set_function_name().expect(GOODCODE),
            Insn::DefineMethodProperty => insn_impl::define_method_property(&chunk).expect(GOODCODE),
            Insn::DefineGetter => insn_impl::define_getter(&chunk, &text).expect(GOODCODE),
            Insn::DefineSetter => insn_impl::define_setter(&chunk, &text).expect(GOODCODE),
            Insn::GetParentsFromSuperclass => insn_impl::get_parents_from_superclass().expect(GOODCODE),
            Insn::CreateDefaultConstructor => insn_impl::create_default_constructor().expect(GOODCODE),
            Insn::MakeClassConstructorAndSetName => insn_impl::make_class_constructor_and_set_name().expect(GOODCODE),
            Insn::MakeConstructor => insn_impl::make_constructor().expect(GOODCODE),
            Insn::MakeConstructorWithProto => insn_impl::make_constructor_with_proto().expect(GOODCODE),
            Insn::SetDerived => insn_impl::set_derived().expect(GOODCODE),
            Insn::AttachElements => insn_impl::attach_elements(&chunk).expect(GOODCODE),
            Insn::AttachSourceText => insn_impl::attach_source_text(&chunk).expect(GOODCODE),
            Insn::GeneratorStartFromFunction => insn_impl::generator_start_from_function(&text).expect(GOODCODE),
            Insn::Yield => insn_impl::yield_insn(&co).await.expect(GOODCODE),
            Insn::NameOnlyFieldRecord => insn_impl::name_only_field_record(Static::No).expect(GOODCODE),
            Insn::NameOnlyStaticFieldRecord => insn_impl::name_only_field_record(Static::Yes).expect(GOODCODE),
            Insn::MakePrivateReference => insn_impl::make_private_reference(&chunk).expect(GOODCODE),
            Insn::GetNewTarget => insn_impl::get_new_target().expect(GOODCODE),
            Insn::GetSuperConstructor => insn_impl::get_super_constructor().expect(GOODCODE),
            Insn::ConstructorCheck => insn_impl::constructor_check().expect(GOODCODE),
            Insn::BindThisAndInit => insn_impl::bind_this_and_init().expect(GOODCODE),
            Insn::StaticClassItem => insn_impl::static_class_item().expect(GOODCODE),
        }
    }

    AGENT.with(|agent| {
        agent.execution_context_stack.borrow_mut().last_mut().unwrap().stack.pop().map_or(
            Ok(ECMAScriptValue::Undefined),
            |svr| {
                svr.map(|sv| match sv {
                    NormalCompletion::Reference(_) | NormalCompletion::Empty => ECMAScriptValue::Undefined,
                    NormalCompletion::Value(v) => v,
                    NormalCompletion::IteratorRecord(_)
                    | NormalCompletion::Environment(..)
                    | NormalCompletion::PrivateName(_)
                    | NormalCompletion::PrivateElement(_)
                    | NormalCompletion::ClassItem(_) => unreachable!(),
                })
            },
        )
    })
}

fn begin_call_evaluation(
    func: &ECMAScriptValue,
    reference: &NormalCompletion,
    arguments: &[ECMAScriptValue],
) -> Result<(), InternalRuntimeError> {
    let this_value = match reference {
        NormalCompletion::IteratorRecord(_)
        | NormalCompletion::Empty
        | NormalCompletion::Environment(..)
        | NormalCompletion::PrivateName(_)
        | NormalCompletion::PrivateElement(_)
        | NormalCompletion::ClassItem(_) => {
            return Err(InternalRuntimeError::ValueOrReferenceExpected);
        }
        NormalCompletion::Value(_) => ECMAScriptValue::Undefined,
        NormalCompletion::Reference(r) => match &r.base {
            Base::Unresolvable => {
                return Err(InternalRuntimeError::UnresolvableReference);
            }
            Base::Environment(e) => e.with_base_object().map_or(ECMAScriptValue::Undefined, ECMAScriptValue::from),
            Base::Value(_) => r.get_this_value(),
        },
    };
    if !func.is_object() {
        let err = Err(create_type_error("not an object"));
        ec_push(err);
        return Ok(());
    }
    if !is_callable(func) {
        let err = Err(create_type_error("not a function"));
        ec_push(err);
        return Ok(());
    }
    initiate_call(func, &this_value, arguments);
    Ok(())
}

fn begin_constructor_evaluation(
    cstr: ECMAScriptValue,
    newtgt: ECMAScriptValue,
    args: &[ECMAScriptValue],
) -> anyhow::Result<()> {
    assert!(is_constructor(&cstr));
    let cstr = Object::try_from(cstr).context("getting a constructor from a value")?;
    let newtgt = Object::try_from(newtgt).context("getting new.target from a value")?;
    initiate_construct(&cstr, args, Some(&newtgt));
    Ok(())
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
    // Runtime Semantics: Evaluation
    // UnaryExpression : delete UnaryExpression
    // 1. Let ref be ? Evaluation of UnaryExpression.
    // 2. If ref is not a Reference Record, return true.
    // 3. If IsUnresolvableReference(ref) is true, then
    //    a. Assert: ref.[[Strict]] is false.
    //    b. Return true.
    // 4. If IsPropertyReference(ref) is true, then
    //    a. Assert: IsPrivateReference(ref) is false.
    //    b. If IsSuperReference(ref) is true, throw a ReferenceError exception.
    //    c. Let baseObj be ? ToObject(ref.[[Base]]).
    //    d. If ref.[[ReferencedName]] is not a property key, then
    //       i. Set ref.[[ReferencedName]] to ? ToPropertyKey(ref.[[ReferencedName]]).
    //    e. Let deleteStatus be ? baseObj.[[Delete]](ref.[[ReferencedName]]).
    //    f. If deleteStatus is false and ref.[[Strict]] is true, throw a TypeError exception.
    //    g. Return deleteStatus.
    // 5. Else,
    //    a. Let base be ref.[[Base]].
    //    b. Assert: base is an Environment Record.
    //    c. Return ? base.DeleteBinding(ref.[[ReferencedName]]).
    let reference = expr?;
    match reference {
        NormalCompletion::IteratorRecord(_)
        | NormalCompletion::Environment(..)
        | NormalCompletion::Empty
        | NormalCompletion::PrivateName(_)
        | NormalCompletion::PrivateElement(_)
        | NormalCompletion::ClassItem(_)
        | NormalCompletion::Value(_) => Ok(true.into()),
        NormalCompletion::Reference(r) => match *r {
            Reference { base: Base::Unresolvable, .. } => Ok(true.into()),
            Reference { base: Base::Value(_), this_value: Some(_), .. } => {
                Err(create_reference_error("super properties not deletable"))
            }
            Reference { base: Base::Value(val), referenced_name, strict, this_value: None } => {
                let base_obj = to_object(val)?;
                let name_value =
                    ECMAScriptValue::try_from(referenced_name).expect("Property name will never be private");
                let referenced_name = match name_value {
                    ECMAScriptValue::String(jsstring) => PropertyKey::from(jsstring),
                    ECMAScriptValue::Symbol(symbol) => PropertyKey::from(symbol),
                    ECMAScriptValue::Undefined
                    | ECMAScriptValue::Null
                    | ECMAScriptValue::Boolean(_)
                    | ECMAScriptValue::Number(_)
                    | ECMAScriptValue::BigInt(_)
                    | ECMAScriptValue::Object(_) => to_property_key(name_value)?,
                };
                let delete_status = base_obj.o.delete(&referenced_name)?;
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

fn apply_string_or_numeric_binary_operator(left: ECMAScriptValue, right: ECMAScriptValue, op: BinOp) -> FullCompletion {
    let (left, right) = if op == BinOp::Add {
        let left_prim = to_primitive(left, None)?;
        let right_prim = to_primitive(right, None)?;
        if left_prim.is_string() || right_prim.is_string() {
            let left_str = to_string(left_prim)?;
            let right_str = to_string(right_prim)?;
            return Ok(NormalCompletion::from(left_str.concat(right_str)));
        }
        (left_prim, right_prim)
    } else {
        (left, right)
    };
    let left_num = to_numeric(left)?;
    let right_num = to_numeric(right)?;
    match (left_num, right_num, op) {
        (Numeric::Number(left), Numeric::Number(right), BinOp::Exponentiate) => {
            Ok(NormalCompletion::from(exponentiate(left, right)))
        }
        (Numeric::Number(left), Numeric::Number(right), BinOp::Multiply) => Ok(NormalCompletion::from(left * right)),
        (Numeric::Number(left), Numeric::Number(right), BinOp::Divide) => Ok(NormalCompletion::from(left / right)),
        (Numeric::Number(left), Numeric::Number(right), BinOp::Remainder) => Ok(NormalCompletion::from(left % right)),
        (Numeric::Number(left), Numeric::Number(right), BinOp::Add) => Ok(NormalCompletion::from(left + right)),
        (Numeric::Number(left), Numeric::Number(right), BinOp::Subtract) => Ok(NormalCompletion::from(left - right)),
        (Numeric::Number(left), Numeric::Number(right), BinOp::LeftShift) => {
            let left_num = to_int32_f64(left);
            let right_num = to_uint32_f64(right);
            let shift_count = right_num % 32;
            Ok(NormalCompletion::from(left_num << shift_count))
        }
        (Numeric::Number(left), Numeric::Number(right), BinOp::SignedRightShift) => {
            let left_num = to_int32_f64(left);
            let right_num = to_uint32_f64(right);
            let shift_count = right_num % 32;
            Ok(NormalCompletion::from(left_num >> shift_count))
        }
        (Numeric::Number(left), Numeric::Number(right), BinOp::UnsignedRightShift) => {
            let left_num = to_uint32_f64(left);
            let right_num = to_uint32_f64(right);
            let shift_count = right_num % 32;
            Ok(NormalCompletion::from(left_num >> shift_count))
        }
        (Numeric::Number(left), Numeric::Number(right), BinOp::BitwiseAnd) => {
            let left_num = to_int32_f64(left);
            let right_num = to_int32_f64(right);
            Ok(NormalCompletion::from(left_num & right_num))
        }
        (Numeric::Number(left), Numeric::Number(right), BinOp::BitwiseOr) => {
            let left_num = to_int32_f64(left);
            let right_num = to_int32_f64(right);
            Ok(NormalCompletion::from(left_num | right_num))
        }
        (Numeric::Number(left), Numeric::Number(right), BinOp::BitwiseXor) => {
            let left_num = to_int32_f64(left);
            let right_num = to_int32_f64(right);
            Ok(NormalCompletion::from(left_num ^ right_num))
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
            .map_or_else(|| Err(create_range_error("Division by zero")), Ok),
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

fn evaluate_initialized_class_field_definition(
    info: &StashedFunctionData,
    home_object: Object,
    name: Option<ClassName>,
    text: &str,
) -> anyhow::Result<Object> {
    // Pieces from ClassFieldDefinitionEvaluation
    //  1. Let env be the LexicalEnvironment of the running execution context.
    //  2. Let privateEnv be the running execution context's PrivateEnvironment.
    //  3. Let initializer be OrdinaryFunctionCreate(%Function.prototype%, sourceText, formalParameterList,
    //     Initializer, NON-LEXICAL-THIS, env, privateEnv).
    //  4. Perform MakeMethod(initializer, homeObject).
    //  5. Set initializer.[[ClassFieldInitializerName]] to name.
    //  6. Return initializer.

    let to_compile: Rc<FieldDefinition> =
        info.to_compile.clone().try_into().expect("This routine only used with class field definitions");
    let prod_text_loc = to_compile.location().span;
    let prod_text = &text[prod_text_loc.starting_index..prod_text_loc.starting_index + prod_text_loc.length];
    let chunk_name = nameify(prod_text, 50);
    let mut compiled = Chunk::new(chunk_name);
    to_compile.init.as_ref().unwrap().compile(&mut compiled, info.strict, text, CompileMod::AsFunction)?;
    for line in compiled.disassemble() {
        println!("{line}");
    }

    let env = current_lexical_environment().unwrap();
    let priv_env = current_private_environment();
    let function_prototype = intrinsic(IntrinsicId::FunctionPrototype);
    let initializer = ordinary_function_create(
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

    make_method(initializer.o.to_function_obj().unwrap(), home_object);
    initializer.o.to_function_obj().unwrap().function_data().borrow_mut().class_field_initializer_name = name;

    Ok(initializer)
}

fn evaluate_class_static_block_definition(
    info: &StashedFunctionData,
    home_object: Object,
    text: &str,
) -> anyhow::Result<Object> {
    // Pieces from ClassStaticBlockDefinitionEvaluation
    //  1. Let lex be the running execution context's LexicalEnvironment.
    //  2. Let privateEnv be the running execution context's PrivateEnvironment.
    //  3. Let bodyFunction be OrdinaryFunctionCreate(%Function.prototype%, sourceText, formalParameters, ClassStaticBlockBody, NON-LEXICAL-THIS, lex, privateEnv).
    //  4. Perform MakeMethod(bodyFunction, homeObject).
    //  5. Return the ClassStaticBlockDefinition Record { [[BodyFunction]]: bodyFunction }.
    let to_compile: Rc<ClassStaticBlock> = info.to_compile.clone().try_into()?;
    let prod_text_loc = to_compile.location().span;
    let prod_text = &text[prod_text_loc.starting_index..prod_text_loc.starting_index + prod_text_loc.length];
    let chunk_name = nameify(prod_text, 50);
    let mut compiled = Chunk::new(chunk_name);
    to_compile.block.as_ref().compile(&mut compiled, text)?;
    for line in compiled.disassemble() {
        println!("{line}");
    }

    let lex = current_lexical_environment().unwrap();
    let private_env = current_private_environment();
    let function_prototype = intrinsic(IntrinsicId::FunctionPrototype);
    let body_function = ordinary_function_create(
        function_prototype,
        info.source_text.as_str(),
        info.params.clone(),
        info.body.clone(),
        ThisLexicality::NonLexicalThis,
        lex,
        private_env,
        info.strict,
        Rc::new(compiled),
    );

    make_method(body_function.o.to_function_obj().unwrap(), home_object);
    Ok(body_function)
}

fn define_method_property(
    home_object: &Object,
    key: FunctionName,
    closure: Object,
    enumerable: bool,
) -> Completion<Option<PrivateElement>> {
    // DefineMethodProperty ( homeObject, key, closure, enumerable )
    // The abstract operation DefineMethodProperty takes arguments homeObject (an Object), key (a property key
    // or Private Name), closure (a function object), and enumerable (a Boolean) and returns either a normal
    // completion containing either a PrivateElement or unused, or an abrupt completion. It performs the
    // following steps when called:
    //
    // 1. Assert: homeObject is an ordinary, extensible object.
    // 2. If key is a Private Name, then
    //    a. Return PrivateElement { [[Key]]: key, [[Kind]]: method, [[Value]]: closure }.
    // 3. Else,
    //    a. Let desc be the PropertyDescriptor { [[Value]]: closure, [[Writable]]: true, [[Enumerable]]:
    //       enumerable, [[Configurable]]: true }.
    //    b. Perform ? DefinePropertyOrThrow(homeObject, key, desc).
    //    c. NOTE: DefinePropertyOrThrow only returns an abrupt completion when attempting to define a class
    //       static method whose key is "prototype".
    //    d. Return unused.

    match key {
        FunctionName::String(_) | FunctionName::Symbol(_) => {
            let ppd = PotentialPropertyDescriptor::new()
                .value(closure)
                .writable(true)
                .enumerable(enumerable)
                .configurable(true);
            let key = PropertyKey::try_from(key).expect("strings and symbols should convert just fine");
            define_property_or_throw(home_object, key, ppd)?;
            Ok(None)
        }
        FunctionName::PrivateName(pn) => {
            Ok(Some(PrivateElement { key: pn, kind: PrivateElementKind::Method { value: closure.into() } }))
        }
    }
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

pub fn instanceof_operator(v: ECMAScriptValue, target: &ECMAScriptValue) -> Completion<bool> {
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
    match target {
        ECMAScriptValue::Object(_) => {
            let hi = wks(WksId::HasInstance);
            let instof_handler = target.get_method(&hi.into())?;
            match &instof_handler {
                ECMAScriptValue::Undefined => {
                    if is_callable(target) {
                        ordinary_has_instance(target, &v)
                    } else {
                        Err(create_type_error("Right-hand side of 'instanceof' is not callable"))
                    }
                }
                _ => {
                    let res = call(&instof_handler, target, &[v])?;
                    Ok(to_boolean(res))
                }
            }
        }
        _ => Err(create_type_error("Right-hand side of 'instanceof' is not an object")),
    }
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
    let script = parse_text(source_text, ParseGoal::Script, false, false);
    let script: Result<Rc<Script>, Vec<Object>> =
        script.try_into().expect("The only types coming back from a Script parsegoal are Script or list-of-errors");
    match script {
        Err(errs) => Err(errs),
        Ok(script) => {
            let mut chunk = Chunk::new("top level script");
            script
                .compile(&mut chunk, false, source_text)
                .map_err(|err| vec![create_syntax_error_object(format!("{err}"), None)])?;
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
            FcnDef::Generator(x) => x.instantiate_function_object(env, private_env, strict, text),
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
    script: &Rc<Script>,
    env: &Rc<GlobalEnvironmentRecord>,
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

    let strict = script.body.as_ref().is_some_and(|b| b.contains_use_strict());

    let result = global_declaration_instantiation(&script, &global_env.unwrap(), strict, &sr.text)
        .and_then(|()| evaluate(sr.compiled, &sr.text));

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
                    if o.is_error_object() {
                        return write!(f, "Thrown: {}", unwind_any_error_object(o));
                    }
                    let maybe_message = to_string(o.into());
                    if let Ok(message) = maybe_message {
                        return write!(f, "Thrown: {message}");
                    }
                }
                write!(f, "Thrown: {error}")
            }
            ProcessError::CompileErrors { values } => {
                write!(f, "During compilation: ")?;
                let mut first = true;
                for err_obj in values {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "[{}]", unwind_any_error_object(err_obj))?;
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

/// Create a per-iteration environment for bindings in for loops
///
/// In `for (let ...)` loops, a new declarative environment is created for each iteration of the loop. This
/// essentially gives any closures created during the loop body different copies of the loop index. This
/// routine is the way that happens, copying any of the "control" variables from the prior environment over.
///
/// See [CreatePerIterationEnvironment](https://tc39.es/ecma262/#sec-createperiterationenvironment) in
/// ECMA-262.
pub fn create_per_iteration_environment(per_iteration_bindings: &AHashSet<JSString>) -> Completion<()> {
    // CreatePerIterationEnvironment ( perIterationBindings )
    // The abstract operation CreatePerIterationEnvironment takes argument perIterationBindings (a List of
    // Strings) and returns either a normal completion containing unused or a throw completion. It performs
    // the following steps when called:
    //
    //  1. If perIterationBindings has any elements, then
    //      a. Let lastIterationEnv be the running execution context's LexicalEnvironment.
    //      b. Let outer be lastIterationEnv.[[OuterEnv]].
    //      c. Assert: outer is not null.
    //      d. Let thisIterationEnv be NewDeclarativeEnvironment(outer).
    //      e. For each element bn of perIterationBindings, do
    //          i. Perform ! thisIterationEnv.CreateMutableBinding(bn, false).
    //          ii. Let lastValue be ? lastIterationEnv.GetBindingValue(bn, true).
    //          iii. Perform ! thisIterationEnv.InitializeBinding(bn, lastValue).
    //      f. Set the running execution context's LexicalEnvironment to thisIterationEnv.
    //  2. Return unused.
    if !per_iteration_bindings.is_empty() {
        let last_iteration_env = AGENT.with(|agent| {
            let ec_stack = agent.execution_context_stack.borrow();
            let running_execution_context = ec_stack.last().expect("There should be running code");
            running_execution_context
                .lexical_environment
                .as_ref()
                .expect("There should be a lexical environment")
                .clone()
        });
        let outer = last_iteration_env.get_outer_env().as_ref().expect("There should be an outer environent").clone();
        let this_iteration_env = DeclarativeEnvironmentRecord::new(Some(outer), "per-iter");
        for bn in per_iteration_bindings {
            this_iteration_env.create_mutable_binding(bn.clone(), false).expect("binding creation should succeed");
            let last_value = last_iteration_env.get_binding_value(bn, true)?;
            this_iteration_env.initialize_binding(bn, last_value).expect("binding initialization should succeed");
        }
        AGENT.with(|agent| {
            let mut ec_stack = agent.execution_context_stack.borrow_mut();
            let running_execution_context = ec_stack.last_mut().expect("There should be running code");
            running_execution_context.lexical_environment = Some(Rc::new(this_iteration_env));
        });
    }
    Ok(())
}

#[derive(Debug)]
struct ForInIteratorInternals {
    object: Object,
    object_was_visited: bool,
    visited_keys: Vec<PropertyKey>,
    remaining_keys: Vec<PropertyKey>,
}

#[derive(Debug)]
pub struct ForInIteratorObject {
    common: RefCell<CommonObjectData>,
    internals: RefCell<ForInIteratorInternals>,
}

impl ForInIteratorObject {
    pub fn new(proto: Option<Object>, obj: Object) -> Self {
        Self {
            common: RefCell::new(CommonObjectData::new(proto, true, FOR_IN_ITERATOR_SLOTS)),
            internals: RefCell::new(ForInIteratorInternals {
                object: obj,
                object_was_visited: false,
                visited_keys: Vec::new(),
                remaining_keys: Vec::new(),
            }),
        }
    }
    fn object(proto: Option<Object>, obj: Object) -> Object {
        Object { o: Rc::new(Self::new(proto, obj)) }
    }
}

impl ObjectInterface for ForInIteratorObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn uses_ordinary_get_prototype_of(&self) -> bool {
        true
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }
    fn to_for_in_iterator(&self) -> Option<&ForInIteratorObject> {
        Some(self)
    }

    // [[GetPrototypeOf]] ( )
    //
    // The [[GetPrototypeOf]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    //  1. Return ! OrdinaryGetPrototypeOf(O).
    fn get_prototype_of(&self) -> Completion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }

    // [[SetPrototypeOf]] ( V )
    //
    // The [[SetPrototypeOf]] internal method of an ordinary object O takes argument V (an Object or null). It performs
    // the following steps when called:
    //
    //  1. Return ! OrdinarySetPrototypeOf(O, V).
    fn set_prototype_of(&self, obj: Option<Object>) -> Completion<bool> {
        Ok(ordinary_set_prototype_of(self, obj))
    }

    // [[IsExtensible]] ( )
    //
    // The [[IsExtensible]] internal method of an ordinary object O takes no arguments. It performs the following steps
    // when called:
    //
    //  1. Return ! OrdinaryIsExtensible(O).
    fn is_extensible(&self) -> Completion<bool> {
        Ok(ordinary_is_extensible(self))
    }

    // [[PreventExtensions]] ( )
    //
    // The [[PreventExtensions]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    //  1. Return ! OrdinaryPreventExtensions(O).
    fn prevent_extensions(&self) -> Completion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }

    // [[GetOwnProperty]] ( P )
    //
    // The [[GetOwnProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ! OrdinaryGetOwnProperty(O, P).
    fn get_own_property(&self, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }

    // [[DefineOwnProperty]] ( P, Desc )
    //
    // The [[DefineOwnProperty]] internal method of an ordinary object O takes arguments P (a property key) and Desc (a
    // Property Descriptor). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryDefineOwnProperty(O, P, Desc).
    fn define_own_property(&self, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool> {
        ordinary_define_own_property(self, key, desc)
    }

    // [[HasProperty]] ( P )
    //
    // The [[HasProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryHasProperty(O, P).
    fn has_property(&self, key: &PropertyKey) -> Completion<bool> {
        ordinary_has_property(self, key)
    }

    // [[Get]] ( P, Receiver )
    //
    // The [[Get]] internal method of an ordinary object O takes arguments P (a property key) and Receiver (an
    // ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryGet(O, P, Receiver).
    fn get(&self, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        ordinary_get(self, key, receiver)
    }

    // [[Set]] ( P, V, Receiver )
    //
    // The [[Set]] internal method of an ordinary object O takes arguments P (a property key), V (an ECMAScript language
    // value), and Receiver (an ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinarySet(O, P, V, Receiver).
    fn set(&self, key: PropertyKey, v: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        ordinary_set(self, key, v, receiver)
    }

    // [[Delete]] ( P )
    //
    // The [[Delete]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryDelete(O, P).
    fn delete(&self, key: &PropertyKey) -> Completion<bool> {
        ordinary_delete(self, key)
    }

    // [[OwnPropertyKeys]] ( )
    //
    // The [[OwnPropertyKeys]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    // 1. Return ! OrdinaryOwnPropertyKeys(O).
    fn own_property_keys(&self) -> Completion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(self))
    }
}

impl<'a> From<&'a ForInIteratorObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a ForInIteratorObject) -> Self {
        obj
    }
}

fn create_for_in_iterator(obj: Object) -> Object {
    let prototype = intrinsic(IntrinsicId::ForInIteratorPrototype);
    ForInIteratorObject::object(Some(prototype), obj)
}

pub fn provision_for_in_iterator_prototype(realm: &Rc<RefCell<Realm>>) {
    // The %ForInIteratorPrototype% Object
    //
    //  * has properties that are inherited by all For-In Iterator Objects.
    //  * is an ordinary object.
    //  * has a [[Prototype]] internal slot whose value is %IteratorPrototype%.
    //  * is never directly accessible to ECMAScript code.

    let iterator_prototype = realm.borrow().intrinsics.iterator_prototype.clone();
    let for_in_iterator_prototype = ordinary_object_create(Some(iterator_prototype));

    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();
    macro_rules! prototype_function {
        ( $steps:expr, $name:expr, $length:expr ) => {
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(
                Box::new($steps),
                None,
                $length,
                key.clone(),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm.clone()),
                Some(function_prototype.clone()),
                None,
            );
            define_property_or_throw(
                &for_in_iterator_prototype,
                key,
                PotentialPropertyDescriptor::new()
                    .value(function_object)
                    .writable(true)
                    .enumerable(false)
                    .configurable(true),
            )
            .unwrap();
        };
    }
    prototype_function!(for_in_iterator_prototype_next, "next", 0.0);

    realm.borrow_mut().intrinsics.for_in_iterator_prototype = for_in_iterator_prototype;
}

fn for_in_iterator_prototype_next(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // %ForInIteratorPrototype%.next ( )
    //  1. Let O be the this value.
    //  2. Assert: O is an Object.
    //  3. Assert: O has all of the internal slots of a For-In Iterator Instance (14.7.5.10.3).
    //  4. Let object be O.[[Object]].
    //  5. Repeat,
    //      a. If O.[[ObjectWasVisited]] is false, then
    //          i. Let keys be ? object.[[OwnPropertyKeys]]().
    //          ii. For each element key of keys, do
    //              1. If key is a String, then
    //                  a. Append key to O.[[RemainingKeys]].
    //          iii. Set O.[[ObjectWasVisited]] to true.
    //      b. Repeat, while O.[[RemainingKeys]] is not empty,
    //          i. Let r be the first element of O.[[RemainingKeys]].
    //          ii. Remove the first element from O.[[RemainingKeys]].
    //          iii. If there does not exist an element v of O.[[VisitedKeys]] such that SameValue(r, v) is
    //               true, then
    //              1. Let desc be ? object.[[GetOwnProperty]](r).
    //              2. If desc is not undefined, then
    //                  a. Append r to O.[[VisitedKeys]].
    //                  b. If desc.[[Enumerable]] is true, return CreateIterResultObject(r, false).
    //      c. Set object to ? object.[[GetPrototypeOf]]().
    //      d. Set O.[[Object]] to object.
    //      e. Set O.[[ObjectWasVisited]] to false.
    //      f. If object is null, return CreateIterResultObject(undefined, true).
    let binding = Object::try_from(this_value).expect("this value should be an object");
    let o = binding.o.to_for_in_iterator().expect("object should be f-i iterator");
    let mut internals = o.internals.borrow_mut();
    let mut object = internals.object.clone();
    loop {
        if !internals.object_was_visited {
            let mut remaining = object
                .o
                .own_property_keys()?
                .into_iter()
                .filter(|key| matches!(key, &PropertyKey::String(_)))
                .collect::<Vec<_>>();
            remaining.reverse();
            internals.remaining_keys = remaining;
            internals.object_was_visited = true;
        }
        while let Some(r) = internals.remaining_keys.pop() {
            if !internals.visited_keys.contains(&r) {
                if let Some(desc) = object.o.get_own_property(&r)? {
                    internals.visited_keys.push(r.clone());
                    if desc.enumerable {
                        return Ok(create_iter_result_object(r.into(), false).into());
                    }
                }
            }
        }
        let parent = object.o.get_prototype_of()?;
        match parent {
            Some(parent) => {
                internals.object = parent;
                object = internals.object.clone();
                internals.object_was_visited = false;
            }
            None => {
                return Ok(create_iter_result_object(ECMAScriptValue::Undefined, true).into());
            }
        }
    }
}

pub fn default_constructor(
    _this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    //  i. Let args be the List of arguments that was passed to this function by [[Call]] or [[Construct]].
    //  ii. If NewTarget is undefined, throw a TypeError exception.
    //  iii. Let F be the active function object.
    //  iv. If F.[[ConstructorKind]] is derived, then
    //      1. NOTE: This branch behaves similarly to constructor(...args) { super(...args); }. The most
    //         notable distinction is that while the aforementioned ECMAScript source text observably calls
    //         the %Symbol.iterator% method on %Array.prototype%, this function does not.
    //      2. Let func be ! F.[[GetPrototypeOf]]().
    //      3. If IsConstructor(func) is false, throw a TypeError exception.
    //      4. Let result be ? Construct(func, args, NewTarget).
    //  v. Else,
    //      1. NOTE: This branch behaves similarly to constructor() {}.
    //      2. Let result be ? OrdinaryCreateFromConstructor(NewTarget, "%Object.prototype%").
    //  vi. Perform ? InitializeInstanceElements(result, F).
    //  vii. Return result.

    if let Some(nt) = new_target {
        let f = active_function_object().expect("a function should be active");
        // f points to *this function* which we know is a builtin.
        let func_interface = f.o.to_builtin_function_obj().expect("f should be a builtin function");
        let kind = func_interface.builtin_function_data().borrow().constructor_kind;
        let result = if kind == Some(ConstructorKind::Derived) {
            match f.o.get_prototype_of().expect("[[GetPrototypeOf]] f should complete successfully") {
                Some(func) if func.is_constructor() => Ok(Object::try_from(construct(&func, arguments, Some(nt))?)
                    .expect("construct should return an object")),
                _ => Err(create_type_error("not a constructor")),
            }
        } else {
            nt.ordinary_create_from_constructor(IntrinsicId::ObjectPrototype, ordinary_object_create)
        }?;
        initialize_instance_elements(&result, &f)?;
        Ok(ECMAScriptValue::Object(result))
    } else {
        Err(create_type_error("Class constructor canot be invoked without 'new'"))
    }
}

#[cfg(test)]
mod tests;
