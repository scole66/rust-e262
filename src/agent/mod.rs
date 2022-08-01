use super::*;
use anyhow::anyhow;
use itertools::Itertools;
use num::pow::Pow;
use num::{BigInt, BigUint, ToPrimitive, Zero};
use std::cell::RefCell;
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
    execution_context_stack: Vec<ExecutionContext>,
    pub symbols: WellKnownSymbols,
    obj_id: usize,
    pub symbol_id: usize,
    gsr: Rc<RefCell<SymbolRegistry>>,
}

impl Agent {
    pub fn new(gsr: Rc<RefCell<SymbolRegistry>>) -> Self {
        Agent {
            obj_id: 1,
            execution_context_stack: vec![],
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
            symbol_id: 14,
            gsr,
        }
    }

    pub fn active_function_object(&self) -> Option<Object> {
        match self.execution_context_stack.len() {
            0 => None,
            n => self.execution_context_stack[n - 1].function.clone(),
        }
    }

    /// Return the active script or module record associated with the current execution
    ///
    /// See [GetActiveScriptOrModule](https://tc39.es/ecma262/#sec-getactivescriptormodule) from ECMA-262.
    pub fn get_active_script_or_module(&self) -> Option<ScriptOrModule> {
        // GetActiveScriptOrModule ( )
        //
        // The abstract operation GetActiveScriptOrModule takes no arguments and returns a Script Record, a Module
        // Record, or null. It is used to determine the running script or module, based on the running execution
        // context. It performs the following steps when called:
        //
        //  1. If the execution context stack is empty, return null.
        //  2. Let ec be the topmost execution context on the execution context stack whose ScriptOrModule component is not null.
        //  3. If no such execution context exists, return null. Otherwise, return ec's ScriptOrModule.
        for ec in self.execution_context_stack.iter() {
            if let Some(script_or_module) = &ec.script_or_module {
                return Some(script_or_module.clone());
            }
        }
        None
    }

    pub fn next_object_id(&mut self) -> usize {
        let result = self.obj_id;
        assert!(result < usize::MAX);
        self.obj_id = result + 1;
        result
    }

    pub fn next_symbol_id(&mut self) -> usize {
        assert!(self.symbol_id < usize::MAX);
        let result = self.symbol_id;
        self.symbol_id += 1;
        result
    }

    pub fn push_execution_context(&mut self, context: ExecutionContext) {
        self.execution_context_stack.push(context)
    }

    pub fn pop_execution_context(&mut self) {
        self.execution_context_stack.pop();
    }

    pub fn ec_push(&mut self, val: FullCompletion) {
        let len = self.execution_context_stack.len();
        assert!(len > 0, "EC Push called with no active EC");
        let ec = &mut self.execution_context_stack[len - 1];
        ec.stack.push(val);
    }

    pub fn ec_pop(&mut self) -> Option<FullCompletion> {
        let len = self.execution_context_stack.len();
        match len {
            0 => None,
            _ => {
                let ec = &mut self.execution_context_stack[len - 1];
                ec.stack.pop()
            }
        }
    }

    pub fn ec_empty_stack(&self) -> bool {
        let len = self.execution_context_stack.len();
        match len {
            0 => true,
            _ => {
                let ec = &self.execution_context_stack[len - 1];
                ec.stack.is_empty()
            }
        }
    }

    pub fn wks(&self, sym_id: WksId) -> Symbol {
        match sym_id {
            WksId::AsyncIterator => &self.symbols.async_iterator_,
            WksId::HasInstance => &self.symbols.has_instance_,
            WksId::IsConcatSpreadable => &self.symbols.is_concat_spreadable_,
            WksId::Iterator => &self.symbols.iterator_,
            WksId::Match => &self.symbols.match_,
            WksId::MatchAll => &self.symbols.match_all_,
            WksId::Replace => &self.symbols.replace_,
            WksId::Search => &self.symbols.search_,
            WksId::Species => &self.symbols.species_,
            WksId::Split => &self.symbols.split_,
            WksId::ToPrimitive => &self.symbols.to_primitive_,
            WksId::ToStringTag => &self.symbols.to_string_tag_,
            WksId::Unscopables => &self.symbols.unscopables_,
        }
        .clone()
    }

    pub fn current_realm_record(&self) -> Option<Rc<RefCell<Realm>>> {
        match self.execution_context_stack.len() {
            0 => None,
            n => Some(self.execution_context_stack[n - 1].realm.clone()),
        }
    }

    pub fn current_lexical_environment(&self) -> Option<Rc<dyn EnvironmentRecord>> {
        match self.execution_context_stack.len() {
            0 => None,
            n => self.execution_context_stack[n - 1].lexical_environment.clone(),
        }
    }

    pub fn current_variable_environment(&self) -> Option<Rc<dyn EnvironmentRecord>> {
        match self.execution_context_stack.len() {
            0 => None,
            n => self.execution_context_stack[n - 1].variable_environment.clone(),
        }
    }

    pub fn current_private_environment(&self) -> Option<Rc<RefCell<PrivateEnvironmentRecord>>> {
        match self.execution_context_stack.len() {
            0 => None,
            n => self.execution_context_stack[n - 1].private_environment.clone(),
        }
    }

    pub fn set_lexical_environment(&mut self, env: Option<Rc<dyn EnvironmentRecord>>) {
        match self.execution_context_stack.len() {
            0 => (),
            n => {
                self.execution_context_stack[n - 1].lexical_environment = env;
            }
        }
    }

    pub fn set_variable_environment(&mut self, env: Option<Rc<dyn EnvironmentRecord>>) {
        match self.execution_context_stack.len() {
            0 => (),
            n => {
                self.execution_context_stack[n - 1].variable_environment = env;
            }
        }
    }

    pub fn intrinsic(&self, id: IntrinsicId) -> Object {
        let realm_ref = self.current_realm_record().unwrap();
        let realm = realm_ref.borrow();
        realm.intrinsics.get(id)
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
    pub fn set_realm_global_object(&mut self, global_obj: Option<Object>, this_value: Option<Object>) {
        let go = global_obj.unwrap_or_else(|| {
            let object_proto = self.intrinsic(IntrinsicId::ObjectPrototype);
            ordinary_object_create(self, Some(object_proto), &[])
        });
        let tv = this_value.unwrap_or_else(|| go.clone());
        let realm_ref = self.current_realm_record().unwrap();
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
    pub fn set_default_global_bindings(&mut self) {
        let global = get_global_object(self).unwrap();

        //////////////////////////////////////////////////////////////////////////////////////////////////////
        /////////       Value Properties of the Global Object
        //////////////////////////////////////////////////////////////////////////////////////////////////////
        macro_rules! global_data {
            ( $name:expr, $value:expr, $writable:expr, $enumerable:expr, $configurable:expr ) => {
                define_property_or_throw(
                    self,
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
            let rc_realm = self.current_realm_record().unwrap();
            let realm_ref = rc_realm.borrow();
            realm_ref.global_env.as_ref().unwrap().get_this_binding(self).unwrap()
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
                global_data!(stringify!($name), self.intrinsic(IntrinsicId::$name), true, false, true);
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
    pub fn initialize_host_defined_realm(&mut self, install_test_hooks: bool) {
        let realm = create_realm(self);
        let new_context = ExecutionContext::new(None, realm, None);
        self.push_execution_context(new_context);
        self.set_realm_global_object(None, None);
        self.set_default_global_bindings();
        if install_test_hooks {
            let global = get_global_object(self).unwrap();
            macro_rules! global_data {
                ( $name:expr, $value:expr, $writable:expr, $enumerable:expr, $configurable:expr ) => {
                    define_property_or_throw(
                        self,
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

    pub fn global_symbol_registry(&self) -> Rc<RefCell<SymbolRegistry>> {
        self.gsr.clone()
    }

    pub fn evaluate(&mut self, chunk: Rc<Chunk>, text: &str) -> Completion<ECMAScriptValue> {
        if self.execution_context_stack.is_empty() {
            return Err(create_type_error(self, "No active execution context"));
        }

        self.prepare_running_ec_for_execution(chunk);
        let result = self.execute(text);

        let ec_idx = self.execution_context_stack.len() - 1;
        assert!(self.execution_context_stack[ec_idx].stack.is_empty());

        result
    }

    pub fn prepare_running_ec_for_execution(&mut self, chunk: Rc<Chunk>) {
        assert!(!self.execution_context_stack.is_empty());
        let index = self.execution_context_stack.len() - 1;
        self.prepare_for_execution(index, chunk);
    }

    pub fn prepare_for_execution(&mut self, index: usize, chunk: Rc<Chunk>) {
        self.execution_context_stack[index].stack = vec![];
        self.execution_context_stack[index].chunk = Some(chunk);
        self.execution_context_stack[index].pc = 0;
    }

    pub fn execute(&mut self, text: &str) -> Completion<ECMAScriptValue> {
        loop {
            let index = self.execution_context_stack.len() - 1;
            let chunk = match self.execution_context_stack[index].chunk.clone() {
                Some(r) => Ok(r),
                None => Err(create_type_error(self, "No compiled units!")),
            }?;

            /* Diagnostics */
            print!("Stack: [ ");
            print!(
                "{}",
                self.execution_context_stack[index]
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
            if self.execution_context_stack[index].pc >= chunk.opcodes.len() {
                break;
            }
            let (_, repr) = chunk.insn_repr_at(self.execution_context_stack[index].pc as usize);
            println!("{:04}{}", self.execution_context_stack[index].pc, repr);

            /* Real work */
            let icode = chunk.opcodes[self.execution_context_stack[index].pc as usize]; // in range due to while condition
            let instruction = Insn::try_from(icode).unwrap(); // failure is a coding error (the compiler broke)
            self.execution_context_stack[index].pc += 1;
            match instruction {
                Insn::Nop => {
                    // Do nothing
                }
                Insn::ToDo => {
                    // Panic with a todo message
                    todo!()
                }
                Insn::String => {
                    let string_index = chunk.opcodes[self.execution_context_stack[index].pc as usize]; // failure is a coding error (the compiler broke)
                    self.execution_context_stack[index].pc += 1;
                    let string = &chunk.strings[string_index as usize];
                    self.execution_context_stack[index].stack.push(Ok(string.into()));
                }
                Insn::Null => self.execution_context_stack[index].stack.push(Ok(ECMAScriptValue::Null.into())),
                Insn::True => self.execution_context_stack[index].stack.push(Ok(true.into())),
                Insn::False => self.execution_context_stack[index].stack.push(Ok(false.into())),
                Insn::Empty => self.execution_context_stack[index].stack.push(Ok(NormalCompletion::Empty)),
                Insn::Undefined => {
                    self.execution_context_stack[index].stack.push(Ok(ECMAScriptValue::Undefined.into()))
                }
                Insn::This => {
                    let this_resolved = self.resolve_this_binding().map(NormalCompletion::from);
                    self.execution_context_stack[index].stack.push(this_resolved);
                }
                Insn::Resolve => {
                    let name = match self.execution_context_stack[index].stack.pop().unwrap().unwrap() {
                        NormalCompletion::Value(ECMAScriptValue::String(s)) => s,
                        _ => unreachable!(),
                    };
                    let resolved = self.resolve_binding(&name, None, false);
                    self.execution_context_stack[index].stack.push(resolved);
                }
                Insn::StrictResolve => {
                    let name = match self.execution_context_stack[index].stack.pop().unwrap().unwrap() {
                        NormalCompletion::Value(ECMAScriptValue::String(s)) => s,
                        _ => unreachable!(),
                    };
                    let resolved = self.resolve_binding(&name, None, true);
                    self.execution_context_stack[index].stack.push(resolved);
                }
                Insn::Float => {
                    let float_index = chunk.opcodes[self.execution_context_stack[index].pc as usize];
                    self.execution_context_stack[index].pc += 1;
                    let number = chunk.floats[float_index as usize];
                    self.execution_context_stack[index].stack.push(Ok(number.into()));
                }
                Insn::Bigint => {
                    let bigint_index = chunk.opcodes[self.execution_context_stack[index].pc as usize];
                    self.execution_context_stack[index].pc += 1;
                    let number = Rc::clone(&chunk.bigints[bigint_index as usize]);
                    self.execution_context_stack[index].stack.push(Ok(number.into()));
                }
                Insn::GetValue => {
                    let reference = self.execution_context_stack[index].stack.pop().unwrap();
                    let value = get_value(self, reference);
                    self.execution_context_stack[index].stack.push(value.map(NormalCompletion::from));
                }
                Insn::PutValue => {
                    let w = self.execution_context_stack[index].stack.pop().unwrap();
                    let v = self.execution_context_stack[index].stack.pop().unwrap();
                    let result = put_value(self, v, w.map(|v| v.try_into().unwrap()));
                    self.execution_context_stack[index].stack.push(result.map(NormalCompletion::from));
                }
                Insn::JumpIfAbrupt => {
                    let jump = chunk.opcodes[self.execution_context_stack[index].pc as usize] as i16;
                    self.execution_context_stack[index].pc += 1;
                    let stack_idx = self.execution_context_stack[index].stack.len() - 1;
                    if self.execution_context_stack[index].stack[stack_idx].is_err() {
                        if jump >= 0 {
                            self.execution_context_stack[index].pc += jump as usize;
                        } else {
                            self.execution_context_stack[index].pc -= (-jump) as usize;
                        }
                    }
                }
                Insn::JumpIfNormal => {
                    let jump = chunk.opcodes[self.execution_context_stack[index].pc as usize] as i16;
                    self.execution_context_stack[index].pc += 1;
                    let stack_idx = self.execution_context_stack[index].stack.len() - 1;
                    if self.execution_context_stack[index].stack[stack_idx].is_ok() {
                        if jump >= 0 {
                            self.execution_context_stack[index].pc += jump as usize;
                        } else {
                            self.execution_context_stack[index].pc -= (-jump) as usize;
                        }
                    }
                }
                Insn::JumpIfFalse | Insn::JumpIfTrue => {
                    let jump = chunk.opcodes[self.execution_context_stack[index].pc as usize] as i16;
                    self.execution_context_stack[index].pc += 1;
                    let stack_idx = self.execution_context_stack[index].stack.len() - 1;
                    let bool_val = bool::from(
                        ECMAScriptValue::try_from(
                            self.execution_context_stack[index].stack[stack_idx]
                                .clone()
                                .expect("Boolean Jumps may only be used with Normal completions"),
                        )
                        .expect("Boolean Jumps may only be used with Values"),
                    );
                    if (instruction == Insn::JumpIfFalse && !bool_val) || (instruction == Insn::JumpIfTrue && bool_val)
                    {
                        if jump >= 0 {
                            self.execution_context_stack[index].pc += jump as usize;
                        } else {
                            self.execution_context_stack[index].pc -= (-jump) as usize;
                        }
                    }
                }
                Insn::JumpPopIfFalse | Insn::JumpPopIfTrue => {
                    let jump = chunk.opcodes[self.execution_context_stack[index].pc as usize] as i16;
                    self.execution_context_stack[index].pc += 1;
                    let bool_val = bool::from(
                        ECMAScriptValue::try_from(
                            self.execution_context_stack[index]
                                .stack
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
                            self.execution_context_stack[index].pc += jump as usize;
                        } else {
                            self.execution_context_stack[index].pc -= (-jump) as usize;
                        }
                    }
                }
                Insn::JumpIfNotNullish => {
                    let jump = chunk.opcodes[self.execution_context_stack[index].pc as usize] as i16;
                    self.execution_context_stack[index].pc += 1;
                    let stack_idx = self.execution_context_stack[index].stack.len() - 1;
                    let val = ECMAScriptValue::try_from(
                        self.execution_context_stack[index].stack[stack_idx]
                            .clone()
                            .expect("Nullish Jumps may only be used with Normal completions"),
                    )
                    .expect("Nullish Jumps may only be used with Values");
                    if val != ECMAScriptValue::Undefined && val != ECMAScriptValue::Null {
                        if jump >= 0 {
                            self.execution_context_stack[index].pc += jump as usize;
                        } else {
                            self.execution_context_stack[index].pc -= (-jump) as usize;
                        }
                    }
                }
                Insn::JumpIfNotUndef => {
                    let jump = chunk.opcodes[self.execution_context_stack[index].pc as usize] as i16;
                    self.execution_context_stack[index].pc += 1;
                    let stack_idx = self.execution_context_stack[index].stack.len() - 1;
                    let val = ECMAScriptValue::try_from(
                        self.execution_context_stack[index].stack[stack_idx]
                            .clone()
                            .expect("Undef Jumps may only be used with Normal completions"),
                    )
                    .expect("Undef Jumps may only be used with Values");
                    if val != ECMAScriptValue::Undefined {
                        if jump >= 0 {
                            self.execution_context_stack[index].pc += jump as usize;
                        } else {
                            self.execution_context_stack[index].pc -= (-jump) as usize;
                        }
                    }
                }
                Insn::JumpNotThrow => {
                    let jump = chunk.opcodes[self.execution_context_stack[index].pc as usize] as i16;
                    self.execution_context_stack[index].pc += 1;
                    let stack_idx = self.execution_context_stack[index].stack.len() - 1;
                    let completion = &self.execution_context_stack[index].stack[stack_idx];

                    if !matches!(completion, Err(AbruptCompletion::Throw { .. })) {
                        if jump >= 0 {
                            self.execution_context_stack[index].pc += jump as usize;
                        } else {
                            self.execution_context_stack[index].pc -= (-jump) as usize;
                        }
                    }
                }
                Insn::Jump => {
                    let jump = chunk.opcodes[self.execution_context_stack[index].pc as usize] as i16;
                    self.execution_context_stack[index].pc += 1;
                    if jump >= 0 {
                        self.execution_context_stack[index].pc += jump as usize;
                    } else {
                        self.execution_context_stack[index].pc -= (-jump) as usize;
                    }
                }
                Insn::UpdateEmpty => {
                    let newer = self.execution_context_stack[index].stack.pop().unwrap();
                    let older = self.execution_context_stack[index].stack.pop().unwrap().unwrap();
                    self.execution_context_stack[index].stack.push(update_empty(newer, older));
                }
                Insn::Pop2Push3 => {
                    // Stack: top lower ====> top lower top
                    let top = self.execution_context_stack[index].stack.pop().unwrap();
                    let lower = self.execution_context_stack[index].stack.pop().unwrap();
                    let bottom = top.clone();
                    self.execution_context_stack[index].stack.push(bottom);
                    self.execution_context_stack[index].stack.push(lower);
                    self.execution_context_stack[index].stack.push(top);
                }
                Insn::Ref | Insn::StrictRef => {
                    let strict = instruction == Insn::StrictRef;
                    // Stack: name base ...
                    let name = {
                        let result: Result<ECMAScriptValue, _> =
                            self.execution_context_stack[index].stack.pop().unwrap().unwrap().try_into();
                        let value: Result<PropertyKey, _> = result.unwrap().try_into();
                        value.unwrap()
                    };
                    // Stack: base ...
                    let base = {
                        let result: Result<ECMAScriptValue, _> =
                            self.execution_context_stack[index].stack.pop().unwrap().unwrap().try_into();
                        result.unwrap()
                    };
                    // Stack: ...
                    let reference = Reference::new(Base::Value(base), name, strict, None);
                    let result = Ok(NormalCompletion::from(reference));
                    self.execution_context_stack[index].stack.push(result);
                    // Stack: ref ...
                }
                Insn::Pop => {
                    let stack_size = self.execution_context_stack[index].stack.len();
                    assert!(stack_size > 0);
                    self.execution_context_stack[index].stack.truncate(stack_size - 1);
                }
                Insn::Swap => {
                    let stack_size = self.execution_context_stack[index].stack.len();
                    assert!(stack_size >= 2);
                    self.execution_context_stack[index].stack.swap(stack_size - 1, stack_size - 2);
                }
                Insn::InitializeReferencedBinding => {
                    let stack_size = self.execution_context_stack[index].stack.len();
                    assert!(stack_size >= 2);
                    let value = self.execution_context_stack[index].stack.pop().unwrap();
                    let lhs = self.execution_context_stack[index].stack.pop().unwrap();
                    let result = initialize_referenced_binding(self, lhs, value.map(|nc| nc.try_into().unwrap()))
                        .map(NormalCompletion::from);
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::PushNewLexEnv => {
                    let current_env = self.current_lexical_environment();
                    let new_env = DeclarativeEnvironmentRecord::new(current_env, "inner block");
                    self.set_lexical_environment(Some(Rc::new(new_env)));
                }
                Insn::PopLexEnv => {
                    let current_env = self.current_lexical_environment().expect("lex environment must exist");
                    let outer_env = current_env.get_outer_env();
                    self.set_lexical_environment(outer_env);
                }
                Insn::PushNewVarEnvFromLex => {
                    let current_env = self.current_lexical_environment();
                    let new_env = DeclarativeEnvironmentRecord::new(current_env, "new var env");
                    self.set_variable_environment(Some(Rc::new(new_env)));
                }
                Insn::PushNewLexEnvFromVar => {
                    let current_env = self.current_variable_environment();
                    let new_env = DeclarativeEnvironmentRecord::new(current_env, "new var env");
                    self.set_lexical_environment(Some(Rc::new(new_env)));
                }
                Insn::SetLexEnvToVarEnv => {
                    let current_env = self.current_variable_environment();
                    self.set_lexical_environment(current_env);
                }

                Insn::CreateStrictImmutableLexBinding | Insn::CreateNonStrictImmutableLexBinding => {
                    let env = self.current_lexical_environment().expect("lex environment must exist");
                    let string_idx = chunk.opcodes[self.execution_context_stack[index].pc as usize] as usize;
                    self.execution_context_stack[index].pc += 1;
                    let name = chunk.strings[string_idx].clone();

                    env.create_immutable_binding(self, name, instruction == Insn::CreateStrictImmutableLexBinding)
                        .expect("binding should not already exist");
                }
                Insn::CreatePermanentMutableLexBinding | Insn::CreatePermanentMutableVarBinding => {
                    let env = if instruction == Insn::CreatePermanentMutableLexBinding {
                        self.current_lexical_environment().expect("lex environment must exist")
                    } else {
                        self.current_variable_environment().expect("var environment must exist")
                    };
                    let string_idx = chunk.opcodes[self.execution_context_stack[index].pc as usize] as usize;
                    self.execution_context_stack[index].pc += 1;
                    let name = chunk.strings[string_idx].clone();

                    env.create_mutable_binding(self, name, false).expect("binding should not already exist");
                }
                Insn::CreatePermanentMutableLexIfMissing => {
                    //  1. Let alreadyDeclared be ! env.HasBinding(paramName).
                    //  2. If alreadyDeclared is false, then
                    //        a. Perform ! env.CreateMutableBinding(paramName, false).
                    let env = self.current_lexical_environment().expect("lex environment must exist");
                    let string_idx = chunk.opcodes[self.execution_context_stack[index].pc as usize] as usize;
                    self.execution_context_stack[index].pc += 1;
                    let name = &chunk.strings[string_idx];

                    let already_declared = env.has_binding(self, name).expect("basic environments can't fail this");
                    if !already_declared {
                        env.create_mutable_binding(self, name.clone(), false).expect("binding does not already exist");
                    }
                }
                Insn::CreateInitializedPermanentMutableLexIfMissing => {
                    //  1. Let alreadyDeclared be ! env.HasBinding(paramName).
                    //  2. If alreadyDeclared is false, then
                    //      a. Perform ! env.CreateMutableBinding(paramName, false).
                    //      b. Perform ! env.InitializeBinding(paramName, undefined).
                    let env = self.current_lexical_environment().expect("lex environment must exist");
                    let string_idx = chunk.opcodes[self.execution_context_stack[index].pc as usize] as usize;
                    self.execution_context_stack[index].pc += 1;
                    let name = &chunk.strings[string_idx];

                    let already_declared = env.has_binding(self, name).expect("basic environments can't fail this");
                    if !already_declared {
                        env.create_mutable_binding(self, name.clone(), false).expect("binding does not already exist");
                        env.initialize_binding(self, name, ECMAScriptValue::Undefined)
                            .expect("binding is not previously initialized");
                    }
                }
                Insn::InitializeLexBinding | Insn::InitializeVarBinding => {
                    let env = if instruction == Insn::InitializeLexBinding {
                        self.current_lexical_environment().expect("lex environment must exist")
                    } else {
                        self.current_variable_environment().expect("var environment must exist")
                    };
                    let string_idx = chunk.opcodes[self.execution_context_stack[index].pc as usize] as usize;
                    self.execution_context_stack[index].pc += 1;
                    let name = &chunk.strings[string_idx];

                    let value = ECMAScriptValue::try_from(
                        self.execution_context_stack[index]
                            .stack
                            .pop()
                            .expect("InitializeLexBinding must have a stack arg")
                            .expect("InitializeLexBinding's stack arg must be a normal completion"),
                    )
                    .expect("InitializeLexBinding's stack arg must be a value");
                    env.initialize_binding(self, name, value).expect("binding should not already have a value");
                }
                Insn::GetLexBinding => {
                    let env = self.current_lexical_environment().expect("lex environment must exist");
                    let string_idx = chunk.opcodes[self.execution_context_stack[index].pc as usize] as usize;
                    self.execution_context_stack[index].pc += 1;
                    let name = &chunk.strings[string_idx];
                    let value = env.get_binding_value(self, name, false).expect("Binding will be there");
                    self.execution_context_stack[index].stack.push(Ok(NormalCompletion::from(value)));
                }
                Insn::SetMutableVarBinding => {
                    let env = self.current_variable_environment().expect("var environment must exist");
                    let string_idx = chunk.opcodes[self.execution_context_stack[index].pc as usize] as usize;
                    self.execution_context_stack[index].pc += 1;
                    let name = &chunk.strings[string_idx];
                    let value = ECMAScriptValue::try_from(
                        self.execution_context_stack[index]
                            .stack
                            .pop()
                            .expect("SetMutableVarBinding must have a stack arg")
                            .expect("SetMutableVarBinding's stack arg must be a normal completion"),
                    )
                    .expect("SetMutableVarBinding's stack arg must be a value");
                    env.set_mutable_binding(self, name.clone(), value, false).expect("error free execution");
                }
                Insn::ExtractThrownValue => {
                    let stack_idx = self.execution_context_stack[index].stack.len() - 1;
                    let completion = &self.execution_context_stack[index].stack[stack_idx];
                    match completion.as_ref().unwrap_err() {
                        AbruptCompletion::Throw { value } => {
                            self.execution_context_stack[index].stack[stack_idx] = Ok(value.clone().into())
                        }
                        _ => panic!("Bad error type for ExtractThrownValue"),
                    }
                }
                Insn::ExtractArg => {
                    // Stack: N arg[N-1] arg[N-2] ... arg[1] arg[0] (when N >= 1)
                    // Out: arg[0] N-1 arg[N-1] arg[N-2] ... arg[1]
                    //   --or, if N == 0 --
                    // Stack: 0
                    // Out: Undefined 0
                    let stack_len = self.execution_context_stack[index].stack.len();
                    assert!(stack_len > 0, "ExtractArg must have an argument list on the stack");
                    let arg_count = f64::try_from(
                        ECMAScriptValue::try_from(
                            self.execution_context_stack[index].stack[stack_len - 1]
                                .clone()
                                .expect("ExtractArg must have a 'count' argument"),
                        )
                        .expect("ExtractArg must have a 'count' argument"),
                    )
                    .expect("ExtractArg 'count' arg must be a number");
                    if arg_count < 0.5 {
                        self.execution_context_stack[index]
                            .stack
                            .push(Ok(NormalCompletion::from(ECMAScriptValue::Undefined)));
                    } else {
                        let arg_count = arg_count as usize;
                        assert!(stack_len > arg_count, "Stack must contain an argument list");
                        let arg0 = self.execution_context_stack[index].stack.remove(stack_len - arg_count - 1);
                        self.execution_context_stack[index].stack[stack_len - 2] =
                            Ok(NormalCompletion::from((arg_count - 1) as u32));
                        self.execution_context_stack[index].stack.push(arg0);
                    }
                }
                Insn::FinishArgs => {
                    // Stack: N arg[N-1] ... arg[0]
                    // Out:
                    // Remove any remaining arguments from the stack (we're at zero, or the caller gave us too much)
                    let arg_count = f64::try_from(
                        ECMAScriptValue::try_from(
                            self.execution_context_stack[index]
                                .stack
                                .pop()
                                .expect("FinishArgs must have a 'count' argument")
                                .expect("FinishArgs must have a 'count' argument"),
                        )
                        .expect("FinishArgs must have a 'count' argument"),
                    )
                    .expect("FinishArgs 'count' arg must be a number");
                    let to_retain = self.execution_context_stack[index].stack.len() - arg_count as usize;
                    self.execution_context_stack[index].stack.truncate(to_retain);
                }
                Insn::Object => {
                    let obj_proto = self.intrinsic(IntrinsicId::ObjectPrototype);
                    let o = ordinary_object_create(self, Some(obj_proto), &[]);
                    self.execution_context_stack[index].stack.push(Ok(ECMAScriptValue::from(o).into()));
                }
                Insn::CreateDataProperty => {
                    let nc_value = self.execution_context_stack[index].stack.pop().unwrap().unwrap();
                    let nc_name = self.execution_context_stack[index].stack.pop().unwrap().unwrap();
                    let nc_obj = self.execution_context_stack[index].stack.pop().unwrap().unwrap();
                    let obj = Object::try_from(nc_obj).unwrap();
                    let name = PropertyKey::try_from(nc_name).unwrap();
                    let value = ECMAScriptValue::try_from(nc_value).unwrap();
                    create_data_property_or_throw(self, &obj, name, value).unwrap();
                    self.execution_context_stack[index].stack.push(Ok(NormalCompletion::from(obj)));
                }
                Insn::SetPrototype => {
                    let nc_value = self.execution_context_stack[index].stack.pop().unwrap().unwrap();
                    let nc_obj = self.execution_context_stack[index].stack.pop().unwrap().unwrap();
                    let obj = Object::try_from(nc_obj).unwrap();
                    let value = ECMAScriptValue::try_from(nc_value).unwrap();
                    let val_obj_res: anyhow::Result<Option<Object>> = value.try_into();
                    if let Ok(new_proto) = val_obj_res {
                        obj.o.set_prototype_of(self, new_proto).unwrap();
                    }
                    self.execution_context_stack[index].stack.push(Ok(NormalCompletion::from(obj)));
                }
                Insn::ToPropertyKey => {
                    let nc_name = self.execution_context_stack[index].stack.pop().unwrap().unwrap();
                    let value_name = ECMAScriptValue::try_from(nc_name).unwrap();
                    let key = to_property_key(self, value_name);
                    let fc = key.map(|pk| NormalCompletion::from(ECMAScriptValue::from(pk)));
                    self.execution_context_stack[index].stack.push(fc);
                }
                Insn::CopyDataProps => {
                    let nc_value = self.execution_context_stack[index].stack.pop().unwrap().unwrap();
                    let nc_obj = self.execution_context_stack[index].stack.pop().unwrap().unwrap();
                    let obj = Object::try_from(nc_obj).unwrap();
                    let value = ECMAScriptValue::try_from(nc_value).unwrap();
                    let result = copy_data_properties(self, &obj, value, &[]);
                    let fc = match result {
                        Ok(_) => Ok(NormalCompletion::from(obj)),
                        Err(e) => Err(e),
                    };
                    self.execution_context_stack[index].stack.push(fc);
                }
                Insn::Dup => {
                    let idx = self.execution_context_stack[index].stack.len() - 1;
                    let fc = self.execution_context_stack[index].stack[idx].clone();
                    self.execution_context_stack[index].stack.push(fc);
                }
                Insn::ToNumeric => {
                    let nc_val = self.execution_context_stack[index].stack.pop().unwrap().unwrap();
                    let val: ECMAScriptValue = nc_val.try_into().unwrap();
                    let result = to_numeric(self, val).map(NormalCompletion::from);
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::Increment => {
                    let nc_val = self.execution_context_stack[index].stack.pop().unwrap().unwrap();
                    let num: Numeric = nc_val.try_into().unwrap();
                    let result_val: ECMAScriptValue = match num {
                        Numeric::Number(n) => (n + 1.0).into(),
                        Numeric::BigInt(bi) => ECMAScriptValue::BigInt(Rc::new(&*bi + 1)),
                    };
                    let fc = Ok(NormalCompletion::from(result_val));
                    self.execution_context_stack[index].stack.push(fc);
                }
                Insn::Decrement => {
                    let nc_val = self.execution_context_stack[index].stack.pop().unwrap().unwrap();
                    let num: Numeric = nc_val.try_into().unwrap();
                    let result_val: ECMAScriptValue = match num {
                        Numeric::Number(n) => (n - 1.0).into(),
                        Numeric::BigInt(bi) => ECMAScriptValue::BigInt(Rc::new(&*bi - 1)),
                    };
                    let fc = Ok(NormalCompletion::from(result_val));
                    self.execution_context_stack[index].stack.push(fc);
                }
                Insn::PreIncrement => {
                    let fc = self.execution_context_stack[index].stack.pop().unwrap();
                    let result = self.prefix_increment(fc);
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::PreDecrement => {
                    let fc = self.execution_context_stack[index].stack.pop().unwrap();
                    let result = self.prefix_decrement(fc);
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::Delete => {
                    let fc = self.execution_context_stack[index].stack.pop().unwrap();
                    let result = self.delete_ref(fc);
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::Void => {
                    let fc = self.execution_context_stack[index].stack.pop().unwrap();
                    let result = self.void_operator(fc);
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::TypeOf => {
                    let fc = self.execution_context_stack[index].stack.pop().unwrap();
                    let result = self.typeof_operator(fc);
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::Unwind => {
                    let vals_to_remove = chunk.opcodes[self.execution_context_stack[index].pc as usize] as usize;
                    self.execution_context_stack[index].pc += 1;
                    assert!(vals_to_remove < self.execution_context_stack[index].stack.len());
                    if vals_to_remove > 0 {
                        let old_index_of_err = self.execution_context_stack[index].stack.len() - 1;
                        let new_index_of_err = old_index_of_err - vals_to_remove;
                        self.execution_context_stack[index].stack.swap(new_index_of_err, old_index_of_err);
                        self.execution_context_stack[index].stack.truncate(new_index_of_err + 1);
                    }
                }
                Insn::UnwindList => {
                    let err_to_keep =
                        self.execution_context_stack[index].stack.pop().expect("UnwindList has two stack args");
                    let vals_to_remove = f64::try_from(
                        ECMAScriptValue::try_from(
                            self.execution_context_stack[index]
                                .stack
                                .pop()
                                .expect("UnwindList has a stack argument")
                                .expect("UnwindList expects a normal completion"),
                        )
                        .expect("UnwindList expects a value"),
                    )
                    .expect("UnwindList expects a number") as usize;
                    if vals_to_remove > 0 {
                        let old_stack_size = self.execution_context_stack[index].stack.len();
                        assert!(vals_to_remove <= old_stack_size);
                        let new_stack_size = old_stack_size - vals_to_remove;
                        self.execution_context_stack[index].stack.truncate(new_stack_size);
                    }
                    self.execution_context_stack[index].stack.push(err_to_keep);
                }
                Insn::Call => {
                    let arg_count_nc = self.execution_context_stack[index].stack.pop().unwrap().unwrap();
                    let arg_count_val = ECMAScriptValue::try_from(arg_count_nc).unwrap();
                    let arg_count: usize = (f64::try_from(arg_count_val).unwrap().round() as i64).try_into().unwrap();
                    let mut arguments = Vec::with_capacity(arg_count);
                    for _ in 1..=arg_count {
                        let nc = ECMAScriptValue::try_from(
                            self.execution_context_stack[index].stack.pop().unwrap().unwrap(),
                        )
                        .unwrap();
                        arguments.push(nc);
                    }
                    arguments.reverse();
                    let func_nc = self.execution_context_stack[index].stack.pop().unwrap().unwrap();
                    let func_val = ECMAScriptValue::try_from(func_nc).unwrap();
                    let ref_nc = self.execution_context_stack[index].stack.pop().unwrap().unwrap();

                    self.begin_call_evaluation(func_val, ref_nc, &arguments);
                }
                Insn::EndFunction => {
                    let stack_len = self.execution_context_stack[index].stack.len();
                    assert!(stack_len >= 2);
                    let result = self.execution_context_stack[index].stack.pop().expect("Stack is at least 2 elements");
                    let f_obj = Object::try_from(
                        ECMAScriptValue::try_from(
                            self.execution_context_stack[index]
                                .stack
                                .pop()
                                .expect("Stack is at least 2 elements")
                                .expect("function obj argument must be a function obj"),
                        )
                        .expect("function obj argument must be a function obj"),
                    )
                    .expect("function obj argument must be a function obj");
                    let callable = f_obj.o.to_callable_obj().expect("function obj argument must be a function obj");
                    callable.end_evaluation(self, result);
                }
                Insn::Return => {
                    let value = ECMAScriptValue::try_from(
                        self.ec_pop().expect("Return needs an argument").expect("Return needs a normal completion"),
                    )
                    .expect("Return needs a value");
                    self.ec_push(Err(AbruptCompletion::Return { value }));
                }
                Insn::UnaryPlus => {
                    let exp = self.execution_context_stack[index].stack.pop().unwrap();
                    let val = get_value(self, exp);
                    let result = match val {
                        Ok(ev) => to_number(self, ev).map(NormalCompletion::from),
                        Err(ac) => Err(ac),
                    };
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::UnaryMinus => {
                    let exp = self.execution_context_stack[index].stack.pop().unwrap();
                    let val = get_value(self, exp);
                    let old_val = match val {
                        Ok(val) => to_numeric(self, val),
                        Err(ac) => Err(ac),
                    };
                    let result = match old_val {
                        Err(ac) => Err(ac),
                        Ok(Numeric::Number(n)) => Ok(NormalCompletion::from(-n)),
                        Ok(Numeric::BigInt(bi)) => Ok(NormalCompletion::from(Rc::new(-&*bi))),
                    };
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::UnaryComplement => {
                    let exp = self.execution_context_stack[index].stack.pop().unwrap();
                    let val = get_value(self, exp);
                    let old_val = match val {
                        Ok(val) => to_numeric(self, val),
                        Err(ac) => Err(ac),
                    };
                    let result = match old_val {
                        Err(ac) => Err(ac),
                        Ok(Numeric::Number(n)) => Ok(NormalCompletion::from(!to_int32(self, n).unwrap())),
                        Ok(Numeric::BigInt(bi)) => Ok(NormalCompletion::from(Rc::new(!&*bi))),
                    };
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::UnaryNot => {
                    let exp = self.execution_context_stack[index].stack.pop().unwrap();
                    let val = get_value(self, exp);
                    let result = match val {
                        Ok(val) => Ok(NormalCompletion::from(!to_boolean(val))),
                        Err(ac) => Err(ac),
                    };
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::Exponentiate => self.binary_operation(index, BinOp::Exponentiate),
                Insn::Multiply => self.binary_operation(index, BinOp::Multiply),
                Insn::Divide => self.binary_operation(index, BinOp::Divide),
                Insn::Modulo => self.binary_operation(index, BinOp::Remainder),
                Insn::Add => self.binary_operation(index, BinOp::Add),
                Insn::Subtract => self.binary_operation(index, BinOp::Subtract),

                Insn::InstantiateIdFreeFunctionExpression => {
                    let id = chunk.opcodes[self.execution_context_stack[index].pc as usize]; // failure is a coding error (the compiler broke)
                    self.execution_context_stack[index].pc += 1;
                    let info = &chunk.function_object_data[id as usize];
                    self.instantiate_ordinary_function_expression_without_binding_id(index, text, info)
                }
                Insn::InstantiateOrdinaryFunctionExpression => {
                    let id = chunk.opcodes[self.execution_context_stack[index].pc as usize]; // failure is a coding error (the compiler broke)
                    self.execution_context_stack[index].pc += 1;
                    let info = &chunk.function_object_data[id as usize];
                    self.instantiate_ordinary_function_expression_with_binding_id(index, text, info)
                }

                Insn::InstantiateArrowFunctionExpression => {
                    let id = chunk.opcodes[self.execution_context_stack[index].pc as usize]; // failure is a coding error (the compiler broke)
                    self.execution_context_stack[index].pc += 1;
                    let info = &chunk.function_object_data[id as usize];
                    self.instantiate_arrow_function_expression(index, text, info)
                }
                Insn::InstantiateOrdinaryFunctionObject => {
                    let string_index = chunk.opcodes[self.execution_context_stack[index].pc as usize]; // failure is a coding error (the compiler broke)
                    self.execution_context_stack[index].pc += 1;
                    let string = &chunk.strings[string_index as usize];
                    let func_index = chunk.opcodes[self.execution_context_stack[index].pc as usize] as usize;
                    self.execution_context_stack[index].pc += 1;
                    let info = &chunk.function_object_data[func_index as usize];
                    self.instantiate_ordinary_function_object(index, text, string, info)
                }
                Insn::LeftShift => self.binary_operation(index, BinOp::LeftShift),
                Insn::SignedRightShift => self.binary_operation(index, BinOp::SignedRightShift),
                Insn::UnsignedRightShift => self.binary_operation(index, BinOp::UnsignedRightShift),
                Insn::Throw => {
                    // Convert the NormalCompletion::Value on top of the stack into a ThrowCompletion with a matching value
                    let exp: ECMAScriptValue = self.execution_context_stack[index]
                        .stack
                        .pop()
                        .expect("Throw requires an argument")
                        .expect("Throw requires a NormalCompletion")
                        .try_into()
                        .expect("Throw requires a value");
                    self.execution_context_stack[index].stack.push(Err(AbruptCompletion::Throw { value: exp }));
                }
                Insn::Less => {
                    let (lval, rval) = self.two_values(index);
                    let result =
                        self.is_less_than(lval, rval, true).map(|optb| NormalCompletion::from(optb.unwrap_or(false)));
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::Greater => {
                    let (lval, rval) = self.two_values(index);
                    let result =
                        self.is_less_than(rval, lval, false).map(|optb| NormalCompletion::from(optb.unwrap_or(false)));
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::LessEqual => {
                    let (lval, rval) = self.two_values(index);
                    let result =
                        self.is_less_than(rval, lval, false).map(|optb| NormalCompletion::from(!optb.unwrap_or(true)));
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::GreaterEqual => {
                    let (lval, rval) = self.two_values(index);
                    let result =
                        self.is_less_than(lval, rval, true).map(|optb| NormalCompletion::from(!optb.unwrap_or(true)));
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::InstanceOf => {
                    let (lval, rval) = self.two_values(index);
                    let result = self.instanceof_operator(lval, rval);
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::In => {
                    let (lval, rval) = self.two_values(index);
                    let result = match rval {
                        ECMAScriptValue::Object(obj) => {
                            let key = to_property_key(self, lval)?;
                            has_property(self, &obj, &key).map(NormalCompletion::from)
                        }
                        _ => Err(create_type_error(self, "Right-hand side of 'in' must be an object")),
                    };
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::Equal => {
                    let (lval, rval) = self.two_values(index);
                    let result = self.is_loosely_equal(&lval, &rval).map(NormalCompletion::from);
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::NotEqual => {
                    let (lval, rval) = self.two_values(index);
                    let result = self.is_loosely_equal(&lval, &rval).map(|val| NormalCompletion::from(!val));
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::StrictEqual => {
                    let (lval, rval) = self.two_values(index);
                    let result = Ok(NormalCompletion::from(lval.is_strictly_equal(&rval)));
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::StrictNotEqual => {
                    let (lval, rval) = self.two_values(index);
                    let result = Ok(NormalCompletion::from(!lval.is_strictly_equal(&rval)));
                    self.execution_context_stack[index].stack.push(result);
                }
                Insn::BitwiseAnd => self.binary_operation(index, BinOp::BitwiseAnd),
                Insn::BitwiseOr => self.binary_operation(index, BinOp::BitwiseOr),
                Insn::BitwiseXor => self.binary_operation(index, BinOp::BitwiseXor),
                Insn::CreateUnmappedArguments => self.create_unmapped_arguments_object(index),
                Insn::CreateMappedArguments => self.create_mapped_arguments_object(index),
                Insn::AddMappedArgument => {
                    let string_index = chunk.opcodes[self.execution_context_stack[index].pc as usize]; // failure is a coding error (the compiler broke)
                    self.execution_context_stack[index].pc += 1;
                    let string = &chunk.strings[string_index as usize];
                    let argument_index = chunk.opcodes[self.execution_context_stack[index].pc as usize] as usize;
                    self.execution_context_stack[index].pc += 1;
                    self.attach_mapped_arg(index, string, argument_index);
                }
                Insn::HandleEmptyBreak => {
                    let prior_result =
                        self.execution_context_stack[index].stack.pop().expect("HandleEmptyBreak requires an argument");
                    let new_result = if let Err(AbruptCompletion::Break { value, target: None }) = prior_result {
                        match value {
                            NormalCompletion::Empty => Ok(NormalCompletion::from(ECMAScriptValue::Undefined)),
                            value => Ok(value),
                        }
                    } else {
                        prior_result
                    };
                    self.execution_context_stack[index].stack.push(new_result);
                }
                Insn::HandleTargetedBreak => {
                    let str_idx = chunk.opcodes[self.execution_context_stack[index].pc as usize] as usize;
                    self.execution_context_stack[index].pc += 1;
                    let label = &chunk.strings[str_idx];
                    let prior_result = self.execution_context_stack[index]
                        .stack
                        .pop()
                        .expect("HandleTargetedBreak requires an argument");
                    let new_result = match prior_result {
                        Err(AbruptCompletion::Break { value, target: Some(target) }) if &target == label => Ok(value),
                        _ => prior_result,
                    };
                    self.execution_context_stack[index].stack.push(new_result);
                }
                Insn::CoalesceValue => {
                    // Stack: stmtResult V ...
                    // If stmtResult.[[Value]] is not empty, set V to stmtResult.[[Value]].
                    let stmt_result =
                        self.execution_context_stack[index].stack.pop().expect("CoalesceValue requires two arguments");
                    let v = ECMAScriptValue::try_from(
                        self.execution_context_stack[index]
                            .stack
                            .pop()
                            .expect("CoalesceValue requires two arguments")
                            .expect("argument V must be  normal completion"),
                    )
                    .expect("argument V must be a value");
                    self.execution_context_stack[index].stack.push(Ok(match stmt_result {
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
                    let set_idx = chunk.opcodes[self.execution_context_stack[index].pc as usize] as usize;
                    self.execution_context_stack[index].pc += 1;
                    let label_set = &chunk.label_sets[set_idx];
                    // 1. If completion.[[Type]] is normal, return true.
                    // 2. If completion.[[Type]] is not continue, return false.
                    // 3. If completion.[[Target]] is empty, return true.
                    // 4. If completion.[[Target]] is an element of labelSet, return true.
                    // 5. Return false.
                    let idx = self.execution_context_stack[index].stack.len() - 1;
                    let completion = &self.execution_context_stack[index].stack[idx];
                    let result = match completion {
                        Ok(_) => true,
                        Err(AbruptCompletion::Continue { value: _, target: None }) => true,
                        Err(AbruptCompletion::Continue { value: _, target: Some(label) }) => label_set.contains(label),
                        _ => false,
                    };
                    self.execution_context_stack[index].stack.push(Ok(NormalCompletion::from(result)));
                }
                Insn::Continue => {
                    self.execution_context_stack[index]
                        .stack
                        .push(Err(AbruptCompletion::Continue { value: NormalCompletion::Empty, target: None }));
                }
                Insn::TargetedContinue => {
                    let str_idx = chunk.opcodes[self.execution_context_stack[index].pc as usize] as usize;
                    self.execution_context_stack[index].pc += 1;
                    let label = chunk.strings[str_idx].clone();
                    self.execution_context_stack[index]
                        .stack
                        .push(Err(AbruptCompletion::Continue { value: NormalCompletion::Empty, target: Some(label) }));
                }
                Insn::Break => {
                    self.execution_context_stack[index]
                        .stack
                        .push(Err(AbruptCompletion::Break { value: NormalCompletion::Empty, target: None }));
                }
                Insn::TargetedBreak => {
                    let str_idx = chunk.opcodes[self.execution_context_stack[index].pc as usize] as usize;
                    self.execution_context_stack[index].pc += 1;
                    let label = chunk.strings[str_idx].clone();
                    self.execution_context_stack[index]
                        .stack
                        .push(Err(AbruptCompletion::Break { value: NormalCompletion::Empty, target: Some(label) }));
                }
            }
        }
        let index = self.execution_context_stack.len() - 1;
        self.execution_context_stack[index]
            .stack
            .pop()
            .map(|svr| {
                svr.map(|sv| match sv {
                    NormalCompletion::Reference(_) | NormalCompletion::Empty => ECMAScriptValue::Undefined,
                    NormalCompletion::Value(v) => v,
                })
            })
            .unwrap_or(Ok(ECMAScriptValue::Undefined))
    }

    fn begin_call_evaluation(
        &mut self,
        func: ECMAScriptValue,
        reference: NormalCompletion,
        arguments: &[ECMAScriptValue],
    ) {
        let this_value = match &reference {
            NormalCompletion::Empty => unreachable!(),
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
            let err = Err(create_type_error(self, "not an object"));
            self.ec_push(err);
            return;
        }
        if !is_callable(&func) {
            let err = Err(create_type_error(self, "not a function"));
            self.ec_push(err);
            return;
        }
        initiate_call(self, &func, &this_value, arguments);
    }

    fn prefix_increment(&mut self, expr: FullCompletion) -> FullCompletion {
        let value = get_value(self, expr.clone())?;
        let old_value = to_numeric(self, value)?;
        let new_value: ECMAScriptValue = match old_value {
            Numeric::Number(n) => (n + 1.0).into(),
            Numeric::BigInt(bi) => ECMAScriptValue::from(&*bi + 1),
        };
        put_value(self, expr, Ok(new_value.clone()))?;
        Ok(NormalCompletion::from(new_value))
    }

    fn prefix_decrement(&mut self, expr: FullCompletion) -> FullCompletion {
        let value = get_value(self, expr.clone())?;
        let old_value = to_numeric(self, value)?;
        let new_value: ECMAScriptValue = match old_value {
            Numeric::Number(n) => (n - 1.0).into(),
            Numeric::BigInt(bi) => ECMAScriptValue::from(&*bi - 1),
        };
        put_value(self, expr, Ok(new_value.clone()))?;
        Ok(NormalCompletion::from(new_value))
    }

    fn delete_ref(&mut self, expr: FullCompletion) -> FullCompletion {
        let reference = expr?;
        match reference {
            NormalCompletion::Empty | NormalCompletion::Value(_) => Ok(true.into()),
            NormalCompletion::Reference(r) => match *r {
                Reference { base: Base::Unresolvable, .. } => Ok(true.into()),
                Reference { base: Base::Value(_), this_value: Some(_), .. } => {
                    Err(create_reference_error(self, "super properties not deletable"))
                }
                Reference { base: Base::Value(val), referenced_name, strict, this_value: None } => {
                    let base_obj = to_object(self, val)?;
                    let delete_status = base_obj
                        .o
                        .delete(self, &referenced_name.try_into().expect("Property name will never be private"))?;
                    if !delete_status && strict {
                        Err(create_type_error(self, "property not deletable"))
                    } else {
                        Ok(delete_status.into())
                    }
                }
                Reference { base: Base::Environment(base), referenced_name, .. } => {
                    let delete_status = base.delete_binding(
                        self,
                        &referenced_name.try_into().expect("Property name will never be private"),
                    )?;
                    Ok(delete_status.into())
                }
            },
        }
    }

    fn void_operator(&mut self, expr: FullCompletion) -> FullCompletion {
        get_value(self, expr)?;
        Ok(ECMAScriptValue::Undefined.into())
    }

    fn typeof_operator(&mut self, expr: FullCompletion) -> FullCompletion {
        if let Ok(NormalCompletion::Reference(r)) = &expr {
            if r.is_unresolvable_reference() {
                return Ok(NormalCompletion::from("undefined"));
            }
        }

        let val = get_value(self, expr)?;
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

    fn two_values(&mut self, index: usize) -> (ECMAScriptValue, ECMAScriptValue) {
        let (right, left) = {
            let stack = &mut self.execution_context_stack[index].stack;
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

    fn binary_operation(&mut self, index: usize, op: BinOp) {
        let (lval, rval) = self.two_values(index);
        let result = self.apply_string_or_numeric_binary_operator(lval, rval, op);
        self.execution_context_stack[index].stack.push(result);
    }

    fn apply_string_or_numeric_binary_operator(
        &mut self,
        lval: ECMAScriptValue,
        rval: ECMAScriptValue,
        op: BinOp,
    ) -> FullCompletion {
        let (lval, rval) = if op == BinOp::Add {
            let lprim = to_primitive(self, lval, None)?;
            let rprim = to_primitive(self, rval, None)?;
            if lprim.is_string() || rprim.is_string() {
                let lstr = to_string(self, lprim)?;
                let rstr = to_string(self, rprim)?;
                return Ok(NormalCompletion::from(lstr.concat(rstr)));
            }
            (lprim, rprim)
        } else {
            (lval, rval)
        };
        let lnum = to_numeric(self, lval)?;
        let rnum = to_numeric(self, rval)?;
        match (lnum, rnum, op) {
            (Numeric::Number(left), Numeric::Number(right), BinOp::Exponentiate) => {
                Ok(NormalCompletion::from(left.powf(right)))
            }
            (Numeric::Number(left), Numeric::Number(right), BinOp::Multiply) => {
                Ok(NormalCompletion::from(left * right))
            }
            (Numeric::Number(left), Numeric::Number(right), BinOp::Divide) => Ok(NormalCompletion::from(left / right)),
            (Numeric::Number(left), Numeric::Number(right), BinOp::Remainder) => {
                Ok(NormalCompletion::from(left % right))
            }
            (Numeric::Number(left), Numeric::Number(right), BinOp::Add) => Ok(NormalCompletion::from(left + right)),
            (Numeric::Number(left), Numeric::Number(right), BinOp::Subtract) => {
                Ok(NormalCompletion::from(left - right))
            }
            (Numeric::Number(left), Numeric::Number(right), BinOp::LeftShift) => {
                let lnum = to_int32(self, left).expect("Numbers are always convertable to Int32");
                let rnum = to_uint32(self, right).expect("Numbers are always convertable to Uint32");
                let shift_count = rnum % 32;
                Ok(NormalCompletion::from(lnum << shift_count))
            }
            (Numeric::Number(left), Numeric::Number(right), BinOp::SignedRightShift) => {
                let lnum = to_int32(self, left).expect("Numbers are always convertable to Int32");
                let rnum = to_uint32(self, right).expect("Numbers are always convertable to Uint32");
                let shift_count = rnum % 32;
                Ok(NormalCompletion::from(lnum >> shift_count))
            }
            (Numeric::Number(left), Numeric::Number(right), BinOp::UnsignedRightShift) => {
                let lnum = to_uint32(self, left).expect("Numbers are always convertable to Uint32");
                let rnum = to_uint32(self, right).expect("Numbers are always convertable to Uint32");
                let shift_count = rnum % 32;
                Ok(NormalCompletion::from(lnum >> shift_count))
            }
            (Numeric::Number(left), Numeric::Number(right), BinOp::BitwiseAnd) => {
                let lnum = to_int32(self, left).expect("Numbers are always convertable to int32");
                let rnum = to_int32(self, right).expect("Numbers are always convertable to int32");
                Ok(NormalCompletion::from(lnum & rnum))
            }
            (Numeric::Number(left), Numeric::Number(right), BinOp::BitwiseOr) => {
                let lnum = to_int32(self, left).expect("Numbers are always convertable to int32");
                let rnum = to_int32(self, right).expect("Numbers are always convertable to int32");
                Ok(NormalCompletion::from(lnum | rnum))
            }
            (Numeric::Number(left), Numeric::Number(right), BinOp::BitwiseXor) => {
                let lnum = to_int32(self, left).expect("Numbers are always convertable to int32");
                let rnum = to_int32(self, right).expect("Numbers are always convertable to int32");
                Ok(NormalCompletion::from(lnum ^ rnum))
            }
            (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::Exponentiate) => {
                let exponent =
                    BigUint::try_from(&*right).map_err(|_| create_range_error(self, "Exponent must be positive"))?;
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
                .unwrap_or_else(|| Err(create_range_error(self, "Division by zero"))),
            (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::Remainder) => {
                if right.is_zero() {
                    Err(create_range_error(self, "Division by zero"))
                } else {
                    Ok(NormalCompletion::from(&*left % &*right))
                }
            }
            (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::Add) => Ok(NormalCompletion::from(&*left + &*right)),
            (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::Subtract) => {
                Ok(NormalCompletion::from(&*left - &*right))
            }
            (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::LeftShift) => bigint_leftshift(&left, &right)
                .map_err(|err| create_range_error(self, err.to_string()))
                .map(NormalCompletion::from),
            (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::SignedRightShift) => {
                bigint_rightshift(&left, &right)
                    .map_err(|err| create_range_error(self, err.to_string()))
                    .map(NormalCompletion::from)
            }
            (Numeric::BigInt(_), Numeric::BigInt(_), BinOp::UnsignedRightShift) => {
                Err(create_type_error(self, "BigInts have no unsigned right shift, use >> instead"))
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
                Err(create_type_error(self, "Cannot mix BigInt and other types, use explicit conversions"))
            }
        }
    }

    fn instantiate_ordinary_function_expression_without_binding_id(
        &mut self,
        index: usize,
        text: &str,
        info: &StashedFunctionData,
    ) {
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
            let typeerror = create_type_error(self, err.to_string());
            let l = self.execution_context_stack[index].stack.len();
            self.execution_context_stack[index].stack[l - 1] = Err(typeerror); // pop then push
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
        let env = self.current_lexical_environment().expect("Lexical environment must exist if code is running");
        let priv_env = self.current_private_environment();

        let name = JSString::try_from(
            ECMAScriptValue::try_from(
                self.execution_context_stack[index]
                    .stack
                    .pop()
                    .expect("Insn only used with argument on stack")
                    .expect("Argument must not be an AbruptCompletion"),
            )
            .expect("Argument must be a value"),
        )
        .expect("Argument must be a string");

        let function_prototype = self.intrinsic(IntrinsicId::FunctionPrototype);

        let closure = ordinary_function_create(
            self,
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

        set_function_name(self, &closure, name.into(), None);
        make_constructor(self, &closure, None);

        self.execution_context_stack[index].stack.push(Ok(closure.into()));
    }

    fn instantiate_ordinary_function_expression_with_binding_id(
        &mut self,
        index: usize,
        text: &str,
        info: &StashedFunctionData,
    ) {
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
            let typeerror = create_type_error(self, err.to_string());
            let l = self.execution_context_stack[index].stack.len();
            self.execution_context_stack[index].stack[l - 1] = Err(typeerror); // pop then push
            return;
        }
        for line in compiled.disassemble() {
            println!("{line}");
        }

        let name = JSString::try_from(
            ECMAScriptValue::try_from(
                self.execution_context_stack[index]
                    .stack
                    .pop()
                    .expect("Insn only used with argument on stack")
                    .expect("Argument must not be an AbruptCompletion"),
            )
            .expect("Argument must be a value"),
        )
        .expect("Argument must be a string");

        let outer_env = self.current_lexical_environment().expect("Lexical environment must exist if code is running");
        let func_env = Rc::new(DeclarativeEnvironmentRecord::new(Some(outer_env), name.clone()));
        let priv_env = self.current_private_environment();

        func_env.create_immutable_binding(self, name.clone(), false).expect("Fresh environment won't fail");
        let function_prototype = self.intrinsic(IntrinsicId::FunctionPrototype);

        let closure = ordinary_function_create(
            self,
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

        set_function_name(self, &closure, name.clone().into(), None);
        make_constructor(self, &closure, None);
        func_env.initialize_binding(self, &name, closure.clone().into()).expect("binding has been created");

        self.execution_context_stack[index].stack.push(Ok(closure.into()));
    }

    fn instantiate_arrow_function_expression(&mut self, index: usize, text: &str, info: &StashedFunctionData) {
        let env = self.current_lexical_environment().unwrap();
        let priv_env = self.current_private_environment();

        let name = JSString::try_from(
            ECMAScriptValue::try_from(self.execution_context_stack[index].stack.pop().unwrap().unwrap()).unwrap(),
        )
        .unwrap();

        let to_compile: Rc<ArrowFunction> =
            info.to_compile.clone().try_into().expect("This routine only used with Arrow Functions");
        let chunk_name = nameify(&info.source_text, 50);
        let mut compiled = Chunk::new(chunk_name);
        let compilation_status = to_compile.body.compile_body(&mut compiled, text, info);
        if let Err(err) = compilation_status {
            let typeerror = create_type_error(self, err.to_string());
            let l = self.execution_context_stack[index].stack.len();
            self.execution_context_stack[index].stack[l - 1] = Err(typeerror); // pop then push
            return;
        }
        for line in compiled.disassemble() {
            println!("{line}");
        }

        let function_prototype = self.intrinsic(IntrinsicId::FunctionPrototype);

        let closure = ordinary_function_create(
            self,
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
        set_function_name(self, &closure, name.into(), None);

        self.execution_context_stack[index].stack.push(Ok(closure.into()));
    }

    fn instantiate_ordinary_function_object(
        &mut self,
        index: usize,
        text: &str,
        name: &JSString,
        info: &StashedFunctionData,
    ) {
        let to_compile: Rc<FunctionDeclaration> =
            info.to_compile.clone().try_into().expect("This routine only used with Function Declarations");
        let chunk_name = nameify(&info.source_text, 50);
        let mut compiled = Chunk::new(chunk_name);
        let compilation_status = to_compile.body.compile_body(&mut compiled, text, info);
        if let Err(err) = compilation_status {
            let typeerror = create_type_error(self, err.to_string());
            let l = self.execution_context_stack[index].stack.len();
            self.execution_context_stack[index].stack[l - 1] = Err(typeerror); // pop then push
            return;
        }
        for line in compiled.disassemble() {
            println!("{line}");
        }

        let env = self.current_lexical_environment().unwrap();
        let priv_env = self.current_private_environment();
        let function_prototype = self.intrinsic(IntrinsicId::FunctionPrototype);

        let closure = ordinary_function_create(
            self,
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
        set_function_name(self, &closure, name.clone().into(), None);
        make_constructor(self, &closure, None);

        self.execution_context_stack[index].stack.push(Ok(closure.into()));
    }

    fn is_less_than(&mut self, x: ECMAScriptValue, y: ECMAScriptValue, left_first: bool) -> Completion<Option<bool>> {
        let (px, py) = if left_first {
            let px = to_primitive(self, x, None)?;
            let py = to_primitive(self, y, None)?;
            (px, py)
        } else {
            let py = to_primitive(self, y, None)?;
            let px = to_primitive(self, x, None)?;
            (px, py)
        };
        if px.is_string() && py.is_string() {
            let sx = JSString::try_from(px).expect("String values must be strings");
            let sy = JSString::try_from(py).expect("String values must be strings");
            return Ok(Some(sx < sy));
        }
        if px.is_string() && py.is_bigint() {
            let nx =
                String::from(JSString::try_from(px).expect("String values must be strings")).parse::<BigInt>().ok();
            let ny: Rc<BigInt> = py.try_into().expect("Bigint values must be bigints");
            return match nx {
                None => Ok(None),
                Some(nx) => Ok(Some(nx < *ny)),
            };
        }
        if px.is_bigint() && py.is_string() {
            let nx: Rc<BigInt> = px.try_into().expect("Bigint values must be bigints");
            let ny =
                String::from(JSString::try_from(py).expect("String values must be strings")).parse::<BigInt>().ok();
            return match ny {
                None => Ok(None),
                Some(ny) => Ok(Some(*nx < ny)),
            };
        }
        let nx = to_numeric(self, px)?;
        let ny = to_numeric(self, py)?;
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

    fn instanceof_operator(&mut self, v: ECMAScriptValue, target: ECMAScriptValue) -> FullCompletion {
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
                let hi = self.wks(WksId::HasInstance);
                let instof_handler = get_method(self, &target, &hi.into())?;
                match &instof_handler {
                    ECMAScriptValue::Undefined => {
                        if !is_callable(&target) {
                            Err(create_type_error(self, "Right-hand side of 'instanceof' is not callable"))
                        } else {
                            self.ordinary_has_instance(&target, &v).map(NormalCompletion::from)
                        }
                    }
                    _ => {
                        let res = call(self, &instof_handler, &target, &[v])?;
                        Ok(NormalCompletion::from(to_boolean(res)))
                    }
                }
            }
            _ => Err(create_type_error(self, "Right-hand side of 'instanceof' is not an object")),
        }
    }

    pub fn create_unmapped_arguments_object(&mut self, index: usize) {
        // Stack should have n arg[n-1] arg[n-2] ... arg[0] ...
        // Those values are NOT consumed; this function assumes they'll be used again.

        let stack_len = self.execution_context_stack[index].stack.len();
        assert!(stack_len > 0, "Stack must not be empty");
        let length = f64::try_from(
            ECMAScriptValue::try_from(
                self.execution_context_stack[index].stack[stack_len - 1].clone().expect("Non-error arguments needed"),
            )
            .expect("Value arguments needed"),
        )
        .expect("Numeric arguments needed") as u32;
        assert!(stack_len > length as usize, "Stack too short to fit all the arguments");

        let obj = ArgumentsObject::object(self, None);
        define_property_or_throw(
            self,
            &obj,
            "length",
            PotentialPropertyDescriptor::new().value(length).writable(true).enumerable(false).configurable(true),
        )
        .expect("Normal Object");

        let first_arg_index = stack_len - length as usize - 1;
        let arguments =
            self.execution_context_stack[index].stack[first_arg_index..first_arg_index + length as usize].to_vec();

        for (arg_number, item) in arguments.into_iter().enumerate() {
            let value =
                ECMAScriptValue::try_from(item.expect("Non-error arguments needed")).expect("Value arguments needed");
            create_data_property_or_throw(self, &obj, arg_number, value).expect("Normal Object");
        }

        let iterator = self.wks(WksId::Iterator);
        let array_values = self.intrinsic(IntrinsicId::ArrayPrototypeValues);
        let throw_type_error = self.intrinsic(IntrinsicId::ThrowTypeError);
        define_property_or_throw(
            self,
            &obj,
            iterator,
            PotentialPropertyDescriptor::new().value(array_values).writable(true).enumerable(false).configurable(true),
        )
        .expect("Normal Object");
        define_property_or_throw(
            self,
            &obj,
            "callee",
            PotentialPropertyDescriptor::new()
                .get(throw_type_error.clone())
                .set(throw_type_error)
                .enumerable(false)
                .configurable(false),
        )
        .expect("Normal Object");

        self.execution_context_stack[index].stack.push(Ok(NormalCompletion::from(obj)));
        // Stack at exit: AObj N arg[N-1] ... arg[0] ...
    }

    pub fn create_mapped_arguments_object(&mut self, index: usize) {
        // Stack should have n arg[n-1] arg[n-2] ... arg[0] func ...

        let stack_len = self.execution_context_stack[index].stack.len();
        assert!(stack_len > 0, "Stack must not be empty");
        let length = f64::try_from(
            ECMAScriptValue::try_from(
                self.execution_context_stack[index].stack[stack_len - 1].clone().expect("Non-error arguments needed"),
            )
            .expect("Value arguments needed"),
        )
        .expect("Numeric arguments needed") as u32;
        assert!(stack_len > length as usize + 1, "Stack too short to fit all the arguments plus the function obj");

        let first_arg_index = stack_len - length as usize - 1;
        let arguments =
            self.execution_context_stack[index].stack[first_arg_index..first_arg_index + length as usize].to_vec();

        let env = self.current_lexical_environment().expect("A lex env must exist");
        let map = ParameterMap::new(env);
        let ao = ArgumentsObject::object(self, Some(map));

        for (idx, item) in arguments.into_iter().enumerate() {
            let val =
                ECMAScriptValue::try_from(item.expect("arguments must be values")).expect("arguments must be values");
            create_data_property_or_throw(self, &ao, idx, val).expect("ArgumentObject won't throw");
        }

        define_property_or_throw(
            self,
            &ao,
            "length",
            PotentialPropertyDescriptor::new().value(length).writable(true).enumerable(false).configurable(true),
        )
        .expect("ArgumentObject won't throw");
        let iterator = self.wks(WksId::Iterator);
        let array_values = self.intrinsic(IntrinsicId::ArrayPrototypeValues);
        define_property_or_throw(
            self,
            &ao,
            iterator,
            PotentialPropertyDescriptor::new().value(array_values).writable(true).enumerable(false).configurable(true),
        )
        .expect("ArgumentObject won't throw");
        let func = ECMAScriptValue::try_from(
            self.execution_context_stack[index].stack[first_arg_index - 1].clone().expect("Function object type error"),
        )
        .expect("Function object type error");
        define_property_or_throw(
            self,
            &ao,
            "callee",
            PotentialPropertyDescriptor::new().value(func).writable(true).enumerable(false).configurable(true),
        )
        .expect("ArgumentObject won't throw");

        self.execution_context_stack[index].stack.push(Ok(NormalCompletion::from(ao)));
        // Stack at exit: AObj N arg[N-1] ... arg[0] func ...
    }

    pub fn attach_mapped_arg(&mut self, index: usize, name: &JSString, idx: usize) {
        // Stack: AObj ...
        let top = self.execution_context_stack[index].stack.len();
        assert!(top > 0, "stack must not be empty");
        let obj = Object::try_from(
            ECMAScriptValue::try_from(
                self.execution_context_stack[index].stack[top - 1].clone().expect("arguments must be values"),
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

pub fn parse_script(
    agent: &mut Agent,
    source_text: &str,
    realm: Rc<RefCell<Realm>>,
) -> Result<ScriptRecord, Vec<Object>> {
    let script = parse_text(agent, source_text, ParseGoal::Script);
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
        agent: &mut Agent,
        env: Rc<dyn EnvironmentRecord>,
        private_env: Option<Rc<RefCell<PrivateEnvironmentRecord>>>,
        strict: bool,
        text: &str,
    ) -> Completion<ECMAScriptValue> {
        match self {
            FcnDef::Function(x) => x.instantiate_function_object(agent, env, private_env, strict, text, x.clone()),
            FcnDef::Generator(x) => x.instantiate_function_object(agent, env, private_env, strict, text, x.clone()),
            FcnDef::AsyncFun(x) => x.instantiate_function_object(agent, env, private_env, strict, text, x.clone()),
            FcnDef::AsyncGen(x) => x.instantiate_function_object(agent, env, private_env, strict, text, x.clone()),
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
    agent: &mut Agent,
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
            return Err(create_syntax_error(agent, format!("{name}: already defined"), None));
        }
        if env.has_lexical_declaration(agent, &name) {
            return Err(create_syntax_error(agent, format!("{name}: already defined"), None));
        }
        let has_restricted_global = env.has_restricted_global_property(agent, &name)?;
        if has_restricted_global {
            return Err(create_syntax_error(agent, format!("{name} is restricted and may not be used"), None));
        }
    }
    for name in var_names {
        if env.has_lexical_declaration(agent, &name) {
            return Err(create_syntax_error(agent, format!("{name}: already defined"), None));
        }
    }
    let var_declarations = script.var_scoped_declarations();
    let mut functions_to_initialize = vec![];
    let mut declared_function_names = vec![];
    for d in var_declarations.iter().rev().cloned().filter_map(|decl| FcnDef::try_from(decl).ok()) {
        let func_name = d.bound_name();
        if !declared_function_names.contains(&func_name) {
            let fn_definable = env.can_declare_global_function(agent, &func_name)?;
            if !fn_definable {
                return Err(create_type_error(agent, format!("Cannot create global function {func_name}")));
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
                let vn_definable = env.can_declare_global_var(agent, &vn)?;
                if !vn_definable {
                    return Err(create_type_error(agent, format!("Cannot create global variable {vn}")));
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
                env.create_immutable_binding(agent, dn, true)?;
            } else {
                println!("   mutable:   {dn}");
                env.create_mutable_binding(agent, dn, false)?;
            }
        }
    }
    for f in functions_to_initialize {
        let name = f.bound_name();
        let func_obj = f.instantiate_function_object(
            agent,
            env.clone() as Rc<dyn EnvironmentRecord>,
            private_env.clone(),
            strict,
            text,
        )?;
        println!("   function:  {name}");
        env.create_global_function_binding(agent, name, func_obj, false)?;
    }
    for vn in declared_var_names {
        println!("   var:       {vn}");
        env.create_global_var_binding(agent, vn, false)?;
    }
    println!("..done");

    Ok(())
}

pub fn script_evaluation(agent: &mut Agent, sr: ScriptRecord) -> Completion<ECMAScriptValue> {
    let global_env = sr.realm.borrow().global_env.clone();
    let mut script_context =
        ExecutionContext::new(None, Rc::clone(&sr.realm), Some(ScriptOrModule::Script(Rc::new(sr.clone()))));
    script_context.lexical_environment = global_env.clone().map(|g| g as Rc<dyn EnvironmentRecord>);
    script_context.variable_environment = global_env.clone().map(|g| g as Rc<dyn EnvironmentRecord>);

    agent.push_execution_context(script_context);

    let script = sr.ecmascript_code.clone();

    let strict = script.body.as_ref().map(|b| b.contains_use_strict()).unwrap_or(false);

    let result = global_declaration_instantiation(agent, script, global_env.unwrap(), strict, &sr.text)
        .and_then(|_| agent.evaluate(sr.compiled, &sr.text));

    agent.pop_execution_context();

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

pub fn process_ecmascript(agent: &mut Agent, source_text: &str) -> Result<ECMAScriptValue, ProcessError> {
    let realm = agent.current_realm_record().unwrap();
    let x = parse_script(agent, source_text, realm).map_err(|errs| ProcessError::CompileErrors { values: errs })?;

    let result = script_evaluation(agent, x);
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
