use super::*;
use anyhow::anyhow;
use itertools::Itertools;
use num::pow::Pow;
use num::{BigInt, BigUint, Zero};
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

//#[allow(clippy::new_without_default)]
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

    pub fn evaluate(&mut self, chunk: Rc<Chunk>) -> Completion<ECMAScriptValue> {
        let ec_idx = match self.execution_context_stack.len() {
            0 => return Err(create_type_error(self, "No active execution context")),
            n => n - 1,
        };

        self.prepare_for_execution(ec_idx, chunk);
        let result = self.execute(ec_idx);

        assert!(self.execution_context_stack[ec_idx].stack.is_empty());

        result
        //todo!()
    }

    pub fn prepare_for_execution(&mut self, index: usize, chunk: Rc<Chunk>) {
        self.execution_context_stack[index].stack = vec![];
        self.execution_context_stack[index].chunk = Some(chunk);
        self.execution_context_stack[index].pc = 0;
    }

    pub fn execute(&mut self, index: usize) -> Completion<ECMAScriptValue> {
        let chunk = match self.execution_context_stack[index].chunk.clone() {
            Some(r) => Ok(r),
            None => Err(create_type_error(self, "No compiled units!")),
        }?;
        //while self.execution_context_stack[index].pc < chunk.opcodes.len() {
        loop {
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

                    let result = self.evaluate_call(func_val, ref_nc, &arguments);

                    self.execution_context_stack[index].stack.push(result);
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
            }
        }
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

    fn evaluate_call(
        &mut self,
        func: ECMAScriptValue,
        reference: NormalCompletion,
        arguments: &[ECMAScriptValue],
    ) -> FullCompletion {
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
            return Err(create_type_error(self, "not an object"));
        }
        if !is_callable(&func) {
            return Err(create_type_error(self, "not a function"));
        }
        call(self, &func, &this_value, arguments).map(NormalCompletion::from)
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

    fn binary_operation(&mut self, index: usize, op: BinOp) {
        let rval =
            ECMAScriptValue::try_from(self.execution_context_stack[index].stack.pop().unwrap().unwrap()).unwrap();
        let lval =
            ECMAScriptValue::try_from(self.execution_context_stack[index].stack.pop().unwrap().unwrap()).unwrap();
        let result = self.apply_string_or_numeric_binary_operator(lval, rval, op);
        self.execution_context_stack[index].stack.push(result);
    }

    #[allow(unused_variables)]
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
                todo!()
            }
            (Numeric::Number(left), Numeric::Number(right), BinOp::BitwiseOr) => {
                todo!()
            }
            (Numeric::Number(left), Numeric::Number(right), BinOp::BitwiseXor) => {
                todo!()
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
                .checked_div(&*right)
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
            (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::LeftShift) => bigint_leftshift(&*left, &*right)
                .map_err(|err| create_range_error(self, err.to_string()))
                .map(NormalCompletion::from),
            (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::SignedRightShift) => {
                bigint_rightshift(&*left, &*right)
                    .map_err(|err| create_range_error(self, err.to_string()))
                    .map(NormalCompletion::from)
            }
            (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::UnsignedRightShift) => {
                Err(create_type_error(self, "BigInts have no unsigned right shift, use >> instead"))
            }
            (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::BitwiseAnd) => {
                todo!()
            }
            (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::BitwiseOr) => {
                todo!()
            }
            (Numeric::BigInt(left), Numeric::BigInt(right), BinOp::BitwiseXor) => {
                todo!()
            }
            (Numeric::BigInt(_), Numeric::Number(_), _) | (Numeric::Number(_), Numeric::BigInt(_), _) => {
                Err(create_type_error(self, "Cannot mix BigInt and other types, use explicit conversions"))
            }
        }
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
            Ok(ScriptRecord { realm, ecmascript_code: script, compiled: Rc::new(chunk) })
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
enum TopLevelFcnDef {
    Function(Rc<FunctionDeclaration>),
    Generator(Rc<GeneratorDeclaration>),
    AsyncFun(Rc<AsyncFunctionDeclaration>),
    AsyncGen(Rc<AsyncGeneratorDeclaration>),
}
impl TryFrom<VarScopeDecl> for TopLevelFcnDef {
    type Error = anyhow::Error;
    fn try_from(src: VarScopeDecl) -> anyhow::Result<Self> {
        match src {
            VarScopeDecl::FunctionDeclaration(fd) => Ok(Self::Function(fd)),
            VarScopeDecl::GeneratorDeclaration(gd) => Ok(Self::Generator(gd)),
            VarScopeDecl::AsyncFunctionDeclaration(afd) => Ok(Self::AsyncFun(afd)),
            VarScopeDecl::AsyncGeneratorDeclaration(agd) => Ok(Self::AsyncGen(agd)),
            _ => Err(anyhow!("Not a top-level function def")),
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
    for d in var_declarations.iter().rev().cloned().filter_map(|decl| TopLevelFcnDef::try_from(decl).ok()) {
        let func_name = match &d {
            TopLevelFcnDef::Function(fd) => fd.bound_names()[0].clone(),
            TopLevelFcnDef::Generator(gd) => gd.bound_names()[0].clone(),
            TopLevelFcnDef::AsyncFun(afd) => afd.bound_names()[0].clone(),
            TopLevelFcnDef::AsyncGen(agd) => agd.bound_names()[0].clone(),
        };
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
        let (name, func_obj) = match f {
            TopLevelFcnDef::Function(fd) => (
                fd.bound_names()[0].clone(),
                fd.instantiate_function_object(agent, env.clone() as Rc<dyn EnvironmentRecord>, private_env),
            ),
            TopLevelFcnDef::Generator(gd) => (
                gd.bound_names()[0].clone(),
                gd.instantiate_function_object(agent, env.clone() as Rc<dyn EnvironmentRecord>, private_env),
            ),
            TopLevelFcnDef::AsyncFun(afd) => (
                afd.bound_names()[0].clone(),
                afd.instantiate_function_object(agent, env.clone() as Rc<dyn EnvironmentRecord>, private_env),
            ),
            TopLevelFcnDef::AsyncGen(agd) => (
                agd.bound_names()[0].clone(),
                agd.instantiate_function_object(agent, env.clone() as Rc<dyn EnvironmentRecord>, private_env),
            ),
        };
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

    let result =
        global_declaration_instantiation(agent, script, global_env.unwrap()).and_then(|_| agent.evaluate(sr.compiled));

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
        Ok(left >> u32::try_from(right)?)
    }
}

#[cfg(test)]
mod tests;
