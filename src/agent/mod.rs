use super::chunk::Chunk;
use super::compiler::Insn;
use super::cr::{update_empty, AbruptCompletion, Completion, FullCompletion, NormalCompletion};
use super::environment_record::{EnvironmentRecord, GlobalEnvironmentRecord};
use super::errors::{create_reference_error, create_syntax_error, create_type_error, unwind_any_error_object};
use super::execution_context::{get_global_object, ExecutionContext, ScriptOrModule, ScriptRecord};
use super::object::{copy_data_properties, define_property_or_throw, ordinary_object_create, Object, PotentialPropertyDescriptor};
use super::parser::async_function_definitions::AsyncFunctionDeclaration;
use super::parser::async_generator_function_definitions::AsyncGeneratorDeclaration;
use super::parser::class_definitions::ClassDeclaration;
use super::parser::declarations_and_variables::LexicalDeclaration;
use super::parser::function_definitions::FunctionDeclaration;
use super::parser::generator_function_definitions::GeneratorDeclaration;
use super::parser::scripts::{Script, VarScopeDecl};
use super::parser::statements_and_declarations::DeclPart;
use super::parser::{parse_text, ParseGoal, ParsedText};
use super::realm::{create_realm, IntrinsicId, Realm};
use super::reference::{get_value, initialize_referenced_binding, put_value, Base, Reference};
use super::strings::JSString;
use super::values::{to_numeric, to_object, to_property_key, ECMAScriptValue, Numeric, PropertyKey, Symbol, SymbolInternals};
use crate::object::create_data_property_or_throw;
use crate::symbol_object::SymbolRegistry;
use anyhow::anyhow;
use itertools::Itertools;
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
                async_iterator_: Symbol(Rc::new(SymbolInternals { id: 1, description: Some(JSString::from("Symbol.asyncIterator")) })),
                has_instance_: Symbol(Rc::new(SymbolInternals { id: 2, description: Some(JSString::from("Symbol.hasInstance")) })),
                is_concat_spreadable_: Symbol(Rc::new(SymbolInternals { id: 3, description: Some(JSString::from("Symbol.isConcatSpreadable")) })),
                iterator_: Symbol(Rc::new(SymbolInternals { id: 4, description: Some(JSString::from("Symbol.iterator")) })),
                match_: Symbol(Rc::new(SymbolInternals { id: 5, description: Some(JSString::from("Symbol.match")) })),
                match_all_: Symbol(Rc::new(SymbolInternals { id: 6, description: Some(JSString::from("Symbol.matchAll")) })),
                replace_: Symbol(Rc::new(SymbolInternals { id: 7, description: Some(JSString::from("Symbol.replace")) })),
                search_: Symbol(Rc::new(SymbolInternals { id: 8, description: Some(JSString::from("Symbol.search")) })),
                species_: Symbol(Rc::new(SymbolInternals { id: 9, description: Some(JSString::from("Symbol.species")) })),
                split_: Symbol(Rc::new(SymbolInternals { id: 10, description: Some(JSString::from("Symbol.split")) })),
                to_primitive_: Symbol(Rc::new(SymbolInternals { id: 11, description: Some(JSString::from("Symbol.toPrimitive")) })),
                to_string_tag_: Symbol(Rc::new(SymbolInternals { id: 12, description: Some(JSString::from("Symbol.toStringTag")) })),
                unscopables_: Symbol(Rc::new(SymbolInternals { id: 13, description: Some(JSString::from("Symbol.unscopables")) })),
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
                    PotentialPropertyDescriptor::new().value(ECMAScriptValue::from($value)).writable($writable).enumerable($enumerable).configurable($configurable),
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
                        PotentialPropertyDescriptor::new().value(ECMAScriptValue::from($value)).writable($writable).enumerable($enumerable).configurable($configurable),
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
        while self.execution_context_stack[index].pc < chunk.opcodes.len() {
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
            let (_, repr) = chunk.insn_repr_at(self.execution_context_stack[index].pc as usize);
            println!("{:04}{}", self.execution_context_stack[index].pc, repr);

            /* Real work */
            let icode = chunk.opcodes[self.execution_context_stack[index].pc as usize]; // in range due to while condition
            let instruction = Insn::try_from(icode).unwrap(); // failure is a coding error (the compiler broke)
            self.execution_context_stack[index].pc += 1;
            match instruction {
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
                Insn::Undefined => self.execution_context_stack[index].stack.push(Ok(ECMAScriptValue::Undefined.into())),
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
                        let result: Result<ECMAScriptValue, _> = self.execution_context_stack[index].stack.pop().unwrap().unwrap().try_into();
                        let value: Result<PropertyKey, _> = result.unwrap().try_into();
                        value.unwrap()
                    };
                    // Stack: base ...
                    let base = {
                        let result: Result<ECMAScriptValue, _> = self.execution_context_stack[index].stack.pop().unwrap().unwrap().try_into();
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
                    let result = initialize_referenced_binding(self, lhs, value.map(|nc| nc.try_into().unwrap())).map(NormalCompletion::from);
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
            NormalCompletion::Reference(r) if r.is_unresolvable_reference() => Ok(true.into()),
            NormalCompletion::Reference(r) if r.is_property_reference() => {
                if r.is_super_reference() {
                    Err(create_reference_error(self, "super properties not deletable"))
                } else {
                    let base_obj = to_object(self, ECMAScriptValue::try_from(r.base).unwrap())?;
                    let delete_status = base_obj.o.delete(self, &r.referenced_name.try_into().unwrap())?;
                    if !delete_status && r.strict {
                        Err(create_type_error(self, "property not deletable"))
                    } else {
                        Ok(delete_status.into())
                    }
                }
            }
            NormalCompletion::Reference(r) => {
                let base: Rc<dyn EnvironmentRecord> = r.base.try_into().unwrap();
                let delete_status = base.delete_binding(self, &r.referenced_name.try_into().unwrap())?;
                Ok(delete_status.into())
            }
        }
    }

    fn void_operator(&mut self, expr: FullCompletion) -> FullCompletion {
        get_value(self, expr).map(NormalCompletion::from)?;
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
}

#[derive(Debug, PartialEq, Copy, Clone)]
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

pub fn parse_script(agent: &mut Agent, source_text: &str, realm: Rc<RefCell<Realm>>) -> Result<ScriptRecord, Vec<Object>> {
    let script = parse_text(agent, source_text, ParseGoal::Script);
    match script {
        ParsedText::Errors(errs) => Err(errs),
        ParsedText::Script(script) => {
            let mut chunk = Chunk::new("top level script");
            script.compile(&mut chunk).unwrap();
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

pub fn global_declaration_instantiation(agent: &mut Agent, script: Rc<Script>, env: Rc<GlobalEnvironmentRecord>) -> Completion<()> {
    println!("Creating Globals...");
    let lex_names = script.lexically_declared_names();
    let var_names = script.var_declared_names();
    for name in lex_names {
        if env.has_var_declaration(&name) {
            return Err(create_syntax_error(agent, format!("{name}: already defined")));
        }
        if env.has_lexical_declaration(agent, &name) {
            return Err(create_syntax_error(agent, format!("{name}: already defined")));
        }
        let has_restricted_global = env.has_restricted_global_property(agent, &name)?;
        if has_restricted_global {
            return Err(create_syntax_error(agent, format!("{name} is restricted and may not be used")));
        }
    }
    for name in var_names {
        if env.has_lexical_declaration(agent, &name) {
            return Err(create_syntax_error(agent, format!("{name}: already defined")));
        }
    }
    let var_declarations = script.var_scoped_declarations();
    let mut functions_to_initialize = vec![];
    let mut declared_function_names = vec![];
    for d in var_declarations
        .iter()
        .rev()
        .filter(|pn| {
            matches!(
                pn,
                VarScopeDecl::FunctionDeclaration(_) | VarScopeDecl::GeneratorDeclaration(_) | VarScopeDecl::AsyncFunctionDeclaration(_) | VarScopeDecl::AsyncGeneratorDeclaration(_)
            )
        })
        .cloned()
        .map(|decl| TopLevelFcnDef::try_from(decl).unwrap())
    {
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
    for d in var_declarations.into_iter().filter(|pn| matches!(pn, VarScopeDecl::VariableDeclaration(_) | VarScopeDecl::ForBinding(_) | VarScopeDecl::BindingIdentifier(_))) {
        for vn in match d {
            VarScopeDecl::VariableDeclaration(vd) => vd.bound_names(),
            VarScopeDecl::ForBinding(fb) => fb.bound_names(),
            VarScopeDecl::BindingIdentifier(bi) => bi.bound_names(),
            _ => unreachable!(),
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
    let lex_declarations = script.lexically_scoped_declarations().into_iter().map(|d| TopLevelLexDecl::try_from(d).unwrap());
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
            TopLevelFcnDef::Function(fd) => (fd.bound_names()[0].clone(), fd.instantiate_function_object(agent, env.clone() as Rc<dyn EnvironmentRecord>, private_env)),
            TopLevelFcnDef::Generator(gd) => (gd.bound_names()[0].clone(), gd.instantiate_function_object(agent, env.clone() as Rc<dyn EnvironmentRecord>, private_env)),
            TopLevelFcnDef::AsyncFun(afd) => (afd.bound_names()[0].clone(), afd.instantiate_function_object(agent, env.clone() as Rc<dyn EnvironmentRecord>, private_env)),
            TopLevelFcnDef::AsyncGen(agd) => (agd.bound_names()[0].clone(), agd.instantiate_function_object(agent, env.clone() as Rc<dyn EnvironmentRecord>, private_env)),
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
    let mut script_context = ExecutionContext::new(None, Rc::clone(&sr.realm), Some(ScriptOrModule::Script(Rc::new(sr.clone()))));
    script_context.lexical_environment = global_env.clone().map(|g| g as Rc<dyn EnvironmentRecord>);
    script_context.variable_environment = global_env.clone().map(|g| g as Rc<dyn EnvironmentRecord>);

    agent.push_execution_context(script_context);

    let script = sr.ecmascript_code.clone();

    let result = global_declaration_instantiation(agent, script, global_env.unwrap()).and_then(|_| agent.evaluate(sr.compiled));

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
        Err(AbruptCompletion::Throw { value }) => Err(ProcessError::RuntimeError { error: value }),
        Err(_) => Err(ProcessError::InternalError { reason: "Impossible completion returned".to_string() }),
    }
}

#[cfg(test)]
mod tests;
