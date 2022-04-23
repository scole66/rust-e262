use super::environment_record::GlobalEnvironmentRecord;
use super::execution_context::{get_global_object, ExecutionContext};
use super::object::{define_property_or_throw, ordinary_object_create, Object, PotentialPropertyDescriptor};
use super::realm::{create_realm, IntrinsicId};
use super::strings::JSString;
use super::values::{ECMAScriptValue, Symbol, SymbolInternals};
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
    pub obj_id: usize,
    pub symbol_id: usize,
}

#[allow(clippy::new_without_default)]
impl Agent {
    pub fn new() -> Self {
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
        }
    }

    pub fn running_execution_context(&self) -> Option<&ExecutionContext> {
        let len = self.execution_context_stack.len();
        if len > 0 {
            Some(&self.execution_context_stack[len - 1])
        } else {
            None
        }
    }
    pub fn running_execution_context_mut(&mut self) -> Option<&mut ExecutionContext> {
        let len = self.execution_context_stack.len();
        if len > 0 {
            Some(&mut self.execution_context_stack[len - 1])
        } else {
            None
        }
    }

    pub fn active_function_object(&mut self) -> Option<Object> {
        self.running_execution_context().and_then(|ec| ec.function.as_ref().cloned())
    }

    pub fn next_object_id(&mut self) -> usize {
        assert!(self.obj_id < usize::MAX);
        let result = self.obj_id;
        self.obj_id += 1;
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

    pub fn intrinsic(&self, id: IntrinsicId) -> Object {
        self.running_execution_context().unwrap().realm.borrow().intrinsics.get(id)
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
        let mut realm = self.running_execution_context().unwrap().realm.borrow_mut();

        realm.global_object = Some(go.clone());
        let new_global_env = GlobalEnvironmentRecord::new(go, tv);
        realm.global_env = Some(Rc::new(RefCell::new(new_global_env)));
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
            let o_env = &self.running_execution_context().unwrap().realm.borrow().global_env;
            let global_env = o_env.as_ref().unwrap().borrow();
            global_env.get_this_binding()
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
    pub fn initialize_host_defined_realm(&mut self) {
        let realm = create_realm(self);
        let new_context = ExecutionContext::new(None, realm, None);
        self.push_execution_context(new_context);
        self.set_realm_global_object(None, None);
        self.set_default_global_bindings();
    }

    #[allow(unused_variables)]
    pub fn evaluate(&mut self, chunk: Rc<Chunk>) -> Completion {
        todo!()
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

use super::chunk::Chunk;
use super::execution_context::ScriptRecord;
use super::parser::scripts::VarScopeDecl;
use super::parser::{parse_text, ParseGoal, ParsedText};
use super::realm::Realm;
use std::cell::RefCell;

pub fn parse_script(agent: &mut Agent, source_text: &str, realm: Rc<RefCell<Realm>>) -> Result<ScriptRecord, Vec<Object>> {
    let script = parse_text(agent, source_text, ParseGoal::Script);
    match script {
        ParsedText::Errors(errs) => Err(errs),
        ParsedText::Script(script) => {
            let mut chunk = Chunk::new("top level script");
            script.compile(&mut chunk).unwrap();
            Ok(ScriptRecord { realm, ecmascript_code: script, compiled: Rc::new(chunk) })
        }
    }
}

#[allow(unused_variables)]
pub fn global_declaration_instantiation(agent: &mut Agent, script: Rc<Script>, env: Rc<RefCell<GlobalEnvironmentRecord>>) -> Completion {
    let lex_names = script.lexically_declared_names();
    let var_names = script.var_declared_names();
    for name in lex_names {
        if env.borrow().has_var_declaration(&name) {
            return Err(create_syntax_error(agent, format!("{name}: already defined")));
        }
        if env.borrow().has_lexical_declaration(agent, &name) {
            return Err(create_syntax_error(agent, format!("{name}: already defined")));
        }
        let has_restricted_global = env.borrow().has_restricted_global_property(agent, &name)?;
        if has_restricted_global {
            return Err(create_syntax_error(agent, format!("{name} is restricted and may not be used")));
        }
    }
    for name in var_names {
        if env.borrow().has_lexical_declaration(agent, &name) {
            return Err(create_syntax_error(agent, format!("{name}: already defined")));
        }
    }
    let var_declarations = script.var_scoped_declarations();
    let mut functions_to_initialize = vec![];
    let mut declared_function_names = vec![];
    for d in var_declarations.iter().rev().filter(|&pn| {
        matches!(pn, VarScopeDecl::FunctionDeclaration(_) | VarScopeDecl::GeneratorDeclaration(_) | VarScopeDecl::AsyncFunctionDeclaration(_) | VarScopeDecl::AsyncGeneratorDeclaration(_))
    }) {
        let func_name = match d {
            VarScopeDecl::FunctionDeclaration(fd) => fd.bound_names()[0].clone(),
            VarScopeDecl::GeneratorDeclaration(gd) => gd.bound_names()[0].clone(),
            VarScopeDecl::AsyncFunctionDeclaration(afd) => afd.bound_names()[0].clone(),
            VarScopeDecl::AsyncGeneratorDeclaration(agd) => agd.bound_names()[0].clone(),
            _ => unreachable!(),
        };
        if !declared_function_names.contains(&func_name) {
            let fn_definable = env.borrow().can_declare_global_function(agent, &func_name)?;
            if !fn_definable {
                return Err(create_type_error(agent, format!("Cannot create global function {func_name}")));
            }
            declared_function_names.push(func_name);
            functions_to_initialize.insert(0, d.clone());
        }
    }
    let mut declared_var_names = vec![];
    for d in var_declarations.iter().filter(|&pn| matches!(pn, VarScopeDecl::VariableDeclaration(_) | VarScopeDecl::ForBinding(_) | VarScopeDecl::BindingIdentifier(_))) {
        for vn in match d {
            VarScopeDecl::VariableDeclaration(vd) => vd.bound_names(),
            VarScopeDecl::ForBinding(fb) => fb.bound_names(),
            VarScopeDecl::BindingIdentifier(bi) => bi.bound_names(),
            _ => unreachable!(),
        } {
            if !declared_function_names.contains(&vn) {
                let vn_definable = env.borrow().can_declare_global_var(agent, &vn)?;
                if !vn_definable {
                    return Err(create_type_error(agent, format!("Cannot create global variable {vn}")));
                }
                if !declared_var_names.contains(&vn) {
                    declared_var_names.push(vn);
                }
            }
        }
    }
    let lex_declarations = script.lexically_scoped_declarations();
    let private_env = None;

    todo!()
}

use crate::cr::Completion;
use crate::environment_record::EnvironmentRecord;
use crate::errors::{create_syntax_error, create_type_error};
use crate::execution_context::ScriptOrModule;
use crate::parser::scripts::Script;

pub fn script_evaluation(agent: &mut Agent, sr: ScriptRecord) -> Completion {
    let global_env = sr.realm.borrow().global_env.clone();
    let mut script_context = ExecutionContext::new(None, Rc::clone(&sr.realm), Some(ScriptOrModule::Script(Rc::new(sr.clone()))));
    script_context.lexical_environment = global_env.clone().map(|g| g as Rc<RefCell<dyn EnvironmentRecord>>);
    script_context.variable_environment = global_env.clone().map(|g| g as Rc<RefCell<dyn EnvironmentRecord>>);

    agent.push_execution_context(script_context);

    let script = sr.ecmascript_code.clone();

    let mut result = global_declaration_instantiation(agent, script, global_env.unwrap());
    if result.is_ok() {
        result = agent.evaluate(sr.compiled);
        if let Ok(ECMAScriptValue::Empty) = result {
            result = Ok(ECMAScriptValue::Undefined);
        }
    }

    agent.pop_execution_context();

    result
}

pub fn process_ecmascript(agent: &mut Agent, source_text: &str) -> Result<ECMAScriptValue, String> {
    let realm = Rc::clone(&agent.running_execution_context().unwrap().realm);
    let x = parse_script(agent, source_text, realm).map_err(|_| "errors happened during compilation".to_string())?;

    script_evaluation(agent, x).map_err(|_| "errors happend during evaluation".to_string())?;
    todo!()
}

#[cfg(test)]
mod tests;
