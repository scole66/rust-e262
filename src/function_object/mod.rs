use super::agent::*;
//use super::chunk::*;
use super::cr::*;
use super::environment_record::*;
use super::execution_context::*;
use super::object::*;
use super::parser::arrow_function_definitions::*;
use super::parser::async_function_definitions::*;
use super::parser::async_generator_function_definitions::*;
use super::parser::function_definitions::*;
use super::parser::generator_function_definitions::*;
use super::parser::parameter_lists::*;
use super::realm::*;
use super::strings::*;
use super::values::*;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ConstructorKind {
    Base,
    Derived,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ThisMode {
    Lexical,
    Strict,
    Global,
}

#[derive(Debug, Clone)]
pub enum ClassName {
    String(JSString),
    Symbol(Symbol),
    Private(PrivateName),
    Empty,
}

#[derive(Debug)]
pub struct ClassFieldDefinitionRecord {}

#[derive(Debug, Clone)]
pub enum BodySource {
    Function(Rc<FunctionBody>),
    Generator(Rc<GeneratorBody>),
    AsyncFunction(Rc<AsyncFunctionBody>),
    AsyncGenerator(Rc<AsyncGeneratorBody>),
    ConciseBody(Rc<ConciseBody>),
}

impl From<Rc<FunctionBody>> for BodySource {
    fn from(src: Rc<FunctionBody>) -> Self {
        Self::Function(src)
    }
}

impl From<Rc<ConciseBody>> for BodySource {
    fn from(src: Rc<ConciseBody>) -> Self {
        Self::ConciseBody(src)
    }
}

#[derive(Debug, Clone)]
pub enum ParamSource {
    FormalParameters(Rc<FormalParameters>),
    ArrowParameters(Rc<ArrowParameters>),
}

impl From<Rc<FormalParameters>> for ParamSource {
    fn from(src: Rc<FormalParameters>) -> Self {
        Self::FormalParameters(src)
    }
}
impl From<Rc<ArrowParameters>> for ParamSource {
    fn from(src: Rc<ArrowParameters>) -> Self {
        Self::ArrowParameters(src)
    }
}
impl ParamSource {
    pub fn expected_argument_count(&self) -> f64 {
        match self {
            ParamSource::FormalParameters(formals) => formals.expected_argument_count(),
            ParamSource::ArrowParameters(arrow) => arrow.expected_argument_count(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionObjectData {
    pub environment: Rc<dyn EnvironmentRecord>,
    private_environment: Option<Rc<RefCell<PrivateEnvironmentRecord>>>,
    formal_parameters: ParamSource,
    ecmascript_code: BodySource,
    constructor_kind: ConstructorKind,
    pub realm: Rc<RefCell<Realm>>,
    script_or_module: Option<ScriptOrModule>,
    pub this_mode: ThisMode,
    strict: bool,
    pub home_object: Option<Object>,
    source_text: String,
    fields: Vec<ClassFieldDefinitionRecord>,
    private_methods: Vec<PrivateElement>,
    class_field_initializer_name: ClassName,
    is_class_constructor: bool,
}

#[derive(Debug)]
pub struct FunctionObject {
    common: RefCell<CommonObjectData>,
    function_data: RefCell<FunctionObjectData>,
}

impl<'a> From<&'a FunctionObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a FunctionObject) -> Self {
        obj
    }
}

pub trait CallableObject: ObjectInterface {
    fn call(
        &self,
        agent: &mut Agent,
        self_object: &Object,
        this_argument: &ECMAScriptValue,
        arguments_list: &[ECMAScriptValue],
    ) -> Completion<ECMAScriptValue>;
}

pub trait ConstructableObject: CallableObject {
    fn construct(
        &self,
        agent: &mut Agent,
        self_object: &Object,
        arguments_list: &[ECMAScriptValue],
        new_target: &Object,
    ) -> Completion<ECMAScriptValue>;
}

impl ObjectInterface for FunctionObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn is_ordinary(&self) -> bool {
        true
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }
    fn to_function_obj(&self) -> Option<&dyn FunctionInterface> {
        Some(self)
    }
    fn to_callable_obj(&self) -> Option<&dyn CallableObject> {
        // Whereas this is _anything_ that implements [[Call]]
        Some(self)
    }
    fn is_callable_obj(&self) -> bool {
        true
    }

    fn get_prototype_of(&self, _agent: &mut Agent) -> Completion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }
    fn set_prototype_of(&self, _agent: &mut Agent, obj: Option<Object>) -> Completion<bool> {
        Ok(ordinary_set_prototype_of(self, obj))
    }
    fn is_extensible(&self, _agent: &mut Agent) -> Completion<bool> {
        Ok(ordinary_is_extensible(self))
    }
    fn prevent_extensions(&self, _agent: &mut Agent) -> Completion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }
    fn get_own_property(&self, _agent: &mut Agent, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }
    fn define_own_property(
        &self,
        agent: &mut Agent,
        key: PropertyKey,
        desc: PotentialPropertyDescriptor,
    ) -> Completion<bool> {
        ordinary_define_own_property(agent, self, key, desc)
    }
    fn has_property(&self, agent: &mut Agent, key: &PropertyKey) -> Completion<bool> {
        ordinary_has_property(agent, self, key)
    }
    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        ordinary_get(agent, self, key, receiver)
    }
    fn set(
        &self,
        agent: &mut Agent,
        key: PropertyKey,
        v: ECMAScriptValue,
        receiver: &ECMAScriptValue,
    ) -> Completion<bool> {
        ordinary_set(agent, self, key, v, receiver)
    }
    fn delete(&self, agent: &mut Agent, key: &PropertyKey) -> Completion<bool> {
        ordinary_delete(agent, self, key)
    }
    fn own_property_keys(&self, agent: &mut Agent) -> Completion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(agent, self))
    }
}

impl CallableObject for FunctionObject {
    fn call(
        &self,
        _agent: &mut Agent,
        _self_object: &Object,
        _this_argument: &ECMAScriptValue,
        _arguments_list: &[ECMAScriptValue],
    ) -> Completion<ECMAScriptValue> {
        todo!()
    }
}

impl FunctionInterface for FunctionObject {
    fn function_data(&self) -> &RefCell<FunctionObjectData> {
        &self.function_data
    }
}

impl FunctionObject {
    #[allow(clippy::too_many_arguments)]
    pub fn object(
        agent: &mut Agent,
        prototype: Option<Object>,
        environment: Rc<dyn EnvironmentRecord>,
        private_environment: Option<Rc<RefCell<PrivateEnvironmentRecord>>>,
        formal_parameters: ParamSource,
        ecmascript_code: BodySource,
        constructor_kind: ConstructorKind,
        realm: Rc<RefCell<Realm>>,
        script_or_module: Option<ScriptOrModule>,
        this_mode: ThisMode,
        strict: bool,
        home_object: Option<Object>,
        source_text: &str,
        fields: Vec<ClassFieldDefinitionRecord>,
        private_methods: Vec<PrivateElement>,
        class_field_initializer_name: ClassName,
        is_class_constructor: bool,
    ) -> Object {
        Object {
            o: Rc::new(Self {
                common: RefCell::new(CommonObjectData::new(agent, prototype, true, FUNCTION_OBJECT_SLOTS)),
                function_data: RefCell::new(FunctionObjectData {
                    environment,
                    private_environment,
                    formal_parameters,
                    ecmascript_code,
                    constructor_kind,
                    realm,
                    script_or_module,
                    this_mode,
                    strict,
                    home_object,
                    source_text: source_text.to_string(),
                    is_class_constructor,
                    fields,
                    private_methods,
                    class_field_initializer_name,
                }),
            }),
        }
    }
}

// SetFunctionName ( F, name [ , prefix ] )
//
// The abstract operation SetFunctionName takes arguments F (a function object) and name (a property key or Private
// Name) and optional argument prefix (a String). It adds a "name" property to F. It performs the following steps when
// called:
//
//      1. Assert: F is an extensible object that does not have a "name" own property.
//      2. If Type(name) is Symbol, then
//          a. Let description be name's [[Description]] value.
//          b. If description is undefined, set name to the empty String.
//          c. Else, set name to the string-concatenation of "[", description, and "]".
//      3. Else if name is a Private Name, then
//          a. Set name to name.[[Description]].
//      4. If F has an [[InitialName]] internal slot, then
//          a. Set F.[[InitialName]] to name.
//      5. If prefix is present, then
//          a. Set name to the string-concatenation of prefix, the code unit 0x0020 (SPACE), and name.
//          b. If F has an [[InitialName]] internal slot, then
//              i. Optionally, set F.[[InitialName]] to name.
//      6. Return ! DefinePropertyOrThrow(F, "name", PropertyDescriptor { [[Value]]: name, [[Writable]]: false,
//         [[Enumerable]]: false, [[Configurable]]: true }).
pub fn set_function_name(agent: &mut Agent, func: &Object, name: FunctionName, prefix: Option<JSString>) {
    let name_before_prefix = match name {
        FunctionName::String(s) => s,
        FunctionName::PrivateName(pn) => pn.description,
        FunctionName::Symbol(sym) => sym.description().map_or_else(
            || JSString::from(""),
            |description| {
                let mut n: Vec<u16> = vec!['[' as u16];
                n.extend_from_slice(description.as_slice());
                n.push(']' as u16);
                JSString::from(n)
            },
        ),
    };
    let name_after_prefix = match prefix {
        None => name_before_prefix,
        Some(pfx) => {
            let mut name: Vec<u16> = Vec::with_capacity(pfx.len() + name_before_prefix.len() + 1);
            name.extend_from_slice(pfx.as_slice());
            name.push(' ' as u16);
            name.extend_from_slice(name_before_prefix.as_slice());
            JSString::from(name)
        }
    };
    if let Some(builtin) = func.o.to_builtin_function_obj() {
        builtin.builtin_function_data().borrow_mut().initial_name = Some(FunctionName::from(name_after_prefix.clone()));
    }
    define_property_or_throw(
        agent,
        func,
        "name",
        PotentialPropertyDescriptor::new()
            .value(name_after_prefix)
            .writable(false)
            .enumerable(false)
            .configurable(true),
    )
    .unwrap()
}

// SetFunctionLength ( F, length )
//
// The abstract operation SetFunctionLength takes arguments F (a function object) and length (a non-negative integer or
// +∞). It adds a "length" property to F. It performs the following steps when called:
//
//      1. Assert: F is an extensible object that does not have a "length" own property.
//      2. Return ! DefinePropertyOrThrow(F, "length", PropertyDescriptor { [[Value]]: 𝔽(length), [[Writable]]: false,
//         [[Enumerable]]: false, [[Configurable]]: true }).
pub fn set_function_length(agent: &mut Agent, func: &Object, length: f64) {
    define_property_or_throw(
        agent,
        func,
        "length",
        PotentialPropertyDescriptor::new().value(length).writable(false).enumerable(false).configurable(true),
    )
    .unwrap();
}

///////////////////////////////////////////////////////////////////
/// BUILT IN FUNCTIONS
///////////////////////////////////////////////////////////////////

// A small little arguments iterator, useful for built-in functions.
// When you have
// fn builtin_function(..., arguments: &[ECMAScriptValue]) -> ...
// Then in your code you can say:
//      let mut args = FuncArgs::from(arguments);
//      let first_arg = args.next_arg();
//      let second_arg = args.next_arg();
// etc. If the args are there, you get them, if the arguments array is short, then you get undefined.
// args.remaining() returns an iterator over the "rest" of the args (since "next_arg" won't tell if you've "gotten to the end")
pub struct FuncArgs<'a> {
    iterator: std::slice::Iter<'a, ECMAScriptValue>,
    count: usize,
}
impl<'a> From<&'a [ECMAScriptValue]> for FuncArgs<'a> {
    fn from(source: &'a [ECMAScriptValue]) -> Self {
        let count = source.len();
        Self { iterator: source.iter(), count }
    }
}
impl<'a> FuncArgs<'a> {
    pub fn next_arg(&mut self) -> ECMAScriptValue {
        self.iterator.next().cloned().unwrap_or(ECMAScriptValue::Undefined)
    }
    pub fn count(&self) -> usize {
        self.count
    }
    pub fn remaining(&mut self) -> &mut std::slice::Iter<'a, ECMAScriptValue> {
        &mut self.iterator
    }
}

#[derive(Debug)]
pub enum FunctionName {
    String(JSString),
    Symbol(Symbol),
    PrivateName(PrivateName),
}

impl From<PropertyKey> for FunctionName {
    fn from(source: PropertyKey) -> Self {
        match source {
            PropertyKey::String(s) => FunctionName::String(s),
            PropertyKey::Symbol(s) => FunctionName::Symbol(s),
        }
    }
}
impl From<PrivateName> for FunctionName {
    fn from(source: PrivateName) -> Self {
        FunctionName::PrivateName(source)
    }
}

impl From<JSString> for FunctionName {
    fn from(source: JSString) -> Self {
        FunctionName::String(source)
    }
}

pub struct BuiltInFunctionData {
    pub realm: Rc<RefCell<Realm>>,
    pub initial_name: Option<FunctionName>,
    pub steps: fn(&mut Agent, ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>,
    pub is_constructor: bool,
}

impl fmt::Debug for BuiltInFunctionData {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), std::fmt::Error> {
        f.debug_struct("BuiltInFunctionData")
            .field("realm", &self.realm)
            .field("initial_name", &self.initial_name)
            .finish()
    }
}

impl BuiltInFunctionData {
    pub fn new(
        realm: Rc<RefCell<Realm>>,
        initial_name: Option<FunctionName>,
        steps: fn(&mut Agent, ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>,
        is_constructor: bool,
    ) -> Self {
        Self { realm, initial_name, steps, is_constructor }
    }
}

#[derive(Debug)]
pub struct BuiltInFunctionObject {
    common: RefCell<CommonObjectData>,
    builtin_data: RefCell<BuiltInFunctionData>,
}

impl<'a> From<&'a BuiltInFunctionObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a BuiltInFunctionObject) -> Self {
        obj
    }
}

impl BuiltinFunctionInterface for BuiltInFunctionObject {
    fn builtin_function_data(&self) -> &RefCell<BuiltInFunctionData> {
        &self.builtin_data
    }
}

impl BuiltInFunctionObject {
    pub fn new(
        agent: &mut Agent,
        prototype: Option<Object>,
        extensible: bool,
        realm: Rc<RefCell<Realm>>,
        initial_name: Option<FunctionName>,
        steps: fn(&mut Agent, ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>,
        is_constructor: bool,
    ) -> Rc<Self> {
        Rc::new(Self {
            common: RefCell::new(CommonObjectData::new(agent, prototype, extensible, BUILTIN_FUNCTION_SLOTS)),
            builtin_data: RefCell::new(BuiltInFunctionData::new(realm, initial_name, steps, is_constructor)),
        })
    }

    pub fn object(
        agent: &mut Agent,
        prototype: Option<Object>,
        extensible: bool,
        realm: Rc<RefCell<Realm>>,
        initial_name: Option<FunctionName>,
        steps: fn(&mut Agent, ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>,
        is_constructor: bool,
    ) -> Object {
        Object { o: Self::new(agent, prototype, extensible, realm, initial_name, steps, is_constructor) }
    }
}

pub trait BuiltinFunctionInterface {
    fn builtin_function_data(&self) -> &RefCell<BuiltInFunctionData>;
}

impl ObjectInterface for BuiltInFunctionObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn is_ordinary(&self) -> bool {
        true
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }
    fn to_function_obj(&self) -> Option<&dyn FunctionInterface> {
        None
    }
    fn to_callable_obj(&self) -> Option<&dyn CallableObject> {
        Some(self)
    }
    fn to_constructable(&self) -> Option<&dyn ConstructableObject> {
        let is_c = self.builtin_function_data().borrow().is_constructor;
        if is_c {
            Some(self)
        } else {
            None
        }
    }
    fn to_builtin_function_obj(&self) -> Option<&dyn BuiltinFunctionInterface> {
        Some(self)
    }
    fn is_callable_obj(&self) -> bool {
        true
    }

    fn get_prototype_of(&self, _agent: &mut Agent) -> Completion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }
    fn set_prototype_of(&self, _agent: &mut Agent, obj: Option<Object>) -> Completion<bool> {
        Ok(ordinary_set_prototype_of(self, obj))
    }
    fn is_extensible(&self, _agent: &mut Agent) -> Completion<bool> {
        Ok(ordinary_is_extensible(self))
    }
    fn prevent_extensions(&self, _agent: &mut Agent) -> Completion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }
    fn get_own_property(&self, _agent: &mut Agent, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }
    fn define_own_property(
        &self,
        agent: &mut Agent,
        key: PropertyKey,
        desc: PotentialPropertyDescriptor,
    ) -> Completion<bool> {
        ordinary_define_own_property(agent, self, key, desc)
    }
    fn has_property(&self, agent: &mut Agent, key: &PropertyKey) -> Completion<bool> {
        ordinary_has_property(agent, self, key)
    }
    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        ordinary_get(agent, self, key, receiver)
    }
    fn set(
        &self,
        agent: &mut Agent,
        key: PropertyKey,
        v: ECMAScriptValue,
        receiver: &ECMAScriptValue,
    ) -> Completion<bool> {
        ordinary_set(agent, self, key, v, receiver)
    }
    fn delete(&self, agent: &mut Agent, key: &PropertyKey) -> Completion<bool> {
        ordinary_delete(agent, self, key)
    }
    fn own_property_keys(&self, agent: &mut Agent) -> Completion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(agent, self))
    }
}

impl CallableObject for BuiltInFunctionObject {
    // [[Call]] ( thisArgument, argumentsList )
    //
    // The [[Call]] internal method of a built-in function object F takes arguments thisArgument (an ECMAScript language
    // value) and argumentsList (a List of ECMAScript language values). It performs the following steps when called:
    //
    //      1. Let callerContext be the running execution context.
    //      2. If callerContext is not already suspended, suspend callerContext.
    //      3. Let calleeContext be a new execution context.
    //      4. Set the Function of calleeContext to F.
    //      5. Let calleeRealm be F.[[Realm]].
    //      6. Set the Realm of calleeContext to calleeRealm.
    //      7. Set the ScriptOrModule of calleeContext to null.
    //      8. Perform any necessary implementation-defined initialization of calleeContext.
    //      9. Push calleeContext onto the execution context stack; calleeContext is now the running execution context.
    //      10. Let result be the Completion Record that is the result of evaluating F in a manner that conforms to the
    //          specification of F. thisArgument is the this value, argumentsList provides the named parameters, and the
    //          NewTarget value is undefined.
    //      11. Remove calleeContext from the execution context stack and restore callerContext as the running execution
    //          context.
    //      12. Return result.
    //
    // NOTE     | When calleeContext is removed from the execution context stack it must not be destroyed if it has been
    //          | suspended and retained by an accessible generator object for later resumption.
    fn call(
        &self,
        agent: &mut Agent,
        self_object: &Object,
        this_argument: &ECMAScriptValue,
        arguments_list: &[ECMAScriptValue],
    ) -> Completion<ECMAScriptValue> {
        assert_eq!(self.id(), self_object.o.id());
        let callee_context =
            ExecutionContext::new(Some(self_object.clone()), self.builtin_data.borrow().realm.clone(), None);
        agent.push_execution_context(callee_context);
        let result = (self.builtin_data.borrow().steps)(agent, this_argument.clone(), None, arguments_list);
        agent.pop_execution_context();

        result
    }
}

impl ConstructableObject for BuiltInFunctionObject {
    // [[Construct]] ( argumentsList, newTarget )
    //
    // The [[Construct]] internal method of a built-in function object F takes arguments argumentsList (a List of
    // ECMAScript language values) and newTarget (a constructor). The steps performed are the same as [[Call]] (see
    // 10.3.1) except that step 10 is replaced by:
    //
    // 10. Let result be the Completion Record that is the result of evaluating F in a manner that conforms to the
    //     specification of F. The this value is uninitialized, argumentsList provides the named parameters, and
    //     newTarget provides the NewTarget value.
    fn construct(
        &self,
        agent: &mut Agent,
        self_object: &Object,
        arguments_list: &[ECMAScriptValue],
        new_target: &Object,
    ) -> Completion<ECMAScriptValue> {
        assert_eq!(self.id(), self_object.o.id());
        let callee_context =
            ExecutionContext::new(Some(self_object.clone()), self.builtin_data.borrow().realm.clone(), None);
        agent.push_execution_context(callee_context);
        let result =
            (self.builtin_data.borrow().steps)(agent, ECMAScriptValue::Undefined, Some(new_target), arguments_list);
        agent.pop_execution_context();

        result
    }
}

// CreateBuiltinFunction ( behaviour, length, name, internalSlotsList [ , realm [ , prototype [ , prefix ] ] ] )
//
// The abstract operation CreateBuiltinFunction takes arguments behaviour, length (a non-negative integer or +∞), name
// (a property key), and internalSlotsList (a List of names of internal slots) and optional arguments realm (a Realm
// Record), prototype (an Object or null), and prefix (a String). internalSlotsList contains the names of additional
// internal slots that must be defined as part of the object. This operation creates a built-in function object. It
// performs the following steps when called:
//
//      1. Assert: behaviour is either an Abstract Closure, a set of algorithm steps, or some other definition of a
//         function's behaviour provided in this specification.
//      2. If realm is not present, set realm to the current Realm Record.
//      3. Assert: realm is a Realm Record.
//      4. If prototype is not present, set prototype to realm.[[Intrinsics]].[[%Function.prototype%]].
//      5. Let func be a new built-in function object that, when called, performs the action described by behaviour
//         using the provided arguments as the values of the corresponding parameters specified by behaviour. The new
//         function object has internal slots whose names are the elements of internalSlotsList, and an [[InitialName]]
//         internal slot.
//      6. Set func.[[Realm]] to realm.
//      7. Set func.[[Prototype]] to prototype.
//      8. Set func.[[Extensible]] to true.
//      9. Set func.[[InitialName]] to null.
//      10. Perform ! SetFunctionLength(func, length).
//      11. If prefix is not present, then
//          a. Perform ! SetFunctionName(func, name).
//      12. Else,
//          a. Perform ! SetFunctionName(func, name, prefix).
//      13. Return func.
//
// Each built-in function defined in this specification is created by calling the CreateBuiltinFunction abstract
// operation.
#[allow(clippy::too_many_arguments)]
pub fn create_builtin_function(
    agent: &mut Agent,
    behavior: fn(&mut Agent, ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>,
    is_constructor: bool,
    length: f64,
    name: PropertyKey,
    _internal_slots_list: &[InternalSlotName],
    realm: Option<Rc<RefCell<Realm>>>,
    prototype: Option<Object>,
    prefix: Option<JSString>,
) -> Object {
    let realm_to_use = realm.unwrap_or_else(|| agent.current_realm_record().unwrap());
    let prototype_to_use = prototype.unwrap_or_else(|| realm_to_use.borrow().intrinsics.function_prototype.clone());
    let func = BuiltInFunctionObject::object(
        agent,
        Some(prototype_to_use),
        true,
        realm_to_use,
        None,
        behavior,
        is_constructor,
    );
    set_function_length(agent, &func, length);
    set_function_name(agent, &func, FunctionName::from(name), prefix);
    func
}

impl FunctionDeclaration {
    #[allow(unused_variables)]
    pub fn instantiate_function_object(
        &self,
        agent: &mut Agent,
        env: Rc<dyn EnvironmentRecord>,
        private_env: Option<&PrivateEnvironmentRecord>,
    ) -> ECMAScriptValue {
        todo!()
    }
}

impl GeneratorDeclaration {
    #[allow(unused_variables)]
    pub fn instantiate_function_object(
        &self,
        agent: &mut Agent,
        env: Rc<dyn EnvironmentRecord>,
        private_env: Option<&PrivateEnvironmentRecord>,
    ) -> ECMAScriptValue {
        todo!()
    }
}

impl AsyncFunctionDeclaration {
    #[allow(unused_variables)]
    pub fn instantiate_function_object(
        &self,
        agent: &mut Agent,
        env: Rc<dyn EnvironmentRecord>,
        private_env: Option<&PrivateEnvironmentRecord>,
    ) -> ECMAScriptValue {
        todo!()
    }
}

impl AsyncGeneratorDeclaration {
    #[allow(unused_variables)]
    pub fn instantiate_function_object(
        &self,
        agent: &mut Agent,
        env: Rc<dyn EnvironmentRecord>,
        private_env: Option<&PrivateEnvironmentRecord>,
    ) -> ECMAScriptValue {
        todo!()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FunctionThisMode {
    LexicalThis,
    NonLexicalThis,
}

/// Create a ECMAScript Function Object, as though from source code
///
/// See [OrdinaryFunctionCreate](https://tc39.es/ecma262/#sec-ordinaryfunctioncreate) from ECMA-262.
#[allow(clippy::too_many_arguments)]
pub fn ordinary_function_create(
    agent: &mut Agent,
    function_prototype: Object,
    source_text: &str,
    parameter_list: ParamSource,
    body: BodySource,
    this_mode: FunctionThisMode,
    env: Rc<dyn EnvironmentRecord>,
    private_env: Option<Rc<RefCell<PrivateEnvironmentRecord>>>,
    strict: bool,
) -> Object {
    // OrdinaryFunctionCreate ( functionPrototype, sourceText, ParameterList, Body, thisMode, env, privateEnv )
    //
    // The abstract operation OrdinaryFunctionCreate takes arguments functionPrototype (an Object), sourceText (a
    // sequence of Unicode code points), ParameterList (a Parse Node), Body (a Parse Node), thisMode (lexical-this or
    // non-lexical-this), env (an Environment Record), and privateEnv (a PrivateEnvironment Record or null) and returns
    // a function object. It is used to specify the runtime creation of a new function with a default [[Call]] internal
    // method and no [[Construct]] internal method (although one may be subsequently added by an operation such as
    // MakeConstructor). sourceText is the source text of the syntactic definition of the function to be created. It
    // performs the following steps when called:
    //
    //  1. Let internalSlotsList be the internal slots listed in Table 33.
    //  2. Let F be OrdinaryObjectCreate(functionPrototype, internalSlotsList).
    //  3. Set F.[[Call]] to the definition specified in 10.2.1.
    //  4. Set F.[[SourceText]] to sourceText.
    //  5. Set F.[[FormalParameters]] to ParameterList.
    //  6. Set F.[[ECMAScriptCode]] to Body.
    //  7. If the source text matched by Body is strict mode code, let Strict be true; else let Strict be false.
    //  8. Set F.[[Strict]] to Strict.
    //  9. If thisMode is lexical-this, set F.[[ThisMode]] to lexical.
    //  10. Else if Strict is true, set F.[[ThisMode]] to strict.
    //  11. Else, set F.[[ThisMode]] to global.
    //  12. Set F.[[IsClassConstructor]] to false.
    //  13. Set F.[[Environment]] to env.
    //  14. Set F.[[PrivateEnvironment]] to privateEnv.
    //  15. Set F.[[ScriptOrModule]] to GetActiveScriptOrModule().
    //  16. Set F.[[Realm]] to the current Realm Record.
    //  17. Set F.[[HomeObject]] to undefined.
    //  18. Set F.[[Fields]] to a new empty List.
    //  19. Set F.[[PrivateMethods]] to a new empty List.
    //  20. Set F.[[ClassFieldInitializerName]] to empty.
    //  21. Let len be the ExpectedArgumentCount of ParameterList.
    //  22. Perform SetFunctionLength(F, len).
    //  23. Return F.
    let this_mode = match this_mode {
        FunctionThisMode::LexicalThis => ThisMode::Lexical,
        FunctionThisMode::NonLexicalThis => match strict {
            true => ThisMode::Strict,
            false => ThisMode::Global,
        },
    };
    let script_or_module = agent.get_active_script_or_module();
    let realm = agent.current_realm_record().unwrap();
    let prototype = Some(function_prototype);

    let arg_count = parameter_list.expected_argument_count();

    let func = FunctionObject::object(
        agent,
        prototype,
        env,
        private_env,
        parameter_list,
        body,
        ConstructorKind::Base,
        realm,
        script_or_module,
        this_mode,
        strict,
        None,
        source_text,
        Vec::<ClassFieldDefinitionRecord>::new(),
        Vec::<PrivateElement>::new(),
        ClassName::Empty,
        false,
    );

    set_function_length(agent, &func, arg_count);

    func
}

pub struct MakeConstructorOptionalArgs {
    writable_prototype: bool,
    prototype: Option<Object>,
}

#[allow(unused_variables)]
pub fn make_constructor(agent: &mut Agent, func: &Object, args: Option<MakeConstructorOptionalArgs>) {
    todo!()
}

#[cfg(test)]
mod tests;
