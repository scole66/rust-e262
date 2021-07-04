use super::agent::Agent;
use super::cr::AltCompletion;
use super::object::{define_property_or_throw, immutable_prototype_exotic_object_create, ordinary_object_create, DeadObject, InternalSlotName, Object, PotentialPropertyDescriptor};
use super::object_object::attach_object_prototype_properties;
use super::strings::JSString;
use super::values::{ECMAScriptValue, PropertyKey};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct GlobalEnvironmentRecord {}
#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum IntrinsicIdentifier {
    Boolean,
    BooleanPrototype,
    ErrorPrototype,
    FunctionPrototype,
    ThrowTypeError,
    TypeErrorPrototype,
    ReferenceErrorPrototype,
    SyntaxErrorPrototype,
}
#[derive(Debug)]
pub struct Intrinsics {
    pub aggregate_error: Object,                    // aka "AggregateError", The AggregateError constructor
    pub array: Object,                              // aka "Array", The Array constructor
    pub array_buffer: Object,                       // ArrayBuffer	The ArrayBuffer constructor (25.1.3)
    pub array_iterator_prototype: Object,           // The prototype of Array iterator objects (23.1.5)
    pub async_from_sync_iterator_prototype: Object, // The prototype of async-from-sync iterator objects (27.1.4)
    pub async_function: Object,                     // The constructor of async function objects (27.7.1)
    pub async_generator_function: Object,           // The constructor of async iterator objects (27.4.1)
    pub async_iterator_prototype: Object,           // An object that all standard built-in async iterator objects indirectly inherit from
    pub atomics: Object,                            // Atomics	The Atomics object (25.4)
    pub big_int: Object,                            // BigInt	The BigInt constructor (21.2.1)
    pub big_int64_array: Object,                    // BigInt64Array	The BigInt64Array constructor (23.2)
    pub big_uint64_array: Object,                   // BigUint64Array	The BigUint64Array constructor (23.2)
    pub boolean: Object,                            // Boolean	The Boolean constructor (20.3.1)
    pub boolean_prototype: Object,                  // The Boolean object prototoype
    pub data_view: Object,                          // DataView	The DataView constructor (25.3.2)
    pub date: Object,                               // Date	The Date constructor (21.4.2)
    pub decode_uri: Object,                         // decodeURI	The decodeURI function (19.2.6.2)
    pub decode_uri_component: Object,               // decodeURIComponent	The decodeURIComponent function (19.2.6.3)
    pub encode_uri: Object,                         // encodeURI	The encodeURI function (19.2.6.4)
    pub encode_uri_component: Object,               // encodeURIComponent	The encodeURIComponent function (19.2.6.5)
    pub error: Object,                              // Error	The Error constructor (20.5.1)
    pub error_prototype: Object,                    //
    pub eval: Object,                               // eval	The eval function (19.2.1)
    pub eval_error: Object,                         // EvalError	The EvalError constructor (20.5.5.1)
    pub finalization_registry: Object,              // FinalizationRegistry	The FinalizationRegistry constructor (26.2.1)
    pub float32_array: Object,                      // Float32Array	The Float32Array constructor (23.2)
    pub float64_array: Object,                      // Float64Array	The Float64Array constructor (23.2)
    pub for_in_iterator_prototype: Object,          // The prototype of For-In iterator objects (14.7.5.10)
    pub function: Object,                           // Function	The Function constructor (20.2.1)
    pub function_prototype: Object,                 //
    pub generator_function: Object,                 // The constructor of generator objects (27.3.1)
    pub int8_array: Object,                         // Int8Array	The Int8Array constructor (23.2)
    pub int16_array: Object,                        // Int16Array	The Int16Array constructor (23.2)
    pub int32_array: Object,                        // Int32Array	The Int32Array constructor (23.2)
    pub is_finite: Object,                          // isFinite	The isFinite function (19.2.2)
    pub is_nan: Object,                             // isNaN	The isNaN function (19.2.3)
    pub iterator_prototype: Object,                 // An object that all standard built-in iterator objects indirectly inherit from
    pub json: Object,                               // JSON	The JSON object (25.5)
    pub map: Object,                                // Map	The Map constructor (24.1.1)
    pub map_iterator_prototype: Object,             // The prototype of Map iterator objects (24.1.5)
    pub math: Object,                               // Math	The Math object (21.3)
    pub number: Object,                             // Number	The Number constructor (21.1.1)
    pub object: Object,                             // Object	The Object constructor (20.1.1)
    pub object_prototype: Object,                   // The Object prototype object
    pub parse_float: Object,                        // parseFloat	The parseFloat function (19.2.4)
    pub parse_int: Object,                          // parseInt	The parseInt function (19.2.5)
    pub promise: Object,                            // Promise	The Promise constructor (27.2.3)
    pub proxy: Object,                              // Proxy	The Proxy constructor (28.2.1)
    pub range_error: Object,                        // RangeError	The RangeError constructor (20.5.5.2)
    pub reference_error: Object,                    // ReferenceError	The ReferenceError constructor (20.5.5.3)
    pub reference_error_prototype: Object,          //
    pub reflect: Object,                            // Reflect	The Reflect object (28.1)
    pub reg_exp: Object,                            // RegExp	The RegExp constructor (22.2.3)
    pub reg_exp_string_iterator_prototype: Object,  // The prototype of RegExp String Iterator objects (22.2.7)
    pub set: Object,                                // Set	The Set constructor (24.2.1)
    pub set_iterator_prototype: Object,             // The prototype of Set iterator objects (24.2.5)
    pub shared_array_buffer: Object,                // SharedArrayBuffer	The SharedArrayBuffer constructor (25.2.2)
    pub string: Object,                             // String	The String constructor (22.1.1)
    pub string_iterator_prototype: Object,          // The prototype of String iterator objects (22.1.5)
    pub symbol: Object,                             // Symbol	The Symbol constructor (20.4.1)
    pub syntax_error: Object,                       // SyntaxError	The SyntaxError constructor (20.5.5.4)
    pub syntax_error_prototype: Object,             //
    pub throw_type_error: Object,                   // A function object that unconditionally throws a new instance of %TypeError%
    pub typed_array: Object,                        // The super class of all typed Array constructors (23.2.1)
    pub type_error: Object,                         // TypeError	The TypeError constructor (20.5.5.5)
    pub type_error_prototype: Object,               //
    pub uint8_array: Object,                        // Uint8Array	The Uint8Array constructor (23.2)
    pub uint8_clampedarray: Object,                 // Uint8ClampedArray	The Uint8ClampedArray constructor (23.2)
    pub uint16_array: Object,                       // Uint16Array	The Uint16Array constructor (23.2)
    pub uint32_array: Object,                       // Uint32Array	The Uint32Array constructor (23.2)
    pub uri_error: Object,                          // URIError	The URIError constructor (20.5.5.6)
    pub weak_map: Object,                           // WeakMap	The WeakMap constructor (24.3.1)
    pub weak_ref: Object,                           // WeakRef	The WeakRef constructor (26.1.1)
    pub weak_set: Object,                           // WeakSet	The WeakSet constructor (24.4.1)
}

impl Intrinsics {
    pub fn get(&self, id: IntrinsicIdentifier) -> Object {
        match id {
            IntrinsicIdentifier::FunctionPrototype => &self.function_prototype,
            IntrinsicIdentifier::ThrowTypeError => &self.throw_type_error,
            IntrinsicIdentifier::BooleanPrototype => &self.boolean_prototype,
            IntrinsicIdentifier::Boolean => &self.boolean,
            IntrinsicIdentifier::ErrorPrototype => &self.error_prototype,
            IntrinsicIdentifier::TypeErrorPrototype => &self.type_error_prototype,
            IntrinsicIdentifier::ReferenceErrorPrototype => &self.reference_error_prototype,
            IntrinsicIdentifier::SyntaxErrorPrototype => &self.syntax_error_prototype,
        }
        .clone()
    }
}

#[derive(Debug)]
pub struct Realm {
    pub intrinsics: Intrinsics,
    pub global_object: Option<Object>,
    pub global_env: Option<GlobalEnvironmentRecord>,
    // TemplateMap: later, when needed
}

pub fn get_function_realm<'a, 'b>(_agent: &mut Agent, _obj: &'b Object) -> AltCompletion<&'a Realm> {
    todo!()
}

// CreateRealm ( )
//
// The abstract operation CreateRealm takes no arguments. It performs the following steps when called:
//
//  1. Let realmRec be a new Realm Record.
//  2. Perform CreateIntrinsics(realmRec).
//  3. Set realmRec.[[GlobalObject]] to undefined.
//  4. Set realmRec.[[GlobalEnv]] to undefined.
//  5. Set realmRec.[[TemplateMap]] to a new empty List.
//  6. Return realmRec.
pub fn create_realm(agent: &mut Agent) -> Rc<RefCell<Realm>> {
    let r = Rc::new(RefCell::new(Realm { intrinsics: dead_intrinsics(agent), global_object: None, global_env: None }));
    create_intrinsics(agent, r.clone());
    r
}

fn dead_intrinsics(agent: &mut Agent) -> Intrinsics {
    let dead = DeadObject::object(agent);
    Intrinsics {
        aggregate_error: dead.clone(),
        array: dead.clone(),
        array_buffer: dead.clone(),
        array_iterator_prototype: dead.clone(),
        async_from_sync_iterator_prototype: dead.clone(),
        async_function: dead.clone(),
        async_generator_function: dead.clone(),
        async_iterator_prototype: dead.clone(),
        atomics: dead.clone(),
        big_int: dead.clone(),
        big_int64_array: dead.clone(),
        big_uint64_array: dead.clone(),
        boolean: dead.clone(),
        boolean_prototype: dead.clone(),
        data_view: dead.clone(),
        date: dead.clone(),
        decode_uri: dead.clone(),
        decode_uri_component: dead.clone(),
        encode_uri: dead.clone(),
        encode_uri_component: dead.clone(),
        error: dead.clone(),
        error_prototype: dead.clone(),
        eval: dead.clone(),
        eval_error: dead.clone(),
        finalization_registry: dead.clone(),
        float32_array: dead.clone(),
        float64_array: dead.clone(),
        for_in_iterator_prototype: dead.clone(),
        function: dead.clone(),
        function_prototype: dead.clone(),
        generator_function: dead.clone(),
        int8_array: dead.clone(),
        int16_array: dead.clone(),
        int32_array: dead.clone(),
        is_finite: dead.clone(),
        is_nan: dead.clone(),
        iterator_prototype: dead.clone(),
        json: dead.clone(),
        map: dead.clone(),
        map_iterator_prototype: dead.clone(),
        math: dead.clone(),
        number: dead.clone(),
        object: dead.clone(),
        object_prototype: dead.clone(),
        parse_float: dead.clone(),
        parse_int: dead.clone(),
        promise: dead.clone(),
        proxy: dead.clone(),
        range_error: dead.clone(),
        reference_error: dead.clone(),
        reference_error_prototype: dead.clone(),
        reflect: dead.clone(),
        reg_exp: dead.clone(),
        reg_exp_string_iterator_prototype: dead.clone(),
        set: dead.clone(),
        set_iterator_prototype: dead.clone(),
        shared_array_buffer: dead.clone(),
        string: dead.clone(),
        string_iterator_prototype: dead.clone(),
        symbol: dead.clone(),
        syntax_error: dead.clone(),
        syntax_error_prototype: dead.clone(),
        throw_type_error: dead.clone(),
        typed_array: dead.clone(),
        type_error: dead.clone(),
        type_error_prototype: dead.clone(),
        uint8_array: dead.clone(),
        uint8_clampedarray: dead.clone(),
        uint16_array: dead.clone(),
        uint32_array: dead.clone(),
        uri_error: dead.clone(),
        weak_map: dead.clone(),
        weak_ref: dead.clone(),
        weak_set: dead,
    }
}

// CreateIntrinsics ( realmRec )
//
// The abstract operation CreateIntrinsics takes argument realmRec. It performs the following steps when called:
//
//  1. Let intrinsics be a new Record.
//  2. Set realmRec.[[Intrinsics]] to intrinsics.
//  3. Set fields of intrinsics with the values listed in Table 8. The field names are the names listed in column one of
//     the table. The value of each field is a new object value fully and recursively populated with property values as
//     defined by the specification of each object in clauses 19 through 28. All object property values are newly
//     created object values. All values that are built-in function objects are created by performing
//     CreateBuiltinFunction(steps, length, name, slots, realmRec, prototype) where steps is the definition of that
//     function provided by this specification, name is the initial value of the function's name property, length is the
//     initial value of the function's length property, slots is a list of the names, if any, of the function's
//     specified internal slots, and prototype is the specified value of the function's [[Prototype]] internal slot. The
//     creation of the intrinsics and their properties must be ordered to avoid any dependencies upon objects that have
//     not yet been created.
//  4. Perform AddRestrictedFunctionProperties(intrinsics.[[%Function.prototype%]], realmRec).
//  5. Return intrinsics.
pub fn create_intrinsics(agent: &mut Agent, realm_rec: Rc<RefCell<Realm>>) {
    // ToDo: All of step 3.

    // %Object.prototype%
    realm_rec.borrow_mut().intrinsics.object_prototype = immutable_prototype_exotic_object_create(agent, None);
    // %Function.prototype%
    let fp = ordinary_object_create(agent, Some(&realm_rec.borrow().intrinsics.object_prototype), &[]);
    realm_rec.borrow_mut().intrinsics.function_prototype = fp;
    // %ThrowTypeError%
    let tte = ordinary_object_create(agent, Some(&realm_rec.borrow().intrinsics.function_prototype), &[]);
    tte.o.prevent_extensions().unwrap();
    realm_rec.borrow_mut().intrinsics.throw_type_error = tte;
    ///////////////////////////////////////////////////////////////////
    // %Boolean% and %Boolean.prototype%
    let boolproto = ordinary_object_create(agent, Some(&realm_rec.borrow().intrinsics.object_prototype), &[InternalSlotName::BooleanData]);
    realm_rec.borrow_mut().intrinsics.boolean_prototype = boolproto;
    let bool_constructor = ordinary_object_create(agent, Some(&realm_rec.borrow().intrinsics.function_prototype), &[]);
    define_property_or_throw(
        agent,
        &bool_constructor,
        &PropertyKey::from("prototype"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::Object(realm_rec.borrow().intrinsics.boolean_prototype.clone())),
            writable: Some(false),
            enumerable: Some(false),
            configurable: Some(false),
            ..Default::default()
        },
    )
    .unwrap();
    realm_rec.borrow_mut().intrinsics.boolean = bool_constructor;
    define_property_or_throw(
        agent,
        &realm_rec.borrow().intrinsics.boolean_prototype,
        &PropertyKey::from("constructor"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::Object(realm_rec.borrow().intrinsics.boolean.clone())),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();
    ///////////////////////////////////////////////////////////////////
    // %Error% and %Error.prototype%
    let error_proto = ordinary_object_create(agent, Some(&realm_rec.borrow().intrinsics.object_prototype), &[]);
    realm_rec.borrow_mut().intrinsics.error_prototype = error_proto;
    let error_constructor = ordinary_object_create(agent, Some(&realm_rec.borrow().intrinsics.function_prototype), &[]);
    define_property_or_throw(
        agent,
        &error_constructor,
        &PropertyKey::from("prototype"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::Object(realm_rec.borrow().intrinsics.error_prototype.clone())),
            writable: Some(false),
            enumerable: Some(false),
            configurable: Some(false),
            ..Default::default()
        },
    )
    .unwrap();
    realm_rec.borrow_mut().intrinsics.error = error_constructor;
    define_property_or_throw(
        agent,
        &realm_rec.borrow().intrinsics.error_prototype,
        &PropertyKey::from("constructor"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::Object(realm_rec.borrow().intrinsics.error.clone())),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();
    define_property_or_throw(
        agent,
        &realm_rec.borrow().intrinsics.error_prototype,
        &PropertyKey::from("message"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::String(JSString::from(""))),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();
    define_property_or_throw(
        agent,
        &realm_rec.borrow().intrinsics.error_prototype,
        &PropertyKey::from("name"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::String(JSString::from("Error"))),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();
    ///////////////////////////////////////////////////////////////////
    // %TypeError% and %TypeError.prototype%
    let type_error_proto = ordinary_object_create(agent, Some(&realm_rec.borrow().intrinsics.error_prototype), &[]);
    realm_rec.borrow_mut().intrinsics.type_error_prototype = type_error_proto;
    let type_error_constructor = ordinary_object_create(agent, Some(&realm_rec.borrow().intrinsics.function_prototype), &[]);
    define_property_or_throw(
        agent,
        &type_error_constructor,
        &PropertyKey::from("prototype"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::Object(realm_rec.borrow().intrinsics.type_error_prototype.clone())),
            writable: Some(false),
            enumerable: Some(false),
            configurable: Some(false),
            ..Default::default()
        },
    )
    .unwrap();
    define_property_or_throw(
        agent,
        &type_error_constructor,
        &PropertyKey::from("name"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::String(JSString::from("TypeError"))),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();
    realm_rec.borrow_mut().intrinsics.type_error = type_error_constructor;
    define_property_or_throw(
        agent,
        &realm_rec.borrow().intrinsics.type_error_prototype,
        &PropertyKey::from("constructor"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::Object(realm_rec.borrow().intrinsics.error.clone())),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();
    define_property_or_throw(
        agent,
        &realm_rec.borrow().intrinsics.type_error_prototype,
        &PropertyKey::from("message"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::String(JSString::from(""))),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();
    define_property_or_throw(
        agent,
        &realm_rec.borrow().intrinsics.type_error_prototype,
        &PropertyKey::from("name"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::String(JSString::from("TypeError"))),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();
    ///////////////////////////////////////////////////////////////////
    // %ReferenceError% and %ReferenceError.prototype%
    let reference_error_proto = ordinary_object_create(agent, Some(&realm_rec.borrow().intrinsics.error_prototype), &[]);
    realm_rec.borrow_mut().intrinsics.reference_error_prototype = reference_error_proto;
    let reference_error_constructor = ordinary_object_create(agent, Some(&realm_rec.borrow().intrinsics.function_prototype), &[]);
    define_property_or_throw(
        agent,
        &reference_error_constructor,
        &PropertyKey::from("prototype"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::Object(realm_rec.borrow().intrinsics.reference_error_prototype.clone())),
            writable: Some(false),
            enumerable: Some(false),
            configurable: Some(false),
            ..Default::default()
        },
    )
    .unwrap();
    define_property_or_throw(
        agent,
        &reference_error_constructor,
        &PropertyKey::from("name"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::String(JSString::from("ReferenceError"))),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();
    realm_rec.borrow_mut().intrinsics.reference_error = reference_error_constructor;
    define_property_or_throw(
        agent,
        &realm_rec.borrow().intrinsics.reference_error_prototype,
        &PropertyKey::from("constructor"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::Object(realm_rec.borrow().intrinsics.error.clone())),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();
    define_property_or_throw(
        agent,
        &realm_rec.borrow().intrinsics.reference_error_prototype,
        &PropertyKey::from("message"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::String(JSString::from(""))),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();
    define_property_or_throw(
        agent,
        &realm_rec.borrow().intrinsics.reference_error_prototype,
        &PropertyKey::from("name"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::String(JSString::from("ReferenceError"))),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();
    ///////////////////////////////////////////////////////////////////
    // %SyntaxError% and %SyntaxError.prototype%
    let syntax_error_proto = ordinary_object_create(agent, Some(&realm_rec.borrow().intrinsics.error_prototype), &[]);
    realm_rec.borrow_mut().intrinsics.syntax_error_prototype = syntax_error_proto;
    let syntax_error_constructor = ordinary_object_create(agent, Some(&realm_rec.borrow().intrinsics.function_prototype), &[]);
    define_property_or_throw(
        agent,
        &syntax_error_constructor,
        &PropertyKey::from("prototype"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::Object(realm_rec.borrow().intrinsics.syntax_error_prototype.clone())),
            writable: Some(false),
            enumerable: Some(false),
            configurable: Some(false),
            ..Default::default()
        },
    )
    .unwrap();
    define_property_or_throw(
        agent,
        &syntax_error_constructor,
        &PropertyKey::from("name"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::String(JSString::from("SyntaxError"))),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();
    realm_rec.borrow_mut().intrinsics.syntax_error = syntax_error_constructor;
    define_property_or_throw(
        agent,
        &realm_rec.borrow().intrinsics.syntax_error_prototype,
        &PropertyKey::from("constructor"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::Object(realm_rec.borrow().intrinsics.error.clone())),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();
    define_property_or_throw(
        agent,
        &realm_rec.borrow().intrinsics.syntax_error_prototype,
        &PropertyKey::from("message"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::String(JSString::from(""))),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();
    define_property_or_throw(
        agent,
        &realm_rec.borrow().intrinsics.syntax_error_prototype,
        &PropertyKey::from("name"),
        &PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::String(JSString::from("SyntaxError"))),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();

    add_restricted_function_properties(agent, &realm_rec.borrow().intrinsics.function_prototype, realm_rec.clone());

    attach_object_prototype_properties(agent, realm_rec.clone(), &realm_rec.borrow().intrinsics.object_prototype);
}

// From function objects...
// AddRestrictedFunctionProperties ( F, realm )
//
// The abstract operation AddRestrictedFunctionProperties takes arguments F (a function object) and realm (a Realm
// Record). It performs the following steps when called:
//
// 1. Assert: realm.[[Intrinsics]].[[%ThrowTypeError%]] exists and has been initialized.
// 2. Let thrower be realm.[[Intrinsics]].[[%ThrowTypeError%]].
// 3. Perform ! DefinePropertyOrThrow(F, "caller", PropertyDescriptor { [[Get]]: thrower, [[Set]]: thrower,
//    [[Enumerable]]: false, [[Configurable]]: true }).
// 4. Return ! DefinePropertyOrThrow(F, "arguments", PropertyDescriptor { [[Get]]: thrower, [[Set]]: thrower,
//    [[Enumerable]]: false, [[Configurable]]: true }).
pub fn add_restricted_function_properties(agent: &mut Agent, f: &Object, realm: Rc<RefCell<Realm>>) {
    let thrower = ECMAScriptValue::Object(realm.borrow().intrinsics.throw_type_error.clone());
    define_property_or_throw(
        agent,
        f,
        &PropertyKey::from("caller"),
        &PotentialPropertyDescriptor { get: Some(thrower.clone()), set: Some(thrower.clone()), enumerable: Some(false), configurable: Some(true), ..Default::default() },
    )
    .unwrap();
    define_property_or_throw(
        agent,
        f,
        &PropertyKey::from("arguments"),
        &PotentialPropertyDescriptor { get: Some(thrower.clone()), set: Some(thrower), enumerable: Some(false), configurable: Some(true), ..Default::default() },
    )
    .unwrap();
}
