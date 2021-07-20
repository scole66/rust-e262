use super::agent::Agent;
use super::cr::{AbruptCompletion, AltCompletion, Completion, CompletionInfo};
use super::function_object::{create_builtin_function, Arguments};
use super::object::{
    define_property_or_throw, get, ordinary_create_from_constructor, ordinary_define_own_property, ordinary_delete, ordinary_get, ordinary_get_own_property, ordinary_get_prototype_of,
    ordinary_has_property, ordinary_is_extensible, ordinary_object_create, ordinary_own_property_keys, ordinary_prevent_extensions, ordinary_set, ordinary_set_prototype_of,
    CommonObjectData, InternalSlotName, Object, ObjectInterface, PotentialPropertyDescriptor, PropertyDescriptor, BUILTIN_FUNCTION_SLOTS, ERROR_OBJECT_SLOTS,
};
use super::realm::IntrinsicId;
use super::realm::Realm;
use super::strings::JSString;
use super::values::{to_string, ECMAScriptValue, PropertyKey};
use std::cell::RefCell;
use std::rc::Rc;

fn create_native_error_object(agent: &mut Agent, message: &str, error_constructor: Object, proto_id: IntrinsicId) -> Object {
    let o = ordinary_create_from_constructor(agent, &error_constructor, proto_id, &[InternalSlotName::ErrorData]).unwrap();
    let desc = PotentialPropertyDescriptor {
        value: Some(ECMAScriptValue::String(JSString::from(message))),
        writable: Some(true),
        enumerable: Some(false),
        configurable: Some(true),
        ..Default::default()
    };
    define_property_or_throw(agent, &o, PropertyKey::from("message"), desc).unwrap();
    o
}

pub fn create_type_error_object(agent: &mut Agent, message: &str) -> Object {
    let error_constructor = agent.intrinsic(IntrinsicId::TypeError);
    create_native_error_object(agent, message, error_constructor, IntrinsicId::TypeErrorPrototype)
}

pub fn create_type_error(agent: &mut Agent, message: &str) -> AbruptCompletion {
    AbruptCompletion::Throw(CompletionInfo { value: Some(ECMAScriptValue::Object(create_type_error_object(agent, message))), target: None })
}

pub fn create_reference_error_object(agent: &mut Agent, message: &str) -> Object {
    let cstr = agent.intrinsic(IntrinsicId::ReferenceError);
    create_native_error_object(agent, message, cstr, IntrinsicId::ReferenceErrorPrototype)
}

pub fn create_reference_error(agent: &mut Agent, message: &str) -> AbruptCompletion {
    AbruptCompletion::Throw(CompletionInfo { value: Some(ECMAScriptValue::Object(create_reference_error_object(agent, message))), target: None })
}

pub fn create_syntax_error_object(agent: &mut Agent, message: &str) -> Object {
    let cstr = agent.intrinsic(IntrinsicId::SyntaxError);
    create_native_error_object(agent, message, cstr, IntrinsicId::SyntaxErrorPrototype)
}

pub fn create_syntax_error(agent: &mut Agent, message: &str) -> AbruptCompletion {
    AbruptCompletion::Throw(CompletionInfo { value: Some(ECMAScriptValue::Object(create_syntax_error_object(agent, message))), target: None })
}

pub fn create_range_error_object(agent: &mut Agent, message: &str) -> Object {
    let cstr = agent.intrinsic(IntrinsicId::RangeError);
    create_native_error_object(agent, message, cstr, IntrinsicId::RangeErrorPrototype)
}

pub fn create_range_error(agent: &mut Agent, message: &str) -> AbruptCompletion {
    AbruptCompletion::Throw(CompletionInfo { value: Some(ECMAScriptValue::Object(create_range_error_object(agent, message))), target: None })
}

#[derive(Debug)]
pub struct ErrorObject {
    common: RefCell<CommonObjectData>,
}

impl ErrorObject {
    pub fn object(agent: &mut Agent, prototype: Option<Object>) -> Object {
        Object { o: Rc::new(Self { common: RefCell::new(CommonObjectData::new(agent, prototype, true, &ERROR_OBJECT_SLOTS)) }) }
    }
}

impl<'a> From<&'a ErrorObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a ErrorObject) -> Self {
        obj
    }
}

impl ObjectInterface for ErrorObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn is_ordinary(&self) -> bool {
        true
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }
    fn to_error_obj(&self) -> Option<&dyn ObjectInterface> {
        Some(self)
    }
    fn is_error_object(&self) -> bool {
        true
    }

    fn get_prototype_of(&self, _agent: &mut Agent) -> AltCompletion<Option<Object>> {
        Ok(ordinary_get_prototype_of(&*self))
    }
    fn set_prototype_of(&self, _agent: &mut Agent, obj: Option<Object>) -> AltCompletion<bool> {
        Ok(ordinary_set_prototype_of(&*self, obj))
    }
    fn is_extensible(&self, _agent: &mut Agent) -> AltCompletion<bool> {
        Ok(ordinary_is_extensible(&*self))
    }
    fn prevent_extensions(&self, _agent: &mut Agent) -> AltCompletion<bool> {
        Ok(ordinary_prevent_extensions(&*self))
    }
    fn get_own_property(&self, _agent: &mut Agent, key: &PropertyKey) -> AltCompletion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(&*self, key))
    }
    fn define_own_property(&self, agent: &mut Agent, key: PropertyKey, desc: PotentialPropertyDescriptor) -> AltCompletion<bool> {
        ordinary_define_own_property(agent, &*self, key, desc)
    }
    fn has_property(&self, agent: &mut Agent, key: &PropertyKey) -> AltCompletion<bool> {
        ordinary_has_property(agent, &*self, key)
    }
    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion {
        ordinary_get(agent, &*self, key, receiver)
    }
    fn set(&self, agent: &mut Agent, key: PropertyKey, v: ECMAScriptValue, receiver: &ECMAScriptValue) -> AltCompletion<bool> {
        ordinary_set(agent, &*self, key, v, receiver)
    }
    fn delete(&self, agent: &mut Agent, key: &PropertyKey) -> AltCompletion<bool> {
        ordinary_delete(agent, &*self, key)
    }
    fn own_property_keys(&self, _agent: &mut Agent) -> AltCompletion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(self))
    }
}

pub fn provision_error_intrinsic(agent: &mut Agent, realm: &Rc<RefCell<Realm>>) {
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();

    // The Error Constructor
    //
    // The Error constructor:
    //
    //    * is %Error%.
    //    * is the initial value of the "Error" property of the global object.
    //    * creates and initializes a new Error object when called as a function rather than as a constructor. Thus the
    //      function call Error(…) is equivalent to the object creation expression new Error(…) with the same
    //      arguments.
    //    * may be used as the value of an extends clause of a class definition. Subclass constructors that intend to
    //      inherit the specified Error behaviour must include a super call to the Error constructor to create and
    //      initialize subclass instances with an [[ErrorData]] internal slot.

    // Properties of the Error Constructor
    //
    // The Error constructor:
    //
    //    * has a [[Prototype]] internal slot whose value is %Function.prototype%.

    let error_constructor = create_builtin_function(
        agent,
        error_constructor_function,
        true,
        1_f64,
        PropertyKey::from("Error"),
        &BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype.clone()),
        None,
    );
    // Constructor Data Properties
    macro_rules! constructor_data {
        ( $value:expr, $name:expr ) => {{
            let key = PropertyKey::from($name);
            define_property_or_throw(
                agent,
                &error_constructor,
                key,
                PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from($value)), writable: Some(false), enumerable: Some(false), configurable: Some(false), ..Default::default() },
            )
            .unwrap();
        }};
    }

    // Properties of the Error Prototype Object
    //
    // The Error prototype object:
    //
    //    * is %Error.prototype%.
    //    * is an ordinary object.
    //    * is not an Error instance and does not have an [[ErrorData]] internal slot.
    //    * has a [[Prototype]] internal slot whose value is %Object.prototype%.
    let error_prototype = ordinary_object_create(agent, Some(&object_prototype), &[]);

    // Error.prototype
    //
    // The initial value of Error.prototype is the Error prototype object.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    constructor_data!(&error_prototype, "prototype");

    // Prototype Data Properties
    macro_rules! prototype_data {
        ( $value:expr, $name:expr ) => {{
            let key = PropertyKey::from($name);
            define_property_or_throw(
                agent,
                &error_prototype,
                key,
                PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from($value)), writable: Some(true), enumerable: Some(false), configurable: Some(true), ..Default::default() },
            )
            .unwrap();
        }};
    }
    // Error.prototype.constructor
    //
    // The initial value of Error.prototype.constructor is %Error%.
    prototype_data!(&error_constructor, "constructor");

    // Error.prototype.message
    //
    // The initial value of Error.prototype.message is the empty String.
    prototype_data!("", "message");

    // Error.prototype.name
    //
    // The initial value of Error.prototype.name is "Error".
    prototype_data!("Error", "name");

    // Prototype Function Properties
    macro_rules! prototype_function {
        ( $steps:expr, $name:expr, $length:expr ) => {
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(agent, $steps, false, $length, key.clone(), &BUILTIN_FUNCTION_SLOTS, Some(realm.clone()), Some(function_prototype.clone()), None);
            define_property_or_throw(
                agent,
                &error_prototype,
                key,
                PotentialPropertyDescriptor {
                    value: Some(ECMAScriptValue::from(function_object)),
                    writable: Some(true),
                    enumerable: Some(false),
                    configurable: Some(true),
                    ..Default::default()
                },
            )
            .unwrap();
        };
    }

    prototype_function!(error_prototype_tostring, "toString", 0.0);

    realm.borrow_mut().intrinsics.error = error_constructor;
    realm.borrow_mut().intrinsics.error_prototype = error_prototype;
}

// Error ( message )
//
// When the Error function is called with argument message, the following steps are taken:
//
//      1. If NewTarget is undefined, let newTarget be the active function object; else let newTarget be NewTarget.
//      2. Let O be ? OrdinaryCreateFromConstructor(newTarget, "%Error.prototype%", « [[ErrorData]] »).
//      3. If message is not undefined, then
//          a. Let msg be ? ToString(message).
//          b. Let msgDesc be the PropertyDescriptor { [[Value]]: msg, [[Writable]]: true, [[Enumerable]]: false,
//             [[Configurable]]: true }.
//          c. Perform ! DefinePropertyOrThrow(O, "message", msgDesc).
//      4. Return O.
pub fn error_constructor_function(agent: &mut Agent, _this_value: ECMAScriptValue, new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion {
    let mut args = Arguments::from(arguments);
    let message = args.next_arg();
    let afo: Option<Object>;

    let nt = match new_target {
        Some(objref) => Some(objref),
        None => {
            afo = agent.active_function_object();
            afo.as_ref()
        }
    }
    .unwrap();
    //let nt = new_target.unwrap_or_else(|| agent.active_function_object().unwrap());
    let o = ordinary_create_from_constructor(agent, nt, IntrinsicId::ErrorPrototype, &[InternalSlotName::ErrorData])?;
    if !message.is_undefined() {
        let msg = to_string(agent, message)?;
        let msg_desc = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(msg)), writable: Some(true), enumerable: Some(false), configurable: Some(true), ..Default::default() };
        define_property_or_throw(agent, &o, PropertyKey::from("message"), msg_desc).unwrap();
    }
    Ok(ECMAScriptValue::from(o))
}

// Error.prototype.toString ( )
//
// The following steps are taken:
//
//      1. Let O be the this value.
//      2. If Type(O) is not Object, throw a TypeError exception.
//      3. Let name be ? Get(O, "name").
//      4. If name is undefined, set name to "Error"; otherwise set name to ? ToString(name).
//      5. Let msg be ? Get(O, "message").
//      6. If msg is undefined, set msg to the empty String; otherwise set msg to ? ToString(msg).
//      7. If name is the empty String, return msg.
//      8. If msg is the empty String, return name.
//      9. Return the string-concatenation of name, the code unit 0x003A (COLON), the code unit 0x0020 (SPACE), and msg.
pub fn error_prototype_tostring(agent: &mut Agent, this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    if let ECMAScriptValue::Object(o) = this_value {
        let name_prop = get(agent, &o, &PropertyKey::from("name"))?;
        let name = if name_prop.is_undefined() { JSString::from("Error") } else { to_string(agent, name_prop)? };
        let msg_prop = get(agent, &o, &PropertyKey::from("message"))?;
        let msg = if msg_prop.is_undefined() { JSString::from("") } else { to_string(agent, msg_prop)? };
        if name.len() == 0 {
            Ok(ECMAScriptValue::from(msg))
        } else if msg.len() == 0 {
            Ok(ECMAScriptValue::from(name))
        } else {
            Ok(ECMAScriptValue::from(format!("{}: {}", name, msg)))
        }
    } else {
        Err(create_type_error(agent, "Error.prototype.toString called with non-object this value"))
    }
}

fn provision_native_error_intrinsics(
    agent: &mut Agent,
    realm: &Rc<RefCell<Realm>>,
    name: &str,
    native_error_constructor_function: fn(&mut Agent, ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion,
) -> (Object, Object) {
    let error = realm.borrow().intrinsics.error.clone();
    let error_prototype = realm.borrow().intrinsics.error_prototype.clone();

    // Each NativeError constructor:
    //
    //    * creates and initializes a new NativeError object when called as a function rather than as a constructor. A
    //      call of the object as a function is equivalent to calling it as a constructor with the same arguments. Thus
    //      the function call NativeError(…) is equivalent to the object creation expression new NativeError(…) with
    //      the same arguments.
    //    * may be used as the value of an extends clause of a class definition. Subclass constructors that intend to
    //      inherit the specified NativeError behaviour must include a super call to the NativeError constructor to
    //      create and initialize subclass instances with an [[ErrorData]] internal slot.
    //
    // Properties of the NativeError Constructors
    //
    // Each NativeError constructor:
    //
    //    * has a [[Prototype]] internal slot whose value is %Error%.
    //    * has a "name" property whose value is the String value "NativeError".

    let native_error_constructor =
        create_builtin_function(agent, native_error_constructor_function, true, 1_f64, PropertyKey::from("Error"), &BUILTIN_FUNCTION_SLOTS, Some(realm.clone()), Some(error), None);
    // Constructor Data Properties
    macro_rules! constructor_data {
        ( $value:expr, $name:expr ) => {{
            let key = PropertyKey::from($name);
            define_property_or_throw(
                agent,
                &native_error_constructor,
                key,
                PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from($value)), writable: Some(false), enumerable: Some(false), configurable: Some(false), ..Default::default() },
            )
            .unwrap();
        }};
    }
    constructor_data!(name, "name");

    // Properties of the NativeError Prototype Objects
    //
    // Each NativeError prototype object:
    //
    //    * is an ordinary object.
    //    * is not an Error instance and does not have an [[ErrorData]] internal slot.
    //    * has a [[Prototype]] internal slot whose value is %Error.prototype%.
    let native_error_prototype = ordinary_object_create(agent, Some(&error_prototype), &[]);

    // NativeError.prototype
    //
    // The initial value of NativeError.prototype is a NativeError prototype object (20.5.6.3). Each NativeError
    // constructor has a distinct prototype object.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    constructor_data!(&native_error_prototype, "prototype");

    // Prototype Data Properties
    macro_rules! prototype_data {
        ( $value:expr, $name:expr ) => {{
            let key = PropertyKey::from($name);
            define_property_or_throw(
                agent,
                &native_error_prototype,
                key,
                PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from($value)), writable: Some(true), enumerable: Some(false), configurable: Some(true), ..Default::default() },
            )
            .unwrap();
        }};
    }
    // NativeError.prototype.constructor
    //
    // The initial value of the "constructor" property of the prototype for a given NativeError constructor is the
    // corresponding intrinsic object %NativeError% (20.5.6.1).
    prototype_data!(&native_error_constructor, "constructor");

    // NativeError.prototype.message
    //
    // The initial value of the "message" property of the prototype for a given NativeError constructor is the empty
    // String.
    prototype_data!("", "message");

    // NativeError.prototype.name
    //
    // The initial value of the "name" property of the prototype for a given NativeError constructor is the String
    // value consisting of the name of the constructor (the name used instead of NativeError).
    prototype_data!(name, "name");

    (native_error_constructor, native_error_prototype)
}

// NativeError ( message )
//
// When a NativeError function is called with argument message, the following steps are taken:
//
//  1. If NewTarget is undefined, let newTarget be the active function object; else let newTarget be NewTarget.
//  2. Let O be ? OrdinaryCreateFromConstructor(newTarget, "%NativeError.prototype%", « [[ErrorData]] »).
//  3. If message is not undefined, then
//      a. Let msg be ? ToString(message).
//      b. Let msgDesc be the PropertyDescriptor { [[Value]]: msg, [[Writable]]: true, [[Enumerable]]: false,
//         [[Configurable]]: true }.
//      c. Perform ! DefinePropertyOrThrow(O, "message", msgDesc).
//  4. Return O.
//
// The actual value of the string passed in step 2 is either "%EvalError.prototype%", "%RangeError.prototype%",
// "%ReferenceError.prototype%", "%SyntaxError.prototype%", "%TypeError.prototype%", or "%URIError.prototype%"
// corresponding to which NativeError constructor is being defined.
fn native_error_constructor_function(agent: &mut Agent, _this_value: ECMAScriptValue, new_target: Option<&Object>, arguments: &[ECMAScriptValue], intrinsic_id: IntrinsicId) -> Completion {
    let mut args = Arguments::from(arguments);
    let message = args.next_arg();
    let afo: Option<Object>;

    let nt = match new_target {
        Some(objref) => Some(objref),
        None => {
            afo = agent.active_function_object();
            afo.as_ref()
        }
    }
    .unwrap();
    let o = ordinary_create_from_constructor(agent, nt, intrinsic_id, &[InternalSlotName::ErrorData])?;
    if !message.is_undefined() {
        let msg = to_string(agent, message)?;
        let msg_desc = PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(msg)), writable: Some(true), enumerable: Some(false), configurable: Some(true), ..Default::default() };
        define_property_or_throw(agent, &o, PropertyKey::from("message"), msg_desc).unwrap();
    }
    Ok(ECMAScriptValue::from(o))
}

fn type_error_constructor_function(agent: &mut Agent, this_value: ECMAScriptValue, new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion {
    native_error_constructor_function(agent, this_value, new_target, arguments, IntrinsicId::TypeErrorPrototype)
}
fn eval_error_constructor_function(agent: &mut Agent, this_value: ECMAScriptValue, new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion {
    native_error_constructor_function(agent, this_value, new_target, arguments, IntrinsicId::EvalErrorPrototype)
}
fn range_error_constructor_function(agent: &mut Agent, this_value: ECMAScriptValue, new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion {
    native_error_constructor_function(agent, this_value, new_target, arguments, IntrinsicId::RangeErrorPrototype)
}
fn reference_error_constructor_function(agent: &mut Agent, this_value: ECMAScriptValue, new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion {
    native_error_constructor_function(agent, this_value, new_target, arguments, IntrinsicId::ReferenceErrorPrototype)
}
fn syntax_error_constructor_function(agent: &mut Agent, this_value: ECMAScriptValue, new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion {
    native_error_constructor_function(agent, this_value, new_target, arguments, IntrinsicId::SyntaxErrorPrototype)
}
fn uri_error_constructor_function(agent: &mut Agent, this_value: ECMAScriptValue, new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion {
    native_error_constructor_function(agent, this_value, new_target, arguments, IntrinsicId::URIErrorPrototype)
}

pub fn provision_type_error_intrinsic(agent: &mut Agent, realm: &Rc<RefCell<Realm>>) {
    let (constructor, prototype) = provision_native_error_intrinsics(agent, realm, "TypeError", type_error_constructor_function);
    realm.borrow_mut().intrinsics.type_error = constructor;
    realm.borrow_mut().intrinsics.type_error_prototype = prototype;
}
pub fn provision_eval_error_intrinsic(agent: &mut Agent, realm: &Rc<RefCell<Realm>>) {
    let (constructor, prototype) = provision_native_error_intrinsics(agent, realm, "EvalError", eval_error_constructor_function);
    realm.borrow_mut().intrinsics.eval_error = constructor;
    realm.borrow_mut().intrinsics.eval_error_prototype = prototype;
}
pub fn provision_range_error_intrinsic(agent: &mut Agent, realm: &Rc<RefCell<Realm>>) {
    let (constructor, prototype) = provision_native_error_intrinsics(agent, realm, "RangeError", range_error_constructor_function);
    realm.borrow_mut().intrinsics.range_error = constructor;
    realm.borrow_mut().intrinsics.range_error_prototype = prototype;
}
pub fn provision_reference_error_intrinsic(agent: &mut Agent, realm: &Rc<RefCell<Realm>>) {
    let (constructor, prototype) = provision_native_error_intrinsics(agent, realm, "ReferenceError", reference_error_constructor_function);
    realm.borrow_mut().intrinsics.reference_error = constructor;
    realm.borrow_mut().intrinsics.reference_error_prototype = prototype;
}
pub fn provision_syntax_error_intrinsic(agent: &mut Agent, realm: &Rc<RefCell<Realm>>) {
    let (constructor, prototype) = provision_native_error_intrinsics(agent, realm, "SyntaxError", syntax_error_constructor_function);
    realm.borrow_mut().intrinsics.syntax_error = constructor;
    realm.borrow_mut().intrinsics.syntax_error_prototype = prototype;
}
pub fn provision_uri_error_intrinsic(agent: &mut Agent, realm: &Rc<RefCell<Realm>>) {
    let (constructor, prototype) = provision_native_error_intrinsics(agent, realm, "URIError", uri_error_constructor_function);
    realm.borrow_mut().intrinsics.uri_error = constructor;
    realm.borrow_mut().intrinsics.uri_error_prototype = prototype;
}
