use super::*;
use std::cell::RefCell;
use std::rc::Rc;

fn create_native_error_object_internal(
    message: JSString,
    error_constructor: &Object,
    proto_id: IntrinsicId,
    location: Option<Location>,
) -> Object {
    let o = error_constructor.ordinary_create_from_constructor(proto_id, &[InternalSlotName::ErrorData]).unwrap();
    let desc = PotentialPropertyDescriptor::new().value(message).writable(true).enumerable(false).configurable(true);
    define_property_or_throw(&o, "message", desc).unwrap();
    if let Some(location) = location {
        let obj_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let loc = ordinary_object_create(Some(obj_proto), &[]);
        define_property_or_throw(
            &loc,
            "line",
            PotentialPropertyDescriptor::new().value(location.starting_line).writable(true).configurable(true),
        )
        .unwrap();
        define_property_or_throw(
            &loc,
            "column",
            PotentialPropertyDescriptor::new().value(location.starting_column).writable(true).configurable(true),
        )
        .unwrap();
        define_property_or_throw(
            &loc,
            "byte_length",
            PotentialPropertyDescriptor::new().value(location.span.length).writable(true).configurable(true),
        )
        .unwrap();
        define_property_or_throw(
            &o,
            "location",
            PotentialPropertyDescriptor::new().value(loc).writable(true).configurable(true),
        )
        .unwrap();
    }
    o
}

fn create_native_error_object(
    message: impl Into<JSString>,
    error_constructor: &Object,
    proto_id: IntrinsicId,
    location: Option<Location>,
) -> Object {
    let message = message.into();
    create_native_error_object_internal(message, error_constructor, proto_id, location)
}

pub fn create_type_error_object(message: impl Into<JSString>) -> Object {
    let error_constructor = intrinsic(IntrinsicId::TypeError);
    create_native_error_object(message, &error_constructor, IntrinsicId::TypeErrorPrototype, None)
}

pub fn create_type_error(message: impl Into<JSString>) -> AbruptCompletion {
    AbruptCompletion::Throw { value: ECMAScriptValue::Object(create_type_error_object(message)) }
}

pub fn create_reference_error_object(message: impl Into<JSString>) -> Object {
    let cstr = intrinsic(IntrinsicId::ReferenceError);
    create_native_error_object(message, &cstr, IntrinsicId::ReferenceErrorPrototype, None)
}

pub fn create_reference_error(message: impl Into<JSString>) -> AbruptCompletion {
    AbruptCompletion::Throw { value: ECMAScriptValue::Object(create_reference_error_object(message)) }
}

pub fn create_syntax_error_object(message: impl Into<JSString>, location: Option<Location>) -> Object {
    let cstr = intrinsic(IntrinsicId::SyntaxError);
    create_native_error_object(message, &cstr, IntrinsicId::SyntaxErrorPrototype, location)
}

pub fn create_syntax_error(message: impl Into<JSString>, location: Option<Location>) -> AbruptCompletion {
    AbruptCompletion::Throw { value: ECMAScriptValue::Object(create_syntax_error_object(message, location)) }
}

pub fn create_range_error_object(message: impl Into<JSString>) -> Object {
    let cstr = intrinsic(IntrinsicId::RangeError);
    create_native_error_object(message, &cstr, IntrinsicId::RangeErrorPrototype, None)
}

pub fn create_range_error(message: impl Into<JSString>) -> AbruptCompletion {
    AbruptCompletion::Throw { value: ECMAScriptValue::Object(create_range_error_object(message)) }
}

pub fn create_eval_error_object(message: impl Into<JSString>) -> Object {
    let cstr = intrinsic(IntrinsicId::EvalError);
    create_native_error_object(message, &cstr, IntrinsicId::EvalErrorPrototype, None)
}

pub fn create_eval_error(message: impl Into<JSString>) -> AbruptCompletion {
    AbruptCompletion::Throw { value: ECMAScriptValue::Object(create_eval_error_object(message)) }
}

pub fn create_uri_error_object(message: impl Into<JSString>) -> Object {
    let cstr = intrinsic(IntrinsicId::URIError);
    create_native_error_object(message, &cstr, IntrinsicId::URIErrorPrototype, None)
}

pub fn create_uri_error(message: impl Into<JSString>) -> AbruptCompletion {
    AbruptCompletion::Throw { value: ECMAScriptValue::Object(create_uri_error_object(message)) }
}

#[derive(Debug)]
pub struct ErrorObject {
    common: RefCell<CommonObjectData>,
}

impl ErrorObject {
    pub fn new(prototype: Option<Object>) -> Self {
        Self { common: RefCell::new(CommonObjectData::new(prototype, true, ERROR_OBJECT_SLOTS)) }
    }
    pub fn object(prototype: Option<Object>) -> Object {
        Object { o: Rc::new(Self::new(prototype)) }
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
    fn uses_ordinary_get_prototype_of(&self) -> bool {
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
    fn kind(&self) -> &'static str {
        ERROR_TAG
    }

    fn get_prototype_of(&self) -> Completion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }
    fn set_prototype_of(&self, obj: Option<Object>) -> Completion<bool> {
        Ok(ordinary_set_prototype_of(self, obj))
    }
    fn is_extensible(&self) -> Completion<bool> {
        Ok(ordinary_is_extensible(self))
    }
    fn prevent_extensions(&self) -> Completion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }
    fn get_own_property(&self, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }
    fn define_own_property(&self, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool> {
        ordinary_define_own_property(self, key, desc)
    }
    fn has_property(&self, key: &PropertyKey) -> Completion<bool> {
        ordinary_has_property(self, key)
    }
    fn get(&self, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        ordinary_get(self, key, receiver)
    }
    fn set(&self, key: PropertyKey, v: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        ordinary_set(self, key, v, receiver)
    }
    fn delete(&self, key: &PropertyKey) -> Completion<bool> {
        ordinary_delete(self, key)
    }
    fn own_property_keys(&self) -> Completion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(self))
    }
}

pub fn provision_error_intrinsic(realm: &Rc<RefCell<Realm>>) {
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
        error_constructor_function,
        true,
        1_f64,
        PropertyKey::from("Error"),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype.clone()),
        None,
    );
    // Constructor Data Properties
    macro_rules! constructor_data {
        ( $value:expr, $name:expr ) => {{
            define_property_or_throw(
                &error_constructor,
                $name,
                PotentialPropertyDescriptor::new().value($value).writable(false).enumerable(false).configurable(false),
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
    let error_prototype = ordinary_object_create(Some(object_prototype), &[]);

    // Error.prototype
    //
    // The initial value of Error.prototype is the Error prototype object.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    constructor_data!(&error_prototype, "prototype");

    // Prototype Data Properties
    macro_rules! prototype_data {
        ( $value:expr, $name:expr ) => {{
            define_property_or_throw(
                &error_prototype,
                $name,
                PotentialPropertyDescriptor::new().value($value).writable(true).enumerable(false).configurable(true),
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
            let function_object = create_builtin_function(
                $steps,
                false,
                $length,
                key.clone(),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm.clone()),
                Some(function_prototype.clone()),
                None,
            );
            define_property_or_throw(
                &error_prototype,
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
pub fn error_constructor_function(
    this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    native_error_constructor_function(this_value, new_target, arguments, IntrinsicId::Error)
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
pub fn error_prototype_tostring(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    if let ECMAScriptValue::Object(o) = this_value {
        let name_prop = o.get(&PropertyKey::from("name"))?;
        let name = if name_prop.is_undefined() { JSString::from("Error") } else { to_string(name_prop)? };
        let msg_prop = o.get(&PropertyKey::from("message"))?;
        let msg = if msg_prop.is_undefined() { JSString::from("") } else { to_string(msg_prop)? };
        if name.is_empty() {
            Ok(ECMAScriptValue::from(msg))
        } else if msg.is_empty() {
            Ok(ECMAScriptValue::from(name))
        } else {
            Ok(ECMAScriptValue::from(name.concat(": ").concat(msg)))
        }
    } else {
        Err(create_type_error("Error.prototype.toString called with non-object this value"))
    }
}

fn provision_native_error_intrinsics(
    realm: &Rc<RefCell<Realm>>,
    name: &str,
    native_error_constructor_function: fn(
        &ECMAScriptValue,
        Option<&Object>,
        &[ECMAScriptValue],
    ) -> Completion<ECMAScriptValue>,
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

    let native_error_constructor = create_builtin_function(
        native_error_constructor_function,
        true,
        1_f64,
        PropertyKey::from(name),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(error),
        None,
    );
    // Constructor Data Properties
    macro_rules! constructor_data {
        ( $value:expr, $name:expr ) => {{
            define_property_or_throw(
                &native_error_constructor,
                $name,
                PotentialPropertyDescriptor::new().value($value).writable(false).enumerable(false).configurable(false),
            )
            .unwrap();
        }};
    }

    // Properties of the NativeError Prototype Objects
    //
    // Each NativeError prototype object:
    //
    //    * is an ordinary object.
    //    * is not an Error instance and does not have an [[ErrorData]] internal slot.
    //    * has a [[Prototype]] internal slot whose value is %Error.prototype%.
    let native_error_prototype = ordinary_object_create(Some(error_prototype), &[]);

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
            define_property_or_throw(
                &native_error_prototype,
                $name,
                PotentialPropertyDescriptor::new().value($value).writable(true).enumerable(false).configurable(true),
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
fn native_error_constructor_function(
    _this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
    intrinsic_id: IntrinsicId,
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let message = args.next_arg();
    let afo: Object;

    let nt = match new_target {
        Some(objref) => objref,
        None => {
            afo = active_function_object().unwrap();
            &afo
        }
    };
    let o = nt.ordinary_create_from_constructor(intrinsic_id, &[InternalSlotName::ErrorData])?;
    if !message.is_undefined() {
        let msg = to_string(message)?;
        let msg_desc =
            PotentialPropertyDescriptor::new().value(msg).writable(true).enumerable(false).configurable(true);
        define_property_or_throw(&o, "message", msg_desc).unwrap();
    }
    Ok(ECMAScriptValue::from(o))
}

fn type_error_constructor_function(
    this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    native_error_constructor_function(this_value, new_target, arguments, IntrinsicId::TypeErrorPrototype)
}
fn eval_error_constructor_function(
    this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    native_error_constructor_function(this_value, new_target, arguments, IntrinsicId::EvalErrorPrototype)
}
fn range_error_constructor_function(
    this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    native_error_constructor_function(this_value, new_target, arguments, IntrinsicId::RangeErrorPrototype)
}
fn reference_error_constructor_function(
    this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    native_error_constructor_function(this_value, new_target, arguments, IntrinsicId::ReferenceErrorPrototype)
}
fn syntax_error_constructor_function(
    this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    native_error_constructor_function(this_value, new_target, arguments, IntrinsicId::SyntaxErrorPrototype)
}
fn uri_error_constructor_function(
    this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    native_error_constructor_function(this_value, new_target, arguments, IntrinsicId::URIErrorPrototype)
}

pub fn provision_type_error_intrinsic(realm: &Rc<RefCell<Realm>>) {
    let (constructor, prototype) =
        provision_native_error_intrinsics(realm, "TypeError", type_error_constructor_function);
    realm.borrow_mut().intrinsics.type_error = constructor;
    realm.borrow_mut().intrinsics.type_error_prototype = prototype;
}
pub fn provision_eval_error_intrinsic(realm: &Rc<RefCell<Realm>>) {
    let (constructor, prototype) =
        provision_native_error_intrinsics(realm, "EvalError", eval_error_constructor_function);
    realm.borrow_mut().intrinsics.eval_error = constructor;
    realm.borrow_mut().intrinsics.eval_error_prototype = prototype;
}
pub fn provision_range_error_intrinsic(realm: &Rc<RefCell<Realm>>) {
    let (constructor, prototype) =
        provision_native_error_intrinsics(realm, "RangeError", range_error_constructor_function);
    realm.borrow_mut().intrinsics.range_error = constructor;
    realm.borrow_mut().intrinsics.range_error_prototype = prototype;
}
pub fn provision_reference_error_intrinsic(realm: &Rc<RefCell<Realm>>) {
    let (constructor, prototype) =
        provision_native_error_intrinsics(realm, "ReferenceError", reference_error_constructor_function);
    realm.borrow_mut().intrinsics.reference_error = constructor;
    realm.borrow_mut().intrinsics.reference_error_prototype = prototype;
}
pub fn provision_syntax_error_intrinsic(realm: &Rc<RefCell<Realm>>) {
    let (constructor, prototype) =
        provision_native_error_intrinsics(realm, "SyntaxError", syntax_error_constructor_function);
    realm.borrow_mut().intrinsics.syntax_error = constructor;
    realm.borrow_mut().intrinsics.syntax_error_prototype = prototype;
}
pub fn provision_uri_error_intrinsic(realm: &Rc<RefCell<Realm>>) {
    let (constructor, prototype) = provision_native_error_intrinsics(realm, "URIError", uri_error_constructor_function);
    realm.borrow_mut().intrinsics.uri_error = constructor;
    realm.borrow_mut().intrinsics.uri_error_prototype = prototype;
}

/// Transform an ECMAScript Error object into a Rust string
pub fn unwind_any_error_value(err: ECMAScriptValue) -> String {
    to_string(err).unwrap().into()
}

/// Transform an ECMAScript Throw Completion into a Rust string
pub fn unwind_any_error(completion: AbruptCompletion) -> String {
    match completion {
        AbruptCompletion::Throw { value: err } => unwind_any_error_value(err),
        _ => panic!("Improper completion for error: {completion:?}"),
    }
}

pub fn unwind_any_error_object(o: &Object) -> String {
    let name_prop = o.get(&"name".into()).unwrap_or(ECMAScriptValue::Undefined);
    let name = if name_prop.is_undefined() { String::from("Error") } else { name_prop.to_string() };
    let msg_prop = o.get(&"message".into()).unwrap_or(ECMAScriptValue::Undefined);
    let msg = if msg_prop.is_undefined() { String::new() } else { msg_prop.to_string() };
    if name.is_empty() {
        msg
    } else if msg.is_empty() {
        name
    } else {
        format!("{name}: {msg}")
    }
}

#[cfg(test)]
mod tests;
