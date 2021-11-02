use crate::agent::{Agent, WksId};
use crate::arrays::is_array;
use crate::cr::Completion;
use crate::errors::create_type_error;
use crate::function_object::{create_builtin_function, Arguments};
use crate::object::{
    define_property_or_throw, get, ordinary_create_from_constructor, ordinary_object_create, set, to_property_descriptor, Object, PotentialPropertyDescriptor, BUILTIN_FUNCTION_SLOTS,
};
use crate::realm::{IntrinsicId, Realm};
use crate::strings::JSString;
use crate::values::{to_object, to_property_key, ECMAScriptValue, PropertyKey};
use std::cell::RefCell;
use std::rc::Rc;

// Object.prototype.valueOf ( )
//
// When the valueOf method is called, the following steps are taken:
//
//      1. Return ? ToObject(this value).
fn object_prototype_value_of(agent: &mut Agent, this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    to_object(agent, this_value).map(ECMAScriptValue::from)
}

// Object.prototype.toString ( )
//
// When the toString method is called, the following steps are taken:
//
//      1. If the this value is undefined, return "[object Undefined]".
//      2. If the this value is null, return "[object Null]".
//      3. Let O be ! ToObject(this value).
//      4. Let isArray be ? IsArray(O).
//      5. If isArray is true, let builtinTag be "Array".
//      6. Else if O has a [[ParameterMap]] internal slot, let builtinTag be "Arguments".
//      7. Else if O has a [[Call]] internal method, let builtinTag be "Function".
//      8. Else if O has an [[ErrorData]] internal slot, let builtinTag be "Error".
//      9. Else if O has a [[BooleanData]] internal slot, let builtinTag be "Boolean".
//      10. Else if O has a [[NumberData]] internal slot, let builtinTag be "Number".
//      11. Else if O has a [[StringData]] internal slot, let builtinTag be "String".
//      12. Else if O has a [[DateValue]] internal slot, let builtinTag be "Date".
//      13. Else if O has a [[RegExpMatcher]] internal slot, let builtinTag be "RegExp".
//      14. Else, let builtinTag be "Object".
//      15. Let tag be ? Get(O, @@toStringTag).
//      16. If Type(tag) is not String, set tag to builtinTag.
//      17. Return the string-concatenation of "[object ", tag, and "]".
//
// NOTE     | Historically, this function was occasionally used to access the String value of the [[Class]] internal
//            slot that was used in previous editions of this specification as a nominal type tag for various built-in
//            objects. The above definition of toString preserves compatibility for legacy code that uses toString as a
//            test for those specific kinds of built-in objects. It does not provide a reliable type testing mechanism
//            for other kinds of built-in or program defined objects. In addition, programs can use @@toStringTag in
//            ways that will invalidate the reliability of such legacy type tests.
fn object_prototype_to_string(agent: &mut Agent, this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    if this_value.is_undefined() {
        return Ok(ECMAScriptValue::from("[object Undefined]"));
    }
    if this_value.is_null() {
        return Ok(ECMAScriptValue::from("[object Null]"));
    }
    let o = to_object(agent, this_value).unwrap();
    let builtin_tag = if is_array(&o)? {
        "Array"
    } else if o.o.is_arguments_object() {
        "Arguments"
    } else if o.o.is_callable_obj() {
        "Function"
    } else if o.o.is_error_object() {
        "Error"
    } else if o.o.is_boolean_object() {
        "Boolean"
    } else if o.o.is_number_object() {
        "Number"
    } else if o.o.is_string_object() {
        "String"
    } else if o.o.is_date_object() {
        "Date"
    } else if o.o.is_regexp_object() {
        "RegExp"
    } else {
        "Object"
    };
    let to_string_tag_symbol = agent.wks(WksId::ToStringTag);
    let tag = get(agent, &o, &PropertyKey::from(to_string_tag_symbol))?;
    let tag_string = match tag {
        ECMAScriptValue::String(s) => s,
        _ => JSString::from(builtin_tag),
    };
    let mut result_vec = "[object ".encode_utf16().collect::<Vec<u16>>();
    result_vec.extend_from_slice(tag_string.as_slice());
    result_vec.push(']' as u16);
    Ok(ECMAScriptValue::from(result_vec))
}

pub fn provision_object_intrinsic(agent: &mut Agent, realm: &Rc<RefCell<Realm>>) {
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();

    // The Object Constructor
    //
    // The Object constructor:
    //
    //      * is %Object%.
    //      * is the initial value of the "Object" property of the global object.
    //      * creates a new ordinary object when called as a constructor.
    //      * performs a type conversion when called as a function rather than as a constructor.
    //      * may be used as the value of an extends clause of a class definition.
    //
    // Properties of the Object Constructor
    //
    // The Object constructor:
    //
    //      * has a [[Prototype]] internal slot whose value is %Function.prototype%.
    let object_constructor = create_builtin_function(
        agent,
        object_constructor_function,
        true,
        1.0,
        PropertyKey::from("Object"),
        &BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype.clone()),
        None,
    );

    // Constructor Function Properties
    macro_rules! constructor_function {
        ( $steps:expr, $name:expr, $length:expr ) => {
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(agent, $steps, false, $length, key.clone(), &BUILTIN_FUNCTION_SLOTS, Some(realm.clone()), Some(function_prototype.clone()), None);
            define_property_or_throw(
                agent,
                &object_constructor,
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
    constructor_function!(object_assign, "assign", 2.0);
    constructor_function!(object_create, "create", 2.0);
    constructor_function!(object_define_properties, "defineProperties", 2.0);
    constructor_function!(object_define_property, "defineProperty", 3.0);
    constructor_function!(object_entries, "entries", 1.0);
    constructor_function!(object_freeze, "freeze", 1.0);
    constructor_function!(object_from_entries, "fromEntries", 1.0);
    constructor_function!(object_get_own_property_descriptor, "getOwnPropertyDescriptor", 2.0);
    constructor_function!(object_get_own_property_descriptors, "getOwnPropertyDescriptors", 1.0);
    constructor_function!(object_get_own_property_names, "getOwnPropertyNames", 1.0);
    constructor_function!(object_get_own_property_symbols, "getOwnPropertySymbols", 1.0);
    constructor_function!(object_get_prototype_of, "getPrototypeOf", 1.0);
    constructor_function!(object_has_own, "hasOwn", 2.0);
    constructor_function!(object_is, "is", 2.0);
    constructor_function!(object_is_extensible, "isExtensible", 1.0);
    constructor_function!(object_is_frozen, "isFrozen", 1.0);
    constructor_function!(object_is_sealed, "isSealed", 1.0);
    constructor_function!(object_keys, "keys", 1.0);
    constructor_function!(object_prevent_extensions, "preventExtensions", 1.0);
    constructor_function!(object_seal, "seal", 1.0);
    constructor_function!(object_set_prototype_of, "setPrototypeOf", 2.0);
    constructor_function!(object_values, "values", 1.0);

    // Object.prototype
    //
    // The initial value of Object.prototype is the Object prototype object.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    define_property_or_throw(
        agent,
        &object_constructor,
        PropertyKey::from("prototype"),
        PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::from(object_prototype.clone())),
            writable: Some(false),
            enumerable: Some(false),
            configurable: Some(false),
            ..Default::default()
        },
    )
    .unwrap();

    // Prototype function properties
    macro_rules! prototype_function {
        ( $steps:expr, $name:expr, $length:expr ) => {
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(agent, $steps, false, $length, key.clone(), &BUILTIN_FUNCTION_SLOTS, Some(realm.clone()), Some(function_prototype.clone()), None);
            define_property_or_throw(
                agent,
                &object_prototype,
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

    prototype_function!(object_prototype_value_of, "valueOf", 0.0);
    prototype_function!(object_prototype_to_string, "toString", 0.0);
    prototype_function!(object_prototype_has_own_property, "hasOwnProperty", 1.0);
    prototype_function!(object_prototype_is_prototype_of, "isPrototypeOf", 1.0);
    prototype_function!(object_prototype_property_is_enumerable, "propertyIsEnumerable", 1.0);
    prototype_function!(object_prototype_to_locale_string, "toLocaleString", 0.0);

    // Object.prototype.constructor
    //
    // The initial value of Object.prototype.constructor is %Object%.
    define_property_or_throw(
        agent,
        &object_prototype,
        PropertyKey::from("constructor"),
        PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::from(object_constructor.clone())),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();

    realm.borrow_mut().intrinsics.object = object_constructor;
}

// Object ( [ value ] )
//
// When the Object function is called with optional argument value, the following steps are taken:
//
//  1. If NewTarget is neither undefined nor the active function, then
//      a. Return ? OrdinaryCreateFromConstructor(NewTarget, "%Object.prototype%").
//  2. If value is undefined or null, return ! OrdinaryObjectCreate(%Object.prototype%).
//  3. Return ! ToObject(value).
// The "length" property of the Object function is 1𝔽.
fn object_constructor_function(agent: &mut Agent, _this_value: ECMAScriptValue, new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion {
    if let Some(nt) = new_target {
        if let Some(afo) = agent.active_function_object() {
            if *nt != afo {
                return ordinary_create_from_constructor(agent, nt, IntrinsicId::ObjectPrototype, &[]).map(ECMAScriptValue::from);
            }
        }
    }
    let mut args = Arguments::from(arguments);
    let value = args.next_arg();
    let obj =
        if value.is_null() || value.is_undefined() { ordinary_object_create(agent, Some(&agent.intrinsic(IntrinsicId::ObjectPrototype)), &[]) } else { to_object(agent, value).unwrap() };
    Ok(ECMAScriptValue::from(obj))
}

// Object.assign ( target, ...sources )
//
// The assign function is used to copy the values of all of the enumerable own properties from one or more source
// objects to a target object. When the assign function is called, the following steps are taken:
//
//  1. Let to be ? ToObject(target).
//  2. If only one argument was passed, return to.
//  3. For each element nextSource of sources, do
//      a. If nextSource is neither undefined nor null, then
//          i. Let from be ! ToObject(nextSource).
//          ii. Let keys be ? from.[[OwnPropertyKeys]]().
//          iii. For each element nextKey of keys, do
//              1. Let desc be ? from.[[GetOwnProperty]](nextKey).
//              2. If desc is not undefined and desc.[[Enumerable]] is true, then
//                  a. Let propValue be ? Get(from, nextKey).
//                  b. Perform ? Set(to, nextKey, propValue, true).
//  4. Return to.
//
// The "length" property of the assign function is 2𝔽.
fn object_assign(agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion {
    let mut args = Arguments::from(arguments);
    let target = args.next_arg();
    let to = to_object(agent, target)?;
    for next_source in args.remaining() {
        if !(next_source.is_null() || next_source.is_undefined()) {
            let from = to_object(agent, next_source.clone()).unwrap();
            let keys = from.o.own_property_keys(agent)?;
            for next_key in keys {
                let option_desc = from.o.get_own_property(agent, &next_key)?;
                if let Some(desc) = option_desc {
                    if desc.enumerable {
                        let prop_value = get(agent, &from, &next_key)?;
                        set(agent, &to, next_key, prop_value, true)?;
                    }
                }
            }
        }
    }
    Ok(ECMAScriptValue::from(to))
}

// Object.create ( O, Properties )
//
// The create function creates a new object with a specified prototype. When the create function is called, the
// following steps are taken:
//
//  1. If Type(O) is neither Object nor Null, throw a TypeError exception.
//  2. Let obj be ! OrdinaryObjectCreate(O).
//  3. If Properties is not undefined, then
//      a. Return ? ObjectDefineProperties(obj, Properties).
//  4. Return obj.
fn object_create(agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion {
    let mut args = Arguments::from(arguments);
    let o_arg = args.next_arg();
    let o = match &o_arg {
        ECMAScriptValue::Object(o) => Some(o),
        ECMAScriptValue::Null => None,
        _ => {
            return Err(create_type_error(agent, "Prototype argument for Object.create must be an Object or null."));
        }
    };
    let properties = args.next_arg();
    let obj = ordinary_object_create(agent, o, &[]);
    if !properties.is_undefined() {
        object_define_properties_helper(agent, obj, properties)
    } else {
        Ok(ECMAScriptValue::from(obj))
    }
}

// ObjectDefineProperties ( O, Properties )
//
// The abstract operation ObjectDefineProperties takes arguments O (an Object) and Properties. It performs the
// following steps when called:
//
//  1. Let props be ? ToObject(Properties).
//  2. Let keys be ? props.[[OwnPropertyKeys]]().
//  3. Let descriptors be a new empty List.
//  4. For each element nextKey of keys, do
//      a. Let propDesc be ? props.[[GetOwnProperty]](nextKey).
//      b. If propDesc is not undefined and propDesc.[[Enumerable]] is true, then
//          i. Let descObj be ? Get(props, nextKey).
//          ii. Let desc be ? ToPropertyDescriptor(descObj).
//          iii. Append the pair (a two element List) consisting of nextKey and desc to the end of descriptors.
//  5. For each element pair of descriptors, do
//      a. Let P be the first element of pair.
//      b. Let desc be the second element of pair.
//      c. Perform ? DefinePropertyOrThrow(O, P, desc).
//  6. Return O.
fn object_define_properties_helper(agent: &mut Agent, o: Object, properties: ECMAScriptValue) -> Completion {
    let props = to_object(agent, properties)?;
    let keys = props.o.own_property_keys(agent)?;
    let mut descriptors = Vec::new();
    for next_key in keys {
        let prop_desc = props.o.get_own_property(agent, &next_key)?;
        if let Some(pd) = prop_desc {
            if pd.enumerable {
                let desc_obj = get(agent, &props, &next_key)?;
                let desc = to_property_descriptor(agent, &desc_obj)?;
                descriptors.push((next_key, desc));
            }
        }
    }
    for (p, desc) in descriptors {
        define_property_or_throw(agent, &o, p, desc)?;
    }

    Ok(o.into())
}

// Object.defineProperties ( O, Properties )
//
// The defineProperties function is used to add own properties and/or update the attributes of existing own properties
// of an object. When the defineProperties function is called, the following steps are taken:
//
//  1. If Type(O) is not Object, throw a TypeError exception.
//  2. Return ? ObjectDefineProperties(O, Properties).
fn object_define_properties(agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion {
    let mut args = Arguments::from(arguments);
    let o_arg = args.next_arg();
    match o_arg {
        ECMAScriptValue::Object(o) => {
            let properties = args.next_arg();
            object_define_properties_helper(agent, o, properties)
        }
        _ => Err(create_type_error(agent, "Object.defineProperties called on non-object")),
    }
}

// Object.defineProperty ( O, P, Attributes )
//
// The defineProperty function is used to add an own property and/or update the attributes of an existing own property
// of an object. When the defineProperty function is called, the following steps are taken:
//
//  1. If Type(O) is not Object, throw a TypeError exception.
//  2. Let key be ? ToPropertyKey(P).
//  3. Let desc be ? ToPropertyDescriptor(Attributes).
//  4. Perform ? DefinePropertyOrThrow(O, key, desc).
//  5. Return O.
fn object_define_property(agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion {
    let mut args = Arguments::from(arguments);
    let o_arg = args.next_arg();
    match o_arg {
        ECMAScriptValue::Object(o) => {
            let p = args.next_arg();
            let attributes = args.next_arg();

            let key = to_property_key(agent, p)?;
            let desc = to_property_descriptor(agent, &attributes)?;

            define_property_or_throw(agent, &o, key, desc)?;

            Ok(ECMAScriptValue::from(o))
        }
        _ => Err(create_type_error(agent, "Object.defineProperty called on non-object")),
    }
}

fn object_entries(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_freeze(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_from_entries(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_get_own_property_descriptor(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_get_own_property_descriptors(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_get_own_property_names(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_get_own_property_symbols(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_get_prototype_of(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_has_own(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_is(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_is_extensible(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_is_frozen(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_is_sealed(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_keys(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_prevent_extensions(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_seal(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_set_prototype_of(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_values(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_prototype_has_own_property(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_prototype_is_prototype_of(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_prototype_property_is_enumerable(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn object_prototype_to_locale_string(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}

#[cfg(test)]
mod tests;
