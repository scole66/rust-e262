use super::*;
use std::cell::RefCell;
use std::rc::Rc;

// Object.prototype.valueOf ( )
//
// When the valueOf method is called, the following steps are taken:
//
//      1. Return ? ToObject(this value).
fn object_prototype_value_of(
    this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    to_object(this_value).map(ECMAScriptValue::from)
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
pub fn object_prototype_to_string(
    this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    if this_value.is_undefined() {
        return Ok(ECMAScriptValue::from("[object Undefined]"));
    }
    if this_value.is_null() {
        return Ok(ECMAScriptValue::from("[object Null]"));
    }
    let o = to_object(this_value).unwrap();
    let builtin_tag = if o.is_array()? {
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
    let to_string_tag_symbol = wks(WksId::ToStringTag);
    let tag = o.get(&PropertyKey::from(to_string_tag_symbol))?;
    let tag_string = match tag {
        ECMAScriptValue::String(s) => s,
        _ => JSString::from(builtin_tag),
    };
    let mut result_vec = "[object ".encode_utf16().collect::<Vec<u16>>();
    result_vec.extend_from_slice(tag_string.as_slice());
    result_vec.push(']' as u16);
    Ok(ECMAScriptValue::from(result_vec))
}

pub fn provision_object_intrinsic(realm: &Rc<RefCell<Realm>>) {
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
        object_constructor_function,
        true,
        1.0,
        PropertyKey::from("Object"),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype.clone()),
        None,
    );

    // Constructor Function Properties
    macro_rules! constructor_function {
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
                &object_constructor,
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
        &object_constructor,
        "prototype",
        PotentialPropertyDescriptor::new()
            .value(object_prototype.clone())
            .writable(false)
            .enumerable(false)
            .configurable(false),
    )
    .unwrap();

    // Prototype function properties
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
                &object_prototype,
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
        &object_prototype,
        "constructor",
        PotentialPropertyDescriptor::new()
            .value(object_constructor.clone())
            .writable(true)
            .enumerable(false)
            .configurable(true),
    )
    .unwrap();

    realm.borrow_mut().intrinsics.object = object_constructor;

    let tostring = Object::try_from(object_prototype.get(&"toString".into()).expect("toString should exist"))
        .expect("toString should be a function object");
    realm.borrow_mut().intrinsics.object_prototype_to_string = tostring;
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
fn object_constructor_function(
    _this_value: ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    if let Some(nt) = new_target {
        if let Some(afo) = active_function_object() {
            if *nt != afo {
                return nt
                    .ordinary_create_from_constructor(IntrinsicId::ObjectPrototype, &[])
                    .map(ECMAScriptValue::from);
            }
        }
    }
    let mut args = FuncArgs::from(arguments);
    let value = args.next_arg();
    let obj = if value.is_null() || value.is_undefined() {
        ordinary_object_create(Some(intrinsic(IntrinsicId::ObjectPrototype)), &[])
    } else {
        to_object(value).unwrap()
    };
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
fn object_assign(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let target = args.next_arg();
    let to = to_object(target)?;
    for next_source in args.remaining() {
        if !(next_source.is_null() || next_source.is_undefined()) {
            let from = to_object(next_source.clone()).unwrap();
            let keys = from.o.own_property_keys()?;
            for next_key in keys {
                let option_desc = from.o.get_own_property(&next_key)?;
                if let Some(desc) = option_desc {
                    if desc.enumerable {
                        let prop_value = from.get(&next_key)?;
                        to.set(next_key, prop_value, true)?;
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
fn object_create(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let o_arg = args.next_arg();
    let o = match o_arg {
        ECMAScriptValue::Object(o) => Some(o),
        ECMAScriptValue::Null => None,
        _ => {
            return Err(create_type_error("Prototype argument for Object.create must be an Object or null."));
        }
    };
    let properties = args.next_arg();
    let obj = ordinary_object_create(o, &[]);
    if !properties.is_undefined() {
        object_define_properties_helper(obj, properties)
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
fn object_define_properties_helper(o: Object, properties: ECMAScriptValue) -> Completion<ECMAScriptValue> {
    let props = to_object(properties)?;
    let keys = props.o.own_property_keys()?;
    let mut descriptors = Vec::new();
    for next_key in keys {
        let prop_desc = props.o.get_own_property(&next_key)?;
        if let Some(pd) = prop_desc {
            if pd.enumerable {
                let desc_obj = props.get(&next_key)?;
                let desc = to_property_descriptor(&desc_obj)?;
                descriptors.push((next_key, desc));
            }
        }
    }
    for (p, desc) in descriptors {
        define_property_or_throw(&o, p, desc)?;
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
fn object_define_properties(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let o_arg = args.next_arg();
    match o_arg {
        ECMAScriptValue::Object(o) => {
            let properties = args.next_arg();
            object_define_properties_helper(o, properties)
        }
        _ => Err(create_type_error("Object.defineProperties called on non-object")),
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
fn object_define_property(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let o_arg = args.next_arg();
    match o_arg {
        ECMAScriptValue::Object(o) => {
            let p = args.next_arg();
            let attributes = args.next_arg();

            let key = to_property_key(p)?;
            let desc = to_property_descriptor(&attributes)?;

            define_property_or_throw(&o, key, desc)?;

            Ok(ECMAScriptValue::from(o))
        }
        _ => Err(create_type_error("Object.defineProperty called on non-object")),
    }
}

// Object.entries ( O )
//
// When the entries function is called with argument O, the following steps are taken:
//
//  1. Let obj be ? ToObject(O).
//  2. Let nameList be ? EnumerableOwnPropertyNames(obj, key+value).
//  3. Return CreateArrayFromList(nameList).
//
// https://tc39.es/ecma262/#sec-object.entries
fn object_entries(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let o_arg = args.next_arg();
    let obj = to_object(o_arg)?;
    let name_list = enumerable_own_properties(&obj, KeyValueKind::KeyValue)?;
    Ok(create_array_from_list(&name_list).into())
}

// Object.freeze ( O )
//
// When the freeze function is called, the following steps are taken:
//
//  1. If Type(O) is not Object, return O.
//  2. Let status be ? SetIntegrityLevel(O, frozen).
//  3. If status is false, throw a TypeError exception.
//  4. Return O.
//
// https://tc39.es/ecma262/#sec-object.freeze
fn object_freeze(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let o_arg = args.next_arg();
    match o_arg {
        ECMAScriptValue::Object(o) => {
            let status = set_integrity_level(&o, IntegrityLevel::Frozen)?;
            if !status {
                Err(create_type_error("Object cannot be frozen"))
            } else {
                Ok(o.into())
            }
        }
        _ => Ok(o_arg),
    }
}

/// Transforms an iterable of key-value pairs into an object
///
/// ## Syntax
///
/// ```javascript
/// Object.fromEntries(iterable)
/// ```
///
/// ### Parameters
///
/// * `iterable`: An iterable, such as an Array or Map, containing a list of objects. Each object should have
///   two properties:
///      * `0`: A string or symbol representing the property key.
///      * `1`: The property value.
///
/// Typically, this object is implemented as a two-element array, with the first element being the property
/// key and the second element being the property value.
///
/// ### Return Value
/// A new object whose properties are given by the entries of the iterable.
///
/// ## Description
/// The `Object.fromEntries()` method takes a list of key-value pairs and returns a new object whose
/// properties are given by those entries. The iterable argument is expected to be an object that implements
/// an `@@iterator` method. The method returns an iterator object that produces two-element array-like
/// objects. The first element is a value that will be used as a property key, and the second element is the
/// value to associate with that property key.
///
/// `Object.fromEntries()` performs the reverse of `Object.entries()`, except that `Object.entries()` only
/// returns string-keyed properties, while `Object.fromEntries()` can also create symbol-keyed properties.
///
/// *Note*: Unlike `Array.from()`, `Object.fromEntries()` does not use the value of `this`, so calling it on
/// another constructor does not create objects of that type.
///
/// See
/// [`Object.fromEntries()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/fromEntries).
fn object_from_entries(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.fromEntries ( iterable )
    // This function performs the following steps when called:
    //
    //  1. Perform ? RequireObjectCoercible(iterable).
    //  2. Let obj be OrdinaryObjectCreate(%Object.prototype%).
    //  3. Assert: obj is an extensible ordinary object with no own properties.
    //  4. Let closure be a new Abstract Closure with parameters (key, value) that captures obj and performs
    //     the following steps when called:
    //      a. Let propertyKey be ? ToPropertyKey(key).
    //      b. Perform ! CreateDataPropertyOrThrow(obj, propertyKey, value).
    //      c. Return undefined.
    //  5. Let adder be CreateBuiltinFunction(closure, 2, "", « »).
    //  6. Return ? AddEntriesFromIterable(obj, iterable, adder).
    // NOTE: The function created for adder is never directly accessible to ECMAScript code.
    let mut args = FuncArgs::from(arguments);
    let iterable = args.next_arg();
    require_object_coercible(&iterable)?;
    let obj_proto = intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(Some(obj_proto), &[]);
    let closure = |this: ECMAScriptValue, _: Option<&Object>, args: &[ECMAScriptValue]| {
        let this_obj = Object::try_from(this).expect("'this' should be an object");
        let mut args = FuncArgs::from(args);
        let key = args.next_arg();
        let value = args.next_arg();
        let property_key = to_property_key(key)?;
        this_obj.create_data_property_or_throw(property_key, value).unwrap();
        let result: Completion<ECMAScriptValue> = Ok(ECMAScriptValue::Undefined);
        result
    };
    let adder = create_builtin_function(closure, false, 2.0, "".into(), &[], None, None, None);
    add_entries_from_iterable(&obj.into(), &iterable, &adder.into())
        .map(|nc| nc.try_into().expect("outside the compiler, this should always return a value"))
}

fn object_get_own_property_descriptor(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.getOwnPropertyDescriptor ( O, P )
    // This function performs the following steps when called:
    //
    //  1. Let obj be ? ToObject(O).
    //  2. Let key be ? ToPropertyKey(P).
    //  3. Let desc be ? obj.[[GetOwnProperty]](key).
    //  4. Return FromPropertyDescriptor(desc).
    let mut args = FuncArgs::from(arguments);
    let o = args.next_arg();
    let p = args.next_arg();
    let obj = to_object(o)?;
    let key = to_property_key(p)?;
    let desc = obj.o.get_own_property(&key)?;
    let result = from_property_descriptor(desc);
    Ok(match result {
        Some(obj) => ECMAScriptValue::from(obj),
        None => ECMAScriptValue::Undefined,
    })
}

fn object_get_own_property_descriptors(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.getOwnPropertyDescriptors ( O )
    // This function performs the following steps when called:
    //
    //  1. Let obj be ? ToObject(O).
    //  2. Let ownKeys be ? obj.[[OwnPropertyKeys]]().
    //  3. Let descriptors be OrdinaryObjectCreate(%Object.prototype%).
    //  4. For each element key of ownKeys, do
    //      a. Let desc be ? obj.[[GetOwnProperty]](key).
    //      b. Let descriptor be FromPropertyDescriptor(desc).
    //      c. If descriptor is not undefined, perform ! CreateDataPropertyOrThrow(descriptors, key,
    //         descriptor).
    //  5. Return descriptors.
    let mut args = FuncArgs::from(arguments);
    let o = args.next_arg();
    let obj = to_object(o)?;
    let own_keys = obj.o.own_property_keys()?;
    let object_proto = intrinsic(IntrinsicId::ObjectPrototype);
    let descriptors = ordinary_object_create(Some(object_proto), &[]);
    for key in own_keys {
        let desc = obj.o.get_own_property(&key)?;
        let descriptor = from_property_descriptor(desc);
        if let Some(descriptor) = descriptor {
            descriptors.create_data_property_or_throw(key, descriptor).expect("Simple property addition should work");
        }
    }
    Ok(ECMAScriptValue::from(descriptors))
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum KeyType {
    String,
    Symbol,
}
fn get_own_property_keys(o: ECMAScriptValue, typ: KeyType) -> Completion<Vec<ECMAScriptValue>> {
    // GetOwnPropertyKeys ( O, type )
    // The abstract operation GetOwnPropertyKeys takes arguments O (an ECMAScript language value) and type
    // (string or symbol) and returns either a normal completion containing a List of property keys or a throw
    // completion. It performs the following steps when called:
    //
    //  1. Let obj be ? ToObject(O).
    //  2. Let keys be ? obj.[[OwnPropertyKeys]]().
    //  3. Let nameList be a new empty List.
    //  4. For each element nextKey of keys, do
    //      a. If nextKey is a Symbol and type is symbol, or if nextKey is a String and type is string, then
    //          i. Append nextKey to nameList.
    //  5. Return nameList.
    let obj = to_object(o)?;
    let keys = obj.o.own_property_keys()?;
    Ok(keys
        .into_iter()
        .filter(|key| match typ {
            KeyType::String => matches!(key, &PropertyKey::String(_)),
            KeyType::Symbol => matches!(key, &PropertyKey::Symbol(_)),
        })
        .map(ECMAScriptValue::from)
        .collect::<Vec<_>>())
}

fn object_get_own_property_names(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.getOwnPropertyNames ( O )
    // This function performs the following steps when called:
    //
    //  1. Return CreateArrayFromList(? GetOwnPropertyKeys(O, string)).
    let mut args = FuncArgs::from(arguments);
    let o = args.next_arg();
    let keys = get_own_property_keys(o, KeyType::String)?;
    Ok(create_array_from_list(keys.as_slice()).into())
}
fn object_get_own_property_symbols(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.getOwnPropertySymbols ( O )
    // This function performs the following steps when called:
    //
    //  1. Return CreateArrayFromList(? GetOwnPropertyKeys(O, symbol)).
    let mut args = FuncArgs::from(arguments);
    let o = args.next_arg();
    let keys = get_own_property_keys(o, KeyType::Symbol)?;
    Ok(create_array_from_list(keys.as_slice()).into())
}

fn object_get_prototype_of(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.getPrototypeOf ( O )
    // This function performs the following steps when called:
    //
    //  1. Let obj be ? ToObject(O).
    //  2. Return ? obj.[[GetPrototypeOf]]().
    let mut args = FuncArgs::from(arguments);
    let o = args.next_arg();
    let obj = to_object(o)?;
    Ok(match obj.o.get_prototype_of()? {
        Some(obj) => ECMAScriptValue::from(obj),
        None => ECMAScriptValue::Undefined,
    })
}

fn object_has_own(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.hasOwn ( O, P )
    // This function performs the following steps when called:
    //
    //  1. Let obj be ? ToObject(O).
    //  2. Let key be ? ToPropertyKey(P).
    //  3. Return ? HasOwnProperty(obj, key).
    let mut args = FuncArgs::from(arguments);
    let o = args.next_arg();
    let p = args.next_arg();
    let obj = to_object(o)?;
    let key = to_property_key(p)?;
    obj.has_own_property(&key).map(ECMAScriptValue::from)
}

fn object_is(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.is ( value1, value2 )
    // This function performs the following steps when called:
    //
    //  1. Return SameValue(value1, value2).
    let mut args = FuncArgs::from(arguments);
    let value1 = args.next_arg();
    let value2 = args.next_arg();
    Ok(value1.same_value(&value2).into())
}
fn object_is_extensible(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.isExtensible ( O )
    // This function performs the following steps when called:
    //
    //  1. If O is not an Object, return false.
    //  2. Return ? IsExtensible(O).
    let mut args = FuncArgs::from(arguments);
    let o = args.next_arg();
    match o {
        ECMAScriptValue::Undefined
        | ECMAScriptValue::Null
        | ECMAScriptValue::Boolean(_)
        | ECMAScriptValue::String(_)
        | ECMAScriptValue::Number(_)
        | ECMAScriptValue::BigInt(_)
        | ECMAScriptValue::Symbol(_) => Ok(ECMAScriptValue::from(false)),
        ECMAScriptValue::Object(obj) => is_extensible(&obj).map(ECMAScriptValue::from),
    }
}

fn object_is_frozen(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.isFrozen ( O )
    // This function performs the following steps when called:
    //
    // 1. If O is not an Object, return true.
    // 2. Return ? TestIntegrityLevel(O, frozen).
    let mut args = FuncArgs::from(arguments);
    let o = args.next_arg();
    match o {
        ECMAScriptValue::Undefined
        | ECMAScriptValue::Null
        | ECMAScriptValue::Boolean(_)
        | ECMAScriptValue::String(_)
        | ECMAScriptValue::Number(_)
        | ECMAScriptValue::BigInt(_)
        | ECMAScriptValue::Symbol(_) => Ok(ECMAScriptValue::from(true)),
        ECMAScriptValue::Object(obj) => test_integrity_level(&obj, IntegrityLevel::Frozen).map(ECMAScriptValue::from),
    }
}

fn object_is_sealed(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.isSealed ( O )
    // This function performs the following steps when called:
    //
    //  1. If O is not an Object, return true.
    //  2. Return ? TestIntegrityLevel(O, sealed).
    let mut args = FuncArgs::from(arguments);
    let o = args.next_arg();
    match o {
        ECMAScriptValue::Undefined
        | ECMAScriptValue::Null
        | ECMAScriptValue::Boolean(_)
        | ECMAScriptValue::String(_)
        | ECMAScriptValue::Number(_)
        | ECMAScriptValue::BigInt(_)
        | ECMAScriptValue::Symbol(_) => Ok(ECMAScriptValue::from(true)),
        ECMAScriptValue::Object(obj) => test_integrity_level(&obj, IntegrityLevel::Sealed).map(ECMAScriptValue::from),
    }
}
fn object_keys(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.keys ( O )
    // This function performs the following steps when called:
    //
    //  1. Let obj be ? ToObject(O).
    //  2. Let keyList be ? EnumerableOwnProperties(obj, key).
    //  3. Return CreateArrayFromList(keyList).
    let mut args = FuncArgs::from(arguments);
    let o = args.next_arg();
    let obj = to_object(o)?;
    let key_list = enumerable_own_properties(&obj, KeyValueKind::Key)?;
    Ok(create_array_from_list(key_list.as_slice()).into())
}

fn object_prevent_extensions(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.preventExtensions ( O )
    // This function performs the following steps when called:
    //
    //  1. If O is not an Object, return O.
    //  2. Let status be ? O.[[PreventExtensions]]().
    //  3. If status is false, throw a TypeError exception.
    //  4. Return O.
    let mut args = FuncArgs::from(arguments);
    let o = args.next_arg();
    match o {
        ECMAScriptValue::Undefined
        | ECMAScriptValue::Null
        | ECMAScriptValue::Boolean(_)
        | ECMAScriptValue::String(_)
        | ECMAScriptValue::Number(_)
        | ECMAScriptValue::BigInt(_)
        | ECMAScriptValue::Symbol(_) => Ok(o),
        ECMAScriptValue::Object(o) => {
            let status = o.o.prevent_extensions()?;
            if !status {
                Err(create_type_error("cannot prevent extensions for this object"))
            } else {
                Ok(o.into())
            }
        }
    }
}

fn object_seal(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.seal ( O )
    // This function performs the following steps when called:
    //
    // 1. If O is not an Object, return O.
    // 2. Let status be ? SetIntegrityLevel(O, sealed).
    // 3. If status is false, throw a TypeError exception.
    // 4. Return O.
    let mut args = FuncArgs::from(arguments);
    let o = args.next_arg();
    match o {
        ECMAScriptValue::Undefined
        | ECMAScriptValue::Null
        | ECMAScriptValue::Boolean(_)
        | ECMAScriptValue::String(_)
        | ECMAScriptValue::Number(_)
        | ECMAScriptValue::BigInt(_)
        | ECMAScriptValue::Symbol(_) => Ok(o),
        ECMAScriptValue::Object(o) => {
            let status = set_integrity_level(&o, IntegrityLevel::Sealed)?;
            if !status {
                Err(create_type_error("cannot seal this object"))
            } else {
                Ok(o.into())
            }
        }
    }
}

fn object_set_prototype_of(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.setPrototypeOf ( O, proto )
    // This function performs the following steps when called:
    //
    //  1. Set O to ? RequireObjectCoercible(O).
    //  2. If proto is not an Object and proto is not null, throw a TypeError exception.
    //  3. If O is not an Object, return O.
    //  4. Let status be ? O.[[SetPrototypeOf]](proto).
    //  5. If status is false, throw a TypeError exception.
    //  6. Return O.
    let mut args = FuncArgs::from(arguments);
    let o = args.next_arg();
    let proto = args.next_arg();
    require_object_coercible(&o)?;
    let proto = match proto {
        ECMAScriptValue::Undefined
        | ECMAScriptValue::Boolean(_)
        | ECMAScriptValue::String(_)
        | ECMAScriptValue::Number(_)
        | ECMAScriptValue::BigInt(_)
        | ECMAScriptValue::Symbol(_) => {
            return Err(create_type_error("Prototype must be an object or null"));
        }
        ECMAScriptValue::Null => None,
        ECMAScriptValue::Object(p) => Some(p),
    };
    match o {
        ECMAScriptValue::Undefined
        | ECMAScriptValue::Null
        | ECMAScriptValue::Boolean(_)
        | ECMAScriptValue::String(_)
        | ECMAScriptValue::Number(_)
        | ECMAScriptValue::BigInt(_)
        | ECMAScriptValue::Symbol(_) => Ok(o),
        ECMAScriptValue::Object(o) => {
            let status = o.o.set_prototype_of(proto)?;
            if !status {
                Err(create_type_error("Prototype setting failed"))
            } else {
                Ok(o.into())
            }
        }
    }
}

fn object_values(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.values ( O )
    // This function performs the following steps when called:
    //
    //  1. Let obj be ? ToObject(O).
    //  2. Let valueList be ? EnumerableOwnProperties(obj, value).
    //  3. Return CreateArrayFromList(valueList).
    let mut args = FuncArgs::from(arguments);
    let o = args.next_arg();
    let obj = to_object(o)?;
    let value_list = enumerable_own_properties(&obj, KeyValueKind::Value)?;
    Ok(create_array_from_list(value_list.as_slice()).into())
}

fn object_prototype_has_own_property(
    this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.prototype.hasOwnProperty ( V )
    // This method performs the following steps when called:
    //
    //  1. Let P be ? ToPropertyKey(V).
    //  2. Let O be ? ToObject(this value).
    //  3. Return ? HasOwnProperty(O, P).
    // NOTE The ordering of steps 1 and 2 is chosen to ensure that any exception that would have been thrown
    // by step 1 in previous editions of this specification will continue to be thrown even if the this value
    // is undefined or null.
    let mut args = FuncArgs::from(arguments);
    let v = args.next_arg();
    let p = to_property_key(v)?;
    let o = to_object(this_value)?;
    o.has_own_property(&p).map(ECMAScriptValue::from)
}

fn object_prototype_is_prototype_of(
    this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.prototype.isPrototypeOf ( V )
    // This method performs the following steps when called:
    //
    //  1. If V is not an Object, return false.
    //  2. Let O be ? ToObject(this value).
    //  3. Repeat,
    //      a. Set V to ? V.[[GetPrototypeOf]]().
    //      b. If V is null, return false.
    //      c. If SameValue(O, V) is true, return true.
    // NOTE The ordering of steps 1 and 2 preserves the behaviour specified by previous editions of this
    // specification for the case where V is not an object and the this value is undefined or null.
    let mut args = FuncArgs::from(arguments);
    let v = args.next_arg();
    match v {
        ECMAScriptValue::Object(mut v) => {
            let o = to_object(this_value)?;
            loop {
                match v.o.get_prototype_of()? {
                    None => return Ok(ECMAScriptValue::from(false)),
                    Some(new_v) => {
                        v = new_v;
                        if o == v {
                            return Ok(ECMAScriptValue::from(true));
                        }
                    }
                }
            }
        }
        _ => Ok(ECMAScriptValue::from(false)),
    }
}

fn object_prototype_property_is_enumerable(
    this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.prototype.propertyIsEnumerable ( V )
    // This method performs the following steps when called:
    //
    //  1. Let P be ? ToPropertyKey(V).
    //  2. Let O be ? ToObject(this value).
    //  3. Let desc be ? O.[[GetOwnProperty]](P).
    //  4. If desc is undefined, return false.
    //  5. Return desc.[[Enumerable]].
    //
    // NOTE 1 This method does not consider objects in the prototype chain.
    //
    // NOTE 2 The ordering of steps 1 and 2 is chosen to ensure that any exception that would have been thrown
    // by step 1 in previous editions of this specification will continue to be thrown even if the this value
    // is undefined or null.
    let mut args = FuncArgs::from(arguments);
    let v = args.next_arg();
    let p = to_property_key(v)?;
    let o = to_object(this_value)?;
    let desc = o.o.get_own_property(&p)?;
    Ok(match desc {
        None => false,
        Some(desc) => desc.enumerable,
    }
    .into())
}

fn object_prototype_to_locale_string(
    this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Object.prototype.toLocaleString ( [ reserved1 [ , reserved2 ] ] )
    // This method performs the following steps when called:
    //
    //  1. Let O be the this value.
    //  2. Return ? Invoke(O, "toString").
    this_value.invoke(&"toString".into(), &[])
}

#[cfg(test)]
mod tests;
