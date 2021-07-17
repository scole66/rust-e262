use crate::agent::{Agent, WksId};
use crate::cr::Completion;
use crate::function_object::create_builtin_function;
use crate::object::{define_property_or_throw, Object, PotentialPropertyDescriptor, BUILTIN_FUNCTION_SLOTS};
use crate::realm::Realm;
use crate::values::{to_object, ECMAScriptValue, PropertyKey};
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
use super::arrays::is_array;
use super::object::get;
use super::strings::JSString;
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

pub fn attach_object_prototype_properties(agent: &mut Agent, realm: Rc<RefCell<Realm>>, target: &Object) {
    let function_proto = realm.borrow().intrinsics.function_prototype.clone();

    let mut connect = |name, length, steps| {
        let key = PropertyKey::from(name);
        let fcn = create_builtin_function(agent, steps, length, key.clone(), &BUILTIN_FUNCTION_SLOTS, Some(realm.clone()), Some(function_proto.clone()), None);
        define_property_or_throw(
            agent,
            target,
            key,
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(fcn)), writable: Some(true), enumerable: Some(false), configurable: Some(true), ..Default::default() },
        )
        .unwrap();
    };

    connect("valueOf", 0_f64, object_prototype_value_of);
    connect("toString", 0_f64, object_prototype_to_string);
}
