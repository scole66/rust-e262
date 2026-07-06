// JSON global object

use super::*;
use std::mem;

pub(crate) fn provision_json_intrinsic(realm: &Rc<RefCell<Realm>>) {
    // The JSON object:
    //
    //   * is %JSON%.
    //   * is the initial value of the "JSON" property of the global object.
    //   * is an ordinary object.
    //   * contains two functions, parse and stringify, that are used to parse and construct JSON texts.
    //   * has a [[Prototype]] internal slot whose value is %Object.prototype%.
    //   * does not have a [[Construct]] internal method; it cannot be used as a constructor with the new operator.
    //   * does not have a [[Call]] internal method; it cannot be invoked as a function.
    //
    // The JSON Data Interchange Format is defined in ECMA-404. The JSON interchange format used in this specification
    // is exactly that described by ECMA-404. Conforming implementations of JSON.parse and JSON.stringify must support
    // the exact interchange format described in the ECMA-404 specification without any deletions or extensions to the
    // format.
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();
    let json_object = ordinary_object_create(Some(object_prototype));
    realm.borrow_mut().intrinsics.json = json_object.clone();

    macro_rules! json_function {
        ( $steps:expr, $name:expr, $length:expr ) => {
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(
                Box::new($steps),
                None,
                f64::from($length),
                key.clone(),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm.clone()),
                Some(function_prototype.clone()),
                None,
            );
            define_property_or_throw(
                &json_object,
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

    json_function!(json_stringify, "stringify", 3);
    json_function!(json_parse, "parse", 2);
}

fn json_stringify(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // JSON.stringify ( value [ , replacer [ , space ] ] )
    let mut args = FuncArgs::from(arguments);
    let value = args.next_arg();
    let replacer = args.next_arg();
    let mut space = args.next_arg();

    // This function returns a String in UTF-16 encoded JSON format representing an ECMAScript language value, or undefined. It can take three parameters. The value parameter is an ECMAScript language value, which is usually an object or array, although it can also be a String, Boolean, Number or null. The optional replacer parameter is either a function that alters the way objects and arrays are stringified, or an array of Strings and Numbers that acts as an inclusion list for selecting the object properties that will be stringified. The optional space parameter is a String or Number that allows the result to have white space injected into it to improve human readability.
    //
    // It performs the following steps when called:
    //
    // 1. Let stack be a new empty List.
    let stack = vec![];
    // 2. Let indent be the empty String.
    let indent = JSString::from("");
    // 3. Let propertyList be undefined.
    let mut property_list = vec![];
    // 4. Let replacerFunc be undefined.
    let mut replacer_func = None;
    // 5. If replacer is an Object, then
    if let ECMAScriptValue::Object(replacer) = replacer {
        // a. If IsCallable(replacer) is true, then
        if replacer.is_callable() {
            // i. Set replacerFunc to replacer.
            replacer_func = Some(replacer);
        } else if replacer.is_array()? {
            // b. Else,
            // i. Let isArray be ? IsArray(replacer).
            // ii. If isArray is true, then
            // 1. Set propertyList to a new empty List.
            // 2. Let length be ? LengthOfArrayLike(replacer).
            let length = to_usize(replacer.length_of_array_like()?).expect(JS_INTEGER_USIZE_EXPECT);
            // 3. Let k be 0.
            let mut k = 0;
            // 4. Repeat, while k < length,
            while k < length {
                // a. Let propertyKey be ! ToString(𝔽(k)).
                let property_key = PropertyKey::from(k);
                // b. Let propertyValue be ? Get(replacer, propertyKey).
                let property_value = replacer.get(&property_key)?;
                // c. Let item be undefined.
                let mut item = None;
                // d. If propertyValue is a String, then
                if let ECMAScriptValue::String(jss) = property_value {
                    // i. Set item to propertyValue.
                    item = Some(jss);
                }
                // e. Else if propertyValue is a Number, then
                else if matches!(property_value, ECMAScriptValue::Number(_)) {
                    // i. Set item to ! ToString(propertyValue).
                    item = Some(to_string(property_value).expect("Numbers convert without error"));
                }
                // f. Else if propertyValue is an Object, then
                else if let ECMAScriptValue::Object(obj) = &property_value {
                    // i. If propertyValue has a [[StringData]] or [[NumberData]] internal slot, set item to ? ToString(propertyValue).
                    if obj.o.to_number_obj().is_some() || obj.o.to_string_obj().is_some() {
                        item = Some(to_string(property_value)?);
                    }
                }
                // g. If item is not undefined and propertyList does not contain item, then
                if let Some(key) = item
                    && !property_list.contains(&key)
                {
                    //    i. Append item to propertyList.
                    property_list.push(key);
                }
                // h. Set k to k + 1.
                k += 1;
            }
        }
    }
    // 6. If space is an Object, then
    if let ECMAScriptValue::Object(obj) = &space {
        // a. If space has a [[NumberData]] internal slot, then
        if obj.o.to_number_obj().is_some() {
            // i. Set space to ? ToNumber(space).
            space = ECMAScriptValue::Number(space.to_number()?);
        }
        // b. Else if space has a [[StringData]] internal slot, then
        else if obj.o.to_string_obj().is_some() {
            // i. Set space to ? ToString(space).
            space = ECMAScriptValue::String(to_string(space)?);
        }
    }
    // 7. If space is a Number, then
    let gap = if matches!(space, ECMAScriptValue::Number(_)) {
        // a. Let spaceMV be ! ToIntegerOrInfinity(space).
        // b. Set spaceMV to min(10, spaceMV).
        let space_mv = (10.0_f64).min(space.to_integer_or_infinity().expect("numbers don't throw"));
        // c. If spaceMV < 1, let gap be the empty String; else let gap be the String value containing spaceMV occurrences of the code unit 0x0020 (SPACE).
        if space_mv < 1.0 {
            JSString::from("")
        } else {
            JSString::from(" ".repeat(to_usize(space_mv).expect("current value is zero to ten")))
        }
    }
    // 8. Else if space is a String, then
    else if let ECMAScriptValue::String(jss) = space {
        // a. If the length of space ≤ 10, let gap be space; else let gap be the substring of space from 0 to 10.
        if jss.len() <= 10 { jss } else { JSString::from(&jss.as_slice()[0..10]) }
    }
    // 9. Else,
    else {
        // a. Let gap be the empty String.
        JSString::from("")
    };
    // 10. Let wrapper be OrdinaryObjectCreate(%Object.prototype%).
    let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
    let wrapper = ordinary_object_create(Some(object_prototype));
    // 11. Perform ! CreateDataPropertyOrThrow(wrapper, the empty String, value).
    wrapper.create_data_property_or_throw("", value).expect(GOODOBJ);
    // 12. Let state be the JSON Serialization Record { [[ReplacerFunction]]: replacerFunc, [[Stack]]: stack, [[Indent]]: indent, [[Gap]]: gap, [[PropertyList]]: propertyList }.
    let mut state = JSONSerialization {
        replacer_function: replacer_func,
        property_list: Rc::from(property_list.into_boxed_slice()),
        gap,
        stack,
        indent,
    };
    // 13. Return ? SerializeJSONProperty(state, the empty String, wrapper).
    serialize_json_property(&mut state, &JSString::from(""), wrapper).map(ECMAScriptValue::from)
}

fn json_parse(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

#[expect(dead_code)]
struct JSONSerialization {
    replacer_function: Option<Object>,
    property_list: Rc<[JSString]>,
    gap: JSString,
    stack: Vec<ECMAScriptValue>,
    indent: JSString,
}

/// Returns `code_unit` formatted as a JSON-compatible Unicode escape.
///
/// Examples:
/// - `0x000A` -> `"\\u000a"`
/// - `0xD800` -> `"\\ud800"`
fn unicode_escape(code_unit: u16) -> JSString {
    format!("\\u{code_unit:04x}").into()
}

/// Returns `value` quoted as a JSON string.
///
/// Implements ECMAScript `QuoteJSONString`, escaping:
/// - JSON short escapes: `\b`, `\t`, `\n`, `\f`, `\r`, `\"`, `\\`
/// - control characters below U+0020
/// - isolated UTF-16 surrogate code points
fn quote_json_string(value: &JSString) -> JSString {
    // Start with the opening quote. The closing quote is appended at the end.
    let mut product = JSString::from("\"");

    for code_point in value.to_code_points() {
        match code_point {
            // JSON short escapes from the ECMAScript QuoteJSONString table.
            0x0008 => product = product.concat("\\b"),
            0x0009 => product = product.concat("\\t"),
            0x000A => product = product.concat("\\n"),
            0x000C => product = product.concat("\\f"),
            0x000D => product = product.concat("\\r"),
            0x0022 => product = product.concat("\\\""),
            0x005C => product = product.concat("\\\\"),

            // Other control characters and isolated surrogates must be escaped
            // using a fixed-width Unicode escape.
            0x0000..0x0020 | 0xD800..=0xDFFF => {
                let unit = u16::try_from(code_point).expect("matched range fits in u16");
                product = product.concat(unicode_escape(unit));
            }

            // All remaining code points are emitted as their UTF-16 encoding.
            _ => {
                let mut dst = [0_u16, 0_u16];
                let encoded: &[u16] =
                    utf16_encode_code_point(code_point, &mut dst).expect("code_point is a valid Unicode code point");
                product = product.concat(encoded);
            }
        }
    }

    product.concat("\"")
}

fn serialize_json_property(state: &mut JSONSerialization, key: &JSString, holder: Object) -> Completion<Option<JSString>> {
    // Read the property value before applying `toJSON` or the replacer.
    let mut value = holder.get(&key.clone().into())?;

    // Objects and BigInts get a chance to define their own JSON representation.
    if matches!(value, ECMAScriptValue::Object(_) | ECMAScriptValue::BigInt(_)) {
        let to_json = value.get(&"toJSON".into())?;

        if to_json.is_callable() {
            value = call(&to_json, &value, &[key.clone().into()])?;
        }
    }

    // A replacer function post-processes the value after `toJSON`.
    if let Some(obj) = &state.replacer_function {
        value = call(
            &ECMAScriptValue::Object(obj.clone()),
            &ECMAScriptValue::Object(holder),
            &[key.clone().into(), value],
        )?;
    }

    // Raw JSON objects bypass normal quoting/serialization and contribute their
    // stored raw JSON string directly.
    if let ECMAScriptValue::Object(val_obj) = &value {
        if val_obj.is_raw_json() {
            return Ok(Some(
                val_obj
                    .get(&"rawJSON".into())?
                    .into_jsstring()
                    .expect("raw JSON objects always store rawJSON as a string"),
            ));
        }
    }

    // Primitive values either serialize directly, disappear from object output,
    // or throw if JSON does not support them.
    match &value {
        ECMAScriptValue::Null => Ok(Some(JSString::from("null"))),
        ECMAScriptValue::Symbol(_) | ECMAScriptValue::Undefined => Ok(None),
        ECMAScriptValue::Boolean(true) => Ok(Some(JSString::from("true"))),
        ECMAScriptValue::Boolean(false) => Ok(Some(JSString::from("false"))),
        ECMAScriptValue::String(jsstring) => Ok(Some(quote_json_string(jsstring))),
        ECMAScriptValue::Number(n) => {
            if n.is_finite() {
                Ok(Some(to_string(value).expect("finite numbers convert to strings")))
            } else {
                Ok(Some(JSString::from("null")))
            }
        }
        ECMAScriptValue::BigInt(_) => Err(create_type_error("BigInts cannot be JSON-stringified")),
        ECMAScriptValue::Object(object) => {
            // Callable objects serialize like `undefined`.
            if object.is_callable() {
                Ok(None)
            } else if object.is_array()? {
                serialize_json_array( state, value)
            } else {
                serialize_json_object( state, value)
            }
        }
    }
}

#[expect(unused_variables)]
fn serialize_json_array(state: &mut JSONSerialization, value: ECMAScriptValue) -> Completion<Option<JSString>> {
    // SerializeJSONArray ( state, value )
    // The abstract operation SerializeJSONArray takes arguments state (a JSON Serialization Record) and value (an Object) and returns either a normal completion containing a String or a throw completion. It serializes an array. It performs the following steps when called:
    // 
    // 1. If state.[[Stack]] contains value, throw a TypeError exception because the structure is cyclical.
    if state.stack.contains(&value) {
        return Err(create_type_error("cyclical structure detected in JSON stringify"));
    }
    // 2. Append value to state.[[Stack]].
    state.stack.push(value.clone());
    // 3. Let stepBack be state.[[Indent]].
    // 4. Set state.[[Indent]] to the string-concatenation of state.[[Indent]] and state.[[Gap]].
    let new_indent = state.indent.concat_from_ref(&state.gap);
    let step_back = mem::replace(&mut state.indent, new_indent);
    // 5. Let partial be a new empty List.
    // 6. Let length be ? LengthOfArrayLike(value).
    // 7. Let index be 0.
    // 8. Repeat, while index < length,
    //    a. Let stringP be ? SerializeJSONProperty(state, ! ToString(𝔽(index)), value).
    //    b. If stringP is undefined, then
    //       i. Append "null" to partial.
    //    c. Else,
    //       i. Append stringP to partial.
    //    d. Set index to index + 1.
    // 9. If partial is empty, then
    //    a. Let final be "[]".
    // 10. Else,
    //     a. If state.[[Gap]] is the empty String, then
    //        i. Let properties be the String value formed by concatenating all the element Strings of partial with each adjacent pair of Strings separated with the code unit 0x002C (COMMA). A comma is not inserted either before the first String or after the last String.
    //        ii. Let final be the string-concatenation of "[", properties, and "]".
    //     b. Else,
    //        i. Let separator be the string-concatenation of the code unit 0x002C (COMMA), the code unit 0x000A (LINE FEED), and state.[[Indent]].
    //        ii. Let properties be the String value formed by concatenating all the element Strings of partial with each adjacent pair of Strings separated with separator. The separator String is not inserted either before the first String or after the last String.
    //        iii. Let final be the string-concatenation of "[", the code unit 0x000A (LINE FEED), state.[[Indent]], properties, the code unit 0x000A (LINE FEED), stepBack, and "]".
    // 11. Remove the last element of state.[[Stack]].
    // 12. Set state.[[Indent]] to stepBack.
    // 13. Return final.
    todo!()
}

#[expect(unused_variables)]
fn serialize_json_object(state: &mut JSONSerialization, value: ECMAScriptValue) -> Completion<Option<JSString>> {
    todo!()
}
