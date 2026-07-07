// JSON global object

use super::*;
use itertools::Itertools;
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
    // JSON.stringify(value [ , replacer [ , space ] ])

    let mut args = FuncArgs::from(arguments);
    let value = args.next_arg();
    let replacer = args.next_arg();
    let mut space = args.next_arg();

    // 1-4. Initialize the serialization state fields.
    let stack = vec![];
    let indent = JSString::from("");
    let mut property_list: Option<Vec<JSString>> = None;
    let mut replacer_func = None;

    // 5. If replacer is callable, use it as the replacer function. Otherwise,
    // if it is an Array, build the property inclusion list from its String and
    // Number entries, including boxed String/Number objects.
    if let ECMAScriptValue::Object(replacer) = replacer {
        if replacer.is_callable() {
            replacer_func = Some(replacer);
        } else if replacer.is_array()? {
            let length = to_usize(replacer.length_of_array_like()?).expect(JS_INTEGER_USIZE_EXPECT);

            for k in 0..length {
                let property_key = PropertyKey::from(k);
                let property_value = replacer.get(&property_key)?;

                let item = match property_value {
                    ECMAScriptValue::String(jss) => Some(jss),

                    ECMAScriptValue::Number(_) => {
                        Some(to_string(property_value).expect("Numbers convert without error"))
                    }

                    ECMAScriptValue::Object(ref obj)
                        if obj.o.to_number_obj().is_some() || obj.o.to_string_obj().is_some() =>
                    {
                        Some(to_string(property_value)?)
                    }

                    _ => None,
                };

                if let Some(key) = item
                    && !property_list.as_ref().is_some_and(|list| list.contains(&key))
                {
                    property_list.get_or_insert_with(Vec::new).push(key);
                }
            }
        }
    }

    // 6. Unbox Number and String objects used as the space argument.
    if let ECMAScriptValue::Object(obj) = &space {
        if obj.o.to_number_obj().is_some() {
            space = ECMAScriptValue::Number(space.to_number()?);
        } else if obj.o.to_string_obj().is_some() {
            space = ECMAScriptValue::String(to_string(space)?);
        }
    }

    // 7-9. Convert the space argument into the gap string used for pretty output.
    let gap = match space {
        ECMAScriptValue::Number(_) => {
            let space_mv = space.to_integer_or_infinity().expect("Numbers do not throw during ToIntegerOrInfinity");

            let space_mv = 10.0_f64.min(space_mv);

            if space_mv < 1.0 {
                JSString::from("")
            } else {
                JSString::from(" ".repeat(to_usize(space_mv).expect("space value is in the range 1..=10")))
            }
        }

        ECMAScriptValue::String(jss) => {
            if jss.len() <= 10 {
                jss
            } else {
                JSString::from(&jss.as_slice()[0..10])
            }
        }

        _ => JSString::from(""),
    };

    // 10-11. Wrap the input value under the empty-string property name.
    let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
    let wrapper = ordinary_object_create(Some(object_prototype));

    wrapper.create_data_property_or_throw("", value).expect(GOODOBJ);

    // 12. Create the JSON serialization record.
    let mut state = JSONSerialization {
        replacer_function: replacer_func,
        property_list: property_list.map(|list| Rc::from(list.into_boxed_slice())),
        gap,
        stack,
        indent,
    };

    // 13. Serialize the wrapper's empty-string property.
    serialize_json_property(&mut state, &JSString::from(""), &wrapper).map(ECMAScriptValue::from)
}

fn json_parse(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

struct JSONSerialization {
    replacer_function: Option<Object>,
    property_list: Option<Rc<[JSString]>>,
    gap: JSString,
    stack: Vec<Object>,
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
            0x0008 => product = product.concat(utf16_const!("\\b")),
            0x0009 => product = product.concat(utf16_const!("\\t")),
            0x000A => product = product.concat(utf16_const!("\\n")),
            0x000C => product = product.concat(utf16_const!("\\f")),
            0x000D => product = product.concat(utf16_const!("\\r")),
            0x0022 => product = product.concat(utf16_const!("\\\"")),
            0x005C => product = product.concat(utf16_const!("\\\\")),

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

    product.concat(utf16_const!("\""))
}

fn serialize_json_property(
    state: &mut JSONSerialization,
    key: &JSString,
    holder: &Object,
) -> Completion<Option<JSString>> {
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
            &ECMAScriptValue::Object(holder.clone()),
            &[key.clone().into(), value],
        )?;
    }

    // Raw JSON objects bypass normal quoting/serialization and contribute their
    // stored raw JSON string directly.
    if let ECMAScriptValue::Object(val_obj) = &value
        && val_obj.is_raw_json()
    {
        return Ok(Some(
            val_obj.get(&"rawJSON".into())?.into_jsstring().expect("raw JSON objects always store rawJSON as a string"),
        ));
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
                serialize_json_array(state, object).map(Some)
            } else {
                serialize_json_object(state, object).map(Some)
            }
        }
    }
}

fn serialize_json_array(state: &mut JSONSerialization, value: &Object) -> Completion<JSString> {
    // SerializeJSONArray(state, value)

    // Reject cyclic structures.
    if state.stack.contains(value) {
        return Err(create_type_error("cyclical structure detected in JSON stringify"));
    }

    // Track the current array while serializing its elements.
    state.stack.push(value.clone());

    // Increase indentation for nested output, saving the previous indentation.
    let new_indent = state.indent.concat(&state.gap);
    let step_back = mem::replace(&mut state.indent, new_indent);

    // Serialize each array index. Elements that serialize to undefined become null.
    let length = to_usize(value.length_of_array_like()?).expect(JS_INTEGER_USIZE_EXPECT);
    let mut partial = Vec::with_capacity(length);

    for index in 0..length {
        let key = JSString::from(format!("{index}"));

        match serialize_json_property(state, &key, value)? {
            Some(serialized) => partial.push(serialized),
            None => partial.push(JSString::from("null")),
        }
    }

    // Empty arrays serialize directly as [].
    let result = if partial.is_empty() {
        JSString::from("[]")
    } else if state.gap.is_empty() {
        // Compact form: join elements with "," and wrap in brackets.
        let comma = JSString::from(",");

        let mut result = JSString::from("[");
        for property in Itertools::intersperse(partial.iter(), &comma) {
            result = result.concat(property);
        }

        result.concat(utf16_const!("]"))
    } else {
        // Pretty-printed form: join elements with ",\n{indent}" and wrap with
        // leading/trailing newlines.
        let separator = JSString::from(",").concat(NEWLINE).concat(&state.indent);

        let mut result = JSString::from("[\n").concat(&state.indent);
        for property in Itertools::intersperse(partial.iter(), &separator) {
            result = result.concat(property);
        }

        result.concat(utf16_const!("\n")).concat(&step_back).concat(utf16_const!("]"))
    };

    // Leave this array serialization frame.
    state.stack.pop();

    // Restore the caller's indentation.
    state.indent = step_back;

    Ok(result)
}

fn serialize_json_object(state: &mut JSONSerialization, value: &Object) -> Completion<JSString> {
    let vec_holder;
    let rc_holder;

    // SerializeJSONObject(state, value)

    // Reject cyclic structures.
    if state.stack.contains(value) {
        return Err(create_type_error("cyclical structure detected in JSON stringify"));
    }

    // Track the current object while serializing its properties.
    state.stack.push(value.clone());

    // Increase indentation for nested output, saving the caller's indentation.
    let new_indent = state.indent.concat(&state.gap);
    let step_back = mem::replace(&mut state.indent, new_indent);

    // Use the replacer property list when present; otherwise collect this
    // object's enumerable own property keys.
    let keys = if let Some(list) = state.property_list.clone() {
        rc_holder = list;
        rc_holder.as_ref()
    } else {
        vec_holder = enumerable_own_properties(value, KeyValueKind::Key)?
            .into_iter()
            .map(|prop_str| prop_str.into_jsstring().expect("enumerable own props are always strings"))
            .collect::<Vec<_>>();

        vec_holder.as_slice()
    };

    // Serialize each selected property. Properties that serialize to undefined
    // are omitted from the final object.
    let mut partial = Vec::with_capacity(keys.len());

    for property_key in keys {
        let string_p = serialize_json_property(state, property_key, value)?;

        if let Some(property_string) = string_p {
            // Build `"key":value` or `"key": value`, depending on pretty-printing.
            let member = quote_json_string(property_key).concat(COLON);

            let member = if state.gap.is_empty() { member } else { member.concat(BLANK) };

            partial.push(member.concat(property_string));
        }
    }

    let result = if partial.is_empty() {
        // Empty objects serialize directly as {}.
        JSString::from("{}")
    } else if state.gap.is_empty() {
        // Compact form: join members with "," and wrap in braces.
        let comma = JSString::from(",");

        let mut result = JSString::from("{");
        for property in Itertools::intersperse(partial.iter(), &comma) {
            result = result.concat(property);
        }

        result.concat(utf16_const!("}"))
    } else {
        // Pretty-printed form: join members with ",\n{indent}" and wrap with
        // leading/trailing newlines.
        let separator = JSString::from(",").concat(NEWLINE).concat(&state.indent);

        let mut result = JSString::from("{\n").concat(&state.indent);
        for property in Itertools::intersperse(partial.iter(), &separator) {
            result = result.concat(property);
        }

        result.concat(utf16_const!("\n")).concat(&step_back).concat(utf16_const!("}"))
    };

    // Leave this object serialization frame and restore the caller's indentation.
    state.stack.pop();
    state.indent = step_back;

    Ok(result)
}
