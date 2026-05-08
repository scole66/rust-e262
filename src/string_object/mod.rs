use super::*;
use genawaiter::rc::Co;
use std::cell::RefCell;
use std::slice;
use unicode_normalization::UnicodeNormalization;

/// String Objects
///
/// See [String Objects](https://tc39.es/ecma262/#sec-string-objects) in ECMA-262.

// String Exotic Objects
//
// See [String Exotic Objects](https://tc39.es/ecma262/#sec-string-exotic-objects) in ECMA-262.
//
// A String object is an exotic object that encapsulates a String value and exposes virtual integer-indexed
// data properties corresponding to the individual code unit elements of the String value. String exotic
// objects always have a data property named "length" whose value is the number of code unit elements in the
// encapsulated String value. Both the code unit data properties and the "length" property are non-writable
// and non-configurable.
//
// An object is a String exotic object (or simply, a String object) if its [[GetOwnProperty]],
// [[DefineOwnProperty]], and [[OwnPropertyKeys]] internal methods use the following implementations, and its
// other essential internal methods use the definitions found in 10.1. These methods are installed in
// StringCreate.
//
// String exotic objects have the same internal slots as ordinary objects. They also have a [[StringData]]
// internal slot.

#[derive(Debug)]
pub(crate) struct StringObject {
    common: RefCell<CommonObjectData>,
    string_data: JSString,
}

impl<'a> From<&'a StringObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a StringObject) -> Self {
        obj
    }
}

impl ObjectInterface for StringObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }

    fn uses_ordinary_get_prototype_of(&self) -> bool {
        true
    }

    fn id(&self) -> usize {
        self.common.borrow().objid
    }

    fn to_string_obj(&self) -> Option<&StringObject> {
        Some(self)
    }

    fn kind(&self) -> ObjectTag {
        ObjectTag::String
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
        // [[GetOwnProperty]] ( P )
        //
        // The [[GetOwnProperty]] internal method of a String exotic object S takes argument P (a property
        // key) and returns a normal completion containing either a Property Descriptor or undefined. It
        // performs the following steps when called:
        //
        // 1. Let desc be OrdinaryGetOwnProperty(S, P).
        // 2. If desc is not undefined, return desc.
        // 3. Return StringGetOwnProperty(S, P).
        Ok(ordinary_get_own_property(self, key).or_else(|| self.string_get_own_property(key)))
    }

    fn define_own_property(&self, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool> {
        // [[DefineOwnProperty]] ( P, Desc )
        //
        // The [[DefineOwnProperty]] internal method of a String exotic object S takes arguments P (a property
        // key) and Desc (a Property Descriptor) and returns a normal completion containing a Boolean. It
        // performs the following steps when called:
        //
        //  1. Let stringDesc be StringGetOwnProperty(S, P).
        //  2. If stringDesc is not undefined, then
        //      a. Let extensible be S.[[Extensible]].
        //      b. Return IsCompatiblePropertyDescriptor(extensible, Desc, stringDesc).
        //  3. Return ! OrdinaryDefineOwnProperty(S, P, Desc).
        let string_desc = self.string_get_own_property(&key);
        match string_desc {
            Some(string_desc) => {
                let extensible = self.common.borrow().extensible;
                Ok(is_compatible_property_descriptor(extensible, desc, Some(&string_desc)))
            }
            _ => ordinary_define_own_property(self, key, desc),
        }
    }

    fn has_property(&self, key: &PropertyKey) -> Completion<bool> {
        ordinary_has_property(self, key)
    }

    fn get(&self, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        ordinary_get(self, key, receiver)
    }

    fn set(&self, key: PropertyKey, value: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        ordinary_set(self, key, value, receiver)
    }

    fn delete(&self, key: &PropertyKey) -> Completion<bool> {
        ordinary_delete(self, key)
    }

    fn own_property_keys(&self) -> Completion<Vec<PropertyKey>> {
        let mut keys = vec![];

        // String exotic objects report their string-index keys first, synthesized
        // from [[StringData]] rather than stored in the ordinary property table.
        // The indexes are UTF-16 code unit positions.
        let string = &self.string_data;
        let len = string.len();
        for idx in 0..len {
            keys.push(PropertyKey::from(idx));
        }

        let bindings = &self.common.borrow().properties;

        // Ordinary own keys are collected into the spec's required ordering groups:
        // array indexes after the string length, then other strings, then symbols.
        let mut norm_keys: Vec<(PropertyKey, usize)> = Vec::new();
        let mut symb_keys: Vec<(PropertyKey, usize)> = Vec::new();
        let mut extra_numbers: Vec<(PropertyKey, u32)> = Vec::new();

        for (key, desc) in bindings {
            if key.is_array_index() {
                let keyval = array_index_key(key);

                // Indexes covered by [[StringData]] are synthesized above and are
                // non-configurable, so they should not also appear as ordinary
                // properties.
                assert!(keyval as usize >= len);

                extra_numbers.push((key.clone(), keyval));
            } else {
                match key {
                    // `spot` records creation order for ordinary string and symbol
                    // properties.
                    PropertyKey::String(_) => {
                        norm_keys.push((key.clone(), desc.spot));
                    }
                    PropertyKey::Symbol(_) => {
                        symb_keys.push((key.clone(), desc.spot));
                    }
                }
            }
        }

        // Numeric indexes are ordered numerically; other strings and symbols keep
        // their original creation order within their own groups.
        extra_numbers.sort_by_key(|x| x.1);
        norm_keys.sort_by_key(|x| x.1);
        symb_keys.sort_by_key(|x| x.1);

        keys.extend(extra_numbers.into_iter().map(|x| x.0));
        keys.extend(norm_keys.into_iter().map(|x| x.0));
        keys.extend(symb_keys.into_iter().map(|x| x.0));

        Ok(keys)
    }
}

pub(crate) fn string_create(value: JSString, prototype: Option<Object>) -> Object {
    StringObject::object(value, prototype)
}
impl From<JSString> for Object {
    fn from(s: JSString) -> Self {
        let prototype = intrinsic(IntrinsicId::StringPrototype);
        string_create(s, Some(prototype))
    }
}
impl From<&str> for Object {
    fn from(s: &str) -> Self {
        Object::from(JSString::from(s))
    }
}

impl StringObject {
    pub(crate) fn new(value: JSString, prototype: Option<Object>) -> Self {
        Self { common: RefCell::new(CommonObjectData::new(prototype, true, STRING_OBJECT_SLOTS)), string_data: value }
    }

    pub(crate) fn object(value: JSString, prototype: Option<Object>) -> Object {
        // The String object's length is fixed from its [[StringData]] at creation
        // time and counts UTF-16 code units, not Unicode scalar values.
        let length = value.len();

        // `Self::new` installs the String exotic behavior and stores both the
        // prototype and the primitive string data in the object.
        let s = Object { o: Rc::new(Self::new(value, prototype)) };

        // String objects expose a non-writable, non-enumerable, non-configurable
        // own `length` property. The string index properties are handled by the
        // exotic internal methods rather than being created here.
        define_property_or_throw(
            &s,
            "length",
            PotentialPropertyDescriptor::new().value(length).writable(false).enumerable(false).configurable(false),
        )
        .expect("fresh object can't fail to create this");

        s
    }
    pub(crate) fn string_get_own_property(&self, key: &PropertyKey) -> Option<PropertyDescriptor> {
        if let PropertyKey::String(p) = key {
            // String exotic index properties only exist for canonical numeric string
            // keys like "0", "1", and "42"; symbols and non-canonical strings fall
            // through to ordinary property lookup.
            let index = canonical_numeric_index_string(p)?;

            // Only integral numeric indexes are valid string element properties.
            // Values like "NaN", "Infinity", and "1.5" are not string indexes.
            is_integral_number(&index.into()).then_some(())?;

            // "-0" is canonical numeric, but it is not a valid string element key.
            // This keeps "-0" distinct from "0" as required by the exotic object rules.
            (index != 0.0 || index.signum() != -1.0).then_some(())?;

            let string = &self.string_data;
            let len = string.len();

            // String element indexes are bounded by the UTF-16 code unit length.
            #[expect(clippy::cast_precision_loss)]
            (index >= 0.0 && index < len as f64).then_some(())?;

            // Safe after the integral and bounds checks above. The returned property
            // is exactly one UTF-16 code unit, even if it is half of a surrogate pair.
            let idx = to_usize(index).expect(JS_INTEGER_USIZE_EXPECT);
            let value = JSString::from(&string.as_slice()[idx..=idx]);

            // String index properties are synthesized from [[StringData]] and are
            // read-only, enumerable, and non-configurable.
            Some(PropertyDescriptor {
                property: PropertyKind::Data(DataProperty { value: value.into(), writable: false }),
                enumerable: true,
                configurable: false,
                spot: 0,
            })
        } else {
            None
        }
    }
}

pub(crate) fn provision_string_intrinsic(realm: &Rc<RefCell<Realm>>) {
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();

    // The String Constructor
    //
    // * is %String%.
    // * is the initial value of the "String" property of the global object.
    // * creates and initializes a new String object when called as a constructor.
    // * performs a type conversion when called as a function rather than as a constructor.
    // * may be used as the value of an extends clause of a class definition. Subclass constructors that
    //   intend to inherit the specified String behaviour must include a super call to the String
    //   constructor to create and initialize the subclass instance with a [[StringData]] internal slot.
    //
    // Properties of the String Constructor
    // The String constructor:
    //
    // * has a [[Prototype]] internal slot whose value is %Function.prototype%.
    let string_constructor = create_builtin_function(
        Box::new(string_constructor_function),
        Some(ConstructorKind::Base),
        1.0,
        PropertyKey::from("String"),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype.clone()),
        None,
    );

    // Constructor Function Properties
    macro_rules! constructor_function {
        ( $steps:expr_2021, $name:expr_2021, $length:expr_2021 ) => {
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(
                Box::new($steps),
                None,
                $length,
                key.clone(),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm.clone()),
                Some(function_prototype.clone()),
                None,
            );
            define_property_or_throw(
                &string_constructor,
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

    constructor_function!(string_from_char_code, "fromCharCode", 1.0);
    constructor_function!(string_from_code_point, "fromCodePoint", 1.0);
    constructor_function!(string_raw, "raw", 1.0);

    // Constructor Data Properties
    macro_rules! constructor_data {
        ( $name:expr_2021, $value:expr_2021, $writable:expr_2021, $enumerable:expr_2021, $configurable:expr_2021 ) => {
            define_property_or_throw(
                &string_constructor,
                $name,
                PotentialPropertyDescriptor::new()
                    .value(ECMAScriptValue::from($value))
                    .writable($writable)
                    .enumerable($enumerable)
                    .configurable($configurable),
            )
            .unwrap();
        };
    }

    // The String prototype object:
    //
    // * is %String.prototype%.
    // * is a String exotic object and has the internal methods specified for such objects.
    // * has a [[StringData]] internal slot whose value is the empty String.
    // * has a "length" property whose initial value is +0𝔽 and whose attributes are { [[Writable]]:
    //   false, [[Enumerable]]: false, [[Configurable]]: false }.
    // * has a [[Prototype]] internal slot whose value is %Object.prototype%.
    let string_prototype = string_create("".into(), Some(object_prototype));

    // Prototype Function Properties
    macro_rules! prototype_function {
        ( $steps:expr_2021, $name:expr_2021, $length:expr_2021 ) => {
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(
                Box::new($steps),
                None,
                $length,
                key.clone(),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm.clone()),
                Some(function_prototype.clone()),
                None,
            );
            define_property_or_throw(
                &string_prototype,
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
    prototype_function!(string_prototype_at, "at", 1.0);
    prototype_function!(string_prototype_char_at, "charAt", 1.0);
    prototype_function!(string_prototype_char_code_at, "charCodeAt", 1.0);
    prototype_function!(string_prototype_code_point_at, "codePointAt", 1.0);
    prototype_function!(string_prototype_concat, "concat", 1.0);
    prototype_function!(string_prototype_ends_with, "endsWith", 1.0);
    prototype_function!(string_prototype_includes, "includes", 1.0);
    prototype_function!(string_prototype_index_of, "indexOf", 1.0);
    prototype_function!(string_prototype_is_well_formed, "isWellFormed", 0.0);
    prototype_function!(string_prototype_last_index_of, "lastIndexOf", 1.0);
    prototype_function!(string_prototype_locale_compare, "localeCompare", 1.0);
    prototype_function!(string_prototype_match, "match", 1.0);
    prototype_function!(string_prototype_match_all, "matchAll", 1.0);
    prototype_function!(string_prototype_normalize, "normalize", 0.0);
    prototype_function!(string_prototype_pad_end, "padEnd", 1.0);
    prototype_function!(string_prototype_pad_start, "padStart", 1.0);
    prototype_function!(string_prototype_repeat, "repeat", 1.0);
    prototype_function!(string_prototype_replace, "replace", 2.0);
    prototype_function!(string_prototype_replace_all, "replaceAll", 2.0);
    prototype_function!(string_prototype_search, "search", 1.0);
    prototype_function!(string_prototype_slice, "slice", 2.0);
    prototype_function!(string_prototype_split, "split", 2.0);
    prototype_function!(string_prototype_starts_with, "startsWith", 1.0);
    prototype_function!(string_prototype_substring, "substring", 2.0);
    prototype_function!(string_prototype_to_locale_lower_case, "toLocaleLowerCase", 0.0);
    prototype_function!(string_prototype_to_locale_upper_case, "toLocaleUpperCase", 0.0);
    prototype_function!(string_prototype_to_lower_case, "toLowerCase", 0.0);
    prototype_function!(string_prototype_to_string, "toString", 0.0);
    prototype_function!(string_prototype_to_upper_case, "toUpperCase", 0.0);
    prototype_function!(string_prototype_trim, "trim", 0.0);
    prototype_function!(string_prototype_trim_end, "trimEnd", 0.0);
    prototype_function!(string_prototype_trim_start, "trimStart", 0.0);
    prototype_function!(string_prototype_value_of, "valueOf", 0.0);
    prototype_function!(string_prototype_iterator, wks(WksId::Iterator), 0.0);

    macro_rules! prototype_data {
        ( $name:expr_2021, $value:expr_2021, $writable:expr_2021, $enumerable:expr_2021, $configurable:expr_2021 ) => {
            define_property_or_throw(
                &string_prototype,
                $name,
                PotentialPropertyDescriptor::new()
                    .value(ECMAScriptValue::from($value))
                    .writable($writable)
                    .enumerable($enumerable)
                    .configurable($configurable),
            )
            .unwrap();
        };
    }

    constructor_data!("prototype", string_prototype.clone(), false, false, false);
    prototype_data!("constructor", string_constructor.clone(), true, false, true);

    realm.borrow_mut().intrinsics.string = string_constructor;
    realm.borrow_mut().intrinsics.string_prototype = string_prototype;
}

fn string_constructor_function(
    _this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let s = if arguments.is_empty() {
        // `String()` and `new String()` both default a missing value to the
        // empty string.
        JSString::from("")
    } else {
        let value = &arguments[0];

        // Called as a function, `String(symbol)` is the one Symbol-to-string
        // conversion that does not throw. Called as a constructor, Symbol still
        // goes through ordinary ToString and therefore throws.
        if let (None, ECMAScriptValue::Symbol(sym)) = (new_target, value) {
            return Ok(ECMAScriptValue::from(sym.descriptive_string()));
        }

        // For all non-Symbol values, and for constructor calls, use ordinary
        // ToString semantics so side effects and errors are preserved.
        to_string(value.clone())?
    };

    if let Some(nt) = new_target {
        // Constructor calls create a boxed String object whose prototype comes
        // from `new.target`, so subclassing can choose a different prototype.
        let prototype = nt.get_prototype_from_constructor(IntrinsicId::StringPrototype)?;
        let s_obj = string_create(s, Some(prototype));
        Ok(ECMAScriptValue::from(s_obj))
    } else {
        // Function calls return the primitive string value, not a String object.
        Ok(ECMAScriptValue::from(s))
    }
}

fn this_string_value(value: ECMAScriptValue, from_where: &str) -> Completion<JSString> {
    let string_value = match value {
        // String prototype methods that use this helper require an actual string
        // value, so primitive strings can be returned directly.
        ECMAScriptValue::String(s) => Some(s),

        // Boxed String objects carry their primitive value in [[StringData]].
        ECMAScriptValue::Object(obj) => obj.o.to_string_obj().map(|sobj| sobj.string_data.clone()),

        // Unlike the generic String methods that call ToString on `this`, these
        // callers only accept primitive strings or boxed String objects.
        _ => None,
    };

    string_value.ok_or_else(|| create_type_error(format!("{from_where} requires that 'this' be a String")))
}

fn string_from_char_code(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Convert arguments left-to-right using ToUint16. This preserves observable
    // coercion order and stops immediately if any conversion throws.
    let code_units =
        arguments.iter().map(ECMAScriptValue::to_uint16).collect::<Result<Vec<u16>, AbruptCompletion>>()?;

    // `fromCharCode` works directly with UTF-16 code units. Values are wrapped
    // by ToUint16 rather than validated as Unicode code points, so lone
    // surrogates are allowed here.
    Ok(JSString::from(code_units).into())
}

fn string_from_code_point(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Most code points fit in one UTF-16 code unit, but code points above U+FFFF
    // need a surrogate pair. Reserve the worst case to avoid repeated reallocations.
    let mut out = Vec::with_capacity(arguments.len() * 2);

    for val in arguments {
        // Convert each argument as it is processed. This preserves observable
        // ToNumber ordering and stops immediately if a conversion throws.
        let next_cp = val.to_number()?;

        // `fromCodePoint` only accepts integer Unicode scalar value numbers.
        // NaN, infinities, fractions, negatives, and values above U+10FFFF are
        // RangeErrors rather than being wrapped or truncated.
        if next_cp.fract() != 0.0 || !(0.0..=1_114_111.0).contains(&next_cp) {
            return Err(create_range_error("code points must be integers in the range 0..0x10ffff"));
        }

        // Safe after the validation above. The UTF-16 encoder writes either one
        // code unit or a surrogate pair into this fixed-size buffer.
        let cp = to_uint32_f64(next_cp);
        let mut buf = [0u16; 2];
        let units = utf16_encode_code_point(cp, &mut buf).expect("points should be in range");

        out.extend_from_slice(units);
    }

    // With no arguments, `out` is still empty, producing the empty string.
    Ok(JSString::from(out.as_slice()).into())
}

fn string_raw(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let template = args.next_arg();

    // Keep references to the remaining arguments so substitution values are
    // converted lazily, interleaved with raw literal access below.
    let substitutions = args.remaining().collect::<Vec<_>>();

    let substitution_count = substitutions.len();

    // `String.raw` reads from `template.raw`, so the first argument must be
    // object-coercible before we can fetch the raw strings array-like object.
    let cooked = to_object(template)?;
    let literals = to_object(cooked.get(&"raw".into())?)?;

    // The raw template object is array-like; its length controls how many
    // literal chunks are read, not the number of substitutions.
    let literal_count = literals.length_of_array_like()?;

    // No raw literal chunks means the final cooked string is empty, regardless
    // of any substitutions that were provided.
    if literal_count <= 0.0 {
        return Ok(JSString::from("").into());
    }

    let literal_count = to_usize(literal_count).expect(JS_INTEGER_USIZE_EXPECT);

    let mut result = JSString::from("");
    let mut next_index = 0;

    loop {
        // Fetch and stringify each raw literal just before appending it. This
        // preserves observable property access and ToString ordering.
        let next_literal_val = literals.get(&next_index.into())?;
        let next_literal = to_string(next_literal_val)?;
        result = result.concat(next_literal);

        // The result always ends with a literal chunk. Stop here instead of
        // trying to read a substitution after the final literal.
        if next_index + 1 == literal_count {
            return Ok(result.into());
        }

        if next_index < substitution_count {
            // Substitutions are inserted only between literal chunks. Extra
            // substitutions are ignored, and missing substitutions contribute
            // nothing.
            let next_sub_val = substitutions[next_index].clone();
            let next_sub = to_string(next_sub_val)?;
            result = result.concat(next_sub);
        }

        next_index += 1;
    }
}

// 22.1.3.1 String.prototype.at ( index )
fn string_prototype_at(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let index = args.next_arg();

    // `at` is intentionally generic, so the receiver only needs to be
    // coercible and string-convertible; it does not need to be a String object.
    this_value.require_object_coercible()?;

    // Convert the receiver before `index`, preserving the spec's observable
    // coercion order if either conversion has side effects or throws.
    let strx = to_string(this_value.clone())?;

    // String positions are UTF-16 code unit indexes, not Unicode scalar indexes.
    let len = to_f64(strx.len()).expect(JS_INTEGER_F64_EXPECT);

    // `at` supports negative indexing. Normalize the argument using JS integer
    // conversion semantics before translating negative indexes from the end.
    let relative_index = index.to_integer_or_infinity()?;

    let k = if relative_index >= 0.0 { relative_index } else { relative_index + len };

    // Out-of-range positions do not throw; `at` returns `undefined`.
    if k < 0.0 || k >= len {
        return Ok(ECMAScriptValue::Undefined);
    }

    // Safe after the bounds check above. Return exactly one UTF-16 code unit,
    // even if that code unit is one half of a surrogate pair.
    let k = to_usize(k).expect(JS_INTEGER_USIZE_EXPECT);
    Ok(ECMAScriptValue::String(JSString::from(&strx.as_slice()[k..=k])))
}

// 22.1.3.2 String.prototype.charAt ( pos )
fn string_prototype_char_at(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let pos = args.next_arg();

    // `charAt` is intentionally generic, so the receiver only needs to be
    // coercible and string-convertible; it does not need to be a String object.
    this_value.require_object_coercible()?;

    // Convert the receiver before `pos`, preserving the spec's observable
    // coercion order if either conversion has side effects or throws.
    let strx = to_string(this_value.clone())?;

    // JavaScript indexing semantics accept fractions, NaN, and infinities here;
    // normalize them before checking the string bounds.
    let position = pos.to_integer_or_infinity()?;

    // String positions are UTF-16 code unit indexes, not Unicode scalar indexes.
    let size = to_f64(strx.len()).expect(JS_INTEGER_F64_EXPECT);

    Ok(ECMAScriptValue::String(if position < 0.0 || position >= size {
        // Out-of-range positions do not throw; `charAt` returns the empty
        // string instead.
        JSString::from("")
    } else {
        // Safe after the bounds check above. Return exactly one UTF-16 code
        // unit, even if that code unit is one half of a surrogate pair.
        let position = to_usize(position).expect(JS_INTEGER_USIZE_EXPECT);
        JSString::from(&strx.as_slice()[position..=position])
    }))
}

// 22.1.3.3 String.prototype.charCodeAt ( pos )
fn string_prototype_char_code_at(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let pos = args.next_arg();

    // `charCodeAt` is intentionally generic, so the receiver only needs to be
    // coercible and string-convertible; it does not need to be a String object.
    this_value.require_object_coercible()?;

    // Convert the receiver before `pos`, preserving the spec's observable
    // coercion order if either conversion has side effects or throws.
    let strx = to_string(this_value.clone())?;

    // JavaScript indexing semantics accept fractions, NaN, and infinities here;
    // normalize them before checking the string bounds.
    let position = pos.to_integer_or_infinity()?;

    // String positions are UTF-16 code unit indexes, not Unicode scalar indexes.
    let size = to_f64(strx.len()).expect(JS_INTEGER_F64_EXPECT);

    // Unlike `codePointAt`, an out-of-range lookup returns NaN rather than
    // `undefined`.
    if position < 0.0 || position >= size {
        Ok(ECMAScriptValue::Number(f64::NAN))
    } else {
        // Safe after the bounds check above, and still a code-unit index.
        // This returns exactly one UTF-16 code unit, even if it is a surrogate.
        let position = to_usize(position).expect(JS_INTEGER_USIZE_EXPECT);
        let val = f64::from(strx.as_slice()[position]);

        Ok(ECMAScriptValue::from(val))
    }
}

// 22.1.3.4 String.prototype.codePointAt ( pos )
fn string_prototype_code_point_at(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let pos = args.next_arg();

    // `codePointAt` is intentionally generic, so the receiver only needs to be
    // coercible and string-convertible; it does not need to be a String object.
    this_value.require_object_coercible()?;

    // Convert the receiver before `pos`, preserving the spec's observable
    // coercion order if either conversion has side effects or throws.
    let strx = to_string(this_value.clone())?;

    // JavaScript indexing semantics accept fractions, NaN, and infinities here;
    // normalize them before checking the string bounds.
    let position = pos.to_integer_or_infinity()?;

    // String positions are UTF-16 code unit indexes, not Unicode scalar indexes.
    let size = to_f64(strx.len()).expect(JS_INTEGER_F64_EXPECT);

    // Out-of-range positions do not throw; they produce `undefined`.
    if position < 0.0 || position >= size {
        Ok(ECMAScriptValue::Undefined)
    } else {
        // Safe after the bounds check above, and still a code-unit index.
        let position = to_usize(position).expect(JS_INTEGER_USIZE_EXPECT);

        // `CodePointAt` combines a leading surrogate with a following trailing
        // surrogate when present; otherwise it returns the single code unit.
        let cp = code_point_at(&strx, position);

        Ok(ECMAScriptValue::from(cp.code_point))
    }
}

// 22.1.3.5 String.prototype.concat ( ...args )
fn string_prototype_concat(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);

    // `concat` is intentionally generic: the receiver only needs to be
    // coercible and string-convertible, not an actual String object.
    this_value.require_object_coercible()?;

    // The receiver is converted first, so any side effects or abrupt completion
    // from `this.toString` happen before argument conversions.
    let strx = to_string(this_value.clone())?;

    // The result is always a primitive String value, even when the receiver is a
    // String object or another object with custom string conversion.
    let mut result = strx;

    for next in args.remaining().cloned() {
        // Convert and append one argument at a time. This preserves observable
        // ToString ordering and stops immediately if any conversion throws.
        let next_string = to_string(next)?;
        result = result.concat(next_string);
    }

    Ok(ECMAScriptValue::String(result))
}

// 22.1.3.7 String.prototype.endsWith ( searchString [ , endPosition ] )
fn string_prototype_ends_with(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let search_string = args.next_arg();
    let end_position = args.next_arg();

    // `endsWith` is intentionally generic, so `this` only needs to be
    // coercible and string-convertible; it does not need to already be a String.
    this_value.require_object_coercible()?;
    let strx = to_string(this_value.clone())?;

    // RegExp search values are explicitly rejected so future spec extensions
    // can define RegExp-specific behavior without being web-incompatible.
    let is_reg_exp = search_string.is_regexp()?;
    if is_reg_exp {
        return Err(create_type_error("String.prototype.endsWith: searchString must not be a RegExp"));
    }

    // Convert after the RegExp check: Symbols may throw here, and RegExps
    // should throw the RegExp-specific TypeError above instead.
    let search_str = to_string(search_string)?;

    // String lengths in this engine are indexed by code units, matching the
    // ECMAScript String algorithms rather than Unicode scalar values.
    let len = to_f64(strx.len()).expect(JS_INTEGER_F64_EXPECT);

    // A missing end position means "use the whole string"; otherwise normalize
    // the caller-provided limit using JS integer conversion semantics.
    let pos = if end_position.is_undefined() { len } else { end_position.to_integer_or_infinity()? };

    // Clamp the effective end index into the valid string range before turning
    // it back into a Rust index.
    let end = to_usize(pos.clamp(0.0, len)).expect(JS_INTEGER_USIZE_EXPECT);

    let search_length = search_str.len();

    // Every string ends with the empty string, regardless of end position.
    if search_length == 0 {
        return Ok(ECMAScriptValue::Boolean(true));
    }

    // If the search string would have to start before index 0, it cannot match.
    // Checking this before subtraction also avoids unsigned underflow.
    if search_length > end {
        return Ok(ECMAScriptValue::Boolean(false));
    }

    let start = end - search_length;

    // Compare only the suffix ending at the caller-selected end index.
    let substring = &strx.as_slice()[start..end];
    Ok(ECMAScriptValue::Boolean(substring == search_str.as_slice()))
}

// 22.1.3.8 String.prototype.includes ( searchString [ , position ] )
fn string_prototype_includes(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let search_string = args.next_arg();
    let position = args.next_arg();

    // `includes` is intentionally generic, so the receiver only needs to be
    // coercible and string-convertible; it does not need to be a String object.
    this_value.require_object_coercible()?;

    // Convert the receiver before checking or converting the search value,
    // preserving the spec's observable coercion order.
    let strx = to_string(this_value.clone())?;

    // RegExp search values are explicitly rejected so future spec extensions
    // can define RegExp-specific behavior without being web-incompatible.
    let is_regexp = search_string.is_regexp()?;
    if is_regexp {
        return Err(create_type_error("String.prototype.includes: searchString must not be a RegExp"));
    }

    // Convert after the RegExp check: RegExps should throw the RegExp-specific
    // TypeError above rather than being stringified.
    let search_str = to_string(search_string)?;

    // A missing `position` becomes 0 through ToIntegerOrInfinity. Other JS
    // number-ish values, including NaN and infinities, are normalized here too.
    let pos = position.to_integer_or_infinity()?;

    // String positions are UTF-16 code unit indexes, not Unicode scalar indexes.
    let len = to_f64(strx.len()).expect(JS_INTEGER_F64_EXPECT);

    // Clamp the starting index into the valid string range before converting it
    // back into a Rust index.
    let start = to_usize(pos.clamp(0.0, len)).expect(JS_INTEGER_USIZE_EXPECT);

    // Search only at or after the caller-selected start position.
    let index = strx.index_of(&search_str, start);

    Ok(ECMAScriptValue::Boolean(index.is_some()))
}

// 22.1.3.9 String.prototype.indexOf ( searchString [ , position ] )
fn string_prototype_index_of(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let search_string = args.next_arg();
    let position = args.next_arg();

    // `indexOf` is intentionally generic, so the receiver only needs to be
    // coercible and string-convertible; it does not need to be a String object.
    require_object_coercible(this_value)?;

    // Convert the receiver before the search string, preserving the spec's
    // observable coercion order if either conversion has side effects or throws.
    let s = to_string(this_value.clone())?;

    // Unlike `includes`, `indexOf` does not reject RegExp search values; they
    // are converted with ordinary ToString semantics.
    let search_str = to_string(search_string)?;

    // A missing `position` becomes 0 through ToIntegerOrInfinity. Other JS
    // number-ish values, including NaN and infinities, are normalized here too.
    let pos = position.to_integer_or_infinity()?;

    // String positions are UTF-16 code unit indexes, not Unicode scalar indexes.
    let len = s.len();
    let max = to_f64(len).expect(JS_INTEGER_F64_EXPECT);

    // Clamp the starting index into the valid string range before converting it
    // back into a Rust index.
    let start = to_usize(pos.clamp(0.0, max)).expect(JS_INTEGER_USIZE_EXPECT);

    // Return the first match at or after `start`, or -1 when no match exists.
    Ok(s.index_of(&search_str, start).map(|index| to_f64(index).expect(JS_INTEGER_F64_EXPECT)).unwrap_or(-1.0).into())
}

fn string_prototype_is_well_formed(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // `isWellFormed` is intentionally generic, so the receiver only needs to be
    // coercible and string-convertible; it does not need to be a String object.
    this_value.require_object_coercible()?;

    // The check is performed on the string-converted receiver, preserving any
    // observable side effects or errors from ToString.
    let strx = to_string(this_value.clone())?;

    // Well-formed Unicode strings must not contain unpaired UTF-16 surrogates.
    Ok(ECMAScriptValue::Boolean(strx.is_well_formed_unicode()))
}

// 22.1.3.10 String.prototype.lastIndexOf ( searchString [ , position ] )
fn string_prototype_last_index_of(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let search_string = args.next_arg();
    let position = args.next_arg();

    // `lastIndexOf` is intentionally generic, so the receiver only needs to be
    // coercible and string-convertible; it does not need to be a String object.
    this_value.require_object_coercible()?;

    // Convert the receiver before the search string, preserving the spec's
    // observable coercion order if either conversion has side effects or throws.
    let strx = to_string(this_value.clone())?;

    // `lastIndexOf`, like `indexOf`, does not reject RegExp search values; they
    // are converted with ordinary ToString semantics.
    let search_str = to_string(search_string)?;

    // `lastIndexOf` treats an omitted position differently from `indexOf`: an
    // omitted value becomes NaN first, then NaN is interpreted as +Infinity so
    // the search starts as far right as possible.
    let num_pos = position.to_number()?;
    let pos = if num_pos.is_nan() { f64::INFINITY } else { to_integer_or_infinity(num_pos) };

    // String positions are UTF-16 code unit indexes, not Unicode scalar indexes.
    let len = to_f64(strx.len()).expect(JS_INTEGER_F64_EXPECT);
    let search_len = to_f64(search_str.len()).expect(JS_INTEGER_F64_EXPECT);

    // If the search string is longer than the target, there is no valid starting
    // index where it can fit.
    if len < search_len {
        return Ok(ECMAScriptValue::Number(-1.0));
    }

    // Clamp to the greatest starting index where `searchStr` can still fit
    // entirely inside `strx`.
    let start = to_usize(pos.clamp(0.0, len - search_len)).expect(JS_INTEGER_USIZE_EXPECT);

    // Search backward from `start`; report -1 for the spec's not-found result.
    match strx.last_index_of(&search_str, start) {
        None => Ok(ECMAScriptValue::Number(-1.0)),
        Some(result) => Ok(ECMAScriptValue::from(result)),
    }
}

// 22.1.3.11 String.prototype.localeCompare ( that [ , reserved1 [ , reserved2 ] ] )
fn string_prototype_locale_compare(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let that = args.next_arg();

    // Without ECMA-402, locale-sensitive collation is implementation-defined.
    // Still preserve the required receiver coercion behavior.
    this_value.require_object_coercible()?;

    // Convert `this` before `that`, preserving observable ToString ordering.
    let s = to_string(this_value.clone())?;
    let that = to_string(that)?;

    // `locales` and `options` are only meaningful for ECMA-402. In a non-402
    // engine, extra arguments are intentionally ignored rather than validated.
    //
    // Use a stable UTF-16 code-unit ordering as the implementation-defined
    // fallback. Callers may only rely on the sign, not the exact magnitude.
    let result = match s.as_slice().cmp(that.as_slice()) {
        std::cmp::Ordering::Less => -1.0,
        std::cmp::Ordering::Equal => 0.0,
        std::cmp::Ordering::Greater => 1.0,
    };

    Ok(ECMAScriptValue::Number(result))
}

// 22.1.3.12 String.prototype.match ( regexp )
fn string_prototype_match(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let regexp_or_pattern = args.next_arg();

    let match_key = PropertyKey::from(wks(WksId::Match));

    // Validate the receiver first, then give objects with @@match a chance to
    // handle the original receiver value before any string conversion happens.
    this_value.require_object_coercible()?;

    if let ECMAScriptValue::Object(obj) = &regexp_or_pattern {
        // Objects can override string matching with @@match. When present, that
        // method completely handles the operation and controls any coercion.
        let matcher = obj.get_method(&match_key)?;
        if !matcher.is_undefined() {
            return call(&matcher, &regexp_or_pattern, slice::from_ref(this_value));
        }
    }

    // No custom matcher was provided, so use the built-in RegExp path.
    let strx = to_string(this_value.clone())?;

    // Non-RegExp values are compiled as patterns; existing RegExp objects are
    // handled by RegExpCreate according to the constructor rules.
    let regexp = ECMAScriptValue::from(reg_exp_create(regexp_or_pattern, None)?);

    // Delegate the actual matching semantics to RegExp.prototype[@@match].
    regexp.invoke(&match_key, &[ECMAScriptValue::String(strx)])
}

// 22.1.3.13 String.prototype.matchAll ( regexp )
fn string_prototype_match_all(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let regexp_or_pattern = args.next_arg();

    let key = PropertyKey::from(wks(WksId::MatchAll));

    // Validate the receiver first, then give objects with @@matchAll a chance
    // to handle the original receiver value before any string conversion happens.
    this_value.require_object_coercible()?;

    if let ECMAScriptValue::Object(obj) = &regexp_or_pattern {
        // RegExp inputs must be global. Use IsRegExp here so an object can opt
        // into or out of RegExp treatment with @@match.
        let is_reg_exp = obj.is_reg_exp()?;
        if is_reg_exp {
            // The global check is based on the observable `flags` property, not
            // directly on an internal RegExp slot.
            let flags = obj.get(&"flags".into())?;
            flags.require_object_coercible()?;

            // Convert flags after the object-coercible check so null/undefined
            // produce the required TypeError before ToString.
            let flags = to_string(flags)?;
            if !flags.contains('g' as u16) {
                return Err(create_type_error("String.prototype.matchAll: regexp must have the global flag set"));
            }
        }

        // Objects can override match-all behavior with @@matchAll. When present,
        // that method handles the operation and controls any receiver coercion.
        let matcher = obj.get_method(&key)?;
        if !matcher.is_undefined() {
            return call(&matcher, &regexp_or_pattern, slice::from_ref(this_value));
        }
    }

    // No custom matcher was provided, so use the built-in RegExp path. Convert
    // the receiver only after the @@matchAll lookup to preserve observable order.
    let strx = to_string(this_value.clone())?;

    // Non-RegExp values are compiled as global patterns for matchAll's iterator
    // semantics.
    let regexp = ECMAScriptValue::from(reg_exp_create(regexp_or_pattern, Some(JSString::from("g")))?);

    // Delegate the iterator creation and matching semantics to
    // RegExp.prototype[@@matchAll].
    regexp.invoke(&key, &[strx.into()])
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NormalizationForm {
    Nfc,
    Nfd,
    Nfkc,
    Nfkd,
}

impl TryFrom<ECMAScriptValue> for NormalizationForm {
    type Error = AbruptCompletion;

    fn try_from(form: ECMAScriptValue) -> Result<Self, Self::Error> {
        const NFC: &[u16] = &[b'N' as u16, b'F' as u16, b'C' as u16];
        const NFD: &[u16] = &[b'N' as u16, b'F' as u16, b'D' as u16];
        const NFKC: &[u16] = &[b'N' as u16, b'F' as u16, b'K' as u16, b'C' as u16];
        const NFKD: &[u16] = &[b'N' as u16, b'F' as u16, b'K' as u16, b'D' as u16];

        if form.is_undefined() {
            // The default normalization form is NFC. Return the enum directly
            // instead of allocating and parsing a temporary "NFC" JS string.
            Ok(Self::Nfc)
        } else {
            // Non-undefined forms use normal JS string conversion, so Symbols,
            // objects with side effects, and abrupt completions behave correctly.
            let form = to_string(form)?;

            // Normalization form names are ASCII, but JSString is stored as
            // UTF-16 code units. Keep that representation detail contained at
            // this conversion boundary.
            match form.as_slice() {
                NFC => Ok(Self::Nfc),
                NFD => Ok(Self::Nfd),
                NFKC => Ok(Self::Nfkc),
                NFKD => Ok(Self::Nfkd),
                _ => Err(create_range_error("normalization form must be one of NFC, NFD, NFKC, or NFKD")),
            }
        }
    }
}

// 22.1.3.14 String.prototype.normalize ( [ form ] )
fn string_prototype_normalize(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let form = args.next_arg();

    // `normalize` is intentionally generic, so the receiver only needs to be
    // coercible and string-convertible; it does not need to be a String object.
    this_value.require_object_coercible()?;

    // Convert the receiver before the form argument, preserving the spec's
    // observable coercion order if either conversion has side effects or throws.
    let strx = to_string(this_value.clone())?;

    // Convert the optional JS form argument into a Rust enum once. The rest of
    // the implementation can switch on the normalization operation directly
    // instead of repeatedly comparing UTF-16 form-name strings.
    let form = NormalizationForm::try_from(form)?;

    // ECMAScript strings are UTF-16 and may contain lone surrogates. Normalize
    // the well-formed Unicode portions, while preserving lone surrogate code
    // units unchanged.
    Ok(ECMAScriptValue::String(normalize_js_string_preserving_surrogates(&strx, form)))
}

fn normalize_js_string_preserving_surrogates(strx: &JSString, form: NormalizationForm) -> JSString {
    let units = strx.as_slice();
    let mut out = Vec::with_capacity(units.len());
    let mut buf = String::new();

    let mut i = 0;
    while i < units.len() {
        let u = units[i];

        if (0xD800..=0xDBFF).contains(&u) {
            if let Some(&v) = units.get(i + 1)
                && (0xDC00..=0xDFFF).contains(&v)
            {
                // A leading surrogate followed by a trailing surrogate is a valid UTF-16 encoding of one Unicode scalar
                // value. Decode it before passing text to the normalization library.
                let high = u32::from(u) - 0xD800;
                let low = u32::from(v) - 0xDC00;
                let cp = 0x10000 + ((high << 10) | low);
                let ch = char::from_u32(cp).expect("decoded surrogate pair should be a valid Unicode scalar value");
                buf.push(ch);
                i += 2;
                continue;
            }

            // Lone leading surrogates are not Unicode scalar values, and Rust `String` cannot represent them. Flush any
            // pending valid text, then preserve the surrogate code unit unchanged.
            flush_normalized(&mut out, &mut buf, form);
            out.push(u);
            i += 1;
        } else if (0xDC00..=0xDFFF).contains(&u) {
            // A trailing surrogate without a matching leading surrogate is also preserved unchanged for ECMAScript
            // string compatibility.
            flush_normalized(&mut out, &mut buf, form);
            out.push(u);
            i += 1;
        } else {
            // Non-surrogate BMP code units are valid Unicode scalar values and can be accumulated for normalization.
            let ch = char::from_u32(u32::from(u)).expect("non-surrogate UTF-16 code unit should be a char");

            buf.push(ch);
            i += 1;
        }
    }

    // Normalize and append the final run of well-formed Unicode text, if any.
    flush_normalized(&mut out, &mut buf, form);

    JSString::from(out.as_slice())
}

fn flush_normalized(out: &mut Vec<u16>, buf: &mut String, form: NormalizationForm) {
    if buf.is_empty() {
        return;
    }

    // The Unicode normalization library works on Rust `str`, so callers only
    // pass runs that contain valid Unicode scalar values.
    let normalized = match form {
        NormalizationForm::Nfc => buf.nfc().collect::<String>(),
        NormalizationForm::Nfd => buf.nfd().collect::<String>(),
        NormalizationForm::Nfkc => buf.nfkc().collect::<String>(),
        NormalizationForm::Nfkd => buf.nfkd().collect::<String>(),
    };

    // Convert the normalized scalar-value text back into the engine's UTF-16
    // representation and clear the buffer for the next well-formed run.
    out.extend(normalized.encode_utf16());
    buf.clear();
}

// 22.1.3.15 String.prototype.padEnd ( maxLength [ , fillString ] )
fn string_prototype_pad_end(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let max_length = args.next_arg();
    let fill_string = args.next_arg();

    // `padEnd` is intentionally generic, so the receiver only needs to be coercible and string-convertible; it does not
    // need to be a String object.
    this_value.require_object_coercible()?;

    // Share the padding algorithm with `padStart`; this method only chooses where the generated fill text is placed.
    string_padding_builtins_impl(this_value, &max_length, fill_string, PadPlacement::End)
}

/// Implements the shared padding behavior for `padStart` and `padEnd`.
///
/// The receiver is converted to a string first, then `max_length` is normalized with `ToLength`. If the requested
/// length is not greater than the string's current UTF-16 code unit length, the original string is returned unchanged.
/// Otherwise, `fill_string` is stringified, repeated, truncated, and placed according to `placement`.
///
/// # Errors
///
/// Returns an abrupt completion if converting `this_value` to a string, normalizing `max_length`, or stringifying
/// `fill_string` fails.
///
/// # Panics
///
/// Panics only if the engine's representation invariant is broken: a valid ECMAScript length should always fit in
/// `usize`.
fn string_padding_builtins_impl(
    this_value: &ECMAScriptValue,
    max_length: &ECMAScriptValue,
    fill_string: ECMAScriptValue,
    placement: PadPlacement,
) -> Completion<ECMAScriptValue> {
    // Convert the receiver before the length and fill arguments, preserving the
    // spec's observable coercion order.
    let strx = to_string(this_value.clone())?;

    // ToLength clamps JS length-like values into the valid array/string length
    // range before converting to a Rust index type.
    let int_max_length = to_usize(max_length.to_length()?).expect(JS_INTEGER_USIZE_EXPECT);

    // Padding is a no-op when the requested length is not greater than the
    // current UTF-16 code unit length.
    let string_length = strx.len();
    if int_max_length <= string_length {
        return Ok(strx.into());
    }

    // The default fill is a single ASCII space. A provided fill value is
    // stringified only when padding is actually needed, preserving coercion
    // ordering and avoiding unnecessary side effects.
    let fill_string = if fill_string.is_undefined() { JSString::from(" ") } else { to_string(fill_string)? };

    // `StringPad` handles repeating and truncating the fill string, then places
    // it on the requested side of the original string.
    Ok(strx.string_pad(int_max_length, &fill_string, placement).into())
}

// 22.1.3.16 String.prototype.padStart ( maxLength [ , fillString ] )
fn string_prototype_pad_start(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // String.prototype.padStart ( maxLength [ , fillString ] )
    let mut args = FuncArgs::from(arguments);
    let max_length = args.next_arg();
    let fill_string = args.next_arg();

    // `padStart` is intentionally generic, so the receiver only needs to be coercible and string-convertible; it does
    // not need to be a String object.
    this_value.require_object_coercible()?;

    // Share the padding algorithm with `padEnd`; this method only chooses where the generated fill text is placed.
    string_padding_builtins_impl(this_value, &max_length, fill_string, PadPlacement::Start)
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub(crate) enum PadPlacement {
    Start,
    End,
}

impl JSString {
    /// Pads the string to `max_length` UTF-16 code units.
    ///
    /// Repeats `fill_string` as needed, truncates the repeated filler to the exact required length, and places it
    /// before or after the original string according to `placement`. Returns the original string unchanged when it is
    /// already long enough or when `fill_string` is empty.
    #[must_use]
    pub(crate) fn string_pad(&self, max_length: usize, fill_string: &JSString, placement: PadPlacement) -> JSString {
        let string_length = self.len();

        // Padding is a no-op when the string is already long enough, or when
        // the caller supplied an empty fill string.
        if max_length <= string_length || fill_string.is_empty() {
            return self.clone();
        }

        let fill_len = max_length - string_length;
        let source = self.as_slice();
        let filler = fill_string.as_slice();

        // Allocate the final output once. The filler is copied in repeated
        // chunks, with the last chunk truncated to the exact remaining length.
        let mut out = Vec::with_capacity(max_length);

        if placement == PadPlacement::Start {
            append_repeated_prefix(&mut out, filler, fill_len);
            out.extend_from_slice(source);
        } else {
            out.extend_from_slice(source);
            append_repeated_prefix(&mut out, filler, fill_len);
        }

        JSString::from(out.as_slice())
    }
}

fn append_repeated_prefix(out: &mut Vec<u16>, filler: &[u16], len: usize) {
    if filler.is_empty() || len == 0 {
        return;
    }

    let full_repeats = len / filler.len();
    let remainder = len % filler.len();

    for _ in 0..full_repeats {
        out.extend_from_slice(filler);
    }

    out.extend_from_slice(&filler[..remainder]);
}

pub(crate) fn to_zero_padded_decimal_string(n: usize, min_length: usize) -> JSString {
    // ToZeroPaddedDecimalString ( n, minLength )
    // The abstract operation ToZeroPaddedDecimalString takes arguments n (a non-negative integer) and minLength (a
    // non-negative integer) and returns a String. It performs the following steps when called:
    //
    // 1. Let S be the String representation of n, formatted as a decimal number.
    // 2. Return StringPad(S, minLength, "0", start).
    let s = JSString::from(format!("{n}"));
    s.string_pad(min_length, &JSString::from("0"), PadPlacement::Start)
}

/// Implements `String.prototype.repeat`.
///
/// Converts the receiver to a string, normalizes `count` with `ToIntegerOrInfinity`, and returns a new string
/// containing `count` copies of the receiver's UTF-16 code units.
///
/// # Errors
///
/// Returns a `RangeError` abrupt completion when `count` is negative or positive infinity. Also returns an abrupt
/// completion if the receiver cannot be stringified or if converting `count` fails.
///
/// # Panics
///
/// Panics only if the engine's representation invariant is broken: a valid ECMAScript integer index should fit in
/// `usize`.
fn string_prototype_repeat(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let count = args.next_arg();

    // `repeat` is intentionally generic, so the receiver only needs to be coercible and string-convertible; it does not
    // need to be a String object.
    this_value.require_object_coercible()?;

    // Convert the receiver before the repeat count, preserving the spec's observable coercion order if either
    // conversion has side effects or throws.
    let strx = to_string(this_value.clone())?;

    // Normalize JS numeric values before validating the repeat count. NaN becomes 0 here, while negative values and
    // +Infinity are rejected below.
    let n = count.to_integer_or_infinity()?;

    if n < 0.0 || n == f64::INFINITY {
        // Negative repeat counts and infinite output sizes are RangeErrors.
        Err(create_range_error("Invalid count value"))
    } else if n == 0.0 {
        // Repeating zero times always returns the empty string, even when the receiver itself is non-empty.
        Ok(JSString::from("").into())
    } else {
        // Safe after validation above. Repeat by UTF-16 code units, preserving the original code-unit sequence exactly.
        let n = to_usize(n).expect(JS_INTEGER_USIZE_EXPECT);

        // Allocate the final buffer once rather than repeatedly concatenating intermediate strings.
        let mut result = Vec::with_capacity(strx.len() * n);
        for _ in 0..n {
            result.extend_from_slice(strx.as_slice());
        }

        Ok(JSString::from(result).into())
    }
}

// 22.1.3.18 String.prototype.replace ( searchValue, replaceValue )
fn string_prototype_replace(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let search_value = args.next_arg();
    let replace_value = args.next_arg();

    // `replace` is intentionally generic, so the receiver only needs to be
    // coercible. A custom @@replace method receives the original receiver value,
    // before any string conversion happens.
    require_object_coercible(this_value)?;

    if !search_value.is_null() && !search_value.is_undefined() {
        // Objects can override replacement behavior with @@replace. When
        // present, that method completely handles searching, replacement, and
        // receiver coercion.
        let replacer = search_value.get_method(&PropertyKey::from(wks(WksId::Replace)))?;
        if !replacer.is_undefined() {
            return call(&replacer, &search_value, &[this_value.clone(), replace_value]);
        }
    }

    // No custom replacer was provided, so use the built-in string-search path.
    // Convert the receiver before the search value, preserving observable
    // coercion order.
    let string = to_string(this_value.clone())?;
    let search_string = to_string(search_value)?;

    // Functional replacers are called later with the matched text, index, and
    // full string. Non-callable replacers are stringified before searching.
    let functional_replace = is_callable(&replace_value);
    let replace_string = if functional_replace { JSString::from("") } else { to_string(replace_value.clone())? };

    let search_length = search_string.len();

    // Plain string replacement uses the first occurrence, and returns the original
    // string unchanged when there is no match.
    let Some(position) = string.index_of(&search_string, 0) else {
        return Ok(ECMAScriptValue::String(string));
    };

    let preceding = &string.as_slice()[0..position];
    let following = &string.as_slice()[position + search_length..];

    let replacement = if functional_replace {
        // The function replacer receives no captures or named groups in the
        // plain string-search path: just match, position, and full string.
        to_string(call(
            &replace_value,
            &ECMAScriptValue::Undefined,
            &[
                ECMAScriptValue::String(search_string),
                ECMAScriptValue::from(position),
                ECMAScriptValue::String(string.clone()),
            ],
        )?)?
    } else {
        // Template replacers still process replacement patterns like "$&",
        // "$`", "$'", and "$$"; there are no captures for plain string search.
        get_substitution(&search_string, &string, position, &[], None, &replace_string)?
    };

    Ok(ECMAScriptValue::String(JSString::from(preceding).concat(replacement).concat(JSString::from(following))))
}

// 22.1.3.19 String.prototype.replaceAll ( searchValue, replaceValue )
fn string_prototype_replace_all(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.20 String.prototype.search ( regexp )
fn string_prototype_search(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.21 String.prototype.slice ( start, end )
fn string_prototype_slice(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // String.prototype.slice ( start, end )
    //
    // This method returns a substring of the result of converting this object to a String, starting from index start
    // and running to, but not including, index end (or through the end of the String if end is undefined). If start is
    // negative, it is treated as sourceLength + start where sourceLength is the length of the String. If end is
    // negative, it is treated as sourceLength + end where sourceLength is the length of the String. The result is a
    // String value, not a String object.
    //
    // It performs the following steps when called:
    //
    // 1. Let O be the this value.
    // 2. Perform ? RequireObjectCoercible(O).
    // 3. Let S be ? ToString(O).
    // 4. Let len be the length of S.
    // 5. Let intStart be ? ToIntegerOrInfinity(start).
    // 6. If intStart = -∞, let from be 0.
    // 7. Else if intStart < 0, let from be max(len + intStart, 0).
    // 8. Else, let from be min(intStart, len).
    // 9. If end is undefined, let intEnd be len; else let intEnd be ? ToIntegerOrInfinity(end).
    // 10. If intEnd = -∞, let to be 0.
    // 11. Else if intEnd < 0, let to be max(len + intEnd, 0).
    // 12. Else, let to be min(intEnd, len).
    // 13. If from ≥ to, return the empty String.
    // 14. Return the substring of S from from to to.
    //
    // Note: This method is intentionally generic; it does not require that its this value be a String object. Therefore
    // it can be transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let start = args.next_arg();
    let end = args.next_arg();

    let obj = this_value;
    obj.require_object_coercible()?;
    let string = to_string(obj.clone())?;
    let len = to_f64(string.len()).expect("f64s should be able to hold string lengths");

    let int_start = start.to_integer_or_infinity()?;
    let from = to_usize(if int_start == f64::NEG_INFINITY {
        0.0
    } else if int_start < 0.0 {
        (int_start + len).max(0.0)
    } else {
        int_start.min(len)
    })
    .expect(JS_INTEGER_USIZE_EXPECT);

    let int_end = if end.is_undefined() { len } else { end.to_integer_or_infinity()? };
    let to = to_usize(if int_end == f64::NEG_INFINITY {
        0.0
    } else if int_end < 0.0 {
        (len + int_end).max(0.0)
    } else {
        int_end.min(len)
    })
    .expect(JS_INTEGER_USIZE_EXPECT);

    if from >= to {
        Ok(ECMAScriptValue::from(""))
    } else {
        let buf = &string.as_slice()[from..to];
        Ok(ECMAScriptValue::String(JSString::from(buf)))
    }
}

// 22.1.3.22 String.prototype.split ( separator, limit )
#[expect(clippy::many_single_char_names)]
fn string_prototype_split(
    this: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // String.prototype.split ( separator, limit )
    // This method returns an Array into which substrings of the result of converting this object to a String
    // have been stored. The substrings are determined by searching from left to right for occurrences of
    // separator; these occurrences are not part of any String in the returned array, but serve to divide up
    // the String value. The value of separator may be a String of any length or it may be an object, such as
    // a RegExp, that has a %Symbol.split% method.
    //
    // It performs the following steps when called:
    //
    //  1. Let O be ? RequireObjectCoercible(this value).
    //  2. If separator is neither undefined nor null, then
    //      a. Let splitter be ? GetMethod(separator, %Symbol.split%).
    //      b. If splitter is not undefined, then
    //          i. Return ? Call(splitter, separator, « O, limit »).
    //  3. Let S be ? ToString(O).
    //  4. If limit is undefined, let lim be 2**32 - 1; else let lim be ℝ(? ToUint32(limit)).
    //  5. Let R be ? ToString(separator).
    //  6. If lim = 0, then
    //      a. Return CreateArrayFromList(« »).
    //  7. If separator is undefined, then
    //      a. Return CreateArrayFromList(« S »).
    //  8. Let separatorLength be the length of R.
    //  9. If separatorLength = 0, then
    //      a. Let strLen be the length of S.
    //      b. Let outLen be the result of clamping lim between 0 and strLen.
    //      c. Let head be the substring of S from 0 to outLen.
    //      d. Let codeUnits be a List consisting of the sequence of code units that are the elements of head.
    //      e. Return CreateArrayFromList(codeUnits).
    //  10. If S is the empty String, return CreateArrayFromList(« S »).
    //  11. Let substrings be a new empty List.
    //  12. Let i be 0.
    //  13. Let j be StringIndexOf(S, R, 0).
    //  14. Repeat, while j is not not-found,
    //      a. Let T be the substring of S from i to j.
    //      b. Append T to substrings.
    //      c. If the number of elements in substrings is lim, return CreateArrayFromList(substrings).
    //      d. Set i to j + separatorLength.
    //      e. Set j to StringIndexOf(S, R, i).
    //  15. Let T be the substring of S from i.
    //  16. Append T to substrings.
    //  17. Return CreateArrayFromList(substrings).
    //
    // Note 1 | The value of separator may be an empty String. In this case, separator does not match the
    //        | empty substring at the beginning or end of the input String, nor does it match the empty
    //        | substring at the end of the previous separator match. If separator is the empty String, the
    //        | String is split up into individual code unit elements; the length of the result array equals
    //        | the length of the String, and each substring contains one code unit.
    //        |
    //        | If the this value is (or converts to) the empty String, the result depends on whether
    //        | separator can match the empty String. If it can, the result array contains no elements.
    //        | Otherwise, the result array contains one element, which is the empty String.
    //        |
    //        | If separator is undefined, then the result array contains just one String, which is the this
    //        | value (converted to a String). If limit is not undefined, then the output array is truncated
    //        | so that it contains no more than limit elements.
    //
    // Note 2 | This method is intentionally generic; it does not require that its this value be a String
    //        | object. Therefore, it can be transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let separator = args.next_arg();
    let limit = args.next_arg();
    require_object_coercible(this)?;
    let o = this.clone();
    if ![ECMAScriptValue::Undefined, ECMAScriptValue::Null].contains(&separator) {
        let splitter = separator.get_method(&PropertyKey::from(wks(WksId::Split)))?;
        if splitter != ECMAScriptValue::Undefined {
            return call(&splitter, &separator, &[o, limit]);
        }
    }
    let s = to_string(o)?;
    let lim = if limit == ECMAScriptValue::Undefined { 4_294_967_295 } else { limit.to_uint32()? };
    let r = to_string(separator.clone())?;
    if lim == 0 {
        return Ok(ECMAScriptValue::Object(create_array_from_list(&[])));
    }
    if separator == ECMAScriptValue::Undefined {
        return Ok(ECMAScriptValue::Object(create_array_from_list(&[ECMAScriptValue::String(s)])));
    }
    let separator_length = r.len();
    if separator_length == 0 {
        let str_len = u32::try_from(s.len()).unwrap_or(u32::MAX);
        let out_len = lim.clamp(0, str_len);
        let head = s.as_slice()[0..out_len as usize]
            .iter()
            .map(|num| ECMAScriptValue::from(JSString::from(&[*num] as &[u16])))
            .collect::<Vec<_>>();
        return Ok(ECMAScriptValue::Object(create_array_from_list(&head)));
    }

    if s.is_empty() {
        return Ok(ECMAScriptValue::Object(create_array_from_list(&[ECMAScriptValue::from(s)])));
    }

    let mut substrings = vec![];
    let mut i = 0;
    let mut maybe_j = s.string_index_of(&r, 0);
    while let Some(j) = maybe_j {
        let t = ECMAScriptValue::String(JSString::from(&s.as_slice()[i..j]));
        substrings.push(t);
        if substrings.len() == lim as usize {
            return Ok(ECMAScriptValue::Object(create_array_from_list(&substrings)));
        }
        i = j + separator_length;
        maybe_j = s.string_index_of(&r, i);
    }
    let t = ECMAScriptValue::String(JSString::from(&s.as_slice()[i..]));
    substrings.push(t);
    Ok(ECMAScriptValue::Object(create_array_from_list(&substrings)))
}

// 22.1.3.23 String.prototype.startsWith ( searchString [ , position ] )
fn string_prototype_starts_with(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.24 String.prototype.substring ( start, end )
fn string_prototype_substring(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.25 String.prototype.toLocaleLowerCase ( [ reserved1 [ , reserved2 ] ] )
fn string_prototype_to_locale_lower_case(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.26 String.prototype.toLocaleUpperCase ( [ reserved1 [ , reserved2 ] ] )
fn string_prototype_to_locale_upper_case(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.27 String.prototype.toLowerCase ( )
fn string_prototype_to_lower_case(
    this: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // String.prototype.toLowerCase ( )
    // This method interprets a String value as a sequence of UTF-16 encoded code points, as described in 6.1.4.
    //
    // It performs the following steps when called:
    //
    //  1. Let O be ? RequireObjectCoercible(this value).
    //  2. Let S be ? ToString(O).
    //  3. Let sText be StringToCodePoints(S).
    //  4. Let lowerText be toLowercase(sText), according to the Unicode Default Case Conversion algorithm.
    //  5. Let L be CodePointsToString(lowerText).
    //  6. Return L.
    //
    // The result must be derived according to the locale-insensitive case mappings in the Unicode Character Database
    // (this explicitly includes not only the file UnicodeData.txt, but also all locale-insensitive mappings in the file
    // SpecialCasing.txt that accompanies it).
    //
    // Note 1 The case mapping of some code points may produce multiple code points. In this case the result String may
    // not be the same length as the source String. Because both toUpperCase and toLowerCase have context-sensitive
    // behaviour, the methods are not symmetrical. In other words, s.toUpperCase().toLowerCase() is not necessarily
    // equal to s.toLowerCase().
    //
    // Note 2 This method is intentionally generic; it does not require that its this value be a String object.
    // Therefore, it can be transferred to other kinds of objects for use as a method.
    require_object_coercible(this)?;
    let s = to_string(this.clone())?;
    let stext = s.to_code_points();

    let mut result = vec![];
    for c in stext {
        match char::try_from(c) {
            Ok(ch) => {
                let chars = ch.to_lowercase().collect::<Vec<_>>();
                let mut buf = [0; 2];
                for c in chars {
                    let encoded = c.encode_utf16(&mut buf);
                    result.extend_from_slice(encoded);
                }
            }
            Err(_) => {
                let mut buf = [0; 2];
                let encoded = utf16_encode_code_point(c, &mut buf).expect("char points should be in range");
                result.extend_from_slice(encoded);
            }
        }
    }

    Ok(ECMAScriptValue::String(JSString::from(result)))
}

// 22.1.3.28 String.prototype.toString ( )
fn string_prototype_to_string(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // String.prototype.toString ( )
    // This method performs the following steps when called:
    //
    //  1. Return ? thisStringValue(this value).
    let s = this_string_value(this_value.clone(), "String.prototype.toString")?;
    Ok(s.into())
}
// 22.1.3.29 String.prototype.toUpperCase ( )
fn string_prototype_to_upper_case(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.30 String.prototype.trim ( )
fn string_prototype_trim(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

#[derive(Copy, Clone, PartialEq)]
pub(crate) enum TrimHint {
    Start,
    End,
    Both,
}

pub(crate) fn trim_string(string: ECMAScriptValue, hint: TrimHint) -> Completion<JSString> {
    // TrimString ( string, where )
    // The abstract operation TrimString takes arguments string (an ECMAScript language value) and where
    // (start, end, or start+end) and returns either a normal completion containing a String or a throw
    // completion. It interprets string as a sequence of UTF-16 encoded code points, as described in 6.1.4. It
    // performs the following steps when called:
    //
    // 1. Let str be ? RequireObjectCoercible(string).
    // 2. Let S be ? ToString(str).
    // 3. If where is start, then
    //    a. Let T be the String value that is a copy of S with leading white space removed.
    // 4. Else if where is end, then
    //    a. Let T be the String value that is a copy of S with trailing white space removed.
    // 5. Else,
    //    a. Assert: where is start+end.
    //    b. Let T be the String value that is a copy of S with both leading and trailing white space removed.
    // 6. Return T.
    //
    // The definition of white space is the union of WhiteSpace and LineTerminator. When determining whether a
    // Unicode code point is in Unicode general category “Space_Separator” (“Zs”), code unit sequences are
    // interpreted as UTF-16 encoded code point sequences as specified in 6.1.4.

    require_object_coercible(&string)?;
    let s = to_string(string)?;
    let mut start = 0;
    let mut final_idx = s.len();
    if hint == TrimHint::Start || hint == TrimHint::Both {
        while start < s.len() && is_str_whitespace(s[start]) {
            start += 1;
        }
    }
    if hint == TrimHint::End || hint == TrimHint::Both {
        while final_idx > 0 && is_str_whitespace(s[final_idx - 1]) {
            final_idx -= 1;
        }
    }
    Ok(JSString::from(&s.as_slice()[start..final_idx]))
}

// 22.1.3.31 String.prototype.trimEnd ( )
fn string_prototype_trim_end(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.32 String.prototype.trimStart ( )
fn string_prototype_trim_start(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.33 String.prototype.valueOf ( )
fn string_prototype_value_of(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // String.prototype.valueOf ( )
    // This method performs the following steps when called:
    //
    //  1. Return ? thisStringValue(this value).
    let s = this_string_value(this_value.clone(), "String.prototype.valueOf")?;
    Ok(s.into())
}

async fn string_iterator(
    co: Co<ECMAScriptValue, Completion<ECMAScriptValue>>,
    s: JSString,
) -> Completion<ECMAScriptValue> {
    //  1. Let len be the length of s.
    //  2. Let position be 0.
    //  3. Repeat, while position < len,
    //     a. Let cp be CodePointAt(s, position).
    //     b. Let nextIndex be position + cp.[[CodeUnitCount]].
    //     c. Let resultString be the substring of s from position to nextIndex.
    //     d. Set position to nextIndex.
    //     e. Perform ? GeneratorYield(CreateIteratorResultObject(resultString, false)).
    //  4. Return undefined.
    let len = s.len();
    let mut position = 0;
    while position < len {
        let cp = code_point_at(&s, position);
        let next_index = position + usize::from(cp.code_unit_count);
        let result_string = JSString::from(&s.as_slice()[position..next_index]);
        position = next_index;
        let res = ECMAScriptValue::from(create_iter_result_object(ECMAScriptValue::from(result_string), false));
        generator_yield(&co, res).await?;
    }
    Ok(ECMAScriptValue::Undefined)
}

fn string_prototype_iterator(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // String.prototype [ %Symbol.iterator% ] ( )
    // This method returns an Iterator object (27.1.1.2) that iterates over the code points of a String value,
    // returning each code point as a String value.
    //
    // It performs the following steps when called:
    //
    //  1. Let O be ? RequireObjectCoercible(this value).
    //  2. Let s be ? ToString(O).
    //  3. Let closure be a new Abstract Closure with no parameters that captures s and performs the following
    //     steps when called:
    //     a. Let len be the length of s.
    //     b. Let position be 0.
    //     c. Repeat, while position < len,
    //        i. Let cp be CodePointAt(s, position).
    //        ii. Let nextIndex be position + cp.[[CodeUnitCount]].
    //        iii. Let resultString be the substring of s from position to nextIndex.
    //        iv. Set position to nextIndex.
    //        v. Perform ? GeneratorYield(CreateIteratorResultObject(resultString, false)).
    //     d. Return undefined.
    //  4. Return CreateIteratorFromClosure(closure, "%StringIteratorPrototype%", %StringIteratorPrototype%).
    require_object_coercible(this_value)?;
    let s = to_string(this_value.clone())?;
    let closure = move |co| string_iterator(co, s);

    Ok(ECMAScriptValue::Object(create_iterator_from_closure(
        asyncfn_wrap(closure),
        "%StringIteratorPrototype%",
        Some(intrinsic(IntrinsicId::StringIteratorPrototype)),
    )))
}

pub(crate) fn provision_string_iterator_prototype(realm: &Rc<RefCell<Realm>>) {
    // The %StringIteratorPrototype% object:
    //
    //  * has properties that are inherited by all String Iterator Objects.
    //  * is an ordinary object.
    //  * has a [[Prototype]] internal slot whose value is %Iterator.prototype%.
    let string_iterator_prototype = ordinary_object_create(Some(realm.borrow().intrinsics.iterator_prototype.clone()));

    // %StringIteratorPrototype% [ %Symbol.toStringTag% ]
    // The initial value of the %Symbol.toStringTag% property is the String value "String Iterator".
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true }.
    define_property_or_throw(
        &string_iterator_prototype,
        wks(WksId::ToStringTag),
        PotentialPropertyDescriptor::new()
            .value("String Iterator")
            .writable(false)
            .enumerable(false)
            .configurable(true),
    )
    .expect("internal object");

    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();
    macro_rules! prototype_function {
        ( $steps:expr_2021, $name:expr_2021, $length:expr_2021 ) => {
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
                &string_iterator_prototype,
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

    prototype_function!(string_iterator_prototype_next, "next", 0);

    realm.borrow_mut().intrinsics.string_iterator_prototype = string_iterator_prototype;
}

fn string_iterator_prototype_next(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // %StringIteratorPrototype%.next ( )
    //  1. Return ? GeneratorResume(this value, empty, "%StringIteratorPrototype%").
    generator_resume(this_value, ECMAScriptValue::Undefined, "%StringIteratorPrototype%")
}

pub(crate) fn get_substitution(
    matched: &JSString,
    strx: &JSString,
    position: usize,
    captures: &[Option<JSString>],
    named_captures: Option<&Object>,
    replacement_template: &JSString,
) -> Completion<JSString> {
    // GetSubstitution ( matched, str, position, captures, namedCaptures, replacementTemplate )
    //
    // The abstract operation GetSubstitution takes arguments matched (a String), str (a String), position (a
    // non-negative integer), captures (a List of either Strings or undefined), namedCaptures (an Object or
    // undefined), and replacementTemplate (a String) and returns either a normal completion containing a String or
    // a throw completion. For the purposes of this abstract operation, a decimal digit is a code unit in the
    // inclusive interval from 0x0030 (DIGIT ZERO) to 0x0039 (DIGIT NINE). It performs the following steps when
    // called:
    //
    macro_rules! c {
              ($($ch:literal),* $(,)?) => {
                [
                    $(
                        const {
                            let ch = $ch as u32;
                            assert!(
                                ch <= u16::MAX as u32,
                                "c!(...) requires chars in the BMP (encodable as one UTF-16 code unit)"
                            );
                            #[expect(clippy::cast_possible_truncation)]
                            { ch as u16 }
                        }
                    ),*
                ]
            };
        }

    // 1. Let stringLength be the length of str.
    let string_length = strx.len();
    // 2. Assert: position ≤ stringLength.
    // 3. Let result be the empty String.
    let mut result = JSString::from("");
    // 4. Let templateRemainder be replacementTemplate.
    let mut template_remainder = replacement_template.as_slice();
    let mut string_hold: JSString;
    // 5. Repeat, while templateRemainder is not the empty String,
    while !template_remainder.is_empty() {
        // a. NOTE: The following steps isolate ref (a prefix of templateRemainder), determine refReplacement (its
        //    replacement), and then append that replacement to result.
        // b. If templateRemainder starts with "$$", then
        let (reference, ref_replacement) = if template_remainder.starts_with(&c!['$', '$']) {
            // i. Let ref be "$$".
            let r = &template_remainder[0..2];
            // ii. Let refReplacement be "$".
            let ref_replacement = &c!['$'][..];
            (r, ref_replacement)
        // c. Else if templateRemainder starts with "$`", then
        } else if template_remainder.starts_with(&c!['$', '`']) {
            // i. Let ref be "$`".
            let r = &template_remainder[0..2];
            // ii. Let refReplacement be the substring of str from 0 to position.
            let ref_replacement = &strx.as_slice()[0..position];
            (r, ref_replacement)
        // d. Else if templateRemainder starts with "$&", then
        } else if template_remainder.starts_with(&c!['$', '&']) {
            // i. Let ref be "$&".
            let r = &template_remainder[0..2];
            // ii. Let refReplacement be matched.
            let ref_replacement = matched.as_slice();
            (r, ref_replacement)
        // e. Else if templateRemainder starts with "$'" (0x0024 (DOLLAR SIGN) followed by 0x0027 (APOSTROPHE)), then
        } else if template_remainder.starts_with(&c!['$', '\'']) {
            // i. Let ref be "$'".
            let r = &template_remainder[0..2];
            // ii. Let matchLength be the length of matched.
            let match_length = matched.len();
            // iii. Let tailPos be position + matchLength.
            let tail_pos = position + match_length;
            // iv. Let refReplacement be the substring of str from min(tailPos, stringLength).
            let ref_replacement = &strx.as_slice()[tail_pos.min(string_length)..];
            // v. NOTE: tailPos can exceed stringLength only if this abstract operation was invoked by a call to the
            //    intrinsic %Symbol.replace% method of %RegExp.prototype% on an object whose "exec" property is not
            //    the intrinsic %RegExp.prototype.exec%.
            (r, ref_replacement)
        // f. Else if templateRemainder starts with "$" followed by 1 or more decimal digits, then
        } else if template_remainder.len() >= 2
            && template_remainder[0] == '$' as u16
            && (0x30..=0x39).contains(&template_remainder[1])
        {
            // i. If templateRemainder starts with "$" followed by 2 or more decimal digits, let digitCount be 2;
            //    else let digitCount be 1.
            let mut digit_count =
                if template_remainder.len() >= 3 && (0x30..=0x39).contains(&template_remainder[2]) { 2 } else { 1 };
            // ii. Let digits be the substring of templateRemainder from 1 to 1 + digitCount.
            let mut digits = &template_remainder[1..=digit_count];
            // iii. Let index be ℝ(StringToNumber(digits)).
            let mut index = digits
                .iter()
                .map(|big| char::try_from(u32::from(*big)).expect("digit values should transform"))
                .collect::<String>()
                .parse::<usize>()
                .expect("two digits should parse just fine");
            // iv. Assert: 0 ≤ index ≤ 99.
            // v. Let captureLen be the number of elements in captures.
            let capture_len = captures.len();
            // vi. If index > captureLen and digitCount = 2, then
            if index > capture_len && digit_count == 2 {
                // 1. NOTE: When a two-digit replacement pattern specifies an index exceeding the count of capturing
                //    groups, it is treated as a one-digit replacement pattern followed by a literal digit.
                // 2. Set digitCount to 1.
                digit_count = 1;
                // 3. Set digits to the substring of digits from 0 to 1.
                digits = &digits[0..1];
                // 4. Set index to ℝ(StringToNumber(digits)).
                index = usize::from(digits[0]) - 0x30;
            }
            // vii. Let ref be the substring of templateRemainder from 0 to 1 + digitCount.
            let r = &template_remainder[0..=digit_count];
            // viii. If 1 ≤ index ≤ captureLen, then
            let ref_replacement = if 1 <= index && index <= capture_len {
                // 1. Let capture be captures[index - 1].
                let capture = captures[index - 1].as_ref();
                // 3. Else,
                if let Some(capture) = capture {
                    // a. Let refReplacement be capture.
                    capture.as_slice()
                // 2. If capture is undefined, then
                } else {
                    // a. Let refReplacement be the empty String.
                    &[][..]
                }
            // ix. Else,
            } else {
                // 1. Let refReplacement be ref.
                r
            };
            (r, ref_replacement)
        // g. Else if templateRemainder starts with "$<", then
        } else if template_remainder.starts_with(&c!['$', '<']) {
            // i. Let gtPos be StringIndexOf(templateRemainder, ">", 0).
            let gt_pos = template_remainder.iter().enumerate().find(|(_, ch)| **ch == '>' as u16).map(|(idx, _)| idx);
            // iii. Else,
            if let (Some(gt_pos), Some(named_captures)) = (gt_pos, named_captures.as_ref()) {
                // 1. Let ref be the substring of templateRemainder from 0 to gtPos + 1.
                let r = &template_remainder[0..=gt_pos];
                // 2. Let groupName be the substring of templateRemainder from 2 to gtPos.
                let group_name = &template_remainder[2..gt_pos];
                // 3. Assert: namedCaptures is an Object.
                // 4. Let capture be ? Get(namedCaptures, groupName).
                let capture = named_captures.get(&group_name.into())?;
                // 5. If capture is undefined, then
                let ref_replacement = if capture.is_undefined() {
                    // a. Let refReplacement be the empty String.
                    &[][..]
                // 6. Else,
                } else {
                    // a. Let refReplacement be ? ToString(capture).
                    string_hold = to_string(capture)?;
                    string_hold.as_slice()
                };
                (r, ref_replacement)
            // ii. If gtPos is not-found or namedCaptures is undefined, then
            } else {
                // 1. Let ref be "$<".
                let r = &c!['$', '<'][..];
                // 2. Let refReplacement be ref.
                (r, r)
            }
        // h. Else,
        } else {
            // i. Let ref be the substring of templateRemainder from 0 to 1.
            let r = &template_remainder[0..1];
            // ii. Let refReplacement be ref.
            (r, r)
        };
        // i. Let refLength be the length of ref.
        let ref_length = reference.len();
        // j. Set templateRemainder to the substring of templateRemainder from refLength.
        template_remainder = &template_remainder[ref_length..];
        // k. Set result to the string-concatenation of result and refReplacement.
        result = result.concat(ref_replacement);
    }
    // 6. Return result.
    Ok(result)
}

#[cfg(test)]
mod tests;
