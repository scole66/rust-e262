use super::*;
use std::cell::RefCell;

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
pub struct StringObject {
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

    fn is_string_object(&self) -> bool {
        true
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
        if let Some(string_desc) = string_desc {
            let extensible = self.common.borrow().extensible;
            Ok(is_compatible_property_descriptor(extensible, desc, Some(&string_desc)))
        } else {
            ordinary_define_own_property(self, key, desc)
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
        // [[OwnPropertyKeys]] ( )
        //
        // The [[OwnPropertyKeys]] internal method of a String exotic object O takes no arguments and returns
        // a normal completion containing a List of property keys. It performs the following steps when
        // called:
        //
        //  1. Let keys be a new empty List.
        //  2. Let str be O.[[StringData]].
        //  3. Assert: Type(str) is String.
        //  4. Let len be the length of str.
        //  5. For each integer i starting with 0 such that i < len, in ascending order, do
        //      a. Add ! ToString(𝔽(i)) as the last element of keys.
        //  6. For each own property key P of O such that P is an array index and ! ToIntegerOrInfinity(P) ≥
        //     len, in ascending numeric index order, do
        //      a. Add P as the last element of keys.
        //  7. For each own property key P of O such that Type(P) is String and P is not an array index, in
        //     ascending chronological order of property creation, do
        //      a. Add P as the last element of keys.
        //  8. For each own property key P of O such that Type(P) is Symbol, in ascending chronological order
        //     of property creation, do
        //      a. Add P as the last element of keys.
        //  9. Return keys.
        let mut keys = vec![];
        let string = &self.string_data;
        let len = string.len();
        for idx in 0..len {
            keys.push(PropertyKey::from(idx));
        }
        let bindings = &self.common.borrow().properties;
        let mut norm_keys: Vec<(PropertyKey, usize)> = Vec::new();
        let mut symb_keys: Vec<(PropertyKey, usize)> = Vec::new();
        let mut extra_numbers: Vec<(PropertyKey, u32)> = Vec::new();
        for (key, desc) in bindings {
            if key.is_array_index() {
                let keyval = array_index_key(key);
                assert!(keyval as usize >= len); // All lower array indices are part of the string, and no one should have been able to make them independently.
                extra_numbers.push((key.clone(), keyval));
            } else {
                match key {
                    PropertyKey::String(_) => {
                        norm_keys.push((key.clone(), desc.spot));
                    }
                    PropertyKey::Symbol(_) => {
                        symb_keys.push((key.clone(), desc.spot));
                    }
                }
            }
        }
        extra_numbers.sort_by_key(|x| x.1);
        norm_keys.sort_by_key(|x| x.1);
        symb_keys.sort_by_key(|x| x.1);

        keys.extend(extra_numbers.into_iter().map(|x| x.0));
        keys.extend(norm_keys.into_iter().map(|x| x.0));
        keys.extend(symb_keys.into_iter().map(|x| x.0));

        Ok(keys)
    }
}

pub fn string_create(value: JSString, prototype: Option<Object>) -> Object {
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
    pub fn new(value: JSString, prototype: Option<Object>) -> Self {
        Self { common: RefCell::new(CommonObjectData::new(prototype, true, STRING_OBJECT_SLOTS)), string_data: value }
    }

    pub fn object(value: JSString, prototype: Option<Object>) -> Object {
        // StringCreate ( value, prototype )
        //
        // The abstract operation StringCreate takes arguments value (a String) and prototype and returns a
        // String exotic object. It is used to specify the creation of new String exotic objects. It performs
        // the following steps when called:
        //
        // 1. Let S be MakeBasicObject(« [[Prototype]], [[Extensible]], [[StringData]] »).
        // 2. Set S.[[Prototype]] to prototype.
        // 3. Set S.[[StringData]] to value.
        // 4. Set S.[[GetOwnProperty]] as specified in 10.4.3.1.
        // 5. Set S.[[DefineOwnProperty]] as specified in 10.4.3.2.
        // 6. Set S.[[OwnPropertyKeys]] as specified in 10.4.3.3.
        // 7. Let length be the number of code unit elements in value.
        // 8. Perform ! DefinePropertyOrThrow(S, "length", PropertyDescriptor { [[Value]]: 𝔽(length),
        //    [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }).
        // 9. Return S.
        let length = value.len();
        let s = Object { o: Rc::new(Self::new(value, prototype)) };
        define_property_or_throw(
            &s,
            "length",
            PotentialPropertyDescriptor::new().value(length).writable(false).enumerable(false).configurable(false),
        )
        .expect("fresh object can't fail to create this");

        s
    }

    pub fn string_get_own_property(&self, key: &PropertyKey) -> Option<PropertyDescriptor> {
        // StringGetOwnProperty ( S, P )
        //
        // The abstract operation StringGetOwnProperty takes arguments S (an Object that has a [[StringData]]
        // internal slot) and P (a property key) and returns a Property Descriptor or undefined. It performs
        // the following steps when called:
        //
        //   1. If Type(P) is not String, return undefined.
        //   2. Let index be CanonicalNumericIndexString(P).
        //   3. If index is undefined, return undefined.
        //   4. If IsIntegralNumber(index) is false, return undefined.
        //   5. If index is -0𝔽, return undefined.
        //   6. Let str be S.[[StringData]].
        //   7. Assert: Type(str) is String.
        //   8. Let len be the length of str.
        //   9. If ℝ(index) < 0 or len ≤ ℝ(index), return undefined.
        //  10. Let resultStr be the substring of str from ℝ(index) to ℝ(index) + 1.
        //  11. Return the PropertyDescriptor { [[Value]]: resultStr, [[Writable]]: false, [[Enumerable]]:
        //      true, [[Configurable]]: false }.
        if let PropertyKey::String(p) = key {
            let index = canonical_numeric_index_string(p)?;
            is_integral_number(&index.into()).then_some(())?;
            (index != 0.0 || index.signum() != -1.0).then_some(())?;
            let string = &self.string_data;
            let len = string.len();
            #[expect(clippy::cast_precision_loss)]
            (index >= 0.0 && index < len as f64).then_some(())?;
            let idx = to_usize(index).expect("index should be a valid integer");
            let value = JSString::from(&string.as_slice()[idx..=idx]);
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

pub fn provision_string_intrinsic(realm: &Rc<RefCell<Realm>>) {
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
        ( $steps:expr, $name:expr, $length:expr ) => {
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
        ( $name:expr, $value:expr, $writable:expr, $enumerable:expr, $configurable:expr ) => {
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
        ( $steps:expr, $name:expr, $length:expr ) => {
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
    prototype_function!(string_prototype_slice, "slice", 1.0);
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
        ( $name:expr, $value:expr, $writable:expr, $enumerable:expr, $configurable:expr ) => {
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
    // String ( value )
    // This function performs the following steps when called:
    //
    //  1. If value is not present, let s be the empty String.
    //  2. Else,
    //      a. If NewTarget is undefined and Type(value) is Symbol, return SymbolDescriptiveString(value).
    //      b. Let s be ? ToString(value).
    //  3. If NewTarget is undefined, return s.
    //  4. Return StringCreate(s, ? GetPrototypeFromConstructor(NewTarget, "%String.prototype%")).
    let s = if arguments.is_empty() {
        JSString::from("")
    } else {
        let value = &arguments[0];
        if let (None, ECMAScriptValue::Symbol(sym)) = (new_target, value) {
            return Ok(ECMAScriptValue::from(sym.descriptive_string()));
        }
        to_string(value.clone())?
    };
    if let Some(nt) = new_target {
        let prototype = nt.get_prototype_from_constructor(IntrinsicId::StringPrototype)?;
        let s_obj = string_create(s, Some(prototype));
        Ok(ECMAScriptValue::from(s_obj))
    } else {
        Ok(ECMAScriptValue::from(s))
    }
}

fn this_string_value(value: ECMAScriptValue, from_where: &str) -> Completion<JSString> {
    // The abstract operation thisStringValue takes argument value. It performs the following steps when
    // called:
    //
    //  1. If Type(value) is String, return value.
    //  2. If Type(value) is Object and value has a [[StringData]] internal slot, then
    //      a. Let s be value.[[StringData]].
    //      b. Assert: Type(s) is String.
    //      c. Return s.
    //  3. Throw a TypeError exception.
    match value {
        ECMAScriptValue::String(s) => Ok(s),
        ECMAScriptValue::Object(obj) if obj.o.is_string_object() => {
            let sobj = obj.o.to_string_obj().unwrap();
            Ok(sobj.string_data.clone())
        }
        _ => Err(create_type_error(format!("{from_where} requires that 'this' be a String"))),
    }
}

fn string_from_char_code(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // String.fromCharCode ( ...codeUnits )
    //
    // This function may be called with any number of arguments which form the rest parameter codeUnits.
    //
    // It performs the following steps when called:
    //
    //  1. Let elements be a new empty List.
    //  2. For each element next of codeUnits, do
    //      a. Let nextCU be ℝ(? ToUint16(next)).
    //      b. Append nextCU to elements.
    //  3. Return the String value whose code units are the elements in the List elements. If codeUnits is empty, the
    //     empty String is returned.
    Ok(JSString::from(
        arguments.iter().map(ECMAScriptValue::to_uint16).collect::<Result<Vec<u16>, AbruptCompletion>>()?,
    )
    .into())
}
fn string_from_code_point(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn string_raw(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

// 22.1.3.1 String.prototype.at ( index )
fn string_prototype_at(_: &ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.2 String.prototype.charAt ( pos )
fn string_prototype_char_at(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.3 String.prototype.charCodeAt ( pos )
fn string_prototype_char_code_at(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.4 String.prototype.codePointAt ( pos )
fn string_prototype_code_point_at(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.5 String.prototype.concat ( ...args )
fn string_prototype_concat(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.7 String.prototype.endsWith ( searchString [ , endPosition ] )
fn string_prototype_ends_with(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.8 String.prototype.includes ( searchString [ , position ] )
fn string_prototype_includes(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.9 String.prototype.indexOf ( searchString [ , position ] )
fn string_prototype_index_of(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // String.prototype.indexOf ( searchString [ , position ] )
    //
    // NOTE 1: If searchString appears as a substring of the result of converting this object to a String, at
    // one or more indices that are greater than or equal to position, then the smallest such index is
    // returned; otherwise, -1𝔽 is returned. If position is undefined, +0𝔽 is assumed, so as to search all
    // of the String.
    //
    // This method performs the following steps when called:
    //
    //  1. Let O be ? RequireObjectCoercible(this value).
    //  2. Let S be ? ToString(O).
    //  3. Let searchStr be ? ToString(searchString).
    //  4. Let pos be ? ToIntegerOrInfinity(position).
    //  5. Assert: If position is undefined, then pos is 0.
    //  6. Let len be the length of S.
    //  7. Let start be the result of clamping pos between 0 and len.
    //  8. Return 𝔽(StringIndexOf(S, searchStr, start)).
    //
    // NOTE 2: This method is intentionally generic; it does not require that its this value be a String
    // object. Therefore, it can be transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let search_string = args.next_arg();
    let position = args.next_arg();

    require_object_coercible(this_value)?;
    let s = to_string(this_value.clone())?;
    let search_str = to_string(search_string)?;
    let pos = position.to_integer_or_infinity()?;
    let len = s.len();
    let max = to_f64(len).expect("len should fit within a float");
    let start = to_usize(pos.clamp(0.0, max)).expect("start should be within the string's length, which fits a usize");
    Ok(s.index_of(&search_str, start).into())
}

// 22.1.3.10 String.prototype.lastIndexOf ( searchString [ , position ] )
fn string_prototype_last_index_of(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.11 String.prototype.localeCompare ( that [ , reserved1 [ , reserved2 ] ] )
fn string_prototype_locale_compare(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.12 String.prototype.match ( regexp )
fn string_prototype_match(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.13 String.prototype.matchAll ( regexp )
fn string_prototype_match_all(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.14 String.prototype.normalize ( [ form ] )
fn string_prototype_normalize(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.15 String.prototype.padEnd ( maxLength [ , fillString ] )
fn string_prototype_pad_end(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.16 String.prototype.padStart ( maxLength [ , fillString ] )
fn string_prototype_pad_start(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum PadPlacement {
    Start,
    End,
}

impl JSString {
    #[must_use]
    pub fn string_pad(&self, max_length: usize, fill_string: &JSString, placement: PadPlacement) -> JSString {
        // StringPad ( S, maxLength, fillString, placement )
        // The abstract operation StringPad takes arguments S (a String), maxLength (a non-negative integer), fillString
        // (a String), and placement (start or end) and returns a String. It performs the following steps when called:
        //
        //  1. Let stringLength be the length of S.
        //  2. If maxLength ≤ stringLength, return S.
        //  3. If fillString is the empty String, return S.
        //  4. Let fillLen be maxLength - stringLength.
        //  5. Let truncatedStringFiller be the String value consisting of repeated concatenations of fillString
        //     truncated to length fillLen.
        //  6. If placement is start, return the string-concatenation of truncatedStringFiller and S.
        //  7. Else, return the string-concatenation of S and truncatedStringFiller.
        //
        // Note 1
        // The argument maxLength will be clamped such that it can be no smaller than the length of S.
        //
        // Note 2
        // The argument fillString defaults to " " (the String value consisting of the code unit 0x0020 SPACE).
        let string_length = self.len();
        if max_length <= string_length || fill_string.is_empty() {
            return self.clone();
        }
        let fill_len = max_length - string_length;
        let mut truncated_string_filler = JSString::from("");
        while truncated_string_filler.len() < fill_len {
            truncated_string_filler = truncated_string_filler.concat(fill_string.clone());
        }
        if truncated_string_filler.len() > fill_len {
            truncated_string_filler = JSString::from(&truncated_string_filler.as_slice()[0..fill_len]);
        }
        match placement {
            PadPlacement::Start => truncated_string_filler.concat(self.clone()),
            PadPlacement::End => self.concat(truncated_string_filler),
        }
    }
}

pub fn to_zero_padded_decimal_string(n: usize, min_length: usize) -> JSString {
    // ToZeroPaddedDecimalString ( n, minLength )
    // The abstract operation ToZeroPaddedDecimalString takes arguments n (a non-negative integer) and minLength (a
    // non-negative integer) and returns a String. It performs the following steps when called:
    //
    // 1. Let S be the String representation of n, formatted as a decimal number.
    // 2. Return StringPad(S, minLength, "0", start).
    let s = JSString::from(format!("{n}"));
    s.string_pad(min_length, &JSString::from("0"), PadPlacement::Start)
}

// 22.1.3.17 String.prototype.repeat ( count )
fn string_prototype_repeat(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
// 22.1.3.18 String.prototype.replace ( searchValue, replaceValue )
fn string_prototype_replace(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
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
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
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

fn string_prototype_iterator(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

#[cfg(test)]
mod tests;
