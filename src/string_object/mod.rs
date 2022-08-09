use super::*;
use std::cell::RefCell;
//use std::rc::Rc;

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
    string_data: RefCell<JSString>,
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

    fn is_ordinary(&self) -> bool {
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

    fn get_prototype_of(&self, _agent: &mut Agent) -> Completion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }

    fn set_prototype_of(&self, _agent: &mut Agent, obj: Option<Object>) -> Completion<bool> {
        Ok(ordinary_set_prototype_of(self, obj))
    }

    fn is_extensible(&self, _agent: &mut Agent) -> Completion<bool> {
        Ok(ordinary_is_extensible(self))
    }

    fn prevent_extensions(&self, _agent: &mut Agent) -> Completion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }

    fn get_own_property(&self, _agent: &mut Agent, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
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

    fn define_own_property(
        &self,
        agent: &mut Agent,
        key: PropertyKey,
        desc: PotentialPropertyDescriptor,
    ) -> Completion<bool> {
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
            Ok(is_compatible_property_descriptor(extensible, desc, &string_desc))
        } else {
            ordinary_define_own_property(agent, self, key, desc)
        }
    }

    fn has_property(&self, agent: &mut Agent, key: &PropertyKey) -> Completion<bool> {
        ordinary_has_property(agent, self, key)
    }

    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        ordinary_get(agent, self, key, receiver)
    }

    fn set(
        &self,
        agent: &mut Agent,
        key: PropertyKey,
        value: ECMAScriptValue,
        receiver: &ECMAScriptValue,
    ) -> Completion<bool> {
        ordinary_set(agent, self, key, value, receiver)
    }

    fn delete(&self, agent: &mut Agent, key: &PropertyKey) -> Completion<bool> {
        ordinary_delete(agent, self, key)
    }

    fn own_property_keys(&self, _agent: &mut Agent) -> Completion<Vec<PropertyKey>> {
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
        let string = self.string_data.borrow();
        let len = string.len();
        for idx in 0..len {
            keys.push(PropertyKey::from(idx));
        }
        let bindings = &self.common.borrow().properties;
        let mut norm_keys: Vec<(PropertyKey, usize)> = Vec::new();
        let mut symb_keys: Vec<(PropertyKey, usize)> = Vec::new();
        let mut extra_numbers: Vec<(PropertyKey, u32)> = Vec::new();
        for (key, desc) in bindings.iter() {
            if key.is_array_index() {
                let keyval = array_index_key(key);
                if keyval as usize >= len {
                    extra_numbers.push((key.clone(), keyval));
                }
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

impl Agent {
    pub fn string_create(&mut self, value: JSString, prototype: Option<Object>) -> Object {
        StringObject::object(self, value, prototype)
    }
    pub fn create_string_object(&mut self, s: JSString) -> Object {
        let prototype = self.intrinsic(IntrinsicId::StringPrototype);
        self.string_create(s, Some(prototype))
    }
}

impl StringObject {
    pub fn object(agent: &mut Agent, value: JSString, prototype: Option<Object>) -> Object {
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
        let length = value.len() as f64;
        let s = Object {
            o: Rc::new(Self {
                common: RefCell::new(CommonObjectData::new(agent, prototype, true, STRING_OBJECT_SLOTS)),
                string_data: RefCell::new(value),
            }),
        };
        define_property_or_throw(
            agent,
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
            let index = canonical_numeric_index_string(p.clone())?;
            is_integral_number(&index.into()).then_some(())?;
            (index == 0.0 && index.signum() == -1.0).then_some(())?;
            let string = self.string_data.borrow();
            let len = string.len();
            (index < 0.0 || index >= len as f64).then_some(())?;
            let idx = index as usize;
            let value = JSString::from(&string.as_slice()[idx..idx + 1]);
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
