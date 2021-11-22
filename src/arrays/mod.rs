use super::agent::{Agent, WksId};
use super::cr::{AltCompletion, Completion};
use super::errors::{create_range_error, create_type_error};
use super::function_object::create_builtin_function;
use super::object::{
    construct, define_property_or_throw, get, get_function_realm, make_basic_object, ordinary_define_own_property, ordinary_delete, ordinary_get, ordinary_get_own_property,
    ordinary_get_prototype_of, ordinary_has_property, ordinary_is_extensible, ordinary_own_property_keys, ordinary_prevent_extensions, ordinary_set, ordinary_set_prototype_of,
    CommonObjectData, DataDescriptor, InternalSlotName, Object, ObjectInterface, PotentialPropertyDescriptor, PropertyDescriptor, ARRAY_OBJECT_SLOTS, BUILTIN_FUNCTION_SLOTS,
};
use super::realm::{IntrinsicId, Realm};
use super::strings::JSString;
use super::values::{is_constructor, number_same_value_zero, to_number, to_uint32, ArrayIndex, ECMAScriptValue, PropertyKey};
use std::cell::RefCell;
use std::rc::Rc;

// Array Exotic Objects
//
// An Array is an exotic object that gives special treatment to array index property keys (see 6.1.7). A property whose
// property name is an array index is also called an element. Every Array has a non-configurable "length" property
// whose value is always a non-negative integral Number whose mathematical value is less than 2**32. The value of the
// "length" property is numerically greater than the name of every own property whose name is an array index; whenever
// an own property of an Array is created or changed, other properties are adjusted as necessary to maintain this
// invariant. Specifically, whenever an own property is added whose name is an array index, the value of the "length"
// property is changed, if necessary, to be one more than the numeric value of that array index; and whenever the value
// of the "length" property is changed, every own property whose name is an array index whose value is not smaller than
// the new length is deleted. This constraint applies only to own properties of an Array and is unaffected by "length"
// or array index properties that may be inherited from its prototypes.
//
// NOTE |   A String property name P is an array index if and only if ToString(ToUint32(P)) equals P and ToUint32(P) is
//      |   not the same value as 𝔽(2**32 - 1).
//
// An object is an Array exotic object (or simply, an Array) if its [[DefineOwnProperty]] internal method uses the
// following implementation, and its other essential internal methods use the definitions found in 10.1. These methods
// are installed in ArrayCreate.

#[derive(Debug)]
pub struct ArrayObject {
    common: RefCell<CommonObjectData>,
}

impl<'a> From<&'a ArrayObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a ArrayObject) -> Self {
        obj
    }
}

impl ObjectInterface for ArrayObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn is_ordinary(&self) -> bool {
        true
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }
    fn get_prototype_of(&self, _: &mut Agent) -> AltCompletion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }
    fn set_prototype_of(&self, _: &mut Agent, obj: Option<Object>) -> AltCompletion<bool> {
        Ok(ordinary_set_prototype_of(self, obj))
    }
    fn is_extensible(&self, _: &mut Agent) -> AltCompletion<bool> {
        Ok(ordinary_is_extensible(self))
    }
    fn prevent_extensions(&self, _: &mut Agent) -> AltCompletion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }
    fn get_own_property(&self, _: &mut Agent, key: &PropertyKey) -> AltCompletion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }
    // [[DefineOwnProperty]] ( P, Desc )
    //
    // The [[DefineOwnProperty]] internal method of an Array exotic object A takes arguments P (a property key) and
    // Desc (a Property Descriptor). It performs the following steps when called:
    //
    //  1. If P is "length", then
    //      a. Return ? ArraySetLength(A, Desc).
    //  2. Else if P is an array index, then
    //      a. Let oldLenDesc be OrdinaryGetOwnProperty(A, "length").
    //      b. Assert: ! IsDataDescriptor(oldLenDesc) is true.
    //      c. Assert: oldLenDesc.[[Configurable]] is false.
    //      d. Let oldLen be oldLenDesc.[[Value]].
    //      e. Assert: oldLen is a non-negative integral Number.
    //      f. Let index be ! ToUint32(P).
    //      g. If index ≥ oldLen and oldLenDesc.[[Writable]] is false, return false.
    //      h. Let succeeded be ! OrdinaryDefineOwnProperty(A, P, Desc).
    //      i. If succeeded is false, return false.
    //      j. If index ≥ oldLen, then
    //          i. Set oldLenDesc.[[Value]] to index + 1𝔽.
    //          ii. Set succeeded to OrdinaryDefineOwnProperty(A, "length", oldLenDesc).
    //          iii. Assert: succeeded is true.
    //      k. Return true.
    //  3. Return OrdinaryDefineOwnProperty(A, P, Desc).
    fn define_own_property(&self, agent: &mut Agent, key: PropertyKey, desc: PotentialPropertyDescriptor) -> AltCompletion<bool> {
        let length_key = PropertyKey::from("length");
        if key == length_key {
            self.set_length(agent, desc)
        } else if key.is_array_index(agent) {
            let p = JSString::try_from(key).unwrap();
            let old_len_desc = DataDescriptor::try_from(ordinary_get_own_property(self, &length_key).unwrap()).unwrap();
            let old_len = to_uint32(agent, old_len_desc.value).unwrap();
            let index = to_uint32(agent, p.clone()).unwrap();
            if index >= old_len && !old_len_desc.writable {
                Ok(false)
            } else {
                let succeeded = ordinary_define_own_property(agent, self, PropertyKey::from(p), desc).unwrap();
                if !succeeded {
                    Ok(false)
                } else {
                    if index >= old_len {
                        ordinary_define_own_property(
                            agent,
                            self,
                            length_key,
                            PotentialPropertyDescriptor {
                                value: Some(ECMAScriptValue::from(index + 1)),
                                writable: Some(old_len_desc.writable),
                                enumerable: Some(old_len_desc.enumerable),
                                configurable: Some(old_len_desc.configurable),
                                ..Default::default()
                            },
                        )
                        .unwrap();
                    }
                    Ok(true)
                }
            }
        } else {
            ordinary_define_own_property(agent, self, key, desc)
        }
    }
    fn has_property(&self, agent: &mut Agent, key: &PropertyKey) -> AltCompletion<bool> {
        ordinary_has_property(agent, self, key)
    }
    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion {
        ordinary_get(agent, self, key, receiver)
    }
    fn set(&self, agent: &mut Agent, key: PropertyKey, value: ECMAScriptValue, receiver: &ECMAScriptValue) -> AltCompletion<bool> {
        ordinary_set(agent, self, key, value, receiver)
    }
    fn delete(&self, agent: &mut Agent, key: &PropertyKey) -> AltCompletion<bool> {
        ordinary_delete(agent, self, key)
    }
    fn own_property_keys(&self, agent: &mut Agent) -> AltCompletion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(agent, self))
    }
    fn is_array_object(&self) -> bool {
        true
    }
    fn to_array_object(&self) -> Option<&ArrayObject> {
        Some(self)
    }
}

pub fn array_create(agent: &mut Agent, length: u64, proto: Option<Object>) -> AltCompletion<Object> {
    ArrayObject::create(agent, length, proto)
}

impl ArrayObject {
    // ArrayCreate ( length [ , proto ] )
    //
    // The abstract operation ArrayCreate takes argument length (a non-negative integer) and optional argument proto.
    // It is used to specify the creation of new Arrays. It performs the following steps when called:
    //
    //  1. If length > 2**32 - 1, throw a RangeError exception.
    //  2. If proto is not present, set proto to %Array.prototype%.
    //  3. Let A be ! MakeBasicObject(« [[Prototype]], [[Extensible]] »).
    //  4. Set A.[[Prototype]] to proto.
    //  5. Set A.[[DefineOwnProperty]] as specified in 10.4.2.1.
    //  6. Perform ! OrdinaryDefineOwnProperty(A, "length", PropertyDescriptor { [[Value]]: 𝔽(length), [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false }).
    //  7. Return A.
    pub fn create(agent: &mut Agent, length: u64, proto: Option<Object>) -> AltCompletion<Object> {
        if length > 4294967295 {
            return Err(create_range_error(agent, "Array lengths greater than 4294967295 are not allowed"));
        }
        let length: u32 = length.try_into().unwrap();
        let proto = proto.unwrap_or_else(|| agent.intrinsic(IntrinsicId::ArrayPrototype));
        let a = make_basic_object(agent, &[InternalSlotName::Prototype, InternalSlotName::Extensible, InternalSlotName::ArrayMarker], Some(proto));
        ordinary_define_own_property(
            agent,
            &a,
            "length".into(),
            PotentialPropertyDescriptor { value: Some(length.into()), writable: Some(true), enumerable: Some(false), configurable: Some(false), ..Default::default() },
        )
        .unwrap();
        Ok(a)
    }

    pub fn object(agent: &mut Agent, prototype: Option<Object>) -> Object {
        Object { o: Rc::new(Self { common: RefCell::new(CommonObjectData::new(agent, prototype, true, ARRAY_OBJECT_SLOTS)) }) }
    }

    // ArraySetLength ( A, Desc )
    //
    // The abstract operation ArraySetLength takes arguments A (an Array) and Desc (a Property Descriptor). It performs
    // the following steps when called:
    //
    //  1. If Desc.[[Value]] is absent, then
    //      a. Return OrdinaryDefineOwnProperty(A, "length", Desc).
    //  2. Let newLenDesc be a copy of Desc.
    //  3. Let newLen be ? ToUint32(Desc.[[Value]]).
    //  4. Let numberLen be ? ToNumber(Desc.[[Value]]).
    //  5. If SameValueZero(newLen, numberLen) is false, throw a RangeError exception.
    //  6. Set newLenDesc.[[Value]] to newLen.
    //  7. Let oldLenDesc be OrdinaryGetOwnProperty(A, "length").
    //  8. Assert: ! IsDataDescriptor(oldLenDesc) is true.
    //  9. Assert: oldLenDesc.[[Configurable]] is false.
    //  10. Let oldLen be oldLenDesc.[[Value]].
    //  11. If newLen ≥ oldLen, then
    //      a. Return OrdinaryDefineOwnProperty(A, "length", newLenDesc).
    //  12. If oldLenDesc.[[Writable]] is false, return false.
    //  13. If newLenDesc.[[Writable]] is absent or has the value true, let newWritable be true.
    //  14. Else,
    //      a. NOTE: Setting the [[Writable]] attribute to false is deferred in case any elements cannot be deleted.
    //      b. Let newWritable be false.
    //      c. Set newLenDesc.[[Writable]] to true.
    //  15. Let succeeded be ! OrdinaryDefineOwnProperty(A, "length", newLenDesc).
    //  16. If succeeded is false, return false.
    //  17. For each own property key P of A that is an array index, whose numeric value is greater than or equal to newLen, in descending numeric index order, do
    //      a. Let deleteSucceeded be ! A.[[Delete]](P).
    //      b. If deleteSucceeded is false, then
    //          i. Set newLenDesc.[[Value]] to ! ToUint32(P) + 1𝔽.
    //          ii. If newWritable is false, set newLenDesc.[[Writable]] to false.
    //          iii. Perform ! OrdinaryDefineOwnProperty(A, "length", newLenDesc).
    //          iv. Return false.
    //  18. If newWritable is false, then
    //      a. Set succeeded to ! OrdinaryDefineOwnProperty(A, "length", PropertyDescriptor { [[Writable]]: false }).
    //      b. Assert: succeeded is true.
    //  19. Return true.
    //
    // NOTE |   In steps 3 and 4, if Desc.[[Value]] is an object then its valueOf method is called twice. This is
    //      |   legacy behaviour that was specified with this effect starting with the 2nd Edition of this specification.
    fn set_length(&self, agent: &mut Agent, descriptor: PotentialPropertyDescriptor) -> AltCompletion<bool> {
        if descriptor.value.is_none() {
            return ordinary_define_own_property(agent, self, PropertyKey::from("length"), descriptor);
        }
        let mut new_len_desc = descriptor.clone();
        let new_len = to_uint32(agent, descriptor.value.clone().unwrap())?;
        let number_len = to_number(agent, descriptor.value.unwrap())?;
        if !number_same_value_zero(new_len as f64, number_len) {
            return Err(create_range_error(agent, "Invalid array length"));
        }
        new_len_desc.value = Some(ECMAScriptValue::from(new_len));
        let old_len_desc = ordinary_get_own_property(self, &"length".into()).unwrap();
        let old_len_desc = DataDescriptor::try_from(old_len_desc).unwrap();
        let old_len = to_uint32(agent, old_len_desc.value).unwrap();
        if new_len >= old_len {
            return ordinary_define_own_property(agent, self, "length".into(), new_len_desc);
        }
        if !old_len_desc.writable {
            return Ok(false);
        }
        let new_writable = if new_len_desc.writable.is_none() || new_len_desc.writable.unwrap() {
            true
        } else {
            new_len_desc.writable = Some(true);
            false
        };
        let succeeded = ordinary_define_own_property(agent, self, "length".into(), new_len_desc.clone()).unwrap();
        if !succeeded {
            return Ok(false);
        }
        let mut keys: Vec<PropertyKey>;
        {
            let data = self.common.borrow();
            keys = data.properties.keys().filter(|key| ArrayIndex::try_from(*key).map_or(false, |idx| u32::from(idx) >= new_len)).cloned().collect();
        }
        keys.sort_by_cached_key(|key| ArrayIndex::try_from(key).unwrap());
        for p in keys.into_iter().rev() {
            let delete_succeeded = self.delete(agent, &p).unwrap();
            if !delete_succeeded {
                new_len_desc.value = Some(ECMAScriptValue::from(to_uint32(agent, p).unwrap() + 1));
                if !new_writable {
                    new_len_desc.writable = Some(false);
                }
                ordinary_define_own_property(agent, self, "length".into(), new_len_desc).unwrap();
                return Ok(false);
            }
        }
        if !new_writable {
            ordinary_define_own_property(agent, self, "length".into(), PotentialPropertyDescriptor { writable: Some(false), ..Default::default() }).unwrap();
        }
        Ok(true)
    }
}

// ArraySpeciesCreate ( originalArray, length )
//
// The abstract operation ArraySpeciesCreate takes arguments originalArray and length (a non-negative integer). It is
// used to specify the creation of a new Array or similar object using a constructor function that is derived from
// originalArray. It does not enforce that the constructor function returns an Array. It performs the following steps
// when called:
//
//  1. Let isArray be ? IsArray(originalArray).
//  2. If isArray is false, return ? ArrayCreate(length).
//  3. Let C be ? Get(originalArray, "constructor").
//  4. If IsConstructor(C) is true, then
//      a. Let thisRealm be the current Realm Record.
//      b. Let realmC be ? GetFunctionRealm(C).
//      c. If thisRealm and realmC are not the same Realm Record, then
//          i. If SameValue(C, realmC.[[Intrinsics]].[[%Array%]]) is true, set C to undefined.
//  5. If Type(C) is Object, then
//      a. Set C to ? Get(C, @@species).
//      b. If C is null, set C to undefined.
//  6. If C is undefined, return ? ArrayCreate(length).
//  7. If IsConstructor(C) is false, throw a TypeError exception.
//  8. Return ? Construct(C, « 𝔽(length) »).
//
// NOTE |   If originalArray was created using the standard built-in Array constructor for a realm that is not the
//      |   realm of the running execution context, then a new Array is created using the realm of the running
//      |   execution context. This maintains compatibility with Web browsers that have historically had that behaviour
//      |   for the Array.prototype methods that now are defined using ArraySpeciesCreate.
pub fn array_species_create(agent: &mut Agent, original_array: &Object, length: u64) -> Completion {
    let is_array = original_array.is_array(agent)?;
    if !is_array {
        return Ok(ArrayObject::create(agent, length, None)?.into());
    }
    let mut c = get(agent, original_array, &"constructor".into())?;
    if is_constructor(&c) {
        let c_obj = Object::try_from(&c).unwrap();
        let this_realm = agent.running_execution_context().unwrap().realm.clone();
        let realm_c = get_function_realm(agent, &c_obj)?;
        if Rc::ptr_eq(&this_realm, &realm_c) && c_obj == realm_c.borrow().intrinsics.array {
            c = ECMAScriptValue::Undefined;
        }
    }
    if c.is_object() {
        let c_obj = Object::try_from(&c).unwrap();
        let species = get(agent, &c_obj, &agent.wks(WksId::Species).into())?;
        if species.is_null() {
            c = ECMAScriptValue::Undefined;
        } else {
            c = species;
        }
    }
    if c.is_undefined() {
        return Ok(ArrayObject::create(agent, length, None)?.into());
    }
    if !is_constructor(&c) {
        return Err(create_type_error(agent, "Array species constructor invalid"));
    }
    let c_obj = Object::try_from(&c).unwrap();
    construct(agent, &c_obj, &[length.into()], None)
}

// IsArray ( argument )
//
// The abstract operation IsArray takes argument argument. It performs the following steps when called:
//
//  1. If Type(argument) is not Object, return false.
//  2. If argument is an Array exotic object, return true.
//  3. If argument is a Proxy exotic object, then
//      a. If argument.[[ProxyHandler]] is null, throw a TypeError exception.
//      b. Let target be argument.[[ProxyTarget]].
//      c. Return ? IsArray(target).
//  4. Return false.
pub fn is_array(agent: &mut Agent, argument: &ECMAScriptValue) -> AltCompletion<bool> {
    argument.is_array(agent)
}

pub fn provision_array_intrinsic(agent: &mut Agent, realm: &Rc<RefCell<Realm>>) {
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();

    // The Array Constructor
    //
    // The Array constructor:
    //
    //  * is %Array%.
    //  * is the initial value of the "Array" property of the global object.
    //  * creates and initializes a new Array when called as a constructor.
    //  * also creates and initializes a new Array when called as a function rather than as a constructor. Thus the
    //    function call Array(…) is equivalent to the object creation expression new Array(…) with the same arguments.
    //  * is a function whose behaviour differs based upon the number and types of its arguments.
    //  * may be used as the value of an extends clause of a class definition. Subclass constructors that intend to
    //    inherit the exotic Array behaviour must include a super call to the Array constructor to initialize subclass
    //    instances that are Array exotic objects. However, most of the Array.prototype methods are generic methods
    //    that are not dependent upon their this value being an Array exotic object.
    //  * has a "length" property whose value is 1𝔽.
    //
    // Properties of the Array Constructor
    //
    //  The Array constructor:
    //
    //  * has a [[Prototype]] internal slot whose value is %Function.prototype%.
    let array_constructor = create_builtin_function(
        agent,
        array_constructor_function,
        true,
        1.0,
        PropertyKey::from("Array"),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype.clone()),
        None,
    );

    // Constructor Function Properties
    macro_rules! constructor_function {
        ( $steps:expr, $name:expr, $length:expr ) => {
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(agent, $steps, false, $length, key.clone(), BUILTIN_FUNCTION_SLOTS, Some(realm.clone()), Some(function_prototype.clone()), None);
            define_property_or_throw(
                agent,
                &array_constructor,
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
    constructor_function!(array_from, "from", 1.0);
    constructor_function!(array_is_array, "isArray", 1.0);
    constructor_function!(array_of, "of", 0.0);
    // todo!() get Array[@@species]

    // Properties of the Array Prototype Object
    //
    // The Array prototype object:
    //
    //  * is %Array.prototype%.
    //  * is an Array exotic object and has the internal methods specified for such objects.
    //  * has a "length" property whose initial value is +0𝔽 and whose attributes are { [[Writable]]: true,
    //    [[Enumerable]]: false, [[Configurable]]: false }.
    //  * has a [[Prototype]] internal slot whose value is %Object.prototype%.
    //
    // NOTE |   The Array prototype object is specified to be an Array exotic object to ensure compatibility with
    //          ECMAScript code that was created prior to the ECMAScript 2015 specification.
    let array_prototype = ArrayObject::create(agent, 0, Some(object_prototype)).unwrap();

    // Array.prototype
    //
    // The value of Array.prototype is the Array prototype object.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    define_property_or_throw(
        agent,
        &array_constructor,
        PropertyKey::from("prototype"),
        PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::from(array_prototype.clone())),
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
            let function_object = create_builtin_function(agent, $steps, false, $length, key.clone(), BUILTIN_FUNCTION_SLOTS, Some(realm.clone()), Some(function_prototype.clone()), None);
            define_property_or_throw(
                agent,
                &array_prototype,
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

    prototype_function!(array_prototype_at, "at", 1.0); // ( index )
    prototype_function!(array_prototype_concat, "concat", 0.0); // ( ...items )
    prototype_function!(array_prototype_copy_within, "copyWithin", 2.0); // ( target, start [ , end ] )
    prototype_function!(array_prototype_entries, "entries", 0.0); // ( )
    prototype_function!(array_prototype_every, "every", 1.0); // ( callbackfn [ , thisArg ] )
    prototype_function!(array_prototype_fill, "fill", 1.0); // ( value [ , start [ , end ] ] )
    prototype_function!(array_prototype_filter, "filter", 1.0); // ( callbackfn [ , thisArg ] )
    prototype_function!(array_prototype_find, "find", 1.0); // ( predicate [ , thisArg ] )
    prototype_function!(array_prototype_find_index, "findIndex", 1.0); // ( predicate [ , thisArg ] )
    prototype_function!(array_prototype_flat, "flat", 0.0); // ( [ depth ] )
    prototype_function!(array_prototype_flat_map, "flatMap", 1.0); // ( mapperFunction [ , thisArg ] )
    prototype_function!(array_prototype_for_each, "forEach", 1.0); // ( callbackfn [ , thisArg ] )
    prototype_function!(array_prototype_includes, "includes", 1.0); // ( searchElement [ , fromIndex ] )
    prototype_function!(array_prototype_index_of, "indexOf", 1.0); // ( searchElement [ , fromIndex ] )
    prototype_function!(array_prototype_join, "join", 1.0); // ( separator )
    prototype_function!(array_prototype_keys, "keys", 0.0); // ( )
    prototype_function!(array_prototype_last_index_of, "lastIndexOf", 1.0); // ( searchElement [ , fromIndex ] )
    prototype_function!(array_prototype_map, "map", 1.0); // ( callbackfn [ , thisArg ] )
    prototype_function!(array_prototype_pop, "pop", 0.0); // ( )
    prototype_function!(array_prototype_push, "push", 0.0); // ( ...items )
    prototype_function!(array_prototype_reduce, "reduce", 1.0); // ( callbackfn [ , initialValue ] )
    prototype_function!(array_prototype_reduce_right, "reduceRight", 1.0); // ( callbackfn [ , initialValue ] )
    prototype_function!(array_prototype_reverse, "reverse", 0.0); // ( )
    prototype_function!(array_prototype_shift, "shift", 0.0); // ( )
    prototype_function!(array_prototype_slice, "slice", 2.0); // ( start, end )
    prototype_function!(array_prototype_some, "some", 1.0); // ( callbackfn [ , thisArg ] )
    prototype_function!(array_prototype_sort, "sort", 1.0); // ( comparefn )
    prototype_function!(array_prototype_splice, "splice", 2.0); // ( start, deleteCount, ...items )
    prototype_function!(array_prototype_to_locale_string, "toLocaleString", 0.0); // ( [ reserved1 [ , reserved2 ] ] )
    prototype_function!(array_prototype_to_string, "toString", 0.0); // ( )
    prototype_function!(array_prototype_unshift, "unshift", 0.0); // ( ...items )
    prototype_function!(array_prototype_values, "values", 0.0); // ( )
                                                                //prototype_function!(array_prototype_[, "[", ); // @@iterator ] ( )
                                                                //prototype_function!(array_prototype_[, "[", ); // @@unscopables ]

    // Array.prototype.constructor
    //
    // The initial value of Array.prototype.constructor is %Array%.
    define_property_or_throw(
        agent,
        &array_prototype,
        PropertyKey::from("constructor"),
        PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::from(array_constructor.clone())),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();

    realm.borrow_mut().intrinsics.array = array_constructor;
    realm.borrow_mut().intrinsics.array_prototype = array_prototype;
}

fn array_constructor_function(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_from(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_is_array(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_of(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_at(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_concat(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_copy_within(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_entries(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_every(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_fill(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_filter(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_find(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_find_index(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_flat(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_flat_map(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_for_each(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_includes(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_index_of(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_join(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_keys(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_last_index_of(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_map(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_pop(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_push(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_reduce(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_reduce_right(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_reverse(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_shift(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_slice(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_some(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_sort(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_splice(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_to_locale_string(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_to_string(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_unshift(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}
fn array_prototype_values(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    todo!()
}

#[cfg(test)]
mod tests;
