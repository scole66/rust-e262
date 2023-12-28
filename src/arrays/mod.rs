use super::*;
use genawaiter::rc::Co;
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
    fn uses_ordinary_get_prototype_of(&self) -> bool {
        true
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
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
    fn define_own_property(&self, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool> {
        let length_key = PropertyKey::from("length");
        if key == length_key {
            self.set_length(desc)
        } else if key.is_array_index() {
            let p = JSString::try_from(key).unwrap();
            let old_len_desc = DataDescriptor::try_from(ordinary_get_own_property(self, &length_key).unwrap()).unwrap();
            let old_len = to_uint32(old_len_desc.value).unwrap();
            let index = to_uint32(p.clone()).unwrap();
            if index >= old_len && !old_len_desc.writable {
                Ok(false)
            } else {
                let succeeded = ordinary_define_own_property(self, PropertyKey::from(p), desc).unwrap();
                if !succeeded {
                    Ok(false)
                } else {
                    if index >= old_len {
                        ordinary_define_own_property(
                            self,
                            length_key,
                            PotentialPropertyDescriptor::new()
                                .value(ECMAScriptValue::from(index + 1))
                                .writable(old_len_desc.writable)
                                .enumerable(old_len_desc.enumerable)
                                .configurable(old_len_desc.configurable),
                        )
                        .unwrap();
                    }
                    Ok(true)
                }
            }
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
        Ok(ordinary_own_property_keys(self))
    }
    fn is_array_object(&self) -> bool {
        true
    }
    fn to_array_object(&self) -> Option<&ArrayObject> {
        Some(self)
    }
}

pub fn array_create(length: u64, proto: Option<Object>) -> Completion<Object> {
    ArrayObject::create(length, proto)
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
    pub fn create(length: u64, proto: Option<Object>) -> Completion<Object> {
        if length > 4294967295 {
            return Err(create_range_error("Array lengths greater than 4294967295 are not allowed"));
        }
        let length: u32 = length.try_into().unwrap();
        let proto = proto.unwrap_or_else(|| intrinsic(IntrinsicId::ArrayPrototype));
        let a = make_basic_object(
            &[InternalSlotName::Prototype, InternalSlotName::Extensible, InternalSlotName::ArrayMarker],
            Some(proto),
        );
        ordinary_define_own_property(
            &a,
            "length",
            PotentialPropertyDescriptor::new().value(length).writable(true).enumerable(false).configurable(false),
        )
        .unwrap();
        Ok(a)
    }

    pub fn new(prototype: Option<Object>) -> Self {
        Self { common: RefCell::new(CommonObjectData::new(prototype, true, ARRAY_OBJECT_SLOTS)) }
    }
    pub fn object(prototype: Option<Object>) -> Object {
        Object { o: Rc::new(Self::new(prototype)) }
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
    fn set_length(&self, descriptor: PotentialPropertyDescriptor) -> Completion<bool> {
        if descriptor.value.is_none() {
            return ordinary_define_own_property(self, PropertyKey::from("length"), descriptor);
        }
        let mut new_len_desc = descriptor.clone();
        let new_len = to_uint32(descriptor.value.clone().unwrap())?;
        let number_len = to_number(descriptor.value.unwrap())?;
        if !number_same_value_zero(new_len as f64, number_len) {
            return Err(create_range_error("Invalid array length"));
        }
        new_len_desc.value = Some(ECMAScriptValue::from(new_len));
        let old_len_desc = ordinary_get_own_property(self, &"length".into()).unwrap();
        let old_len_desc = DataDescriptor::try_from(old_len_desc).unwrap();
        let old_len = to_uint32(old_len_desc.value).unwrap();
        if new_len >= old_len {
            return ordinary_define_own_property(self, "length", new_len_desc);
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
        let succeeded = ordinary_define_own_property(self, "length", new_len_desc.clone()).unwrap();
        if !succeeded {
            return Ok(false);
        }
        let mut keys: Vec<PropertyKey>;
        {
            let data = self.common.borrow();
            keys = data
                .properties
                .keys()
                .filter(|key| ArrayIndex::try_from(*key).map_or(false, |idx| u32::from(idx) >= new_len))
                .cloned()
                .collect();
        }
        keys.sort_by_cached_key(|key| ArrayIndex::try_from(key).unwrap());
        for p in keys.into_iter().rev() {
            let delete_succeeded = self.delete(&p).unwrap();
            if !delete_succeeded {
                new_len_desc.value = Some(ECMAScriptValue::from(to_uint32(p).unwrap() + 1));
                if !new_writable {
                    new_len_desc.writable = Some(false);
                }
                ordinary_define_own_property(self, "length", new_len_desc).unwrap();
                return Ok(false);
            }
        }
        if !new_writable {
            ordinary_define_own_property(self, "length", PotentialPropertyDescriptor::new().writable(false)).unwrap();
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
pub fn array_species_create(original_array: &Object, length: u64) -> Completion<ECMAScriptValue> {
    let is_array = original_array.is_array()?;
    if !is_array {
        return Ok(ArrayObject::create(length, None)?.into());
    }
    let mut c = original_array.get(&"constructor".into())?;
    if is_constructor(&c) {
        let c_obj = Object::try_from(&c).unwrap();
        let this_realm = current_realm_record().unwrap();
        let realm_c = c_obj.get_function_realm()?;
        if Rc::ptr_eq(&this_realm, &realm_c) && c_obj == realm_c.borrow().intrinsics.array {
            c = ECMAScriptValue::Undefined;
        }
    }
    if c.is_object() {
        let c_obj = Object::try_from(&c).unwrap();
        let species = c_obj.get(&wks(WksId::Species).into())?;
        if species.is_null() {
            c = ECMAScriptValue::Undefined;
        } else {
            c = species;
        }
    }
    if c.is_undefined() {
        return Ok(ArrayObject::create(length, None)?.into());
    }
    if !is_constructor(&c) {
        return Err(create_type_error("Array species constructor invalid"));
    }
    let c_obj = Object::try_from(&c).unwrap();
    construct(&c_obj, &[length.into()], None)
}

pub fn provision_array_intrinsic(realm: &Rc<RefCell<Realm>>) {
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
                &array_constructor,
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
    constructor_function!(array_from, "from", 1.0);
    constructor_function!(array_is_array, "isArray", 1.0);
    constructor_function!(array_of, "of", 0.0);
    let species_sym = wks(WksId::Species);
    let species_fcn = create_builtin_function(
        array_species,
        false,
        0.0,
        species_sym.clone().into(),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype.clone()),
        Some("get".into()),
    );
    let species_ppd = PotentialPropertyDescriptor::new().get(species_fcn).enumerable(false).configurable(true);
    define_property_or_throw(&array_constructor, species_sym, species_ppd).unwrap();

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
    let array_prototype = ArrayObject::create(0, Some(object_prototype)).unwrap();

    // Array.prototype
    //
    // The value of Array.prototype is the Array prototype object.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    define_property_or_throw(
        &array_constructor,
        "prototype",
        PotentialPropertyDescriptor::new()
            .value(array_prototype.clone())
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
                &array_prototype,
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

    prototype_function!(array_prototype_at, "at", 1.0); // ( index )
    prototype_function!(array_prototype_concat, "concat", 1.0); // ( ...items )
    prototype_function!(array_prototype_copy_within, "copyWithin", 2.0); // ( target, start [ , end ] )
    prototype_function!(array_prototype_entries, "entries", 0.0); // ( )
    prototype_function!(array_prototype_every, "every", 1.0); // ( callbackfn [ , thisArg ] )
    prototype_function!(array_prototype_fill, "fill", 1.0); // ( value [ , start [ , end ] ] )
    prototype_function!(array_prototype_filter, "filter", 1.0); // ( callbackfn [ , thisArg ] )
    prototype_function!(array_prototype_find, "find", 1.0); // ( predicate [ , thisArg ] )
    prototype_function!(array_prototype_find_index, "findIndex", 1.0); // ( predicate [ , thisArg ] )
    prototype_function!(array_prototype_find_last, "findLast", 1.0);
    prototype_function!(array_prototype_find_last_index, "findLastIndex", 1.0);
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
    prototype_function!(array_prototype_push, "push", 1.0); // ( ...items )
    prototype_function!(array_prototype_reduce, "reduce", 1.0); // ( callbackfn [ , initialValue ] )
    prototype_function!(array_prototype_reduce_right, "reduceRight", 1.0); // ( callbackfn [ , initialValue ] )
    prototype_function!(array_prototype_reverse, "reverse", 0.0); // ( )
    prototype_function!(array_prototype_shift, "shift", 0.0); // ( )
    prototype_function!(array_prototype_slice, "slice", 2.0); // ( start, end )
    prototype_function!(array_prototype_some, "some", 1.0); // ( callbackfn [ , thisArg ] )
    prototype_function!(array_prototype_sort, "sort", 1.0); // ( comparefn )
    prototype_function!(array_prototype_splice, "splice", 2.0); // ( start, deleteCount, ...items )
    prototype_function!(array_prototype_to_locale_string, "toLocaleString", 0.0); // ( [ reserved1 [ , reserved2 ] ] )
    prototype_function!(array_prototype_to_reversed, "toReversed", 0.0); // ( )
    prototype_function!(array_prototype_to_sorted, "toSorted", 1.0); // ( comparefn )
    prototype_function!(array_prototype_to_spliced, "toSpliced", 2.0); // ( start, skipCount, ...items )
    prototype_function!(array_prototype_to_string, "toString", 0.0); // ( )
    prototype_function!(array_prototype_unshift, "unshift", 1.0); // ( ...items )
    prototype_function!(array_prototype_values, "values", 0.0); // ( )
    prototype_function!(array_prototype_with, "with", 2.0); // ( index, value )

    // Array.prototype [ @@iterator ] ( )
    // The initial value of the @@iterator property is %Array.prototype.values%,
    let array_prototype_values =
        array_prototype.get(&"values".into()).expect("a property just added should be gettable");
    let values_ppd = PotentialPropertyDescriptor::new()
        .value(array_prototype_values.clone())
        .enumerable(false)
        .writable(true)
        .configurable(true);
    define_property_or_throw(&array_prototype, wks(WksId::Iterator), values_ppd).expect("property should be ok to add");

    //prototype_function!(array_prototype_[, "[", ); // @@unscopables ]

    // Array.prototype.constructor
    //
    // The initial value of Array.prototype.constructor is %Array%.
    define_property_or_throw(
        &array_prototype,
        "constructor",
        PotentialPropertyDescriptor::new()
            .value(array_constructor.clone())
            .writable(true)
            .enumerable(false)
            .configurable(true),
    )
    .unwrap();

    realm.borrow_mut().intrinsics.array = array_constructor;
    realm.borrow_mut().intrinsics.array_prototype = array_prototype;
    realm.borrow_mut().intrinsics.array_prototype_values =
        Object::try_from(array_prototype_values).expect("values should be an object");
}

pub fn provision_array_iterator_intrinsic(realm: &Rc<RefCell<Realm>>) {
    // The %ArrayIteratorPrototype% Object
    //
    // * has properties that are inherited by all Array Iterator Objects.
    // * is an ordinary object.
    // * has a [[Prototype]] internal slot whose value is %IteratorPrototype%.
    let iterator_prototype = realm.borrow().intrinsics.iterator_prototype.clone();
    let array_iterator_prototype = ordinary_object_create(Some(iterator_prototype), &[]);

    // %ArrayIteratorPrototype% [ @@toStringTag ]
    // The initial value of the @@toStringTag property is the String value "Array Iterator".
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true }.
    let tag_ppd =
        PotentialPropertyDescriptor::new().writable(false).enumerable(false).configurable(true).value("Array Iterator");
    define_property_or_throw(&array_iterator_prototype, wks(WksId::ToStringTag), tag_ppd)
        .expect("object setup should be fine");

    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();
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
                &array_iterator_prototype,
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
    prototype_function!(array_iterator_prototype_next, "next", 0.0);

    realm.borrow_mut().intrinsics.array_iterator_prototype = array_iterator_prototype;
}

fn array_constructor_function(
    _this_value: ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array ( ...values )
    // This function performs the following steps when called:
    //
    //  1. If NewTarget is undefined, let newTarget be the active function object; else let newTarget be
    //     NewTarget.
    //  2. Let proto be ? GetPrototypeFromConstructor(newTarget, "%Array.prototype%").
    //  3. Let numberOfArgs be the number of elements in values.
    //  4. If numberOfArgs = 0, then
    //      a. Return ! ArrayCreate(0, proto).
    //  5. Else if numberOfArgs = 1, then
    //      a. Let len be values[0].
    //      b. Let array be ! ArrayCreate(0, proto).
    //      c. If len is not a Number, then
    //          i. Perform ! CreateDataPropertyOrThrow(array, "0", len).
    //          ii. Let intLen be 1𝔽.
    //      d. Else,
    //          i. Let intLen be ! ToUint32(len).
    //          ii. If SameValueZero(intLen, len) is false, throw a RangeError exception.
    //      e. Perform ! Set(array, "length", intLen, true).
    //      f. Return array.
    //  6. Else,
    //      a. Assert: numberOfArgs ≥ 2.
    //      b. Let array be ? ArrayCreate(numberOfArgs, proto).
    //      c. Let k be 0.
    //      d. Repeat, while k < numberOfArgs,
    //          i. Let Pk be ! ToString(𝔽(k)).
    //          ii. Let itemK be values[k].
    //          iii. Perform ! CreateDataPropertyOrThrow(array, Pk, itemK).
    //          iv. Set k to k + 1.
    //      e. Assert: The mathematical value of array's "length" property is numberOfArgs.
    //      f. Return array.
    let nt = match new_target {
        Some(obj) => obj.clone(),
        None => active_function_object().expect("we should be inside a function (the array constructor, actually)"),
    };
    let proto = nt.get_prototype_from_constructor(IntrinsicId::ArrayPrototype)?;
    let number_of_args = arguments.len() as u64;
    match number_of_args {
        0 => array_create(0, Some(proto)).map(ECMAScriptValue::from),
        1 => {
            let len = arguments[0].clone();
            let array = array_create(0, Some(proto)).expect("Array creation with zero length should succeed");
            let int_len = match len {
                ECMAScriptValue::Number(len) => {
                    let int_len = to_uint32(len).expect("number to uint32 should not fail");
                    if !number_same_value_zero(int_len as f64, len) {
                        return Err(create_range_error("Bad length in array construction"));
                    }
                    int_len
                }
                _ => {
                    array.create_data_property_or_throw("0", len).expect("Property creation should be successful");
                    1
                }
            };
            array.set("length", int_len, true).expect("Set should succeed");
            Ok(array.into())
        }
        _ => {
            let array = array_create(number_of_args, Some(proto))
                .expect("it takes 96 GB to hold a number of args big enough to fail. we won't get there.");
            for k in 0..number_of_args {
                let pk = format!("{k}");
                array
                    .create_data_property_or_throw(pk, arguments[k as usize].clone())
                    .expect("property creation should succeed");
            }
            Ok(array.into())
        }
    }
}

fn array_from(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_is_array(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.isArray ( arg )
    // This function performs the following steps when called:
    //
    //  1. Return ? IsArray(arg).
    let mut args = FuncArgs::from(arguments);
    let arg = args.next_arg();
    arg.is_array().map(ECMAScriptValue::from)
}
fn array_of(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_species(
    this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // get Array [ @@species ]
    // Array[@@species] is an accessor property whose set accessor function is undefined. Its get accessor
    // function performs the following steps when called:
    //
    //  1. Return the this value.
    Ok(this_value)
}
fn array_prototype_at(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_concat(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_copy_within(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_entries(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_every(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_fill(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_filter(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_find(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_find_index(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_find_last(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_find_last_index(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_flat(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_flat_map(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_for_each(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_includes(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_index_of(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

// Array.prototype.join ( separator )
//
// This method converts the elements of the array to Strings, and then concatenates these Strings, separated
// by occurrences of the separator. If no separator is provided, a single comma is used as the separator.
//
// It performs the following steps when called:
//
//  1. Let O be ? ToObject(this value).
//  2. Let len be ? LengthOfArrayLike(O).
//  3. If separator is undefined, let sep be ",".
//  4. Else, let sep be ? ToString(separator).
//  5. Let R be the empty String.
//  6. Let k be 0.
//  7. Repeat, while k < len,
//      a. If k > 0, set R to the string-concatenation of R and sep.
//      b. Let element be ? Get(O, ! ToString(𝔽(k))).
//      c. If element is either undefined or null, let next be the empty String; otherwise, let next be ? ToString(element).
//      d. Set R to the string-concatenation of R and next.
//      e. Set k to k + 1.
//  8. Return R.
fn array_prototype_join(
    this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let separator = args.next_arg();
    let o = to_object(this_value)?;
    let len = length_of_array_like(&o)?;

    let sep = if separator.is_undefined() { JSString::from(",") } else { to_string(separator)? };
    let mut r = JSString::from("");
    let mut k = 0;
    while k < len {
        if k > 0 {
            r = r.concat(sep.clone());
        }
        let element = o.get(&to_string(k).expect("numbers should be string-able").into())?;
        let next = if element.is_undefined() || element.is_null() { JSString::from("") } else { to_string(element)? };
        r = r.concat(next);
        k += 1;
    }
    Ok(ECMAScriptValue::from(r))
}

fn array_prototype_keys(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_last_index_of(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_map(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_pop(
    this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.pop ( )
    // NOTE 1 This method removes the last element of the array and returns it.
    //
    // This method performs the following steps when called:
    //
    //  1. Let O be ? ToObject(this value).
    //  2. Let len be ? LengthOfArrayLike(O).
    //  3. If len = 0, then
    //      a. Perform ? Set(O, "length", +0𝔽, true).
    //      b. Return undefined.
    //  4. Else,
    //      a. Assert: len > 0.
    //      b. Let newLen be 𝔽(len - 1).
    //      c. Let index be ! ToString(newLen).
    //      d. Let element be ? Get(O, index).
    //      e. Perform ? DeletePropertyOrThrow(O, index).
    //      f. Perform ? Set(O, "length", newLen, true).
    //      g. Return element.
    // NOTE 2 This method is intentionally generic; it does not require that its this value be an Array.
    // Therefore it can be transferred to other kinds of objects for use as a method.
    let o = to_object(this_value)?;
    let len = length_of_array_like(&o)?;
    if len == 0 {
        o.set("length", 0.0, true)?;
        Ok(ECMAScriptValue::Undefined)
    } else {
        let newlen = len - 1;
        let index = PropertyKey::from(format!("{newlen}"));
        let element = o.get(&index)?;
        o.delete_property_or_throw(&index)?;
        o.set("length", newlen, true)?;
        Ok(element)
    }
}

fn array_prototype_push(
    this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.push ( ...items )
    // NOTE 1 This method appends the arguments to the end of the array, in the order in which they appear. It
    // returns the new length of the array.
    //
    // This method performs the following steps when called:
    //
    //  1. Let O be ? ToObject(this value).
    //  2. Let len be ? LengthOfArrayLike(O).
    //  3. Let argCount be the number of elements in items.
    //  4. If len + argCount > 2^53 - 1, throw a TypeError exception.
    //  5. For each element E of items, do
    //      a. Perform ? Set(O, ! ToString(𝔽(len)), E, true).
    //      b. Set len to len + 1.
    //  6. Perform ? Set(O, "length", 𝔽(len), true).
    //  7. Return 𝔽(len).
    // The "length" property of this method is 1𝔽.
    //
    // NOTE 2 This method is intentionally generic; it does not require that its this value be an Array.
    // Therefore it can be transferred to other kinds of objects for use as a method.
    let o = to_object(this_value)?;
    let len = length_of_array_like(&o)? as usize;
    let arg_count = arguments.len();
    let new_len = len + arg_count;
    if new_len >= 1 << 53 {
        return Err(create_type_error("Array too large"));
    }
    for (idx, e) in arguments.iter().cloned().enumerate() {
        o.set(PropertyKey::from(format!("{}", len + idx)), e, true)?;
    }
    o.set("length", new_len, true)?;
    Ok(new_len.into())
}

fn array_prototype_reduce(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_reduce_right(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_reverse(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_shift(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_slice(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_some(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_sort(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_splice(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_to_locale_string(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_to_reversed(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_to_sorted(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_to_spliced(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

// Array.prototype.toString ( )
//
// This method performs the following steps when called:
//
//  1. Let array be ? ToObject(this value).
//  2. Let func be ? Get(array, "join").
//  3. If IsCallable(func) is false, set func to the intrinsic function %Object.prototype.toString%.
//  4. Return ? Call(func, array).
fn array_prototype_to_string(
    this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let array = to_object(this_value)?;
    let mut func = array.get(&"join".into())?;
    if !is_callable(&func) {
        func = ECMAScriptValue::from(intrinsic(IntrinsicId::ObjectPrototypeToString));
    }
    call(&func, &ECMAScriptValue::from(array), &[])
}

fn array_prototype_unshift(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

// Array.prototype.values ( )
//
// This method performs the following steps when called:
//
//  1. Let O be ? ToObject(this value).
//  2. Return CreateArrayIterator(O, value).
fn array_prototype_values(
    this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let o = to_object(this_value)?;
    Ok(ECMAScriptValue::from(create_array_iterator(o, KeyValueKind::Value)))
}

fn array_prototype_with(
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}

// Array Iterator Objects
// An Array Iterator is an object, that represents a specific iteration over some specific Array instance
// object. There is not a named constructor for Array Iterator objects. Instead, Array iterator objects are
// created by calling certain methods of Array instance objects.

fn array_iterator_prototype_next(
    this_value: ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // %ArrayIteratorPrototype%.next ( )
    //  1. Return ? GeneratorResume(this value, empty, "%ArrayIteratorPrototype%").
    generator_resume(this_value, ECMAScriptValue::Undefined, "%ArrayIteratorPrototype%")
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum KeyValueKind {
    Key,
    Value,
    KeyValue,
}

async fn array_iterator(
    co: Co<ECMAScriptValue, Completion<ECMAScriptValue>>,
    array: Object,
    kind: KeyValueKind,
) -> Completion<ECMAScriptValue> {
    // a. Let index be 0.
    // b. Repeat,
    //     i. If array has a [[TypedArrayName]] internal slot, then
    //         1. If IsDetachedBuffer(array.[[ViewedArrayBuffer]]) is true, throw a TypeError exception.
    //         2. Let len be array.[[ArrayLength]].
    //    ii. Else,
    //         1. Let len be ? LengthOfArrayLike(array).
    //   iii. If index ≥ len, return NormalCompletion(undefined).
    //    iv. If kind is key, perform ? GeneratorYield(CreateIterResultObject(𝔽(index), false)).
    //     v. Else,
    //         1. Let elementKey be ! ToString(𝔽(index)).
    //         2. Let elementValue be ? Get(array, elementKey).
    //         3. If kind is value, perform ? GeneratorYield(CreateIterResultObject(elementValue, false)).
    //         4. Else,
    //             a. Assert: kind is key+value.
    //             b. Let result be CreateArrayFromList(« 𝔽(index), elementValue »).
    //             c. Perform ? GeneratorYield(CreateIterResultObject(result, false)).
    //    vi. Set index to index + 1.
    let mut index = 0;
    loop {
        assert!(!array.is_typed_array()); // when typed arrays are added, this needs to be accounted for
        let len = length_of_array_like(&array)?;
        if index >= len {
            return Ok(ECMAScriptValue::Undefined);
        }
        if kind == KeyValueKind::Key {
            let res = ECMAScriptValue::from(create_iter_result_object(ECMAScriptValue::from(index), false));
            generator_yield(&co, res).await?;
        } else {
            let element_key = to_string(index).expect("numbers should always have string representations");
            let element_value = array.get(&element_key.into())?;
            if kind == KeyValueKind::Value {
                let res = ECMAScriptValue::from(create_iter_result_object(element_value, false));
                generator_yield(&co, res).await?;
            } else {
                let result =
                    ECMAScriptValue::from(create_array_from_list(&[ECMAScriptValue::from(index), element_value]));
                let res = ECMAScriptValue::from(create_iter_result_object(result, false));
                generator_yield(&co, res).await?;
            }
        }
        index += 1;
    }
}

pub fn create_array_iterator(array: Object, kind: KeyValueKind) -> Object {
    // CreateArrayIterator ( array, kind )
    // The abstract operation CreateArrayIterator takes arguments array (an Object) and kind (key+value, key,
    // or value) and returns a Generator. It is used to create iterator objects for Array methods that return
    // such iterators. It performs the following steps when called:
    //
    //  1. Let closure be a new Abstract Closure with no parameters that captures kind and array and performs
    //     the following steps when called:
    //      a. Let index be 0.
    //      b. Repeat,
    //          i. If array has a [[TypedArrayName]] internal slot, then
    //              1. If IsDetachedBuffer(array.[[ViewedArrayBuffer]]) is true, throw a TypeError exception.
    //              2. Let len be array.[[ArrayLength]].
    //         ii. Else,
    //              1. Let len be ? LengthOfArrayLike(array).
    //        iii. If index ≥ len, return NormalCompletion(undefined).
    //         iv. If kind is key, perform ? GeneratorYield(CreateIterResultObject(𝔽(index), false)).
    //          v. Else,
    //              1. Let elementKey be ! ToString(𝔽(index)).
    //              2. Let elementValue be ? Get(array, elementKey).
    //              3. If kind is value, perform ? GeneratorYield(CreateIterResultObject(elementValue, false)).
    //              4. Else,
    //                  a. Assert: kind is key+value.
    //                  b. Let result be CreateArrayFromList(« 𝔽(index), elementValue »).
    //                  c. Perform ? GeneratorYield(CreateIterResultObject(result, false)).
    //         vi. Set index to index + 1.
    //  2. Return CreateIteratorFromClosure(closure, "%ArrayIteratorPrototype%", %ArrayIteratorPrototype%).
    let closure = move |co| array_iterator(co, array, kind);

    create_iterator_from_closure(
        asyncfn_wrap(closure),
        "%ArrayIteratorPrototype%",
        Some(intrinsic(IntrinsicId::ArrayIteratorPrototype)),
    )
}

#[cfg(test)]
mod tests;
