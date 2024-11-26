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

const ARRAY_INDEX_LIMIT: f64 = 9_007_199_254_740_991.0; // (2 ^ 53) - 1

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
            let old_len = old_len_desc.value.to_uint32().unwrap();
            let index = p.to_uint32();
            if index >= old_len && !old_len_desc.writable {
                Ok(false)
            } else {
                let succeeded = ordinary_define_own_property(self, PropertyKey::from(p), desc).unwrap();
                if succeeded {
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
                } else {
                    Ok(false)
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
    fn kind(&self) -> ObjectTag {
        ObjectTag::Array
    }
}

pub fn array_create(length: f64, proto: Option<Object>) -> Completion<Object> {
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
    pub fn create(length: f64, proto: Option<Object>) -> Completion<Object> {
        if length > 4_294_967_295.0 {
            return Err(create_range_error("Array lengths greater than 4294967295 are not allowed"));
        }
        let proto = proto.unwrap_or_else(|| intrinsic(IntrinsicId::ArrayPrototype));
        let a = ArrayObject::object(Some(proto));
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
        let new_len = descriptor.value.as_ref().unwrap().to_uint32()?;
        let number_len = descriptor.value.as_ref().unwrap().to_number()?;
        if !number_same_value_zero(f64::from(new_len), number_len) {
            return Err(create_range_error("Invalid array length"));
        }
        new_len_desc.value = Some(ECMAScriptValue::from(new_len));
        let old_len_desc = ordinary_get_own_property(self, &"length".into()).unwrap();
        let old_len_desc = DataDescriptor::try_from(old_len_desc).unwrap();
        let old_len = old_len_desc.value.to_uint32().unwrap();
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
                new_len_desc.value = Some(ECMAScriptValue::from(ECMAScriptValue::from(p).to_uint32().unwrap() + 1));
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
pub fn array_species_create(original_array: &Object, length: f64) -> Completion<ECMAScriptValue> {
    let is_array = original_array.is_array()?;
    if !is_array {
        return Ok(ArrayObject::create(length, None)?.into());
    }
    let mut c = original_array.get(&"constructor".into())?;
    if is_constructor(&c) {
        let c_obj = Object::try_from(&c).unwrap();
        let this_realm = current_realm_record().unwrap();
        let realm_c = c_obj.get_function_realm()?;
        if !Rc::ptr_eq(&this_realm, &realm_c) && c_obj == realm_c.borrow().intrinsics.array {
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
impl Object {
    pub fn array_species_create(&self, length: f64) -> Completion<ECMAScriptValue> {
        array_species_create(self, length)
    }
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
        Box::new(array_constructor_function),
        Some(ConstructorKind::Base),
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
        Box::new(array_species),
        None,
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
    let array_prototype = ArrayObject::create(0.0, Some(object_prototype)).unwrap();

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
    let array_iterator_prototype = ordinary_object_create(Some(iterator_prototype));

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
    _this_value: &ECMAScriptValue,
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
    let number_of_args = arguments.len();
    match number_of_args {
        0 => array_create(0.0, Some(proto)).map(ECMAScriptValue::from),
        1 => {
            let len = arguments[0].clone();
            let array = array_create(0.0, Some(proto)).expect("Array creation with zero length should succeed");
            let int_len = match len {
                ECMAScriptValue::Number(len) => {
                    let int_len = to_uint32_f64(len);
                    if !number_same_value_zero(f64::from(int_len), len) {
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
            let arg_count = to_f64(number_of_args).unwrap();
            let array = array_create(arg_count, Some(proto))
                .expect("it takes 96 GB to hold a number of args big enough to fail. we won't get there.");
            for (k, arg) in arguments.iter().enumerate().take(number_of_args) {
                array.create_data_property_or_throw(k, arg.clone()).expect("property creation should succeed");
            }
            Ok(array.into())
        }
    }
}

fn array_from(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.from ( items [ , mapper [ , thisArg ] ] )
    // This method performs the following steps when called:
    //
    // 1. Let C be the this value.
    // 2. If mapper is undefined, then
    //    a. Let mapping be false.
    // 3. Else,
    //    a. If IsCallable(mapper) is false, throw a TypeError exception.
    //    b. Let mapping be true.
    // 4. Let usingIterator be ? GetMethod(items, %Symbol.iterator%).
    // 5. If usingIterator is not undefined, then
    //    a. If IsConstructor(C) is true, then
    //       i. Let A be ? Construct(C).
    //    b. Else,
    //       i. Let A be ! ArrayCreate(0).
    //    c. Let iteratorRecord be ? GetIteratorFromMethod(items, usingIterator).
    //    d. Let k be 0.
    //    e. Repeat,
    //       i. If k ≥ 2**53 - 1, then
    //          1. Let error be ThrowCompletion(a newly created TypeError object).
    //          2. Return ? IteratorClose(iteratorRecord, error).
    //       ii. Let Pk be ! ToString(𝔽(k)).
    //       iii. Let next be ? IteratorStepValue(iteratorRecord).
    //       iv. If next is done, then
    //           1. Perform ? Set(A, "length", 𝔽(k), true).
    //           2. Return A.
    //       v. If mapping is true, then
    //          1. Let mappedValue be Completion(Call(mapper, thisArg, « next, 𝔽(k) »)).
    //          2. IfAbruptCloseIterator(mappedValue, iteratorRecord).
    //       vi. Else,
    //           1. Let mappedValue be next.
    //       vii. Let defineStatus be Completion(CreateDataPropertyOrThrow(A, Pk, mappedValue)).
    //       viii. IfAbruptCloseIterator(defineStatus, iteratorRecord).
    //       ix. Set k to k + 1.
    // 6. NOTE: items is not iterable so assume it is an array-like object.
    // 7. Let arrayLike be ! ToObject(items).
    // 8. Let len be ? LengthOfArrayLike(arrayLike).
    // 9. If IsConstructor(C) is true, then
    //    a. Let A be ? Construct(C, « 𝔽(len) »).
    // 10. Else,
    //     a. Let A be ? ArrayCreate(len).
    // 11. Let k be 0.
    // 12. Repeat, while k < len,
    //     a. Let Pk be ! ToString(𝔽(k)).
    //     b. Let kValue be ? Get(arrayLike, Pk).
    //     c. If mapping is true, then
    //        i. Let mappedValue be ? Call(mapper, thisArg, « kValue, 𝔽(k) »).
    //     d. Else,
    //        i. Let mappedValue be kValue.
    //     e. Perform ? CreateDataPropertyOrThrow(A, Pk, mappedValue).
    //     f. Set k to k + 1.
    // 13. Perform ? Set(A, "length", 𝔽(len), true).
    // 14. Return A.
    //
    // Note
    // This method is an intentionally generic factory method; it does not require that its this value be the Array
    // constructor. Therefore it can be transferred to or inherited by any other constructors that may be called with a
    // single numeric argument.
    let mut args = FuncArgs::from(arguments);
    let items = args.next_arg();
    let mapper = args.next_arg();
    let this_arg = args.next_arg();

    let c = this_value;
    let mapping = if mapper == ECMAScriptValue::Undefined {
        false
    } else {
        if !mapper.is_callable() {
            return Err(create_type_error("Array.prototype.from requires a callable mapper"));
        }
        true
    };
    let using_iterator = items.get_method(&PropertyKey::from(wks(WksId::Iterator)))?;
    if using_iterator != ECMAScriptValue::Undefined {
        let a = if c.is_constructor() {
            let cstr: &Object = c.try_into().expect("things that are constructors should be objects");
            TryInto::<Object>::try_into(construct(cstr, &[], None)?)
                .expect("things constructors make should be objects")
        } else {
            array_create(0.0, None).expect("arrays of length zero should always be constructable")
        };
        let iterator_record = get_iterator_from_method(&items, &using_iterator)?;
        let mut k: usize = 0;
        loop {
            if k >= (2 ^ 53) - 1 {
                let error = Err(create_type_error("Array.from: iterable too long"));
                return iterator_close(&iterator_record, error);
            }
            let pk = PropertyKey::from(k);
            let next = iterator_step_value(&iterator_record)?;
            if let Some(next) = next {
                let mapped_value = if mapping {
                    match call(&mapper, &this_arg, &[next.clone(), ECMAScriptValue::from(k)]) {
                        Err(err) => {
                            return iterator_close(&iterator_record, Err(err));
                        }
                        Ok(val) => val,
                    }
                } else {
                    next
                };
                let define_status = a.create_data_property_or_throw(pk, mapped_value);
                if let Err(err) = define_status {
                    return iterator_close(&iterator_record, Err(err));
                }
                k += 1;
            } else {
                a.set("length", k, true)?;
                return Ok(ECMAScriptValue::Object(a));
            }
        }
    }
    let array_like = to_object(items).expect("we should have already aborted if this isn't convertable to an object");
    let len = array_like.length_of_array_like()?;
    let a = if c.is_constructor() {
        let cstr: &Object = c.try_into().expect("things that are constructors should be objects");
        TryInto::<Object>::try_into(construct(cstr, &[ECMAScriptValue::from(len)], None)?)
            .expect("things constructors make should be objects")
    } else {
        array_create(len, None)?
    };
    let mut k = 0.0;
    while k < len {
        let pk = PropertyKey::from(k);
        let kvalue = array_like.get(&pk)?;
        let mapped_value =
            if mapping { call(&mapper, &this_arg.clone(), &[kvalue, ECMAScriptValue::from(k)])? } else { kvalue };
        a.create_data_property_or_throw(pk, mapped_value)?;
        k += 1.0;
    }
    a.set("length", k, true)?;

    Ok(ECMAScriptValue::Object(a))
}

fn array_is_array(
    _this_value: &ECMAScriptValue,
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
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.of ( ...items )
    // This method performs the following steps when called:
    //
    // 1. Let len be the number of elements in items.
    // 2. Let lenNumber be 𝔽(len).
    // 3. Let C be the this value.
    // 4. If IsConstructor(C) is true, then
    //    a. Let A be ? Construct(C, « lenNumber »).
    // 5. Else,
    //    a. Let A be ? ArrayCreate(len).
    // 6. Let k be 0.
    // 7. Repeat, while k < len,
    //    a. Let kValue be items[k].
    //    b. Let Pk be ! ToString(𝔽(k)).
    //    c. Perform ? CreateDataPropertyOrThrow(A, Pk, kValue).
    //    d. Set k to k + 1.
    // 8. Perform ? Set(A, "length", lenNumber, true).
    // 9. Return A.
    //
    // Note
    // This method is an intentionally generic factory method; it does not require that its this value be the Array
    // constructor. Therefore it can be transferred to or inherited by other constructors that may be called with a
    // single numeric argument.
    let len = arguments.len();
    let len_number = ECMAScriptValue::from(len);
    let c = this_value;
    let a = if c.is_constructor() {
        let cstr: &Object = c.try_into().expect("things that are constructors should be objects");
        TryInto::<Object>::try_into(construct(cstr, &[len_number], None)?)
            .expect("things constructors make should be objects")
    } else {
        array_create(to_f64(len).unwrap(), None)?
    };
    let mut k = 0;
    while k < len {
        let kvalue = &arguments[k];
        let pk = PropertyKey::from(k);
        a.create_data_property_or_throw(pk, kvalue.clone())?;
        k += 1;
    }
    a.set("length", len, true)?;
    Ok(ECMAScriptValue::Object(a))
}

#[expect(clippy::unnecessary_wraps)]
fn array_species(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // get Array [ @@species ]
    // Array[@@species] is an accessor property whose set accessor function is undefined. Its get accessor
    // function performs the following steps when called:
    //
    //  1. Return the this value.
    Ok(this_value.clone())
}

fn array_prototype_at(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.at ( index )
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. Let relativeIndex be ? ToIntegerOrInfinity(index).
    // 4. If relativeIndex ≥ 0, then
    //    a. Let k be relativeIndex.
    // 5. Else,
    //    a. Let k be len + relativeIndex.
    // 6. If k < 0 or k ≥ len, return undefined.
    // 7. Return ? Get(O, ! ToString(𝔽(k))).
    let mut args = FuncArgs::from(arguments);
    let index = args.next_arg();
    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    let relative_index = index.to_integer_or_infinity()?;
    let k = if relative_index >= 0.0 { relative_index } else { len + relative_index };
    if k < 0.0 || k >= len {
        return Ok(ECMAScriptValue::Undefined);
    }
    o.get(&PropertyKey::from(k))
}

fn array_prototype_concat(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.concat ( ...items )
    // This method returns an array containing the array elements of the object followed by the array elements of each
    // argument.
    //
    // It performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let A be ? ArraySpeciesCreate(O, 0).
    // 3. Let n be 0.
    // 4. Prepend O to items.
    // 5. For each element E of items, do
    //    a. Let spreadable be ? IsConcatSpreadable(E).
    //    b. If spreadable is true, then
    //       i. Let len be ? LengthOfArrayLike(E).
    //       ii. If n + len > 2**53 - 1, throw a TypeError exception.
    //       iii. Let k be 0.
    //       iv. Repeat, while k < len,
    //           1. Let Pk be ! ToString(𝔽(k)).
    //           2. Let exists be ? HasProperty(E, Pk).
    //           3. If exists is true, then
    //              a. Let subElement be ? Get(E, Pk).
    //              b. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), subElement).
    //           4. Set n to n + 1.
    //           5. Set k to k + 1.
    //    c. Else,
    //       i. NOTE: E is added as a single item rather than spread.
    //       ii. If n ≥ 2**53 - 1, throw a TypeError exception.
    //       iii. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), E).
    //       iv. Set n to n + 1.
    // 6. Perform ? Set(A, "length", 𝔽(n), true).
    // 7. Return A.
    // The "length" property of this method is 1𝔽.
    //
    // Note 1
    // The explicit setting of the "length" property in step 6 is intended to ensure the length is correct when the
    // final non-empty element of items has trailing holes or when A is not a built-in Array.
    //
    // Note 2
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore it can be
    // transferred to other kinds of objects for use as a method.
    let obj = to_object(this_value.clone())?;
    let array = array_species_create(&obj, 0.0)?;
    let array_obj = array.object_ref().expect("species of array should be objects");
    let mut output_index = 0.0;
    for elem in [ECMAScriptValue::Object(obj)].iter().chain(arguments.iter()) {
        let spreadable = elem.is_concat_spreadable()?;
        if spreadable {
            let elem_obj = elem.object_ref().expect("spreadable things should be objects");
            let len = elem_obj.length_of_array_like()?;
            if len + output_index > ARRAY_INDEX_LIMIT {
                return Err(create_type_error("Array.prototype.concat: iterable too long"));
            }
            let mut k = 0.0;
            while k < len {
                let pk = PropertyKey::from(k);
                if elem_obj.o.has_property(&pk)? {
                    let sub_element = elem_obj.get(&pk)?;
                    array_obj.create_data_property_or_throw(PropertyKey::from(output_index), sub_element)?;
                }
                output_index += 1.0;
                k += 1.0;
            }
        } else {
            if output_index >= ARRAY_INDEX_LIMIT {
                return Err(create_type_error("Array.prototype.concat: iterable too long"));
            }
            array_obj.create_data_property_or_throw(PropertyKey::from(output_index), elem.clone())?;
            output_index += 1.0;
        }
    }
    array_obj.set("length", output_index, true)?;
    Ok(array)
}

impl ECMAScriptValue {
    pub fn is_concat_spreadable(&self) -> Completion<bool> {
        // IsConcatSpreadable ( O )
        // The abstract operation IsConcatSpreadable takes argument O (an ECMAScript language value) and returns either
        // a normal completion containing a Boolean or a throw completion. It performs the following steps when called:
        //
        //  1. If O is not an Object, return false.
        //  2. Let spreadable be ? Get(O, %Symbol.isConcatSpreadable%).
        //  3. If spreadable is not undefined, return ToBoolean(spreadable).
        //  4. Return ? IsArray(O).
        match self {
            ECMAScriptValue::Object(o) => {
                let spreadable = o.get(&PropertyKey::from(wks(WksId::IsConcatSpreadable)))?;
                match spreadable {
                    ECMAScriptValue::Undefined => o.is_array(),
                    _ => Ok(to_boolean(spreadable)),
                }
            }
            _ => Ok(false),
        }
    }
}

fn array_prototype_copy_within(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.copyWithin ( target, start [ , end ] )
    // Note 1
    // The end argument is optional. If it is not provided, the length of the this value is used.
    //
    // Note 2
    // If target is negative, it is treated as length + target where length is the length of the array. If start is
    // negative, it is treated as length + start. If end is negative, it is treated as length + end.
    //
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. Let relativeTarget be ? ToIntegerOrInfinity(target).
    // 4. If relativeTarget = -∞, let to be 0.
    // 5. Else if relativeTarget < 0, let to be max(len + relativeTarget, 0).
    // 6. Else, let to be min(relativeTarget, len).
    // 7. Let relativeStart be ? ToIntegerOrInfinity(start).
    // 8. If relativeStart = -∞, let from be 0.
    // 9. Else if relativeStart < 0, let from be max(len + relativeStart, 0).
    // 10. Else, let from be min(relativeStart, len).
    // 11. If end is undefined, let relativeEnd be len; else let relativeEnd be ? ToIntegerOrInfinity(end).
    // 12. If relativeEnd = -∞, let final be 0.
    // 13. Else if relativeEnd < 0, let final be max(len + relativeEnd, 0).
    // 14. Else, let final be min(relativeEnd, len).
    // 15. Let count be min(final - from, len - to).
    // 16. If from < to and to < from + count, then
    //     a. Let direction be -1.
    //     b. Set from to from + count - 1.
    //     c. Set to to to + count - 1.
    // 17. Else,
    //     a. Let direction be 1.
    // 18. Repeat, while count > 0,
    //     a. Let fromKey be ! ToString(𝔽(from)).
    //     b. Let toKey be ! ToString(𝔽(to)).
    //     c. Let fromPresent be ? HasProperty(O, fromKey).
    //     d. If fromPresent is true, then
    //        i. Let fromValue be ? Get(O, fromKey).
    //        ii. Perform ? Set(O, toKey, fromValue, true).
    //     e. Else,
    //        i. Assert: fromPresent is false.
    //        ii. Perform ? DeletePropertyOrThrow(O, toKey).
    //     f. Set from to from + direction.
    //     g. Set to to to + direction.
    //     h. Set count to count - 1.
    // 19. Return O.
    //
    // Note 3
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore it can be
    // transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let target = args.next_arg();
    let start = args.next_arg();
    let end = args.next_arg();

    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    let relative_target = target.to_integer_or_infinity()?;
    let to = if relative_target < 0.0 { (len + relative_target).max(0.0) } else { relative_target.min(len) };
    let relative_start = start.to_integer_or_infinity()?;
    let from = if relative_start < 0.0 { (len + relative_start).max(0.0) } else { relative_start.min(len) };
    let relative_end = if end.is_undefined() { len } else { end.to_integer_or_infinity()? };
    let final_spot = if relative_end < 0.0 { (len + relative_end).max(0.0) } else { relative_end.min(len) };
    let mut count = (final_spot - from).min(len - to);
    let (direction, mut from, mut to) = if ((from + 1.0)..(from + count)).contains(&to) {
        (-1.0, from + count - 1.0, to + count - 1.0)
    } else {
        (1.0, from, to)
    };
    while count > 0.0 {
        let from_key = PropertyKey::from(from);
        let to_key = PropertyKey::from(to);
        let from_present = o.o.has_property(&from_key)?;
        if from_present {
            let from_value = o.get(&from_key)?;
            o.set(to_key, from_value, true)?;
        } else {
            o.delete_property_or_throw(&to_key)?;
        }
        from += direction;
        to += direction;
        count -= 1.0;
    }

    Ok(ECMAScriptValue::Object(o))
}

fn array_prototype_entries(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.entries ( )
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Return CreateArrayIterator(O, key+value).
    let o = to_object(this_value.clone())?;
    Ok(ECMAScriptValue::Object(create_array_iterator(o, KeyValueKind::KeyValue)))
}
fn array_prototype_every(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.every ( callback [ , thisArg ] )
    // Note 1
    // callback should be a function that accepts three arguments and returns a value that is coercible to a Boolean
    // value. every calls callback once for each element present in the array, in ascending order, until it finds one
    // where callback returns false. If such an element is found, every immediately returns false. Otherwise, every
    // returns true. callback is called only for elements of the array which actually exist; it is not called for
    // missing elements of the array.
    //
    // If a thisArg parameter is provided, it will be used as the this value for each invocation of callback. If it is
    // not provided, undefined is used instead.
    //
    // callback is called with three arguments: the value of the element, the index of the element, and the object being
    // traversed.
    //
    // every does not directly mutate the object on which it is called but the object may be mutated by the calls to
    // callback.
    //
    // The range of elements processed by every is set before the first call to callback. Elements which are appended to
    // the array after the call to every begins will not be visited by callback. If existing elements of the array are
    // changed, their value as passed to callback will be the value at the time every visits them; elements that are
    // deleted after the call to every begins and before being visited are not visited. every acts like the "for all"
    // quantifier in mathematics. In particular, for an empty array, it returns true.
    //
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. If IsCallable(callback) is false, throw a TypeError exception.
    // 4. Let k be 0.
    // 5. Repeat, while k < len,
    //    a. Let Pk be ! ToString(𝔽(k)).
    //    b. Let kPresent be ? HasProperty(O, Pk).
    //    c. If kPresent is true, then
    //       i. Let kValue be ? Get(O, Pk).
    //       ii. Let testResult be ToBoolean(? Call(callback, thisArg, « kValue, 𝔽(k), O »)).
    //       iii. If testResult is false, return false.
    //    d. Set k to k + 1.
    // 6. Return true.
    //
    // Note 2
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore it can be
    // transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let callback = args.next_arg();
    let this_arg = args.next_arg();
    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    if !callback.is_callable() {
        return Err(create_type_error("Array.prototype.every: callback is not callable"));
    }
    let mut k = 0.0;
    while k < len {
        let pk = PropertyKey::from(k);
        if o.o.has_property(&pk)? {
            let k_value = o.get(&pk)?;
            let test_result =
                call(&callback, &this_arg, &[k_value, ECMAScriptValue::from(k), ECMAScriptValue::from(o.clone())])?
                    .to_boolean();
            if !test_result {
                return Ok(ECMAScriptValue::Boolean(false));
            }
        }
        k += 1.0;
    }
    Ok(ECMAScriptValue::Boolean(true))
}

fn array_prototype_fill(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.fill ( value [ , start [ , end ] ] )
    // Note 1
    // The start argument is optional. If it is not provided, +0𝔽 is used.
    //
    // The end argument is optional. If it is not provided, the length of the this value is used.
    //
    // Note 2
    // If start is negative, it is treated as length + start where length is the length of the array. If end is
    // negative, it is treated as length + end.
    //
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. Let relativeStart be ? ToIntegerOrInfinity(start).
    // 4. If relativeStart = -∞, let k be 0.
    // 5. Else if relativeStart < 0, let k be max(len + relativeStart, 0).
    // 6. Else, let k be min(relativeStart, len).
    // 7. If end is undefined, let relativeEnd be len; else let relativeEnd be ? ToIntegerOrInfinity(end).
    // 8. If relativeEnd = -∞, let final be 0.
    // 9. Else if relativeEnd < 0, let final be max(len + relativeEnd, 0).
    // 10. Else, let final be min(relativeEnd, len).
    // 11. Repeat, while k < final,
    //     a. Let Pk be ! ToString(𝔽(k)).
    //     b. Perform ? Set(O, Pk, value, true).
    //     c. Set k to k + 1.
    // 12. Return O.
    //
    // Note 3
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore it can be
    // transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let value = args.next_arg();
    let start = args.next_arg();
    let end = args.next_arg();

    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    let relative_start = start.to_integer_or_infinity()?;
    let k = if relative_start < 0.0 { (len + relative_start).max(0.0) } else { relative_start.min(len) };
    let relative_end = if end.is_undefined() { len } else { end.to_integer_or_infinity()? };
    let final_spot = if relative_end < 0.0 { (len + relative_end).max(0.0) } else { relative_end.min(len) };
    let mut k = k;
    while k < final_spot {
        let pk = PropertyKey::from(k);
        o.set(pk, value.clone(), true)?;
        k += 1.0;
    }
    Ok(ECMAScriptValue::Object(o))
}

fn array_prototype_filter(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.filter ( callback [ , thisArg ] )
    // Note 1
    // callback should be a function that accepts three arguments and returns a value that is coercible to a Boolean
    // value. filter calls callback once for each element in the array, in ascending order, and constructs a new array
    // of all the values for which callback returns true. callback is called only for elements of the array which
    // actually exist; it is not called for missing elements of the array.
    //
    // If a thisArg parameter is provided, it will be used as the this value for each invocation of callback. If it is
    // not provided, undefined is used instead.
    //
    // callback is called with three arguments: the value of the element, the index of the element, and the object being
    // traversed.
    //
    // filter does not directly mutate the object on which it is called but the object may be mutated by the calls to
    // callback.
    //
    // The range of elements processed by filter is set before the first call to callback. Elements which are appended
    // to the array after the call to filter begins will not be visited by callback. If existing elements of the array
    // are changed their value as passed to callback will be the value at the time filter visits them; elements that are
    // deleted after the call to filter begins and before being visited are not visited.
    //
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. If IsCallable(callback) is false, throw a TypeError exception.
    // 4. Let A be ? ArraySpeciesCreate(O, 0).
    // 5. Let k be 0.
    // 6. Let to be 0.
    // 7. Repeat, while k < len,
    //    a. Let Pk be ! ToString(𝔽(k)).
    //    b. Let kPresent be ? HasProperty(O, Pk).
    //    c. If kPresent is true, then
    //       i. Let kValue be ? Get(O, Pk).
    //       ii. Let selected be ToBoolean(? Call(callback, thisArg, « kValue, 𝔽(k), O »)).
    //       iii. If selected is true, then
    //            1. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(to)), kValue).
    //            2. Set to to to + 1.
    //    d. Set k to k + 1.
    // 8. Return A.
    //
    // Note 2
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore it can be
    // transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let callback = args.next_arg();
    let this_arg = args.next_arg();
    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    if !callback.is_callable() {
        return Err(create_type_error("Array.prototype.filter: callback is not callable"));
    }
    let a = array_species_create(&o, 0.0)?;
    let a_obj = a.object_ref().expect("Arrays should be objects");
    let mut k = 0.0;
    let mut to = 0.0;
    while k < len {
        let pk = PropertyKey::from(k);
        if o.o.has_property(&pk)? {
            let k_value = o.get(&pk)?;
            let selected = call(
                &callback,
                &this_arg,
                &[k_value.clone(), ECMAScriptValue::from(k), ECMAScriptValue::from(o.clone())],
            )?
            .to_boolean();
            if selected {
                a_obj.create_data_property_or_throw(PropertyKey::from(to), k_value)?;
                to += 1.0;
            }
        }
        k += 1.0;
    }
    Ok(a)
}

fn array_prototype_find(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.find ( predicate [ , thisArg ] )
    // Note 1
    // This method calls predicate once for each element of the array, in ascending index order, until it finds one
    // where predicate returns a value that coerces to true. If such an element is found, find immediately returns that
    // element value. Otherwise, find returns undefined.
    //
    // See FindViaPredicate for additional information.
    //
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. Let findRec be ? FindViaPredicate(O, len, ascending, predicate, thisArg).
    // 4. Return findRec.[[Value]].
    //
    // Note 2
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore it can be
    // transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let predicate = args.next_arg();
    let this_arg = args.next_arg();

    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    let find_rec = o.find_via_predicate(len, SearchDirection::Ascending, &predicate, &this_arg)?;
    Ok(find_rec.value)
}

fn array_prototype_find_index(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.findIndex ( predicate [ , thisArg ] )
    // Note 1
    // This method calls predicate once for each element of the array, in ascending index order, until it finds one
    // where predicate returns a value that coerces to true. If such an element is found, findIndex immediately returns
    // the index of that element value. Otherwise, findIndex returns -1.
    //
    // See FindViaPredicate for additional information.
    //
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. Let findRec be ? FindViaPredicate(O, len, ascending, predicate, thisArg).
    // 4. Return findRec.[[Index]].
    //
    // Note 2
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore it can be
    // transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let predicate = args.next_arg();
    let this_arg = args.next_arg();

    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    let find_rec = o.find_via_predicate(len, SearchDirection::Ascending, &predicate, &this_arg)?;
    Ok(ECMAScriptValue::from(find_rec.index))
}

fn array_prototype_find_last(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.findLast ( predicate [ , thisArg ] )
    // Note 1
    // This method calls predicate once for each element of the array, in descending index order, until it finds one
    // where predicate returns a value that coerces to true. If such an element is found, findLast immediately returns
    // that element value. Otherwise, findLast returns undefined.
    //
    // See FindViaPredicate for additional information.
    //
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. Let findRec be ? FindViaPredicate(O, len, descending, predicate, thisArg).
    // 4. Return findRec.[[Value]].
    //
    // Note 2
    // This method is intentionally generic; it does not require that its this value be an Array object. Therefore it
    // can be transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let predicate = args.next_arg();
    let this_arg = args.next_arg();

    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    let find_rec = o.find_via_predicate(len, SearchDirection::Descending, &predicate, &this_arg)?;
    Ok(find_rec.value)
}

fn array_prototype_find_last_index(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.findLastIndex ( predicate [ , thisArg ] )
    // Note 1
    // This method calls predicate once for each element of the array, in descending index order, until it finds one
    // where predicate returns a value that coerces to true. If such an element is found, findLastIndex immediately
    // returns the index of that element value. Otherwise, findLastIndex returns -1.
    //
    // See FindViaPredicate for additional information.
    //
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. Let findRec be ? FindViaPredicate(O, len, descending, predicate, thisArg).
    // 4. Return findRec.[[Index]].
    //
    // Note 2
    // This method is intentionally generic; it does not require that its this value be an Array object. Therefore it
    // can be transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let predicate = args.next_arg();
    let this_arg = args.next_arg();

    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    let find_rec = o.find_via_predicate(len, SearchDirection::Descending, &predicate, &this_arg)?;
    Ok(ECMAScriptValue::from(find_rec.index))
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum SearchDirection {
    Ascending,
    Descending,
}

#[derive(Debug, Clone, PartialEq)]
struct FindResult {
    value: ECMAScriptValue,
    index: f64,
}

impl Object {
    fn find_via_predicate(
        &self,
        len: f64,
        direction: SearchDirection,
        predicate: &ECMAScriptValue,
        this_arg: &ECMAScriptValue,
    ) -> Completion<FindResult> {
        // FindViaPredicate ( O, len, direction, predicate, thisArg )
        // The abstract operation FindViaPredicate takes arguments O (an Object), len (a non-negative integer),
        // direction (ascending or descending), predicate (an ECMAScript language value), and thisArg (an ECMAScript
        // language value) and returns either a normal completion containing a Record with fields [[Index]] (an integral
        // Number) and [[Value]] (an ECMAScript language value) or a throw completion.
        //
        // O should be an array-like object or a TypedArray. This operation calls predicate once for each element of O,
        // in either ascending index order or descending index order (as indicated by direction), until it finds one
        // where predicate returns a value that coerces to true. At that point, this operation returns a Record that
        // gives the index and value of the element found. If no such element is found, this operation returns a Record
        // that specifies -1𝔽 for the index and undefined for the value.
        //
        // predicate should be a function. When called for an element of the array, it is passed three arguments: the
        // value of the element, the index of the element, and the object being traversed. Its return value will be
        // coerced to a Boolean value.
        //
        // thisArg will be used as the this value for each invocation of predicate.
        //
        // This operation does not directly mutate the object on which it is called, but the object may be mutated by
        // the calls to predicate.
        //
        // The range of elements processed is set before the first call to predicate, just before the traversal begins.
        // Elements that are appended to the array after this will not be visited by predicate. If existing elements of
        // the array are changed, their value as passed to predicate will be the value at the time that this operation
        // visits them. Elements that are deleted after traversal begins and before being visited are still visited and
        // are either looked up from the prototype or are undefined.
        //
        // It performs the following steps when called:
        //
        // 1. If IsCallable(predicate) is false, throw a TypeError exception.
        // 2. If direction is ascending, then
        //    a. Let indices be a List of the integers in the interval from 0 (inclusive) to len (exclusive), in ascending order.
        // 3. Else,
        //    a. Let indices be a List of the integers in the interval from 0 (inclusive) to len (exclusive), in descending order.
        // 4. For each integer k of indices, do
        //    a. Let Pk be ! ToString(𝔽(k)).
        //    b. NOTE: If O is a TypedArray, the following invocation of Get will return a normal completion.
        //    c. Let kValue be ? Get(O, Pk).
        //    d. Let testResult be ? Call(predicate, thisArg, « kValue, 𝔽(k), O »).
        //    e. If ToBoolean(testResult) is true, return the Record { [[Index]]: 𝔽(k), [[Value]]: kValue }.
        // 5. Return the Record { [[Index]]: -1𝔽, [[Value]]: undefined }.
        if !predicate.is_callable() {
            return Err(create_type_error("Array.prototype.find: predicate is not callable"));
        }
        let (mut k, delta) = if direction == SearchDirection::Ascending { (0.0, 1.0) } else { (len - 1.0, -1.0) };
        while k >= 0.0 && k < len {
            let pk = PropertyKey::from(k);
            let k_value = self.get(&pk)?;
            let test_result = call(
                predicate,
                this_arg,
                &[k_value.clone(), ECMAScriptValue::from(k), ECMAScriptValue::from(self.clone())],
            )?
            .to_boolean();
            if test_result {
                return Ok(FindResult { value: k_value, index: k });
            }
            k += delta;
        }
        Ok(FindResult { value: ECMAScriptValue::Undefined, index: -1.0 })
    }
}

fn array_prototype_flat(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.flat ( [ depth ] )
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let sourceLen be ? LengthOfArrayLike(O).
    // 3. Let depthNum be 1.
    // 4. If depth is not undefined, then
    //    a. Set depthNum to ? ToIntegerOrInfinity(depth).
    //    b. If depthNum < 0, set depthNum to 0.
    // 5. Let A be ? ArraySpeciesCreate(O, 0).
    // 6. Perform ? FlattenIntoArray(A, O, sourceLen, 0, depthNum).
    // 7. Return A.
    let mut args = FuncArgs::from(arguments);
    let depth = args.next_arg();

    let o = to_object(this_value.clone())?;
    let source_len = o.length_of_array_like()?;
    let depth_num = if depth.is_undefined() { 1.0 } else { depth.to_integer_or_infinity()?.max(0.0) };
    let a = o.array_species_create(0.0)?;
    let a_obj = a.object_ref().expect("array species create should return an object");
    a_obj.flatten_into_array(&o, source_len, 0.0, &FlattenOpts::Depth(depth_num))?;
    Ok(a)
}

#[derive(Debug, Copy, Clone)]
pub enum FlattenOpts<'a> {
    Depth(f64),
    Mapper { mapper_function: &'a ECMAScriptValue, this_arg: &'a ECMAScriptValue },
}

impl Object {
    pub fn flatten_into_array(
        &self,
        source: &Object,
        source_len: f64,
        start: f64,
        map_or_depth: &FlattenOpts,
    ) -> Completion<f64> {
        // FlattenIntoArray ( target, source, sourceLen, start, depth [ , mapperFunction [ , thisArg ] ] )
        // The abstract operation FlattenIntoArray takes arguments target (an Object), source (an Object), sourceLen (a
        // non-negative integer), start (a non-negative integer), and depth (a non-negative integer or +∞) and optional
        // arguments mapperFunction (a function object) and thisArg (an ECMAScript language value) and returns either a
        // normal completion containing a non-negative integer or a throw completion. It performs the following steps
        // when called:
        //
        // 1. Assert: If mapperFunction is present, then IsCallable(mapperFunction) is true, thisArg is present, and depth is 1.
        // 2. Let targetIndex be start.
        // 3. Let sourceIndex be +0𝔽.
        // 4. Repeat, while ℝ(sourceIndex) < sourceLen,
        //    a. Let P be ! ToString(sourceIndex).
        //    b. Let exists be ? HasProperty(source, P).
        //    c. If exists is true, then
        //       i. Let element be ? Get(source, P).
        //       ii. If mapperFunction is present, then
        //           1. Set element to ? Call(mapperFunction, thisArg, « element, sourceIndex, source »).
        //       iii. Let shouldFlatten be false.
        //       iv. If depth > 0, then
        //           1. Set shouldFlatten to ? IsArray(element).
        //       v. If shouldFlatten is true, then
        //          1. If depth = +∞, let newDepth be +∞.
        //          2. Else, let newDepth be depth - 1.
        //          3. Let elementLen be ? LengthOfArrayLike(element).
        //          4. Set targetIndex to ? FlattenIntoArray(target, element, elementLen, targetIndex, newDepth).
        //       vi. Else,
        //           1. If targetIndex ≥ 2**53 - 1, throw a TypeError exception.
        //           2. Perform ? CreateDataPropertyOrThrow(target, ! ToString(𝔽(targetIndex)), element).
        //           3. Set targetIndex to targetIndex + 1.
        //    d. Set sourceIndex to sourceIndex + 1𝔽.
        // 5. Return targetIndex.
        let depth = match map_or_depth {
            FlattenOpts::Depth(d) => *d,
            FlattenOpts::Mapper { .. } => 1.0,
        };
        let mut target_index = start;
        let mut source_index = 0.0;
        while source_index < source_len {
            let p = PropertyKey::from(source_index);
            let exists = source.o.has_property(&p)?;
            if exists {
                let element = source.get(&p)?;
                let element = match map_or_depth {
                    FlattenOpts::Depth(_) => element,
                    FlattenOpts::Mapper { mapper_function, this_arg } => call(
                        mapper_function,
                        this_arg,
                        &[element.clone(), ECMAScriptValue::from(source_index), ECMAScriptValue::from(source.clone())],
                    )?,
                };
                let should_flatten = depth > 0.0 && element.is_array()?;
                if should_flatten {
                    let new_depth = depth - 1.0;
                    let elem_obj = element.object_ref().expect("element is an array and thus should be an object");
                    let element_len = elem_obj.length_of_array_like()?;
                    target_index =
                        self.flatten_into_array(elem_obj, element_len, target_index, &FlattenOpts::Depth(new_depth))?;
                } else {
                    if target_index >= 9_007_199_254_740_991.0 {
                        return Err(create_type_error("Array.prototype.flat: target index too large"));
                    }
                    self.create_data_property_or_throw(target_index, element)?;
                    target_index += 1.0;
                }
            }
            source_index += 1.0;
        }
        Ok(target_index)
    }
}

fn array_prototype_flat_map(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.flatMap ( mapperFunction [ , thisArg ] )
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let sourceLen be ? LengthOfArrayLike(O).
    // 3. If IsCallable(mapperFunction) is false, throw a TypeError exception.
    // 4. Let A be ? ArraySpeciesCreate(O, 0).
    // 5. Perform ? FlattenIntoArray(A, O, sourceLen, 0, 1, mapperFunction, thisArg).
    // 6. Return A.
    let mut args = FuncArgs::from(arguments);
    let mapper_function = args.next_arg();
    let this_arg = args.next_arg();
    let o = to_object(this_value.clone())?;
    let source_len = o.length_of_array_like()?;
    if !mapper_function.is_callable() {
        return Err(create_type_error("Array.prototype.flatMap: mapper function is not callable"));
    }
    let a = o.array_species_create(0.0)?;
    let a_obj = a.object_ref().expect("array species create should return an object");
    a_obj.flatten_into_array(
        &o,
        source_len,
        0.0,
        &FlattenOpts::Mapper { mapper_function: &mapper_function, this_arg: &this_arg },
    )?;
    Ok(a)
}

fn array_prototype_for_each(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.forEach ( callbackfn [ , thisArg ] )
    // NOTE 1
    // | ========
    // | callbackfn should be a function that accepts three arguments. forEach calls callbackfn once for each element
    // | present in the array, in ascending order. callbackfn is called only for elements of the array which actually
    // | exist; it is not called for missing elements of the array.
    // |
    // | If a thisArg parameter is provided, it will be used as the this value for each invocation of callbackfn. If it
    // | is not provided, undefined is used instead.
    // |
    // | callbackfn is called with three arguments: the value of the element, the index of the element, and the object
    // | being traversed.
    // |
    // | forEach does not directly mutate the object on which it is called but the object may be mutated by the calls to
    // | callbackfn.
    // |
    // | The range of elements processed by forEach is set before the first call to callbackfn. Elements which are
    // | appended to the array after the call to forEach begins will not be visited by callbackfn. If existing elements
    // | of the array are changed, their value as passed to callbackfn will be the value at the time forEach visits
    // | them; elements that are deleted after the call to forEach begins and before being visited are not visited.
    // | ========
    //
    // This method performs the following steps when called:
    //
    //  1. Let O be ? ToObject(this value).
    //  2. Let len be ? LengthOfArrayLike(O).
    //  3. If IsCallable(callbackfn) is false, throw a TypeError exception.
    //  4. Let k be 0.
    //  5. Repeat, while k < len,
    //      a. Let Pk be ! ToString(𝔽(k)).
    //      b. Let kPresent be ? HasProperty(O, Pk).
    //      c. If kPresent is true, then
    //          i. Let kValue be ? Get(O, Pk).
    //          ii. Perform ? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »).
    //      d. Set k to k + 1.
    //  6. Return undefined.
    //
    // NOTE 2
    // | ========
    // | This method is intentionally generic; it does not require that its this value be an Array. Therefore it can be
    // | transferred to other kinds of objects for use as a method.
    // | ========
    let mut args = FuncArgs::from(arguments);
    let callbackfn = args.next_arg();
    let this_arg = args.next_arg();

    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    if !is_callable(&callbackfn) {
        return Err(create_type_error("Array.prototype.forEach requires a callable first argument"));
    }
    let mut k = 0.0;
    while k < len {
        let pk = PropertyKey::from(k);
        if has_property(&o, &pk)? {
            let kvalue = o.get(&pk)?;
            call(&callbackfn, &this_arg, &[kvalue, ECMAScriptValue::from(k), ECMAScriptValue::from(o.clone())])?;
        }
        k += 1.0;
    }
    Ok(ECMAScriptValue::Undefined)
}

fn array_prototype_includes(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.includes ( searchElement [ , fromIndex ] )
    // Note 1
    // This method compares searchElement to the elements of the array, in ascending order, using the SameValueZero
    // algorithm, and if found at any position, returns true; otherwise, it returns false.
    //
    // The optional second argument fromIndex defaults to +0𝔽 (i.e. the whole array is searched). If it is greater than
    // or equal to the length of the array, false is returned, i.e. the array will not be searched. If it is less than
    // -0𝔽, it is used as the offset from the end of the array to compute fromIndex. If the computed index is less than
    // or equal to +0𝔽, the whole array will be searched.
    //
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. If len = 0, return false.
    // 4. Let n be ? ToIntegerOrInfinity(fromIndex).
    // 5. Assert: If fromIndex is undefined, then n is 0.
    // 6. If n = +∞, return false.
    // 7. Else if n = -∞, set n to 0.
    // 8. If n ≥ 0, then
    //    a. Let k be n.
    // 9. Else,
    //    a. Let k be len + n.
    //    b. If k < 0, set k to 0.
    // 10. Repeat, while k < len,
    //     a. Let elementK be ? Get(O, ! ToString(𝔽(k))).
    //     b. If SameValueZero(searchElement, elementK) is true, return true.
    //     c. Set k to k + 1.
    // 11. Return false.
    //
    // Note 2
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore it can be
    // transferred to other kinds of objects for use as a method.
    //
    // Note 3
    // This method intentionally differs from the similar indexOf method in two ways. First, it uses the SameValueZero
    // algorithm, instead of IsStrictlyEqual, allowing it to detect NaN array elements. Second, it does not skip missing
    // array elements, instead treating them as undefined.
    let mut args = FuncArgs::from(arguments);
    let search_element = args.next_arg();
    let from_index = args.next_arg();
    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    if len == 0.0 {
        return Ok(ECMAScriptValue::from(false));
    }
    let n = from_index.to_integer_or_infinity()?;
    let mut k = if n >= 0.0 { n } else { (len + n).max(0.0) };
    while k < len {
        let element_k = o.get(&PropertyKey::from(k))?;
        if search_element.same_value_zero(&element_k) {
            return Ok(ECMAScriptValue::Boolean(true));
        }
        k += 1.0;
    }
    Ok(ECMAScriptValue::Boolean(false))
}

fn array_prototype_index_of(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.indexOf ( searchElement [ , fromIndex ] )
    // This method compares searchElement to the elements of the array, in ascending order, using the
    // IsStrictlyEqual algorithm, and if found at one or more indices, returns the smallest such index;
    // otherwise, it returns -1𝔽.
    //
    // Note 1
    // The optional second argument fromIndex defaults to +0𝔽 (i.e. the whole array is searched). If it is
    // greater than or equal to the length of the array, -1𝔽 is returned, i.e. the array will not be
    // searched. If it is less than -0𝔽, it is used as the offset from the end of the array to compute
    // fromIndex. If the computed index is less than or equal to +0𝔽, the whole array will be searched.
    //
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. If len = 0, return -1𝔽.
    // 4. Let n be ? ToIntegerOrInfinity(fromIndex).
    // 5. Assert: If fromIndex is undefined, then n is 0.
    // 6. If n = +∞, return -1𝔽.
    // 7. Else if n = -∞, set n to 0.
    // 8. If n ≥ 0, then
    //    a. Let k be n.
    // 9. Else,
    //    a. Let k be len + n.
    //    b. If k < 0, set k to 0.
    // 10. Repeat, while k < len,
    //     a. Let Pk be ! ToString(𝔽(k)).
    //     b. Let kPresent be ? HasProperty(O, Pk).
    //     c. If kPresent is true, then
    //        i. Let elementK be ? Get(O, Pk).
    //        ii. If IsStrictlyEqual(searchElement, elementK) is true, return 𝔽(k).
    //     d. Set k to k + 1.
    // 11. Return -1𝔽.
    //
    // Note 2
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore it
    // can be transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let search_element = args.next_arg();
    let from_index = args.next_arg();

    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    if len == 0.0 {
        return Ok(ECMAScriptValue::from(-1));
    }
    let n = from_index.to_integer_or_infinity()?;
    if n == f64::INFINITY {
        return Ok(ECMAScriptValue::from(-1));
    }
    let n = if n == f64::NEG_INFINITY { 0.0 } else { n };
    let mut k = if n >= 0.0 { n } else { (len + n).max(0.0) };
    while k < len {
        let pk = PropertyKey::from(JSString::from(k));
        let kpresent = has_property(&o, &pk)?;
        if kpresent {
            let element_k = o.get(&pk)?;
            if search_element.is_strictly_equal(&element_k) {
                return Ok(ECMAScriptValue::from(k));
            }
        }
        k += 1.0;
    }
    Ok(ECMAScriptValue::from(-1))
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
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let separator = args.next_arg();
    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;

    let sep = if separator.is_undefined() { JSString::from(",") } else { to_string(separator)? };
    let mut r = JSString::from("");
    let mut k = 0.0;
    while k < len {
        if k > 0.0 {
            r = r.concat(sep.clone());
        }
        let element = o.get(&JSString::from(k).into())?;
        let next = if element.is_undefined() || element.is_null() { JSString::from("") } else { to_string(element)? };
        r = r.concat(next);
        k += 1.0;
    }
    Ok(ECMAScriptValue::from(r))
}

fn array_prototype_keys(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.keys ( )
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Return CreateArrayIterator(O, key).
    let o = to_object(this_value.clone())?;
    let iterator = create_array_iterator(o, KeyValueKind::Key);
    Ok(ECMAScriptValue::from(iterator))
}
fn array_prototype_last_index_of(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.lastIndexOf ( searchElement [ , fromIndex ] )
    // Note 1
    // This method compares searchElement to the elements of the array in descending order using the IsStrictlyEqual
    // algorithm, and if found at one or more indices, returns the largest such index; otherwise, it returns -1𝔽.
    //
    // The optional second argument fromIndex defaults to the array's length minus one (i.e. the whole array is
    // searched). If it is greater than or equal to the length of the array, the whole array will be searched. If it is
    // less than -0𝔽, it is used as the offset from the end of the array to compute fromIndex. If the computed index is
    // less than or equal to +0𝔽, -1𝔽 is returned.
    //
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. If len = 0, return -1𝔽.
    // 4. If fromIndex is present, let n be ? ToIntegerOrInfinity(fromIndex); else let n be len - 1.
    // 5. If n = -∞, return -1𝔽.
    // 6. If n ≥ 0, then
    //    a. Let k be min(n, len - 1).
    // 7. Else,
    //    a. Let k be len + n.
    // 8. Repeat, while k ≥ 0,
    //    a. Let Pk be ! ToString(𝔽(k)).
    //    b. Let kPresent be ? HasProperty(O, Pk).
    //    c. If kPresent is true, then
    //       i. Let elementK be ? Get(O, Pk).
    //       ii. If IsStrictlyEqual(searchElement, elementK) is true, return 𝔽(k).
    //    d. Set k to k - 1.
    // 9. Return -1𝔽.
    //
    // Note 2
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore it can be
    // transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let search_element = args.next_arg();
    let from_index = args.next_if_exists();
    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    if len == 0.0 {
        return Ok(ECMAScriptValue::from(-1));
    }
    let n = from_index.map(|fi| fi.to_integer_or_infinity()).transpose()?.unwrap_or(len - 1.0);
    let mut k = if n >= 0.0 { n.min(len - 1.0) } else { len + n };
    while k >= 0.0 {
        let pk = PropertyKey::from(k);
        let kpresent = has_property(&o, &pk)?;
        if kpresent {
            let element_k = o.get(&pk)?;
            if search_element.is_strictly_equal(&element_k) {
                return Ok(ECMAScriptValue::from(k));
            }
        }
        k -= 1.0;
    }
    Ok(ECMAScriptValue::from(-1))
}

fn array_prototype_map(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.map ( callbackfn [ , thisArg ] )
    //
    // NOTE 1
    // callbackfn should be a function that accepts three arguments. map calls callbackfn once for each
    // element in the array, in ascending order, and constructs a new Array from the results. callbackfn is
    // called only for elements of the array which actually exist; it is not called for missing elements of
    // the array.
    //
    // If a thisArg parameter is provided, it will be used as the this value for each invocation of
    // callbackfn. If it is not provided, undefined is used instead.
    //
    // callbackfn is called with three arguments: the value of the element, the index of the element, and the
    // object being traversed.
    //
    // map does not directly mutate the object on which it is called but the object may be mutated by the
    // calls to callbackfn.
    //
    // The range of elements processed by map is set before the first call to callbackfn. Elements which are
    // appended to the array after the call to map begins will not be visited by callbackfn. If existing
    // elements of the array are changed, their value as passed to callbackfn will be the value at the time
    // map visits them; elements that are deleted after the call to map begins and before being visited are
    // not visited.
    //
    // This method performs the following steps when called:
    //
    //  1. Let O be ? ToObject(this value).
    //  2. Let len be ? LengthOfArrayLike(O).
    //  3. If IsCallable(callbackfn) is false, throw a TypeError exception.
    //  4. Let A be ? ArraySpeciesCreate(O, len).
    //  5. Let k be 0.
    //  6. Repeat, while k < len,
    //      a. Let Pk be ! ToString(𝔽(k)).
    //      b. Let kPresent be ? HasProperty(O, Pk).
    //      c. If kPresent is true, then
    //          i. Let kValue be ? Get(O, Pk).
    //          ii. Let mappedValue be ? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »).
    //          iii. Perform ? CreateDataPropertyOrThrow(A, Pk, mappedValue).
    //      d. Set k to k + 1.
    //  7. Return A.
    //
    // NOTE 2
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore it
    // can be transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let callbackfn = args.next_arg();
    let this_arg = args.next_arg();

    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    if !is_callable(&callbackfn) {
        return Err(create_type_error("Array.prototype.map: callback function was not callable"));
    }
    let a = to_object(array_species_create(&o, len)?).expect("array creation should make object");
    let mut k = 0.0;
    while k < len {
        let pk = PropertyKey::from(k);
        let k_present = has_property(&o, &pk)?;
        if k_present {
            let k_value = o.get(&pk)?;
            let mapped_value = call(&callbackfn, &this_arg, &[k_value, k.into(), o.clone().into()])?;
            a.create_data_property_or_throw(pk, mapped_value)?;
        }
        k += 1.0;
    }

    Ok(a.into())
}

fn array_prototype_pop(
    this_value: &ECMAScriptValue,
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
    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    if len == 0.0 {
        o.set("length", 0.0, true)?;
        Ok(ECMAScriptValue::Undefined)
    } else {
        let newlen = len - 1.0;
        let index = PropertyKey::from(format!("{newlen}"));
        let element = o.get(&index)?;
        o.delete_property_or_throw(&index)?;
        o.set("length", newlen, true)?;
        Ok(element)
    }
}

fn array_prototype_push(
    this_value: &ECMAScriptValue,
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
    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    let arg_count = to_f64(arguments.len())
        .map_err(|_| ())
        .and_then(|arg_count| if len > 9_007_199_254_740_991.0 - arg_count { Err(()) } else { Ok(arg_count) })
        .map_err(|()| create_type_error("Array too large"))?;
    let new_len = len + arg_count;
    for (idx, e) in
        arguments.iter().cloned().enumerate().map(|(idx, e)| (to_f64(idx).expect("limits should be in range"), e))
    {
        o.set(len + idx, e, true)?;
    }
    o.set("length", new_len, true)?;
    Ok(new_len.into())
}

fn array_prototype_reduce(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.reduce ( callback [ , initialValue ] )
    // Note 1
    // callback should be a function that takes four arguments. reduce calls the callback, as a function, once for each
    // element after the first element present in the array, in ascending order.
    //
    // callback is called with four arguments: the previousValue (value from the previous call to callback), the
    // currentValue (value of the current element), the currentIndex, and the object being traversed. The first time
    // that callback is called, the previousValue and currentValue can be one of two values. If an initialValue was
    // supplied in the call to reduce, then previousValue will be initialValue and currentValue will be the first value
    // in the array. If no initialValue was supplied, then previousValue will be the first value in the array and
    // currentValue will be the second. It is a TypeError if the array contains no elements and initialValue is not
    // provided.
    //
    // reduce does not directly mutate the object on which it is called but the object may be mutated by the calls to
    // callback.
    //
    // The range of elements processed by reduce is set before the first call to callback. Elements that are appended to
    // the array after the call to reduce begins will not be visited by callback. If existing elements of the array are
    // changed, their value as passed to callback will be the value at the time reduce visits them; elements that are
    // deleted after the call to reduce begins and before being visited are not visited.
    //
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. If IsCallable(callback) is false, throw a TypeError exception.
    // 4. If len = 0 and initialValue is not present, throw a TypeError exception.
    // 5. Let k be 0.
    // 6. Let accumulator be undefined.
    // 7. If initialValue is present, then
    //    a. Set accumulator to initialValue.
    // 8. Else,
    //    a. Let kPresent be false.
    //    b. Repeat, while kPresent is false and k < len,
    //       i. Let Pk be ! ToString(𝔽(k)).
    //       ii. Set kPresent to ? HasProperty(O, Pk).
    //       iii. If kPresent is true, then
    //            1. Set accumulator to ? Get(O, Pk).
    //       iv. Set k to k + 1.
    //    c. If kPresent is false, throw a TypeError exception.
    // 9. Repeat, while k < len,
    //    a. Let Pk be ! ToString(𝔽(k)).
    //    b. Let kPresent be ? HasProperty(O, Pk).
    //    c. If kPresent is true, then
    //       i. Let kValue be ? Get(O, Pk).
    //       ii. Set accumulator to ? Call(callback, undefined, « accumulator, kValue, 𝔽(k), O »).
    //    d. Set k to k + 1.
    // 10. Return accumulator.
    //
    // Note 2
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore it can be
    // transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let callback = args.next_arg();
    let initial_value = args.next_if_exists();
    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    if !is_callable(&callback) {
        return Err(create_type_error("Array.prototype.reduce: callback function was not callable"));
    }
    let mut k = 0.0;
    let mut accumulator = if let Some(initial_value) = initial_value {
        initial_value
    } else {
        loop {
            if k >= len {
                break Err(create_type_error("Array.prototype.reduce: empty array with no initial value"));
            }
            let pk = PropertyKey::from(k);
            k += 1.0;
            if has_property(&o, &pk)? {
                break o.get(&pk);
            }
        }?
    };
    while k < len {
        let pk = PropertyKey::from(k);
        let k_present = has_property(&o, &pk)?;
        if k_present {
            let k_value = o.get(&pk)?;
            accumulator =
                call(&callback, &ECMAScriptValue::Undefined, &[accumulator, k_value, k.into(), o.clone().into()])?;
        }
        k += 1.0;
    }
    Ok(accumulator)
}

fn array_prototype_reduce_right(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.reduceRight ( callback [ , initialValue ] )
    // Note 1
    // callback should be a function that takes four arguments. reduceRight calls the callback, as a function, once for
    // each element after the first element present in the array, in descending order.
    //
    // callback is called with four arguments: the previousValue (value from the previous call to callback), the
    // currentValue (value of the current element), the currentIndex, and the object being traversed. The first time the
    // function is called, the previousValue and currentValue can be one of two values. If an initialValue was supplied
    // in the call to reduceRight, then previousValue will be initialValue and currentValue will be the last value in
    // the array. If no initialValue was supplied, then previousValue will be the last value in the array and
    // currentValue will be the second-to-last value. It is a TypeError if the array contains no elements and
    // initialValue is not provided.
    //
    // reduceRight does not directly mutate the object on which it is called but the object may be mutated by the calls
    // to callback.
    //
    // The range of elements processed by reduceRight is set before the first call to callback. Elements that are
    // appended to the array after the call to reduceRight begins will not be visited by callback. If existing elements
    // of the array are changed by callback, their value as passed to callback will be the value at the time reduceRight
    // visits them; elements that are deleted after the call to reduceRight begins and before being visited are not
    // visited.
    //
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. If IsCallable(callback) is false, throw a TypeError exception.
    // 4. If len = 0 and initialValue is not present, throw a TypeError exception.
    // 5. Let k be len - 1.
    // 6. Let accumulator be undefined.
    // 7. If initialValue is present, then
    //    a. Set accumulator to initialValue.
    // 8. Else,
    //    a. Let kPresent be false.
    //    b. Repeat, while kPresent is false and k ≥ 0,
    //       i. Let Pk be ! ToString(𝔽(k)).
    //       ii. Set kPresent to ? HasProperty(O, Pk).
    //       iii. If kPresent is true, then
    //            1. Set accumulator to ? Get(O, Pk).
    //       iv. Set k to k - 1.
    //    c. If kPresent is false, throw a TypeError exception.
    // 9. Repeat, while k ≥ 0,
    //    a. Let Pk be ! ToString(𝔽(k)).
    //    b. Let kPresent be ? HasProperty(O, Pk).
    //    c. If kPresent is true, then
    //       i. Let kValue be ? Get(O, Pk).
    //       ii. Set accumulator to ? Call(callback, undefined, « accumulator, kValue, 𝔽(k), O »).
    //    d. Set k to k - 1.
    // 10. Return accumulator.
    //
    // Note 2
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore it can be
    // transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let callback = args.next_arg();
    let initial_value = args.next_if_exists();
    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    if !is_callable(&callback) {
        return Err(create_type_error("Array.prototype.reduceRight: callback function was not callable"));
    }
    let mut k = len - 1.0;
    let mut accumulator = if let Some(initial_value) = initial_value {
        initial_value
    } else {
        loop {
            if k < 0.0 {
                break Err(create_type_error("Array.prototype.reduceRight: empty array with no initial value"));
            }
            let pk = PropertyKey::from(k);
            k -= 1.0;
            if has_property(&o, &pk)? {
                break o.get(&pk);
            }
        }?
    };
    while k >= 0.0 {
        let pk = PropertyKey::from(k);
        let k_present = has_property(&o, &pk)?;
        if k_present {
            let k_value = o.get(&pk)?;
            accumulator =
                call(&callback, &ECMAScriptValue::Undefined, &[accumulator, k_value, k.into(), o.clone().into()])?;
        }
        k -= 1.0;
    }
    Ok(accumulator)
}

fn array_prototype_reverse(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.reverse ( )
    // Note 1
    // This method rearranges the elements of the array so as to reverse their order. It returns the reversed array.
    //
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. Let middle be floor(len / 2).
    // 4. Let lower be 0.
    // 5. Repeat, while lower ≠ middle,
    //    a. Let upper be len - lower - 1.
    //    b. Let upperP be ! ToString(𝔽(upper)).
    //    c. Let lowerP be ! ToString(𝔽(lower)).
    //    d. Let lowerExists be ? HasProperty(O, lowerP).
    //    e. If lowerExists is true, then
    //       i. Let lowerValue be ? Get(O, lowerP).
    //    f. Let upperExists be ? HasProperty(O, upperP).
    //    g. If upperExists is true, then
    //       i. Let upperValue be ? Get(O, upperP).
    //    h. If lowerExists is true and upperExists is true, then
    //       i. Perform ? Set(O, lowerP, upperValue, true).
    //       ii. Perform ? Set(O, upperP, lowerValue, true).
    //    i. Else if lowerExists is false and upperExists is true, then
    //       i. Perform ? Set(O, lowerP, upperValue, true).
    //       ii. Perform ? DeletePropertyOrThrow(O, upperP).
    //    j. Else if lowerExists is true and upperExists is false, then
    //       i. Perform ? DeletePropertyOrThrow(O, lowerP).
    //       ii. Perform ? Set(O, upperP, lowerValue, true).
    //    k. Else,
    //       i. Assert: lowerExists and upperExists are both false.
    //       ii. NOTE: No action is required.
    //    l. Set lower to lower + 1.
    // 6. Return O.
    //
    // Note 2
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore, it can be
    // transferred to other kinds of objects for use as a method.
    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    let middle = (len / 2.0).floor();
    let mut lower = 0.0;
    while lower != middle {
        let upper = len - lower - 1.0;
        let upper_p = PropertyKey::from(upper);
        let lower_p = PropertyKey::from(lower);
        let lower_value = if has_property(&o, &lower_p)? { Some(o.get(&lower_p)?) } else { None };
        let upper_value = if has_property(&o, &upper_p)? { Some(o.get(&upper_p)?) } else { None };
        match (lower_value, upper_value) {
            (Some(lower_value), Some(upper_value)) => {
                o.set(upper_p, lower_value, true)?;
                o.set(lower_p, upper_value, true)?;
            }
            (Some(lower_value), None) => {
                o.set(upper_p, lower_value, true)?;
                o.delete_property_or_throw(&lower_p)?;
            }
            (None, Some(upper_value)) => {
                o.set(lower_p, upper_value, true)?;
                o.delete_property_or_throw(&upper_p)?;
            }
            (None, None) => {
                // No action required.
            }
        }
        lower += 1.0;
    }
    Ok(o.into())
}

fn array_prototype_shift(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.shift ( )
    // This method removes the first element of the array and returns it.
    //
    // It performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. If len = 0, then
    //    a. Perform ? Set(O, "length", +0𝔽, true).
    //    b. Return undefined.
    // 4. Let first be ? Get(O, "0").
    // 5. Let k be 1.
    // 6. Repeat, while k < len,
    //    a. Let from be ! ToString(𝔽(k)).
    //    b. Let to be ! ToString(𝔽(k - 1)).
    //    c. Let fromPresent be ? HasProperty(O, from).
    //    d. If fromPresent is true, then
    //       i. Let fromValue be ? Get(O, from).
    //       ii. Perform ? Set(O, to, fromValue, true).
    //    e. Else,
    //       i. Assert: fromPresent is false.
    //       ii. Perform ? DeletePropertyOrThrow(O, to).
    //    f. Set k to k + 1.
    // 7. Perform ? DeletePropertyOrThrow(O, ! ToString(𝔽(len - 1))).
    // 8. Perform ? Set(O, "length", 𝔽(len - 1), true).
    // 9. Return first.
    //
    // Note
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore it can be
    // transferred to other kinds of objects for use as a method.
    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    if len == 0.0 {
        o.set("length", 0.0, true)?;
        Ok(ECMAScriptValue::Undefined)
    } else {
        let first = o.get(&PropertyKey::from("0"))?;
        let mut k = 1.0;
        while k < len {
            let from = PropertyKey::from(k);
            let to = PropertyKey::from(k - 1.0);
            let from_present = has_property(&o, &from)?;
            if from_present {
                let from_value = o.get(&from)?;
                o.set(to, from_value, true)?;
            } else {
                o.delete_property_or_throw(&to)?;
            }
            k += 1.0;
        }
        o.delete_property_or_throw(&PropertyKey::from(len - 1.0))?;
        o.set("length", len - 1.0, true)?;
        Ok(first)
    }
}

fn array_prototype_slice(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.slice ( start, end )
    // This method returns an array containing the elements of the array from element start up to, but not including,
    // element end (or through the end of the array if end is undefined). If start is negative, it is treated as length
    // + start where length is the length of the array. If end is negative, it is treated as length + end where length
    // is the length of the array.
    //
    // It performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. Let relativeStart be ? ToIntegerOrInfinity(start).
    // 4. If relativeStart = -∞, let k be 0.
    // 5. Else if relativeStart < 0, let k be max(len + relativeStart, 0).
    // 6. Else, let k be min(relativeStart, len).
    // 7. If end is undefined, let relativeEnd be len; else let relativeEnd be ? ToIntegerOrInfinity(end).
    // 8. If relativeEnd = -∞, let final be 0.
    // 9. Else if relativeEnd < 0, let final be max(len + relativeEnd, 0).
    // 10. Else, let final be min(relativeEnd, len).
    // 11. Let count be max(final - k, 0).
    // 12. Let A be ? ArraySpeciesCreate(O, count).
    // 13. Let n be 0.
    // 14. Repeat, while k < final,
    //     a. Let Pk be ! ToString(𝔽(k)).
    //     b. Let kPresent be ? HasProperty(O, Pk).
    //     c. If kPresent is true, then
    //        i. Let kValue be ? Get(O, Pk).
    //        ii. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), kValue).
    //     d. Set k to k + 1.
    //     e. Set n to n + 1.
    // 15. Perform ? Set(A, "length", 𝔽(n), true).
    // 16. Return A.
    //
    // Note 1
    // The explicit setting of the "length" property in step 15 is intended to ensure the length is correct even when A
    // is not a built-in Array.
    //
    // Note 2
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore it can be
    // transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let start = args.next_arg();
    let end = args.next_arg();
    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    let relative_start = start.to_integer_or_infinity()?;
    let mut k = if relative_start < 0.0 { (len + relative_start).max(0.0) } else { relative_start.min(len) };
    let relative_end = if end.is_undefined() { len } else { end.to_integer_or_infinity()? };
    let final_ = if relative_end < 0.0 { (len + relative_end).max(0.0) } else { relative_end.min(len) };
    let count = (final_ - k).max(0.0);
    let a = array_species_create(&o, count)?;
    let a_obj = a.object_ref().expect("ArraySpeciesCreate should return an object value");
    let mut n = 0_u64;
    while k < final_ {
        let pk = PropertyKey::from(k);
        let k_present = has_property(&o, &pk)?;
        if k_present {
            let k_value = o.get(&pk)?;
            a_obj.create_data_property_or_throw(n, k_value)?;
        }
        k += 1.0;
        n += 1;
    }
    a_obj.set("length", n, true)?;
    Ok(a)
}

fn array_prototype_some(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.some ( callback [ , thisArg ] )
    // Note 1
    // callback should be a function that accepts three arguments and returns a value that is coercible to a Boolean
    // value. some calls callback once for each element present in the array, in ascending order, until it finds one
    // where callback returns true. If such an element is found, some immediately returns true. Otherwise, some returns
    // false. callback is called only for elements of the array which actually exist; it is not called for missing
    // elements of the array.
    //
    // If a thisArg parameter is provided, it will be used as the this value for each invocation of callback. If it is
    // not provided, undefined is used instead.
    //
    // callback is called with three arguments: the value of the element, the index of the element, and the object being
    // traversed.
    //
    // some does not directly mutate the object on which it is called but the object may be mutated by the calls to
    // callback.
    //
    // The range of elements processed by some is set before the first call to callback. Elements that are appended to
    // the array after the call to some begins will not be visited by callback. If existing elements of the array are
    // changed, their value as passed to callback will be the value at the time that some visits them; elements that are
    // deleted after the call to some begins and before being visited are not visited. some acts like the "exists"
    // quantifier in mathematics. In particular, for an empty array, it returns false.
    //
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. If IsCallable(callback) is false, throw a TypeError exception.
    // 4. Let k be 0.
    // 5. Repeat, while k < len,
    //    a. Let Pk be ! ToString(𝔽(k)).
    //    b. Let kPresent be ? HasProperty(O, Pk).
    //    c. If kPresent is true, then
    //       i. Let kValue be ? Get(O, Pk).
    //       ii. Let testResult be ToBoolean(? Call(callback, thisArg, « kValue, 𝔽(k), O »)).
    //       iii. If testResult is true, return true.
    //    d. Set k to k + 1.
    // 6. Return false.
    //
    // Note 2
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore it can be
    // transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let callback = args.next_arg();
    let this_arg = args.next_arg();
    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    if !callback.is_callable() {
        return Err(create_type_error("Array.prototype.some: Callback must be a function"));
    }
    let mut k = 0.0;
    while k < len {
        let pk = PropertyKey::from(k);
        if o.has_property(&pk)? {
            let k_value = o.get(&pk)?;
            let test_result =
                call(&callback, &this_arg, &[k_value, ECMAScriptValue::Number(k), ECMAScriptValue::Object(o.clone())])?
                    .to_boolean();
            if test_result {
                return Ok(ECMAScriptValue::Boolean(true));
            }
        }
        k += 1.0;
    }
    Ok(ECMAScriptValue::Boolean(false))
}

fn array_prototype_sort(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.sort ( comparator )
    // This method sorts the elements of this array. If comparator is not undefined, it should be a function that
    // accepts two arguments x and y and returns a negative Number if x < y, a positive Number if x > y, or a zero
    // otherwise.
    //
    // It performs the following steps when called:
    //
    // 1. If comparator is not undefined and IsCallable(comparator) is false, throw a TypeError exception.
    // 2. Let obj be ? ToObject(this value).
    // 3. Let len be ? LengthOfArrayLike(obj).
    // 4. Let SortCompare be a new Abstract Closure with parameters (x, y) that captures comparator and performs the
    //    following steps when called:
    //    a. Return ? CompareArrayElements(x, y, comparator).
    // 5. Let sortedList be ? SortIndexedProperties(obj, len, SortCompare, skip-holes).
    // 6. Let itemCount be the number of elements in sortedList.
    // 7. Let j be 0.
    // 8. Repeat, while j < itemCount,
    //    a. Perform ? Set(obj, ! ToString(𝔽(j)), sortedList[j], true).
    //    b. Set j to j + 1.
    // 9. NOTE: The call to SortIndexedProperties in step 5 uses skip-holes. The remaining indices are deleted to
    //    preserve the number of holes that were detected and excluded from the sort.
    // 10. Repeat, while j < len,
    //     a. Perform ? DeletePropertyOrThrow(obj, ! ToString(𝔽(j))).
    //     b. Set j to j + 1.
    // 11. Return obj.
    //
    // Note 1
    // Because non-existent property values always compare greater than undefined property values, and undefined always
    // compares greater than any other value (see CompareArrayElements), undefined property values always sort to the
    // end of the result, followed by non-existent property values.
    //
    // Note 2
    // Method calls performed by the ToString abstract operations in steps 5 and 6 have the potential to cause
    // SortCompare to not behave as a consistent comparator.
    //
    // Note 3
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore, it can be
    // transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let comparator = args.next_arg();
    if !comparator.is_undefined() && !comparator.is_callable() {
        return Err(create_type_error("Array.prototype.sort: Comparator must be a function"));
    }
    let obj = to_object(this_value.clone())?;
    let len = obj.length_of_array_like()?;
    let sort_compare = move |x: &ECMAScriptValue, y: &ECMAScriptValue| compare_array_elements(x, y, &comparator);
    let sorted_list = sort_indexed_properties(&obj, len, sort_compare, Holes::Skip)?;
    let item_count = sorted_list.len();
    for (idx, (_, item)) in sorted_list.into_iter().enumerate() {
        obj.set(idx, item, true)?;
    }
    // len is an f64 with an nonnegative integer value, so it will fit in the usize.
    #[expect(clippy::cast_possible_truncation)]
    #[expect(clippy::cast_sign_loss)]
    for idx in item_count..(len as usize) {
        obj.delete_property_or_throw(&PropertyKey::from(idx))?;
    }
    Ok(obj.into())
}

#[expect(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq)]
enum Holes {
    Include,
    Skip,
}

fn sort_indexed_properties(
    obj: &Object,
    len: f64,
    sort_compare: impl for<'a, 'b> Fn(&'a ECMAScriptValue, &'b ECMAScriptValue) -> Completion<std::cmp::Ordering>,
    holes: Holes,
) -> Completion<Vec<(f64, ECMAScriptValue)>> {
    // SortIndexedProperties ( obj, len, SortCompare, holes )
    // The abstract operation SortIndexedProperties takes arguments obj (an Object), len (a non-negative integer),
    // SortCompare (an Abstract Closure with two parameters), and holes (skip-holes or read-through-holes) and returns
    // either a normal completion containing a List of ECMAScript language values or a throw completion. It performs the
    // following steps when called:
    //
    // 1. Let items be a new empty List.
    // 2. Let k be 0.
    // 3. Repeat, while k < len,
    //    a. Let Pk be ! ToString(𝔽(k)).
    //    b. If holes is skip-holes, then
    //       i. Let kRead be ? HasProperty(obj, Pk).
    //    c. Else,
    //       i. Assert: holes is read-through-holes.
    //       ii. Let kRead be true.
    //    d. If kRead is true, then
    //       i. Let kValue be ? Get(obj, Pk).
    //       ii. Append kValue to items.
    //    e. Set k to k + 1.
    // 4. Sort items using an implementation-defined sequence of calls to SortCompare. If any such call returns an
    //    abrupt completion, stop before performing any further calls to SortCompare and return that Completion Record.
    // 5. Return items.
    let mut items = vec![];
    let mut k = 0.0;
    while k < len {
        let pk = PropertyKey::from(k);
        let k_read = if holes == Holes::Skip { obj.has_property(&pk)? } else { true };
        if k_read {
            let k_value = obj.get(&pk)?;
            items.push((k, k_value));
        }
        k += 1.0;
    }
    try_sort_by(&mut items, sort_compare)?;
    Ok(items)
}

fn try_sort_by<T, F>(arr: &mut [(f64, T)], compare: F) -> Completion<()>
where
    F: for<'a, 'b> Fn(&'a T, &'b T) -> Completion<std::cmp::Ordering>,
{
    // Handle empty or single-element arrays
    if arr.len() <= 1 {
        return Ok(());
    }

    // Create a stack for tracking partition ranges to avoid recursion
    let mut stack = Vec::new();
    stack.push((0, arr.len() - 1));

    while let Some((low, high)) = stack.pop() {
        if low < high {
            // Partition the array and get pivot position
            match partition(arr, low, high, &compare) {
                Ok(pivot) => {
                    // Push right partition if it exists
                    if pivot + 1 < high {
                        stack.push((pivot + 1, high));
                    }
                    // Push left partition if it exists
                    if pivot > low {
                        stack.push((low, pivot - 1));
                    }
                }
                Err(e) => return Err(e), // Early exit on comparison error
            }
        }
    }
    Ok(())
}

fn partition<T, F>(arr: &mut [(f64, T)], low: usize, high: usize, compare: F) -> Completion<usize>
where
    F: Fn(&T, &T) -> Completion<std::cmp::Ordering>,
{
    let pivot = high;
    let mut i = low;

    for j in low..high {
        // Get references that satisfy the borrow checker
        let (j_idx, ref j_val) = arr[j];
        let (pivot_idx, ref pivot_val) = arr[pivot];

        // Compare current element with pivot
        match compare(j_val, pivot_val)? {
            std::cmp::Ordering::Less => {
                arr.swap(i, j);
                i += 1;
            }
            std::cmp::Ordering::Equal => {
                // Ensure stability by using the original index to determine order
                if j_idx < pivot_idx {
                    arr.swap(i, j);
                    i += 1;
                }
            }
            std::cmp::Ordering::Greater => {}
        }
    }

    arr.swap(i, pivot);
    Ok(i)
}

fn compare_array_elements(
    x: &ECMAScriptValue,
    y: &ECMAScriptValue,
    comparator: &ECMAScriptValue,
) -> Completion<std::cmp::Ordering> {
    // CompareArrayElements ( x, y, comparator )
    // The abstract operation CompareArrayElements takes arguments x (an ECMAScript language value), y (an ECMAScript
    // language value), and comparator (a function object or undefined) and returns either a normal completion
    // containing a Number or an abrupt completion. It performs the following steps when called:
    //
    // 1. If x and y are both undefined, return +0𝔽.
    // 2. If x is undefined, return 1𝔽.
    // 3. If y is undefined, return -1𝔽.
    // 4. If comparator is not undefined, then
    //    a. Let v be ? ToNumber(? Call(comparator, undefined, « x, y »)).
    //    b. If v is NaN, return +0𝔽.
    //    c. Return v.
    // 5. Let xString be ? ToString(x).
    // 6. Let yString be ? ToString(y).
    // 7. Let xSmaller be ! IsLessThan(xString, yString, true).
    // 8. If xSmaller is true, return -1𝔽.
    // 9. Let ySmaller be ! IsLessThan(yString, xString, true).
    // 10. If ySmaller is true, return 1𝔽.
    // 11. Return +0𝔽.
    match (x.is_undefined(), y.is_undefined()) {
        (true, true) => Ok(std::cmp::Ordering::Equal),
        (true, false) => Ok(std::cmp::Ordering::Greater),
        (false, true) => Ok(std::cmp::Ordering::Less),
        (false, false) => {
            if !comparator.is_undefined() {
                let v = call(comparator, &ECMAScriptValue::Undefined, &[x.clone(), y.clone()])?.to_number()?;
                if v.is_nan() || v == 0.0 {
                    return Ok(std::cmp::Ordering::Equal);
                }
                if v > 0.0 {
                    return Ok(std::cmp::Ordering::Greater);
                }
                return Ok(std::cmp::Ordering::Less);
            }
            let x_string = ECMAScriptValue::String(to_string(x.clone())?);
            let y_string = ECMAScriptValue::String(to_string(y.clone())?);
            if x_string
                .is_less_than(&y_string, true)?
                .expect("should not have NaN issues, as inputs are strings, not numbers")
            {
                Ok(std::cmp::Ordering::Less)
            } else if y_string
                .is_less_than(&x_string, true)?
                .expect("should not have NaN issues, as inputs are strings, not numbers")
            {
                Ok(std::cmp::Ordering::Greater)
            } else {
                Ok(std::cmp::Ordering::Equal)
            }
        }
    }
}

fn array_prototype_splice(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.splice ( start, deleteCount, ...items )
    // Note 1
    // This method deletes the deleteCount elements of the array starting at integer index start and replaces them with
    // the elements of items. It returns an Array containing the deleted elements (if any).
    //
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. Let relativeStart be ? ToIntegerOrInfinity(start).
    // 4. If relativeStart = -∞, let actualStart be 0.
    // 5. Else if relativeStart < 0, let actualStart be max(len + relativeStart, 0).
    // 6. Else, let actualStart be min(relativeStart, len).
    // 7. Let itemCount be the number of elements in items.
    // 8. If start is not present, then
    //    a. Let actualDeleteCount be 0.
    // 9. Else if deleteCount is not present, then
    //    a. Let actualDeleteCount be len - actualStart.
    // 10. Else,
    //     a. Let dc be ? ToIntegerOrInfinity(deleteCount).
    //     b. Let actualDeleteCount be the result of clamping dc between 0 and len - actualStart.
    // 11. If len + itemCount - actualDeleteCount > 2**53 - 1, throw a TypeError exception.
    // 12. Let A be ? ArraySpeciesCreate(O, actualDeleteCount).
    // 13. Let k be 0.
    // 14. Repeat, while k < actualDeleteCount,
    //     a. Let from be ! ToString(𝔽(actualStart + k)).
    //     b. If ? HasProperty(O, from) is true, then
    //        i. Let fromValue be ? Get(O, from).
    //        ii. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(k)), fromValue).
    //     c. Set k to k + 1.
    // 15. Perform ? Set(A, "length", 𝔽(actualDeleteCount), true).
    // 16. If itemCount < actualDeleteCount, then
    //     a. Set k to actualStart.
    //     b. Repeat, while k < (len - actualDeleteCount),
    //        i. Let from be ! ToString(𝔽(k + actualDeleteCount)).
    //        ii. Let to be ! ToString(𝔽(k + itemCount)).
    //        iii. If ? HasProperty(O, from) is true, then
    //             1. Let fromValue be ? Get(O, from).
    //             2. Perform ? Set(O, to, fromValue, true).
    //        iv. Else,
    //            1. Perform ? DeletePropertyOrThrow(O, to).
    //        v. Set k to k + 1.
    //     c. Set k to len.
    //     d. Repeat, while k > (len - actualDeleteCount + itemCount),
    //        i. Perform ? DeletePropertyOrThrow(O, ! ToString(𝔽(k - 1))).
    //        ii. Set k to k - 1.
    // 17. Else if itemCount > actualDeleteCount, then
    //     a. Set k to (len - actualDeleteCount).
    //     b. Repeat, while k > actualStart,
    //        i. Let from be ! ToString(𝔽(k + actualDeleteCount - 1)).
    //        ii. Let to be ! ToString(𝔽(k + itemCount - 1)).
    //        iii. If ? HasProperty(O, from) is true, then
    //             1. Let fromValue be ? Get(O, from).
    //             2. Perform ? Set(O, to, fromValue, true).
    //        iv. Else,
    //            1. Perform ? DeletePropertyOrThrow(O, to).
    //        v. Set k to k - 1.
    // 18. Set k to actualStart.
    // 19. For each element E of items, do
    //     a. Perform ? Set(O, ! ToString(𝔽(k)), E, true).
    //     b. Set k to k + 1.
    // 20. Perform ? Set(O, "length", 𝔽(len - actualDeleteCount + itemCount), true).
    // 21. Return A.
    //
    // Note 2
    // The explicit setting of the "length" property in steps 15 and 20 is intended to ensure the lengths are correct
    // even when the objects are not built-in Arrays.
    //
    // Note 3
    // This method is intentionally generic; it does not require that its this value be an Array. Therefore it can be
    // transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let start = args.next_arg();
    let delete_count = args.next_arg();
    let items = args.remaining();

    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    let relative_start = start.to_integer_or_infinity()?;
    let actual_start = if relative_start < 0.0 { (len + relative_start).max(0.0) } else { relative_start.min(len) };
    #[expect(clippy::cast_precision_loss)]
    let item_count = items.len() as f64;
    let actual_delete_count = if start.is_undefined() {
        0.0
    } else if delete_count.is_undefined() {
        len - actual_start
    } else {
        let dc = delete_count.to_integer_or_infinity()?;
        dc.clamp(0.0, len - actual_start)
    };
    if len + item_count - actual_delete_count > 9_007_199_254_740_991.0 {
        return Err(create_type_error("Array too large"));
    }
    let a = array_species_create(&o, actual_delete_count)?;
    let a_obj = a.object_ref().expect("array creation should make an object");
    let mut k = 0.0;
    while k < actual_delete_count {
        let from = PropertyKey::from(actual_start + k);
        if has_property(&o, &from)? {
            let from_value = o.get(&from)?;
            a_obj.create_data_property_or_throw(k, from_value)?;
        }
        k += 1.0;
    }
    a_obj.set("length", actual_delete_count, true)?;
    if item_count < actual_delete_count {
        k = actual_start;
        while k < len - actual_delete_count {
            let from = PropertyKey::from(k + actual_delete_count);
            let to = PropertyKey::from(k + item_count);
            if has_property(&o, &from)? {
                let from_value = o.get(&from)?;
                o.set(to, from_value, true)?;
            } else {
                o.delete_property_or_throw(&to)?;
            }
            k += 1.0;
        }
        k = len;
        while k > len - actual_delete_count + item_count {
            o.delete_property_or_throw(&PropertyKey::from(k - 1.0))?;
            k -= 1.0;
        }
    } else if item_count > actual_delete_count {
        k = len - actual_delete_count;
        while k > actual_start {
            let from = PropertyKey::from(k + actual_delete_count - 1.0);
            let to = PropertyKey::from(k + item_count - 1.0);
            if has_property(&o, &from)? {
                let from_value = o.get(&from)?;
                o.set(to, from_value, true)?;
            } else {
                o.delete_property_or_throw(&to)?;
            }
            k -= 1.0;
        }
    }
    k = actual_start;
    for item in items {
        o.set(k, item.clone(), true)?;
        k += 1.0;
    }
    o.set("length", len - actual_delete_count + item_count, true)?;
    Ok(a)
}

fn array_prototype_to_locale_string(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_to_reversed(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_to_sorted(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    todo!()
}
fn array_prototype_to_spliced(
    _this_value: &ECMAScriptValue,
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
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let array = to_object(this_value.clone())?;
    let mut func = array.get(&"join".into())?;
    if !is_callable(&func) {
        func = ECMAScriptValue::from(intrinsic(IntrinsicId::ObjectPrototypeToString));
    }
    call(&func, &ECMAScriptValue::from(array), &[])
}

fn array_prototype_unshift(
    _this_value: &ECMAScriptValue,
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
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let o = to_object(this_value.clone())?;
    Ok(ECMAScriptValue::from(create_array_iterator(o, KeyValueKind::Value)))
}

fn array_prototype_with(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Array.prototype.with ( index, value )
    // This method performs the following steps when called:
    //
    // 1. Let O be ? ToObject(this value).
    // 2. Let len be ? LengthOfArrayLike(O).
    // 3. Let relativeIndex be ? ToIntegerOrInfinity(index).
    // 4. If relativeIndex ≥ 0, let actualIndex be relativeIndex.
    // 5. Else, let actualIndex be len + relativeIndex.
    // 6. If actualIndex ≥ len or actualIndex < 0, throw a RangeError exception.
    // 7. Let A be ? ArrayCreate(len).
    // 8. Let k be 0.
    // 9. Repeat, while k < len,
    //    a. Let Pk be ! ToString(𝔽(k)).
    //    b. If k = actualIndex, let fromValue be value.
    //    c. Else, let fromValue be ? Get(O, Pk).
    //    d. Perform ! CreateDataPropertyOrThrow(A, Pk, fromValue).
    //    e. Set k to k + 1.
    // 10. Return A.
    let mut args = FuncArgs::from(arguments);
    let index = args.next_arg();
    let value = args.next_arg();
    let o = to_object(this_value.clone())?;
    let len = o.length_of_array_like()?;
    let relative_index = index.to_integer_or_infinity()?;
    let actual_index = if relative_index < 0.0 { len + relative_index } else { relative_index };
    if actual_index >= len || actual_index < 0.0 {
        return Err(create_range_error("Array.prototype.with: Index out of range"));
    }
    let a = array_create(len, None)?;
    let mut k = 0.0;
    while k < len {
        let pk = PropertyKey::from(k);
        let from_value = if k == actual_index { value.clone() } else { o.get(&pk)? };
        a.create_data_property_or_throw(pk, from_value)?;
        k += 1.0;
    }
    Ok(ECMAScriptValue::Object(a))
}

// Array Iterator Objects
// An Array Iterator is an object, that represents a specific iteration over some specific Array instance
// object. There is not a named constructor for Array Iterator objects. Instead, Array iterator objects are
// created by calling certain methods of Array instance objects.

fn array_iterator_prototype_next(
    this_value: &ECMAScriptValue,
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
    let mut index = 0.0;
    loop {
        assert!(!array.is_typed_array()); // when typed arrays are added, this needs to be accounted for
        let len = array.length_of_array_like()?;
        if index >= len {
            return Ok(ECMAScriptValue::Undefined);
        }
        if kind == KeyValueKind::Key {
            let res = ECMAScriptValue::from(create_iter_result_object(ECMAScriptValue::from(index), false));
            generator_yield(&co, res).await?;
        } else {
            let element_key = PropertyKey::from(index);
            let element_value = array.get(&element_key)?;
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
        index += 1.0;
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
