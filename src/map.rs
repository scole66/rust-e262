// The Map Constructor
use genawaiter::rc::Co;
use multi_map::MultiMap;

use super::*;

// Map Objects
//
// Maps are collections of key/value pairs where both the keys and values may be arbitrary ECMAScript language values. A
// distinct key value may only occur in one key/value pair within the Map's collection. Distinct key values are
// discriminated using the SameValueZero comparison algorithm.
//
// Maps must be implemented using either hash tables or other mechanisms that, on average, provide access times that are
// sublinear on the number of elements in the collection. The data structure used in this specification is only intended
// to describe the required observable semantics of Maps. It is not intended to be a viable implementation model.

/// Iterate over the iterable and use the function in adder to add the values to target
///
/// The parameter iterable is expected to be an object that implements an @@iterator method that returns an
/// iterator object that produces a two element array-like object whose first element is a value that will be
/// used as a Map key and whose second element is the value to associate with that key.
///
/// See [AddEntriesFromIterable](https://tc39.es/ecma262/#sec-add-entries-from-iterable) in ECMA-262.
pub(crate) fn add_entries_from_iterable(
    target: &ECMAScriptValue,
    iterable: &ECMAScriptValue,
    adder: &ECMAScriptValue,
) -> FullCompletion {
    // AddEntriesFromIterable ( target, iterable, adder )
    // The abstract operation AddEntriesFromIterable takes arguments target (an Object), iterable (an
    // ECMAScript language value, but not undefined or null), and adder (a function object) and returns either
    // a normal completion containing an ECMAScript language value or a throw completion. adder will be
    // invoked, with target as the receiver. It performs the following steps when called:
    //
    //  1. Let iteratorRecord be ? GetIterator(iterable, sync).
    //  2. Repeat,
    //      a. Let next be ? IteratorStep(iteratorRecord).
    //      b. If next is false, return target.
    //      c. Let nextItem be ? IteratorValue(next).
    //      d. If nextItem is not an Object, then
    //          i. Let error be ThrowCompletion(a newly created TypeError object).
    //          ii. Return ? IteratorClose(iteratorRecord, error).
    //      e. Let k be Completion(Get(nextItem, "0")).
    //      f. IfAbruptCloseIterator(k, iteratorRecord).
    //      g. Let v be Completion(Get(nextItem, "1")).
    //      h. IfAbruptCloseIterator(v, iteratorRecord).
    //      i. Let status be Completion(Call(adder, target, « k, v »)).
    //      j. IfAbruptCloseIterator(status, iteratorRecord).

    let iterator_record = get_iterator(iterable, IteratorKind::Sync)?;
    loop {
        match iterator_step(&iterator_record)? {
            None => break Ok(NormalCompletion::from(target.clone())),
            Some(next) => {
                let next_item = iterator_value(&next)?;
                match next_item {
                    ECMAScriptValue::Undefined
                    | ECMAScriptValue::Null
                    | ECMAScriptValue::Boolean(_)
                    | ECMAScriptValue::String(_)
                    | ECMAScriptValue::Number(_)
                    | ECMAScriptValue::BigInt(_)
                    | ECMAScriptValue::Symbol(_) => {
                        let error = create_type_error("Iterator for mapping adder must return an object value");
                        return iterator_close(&iterator_record, Err(error));
                    }
                    ECMAScriptValue::Object(next_item) => {
                        let k = match next_item.get(&"0".into()) {
                            Ok(val) => val,
                            Err(err) => {
                                return iterator_close(&iterator_record, Err(err));
                            }
                        };
                        let v = match next_item.get(&"1".into()) {
                            Ok(val) => val,
                            Err(err) => {
                                return iterator_close(&iterator_record, Err(err));
                            }
                        };
                        match call(adder, target, &[k, v]) {
                            Ok(_) => (),
                            Err(err) => {
                                return iterator_close(&iterator_record, Err(err));
                            }
                        }
                    }
                }
            }
        }
    }
}

pub(crate) fn provision_map_intrinsic(realm: &Rc<RefCell<Realm>>) {
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();

    // The Map constructor:
    //
    // * is %Map%.
    // * is the initial value of the "Map" property of the global object.
    // * creates and initializes a new Map when called as a constructor.
    // * is not intended to be called as a function and will throw an exception when called in that manner.
    // * may be used as the value in an extends clause of a class definition. Subclass constructors that intend to
    //   inherit the specified Map behaviour must include a super call to the Map constructor to create and initialize
    //   the subclass instance with the internal state necessary to support the Map.prototype built-in methods.

    // Properties of the Map Constructor
    // The Map constructor:
    //
    // * has a [[Prototype]] internal slot whose value is %Function.prototype%.

    let map_constructor = create_builtin_function(
        Box::new(map_constructor_function),
        Some(ConstructorKind::Base),
        0.0,
        PropertyKey::from("Map"),
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
                f64::from($length),
                key.clone(),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm.clone()),
                Some(function_prototype.clone()),
                None,
            );
            define_property_or_throw(
                &map_constructor,
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

    constructor_function!(map_groupby, "groupBy", 2);
    let species_sym = wks(WksId::Species);
    let species_fcn = create_builtin_function(
        Box::new(map_species),
        None,
        0.0,
        species_sym.clone().into(),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype.clone()),
        Some("get".into()),
    );
    let species_ppd = PotentialPropertyDescriptor::new().get(species_fcn).enumerable(false).configurable(true);
    define_property_or_throw(&map_constructor, species_sym, species_ppd).unwrap();

    let map_prototype = ordinary_object_create(Some(object_prototype.clone()));
    define_property_or_throw(
        &map_constructor,
        "prototype",
        PotentialPropertyDescriptor::new()
            .value(map_prototype.clone())
            .writable(false)
            .enumerable(false)
            .configurable(false),
    )
    .unwrap();

    // Prototype function properties
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
                &map_prototype,
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

    prototype_function!(map_prototype_clear, "clear", 0); // ( )
    prototype_function!(map_prototype_delete, "delete", 1); // ( key )
    prototype_function!(map_prototype_entries, "entries", 0); // ( )
    prototype_function!(map_prototype_foreach, "forEach", 1); // ( callback [ , thisArg ] )
    prototype_function!(map_prototype_get, "get", 1); // ( key )
    prototype_function!(map_prototype_has, "has", 1); // ( key )
    prototype_function!(map_prototype_keys, "keys", 0); // ( )
    prototype_function!(map_prototype_set, "set", 2); // ( key, value )
    prototype_function!(map_prototype_values, "values", 0); // ( )

    // Map.prototype.constructor
    define_property_or_throw(
        &map_prototype,
        "constructor",
        PotentialPropertyDescriptor::new()
            .value(map_constructor.clone())
            .writable(true)
            .enumerable(false)
            .configurable(true),
    )
    .unwrap();

    // get Map.prototype.size
    let key = PropertyKey::from("size");
    let function = create_builtin_function(
        Box::new(map_prototype_get_size),
        None,
        0.0,
        key.clone(),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype.clone()),
        Some(JSString::from("get")),
    );
    define_property_or_throw(
        &map_prototype,
        key,
        PotentialPropertyDescriptor::new().get(ECMAScriptValue::Object(function)).enumerable(false).configurable(true),
    )
    .unwrap();

    // Map.prototype [ %Symbol.iterator% ] ( )
    let key = PropertyKey::from(wks(WksId::Iterator));
    let function = map_prototype.get(&PropertyKey::from("entries")).unwrap();
    define_property_or_throw(
        &map_prototype,
        key,
        PotentialPropertyDescriptor::new().value(function).writable(true).enumerable(false).configurable(true),
    )
    .unwrap();

    // Map.prototype [ %Symbol.toStringTag% ]
    let key = PropertyKey::from(wks(WksId::ToStringTag));
    define_property_or_throw(
        &map_prototype,
        key,
        PotentialPropertyDescriptor::new().value("Map").writable(false).enumerable(false).configurable(true),
    )
    .unwrap();

    // The %MapIteratorPrototype% object:
    //
    //  * has properties that are inherited by all Map Iterator objects.
    //  * is an ordinary object.
    //  * has a [[Prototype]] internal slot whose value is %Iterator.prototype%.
    //
    let iterator_prototype = realm.borrow().intrinsics.iterator_prototype.clone();
    let map_iterator_prototype = ordinary_object_create(Some(iterator_prototype));

    macro_rules! iterator_prototype_function {
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
                &map_iterator_prototype,
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
    iterator_prototype_function!(map_iterator_prototype_next, "next", 0);

    // %MapIteratorPrototype% [ %Symbol.toStringTag% ]
    // The initial value of the %Symbol.toStringTag% property is the String value "Map Iterator".
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true }.
    let key = PropertyKey::from(wks(WksId::ToStringTag));
    define_property_or_throw(
        &map_iterator_prototype,
        key,
        PotentialPropertyDescriptor::new().value("Map Iterator").writable(false).enumerable(false).configurable(true),
    )
    .unwrap();

    realm.borrow_mut().intrinsics.map = map_constructor;
    realm.borrow_mut().intrinsics.map_prototype = map_prototype;
    realm.borrow_mut().intrinsics.map_iterator_prototype = map_iterator_prototype;
}

#[derive(Debug, Clone)]
struct MapEntry {
    key: ECMAScriptValue,
    value: ECMAScriptValue,
}
#[derive(Debug, Default)]
struct MapInternals {
    write_index: usize,
    map: MultiMap<usize, ECMAScriptValue, MapEntry>,
}

#[derive(Debug)]
pub(crate) struct MapObject {
    common: RefCell<CommonObjectData>,
    map_data: RefCell<MapInternals>,
}

impl<'a> From<&'a MapObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a MapObject) -> Self {
        obj
    }
}

impl ObjectInterface for MapObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn uses_ordinary_get_prototype_of(&self) -> bool {
        true
    }
    fn kind(&self) -> ObjectTag {
        ObjectTag::Map
    }
    fn to_map_obj(&self) -> Option<&MapObject> {
        Some(self)
    }

    // [[GetPrototypeOf]] ( )
    //
    // The [[GetPrototypeOf]] internal method of an ordinary object O takes no arguments and returns a normal completion
    // containing either an Object or null. It performs the following steps when called:
    //
    //  1. Return OrdinaryGetPrototypeOf(O).
    fn get_prototype_of(&self) -> Completion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }

    // [[SetPrototypeOf]] ( V )
    //
    // The [[SetPrototypeOf]] internal method of an ordinary object O takes argument V (an Object or null). It performs
    // the following steps when called:
    //
    //  1. Return ! OrdinarySetPrototypeOf(O, V).
    fn set_prototype_of(&self, obj: Option<Object>) -> Completion<bool> {
        Ok(ordinary_set_prototype_of(self, obj))
    }

    // [[IsExtensible]] ( )
    //
    // The [[IsExtensible]] internal method of an ordinary object O takes no arguments. It performs the following steps
    // when called:
    //
    //  1. Return ! OrdinaryIsExtensible(O).
    fn is_extensible(&self) -> Completion<bool> {
        Ok(ordinary_is_extensible(self))
    }

    // [[PreventExtensions]] ( )
    //
    // The [[PreventExtensions]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    //  1. Return ! OrdinaryPreventExtensions(O).
    fn prevent_extensions(&self) -> Completion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }

    // [[GetOwnProperty]] ( P )
    //
    // The [[GetOwnProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ! OrdinaryGetOwnProperty(O, P).
    fn get_own_property(&self, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }

    // [[DefineOwnProperty]] ( P, Desc )
    //
    // The [[DefineOwnProperty]] internal method of an ordinary object O takes arguments P (a property key) and Desc (a
    // Property Descriptor). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryDefineOwnProperty(O, P, Desc).
    fn define_own_property(&self, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool> {
        ordinary_define_own_property(self, key, desc)
    }

    // [[HasProperty]] ( P )
    //
    // The [[HasProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryHasProperty(O, P).
    fn has_property(&self, key: &PropertyKey) -> Completion<bool> {
        ordinary_has_property(self, key)
    }

    // [[Get]] ( P, Receiver )
    //
    // The [[Get]] internal method of an ordinary object O takes arguments P (a property key) and Receiver (an
    // ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryGet(O, P, Receiver).
    fn get(&self, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        ordinary_get(self, key, receiver)
    }

    // [[Set]] ( P, V, Receiver )
    //
    // The [[Set]] internal method of an ordinary object O takes arguments P (a property key), V (an ECMAScript language
    // value), and Receiver (an ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinarySet(O, P, V, Receiver).
    fn set(&self, key: PropertyKey, v: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        ordinary_set(self, key, v, receiver)
    }

    // [[Delete]] ( P )
    //
    // The [[Delete]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryDelete(O, P).
    fn delete(&self, key: &PropertyKey) -> Completion<bool> {
        ordinary_delete(self, key)
    }

    // [[OwnPropertyKeys]] ( )
    //
    // The [[OwnPropertyKeys]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    // 1. Return ! OrdinaryOwnPropertyKeys(O).
    fn own_property_keys(&self) -> Completion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(self))
    }
}

impl MapObject {
    fn new(prototype: Option<Object>) -> Self {
        Self {
            common: RefCell::new(CommonObjectData::new(prototype, true, DATE_OBJECT_SLOTS)),
            map_data: RefCell::new(MapInternals::default()),
        }
    }

    pub(crate) fn object(prototype: Option<Object>) -> Object {
        Object { o: Rc::new(Self::new(prototype)) }
    }
}

fn map_constructor_function(
    _this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Map ( [ iterable ] )
    // This function performs the following steps when called:
    //
    //  1. If NewTarget is undefined, throw a TypeError exception.
    //  2. Let map be ? OrdinaryCreateFromConstructor(NewTarget, "%Map.prototype%", « [[MapData]] »).
    //  3. Set map.[[MapData]] to a new empty List.
    //  4. If iterable is either undefined or null, return map.
    //  5. Let adder be ? Get(map, "set").
    //  6. If IsCallable(adder) is false, throw a TypeError exception.
    //  7. Return ? AddEntriesFromIterable(map, iterable, adder).
    //
    // Note
    // If the parameter iterable is present, it is expected to be an object that implements an %Symbol.iterator% method
    // that returns an iterator object that produces a two element array-like object whose first element is a value that
    // will be used as a Map key and whose second element is the value to associate with that key.
    let mut args = FuncArgs::from(arguments);
    let iterable = args.next_arg();
    let nt = new_target.ok_or_else(|| create_type_error("Map must be called as a constructor"))?;
    let map = nt.ordinary_create_from_constructor(IntrinsicId::MapPrototype, MapObject::object)?;
    if iterable == ECMAScriptValue::Undefined || iterable == ECMAScriptValue::Null {
        Ok(ECMAScriptValue::Object(map))
    } else {
        let adder = map.get(&PropertyKey::from("set"))?;
        if adder.is_callable() {
            let map = ECMAScriptValue::Object(map);
            add_entries_from_iterable(&map, &iterable, &adder).map(|nc| ECMAScriptValue::try_from(nc).unwrap())
        } else {
            Err(create_type_error("Derived classes of Map must have a callable `set` function"))
        }
    }
}

fn map_groupby(_: &ECMAScriptValue, _: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
    // Map.groupBy ( items, callback )
    //
    // Note
    // callback should be a function that accepts two arguments. groupBy calls callback once for each element in items,
    // in ascending order, and constructs a new Map. Each value returned by callback is used as a key in the Map. For
    // each such key, the result Map has an entry whose key is that key and whose value is an array containing all the
    // elements for which callback returned that key.
    //
    // callback is called with two arguments: the value of the element and the index of the element.
    //
    // The return value of groupBy is a Map.
    //
    // This function performs the following steps when called:
    //
    // 1. Let groups be ? GroupBy(items, callback, collection).
    // 2. Let map be ! Construct(%Map%).
    // 3. For each Record { [[Key]], [[Elements]] } g of groups, do
    //    a. Let elements be CreateArrayFromList(g.[[Elements]]).
    //    b. Let entry be the Record { [[Key]]: g.[[Key]], [[Value]]: elements }.
    //    c. Append entry to map.[[MapData]].
    // 4. Return map.
    let mut args = FuncArgs::from(arguments);
    let items = args.next_arg();
    let callback = args.next_arg();

    let groups = group_by(&items, &callback, Coercion::Collection)?;
    let map = construct(&intrinsic(IntrinsicId::Map), &[], None).expect("Core constructor should work");
    let map_clone = map.clone();
    let m = map.to_map_object().unwrap();
    let mut data = m.map_data.borrow_mut();
    for g in groups {
        let elements = ECMAScriptValue::Object(create_array_from_list(&g.elements));
        let index = data.write_index;
        data.map.insert(index, g.key.clone(), MapEntry { key: g.key, value: elements });
        data.write_index += 1;
    }
    Ok(map_clone)
}

#[expect(clippy::unnecessary_wraps)]
fn map_species(this_value: &ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
    // get Map [ %Symbol.species% ]
    // Map[%Symbol.species%] is an accessor property whose set accessor function is undefined. Its get accessor function
    // performs the following steps when called:
    //
    // 1. Return the this value.
    //
    // Note
    // Methods that create derived collection objects should call %Symbol.species% to determine the constructor to use
    // to create the derived objects. Subclass constructor may over-ride %Symbol.species% to change the default
    // constructor assignment.
    Ok(this_value.clone())
}

fn map_prototype_clear(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Map.prototype.clear ( )
    // This method performs the following steps when called:
    //
    // 1. Let M be the this value.
    // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
    // 3. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
    //    a. Set p.[[Key]] to empty.
    //    b. Set p.[[Value]] to empty.
    // 4. Return undefined.
    //
    // Note
    // The existing [[MapData]] List is preserved because there may be existing Map Iterator objects that are suspended
    // midway through iterating over that List.
    let m =
        this_value.to_map_object().ok_or_else(|| create_type_error("Map.prototype.clear requires a maplike object"))?;
    let mut mapdata = m.map_data.borrow_mut();
    let max = mapdata.write_index;
    for idx in 0..max {
        mapdata.map.remove(&idx);
    }
    Ok(ECMAScriptValue::Undefined)
}

fn map_prototype_delete(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Map.prototype.delete ( key )
    // This method performs the following steps when called:
    //
    // 1. Let M be the this value.
    // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
    // 3. Set key to CanonicalizeKeyedCollectionKey(key).
    // 4. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
    //    a. If p.[[Key]] is not empty and SameValue(p.[[Key]], key) is true, then
    //       i. Set p.[[Key]] to empty.
    //       ii. Set p.[[Value]] to empty.
    //       iii. Return true.
    // 5. Return false.
    //
    // Note
    // The value empty is used as a specification device to indicate that an entry has been deleted. Actual
    // implementations may take other actions such as physically removing the entry from internal data structures.
    let mut args = FuncArgs::from(arguments);
    let key = args.next_arg().canonicalize_keyed_collection_key();
    let m = this_value
        .to_map_object()
        .ok_or_else(|| create_type_error("Map.prototype.delete requires a maplike object"))?;
    let mut mapdata = m.map_data.borrow_mut();
    let result = mapdata.map.remove_alt(&key).is_some();
    Ok(ECMAScriptValue::Boolean(result))
}

fn map_prototype_entries(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Map.prototype.entries ( )
    // This method performs the following steps when called:
    //
    // 1. Let M be the this value.
    // 2. Return ? CreateMapIterator(M, key+value).
    create_map_iterator(this_value, KeyValueKind::KeyValue)
}

fn map_prototype_foreach(
    this: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Map.prototype.forEach ( callback [ , thisArg ] )
    // This method performs the following steps when called:
    //
    // 1. Let M be the this value.
    // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
    // 3. If IsCallable(callback) is false, throw a TypeError exception.
    // 4. Let entries be M.[[MapData]].
    // 5. Let numEntries be the number of elements in entries.
    // 6. Let index be 0.
    // 7. Repeat, while index < numEntries,
    //    a. Let e be entries[index].
    //    b. Set index to index + 1.
    //    c. If e.[[Key]] is not empty, then
    //       i. Perform ? Call(callback, thisArg, « e.[[Value]], e.[[Key]], M »).
    //       ii. NOTE: The number of elements in entries may have increased during execution of callback.
    //       iii. Set numEntries to the number of elements in entries.
    // 8. Return undefined.
    //
    // Note
    // callback should be a function that accepts three arguments. forEach calls callback once for each key/value pair
    // present in the Map, in key insertion order. callback is called only for keys of the Map which actually exist; it
    // is not called for keys that have been deleted from the Map.
    //
    // If a thisArg parameter is provided, it will be used as the this value for each invocation of callback. If it is
    // not provided, undefined is used instead.
    //
    // callback is called with three arguments: the value of the item, the key of the item, and the Map being traversed.
    //
    // forEach does not directly mutate the object on which it is called but the object may be mutated by the calls to
    // callback. Each entry of a map's [[MapData]] is only visited once. New keys added after the call to forEach begins
    // are visited. A key will be revisited if it is deleted after it has been visited and then re-added before the
    // forEach call completes. Keys that are deleted after the call to forEach begins and before being visited are not
    // visited unless the key is added again before the forEach call completes.
    let mut args = FuncArgs::from(arguments);
    let callback = args.next_arg();
    let this_value = args.next_arg();
    let m_obj = this.clone();
    let m = this.to_map_object().ok_or_else(|| create_type_error("Map.prototype.forEach requires a maplike object"))?;
    if !callback.is_callable() {
        return Err(create_type_error("Map.prototype.forEach: callable must be a function"));
    }
    let mut num_entries = m.map_data.borrow().write_index;
    let mut index = 0;
    while index < num_entries {
        let entry = {
            let map_data = m.map_data.borrow();
            map_data.map.get(&index).cloned()
        };
        index += 1;
        if let Some(e) = entry {
            call(&callback, &this_value, &[e.value, e.key, m_obj.clone()])?;
            num_entries = m.map_data.borrow().write_index;
        }
    }
    Ok(ECMAScriptValue::Undefined)
}

fn map_prototype_get(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Map.prototype.get ( key )
    // This method performs the following steps when called:
    //
    // 1. Let M be the this value.
    // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
    // 3. Set key to CanonicalizeKeyedCollectionKey(key).
    // 4. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
    //    a. If p.[[Key]] is not empty and SameValue(p.[[Key]], key) is true, return p.[[Value]].
    // 5. Return undefined.
    let mut args = FuncArgs::from(arguments);
    let key = args.next_arg().canonicalize_keyed_collection_key();
    let m =
        this_value.to_map_object().ok_or_else(|| create_type_error("Map.prototype.get requires a maplike object"))?;
    let map_data = m.map_data.borrow();
    let entry = map_data.map.get_alt(&key);
    let value = match entry {
        Some(item) => item.value.clone(),
        None => ECMAScriptValue::Undefined,
    };
    Ok(value)
}

fn map_prototype_has(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Map.prototype.has ( key )
    // This method performs the following steps when called:
    //
    // 1. Let M be the this value.
    // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
    // 3. Set key to CanonicalizeKeyedCollectionKey(key).
    // 4. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
    //    a. If p.[[Key]] is not empty and SameValue(p.[[Key]], key) is true, return true.
    // 5. Return false.
    let mut args = FuncArgs::from(arguments);
    let key = args.next_arg().canonicalize_keyed_collection_key();
    let m =
        this_value.to_map_object().ok_or_else(|| create_type_error("Map.prototype.get requires a maplike object"))?;
    let map_data = m.map_data.borrow();
    let result = map_data.map.contains_key_alt(&key);
    Ok(ECMAScriptValue::Boolean(result))
}

fn map_prototype_keys(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Map.prototype.keys ( )
    // This method performs the following steps when called:
    //
    // 1. Let M be the this value.
    // 2. Return ? CreateMapIterator(M, key).
    create_map_iterator(this_value, KeyValueKind::Key)
}

fn map_prototype_set(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Map.prototype.set ( key, value )
    // This method performs the following steps when called:
    //
    // 1. Let M be the this value.
    // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
    // 3. Set key to CanonicalizeKeyedCollectionKey(key).
    // 4. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
    //    a. If p.[[Key]] is not empty and SameValue(p.[[Key]], key) is true, then
    //       i. Set p.[[Value]] to value.
    //       ii. Return M.
    // 5. Let p be the Record { [[Key]]: key, [[Value]]: value }.
    // 6. Append p to M.[[MapData]].
    // 7. Return M.
    let mut args = FuncArgs::from(arguments);
    let key = args.next_arg();
    let value = args.next_arg();
    let m =
        this_value.to_map_object().ok_or_else(|| create_type_error("Map.prototype.set requires a maplike object"))?;
    let key = key.canonicalize_keyed_collection_key();
    let mut map = m.map_data.borrow_mut();
    match map.map.get_mut_alt(&key) {
        Some(entry) => {
            *entry = MapEntry { key, value };
        }
        _ => {
            let index = map.write_index;
            map.map.insert(index, key.clone(), MapEntry { key, value });
            map.write_index += 1;
        }
    }
    Ok(this_value.clone())
}

fn map_prototype_get_size(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // get Map.prototype.size
    // Map.prototype.size is an accessor property whose set accessor function is undefined. Its get accessor function
    // performs the following steps when called:
    //
    // 1. Let M be the this value.
    // 2. Perform ? RequireInternalSlot(M, [[MapData]]).
    // 3. Let count be 0.
    // 4. For each Record { [[Key]], [[Value]] } p of M.[[MapData]], do
    //    a. If p.[[Key]] is not empty, set count to count + 1.
    // 5. Return 𝔽(count).
    let m =
        this_value.to_map_object().ok_or_else(|| create_type_error("Map.prototype.size requires a maplike object"))?;
    let map_data = m.map_data.borrow();
    // dammit, multi-map doesn't have _len_???
    let result = map_data.map.iter().count();
    Ok(ECMAScriptValue::from(result))
}

fn map_prototype_values(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Map.prototype.values ( )
    // This method performs the following steps when called:
    //
    // 1. Let M be the this value.
    // 2. Return ? CreateMapIterator(M, value).
    create_map_iterator(this_value, KeyValueKind::Value)
}

impl ECMAScriptValue {
    #[must_use]
    pub(crate) fn canonicalize_keyed_collection_key(self) -> Self {
        // CanonicalizeKeyedCollectionKey ( key )
        // The abstract operation CanonicalizeKeyedCollectionKey takes argument key (an ECMAScript language value) and
        // returns an ECMAScript language value. It performs the following steps when called:
        //
        // 1. If key is -0𝔽, return +0𝔽.
        // 2. Return key.
        match self {
            ECMAScriptValue::Number(-0.0) => ECMAScriptValue::Number(0.0),
            _ => self,
        }
    }
}

#[derive(Debug, Default)]
struct KeyedGroup {
    // Note: The second and third values here should be the same. The only reason I do it this way is because this
    // MultiMap doesn't let me use one key to look up the other key.
    map: MultiMap<usize, ECMAScriptValue, (ECMAScriptValue, Vec<ECMAScriptValue>)>,
    write_index: usize,
}

fn add_value_to_keyed_group(groups: &mut KeyedGroup, key: ECMAScriptValue, value: ECMAScriptValue) {
    // AddValueToKeyedGroup ( groups, key, value )
    // The abstract operation AddValueToKeyedGroup takes arguments groups (a List of Records with fields [[Key]] (an
    // ECMAScript language value) and [[Elements]] (a List of ECMAScript language values)), key (an ECMAScript language
    // value), and value (an ECMAScript language value) and returns unused. It performs the following steps when called:
    //
    // 1. For each Record { [[Key]], [[Elements]] } g of groups, do
    //    a. If SameValue(g.[[Key]], key) is true, then
    //       i. Assert: Exactly one element of groups meets this criterion.
    //       ii. Append value to g.[[Elements]].
    //       iii. Return unused.
    // 2. Let group be the Record { [[Key]]: key, [[Elements]]: « value » }.
    // 3. Append group to groups.
    // 4. Return unused.
    match groups.map.get_mut_alt(&key) {
        Some(entry) => {
            entry.1.push(value);
        }
        _ => {
            let index = groups.write_index;
            groups.map.insert(index, key.clone(), (key, vec![value]));
            groups.write_index += 1;
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[expect(dead_code)]
enum Coercion {
    Collection,
    Property,
}

#[derive(Debug)]
struct GroupRecord {
    key: ECMAScriptValue,
    elements: Vec<ECMAScriptValue>,
}

impl From<KeyedGroup> for Vec<GroupRecord> {
    fn from(value: KeyedGroup) -> Self {
        (0..value.write_index)
            .filter_map(|index| {
                value
                    .map
                    .get(&index)
                    .map(|(key, elements)| GroupRecord { key: key.clone(), elements: elements.clone() })
            })
            .collect::<Vec<_>>()
    }
}

fn group_by(
    items: &ECMAScriptValue,
    callback: &ECMAScriptValue,
    key_coercion: Coercion,
) -> Completion<Vec<GroupRecord>> {
    // GroupBy ( items, callback, keyCoercion )
    // The abstract operation GroupBy takes arguments items (an ECMAScript language value), callback (an ECMAScript
    // language value), and keyCoercion (property or collection) and returns either a normal completion containing a
    // List of Records with fields [[Key]] (an ECMAScript language value) and [[Elements]] (a List of ECMAScript
    // language values), or a throw completion. It performs the following steps when called:
    //
    // 1. Perform ? RequireObjectCoercible(items).
    // 2. If IsCallable(callback) is false, throw a TypeError exception.
    // 3. Let groups be a new empty List.
    // 4. Let iteratorRecord be ? GetIterator(items, sync).
    // 5. Let k be 0.
    // 6. Repeat,
    //    a. If k ≥ 2**53 - 1, then
    //       i. Let error be ThrowCompletion(a newly created TypeError object).
    //       ii. Return ? IteratorClose(iteratorRecord, error).
    //    b. Let next be ? IteratorStepValue(iteratorRecord).
    //    c. If next is done, then
    //       i. Return groups.
    //    d. Let value be next.
    //    e. Let key be Completion(Call(callback, undefined, « value, 𝔽(k) »)).
    //    f. IfAbruptCloseIterator(key, iteratorRecord).
    //    g. If keyCoercion is property, then
    //       i. Set key to Completion(ToPropertyKey(key)).
    //       ii. IfAbruptCloseIterator(key, iteratorRecord).
    //    h. Else,
    //       i. Assert: keyCoercion is collection.
    //       ii. Set key to CanonicalizeKeyedCollectionKey(key).
    //    i. Perform AddValueToKeyedGroup(groups, key, value).
    //    j. Set k to k + 1.
    require_object_coercible(items)?;
    if !callback.is_callable() {
        return Err(create_type_error("Map.groupBy: callable must be callable"));
    }
    let mut groups = KeyedGroup::default();
    let iterator_record = get_iterator(items, IteratorKind::Sync)?;
    let mut k = 0;
    loop {
        if k >= (2 ^ 53) - 1 {
            let error = create_type_error("Map.groupBy: too many items");
            let result = iterator_record.close::<ECMAScriptValue>(Err(error)).unwrap_err();
            return Err(result);
        }
        let next = iterator_record.step_value()?;
        match next {
            Some(value) => {
                let key = call(callback, &ECMAScriptValue::Undefined, &[value.clone(), ECMAScriptValue::from(k)]);
                match key {
                    Err(err) => {
                        let result = iterator_record.close::<ECMAScriptValue>(Err(err)).unwrap_err();
                        return Err(result);
                    }
                    Ok(key) => {
                        let key = match key_coercion {
                            Coercion::Property => match key.to_property_key() {
                                Err(err) => {
                                    let result = iterator_record.close::<ECMAScriptValue>(Err(err)).unwrap_err();
                                    return Err(result);
                                }
                                Ok(key) => ECMAScriptValue::from(key),
                            },
                            Coercion::Collection => key.canonicalize_keyed_collection_key(),
                        };
                        add_value_to_keyed_group(&mut groups, key, value);
                        k += 1;
                    }
                }
            }
            _ => {
                return Ok(groups.into());
            }
        }
    }
}

async fn map_iterator(
    co: Co<ECMAScriptValue, Completion<ECMAScriptValue>>,
    map: ECMAScriptValue,
    kind: KeyValueKind,
) -> Completion<ECMAScriptValue> {
    // a. Let entries be map.[[MapData]].
    // b. Let index be 0.
    // c. Let numEntries be the number of elements in entries.
    // d. Repeat, while index < numEntries,
    //    i. Let e be entries[index].
    //    ii. Set index to index + 1.
    //    iii. If e.[[Key]] is not empty, then
    //         1. If kind is key, then
    //            a. Let result be e.[[Key]].
    //         2. Else if kind is value, then
    //            a. Let result be e.[[Value]].
    //         3. Else,
    //            a. Assert: kind is key+value.
    //            b. Let result be CreateArrayFromList(« e.[[Key]], e.[[Value]] »).
    //         4. Perform ? GeneratorYield(CreateIteratorResultObject(result, false)).
    //         5. NOTE: The number of elements in entries may have increased while execution of this abstract
    //            operation was paused by GeneratorYield.
    //         6. Set numEntries to the number of elements in entries.
    // e. Return undefined.
    let map_obj = map.to_map_object().expect("Only maps get here");
    let mut num_entries = map_obj.map_data.borrow().write_index;
    let mut index = 0;
    while index < num_entries {
        let lookup_result = {
            let map_data = map_obj.map_data.borrow();
            let e = map_data.map.get(&index);
            index += 1;
            if let Some(e) = e {
                let result = match kind {
                    KeyValueKind::Key => e.key.clone(),
                    KeyValueKind::Value => e.value.clone(),
                    KeyValueKind::KeyValue => {
                        ECMAScriptValue::Object(create_array_from_list(&[e.key.clone(), e.value.clone()]))
                    }
                };
                Some(result)
            } else {
                None
            }
        };
        if let Some(result) = lookup_result {
            let ro = ECMAScriptValue::Object(create_iter_result_object(result, false));
            generator_yield(&co, ro).await?;
            num_entries = map_obj.map_data.borrow().write_index;
        }
    }
    Ok(ECMAScriptValue::Undefined)
}

fn create_map_iterator(map: &ECMAScriptValue, kind: KeyValueKind) -> Completion<ECMAScriptValue> {
    // CreateMapIterator ( map, kind )
    // The abstract operation CreateMapIterator takes arguments map (an ECMAScript language value) and kind (key+value,
    // key, or value) and returns either a normal completion containing a Generator or a throw completion. It is used to
    // create iterator objects for Map methods that return such iterators. It performs the following steps when called:
    //
    // 1. Perform ? RequireInternalSlot(map, [[MapData]]).
    // 2. Let closure be a new Abstract Closure with no parameters that captures map and kind and performs the following
    //    steps when called:
    //    a-e. : See `map_iterator`, above.
    // 3. Return CreateIteratorFromClosure(closure, "%MapIteratorPrototype%", %MapIteratorPrototype%).

    map.to_map_object().ok_or_else(|| create_type_error("Map.prototype.set requires a maplike object"))?;
    let map = map.clone();
    let closure = move |co| map_iterator(co, map, kind);

    Ok(ECMAScriptValue::Object(create_iterator_from_closure(
        asyncfn_wrap(closure),
        "%MapIteratorPrototype%",
        Some(intrinsic(IntrinsicId::MapIteratorPrototype)),
    )))
}

fn map_iterator_prototype_next(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // %MapIteratorPrototype%.next ( )
    //  1. Return ? GeneratorResume(this value, empty, "%MapIteratorPrototype%").
    generator_resume(this_value, ECMAScriptValue::Undefined, "%MapIteratorPrototype%")
}

#[cfg(test)]
mod tests;
