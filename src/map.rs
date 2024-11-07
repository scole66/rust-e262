// The Map Constructor

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
pub fn add_entries_from_iterable(
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

pub fn provision_map_intrinsic(realm: &Rc<RefCell<Realm>>) {
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
        PotentialPropertyDescriptor::new()
            .value(ECMAScriptValue::Object(function))
            .writable(true)
            .enumerable(false)
            .configurable(true),
    )
    .unwrap();

    // Map.prototype [ %Symbol.iterator% ] ( )
    let key = PropertyKey::from(wks(WksId::Iterator));
    let function = create_builtin_function(
        Box::new(map_prototype_symbol_iterator),
        None,
        0.0,
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
            .value(ECMAScriptValue::Object(function))
            .writable(true)
            .enumerable(false)
            .configurable(true),
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

    realm.borrow_mut().intrinsics.map = map_constructor;
}

macro_rules! todo_function {
    ( $name:ident ) => {
        fn $name(_: &ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
            todo!()
        }
    };
}

todo_function!(map_constructor_function);
todo_function!(map_groupby);
todo_function!(map_species);
todo_function!(map_prototype_clear);
todo_function!(map_prototype_delete);
todo_function!(map_prototype_entries);
todo_function!(map_prototype_foreach);
todo_function!(map_prototype_get);
todo_function!(map_prototype_has);
todo_function!(map_prototype_keys);
todo_function!(map_prototype_set);
todo_function!(map_prototype_get_size);
todo_function!(map_prototype_values);
todo_function!(map_prototype_symbol_iterator);

#[cfg(test)]
mod tests;
