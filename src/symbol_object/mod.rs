//! Symbol Objects
//!
//! See [Symbol Objects](https://tc39.es/ecma262/#sec-symbol-objects) in ECMA-262.

use super::*;
use bimap::BiMap;
use std::cell::RefCell;
use std::rc::Rc;

// Properties of Symbol Instances
//
// Symbol instances are ordinary objects that inherit properties from the Symbol prototype object. Symbol instances have
// a [[SymbolData]] internal slot. The [[SymbolData]] internal slot is the Symbol value represented by this Symbol
// object.

pub trait SymbolObjectInterface: ObjectInterface {
    fn symbol_data(&self) -> &Symbol;
}

#[derive(Debug)]
pub struct SymbolObject {
    common: RefCell<CommonObjectData>,
    symbol_data: Symbol,
}

impl<'a> From<&'a SymbolObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a SymbolObject) -> Self {
        obj
    }
}

impl ObjectInterface for SymbolObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }

    fn uses_ordinary_get_prototype_of(&self) -> bool {
        true
    }

    fn id(&self) -> usize {
        self.common.borrow().objid
    }

    fn to_symbol_obj(&self) -> Option<&dyn SymbolObjectInterface> {
        Some(self)
    }
    fn is_symbol_object(&self) -> bool {
        true
    }

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

impl SymbolObjectInterface for SymbolObject {
    fn symbol_data(&self) -> &Symbol {
        &self.symbol_data
    }
}

impl SymbolObject {
    pub fn new(prototype: Option<Object>, sym: Symbol) -> Self {
        Self { common: RefCell::new(CommonObjectData::new(prototype, true, SYMBOL_OBJECT_SLOTS)), symbol_data: sym }
    }
    pub fn object(prototype: Option<Object>, sym: Symbol) -> Object {
        Object { o: Rc::new(Self::new(prototype, sym)) }
    }
}

impl From<Symbol> for Object {
    fn from(sym: Symbol) -> Self {
        let symbol_proto = intrinsic(IntrinsicId::SymbolPrototype);
        SymbolObject::object(Some(symbol_proto), sym)
    }
}

pub fn provision_symbol_intrinsic(realm: &Rc<RefCell<Realm>>) {
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();

    // The Symbol Constructor
    //
    // The Symbol constructor:
    //
    //  * is %Symbol%.
    //  * is the initial value of the "Symbol" property of the global object.
    //  * returns a new Symbol value when called as a function.
    //  * is not intended to be used with the new operator.
    //  * is not intended to be subclassed.
    //  * may be used as the value of an extends clause of a class definition but a super call to it will cause an
    //    exception.
    //
    // Properties of the Symbol Constructor
    //
    // The Symbol constructor:
    //
    //  * has a [[Prototype]] internal slot whose value is %Function.prototype%.
    let symbol_constructor = create_builtin_function(
        Box::new(symbol_constructor_function),
        Some(ConstructorKind::Base),
        0.0,
        PropertyKey::from("Symbol"),
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
                &symbol_constructor,
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
    constructor_function!(symbol_for, "for", 1.0);
    constructor_function!(symbol_key_for, "keyFor", 1.0);

    // Constructor Data Properties
    macro_rules! constructor_data {
        ( $name:expr_2021, $value:expr_2021, $writable:expr_2021, $enumerable:expr_2021, $configurable:expr_2021 ) => {
            define_property_or_throw(
                &symbol_constructor,
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
    constructor_data!("asyncIterator", wks(WksId::AsyncIterator), false, false, false);
    constructor_data!("hasInstance", wks(WksId::HasInstance), false, false, false);
    constructor_data!("isConcatSpreadable", wks(WksId::IsConcatSpreadable), false, false, false);
    constructor_data!("iterator", wks(WksId::Iterator), false, false, false);
    constructor_data!("match", wks(WksId::Match), false, false, false);
    constructor_data!("matchAll", wks(WksId::MatchAll), false, false, false);
    constructor_data!("replace", wks(WksId::Replace), false, false, false);
    constructor_data!("search", wks(WksId::Search), false, false, false);
    constructor_data!("species", wks(WksId::Species), false, false, false);
    constructor_data!("split", wks(WksId::Split), false, false, false);
    constructor_data!("toPrimitive", wks(WksId::ToPrimitive), false, false, false);
    constructor_data!("toStringTag", wks(WksId::ToStringTag), false, false, false);
    constructor_data!("unscopables", wks(WksId::Unscopables), false, false, false);

    // The Symbol prototype object:
    //
    //  * is %Symbol.prototype%.
    //  * is an ordinary object.
    //  * is not a Symbol instance and does not have a [[SymbolData]] internal slot.
    //  * has a [[Prototype]] internal slot whose value is %Object.prototype%.
    let symbol_prototype = ordinary_object_create(Some(object_prototype));

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
                &symbol_prototype,
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
    prototype_function!(symbol_to_string, "toString", 0.0);
    prototype_function!(symbol_value_of, "valueOf", 0.0);

    macro_rules! prototype_data {
        ( $name:expr_2021, $value:expr_2021, $writable:expr_2021, $enumerable:expr_2021, $configurable:expr_2021 ) => {
            define_property_or_throw(
                &symbol_prototype,
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

    constructor_data!("prototype", symbol_prototype.clone(), false, false, false);
    prototype_data!("constructor", symbol_constructor.clone(), true, false, true);

    let descriptor_getter = create_builtin_function(
        Box::new(symbol_description),
        None,
        0.0,
        PropertyKey::from("description"),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype.clone()),
        Some("get".into()),
    );
    define_property_or_throw(
        &symbol_prototype,
        "description",
        PotentialPropertyDescriptor::new().get(descriptor_getter).enumerable(false).configurable(true),
    )
    .unwrap();
    let to_prop_sym = wks(WksId::ToPrimitive);
    let to_primitive_func = create_builtin_function(
        Box::new(symbol_value_of),
        None,
        1.0,
        PropertyKey::from(to_prop_sym.clone()),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype),
        None,
    );
    define_property_or_throw(
        &symbol_prototype,
        to_prop_sym,
        PotentialPropertyDescriptor::new().value(to_primitive_func).writable(true).enumerable(false).configurable(true),
    )
    .unwrap();
    let to_tag_sym = wks(WksId::ToStringTag);
    define_property_or_throw(
        &symbol_prototype,
        to_tag_sym,
        PotentialPropertyDescriptor::new().value("Symbol").writable(false).enumerable(false).configurable(false),
    )
    .unwrap();

    realm.borrow_mut().intrinsics.symbol = symbol_constructor;
    realm.borrow_mut().intrinsics.symbol_prototype = symbol_prototype;
}

/// Symbol
///
/// Symbol is a built-in object whose constructor returns a symbol primitive — also called a Symbol value or just a
/// Symbol — that's guaranteed to be unique. Symbols are often used to add unique property keys to an object that won't
/// collide with keys any other code might add to the object, and which are hidden from any mechanisms other code will
/// typically use to access the object. That enables a form of weak encapsulation, or a weak form of information
/// hiding.
///
/// Every `Symbol()` call is guaranteed to return a unique Symbol. Every `Symbol.for("key")` call will always return
/// the same Symbol for a given value of `"key"`. When `Symbol.for("key")` is called, if a Symbol with the given key
/// can be found in the global Symbol registry, that Symbol is returned. Otherwise, a new Symbol is created, added to
/// the global Symbol registry under the given key, and returned.
fn symbol_constructor_function(
    _this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Symbol ( [ description ] )
    // When Symbol is called with optional argument description, the following steps are taken:
    //
    // 1. If NewTarget is not undefined, throw a TypeError exception.
    // 2. If description is undefined, let descString be undefined.
    // 3. Else, let descString be ? ToString(description).
    // 4. Return a new unique Symbol value whose [[Description]] value is descString.
    if new_target.is_some() {
        Err(create_type_error("Symbol is not a constructor"))
    } else {
        let mut args = FuncArgs::from(arguments);
        let description = args.next_arg();
        let desc_string = match description {
            ECMAScriptValue::Undefined => None,
            _ => Some(to_string(description)?),
        };
        let new_symbol = Symbol::new(desc_string);
        Ok(new_symbol.into())
    }
}

pub fn global_symbol(key: JSString) -> Symbol {
    let gsm = global_symbol_registry();
    let mut registry = gsm.borrow_mut();
    let maybe_sym = registry.symbol_by_key(&key);
    match maybe_sym {
        Some(sym) => sym,
        None => {
            let new_symbol = Symbol::new(Some(key.clone()));
            registry.add(key, new_symbol.clone());
            new_symbol
        }
    }
}

/// Symbol.for()
///
/// The `Symbol.for(key)` method searches for existing symbols in a runtime-wide symbol registry with the given key and
/// returns it if found. Otherwise a new symbol gets created in the global symbol registry with this key.
///
/// See [Symbol.for](https://tc39.es/ecma262/#sec-symbol.for) in ECMA-262.
fn symbol_for(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Symbol.for ( key )
    // When Symbol.for is called with argument key it performs the following steps:
    //
    //  1. Let stringKey be ? ToString(key).
    //  2. For each element e of the GlobalSymbolRegistry List, do
    //      a. If SameValue(e.[[Key]], stringKey) is true, return e.[[Symbol]].
    //  3. Assert: GlobalSymbolRegistry does not currently contain an entry for stringKey.
    //  4. Let newSymbol be a new unique Symbol value whose [[Description]] value is stringKey.
    //  5. Append the Record { [[Key]]: stringKey, [[Symbol]]: newSymbol } to the GlobalSymbolRegistry List.
    //  6. Return newSymbol.
    //
    // The GlobalSymbolRegistry is a List that is globally available. It is shared by all realms. Prior to the
    // evaluation of any ECMAScript code it is initialized as a new empty List. Elements of the GlobalSymbolRegistry
    // are Records with the structure defined in Table 62.
    //
    // Table 62: GlobalSymbolRegistry Record Fields
    // +------------+----------+--------------------------------------------------+
    // | Field Name | Value    | Usage                                            |
    // +------------+----------+--------------------------------------------------+
    // | [[Key]]    | a String | A string key used to globally identify a Symbol. |
    // +------------+----------+--------------------------------------------------+
    // | [[Symbol]] | a Symbol | A symbol that can be retrieved from any realm.   |
    // +------------+----------+--------------------------------------------------+
    let mut args = FuncArgs::from(arguments);
    let key = args.next_arg();
    let string_key = to_string(key)?;
    Ok(global_symbol(string_key).into())
}

/// Symbol.keyFor()
///
/// The Symbol.keyFor(sym) method retrieves a shared symbol key from the global symbol registry for the given symbol.
///
/// See [Symbol.keyFor](https://tc39.es/ecma262/#sec-symbol.keyfor) in ECMA-262.
fn symbol_key_for(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Symbol.keyFor ( sym )
    // When Symbol.keyFor is called with argument sym it performs the following steps:
    //
    //  1. If Type(sym) is not Symbol, throw a TypeError exception.
    //  2. For each element e of the GlobalSymbolRegistry List (see 20.4.2.2), do
    //      a. If SameValue(e.[[Symbol]], sym) is true, return e.[[Key]].
    //  3. Assert: GlobalSymbolRegistry does not currently contain an entry for sym.
    //  4. Return undefined.
    let mut args = FuncArgs::from(arguments);
    let sym = args.next_arg();
    match sym {
        ECMAScriptValue::Symbol(sym) => {
            let gsm = global_symbol_registry();
            let registry = gsm.borrow();
            let maybe_key = registry.key_by_symbol(&sym);
            match maybe_key {
                Some(key) => Ok(key.into()),
                None => Ok(ECMAScriptValue::Undefined),
            }
        }
        _ => Err(create_type_error("value is not a symbol")),
    }
}

fn this_symbol_value(this_value: &ECMAScriptValue) -> Completion<Symbol> {
    match this_value {
        ECMAScriptValue::Symbol(s) => Some(s.clone()),
        ECMAScriptValue::Object(o) => o.o.to_symbol_obj().map(|so| so.symbol_data().clone()),
        _ => None,
    }
    .ok_or_else(|| create_type_error("Not a symbol"))
}

fn symbol_to_string(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let sym = this_symbol_value(this_value)?;
    Ok(sym.descriptive_string().into())
}

fn symbol_value_of(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    Ok(this_symbol_value(this_value)?.into())
}

fn symbol_description(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let sym = this_symbol_value(this_value)?;
    Ok(sym.description().map_or(ECMAScriptValue::Undefined, ECMAScriptValue::from))
}

#[derive(Debug, Default)]
pub struct SymbolRegistry {
    symbols: BiMap<Symbol, JSString>,
}

impl SymbolRegistry {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn symbol_by_key(&self, key: &JSString) -> Option<Symbol> {
        self.symbols.get_by_right(key).cloned()
    }
    pub fn key_by_symbol(&self, sym: &Symbol) -> Option<JSString> {
        self.symbols.get_by_left(sym).cloned()
    }
    pub fn add(&mut self, key: JSString, sym: Symbol) {
        self.symbols.insert_no_overwrite(sym, key).unwrap();
    }
    pub fn len(&self) -> usize {
        self.symbols.len()
    }
    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }
}

#[cfg(test)]
mod tests;
