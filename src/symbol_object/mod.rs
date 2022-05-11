use super::agent::*;
use super::cr::*;
use super::function_object::*;
use super::object::*;
use super::realm::*;
use super::values::*;
use std::cell::RefCell;
use std::rc::Rc;

/// Symbol Objects
///
/// See [Symbol Objects](https://tc39.es/ecma262/#sec-symbol-objects) in ECMA-262.

// Properties of Symbol Instances
//
// Symbol instances are ordinary objects that inherit properties from the Symbol prototype object. Symbol instances have
// a [[SymbolData]] internal slot. The [[SymbolData]] internal slot is the Symbol value represented by this Symbol
// object.

pub trait SymbolObjectInterface: ObjectInterface {
    fn symbol_data(&self) -> &RefCell<Option<Symbol>>;
}

#[derive(Debug)]
pub struct SymbolObject {
    common: RefCell<CommonObjectData>,
    symbol_data: RefCell<Option<Symbol>>,
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

    fn is_ordinary(&self) -> bool {
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

    fn get_prototype_of(&self, _agent: &mut Agent) -> Completion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }

    // [[SetPrototypeOf]] ( V )
    //
    // The [[SetPrototypeOf]] internal method of an ordinary object O takes argument V (an Object or null). It performs
    // the following steps when called:
    //
    //  1. Return ! OrdinarySetPrototypeOf(O, V).
    fn set_prototype_of(&self, _agent: &mut Agent, obj: Option<Object>) -> Completion<bool> {
        Ok(ordinary_set_prototype_of(self, obj))
    }

    // [[IsExtensible]] ( )
    //
    // The [[IsExtensible]] internal method of an ordinary object O takes no arguments. It performs the following steps
    // when called:
    //
    //  1. Return ! OrdinaryIsExtensible(O).
    fn is_extensible(&self, _agent: &mut Agent) -> Completion<bool> {
        Ok(ordinary_is_extensible(self))
    }

    // [[PreventExtensions]] ( )
    //
    // The [[PreventExtensions]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    //  1. Return ! OrdinaryPreventExtensions(O).
    fn prevent_extensions(&self, _agent: &mut Agent) -> Completion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }

    // [[GetOwnProperty]] ( P )
    //
    // The [[GetOwnProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ! OrdinaryGetOwnProperty(O, P).
    fn get_own_property(&self, _agent: &mut Agent, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }

    // [[DefineOwnProperty]] ( P, Desc )
    //
    // The [[DefineOwnProperty]] internal method of an ordinary object O takes arguments P (a property key) and Desc (a
    // Property Descriptor). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryDefineOwnProperty(O, P, Desc).
    fn define_own_property(&self, agent: &mut Agent, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool> {
        ordinary_define_own_property(agent, self, key, desc)
    }

    // [[HasProperty]] ( P )
    //
    // The [[HasProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryHasProperty(O, P).
    fn has_property(&self, agent: &mut Agent, key: &PropertyKey) -> Completion<bool> {
        ordinary_has_property(agent, self, key)
    }

    // [[Get]] ( P, Receiver )
    //
    // The [[Get]] internal method of an ordinary object O takes arguments P (a property key) and Receiver (an
    // ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryGet(O, P, Receiver).
    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        ordinary_get(agent, self, key, receiver)
    }

    // [[Set]] ( P, V, Receiver )
    //
    // The [[Set]] internal method of an ordinary object O takes arguments P (a property key), V (an ECMAScript language
    // value), and Receiver (an ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinarySet(O, P, V, Receiver).
    fn set(&self, agent: &mut Agent, key: PropertyKey, v: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        ordinary_set(agent, self, key, v, receiver)
    }

    // [[Delete]] ( P )
    //
    // The [[Delete]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryDelete(O, P).
    fn delete(&self, agent: &mut Agent, key: &PropertyKey) -> Completion<bool> {
        ordinary_delete(agent, self, key)
    }

    // [[OwnPropertyKeys]] ( )
    //
    // The [[OwnPropertyKeys]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    // 1. Return ! OrdinaryOwnPropertyKeys(O).
    fn own_property_keys(&self, agent: &mut Agent) -> Completion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(agent, self))
    }
}

impl SymbolObjectInterface for SymbolObject {
    fn symbol_data(&self) -> &RefCell<Option<Symbol>> {
        &self.symbol_data
    }
}

impl SymbolObject {
    pub fn object(agent: &mut Agent, prototype: Option<Object>) -> Object {
        Object { o: Rc::new(Self { common: RefCell::new(CommonObjectData::new(agent, prototype, true, SYMBOL_OBJECT_SLOTS)), symbol_data: RefCell::new(None) }) }
    }
}

pub fn create_symbol_object(agent: &mut Agent, sym: Symbol) -> Object {
    let symbol_proto = agent.intrinsic(IntrinsicId::SymbolPrototype);
    let obj = SymbolObject::object(agent, Some(symbol_proto));
    *obj.o.to_symbol_obj().unwrap().symbol_data().borrow_mut() = Some(sym);
    obj
}

#[allow(unused_variables)]
pub fn provision_symbol_intrinsic(agent: &mut Agent, realm: &Rc<RefCell<Realm>>) {
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
        agent,
        symbol_constructor_function,
        true,
        0.0,
        PropertyKey::from("Symbol"),
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
            define_property_or_throw(agent, &symbol_constructor, key, PotentialPropertyDescriptor::new().value(function_object).writable(true).enumerable(false).configurable(true)).unwrap();
        };
    }
    constructor_function!(symbol_for, "for", 1.0);
    constructor_function!(symbol_key_for, "keyFor", 1.0);

    // Constructor Data Properties
    macro_rules! constructor_data {
        ( $name:expr, $value:expr, $writable:expr, $enumerable:expr, $configurable:expr ) => {
            define_property_or_throw(
                agent,
                &symbol_constructor,
                $name,
                PotentialPropertyDescriptor::new().value(ECMAScriptValue::from($value)).writable($writable).enumerable($enumerable).configurable($configurable),
            )
            .unwrap();
        };
    }
    constructor_data!("asyncIterator", agent.wks(WksId::AsyncIterator), false, false, false);
    constructor_data!("hasInstance", agent.wks(WksId::HasInstance), false, false, false);
    constructor_data!("isConcatSpreadable", agent.wks(WksId::IsConcatSpreadable), false, false, false);
    constructor_data!("iterator", agent.wks(WksId::Iterator), false, false, false);
    constructor_data!("match", agent.wks(WksId::Match), false, false, false);
    constructor_data!("matchAll", agent.wks(WksId::MatchAll), false, false, false);
    //constructor_data!("prototype", symbol_prototype, false, false, false);
    constructor_data!("replace", agent.wks(WksId::Replace), false, false, false);
    constructor_data!("search", agent.wks(WksId::Search), false, false, false);
    constructor_data!("species", agent.wks(WksId::Species), false, false, false);
    constructor_data!("split", agent.wks(WksId::Split), false, false, false);
    constructor_data!("toPrimitive", agent.wks(WksId::ToPrimitive), false, false, false);
    constructor_data!("toStringTag", agent.wks(WksId::ToStringTag), false, false, false);
    constructor_data!("unscopables", agent.wks(WksId::Unscopables), false, false, false);

    realm.borrow_mut().intrinsics.symbol = symbol_constructor;
}

fn symbol_constructor_function(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
    todo!()
}
fn symbol_for(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
    todo!()
}
fn symbol_key_for(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
    todo!()
}
