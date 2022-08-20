use super::*;
use std::cell::RefCell;
use std::rc::Rc;

pub trait BooleanObjectInterface: ObjectInterface {
    fn boolean_data(&self) -> &RefCell<bool>;
}

#[derive(Debug)]
pub struct BooleanObject {
    common: RefCell<CommonObjectData>,
    boolean_data: RefCell<bool>,
}

impl<'a> From<&'a BooleanObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a BooleanObject) -> Self {
        obj
    }
}

impl ObjectInterface for BooleanObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn is_ordinary(&self) -> bool {
        true
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }
    fn to_boolean_obj(&self) -> Option<&dyn BooleanObjectInterface> {
        Some(self)
    }
    fn is_boolean_object(&self) -> bool {
        true
    }

    fn get_prototype_of(&self, _agent: &Agent) -> Completion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }

    // [[SetPrototypeOf]] ( V )
    //
    // The [[SetPrototypeOf]] internal method of an ordinary object O takes argument V (an Object or null). It performs
    // the following steps when called:
    //
    //  1. Return ! OrdinarySetPrototypeOf(O, V).
    fn set_prototype_of(&self, _agent: &Agent, obj: Option<Object>) -> Completion<bool> {
        Ok(ordinary_set_prototype_of(self, obj))
    }

    // [[IsExtensible]] ( )
    //
    // The [[IsExtensible]] internal method of an ordinary object O takes no arguments. It performs the following steps
    // when called:
    //
    //  1. Return ! OrdinaryIsExtensible(O).
    fn is_extensible(&self, _agent: &Agent) -> Completion<bool> {
        Ok(ordinary_is_extensible(self))
    }

    // [[PreventExtensions]] ( )
    //
    // The [[PreventExtensions]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    //  1. Return ! OrdinaryPreventExtensions(O).
    fn prevent_extensions(&self, _agent: &Agent) -> Completion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }

    // [[GetOwnProperty]] ( P )
    //
    // The [[GetOwnProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ! OrdinaryGetOwnProperty(O, P).
    fn get_own_property(&self, _agent: &Agent, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }

    // [[DefineOwnProperty]] ( P, Desc )
    //
    // The [[DefineOwnProperty]] internal method of an ordinary object O takes arguments P (a property key) and Desc (a
    // Property Descriptor). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryDefineOwnProperty(O, P, Desc).
    fn define_own_property(
        &self,
        agent: &Agent,
        key: PropertyKey,
        desc: PotentialPropertyDescriptor,
    ) -> Completion<bool> {
        ordinary_define_own_property(agent, self, key, desc)
    }

    // [[HasProperty]] ( P )
    //
    // The [[HasProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryHasProperty(O, P).
    fn has_property(&self, agent: &Agent, key: &PropertyKey) -> Completion<bool> {
        ordinary_has_property(agent, self, key)
    }

    // [[Get]] ( P, Receiver )
    //
    // The [[Get]] internal method of an ordinary object O takes arguments P (a property key) and Receiver (an
    // ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryGet(O, P, Receiver).
    fn get(&self, agent: &Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        ordinary_get(agent, self, key, receiver)
    }

    // [[Set]] ( P, V, Receiver )
    //
    // The [[Set]] internal method of an ordinary object O takes arguments P (a property key), V (an ECMAScript language
    // value), and Receiver (an ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinarySet(O, P, V, Receiver).
    fn set(&self, agent: &Agent, key: PropertyKey, v: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        ordinary_set(agent, self, key, v, receiver)
    }

    // [[Delete]] ( P )
    //
    // The [[Delete]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryDelete(O, P).
    fn delete(&self, agent: &Agent, key: &PropertyKey) -> Completion<bool> {
        ordinary_delete(agent, self, key)
    }

    // [[OwnPropertyKeys]] ( )
    //
    // The [[OwnPropertyKeys]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    // 1. Return ! OrdinaryOwnPropertyKeys(O).
    fn own_property_keys(&self, _agent: &Agent) -> Completion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(self))
    }
}

impl BooleanObjectInterface for BooleanObject {
    fn boolean_data(&self) -> &RefCell<bool> {
        &self.boolean_data
    }
}

impl BooleanObject {
    pub fn object(agent: &Agent, prototype: Option<Object>) -> Object {
        Object {
            o: Rc::new(Self {
                common: RefCell::new(CommonObjectData::new(agent, prototype, true, BOOLEAN_OBJECT_SLOTS)),
                boolean_data: RefCell::new(false),
            }),
        }
    }
}

// We have a need to "make a boolean object", for use in (for example) ToObject. The spec doesn't have a sequence of
// steps to do that, exactly, but it does have steps for the Boolean constructor. If we prune out the bits that don't
// matter, we're left with the sequence of steps:
//
//  3. Let O be ? OrdinaryCreateFromConstructor(%Boolean%, "%Boolean.prototype%", « [[BooleanData]] »).
//  4. Set O.[[BooleanData]] to b.
//  5. Return O.
pub fn create_boolean_object(agent: &Agent, b: bool) -> Object {
    let constructor = agent.intrinsic(IntrinsicId::Boolean);
    let o = ordinary_create_from_constructor(
        agent,
        &constructor,
        IntrinsicId::BooleanPrototype,
        &[InternalSlotName::BooleanData],
    )
    .unwrap();
    *o.o.to_boolean_obj().unwrap().boolean_data().borrow_mut() = b;
    o
}

// The abstract operation thisBooleanValue takes argument value. It performs the following steps when called:
//
//  1. If Type(value) is Boolean, return value.
//  2. If Type(value) is Object and value has a [[BooleanData]] internal slot, then
//      a. Let b be value.[[BooleanData]].
//      b. Assert: Type(b) is Boolean.
//      c. Return b.
//  3. Throw a TypeError exception.
pub fn this_boolean_value(agent: &Agent, value: &ECMAScriptValue) -> Completion<bool> {
    match value {
        ECMAScriptValue::Boolean(b) => Ok(*b),
        ECMAScriptValue::Object(o) => {
            let bool_obj = o.o.to_boolean_obj();
            if let Some(b_obj) = bool_obj {
                let b = *b_obj.boolean_data().borrow();
                Ok(b)
            } else {
                Err(create_type_error(agent, "Object has no boolean value"))
            }
        }
        _ => Err(create_type_error(agent, "Value is not boolean")),
    }
}

#[cfg(test)]
mod tests;
