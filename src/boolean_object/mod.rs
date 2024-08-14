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
    fn uses_ordinary_get_prototype_of(&self) -> bool {
        true
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }
    fn to_boolean_obj(&self) -> Option<&dyn BooleanObjectInterface> {
        Some(self)
    }
    fn kind(&self) -> ObjectTag {
        ObjectTag::Boolean
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

impl BooleanObjectInterface for BooleanObject {
    fn boolean_data(&self) -> &RefCell<bool> {
        &self.boolean_data
    }
}

impl BooleanObject {
    pub fn new(prototype: Option<Object>) -> Self {
        Self {
            common: RefCell::new(CommonObjectData::new(prototype, true, BOOLEAN_OBJECT_SLOTS)),
            boolean_data: RefCell::new(false),
        }
    }
    pub fn object(prototype: Option<Object>) -> Object {
        Object { o: Rc::new(Self::new(prototype)) }
    }
}

// We have a need to "make a boolean object", for use in (for example) ToObject. The spec doesn't have a sequence of
// steps to do that, exactly, but it does have steps for the Boolean constructor. If we prune out the bits that don't
// matter, we're left with the sequence of steps:
//
//  3. Let O be ? OrdinaryCreateFromConstructor(%Boolean%, "%Boolean.prototype%", « [[BooleanData]] »).
//  4. Set O.[[BooleanData]] to b.
//  5. Return O.
impl From<bool> for Object {
    fn from(b: bool) -> Self {
        let constructor = intrinsic(IntrinsicId::Boolean);
        let o = constructor
            .ordinary_create_from_constructor(IntrinsicId::BooleanPrototype, &[InternalSlotName::BooleanData])
            .unwrap();
        *o.o.to_boolean_obj().unwrap().boolean_data().borrow_mut() = b;
        o
    }
}

// The abstract operation thisBooleanValue takes argument value. It performs the following steps when called:
//
//  1. If Type(value) is Boolean, return value.
//  2. If Type(value) is Object and value has a [[BooleanData]] internal slot, then
//      a. Let b be value.[[BooleanData]].
//      b. Assert: Type(b) is Boolean.
//      c. Return b.
//  3. Throw a TypeError exception.
pub fn this_boolean_value(value: &ECMAScriptValue) -> Completion<bool> {
    match value {
        ECMAScriptValue::Boolean(b) => Ok(*b),
        ECMAScriptValue::Object(o) => {
            let bool_obj = o.o.to_boolean_obj();
            if let Some(b_obj) = bool_obj {
                let b = *b_obj.boolean_data().borrow();
                Ok(b)
            } else {
                Err(create_type_error("Object has no boolean value"))
            }
        }
        _ => Err(create_type_error("Value is not boolean")),
    }
}

pub fn provision_boolean_intrinsic(realm: &Rc<RefCell<Realm>>) {
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();

    let boolean_prototype = BooleanObject::object(Some(object_prototype));
    realm.borrow_mut().intrinsics.boolean_prototype = boolean_prototype.clone();

    // The Boolean constructor:
    //
    //  * is %Boolean%.
    //  * is the initial value of the "Boolean" property of the global object.
    //  * creates and initializes a new Boolean object when called as a constructor.
    //  * performs a type conversion when called as a function rather than as a constructor.
    //  * may be used as the value of an extends clause of a class definition. Subclass constructors that
    //    intend to inherit the specified Boolean behaviour must include a super call to the Boolean
    //    constructor to create and initialize the subclass instance with a [[BooleanData]] internal slot.
    //
    // Properties of the Boolean Constructor
    //
    // The Boolean constructor:
    //
    //  * has a [[Prototype]] internal slot whose value is %Function.prototype%.
    let bool_constructor = create_builtin_function(
        boolean_constructor_function,
        true,
        1_f64,
        PropertyKey::from("Boolean"),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_prototype.clone()),
        None,
    );
    realm.borrow_mut().intrinsics.boolean = bool_constructor.clone();

    // Boolean.prototype
    // The initial value of Boolean.prototype is the Boolean prototype object.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    let bool_proto_ppd = PotentialPropertyDescriptor::new()
        .value(&boolean_prototype)
        .writable(false)
        .enumerable(false)
        .configurable(false);
    define_property_or_throw(&bool_constructor, "prototype", bool_proto_ppd).unwrap();

    // The Boolean prototype object:
    //
    //  * is %Boolean.prototype%.
    //  * is an ordinary object.
    //  * is itself a Boolean object; it has a [[BooleanData]] internal slot with the value false.
    //  * has a [[Prototype]] internal slot whose value is %Object.prototype%.

    // Boolean.prototype.constructor
    // The initial value of Boolean.prototype.constructor is %Boolean%.
    define_property_or_throw(
        &boolean_prototype,
        "constructor",
        PotentialPropertyDescriptor::new()
            .value(bool_constructor.clone())
            .writable(true)
            .enumerable(false)
            .configurable(true),
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
                &boolean_prototype,
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
    prototype_function!(boolean_prototype_to_string, "toString", 0.0); // ( )
    prototype_function!(boolean_prototype_value_of, "valueOf", 0.0); // ( )
}

fn boolean_constructor_function(
    _this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Boolean ( value )
    // This function performs the following steps when called:
    //
    //  1. Let b be ToBoolean(value).
    //  2. If NewTarget is undefined, return b.
    //  3. Let O be ? OrdinaryCreateFromConstructor(NewTarget, "%Boolean.prototype%", « [[BooleanData]] »).
    //  4. Set O.[[BooleanData]] to b.
    //  5. Return O.
    let mut args = FuncArgs::from(arguments);
    let value = args.next_arg();
    let b = to_boolean(value);
    match new_target {
        None => Ok(b.into()),
        Some(obj) => {
            let o =
                obj.ordinary_create_from_constructor(IntrinsicId::BooleanPrototype, &[InternalSlotName::BooleanData])?;
            let bool_obj = o.o.to_boolean_obj().expect("we just crafted a boolean object");
            *bool_obj.boolean_data().borrow_mut() = b;
            Ok(o.into())
        }
    }
}

fn boolean_prototype_to_string(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Boolean.prototype.toString ( )
    // This method performs the following steps when called:
    //
    //  1. Let b be ? ThisBooleanValue(this value).
    //  2. If b is true, return "true"; else return "false".
    Ok(if this_boolean_value(this_value)? { "true" } else { "false" }.into())
}

fn boolean_prototype_value_of(
    this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Boolean.prototype.valueOf ( )
    // This method performs the following steps when called:
    //
    //  1. Return ? ThisBooleanValue(this value).
    Ok(this_boolean_value(this_value)?.into())
}

#[cfg(test)]
mod tests;
