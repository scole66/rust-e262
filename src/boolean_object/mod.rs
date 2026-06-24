use super::*;
use std::cell::RefCell;
use std::rc::Rc;

pub(crate) trait BooleanObjectInterface: ObjectInterface {
    fn boolean_data(&self) -> &bool;
}

#[derive(Debug)]
pub(crate) struct BooleanObject {
    common: RefCell<CommonObjectData>,
    boolean_data: bool,
}

impl<'a> From<&'a BooleanObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a BooleanObject) -> Self {
        obj
    }
}

impl ObjectInterface for BooleanObject {
    fn as_object_interface(&self) -> &dyn ObjectInterface {
        self
    }
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
}

impl BooleanObjectInterface for BooleanObject {
    fn boolean_data(&self) -> &bool {
        &self.boolean_data
    }
}

impl BooleanObject {
    pub(crate) fn new(prototype: Option<Object>, value: bool) -> Self {
        Self { common: RefCell::new(CommonObjectData::new(prototype, true, BOOLEAN_OBJECT_SLOTS)), boolean_data: value }
    }
    pub(crate) fn object(prototype: Option<Object>, value: bool) -> Object {
        Object { o: Rc::new(Self::new(prototype, value)) }
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
        constructor
            .ordinary_create_from_constructor(IntrinsicId::BooleanPrototype, |proto| BooleanObject::object(proto, b))
            .unwrap()
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
pub(crate) fn this_boolean_value(value: &ECMAScriptValue) -> Completion<bool> {
    match value {
        ECMAScriptValue::Boolean(b) => Ok(*b),
        ECMAScriptValue::Object(o) => {
            let bool_obj = o.o.to_boolean_obj();
            if let Some(b_obj) = bool_obj {
                let b = *b_obj.boolean_data();
                Ok(b)
            } else {
                Err(create_type_error("Object has no boolean value"))
            }
        }
        _ => Err(create_type_error("Value is not boolean")),
    }
}

pub(crate) fn provision_boolean_intrinsic(realm: &Rc<RefCell<Realm>>) {
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();

    let boolean_prototype = BooleanObject::object(Some(object_prototype), false);
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
        Box::new(boolean_constructor_function),
        Some(ConstructorKind::Base),
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
            let o = obj.ordinary_create_from_constructor(IntrinsicId::BooleanPrototype, |proto| {
                BooleanObject::object(proto, b)
            })?;
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
