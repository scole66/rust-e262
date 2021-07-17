use super::agent::Agent;
use super::comparison::is_integral_number;
use super::cr::{AltCompletion, Completion};
use super::object::{
    ordinary_create_from_constructor, ordinary_define_own_property, ordinary_delete, ordinary_get, ordinary_get_own_property, ordinary_get_prototype_of, ordinary_has_property,
    ordinary_is_extensible, ordinary_own_property_keys, ordinary_prevent_extensions, ordinary_set, ordinary_set_prototype_of, CommonObjectData, InternalSlotName, Object, ObjectInterface,
    PotentialPropertyDescriptor, PropertyDescriptor, NUMBER_OBJECT_SLOTS,
};
use super::realm::{IntrinsicId, Realm};
use super::values::{to_numeric, ECMAScriptValue, Numeric, PropertyKey};
use num::ToPrimitive;
use std::cell::RefCell;
use std::rc::Rc;

pub trait NumberObjectInterface: ObjectInterface {
    fn number_data(&self) -> &RefCell<f64>;
}

#[derive(Debug)]
pub struct NumberObject {
    common: RefCell<CommonObjectData>,
    number_data: RefCell<f64>,
}

impl<'a> From<&'a NumberObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a NumberObject) -> Self {
        obj
    }
}

impl ObjectInterface for NumberObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn is_ordinary(&self) -> bool {
        true
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }
    fn to_number_obj(&self) -> Option<&dyn NumberObjectInterface> {
        Some(self)
    }
    fn is_number_object(&self) -> bool {
        true
    }

    fn get_prototype_of(&self, _agent: &mut Agent) -> AltCompletion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }

    // [[SetPrototypeOf]] ( V )
    //
    // The [[SetPrototypeOf]] internal method of an ordinary object O takes argument V (an Object or null). It performs
    // the following steps when called:
    //
    //  1. Return ! OrdinarySetPrototypeOf(O, V).
    fn set_prototype_of(&self, _agent: &mut Agent, obj: Option<Object>) -> AltCompletion<bool> {
        Ok(ordinary_set_prototype_of(self, obj))
    }

    // [[IsExtensible]] ( )
    //
    // The [[IsExtensible]] internal method of an ordinary object O takes no arguments. It performs the following steps
    // when called:
    //
    //  1. Return ! OrdinaryIsExtensible(O).
    fn is_extensible(&self, _agent: &mut Agent) -> AltCompletion<bool> {
        Ok(ordinary_is_extensible(self))
    }

    // [[PreventExtensions]] ( )
    //
    // The [[PreventExtensions]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    //  1. Return ! OrdinaryPreventExtensions(O).
    fn prevent_extensions(&self, _agent: &mut Agent) -> AltCompletion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }

    // [[GetOwnProperty]] ( P )
    //
    // The [[GetOwnProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ! OrdinaryGetOwnProperty(O, P).
    fn get_own_property(&self, _agent: &mut Agent, key: &PropertyKey) -> AltCompletion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }

    // [[DefineOwnProperty]] ( P, Desc )
    //
    // The [[DefineOwnProperty]] internal method of an ordinary object O takes arguments P (a property key) and Desc (a
    // Property Descriptor). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryDefineOwnProperty(O, P, Desc).
    fn define_own_property(&self, agent: &mut Agent, key: PropertyKey, desc: PotentialPropertyDescriptor) -> AltCompletion<bool> {
        ordinary_define_own_property(agent, self, key, desc)
    }

    // [[HasProperty]] ( P )
    //
    // The [[HasProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryHasProperty(O, P).
    fn has_property(&self, agent: &mut Agent, key: &PropertyKey) -> AltCompletion<bool> {
        ordinary_has_property(agent, self, key)
    }

    // [[Get]] ( P, Receiver )
    //
    // The [[Get]] internal method of an ordinary object O takes arguments P (a property key) and Receiver (an
    // ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryGet(O, P, Receiver).
    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion {
        ordinary_get(agent, self, key, receiver)
    }

    // [[Set]] ( P, V, Receiver )
    //
    // The [[Set]] internal method of an ordinary object O takes arguments P (a property key), V (an ECMAScript language
    // value), and Receiver (an ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinarySet(O, P, V, Receiver).
    fn set(&self, agent: &mut Agent, key: PropertyKey, v: ECMAScriptValue, receiver: &ECMAScriptValue) -> AltCompletion<bool> {
        ordinary_set(agent, self, key, v, receiver)
    }

    // [[Delete]] ( P )
    //
    // The [[Delete]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryDelete(O, P).
    fn delete(&self, agent: &mut Agent, key: &PropertyKey) -> AltCompletion<bool> {
        ordinary_delete(agent, self, key)
    }

    // [[OwnPropertyKeys]] ( )
    //
    // The [[OwnPropertyKeys]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    // 1. Return ! OrdinaryOwnPropertyKeys(O).
    fn own_property_keys(&self, _agent: &mut Agent) -> AltCompletion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(self))
    }
}

impl NumberObjectInterface for NumberObject {
    fn number_data(&self) -> &RefCell<f64> {
        &self.number_data
    }
}

impl NumberObject {
    pub fn object(agent: &mut Agent, prototype: Option<Object>) -> Object {
        Object { o: Rc::new(Self { common: RefCell::new(CommonObjectData::new(agent, prototype, true, &NUMBER_OBJECT_SLOTS)), number_data: RefCell::new(0_f64) }) }
    }
}

use super::function_object::create_builtin_function;
use super::object::{define_property_or_throw, ordinary_object_create, BUILTIN_FUNCTION_SLOTS};

pub fn provision_number_intrinsic(agent: &mut Agent, realm: &Rc<RefCell<Realm>>) {
    let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let function_prototype = agent.intrinsic(IntrinsicId::FunctionPrototype);

    let number_prototype = ordinary_object_create(agent, Some(&object_prototype), &[InternalSlotName::NumberData]);
    realm.borrow_mut().intrinsics.number_prototype = number_prototype.clone();

    // The Number Constructor
    //
    // The Number constructor:
    //
    //      * is %Number%.
    //      * is the initial value of the "Number" property of the global object.
    //      * creates and initializes a new Number object when called as a constructor.
    //      * performs a type conversion when called as a function rather than as a constructor.
    //      * may be used as the value of an extends clause of a class definition. Subclass constructors that intend to
    //        inherit the specified Number behaviour must include a super call to the Number constructor to create and
    //        initialize the subclass instance with a [[NumberData]] internal slot.
    //
    // Properties of the Number Constructor
    //
    // The Number constructor:
    //
    //      * has a [[Prototype]] internal slot whose value is %Function.prototype%.
    let number_constructor =
        create_builtin_function(agent, number_constructor_function, 1_f64, PropertyKey::from("Number"), &BUILTIN_FUNCTION_SLOTS, Some(realm.clone()), Some(function_prototype.clone()), None);

    let constructor_function = |steps, name: &str, length| {
        let key = PropertyKey::from(name);
        let function_object = create_builtin_function(agent, steps, length, key.clone(), &BUILTIN_FUNCTION_SLOTS, Some(realm.clone()), Some(function_prototype.clone()), None);
        define_property_or_throw(
            agent,
            &number_constructor,
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

    // Constructor Functions
    constructor_function(number_is_finite, "isFinite", 1_f64);
    constructor_function(number_is_integer, "isInteger", 1_f64);
    constructor_function(number_is_nan, "isNaN", 1_f64);
    constructor_function(number_is_safe_integer, "isSafeInteger", 1_f64);

    let constructor_data = |value, name: &str| {
        let key = PropertyKey::from(name);
        define_property_or_throw(
            agent,
            &number_constructor,
            key,
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(value)), writable: Some(false), enumerable: Some(false), configurable: Some(false), ..Default::default() },
        )
        .unwrap();
    };

    // Number.EPSILON
    //
    // The value of Number.EPSILON is the Number value for the magnitude of the difference between 1 and the smallest
    // value greater than 1 that is representable as a Number value, which is approximately
    // 2.2204460492503130808472633361816 × 10**-16.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    constructor_data(f64::EPSILON, "EPSILON");

    // Number.MAX_SAFE_INTEGER
    //
    // NOTE     | The value of Number.MAX_SAFE_INTEGER is the largest integral Number n such that ℝ(n) and ℝ(n) + 1 are
    //            both exactly representable as a Number value.
    //
    // The value of Number.MAX_SAFE_INTEGER is 9007199254740991𝔽 (𝔽(2**53 - 1)).
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    constructor_data(9007199254740991.0, "MAX_SAFE_INTEGER");

    // Number.MAX_VALUE
    //
    // The value of Number.MAX_VALUE is the largest positive finite value of the Number type, which is approximately
    // 1.7976931348623157 × 10**308.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    constructor_data(f64::MAX, "MAX_VALUE");

    // Number.MIN_SAFE_INTEGER
    //
    // NOTE     | The value of Number.MIN_SAFE_INTEGER is the smallest integral Number n such that ℝ(n) and ℝ(n) - 1 are
    //            both exactly representable as a Number value.
    //
    // The value of Number.MIN_SAFE_INTEGER is -9007199254740991𝔽 (𝔽(-(2**53 - 1))).
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    constructor_data(-9007199254740991.0, "MIN_SAFE_INTEGER");

    // Number.MIN_VALUE
    //
    // The value of Number.MIN_VALUE is the smallest positive value of the Number type, which is approximately 5 ×
    // 10**(-324).
    //
    // In the IEEE 754-2019 double precision binary representation, the smallest possible value is a denormalized
    // number. If an implementation does not support denormalized values, the value of Number.MIN_VALUE must be the
    // smallest non-zero positive value that can actually be represented by the implementation.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    constructor_data(5e-324, "MIN_VALUE");

    // Number.NaN
    //
    // The value of Number.NaN is NaN.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    constructor_data(f64::NAN, "NaN");

    // Number.NEGATIVE_INFINITY
    //
    // The value of Number.NEGATIVE_INFINITY is -∞𝔽.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    constructor_data(f64::NEG_INFINITY, "NEGATIVE_INFINITY");

    // Number.POSITIVE_INFINITY
    //
    // The value of Number.POSITIVE_INFINITY is +∞𝔽.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    constructor_data(f64::INFINITY, "POSITIVE_INFINITY");

    // Number.prototype
    //
    // The initial value of Number.prototype is the Number prototype object.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    define_property_or_throw(
        agent,
        &number_constructor,
        PropertyKey::from("prototype"),
        PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::from(&number_prototype)),
            writable: Some(false),
            enumerable: Some(false),
            configurable: Some(false),
            ..Default::default()
        },
    )
    .unwrap();
    realm.borrow_mut().intrinsics.number = number_constructor.clone();
    define_property_or_throw(
        agent,
        &number_prototype,
        PropertyKey::from("constructor"),
        PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::from(number_constructor)), // consumes number_constructor
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();
}

// 4. Let O be ? OrdinaryCreateFromConstructor(NewTarget, "%Number.prototype%", « [[NumberData]] »).
// 5. Set O.[[NumberData]] to n.
// 6. Return O.
pub fn create_number_object(agent: &mut Agent, n: f64) -> Object {
    let constructor = agent.intrinsic(IntrinsicId::Number);
    let o = ordinary_create_from_constructor(agent, &constructor, IntrinsicId::NumberPrototype, &[InternalSlotName::NumberData]).unwrap();
    *o.o.to_number_obj().unwrap().number_data().borrow_mut() = n;
    o
}

// Number ( value )
//
// When Number is called with argument value, the following steps are taken:
//
//      1. If value is present, then
//          a. Let prim be ? ToNumeric(value).
//          b. If Type(prim) is BigInt, let n be 𝔽(ℝ(prim)).
//          c. Otherwise, let n be prim.
//      2. Else,
//          a. Let n be +0𝔽.
//      3. If NewTarget is undefined, return n.
//      4. Let O be ? OrdinaryCreateFromConstructor(NewTarget, "%Number.prototype%", « [[NumberData]] »).
//      5. Set O.[[NumberData]] to n.
//      6. Return O.
fn number_constructor_function(agent: &mut Agent, _this_value: ECMAScriptValue, new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion {
    let mut args = Arguments::from(arguments);
    let n = if args.count() >= 1 {
        let value = args.next_arg();
        let prim = to_numeric(agent, value)?;
        match prim {
            Numeric::BigInt(bi) => bi.to_f64().unwrap(),
            Numeric::Number(n) => n,
        }
    } else {
        0_f64
    };

    match new_target {
        None => Ok(ECMAScriptValue::from(n)),
        Some(nt) => {
            let o = ordinary_create_from_constructor(agent, nt, IntrinsicId::NumberPrototype, &NUMBER_OBJECT_SLOTS)?;
            *o.o.to_number_obj().unwrap().number_data().borrow_mut() = n;
            Ok(ECMAScriptValue::from(o))
        }
    }
}

// Number.isFinite ( number )
//
// When Number.isFinite is called with one argument number, the following steps are taken:
//
//      1. If Type(number) is not Number, return false.
//      2. If number is NaN, +∞𝔽, or -∞𝔽, return false.
//      3. Otherwise, return true.
fn number_is_finite(agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion {
    let mut args = Arguments::from(arguments);
    let number = args.next_arg();
    Ok(ECMAScriptValue::from(match number {
        ECMAScriptValue::Number(n) => n.is_finite(),
        _ => false,
    }))
}

// Number.isInteger ( number )
//
// When Number.isInteger is called with one argument number, the following steps are taken:
//
//      1. Return ! IsIntegralNumber(number).
fn number_is_integer(agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion {
    let mut args = Arguments::from(arguments);
    let number = args.next_arg();
    Ok(ECMAScriptValue::from(is_integral_number(number)))
}

// Number.isNaN ( number )
//
// When Number.isNaN is called with one argument number, the following steps are taken:
//
//      1. If Type(number) is not Number, return false.
//      2. If number is NaN, return true.
//      3. Otherwise, return false.
//
// NOTE     This function differs from the global isNaN function (19.2.3) in that it does not convert its argument to a
//          Number before determining whether it is NaN.
fn number_is_nan(agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion {
    let mut args = Arguments::from(arguments);
    let number = args.next_arg();
    Ok(ECMAScriptValue::from(match number {
        ECMAScriptValue::Number(n) => n.is_nan(),
        _ => false,
    }))
}

// Number.isSafeInteger ( number )
//
// When Number.isSafeInteger is called with one argument number, the following steps are taken:
//
//      1. If ! IsIntegralNumber(number) is true, then
//          a. If abs(ℝ(number)) ≤ 2**53 - 1, return true.
//      2. Return false.
fn number_is_safe_integer(agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion {
    let mut args = Arguments::from(arguments);
    let number = args.next_arg();
    Ok(ECMAScriptValue::from(match number.clone() {
        ECMAScriptValue::Number(n) => is_integral_number(&number) && n.abs() <= 9007199254740991.0,
        _ => false,
    }))
}
