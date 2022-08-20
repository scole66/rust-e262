use super::*;
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

impl NumberObjectInterface for NumberObject {
    fn number_data(&self) -> &RefCell<f64> {
        &self.number_data
    }
}

impl NumberObject {
    pub fn object(agent: &Agent, prototype: Option<Object>) -> Object {
        Object {
            o: Rc::new(Self {
                common: RefCell::new(CommonObjectData::new(agent, prototype, true, NUMBER_OBJECT_SLOTS)),
                number_data: RefCell::new(0_f64),
            }),
        }
    }
}

pub fn provision_number_intrinsic(agent: &Agent, realm: &Rc<RefCell<Realm>>) {
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();

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
    let number_constructor = create_builtin_function(
        agent,
        number_constructor_function,
        true,
        1_f64,
        PropertyKey::from("Number"),
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
                agent,
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
                agent,
                &number_constructor,
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
    constructor_function!(number_is_finite, "isFinite", 1_f64);
    constructor_function!(number_is_integer, "isInteger", 1_f64);
    constructor_function!(number_is_nan, "isNaN", 1_f64);
    constructor_function!(number_is_safe_integer, "isSafeInteger", 1_f64);

    // Constructor Data Properties
    macro_rules! constructor_data {
        ( $value:expr, $name:expr ) => {{
            define_property_or_throw(
                agent,
                &number_constructor,
                $name,
                PotentialPropertyDescriptor::new().value($value).writable(false).enumerable(false).configurable(false),
            )
            .unwrap();
        }};
    }
    // Number.EPSILON
    //
    // The value of Number.EPSILON is the Number value for the magnitude of the difference between 1 and the smallest
    // value greater than 1 that is representable as a Number value, which is approximately
    // 2.2204460492503130808472633361816 × 10**-16.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    constructor_data!(f64::EPSILON, "EPSILON");

    // Number.MAX_SAFE_INTEGER
    //
    // NOTE     | The value of Number.MAX_SAFE_INTEGER is the largest integral Number n such that ℝ(n) and ℝ(n) + 1 are
    //            both exactly representable as a Number value.
    //
    // The value of Number.MAX_SAFE_INTEGER is 9007199254740991𝔽 (𝔽(2**53 - 1)).
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    constructor_data!(9007199254740991.0, "MAX_SAFE_INTEGER");

    // Number.MAX_VALUE
    //
    // The value of Number.MAX_VALUE is the largest positive finite value of the Number type, which is approximately
    // 1.7976931348623157 × 10**308.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    constructor_data!(f64::MAX, "MAX_VALUE");

    // Number.MIN_SAFE_INTEGER
    //
    // NOTE     | The value of Number.MIN_SAFE_INTEGER is the smallest integral Number n such that ℝ(n) and ℝ(n) - 1 are
    //            both exactly representable as a Number value.
    //
    // The value of Number.MIN_SAFE_INTEGER is -9007199254740991𝔽 (𝔽(-(2**53 - 1))).
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    constructor_data!(-9007199254740991.0, "MIN_SAFE_INTEGER");

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
    constructor_data!(5e-324, "MIN_VALUE");

    // Number.NaN
    //
    // The value of Number.NaN is NaN.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    constructor_data!(f64::NAN, "NaN");

    // Number.NEGATIVE_INFINITY
    //
    // The value of Number.NEGATIVE_INFINITY is -∞𝔽.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    constructor_data!(f64::NEG_INFINITY, "NEGATIVE_INFINITY");

    // Number.POSITIVE_INFINITY
    //
    // The value of Number.POSITIVE_INFINITY is +∞𝔽.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    constructor_data!(f64::INFINITY, "POSITIVE_INFINITY");

    // Number.prototype
    //
    // The initial value of Number.prototype is the Number prototype object.
    //
    // This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.
    let number_prototype = ordinary_object_create(agent, Some(object_prototype), &[InternalSlotName::NumberData]);
    constructor_data!(&number_prototype, "prototype");

    // Properties of the Number Prototype Object
    //
    // The Number prototype object:
    //
    //   * is %Number.prototype%.
    //   * is an ordinary object.
    //   * is itself a Number object; it has a [[NumberData]] internal slot with the value +0𝔽.
    //   * has a [[Prototype]] internal slot whose value is %Object.prototype%.
    //   * Unless explicitly stated otherwise, the methods of the Number prototype object defined below are not generic
    //     and the this value passed to them must be either a Number value or an object that has a [[NumberData]]
    //     internal slot that has been initialized to a Number value.

    // Prototype Data Properties
    macro_rules! prototype_data {
        ( $value:expr, $name:expr ) => {{
            define_property_or_throw(
                agent,
                &number_prototype,
                $name,
                PotentialPropertyDescriptor::new().value($value).writable(true).enumerable(false).configurable(true),
            )
            .unwrap();
        }};
    }

    // Number.prototype.constructor
    //
    // The initial value of Number.prototype.constructor is %Number%.
    prototype_data!(&number_constructor, "constructor");

    // Prototype Function Properties
    macro_rules! prototype_function {
        ( $steps:expr, $name:expr, $length:expr ) => {
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(
                agent,
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
                agent,
                &number_prototype,
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
    prototype_function!(number_prototype_to_exponential, "toExponential", 1.0);
    prototype_function!(number_prototype_to_fixed, "toFixed", 1.0);
    prototype_function!(number_prototype_to_locale_string, "toLocaleString", 0.0);
    prototype_function!(number_prototype_to_precision, "toPrecision", 1.0);
    prototype_function!(number_prototype_to_string, "toString", 1.0);
    prototype_function!(number_prototype_value_of, "valueOf", 0.0);

    realm.borrow_mut().intrinsics.number = number_constructor;
    realm.borrow_mut().intrinsics.number_prototype = number_prototype;
}

// 4. Let O be ? OrdinaryCreateFromConstructor(NewTarget, "%Number.prototype%", « [[NumberData]] »).
// 5. Set O.[[NumberData]] to n.
// 6. Return O.
pub fn create_number_object(agent: &Agent, n: f64) -> Object {
    let constructor = agent.intrinsic(IntrinsicId::Number);
    let o = ordinary_create_from_constructor(
        agent,
        &constructor,
        IntrinsicId::NumberPrototype,
        &[InternalSlotName::NumberData],
    )
    .unwrap();
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
fn number_constructor_function(
    agent: &Agent,
    _this_value: ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
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
            let o = ordinary_create_from_constructor(
                agent,
                nt,
                IntrinsicId::NumberPrototype,
                &[InternalSlotName::NumberData],
            )?;
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
fn number_is_finite(
    _agent: &Agent,
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
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
fn number_is_integer(
    _agent: &Agent,
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let number = args.next_arg();
    Ok(ECMAScriptValue::from(is_integral_number(&number)))
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
fn number_is_nan(
    _agent: &Agent,
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
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
fn number_is_safe_integer(
    _agent: &Agent,
    _this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let number = args.next_arg();
    Ok(ECMAScriptValue::from(match number {
        ECMAScriptValue::Number(n) => is_integral_number(&number) && n.abs() <= 9007199254740991.0,
        _ => false,
    }))
}

// The abstract operation thisNumberValue takes argument value. It performs the following steps when called:
//
//  1. If Type(value) is Number, return value.
//  2. If Type(value) is Object and value has a [[NumberData]] internal slot, then
//      a. Let n be value.[[NumberData]].
//      b. Assert: Type(n) is Number.
//      c. Return n.
//  3. Throw a TypeError exception.
//
// The phrase “this Number value” within the specification of a method refers to the result returned by calling the
// abstract operation thisNumberValue with the this value of the method invocation passed as the argument.
fn this_number_value(agent: &Agent, value: ECMAScriptValue) -> Completion<f64> {
    match value {
        ECMAScriptValue::Number(x) => Ok(x),
        ECMAScriptValue::Object(o) if o.o.is_number_object() => {
            let no = o.o.to_number_obj().unwrap();
            let n = *no.number_data().borrow();
            Ok(n)
        }
        _ => Err(create_type_error(agent, "Number method called with non-number receiver")),
    }
}

// Number.prototype.toExponential ( fractionDigits )
//
// Return a String containing this Number value represented in decimal exponential notation with one digit before the
// significand's decimal point and fractionDigits digits after the significand's decimal point. If fractionDigits is
// undefined, include as many significand digits as necessary to uniquely specify the Number (just like in ToString
// except that in this case the Number is always output in exponential notation). Specifically, perform the following
// steps:
//
//  1. Let x be ? thisNumberValue(this value).
//  2. Let f be ? ToIntegerOrInfinity(fractionDigits).
//  3. Assert: If fractionDigits is undefined, then f is 0.
//  4. If x is not finite, return ! Number::toString(x).
//  5. If f < 0 or f > 100, throw a RangeError exception.
//  6. Set x to ℝ(x).
//  7. Let s be the empty String.
//  8. If x < 0, then
//      a. Set s to "-".
//      b. Set x to -x.
//  9. If x = 0, then
//      a. Let m be the String value consisting of f + 1 occurrences of the code unit 0x0030 (DIGIT ZERO).
//      b. Let e be 0.
//  10. Else,
//      a. If fractionDigits is not undefined, then
//          i. Let e and n be integers such that 10**f ≤ n < 10**(f + 1) and for which n × 10**(e - f) - x is as close
//             to zero as possible. If there are two such sets of e and n, pick the e and n for which n × 10**(e - f)
//             is larger.
//      b. Else,
//          i. Let e, n, and f be integers such that f ≥ 0, 10**f ≤ n < 10**(f + 1), 𝔽(n × 10**(e - f)) is 𝔽(x), and
//             f is as small as possible. Note that the decimal representation of n has f + 1 digits, n is not
//             divisible by 10, and the least significant digit of n is not necessarily uniquely determined by these
//             criteria.
//      c. Let m be the String value consisting of the digits of the decimal representation of n (in order, with no
//         leading zeroes).
//  11. If f ≠ 0, then
//      a. Let a be the first code unit of m.
//      b. Let b be the other f code units of m.
//      c. Set m to the string-concatenation of a, ".", and b.
//  12. If e = 0, then
//      a. Let c be "+".
//      b. Let d be "0".
//  13. Else,
//      a. If e > 0, let c be "+".
//      b. Else,
//          i. Assert: e < 0.
//          ii. Let c be "-".
//          iii. Set e to -e.
//      c. Let d be the String value consisting of the digits of the decimal representation of e (in order, with no
//         leading zeroes).
//  14. Set m to the string-concatenation of m, "e", c, and d.
//  15. Return the string-concatenation of s and m.
//
// NOTE     For implementations that provide more accurate conversions than required by the rules above, it is
//          recommended that the following alternative version of step 10.b.i be used as a guideline:
//
//          1. Let e, n, and f be integers such that f ≥ 0, 10f ≤ n < 10f + 1, 𝔽(n × 10e - f) is 𝔽(x), and f is as
//             small as possible. If there are multiple possibilities for n, choose the value of n for which 𝔽(n × 10e
//             - f) is closest in value to 𝔽(x). If there are two such possible values of n, choose the one that is
//             even.
fn number_prototype_to_exponential(
    agent: &Agent,
    this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let fraction_digits = args.next_arg();

    let value = this_number_value(agent, this_value)?;
    let fraction = to_integer_or_infinity(agent, fraction_digits.clone())?;
    if !value.is_finite() {
        return Ok(ECMAScriptValue::from(to_string(agent, ECMAScriptValue::from(value)).unwrap()));
    }
    if !(0.0..=100.0).contains(&fraction) {
        let fd_str = to_string(agent, fraction_digits).unwrap();
        return Err(create_range_error(agent, format!("FractionDigits ‘{}’ must lie within the range 0..100", fd_str)));
    }
    let fraction = fraction as i32;

    let info;
    let mut workbuf: [u8; 101] = [0; 101];
    let digits;
    if !fraction_digits.is_undefined() {
        info = dtoa_precise(value, fraction + 1);
        // We need fraction +1 digits to come out of this.
        let strbuf = info.chars.as_bytes();
        // copy digits out of that, right-padding with '0', until we get p chars.
        let mut out_of_chars = false;
        for idx in 0..(fraction + 1) as usize {
            workbuf[idx] = if out_of_chars {
                b'0'
            } else {
                let ch = strbuf[idx];
                if ch == 0 {
                    out_of_chars = true;
                    b'0'
                } else {
                    ch
                }
            };
        }
        digits = &workbuf[0..(fraction + 1) as usize];
    } else {
        info = dtoa(value);
        let strbuf = info.chars.as_bytes();
        // Find the first null
        let mut null_idx: Option<usize> = None;
        for (idx, ch) in strbuf.iter().enumerate() {
            if *ch == 0 {
                null_idx = Some(idx);
                break;
            }
        }
        digits = &strbuf[0..null_idx.unwrap()];
    }
    let exp = info.decpt - 1;
    let sign = if value < 0.0 { "-" } else { "" };

    Ok(ECMAScriptValue::from(if fraction == 0 {
        format!("{}{}e{:+}", sign, String::from_utf8_lossy(digits), exp)
    } else {
        let first_digit = &digits[0..1];
        let remaining = &digits[1..];
        format!("{}{}.{}e{:+}", sign, String::from_utf8_lossy(first_digit), String::from_utf8_lossy(remaining), exp)
    }))
}

// Number.prototype.toFixed ( fractionDigits )
//
// NOTE 1       toFixed returns a String containing this Number value represented in decimal fixed-point notation with
//              fractionDigits digits after the decimal point. If fractionDigits is undefined, 0 is assumed.
//
// The following steps are performed:
//
//  1. Let x be ? thisNumberValue(this value).
//  2. Let f be ? ToIntegerOrInfinity(fractionDigits).
//  3. Assert: If fractionDigits is undefined, then f is 0.
//  4. If f is not finite, throw a RangeError exception.
//  5. If f < 0 or f > 100, throw a RangeError exception.
//  6. If x is not finite, return ! Number::toString(x).
//  7. Set x to ℝ(x).
//  8. Let s be the empty String.
//  9. If x < 0, then
//      a. Set s to "-".
//      b. Set x to -x.
//  10. If x ≥ 10**21, then
//      a. Let m be ! ToString(𝔽(x)).
//  11. Else,
//      a. Let n be an integer for which n / 10**f - x is as close to zero as possible. If there are two such n, pick the
//         larger n.
//      b. If n = 0, let m be the String "0". Otherwise, let m be the String value consisting of the digits of the
//         decimal representation of n (in order, with no leading zeroes).
//      c. If f ≠ 0, then
//          i. Let k be the length of m.
//          ii. If k ≤ f, then
//              1. Let z be the String value consisting of f + 1 - k occurrences of the code unit 0x0030 (DIGIT ZERO).
//              2. Set m to the string-concatenation of z and m.
//              3. Set k to f + 1.
//          iii. Let a be the first k - f code units of m.
//          iv. Let b be the other f code units of m.
//          v. Set m to the string-concatenation of a, ".", and b.
//  12. Return the string-concatenation of s and m.
//
// NOTE 2       The output of toFixed may be more precise than toString for some values because toString only prints
//              enough significant digits to distinguish the number from adjacent Number values. For example,
//
//                  (1000000000000000128).toString() returns "1000000000000000100", while
//                  (1000000000000000128).toFixed(0) returns "1000000000000000128".
//
fn number_prototype_to_fixed(
    agent: &Agent,
    this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let fraction_digits = args.next_arg();
    let value = this_number_value(agent, this_value)?;
    let fraction = to_integer_or_infinity(agent, fraction_digits)?;
    if !fraction.is_finite() || !(0.0..=100.0).contains(&fraction) {
        return Err(create_range_error(agent, "Argument for Number.toFixed must be in the range 0..100"));
    }
    if !value.is_finite() || value.abs() >= 1.0e21 {
        return to_string(agent, ECMAScriptValue::from(value)).map(ECMAScriptValue::from);
    }
    let magnitude = value.abs();

    Ok(ECMAScriptValue::from({
        let sign = if value < 0.0 { "-" } else { "" };
        let f = fraction as i32;
        let mut workbuf: [u8; 101] = [b'0'; 101];
        let info = dtoa_fixed(magnitude, f);
        let mut k = info.decpt + f;
        if f == 0 {
            // sign + the k digits from dtoa.
            let strbuf = info.chars.as_bytes();
            for idx in 0..k as usize {
                workbuf[idx] = {
                    let ch = strbuf[idx];
                    if ch == 0 {
                        break;
                    } else {
                        ch
                    }
                };
            }
            format!("{}{}", sign, String::from_utf8_lossy(&workbuf[0..k.max(1) as usize]))
        } else {
            let mut write_offset = 0;
            if k <= f {
                write_offset = f + 1 - k;
                k = f + 1;
            }
            let strbuf = info.chars.as_bytes();
            // copy digits out of the dtoabuffer, until we get info.decpt chars or run out.
            for read_idx in 0..(info.decpt + f - write_offset) as usize {
                workbuf[read_idx + write_offset as usize] = {
                    let ch = strbuf[read_idx];
                    if ch == 0 {
                        break;
                    } else {
                        ch
                    }
                };
            }
            let before_point = String::from_utf8_lossy(&workbuf[0..(k - f) as usize]);
            let after_point = String::from_utf8_lossy(&workbuf[(k - f) as usize..k as usize]);

            format!("{}{}.{}", sign, before_point, after_point)
        }
    }))
}

// Number.prototype.toLocaleString ( [ reserved1 [ , reserved2 ] ] )
//
// An ECMAScript implementation that includes the ECMA-402 Internationalization API must implement the
// Number.prototype.toLocaleString method as specified in the ECMA-402 specification. If an ECMAScript implementation
// does not include the ECMA-402 API the following specification of the toLocaleString method is used.
//
// Produces a String value that represents this Number value formatted according to the conventions of the host
// environment's current locale. This function is implementation-defined, and it is permissible, but not encouraged,
// for it to return the same thing as toString.
//
// The meanings of the optional parameters to this method are defined in the ECMA-402 specification; implementations
// that do not include ECMA-402 support must not use those parameter positions for anything else.
fn number_prototype_to_locale_string(
    agent: &Agent,
    this_value: ECMAScriptValue,
    new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    number_prototype_to_string(agent, this_value, new_target, &[]) // Don't send the args along, because reserved1 & 2 are not meaningful
}

// Number.prototype.toPrecision ( precision )
//
// Return a String containing this Number value represented either in decimal exponential notation with one digit
// before the significand's decimal point and precision - 1 digits after the significand's decimal point or in decimal
// fixed notation with precision significant digits. If precision is undefined, call ToString instead. Specifically,
// perform the following steps:
//
//  1. Let x be ? thisNumberValue(this value).
//  2. If precision is undefined, return ! ToString(x).
//  3. Let p be ? ToIntegerOrInfinity(precision).
//  4. If x is not finite, return ! Number::toString(x).
//  5. If p < 1 or p > 100, throw a RangeError exception.
//  6. Set x to ℝ(x).
//  7. Let s be the empty String.
//  8. If x < 0, then
//      a. Set s to the code unit 0x002D (HYPHEN-MINUS).
//      b. Set x to -x.
//  9. If x = 0, then
//      a. Let m be the String value consisting of p occurrences of the code unit 0x0030 (DIGIT ZERO).
//      b. Let e be 0.
//  10. Else,
//      a. Let e and n be integers such that 10p - 1 ≤ n < 10p and for which n × 10e - p + 1 - x is as close to zero as
//         possible. If there are two such sets of e and n, pick the e and n for which n × 10e - p + 1 is larger.
//      b. Let m be the String value consisting of the digits of the decimal representation of n (in order, with no
//         leading zeroes).
//      c. If e < -6 or e ≥ p, then
//          i. Assert: e ≠ 0.
//          ii. If p ≠ 1, then
//              1. Let a be the first code unit of m.
//              2. Let b be the other p - 1 code units of m.
//              3. Set m to the string-concatenation of a, ".", and b.
//          iii. If e > 0, then
//              1. Let c be the code unit 0x002B (PLUS SIGN).
//          iv. Else,
//              1. Assert: e < 0.
//              2. Let c be the code unit 0x002D (HYPHEN-MINUS).
//              3. Set e to -e.
//          v. Let d be the String value consisting of the digits of the decimal representation of e (in order, with no
//             leading zeroes).
//          vi. Return the string-concatenation of s, m, the code unit 0x0065 (LATIN SMALL LETTER E), c, and d.
//  11. If e = p - 1, return the string-concatenation of s and m.
//  12. If e ≥ 0, then
//      a. Set m to the string-concatenation of the first e + 1 code units of m, the code unit 0x002E (FULL STOP), and
//         the remaining p - (e + 1) code units of m.
//  13. Else,
//      a. Set m to the string-concatenation of the code unit 0x0030 (DIGIT ZERO), the code unit 0x002E (FULL STOP),
//         -(e + 1) occurrences of the code unit 0x0030 (DIGIT ZERO), and the String m.
//  14. Return the string-concatenation of s and m.
fn number_prototype_to_precision(
    agent: &Agent,
    this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let precision = args.next_arg();
    let value = this_number_value(agent, this_value)?;
    if precision.is_undefined() {
        return Ok(ECMAScriptValue::from(to_string(agent, ECMAScriptValue::from(value)).unwrap()));
    }
    let prec = to_integer_or_infinity(agent, precision.clone())?;
    if !value.is_finite() {
        return Ok(ECMAScriptValue::from(to_string(agent, ECMAScriptValue::from(value)).unwrap()));
    }
    if !(1.0..=100.0).contains(&prec) {
        let precision_str = to_string(agent, precision).unwrap();
        return Err(create_range_error(
            agent,
            format!("Precision ‘{}’ must lie within the range 1..100", precision_str),
        ));
    }

    let p_size = prec as usize;
    let p_val = prec as i32;
    let info = dtoa_precise(value, p_val);
    let strbuf = info.chars.as_bytes();
    // copy digits out of that, right-padding with '0', until we get p chars.
    let mut workbuf: [u8; 101] = [0; 101];
    let mut out_of_chars = false;
    for idx in 0..p_size {
        workbuf[idx] = if out_of_chars {
            b'0'
        } else {
            let ch = strbuf[idx];
            if ch == 0 {
                out_of_chars = true;
                b'0'
            } else {
                ch
            }
        };
    }
    let digits = &workbuf[0..p_size];
    let e = info.decpt - 1;
    let sign = if info.sign != 0 && value != 0.0 { "-" } else { "" };

    Ok(ECMAScriptValue::from(
        // exponential form
        if e < -6 || e >= p_val {
            let before_pt = String::from_utf8_lossy(&digits[0..1]);
            let decpt = if p_val != 1 { "." } else { "" };
            let after_pt = String::from_utf8_lossy(&digits[1..p_size]);
            format!("{}{}{}{}e{}", sign, before_pt, decpt, after_pt, e)
        } else if e == p_val - 1 {
            // No decimal point
            format!("{}{}", sign, String::from_utf8_lossy(digits))
        } else if e >= 0 {
            // No leading zeroes
            let e_us: usize = e as usize + 1;
            let a = &digits[0..e_us];
            let b = &digits[e_us..p_size];
            format!("{}{}.{}", sign, String::from_utf8_lossy(a), String::from_utf8_lossy(b))
        } else {
            // Leading zeroes
            let digit_str = String::from_utf8_lossy(digits);
            let num_zeroes: usize = (-e - 1) as usize;
            format!("{}0.{:0>width$}", sign, digit_str, width = num_zeroes + digit_str.len())
        },
    ))
}

pub fn next_double(dbl: f64) -> f64 {
    // Copied from the V8 source
    // Returns the next greater double. Returns +infinity on input +infinity.
    // double NextDouble() const {
    //   if (d64_ == kInfinity) return Double(kInfinity).value();
    //   if (Sign() < 0 && Significand() == 0) {
    //     // -0.0
    //     return 0.0;
    //   }
    //   if (Sign() < 0) {
    //     return Double(d64_ - 1).value();
    //   } else {
    //     return Double(d64_ + 1).value();
    //   }
    // }
    if dbl == f64::INFINITY {
        f64::INFINITY
    } else if dbl.signum() < 0.0 {
        if dbl == 0.0 {
            // -0.0
            0.0
        } else {
            f64::from_bits(dbl.to_bits() - 1)
        }
    } else {
        f64::from_bits(dbl.to_bits() + 1)
    }
}

fn double_exponent(dbl: f64) -> i32 {
    // Note: This exponent is not exactly the exponent from IEE-754. This is really more about "are my least significant
    // values in the unit digit?" I.e.: Since (LARGE % small) == nonsense; we need to do other tricks. This function
    // helps get us there.
    //
    // The exponent returned is what the exponent would be if the double were
    //      1<significand_bits>.0 x 2^exp
    // rather than
    //      1.<significand_bits> x 2^exp
    const PHYSICAL_SIGNIFICAND_SIZE: i32 = 52;
    const EXPONENT_BIAS: i32 = 0x3ff + PHYSICAL_SIGNIFICAND_SIZE;
    const DENORMAL_EXPONENT: i32 = -EXPONENT_BIAS + 1;

    if dbl.is_subnormal() {
        DENORMAL_EXPONENT
    } else {
        let biased_e = (dbl.to_bits() >> PHYSICAL_SIGNIFICAND_SIZE) as i32;
        biased_e - EXPONENT_BIAS
    }
}

#[allow(clippy::float_cmp)]
#[allow(unused_assignments)] // Remove this when the Condition B panic is removed
#[allow(unreachable_code)] // Remove this when the Condition B panic is removed
pub fn double_to_radix_string(val: f64, radix: i32) -> String {
    // This code is pretty blatantly grabbed from v8 source, and rewritten in rust.
    // See: https://github.com/v8/v8/blob/3847b33fda814db5c7540501c1646eb3a85198a7/src/numbers/conversions.cc#L1378

    // Character array used for conversion.
    let chars = b"0123456789abcdefghijklmnopqrstuvwxyz";

    // Temporary buffer for the result. We start with the decimal point in the
    // middle and write to the left for the integer part and to the right for the
    // fractional part. 1024 characters for the exponent and 52 for the mantissa
    // either way, with additional space for sign, decimal point and string
    // termination should be sufficient.
    const KBUFFERSIZE: usize = 2200;
    let mut buffer: [u8; KBUFFERSIZE] = [0; KBUFFERSIZE];
    let mut integer_cursor = KBUFFERSIZE / 2;
    let mut fraction_cursor = integer_cursor;

    let negative = val < 0.0;
    let value = if negative { -val } else { val };

    // Split the value into an integer part and a fractional part.
    let mut integer = value.floor();
    let mut fraction = value - integer;
    // We only compute fractional digits up to the input double's precision.
    let mut delta = 0.5 * (next_double(value) - value);
    delta = delta.max(next_double(0.0));
    if fraction >= delta {
        // Insert decimal point.
        buffer[fraction_cursor] = b'.';
        fraction_cursor += 1;
        while fraction >= delta {
            // Shift up by one digit.
            fraction *= radix as f64;
            delta *= radix as f64;
            // Write digit.
            let digit = fraction as usize;
            buffer[fraction_cursor] = chars[digit];
            fraction_cursor += 1;
            // Calculate remainder.
            fraction -= digit as f64;
            // Round to even.
            assert!(
                !(fraction + delta > 1.0 && fraction == 0.5 && digit & 1 != 0),
                "Condition A met with radix {} and input val {}: Please add this to coverage and remove this panic.",
                radix,
                val
            );
            if (fraction > 0.5 || (fraction == 0.5 && digit & 1 != 0)) && fraction + delta > 1.0 {
                // We need to back trace already written digits in case of carry-over.
                loop {
                    fraction_cursor -= 1;
                    if fraction_cursor == KBUFFERSIZE / 2 {
                        // Carry over to the integer part.
                        integer += 1.0;
                        panic!("Condition B met with radix {} and input val {}: Please add this to coverage and remove this panic.", radix, val);
                        break;
                    }
                    let c = buffer[fraction_cursor] as i32;
                    // Reconstruct digit.
                    let digit = if c > '9' as i32 { c - 'a' as i32 + 10 } else { c - '0' as i32 };
                    if digit + 1 < radix {
                        buffer[fraction_cursor] = chars[digit as usize + 1];
                        fraction_cursor += 1;
                        break;
                    } else {
                        panic!("Condition C met with radix {} and input val {}: Please add this to coverage and remove this panic.", radix, val);
                    }
                }
                break;
            }
        }
    }

    // Compute integer digits. Fill unrepresented digits with zero.
    while double_exponent(integer / radix as f64) > 0 {
        integer /= radix as f64;
        integer_cursor -= 1;
        buffer[integer_cursor] = b'0';
    }
    loop {
        let remainder = integer % radix as f64;
        integer_cursor -= 1;
        buffer[integer_cursor] = chars[remainder as usize];
        integer = (integer - remainder) / radix as f64;
        if integer <= 0.0 {
            break;
        }
    }

    // Add sign
    if negative {
        integer_cursor -= 1;
        buffer[integer_cursor] = b'-';
    }
    // Allocate new String as return value.
    String::from_utf8_lossy(&buffer[integer_cursor..fraction_cursor]).to_string()
}

// Number.prototype.toString ( [ radix ] )
//
// NOTE     The optional radix should be an integral Number value in the inclusive range 2𝔽 to 36𝔽. If radix is
//          undefined then 10𝔽 is used as the value of radix.
//
// The following steps are performed:
//
// 1. Let x be ? thisNumberValue(this value).
// 2. If radix is undefined, let radixMV be 10.
// 3. Else, let radixMV be ? ToIntegerOrInfinity(radix).
// 4. If radixMV < 2 or radixMV > 36, throw a RangeError exception.
// 5. If radixMV = 10, return ! ToString(x).
// 6. Return the String representation of this Number value using the radix specified by radixMV. Letters a-z are used
//    for digits with values 10 through 35. The precise algorithm is implementation-defined, however the algorithm
//    should be a generalization of that specified in 6.1.6.1.20.
//
// The toString function is not generic; it throws a TypeError exception if its this value is not a Number or a Number
// object. Therefore, it cannot be transferred to other kinds of objects for use as a method.
//
// The "length" property of the toString method is 1𝔽.
fn number_prototype_to_string(
    agent: &Agent,
    this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let mut args = FuncArgs::from(arguments);
    let radix = args.next_arg();
    let x = this_number_value(agent, this_value)?;
    let radix_mv = if radix.is_undefined() { 10.0 } else { to_integer_or_infinity(agent, radix)? };
    if !(2.0..=36.0).contains(&radix_mv) {
        Err(create_range_error(agent, format!("Radix {} out of range (must be in 2..36)", radix_mv)))
    } else {
        let iradix = radix_mv as i32;
        if iradix == 10 {
            Ok(ECMAScriptValue::from(to_string(agent, ECMAScriptValue::from(x)).unwrap()))
        } else {
            Ok(ECMAScriptValue::from(double_to_radix_string(x, iradix)))
        }
    }
}

// Number.prototype.valueOf ( )
//  1. Return ? thisNumberValue(this value).
fn number_prototype_value_of(
    agent: &Agent,
    this_value: ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    this_number_value(agent, this_value).map(ECMAScriptValue::Number)
}

#[cfg(test)]
mod tests;
