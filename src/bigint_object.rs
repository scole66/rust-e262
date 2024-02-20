use super::*;
use num::{BigInt, FromPrimitive, Integer};
use std::rc::Rc;

pub fn provision_big_int_intrinsic(realm: &Rc<RefCell<Realm>>) {
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();

    // The BigInt constructor:
    //
    //  * is %BigInt%.
    //  * is the initial value of the "BigInt" property of the global object.
    //  * performs a type conversion when called as a function rather than as a constructor.
    //  * is not intended to be used with the new operator or to be subclassed. It may be used as the value of an
    //    extends clause of a class definition but a super call to the BigInt constructor will cause an exception.

    // Properties of the BigInt Constructor
    // The BigInt constructor:
    //
    //  * has a [[Prototype]] internal slot whose value is %Function.prototype%.
    let bigint_constructor = create_builtin_function(
        bigint_constructor_function,
        true,
        1.0,
        PropertyKey::from("BigInt"),
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
                $steps,
                false,
                $length as f64,
                key.clone(),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm.clone()),
                Some(function_prototype.clone()),
                None,
            );
            define_property_or_throw(
                &bigint_constructor,
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
    constructor_function!(bigint_as_int_n, "asIntN", 2);
    constructor_function!(bigint_as_uint_n, "asUintN", 2);

    // Properties of the BigInt Prototype Object
    // The BigInt prototype object:
    //
    //  * is %BigInt.prototype%.
    //  * is an ordinary object.
    //  * is not a BigInt object; it does not have a [[BigIntData]] internal slot.
    //  * has a [[Prototype]] internal slot whose value is %Object.prototype%.
    //  * The phrase “this BigInt value” within the specification of a method refers to the result returned by calling
    //    the abstract operation ThisBigIntValue with the this value of the method invocation passed as the argument.
    let bigint_prototype = ordinary_object_create(Some(object_prototype.clone()), &[]);
    define_property_or_throw(
        &bigint_constructor,
        "prototype",
        PotentialPropertyDescriptor::new()
            .value(bigint_prototype.clone())
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
                $steps,
                false,
                $length as f64,
                key.clone(),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm.clone()),
                Some(function_prototype.clone()),
                None,
            );
            define_property_or_throw(
                &bigint_prototype,
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

    prototype_function!(bigint_to_locale_string, "toLocaleString", 0);
    prototype_function!(bigint_to_string, "toString", 0);
    prototype_function!(bigint_value_of, "valueOf", 0);

    define_property_or_throw(
        &bigint_prototype,
        "constructor",
        PotentialPropertyDescriptor::new()
            .value(bigint_constructor.clone())
            .writable(true)
            .enumerable(false)
            .configurable(true),
    )
    .unwrap();

    define_property_or_throw(
        &bigint_prototype,
        wks(WksId::ToStringTag),
        PotentialPropertyDescriptor::new().value("BigInt").writable(false).enumerable(false).configurable(true),
    )
    .unwrap();

    realm.borrow_mut().intrinsics.big_int = bigint_constructor;
    realm.borrow_mut().intrinsics.big_int_prototype = bigint_prototype;
}

fn bigint_constructor_function(
    _this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // BigInt ( value )
    // This function performs the following steps when called:
    //
    //  1. If NewTarget is not undefined, throw a TypeError exception.
    //  2. Let prim be ? ToPrimitive(value, NUMBER).
    //  3. If prim is a Number, return ? NumberToBigInt(prim).
    //  4. Otherwise, return ? ToBigInt(prim).
    let mut args = FuncArgs::from(arguments);
    let value = args.next_arg();
    if new_target.is_some() {
        Err(create_type_error("BigInt may not be used as a constructor"))
    } else {
        let prim = to_primitive(value, Some(ConversionHint::Number))?;
        match prim {
            ECMAScriptValue::Number(_) => Ok(number_to_big_int(prim)?.into()),
            _ => Ok(to_big_int(prim)?.into()),
        }
    }
}

fn bigint_as_int_n(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // BigInt.asIntN ( bits, bigint )
    // This function performs the following steps when called:
    //
    //  1. Set bits to ? ToIndex(bits).
    //  2. Set bigint to ? ToBigInt(bigint).
    //  3. Let mod be ℝ(bigint) modulo 2**bits.
    //  4. If mod ≥ 2**(bits - 1), return ℤ(mod - 2**bits); otherwise, return ℤ(mod).
    let mut args = FuncArgs::from(arguments);
    let bits = args.next_arg();
    let bigint = args.next_arg();
    let bits = to_index(bits)?;
    let bigint = to_big_int(bigint)?;
    if bits == 0 {
        return Ok(Rc::new(BigInt::from(0)).into());
    }
    let modulo = bigint.mod_floor(&(BigInt::from(1) << bits));
    Ok(Rc::new(if modulo >= BigInt::from(1) << (bits - 1) { modulo - (BigInt::from(1) << bits) } else { modulo })
        .into())
}

fn bigint_as_uint_n(
    _: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // BigInt.asUintN ( bits, bigint )
    // This function performs the following steps when called:
    //
    //  1. Set bits to ? ToIndex(bits).
    //  2. Set bigint to ? ToBigInt(bigint).
    //  3. Return ℤ(ℝ(bigint) modulo 2**bits).
    let mut args = FuncArgs::from(arguments);
    let bits = args.next_arg();
    let bigint = args.next_arg();
    let bits = to_index(bits)?;
    let bigint = to_big_int(bigint)?;
    Ok(Rc::new(bigint.mod_floor(&(BigInt::from(1) << bits))).into())
}

fn bigint_to_locale_string(
    this_value: &ECMAScriptValue,
    new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // BigInt.prototype.toLocaleString ( [ reserved1 [ , reserved2 ] ] )
    // An ECMAScript implementation that includes the ECMA-402 Internationalization API must implement this method as
    // specified in the ECMA-402 specification. If an ECMAScript implementation does not include the ECMA-402 API the
    // following specification of this method is used:
    //
    // This method produces a String value that represents this BigInt value formatted according to the conventions of
    // the host environment's current locale. This method is implementation-defined, and it is permissible, but not
    // encouraged, for it to return the same thing as toString.
    //
    // The meanings of the optional parameters to this method are defined in the ECMA-402 specification; implementations
    // that do not include ECMA-402 support must not use those parameter positions for anything else.
    bigint_to_string(this_value, new_target, &[])
}

fn bigint_to_string(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // BigInt.prototype.toString ( [ radix ] )
    // NOTE The optional radix should be an integral Number value in the inclusive interval from 2𝔽 to 36𝔽. If radix
    // is undefined then 10𝔽 is used as the value of radix.
    //
    // This method performs the following steps when called:
    //
    //  1. Let x be ? ThisBigIntValue(this value).
    //  2. If radix is undefined, let radixMV be 10.
    //  3. Else, let radixMV be ? ToIntegerOrInfinity(radix).
    //  4. If radixMV is not in the inclusive interval from 2 to 36, throw a RangeError exception.
    //  5. Return BigInt::toString(x, radixMV).
    // This method is not generic; it throws a TypeError exception if its this value is not a BigInt or a BigInt object.
    // Therefore, it cannot be transferred to other kinds of objects for use as a method.
    let mut args = FuncArgs::from(arguments);
    let radix = args.next_arg();
    let x = this_bigint_value(this_value.clone())?;
    let radix_mv = if radix == ECMAScriptValue::Undefined { 10.0 } else { to_integer_or_infinity(radix)? };
    if (2.0..=36.0).contains(&radix_mv) {
        let radix_mv = to_uint32(radix_mv).expect("radix_mv should be in [2..36]");
        Ok(bigint_to_string_radix(&x, radix_mv).into())
    } else {
        Err(create_range_error("A radix must be an interger between 2 and 36, inclusive"))
    }
}

fn bigint_value_of(
    this_value: &ECMAScriptValue,
    _: Option<&Object>,
    _: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // BigInt.prototype.valueOf ( )
    //  1. Return ? ThisBigIntValue(this value).
    this_bigint_value(this_value.clone()).map(ECMAScriptValue::from)
}

#[derive(Debug)]
pub struct BigIntObject {
    common: RefCell<CommonObjectData>,
    bigint_data: Rc<BigInt>,
}

impl<'a> From<&'a BigIntObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a BigIntObject) -> Self {
        obj
    }
}

impl ObjectInterface for BigIntObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn uses_ordinary_get_prototype_of(&self) -> bool {
        true
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }
    fn to_bigint_object(&self) -> Option<&BigIntObject> {
        Some(self)
    }
    fn is_bigint_object(&self) -> bool {
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

impl BigIntObject {
    fn bigint_data(&self) -> &Rc<BigInt> {
        &self.bigint_data
    }

    pub fn new(prototype: Option<Object>, value: Rc<BigInt>) -> Self {
        Self { common: RefCell::new(CommonObjectData::new(prototype, true, BIGINT_OBJECT_SLOTS)), bigint_data: value }
    }
    pub fn object(prototype: Option<Object>, value: Rc<BigInt>) -> Object {
        Object { o: Rc::new(Self::new(prototype, value)) }
    }

    pub fn value(&self) -> Rc<BigInt> {
        Rc::clone(&self.bigint_data)
    }
}

pub fn create_bigint_object(b: Rc<BigInt>) -> Object {
    let prototype = intrinsic(IntrinsicId::BigIntPrototype);
    BigIntObject::object(Some(prototype), b)
}

fn this_bigint_value(value: ECMAScriptValue) -> Completion<Rc<BigInt>> {
    // ThisBigIntValue ( value )
    // The abstract operation ThisBigIntValue takes argument value (an ECMAScript language value) and returns either a
    // normal completion containing a BigInt or a throw completion. It performs the following steps when called:
    //
    //  1. If value is a BigInt, return value.
    //  2. If value is an Object and value has a [[BigIntData]] internal slot, then
    //      a. Assert: value.[[BigIntData]] is a BigInt.
    //      b. Return value.[[BigIntData]].
    //  3. Throw a TypeError exception.
    const ERRMSG: &str = "Value is not a Big Int";
    match value {
        ECMAScriptValue::BigInt(bi) => Ok(bi),
        ECMAScriptValue::Object(obj) => {
            if let Some(bio) = obj.o.to_bigint_object() {
                Ok(bio.bigint_data().clone())
            } else {
                Err(create_type_error(ERRMSG))
            }
        }
        ECMAScriptValue::Undefined
        | ECMAScriptValue::Null
        | ECMAScriptValue::Boolean(_)
        | ECMAScriptValue::String(_)
        | ECMAScriptValue::Number(_)
        | ECMAScriptValue::Symbol(_) => Err(create_type_error(ERRMSG)),
    }
}

fn number_to_big_int(number: ECMAScriptValue) -> Completion<Rc<BigInt>> {
    // NumberToBigInt ( number )
    // The abstract operation NumberToBigInt takes argument number (a Number) and returns either a normal completion
    // containing a BigInt or a throw completion. It performs the following steps when called:
    //
    //  1. If IsIntegralNumber(number) is false, throw a RangeError exception.
    //  2. Return ℤ(ℝ(number)).
    const ERRMSG: &str = "Non-integral number used in bigint creation";
    if is_integral_number(&number) {
        let num = to_number(number).unwrap();
        Ok(Rc::new(BigInt::from_f64(num).ok_or_else(|| create_range_error(ERRMSG))?))
    } else {
        Err(create_range_error(ERRMSG))
    }
}

fn to_big_int(value: ECMAScriptValue) -> Completion<Rc<BigInt>> {
    // ToBigInt ( argument )
    // The abstract operation ToBigInt takes argument argument (an ECMAScript language value) and returns either a
    // normal completion containing a BigInt or a throw completion. It converts argument to a BigInt value, or throws if
    // an implicit conversion from Number would be required. It performs the following steps when called:
    //
    //  1. Let prim be ? ToPrimitive(argument, NUMBER).
    //  2. Return the value that prim corresponds to in Table 12.
    //
    // Table 12: BigInt Conversions
    //  +---------------+------------------------------------------------------|
    //  | Argument Type | Result                                               |
    //  +---------------+------------------------------------------------------|
    //  | Undefined     | Throw a TypeError exception.                         |
    //  +---------------+------------------------------------------------------|
    //  | Null          | Throw a TypeError exception.                         |
    //  +---------------+------------------------------------------------------|
    //  | Boolean       | Return 1n if prim is true and 0n if prim is false.   |
    //  +---------------+------------------------------------------------------|
    //  | BigInt        | Return prim.                                         |
    //  +---------------+------------------------------------------------------|
    //  | Number        | Throw a TypeError exception.                         |
    //  +---------------+------------------------------------------------------|
    //  | String        | 1. Let n be StringToBigInt(prim).                    |
    //  |               | 2. If n is undefined, throw a SyntaxError exception. |
    //  |               | 3. Return n.                                         |
    //  +---------------+------------------------------------------------------|
    //  | Symbol        | Throw a TypeError exception.                         |
    //  +---------------+------------------------------------------------------|
    let prim = to_primitive(value, Some(ConversionHint::Number))?;
    match prim {
        ECMAScriptValue::Undefined
        | ECMAScriptValue::Null
        | ECMAScriptValue::Number(_)
        | ECMAScriptValue::Symbol(_) => Err(create_type_error("Value cannot be converted to bigint")),
        ECMAScriptValue::Boolean(b) => Ok(Rc::new(BigInt::from(b))),
        ECMAScriptValue::String(s) => {
            let n = string_to_bigint(&s);
            n.ok_or_else(|| create_syntax_error("Invalid character sequence for bigint", None))
        }
        ECMAScriptValue::BigInt(bi) => Ok(bi),
        ECMAScriptValue::Object(_) => unreachable!(),
    }
}
