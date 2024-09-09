use super::*;
use rand::prelude::*;
use std::f64;

pub fn provision_math_intrinsic(realm: &Rc<RefCell<Realm>>) {
    // The Math object:
    //
    //  * is %Math%.
    //  * is the initial value of the "Math" property of the global object.
    //  * is an ordinary object.
    //  * has a [[Prototype]] internal slot whose value is %Object.prototype%.
    //  * is not a function object.
    //  * does not have a [[Construct]] internal method; it cannot be used as a constructor with the new operator.
    //  * does not have a [[Call]] internal method; it cannot be invoked as a function.

    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let math = ordinary_object_create(Some(object_prototype));
    realm.borrow_mut().intrinsics.math = math.clone();

    macro_rules! data_property {
        ( $name:expr, $value:expr ) => {
            define_property_or_throw(
                &math,
                $name,
                PotentialPropertyDescriptor::new()
                    .value(ECMAScriptValue::from($value))
                    .writable(false)
                    .enumerable(false)
                    .configurable(false),
            )
            .unwrap();
        };
    }

    data_property!("E", f64::consts::E);
    data_property!("LN10", f64::consts::LN_10);
    data_property!("LN2", f64::consts::LN_2);
    data_property!("LOG10E", f64::consts::LOG10_E);
    data_property!("LOG2E", f64::consts::LOG2_E);
    data_property!("PI", f64::consts::PI);
    data_property!("SQRT1_2", f64::consts::FRAC_1_SQRT_2);
    data_property!("SQRT2", f64::consts::SQRT_2);

    define_property_or_throw(
        &math,
        wks(WksId::ToStringTag),
        PotentialPropertyDescriptor::new().value("Math").writable(false).enumerable(false).configurable(true),
    )
    .unwrap();

    let function_prototype = realm.borrow().intrinsics.function_prototype.clone();
    macro_rules! function_property {
        ( $steps:expr, $name:expr, $length:expr ) => {
            let key = PropertyKey::from($name);
            let function_object = create_builtin_function(
                $steps,
                false,
                f64::from($length),
                key.clone(),
                BUILTIN_FUNCTION_SLOTS,
                Some(realm.clone()),
                Some(function_prototype.clone()),
                None,
            );
            define_property_or_throw(
                &math,
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
    // Math.abs ( x )
    function_property!(math_abs, "abs", 1);
    // Math.acos ( x )
    function_property!(math_acos, "acos", 1);
    // Math.acosh ( x )
    function_property!(math_acosh, "acosh", 1);
    // Math.asin ( x )
    function_property!(math_asin, "asin", 1);
    // Math.asinh ( x )
    function_property!(math_asinh, "asinh", 1);
    // Math.atan ( x )
    function_property!(math_atan, "atan", 1);
    // Math.atanh ( x )
    function_property!(math_atanh, "atanh", 1);
    // Math.atan2 ( y, x )
    function_property!(math_atan2, "atan2", 2);
    // Math.cbrt ( x )
    function_property!(math_cbrt, "cbrt", 1);
    // Math.ceil ( x )
    function_property!(math_ceil, "ceil", 1);
    // Math.clz32 ( x )
    function_property!(math_clz32, "clz32", 1);
    // Math.cos ( x )
    function_property!(math_cos, "cos", 1);
    // Math.cosh ( x )
    function_property!(math_cosh, "cosh", 1);
    // Math.exp ( x )
    function_property!(math_exp, "exp", 1);
    // Math.expm1 ( x )
    function_property!(math_expm1, "expm1", 1);
    // Math.floor ( x )
    function_property!(math_floor, "floor", 1);
    // Math.fround ( x )
    function_property!(math_fround, "fround", 1);
    // Math.hypot ( ...args )
    function_property!(math_hypot, "hypot", 2);
    // Math.imul ( x, y )
    function_property!(math_imul, "imul", 2);
    // Math.log ( x )
    function_property!(math_log, "log", 1);
    // Math.log1p ( x )
    function_property!(math_log1p, "log1p", 1);
    // Math.log10 ( x )
    function_property!(math_log10, "log10", 1);
    // Math.log2 ( x )
    function_property!(math_log2, "log2", 1);
    // Math.max ( ...args )
    function_property!(math_max, "max", 2);
    // Math.min ( ...args )
    function_property!(math_min, "min", 2);
    // Math.pow ( base, exponent )
    function_property!(math_pow, "pow", 2);
    // Math.random ( )
    function_property!(math_random, "random", 0);
    // Math.round ( x )
    function_property!(math_round, "round", 1);
    // Math.sign ( x )
    function_property!(math_sign, "sign", 1);
    // Math.sin ( x )
    function_property!(math_sin, "sin", 1);
    // Math.sinh ( x )
    function_property!(math_sinh, "sinh", 1);
    // Math.sqrt ( x )
    function_property!(math_sqrt, "sqrt", 1);
    // Math.tan ( x )
    function_property!(math_tan, "tan", 1);
    // Math.tanh ( x )
    function_property!(math_tanh, "tanh", 1);
    // Math.trunc ( x )
    function_property!(math_trunc, "trunc", 1);
}

fn math_abs(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.abs ( x )
    // This function returns the absolute value of x; the result has the same magnitude as x but has positive sign.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is NaN, return NaN.
    //  3. If n is -0𝔽, return +0𝔽.
    //  4. If n is -∞𝔽, return +∞𝔽.
    //  5. If n < -0𝔽, return -n.
    //  6. Return n.
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.abs().into())
}

fn math_acos(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.acos ( x )
    // This function returns the inverse cosine of x. The result is expressed in radians and is in the inclusive
    // interval from +0𝔽 to 𝔽(π).
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is NaN, n > 1𝔽, or n < -1𝔽, return NaN.
    //  3. If n is 1𝔽, return +0𝔽.
    //  4. Return an implementation-approximated Number value representing the result of the inverse cosine of ℝ(n).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.acos().into())
}

fn math_acosh(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.acosh ( x )
    // This function returns the inverse hyperbolic cosine of x.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is either NaN or +∞𝔽, return n.
    //  3. If n is 1𝔽, return +0𝔽.
    //  4. If n < 1𝔽, return NaN.
    //  5. Return an implementation-approximated Number value representing the result of the inverse hyperbolic cosine
    //     of ℝ(n).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.acosh().into())
}

fn math_asin(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.asin ( x )
    // This function returns the inverse sine of x. The result is expressed in radians and is in the inclusive interval
    // from 𝔽(-π / 2) to 𝔽(π / 2).
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
    //  3. If n > 1𝔽 or n < -1𝔽, return NaN.
    //  4. Return an implementation-approximated Number value representing the result of the inverse sine of ℝ(n).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.asin().into())
}

fn math_asinh(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.asinh ( x )
    // This function returns the inverse hyperbolic sine of x.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is not finite or n is either +0𝔽 or -0𝔽, return n.
    //  3. Return an implementation-approximated Number value representing the result of the inverse hyperbolic sine of
    //     ℝ(n).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.asinh().into())
}

fn math_atan(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.atan ( x )
    // This function returns the inverse tangent of x. The result is expressed in radians and is in the inclusive
    // interval from 𝔽(-π / 2) to 𝔽(π / 2).
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
    //  3. If n is +∞𝔽, return an implementation-approximated Number value representing π / 2.
    //  4. If n is -∞𝔽, return an implementation-approximated Number value representing -π / 2.
    //  5. Return an implementation-approximated Number value representing the result of the inverse tangent of ℝ(n).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.atan().into())
}

fn math_atanh(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.atanh ( x )
    // This function returns the inverse hyperbolic tangent of x.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
    //  3. If n > 1𝔽 or n < -1𝔽, return NaN.
    //  4. If n is 1𝔽, return +∞𝔽.
    //  5. If n is -1𝔽, return -∞𝔽.
    //  6. Return an implementation-approximated Number value representing the result of the inverse hyperbolic tangent
    //     of ℝ(n).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.atanh().into())
}

fn math_atan2(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.atan2 ( y, x )
    // This function returns the inverse tangent of the quotient y / x of the arguments y and x, where the signs of y
    // and x are used to determine the quadrant of the result. Note that it is intentional and traditional for the
    // two-argument inverse tangent function that the argument named y be first and the argument named x be second. The
    // result is expressed in radians and is in the inclusive interval from -π to +π.
    //
    // It performs the following steps when called:
    //
    //  1. Let ny be ? ToNumber(y).
    //  2. Let nx be ? ToNumber(x).
    //  3. If ny is NaN or nx is NaN, return NaN.
    //  4. If ny is +∞𝔽, then
    //      a. If nx is +∞𝔽, return an implementation-approximated Number value representing π / 4.
    //      b. If nx is -∞𝔽, return an implementation-approximated Number value representing 3π / 4.
    //      c. Return an implementation-approximated Number value representing π / 2.
    //  5. If ny is -∞𝔽, then
    //      a. If nx is +∞𝔽, return an implementation-approximated Number value representing -π / 4.
    //      b. If nx is -∞𝔽, return an implementation-approximated Number value representing -3π / 4.
    //      c. Return an implementation-approximated Number value representing -π / 2.
    //  6. If ny is +0𝔽, then
    //      a. If nx > +0𝔽 or nx is +0𝔽, return +0𝔽.
    //      b. Return an implementation-approximated Number value representing π.
    //  7. If ny is -0𝔽, then
    //      a. If nx > +0𝔽 or nx is +0𝔽, return -0𝔽.
    //      b. Return an implementation-approximated Number value representing -π.
    //  8. Assert: ny is finite and is neither +0𝔽 nor -0𝔽.
    //  9. If ny > +0𝔽, then
    //      a. If nx is +∞𝔽, return +0𝔽.
    //      b. If nx is -∞𝔽, return an implementation-approximated Number value representing π.
    //      c. If nx is either +0𝔽 or -0𝔽, return an implementation-approximated Number value representing π / 2.
    //  10. If ny < -0𝔽, then
    //      a. If nx is +∞𝔽, return -0𝔽.
    //      b. If nx is -∞𝔽, return an implementation-approximated Number value representing -π.
    //      c. If nx is either +0𝔽 or -0𝔽, return an implementation-approximated Number value representing -π / 2.
    //  11. Assert: nx is finite and is neither +0𝔽 nor -0𝔽.
    //  12. Return an implementation-approximated Number value representing the result of the inverse tangent of the
    //      quotient ℝ(ny) / ℝ(nx).
    let mut args = FuncArgs::from(arguments);
    let y = args.next_arg().to_number()?;
    let x = args.next_arg().to_number()?;
    Ok(y.atan2(x).into())
}

fn math_cbrt(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.cbrt ( x )
    // This function returns the cube root of x.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is not finite or n is either +0𝔽 or -0𝔽, return n.
    //  3. Return an implementation-approximated Number value representing the result of the cube root of ℝ(n).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.cbrt().into())
}

fn math_ceil(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.ceil ( x )
    // This function returns the smallest (closest to -∞) integral Number value that is not less than x. If x is already
    // an integral Number, the result is x.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is not finite or n is either +0𝔽 or -0𝔽, return n.
    //  3. If n < -0𝔽 and n > -1𝔽, return -0𝔽.
    //  4. If n is an integral Number, return n.
    //  5. Return the smallest (closest to -∞) integral Number value that is not less than n.
    // NOTE
    // The value of Math.ceil(x) is the same as the value of -Math.floor(-x).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.ceil().into())
}

fn math_clz32(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.clz32 ( x )
    // This function performs the following steps when called:
    //
    //  1. Let n be ? ToUint32(x).
    //  2. Let p be the number of leading zero bits in the unsigned 32-bit binary representation of n.
    //  3. Return 𝔽(p).
    // NOTE
    // If n is either +0𝔽 or -0𝔽, this method returns 32𝔽. If the most significant bit of the 32-bit binary encoding of n is 1, this method returns +0𝔽.
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_uint32()?.leading_zeros().into())
}

fn math_cos(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.cos ( x )
    // This function returns the cosine of x. The argument is expressed in radians.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is not finite, return NaN.
    //  3. If n is either +0𝔽 or -0𝔽, return 1𝔽.
    //  4. Return an implementation-approximated Number value representing the result of the cosine of ℝ(n).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.cos().into())
}

fn math_cosh(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.cosh ( x )
    // This function returns the hyperbolic cosine of x.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is NaN, return NaN.
    //  3. If n is either +∞𝔽 or -∞𝔽, return +∞𝔽.
    //  4. If n is either +0𝔽 or -0𝔽, return 1𝔽.
    //  5. Return an implementation-approximated Number value representing the result of the hyperbolic cosine of ℝ(n).
    // NOTE
    // The value of Math.cosh(x) is the same as the value of (Math.exp(x) + Math.exp(-x)) / 2.
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.cosh().into())
}

fn math_exp(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.exp ( x )
    // This function returns the exponential function of x (e raised to the power of x, where e is the base of the
    // natural logarithms).
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is either NaN or +∞𝔽, return n.
    //  3. If n is either +0𝔽 or -0𝔽, return 1𝔽.
    //  4. If n is -∞𝔽, return +0𝔽.
    //  5. Return an implementation-approximated Number value representing the result of the exponential function of
    //     ℝ(n).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.exp().into())
}

fn math_expm1(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.expm1 ( x )
    // This function returns the result of subtracting 1 from the exponential function of x (e raised to the power of x,
    // where e is the base of the natural logarithms). The result is computed in a way that is accurate even when the
    // value of x is close to 0.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is one of NaN, +0𝔽, -0𝔽, or +∞𝔽, return n.
    //  3. If n is -∞𝔽, return -1𝔽.
    //  4. Return an implementation-approximated Number value representing the result of subtracting 1 from the
    //     exponential function of ℝ(n).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.exp_m1().into())
}

fn math_floor(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.floor ( x )
    // This function returns the greatest (closest to +∞) integral Number value that is not greater than x. If x is already an integral Number, the result is x.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is not finite or n is either +0𝔽 or -0𝔽, return n.
    //  3. If n < 1𝔽 and n > +0𝔽, return +0𝔽.
    //  4. If n is an integral Number, return n.
    //  5. Return the greatest (closest to +∞) integral Number value that is not greater than n.
    // NOTE
    // The value of Math.floor(x) is the same as the value of -Math.ceil(-x).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.floor().into())
}

#[expect(clippy::cast_possible_truncation)]
fn math_fround(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.fround ( x )
    // This function performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is NaN, return NaN.
    //  3. If n is one of +0𝔽, -0𝔽, +∞𝔽, or -∞𝔽, return n.
    //  4. Let n32 be the result of converting n to IEEE 754-2019 binary32 format using roundTiesToEven mode.
    //  5. Let n64 be the result of converting n32 to IEEE 754-2019 binary64 format.
    //  6. Return the ECMAScript Number value corresponding to n64.
    let mut args = FuncArgs::from(arguments);
    let n = args.next_arg().to_number()?;
    let n32 = n as f32;
    Ok(f64::from(n32).into())
}

fn math_hypot(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.hypot ( ...args )
    // Given zero or more arguments, this function returns the square root of the sum of squares of its arguments.
    //
    // It performs the following steps when called:
    //
    //  1. Let coerced be a new empty List.
    //  2. For each element arg of args, do
    //      a. Let n be ? ToNumber(arg).
    //      b. Append n to coerced.
    //  3. For each element number of coerced, do
    //      a. If number is either +∞𝔽 or -∞𝔽, return +∞𝔽.
    //  4. Let onlyZero be true.
    //  5. For each element number of coerced, do
    //      a. If number is NaN, return NaN.
    //      b. If number is neither +0𝔽 nor -0𝔽, set onlyZero to false.
    //  6. If onlyZero is true, return +0𝔽.
    //  7. Return an implementation-approximated Number value representing the square root of the sum of squares of the
    //     mathematical values of the elements of coerced.
    // The "length" property of this function is 2𝔽.
    //
    // NOTE
    // Implementations should take care to avoid the loss of precision from overflows and underflows that are prone to
    // occur in naive implementations when this function is called with two or more arguments.
    let coerced = arguments.iter().map(ECMAScriptValue::to_number).collect::<Completion<Vec<_>>>()?;
    let mut saw_nan = false;
    let mut only_zero = true;
    for item in &coerced {
        if item.is_infinite() {
            return Ok(f64::INFINITY.into());
        }
        if item.is_nan() {
            saw_nan = true;
        }
        if *item != 0.0 {
            only_zero = false;
        }
    }
    if saw_nan {
        return Ok(f64::NAN.into());
    }
    if only_zero {
        return Ok(0.into());
    }
    Ok(coerced[1..].iter().fold(coerced[0], |acc, f| acc.hypot(*f)).into())
}

fn math_imul(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.imul ( x, y )
    // This function performs the following steps when called:
    //
    //  1. Let a be ℝ(? ToUint32(x)).
    //  2. Let b be ℝ(? ToUint32(y)).
    //  3. Let product be (a × b) modulo 2**32.
    //  4. If product ≥ 2**31, return 𝔽(product - 2**32); otherwise return 𝔽(product).
    let mut args = FuncArgs::from(arguments);
    let a = args.next_arg().to_uint32()?;
    let b = args.next_arg().to_uint32()?;
    let product = a * b;
    let result = if product >= 2_147_483_648 { i64::from(product) - 4_294_967_296 } else { i64::from(product) };
    Ok(result.into())
}

fn math_log(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.log ( x )
    // This function returns the natural logarithm of x.
    //
    // It performs the following steps when called:
    //
    // 1. Let n be ? ToNumber(x).
    // 2. If n is either NaN or +∞𝔽, return n.
    // 3. If n is 1𝔽, return +0𝔽.
    // 4. If n is either +0𝔽 or -0𝔽, return -∞𝔽.
    // 5. If n < -0𝔽, return NaN.
    // 6. Return an implementation-approximated Number value representing the result of the natural logarithm of ℝ(n).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.ln().into())
}

fn math_log1p(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.log1p ( x )
    // This function returns the natural logarithm of 1 + x. The result is computed in a way that is accurate even when
    // the value of x is close to zero.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is one of NaN, +0𝔽, -0𝔽, or +∞𝔽, return n.
    //  3. If n is -1𝔽, return -∞𝔽.
    //  4. If n < -1𝔽, return NaN.
    //  5. Return an implementation-approximated Number value representing the result of the natural logarithm of 1 +
    //     ℝ(n).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.ln_1p().into())
}

fn math_log10(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.log10 ( x )
    // This function returns the base 10 logarithm of x.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is either NaN or +∞𝔽, return n.
    //  3. If n is 1𝔽, return +0𝔽.
    //  4. If n is either +0𝔽 or -0𝔽, return -∞𝔽.
    //  5. If n < -0𝔽, return NaN.
    //  6. Return an implementation-approximated Number value representing the result of the base 10 logarithm of ℝ(n).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.log10().into())
}

fn math_log2(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.log2 ( x )
    // This function returns the base 2 logarithm of x.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is either NaN or +∞𝔽, return n.
    //  3. If n is 1𝔽, return +0𝔽.
    //  4. If n is either +0𝔽 or -0𝔽, return -∞𝔽.
    //  5. If n < -0𝔽, return NaN.
    //  6. Return an implementation-approximated Number value representing the result of the base 2 logarithm of ℝ(n).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.log2().into())
}

fn math_max(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.max ( ...args )
    // Given zero or more arguments, this function calls ToNumber on each of the arguments and returns the largest of
    // the resulting values.
    //
    // It performs the following steps when called:
    //
    //  1. Let coerced be a new empty List.
    //  2. For each element arg of args, do
    //      a. Let n be ? ToNumber(arg).
    //      b. Append n to coerced.
    //  3. Let highest be -∞𝔽.
    //  4. For each element number of coerced, do
    //      a. If number is NaN, return NaN.
    //      b. If number is +0𝔽 and highest is -0𝔽, set highest to +0𝔽.
    //      c. If number > highest, set highest to number.
    //  5. Return highest.
    // NOTE The comparison of values to determine the largest value is done using the IsLessThan algorithm except that
    // +0𝔽 is considered to be larger than -0𝔽.
    let coerced = arguments.iter().map(ECMAScriptValue::to_number).collect::<Completion<Vec<_>>>()?;
    let mut highest = f64::NEG_INFINITY;
    for number in coerced {
        if number.is_nan() {
            return Ok(f64::NAN.into());
        }
        if (number == 0.0 && number.signum() > 0.0 && highest == 0.0 && highest.signum() < 0.0) || number > highest {
            highest = number;
        }
    }
    Ok(highest.into())
}

fn math_min(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.min ( ...args )
    // Given zero or more arguments, this function calls ToNumber on each of the arguments and returns the smallest of
    // the resulting values.
    //
    // It performs the following steps when called:
    //
    //  1. Let coerced be a new empty List.
    //  2. For each element arg of args, do
    //      a. Let n be ? ToNumber(arg).
    //      b. Append n to coerced.
    //  3. Let lowest be +∞𝔽.
    //  4. For each element number of coerced, do
    //      a. If number is NaN, return NaN.
    //      b. If number is -0𝔽 and lowest is +0𝔽, set lowest to -0𝔽.
    //      c. If number < lowest, set lowest to number.
    //  5. Return lowest.
    // NOTE The comparison of values to determine the largest value is done using the IsLessThan algorithm except that
    // +0𝔽 is considered to be larger than -0𝔽.
    let coerced = arguments.iter().map(|v| v.clone().to_number()).collect::<Completion<Vec<_>>>()?;
    let mut lowest = f64::INFINITY;
    for number in coerced {
        if number.is_nan() {
            return Ok(f64::NAN.into());
        }
        if (number == 0.0 && number.signum() < 0.0 && lowest == 0.0 && lowest.signum() > 0.0) || number < lowest {
            lowest = number;
        }
    }
    Ok(lowest.into())
}

fn math_pow(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.pow ( base, exponent )
    // This function performs the following steps when called:
    //
    //  1. Set base to ? ToNumber(base).
    //  2. Set exponent to ? ToNumber(exponent).
    //  3. Return Number::exponentiate(base, exponent).
    let mut args = FuncArgs::from(arguments);
    let base = args.next_arg().to_number()?;
    let exponent = args.next_arg().to_number()?;
    Ok(exponentiate(base, exponent).into())
}

#[expect(clippy::unnecessary_wraps)]
fn math_random(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.random ( )
    // This function returns a Number value with positive sign, greater than or equal to +0𝔽 but strictly less than
    // 1𝔽, chosen randomly or pseudo randomly with approximately uniform distribution over that range, using an
    // implementation-defined algorithm or strategy.
    //
    // Each Math.random function created for distinct realms must produce a distinct sequence of values from successive
    // calls.
    Ok(random::<f64>().into())
}

fn math_round(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.round ( x )
    // This function returns the Number value that is closest to x and is integral. If two integral Numbers are equally
    // close to x, then the result is the Number value that is closer to +∞. If x is already integral, the result is x.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is not finite or n is an integral Number, return n.
    //  3. If n < 0.5𝔽 and n > +0𝔽, return +0𝔽.
    //  4. If n < -0𝔽 and n ≥ -0.5𝔽, return -0𝔽.
    //  5. Return the integral Number closest to n, preferring the Number closer to +∞ in the case of a tie.
    // NOTE 1
    // Math.round(3.5) returns 4, but Math.round(-3.5) returns -3.
    //
    // NOTE 2
    // The value of Math.round(x) is not always the same as the value of Math.floor(x + 0.5). When x is -0𝔽 or x is
    // less than +0𝔽 but greater than or equal to -0.5𝔽, Math.round(x) returns -0𝔽, but Math.floor(x + 0.5) returns
    // +0𝔽. Math.round(x) may also differ from the value of Math.floor(x + 0.5) because of internal rounding when
    // computing x + 0.5.
    let mut args = FuncArgs::from(arguments);
    let n = args.next_arg().to_number()?;
    if (-0.5..0.0).contains(&n) {
        return Ok((-0.0).into());
    }
    let mut rounded = n.round();
    if rounded < n && n - rounded == (rounded + 1.0) - n {
        rounded += 1.0;
    }
    Ok(rounded.into())
}

fn math_sign(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.sign ( x )
    // This function returns the sign of x, indicating whether x is positive, negative, or zero.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
    //  3. If n < -0𝔽, return -1𝔽.
    //  4. Return 1𝔽.
    let mut args = FuncArgs::from(arguments);
    let n = args.next_arg().to_number()?;
    if n == 0.0 {
        return Ok(n.into());
    }
    Ok(n.signum().into())
}

fn math_sin(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.sin ( x )
    // This function returns the sine of x. The argument is expressed in radians.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
    //  3. If n is either +∞𝔽 or -∞𝔽, return NaN.
    //  4. Return an implementation-approximated Number value representing the result of the sine of ℝ(n).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.sin().into())
}

fn math_sinh(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.sinh ( x )
    // This function returns the hyperbolic sine of x.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is not finite or n is either +0𝔽 or -0𝔽, return n.
    //  3. Return an implementation-approximated Number value representing the result of the hyperbolic sine of ℝ(n).
    // NOTE
    // The value of Math.sinh(x) is the same as the value of (Math.exp(x) - Math.exp(-x)) / 2.
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.sinh().into())
}

fn math_sqrt(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.sqrt ( x )
    // This function returns the square root of x.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is one of NaN, +0𝔽, -0𝔽, or +∞𝔽, return n.
    //  3. If n < -0𝔽, return NaN.
    //  4. Return an implementation-approximated Number value representing the result of the square root of ℝ(n).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.sqrt().into())
}

fn math_tan(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.tan ( x )
    // This function returns the tangent of x. The argument is expressed in radians.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
    //  3. If n is either +∞𝔽 or -∞𝔽, return NaN.
    //  4. Return an implementation-approximated Number value representing the result of the tangent of ℝ(n).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.tan().into())
}

fn math_tanh(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.tanh ( x )
    // This function returns the hyperbolic tangent of x.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is one of NaN, +0𝔽, or -0𝔽, return n.
    //  3. If n is +∞𝔽, return 1𝔽.
    //  4. If n is -∞𝔽, return -1𝔽.
    //  5. Return an implementation-approximated Number value representing the result of the hyperbolic tangent of ℝ(n).
    // NOTE The value of Math.tanh(x) is the same as the value of (Math.exp(x) - Math.exp(-x)) / (Math.exp(x) +
    // Math.exp(-x)).
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.tanh().into())
}

fn math_trunc(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    // Math.trunc ( x )
    // This function returns the integral part of the number x, removing any fractional digits. If x is already integral, the result is x.
    //
    // It performs the following steps when called:
    //
    //  1. Let n be ? ToNumber(x).
    //  2. If n is not finite or n is either +0𝔽 or -0𝔽, return n.
    //  3. If n < 1𝔽 and n > +0𝔽, return +0𝔽.
    //  4. If n < -0𝔽 and n > -1𝔽, return -0𝔽.
    //  5. Return the integral Number nearest n in the direction of +0𝔽.
    let mut args = FuncArgs::from(arguments);
    Ok(args.next_arg().to_number()?.trunc().into())
}
