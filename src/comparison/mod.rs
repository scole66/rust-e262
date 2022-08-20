use super::*;

// RequireObjectCoercible ( argument )
//
// The abstract operation RequireObjectCoercible takes argument argument. It throws an error if argument is a value
// that cannot be converted to an Object using ToObject. It is defined by the following table:
//
// +---------------+------------------------------+
// | Argument Type | Result                       |
// +---------------+------------------------------+
// | Undefined     | Throw a TypeError exception. |
// +---------------+------------------------------+
// | Null          | Throw a TypeError exception. |
// +---------------+------------------------------+
// | Boolean       | Return argument.             |
// +---------------+------------------------------+
// | Number        | Return argument.             |
// +---------------+------------------------------+
// | String        | Return argument.             |
// +---------------+------------------------------+
// | Symbol        | Return argument.             |
// +---------------+------------------------------+
// | BigInt        | Return argument.             |
// +---------------+------------------------------+
// | Object        | Return argument.             |
// +---------------+------------------------------+
//
// https://tc39.es/ecma262/#sec-requireobjectcoercible
pub fn require_object_coercible(agent: &Agent, argument: &ECMAScriptValue) -> Completion<()> {
    match argument {
        ECMAScriptValue::Undefined | ECMAScriptValue::Null => {
            Err(create_type_error(agent, "Undefined and null are not allowed in this context"))
        }
        _ => Ok(()),
    }
}

// IsExtensible ( O )
//
// The abstract operation IsExtensible takes argument O (an Object) and returns a completion record which, if its
// [[Type]] is normal, has a [[Value]] which is a Boolean. It is used to determine whether additional properties can be
// added to O. It performs the following steps when called:
//
//  1. Assert: Type(O) is Object.
//  2. Return ? O.[[IsExtensible]]().
pub fn is_extensible<'a, T>(agent: &Agent, obj: T) -> Completion<bool>
where
    T: Into<&'a dyn ObjectInterface>,
{
    obj.into().is_extensible(agent)
}

// IsIntegralNumber ( argument )
//
// The abstract operation IsIntegralNumber takes argument argument. It determines if argument is a finite integral
// Number value. It performs the following steps when called:
//
//  1. If Type(argument) is not Number, return false.
//  2. If argument is NaN, +∞𝔽, or -∞𝔽, return false.
//  3. If floor(abs(ℝ(argument))) ≠ abs(ℝ(argument)), return false.
//  4. Return true.
#[allow(clippy::float_cmp)]
pub fn is_integral_number(argument: &ECMAScriptValue) -> bool {
    match argument {
        ECMAScriptValue::Number(n) => {
            if !n.is_finite() {
                false
            } else {
                let magnitude = n.abs();
                magnitude.floor() == magnitude
            }
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests;
