use crate::agent::Agent;
use crate::cr::AltCompletion;
use crate::object::ObjectInterface;
use crate::values::ECMAScriptValue;

// IsExtensible ( O )
//
// The abstract operation IsExtensible takes argument O (an Object) and returns a completion record which, if its
// [[Type]] is normal, has a [[Value]] which is a Boolean. It is used to determine whether additional properties can be
// added to O. It performs the following steps when called:
//
//  1. Assert: Type(O) is Object.
//  2. Return ? O.[[IsExtensible]]().
pub fn is_extensible<'a, T>(agent: &mut Agent, obj: T) -> AltCompletion<bool>
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
