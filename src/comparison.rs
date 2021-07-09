use crate::agent::Agent;
use crate::cr::AltCompletion;
use crate::object::ObjectInterface;

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
