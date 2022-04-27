use super::agent::Agent;
use super::cr::{AltCompletion, Completion};
use super::environment_record::EnvironmentRecord;
use super::errors::{create_reference_error, create_type_error};
use super::execution_context::get_global_object;
use super::object::{private_get, private_set, set};
use super::strings::JSString;
use super::values::{to_object, ECMAScriptValue, PrivateName, PropertyKey, Symbol};
use num::BigInt;
use std::convert::{TryFrom, TryInto};
use std::rc::Rc;

// The Reference Record Specification Type
//
// The Reference Record type is used to explain the behaviour of such operators as delete, typeof, the assignment
// operators, the super keyword and other language features. For example, the left-hand operand of an assignment is
// expected to produce a Reference Record.
//
// A Reference Record is a resolved name or property binding; its fields are defined by Table 10.
//
// Table 10: Reference Record Fields
// +--------------------+------------------------------+---------------------------------------------------------------+
// | Field Name         | Value                        | Meaning                                                       |
// +--------------------+------------------------------+---------------------------------------------------------------+
// | [[Base]]           | One of:                      | The value or Environment Record which holds the binding. A    |
// |                    | * any ECMAScript language    | [[Base]] of unresolvable indicates that the binding could not |
// |                    |   value,                     | be resolved.                                                  |
// |                    | * an Environment Record, or  |                                                               |
// |                    | * unresolvable.              |                                                               |
// +--------------------+------------------------------+---------------------------------------------------------------+
// | [[ReferencedName]] | String, Symbol, or           | The name of the binding. Always a String if [[Base]] value is |
// |                    | PrivateName                  | an Environment Record.                                        |
// +--------------------+------------------------------+---------------------------------------------------------------+
// | [[Strict]]         | Boolean                      | true if the Reference Record originated in strict mode code,  |
// |                    |                              | false otherwise.                                              |
// +--------------------+------------------------------+---------------------------------------------------------------+
// | [[ThisValue]]      | any ECMAScript language      | If not empty, the Reference Record represents a property      |
// |                    | value or empty               | binding that was expressed using the super keyword; it is     |
// |                    |                              | called a Super Reference Record and its [[Base]] value will   |
// |                    |                              | never be an Environment Record. In that case, the             |
// |                    |                              | [[ThisValue]] field holds the this value at the time the      |
// |                    |                              | Reference Record was created.                                 |
// +--------------------+------------------------------+---------------------------------------------------------------+

#[derive(Debug)]
pub enum Base {
    Unresolvable,
    Environment(Rc<dyn EnvironmentRecord>),
    Value(ECMAScriptValue),
}

#[derive(Debug, PartialEq)]
pub enum ReferencedName {
    String(JSString),
    Symbol(Symbol),
    PrivateName(PrivateName),
}
impl<T> From<T> for ReferencedName
where
    T: Into<JSString>,
{
    fn from(val: T) -> Self {
        Self::String(val.into())
    }
}
impl From<Symbol> for ReferencedName {
    fn from(val: Symbol) -> Self {
        Self::Symbol(val)
    }
}
impl From<PrivateName> for ReferencedName {
    fn from(val: PrivateName) -> Self {
        Self::PrivateName(val)
    }
}
impl From<PropertyKey> for ReferencedName {
    fn from(val: PropertyKey) -> Self {
        match val {
            PropertyKey::String(st) => Self::String(st),
            PropertyKey::Symbol(sy) => Self::Symbol(sy),
        }
    }
}
impl TryFrom<ReferencedName> for PropertyKey {
    type Error = &'static str;
    fn try_from(rn: ReferencedName) -> Result<Self, Self::Error> {
        match rn {
            ReferencedName::String(s) => Ok(PropertyKey::from(s)),
            ReferencedName::Symbol(s) => Ok(PropertyKey::from(s)),
            ReferencedName::PrivateName(_) => Err("invalid property key"),
        }
    }
}
impl TryFrom<ReferencedName> for JSString {
    type Error = &'static str;
    fn try_from(rn: ReferencedName) -> Result<Self, Self::Error> {
        match rn {
            ReferencedName::String(s) => Ok(s),
            _ => Err("invalid string"),
        }
    }
}

#[derive(Debug)]
pub struct Reference {
    pub base: Base,
    pub referenced_name: ReferencedName,
    pub strict: bool,
    pub this_value: Option<ECMAScriptValue>,
}

impl Reference {
    pub fn new<T>(base: Base, key: T, strict: bool, this_value: Option<ECMAScriptValue>) -> Self
    where
        T: Into<ReferencedName>,
    {
        Reference { base, referenced_name: key.into(), strict, this_value }
    }

    // IsPropertyReference ( V )
    //
    // The abstract operation IsPropertyReference takes argument V (a Reference Record). It performs the following
    // steps when called:
    //
    //  1. If V.[[Base]] is unresolvable, return false.
    //  2. If V.[[Base]] is an Environment Record, return false; otherwise return true.
    pub fn is_property_reference(&self) -> bool {
        matches!(&self.base, Base::Value(_))
    }

    // IsUnresolvableReference ( V )
    //
    // The abstract operation IsUnresolvableReference takes argument V (a Reference Record). It performs the following
    // steps when called:
    //
    //  1. Assert: V is a Reference Record.
    //  2. If V.[[Base]] is unresolvable, return true; otherwise return false.
    pub fn is_unresolvable_reference(&self) -> bool {
        matches!(&self.base, Base::Unresolvable)
    }

    // IsSuperReference ( V )
    //
    // The abstract operation IsSuperReference takes argument V (a Reference Record). It performs the following steps
    // when called:
    //
    //  1. Assert: V is a Reference Record.
    //  2. If V.[[ThisValue]] is not empty, return true; otherwise return false.
    pub fn is_super_reference(&self) -> bool {
        self.this_value.is_some()
    }

    // IsPrivateReference ( V )
    //
    // The abstract operation IsPrivateReference takes argument V (a Reference Record). It performs the following steps
    // when called:
    //
    //  1. If V.[[ReferencedName]] is a Private Name, return true; otherwise return false.
    pub fn is_private_reference(&self) -> bool {
        matches!(&self.referenced_name, ReferencedName::PrivateName(_))
    }

    // GetThisValue ( V )
    //
    // The abstract operation GetThisValue takes argument V. It performs the following steps when called:
    //
    //  1. Assert: IsPropertyReference(V) is true.
    //  2. If IsSuperReference(V) is true, return V.[[ThisValue]]; otherwise return V.[[Base]].
    pub fn get_this_value(&self) -> ECMAScriptValue {
        match (&self.base, &self.this_value) {
            (Base::Value(_), Some(val)) | (Base::Value(val), None) => val.clone(),
            _ => unreachable!(),
        }
    }
}

// GetValue ( V )
//
// The abstract operation GetValue takes argument V. It performs the following steps when called:
//
//  1. ReturnIfAbrupt(V).
//  2. If V is not a Reference Record, return V.
//  3. If IsUnresolvableReference(V) is true, throw a ReferenceError exception.
//  4. If IsPropertyReference(V) is true, then
//      a. Let baseObj be ? ToObject(V.[[Base]]).
//      b. If IsPrivateReference(V) is true, then
//          i. Return ? PrivateGet(baseObj, V.[[ReferencedName]]).
//      c. Return ? baseObj.[[Get]](V.[[ReferencedName]], GetThisValue(V)).
//  5. Else,
//      a. Let base be V.[[Base]].
//      b. Assert: base is an Environment Record.
//      c. Return ? base.GetBindingValue(V.[[ReferencedName]], V.[[Strict]]) (see 9.1).
//
// NOTE     The object that may be created in step 4.a is not accessible outside of the above abstract operation and the
//          ordinary object [[Get]] internal method. An implementation might choose to avoid the actual creation of the
//          object.
#[derive(Debug)]
pub enum SuperValue {
    Value(ECMAScriptValue),
    Reference(Reference),
}
impl From<ECMAScriptValue> for SuperValue {
    fn from(src: ECMAScriptValue) -> Self {
        Self::Value(src)
    }
}
impl From<bool> for SuperValue {
    fn from(src: bool) -> Self {
        Self::Value(ECMAScriptValue::from(src))
    }
}
impl From<f64> for SuperValue {
    fn from(src: f64) -> Self {
        Self::Value(ECMAScriptValue::from(src))
    }
}
impl From<JSString> for SuperValue {
    fn from(src: JSString) -> Self {
        Self::Value(ECMAScriptValue::from(src))
    }
}
impl From<&JSString> for SuperValue {
    fn from(src: &JSString) -> Self {
        Self::Value(ECMAScriptValue::from(src))
    }
}
impl From<Rc<BigInt>> for SuperValue {
    fn from(src: Rc<BigInt>) -> Self {
        Self::Value(ECMAScriptValue::from(src))
    }
}
impl From<Reference> for SuperValue {
    fn from(src: Reference) -> Self {
        Self::Reference(src)
    }
}
pub fn get_value(agent: &mut Agent, v_completion: AltCompletion<SuperValue>) -> Completion {
    let v = v_completion?;
    match v {
        SuperValue::Value(val) => Ok(val),
        SuperValue::Reference(reference) => match &reference.base {
            Base::Value(val) => {
                let base_obj = to_object(agent, val.clone())?;
                match &reference.referenced_name {
                    ReferencedName::PrivateName(private) => private_get(agent, &base_obj, private),
                    _ => {
                        let this_value = reference.get_this_value();
                        base_obj.o.get(agent, &reference.referenced_name.try_into().unwrap(), &this_value)
                    }
                }
            }
            Base::Unresolvable => Err(create_reference_error(agent, "Unresolvable Reference")),
            Base::Environment(env) => env.get_binding_value(agent, &reference.referenced_name.try_into().unwrap(), reference.strict),
        },
    }
}

// PutValue ( V, W )
//
// The abstract operation PutValue takes arguments V and W. It performs the following steps when called:
//
//  1. ReturnIfAbrupt(V).
//  2. ReturnIfAbrupt(W).
//  3. If V is not a Reference Record, throw a ReferenceError exception.
//  4. If IsUnresolvableReference(V) is true, then
//      a. If V.[[Strict]] is true, throw a ReferenceError exception.
//      b. Let globalObj be GetGlobalObject().
//      c. Return ? Set(globalObj, V.[[ReferencedName]], W, false).
//  5. If IsPropertyReference(V) is true, then
//      a. Let baseObj be ? ToObject(V.[[Base]]).
//      b. If IsPrivateReference(V) is true, then
//          i. Return ? PrivateSet(baseObj, V.[[ReferencedName]], W).
//      c. Let succeeded be ? baseObj.[[Set]](V.[[ReferencedName]], W, GetThisValue(V)).
//      d. If succeeded is false and V.[[Strict]] is true, throw a TypeError exception.
//      e. Return.
//  6. Else,
//      a. Let base be V.[[Base]].
//      b. Assert: base is an Environment Record.
//      c. Return ? base.SetMutableBinding(V.[[ReferencedName]], W, V.[[Strict]]) (see 9.1).
//
// NOTE     The object that may be created in step 5.a is not accessible outside of the above abstract operation and the
//          ordinary object [[Set]] internal method. An implementation might choose to avoid the actual creation of that
//          object.
pub fn put_value(agent: &mut Agent, v_completion: AltCompletion<SuperValue>, w_completion: Completion) -> AltCompletion<()> {
    let v = v_completion?;
    let w = w_completion?;
    match v {
        SuperValue::Value(_) => Err(create_reference_error(agent, "Invalid Reference")),
        SuperValue::Reference(r) => match &r.base {
            Base::Unresolvable => {
                if r.strict {
                    Err(create_reference_error(agent, "Unknown reference"))
                } else {
                    let global_object = get_global_object(agent).unwrap();
                    set(agent, &global_object, r.referenced_name.try_into().unwrap(), w, false)?;
                    Ok(())
                }
            }
            Base::Value(val) => {
                let base_obj = to_object(agent, val.clone())?;
                match &r.referenced_name {
                    ReferencedName::PrivateName(pn) => private_set(agent, &base_obj, pn, w),
                    _ => {
                        let this_value = r.get_this_value();
                        let propkey_ref: &PropertyKey = &r.referenced_name.try_into().unwrap();
                        let succeeded = base_obj.o.set(agent, propkey_ref.clone(), w, &this_value)?;
                        if !succeeded && r.strict {
                            Err(create_type_error(agent, "Invalid Assignment Target"))
                        } else {
                            Ok(())
                        }
                    }
                }
            }
            Base::Environment(env) => env.set_mutable_binding(agent, r.referenced_name.try_into().unwrap(), w, r.strict),
        },
    }
}

// InitializeReferencedBinding ( V, W )
//
// The abstract operation InitializeReferencedBinding takes arguments V and W. It performs the following steps when
// called:
//
//  1. ReturnIfAbrupt(V).
//  2. ReturnIfAbrupt(W).
//  3. Assert: V is a Reference Record.
//  4. Assert: IsUnresolvableReference(V) is false.
//  5. Let base be V.[[Base]].
//  6. Assert: base is an Environment Record.
//  7. Return base.InitializeBinding(V.[[ReferencedName]], W).
pub fn initialize_referenced_binding(agent: &mut Agent, v_completion: AltCompletion<SuperValue>, w_completion: Completion) -> AltCompletion<()> {
    let v = v_completion?;
    let w = w_completion?;
    match v {
        SuperValue::Reference(reference) => match &reference.base {
            Base::Environment(base) => base.initialize_binding(agent, &reference.referenced_name.try_into().unwrap(), w),
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests;
