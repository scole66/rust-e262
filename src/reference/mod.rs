use super::agent::Agent;
use super::cr::{AbruptCompletion, Completion};
use super::environment_record::EnvironmentRecord;
use super::errors::{create_reference_error, create_type_error};
use super::execution_context::get_global_object;
use super::object::set;
use super::values::{to_object, ECMAScriptValue, PropertyKey};
use std::convert::TryInto;
use std::rc::Rc;

// The Reference Record Specification Type
//
// The Reference Record type is used to explain the behaviour of such operators as delete, typeof, the assignment
// operators, the super keyword and other language features. For example, the left-hand operand of an assignment is
// expected to produce a Reference Record.
//
// A Reference Record is a resolved name or property binding; its fields are defined by Table 10.

// Table 10: Reference Record Fields
// +--------------------+------------------------------+---------------------------------------------------------------+
// | Field Name         | Value                        | Meaning                                                       |
// +--------------------+------------------------------+---------------------------------------------------------------+
// | [[Base]]           | One of:                      | The value or Environment Record which holds the binding. A    |
// |                    | any ECMAScript language      | [[Base]] of unresolvable indicates that the binding could not |
// |                    | value except undefined or    | be resolved.                                                  |
// |                    | null, an Environment Record, |                                                               |
// |                    | or unresolvable.             |                                                               |
// +--------------------+------------------------------+---------------------------------------------------------------+
// | [[ReferencedName]] | String or Symbol             | The name of the binding. Always a String if [[Base]] value is |
// |                    |                              | an Environment Record.                                        |
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

pub enum Base {
    Unresolvable,
    Environment(Rc<dyn EnvironmentRecord>),
    Value(ECMAScriptValue),
}

pub struct Reference {
    pub base: Base,
    pub referenced_name: PropertyKey,
    pub strict: bool,
    pub this_value: Option<ECMAScriptValue>,
}

impl Reference {
    pub fn new(base: Base, key: PropertyKey, strict: bool, this_value: Option<ECMAScriptValue>) -> Self {
        Reference { base, referenced_name: key, strict, this_value }
    }
    pub fn property_base(&self) -> Option<ECMAScriptValue> {
        // Get the base of the reference, when is_property_reference is true.
        if let Base::Value(b) = &self.base {
            Some(b.clone())
        } else {
            None
        }
    }
}

// IsPropertyReference ( V )
//
// The abstract operation IsPropertyReference takes argument V. It performs the following steps when called:
//
//  1. Assert: V is a Reference Record.
//  2. If V.[[Base]] is unresolvable, return false.
//  3. If Type(V.[[Base]]) is Boolean, String, Symbol, BigInt, Number, or Object, return true; otherwise return false.
pub fn is_property_reference(v: &Reference) -> bool {
    match &v.base {
        Base::Unresolvable => false,
        Base::Environment(_) => false,
        Base::Value(val) => val.is_boolean() || val.is_string() || val.is_symbol() || val.is_bigint() || val.is_number() || val.is_object(),
    }
}

// IsUnresolvableReference ( V )
//
// The abstract operation IsUnresolvableReference takes argument V. It performs the following steps when called:
//
//  1. Assert: V is a Reference Record.
//  2. If V.[[Base]] is unresolvable, return true; otherwise return false.
pub fn is_unresolvable_reference(v: &Reference) -> bool {
    matches!(&v.base, Base::Unresolvable)
}

// IsSuperReference ( V )
//
// The abstract operation IsSuperReference takes argument V. It performs the following steps when called:
//
//  1. Assert: V is a Reference Record.
//  2. If V.[[ThisValue]] is not empty, return true; otherwise return false.
pub fn is_super_reference(v: &Reference) -> bool {
    v.this_value.is_some()
}

// GetValue ( V )
//
// The abstract operation GetValue takes argument V. It performs the following steps when called:
//
//  1. ReturnIfAbrupt(V).
//  2. If V is not a Reference Record, return V.
//  3. If IsUnresolvableReference(V) is true, throw a ReferenceError exception.
//  4. If IsPropertyReference(V) is true, then
//      a. Let baseObj be ! ToObject(V.[[Base]]).
//      b. Return ? baseObj.[[Get]](V.[[ReferencedName]], GetThisValue(V)).
//  5. Else,
//      a. Let base be V.[[Base]].
//      b. Assert: base is an Environment Record.
//      c. Return ? base.GetBindingValue(V.[[ReferencedName]], V.[[Strict]]) (see 9.1).
//
// NOTE     The object that may be created in step 4.a is not accessible outside of the above abstract operation and the
//          ordinary object [[Get]] internal method. An implementation might choose to avoid the actual creation of the
//          object.
pub enum SuperValue {
    Value(ECMAScriptValue),
    Reference(Reference),
}
pub fn get_value(agent: &mut Agent, v_completion: Result<SuperValue, AbruptCompletion>) -> Completion {
    let v = v_completion?;
    match v {
        SuperValue::Value(val) => Ok(val),
        SuperValue::Reference(reference) => match &reference.base {
            Base::Value(val) => {
                let base_obj = to_object(agent, val.clone()).unwrap();
                base_obj.o.get(agent, &reference.referenced_name, &get_this_value(&reference))
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
//      a. Let baseObj be ! ToObject(V.[[Base]]).
//      b. Let succeeded be ? baseObj.[[Set]](V.[[ReferencedName]], W, GetThisValue(V)).
//      c. If succeeded is false and V.[[Strict]] is true, throw a TypeError exception.
//      d. Return.
//  6. Else,
//      a. Let base be V.[[Base]].
//      b. Assert: base is an Environment Record.
//      c. Return ? base.SetMutableBinding(V.[[ReferencedName]], W, V.[[Strict]]) (see 9.1).
//
// NOTE     The object that may be created in step 5.a is not accessible outside of the above abstract operation and the
//          ordinary object [[Set]] internal method. An implementation might choose to avoid the actual creation of that
//          object.
pub fn put_value(agent: &mut Agent, v_completion: Result<SuperValue, AbruptCompletion>, w_completion: Completion) -> Result<(), AbruptCompletion> {
    let v = v_completion?;
    let w = w_completion?;
    match v {
        SuperValue::Value(_) => Err(create_reference_error(agent, "Invalid Reference")),
        SuperValue::Reference(r) => match &r.base {
            Base::Unresolvable => {
                if r.strict {
                    Err(create_reference_error(agent, "Unknown reference"))
                } else {
                    let global_object = get_global_object(agent);
                    set(agent, &global_object, &r.referenced_name, &w, false)?;
                    Ok(())
                }
            }
            Base::Value(val) => {
                let base_obj = to_object(agent, val.clone()).unwrap();
                let succeeded = base_obj.o.set(agent, &r.referenced_name, &w, &get_this_value(&r))?;
                if !succeeded && r.strict {
                    Err(create_type_error(agent, "Invalid Assignment Target"))
                } else {
                    Ok(())
                }
            }
            Base::Environment(env) => env.set_mutable_binding(agent, r.referenced_name.try_into().unwrap(), w, r.strict),
        },
    }
}

// GetThisValue ( V )
//
// The abstract operation GetThisValue takes argument V. It performs the following steps when called:
//
//  1. Assert: IsPropertyReference(V) is true.
//  2. If IsSuperReference(V) is true, return V.[[ThisValue]]; otherwise return V.[[Base]].
pub fn get_this_value(v: &Reference) -> ECMAScriptValue {
    match &v.this_value {
        Some(val) => val.clone(),
        None => match &v.base {
            Base::Value(val) => val.clone(),
            _ => unreachable!(),
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
pub fn initialize_referenced_binding(agent: &mut Agent, v_completion: Result<SuperValue, AbruptCompletion>, w_completion: Completion) -> Result<(), AbruptCompletion> {
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
