use super::*;
use anyhow::anyhow;
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::ptr::addr_of;
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
// | [[ReferencedName]] | an ECMAScript language value | The name of the binding. Always a String if [[Base]] value is |
// |                    | or a Private Name            | an Environment Record. Otherwise, may be an ECMAScript        |
// |                    |                              | language value other than a String or a Symbol until          |
// |                    |                              | ToPropertyKey is performed.                                   |
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

#[derive(Clone, Debug)]
pub enum Base {
    Unresolvable,
    Environment(Rc<dyn EnvironmentRecord>),
    Value(ECMAScriptValue),
}

impl fmt::Display for Base {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Base::Unresolvable => write!(f, "unresolvable"),
            Base::Value(v) => write!(f, "{v}"),
            Base::Environment(e) => write!(f, "{e:?}"),
        }
    }
}

impl PartialEq for Base {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Environment(left), Self::Environment(right)) => {
                //Rc::ptr_eq(left, right) <<-- Can't do this because fat pointers aren't comparable. Convert to thin pointers to the allocated memory instead.
                let left = addr_of!(**left).cast::<u8>();
                let right = addr_of!(**right).cast::<u8>();
                std::ptr::eq(left, right)
            }
            (Self::Value(left), Self::Value(right)) => left == right,
            (Self::Unresolvable, Self::Unresolvable) => true,
            _ => false,
        }
    }
}

impl TryFrom<Base> for ECMAScriptValue {
    type Error = anyhow::Error;
    fn try_from(src: Base) -> Result<Self, Self::Error> {
        match src {
            Base::Value(v) => Ok(v),
            _ => Err(anyhow!("Reference was not a Property Ref")),
        }
    }
}

impl TryFrom<Base> for Rc<dyn EnvironmentRecord> {
    type Error = anyhow::Error;
    fn try_from(src: Base) -> Result<Self, Self::Error> {
        match src {
            Base::Environment(x) => Ok(x),
            _ => Err(anyhow!("Reference was not an environment ref")),
        }
    }
}

impl From<Option<Object>> for Base {
    fn from(value: Option<Object>) -> Self {
        Base::Value(value.map_or(ECMAScriptValue::Undefined, ECMAScriptValue::Object))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ReferencedName {
    Value(ECMAScriptValue),
    PrivateName(PrivateName),
}
impl fmt::Display for ReferencedName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ReferencedName::Value(v) => write!(f, "{v}"),
            ReferencedName::PrivateName(p) => write!(f, "{p}"),
        }
    }
}
impl<T> From<T> for ReferencedName
where
    T: Into<ECMAScriptValue>,
{
    fn from(val: T) -> Self {
        Self::Value(val.into())
    }
}
impl From<PrivateName> for ReferencedName {
    fn from(val: PrivateName) -> Self {
        Self::PrivateName(val)
    }
}
impl TryFrom<ReferencedName> for PropertyKey {
    type Error = anyhow::Error;
    fn try_from(rn: ReferencedName) -> Result<Self, Self::Error> {
        match rn {
            ReferencedName::Value(v) => PropertyKey::try_from(v),
            ReferencedName::PrivateName(_) => Err(InternalRuntimeError::ImproperPropertyKey)?,
        }
    }
}
impl TryFrom<ReferencedName> for JSString {
    type Error = anyhow::Error;
    fn try_from(rn: ReferencedName) -> Result<Self, Self::Error> {
        match rn {
            ReferencedName::Value(v) => JSString::try_from(v),
            ReferencedName::PrivateName(_) => Err(InternalRuntimeError::StringExpected)?,
        }
    }
}
impl TryFrom<ReferencedName> for ECMAScriptValue {
    type Error = anyhow::Error;
    fn try_from(rn: ReferencedName) -> Result<Self, Self::Error> {
        match rn {
            ReferencedName::Value(v) => Ok(v),
            ReferencedName::PrivateName(_) => Err(InternalRuntimeError::ValueExpected)?,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
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

    pub fn make_super_property_reference(
        actual_this: ECMAScriptValue,
        property_key: ECMAScriptValue,
        strict: bool,
    ) -> anyhow::Result<Completion<Self>> {
        // MakeSuperPropertyReference ( actualThis, propertyKey, strict )
        // The abstract operation MakeSuperPropertyReference takes arguments actualThis (an ECMAScript language value),
        // propertyKey (an ECMAScript language value), and strict (a Boolean) and returns either a normal completion
        // containing a Super Reference Record or a throw completion. It performs the following steps when called:
        //
        //  1. Let env be GetThisEnvironment().
        //  2. Assert: env.HasSuperBinding() is true.
        //  3. Let baseValue be ? env.GetSuperBase().
        //  4. Return the Reference Record { [[Base]]: baseValue, [[ReferencedName]]: propertyKey, [[Strict]]: strict,
        //     [[ThisValue]]: actualThis }.
        Ok(get_this_environment()
            .as_function_environment_record()
            .ok_or(InternalRuntimeError::MissingSuperEnvironnment)?
            .get_super_base()
            .map(|base_value| Self::new(Base::from(base_value), property_key, strict, Some(actual_this))))
    }
}

// GetValue ( V )
//
// The abstract operation GetValue takes argument V. It performs the following steps when called:
//
//  1. If V is not a Reference Record, return V.
//  2. If IsUnresolvableReference(V) is true, throw a ReferenceError exception.
//  3. If IsPropertyReference(V) is true, then
//     a. Let baseObj be ? ToObject(V.[[Base]]).
//     b. If IsPrivateReference(V) is true, then
//        i. Return ? PrivateGet(baseObj, V.[[ReferencedName]]).
//     c. If V.[[ReferencedName]] is not a property key, then
//        i. Set V.[[ReferencedName]] to ? ToPropertyKey(V.[[ReferencedName]]).
//     d. Return ? baseObj.[[Get]](V.[[ReferencedName]], GetThisValue(V)).
//  4. Else,
//     a. Let base be V.[[Base]].
//     b. Assert: base is an Environment Record.
//     c. Return ? base.GetBindingValue(V.[[ReferencedName]], V.[[Strict]]) (see 9.1).
//
// NOTE     The object that may be created in step 4.a is not accessible outside of the above abstract operation and the
//          ordinary object [[Get]] internal method. An implementation might choose to avoid the actual creation of the
//          object.
pub fn get_value(v_completion: FullCompletion) -> Completion<ECMAScriptValue> {
    let v = v_completion?;
    match v {
        NormalCompletion::Value(val) => Ok(val),
        NormalCompletion::Reference(reference) => match &reference.base {
            Base::Value(val) => {
                let base_obj = to_object(val.clone())?;
                match &reference.referenced_name {
                    ReferencedName::PrivateName(private) => private_get(&base_obj, private),
                    ReferencedName::Value(value) => {
                        let this_value = reference.get_this_value();
                        let key = PropertyKey::try_from(value.clone()).or_else(|_| value.to_property_key())?;

                        base_obj.o.get(&key, &this_value)
                    }
                }
            }
            Base::Unresolvable => Err(create_reference_error("Unresolvable Reference")),
            Base::Environment(env) => {
                env.get_binding_value(&reference.referenced_name.try_into().unwrap(), reference.strict)
            }
        },
        NormalCompletion::Empty => Err(create_reference_error("Unresolvable Reference")),
        NormalCompletion::IteratorRecord(_)
        | NormalCompletion::Environment(_)
        | NormalCompletion::PrivateName(_)
        | NormalCompletion::PrivateElement(_)
        | NormalCompletion::ClassItem(_) => {
            panic!("Bad completion type for get_value: {v:?}")
        }
    }
}

// PutValue ( V, W )
//
// The abstract operation PutValue takes arguments V (a Reference Record or an ECMAScript language value) and W (an
// ECMAScript language value) and returns either a normal completion containing unused or an abrupt completion. It
// performs the following steps when called:
//
// 1. If V is not a Reference Record, throw a ReferenceError exception.
// 2. If IsUnresolvableReference(V) is true, then
//    a. If V.[[Strict]] is true, throw a ReferenceError exception.
//    b. Let globalObj be GetGlobalObject().
//    c. Perform ? Set(globalObj, V.[[ReferencedName]], W, false).
//    d. Return unused.
// 3. If IsPropertyReference(V) is true, then
//    a. Let baseObj be ? ToObject(V.[[Base]]).
//    b. If IsPrivateReference(V) is true, then
//       i. Return ? PrivateSet(baseObj, V.[[ReferencedName]], W).
//    c. If V.[[ReferencedName]] is not a property key, then
//       i. Set V.[[ReferencedName]] to ? ToPropertyKey(V.[[ReferencedName]]).
//    d. Let succeeded be ? baseObj.[[Set]](V.[[ReferencedName]], W, GetThisValue(V)).
//    e. If succeeded is false and V.[[Strict]] is true, throw a TypeError exception.
//    f. Return unused.
// 4. Else,
//    a. Let base be V.[[Base]].
//    b. Assert: base is an Environment Record.
//    c. Return ? base.SetMutableBinding(V.[[ReferencedName]], W, V.[[Strict]]) (see 9.1).
//
// NOTE     The object that may be created in step 3.a is not accessible outside of the above abstract operation and the
//          ordinary object [[Set]] internal method. An implementation might choose to avoid the actual creation of that
//          object.
pub fn put_value(v_completion: FullCompletion, w_completion: Completion<ECMAScriptValue>) -> Completion<()> {
    let v = v_completion?;
    let w = w_completion?;
    match v {
        NormalCompletion::Empty
        | NormalCompletion::IteratorRecord(_)
        | NormalCompletion::Environment(_)
        | NormalCompletion::PrivateName(_)
        | NormalCompletion::PrivateElement(_)
        | NormalCompletion::ClassItem(_) => {
            panic!("Bad completion type for put_value: ({v:?})")
        }
        NormalCompletion::Value(_) => Err(create_reference_error("Invalid Reference")),
        NormalCompletion::Reference(r) => match &r.base {
            Base::Unresolvable => {
                if r.strict {
                    Err(create_reference_error("Unknown reference"))
                } else {
                    let global_object = get_global_object().unwrap();
                    let key: PropertyKey = r.referenced_name.try_into().unwrap();
                    global_object.set(key, w, false)?;
                    Ok(())
                }
            }
            Base::Value(val) => {
                let base_obj = to_object(val.clone())?;
                match &r.referenced_name {
                    ReferencedName::PrivateName(pn) => private_set(&base_obj, pn, w),
                    ReferencedName::Value(v) => {
                        let this_value = r.get_this_value();
                        let propkey_ref: &PropertyKey = &v.clone().try_into().or_else(|_| v.to_property_key())?;
                        let succeeded = base_obj.o.set(propkey_ref.clone(), w, &this_value)?;
                        if !succeeded && r.strict {
                            Err(create_type_error("Invalid Assignment Target"))
                        } else {
                            Ok(())
                        }
                    }
                }
            }
            Base::Environment(env) => env.set_mutable_binding(r.referenced_name.try_into().unwrap(), w, r.strict),
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
pub fn initialize_referenced_binding(
    v_completion: FullCompletion,
    w_completion: Completion<ECMAScriptValue>,
) -> Completion<()> {
    let v = v_completion?;
    let w = w_completion?;
    match v {
        NormalCompletion::Reference(reference) => match &reference.base {
            Base::Environment(base) => base.initialize_binding(&reference.referenced_name.try_into().unwrap(), w),
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

pub fn make_private_reference(base_value: ECMAScriptValue, private_identifier: &JSString) -> anyhow::Result<Reference> {
    // MakePrivateReference ( baseValue, privateIdentifier )
    // The abstract operation MakePrivateReference takes arguments baseValue (an ECMAScript language value) and
    // privateIdentifier (a String) and returns a Reference Record. It performs the following steps when called:
    //
    //  1. Let privateEnv be the running execution context's PrivateEnvironment.
    //  2. Assert: privateEnv is not null.
    //  3. Let privateName be ResolvePrivateIdentifier(privateEnv, privateIdentifier).
    //  4. Return the Reference Record { [[Base]]: baseValue, [[ReferencedName]]: privateName, [[Strict]]: true,
    //     [[ThisValue]]: empty }.
    let private_env = current_private_environment().ok_or(InternalRuntimeError::NoPrivateEnv)?;
    let pn = private_env.borrow().resolve_private_identifier(private_identifier);
    Ok(Reference {
        base: Base::Value(base_value),
        referenced_name: ReferencedName::PrivateName(pn),
        strict: true,
        this_value: None,
    })
}

#[cfg(test)]
mod tests;
