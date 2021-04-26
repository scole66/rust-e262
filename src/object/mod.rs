use super::agent::Agent;
use super::boolean_object::{BooleanObject, BooleanObjectInterface};
use super::comparison::is_extensible;
use super::cr::{AltCompletion, Completion};
use super::errors::create_type_error;
use super::errors::ErrorObject;
use super::function_object::CallableObject;
use super::realm::{get_function_realm, IntrinsicIdentifier};
use super::values::{is_callable, to_object, ECMAScriptValue, PropertyKey};
use ahash::{AHashMap, AHashSet};
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub struct DataProperty {
    pub value: ECMAScriptValue,
    pub writable: bool,
}

#[derive(Clone)]
pub struct AccessorProperty {
    pub get: ECMAScriptValue,
    pub set: ECMAScriptValue,
}

#[derive(Clone)]
pub enum PropertyKind {
    Data(DataProperty),
    Accessor(AccessorProperty),
}

#[derive(Clone)]
pub struct PropertyDescriptor {
    pub property: PropertyKind,
    pub enumerable: bool,
    pub configurable: bool,
    spot: usize,
}

pub trait DescriptorKind {
    fn is_data_descriptor(&self) -> bool;
    fn is_accessor_descriptor(&self) -> bool;
    fn is_generic_descriptor(&self) -> bool;
    fn writable(&self) -> Option<bool>;
}

impl DescriptorKind for PropertyDescriptor {
    fn is_data_descriptor(&self) -> bool {
        match self.property {
            PropertyKind::Accessor(_) => false,
            PropertyKind::Data(_) => true,
        }
    }
    fn is_accessor_descriptor(&self) -> bool {
        match self.property {
            PropertyKind::Accessor(_) => true,
            PropertyKind::Data(_) => false,
        }
    }
    fn is_generic_descriptor(&self) -> bool {
        false
    }
    fn writable(&self) -> Option<bool> {
        match &self.property {
            PropertyKind::Data(d) => Some(d.writable),
            PropertyKind::Accessor(_) => None,
        }
    }
}

#[derive(Default)]
pub struct PotentialPropertyDescriptor {
    pub value: Option<ECMAScriptValue>,
    pub writable: Option<bool>,
    pub get: Option<ECMAScriptValue>,
    pub set: Option<ECMAScriptValue>,
    pub enumerable: Option<bool>,
    pub configurable: Option<bool>,
}

impl DescriptorKind for PotentialPropertyDescriptor {
    fn is_accessor_descriptor(&self) -> bool {
        !(self.get.is_none() && self.set.is_none())
    }
    fn is_data_descriptor(&self) -> bool {
        !(self.value.is_none() && self.writable.is_none())
    }
    fn is_generic_descriptor(&self) -> bool {
        !self.is_accessor_descriptor() && !self.is_data_descriptor()
    }
    fn writable(&self) -> Option<bool> {
        self.writable
    }
}

// IsAccessorDescriptor ( Desc )
//
// The abstract operation IsAccessorDescriptor takes argument Desc (a Property Descriptor or undefined). It performs the
// following steps when called:
//
//  1. If Desc is undefined, return false.
//  2. If both Desc.[[Get]] and Desc.[[Set]] are absent, return false.
//  3. Return true.
fn is_accessor_descriptor<T>(desc: &T) -> bool
where
    T: DescriptorKind,
{
    desc.is_accessor_descriptor()
}

// IsDataDescriptor ( Desc )
//
// The abstract operation IsDataDescriptor takes argument Desc (a Property Descriptor or undefined). It performs the
// following steps when called:
//
//  1. If Desc is undefined, return false.
//  2. If both Desc.[[Value]] and Desc.[[Writable]] are absent, return false.
//  3. Return true.
fn is_data_descriptor<T>(desc: &T) -> bool
where
    T: DescriptorKind,
{
    desc.is_data_descriptor()
}

// IsGenericDescriptor ( Desc )
//
// The abstract operation IsGenericDescriptor takes argument Desc (a Property Descriptor or undefined). It performs the
// following steps when called:
//
//  1. If Desc is undefined, return false.
//  2. If IsAccessorDescriptor(Desc) and IsDataDescriptor(Desc) are both false, return true.
//  3. Return false.
fn is_generic_descriptor<T>(desc: &T) -> bool
where
    T: DescriptorKind,
{
    desc.is_generic_descriptor()
}

// OrdinaryGetPrototypeOf ( O )
//
// The abstract operation OrdinaryGetPrototypeOf takes argument O (an Object). It performs the following steps when
// called:
//
//  1. Return O.[[Prototype]].
pub fn ordinary_get_prototype_of<'a, T>(o: T) -> Option<Object>
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let cod_ref = obj.common_object_data().borrow();
    cod_ref.prototype.clone()
}

// OrdinarySetPrototypeOf ( O, V )
//
// The abstract operation OrdinarySetPrototypeOf takes arguments O (an Object) and V (an ECMAScript language value). It
// performs the following steps when called:
//
//  1. Assert: Either Type(V) is Object or Type(V) is Null.
//  2. Let current be O.[[Prototype]].
//  3. If SameValue(V, current) is true, return true.
//  4. Let extensible be O.[[Extensible]].
//  5. If extensible is false, return false.
//  6. Let p be V.
//  7. Let done be false.
//  8. Repeat, while done is false,
//      a. If p is null, set done to true.
//      b. Else if SameValue(p, O) is true, return false.
//      c. Else,
//          i. If p.[[GetPrototypeOf]] is not the ordinary object internal method defined in 10.1.1, set done to true.
//          ii. Else, set p to p.[[Prototype]].
//  9. Set O.[[Prototype]] to V.
//  10. Return true.
//
// NOTE     The loop in step 8 guarantees that there will be no circularities in any prototype chain that only includes
//          objects that use the ordinary object definitions for [[GetPrototypeOf]] and [[SetPrototypeOf]].
pub fn ordinary_set_prototype_of<'a, T>(o: T, val: Option<&Object>) -> bool
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let current = obj.common_object_data().borrow().prototype.clone();
    match (current, val) {
        (None, None) => {
            return true;
        }
        (Some(left), Some(right)) => {
            if left.o.id() == right.o.id() {
                return true;
            }
        }
        _ => {}
    }
    let extensible = obj.common_object_data().borrow().extensible;
    if !extensible {
        return false;
    }
    let mut p = val.map(Object::clone);
    while let Some(pp) = p {
        if pp.o.id() == obj.id() {
            return false;
        } else if !pp.o.is_ordinary() {
            break;
        } else {
            p = pp.o.common_object_data().borrow().prototype.clone();
        }
    }
    obj.common_object_data().borrow_mut().prototype = val.map(Object::clone);
    true
}

// OrdinaryIsExtensible ( O )
//
// The abstract operation OrdinaryIsExtensible takes argument O (an Object). It performs the following steps when
// called:
//
//  1. Return O.[[Extensible]].
pub fn ordinary_is_extensible<'a, T>(o: T) -> bool
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let cod_ref = obj.common_object_data().borrow();
    cod_ref.extensible
}

// OrdinaryPreventExtensions ( O )
//
// The abstract operation OrdinaryPreventExtensions takes argument O (an Object). It performs the following steps when
// called:
//
//  1. Set O.[[Extensible]] to false.
//  2. Return true.
pub fn ordinary_prevent_extensions<'a, T>(o: T) -> bool
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let mut cod_ref = obj.common_object_data().borrow_mut();
    cod_ref.extensible = false;
    true
}

// OrdinaryGetOwnProperty ( O, P )
//
// The abstract operation OrdinaryGetOwnProperty takes arguments O (an Object) and P (a property key). It performs the
// following steps when called:
//
//  1. Assert: IsPropertyKey(P) is true.
//  2. If O does not have an own property with key P, return undefined.
//  3. Let D be a newly created Property Descriptor with no fields.
//  4. Let X be O's own property whose key is P.
//  5. If X is a data property, then
//      a. Set D.[[Value]] to the value of X's [[Value]] attribute.
//      b. Set D.[[Writable]] to the value of X's [[Writable]] attribute.
//  6. Else,
//      a. Assert: X is an accessor property.
//      b. Set D.[[Get]] to the value of X's [[Get]] attribute.
//      c. Set D.[[Set]] to the value of X's [[Set]] attribute.
//  7. Set D.[[Enumerable]] to the value of X's [[Enumerable]] attribute.
//  8. Set D.[[Configurable]] to the value of X's [[Configurable]] attribute.
//  9. Return D.
pub fn ordinary_get_own_property<'a, T>(o: T, key: &PropertyKey) -> Option<PropertyDescriptor>
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let object_info = obj.common_object_data().borrow();
    object_info.properties.get(key).cloned()
}

// OrdinaryDefineOwnProperty ( O, P, Desc )
//
// The abstract operation OrdinaryDefineOwnProperty takes arguments O (an Object), P (a property key), and Desc (a
// Property Descriptor). It performs the following steps when called:
//
//  1. Let current be ? O.[[GetOwnProperty]](P).
//  2. Let extensible be ? IsExtensible(O).
//  3. Return ValidateAndApplyPropertyDescriptor(O, P, extensible, Desc, current).
pub fn ordinary_define_own_property<'a, T>(o: T, p: &PropertyKey, desc: &PotentialPropertyDescriptor) -> AltCompletion<bool>
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let current = obj.get_own_property(&p)?;
    let extensible = is_extensible(obj)?;
    Ok(validate_and_apply_property_descriptor(Some(obj), Some(p), extensible, desc, current.as_ref()))
}

// ValidateAndApplyPropertyDescriptor ( O, P, extensible, Desc, current )
//
// The abstract operation ValidateAndApplyPropertyDescriptor takes arguments O (an Object or undefined), P (a property
// key), extensible (a Boolean), Desc (a Property Descriptor), and current (a Property Descriptor). It performs the
// following steps when called:
//
// NOTE     If undefined is passed as O, only validation is performed and no object updates are performed.
//
//  1. Assert: If O is not undefined, then IsPropertyKey(P) is true.
//  2. If current is undefined, then
//      a. If extensible is false, return false.
//      b. Assert: extensible is true.
//      c. If IsGenericDescriptor(Desc) is true or IsDataDescriptor(Desc) is true, then
//          i. If O is not undefined, create an own data property named P of object O whose [[Value]], [[Writable]],
//             [[Enumerable]], and [[Configurable]] attribute values are described by Desc. If the value of an attribute
//             field of Desc is absent, the attribute of the newly created property is set to its default value.
//      d. Else,
//          i. Assert: ! IsAccessorDescriptor(Desc) is true.
//          ii. If O is not undefined, create an own accessor property named P of object O whose [[Get]], [[Set]],
//              [[Enumerable]], and [[Configurable]] attribute values are described by Desc. If the value of an
//              attribute field of Desc is absent, the attribute of the newly created property is set to its default
//              value.
//      e. Return true.
//  3. If every field in Desc is absent, return true.
//  4. If current.[[Configurable]] is false, then
//      a. If Desc.[[Configurable]] is present and its value is true, return false.
//      b. If Desc.[[Enumerable]] is present and ! SameValue(Desc.[[Enumerable]], current.[[Enumerable]]) is false,
//         return false.
//  5. If ! IsGenericDescriptor(Desc) is true, then
//      a. NOTE: No further validation is required.
//  6. Else if ! SameValue(! IsDataDescriptor(current), ! IsDataDescriptor(Desc)) is false, then
//      a. If current.[[Configurable]] is false, return false.
//      b. If IsDataDescriptor(current) is true, then
//          i. If O is not undefined, convert the property named P of object O from a data property to an accessor
//             property. Preserve the existing values of the converted property's [[Configurable]] and [[Enumerable]]
//             attributes and set the rest of the property's attributes to their default values.
//      c. Else,
//          i. If O is not undefined, convert the property named P of object O from an accessor property to a data
//             property. Preserve the existing values of the converted property's [[Configurable]] and [[Enumerable]]
//             attributes and set the rest of the property's attributes to their default values.
//  7. Else if IsDataDescriptor(current) and IsDataDescriptor(Desc) are both true, then
//      a. If current.[[Configurable]] is false and current.[[Writable]] is false, then
//          i. If Desc.[[Writable]] is present and Desc.[[Writable]] is true, return false.
//          ii. If Desc.[[Value]] is present and SameValue(Desc.[[Value]], current.[[Value]]) is false, return false.
//          iii. Return true.
//  8. Else,
//      a. Assert: ! IsAccessorDescriptor(current) and ! IsAccessorDescriptor(Desc) are both true.
//      b. If current.[[Configurable]] is false, then
//          i. If Desc.[[Set]] is present and SameValue(Desc.[[Set]], current.[[Set]]) is false, return false.
//          ii. If Desc.[[Get]] is present and SameValue(Desc.[[Get]], current.[[Get]]) is false, return false.
//          iii. Return true.
//  9. If O is not undefined, then
//      a. For each field of Desc that is present, set the corresponding attribute of the property named P of object O
//         to the value of the field.
//  10. Return true.
fn validate_and_apply_property_descriptor<'a, T>(oo: Option<T>, p: Option<&PropertyKey>, extensible: bool, desc: &PotentialPropertyDescriptor, current: Option<&PropertyDescriptor>) -> bool
where
    T: Into<&'a dyn ObjectInterface>,
{
    match current {
        None => {
            if !extensible {
                false
            } else {
                if let Some(o) = oo {
                    let mut data = o.into().common_object_data().borrow_mut();
                    let property_descriptor = PropertyDescriptor {
                        enumerable: desc.enumerable.unwrap_or(false),
                        configurable: desc.configurable.unwrap_or(false),
                        property: if is_generic_descriptor(desc) || is_data_descriptor(desc) {
                            PropertyKind::Data(DataProperty { value: desc.value.clone().unwrap_or(ECMAScriptValue::Undefined), writable: desc.writable.unwrap_or(false) })
                        } else {
                            PropertyKind::Accessor(AccessorProperty {
                                get: desc.get.clone().unwrap_or(ECMAScriptValue::Undefined),
                                set: desc.set.clone().unwrap_or(ECMAScriptValue::Undefined),
                            })
                        },
                        spot: data.next_spot,
                    };
                    data.properties.insert(p.cloned().unwrap(), property_descriptor);
                    data.next_spot += 1;
                }
                true
            }
        }
        Some(cur) => {
            if desc.configurable.is_none() && desc.enumerable.is_none() && desc.get.is_none() && desc.set.is_none() && desc.value.is_none() && desc.writable.is_none() {
                true
            } else if !cur.configurable
                && (desc.configurable.unwrap_or(false) || desc.enumerable.unwrap_or(cur.enumerable) != cur.enumerable || cur.is_data_descriptor() != desc.is_data_descriptor())
            {
                false
            } else {
                if is_generic_descriptor(desc) {
                    // Step 5
                    // No further validation required
                } else if !cur.configurable && cur.is_data_descriptor() == desc.is_data_descriptor() {
                    match &cur.property {
                        PropertyKind::Data(data_fields) => {
                            // Step 7
                            if !data_fields.writable {
                                if desc.writable.unwrap_or(false) {
                                    return false;
                                }
                                if let Some(val) = desc.value.clone() {
                                    if val != data_fields.value {
                                        return false;
                                    }
                                }
                                return true;
                            }
                        }
                        PropertyKind::Accessor(acc_fields) => {
                            // Step 8
                            if let Some(getter) = desc.get.clone() {
                                if getter != acc_fields.get {
                                    return false;
                                }
                            }
                            if let Some(setter) = desc.set.clone() {
                                if setter != acc_fields.set {
                                    return false;
                                }
                            }
                            return true;
                        }
                    }
                }
                if let Some(o) = oo {
                    let mut data = o.into().common_object_data().borrow_mut();
                    let mut pd = data.properties.get_mut(&p.unwrap()).unwrap();
                    if let Some(configurable) = desc.configurable {
                        pd.configurable = configurable;
                    }
                    if let Some(enumerable) = desc.enumerable {
                        pd.enumerable = enumerable;
                    }
                    if cur.is_data_descriptor() && desc.is_accessor_descriptor() {
                        pd.property = PropertyKind::Accessor(AccessorProperty {
                            get: desc.get.clone().unwrap_or(ECMAScriptValue::Undefined),
                            set: desc.set.clone().unwrap_or(ECMAScriptValue::Undefined),
                        });
                    } else if cur.is_accessor_descriptor() && desc.is_data_descriptor() {
                        pd.property = PropertyKind::Data(DataProperty { writable: desc.writable.unwrap_or(false), value: desc.value.clone().unwrap_or(ECMAScriptValue::Undefined) });
                    } else {
                        match &mut pd.property {
                            PropertyKind::Accessor(acc_methods) => {
                                if let Some(setter) = desc.set.clone() {
                                    acc_methods.set = setter;
                                }
                                if let Some(getter) = desc.get.clone() {
                                    acc_methods.get = getter;
                                }
                            }
                            PropertyKind::Data(data_fields) => {
                                if let Some(value) = desc.value.clone() {
                                    data_fields.value = value;
                                }
                                if let Some(writable) = desc.writable {
                                    data_fields.writable = writable;
                                }
                            }
                        }
                    }
                }
                true
            }
        }
    }
}

// OrdinaryHasProperty ( O, P )
//
// The abstract operation OrdinaryHasProperty takes arguments O (an Object) and P (a property key). It performs the
// following steps when called:
//
//  1. Assert: IsPropertyKey(P) is true.
//  2. Let hasOwn be ? O.[[GetOwnProperty]](P).
//  3. If hasOwn is not undefined, return true.
//  4. Let parent be ? O.[[GetPrototypeOf]]().
//  5. If parent is not null, then
//      a. Return ? parent.[[HasProperty]](P).
//  6. Return false.
pub fn ordinary_has_property<'a, T>(o: T, p: &PropertyKey) -> AltCompletion<bool>
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let has_own = obj.get_own_property(p)?;
    match has_own {
        Some(_) => Ok(true),
        None => {
            let pot_parent = obj.get_prototype_of()?;
            match pot_parent {
                Some(parent) => parent.o.has_property(p),
                None => Ok(false),
            }
        }
    }
}

// OrdinaryGet ( O, P, Receiver )
//
// The abstract operation OrdinaryGet takes arguments O (an Object), P (a property key), and Receiver (an ECMAScript
// language value). It performs the following steps when called:
//
//  1. Assert: IsPropertyKey(P) is true.
//  2. Let desc be ? O.[[GetOwnProperty]](P).
//  3. If desc is undefined, then
//      a. Let parent be ? O.[[GetPrototypeOf]]().
//      b. If parent is null, return undefined.
//      c. Return ? parent.[[Get]](P, Receiver).
//  4. If IsDataDescriptor(desc) is true, return desc.[[Value]].
//  5. Assert: IsAccessorDescriptor(desc) is true.
//  6. Let getter be desc.[[Get]].
//  7. If getter is undefined, return undefined.
//  8. Return ? Call(getter, Receiver).
pub fn ordinary_get<'a, T>(o: T, agent: &mut Agent, p: &PropertyKey, receiver: &ECMAScriptValue) -> Completion
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let pot_desc = obj.get_own_property(p)?;
    match pot_desc {
        None => {
            let pot_parent = obj.get_prototype_of()?;
            match pot_parent {
                None => Ok(ECMAScriptValue::Undefined),
                Some(parent) => parent.o.get(agent, p, receiver),
            }
        }
        Some(desc) => match desc.property {
            PropertyKind::Data(data_fields) => Ok(data_fields.value),
            PropertyKind::Accessor(acc_methods) => {
                let pot_getter = acc_methods.get;
                match pot_getter {
                    ECMAScriptValue::Undefined => Ok(ECMAScriptValue::Undefined),
                    getter => call(agent, &getter, &receiver, &[]),
                }
            }
        },
    }
}

// OrdinarySet ( O, P, V, Receiver )
//
// The abstract operation OrdinarySet takes arguments O (an Object), P (a property key), V (an ECMAScript language
// value), and Receiver (an ECMAScript language value). It performs the following steps when called:
//
//  1. Assert: IsPropertyKey(P) is true.
//  2. Let ownDesc be ? O.[[GetOwnProperty]](P).
//  3. Return OrdinarySetWithOwnDescriptor(O, P, V, Receiver, ownDesc).
pub fn ordinary_set<'a, T>(o: T, agent: &mut Agent, p: &PropertyKey, v: &ECMAScriptValue, receiver: &ECMAScriptValue) -> AltCompletion<bool>
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let own_desc = obj.get_own_property(p)?;
    ordinary_set_with_own_descriptor(obj, agent, p, v, receiver, own_desc)
}

// OrdinarySetWithOwnDescriptor ( O, P, V, Receiver, ownDesc )
//
// The abstract operation OrdinarySetWithOwnDescriptor takes arguments O (an Object), P (a property key), V (an
// ECMAScript language value), Receiver (an ECMAScript language value), and ownDesc (a Property Descriptor or
// undefined). It performs the following steps when called:
//
//  1. Assert: IsPropertyKey(P) is true.
//  2. If ownDesc is undefined, then
//      a. Let parent be ? O.[[GetPrototypeOf]]().
//      b. If parent is not null, then
//          i. Return ? parent.[[Set]](P, V, Receiver).
//      c. Else,
//          i. Set ownDesc to the PropertyDescriptor { [[Value]]: undefined, [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true }.
//  3. If IsDataDescriptor(ownDesc) is true, then
//      a. If ownDesc.[[Writable]] is false, return false.
//      b. If Type(Receiver) is not Object, return false.
//      c. Let existingDescriptor be ? Receiver.[[GetOwnProperty]](P).
//      d. If existingDescriptor is not undefined, then
//          i. If IsAccessorDescriptor(existingDescriptor) is true, return false.
//          ii. If existingDescriptor.[[Writable]] is false, return false.
//          iii. Let valueDesc be the PropertyDescriptor { [[Value]]: V }.
//          iv. Return ? Receiver.[[DefineOwnProperty]](P, valueDesc).
//      e. Else,
//          i. Assert: Receiver does not currently have a property P.
//          ii. Return ? CreateDataProperty(Receiver, P, V).
//  4. Assert: IsAccessorDescriptor(ownDesc) is true.
//  5. Let setter be ownDesc.[[Set]].
//  6. If setter is undefined, return false.
//  7. Perform ? Call(setter, Receiver, « V »).
//  8. Return true.
pub fn ordinary_set_with_own_descriptor<'a, T>(
    o: T,
    agent: &mut Agent,
    p: &PropertyKey,
    v: &ECMAScriptValue,
    receiver: &ECMAScriptValue,
    pot_own_desc: Option<PropertyDescriptor>,
) -> AltCompletion<bool>
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let own_desc = match pot_own_desc {
        None => {
            let pot_parent = obj.get_prototype_of()?;
            match pot_parent {
                Some(parent) => {
                    return parent.o.set(agent, p, v, receiver);
                }
                None => {
                    PropertyDescriptor { configurable: true, enumerable: true, property: PropertyKind::Data(DataProperty { value: ECMAScriptValue::Undefined, writable: true }), spot: 0 }
                }
            }
        }
        Some(x) => x,
    };
    match &own_desc.property {
        PropertyKind::Data(data_fields) => match data_fields.writable {
            false => Ok(false),
            true => match receiver {
                ECMAScriptValue::Object(receiver) => {
                    let maybe_existing_descriptor = receiver.o.get_own_property(p)?;
                    match maybe_existing_descriptor {
                        Some(existing_descriptor) => match &existing_descriptor.property {
                            PropertyKind::Accessor(_) => Ok(false),
                            PropertyKind::Data(existing_data_fields) => match existing_data_fields.writable {
                                false => Ok(false),
                                true => {
                                    let value_desc = PotentialPropertyDescriptor { value: Some(v.clone()), ..Default::default() };
                                    receiver.o.define_own_property(p, &value_desc)
                                }
                            },
                        },
                        None => create_data_property(receiver, p, v),
                    }
                }
                _ => Ok(false),
            },
        },
        PropertyKind::Accessor(acc_methods) => {
            let setter = acc_methods.set.clone();
            match setter {
                ECMAScriptValue::Undefined => Ok(false),
                setter => {
                    call(agent, &setter, receiver, &[v.clone()])?;
                    Ok(true)
                }
            }
        }
    }
}

// OrdinaryDelete ( O, P )
//
// The abstract operation OrdinaryDelete takes arguments O (an Object) and P (a property key). It performs the following
// steps when called:
//
//  1. Assert: IsPropertyKey(P) is true.
//  2. Let desc be ? O.[[GetOwnProperty]](P).
//  3. If desc is undefined, return true.
//  4. If desc.[[Configurable]] is true, then
//      a. Remove the own property with name P from O.
//      b. Return true.
//  5. Return false.
pub fn ordinary_delete<'a, T>(o: T, p: &PropertyKey) -> AltCompletion<bool>
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let desc = obj.get_own_property(p)?;
    match desc {
        None => Ok(true),
        Some(desc) => match desc.configurable {
            true => {
                obj.common_object_data().borrow_mut().properties.remove(p);
                Ok(true)
            }
            false => Ok(false),
        },
    }
}

// OrdinaryOwnPropertyKeys ( O )
//
// The abstract operation OrdinaryOwnPropertyKeys takes argument O (an Object). It performs the following steps when
// called:
//
//  1. Let keys be a new empty List.
//  2. For each own property key P of O such that P is an array index, in ascending numeric index order, do
//      a. Add P as the last element of keys.
//  3. For each own property key P of O such that Type(P) is String and P is not an array index, in ascending chronological order of property creation, do
//      a. Add P as the last element of keys.
//  4. For each own property key P of O such that Type(P) is Symbol, in ascending chronological order of property creation, do
//      a. Add P as the last element of keys.
//  5. Return keys.
pub fn ordinary_own_property_keys<'a, T>(o: T) -> Vec<PropertyKey>
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let data = obj.common_object_data().borrow();
    let mut keys: Vec<PropertyKey> = Vec::with_capacity(data.properties.len());
    let mut norm_keys: Vec<(PropertyKey, usize)> = Vec::new();
    let mut symb_keys: Vec<(PropertyKey, usize)> = Vec::new();
    for (key, desc) in data.properties.iter() {
        if key.is_array_index() {
            keys.push(key.clone())
        } else {
            match key {
                PropertyKey::String(_) => {
                    norm_keys.push((key.clone(), desc.spot));
                }
                PropertyKey::Symbol(_) => {
                    symb_keys.push((key.clone(), desc.spot));
                }
            }
        }
    }
    keys.sort_by_cached_key(array_index_compare);
    norm_keys.sort_by_key(|x| x.1);
    symb_keys.sort_by_key(|x| x.1);
    for item in norm_keys.iter() {
        keys.push(item.0.clone());
    }
    for item in symb_keys.iter() {
        keys.push(item.0.clone());
    }
    keys
}
fn array_index_compare(item: &PropertyKey) -> u32 {
    match item {
        PropertyKey::String(s) => String::from_utf16_lossy(s.as_slice()).parse::<u32>().unwrap(),
        PropertyKey::Symbol(_) => unreachable!(),
    }
}

pub trait ObjectInterface {
    fn common_object_data(&self) -> &RefCell<CommonObjectData>;
    fn is_ordinary(&self) -> bool; // True if implements ordinary defintions of Get/SetPrototypeOf
    fn id(&self) -> usize; // Unique object id. Used for object "is_same" detection.
    fn to_boolean_obj(&self) -> Option<&dyn BooleanObjectInterface> {
        None
    }
    fn to_error_obj(&self) -> Option<&dyn ObjectInterface> {
        None
    }
    fn to_function_obj(&self) -> Option<&dyn CallableObject> {
        None
    }

    fn get_prototype_of(&self) -> AltCompletion<Option<Object>>;
    fn set_prototype_of(&self, obj: Option<&Object>) -> AltCompletion<bool>;
    fn is_extensible(&self) -> AltCompletion<bool>;
    fn prevent_extensions(&self) -> AltCompletion<bool>;
    fn get_own_property(&self, key: &PropertyKey) -> AltCompletion<Option<PropertyDescriptor>>;
    fn define_own_property(&self, key: &PropertyKey, desc: &PotentialPropertyDescriptor) -> AltCompletion<bool>;
    fn has_property(&self, key: &PropertyKey) -> AltCompletion<bool>;
    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion;
    fn set(&self, agent: &mut Agent, key: &PropertyKey, value: &ECMAScriptValue, receiver: &ECMAScriptValue) -> AltCompletion<bool>;
    fn delete(&self, key: &PropertyKey) -> AltCompletion<bool>;
    fn own_property_keys(&self) -> AltCompletion<Vec<PropertyKey>>;
}

pub trait FunctionInterface {
    fn call(&self, this_argument: ECMAScriptValue, arguments_list: Vec<ECMAScriptValue>) -> Completion;
}

pub struct CommonObjectData {
    pub properties: AHashMap<PropertyKey, PropertyDescriptor>,
    pub prototype: Option<Object>,
    pub extensible: bool,
    pub next_spot: usize,
    pub objid: usize,
}

impl CommonObjectData {
    pub fn new(agent: &mut Agent) -> Self {
        Self { properties: Default::default(), prototype: None, extensible: true, next_spot: 0, objid: agent.next_object_id() }
    }
}

struct OrdinaryObject {
    data: RefCell<CommonObjectData>,
}

impl<'a> From<&'a OrdinaryObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a OrdinaryObject) -> Self {
        obj
    }
}

impl ObjectInterface for OrdinaryObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.data
    }
    fn is_ordinary(&self) -> bool {
        true
    }
    fn id(&self) -> usize {
        self.data.borrow().objid
    }

    // [[GetPrototypeOf]] ( )
    //
    // The [[GetPrototypeOf]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    //  1. Return ! OrdinaryGetPrototypeOf(O).
    fn get_prototype_of(&self) -> AltCompletion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }

    // [[SetPrototypeOf]] ( V )
    //
    // The [[SetPrototypeOf]] internal method of an ordinary object O takes argument V (an Object or null). It performs
    // the following steps when called:
    //
    //  1. Return ! OrdinarySetPrototypeOf(O, V).
    fn set_prototype_of(&self, obj: Option<&Object>) -> AltCompletion<bool> {
        Ok(ordinary_set_prototype_of(self, obj))
    }

    // [[IsExtensible]] ( )
    //
    // The [[IsExtensible]] internal method of an ordinary object O takes no arguments. It performs the following steps
    // when called:
    //
    //  1. Return ! OrdinaryIsExtensible(O).
    fn is_extensible(&self) -> AltCompletion<bool> {
        Ok(ordinary_is_extensible(self))
    }

    // [[PreventExtensions]] ( )
    //
    // The [[PreventExtensions]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    //  1. Return ! OrdinaryPreventExtensions(O).
    fn prevent_extensions(&self) -> AltCompletion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }

    // [[GetOwnProperty]] ( P )
    //
    // The [[GetOwnProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ! OrdinaryGetOwnProperty(O, P).
    fn get_own_property(&self, key: &PropertyKey) -> AltCompletion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }

    // [[DefineOwnProperty]] ( P, Desc )
    //
    // The [[DefineOwnProperty]] internal method of an ordinary object O takes arguments P (a property key) and Desc (a
    // Property Descriptor). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryDefineOwnProperty(O, P, Desc).
    fn define_own_property(&self, key: &PropertyKey, desc: &PotentialPropertyDescriptor) -> AltCompletion<bool> {
        ordinary_define_own_property(self, key, desc)
    }

    // [[HasProperty]] ( P )
    //
    // The [[HasProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryHasProperty(O, P).
    fn has_property(&self, key: &PropertyKey) -> AltCompletion<bool> {
        ordinary_has_property(self, key)
    }

    // [[Get]] ( P, Receiver )
    //
    // The [[Get]] internal method of an ordinary object O takes arguments P (a property key) and Receiver (an
    // ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryGet(O, P, Receiver).
    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion {
        ordinary_get(self, agent, key, receiver)
    }

    // [[Set]] ( P, V, Receiver )
    //
    // The [[Set]] internal method of an ordinary object O takes arguments P (a property key), V (an ECMAScript language
    // value), and Receiver (an ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinarySet(O, P, V, Receiver).
    fn set(&self, agent: &mut Agent, key: &PropertyKey, v: &ECMAScriptValue, receiver: &ECMAScriptValue) -> AltCompletion<bool> {
        ordinary_set(self, agent, key, v, receiver)
    }

    // [[Delete]] ( P )
    //
    // The [[Delete]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryDelete(O, P).
    fn delete(&self, key: &PropertyKey) -> AltCompletion<bool> {
        ordinary_delete(self, key)
    }

    // [[OwnPropertyKeys]] ( )
    //
    // The [[OwnPropertyKeys]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    // 1. Return ! OrdinaryOwnPropertyKeys(O).
    fn own_property_keys(&self) -> AltCompletion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(self))
    }
}

#[derive(Clone, Debug)]
pub struct Object {
    pub o: Rc<dyn ObjectInterface>,
}

impl fmt::Debug for dyn ObjectInterface {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<Object {}>", self.id())
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.o.id() == other.o.id()
    }
}

impl<'a> From<&'a Object> for &'a dyn ObjectInterface {
    fn from(obj: &'a Object) -> Self {
        obj.o.as_ref()
    }
}

impl Object {
    fn new(agent: &mut Agent) -> Self {
        Self { o: Rc::new(OrdinaryObject { data: RefCell::new(CommonObjectData::new(agent)) }) }
    }
}

// MakeBasicObject ( internalSlotsList )
//
// The abstract operation MakeBasicObject takes argument internalSlotsList. It is the source of all ECMAScript objects
// that are created algorithmically, including both ordinary objects and exotic objects. It factors out common steps
// used in creating all objects, and centralizes object creation. It performs the following steps when called:
//
//  1. Assert: internalSlotsList is a List of internal slot names.
//  2. Let obj be a newly created object with an internal slot for each name in internalSlotsList.
//  3. Set obj's essential internal methods to the default ordinary object definitions specified in 10.1.
//  4. Assert: If the caller will not be overriding both obj's [[GetPrototypeOf]] and [[SetPrototypeOf]] essential internal methods, then internalSlotsList contains [[Prototype]].
//  5. Assert: If the caller will not be overriding all of obj's [[SetPrototypeOf]], [[IsExtensible]], and [[PreventExtensions]] essential internal methods, then internalSlotsList contains [[Extensible]].
//  6. If internalSlotsList contains [[Extensible]], set obj.[[Extensible]] to true.
//  7. Return obj.
//
// NOTE     Within this specification, exotic objects are created in abstract operations such as ArrayCreate and
//          BoundFunctionCreate by first calling MakeBasicObject to obtain a basic, foundational object, and then
//          overriding some or all of that object's internal methods. In order to encapsulate exotic object creation,
//          the object's essential internal methods are never modified outside those operations.
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum InternalSlotName {
    Prototype,
    Extensible,
    BooleanData,
    ErrorData,
    Nonsense, // For testing purposes, for the time being.
}
const ORDINARY_OBJECT_SLOTS: [InternalSlotName; 2] = [InternalSlotName::Prototype, InternalSlotName::Extensible];
const BOOLEAN_OBJECT_SLOTS: [InternalSlotName; 3] = [InternalSlotName::Prototype, InternalSlotName::Extensible, InternalSlotName::BooleanData];
const ERROR_OBJECT_SLOTS: [InternalSlotName; 3] = [InternalSlotName::Prototype, InternalSlotName::Extensible, InternalSlotName::ErrorData];
fn slot_match(slot_list: &[InternalSlotName], slot_set: &AHashSet<&InternalSlotName>) -> bool {
    if slot_list.len() != slot_set.len() {
        return false;
    }
    for slot_id in slot_list.iter() {
        if !slot_set.contains(slot_id) {
            return false;
        }
    }
    true
}

pub fn make_basic_object(agent: &mut Agent, internal_slots_list: &[InternalSlotName]) -> Object {
    let mut slot_set = AHashSet::with_capacity(internal_slots_list.len());
    for slot in internal_slots_list.iter() {
        slot_set.insert(slot);
    }

    if slot_match(&ORDINARY_OBJECT_SLOTS, &slot_set) {
        // Ordinary Objects
        Object::new(agent)
    } else if slot_match(&BOOLEAN_OBJECT_SLOTS, &slot_set) {
        BooleanObject::object(agent)
    } else if slot_match(&ERROR_OBJECT_SLOTS, &slot_set) {
        ErrorObject::object(agent)
    } else {
        // Unknown combination of slots
        panic!("Unknown object for slots {:?}", slot_set);
    }
}

// Get ( O, P )
//
// The abstract operation Get takes arguments O (an Object) and P (a property key). It is used to retrieve the value of
// a specific property of an object. It performs the following steps when called:
//
//  1. Assert: Type(O) is Object.
//  2. Assert: IsPropertyKey(P) is true.
//  3. Return ? O.[[Get]](P, O).
pub fn get(agent: &mut Agent, obj: &Object, key: &PropertyKey) -> Completion {
    let val = ECMAScriptValue::Object(obj.clone());
    obj.o.get(agent, key, &val)
}

// GetV ( V, P )
//
// The abstract operation GetV takes arguments V (an ECMAScript language value) and P (a property key). It is used to
// retrieve the value of a specific property of an ECMAScript language value. If the value is not an object, the
// property lookup is performed using a wrapper object appropriate for the type of the value. It performs the following
// steps when called:
//
//  1. Assert: IsPropertyKey(P) is true.
//  2. Let O be ? ToObject(V).
//  3. Return ? O.[[Get]](P, V).
pub fn getv(agent: &mut Agent, v: &ECMAScriptValue, p: &PropertyKey) -> Completion {
    let o = to_object(agent, v.clone())?;
    o.o.get(agent, p, v)
}

// Set ( O, P, V, Throw )
//
// The abstract operation Set takes arguments O (an Object), P (a property key), V (an ECMAScript language value), and
// Throw (a Boolean). It is used to set the value of a specific property of an object. V is the new value for the
// property. It performs the following steps when called:
//
//  1. Assert: Type(O) is Object.
//  2. Assert: IsPropertyKey(P) is true.
//  3. Assert: Type(Throw) is Boolean.
//  4. Let success be ? O.[[Set]](P, V, O).
//  5. If success is false and Throw is true, throw a TypeError exception.
//  6. Return success.
pub fn set(agent: &mut Agent, obj: &Object, propkey: &PropertyKey, value: &ECMAScriptValue, throw: bool) -> AltCompletion<bool> {
    let objval = ECMAScriptValue::Object(obj.clone());
    let success = obj.o.set(agent, propkey, value, &objval)?;
    if !success && throw {
        Err(create_type_error(agent, "Cannot add property, for one of many different possible reasons"))
    } else {
        Ok(success)
    }
}

// CreateDataProperty ( O, P, V )
//
// The abstract operation CreateDataProperty takes arguments O (an Object), P (a property key), and V (an ECMAScript
// language value). It is used to create a new own property of an object. It performs the following steps when called:
//
//  1. Assert: Type(O) is Object.
//  2. Assert: IsPropertyKey(P) is true.
//  3. Let newDesc be the PropertyDescriptor { [[Value]]: V, [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true }.
//  4. Return ? O.[[DefineOwnProperty]](P, newDesc).
//
// NOTE     This abstract operation creates a property whose attributes are set to the same defaults used for properties
//          created by the ECMAScript language assignment operator. Normally, the property will not already exist. If it
//          does exist and is not configurable or if O is not extensible, [[DefineOwnProperty]] will return false.
pub fn create_data_property(obj: &Object, p: &PropertyKey, v: &ECMAScriptValue) -> AltCompletion<bool> {
    let new_desc = PotentialPropertyDescriptor { value: Some(v.clone()), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };
    obj.o.define_own_property(p, &new_desc)
}

// DefinePropertyOrThrow ( O, P, desc )
//
// The abstract operation DefinePropertyOrThrow takes arguments O (an Object), P (a property key), and desc (a Property
// Descriptor). It is used to call the [[DefineOwnProperty]] internal method of an object in a manner that will throw a
// TypeError exception if the requested property update cannot be performed. It performs the following steps when
// called:
//
//  1. Assert: Type(O) is Object.
//  2. Assert: IsPropertyKey(P) is true.
//  3. Let success be ? O.[[DefineOwnProperty]](P, desc).
//  4. If success is false, throw a TypeError exception.
//  5. Return success.
pub fn define_property_or_throw(agent: &mut Agent, obj: &Object, p: &PropertyKey, desc: &PotentialPropertyDescriptor) -> AltCompletion<()> {
    let success = obj.o.define_own_property(p, desc)?;
    if !success {
        Err(create_type_error(agent, "Property cannot be assigned to"))
    } else {
        Ok(())
    }
}

// GetMethod ( V, P )
//
// The abstract operation GetMethod takes arguments V (an ECMAScript language value) and P (a property key). It is used
// to get the value of a specific property of an ECMAScript language value when the value of the property is expected to
// be a function. It performs the following steps when called:
//
//  1. Assert: IsPropertyKey(P) is true.
//  2. Let func be ? GetV(V, P).
//  3. If func is either undefined or null, return undefined.
//  4. If IsCallable(func) is false, throw a TypeError exception.
//  5. Return func.
pub fn get_method(agent: &mut Agent, val: &ECMAScriptValue, key: &PropertyKey) -> Completion {
    let func = getv(agent, val, key)?;
    if func.is_undefined() || func.is_null() {
        Ok(ECMAScriptValue::Undefined)
    } else if !is_callable(&func) {
        Err(create_type_error(agent, "item is not callable"))
    } else {
        Ok(func)
    }
}

// HasProperty ( O, P )
//
// The abstract operation HasProperty takes arguments O (an Object) and P (a property key) and returns a completion
// record which, if its [[Type]] is normal, has a [[Value]] which is a Boolean. It is used to determine whether an
// object has a property with the specified property key. The property may be either an own or inherited. It performs
// the following steps when called:
//
//  1. Assert: Type(O) is Object.
//  2. Assert: IsPropertyKey(P) is true.
//  3. Return ? O.[[HasProperty]](P).
pub fn has_property(obj: &Object, p: &PropertyKey) -> AltCompletion<bool> {
    obj.o.has_property(p)
}

// HasOwnProperty ( O, P )
//
// The abstract operation HasOwnProperty takes arguments O (an Object) and P (a property key) and returns a completion
// record which, if its [[Type]] is normal, has a [[Value]] which is a Boolean. It is used to determine whether an
// object has an own property with the specified property key. It performs the following steps when called:
//
//  1. Assert: Type(O) is Object.
//  2. Assert: IsPropertyKey(P) is true.
//  3. Let desc be ? O.[[GetOwnProperty]](P).
//  4. If desc is undefined, return false.
//  5. Return true.
pub fn has_own_property(obj: &Object, p: &PropertyKey) -> AltCompletion<bool> {
    Ok(obj.o.get_own_property(p)?.is_some())
}

// Call ( F, V [ , argumentsList ] )
//
// The abstract operation Call takes arguments F (an ECMAScript language value) and V (an ECMAScript language value) and
// optional argument argumentsList (a List of ECMAScript language values). It is used to call the [[Call]] internal
// method of a function object. F is the function object, V is an ECMAScript language value that is the this value of
// the [[Call]], and argumentsList is the value passed to the corresponding argument of the internal method. If
// argumentsList is not present, a new empty List is used as its value. It performs the following steps when called:
//
//  1. If argumentsList is not present, set argumentsList to a new empty List.
//  2. If IsCallable(F) is false, throw a TypeError exception.
//  3. Return ? F.[[Call]](V, argumentsList).
pub fn to_callable(val: &ECMAScriptValue) -> Option<&dyn CallableObject> {
    match val {
        ECMAScriptValue::Object(obj) => obj.o.to_function_obj(),
        _ => None,
    }
}

pub fn call(agent: &mut Agent, func: &ECMAScriptValue, this_value: &ECMAScriptValue, args: &[ECMAScriptValue]) -> Completion {
    let maybe_callable = to_callable(func);
    match maybe_callable {
        None => Err(create_type_error(agent, "Value not callable")),
        Some(callable) => callable.call(agent, this_value, args),
    }
}

// OrdinaryObjectCreate ( proto [ , additionalInternalSlotsList ] )
//
// The abstract operation OrdinaryObjectCreate takes argument proto (an Object or null) and optional argument
// additionalInternalSlotsList (a List of names of internal slots). It is used to specify the runtime creation of new
// ordinary objects. additionalInternalSlotsList contains the names of additional internal slots that must be defined as
// part of the object, beyond [[Prototype]] and [[Extensible]]. If additionalInternalSlotsList is not provided, a new
// empty List is used. It performs the following steps when called:
//
//  1. Let internalSlotsList be « [[Prototype]], [[Extensible]] ».
//  2. If additionalInternalSlotsList is present, append each of its elements to internalSlotsList.
//  3. Let O be ! MakeBasicObject(internalSlotsList).
//  4. Set O.[[Prototype]] to proto.
//  5. Return O.
//
// NOTE     Although OrdinaryObjectCreate does little more than call MakeBasicObject, its use communicates the intention
//          to create an ordinary object, and not an exotic one. Thus, within this specification, it is not called by
//          any algorithm that subsequently modifies the internal methods of the object in ways that would make the
//          result non-ordinary. Operations that create exotic objects invoke MakeBasicObject directly.
pub fn ordinary_object_create(agent: &mut Agent, proto: Option<&Object>, additional_internal_slots_list: &[InternalSlotName]) -> Object {
    let mut slots = vec![InternalSlotName::Prototype, InternalSlotName::Extensible];
    slots.extend_from_slice(additional_internal_slots_list);
    let o = make_basic_object(agent, slots.as_slice());
    o.o.common_object_data().borrow_mut().prototype = proto.cloned();
    o
}

// OrdinaryCreateFromConstructor ( constructor, intrinsicDefaultProto [ , internalSlotsList ] )
//
// The abstract operation OrdinaryCreateFromConstructor takes arguments constructor and intrinsicDefaultProto and
// optional argument internalSlotsList (a List of names of internal slots). It creates an ordinary object whose
// [[Prototype]] value is retrieved from a constructor's "prototype" property, if it exists. Otherwise the intrinsic
// named by intrinsicDefaultProto is used for [[Prototype]]. internalSlotsList contains the names of additional internal
// slots that must be defined as part of the object. If internalSlotsList is not provided, a new empty List is used. It
// performs the following steps when called:
//
//  1. Assert: intrinsicDefaultProto is a String value that is this specification's name of an intrinsic object. The
//     corresponding object must be an intrinsic that is intended to be used as the [[Prototype]] value of an object.
//  2. Let proto be ? GetPrototypeFromConstructor(constructor, intrinsicDefaultProto).
//  3. Return ! OrdinaryObjectCreate(proto, internalSlotsList).
pub fn ordinary_create_from_constructor(
    agent: &mut Agent,
    constructor: &Object,
    intrinsic_default_proto: IntrinsicIdentifier,
    internal_slots_list: &[InternalSlotName],
) -> AltCompletion<Object> {
    let proto = get_prototype_from_constructor(agent, constructor, intrinsic_default_proto)?;
    Ok(ordinary_object_create(agent, Some(&proto), internal_slots_list))
}

// GetPrototypeFromConstructor ( constructor, intrinsicDefaultProto )
//
// The abstract operation GetPrototypeFromConstructor takes arguments constructor and intrinsicDefaultProto. It
// determines the [[Prototype]] value that should be used to create an object corresponding to a specific constructor.
// The value is retrieved from the constructor's "prototype" property, if it exists. Otherwise the intrinsic named by
// intrinsicDefaultProto is used for [[Prototype]]. It performs the following steps when called:
//
//  1. Assert: intrinsicDefaultProto is a String value that is this specification's name of an intrinsic object. The
//     corresponding object must be an intrinsic that is intended to be used as the [[Prototype]] value of an object.
//  2. Assert: IsCallable(constructor) is true.
//  3. Let proto be ? Get(constructor, "prototype").
//  4. If Type(proto) is not Object, then
//      a. Let realm be ? GetFunctionRealm(constructor).
//      b. Set proto to realm's intrinsic object named intrinsicDefaultProto.
//  5. Return proto.
//
// NOTE     If constructor does not supply a [[Prototype]] value, the default value that is used is obtained from the
//          realm of the constructor function rather than from the running execution context.
fn get_prototype_from_constructor(agent: &mut Agent, constructor: &Object, intrinsic_default_proto: IntrinsicIdentifier) -> AltCompletion<Object> {
    let proto = get(agent, constructor, &PropertyKey::from("prototype"))?;
    match proto {
        ECMAScriptValue::Object(obj) => Ok(obj),
        _ => {
            let realm = get_function_realm(agent, constructor)?;
            Ok(realm.intrinsics.get(intrinsic_default_proto))
        }
    }
}

pub struct DeadObject {
    objid: usize,
}
impl ObjectInterface for DeadObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        unreachable!();
    }
    fn is_ordinary(&self) -> bool {
        false
    }
    fn id(&self) -> usize {
        self.objid
    }

    fn get_prototype_of(&self) -> AltCompletion<Option<Object>> {
        unreachable!();
    }
    fn set_prototype_of(&self, _obj: Option<&Object>) -> AltCompletion<bool> {
        unreachable!();
    }
    fn is_extensible(&self) -> AltCompletion<bool> {
        unreachable!();
    }
    fn prevent_extensions(&self) -> AltCompletion<bool> {
        unreachable!();
    }
    fn get_own_property(&self, _key: &PropertyKey) -> AltCompletion<Option<PropertyDescriptor>> {
        unreachable!();
    }
    fn define_own_property(&self, _key: &PropertyKey, _desc: &PotentialPropertyDescriptor) -> AltCompletion<bool> {
        unreachable!();
    }
    fn has_property(&self, _key: &PropertyKey) -> AltCompletion<bool> {
        unreachable!();
    }
    fn get(&self, _agent: &mut Agent, _key: &PropertyKey, _receiver: &ECMAScriptValue) -> Completion {
        unreachable!();
    }
    fn set(&self, _agent: &mut Agent, _key: &PropertyKey, _value: &ECMAScriptValue, _receiver: &ECMAScriptValue) -> AltCompletion<bool> {
        unreachable!();
    }
    fn delete(&self, _key: &PropertyKey) -> AltCompletion<bool> {
        unreachable!();
    }
    fn own_property_keys(&self) -> AltCompletion<Vec<PropertyKey>> {
        unreachable!();
    }
}

impl DeadObject {
    pub fn object(agent: &mut Agent) -> Object {
        Object { o: Rc::new(Self { objid: agent.next_object_id() }) }
    }
}

// SetImmutablePrototype ( O, V )
//
// The abstract operation SetImmutablePrototype takes arguments O and V. It performs the following steps when
// called:
//
//  1. Assert: Either Type(V) is Object or Type(V) is Null.
//  2. Let current be ? O.[[GetPrototypeOf]]().
//  3. If SameValue(V, current) is true, return true.
//  4. Return false.
pub fn set_immutable_prototype<'a, T>(o: T, val: Option<&Object>) -> AltCompletion<bool>
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let current = obj.get_prototype_of()?;
    Ok(match (current.as_ref(), val) {
        (None, None) => true,
        (Some(a), Some(b)) => a == b,
        _ => false,
    })
}

pub struct ImmutablePrototypeExoticObject {
    data: RefCell<CommonObjectData>,
}

impl<'a> From<&'a ImmutablePrototypeExoticObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a ImmutablePrototypeExoticObject) -> Self {
        obj
    }
}

impl ObjectInterface for ImmutablePrototypeExoticObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.data
    }
    fn is_ordinary(&self) -> bool {
        false
    }
    fn id(&self) -> usize {
        self.data.borrow().objid
    }

    // [[GetPrototypeOf]] ( )
    //
    // The [[GetPrototypeOf]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    //  1. Return ! OrdinaryGetPrototypeOf(O).
    fn get_prototype_of(&self) -> AltCompletion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }

    // [[SetPrototypeOf]] ( V )
    //
    // The [[SetPrototypeOf]] internal method of an immutable prototype exotic object O takes argument V (an Object or
    // null). It performs the following steps when called:
    //
    //  1. Return ? SetImmutablePrototype(O, V).
    fn set_prototype_of(&self, obj: Option<&Object>) -> AltCompletion<bool> {
        set_immutable_prototype(self, obj)
    }

    // [[IsExtensible]] ( )
    //
    // The [[IsExtensible]] internal method of an ordinary object O takes no arguments. It performs the following steps
    // when called:
    //
    //  1. Return ! OrdinaryIsExtensible(O).
    fn is_extensible(&self) -> AltCompletion<bool> {
        Ok(ordinary_is_extensible(self))
    }

    // [[PreventExtensions]] ( )
    //
    // The [[PreventExtensions]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    //  1. Return ! OrdinaryPreventExtensions(O).
    fn prevent_extensions(&self) -> AltCompletion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }

    // [[GetOwnProperty]] ( P )
    //
    // The [[GetOwnProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ! OrdinaryGetOwnProperty(O, P).
    fn get_own_property(&self, key: &PropertyKey) -> AltCompletion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }

    // [[DefineOwnProperty]] ( P, Desc )
    //
    // The [[DefineOwnProperty]] internal method of an ordinary object O takes arguments P (a property key) and Desc (a
    // Property Descriptor). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryDefineOwnProperty(O, P, Desc).
    fn define_own_property(&self, key: &PropertyKey, desc: &PotentialPropertyDescriptor) -> AltCompletion<bool> {
        ordinary_define_own_property(self, key, desc)
    }

    // [[HasProperty]] ( P )
    //
    // The [[HasProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryHasProperty(O, P).
    fn has_property(&self, key: &PropertyKey) -> AltCompletion<bool> {
        ordinary_has_property(self, key)
    }

    // [[Get]] ( P, Receiver )
    //
    // The [[Get]] internal method of an ordinary object O takes arguments P (a property key) and Receiver (an
    // ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryGet(O, P, Receiver).
    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion {
        ordinary_get(self, agent, key, receiver)
    }

    // [[Set]] ( P, V, Receiver )
    //
    // The [[Set]] internal method of an ordinary object O takes arguments P (a property key), V (an ECMAScript language
    // value), and Receiver (an ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinarySet(O, P, V, Receiver).
    fn set(&self, agent: &mut Agent, key: &PropertyKey, v: &ECMAScriptValue, receiver: &ECMAScriptValue) -> AltCompletion<bool> {
        ordinary_set(self, agent, key, v, receiver)
    }

    // [[Delete]] ( P )
    //
    // The [[Delete]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryDelete(O, P).
    fn delete(&self, key: &PropertyKey) -> AltCompletion<bool> {
        ordinary_delete(self, key)
    }

    // [[OwnPropertyKeys]] ( )
    //
    // The [[OwnPropertyKeys]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    // 1. Return ! OrdinaryOwnPropertyKeys(O).
    fn own_property_keys(&self) -> AltCompletion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(self))
    }
}

pub fn immutable_prototype_exotic_object_create(agent: &mut Agent, proto: Option<&Object>) -> Object {
    let obj = Object { o: Rc::new(ImmutablePrototypeExoticObject { data: RefCell::new(CommonObjectData::new(agent)) }) };
    obj.o.common_object_data().borrow_mut().prototype = proto.cloned();
    obj
}

#[cfg(test)]
mod tests;
