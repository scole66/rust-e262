use super::agent::Agent;
use super::arrays::{array_create, ArrayObject};
use super::boolean_object::{BooleanObject, BooleanObjectInterface};
use super::comparison::is_extensible;
use super::cr::{AltCompletion, Completion};
use super::errors::create_type_error;
use super::errors::ErrorObject;
use super::function_object::{BuiltinFunctionInterface, CallableObject, ConstructableObject, FunctionObjectData};
use super::number_object::{NumberObject, NumberObjectInterface};
use super::realm::{IntrinsicId, Realm};
use super::values::{is_callable, to_boolean, to_object, to_string, ECMAScriptValue, PrivateElement, PrivateElementKind, PrivateName, PropertyKey};
use ahash::{AHashMap, AHashSet};
use std::cell::RefCell;
use std::convert::TryFrom;
use std::fmt::{self, Debug};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone, Default)]
pub struct DataProperty {
    pub value: ECMAScriptValue,
    pub writable: bool,
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct AccessorProperty {
    pub get: ECMAScriptValue,
    pub set: ECMAScriptValue,
}

#[derive(Debug, PartialEq, Clone)]
pub enum PropertyKind {
    Data(DataProperty),
    Accessor(AccessorProperty),
}

impl Default for PropertyKind {
    fn default() -> Self {
        Self::Data(Default::default())
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct PropertyDescriptor {
    pub property: PropertyKind,
    pub enumerable: bool,
    pub configurable: bool,
    pub spot: usize,
}

struct ConcisePropertyDescriptor<'a>(&'a PropertyDescriptor);
impl<'a> fmt::Debug for ConcisePropertyDescriptor<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{ ")?;
        match &self.0.property {
            PropertyKind::Data(data) => {
                data.value.concise(f)?;
                write!(f, " {}", if data.writable { 'w' } else { '-' })?;
            }
            PropertyKind::Accessor(funcs) => {
                write!(f, "[[Get]]: ")?;
                funcs.get.concise(f)?;
                write!(f, " [[Set]]: ")?;
                funcs.set.concise(f)?;
                write!(f, " ")?;
            }
        }
        write!(f, "{}{} }}", if self.0.enumerable { 'e' } else { '-' }, if self.0.configurable { 'c' } else { '-' })
    }
}

impl<'a> From<&'a PropertyDescriptor> for ConcisePropertyDescriptor<'a> {
    fn from(source: &'a PropertyDescriptor) -> Self {
        Self(source)
    }
}

pub struct DataDescriptor {
    pub value: ECMAScriptValue,
    pub writable: bool,
    pub enumerable: bool,
    pub configurable: bool,
}

impl TryFrom<PropertyDescriptor> for DataDescriptor {
    type Error = &'static str;
    fn try_from(source: PropertyDescriptor) -> Result<Self, Self::Error> {
        match &source.property {
            PropertyKind::Accessor(..) => Err("Accessor Property cannot be formed into a DataDescriptor"),
            PropertyKind::Data(DataProperty { value, writable }) => {
                Ok(DataDescriptor { value: value.clone(), writable: *writable, enumerable: source.enumerable, configurable: source.configurable })
            }
        }
    }
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

#[derive(Debug, Default, PartialEq, Clone)]
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

// FromPropertyDescriptor ( Desc )
//
// The abstract operation FromPropertyDescriptor takes argument Desc (a Property Descriptor or undefined). It performs
// the following steps when called:
//
//  1. If Desc is undefined, return undefined.
//  2. Let obj be ! OrdinaryObjectCreate(%Object.prototype%).
//  3. Assert: obj is an extensible ordinary object with no own properties.
//  4. If Desc has a [[Value]] field, then
//      a. Perform ! CreateDataPropertyOrThrow(obj, "value", Desc.[[Value]]).
//  5. If Desc has a [[Writable]] field, then
//      a. Perform ! CreateDataPropertyOrThrow(obj, "writable", Desc.[[Writable]]).
//  6. If Desc has a [[Get]] field, then
//      a. Perform ! CreateDataPropertyOrThrow(obj, "get", Desc.[[Get]]).
//  7. If Desc has a [[Set]] field, then
//      a. Perform ! CreateDataPropertyOrThrow(obj, "set", Desc.[[Set]]).
//  8. If Desc has an [[Enumerable]] field, then
//      a. Perform ! CreateDataPropertyOrThrow(obj, "enumerable", Desc.[[Enumerable]]).
//  9. If Desc has a [[Configurable]] field, then
//      a. Perform ! CreateDataPropertyOrThrow(obj, "configurable", Desc.[[Configurable]]).
//  10. Return obj.
pub fn from_property_descriptor(agent: &mut Agent, desc: Option<PropertyDescriptor>) -> Option<Object> {
    match desc {
        Some(d) => {
            let obj = ordinary_object_create(agent, Some(agent.intrinsic(IntrinsicId::ObjectPrototype)), &[]);
            match &d.property {
                PropertyKind::Data(DataProperty { value, writable }) => {
                    create_data_property_or_throw(agent, &obj, "value", value.clone()).unwrap();
                    create_data_property_or_throw(agent, &obj, "writable", *writable).unwrap();
                }
                PropertyKind::Accessor(AccessorProperty { get, set }) => {
                    create_data_property_or_throw(agent, &obj, "get", get.clone()).unwrap();
                    create_data_property_or_throw(agent, &obj, "set", set.clone()).unwrap();
                }
            }
            create_data_property_or_throw(agent, &obj, "enumerable", d.enumerable).unwrap();
            create_data_property_or_throw(agent, &obj, "configurable", d.configurable).unwrap();
            Some(obj)
        }
        None => None,
    }
}

// ToPropertyDescriptor ( Obj )
//
// The abstract operation ToPropertyDescriptor takes argument Obj. It performs the following steps when called:
//
//  1. If Type(Obj) is not Object, throw a TypeError exception.
//  2. Let desc be a new Property Descriptor that initially has no fields.
//  3. Let hasEnumerable be ? HasProperty(Obj, "enumerable").
//  4. If hasEnumerable is true, then
//      a. Let enumerable be ! ToBoolean(? Get(Obj, "enumerable")).
//      b. Set desc.[[Enumerable]] to enumerable.
//  5. Let hasConfigurable be ? HasProperty(Obj, "configurable").
//  6. If hasConfigurable is true, then
//      a. Let configurable be ! ToBoolean(? Get(Obj, "configurable")).
//      b. Set desc.[[Configurable]] to configurable.
//  7. Let hasValue be ? HasProperty(Obj, "value").
//  8. If hasValue is true, then
//      a. Let value be ? Get(Obj, "value").
//      b. Set desc.[[Value]] to value.
//  9. Let hasWritable be ? HasProperty(Obj, "writable").
//  10. If hasWritable is true, then
//      a. Let writable be ! ToBoolean(? Get(Obj, "writable")).
//      b. Set desc.[[Writable]] to writable.
//  11. Let hasGet be ? HasProperty(Obj, "get").
//  12. If hasGet is true, then
//      a. Let getter be ? Get(Obj, "get").
//      b. If IsCallable(getter) is false and getter is not undefined, throw a TypeError exception.
//      c. Set desc.[[Get]] to getter.
//  13. Let hasSet be ? HasProperty(Obj, "set").
//  14. If hasSet is true, then
//      a. Let setter be ? Get(Obj, "set").
//      b. If IsCallable(setter) is false and setter is not undefined, throw a TypeError exception.
//  c. Set desc.[[Set]] to setter.
//  15. If desc.[[Get]] is present or desc.[[Set]] is present, then
//      a. If desc.[[Value]] is present or desc.[[Writable]] is present, throw a TypeError exception.
//  16. Return desc.
fn get_pd_prop(agent: &mut Agent, obj: &Object, key: impl Into<PropertyKey>) -> AltCompletion<Option<ECMAScriptValue>> {
    Ok({
        let key = key.into();
        if has_property(agent, obj, &key)? {
            Some(get(agent, obj, &key)?)
        } else {
            None
        }
    })
}
fn get_pd_bool(agent: &mut Agent, obj: &Object, key: &str) -> AltCompletion<Option<bool>> {
    Ok(get_pd_prop(agent, obj, key)?.map(to_boolean))
}
pub fn to_property_descriptor(agent: &mut Agent, obj: &ECMAScriptValue) -> AltCompletion<PotentialPropertyDescriptor> {
    match obj {
        ECMAScriptValue::Object(obj) => {
            let enumerable = get_pd_bool(agent, obj, "enumerable")?;
            let configurable = get_pd_bool(agent, obj, "configurable")?;
            let value = get_pd_prop(agent, obj, "value")?;
            let writable = get_pd_bool(agent, obj, "writable")?;
            let get = get_pd_prop(agent, obj, "get")?;
            if let Some(getter) = &get {
                if !getter.is_undefined() && !is_callable(getter) {
                    return Err(create_type_error(agent, "Getter must be callable (or undefined)"));
                }
            }
            let set = get_pd_prop(agent, obj, "set")?;
            if let Some(setter) = &set {
                if !setter.is_undefined() && !is_callable(setter) {
                    return Err(create_type_error(agent, "Setter must be callable (or undefined)"));
                }
            }
            Ok(PotentialPropertyDescriptor { enumerable, configurable, value, writable, get, set })
        }
        _ => Err(create_type_error(agent, "Must be an object")),
    }
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
pub fn ordinary_set_prototype_of<'a, T>(o: T, val: Option<Object>) -> bool
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let current = obj.common_object_data().borrow().prototype.clone();
    if current == val {
        return true;
    }
    let extensible = obj.common_object_data().borrow().extensible;
    if !extensible {
        return false;
    }
    let mut p = val.clone();
    while let Some(pp) = p {
        if pp.o.id() == obj.id() {
            return false;
        }
        if !pp.o.is_ordinary() {
            break;
        }
        p = pp.o.common_object_data().borrow().prototype.clone();
    }
    obj.common_object_data().borrow_mut().prototype = val;
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
pub fn ordinary_define_own_property<'a, T>(agent: &mut Agent, o: T, p: PropertyKey, desc: PotentialPropertyDescriptor) -> AltCompletion<bool>
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let current = obj.get_own_property(agent, &p)?;
    let extensible = is_extensible(agent, obj)?;
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
fn validate_and_apply_property_descriptor<'a, T>(oo: Option<T>, p: Option<PropertyKey>, extensible: bool, desc: PotentialPropertyDescriptor, current: Option<&PropertyDescriptor>) -> bool
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
                        property: if is_generic_descriptor(&desc) || is_data_descriptor(&desc) {
                            PropertyKind::Data(DataProperty { value: desc.value.unwrap_or(ECMAScriptValue::Undefined), writable: desc.writable.unwrap_or(false) })
                        } else {
                            PropertyKind::Accessor(AccessorProperty { get: desc.get.unwrap_or(ECMAScriptValue::Undefined), set: desc.set.unwrap_or(ECMAScriptValue::Undefined) })
                        },
                        spot: data.next_spot,
                    };
                    data.properties.insert(p.unwrap(), property_descriptor);
                    data.next_spot += 1;
                }
                true
            }
        }
        Some(cur) => {
            if desc.configurable.is_none() && desc.enumerable.is_none() && desc.get.is_none() && desc.set.is_none() && desc.value.is_none() && desc.writable.is_none() {
                true
            } else if !cur.configurable && (desc.configurable.unwrap_or(false) || desc.enumerable.unwrap_or(cur.enumerable) != cur.enumerable) {
                false
            } else {
                if is_generic_descriptor(&desc) {
                    // Step 5
                    // No further validation required
                } else if !cur.configurable && cur.is_data_descriptor() != desc.is_data_descriptor() {
                    return false;
                } else if !cur.configurable {
                    match &cur.property {
                        PropertyKind::Data(data_fields) => {
                            // Step 7
                            if !data_fields.writable {
                                if desc.writable.unwrap_or(false) {
                                    return false;
                                }
                                if let Some(val) = desc.value.as_ref() {
                                    if *val != data_fields.value {
                                        return false;
                                    }
                                }
                                return true;
                            }
                        }
                        PropertyKind::Accessor(acc_fields) => {
                            // Step 8
                            if let Some(getter) = desc.get.as_ref() {
                                if *getter != acc_fields.get {
                                    return false;
                                }
                            }
                            if let Some(setter) = desc.set.as_ref() {
                                if *setter != acc_fields.set {
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
                    if cur.is_data_descriptor() && desc.is_accessor_descriptor() && !desc.is_data_descriptor() {
                        pd.property = PropertyKind::Accessor(AccessorProperty { get: desc.get.unwrap_or(ECMAScriptValue::Undefined), set: desc.set.unwrap_or(ECMAScriptValue::Undefined) });
                    } else if cur.is_accessor_descriptor() && desc.is_data_descriptor() {
                        pd.property = PropertyKind::Data(DataProperty { writable: desc.writable.unwrap_or(false), value: desc.value.unwrap_or(ECMAScriptValue::Undefined) });
                    } else {
                        match &mut pd.property {
                            PropertyKind::Accessor(acc_methods) => {
                                if let Some(setter) = desc.set {
                                    acc_methods.set = setter;
                                }
                                if let Some(getter) = desc.get {
                                    acc_methods.get = getter;
                                }
                            }
                            PropertyKind::Data(data_fields) => {
                                if let Some(value) = desc.value {
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
pub fn ordinary_has_property<'a, T>(agent: &mut Agent, o: T, p: &PropertyKey) -> AltCompletion<bool>
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let has_own = obj.get_own_property(agent, p)?;
    match has_own {
        Some(_) => Ok(true),
        None => {
            let pot_parent = obj.get_prototype_of(agent)?;
            match pot_parent {
                Some(parent) => parent.o.has_property(agent, p),
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
pub fn ordinary_get<'a, T>(agent: &mut Agent, o: T, p: &PropertyKey, receiver: &ECMAScriptValue) -> Completion
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let pot_desc = obj.get_own_property(agent, p)?;
    match pot_desc {
        None => {
            let pot_parent = obj.get_prototype_of(agent)?;
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
                    getter => call(agent, &getter, receiver, &[]),
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
pub fn ordinary_set<'a, T>(agent: &mut Agent, o: T, p: PropertyKey, v: ECMAScriptValue, receiver: &ECMAScriptValue) -> AltCompletion<bool>
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let own_desc = obj.get_own_property(agent, &p)?;
    ordinary_set_with_own_descriptor(agent, obj, p, v, receiver, own_desc)
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
    agent: &mut Agent,
    o: T,
    p: PropertyKey,
    v: ECMAScriptValue,
    receiver: &ECMAScriptValue,
    pot_own_desc: Option<PropertyDescriptor>,
) -> AltCompletion<bool>
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let own_desc = match pot_own_desc {
        None => {
            let pot_parent = obj.get_prototype_of(agent)?;
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
                    let maybe_existing_descriptor = receiver.o.get_own_property(agent, &p)?;
                    match maybe_existing_descriptor {
                        Some(existing_descriptor) => match &existing_descriptor.property {
                            PropertyKind::Accessor(_) => Ok(false),
                            PropertyKind::Data(existing_data_fields) => match existing_data_fields.writable {
                                false => Ok(false),
                                true => {
                                    let value_desc = PotentialPropertyDescriptor { value: Some(v), ..Default::default() };
                                    receiver.o.define_own_property(agent, p, value_desc)
                                }
                            },
                        },
                        None => create_data_property(agent, receiver, p, v),
                    }
                }
                _ => Ok(false),
            },
        },
        PropertyKind::Accessor(acc_methods) => {
            let setter = &acc_methods.set;
            match setter {
                ECMAScriptValue::Undefined => Ok(false),
                setter => {
                    call(agent, setter, receiver, &[v])?;
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
pub fn ordinary_delete<'a, T>(agent: &mut Agent, o: T, p: &PropertyKey) -> AltCompletion<bool>
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let desc = obj.get_own_property(agent, p)?;
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
pub fn ordinary_own_property_keys<'a, T>(agent: &mut Agent, o: T) -> Vec<PropertyKey>
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let data = obj.common_object_data().borrow();
    let mut keys: Vec<PropertyKey> = Vec::with_capacity(data.properties.len());
    let mut norm_keys: Vec<(PropertyKey, usize)> = Vec::new();
    let mut symb_keys: Vec<(PropertyKey, usize)> = Vec::new();
    for (key, desc) in data.properties.iter() {
        if key.is_array_index(agent) {
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
    keys.sort_by_cached_key(array_index_key);
    norm_keys.sort_by_key(|x| x.1);
    symb_keys.sort_by_key(|x| x.1);
    for item in norm_keys.into_iter() {
        keys.push(item.0);
    }
    for item in symb_keys.into_iter() {
        keys.push(item.0);
    }
    keys
}
fn array_index_key(item: &PropertyKey) -> u32 {
    match item {
        PropertyKey::String(s) => String::from_utf16_lossy(s.as_slice()).parse::<u32>().unwrap(),
        PropertyKey::Symbol(_) => unreachable!(),
    }
}

pub trait ObjectInterface: Debug {
    fn common_object_data(&self) -> &RefCell<CommonObjectData>;
    fn is_ordinary(&self) -> bool; // True if implements ordinary defintions of Get/SetPrototypeOf
    fn id(&self) -> usize; // Unique object id. Used for object "is_same" detection.
    fn to_boolean_obj(&self) -> Option<&dyn BooleanObjectInterface> {
        None
    }
    fn to_number_obj(&self) -> Option<&dyn NumberObjectInterface> {
        None
    }
    fn to_error_obj(&self) -> Option<&dyn ObjectInterface> {
        None
    }
    fn to_function_obj(&self) -> Option<&dyn FunctionInterface> {
        // This is a standard ECMAScript Function Object --- in particular, not a exotic Built-In Function object
        None
    }
    fn to_callable_obj(&self) -> Option<&dyn CallableObject> {
        // Whereas this is anything that implements [[Call]]
        None
    }
    fn to_constructable(&self) -> Option<&dyn ConstructableObject> {
        None
    }
    fn to_builtin_function_obj(&self) -> Option<&dyn BuiltinFunctionInterface> {
        None
    }
    fn is_arguments_object(&self) -> bool {
        false
    }
    fn is_callable_obj(&self) -> bool {
        false
    }
    fn is_error_object(&self) -> bool {
        false
    }
    fn is_boolean_object(&self) -> bool {
        false
    }
    fn is_number_object(&self) -> bool {
        false
    }
    fn is_string_object(&self) -> bool {
        false
    }
    fn is_date_object(&self) -> bool {
        false
    }
    fn is_regexp_object(&self) -> bool {
        false
    }
    fn is_array_object(&self) -> bool {
        false
    }
    fn to_array_object(&self) -> Option<&ArrayObject> {
        None
    }
    fn is_proxy_object(&self) -> bool {
        false
    }

    fn get_prototype_of(&self, agent: &mut Agent) -> AltCompletion<Option<Object>>;
    fn set_prototype_of(&self, agent: &mut Agent, obj: Option<Object>) -> AltCompletion<bool>;
    fn is_extensible(&self, agent: &mut Agent) -> AltCompletion<bool>;
    fn prevent_extensions(&self, agent: &mut Agent) -> AltCompletion<bool>;
    fn get_own_property(&self, agent: &mut Agent, key: &PropertyKey) -> AltCompletion<Option<PropertyDescriptor>>;
    fn define_own_property(&self, agent: &mut Agent, key: PropertyKey, desc: PotentialPropertyDescriptor) -> AltCompletion<bool>;
    fn has_property(&self, agent: &mut Agent, key: &PropertyKey) -> AltCompletion<bool>;
    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion;
    fn set(&self, agent: &mut Agent, key: PropertyKey, value: ECMAScriptValue, receiver: &ECMAScriptValue) -> AltCompletion<bool>;
    fn delete(&self, agent: &mut Agent, key: &PropertyKey) -> AltCompletion<bool>;
    fn own_property_keys(&self, agent: &mut Agent) -> AltCompletion<Vec<PropertyKey>>;
}

pub trait FunctionInterface: CallableObject {
    fn function_data(&self) -> &RefCell<FunctionObjectData>;
}

// This is really for debugging. It's the output structure from propdump.
#[derive(Debug, PartialEq)]
pub enum PropertyInfoKind {
    Accessor { getter: ECMAScriptValue, setter: ECMAScriptValue },
    Data { value: ECMAScriptValue, writable: bool },
}
#[derive(Debug, PartialEq)]
pub struct PropertyInfo {
    pub name: PropertyKey,
    pub enumerable: bool,
    pub configurable: bool,
    pub kind: PropertyInfoKind,
}

pub struct CommonObjectData {
    pub properties: AHashMap<PropertyKey, PropertyDescriptor>,
    pub prototype: Option<Object>,
    pub extensible: bool,
    pub next_spot: usize,
    pub objid: usize,
    pub slots: Vec<InternalSlotName>,
    pub private_elements: Vec<Rc<PrivateElement>>,
}

impl CommonObjectData {
    pub fn new(agent: &mut Agent, prototype: Option<Object>, extensible: bool, slots: &[InternalSlotName]) -> Self {
        Self { properties: Default::default(), prototype, extensible, next_spot: 0, objid: agent.next_object_id(), slots: Vec::from(slots), private_elements: vec![] }
    }

    pub fn propdump(&self) -> Vec<PropertyInfo> {
        // Dump the properties as a simplified data structure, in a reproducable way. For testing, mostly.
        // (Allows for Eq style tests, heedless of the internal structure of a property descriptor; also sorted in order of addition to object.)
        let mut keys: Vec<&PropertyKey> = self.properties.keys().collect();
        keys.sort_by_cached_key(|a| self.properties.get(*a).unwrap().spot);
        let mut result = vec![];
        for key in keys {
            let prop = self.properties.get(key).unwrap();
            result.push(PropertyInfo {
                name: key.clone(),
                enumerable: prop.enumerable,
                configurable: prop.configurable,
                kind: match &prop.property {
                    PropertyKind::Data(DataProperty { value, writable }) => PropertyInfoKind::Data { value: value.clone(), writable: *writable },
                    PropertyKind::Accessor(AccessorProperty { get, set }) => PropertyInfoKind::Accessor { getter: get.clone(), setter: set.clone() },
                },
            });
        }
        result
    }
}

impl fmt::Debug for CommonObjectData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("CommonObjectData")
            .field("properties", &ConciseProperties::from(&self.properties))
            .field("[[Prototype]]", &ConciseOptionalObject::from(&self.prototype))
            .field("[[Extensible]]", &self.extensible)
            .field("next_spot", &self.next_spot)
            .field("objid", &self.objid)
            .field("slots", &self.slots)
            .finish()
    }
}

struct ConciseObject<'a>(&'a Object);
impl<'a> fmt::Debug for ConciseObject<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.concise(f)
    }
}
impl<'a> From<&'a Object> for ConciseObject<'a> {
    fn from(source: &'a Object) -> Self {
        Self(source)
    }
}
struct ConciseOptionalObject<'a>(&'a Option<Object>);
impl<'a> fmt::Debug for ConciseOptionalObject<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.as_ref() {
            None => write!(f, "None"),
            Some(o) => ConciseObject::from(o).fmt(f),
        }
    }
}
impl<'a> From<&'a Option<Object>> for ConciseOptionalObject<'a> {
    fn from(source: &'a Option<Object>) -> Self {
        Self(source)
    }
}

struct ConciseProperties<'a>(&'a AHashMap<PropertyKey, PropertyDescriptor>);
impl<'a> fmt::Debug for ConciseProperties<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut work = f.debug_struct("AHashMap");
        for (key, value) in self.0.iter() {
            work.field(format!("{}", key).as_str(), &ConcisePropertyDescriptor::from(value));
        }
        work.finish()
    }
}

impl<'a> From<&'a AHashMap<PropertyKey, PropertyDescriptor>> for ConciseProperties<'a> {
    fn from(source: &'a AHashMap<PropertyKey, PropertyDescriptor>) -> Self {
        Self(source)
    }
}

#[derive(Debug)]
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
    fn get_prototype_of(&self, _agent: &mut Agent) -> AltCompletion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }

    // [[SetPrototypeOf]] ( V )
    //
    // The [[SetPrototypeOf]] internal method of an ordinary object O takes argument V (an Object or null). It performs
    // the following steps when called:
    //
    //  1. Return ! OrdinarySetPrototypeOf(O, V).
    fn set_prototype_of(&self, _agent: &mut Agent, obj: Option<Object>) -> AltCompletion<bool> {
        Ok(ordinary_set_prototype_of(self, obj))
    }

    // [[IsExtensible]] ( )
    //
    // The [[IsExtensible]] internal method of an ordinary object O takes no arguments. It performs the following steps
    // when called:
    //
    //  1. Return ! OrdinaryIsExtensible(O).
    fn is_extensible(&self, _agent: &mut Agent) -> AltCompletion<bool> {
        Ok(ordinary_is_extensible(self))
    }

    // [[PreventExtensions]] ( )
    //
    // The [[PreventExtensions]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    //  1. Return ! OrdinaryPreventExtensions(O).
    fn prevent_extensions(&self, _agent: &mut Agent) -> AltCompletion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }

    // [[GetOwnProperty]] ( P )
    //
    // The [[GetOwnProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ! OrdinaryGetOwnProperty(O, P).
    fn get_own_property(&self, _agent: &mut Agent, key: &PropertyKey) -> AltCompletion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }

    // [[DefineOwnProperty]] ( P, Desc )
    //
    // The [[DefineOwnProperty]] internal method of an ordinary object O takes arguments P (a property key) and Desc (a
    // Property Descriptor). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryDefineOwnProperty(O, P, Desc).
    fn define_own_property(&self, agent: &mut Agent, key: PropertyKey, desc: PotentialPropertyDescriptor) -> AltCompletion<bool> {
        ordinary_define_own_property(agent, self, key, desc)
    }

    // [[HasProperty]] ( P )
    //
    // The [[HasProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryHasProperty(O, P).
    fn has_property(&self, agent: &mut Agent, key: &PropertyKey) -> AltCompletion<bool> {
        ordinary_has_property(agent, self, key)
    }

    // [[Get]] ( P, Receiver )
    //
    // The [[Get]] internal method of an ordinary object O takes arguments P (a property key) and Receiver (an
    // ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryGet(O, P, Receiver).
    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion {
        ordinary_get(agent, self, key, receiver)
    }

    // [[Set]] ( P, V, Receiver )
    //
    // The [[Set]] internal method of an ordinary object O takes arguments P (a property key), V (an ECMAScript language
    // value), and Receiver (an ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinarySet(O, P, V, Receiver).
    fn set(&self, agent: &mut Agent, key: PropertyKey, v: ECMAScriptValue, receiver: &ECMAScriptValue) -> AltCompletion<bool> {
        ordinary_set(agent, self, key, v, receiver)
    }

    // [[Delete]] ( P )
    //
    // The [[Delete]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryDelete(O, P).
    fn delete(&self, agent: &mut Agent, key: &PropertyKey) -> AltCompletion<bool> {
        ordinary_delete(agent, self, key)
    }

    // [[OwnPropertyKeys]] ( )
    //
    // The [[OwnPropertyKeys]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    // 1. Return ! OrdinaryOwnPropertyKeys(O).
    fn own_property_keys(&self, agent: &mut Agent) -> AltCompletion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(agent, self))
    }
}

#[derive(Clone, Debug)]
pub struct Object {
    pub o: Rc<dyn ObjectInterface>,
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.o.id() == other.o.id()
    }
}

impl TryFrom<ECMAScriptValue> for Object {
    type Error = &'static str;
    fn try_from(source: ECMAScriptValue) -> Result<Self, Self::Error> {
        if let ECMAScriptValue::Object(o) = source {
            Ok(o)
        } else {
            Err("Only object values may be converted to true objects")
        }
    }
}

impl TryFrom<&ECMAScriptValue> for Object {
    type Error = &'static str;
    fn try_from(source: &ECMAScriptValue) -> Result<Self, Self::Error> {
        if let ECMAScriptValue::Object(o) = source {
            Ok(o.clone())
        } else {
            Err("Only object values may be converted to true objects")
        }
    }
}

impl<'a> From<&'a Object> for &'a dyn ObjectInterface {
    fn from(obj: &'a Object) -> Self {
        obj.o.as_ref()
    }
}

impl Object {
    fn new(agent: &mut Agent, prototype: Option<Object>, extensible: bool) -> Self {
        Self { o: Rc::new(OrdinaryObject { data: RefCell::new(CommonObjectData::new(agent, prototype, extensible, &ORDINARY_OBJECT_SLOTS)) }) }
    }
    pub fn concise(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<Object {}>", self.o.common_object_data().borrow().objid)
    }

    // IsArray ( argument )
    //
    // The abstract operation IsArray takes argument argument. It performs the following steps when called:
    //
    //  1. If Type(argument) is not Object, return false.
    //  2. If argument is an Array exotic object, return true.
    //  3. If argument is a Proxy exotic object, then
    //      a. If argument.[[ProxyHandler]] is null, throw a TypeError exception.
    //      b. Let target be argument.[[ProxyTarget]].
    //      c. Return ? IsArray(target).
    //  4. Return false.
    pub fn is_array(&self, _agent: &mut Agent) -> AltCompletion<bool> {
        if self.o.is_array_object() {
            Ok(true)
        } else if self.o.is_proxy_object() {
            todo!()
        } else {
            Ok(false)
        }
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
    InitialName,
    Realm,
    NumberData,
    ArrayMarker, // No data associated with this; causes an array object to be constructed
    Nonsense,    // For testing purposes, for the time being.
}
pub const ORDINARY_OBJECT_SLOTS: [InternalSlotName; 2] = [InternalSlotName::Prototype, InternalSlotName::Extensible];
pub const BOOLEAN_OBJECT_SLOTS: [InternalSlotName; 3] = [InternalSlotName::Prototype, InternalSlotName::Extensible, InternalSlotName::BooleanData];
pub const ERROR_OBJECT_SLOTS: [InternalSlotName; 3] = [InternalSlotName::Prototype, InternalSlotName::Extensible, InternalSlotName::ErrorData];
pub const BUILTIN_FUNCTION_SLOTS: [InternalSlotName; 4] = [InternalSlotName::Prototype, InternalSlotName::Extensible, InternalSlotName::InitialName, InternalSlotName::Realm];
pub const NUMBER_OBJECT_SLOTS: [InternalSlotName; 3] = [InternalSlotName::Prototype, InternalSlotName::Extensible, InternalSlotName::NumberData];
pub const ARRAY_OBJECT_SLOTS: [InternalSlotName; 3] = [InternalSlotName::Prototype, InternalSlotName::Extensible, InternalSlotName::ArrayMarker];

pub fn slot_match(slot_list: &[InternalSlotName], slot_set: &AHashSet<&InternalSlotName>) -> bool {
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

pub fn make_basic_object(agent: &mut Agent, internal_slots_list: &[InternalSlotName], prototype: Option<Object>) -> Object {
    let mut slot_set = AHashSet::with_capacity(internal_slots_list.len());
    for slot in internal_slots_list.iter() {
        slot_set.insert(slot);
    }

    if slot_match(&ORDINARY_OBJECT_SLOTS, &slot_set) {
        // Ordinary Objects
        Object::new(agent, prototype, true)
    } else if slot_match(&BOOLEAN_OBJECT_SLOTS, &slot_set) {
        BooleanObject::object(agent, prototype)
    } else if slot_match(&ERROR_OBJECT_SLOTS, &slot_set) {
        ErrorObject::object(agent, prototype)
    } else if slot_match(&NUMBER_OBJECT_SLOTS, &slot_set) {
        NumberObject::object(agent, prototype)
    } else if slot_match(&ARRAY_OBJECT_SLOTS, &slot_set) {
        ArrayObject::object(agent, prototype)
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
pub fn set(agent: &mut Agent, obj: &Object, propkey: PropertyKey, value: ECMAScriptValue, throw: bool) -> AltCompletion<bool> {
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
pub fn create_data_property(agent: &mut Agent, obj: &Object, p: PropertyKey, v: ECMAScriptValue) -> AltCompletion<bool> {
    let new_desc = PotentialPropertyDescriptor { value: Some(v), writable: Some(true), enumerable: Some(true), configurable: Some(true), ..Default::default() };
    obj.o.define_own_property(agent, p, new_desc)
}

// CreateDataPropertyOrThrow ( O, P, V )
//
// The abstract operation CreateDataPropertyOrThrow takes arguments O (an Object), P (a property key), and V (an
// ECMAScript language value). It is used to create a new own property of an object. It throws a TypeError exception if
// the requested property update cannot be performed. It performs the following steps when called:
//
//  1. Let success be ? CreateDataProperty(O, P, V).
//  2. If success is false, throw a TypeError exception.
//  3. Return success.
//
// NOTE     | This abstract operation creates a property whose attributes are set to the same defaults used for
//          | properties created by the ECMAScript language assignment operator. Normally, the property will not
//          | already exist. If it does exist and is not configurable or if O is not extensible, [[DefineOwnProperty]]
//          | will return false causing this operation to throw a TypeError exception.
pub fn create_data_property_or_throw(agent: &mut Agent, obj: &Object, p: impl Into<PropertyKey>, v: impl Into<ECMAScriptValue>) -> AltCompletion<()> {
    let success = create_data_property(agent, obj, p.into(), v.into())?;
    if !success {
        Err(create_type_error(agent, "Unable to create data property"))
    } else {
        Ok(())
    }
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
pub fn define_property_or_throw(agent: &mut Agent, obj: &Object, p: PropertyKey, desc: PotentialPropertyDescriptor) -> AltCompletion<()> {
    let success = obj.o.define_own_property(agent, p, desc)?;
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
pub fn has_property(agent: &mut Agent, obj: &Object, p: &PropertyKey) -> AltCompletion<bool> {
    obj.o.has_property(agent, p)
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
pub fn has_own_property(agent: &mut Agent, obj: &Object, p: &PropertyKey) -> AltCompletion<bool> {
    Ok(obj.o.get_own_property(agent, p)?.is_some())
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
        ECMAScriptValue::Object(obj) => obj.o.to_callable_obj(),
        _ => None,
    }
}

pub fn call(agent: &mut Agent, func: &ECMAScriptValue, this_value: &ECMAScriptValue, args: &[ECMAScriptValue]) -> Completion {
    let maybe_callable = to_callable(func);
    match maybe_callable {
        None => Err(create_type_error(agent, "Value not callable")),
        Some(callable) => {
            let self_obj = to_object(agent, func.clone()).unwrap();
            callable.call(agent, &self_obj, this_value, args)
        }
    }
}

// Construct ( F [ , argumentsList [ , newTarget ] ] )
//
// The abstract operation Construct takes argument F (a function object) and optional arguments argumentsList and
// newTarget. It is used to call the [[Construct]] internal method of a function object. argumentsList and newTarget are
// the values to be passed as the corresponding arguments of the internal method. If argumentsList is not present, a new
// empty List is used as its value. If newTarget is not present, F is used as its value. It performs the following steps
// when called:
//
//    1. If newTarget is not present, set newTarget to F.
//    2. If argumentsList is not present, set argumentsList to a new empty List.
//    3. Assert: IsConstructor(F) is true.
//    4. Assert: IsConstructor(newTarget) is true.
//    5. Return ? F.[[Construct]](argumentsList, newTarget).
//
// NOTE     If newTarget is not present, this operation is equivalent to: new F(...argumentsList)
pub fn construct(agent: &mut Agent, func: &Object, args: &[ECMAScriptValue], new_target: Option<&Object>) -> Completion {
    let nt = new_target.unwrap_or(func);
    let cstr = func.o.to_constructable().unwrap();
    cstr.construct(agent, func, args, nt)
}

pub fn to_constructor(val: &ECMAScriptValue) -> Option<&dyn ConstructableObject> {
    match val {
        ECMAScriptValue::Object(obj) => obj.o.to_constructable(),
        _ => None,
    }
}

// SetIntegrityLevel ( O, level )
//
// The abstract operation SetIntegrityLevel takes arguments O (an Object) and level (sealed or frozen). It is used to
// fix the set of own properties of an object. It performs the following steps when called:
//
//  1. Let status be ? O.[[PreventExtensions]]().
//  2. If status is false, return false.
//  3. Let keys be ? O.[[OwnPropertyKeys]]().
//  4. If level is sealed, then
//      a. For each element k of keys, do
//          i. Perform ? DefinePropertyOrThrow(O, k, PropertyDescriptor { [[Configurable]]: false }).
//  5. Else,
//      a. Assert: level is frozen.
//      b. For each element k of keys, do
//          i. Let currentDesc be ? O.[[GetOwnProperty]](k).
//          ii. If currentDesc is not undefined, then
//              1. If IsAccessorDescriptor(currentDesc) is true, then
//                  a. Let desc be the PropertyDescriptor { [[Configurable]]: false }.
//              2. Else,
//                  a. Let desc be the PropertyDescriptor { [[Configurable]]: false, [[Writable]]: false }.
//              3. Perform ? DefinePropertyOrThrow(O, k, desc).
//  6. Return true.
//
// https://tc39.es/ecma262/#sec-setintegritylevel
#[derive(Debug, PartialEq)]
pub enum IntegrityLevel {
    Sealed,
    Frozen,
}
pub fn set_integrity_level(agent: &mut Agent, o: &Object, level: IntegrityLevel) -> AltCompletion<bool> {
    let status = o.o.prevent_extensions(agent)?;
    if !status {
        return Ok(false);
    }
    let keys = o.o.own_property_keys(agent)?;
    if level == IntegrityLevel::Sealed {
        for k in keys {
            define_property_or_throw(agent, o, k, PotentialPropertyDescriptor { configurable: Some(false), ..Default::default() })?;
        }
    } else {
        for k in keys {
            if let Some(current_desc) = o.o.get_own_property(agent, &k)? {
                let desc = if is_accessor_descriptor(&current_desc) {
                    PotentialPropertyDescriptor { configurable: Some(false), ..Default::default() }
                } else {
                    PotentialPropertyDescriptor { configurable: Some(false), writable: Some(false), ..Default::default() }
                };
                define_property_or_throw(agent, o, k, desc)?;
            }
        }
    }
    Ok(true)
}

// CreateArrayFromList ( elements )
//
// The abstract operation CreateArrayFromList takes argument elements (a List of ECMAScript language values). It is
// used to create an Array whose elements are provided by elements. It performs the following steps when called:
//
//  1. Let array be ! ArrayCreate(0).
//  2. Let n be 0.
//  3. For each element e of elements, do
//      a. Perform ! CreateDataPropertyOrThrow(array, ! ToString(𝔽(n)), e).
//      b. Set n to n + 1.
//  4. Return array.
pub fn create_array_from_list(agent: &mut Agent, elements: &[ECMAScriptValue]) -> Object {
    let array = array_create(agent, 0, None).unwrap();
    for (n, e) in elements.iter().enumerate() {
        let key = to_string(agent, u64::try_from(n).unwrap()).unwrap();
        create_data_property_or_throw(agent, &array, key, e.clone()).unwrap();
    }
    array
}

// Invoke ( V, P [ , argumentsList ] )
//
// The abstract operation Invoke takes arguments V (an ECMAScript language value) and P (a property key) and optional
// argument argumentsList (a List of ECMAScript language values). It is used to call a method property of an ECMAScript
// language value. V serves as both the lookup point for the property and the this value of the call. argumentsList is
// the list of arguments values passed to the method. If argumentsList is not present, a new empty List is used as its
// value. It performs the following steps when called:
//
//  1. Assert: IsPropertyKey(P) is true.
//  2. If argumentsList is not present, set argumentsList to a new empty List.
//  3. Let func be ? GetV(V, P).
//  4. Return ? Call(func, V, argumentsList).
pub fn invoke(agent: &mut Agent, v: ECMAScriptValue, p: &PropertyKey, arguments_list: &[ECMAScriptValue]) -> Completion {
    let func = getv(agent, &v, p)?;
    call(agent, &func, &v, arguments_list)
}

// EnumerableOwnPropertyNames ( O, kind )
//
// The abstract operation EnumerableOwnPropertyNames takes arguments O (an Object) and kind (key, value, or key+value).
// It performs the following steps when called:
//
//  1. Let ownKeys be ? O.[[OwnPropertyKeys]]().
//  2. Let properties be a new empty List.
//  3. For each element key of ownKeys, do
//      a. If Type(key) is String, then
//          i. Let desc be ? O.[[GetOwnProperty]](key).
//          ii. If desc is not undefined and desc.[[Enumerable]] is true, then
//              1. If kind is key, append key to properties.
//              2. Else,
//                  a. Let value be ? Get(O, key).
//                  b. If kind is value, append value to properties.
//                  c. Else,
//                      i. Assert: kind is key+value.
//                      ii. Let entry be ! CreateArrayFromList(« key, value »).
//                      iii. Append entry to properties.
//  4. Return properties.
//
// https://tc39.es/ecma262/#sec-enumerableownpropertynames
#[derive(Debug, PartialEq)]
pub enum EnumerationStyle {
    Key,
    Value,
    KeyPlusValue,
}
pub fn enumerable_own_property_names(agent: &mut Agent, obj: &Object, kind: EnumerationStyle) -> AltCompletion<Vec<ECMAScriptValue>> {
    let own_keys = obj.o.own_property_keys(agent)?;
    let mut properties: Vec<ECMAScriptValue> = vec![];
    for key in own_keys.into_iter() {
        if matches!(key, PropertyKey::String(_)) {
            if let Some(desc) = obj.o.get_own_property(agent, &key)? {
                if desc.enumerable {
                    if kind == EnumerationStyle::Key {
                        properties.push(ECMAScriptValue::from(key));
                    } else {
                        let value = get(agent, obj, &key)?;
                        if kind == EnumerationStyle::Value {
                            properties.push(value);
                        } else {
                            let entry = create_array_from_list(agent, &[key.into(), value]);
                            properties.push(entry.into());
                        }
                    }
                }
            }
        }
    }
    Ok(properties)
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
pub fn ordinary_object_create(agent: &mut Agent, proto: Option<Object>, additional_internal_slots_list: &[InternalSlotName]) -> Object {
    let mut slots = vec![InternalSlotName::Prototype, InternalSlotName::Extensible];
    slots.extend_from_slice(additional_internal_slots_list);
    let o = make_basic_object(agent, slots.as_slice(), proto);
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
pub fn ordinary_create_from_constructor(agent: &mut Agent, constructor: &Object, intrinsic_default_proto: IntrinsicId, internal_slots_list: &[InternalSlotName]) -> AltCompletion<Object> {
    let proto = get_prototype_from_constructor(agent, constructor, intrinsic_default_proto)?;
    Ok(ordinary_object_create(agent, Some(proto), internal_slots_list))
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
fn get_prototype_from_constructor(agent: &mut Agent, constructor: &Object, intrinsic_default_proto: IntrinsicId) -> AltCompletion<Object> {
    let proto = get(agent, constructor, &PropertyKey::from("prototype"))?;
    match proto {
        ECMAScriptValue::Object(obj) => Ok(obj),
        _ => {
            let realm = get_function_realm(agent, constructor)?;
            let proto = realm.borrow().intrinsics.get(intrinsic_default_proto);
            Ok(proto)
        }
    }
}

#[derive(Debug)]
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

    fn get_prototype_of(&self, agent: &mut Agent) -> AltCompletion<Option<Object>> {
        Err(create_type_error(agent, "get_prototype_of called on DeadObject"))
    }
    fn set_prototype_of(&self, agent: &mut Agent, _obj: Option<Object>) -> AltCompletion<bool> {
        Err(create_type_error(agent, "set_prototype_of called on DeadObject"))
    }
    fn is_extensible(&self, agent: &mut Agent) -> AltCompletion<bool> {
        Err(create_type_error(agent, "is_extensible called on DeadObject"))
    }
    fn prevent_extensions(&self, agent: &mut Agent) -> AltCompletion<bool> {
        Err(create_type_error(agent, "prevent_extensions called on DeadObject"))
    }
    fn get_own_property(&self, agent: &mut Agent, _key: &PropertyKey) -> AltCompletion<Option<PropertyDescriptor>> {
        Err(create_type_error(agent, "get_own_property called on DeadObject"))
    }
    fn define_own_property(&self, agent: &mut Agent, _key: PropertyKey, _desc: PotentialPropertyDescriptor) -> AltCompletion<bool> {
        Err(create_type_error(agent, "define_own_property called on DeadObject"))
    }
    fn has_property(&self, agent: &mut Agent, _key: &PropertyKey) -> AltCompletion<bool> {
        Err(create_type_error(agent, "has_property called on DeadObject"))
    }
    fn get(&self, agent: &mut Agent, _key: &PropertyKey, _receiver: &ECMAScriptValue) -> Completion {
        Err(create_type_error(agent, "get called on DeadObject"))
    }
    fn set(&self, agent: &mut Agent, _key: PropertyKey, _value: ECMAScriptValue, _receiver: &ECMAScriptValue) -> AltCompletion<bool> {
        Err(create_type_error(agent, "set called on DeadObject"))
    }
    fn delete(&self, agent: &mut Agent, _key: &PropertyKey) -> AltCompletion<bool> {
        Err(create_type_error(agent, "delete called on DeadObject"))
    }
    fn own_property_keys(&self, agent: &mut Agent) -> AltCompletion<Vec<PropertyKey>> {
        Err(create_type_error(agent, "own_property_keys called on DeadObject"))
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
pub fn set_immutable_prototype<'a, T>(agent: &mut Agent, o: T, val: Option<Object>) -> AltCompletion<bool>
where
    T: Into<&'a dyn ObjectInterface>,
{
    let obj = o.into();
    let current = obj.get_prototype_of(agent)?;
    Ok(current == val)
}

#[derive(Debug)]
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
    fn get_prototype_of(&self, _agent: &mut Agent) -> AltCompletion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }

    // [[SetPrototypeOf]] ( V )
    //
    // The [[SetPrototypeOf]] internal method of an immutable prototype exotic object O takes argument V (an Object or
    // null). It performs the following steps when called:
    //
    //  1. Return ? SetImmutablePrototype(O, V).
    fn set_prototype_of(&self, agent: &mut Agent, obj: Option<Object>) -> AltCompletion<bool> {
        set_immutable_prototype(agent, self, obj)
    }

    // [[IsExtensible]] ( )
    //
    // The [[IsExtensible]] internal method of an ordinary object O takes no arguments. It performs the following steps
    // when called:
    //
    //  1. Return ! OrdinaryIsExtensible(O).
    fn is_extensible(&self, _agent: &mut Agent) -> AltCompletion<bool> {
        Ok(ordinary_is_extensible(self))
    }

    // [[PreventExtensions]] ( )
    //
    // The [[PreventExtensions]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    //  1. Return ! OrdinaryPreventExtensions(O).
    fn prevent_extensions(&self, _agent: &mut Agent) -> AltCompletion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }

    // [[GetOwnProperty]] ( P )
    //
    // The [[GetOwnProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ! OrdinaryGetOwnProperty(O, P).
    fn get_own_property(&self, _agent: &mut Agent, key: &PropertyKey) -> AltCompletion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }

    // [[DefineOwnProperty]] ( P, Desc )
    //
    // The [[DefineOwnProperty]] internal method of an ordinary object O takes arguments P (a property key) and Desc (a
    // Property Descriptor). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryDefineOwnProperty(O, P, Desc).
    fn define_own_property(&self, agent: &mut Agent, key: PropertyKey, desc: PotentialPropertyDescriptor) -> AltCompletion<bool> {
        ordinary_define_own_property(agent, self, key, desc)
    }

    // [[HasProperty]] ( P )
    //
    // The [[HasProperty]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryHasProperty(O, P).
    fn has_property(&self, agent: &mut Agent, key: &PropertyKey) -> AltCompletion<bool> {
        ordinary_has_property(agent, self, key)
    }

    // [[Get]] ( P, Receiver )
    //
    // The [[Get]] internal method of an ordinary object O takes arguments P (a property key) and Receiver (an
    // ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinaryGet(O, P, Receiver).
    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion {
        ordinary_get(agent, self, key, receiver)
    }

    // [[Set]] ( P, V, Receiver )
    //
    // The [[Set]] internal method of an ordinary object O takes arguments P (a property key), V (an ECMAScript language
    // value), and Receiver (an ECMAScript language value). It performs the following steps when called:
    //
    //  1. Return ? OrdinarySet(O, P, V, Receiver).
    fn set(&self, agent: &mut Agent, key: PropertyKey, v: ECMAScriptValue, receiver: &ECMAScriptValue) -> AltCompletion<bool> {
        ordinary_set(agent, self, key, v, receiver)
    }

    // [[Delete]] ( P )
    //
    // The [[Delete]] internal method of an ordinary object O takes argument P (a property key). It performs the
    // following steps when called:
    //
    //  1. Return ? OrdinaryDelete(O, P).
    fn delete(&self, agent: &mut Agent, key: &PropertyKey) -> AltCompletion<bool> {
        ordinary_delete(agent, self, key)
    }

    // [[OwnPropertyKeys]] ( )
    //
    // The [[OwnPropertyKeys]] internal method of an ordinary object O takes no arguments. It performs the following
    // steps when called:
    //
    // 1. Return ! OrdinaryOwnPropertyKeys(O).
    fn own_property_keys(&self, agent: &mut Agent) -> AltCompletion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(agent, self))
    }
}

pub fn immutable_prototype_exotic_object_create(agent: &mut Agent, proto: Option<&Object>) -> Object {
    Object { o: Rc::new(ImmutablePrototypeExoticObject { data: RefCell::new(CommonObjectData::new(agent, proto.cloned(), true, &ORDINARY_OBJECT_SLOTS)) }) }
}

// GetFunctionRealm ( obj )
//
// The abstract operation GetFunctionRealm takes argument obj. It performs the following steps when called:
//
//      1. Assert: ! IsCallable(obj) is true.
//      2. If obj has a [[Realm]] internal slot, then
//          a. Return obj.[[Realm]].
//      3. If obj is a bound function exotic object, then
//          a. Let target be obj.[[BoundTargetFunction]].
//          b. Return ? GetFunctionRealm(target).
//      4. If obj is a Proxy exotic object, then
//          a. If obj.[[ProxyHandler]] is null, throw a TypeError exception.
//          b. Let proxyTarget be obj.[[ProxyTarget]].
//          c. Return ? GetFunctionRealm(proxyTarget).
//      5. Return the current Realm Record.
//
// NOTE     Step 5 will only be reached if obj is a non-standard function exotic object that does not have a [[Realm]]
//          internal slot.
pub fn get_function_realm(agent: &mut Agent, obj: &Object) -> AltCompletion<Rc<RefCell<Realm>>> {
    if let Some(f) = obj.o.to_function_obj() {
        Ok(f.function_data().borrow().realm.clone())
    } else if let Some(b) = obj.o.to_builtin_function_obj() {
        Ok(b.builtin_function_data().borrow().realm.clone())
    } else {
        // Since we don't check explicitly that a realm slot existed above, check to make sure that we only get here if
        // a realm slot was _not_ present.
        assert!(!obj.o.common_object_data().borrow().slots.contains(&InternalSlotName::Realm));

        // Add the bound-function check
        // Add the proxy check
        eprintln!("GetFunctionRealm: Skipping over bound-function and proxy checks...");

        Ok(agent.running_execution_context().unwrap().realm.clone())
    }
}

// PrivateElementFind ( O, P )
//
// The abstract operation PrivateElementFind takes arguments O (an Object) and P (a Private Name). It performs the
// following steps when called:
//
//  1. If O.[[PrivateElements]] contains a PrivateElement whose [[Key]] is P, then
//      a. Let entry be that PrivateElement.
//      b. Return entry.
//  2. Return empty.
pub fn private_element_find(o: &Object, p: &PrivateName) -> Option<Rc<PrivateElement>> {
    let cod = o.o.common_object_data().borrow();
    let item = cod.private_elements.iter().find(|&item| item.key == *p);
    item.cloned()
}

// PrivateFieldAdd ( O, P, value )
//
// The abstract operation PrivateFieldAdd takes arguments O (an Object), P (a Private Name), and value (an ECMAScript
// language value). It performs the following steps when called:
//
//  1. Let entry be ! PrivateElementFind(O, P).
//  2. If entry is not empty, throw a TypeError exception.
//  3. Append PrivateElement { [[Key]]: P, [[Kind]]: field, [[Value]]: value } to O.[[PrivateElements]].
pub fn private_field_add(agent: &mut Agent, obj: &Object, p: PrivateName, value: ECMAScriptValue) -> AltCompletion<()> {
    let entry = private_element_find(obj, &p);
    match entry {
        Some(_) => Err(create_type_error(agent, "PrivateName already defined")),
        None => {
            let elements = &mut obj.o.common_object_data().borrow_mut().private_elements;
            elements.push(Rc::new(PrivateElement { key: p, kind: PrivateElementKind::Field { value: RefCell::new(value) } }));
            Ok(())
        }
    }
}

// PrivateMethodOrAccessorAdd ( O, method )
//
// The abstract operation PrivateMethodOrAccessorAdd takes arguments O (an Object) and method (a PrivateElement). It
// performs the following steps when called:
//
//  1. Assert: method.[[Kind]] is either method or accessor.
//  2. Let entry be ! PrivateElementFind(O, method.[[Key]]).
//  3. If entry is not empty, throw a TypeError exception.
//  4. Append method to O.[[PrivateElements]].
//
// NOTE: The values for private methods and accessors are shared across instances. This step does not create a new copy
// of the method or accessor.
pub fn private_method_or_accessor_add(agent: &mut Agent, obj: &Object, method: Rc<PrivateElement>) -> AltCompletion<()> {
    if private_element_find(obj, &method.key).is_some() {
        Err(create_type_error(agent, "PrivateName already defined"))
    } else {
        obj.o.common_object_data().borrow_mut().private_elements.push(method);
        Ok(())
    }
}

// PrivateGet ( O, P )
//
// The abstract operation PrivateGet takes arguments O (an Object) and P (a Private Name). It performs the following
// steps when called:
//
//  1. Let entry be ! PrivateElementFind(O, P).
//  2. If entry is empty, throw a TypeError exception.
//  3. If entry.[[Kind]] is field or method, then
//      a. Return entry.[[Value]].
//  4. Assert: entry.[[Kind]] is accessor.
//  5. If entry.[[Get]] is undefined, throw a TypeError exception.
//  6. Let getter be entry.[[Get]].
//  7. Return ? Call(getter, O).
pub fn private_get(agent: &mut Agent, obj: &Object, pn: &PrivateName) -> Completion {
    match private_element_find(obj, pn) {
        None => Err(create_type_error(agent, "PrivateName not defined")),
        Some(pe) => match &pe.kind {
            PrivateElementKind::Field { value } => Ok(value.borrow().clone()),
            PrivateElementKind::Method { value } => Ok(value.clone()),
            PrivateElementKind::Accessor { get: None, set: _ } => Err(create_type_error(agent, "PrivateName has no getter")),
            PrivateElementKind::Accessor { get: Some(getter), set: _ } => call(agent, &ECMAScriptValue::from(getter), &ECMAScriptValue::from(obj), &[]),
        },
    }
}

// PrivateSet ( O, P, value )
//
// The abstract operation PrivateSet takes arguments O (an Object), P (a Private Name), and value (an ECMAScript
// language value). It performs the following steps when called:
//
//  1. Let entry be ! PrivateElementFind(O, P).
//  2. If entry is empty, throw a TypeError exception.
//  3. If entry.[[Kind]] is field, then
//      a. Set entry.[[Value]] to value.
//  4. Else if entry.[[Kind]] is method, then
//      a. Throw a TypeError exception.
//  5. Else,
//      a. Assert: entry.[[Kind]] is accessor.
//      b. If entry.[[Set]] is undefined, throw a TypeError exception.
//      c. Let setter be entry.[[Set]].
//      d. Perform ? Call(setter, O, « value »).
pub fn private_set(agent: &mut Agent, obj: &Object, pn: &PrivateName, v: ECMAScriptValue) -> AltCompletion<()> {
    match private_element_find(obj, pn) {
        None => Err(create_type_error(agent, "PrivateName not defined")),
        Some(pe) => match &pe.kind {
            PrivateElementKind::Field { value } => {
                *value.borrow_mut() = v;
                Ok(())
            }
            PrivateElementKind::Method { value: _ } => Err(create_type_error(agent, "PrivateName method may not be assigned")),
            PrivateElementKind::Accessor { get: _, set: None } => Err(create_type_error(agent, "PrivateName has no setter")),
            PrivateElementKind::Accessor { get: _, set: Some(setter) } => {
                call(agent, &ECMAScriptValue::from(setter), &ECMAScriptValue::from(obj), &[v])?;
                Ok(())
            }
        },
    }
}

#[cfg(test)]
mod tests;
