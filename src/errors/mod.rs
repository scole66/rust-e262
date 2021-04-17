use super::agent::Agent;
use super::cr::{AbruptCompletion, Completion, CompletionInfo};
use super::object::{
    define_property_or_throw, ordinary_create_from_constructor, ordinary_define_own_property, ordinary_delete, ordinary_get, ordinary_get_own_property, ordinary_get_prototype_of,
    ordinary_has_property, ordinary_is_extensible, ordinary_own_property_keys, ordinary_prevent_extensions, ordinary_set, ordinary_set_prototype_of, CommonObjectData, InternalSlotName,
    Object, ObjectInterface, PotentialPropertyDescriptor, PropertyDescriptor,
};
use super::realm::IntrinsicIdentifier;
use super::strings::JSString;
use super::values::{ECMAScriptValue, PropertyKey};
use std::cell::RefCell;
use std::rc::Rc;

fn create_native_error_object(agent: &mut Agent, message: &str, error_constructor: Object, proto_id: IntrinsicIdentifier) -> Object {
    let o = ordinary_create_from_constructor(agent, &error_constructor, proto_id, &[InternalSlotName::ErrorData]).unwrap();
    let desc = PotentialPropertyDescriptor {
        value: Some(ECMAScriptValue::String(JSString::from(message))),
        writable: Some(true),
        enumerable: Some(false),
        configurable: Some(true),
        ..Default::default()
    };
    define_property_or_throw(agent, &o, &PropertyKey::from("message"), &desc).unwrap();
    o
}

pub fn create_type_error_object(agent: &mut Agent, message: &str) -> Object {
    let error_constructor = agent.running_execution_context().unwrap().realm.intrinsics.type_error.clone();
    create_native_error_object(agent, message, error_constructor, IntrinsicIdentifier::TypeErrorPrototype)
}

pub fn create_type_error(agent: &mut Agent, message: &str) -> AbruptCompletion {
    AbruptCompletion::Throw(CompletionInfo { value: Some(ECMAScriptValue::Object(create_type_error_object(agent, message))), target: None })
}

pub fn create_reference_error_object(agent: &mut Agent, message: &str) -> Object {
    let cstr = agent.running_execution_context().unwrap().realm.intrinsics.reference_error.clone();
    create_native_error_object(agent, message, cstr, IntrinsicIdentifier::ReferenceErrorPrototype)
}
pub fn create_reference_error(agent: &mut Agent, message: &str) -> AbruptCompletion {
    AbruptCompletion::Throw(CompletionInfo{ value: Some(ECMAScriptValue::Object(create_reference_error_object(agent, message))), target: None})
}

pub struct ErrorObject {
    common: RefCell<CommonObjectData>,
}

impl ErrorObject {
    pub fn object(agent: &mut Agent) -> Object {
        Object { o: Rc::new(Self { common: RefCell::new(CommonObjectData::new(agent)) }) }
    }
}

impl<'a> From<&'a ErrorObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a ErrorObject) -> Self {
        obj
    }
}

impl ObjectInterface for ErrorObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn is_ordinary(&self) -> bool {
        true
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }
    fn to_error_obj(&self) -> Option<&dyn ObjectInterface> {
        Some(self)
    }

    fn get_prototype_of(&self) -> Result<Option<Object>, AbruptCompletion> {
        Ok(ordinary_get_prototype_of(&*self))
    }
    fn set_prototype_of(&self, obj: Option<&Object>) -> Result<bool, AbruptCompletion> {
        Ok(ordinary_set_prototype_of(&*self, obj))
    }
    fn is_extensible(&self) -> Result<bool, AbruptCompletion> {
        Ok(ordinary_is_extensible(&*self))
    }
    fn prevent_extensions(&self) -> Result<bool, AbruptCompletion> {
        Ok(ordinary_prevent_extensions(&*self))
    }
    fn get_own_property(&self, key: &PropertyKey) -> Result<Option<PropertyDescriptor>, AbruptCompletion> {
        Ok(ordinary_get_own_property(&*self, key))
    }
    fn define_own_property(&self, key: &PropertyKey, desc: &PotentialPropertyDescriptor) -> Result<bool, AbruptCompletion> {
        ordinary_define_own_property(&*self, key, desc)
    }
    fn has_property(&self, key: &PropertyKey) -> Result<bool, AbruptCompletion> {
        ordinary_has_property(&*self, key)
    }
    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion {
        ordinary_get(&*self, agent, key, receiver)
    }
    fn set(&self, agent: &mut Agent, key: &PropertyKey, v: &ECMAScriptValue, receiver: &ECMAScriptValue) -> Result<bool, AbruptCompletion> {
        ordinary_set(&*self, agent, key, v, receiver)
    }
    fn delete(&self, key: &PropertyKey) -> Result<bool, AbruptCompletion> {
        ordinary_delete(&*self, key)
    }
    fn own_property_keys(&self) -> Result<Vec<PropertyKey>, AbruptCompletion> {
        Ok(ordinary_own_property_keys(self))
    }
}
