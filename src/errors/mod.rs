use super::agent::Agent;
use super::cr::{AbruptCompletion, AltCompletion, Completion, CompletionInfo};
use super::object::{
    define_property_or_throw, ordinary_create_from_constructor, ordinary_define_own_property, ordinary_delete, ordinary_get, ordinary_get_own_property, ordinary_get_prototype_of,
    ordinary_has_property, ordinary_is_extensible, ordinary_own_property_keys, ordinary_prevent_extensions, ordinary_set, ordinary_set_prototype_of, CommonObjectData, InternalSlotName,
    Object, ObjectInterface, PotentialPropertyDescriptor, PropertyDescriptor, ERROR_OBJECT_SLOTS,
};
use super::realm::IntrinsicId;
use super::strings::JSString;
use super::values::{ECMAScriptValue, PropertyKey};
use std::cell::RefCell;
use std::rc::Rc;

fn create_native_error_object(agent: &mut Agent, message: &str, error_constructor: Object, proto_id: IntrinsicId) -> Object {
    let o = ordinary_create_from_constructor(agent, &error_constructor, proto_id, &[InternalSlotName::ErrorData]).unwrap();
    let desc = PotentialPropertyDescriptor {
        value: Some(ECMAScriptValue::String(JSString::from(message))),
        writable: Some(true),
        enumerable: Some(false),
        configurable: Some(true),
        ..Default::default()
    };
    define_property_or_throw(agent, &o, PropertyKey::from("message"), desc).unwrap();
    o
}

pub fn create_type_error_object(agent: &mut Agent, message: &str) -> Object {
    let error_constructor = agent.intrinsic(IntrinsicId::TypeError);
    create_native_error_object(agent, message, error_constructor, IntrinsicId::TypeErrorPrototype)
}

pub fn create_type_error(agent: &mut Agent, message: &str) -> AbruptCompletion {
    AbruptCompletion::Throw(CompletionInfo { value: Some(ECMAScriptValue::Object(create_type_error_object(agent, message))), target: None })
}

pub fn create_reference_error_object(agent: &mut Agent, message: &str) -> Object {
    let cstr = agent.intrinsic(IntrinsicId::ReferenceError);
    create_native_error_object(agent, message, cstr, IntrinsicId::ReferenceErrorPrototype)
}

pub fn create_reference_error(agent: &mut Agent, message: &str) -> AbruptCompletion {
    AbruptCompletion::Throw(CompletionInfo { value: Some(ECMAScriptValue::Object(create_reference_error_object(agent, message))), target: None })
}

pub fn create_syntax_error_object(agent: &mut Agent, message: &str) -> Object {
    let cstr = agent.intrinsic(IntrinsicId::SyntaxError);
    create_native_error_object(agent, message, cstr, IntrinsicId::SyntaxErrorPrototype)
}

pub fn create_syntax_error(agent: &mut Agent, message: &str) -> AbruptCompletion {
    AbruptCompletion::Throw(CompletionInfo { value: Some(ECMAScriptValue::Object(create_syntax_error_object(agent, message))), target: None })
}

#[derive(Debug)]
pub struct ErrorObject {
    common: RefCell<CommonObjectData>,
}

impl ErrorObject {
    pub fn object(agent: &mut Agent, prototype: Option<Object>) -> Object {
        Object { o: Rc::new(Self { common: RefCell::new(CommonObjectData::new(agent, prototype, true, &ERROR_OBJECT_SLOTS)) }) }
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
    fn is_error_object(&self) -> bool {
        true
    }

    fn get_prototype_of(&self, _agent: &mut Agent) -> AltCompletion<Option<Object>> {
        Ok(ordinary_get_prototype_of(&*self))
    }
    fn set_prototype_of(&self, _agent: &mut Agent, obj: Option<Object>) -> AltCompletion<bool> {
        Ok(ordinary_set_prototype_of(&*self, obj))
    }
    fn is_extensible(&self, _agent: &mut Agent) -> AltCompletion<bool> {
        Ok(ordinary_is_extensible(&*self))
    }
    fn prevent_extensions(&self, _agent: &mut Agent) -> AltCompletion<bool> {
        Ok(ordinary_prevent_extensions(&*self))
    }
    fn get_own_property(&self, _agent: &mut Agent, key: &PropertyKey) -> AltCompletion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(&*self, key))
    }
    fn define_own_property(&self, agent: &mut Agent, key: PropertyKey, desc: PotentialPropertyDescriptor) -> AltCompletion<bool> {
        ordinary_define_own_property(agent, &*self, key, desc)
    }
    fn has_property(&self, agent: &mut Agent, key: &PropertyKey) -> AltCompletion<bool> {
        ordinary_has_property(agent, &*self, key)
    }
    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion {
        ordinary_get(agent, &*self, key, receiver)
    }
    fn set(&self, agent: &mut Agent, key: &PropertyKey, v: &ECMAScriptValue, receiver: &ECMAScriptValue) -> AltCompletion<bool> {
        ordinary_set(agent, &*self, key, v, receiver)
    }
    fn delete(&self, agent: &mut Agent, key: &PropertyKey) -> AltCompletion<bool> {
        ordinary_delete(agent, &*self, key)
    }
    fn own_property_keys(&self, _agent: &mut Agent) -> AltCompletion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(self))
    }
}
