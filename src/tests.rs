use super::agent::Agent;
use super::cr::{AbruptCompletion, AltCompletion, Completion, CompletionInfo};
use super::errors::create_type_error;
use super::object::{
    get, ordinary_define_own_property, ordinary_delete, ordinary_get, ordinary_get_own_property, ordinary_get_prototype_of, ordinary_has_property, ordinary_is_extensible,
    ordinary_own_property_keys, ordinary_prevent_extensions, ordinary_set, ordinary_set_prototype_of, CommonObjectData, Object, ObjectInterface, PotentialPropertyDescriptor,
    PropertyDescriptor, ORDINARY_OBJECT_SLOTS,
};
use super::realm::IntrinsicId;
use super::values::{ECMAScriptValue, PropertyKey};
use ahash::RandomState;
use std::cell::RefCell;
use std::hash::{BuildHasher, Hash, Hasher};
use std::io::Result as IoResult;
use std::io::Write;
use std::rc::Rc;

pub struct MockWriter<T>
where
    T: Write,
{
    writer: T,
    pub count: usize,
    target: usize,
    pub error_generated: bool,
}
impl<T> std::io::Write for MockWriter<T>
where
    T: Write,
{
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.count += 1;
        if self.count >= self.target {
            self.error_generated = true;
            Err(std::io::Error::new(std::io::ErrorKind::Other, "oh no!"))
        } else {
            self.writer.write(buf)
        }
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.writer.flush()
    }
}
impl<T> MockWriter<T>
where
    T: Write,
{
    pub fn new(writer: T, errat: usize) -> Self {
        MockWriter { writer, count: 0, target: errat, error_generated: false }
    }
}
pub fn printer_validate<U>(func: U)
where
    U: Fn(&mut MockWriter<Vec<u8>>) -> IoResult<()>,
{
    let mut target = 1;
    loop {
        let mut writer = MockWriter::new(Vec::new(), target);
        let result = func(&mut writer);
        assert!(result.is_err() || !writer.error_generated);
        if !writer.error_generated {
            break;
        }
        target += 1;
    }
}

pub fn unwind_error_object(agent: &mut Agent, kind: &str, err: Object) -> String {
    assert!(err.o.to_error_obj().is_some());
    let name = get(agent, &err, &PropertyKey::from("name")).expect("Error object was missing 'name' property");
    assert!(matches!(name, ECMAScriptValue::String(_)));
    if let ECMAScriptValue::String(name_value) = name {
        assert_eq!(name_value, kind);
    }
    let message = get(agent, &err, &PropertyKey::from("message")).expect("Error object was missing 'message' property");
    assert!(matches!(message, ECMAScriptValue::String(_)));
    if let ECMAScriptValue::String(message_value) = message {
        String::from(message_value)
    } else {
        unreachable!()
    }
}

pub fn unwind_error(agent: &mut Agent, kind: &str, completion: AbruptCompletion) -> String {
    assert!(matches!(completion, AbruptCompletion::Throw(CompletionInfo { value: Some(ECMAScriptValue::Object(_)), target: None })));
    if let AbruptCompletion::Throw(CompletionInfo { value: Some(ECMAScriptValue::Object(err)), target: None }) = completion {
        unwind_error_object(agent, kind, err)
    } else {
        unreachable!()
    }
}

pub fn unwind_type_error(agent: &mut Agent, completion: AbruptCompletion) -> String {
    unwind_error(agent, "TypeError", completion)
}

pub fn unwind_syntax_error(agent: &mut Agent, completion: AbruptCompletion) -> String {
    unwind_error(agent, "SyntaxError", completion)
}

pub fn unwind_syntax_error_object(agent: &mut Agent, err: Object) -> String {
    unwind_error_object(agent, "SyntaxError", err)
}

pub fn unwind_reference_error(agent: &mut Agent, completion: AbruptCompletion) -> String {
    unwind_error(agent, "ReferenceError", completion)
}

pub fn unwind_reference_error_object(agent: &mut Agent, err: Object) -> String {
    unwind_error_object(agent, "ReferenceError", err)
}

pub fn calculate_hash<T: Hash>(factory: &RandomState, t: &T) -> u64 {
    let mut s = factory.build_hasher();
    t.hash(&mut s);
    s.finish()
}

pub fn test_agent() -> Agent {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    agent
}

#[derive(Debug)]
pub struct TestObject {
    common: RefCell<CommonObjectData>,
    get_prototype_of_throws: bool,
    set_prototype_of_throws: bool,
    is_extensible_throws: bool,
    prevent_extensions_throws: bool,
    get_own_property_throws: bool,
    define_own_property_throws: bool,
    has_property_throws: bool,
    get_throws: bool,
    set_throws: bool,
    delete_throws: bool,
    own_property_keys_throws: bool,
}

impl<'a> From<&'a TestObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a TestObject) -> Self {
        obj
    }
}

impl ObjectInterface for TestObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn is_ordinary(&self) -> bool {
        !(self.get_prototype_of_throws || self.set_prototype_of_throws)
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }

    fn get_prototype_of(&self, agent: &mut Agent) -> AltCompletion<Option<Object>> {
        if self.get_prototype_of_throws {
            Err(create_type_error(agent, "[[GetPrototypeOf]] called on TestObject"))
        } else {
            Ok(ordinary_get_prototype_of(self))
        }
    }
    fn set_prototype_of(&self, agent: &mut Agent, obj: Option<Object>) -> AltCompletion<bool> {
        if self.set_prototype_of_throws {
            Err(create_type_error(agent, "[[SetPrototypeOf]] called on TestObject"))
        } else {
            Ok(ordinary_set_prototype_of(self, obj))
        }
    }
    fn is_extensible(&self, agent: &mut Agent) -> AltCompletion<bool> {
        if self.is_extensible_throws {
            Err(create_type_error(agent, "[[IsExtensible]] called on TestObject"))
        } else {
            Ok(ordinary_is_extensible(self))
        }
    }
    fn prevent_extensions(&self, agent: &mut Agent) -> AltCompletion<bool> {
        if self.prevent_extensions_throws {
            Err(create_type_error(agent, "[[PreventExtensions]] called on TestObject"))
        } else {
            Ok(ordinary_prevent_extensions(self))
        }
    }
    fn get_own_property(&self, agent: &mut Agent, key: &PropertyKey) -> AltCompletion<Option<PropertyDescriptor>> {
        if self.get_own_property_throws {
            Err(create_type_error(agent, "[[GetOwnProperty]] called on TestObject"))
        } else {
            Ok(ordinary_get_own_property(self, key))
        }
    }
    fn define_own_property(&self, agent: &mut Agent, key: PropertyKey, desc: PotentialPropertyDescriptor) -> AltCompletion<bool> {
        if self.define_own_property_throws {
            Err(create_type_error(agent, "[[DefineOwnProperty]] called on TestObject"))
        } else {
            ordinary_define_own_property(agent, self, key, desc)
        }
    }
    fn has_property(&self, agent: &mut Agent, key: &PropertyKey) -> AltCompletion<bool> {
        if self.has_property_throws {
            Err(create_type_error(agent, "[[HasProperty]] called on TestObject"))
        } else {
            ordinary_has_property(agent, self, key)
        }
    }
    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion {
        if self.get_throws {
            Err(create_type_error(agent, "[[Get]] called on TestObject"))
        } else {
            ordinary_get(agent, self, key, receiver)
        }
    }
    fn set(&self, agent: &mut Agent, key: &PropertyKey, value: &ECMAScriptValue, receiver: &ECMAScriptValue) -> AltCompletion<bool> {
        if self.set_throws {
            Err(create_type_error(agent, "[[Set]] called on TestObject"))
        } else {
            ordinary_set(agent, self, key, value, receiver)
        }
    }
    fn delete(&self, agent: &mut Agent, key: &PropertyKey) -> AltCompletion<bool> {
        if self.delete_throws {
            Err(create_type_error(agent, "[[Delete]] called on TestObject"))
        } else {
            ordinary_delete(agent, self, key)
        }
    }
    fn own_property_keys(&self, agent: &mut Agent) -> AltCompletion<Vec<PropertyKey>> {
        if self.own_property_keys_throws {
            Err(create_type_error(agent, "[[OwnPropertyKeys]] called on TestObject"))
        } else {
            Ok(ordinary_own_property_keys(self))
        }
    }
}

#[derive(PartialEq)]
pub enum FunctionId {
    GetPrototypeOf,
    SetPrototypeOf,
    IsExtensible,
    PreventExtensions,
    GetOwnProperty,
    DefineOwnProperty,
    HasProperty,
    Get,
    Set,
    Delete,
    OwnPropertyKeys,
}

impl TestObject {
    pub fn object(agent: &mut Agent, throwers: &[FunctionId]) -> Object {
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        Object {
            o: Rc::new(Self {
                common: RefCell::new(CommonObjectData::new(agent, Some(prototype), true, &ORDINARY_OBJECT_SLOTS)),
                get_prototype_of_throws: throwers.contains(&FunctionId::GetPrototypeOf),
                set_prototype_of_throws: throwers.contains(&FunctionId::SetPrototypeOf),
                is_extensible_throws: throwers.contains(&FunctionId::IsExtensible),
                prevent_extensions_throws: throwers.contains(&FunctionId::PreventExtensions),
                get_own_property_throws: throwers.contains(&FunctionId::GetOwnProperty),
                define_own_property_throws: throwers.contains(&FunctionId::DefineOwnProperty),
                has_property_throws: throwers.contains(&FunctionId::HasProperty),
                get_throws: throwers.contains(&FunctionId::Get),
                set_throws: throwers.contains(&FunctionId::Set),
                delete_throws: throwers.contains(&FunctionId::Delete),
                own_property_keys_throws: throwers.contains(&FunctionId::OwnPropertyKeys),
            }),
        }
    }
}
