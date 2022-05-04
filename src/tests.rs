use super::agent::Agent;
use super::cr::{AbruptCompletion, Completion};
use super::errors::create_type_error;
use super::object::{
    get, ordinary_define_own_property, ordinary_delete, ordinary_get, ordinary_get_own_property, ordinary_get_prototype_of, ordinary_has_property, ordinary_is_extensible,
    ordinary_own_property_keys, ordinary_prevent_extensions, ordinary_set, ordinary_set_prototype_of, CommonObjectData, Object, ObjectInterface, PotentialPropertyDescriptor,
    PropertyDescriptor, ORDINARY_OBJECT_SLOTS,
};
use super::realm::IntrinsicId;
use super::values::{to_string, ECMAScriptValue, PropertyKey};
use ahash::RandomState;
use std::cell::{Cell, RefCell};
use std::fmt::{self, Debug};
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
    assert!(matches!(completion, AbruptCompletion::Throw { value: ECMAScriptValue::Object(_) }));
    if let AbruptCompletion::Throw { value: ECMAScriptValue::Object(err) } = completion {
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

pub fn unwind_range_error(agent: &mut Agent, completion: AbruptCompletion) -> String {
    unwind_error(agent, "RangeError", completion)
}

pub fn unwind_range_error_object(agent: &mut Agent, err: Object) -> String {
    unwind_error_object(agent, "RangeError", err)
}

pub fn calculate_hash<T: Hash>(factory: &RandomState, t: &T) -> u64 {
    let mut s = factory.build_hasher();
    t.hash(&mut s);
    s.finish()
}

pub fn test_agent() -> Agent {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm(true);
    agent
}

#[derive(Debug)]
pub struct TestObject {
    common: RefCell<CommonObjectData>,
    get_prototype_of_throws: bool,
    set_prototype_of_throws: bool,
    is_extensible_throws: bool,
    prevent_extensions_throws: bool,
    get_own_property_throws: (bool, Option<PropertyKey>),
    define_own_property_throws: (bool, Option<PropertyKey>),
    has_property_throws: (bool, Option<PropertyKey>),
    get_throws: (bool, Option<PropertyKey>),
    set_throws: (bool, Option<PropertyKey>),
    delete_throws: (bool, Option<PropertyKey>),
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

    fn get_prototype_of(&self, agent: &mut Agent) -> Completion<Option<Object>> {
        if self.get_prototype_of_throws {
            Err(create_type_error(agent, "[[GetPrototypeOf]] called on TestObject"))
        } else {
            Ok(ordinary_get_prototype_of(self))
        }
    }
    fn set_prototype_of(&self, agent: &mut Agent, obj: Option<Object>) -> Completion<bool> {
        if self.set_prototype_of_throws {
            Err(create_type_error(agent, "[[SetPrototypeOf]] called on TestObject"))
        } else {
            Ok(ordinary_set_prototype_of(self, obj))
        }
    }
    fn is_extensible(&self, agent: &mut Agent) -> Completion<bool> {
        if self.is_extensible_throws {
            Err(create_type_error(agent, "[[IsExtensible]] called on TestObject"))
        } else {
            Ok(ordinary_is_extensible(self))
        }
    }
    fn prevent_extensions(&self, agent: &mut Agent) -> Completion<bool> {
        if self.prevent_extensions_throws {
            Err(create_type_error(agent, "[[PreventExtensions]] called on TestObject"))
        } else {
            Ok(ordinary_prevent_extensions(self))
        }
    }
    fn get_own_property(&self, agent: &mut Agent, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        if self.get_own_property_throws.0 && self.get_own_property_throws.1.as_ref().map_or(true, |k| *k == *key) {
            Err(create_type_error(agent, "[[GetOwnProperty]] called on TestObject"))
        } else {
            Ok(ordinary_get_own_property(self, key))
        }
    }
    fn define_own_property(&self, agent: &mut Agent, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool> {
        if self.define_own_property_throws.0 && self.define_own_property_throws.1.as_ref().map_or(true, |k| *k == key) {
            Err(create_type_error(agent, "[[DefineOwnProperty]] called on TestObject"))
        } else {
            ordinary_define_own_property(agent, self, key, desc)
        }
    }
    fn has_property(&self, agent: &mut Agent, key: &PropertyKey) -> Completion<bool> {
        if self.has_property_throws.0 && self.has_property_throws.1.as_ref().map_or(true, |k| *k == *key) {
            Err(create_type_error(agent, "[[HasProperty]] called on TestObject"))
        } else {
            ordinary_has_property(agent, self, key)
        }
    }
    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        if self.get_throws.0 && self.get_throws.1.as_ref().map_or(true, |k| *k == *key) {
            Err(create_type_error(agent, "[[Get]] called on TestObject"))
        } else {
            ordinary_get(agent, self, key, receiver)
        }
    }
    fn set(&self, agent: &mut Agent, key: PropertyKey, value: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        if self.set_throws.0 && self.set_throws.1.as_ref().map_or(true, |k| *k == key) {
            Err(create_type_error(agent, "[[Set]] called on TestObject"))
        } else {
            ordinary_set(agent, self, key, value, receiver)
        }
    }
    fn delete(&self, agent: &mut Agent, key: &PropertyKey) -> Completion<bool> {
        if self.delete_throws.0 && self.delete_throws.1.as_ref().map_or(true, |k| *k == *key) {
            Err(create_type_error(agent, "[[Delete]] called on TestObject"))
        } else {
            ordinary_delete(agent, self, key)
        }
    }
    fn own_property_keys(&self, agent: &mut Agent) -> Completion<Vec<PropertyKey>> {
        if self.own_property_keys_throws {
            Err(create_type_error(agent, "[[OwnPropertyKeys]] called on TestObject"))
        } else {
            Ok(ordinary_own_property_keys(agent, self))
        }
    }
}

#[derive(PartialEq)]
pub enum FunctionId {
    GetPrototypeOf,
    SetPrototypeOf,
    IsExtensible,
    PreventExtensions,
    GetOwnProperty(Option<PropertyKey>),
    DefineOwnProperty(Option<PropertyKey>),
    HasProperty(Option<PropertyKey>),
    Get(Option<PropertyKey>),
    Set(Option<PropertyKey>),
    Delete(Option<PropertyKey>),
    OwnPropertyKeys,
}

macro_rules! matcher_gen {
    ( $name:ident, $ftype:ident ) => {
        fn $name(item: &FunctionId) -> (bool, Option<PropertyKey>) {
            match item {
                FunctionId::$ftype(rval) => (true, rval.clone()),
                _ => (false, None),
            }
        }
    };
}
impl TestObject {
    matcher_gen!(get_match, Get);
    matcher_gen!(set_match, Set);
    matcher_gen!(get_own_match, GetOwnProperty);
    matcher_gen!(define_own_match, DefineOwnProperty);
    matcher_gen!(has_prop_match, HasProperty);
    matcher_gen!(delete_match, Delete);
    fn check_for_key(matcher: fn(&FunctionId) -> (bool, Option<PropertyKey>), throwers: &[FunctionId]) -> (bool, Option<PropertyKey>) {
        for item in throwers {
            match matcher(item) {
                (true, rval) => {
                    return (true, rval);
                }
                (false, _) => {}
            }
        }
        (false, None)
    }
    pub fn object(agent: &mut Agent, throwers: &[FunctionId]) -> Object {
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        Object {
            o: Rc::new(Self {
                common: RefCell::new(CommonObjectData::new(agent, Some(prototype), true, ORDINARY_OBJECT_SLOTS)),
                get_prototype_of_throws: throwers.contains(&FunctionId::GetPrototypeOf),
                set_prototype_of_throws: throwers.contains(&FunctionId::SetPrototypeOf),
                is_extensible_throws: throwers.contains(&FunctionId::IsExtensible),
                prevent_extensions_throws: throwers.contains(&FunctionId::PreventExtensions),
                get_own_property_throws: TestObject::check_for_key(TestObject::get_own_match, throwers),
                define_own_property_throws: TestObject::check_for_key(TestObject::define_own_match, throwers),
                has_property_throws: TestObject::check_for_key(TestObject::has_prop_match, throwers),
                get_throws: TestObject::check_for_key(TestObject::get_match, throwers),
                set_throws: TestObject::check_for_key(TestObject::set_match, throwers),
                delete_throws: TestObject::check_for_key(TestObject::delete_match, throwers),
                own_property_keys_throws: throwers.contains(&FunctionId::OwnPropertyKeys),
            }),
        }
    }
}

type GetPrototypeOfFunction = fn(agent: &mut Agent, this: &AdaptableObject) -> Completion<Option<Object>>;
type SetPrototypeOfFunction = fn(agent: &mut Agent, this: &AdaptableObject, obj: Option<Object>) -> Completion<bool>;
type IsExtensibleFunction = fn(agent: &mut Agent, this: &AdaptableObject) -> Completion<bool>;
type PreventExtensionsFunction = fn(agent: &mut Agent, this: &AdaptableObject) -> Completion<bool>;
type GetOwnPropertyFunction = fn(agent: &mut Agent, this: &AdaptableObject, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>>;
type DefineOwnPropertyFunction = fn(agent: &mut Agent, this: &AdaptableObject, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool>;
type HasPropertyFunction = fn(agent: &mut Agent, this: &AdaptableObject, key: &PropertyKey) -> Completion<bool>;
type GetFunction = fn(agent: &mut Agent, this: &AdaptableObject, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue>;
type SetFunction = fn(agent: &mut Agent, this: &AdaptableObject, key: PropertyKey, value: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool>;
type DeleteFunction = fn(agent: &mut Agent, this: &AdaptableObject, key: &PropertyKey) -> Completion<bool>;
type OwnPropertyKeysFunction = fn(agent: &mut Agent, this: &AdaptableObject) -> Completion<Vec<PropertyKey>>;

pub struct AdaptableObject {
    common: RefCell<CommonObjectData>,
    get_prototype_of_override: Option<GetPrototypeOfFunction>,
    set_prototype_of_override: Option<SetPrototypeOfFunction>,
    is_extensible_override: Option<IsExtensibleFunction>,
    prevent_extensions_override: Option<PreventExtensionsFunction>,
    get_own_property_override: Option<GetOwnPropertyFunction>,
    define_own_property_override: Option<DefineOwnPropertyFunction>,
    has_property_override: Option<HasPropertyFunction>,
    get_override: Option<GetFunction>,
    set_override: Option<SetFunction>,
    delete_override: Option<DeleteFunction>,
    own_property_keys_override: Option<OwnPropertyKeysFunction>,
    pub something: Cell<u64>, // Just a place for instances of this to hold state
}

impl fmt::Debug for AdaptableObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("AdaptableObject")
            .field("common", &self.common)
            .field("get_prototype_of_override", &self.get_prototype_of_override.and(Some("replacement function")))
            .field("set_prototype_of_override", &self.set_prototype_of_override.and(Some("replacement function")))
            .field("is_extensible_override", &self.is_extensible_override.and(Some("replacement function")))
            .field("prevent_extensions_override", &self.prevent_extensions_override.and(Some("replacement function")))
            .field("get_own_property_override", &self.get_own_property_override.and(Some("replacement function")))
            .field("define_own_property_override", &self.define_own_property_override.and(Some("replacement function")))
            .field("has_property_override", &self.has_property_override.and(Some("replacement function")))
            .field("get_override", &self.get_override.and(Some("replacement function")))
            .field("set_override", &self.set_override.and(Some("replacement function")))
            .field("delete_override", &self.delete_override.and(Some("replacement function")))
            .field("own_property_keys_override", &self.own_property_keys_override.and(Some("replacement function")))
            .finish()
    }
}

impl<'a> From<&'a AdaptableObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a AdaptableObject) -> Self {
        obj
    }
}

impl ObjectInterface for AdaptableObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn is_ordinary(&self) -> bool {
        self.get_prototype_of_override.is_none() && self.set_prototype_of_override.is_none()
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }

    fn get_prototype_of(&self, agent: &mut Agent) -> Completion<Option<Object>> {
        match &self.get_prototype_of_override {
            Some(func) => func(agent, self),
            None => Ok(ordinary_get_prototype_of(self)),
        }
    }

    fn set_prototype_of(&self, agent: &mut Agent, obj: Option<Object>) -> Completion<bool> {
        match &self.set_prototype_of_override {
            Some(func) => func(agent, self, obj),
            None => Ok(ordinary_set_prototype_of(self, obj)),
        }
    }
    fn is_extensible(&self, agent: &mut Agent) -> Completion<bool> {
        match &self.is_extensible_override {
            Some(func) => func(agent, self),
            None => Ok(ordinary_is_extensible(self)),
        }
    }
    fn prevent_extensions(&self, agent: &mut Agent) -> Completion<bool> {
        match &self.prevent_extensions_override {
            Some(func) => func(agent, self),
            None => Ok(ordinary_prevent_extensions(self)),
        }
    }
    fn get_own_property(&self, agent: &mut Agent, key: &PropertyKey) -> Completion<Option<PropertyDescriptor>> {
        match &self.get_own_property_override {
            Some(func) => func(agent, self, key),
            None => Ok(ordinary_get_own_property(self, key)),
        }
    }
    fn define_own_property(&self, agent: &mut Agent, key: PropertyKey, desc: PotentialPropertyDescriptor) -> Completion<bool> {
        match &self.define_own_property_override {
            Some(func) => func(agent, self, key, desc),
            None => ordinary_define_own_property(agent, self, key, desc),
        }
    }
    fn has_property(&self, agent: &mut Agent, key: &PropertyKey) -> Completion<bool> {
        match &self.has_property_override {
            Some(func) => func(agent, self, key),
            None => ordinary_has_property(agent, self, key),
        }
    }
    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion<ECMAScriptValue> {
        match &self.get_override {
            Some(func) => func(agent, self, key, receiver),
            None => ordinary_get(agent, self, key, receiver),
        }
    }
    fn set(&self, agent: &mut Agent, key: PropertyKey, value: ECMAScriptValue, receiver: &ECMAScriptValue) -> Completion<bool> {
        match &self.set_override {
            Some(func) => func(agent, self, key, value, receiver),
            None => ordinary_set(agent, self, key, value, receiver),
        }
    }
    fn delete(&self, agent: &mut Agent, key: &PropertyKey) -> Completion<bool> {
        match &self.delete_override {
            Some(func) => func(agent, self, key),
            None => ordinary_delete(agent, self, key),
        }
    }
    fn own_property_keys(&self, agent: &mut Agent) -> Completion<Vec<PropertyKey>> {
        match &self.own_property_keys_override {
            Some(func) => func(agent, self),
            None => Ok(ordinary_own_property_keys(agent, self)),
        }
    }
}

#[derive(Default)]
pub struct AdaptableMethods {
    pub get_prototype_of_override: Option<GetPrototypeOfFunction>,
    pub set_prototype_of_override: Option<SetPrototypeOfFunction>,
    pub is_extensible_override: Option<IsExtensibleFunction>,
    pub prevent_extensions_override: Option<PreventExtensionsFunction>,
    pub get_own_property_override: Option<GetOwnPropertyFunction>,
    pub define_own_property_override: Option<DefineOwnPropertyFunction>,
    pub has_property_override: Option<HasPropertyFunction>,
    pub get_override: Option<GetFunction>,
    pub set_override: Option<SetFunction>,
    pub delete_override: Option<DeleteFunction>,
    pub own_property_keys_override: Option<OwnPropertyKeysFunction>,
}

impl AdaptableObject {
    pub fn object(agent: &mut Agent, methods: AdaptableMethods) -> Object {
        let prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
        Object {
            o: Rc::new(Self {
                common: RefCell::new(CommonObjectData::new(agent, Some(prototype), true, ORDINARY_OBJECT_SLOTS)),
                get_prototype_of_override: methods.get_prototype_of_override,
                set_prototype_of_override: methods.set_prototype_of_override,
                is_extensible_override: methods.is_extensible_override,
                prevent_extensions_override: methods.prevent_extensions_override,
                get_own_property_override: methods.get_own_property_override,
                define_own_property_override: methods.define_own_property_override,
                has_property_override: methods.has_property_override,
                get_override: methods.get_override,
                set_override: methods.set_override,
                delete_override: methods.delete_override,
                own_property_keys_override: methods.own_property_keys_override,
                something: Cell::new(0),
            }),
        }
    }
}

// error
pub fn faux_errors(agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
    Err(create_type_error(agent, "Test Sentinel"))
}

use crate::object::define_property_or_throw;
use crate::realm::{create_realm, Realm};

pub fn create_named_realm(agent: &mut Agent, name: &str) -> Rc<RefCell<Realm>> {
    let r = create_realm(agent);
    let op = r.borrow().intrinsics.get(IntrinsicId::ObjectPrototype);
    define_property_or_throw(agent, &op, "name", PotentialPropertyDescriptor::new().value(name).writable(false).enumerable(false).configurable(false)).unwrap();

    r
}
pub fn get_realm_name(agent: &mut Agent, realm: &Realm) -> String {
    let op = realm.intrinsics.get(IntrinsicId::ObjectPrototype);
    let name = get(agent, &op, &"name".into()).unwrap();
    to_string(agent, name).unwrap().into()
}
