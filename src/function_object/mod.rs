use super::agent::Agent;
use super::cr::{AltCompletion, Completion};
//use super::errors::create_type_error;
use super::object::{
    /*ordinary_create_from_constructor,*/ ordinary_define_own_property, ordinary_delete, ordinary_get, ordinary_get_own_property, ordinary_get_prototype_of, ordinary_has_property,
    ordinary_is_extensible, ordinary_own_property_keys, ordinary_prevent_extensions, ordinary_set, ordinary_set_prototype_of, CommonObjectData, /*InternalSlotName,*/ Object,
    ObjectInterface, PotentialPropertyDescriptor, PropertyDescriptor,
};
//use super::realm::IntrinsicIdentifier;
use super::parser::parameter_lists::FormalParameters;
use super::realm::Realm;
use super::values::{ECMAScriptValue, PropertyKey};
//use super::parser::script::Script;
use super::environment_record::EnvironmentRecord;
use std::cell::RefCell;
use std::rc::Rc;

enum ScriptRecord {}
enum ModuleRecord {}

enum ConstructorKind {
    Base,
    Derived,
}

enum SorM {
    Script(ScriptRecord),
    Module(ModuleRecord),
}

#[derive(PartialEq)]
pub enum ThisMode {
    Lexical,
    Strict,
    Global,
}

struct Script {} // should be in the parser

pub struct FunctionObjectData {
    pub environment: Rc<dyn EnvironmentRecord>,
    formal_parameters: Rc<FormalParameters>,
    ecmascript_code: Rc<Script>,
    constructor_kind: ConstructorKind,
    realm: Rc<Realm>,
    script_or_module: Option<SorM>,
    pub this_mode: ThisMode,
    strict: bool,
    pub home_object: Option<Object>,
    source_text: String,
    is_class_constructor: bool,
}

pub struct FunctionObject {
    common: RefCell<CommonObjectData>,
    function_data: RefCell<FunctionObjectData>,
}

impl<'a> From<&'a FunctionObject> for &'a dyn ObjectInterface {
    fn from(obj: &'a FunctionObject) -> Self {
        obj
    }
}

pub trait CallableObject: ObjectInterface {
    fn function_data(&self) -> &RefCell<FunctionObjectData>;
    fn call(&self, agent: &mut Agent, this_argument: &ECMAScriptValue, arguments_list: &[ECMAScriptValue]) -> Completion;
}

pub trait ConstructableObject: CallableObject {
    fn construct(&self, agent: &mut Agent, arguments_list: &[ECMAScriptValue], new_target: &dyn ConstructableObject) -> Completion;
}

impl ObjectInterface for FunctionObject {
    fn common_object_data(&self) -> &RefCell<CommonObjectData> {
        &self.common
    }
    fn is_ordinary(&self) -> bool {
        true
    }
    fn id(&self) -> usize {
        self.common.borrow().objid
    }
    fn to_function_obj(&self) -> Option<&dyn CallableObject> {
        Some(self)
    }

    fn get_prototype_of(&self) -> AltCompletion<Option<Object>> {
        Ok(ordinary_get_prototype_of(self))
    }
    fn set_prototype_of(&self, obj: Option<&Object>) -> AltCompletion<bool> {
        Ok(ordinary_set_prototype_of(self, obj))
    }
    fn is_extensible(&self) -> AltCompletion<bool> {
        Ok(ordinary_is_extensible(self))
    }
    fn prevent_extensions(&self) -> AltCompletion<bool> {
        Ok(ordinary_prevent_extensions(self))
    }
    fn get_own_property(&self, key: &PropertyKey) -> AltCompletion<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(self, key))
    }
    fn define_own_property(&self, key: &PropertyKey, desc: &PotentialPropertyDescriptor) -> AltCompletion<bool> {
        ordinary_define_own_property(self, key, desc)
    }
    fn has_property(&self, key: &PropertyKey) -> AltCompletion<bool> {
        ordinary_has_property(self, key)
    }
    fn get(&self, agent: &mut Agent, key: &PropertyKey, receiver: &ECMAScriptValue) -> Completion {
        ordinary_get(self, agent, key, receiver)
    }
    fn set(&self, agent: &mut Agent, key: &PropertyKey, v: &ECMAScriptValue, receiver: &ECMAScriptValue) -> AltCompletion<bool> {
        ordinary_set(self, agent, key, v, receiver)
    }
    fn delete(&self, key: &PropertyKey) -> AltCompletion<bool> {
        ordinary_delete(self, key)
    }
    fn own_property_keys(&self) -> AltCompletion<Vec<PropertyKey>> {
        Ok(ordinary_own_property_keys(self))
    }
}

impl CallableObject for FunctionObject {
    fn function_data(&self) -> &RefCell<FunctionObjectData> {
        &self.function_data
    }
    fn call(&self, _agent: &mut Agent, _this_argument: &ECMAScriptValue, _arguments_list: &[ECMAScriptValue]) -> Completion {
        todo!()
    }
}
