use crate::agent::Agent;
use crate::cr::Completion;
use crate::values::{ECMAScriptValue, PropertyKey};
use std::cell::RefCell;
use std::rc::Rc;

struct ObjectInner {
    // to be done
}

#[derive(Clone)]
pub struct Object {
    o: Rc<RefCell<ObjectInner>>,
}

impl Object {
    fn new() -> Self {
        Self { o: Rc::new(RefCell::new(ObjectInner {})) }
    }
}

// In "Operations on Objects"
pub fn get(agent: &mut Agent, obj: &Object, key: &PropertyKey) -> Completion {
    todo!()
}

// In "Operations on Object"
pub fn call(agent: &mut Agent, func: &ECMAScriptValue, receiver: &ECMAScriptValue, args: Option<Vec<ECMAScriptValue>>) -> Completion {
    todo!()
}

pub fn get_method(agent: &mut Agent, val: &ECMAScriptValue, key: &PropertyKey) -> Completion {
    todo!()
}
