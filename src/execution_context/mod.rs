use super::agent::Agent;
use super::object::Object;
use super::realm::Realm;

pub struct ExecutionContext {
    pub realm: Realm,
}

pub fn get_global_object(agent: &mut Agent) -> Object {
    todo!()
}
