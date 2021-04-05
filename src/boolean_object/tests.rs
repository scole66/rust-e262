use super::super::object::ordinary_object_create;
use super::super::tests::unwind_type_error;
use super::*;

#[test]
fn create_boolean_object_01() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let result = create_boolean_object(&mut agent, true);

    let maybe_native = result.o.to_boolean_obj();
    assert!(maybe_native.is_some());

    let native = maybe_native.unwrap();
    assert_eq!(*native.boolean_data().borrow(), true);
}

#[test]
fn create_boolean_object_02() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let result = create_boolean_object(&mut agent, false);

    let maybe_native = result.o.to_boolean_obj();
    assert!(maybe_native.is_some());

    let native = maybe_native.unwrap();
    assert_eq!(*native.boolean_data().borrow(), false);
}

#[test]
fn this_boolean_value_01() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let result = this_boolean_value(&mut agent, &ECMAScriptValue::Boolean(true));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
}
#[test]
fn this_boolean_value_02() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let result = this_boolean_value(&mut agent, &ECMAScriptValue::Boolean(false));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), false);
}
#[test]
fn this_boolean_value_03() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let result = this_boolean_value(&mut agent, &ECMAScriptValue::Null);
    assert!(result.is_err());
    let msg = unwind_type_error(&mut agent, result.unwrap_err());
    assert_eq!(msg, "Value is not boolean");
}
#[test]
fn this_boolean_value_04() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let bool_obj = create_boolean_object(&mut agent, true);
    let result = this_boolean_value(&mut agent, &ECMAScriptValue::Object(bool_obj));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
}
#[test]
fn this_boolean_value_05() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let bool_obj = create_boolean_object(&mut agent, false);
    let result = this_boolean_value(&mut agent, &ECMAScriptValue::Object(bool_obj));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), false);
}

#[test]
fn this_boolean_value_06() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let proto = agent.running_execution_context().unwrap().realm.intrinsics.object_prototype.clone();
    let other_obj = ordinary_object_create(&mut agent, Some(&proto), &[]);
    let result = this_boolean_value(&mut agent, &ECMAScriptValue::Object(other_obj));
    assert!(result.is_err());
    let msg = unwind_type_error(&mut agent, result.unwrap_err());
    assert_eq!(msg, "Object has no boolean value");
}
