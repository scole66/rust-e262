use super::*;
//use crate::strings::JSString;

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
