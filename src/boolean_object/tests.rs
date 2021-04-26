use super::super::object::{ordinary_object_create, PropertyKind};
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

#[test]
fn get_prototype_of_01() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let bool_obj = create_boolean_object(&mut agent, true);
    let proto = bool_obj.o.get_prototype_of().unwrap().unwrap();
    assert_eq!(proto, agent.running_execution_context().unwrap().realm.intrinsics.boolean_prototype)
}

#[test]
fn set_prototype_of_01() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let bool_obj = create_boolean_object(&mut agent, true);
    let res = bool_obj.o.set_prototype_of(None).unwrap();
    assert!(res);
    assert!(bool_obj.o.get_prototype_of().unwrap().is_none());
}

#[test]
fn is_extensible_01() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let bool_obj = create_boolean_object(&mut agent, true);
    let res = bool_obj.o.is_extensible().unwrap();
    assert!(res);
}

#[test]
fn prevent_extensions_01() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let bool_obj = create_boolean_object(&mut agent, true);
    let res = bool_obj.o.prevent_extensions().unwrap();
    assert!(res);
    assert!(!bool_obj.o.is_extensible().unwrap());
}

#[test]
fn define_and_get_own_property_01() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let bool_obj = create_boolean_object(&mut agent, true);
    let res =
        bool_obj.o.define_own_property(&PropertyKey::from("rust"), &PotentialPropertyDescriptor { value: Some(ECMAScriptValue::String("is awesome".into())), ..Default::default() }).unwrap();
    assert!(res);
    let val = bool_obj.o.get_own_property(&PropertyKey::from("rust")).unwrap().unwrap();
    assert_eq!(val.enumerable, false);
    assert_eq!(val.configurable, false);
    assert!(matches!(val.property, PropertyKind::Data(..)));
    if let PropertyKind::Data(d) = val.property {
        assert_eq!(d.value, ECMAScriptValue::String("is awesome".into()));
        assert_eq!(d.writable, false);
    }
}

#[test]
fn has_property_01() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let bool_obj = create_boolean_object(&mut agent, true);
    let res = bool_obj.o.has_property(&PropertyKey::from("rust")).unwrap();
    assert_eq!(res, false);
    let res2 = bool_obj.o.has_property(&PropertyKey::from("constructor")).unwrap();
    assert_eq!(res2, true);
}

#[test]
fn get_01() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let bool_obj = create_boolean_object(&mut agent, true);
    let res = bool_obj.o.get(&mut agent, &PropertyKey::from("rust"), &ECMAScriptValue::Undefined).unwrap();
    assert_eq!(res, ECMAScriptValue::Undefined);
    let res2 = bool_obj.o.get(&mut agent, &PropertyKey::from("constructor"), &ECMAScriptValue::Undefined).unwrap();
    assert!(matches!(res2, ECMAScriptValue::Object(..)));
    if let ECMAScriptValue::Object(obj) = res2 {
        assert_eq!(obj, agent.running_execution_context().unwrap().realm.intrinsics.boolean);
    }
}

#[test]
fn set_01() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let bool_obj = create_boolean_object(&mut agent, true);
    let receiver = ECMAScriptValue::Object(bool_obj.clone());
    let res = bool_obj.o.set(&mut agent, &PropertyKey::from("rust"), &ECMAScriptValue::Null, &receiver).unwrap();
    assert_eq!(res, true);
}

#[test]
fn delete_01() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let bool_obj = create_boolean_object(&mut agent, true);
    let res = bool_obj.o.delete(&PropertyKey::from("rust")).unwrap();
    assert_eq!(res, true);
}

#[test]
fn own_keys_01() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let bool_obj = create_boolean_object(&mut agent, true);
    let res = bool_obj.o.own_property_keys().unwrap();
    assert!(res.is_empty())
}

#[test]
fn is_ordinary_01() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let bool_obj = create_boolean_object(&mut agent, true);
    assert_eq!(bool_obj.o.is_ordinary(), true);
}
