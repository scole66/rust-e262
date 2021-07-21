use super::*;
use crate::object::get;
use crate::realm::IntrinsicId;
use crate::tests::{test_agent, unwind_type_error};

#[test]
fn create_native_error_object_01() {
    let mut agent = test_agent();
    let constructor = agent.intrinsic(IntrinsicId::RangeError);
    let message = "Great Googly Moogly!";
    let proto_id = IntrinsicId::RangeErrorPrototype;

    let result = create_native_error_object(&mut agent, message, constructor, proto_id);

    assert!(result.o.is_error_object());
    let msg_val = get(&mut agent, &result, &PropertyKey::from("message")).unwrap();
    assert_eq!(msg_val, ECMAScriptValue::from(message));
    let kind = get(&mut agent, &result, &PropertyKey::from("name")).unwrap();
    assert_eq!(kind, ECMAScriptValue::from("RangeError"));
}

#[test]
fn create_type_error_object_01() {
    let mut agent = test_agent();

    let result = create_type_error_object(&mut agent, "Happy Days");

    assert!(result.o.is_error_object());
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("TypeError"));
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("Happy Days"));
}

#[test]
fn create_type_error_01() {
    let mut agent = test_agent();

    let result = create_type_error(&mut agent, "A");
    assert!(matches!(result, AbruptCompletion::Throw(_)));
    if let AbruptCompletion::Throw(ci) = result {
        assert!(ci.target.is_none());
        let objval = ci.value.unwrap();
        assert!(objval.is_object());
        if let ECMAScriptValue::Object(obj) = objval {
            assert!(obj.o.is_error_object());
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("TypeError"));
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("A"));
        }
    }
}

#[test]
fn create_eval_error_object_01() {
    let mut agent = test_agent();

    let result = create_eval_error_object(&mut agent, "Happy Days");

    assert!(result.o.is_error_object());
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("EvalError"));
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("Happy Days"));
}

#[test]
fn create_eval_error_01() {
    let mut agent = test_agent();

    let result = create_eval_error(&mut agent, "A");
    assert!(matches!(result, AbruptCompletion::Throw(_)));
    if let AbruptCompletion::Throw(ci) = result {
        assert!(ci.target.is_none());
        let objval = ci.value.unwrap();
        assert!(objval.is_object());
        if let ECMAScriptValue::Object(obj) = objval {
            assert!(obj.o.is_error_object());
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("EvalError"));
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("A"));
        }
    }
}

#[test]
fn create_reference_error_object_01() {
    let mut agent = test_agent();

    let result = create_reference_error_object(&mut agent, "Happy Days");

    assert!(result.o.is_error_object());
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("ReferenceError"));
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("Happy Days"));
}

#[test]
fn create_reference_error_01() {
    let mut agent = test_agent();

    let result = create_reference_error(&mut agent, "A");
    assert!(matches!(result, AbruptCompletion::Throw(_)));
    if let AbruptCompletion::Throw(ci) = result {
        assert!(ci.target.is_none());
        let objval = ci.value.unwrap();
        assert!(objval.is_object());
        if let ECMAScriptValue::Object(obj) = objval {
            assert!(obj.o.is_error_object());
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("ReferenceError"));
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("A"));
        }
    }
}

#[test]
fn create_range_error_object_01() {
    let mut agent = test_agent();

    let result = create_range_error_object(&mut agent, "Happy Days");

    assert!(result.o.is_error_object());
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("RangeError"));
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("Happy Days"));
}

#[test]
fn create_range_error_01() {
    let mut agent = test_agent();

    let result = create_range_error(&mut agent, "A");
    assert!(matches!(result, AbruptCompletion::Throw(_)));
    if let AbruptCompletion::Throw(ci) = result {
        assert!(ci.target.is_none());
        let objval = ci.value.unwrap();
        assert!(objval.is_object());
        if let ECMAScriptValue::Object(obj) = objval {
            assert!(obj.o.is_error_object());
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("RangeError"));
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("A"));
        }
    }
}

#[test]
fn create_syntax_error_object_01() {
    let mut agent = test_agent();

    let result = create_syntax_error_object(&mut agent, "Happy Days");

    assert!(result.o.is_error_object());
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("SyntaxError"));
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("Happy Days"));
}

#[test]
fn create_syntax_error_01() {
    let mut agent = test_agent();

    let result = create_syntax_error(&mut agent, "A");
    assert!(matches!(result, AbruptCompletion::Throw(_)));
    if let AbruptCompletion::Throw(ci) = result {
        assert!(ci.target.is_none());
        let objval = ci.value.unwrap();
        assert!(objval.is_object());
        if let ECMAScriptValue::Object(obj) = objval {
            assert!(obj.o.is_error_object());
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("SyntaxError"));
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("A"));
        }
    }
}

#[test]
fn create_uri_error_object_01() {
    let mut agent = test_agent();

    let result = create_uri_error_object(&mut agent, "Happy Days");

    assert!(result.o.is_error_object());
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("URIError"));
    assert_eq!(get(&mut agent, &result, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("Happy Days"));
}

#[test]
fn create_uri_error_01() {
    let mut agent = test_agent();

    let result = create_uri_error(&mut agent, "A");
    assert!(matches!(result, AbruptCompletion::Throw(_)));
    if let AbruptCompletion::Throw(ci) = result {
        assert!(ci.target.is_none());
        let objval = ci.value.unwrap();
        assert!(objval.is_object());
        if let ECMAScriptValue::Object(obj) = objval {
            assert!(obj.o.is_error_object());
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("name")).unwrap(), ECMAScriptValue::from("URIError"));
            assert_eq!(get(&mut agent, &obj, &PropertyKey::from("message")).unwrap(), ECMAScriptValue::from("A"));
        }
    }
}

#[test]
fn error_object_debug() {
    let mut agent = test_agent();
    let eo = ErrorObject { common: RefCell::new(CommonObjectData::new(&mut agent, None, true, &[])) };
    assert_ne!(format!("{:?}", eo), "");
}

#[test]
fn error_object_object() {
    let mut agent = test_agent();
    let eo = ErrorObject::object(&mut agent, None);

    assert!(eo.o.is_error_object());
    assert!(eo.o.get_prototype_of(&mut agent).unwrap().is_none());
}

fn create_error_object(agent: &mut Agent) -> Object {
    let error_proto = agent.intrinsic(IntrinsicId::ErrorPrototype);
    ErrorObject::object(agent, Some(error_proto))
}
#[test]
fn error_object_common_object_data() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);
    let error_prototype = agent.intrinsic(IntrinsicId::ErrorPrototype);

    let cod = no.o.common_object_data();

    assert!(cod.borrow().properties.is_empty());
    assert_eq!(cod.borrow().prototype, Some(error_prototype));
    assert!(cod.borrow().extensible);
    assert_eq!(cod.borrow().next_spot, 0);
    assert!(cod.borrow().slots.contains(&InternalSlotName::ErrorData));
}
#[test]
fn error_object_is_ordinary() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.is_ordinary();

    assert!(result);
}
#[test]
fn error_object_id() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    // ... essentially, assert that it doesn't panic.
    no.o.id();
}
#[test]
fn error_object_to_error_object() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.to_error_obj();
    assert!(result.is_some());
}
#[test]
fn error_object_is_error_object() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.is_error_object();

    assert!(result);
}
#[test]
fn error_object_get_prototype_of() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.get_prototype_of(&mut agent).unwrap();
    assert!(result.is_some());
}
#[test]
fn error_object_set_prototype_of() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.set_prototype_of(&mut agent, None).unwrap();
    assert!(result);
}
#[test]
fn error_object_is_extensible() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.is_extensible(&mut agent).unwrap();
    assert!(result);
}
#[test]
fn error_object_prevent_extensions() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.prevent_extensions(&mut agent).unwrap();
    assert!(result);
}
#[test]
fn error_object_get_own_property() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.get_own_property(&mut agent, &PropertyKey::from("a")).unwrap();
    assert!(result.is_none());
}
#[test]
fn error_object_define_own_property() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.define_own_property(&mut agent, PropertyKey::from("a"), PotentialPropertyDescriptor { value: Some(ECMAScriptValue::Undefined), ..Default::default() }).unwrap();
    assert!(result);
}
#[test]
fn error_object_has_property() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.has_property(&mut agent, &PropertyKey::from("a")).unwrap();
    assert!(!result);
}
#[test]
fn error_object_get() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.get(&mut agent, &PropertyKey::from("a"), &ECMAScriptValue::from(no.clone())).unwrap();
    assert_eq!(result, ECMAScriptValue::Undefined);
}
#[test]
fn error_object_set() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.set(&mut agent, PropertyKey::from("a"), ECMAScriptValue::from(88.0), &ECMAScriptValue::from(no.clone())).unwrap();
    assert!(result);
}
#[test]
fn error_object_delete() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.delete(&mut agent, &PropertyKey::from("a")).unwrap();
    assert!(result);
}
#[test]
fn error_object_own_property_keys() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    let result = no.o.own_property_keys(&mut agent).unwrap();
    assert_eq!(result, &[]);
}
#[test]
fn error_object_other_automatic_functions() {
    let mut agent = test_agent();
    let no = create_error_object(&mut agent);

    assert!(!no.o.is_number_object());
    assert!(no.o.to_function_obj().is_none());
    assert!(!no.o.is_boolean_object());
    assert!(!no.o.is_string_object());
    assert!(!no.o.is_regexp_object());
    assert!(no.o.to_builtin_function_obj().is_none());
    assert!(!no.o.is_callable_obj());
    assert!(no.o.to_boolean_obj().is_none());
    //assert!(no.o.to_number_obj().is_none());
    assert!(no.o.to_callable_obj().is_none());
    assert!(no.o.to_constructable().is_none());
    assert!(!no.o.is_arguments_object());
    assert!(!no.o.is_date_object());
}

#[test]
fn error_constructor_data_props() {
    let mut agent = test_agent();
    let error_constructor = agent.intrinsic(IntrinsicId::Error);

    let val = get(&mut agent, &error_constructor, &PropertyKey::from("prototype")).unwrap();
    let error_prototype = agent.intrinsic(IntrinsicId::ErrorPrototype);
    assert_eq!(val, ECMAScriptValue::from(error_prototype));
}

#[test]
fn error_prototype_data_props() {
    let mut agent = test_agent();
    let error_prototype = agent.intrinsic(IntrinsicId::ErrorPrototype);

    let val = get(&mut agent, &error_prototype, &PropertyKey::from("constructor")).unwrap();
    let error_constructor = agent.intrinsic(IntrinsicId::Error);
    assert_eq!(val, ECMAScriptValue::from(error_constructor));

    let val = get(&mut agent, &error_prototype, &PropertyKey::from("message")).unwrap();
    assert_eq!(val, ECMAScriptValue::from(""));

    let val = get(&mut agent, &error_prototype, &PropertyKey::from("name")).unwrap();
    assert_eq!(val, ECMAScriptValue::from("Error"));
}