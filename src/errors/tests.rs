use super::*;
use crate::tests::{test_agent, unwind_type_error};
use crate::realm::IntrinsicId;
use crate::object::{get};

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
    let eo = ErrorObject { common: RefCell::new(CommonObjectData::new(&mut agent, None, true, &[]))};
    assert_ne!(format!("{:?}", eo), "");
}

#[test]
fn error_object_object() {
    let mut agent = test_agent();
    let eo = ErrorObject::object(&mut agent, None);

    assert!(eo.o.is_error_object());
    assert!(eo.o.get_prototype_of(&mut agent).unwrap().is_none());
}