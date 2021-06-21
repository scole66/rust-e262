use super::agent::Agent;
use super::cr::{AbruptCompletion, CompletionInfo};
use super::object::{get, Object};
use super::values::{ECMAScriptValue, PropertyKey};

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
