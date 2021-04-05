use super::agent::Agent;
use super::cr::{AbruptCompletion, CompletionInfo};
use super::object::get;
use super::values::{ECMAScriptValue, PropertyKey};

pub fn unwind_type_error(agent: &mut Agent, completion: AbruptCompletion) -> String {
    assert!(matches!(completion, AbruptCompletion::Throw(CompletionInfo { value: Some(ECMAScriptValue::Object(_)), target: None })));
    if let AbruptCompletion::Throw(CompletionInfo { value: Some(ECMAScriptValue::Object(err)), target: None }) = completion {
        assert!(err.o.to_error_obj().is_some());
        let maybe_name = get(agent, &err, &PropertyKey::from("name"));
        assert!(maybe_name.is_ok());
        let name = maybe_name.unwrap();
        assert!(matches!(name, ECMAScriptValue::String(_)));
        if let ECMAScriptValue::String(name_value) = name {
            assert_eq!(name_value, "TypeError");
        }
        let maybe_message = get(agent, &err, &PropertyKey::from("message"));
        assert!(maybe_message.is_ok());
        let message = maybe_message.unwrap();
        assert!(matches!(message, ECMAScriptValue::String(_)));
        if let ECMAScriptValue::String(message_value) = message {
            String::from(message_value)
        } else {
            String::new()
        }
    } else {
        String::new()
    }
}
