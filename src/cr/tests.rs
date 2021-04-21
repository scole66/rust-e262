use super::*;

#[test]
fn completion_info_clone_01() {
    let first = CompletionInfo { value: Some(ECMAScriptValue::Boolean(true)), target: Some(JSString::from("label")) };
    let second = first.clone();
    assert_eq!(first.value, second.value);
    assert_eq!(first.target, second.target);
}

#[test]
fn completion_info_debug_01() {
    // Just for coverage. Essentially, assert that we don't panic.
    let first = CompletionInfo { value: Some(ECMAScriptValue::Boolean(true)), target: Some(JSString::from("label")) };
    format!("{:?}", first);
}

#[test]
fn abrupt_completion_clone_01() {
    let value = Some(ECMAScriptValue::Number(10.0));
    let target = Some(JSString::from("outer"));
    let first = AbruptCompletion::Break(CompletionInfo { value, target });
    let second = first.clone();
    assert!(matches!(second, AbruptCompletion::Break(_)));
    if let AbruptCompletion::Break(ci) = second {
        if let AbruptCompletion::Break(orig) = first {
            assert_eq!(ci.value, orig.value);
            assert_eq!(ci.target, orig.target);
        }
    }
}

#[test]
fn abrupt_completion_debug_01() {
    // Just for coverage. Essentially, assert that we don't panic.
    let value = Some(ECMAScriptValue::Number(10.0));
    let target = Some(JSString::from("outer"));
    let first = AbruptCompletion::Break(CompletionInfo { value, target });
    format!("{:?}", first);
}
