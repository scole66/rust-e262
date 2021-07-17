use super::*;
use crate::values::ECMAScriptValue;

#[test]
fn arguments_from_empty() {
    let arguments: &[ECMAScriptValue] = &[];
    let mut args = Arguments::from(arguments);

    assert_eq!(args.count(), 0);
    assert_eq!(args.next_arg(), ECMAScriptValue::Undefined);
}

#[test]
fn arguments_from_list_of_two() {
    let arguments: &[ECMAScriptValue] = &[ECMAScriptValue::from(10), ECMAScriptValue::from(20)];
    let mut args = Arguments::from(arguments);

    assert_eq!(args.count(), 2);
    assert_eq!(args.next_arg(), ECMAScriptValue::from(10));
    assert_eq!(args.next_arg(), ECMAScriptValue::from(20));
    assert_eq!(args.next_arg(), ECMAScriptValue::Undefined);
    assert_eq!(args.next_arg(), ECMAScriptValue::Undefined);
    assert_eq!(args.count(), 2);
}
