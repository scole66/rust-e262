use super::*;

#[test]
fn is_integral_number_test() {
    assert!(!is_integral_number(&ECMAScriptValue::Undefined));
    assert!(!is_integral_number(&ECMAScriptValue::from(67.3)));
    assert!(is_integral_number(&ECMAScriptValue::from(0)));
    assert!(!is_integral_number(&ECMAScriptValue::from(f64::INFINITY)));
}
