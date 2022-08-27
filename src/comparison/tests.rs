use super::*;
use crate::tests::*;
use num::bigint::BigInt;
use test_case::test_case;

#[test]
fn is_integral_number_test() {
    assert!(!is_integral_number(&ECMAScriptValue::Undefined));
    assert!(!is_integral_number(&ECMAScriptValue::from(67.3)));
    assert!(is_integral_number(&ECMAScriptValue::from(0)));
    assert!(!is_integral_number(&ECMAScriptValue::from(f64::INFINITY)));
}

#[test_case(|| ECMAScriptValue::Undefined => Err("TypeError: Undefined and null are not allowed in this context".to_string()); "undefined")]
#[test_case(|| ECMAScriptValue::Null => Err("TypeError: Undefined and null are not allowed in this context".to_string()); "null")]
#[test_case(|| true.into() => Ok(()); "boolean")]
#[test_case(|| 200.2.into() => Ok(()); "number")]
#[test_case(|| "green".into() => Ok(()); "string")]
#[test_case(|| wks(WksId::Species).into() => Ok(()); "symbol")]
#[test_case(|| BigInt::from(10).into() => Ok(()); "bigint")]
#[test_case(|| ordinary_object_create(None, &[]).into() => Ok(()); "object")]
fn require_object_coercible(make_arg: fn() -> ECMAScriptValue) -> Result<(), String> {
    setup_test_agent();
    let arg = make_arg();

    super::require_object_coercible(&arg).map_err(unwind_any_error)
}
