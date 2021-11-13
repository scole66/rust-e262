use super::*;
use crate::agent::WksId;
use crate::object::ordinary_object_create;
use crate::tests::{test_agent, unwind_any_error};
use num::bigint::BigInt;

#[test]
fn is_integral_number_test() {
    assert!(!is_integral_number(&ECMAScriptValue::Undefined));
    assert!(!is_integral_number(&ECMAScriptValue::from(67.3)));
    assert!(is_integral_number(&ECMAScriptValue::from(0)));
    assert!(!is_integral_number(&ECMAScriptValue::from(f64::INFINITY)));
}

mod require_object_coercible {
    use super::*;
    use test_case::test_case;

    #[test_case(|_| ECMAScriptValue::Undefined => Err("TypeError: Undefined and null are not allowed in this context".to_string()); "undefined")]
    #[test_case(|_| ECMAScriptValue::Null => Err("TypeError: Undefined and null are not allowed in this context".to_string()); "null")]
    #[test_case(|_| true.into() => Ok(()); "boolean")]
    #[test_case(|_| 200.2.into() => Ok(()); "number")]
    #[test_case(|_| "green".into() => Ok(()); "string")]
    #[test_case(|a| a.wks(WksId::Species).into() => Ok(()); "symbol")]
    #[test_case(|_| BigInt::from(10).into(); "bigint")]
    #[test_case(|a| ordinary_object_create(a, None, &[]).into() => Ok(()); "object")]
    fn roc(make_arg: fn(&mut Agent) -> ECMAScriptValue) -> Result<(), String> {
        let mut agent = test_agent();
        let arg = make_arg(&mut agent);

        require_object_coercible(&mut agent, &arg).map_err(|err| unwind_any_error(&mut agent, err))
    }
}
