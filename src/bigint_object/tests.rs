use super::*;
use crate::tests::*;
use ahash::AHashMap;
use test_case::test_case;

mod bigint_object {
    use super::*;

    fn make() -> Object {
        let proto = intrinsic(IntrinsicId::BigIntPrototype);
        BigIntObject::object(Some(proto), Rc::new(10.into()))
    }

    false_function!(is_array_object);
    false_function!(is_callable_obj);
    false_function!(is_date_object);
    false_function!(is_generator_object);
    false_function!(is_plain_object);
    false_function!(is_proxy_object);
    false_function!(is_regexp_object);
    false_function!(is_string_object);
    false_function!(is_symbol_object);
    none_function!(to_arguments_object);
    none_function!(to_array_object);
    none_function!(to_boolean_obj);
    none_function!(to_bound_function_object);
    none_function!(to_builtin_function_obj);
    none_function!(to_builtin_function_with_revocable_proxy_slot);
    none_function!(to_callable_obj);
    none_function!(to_constructable);
    none_function!(to_date_obj);
    none_function!(to_for_in_iterator);
    none_function!(to_function_obj);
    none_function!(to_generator_object);
    none_function!(to_map_obj);
    none_function!(to_number_obj);
    none_function!(to_proxy_object);
    none_function!(to_regexp_object);
    none_function!(to_string_obj);
    none_function!(to_symbol_obj);

    default_kind_test!();
    default_has_property_test!();
    default_is_extensible_test!();
    default_id_test!();
    default_delete_test!();
    default_set_test!();
    default_get_test!(|| PropertyKey::from(wks(WksId::ToStringTag)), ECMAScriptValue::from("BigInt"));
    default_uses_ordinary_get_prototype_of_test!();
    default_prevent_extensions_test!();
    default_own_property_keys_test!();
    default_set_prototype_of_test!();

    #[test]
    fn is_bigint_object() {
        setup_test_agent();
        let obj = make();
        assert!(obj.o.is_bigint_object());
    }

    #[test]
    fn to_bigint_object() {
        setup_test_agent();
        let obj = make();
        let orig_id = obj.o.id();
        assert!(obj.o.to_bigint_object().is_some_and(|bo| bo.id() == orig_id));
    }

    #[test]
    fn value() {
        setup_test_agent();
        let obj = make();
        assert_eq!(obj.o.to_bigint_object().unwrap().value(), Rc::new(BigInt::from(10)));
    }

    #[test]
    fn bigint_data() {
        setup_test_agent();
        let obj = make();
        let bo = obj.o.to_bigint_object().unwrap();
        let contained_value = bo.bigint_data().clone();
        assert_eq!(contained_value, Rc::new(BigInt::from(10)));
    }
}

#[test_case(|| ECMAScriptValue::Undefined => serr("TypeError: Value cannot be converted to bigint"); "undefined")]
#[test_case(|| ECMAScriptValue::Null => serr("TypeError: Value cannot be converted to bigint"); "null")]
#[test_case(|| ECMAScriptValue::Boolean(true) => Ok(Rc::new(BigInt::from(1))); "boolean true")]
#[test_case(|| ECMAScriptValue::Boolean(false) => Ok(Rc::new(BigInt::from(0))); "boolean false")]
#[test_case(|| ECMAScriptValue::BigInt(Rc::new(BigInt::from(1023))) => Ok(Rc::new(BigInt::from(1023))); "big int")]
#[test_case(|| ECMAScriptValue::Number(42.0) => serr("TypeError: Value cannot be converted to bigint"); "number")]
#[test_case(|| ECMAScriptValue::String(JSString::from("hello")) => serr("SyntaxError: Invalid character sequence for bigint"); "string not a number")]
#[test_case(|| ECMAScriptValue::String(JSString::from("1023")) => Ok(Rc::new(BigInt::from(1023))); "string is a number")]
#[test_case(|| ECMAScriptValue::Symbol(wks(WksId::ToStringTag)) => serr("TypeError: Value cannot be converted to bigint"); "symbol")]
#[test_case(|| ECMAScriptValue::Object(DeadObject::object()) => serr("TypeError: get called on DeadObject"); "non-convertable object")]
fn to_big_int(make_value: impl FnOnce() -> ECMAScriptValue) -> Result<Rc<BigInt>, String> {
    setup_test_agent();
    let value = make_value();
    value.to_big_int().map_err(unwind_any_error)
}

#[test_case(3.0 => Ok(Rc::new(BigInt::from(3))); "small, positive")]
#[test_case(f64::INFINITY => serr("RangeError: Non-integral number used in bigint creation"); "really big")]
#[test_case(0.5 => serr("RangeError: Non-integral number used in bigint creation"); "fractional")]
fn number_to_big_int(num: f64) -> Result<Rc<BigInt>, String> {
    setup_test_agent();
    super::number_to_big_int(num).map_err(unwind_any_error)
}

#[test_case(|| ECMAScriptValue::Undefined => serr("TypeError: Value is not a Big Int"); "undefined")]
#[test_case(|| ECMAScriptValue::BigInt(Rc::new(BigInt::from(678))) => Ok(BigInt::from(678)); "plain bigint")]
#[test_case(|| { let proto = intrinsic(IntrinsicId::BigIntPrototype); ECMAScriptValue::from(BigIntObject::object(Some(proto), Rc::new(BigInt::from(88)))) } => Ok(BigInt::from(88)); "bigint object")]
#[test_case(|| { let proto = intrinsic(IntrinsicId::ObjectPrototype); ECMAScriptValue::from(ordinary_object_create(Some(proto))) } => serr("TypeError: Value is not a Big Int"); "non bigint object")]
fn this_bigint_value(make_val: impl FnOnce() -> ECMAScriptValue) -> Result<BigInt, String> {
    setup_test_agent();
    let val = make_val();
    super::this_bigint_value(val).map(Rc::unwrap_or_clone).map_err(unwind_any_error)
}
