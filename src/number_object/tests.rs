use super::*;
use crate::tests::*;
use num::BigInt;

#[test]
fn number_object_debug() {
    setup_test_agent();
    let no = NumberObject {
        common: RefCell::new(CommonObjectData::new(None, false, NUMBER_OBJECT_SLOTS)),
        number_data: RefCell::new(0.0),
    };

    assert_ne!(format!("{no:?}"), "");
}

#[test]
#[allow(clippy::float_cmp)]
fn number_object_object() {
    setup_test_agent();
    let number_prototype = intrinsic(IntrinsicId::NumberPrototype);
    let no = NumberObject::object(Some(number_prototype.clone()));

    assert_eq!(no.o.common_object_data().borrow().prototype, Some(number_prototype));
    assert_eq!(*no.o.to_number_obj().unwrap().number_data().borrow(), 0.0);
}

#[test]
#[allow(clippy::float_cmp)]
fn create_number_object_01() {
    setup_test_agent();
    let no = create_number_object(100.0);

    let number_prototype = intrinsic(IntrinsicId::NumberPrototype);
    assert_eq!(no.o.get_prototype_of().unwrap(), Some(number_prototype));
    assert_eq!(*no.o.to_number_obj().unwrap().number_data().borrow(), 100.0);
}

#[test]
fn number_object_common_object_data() {
    setup_test_agent();
    let no = create_number_object(100.0);
    let number_prototype = intrinsic(IntrinsicId::NumberPrototype);

    let cod = no.o.common_object_data();

    assert!(cod.borrow().properties.is_empty());
    assert_eq!(cod.borrow().prototype, Some(number_prototype));
    assert!(cod.borrow().extensible);
    assert_eq!(cod.borrow().next_spot, 0);
    assert!(cod.borrow().slots.contains(&InternalSlotName::NumberData));
}
#[test]
fn number_object_uses_ordinary_get_prototype_of() {
    setup_test_agent();
    let no = create_number_object(100.0);

    let result = no.o.uses_ordinary_get_prototype_of();

    assert!(result);
}
#[test]
fn number_object_id() {
    setup_test_agent();
    let no = create_number_object(100.0);

    // ... essentially, assert that it doesn't panic.
    no.o.id();
}
#[test]
fn number_object_to_number_object() {
    setup_test_agent();
    let no = create_number_object(100.0);

    let result = no.o.to_number_obj();
    assert!(result.is_some());
}
#[test]
fn number_object_is_number_object() {
    setup_test_agent();
    let no = create_number_object(100.0);

    let result = no.o.is_number_object();

    assert!(result);
}
#[test]
fn number_object_get_prototype_of() {
    setup_test_agent();
    let no = create_number_object(100.0);

    let result = no.o.get_prototype_of().unwrap();
    assert!(result.is_some());
}
#[test]
fn number_object_set_prototype_of() {
    setup_test_agent();
    let no = create_number_object(100.0);

    let result = no.o.set_prototype_of(None).unwrap();
    assert!(result);
}
#[test]
fn number_object_is_extensible() {
    setup_test_agent();
    let no = create_number_object(100.0);

    let result = no.o.is_extensible().unwrap();
    assert!(result);
}
#[test]
fn number_object_prevent_extensions() {
    setup_test_agent();
    let no = create_number_object(100.0);

    let result = no.o.prevent_extensions().unwrap();
    assert!(result);
}
#[test]
fn number_object_get_own_property() {
    setup_test_agent();
    let no = create_number_object(100.0);

    let result = no.o.get_own_property(&PropertyKey::from("a")).unwrap();
    assert!(result.is_none());
}
#[test]
fn number_object_define_own_property() {
    setup_test_agent();
    let no = create_number_object(100.0);

    let result =
        no.o.define_own_property(
            PropertyKey::from("a"),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::Undefined), ..Default::default() },
        )
        .unwrap();
    assert!(result);
}
#[test]
fn number_object_has_property() {
    setup_test_agent();
    let no = create_number_object(100.0);

    let result = no.o.has_property(&PropertyKey::from("a")).unwrap();
    assert!(!result);
}
#[test]
fn number_object_get() {
    setup_test_agent();
    let no = create_number_object(100.0);

    let result = no.o.get(&PropertyKey::from("a"), &ECMAScriptValue::from(no.clone())).unwrap();
    assert_eq!(result, ECMAScriptValue::Undefined);
}
#[test]
fn number_object_set() {
    setup_test_agent();
    let no = create_number_object(100.0);

    let result =
        no.o.set(PropertyKey::from("a"), ECMAScriptValue::from(88.0), &ECMAScriptValue::from(no.clone())).unwrap();
    assert!(result);
}
#[test]
fn number_object_delete() {
    setup_test_agent();
    let no = create_number_object(100.0);

    let result = no.o.delete(&PropertyKey::from("a")).unwrap();
    assert!(result);
}
#[test]
fn number_object_own_property_keys() {
    setup_test_agent();
    let no = create_number_object(100.0);

    let result = no.o.own_property_keys().unwrap();
    assert_eq!(result, &[]);
}
#[test]
fn number_object_other_automatic_functions() {
    setup_test_agent();
    let no = create_number_object(100.0);

    assert!(!no.o.is_error_object());
    assert!(no.o.to_function_obj().is_none());
    assert!(!no.o.is_boolean_object());
    assert!(!no.o.is_string_object());
    assert!(!no.o.is_regexp_object());
    assert!(no.o.to_builtin_function_obj().is_none());
    assert!(!no.o.is_callable_obj());
    assert!(no.o.to_boolean_obj().is_none());
    assert!(no.o.to_error_obj().is_none());
    assert!(no.o.to_callable_obj().is_none());
    assert!(no.o.to_constructable().is_none());
    assert!(!no.o.is_arguments_object());
    assert!(!no.o.is_date_object());
    assert!(!no.o.is_proxy_object());
}

#[test]
fn number_constructor_data_props() {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);

    let val = number_constructor.get(&PropertyKey::from("EPSILON")).unwrap();
    assert_eq!(val, ECMAScriptValue::from(f64::EPSILON));

    let val = number_constructor.get(&PropertyKey::from("MAX_SAFE_INTEGER")).unwrap();
    assert_eq!(val, ECMAScriptValue::from(9_007_199_254_740_991.0));

    let val = number_constructor.get(&PropertyKey::from("MAX_VALUE")).unwrap();
    assert_eq!(val, ECMAScriptValue::from(f64::MAX));

    let val = number_constructor.get(&PropertyKey::from("MIN_SAFE_INTEGER")).unwrap();
    assert_eq!(val, ECMAScriptValue::from(-9_007_199_254_740_991.0));

    let val = number_constructor.get(&PropertyKey::from("MIN_VALUE")).unwrap();
    assert_eq!(val, ECMAScriptValue::from(5e-324));

    let val = number_constructor.get(&PropertyKey::from("NaN")).unwrap();
    assert!(matches!(val, ECMAScriptValue::Number(_)));
    if let ECMAScriptValue::Number(n) = val {
        assert!(n.is_nan());
    }

    let val = number_constructor.get(&PropertyKey::from("NEGATIVE_INFINITY")).unwrap();
    assert_eq!(val, ECMAScriptValue::from(f64::NEG_INFINITY));

    let val = number_constructor.get(&PropertyKey::from("POSITIVE_INFINITY")).unwrap();
    assert_eq!(val, ECMAScriptValue::from(f64::INFINITY));

    let val = number_constructor.get(&PropertyKey::from("prototype")).unwrap();
    let number_prototype = intrinsic(IntrinsicId::NumberPrototype);
    assert_eq!(val, ECMAScriptValue::from(number_prototype));
}

#[test]
fn number_constructor_called_as_function_01() {
    // No arguments passed:
    //   > Number()
    //   0
    setup_test_agent();
    let number_constructor = ECMAScriptValue::from(intrinsic(IntrinsicId::Number));

    let result = call(&number_constructor, &ECMAScriptValue::Undefined, &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(0));
}
#[test]
fn number_constructor_called_as_function_02() {
    // Argument with a "Number" result from ToNumeric.
    //   > Number(true)
    //   1
    setup_test_agent();
    let number_constructor = ECMAScriptValue::from(intrinsic(IntrinsicId::Number));

    let result = call(&number_constructor, &ECMAScriptValue::Undefined, &[ECMAScriptValue::from(true)]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(1));
}
#[test]
fn number_constructor_called_as_function_03() {
    // Argument with a "BigInt" result from ToNumeric.
    //   > Number(10n)
    //   10
    setup_test_agent();
    let number_constructor = ECMAScriptValue::from(intrinsic(IntrinsicId::Number));

    let result =
        call(&number_constructor, &ECMAScriptValue::Undefined, &[ECMAScriptValue::from(BigInt::from(10))]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(10));
}
#[test]
fn number_constructor_called_as_function_04() {
    // Argument that cannot be converted to a number
    //   > Number(Symbol())
    //   Uncaught TypeError: Cannot convert a Symbol value to a number
    //       at Number (<anonymous>)
    setup_test_agent();
    let number_constructor = ECMAScriptValue::from(intrinsic(IntrinsicId::Number));

    let sym = Symbol::new(None);
    let result = call(&number_constructor, &ECMAScriptValue::Undefined, &[ECMAScriptValue::from(sym)]).unwrap_err();
    assert_eq!(unwind_type_error(result), "Symbol values cannot be converted to Number values");
}

#[test]
#[allow(clippy::float_cmp)]
fn number_constructor_as_constructor_01() {
    // No arguments:
    //   > new Number()
    //   [Number: 0]
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);

    let result = construct(&number_constructor, &[], None).unwrap();

    assert!(result.is_object());
    if let ECMAScriptValue::Object(o) = result {
        assert!(o.o.is_number_object());
        let data = *o.o.to_number_obj().unwrap().number_data().borrow();
        assert_eq!(data, 0.0);
    }
}
#[test]
#[allow(clippy::float_cmp)]
fn number_constructor_as_constructor_02() {
    // Argument needing conversion:
    //   > new Number("0xbadfade")
    //   [Number: 195951326]
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let arg = ECMAScriptValue::from("0xbadfade");

    let result = construct(&number_constructor, &[arg], None).unwrap();

    assert!(result.is_object());
    if let ECMAScriptValue::Object(o) = result {
        assert!(o.o.is_number_object());
        let data = *o.o.to_number_obj().unwrap().number_data().borrow();
        assert_eq!(data, 195_951_326.0);
    }
}
#[test]
fn number_constructor_throws() {
    // ordinary_create_from_contructor throws.
    // This looks to be difficult to make happen, but I can imagine some class shenanigans that could do it.
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);

    // This hack is to get around the "not configurable" characteristic of Number.prototype.
    // (It replaces Number.prototype (a data property) with an accessor property that throws when "prototype" is gotten.)
    let new_prop = PropertyKind::Accessor(AccessorProperty {
        get: ECMAScriptValue::from(intrinsic(IntrinsicId::ThrowTypeError)),
        set: ECMAScriptValue::Undefined,
    });
    {
        let mut cod = number_constructor.o.common_object_data().borrow_mut();
        let prop = cod.properties.get_mut(&PropertyKey::from("prototype")).unwrap();
        prop.property = new_prop;
    }

    let result = construct(&number_constructor, &[], None).unwrap_err();
    assert_eq!(unwind_type_error(result), "Generic TypeError");
}

#[test]
fn number_is_finite_no_args() {
    // no args
    //    > Number.isFinite()
    //    false
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let is_finite = number_constructor.get(&PropertyKey::from("isFinite")).unwrap();
    let this_value = ECMAScriptValue::from(number_constructor);

    let result = call(&is_finite, &this_value, &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(false));
}
#[test]
fn number_is_finite_one_arg() {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let is_finite = number_constructor.get(&PropertyKey::from("isFinite")).unwrap();
    let this_value = ECMAScriptValue::from(number_constructor);

    for (arg, expected) in [
        (f64::INFINITY, false),
        (f64::NAN, false),
        (f64::NEG_INFINITY, false),
        (0.0, true),
        (-0.0, true),
        (89.3, true),
        (-89.3, true),
    ] {
        let result = call(&is_finite, &this_value, &[ECMAScriptValue::from(arg)]).unwrap();
        assert_eq!(result, ECMAScriptValue::from(expected));
    }

    let result = call(&is_finite, &this_value, &[ECMAScriptValue::from("blue")]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(false));
}

#[test]
fn number_is_integer_no_args() {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let is_integer = number_constructor.get(&PropertyKey::from("isInteger")).unwrap();
    let this_value = ECMAScriptValue::from(number_constructor);

    let result = call(&is_integer, &this_value, &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(false));
}
#[test]
fn number_is_integer_one_arg() {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let is_integer = number_constructor.get(&PropertyKey::from("isInteger")).unwrap();
    let this_value = ECMAScriptValue::from(number_constructor);

    for (arg, expected) in [
        (f64::INFINITY, false),
        (f64::NAN, false),
        (f64::NEG_INFINITY, false),
        (0.0, true),
        (-0.0, true),
        (89.3, false),
        (-89.3, false),
        (10.0, true),
        (3.33e200, true),
    ] {
        let result = call(&is_integer, &this_value, &[ECMAScriptValue::from(arg)]).unwrap();
        assert_eq!(result, ECMAScriptValue::from(expected));
    }

    let result = call(&is_integer, &this_value, &[ECMAScriptValue::from("blue")]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(false));
}

#[test]
fn number_is_nan_no_args() {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let is_nan = number_constructor.get(&PropertyKey::from("isNaN")).unwrap();
    let this_value = ECMAScriptValue::from(number_constructor);

    let result = call(&is_nan, &this_value, &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(false));
}
#[test]
fn number_is_nan_one_arg() {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let is_nan = number_constructor.get(&PropertyKey::from("isNaN")).unwrap();
    let this_value = ECMAScriptValue::from(number_constructor);

    for (arg, expected) in [
        (f64::INFINITY, false),
        (f64::NAN, true),
        (f64::NEG_INFINITY, false),
        (0.0, false),
        (-0.0, false),
        (89.3, false),
    ] {
        let result = call(&is_nan, &this_value, &[ECMAScriptValue::from(arg)]).unwrap();
        assert_eq!(result, ECMAScriptValue::from(expected));
    }

    let result = call(&is_nan, &this_value, &[ECMAScriptValue::from("blue")]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(false));
}

#[test]
fn number_is_safe_integer_no_args() {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let is_safe_integer = number_constructor.get(&PropertyKey::from("isSafeInteger")).unwrap();
    let this_value = ECMAScriptValue::from(number_constructor);

    let result = call(&is_safe_integer, &this_value, &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(false));
}
#[test]
fn number_is_safe_integer_one_arg() {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let is_safe_integer = number_constructor.get(&PropertyKey::from("isSafeInteger")).unwrap();
    let this_value = ECMAScriptValue::from(number_constructor);

    #[allow(clippy::cast_precision_loss)]
    for (arg, expected) in [
        (f64::INFINITY, false),
        (f64::NAN, false),
        (f64::NEG_INFINITY, false),
        (0.0, true),
        (-0.0, true),
        (89.3, false),
        (3.33e200, false),
        (0x1f_ffff_ffff_ffff_u64 as f64, true),
        (0x20_0000_0000_0000_u64 as f64, false),
        (-0x1f_ffff_ffff_ffff_i64 as f64, true),
        (-0x20_0000_0000_0000_i64 as f64, false),
    ] {
        let result = call(&is_safe_integer, &this_value, &[ECMAScriptValue::from(arg)]).unwrap();
        assert_eq!(result, ECMAScriptValue::from(expected), "Tried {arg}, should have been {expected:?}");
    }

    let result = call(&is_safe_integer, &this_value, &[ECMAScriptValue::from("blue")]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(false));
}

#[test]
#[allow(clippy::float_cmp)]
fn this_number_value_01() {
    // called with number object
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let number = construct(&number_constructor, &[ECMAScriptValue::from(123)], None).unwrap();

    let result = this_number_value(&number).unwrap();
    assert_eq!(result, 123.0);
}
#[test]
#[allow(clippy::float_cmp)]
fn this_number_value_02() {
    // called with number value
    setup_test_agent();

    let result = this_number_value(&ECMAScriptValue::from(123)).unwrap();
    assert_eq!(result, 123.0);
}
#[test]
fn this_number_value_03() {
    // called with non-number object
    setup_test_agent();
    let obj = ordinary_object_create(None, &[]);

    let result = this_number_value(&ECMAScriptValue::from(obj)).unwrap_err();
    assert_eq!(unwind_type_error(result), "Number method called with non-number receiver");
}
#[test]
fn this_number_value_04() {
    // called with non-number, non-object value
    setup_test_agent();

    let result = this_number_value(&ECMAScriptValue::from(true)).unwrap_err();
    assert_eq!(unwind_type_error(result), "Number method called with non-number receiver");
}

#[test]
fn number_proto_to_string_01() {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let number = construct(&number_constructor, &[ECMAScriptValue::from(123)], None).unwrap();

    let result = number.invoke(&PropertyKey::from("toString"), &[]).unwrap();

    assert_eq!(result, ECMAScriptValue::from("123"));
}
#[test]
fn number_proto_to_string_02() {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let number = construct(&number_constructor, &[ECMAScriptValue::from(123.789)], None).unwrap();

    let result = number.invoke(&PropertyKey::from("toString"), &[ECMAScriptValue::from(25)]).unwrap();

    assert_eq!(result, ECMAScriptValue::from("4n.ji33333333"));
}

#[test]
fn number_proto_to_string_03() {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let number = construct(&number_constructor, &[ECMAScriptValue::from(123.789)], None).unwrap();
    let sym = Symbol::new(None);

    let result = number.invoke(&PropertyKey::from("toString"), &[ECMAScriptValue::from(sym)]).unwrap_err();

    assert_eq!(unwind_type_error(result), "Symbol values cannot be converted to Number values");
}
#[test]
fn number_proto_to_string_04() {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let number = construct(&number_constructor, &[ECMAScriptValue::from(123)], None).unwrap();

    let result = number.invoke(&PropertyKey::from("toString"), &[ECMAScriptValue::from(2)]).unwrap();

    assert_eq!(result, ECMAScriptValue::from("1111011"));
}
#[test]
fn number_proto_to_string_05() {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let number = construct(&number_constructor, &[ECMAScriptValue::from(123)], None).unwrap();

    let result = number.invoke(&PropertyKey::from("toString"), &[ECMAScriptValue::from(36)]).unwrap();

    assert_eq!(result, ECMAScriptValue::from("3f"));
}
#[test]
fn number_proto_to_string_06() {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let number = construct(&number_constructor, &[ECMAScriptValue::from(123)], None).unwrap();

    let result = number.invoke(&PropertyKey::from("toString"), &[ECMAScriptValue::from(1)]).unwrap_err();

    assert_eq!(unwind_range_error(result), "Radix 1 out of range (must be in 2..36)");
}
#[test]
fn number_proto_to_string_07() {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let number = construct(&number_constructor, &[ECMAScriptValue::from(123)], None).unwrap();

    let result = number.invoke(&PropertyKey::from("toString"), &[ECMAScriptValue::from(37)]).unwrap_err();

    assert_eq!(unwind_range_error(result), "Radix 37 out of range (must be in 2..36)");
}
#[test]
fn number_proto_to_string_08() {
    // this_number_value is not actually a number
    setup_test_agent();
    let number_prototype = intrinsic(IntrinsicId::NumberPrototype);
    let to_string = number_prototype.get(&PropertyKey::from("toString")).unwrap();

    let result = call(&to_string, &ECMAScriptValue::Null, &[]).unwrap_err();
    assert_eq!(unwind_type_error(result), "Number method called with non-number receiver");
}
#[test]
fn number_proto_to_string_09() {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let number = construct(&number_constructor, &[ECMAScriptValue::from(1.0e100)], None).unwrap();

    let result = number.invoke(&PropertyKey::from("toString"), &[ECMAScriptValue::from(30)]).unwrap();

    assert_eq!(result, ECMAScriptValue::from("anhmc58j7ljq00000000000000000000000000000000000000000000000000000000"));
}

#[test]
fn double_to_radix_string_01() {
    assert_eq!(double_to_radix_string(-2048.0, 16), "-800");
    assert_eq!(double_to_radix_string(0.999_999_99, 3), "0.2222222222222222120101012010002");
    assert_eq!(double_to_radix_string(0.999_999_999_999_9, 26), "0.pppppppppbn");
}

fn number_proto_to_precision_test(value: f64, precision: u32, expected: &str) {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);

    let number = construct(&number_constructor, &[ECMAScriptValue::from(value)], None).unwrap();
    let result = number.invoke(&PropertyKey::from("toPrecision"), &[ECMAScriptValue::from(precision)]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(expected));
}
#[test]
fn number_proto_to_precision_01() {
    number_proto_to_precision_test(2.125, 2, "2.1");
}
#[test]
fn number_proto_to_precision_02() {
    number_proto_to_precision_test(2.7759, 3, "2.78");
}
#[test]
fn number_proto_to_precision_03() {
    number_proto_to_precision_test(9.9999, 3, "10.0");
}
#[test]
fn number_proto_to_precision_04() {
    number_proto_to_precision_test(0.00125, 3, "0.00125");
}
#[test]
fn number_proto_to_precision_05() {
    number_proto_to_precision_test(5.960_464_477_539_063e-8, 3, "5.96e-8");
}
#[test]
fn number_proto_to_precision_06() {
    number_proto_to_precision_test(-0.0, 3, "0.00");
}
#[test]
fn number_proto_to_precision_07() {
    number_proto_to_precision_test(-5.0, 6, "-5.00000");
}
#[test]
fn number_proto_to_precision_08() {
    number_proto_to_precision_test(6_500_000.0, 2, "6.5e6");
}
#[test]
fn number_proto_to_precision_09() {
    number_proto_to_precision_test(9_999_999.0, 2, "1.0e7");
}
#[test]
fn number_proto_to_precision_10() {
    number_proto_to_precision_test(-9_999_999.0, 1, "-1e7");
}
#[test]
fn number_proto_to_precision_11() {
    number_proto_to_precision_test(12345.0, 5, "12345");
}
#[test]
fn number_proto_to_precision_15() {
    number_proto_to_precision_test(f64::NEG_INFINITY, 5, "-Infinity");
}

#[test]
fn number_proto_to_precision_12() {
    // this_number_value is not actually a number
    setup_test_agent();
    let number_prototype = intrinsic(IntrinsicId::NumberPrototype);
    let func = number_prototype.get(&PropertyKey::from("toPrecision")).unwrap();

    let result = call(&func, &ECMAScriptValue::Null, &[]).unwrap_err();
    assert_eq!(unwind_type_error(result), "Number method called with non-number receiver");
}
#[test]
fn number_proto_to_precision_13() {
    // precision not present
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);

    let number = construct(&number_constructor, &[ECMAScriptValue::from(548.333)], None).unwrap();
    let result = number.invoke(&PropertyKey::from("toPrecision"), &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from("548.333"));
}
#[test]
fn number_proto_to_precision_14() {
    // precision not convertable to number
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let sym = ECMAScriptValue::from(Symbol::new(None));

    let number = construct(&number_constructor, &[ECMAScriptValue::from(548.333)], None).unwrap();
    let result = number.invoke(&PropertyKey::from("toPrecision"), &[sym]).unwrap_err();
    assert_eq!(unwind_type_error(result), "Symbol values cannot be converted to Number values");
}
#[test]
fn number_proto_to_precision_16() {
    // precision out of range
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);

    let number = construct(&number_constructor, &[ECMAScriptValue::from(548.333)], None).unwrap();
    let result = number.invoke(&PropertyKey::from("toPrecision"), &[ECMAScriptValue::from(0)]).unwrap_err();
    assert_eq!(unwind_range_error(result), "Precision ‘0’ must lie within the range 1..100");
}
#[test]
fn number_proto_to_precision_17() {
    // precision out of range
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);

    let number = construct(&number_constructor, &[ECMAScriptValue::from(548.333)], None).unwrap();
    let result = number.invoke(&PropertyKey::from("toPrecision"), &[ECMAScriptValue::from(101)]).unwrap_err();
    assert_eq!(unwind_range_error(result), "Precision ‘101’ must lie within the range 1..100");
}
#[test]
fn number_proto_to_precision_18() {
    // precision just in range
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);

    let number = construct(&number_constructor, &[ECMAScriptValue::from(548.333)], None).unwrap();
    let result = number.invoke(&PropertyKey::from("toPrecision"), &[ECMAScriptValue::from(100)]).unwrap();
    assert_eq!(
        result,
        ECMAScriptValue::from(
            "548.3329999999999699866748414933681488037109375000000000000000000000000000000000000000000000000000000"
        )
    );
}

fn number_proto_to_exponent_test(value: f64, fraction_digits: u32, expected: &str) {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);

    let number = construct(&number_constructor, &[ECMAScriptValue::from(value)], None).unwrap();
    let result = number.invoke(&PropertyKey::from("toExponential"), &[ECMAScriptValue::from(fraction_digits)]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(expected));
}
#[test]
fn number_proto_to_exponential_01() {
    number_proto_to_exponent_test(56.333, 3, "5.633e+1");
}
#[test]
fn number_proto_to_exponential_02() {
    number_proto_to_exponent_test(
        0.1,
        100,
        "1.0000000000000000555111512312578270211815834045410156250000000000000000000000000000000000000000000000e-1",
    );
}
#[test]
fn number_proto_to_exponential_03() {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);

    let number = construct(&number_constructor, &[ECMAScriptValue::from(0.1)], None).unwrap();
    let result = number.invoke(&PropertyKey::from("toExponential"), &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from("1e-1"));
}
#[test]
fn number_proto_to_exponential_04() {
    // this_number_value is not actually a number
    setup_test_agent();
    let number_prototype = intrinsic(IntrinsicId::NumberPrototype);
    let func = number_prototype.get(&PropertyKey::from("toExponential")).unwrap();

    let result = call(&func, &ECMAScriptValue::Null, &[]).unwrap_err();
    assert_eq!(unwind_type_error(result), "Number method called with non-number receiver");
}
#[test]
fn number_proto_to_exponential_05() {
    // fractionDigits not convertable to number
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let sym = ECMAScriptValue::from(Symbol::new(None));

    let number = construct(&number_constructor, &[ECMAScriptValue::from(548.333)], None).unwrap();
    let result = number.invoke(&PropertyKey::from("toExponential"), &[sym]).unwrap_err();
    assert_eq!(unwind_type_error(result), "Symbol values cannot be converted to Number values");
}
#[test]
fn number_proto_to_exponential_06() {
    number_proto_to_exponent_test(f64::INFINITY, 5, "Infinity");
}
#[test]
fn number_proto_to_exponential_07() {
    // fractionDigits out of range
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);

    let number = construct(&number_constructor, &[ECMAScriptValue::from(548.333)], None).unwrap();
    let result = number.invoke(&PropertyKey::from("toExponential"), &[ECMAScriptValue::from(101)]).unwrap_err();
    assert_eq!(unwind_range_error(result), "FractionDigits ‘101’ must lie within the range 0..100");
}
#[test]
fn number_proto_to_exponential_08() {
    // fractionDigits out of range
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);

    let number = construct(&number_constructor, &[ECMAScriptValue::from(548.333)], None).unwrap();
    let result = number.invoke(&PropertyKey::from("toExponential"), &[ECMAScriptValue::from(-1)]).unwrap_err();
    assert_eq!(unwind_range_error(result), "FractionDigits ‘-1’ must lie within the range 0..100");
}
#[test]
fn number_proto_to_exponential_09() {
    number_proto_to_exponent_test(-89388.13111, 5, "-8.93881e+4");
}

fn number_proto_to_fixed_test(value: f64, fraction_digits: u32, expected: &str) {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);

    let number = construct(&number_constructor, &[ECMAScriptValue::from(value)], None).unwrap();
    let result = number.invoke(&PropertyKey::from("toFixed"), &[ECMAScriptValue::from(fraction_digits)]).unwrap();
    assert_eq!(result, ECMAScriptValue::from(expected));
}

#[test]
fn number_proto_to_fixed_01() {
    number_proto_to_fixed_test(8888.922, 2, "8888.92");
}
#[test]
fn number_proto_to_fixed_02() {
    number_proto_to_fixed_test(999.999, 2, "1000.00");
}
#[test]
fn number_proto_to_fixed_03() {
    number_proto_to_fixed_test(999.999, 3, "999.999");
}
#[test]
fn number_proto_to_fixed_04() {
    number_proto_to_fixed_test(-8888.922, 2, "-8888.92");
}
#[test]
fn number_proto_to_fixed_05() {
    // The example from the spec
    number_proto_to_fixed_test(1_000_000_000_000_000_128.0, 0, "1000000000000000128");
}
#[test]
fn number_proto_to_fixed_06() {
    number_proto_to_fixed_test(0.0, 5, "0.00000");
}
#[test]
fn number_proto_to_fixed_07() {
    number_proto_to_fixed_test(-0.000_001_1, 4, "-0.0000");
}
#[test]
fn number_proto_to_fixed_08() {
    number_proto_to_fixed_test(-0.0011, 4, "-0.0011");
}
#[test]
fn number_proto_to_fixed_09() {
    number_proto_to_fixed_test(1.23, 4, "1.2300");
}
#[test]
fn number_proto_to_fixed_10() {
    number_proto_to_fixed_test(100.0, 0, "100");
}
#[test]
fn number_proto_to_fixed_11() {
    number_proto_to_fixed_test(f64::INFINITY, 0, "Infinity");
}
#[test]
fn number_proto_to_fixed_12() {
    number_proto_to_fixed_test(f64::NEG_INFINITY, 0, "-Infinity");
}
#[test]
fn number_proto_to_fixed_13() {
    number_proto_to_fixed_test(f64::NAN, 0, "NaN");
}
#[test]
fn number_proto_to_fixed_14() {
    number_proto_to_fixed_test(33.0e302, 10, "3.3e+303");
}
#[test]
fn number_proto_to_fixed_15() {
    number_proto_to_fixed_test(-3.0e21, 10, "-3e+21");
}
#[test]
fn number_proto_to_fixed_16() {
    // empty arg list
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);

    let number = construct(&number_constructor, &[ECMAScriptValue::from(0.1)], None).unwrap();
    let result = number.invoke(&PropertyKey::from("toFixed"), &[]).unwrap();
    assert_eq!(result, ECMAScriptValue::from("0"));
}
#[test]
fn number_proto_to_fixed_17() {
    // bad argument
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);
    let sym = ECMAScriptValue::from(Symbol::new(None));

    let number = construct(&number_constructor, &[ECMAScriptValue::from(0.1)], None).unwrap();
    let result = number.invoke(&PropertyKey::from("toFixed"), &[sym]).unwrap_err();
    assert_eq!(unwind_type_error(result), "Symbol values cannot be converted to Number values");
}
#[test]
fn number_proto_to_fixed_18() {
    // bad argument
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);

    let number = construct(&number_constructor, &[ECMAScriptValue::from(0.1)], None).unwrap();
    let result = number.invoke(&PropertyKey::from("toFixed"), &[ECMAScriptValue::from(-1)]).unwrap_err();
    assert_eq!(unwind_range_error(result), "Argument for Number.toFixed must be in the range 0..100");
}
#[test]
fn number_proto_to_fixed_19() {
    // bad argument
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);

    let number = construct(&number_constructor, &[ECMAScriptValue::from(0.1)], None).unwrap();
    let result = number.invoke(&PropertyKey::from("toFixed"), &[ECMAScriptValue::from(101)]).unwrap_err();
    assert_eq!(unwind_range_error(result), "Argument for Number.toFixed must be in the range 0..100");
}
#[test]
fn number_proto_to_fixed_20() {
    // 100 digits
    number_proto_to_fixed_test(
        0.748_390_178_958_793_8,
        100,
        "0.7483901789587937836145670189580414444208145141601562500000000000000000000000000000000000000000000000",
    );
}
#[test]
fn number_proto_to_fixed_21() {
    // this_number_value is not actually a number
    setup_test_agent();
    let number_prototype = intrinsic(IntrinsicId::NumberPrototype);
    let func = number_prototype.get(&PropertyKey::from("toFixed")).unwrap();

    let result = call(&func, &ECMAScriptValue::Null, &[]).unwrap_err();
    assert_eq!(unwind_type_error(result), "Number method called with non-number receiver");
}

#[test]
fn double_exponent_test() {
    // 1.0 -> 1<52 zeros> x 2^(-52)
    assert_eq!(double_exponent(1.0), -52);
    // 3e-323 -> denormal; just gets -1074...
    assert_eq!(double_exponent(3e-323), -1074);
}

#[test]
#[allow(clippy::float_cmp)]
fn next_double_test() {
    // Infinity -> Infinity
    assert_eq!(next_double(f64::INFINITY), f64::INFINITY);
    // -0.0 -> 0.0
    assert_eq!(next_double(-0.0), 0.0);
    assert_eq!(next_double(-0.0).to_bits(), 0u64);
    // 0.0 -> 5e-324
    assert_eq!(next_double(0.0), 5e-324);
    assert_eq!(next_double(0.0).to_bits(), 1u64);
    // -1.0 -> -0.9999999999999999
    assert_eq!(next_double(-1.0), -0.999_999_999_999_999_9);
    assert_eq!(next_double(-1.0).to_bits(), 0xBFEF_FFFF_FFFF_FFFF);
}

#[test]
fn number_proto_to_locale_string_01() {
    // Implementations of toLocaleString may not use the arguments for any use beyond their ECMA-402 specified uses. In
    // particular, if we defer to toString, the first argument must _not_ be used as the radix argument.
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);

    let number = construct(&number_constructor, &[ECMAScriptValue::from(10)], None).unwrap();
    let result = number.invoke(&PropertyKey::from("toLocaleString"), &[ECMAScriptValue::from(16)]).unwrap();
    assert_eq!(result, ECMAScriptValue::from("10"));
}

#[test]
fn number_proto_value_of() {
    setup_test_agent();
    let number_constructor = intrinsic(IntrinsicId::Number);

    let number = construct(&number_constructor, &[ECMAScriptValue::from(0.1)], None).unwrap();
    let result = number.invoke(&PropertyKey::from("valueOf"), &[]).unwrap();

    assert_eq!(result, ECMAScriptValue::from(0.1));
}
