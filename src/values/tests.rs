use super::*;
use crate::errors::create_type_error;
use crate::function_object::create_builtin_function;
use crate::object::{create_data_property, define_property_or_throw, ordinary_object_create, PotentialPropertyDescriptor, BUILTIN_FUNCTION_SLOTS};
use crate::realm::IntrinsicId;
use crate::tests::{calculate_hash, printer_validate, test_agent, unwind_type_error};
use ahash::RandomState;
use num::bigint::BigInt;
use std::convert::TryInto;

#[test]
fn nts_test_nan() {
    let mut s = Vec::new();
    number_to_string(&mut s, f64::NAN).unwrap();
    assert_eq!(s, "NaN".as_bytes());
}
#[test]
fn nts_test_zero() {
    let mut s = Vec::new();
    number_to_string(&mut s, 0.0).unwrap();
    assert_eq!(s, "0".as_bytes());
}
#[test]
fn nts_test_infinity() {
    let mut s = Vec::new();
    number_to_string(&mut s, f64::INFINITY).unwrap();
    assert_eq!(s, "Infinity".as_bytes());
}
#[test]
fn nts_test_negatives() {
    let mut s = Vec::new();
    number_to_string(&mut s, -6.0).unwrap();
    assert_eq!(s, "-6".as_bytes());
}
#[test]
fn nts_test_ends_with_zeroes() {
    let mut s = Vec::new();
    number_to_string(&mut s, 98000000.0).unwrap();
    assert_eq!(s, "98000000".as_bytes());
}
#[test]
fn nts_test_leading_zeroes() {
    let mut s = Vec::new();
    number_to_string(&mut s, 0.00125).unwrap();
    assert_eq!(s, "0.00125".as_bytes());
}
#[test]
fn nts_test_decimal_mid() {
    let mut s = Vec::new();
    number_to_string(&mut s, 104.5024).unwrap();
    assert_eq!(s, "104.5024".as_bytes());
}
#[test]
fn nts_test_pos_exponent() {
    let mut s = Vec::new();
    number_to_string(&mut s, 6.02e23).unwrap();
    assert_eq!(s, "6.02e+23".as_bytes());
}
#[test]
fn nts_test_neg_exponent() {
    let mut s = Vec::new();
    number_to_string(&mut s, 3.441e-10).unwrap();
    assert_eq!(s, "3.441e-10".as_bytes());
}
#[test]
fn nts_test_1dig_exponent() {
    let mut s = Vec::new();
    number_to_string(&mut s, 3e-10).unwrap();
    assert_eq!(s, "3e-10".as_bytes());
}
#[test]
fn nts_test_ioerrs() {
    printer_validate(|w| number_to_string(w, f64::NAN));
    printer_validate(|w| number_to_string(w, 0.0));
    printer_validate(|w| number_to_string(w, -10.0));
    printer_validate(|w| number_to_string(w, f64::INFINITY));
    printer_validate(|w| number_to_string(w, 56.22));
    printer_validate(|w| number_to_string(w, 0.00222));
    printer_validate(|w| number_to_string(w, 1.2e93));
    printer_validate(|w| number_to_string(w, 1.2e-93));
    printer_validate(|w| number_to_string(w, 6e93));
}

#[test]
fn ecmascript_value_clone() {
    let v1 = ECMAScriptValue::Undefined;
    let v2 = v1.clone();
    assert_eq!(v1, v2);
}
#[test]
fn ecmascript_value_ne() {
    let v1 = ECMAScriptValue::from(45);
    let v2 = ECMAScriptValue::from(45.0);
    let v3 = ECMAScriptValue::from("apple");

    assert_eq!(v1 != v2, false);
    assert_eq!(v1 != v3, true);
}
#[test]
fn ecmascript_value_debug() {
    assert_ne!(format!("{:?}", ECMAScriptValue::Undefined), "");
}
#[test]
fn ecmascript_value_default() {
    let def: ECMAScriptValue = Default::default();
    assert_eq!(def, ECMAScriptValue::Undefined);
}
#[test]
fn ecmascript_value_from() {
    let v = ECMAScriptValue::from(true);
    assert_eq!(v, ECMAScriptValue::Boolean(true));
    let v = ECMAScriptValue::from(10_u32);
    assert_eq!(v, ECMAScriptValue::Number(10.0));
    let v = ECMAScriptValue::from(10_i32);
    assert_eq!(v, ECMAScriptValue::Number(10.0));
    let v = ECMAScriptValue::from(10_u64);
    assert_eq!(v, ECMAScriptValue::Number(10.0));
    let v = ECMAScriptValue::from(10_i64);
    assert_eq!(v, ECMAScriptValue::Number(10.0));
    let v = ECMAScriptValue::from(1152921504606846976_u64);
    assert_eq!(v, ECMAScriptValue::BigInt(Rc::new(BigInt::from(1152921504606846976_u64))));
    let v = ECMAScriptValue::from(1152921504606846976_i64);
    assert_eq!(v, ECMAScriptValue::BigInt(Rc::new(BigInt::from(1152921504606846976_i64))));
    let v = ECMAScriptValue::from(-1152921504606846976_i64);
    assert_eq!(v, ECMAScriptValue::BigInt(Rc::new(BigInt::from(-1152921504606846976_i64))));
    let v = ECMAScriptValue::from(vec!['a' as u16, 'b' as u16, 'c' as u16]);
    assert_eq!(v, ECMAScriptValue::String(JSString::from("abc")));
    let v = ECMAScriptValue::from(&JSString::from("blue"));
    assert_eq!(v, ECMAScriptValue::String(JSString::from("blue")));
}
#[test]
fn ecmascript_value_from_object_ref() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let o = ordinary_object_create(&mut agent, None, &[]);

    let val = ECMAScriptValue::from(&o);
    assert!(val.is_object());
    let copied: Object = val.try_into().unwrap();
    assert_eq!(o, copied);
}
#[test]
fn ecmascript_value_from_object() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let o = ordinary_object_create(&mut agent, None, &[]);
    let orig_id = o.o.id();

    let val = ECMAScriptValue::from(o);
    assert!(val.is_object());
    let new_obj: Object = val.try_into().unwrap();
    assert_eq!(new_obj.o.id(), orig_id);
}
#[test]
fn ecmascript_value_is_undefined() {
    assert_eq!(ECMAScriptValue::Undefined.is_undefined(), true);
    assert_eq!(ECMAScriptValue::Boolean(true).is_undefined(), false);
}
#[test]
fn ecmascript_value_is_null() {
    assert_eq!(ECMAScriptValue::Undefined.is_null(), false);
    assert_eq!(ECMAScriptValue::Boolean(true).is_null(), false);
    assert_eq!(ECMAScriptValue::Null.is_null(), true);
}
#[test]
fn ecmascript_value_is_boolean() {
    assert_eq!(ECMAScriptValue::Undefined.is_boolean(), false);
    assert_eq!(ECMAScriptValue::Boolean(true).is_boolean(), true);
    assert_eq!(ECMAScriptValue::Null.is_boolean(), false);
}
#[test]
fn ecmascript_value_is_number() {
    assert_eq!(ECMAScriptValue::Undefined.is_number(), false);
    assert_eq!(ECMAScriptValue::from(99).is_number(), true);
    assert_eq!(ECMAScriptValue::Null.is_number(), false);
}
#[test]
fn ecmascript_value_is_string() {
    assert_eq!(ECMAScriptValue::Undefined.is_string(), false);
    assert_eq!(ECMAScriptValue::from("alice").is_string(), true);
    assert_eq!(ECMAScriptValue::Null.is_string(), false);
}
#[test]
fn ecmascript_value_is_symbol() {
    let mut agent = test_agent();
    assert_eq!(ECMAScriptValue::Undefined.is_symbol(), false);
    assert_eq!(ECMAScriptValue::from(Symbol::new(&mut agent, Some(JSString::from("Test Symbol")))).is_symbol(), true);
    assert_eq!(ECMAScriptValue::Null.is_symbol(), false);
}
#[test]
fn ecmascript_value_is_bigint() {
    assert_eq!(ECMAScriptValue::Undefined.is_bigint(), false);
    assert_eq!(ECMAScriptValue::from(BigInt::from(10)).is_bigint(), true);
    assert_eq!(ECMAScriptValue::Null.is_bigint(), false);
}
#[test]
fn ecmascript_value_is_object() {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    let o = ordinary_object_create(&mut agent, None, &[]);
    assert_eq!(ECMAScriptValue::Undefined.is_object(), false);
    assert_eq!(ECMAScriptValue::from(o).is_object(), true);
    assert_eq!(ECMAScriptValue::Null.is_object(), false);
}
#[test]
fn ecmascript_value_is_numeric() {
    assert_eq!(ECMAScriptValue::Undefined.is_numeric(), false);
    assert_eq!(ECMAScriptValue::from(99).is_numeric(), true);
    assert_eq!(ECMAScriptValue::Null.is_numeric(), false);
    assert_eq!(ECMAScriptValue::from(BigInt::from(10)).is_numeric(), true);
}
#[test]
fn ecmascript_value_concise() {
    // Calling this on our own isn't really do-able; we need to get there via Display or Debug.
    let mut agent = test_agent();
    let obj_proto = &agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(obj_proto), &[]);
    define_property_or_throw(&mut agent, &obj, PropertyKey::from("Undefined"), PotentialPropertyDescriptor { value: Some(ECMAScriptValue::Undefined), ..Default::default() }).unwrap();
    define_property_or_throw(&mut agent, &obj, PropertyKey::from("Null"), PotentialPropertyDescriptor { value: Some(ECMAScriptValue::Null), ..Default::default() }).unwrap();
    define_property_or_throw(&mut agent, &obj, PropertyKey::from("Number"), PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(10.0)), ..Default::default() }).unwrap();
    define_property_or_throw(&mut agent, &obj, PropertyKey::from("String"), PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from("bob")), ..Default::default() }).unwrap();
    define_property_or_throw(&mut agent, &obj, PropertyKey::from("Boolean"), PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), ..Default::default() }).unwrap();
    define_property_or_throw(&mut agent, &obj, PropertyKey::from("BigInt"), PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(BigInt::from(11))), ..Default::default() })
        .unwrap();
    let sym = Symbol::new(&mut agent, Some(JSString::from("San Francisco")));
    define_property_or_throw(&mut agent, &obj, PropertyKey::from("Symbol"), PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(sym)), ..Default::default() }).unwrap();
    let propobj = &agent.intrinsic(IntrinsicId::Boolean);
    define_property_or_throw(&mut agent, &obj, PropertyKey::from("Object"), PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(propobj)), ..Default::default() }).unwrap();

    assert_ne!(format!("{:?}", obj), "");
}

#[test]
fn symbol_debug() {
    let agent = test_agent();
    assert_ne!(format!("{:?}", agent.wks(WksId::ToPrimitive)), "");
}
#[test]
fn symbol_display_normal() {
    let mut agent = test_agent();
    let symbol = Symbol::new(&mut agent, Some(JSString::from("Normal")));
    assert_eq!(format!("{}", symbol), "Symbol(Normal)");
}
#[test]
fn symbol_display_empty() {
    let mut agent = test_agent();
    let symbol = Symbol::new(&mut agent, None);
    assert_eq!(format!("{}", symbol), "Symbol()");
}
#[test]
fn symbol_clone() {
    let agent = test_agent();
    let s1 = agent.wks(WksId::ToPrimitive);
    let s2 = s1.clone();
    assert_eq!(s1, s2);
}
#[test]
fn symbol_hash() {
    let agent = test_agent();
    let s1 = agent.wks(WksId::ToPrimitive);
    let s2 = s1.clone();
    let s3 = agent.wks(WksId::Unscopables);

    let factory = RandomState::new();

    assert_eq!(s1, s2);
    assert_eq!(calculate_hash(&factory, &s1), calculate_hash(&factory, &s2));
    assert_ne!(s1, s3);
    assert_ne!(calculate_hash(&factory, &s1), calculate_hash(&factory, &s3));
}
#[test]
fn symbol_new() {
    let mut agent = test_agent();
    let s1 = Symbol::new(&mut agent, Some(JSString::from("Symbol #1")));
    let s2 = Symbol::new(&mut agent, Some(JSString::from("Symbol #2")));
    let s3 = Symbol::new(&mut agent, Some(JSString::from("Symbol #1")));
    let s4 = s1.clone();

    assert_ne!(s1, s2);
    assert_ne!(s1, s3);
    assert_eq!(s1, s4);
    assert_ne!(s2, s3);
    assert_ne!(s2, s4);
    assert_ne!(s3, s4);
}
#[test]
fn symbol_description() {
    let mut agent = test_agent();
    let s1 = Symbol::new(&mut agent, Some(JSString::from("Test Symbol")));
    assert_eq!(s1.description(), Some(JSString::from("Test Symbol")));
}
#[test]
fn symbol_internals_debug() {
    assert_ne!(format!("{:?}", SymbolInternals { id: 10, description: Some(JSString::from("description")) }), "");
}
#[test]
fn symbol_internals_clone() {
    let si1 = SymbolInternals { id: 10, description: Some(JSString::from("description")) };
    let si2 = si1.clone();
    assert_eq!(si1.id, si2.id);
    assert_eq!(si1.description, si2.description);
}

#[test]
fn privatename_debug() {
    assert_ne!(format!("{:?}", PrivateName {}), "");
}

#[test]
fn to_boolean_01() {
    assert_eq!(to_boolean(ECMAScriptValue::Undefined), false);
    assert_eq!(to_boolean(ECMAScriptValue::Null), false);
    assert_eq!(to_boolean(ECMAScriptValue::from(true)), true);
    assert_eq!(to_boolean(ECMAScriptValue::from(false)), false);
    assert_eq!(to_boolean(ECMAScriptValue::from(0)), false);
    assert_eq!(to_boolean(ECMAScriptValue::from(67)), true);
    assert_eq!(to_boolean(ECMAScriptValue::from(f64::NAN)), false);
    assert_eq!(to_boolean(ECMAScriptValue::from(-0.0)), false);
    assert_eq!(to_boolean(ECMAScriptValue::from(f64::INFINITY)), true);
    assert_eq!(to_boolean(ECMAScriptValue::from("")), false);
    assert_eq!(to_boolean(ECMAScriptValue::from("rust")), true);
    assert_eq!(to_boolean(ECMAScriptValue::from(BigInt::from(22))), true);
    assert_eq!(to_boolean(ECMAScriptValue::from(BigInt::from(0))), false);

    let mut agent = test_agent();
    assert_eq!(to_boolean(ECMAScriptValue::from(agent.wks(WksId::ToPrimitive))), true);
    let o = ordinary_object_create(&mut agent, None, &[]);
    assert_eq!(to_boolean(ECMAScriptValue::from(o)), true);
}

#[test]
fn property_key_ne() {
    let pk1 = PropertyKey::from("bob");
    let pk2 = PropertyKey::from("phil");
    let pk3 = PropertyKey::from("bob");

    assert_eq!(pk1 != pk2, true);
    assert_eq!(pk1 != pk3, false);
}
#[test]
fn property_key_from() {
    let pk = PropertyKey::from("a");
    assert_eq!(pk, PropertyKey::String(JSString::from("a")));
    let pk = PropertyKey::from(JSString::from("b"));
    assert_eq!(pk, PropertyKey::String(JSString::from("b")));
    let pk = PropertyKey::from(&JSString::from("c"));
    assert_eq!(pk, PropertyKey::String(JSString::from("c")));
    let agent = test_agent();
    let pk = PropertyKey::from(agent.wks(WksId::ToPrimitive));
    assert_eq!(pk, PropertyKey::Symbol(agent.wks(WksId::ToPrimitive)));
    let pk = PropertyKey::from(String::from("d"));
    assert_eq!(pk, PropertyKey::String(JSString::from("d")));
}
#[test]
fn property_key_debug() {
    assert_ne!(format!("{:?}", PropertyKey::from("a")), "");
}
#[test]
fn property_key_display() {
    let agent = test_agent();
    let sym = agent.wks(WksId::HasInstance);
    assert_eq!(format!("{}", PropertyKey::from("tangerine")), "tangerine");
    assert_eq!(format!("{}", PropertyKey::from(sym)), "Symbol(Symbol.hasInstance)");
}
#[test]
fn property_key_clone() {
    let pk1 = PropertyKey::from("a");
    let pk2 = pk1.clone();
    assert_eq!(pk1, pk2);
}
#[test]
fn property_key_is_array_index() {
    assert_eq!(PropertyKey::from("0").is_array_index(), true);
    assert_eq!(PropertyKey::from("10").is_array_index(), true);
    assert_eq!(PropertyKey::from("0.25").is_array_index(), false);
    assert_eq!(PropertyKey::from("0  ").is_array_index(), false);
    assert_eq!(PropertyKey::from("  0").is_array_index(), false);
    assert_eq!(PropertyKey::from("-20").is_array_index(), false);
    assert_eq!(PropertyKey::from("4294967295").is_array_index(), true);
    assert_eq!(PropertyKey::from("4294967296").is_array_index(), false);
    assert_eq!(PropertyKey::from("010").is_array_index(), false);
    assert_eq!(PropertyKey::from("000").is_array_index(), false);
    let agent = test_agent();
    assert_eq!(PropertyKey::from(agent.wks(WksId::ToPrimitive)).is_array_index(), false);
}
#[test]
fn property_key_try_from() {
    let agent = test_agent();
    let pk = PropertyKey::from("key");
    assert_eq!(JSString::try_from(pk).unwrap(), "key");
    let pk = PropertyKey::from(agent.wks(WksId::ToPrimitive));
    assert_eq!(JSString::try_from(pk).unwrap_err(), "Expected String-valued property key");
    let pk = PropertyKey::from("key");
    assert_eq!(JSString::try_from(&pk).unwrap(), "key");
    let pk = PropertyKey::from(agent.wks(WksId::ToPrimitive));
    assert_eq!(JSString::try_from(&pk).unwrap_err(), "Expected String-valued property key");
}

#[test]
fn numeric_debug() {
    assert_ne!(format!("{:?}", Numeric::Number(0.0)), "");
}
#[test]
fn numeric_partialeq() {
    let num1 = Numeric::Number(10.0);
    let num2 = Numeric::Number(-10.0);

    assert_eq!(num1 != num2, true);
    assert_eq!(num1 == num2, false);
}

#[test]
fn to_numeric_01() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let result = to_numeric(&mut agent, ECMAScriptValue::from(obj)).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Cannot convert object to primitive value");
}
#[test]
fn to_numeric_02() {
    let mut agent = test_agent();
    let result = to_numeric(&mut agent, ECMAScriptValue::from(BigInt::from(4747474))).unwrap();
    assert_eq!(result, Numeric::BigInt(Rc::new(BigInt::from(4747474))));
}
#[test]
fn to_numeric_03() {
    let mut agent = test_agent();
    let result = to_numeric(&mut agent, ECMAScriptValue::from(10)).unwrap();
    assert_eq!(result, Numeric::Number(10.0));
}
#[test]
fn to_numeric_04() {
    let mut agent = test_agent();
    let sym = Symbol::new(&mut agent, None);
    let result = to_numeric(&mut agent, ECMAScriptValue::from(sym)).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Symbol values cannot be converted to Number values");
}

#[test]
fn to_number_01() {
    let mut agent = test_agent();
    let input = ECMAScriptValue::Undefined;

    let result = to_number(&mut agent, input).unwrap();
    assert!(result.is_nan());
}
#[test]
#[allow(clippy::float_cmp)]
fn to_number_02() {
    let mut agent = test_agent();
    let input = ECMAScriptValue::Null;

    let result = to_number(&mut agent, input).unwrap();
    assert_eq!(result, 0.0);
}
#[test]
#[allow(clippy::float_cmp)]
fn to_number_03() {
    let mut agent = test_agent();
    let input = ECMAScriptValue::from(true);

    let result = to_number(&mut agent, input).unwrap();
    assert_eq!(result, 1.0);
}
#[test]
#[allow(clippy::float_cmp)]
fn to_number_04() {
    let mut agent = test_agent();
    let input = ECMAScriptValue::from(false);

    let result = to_number(&mut agent, input).unwrap();
    assert_eq!(result, 0.0);
}
#[test]
#[allow(clippy::float_cmp)]
fn to_number_05() {
    let mut agent = test_agent();
    let input = ECMAScriptValue::from(37.6);

    let result = to_number(&mut agent, input).unwrap();
    assert_eq!(result, 37.6);
}
#[test]
fn to_number_06() {
    let mut agent = test_agent();
    let input = ECMAScriptValue::from("blue");

    let result = to_number(&mut agent, input).unwrap();
    assert!(result.is_nan());
}
#[test]
#[allow(clippy::float_cmp)]
fn to_number_07() {
    let mut agent = test_agent();
    let testcases = [
        ("", 0.0),
        ("1", 1.0),
        ("   12   ", 12.0),
        ("-Infinity", f64::NEG_INFINITY),
        ("0030", 30.0),
        (" \t\r\n\u{a0}\u{2029}\u{2028}\u{b}\u{c}\u{feff}", 0.0),
        ("0xabcd", 43981.0),
        ("0X6A8BB", 436411.0),
        ("0b010", 2.0),
        ("0B110", 6.0),
        ("0o766", 502.0),
        ("0O7", 7.0),
        ("0xabcdabcdabcdabcdabcdabcdabcdabcd", 228365892722206371581333312115001109453.0),
    ];

    for (s, e) in testcases {
        let result = to_number(&mut agent, ECMAScriptValue::from(s)).unwrap();
        assert_eq!(result, e);
    }
}
#[test]
fn to_number_08() {
    let mut agent = test_agent();
    let input = ECMAScriptValue::from(BigInt::from(10));

    let result = to_number(&mut agent, input).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "BigInt values cannot be converted to Number values");
}
#[test]
fn to_number_09() {
    let mut agent = test_agent();
    let input = ECMAScriptValue::from(Symbol::new(&mut agent, None));

    let result = to_number(&mut agent, input).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Symbol values cannot be converted to Number values");
}
#[test]
fn to_number_10() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let input = ECMAScriptValue::from(obj);

    let result = to_number(&mut agent, input).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Cannot convert object to primitive value");
}
#[test]
fn to_number_11() {
    let mut agent = test_agent();
    let obj_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(&obj_proto), &[]);
    let input = ECMAScriptValue::from(obj);

    let result = to_number(&mut agent, input).unwrap();
    assert!(result.is_nan());
}

#[test]
#[allow(clippy::float_cmp)]
fn to_integer_or_infinity_01() {
    let mut agent = test_agent();
    let testcases = &[(f64::NAN, 0.0), (f64::INFINITY, f64::INFINITY), (f64::NEG_INFINITY, f64::NEG_INFINITY), (0.0, 0.0), (-0.0, 0.0), (10.2, 10.0), (-10.2, -10.0)];

    for (val, expected) in testcases {
        let result = to_integer_or_infinity(&mut agent, ECMAScriptValue::from(*val)).unwrap();
        assert_eq!(result, *expected);
    }
}
#[test]
fn to_integer_or_infinity_02() {
    let mut agent = test_agent();
    let sym = Symbol::new(&mut agent, None);

    let result = to_integer_or_infinity(&mut agent, ECMAScriptValue::from(sym)).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Symbol values cannot be converted to Number values");
}

#[test]
fn to_string_01() {
    let mut agent = test_agent();
    let result = to_string(&mut agent, ECMAScriptValue::Undefined).unwrap();
    assert_eq!(result, "undefined");
}
#[test]
fn to_string_02() {
    let mut agent = test_agent();
    let result = to_string(&mut agent, ECMAScriptValue::Null).unwrap();
    assert_eq!(result, "null");
}
#[test]
fn to_string_03() {
    let mut agent = test_agent();
    let result = to_string(&mut agent, ECMAScriptValue::from(true)).unwrap();
    assert_eq!(result, "true");
}
#[test]
fn to_string_04() {
    let mut agent = test_agent();
    let result = to_string(&mut agent, ECMAScriptValue::from(false)).unwrap();
    assert_eq!(result, "false");
}
#[test]
fn to_string_05() {
    let mut agent = test_agent();
    let result = to_string(&mut agent, ECMAScriptValue::from(10)).unwrap();
    assert_eq!(result, "10");
}
#[test]
fn to_string_06() {
    let mut agent = test_agent();
    let result = to_string(&mut agent, ECMAScriptValue::from("blue")).unwrap();
    assert_eq!(result, "blue");
}
#[test]
fn to_string_07() {
    let mut agent = test_agent();
    let sym = Symbol::new(&mut agent, None);
    let result = to_string(&mut agent, ECMAScriptValue::Symbol(sym)).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Symbols may not be converted to strings");
}
#[test]
fn to_string_08() {
    let mut agent = test_agent();
    let obj_proto = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(&mut agent, Some(&obj_proto), &[]);
    let result = to_string(&mut agent, ECMAScriptValue::from(obj)).unwrap();
    assert_eq!(result, "[object Object]");
}
#[test]
fn to_string_09() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let result = to_string(&mut agent, ECMAScriptValue::from(obj)).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Cannot convert object to primitive value");
}
fn tostring_symbol(agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    let sym = Symbol::new(agent, None);
    Ok(ECMAScriptValue::from(sym))
}
#[test]
fn to_string_10() {
    let mut agent = test_agent();
    let obj = ordinary_object_create(&mut agent, None, &[]);
    let badtostring = create_builtin_function(&mut agent, tostring_symbol, false, 0_f64, PropertyKey::from("toString"), &[], None, None, None);
    create_data_property(&mut agent, &obj, PropertyKey::from("toString"), ECMAScriptValue::from(badtostring)).unwrap();

    let result = to_string(&mut agent, ECMAScriptValue::from(obj)).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Symbols may not be converted to strings");
}
#[test]
fn to_string_11() {
    let mut agent = test_agent();
    let result = to_string(&mut agent, ECMAScriptValue::from(BigInt::from(789123))).unwrap();
    assert_eq!(result, "789123");
}

#[test]
fn to_object_01() {
    let mut agent = test_agent();
    let err = to_object(&mut agent, ECMAScriptValue::Undefined).unwrap_err();
    let msg = unwind_type_error(&mut agent, err);
    assert_eq!(msg, "Undefined and null cannot be converted to objects");
}
#[test]
fn to_object_02() {
    let mut agent = test_agent();
    let err = to_object(&mut agent, ECMAScriptValue::Null).unwrap_err();
    let msg = unwind_type_error(&mut agent, err);
    assert_eq!(msg, "Undefined and null cannot be converted to objects");
}
#[test]
fn to_object_03() {
    let mut agent = test_agent();
    let test_value = true;
    let result = to_object(&mut agent, ECMAScriptValue::from(test_value)).unwrap();

    let boolean_obj = result.o.to_boolean_obj().unwrap();
    assert_eq!(*boolean_obj.boolean_data().borrow(), test_value);
}
#[test]
fn to_object_04() {
    let mut agent = test_agent();
    let test_value = 1337.0;
    let result = to_object(&mut agent, ECMAScriptValue::from(test_value)).unwrap();

    let number_obj = result.o.to_number_obj().unwrap();
    assert_eq!(*number_obj.number_data().borrow(), test_value);
}
#[test]
#[should_panic] // An XFAIL. String objects not yet implemented.
fn to_object_05() {
    let mut agent = test_agent();
    let test_value = "orange";
    let _result = to_object(&mut agent, ECMAScriptValue::from(test_value)).unwrap();
}
#[test]
#[should_panic] // An XFAIL. Symbol objects not yet implemented.
fn to_object_06() {
    let mut agent = test_agent();
    let test_value = agent.wks(WksId::ToPrimitive);
    let _result = to_object(&mut agent, ECMAScriptValue::from(test_value)).unwrap();
}
#[test]
#[should_panic] // An XFAIL. BigInt objects not yet implemented.
fn to_object_07() {
    let mut agent = test_agent();
    let test_value = BigInt::from(10);
    let _result = to_object(&mut agent, ECMAScriptValue::from(test_value)).unwrap();
}
#[test]
fn to_object_08() {
    let mut agent = test_agent();
    let test_value = ordinary_object_create(&mut agent, None, &[]);
    let id = test_value.o.id();
    let result = to_object(&mut agent, ECMAScriptValue::from(test_value)).unwrap();
    assert_eq!(result.o.id(), id);
}

// ordinary_to_primitive:
// * non-object value & non-object string -> two diff results
// * object value & non-object string -> always the string
// * non-object value & object string -> always the value
// * object value & object string -> type error

// non-object number
fn faux_makes_number(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    Ok(ECMAScriptValue::from(123456))
}
// non-object string
fn faux_makes_string(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    Ok(ECMAScriptValue::from("test result"))
}
// object value
fn faux_makes_obj(agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    let object_prototype = agent.intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(agent, Some(&object_prototype), &[]);
    Ok(ECMAScriptValue::from(obj))
}
// error
fn faux_errors(agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    Err(create_type_error(agent, "Test Sentinel"))
}
enum FauxKind {
    Object,
    Primitive,
    Error,
}
fn make_test_obj(agent: &mut Agent, valueof: FauxKind, tostring: FauxKind) -> Object {
    let realm = agent.running_execution_context().unwrap().realm.clone();
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_proto = realm.borrow().intrinsics.function_prototype.clone();
    let target = ordinary_object_create(agent, Some(&object_prototype), &[]);
    let mut connect = |name, length, steps| {
        let key = PropertyKey::from(name);
        let fcn = create_builtin_function(agent, steps, false, length, key.clone(), &BUILTIN_FUNCTION_SLOTS, Some(realm.clone()), Some(function_proto.clone()), None);
        define_property_or_throw(
            agent,
            &target,
            key,
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(fcn)), writable: Some(true), enumerable: Some(false), configurable: Some(true), ..Default::default() },
        )
        .unwrap();
    };

    connect(
        "valueOf",
        0_f64,
        match valueof {
            FauxKind::Object => faux_makes_obj,
            FauxKind::Primitive => faux_makes_number,
            FauxKind::Error => faux_errors,
        },
    );
    connect(
        "toString",
        0_f64,
        match tostring {
            FauxKind::Object => faux_makes_obj,
            FauxKind::Primitive => faux_makes_string,
            FauxKind::Error => faux_errors,
        },
    );

    target
}
fn make_tostring_getter_error(agent: &mut Agent) -> Object {
    // valueOf returns 123456; tostring is a getter that errors
    let realm = agent.running_execution_context().unwrap().realm.clone();
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_proto = realm.borrow().intrinsics.function_prototype.clone();
    let target = ordinary_object_create(agent, Some(&object_prototype), &[]);
    let key = PropertyKey::from("valueOf");
    let fcn = create_builtin_function(agent, faux_makes_number, false, 0_f64, key.clone(), &BUILTIN_FUNCTION_SLOTS, Some(realm.clone()), Some(function_proto.clone()), None);
    define_property_or_throw(
        agent,
        &target,
        key,
        PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(fcn)), writable: Some(true), enumerable: Some(false), configurable: Some(true), ..Default::default() },
    )
    .unwrap();

    let key = PropertyKey::from("toString");
    let tostring_getter = create_builtin_function(agent, faux_errors, false, 0_f64, key.clone(), &BUILTIN_FUNCTION_SLOTS, Some(realm), Some(function_proto), Some(JSString::from("get")));
    define_property_or_throw(
        agent,
        &target,
        key,
        PotentialPropertyDescriptor { enumerable: Some(false), configurable: Some(true), get: Some(ECMAScriptValue::from(tostring_getter)), ..Default::default() },
    )
    .unwrap();

    target
}
fn make_test_obj_uncallable(agent: &mut Agent) -> Object {
    let realm = agent.running_execution_context().unwrap().realm.clone();
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let target = ordinary_object_create(agent, Some(&object_prototype), &[]);
    let mut connect = |name| {
        let key = PropertyKey::from(name);
        define_property_or_throw(
            agent,
            &target,
            key,
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::Undefined), writable: Some(true), enumerable: Some(false), configurable: Some(true), ..Default::default() },
        )
        .unwrap();
    };
    connect("valueOf");
    connect("toString");

    target
}

#[test]
fn ordinary_to_primitive_nonoobj() {
    let mut agent = test_agent();
    let test_obj = make_test_obj(&mut agent, FauxKind::Primitive, FauxKind::Primitive);
    let result_1 = ordinary_to_primitive(&mut agent, &test_obj, ConversionHint::Number).unwrap();
    assert_eq!(result_1, ECMAScriptValue::from(123456));

    let result_2 = ordinary_to_primitive(&mut agent, &test_obj, ConversionHint::String).unwrap();
    assert_eq!(result_2, ECMAScriptValue::from("test result"));
}
#[test]
fn ordinary_to_primitive_obj() {
    let mut agent = test_agent();
    let test_obj = make_test_obj(&mut agent, FauxKind::Object, FauxKind::Object);
    let result_1 = ordinary_to_primitive(&mut agent, &test_obj, ConversionHint::Number).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result_1), "Cannot convert object to primitive value");

    let result_2 = ordinary_to_primitive(&mut agent, &test_obj, ConversionHint::String).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result_2), "Cannot convert object to primitive value");
}
#[test]
fn ordinary_to_primitive_obj_nonobj() {
    let mut agent = test_agent();
    let test_obj = make_test_obj(&mut agent, FauxKind::Object, FauxKind::Primitive);
    let result_1 = ordinary_to_primitive(&mut agent, &test_obj, ConversionHint::Number).unwrap();
    assert_eq!(result_1, ECMAScriptValue::from("test result"));

    let result_2 = ordinary_to_primitive(&mut agent, &test_obj, ConversionHint::String).unwrap();
    assert_eq!(result_2, ECMAScriptValue::from("test result"));
}
#[test]
fn ordinary_to_primitive_nonobj_obj() {
    let mut agent = test_agent();
    let test_obj = make_test_obj(&mut agent, FauxKind::Primitive, FauxKind::Object);
    let result_1 = ordinary_to_primitive(&mut agent, &test_obj, ConversionHint::Number).unwrap();
    assert_eq!(result_1, ECMAScriptValue::from(123456));

    let result_2 = ordinary_to_primitive(&mut agent, &test_obj, ConversionHint::String).unwrap();
    assert_eq!(result_2, ECMAScriptValue::from(123456));
}
#[test]
fn ordinary_to_primitive_call_errors() {
    let mut agent = test_agent();
    let test_obj = make_test_obj(&mut agent, FauxKind::Primitive, FauxKind::Error);
    let result_1 = ordinary_to_primitive(&mut agent, &test_obj, ConversionHint::Number).unwrap();
    assert_eq!(result_1, ECMAScriptValue::from(123456));

    let result_2 = ordinary_to_primitive(&mut agent, &test_obj, ConversionHint::String).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result_2), "Test Sentinel");
}
#[test]
fn ordinary_to_primitive_get_errors() {
    let mut agent = test_agent();
    let test_obj = make_tostring_getter_error(&mut agent);
    let result_1 = ordinary_to_primitive(&mut agent, &test_obj, ConversionHint::Number).unwrap();
    assert_eq!(result_1, ECMAScriptValue::from(123456));

    let result_2 = ordinary_to_primitive(&mut agent, &test_obj, ConversionHint::String).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result_2), "Test Sentinel");
}
#[test]
fn ordinary_to_primitive_uncallables() {
    let mut agent = test_agent();
    let test_obj = make_test_obj_uncallable(&mut agent);
    let result_1 = ordinary_to_primitive(&mut agent, &test_obj, ConversionHint::Number).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result_1), "Cannot convert object to primitive value");

    let result_2 = ordinary_to_primitive(&mut agent, &test_obj, ConversionHint::String).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result_2), "Cannot convert object to primitive value");
}

#[test]
fn to_primitive_no_change() {
    let mut agent = test_agent();
    // Undefined
    let result = to_primitive(&mut agent, &ECMAScriptValue::Undefined, None).unwrap();
    assert!(result.is_undefined());
    // Null
    let result = to_primitive(&mut agent, &ECMAScriptValue::Null, None).unwrap();
    assert!(result.is_null());
    // Boolean
    let result = to_primitive(&mut agent, &ECMAScriptValue::from(true), None).unwrap();
    assert_eq!(result, ECMAScriptValue::from(true));
    // Number
    let result = to_primitive(&mut agent, &ECMAScriptValue::from(20), None).unwrap();
    assert_eq!(result, ECMAScriptValue::from(20));
    // String
    let result = to_primitive(&mut agent, &ECMAScriptValue::from("test"), None).unwrap();
    assert_eq!(result, ECMAScriptValue::from("test"));
    // Symbol
    let sym = Symbol::new(&mut agent, Some(JSString::from("Symbolic")));
    let result = to_primitive(&mut agent, &ECMAScriptValue::from(sym.clone()), None).unwrap();
    assert_eq!(result, ECMAScriptValue::from(sym));
    // BigInt
    let bi = "123456789012345678901234567890".parse::<BigInt>().unwrap();
    let result = to_primitive(&mut agent, &ECMAScriptValue::from(bi), None).unwrap();
    assert_eq!(result, ECMAScriptValue::from("123456789012345678901234567890".parse::<BigInt>().unwrap()));
}
#[test]
fn to_primitive_prefer_number() {
    let mut agent = test_agent();
    let test_obj = make_test_obj(&mut agent, FauxKind::Primitive, FauxKind::Primitive);
    let test_value = ECMAScriptValue::from(test_obj);

    let result = to_primitive(&mut agent, &test_value, None).unwrap();
    assert_eq!(result, ECMAScriptValue::from(123456));
    let result = to_primitive(&mut agent, &test_value, Some(ConversionHint::Number)).unwrap();
    assert_eq!(result, ECMAScriptValue::from(123456));
    let result = to_primitive(&mut agent, &test_value, Some(ConversionHint::String)).unwrap();
    assert_eq!(result, ECMAScriptValue::from("test result"));
}
fn exotic_to_prim(_agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, arguments: &[ECMAScriptValue]) -> Completion {
    if arguments.len() == 1 {
        if let ECMAScriptValue::String(s) = &arguments[0] {
            Ok(ECMAScriptValue::from(format!("Saw {}", s)))
        } else {
            Ok(ECMAScriptValue::from(format!("Saw {:?}", arguments[0])))
        }
    } else {
        Ok(ECMAScriptValue::from(format!("Incorrect arg count: there were {} elements, should have been 1", arguments.len())))
    }
}
fn make_toprimitive_obj(agent: &mut Agent, steps: fn(&mut Agent, ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion) -> Object {
    let realm = agent.running_execution_context().unwrap().realm.clone();
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_proto = realm.borrow().intrinsics.function_prototype.clone();
    let target = ordinary_object_create(agent, Some(&object_prototype), &[]);
    let key = PropertyKey::from(agent.wks(WksId::ToPrimitive));
    let fcn = create_builtin_function(agent, steps, false, 1_f64, key.clone(), &BUILTIN_FUNCTION_SLOTS, Some(realm), Some(function_proto), None);
    define_property_or_throw(
        agent,
        &target,
        key,
        PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(fcn)), writable: Some(true), enumerable: Some(false), configurable: Some(true), ..Default::default() },
    )
    .unwrap();
    target
}
#[test]
fn to_primitive_uses_exotics() {
    let mut agent = test_agent();
    let test_obj = make_toprimitive_obj(&mut agent, exotic_to_prim);
    let test_value = ECMAScriptValue::from(test_obj);

    let result = to_primitive(&mut agent, &test_value, None).unwrap();
    assert_eq!(result, ECMAScriptValue::from("Saw default"));
    let result = to_primitive(&mut agent, &test_value, Some(ConversionHint::Number)).unwrap();
    assert_eq!(result, ECMAScriptValue::from("Saw number"));
    let result = to_primitive(&mut agent, &test_value, Some(ConversionHint::String)).unwrap();
    assert_eq!(result, ECMAScriptValue::from("Saw string"));
}
fn exotic_returns_object(agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    let realm = agent.running_execution_context().unwrap().realm.clone();
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let target = ordinary_object_create(agent, Some(&object_prototype), &[]);
    Ok(ECMAScriptValue::from(target))
}
#[test]
fn to_primitive_exotic_returns_object() {
    let mut agent = test_agent();
    let test_obj = make_toprimitive_obj(&mut agent, exotic_returns_object);
    let test_value = ECMAScriptValue::from(test_obj);

    let result = to_primitive(&mut agent, &test_value, None).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Cannot convert object to primitive value");
}
fn exotic_throws(agent: &mut Agent, _this_value: ECMAScriptValue, _new_target: Option<&Object>, _arguments: &[ECMAScriptValue]) -> Completion {
    Err(create_type_error(agent, "Test Sentinel"))
}
#[test]
fn to_primitive_exotic_throws() {
    let mut agent = test_agent();
    let test_obj = make_toprimitive_obj(&mut agent, exotic_throws);
    let test_value = ECMAScriptValue::from(test_obj);

    let result = to_primitive(&mut agent, &test_value, None).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Test Sentinel");
}
#[test]
fn to_primitive_exotic_getter_throws() {
    let mut agent = test_agent();
    let realm = agent.running_execution_context().unwrap().realm.clone();
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_proto = realm.borrow().intrinsics.function_prototype.clone();
    let target = ordinary_object_create(&mut agent, Some(&object_prototype), &[]);
    let key = PropertyKey::from(agent.wks(WksId::ToPrimitive));
    let toprim_getter = create_builtin_function(&mut agent, faux_errors, false, 0_f64, key.clone(), &BUILTIN_FUNCTION_SLOTS, Some(realm), Some(function_proto), Some(JSString::from("get")));
    define_property_or_throw(
        &mut agent,
        &target,
        key,
        PotentialPropertyDescriptor { enumerable: Some(false), configurable: Some(true), get: Some(ECMAScriptValue::from(toprim_getter)), ..Default::default() },
    )
    .unwrap();
    let test_value = ECMAScriptValue::from(target);

    let result = to_primitive(&mut agent, &test_value, None).unwrap_err();
    assert_eq!(unwind_type_error(&mut agent, result), "Test Sentinel");
}
