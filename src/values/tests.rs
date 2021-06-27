use super::*;
use crate::object::ordinary_object_create;
use ahash::RandomState;
use std::convert::TryInto;
use std::hash::{BuildHasher, Hash, Hasher};

fn calculate_hash<T: Hash>(factory: &RandomState, t: &T) -> u64 {
    let mut s = factory.build_hasher();
    t.hash(&mut s);
    s.finish()
}

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
    assert_eq!(ECMAScriptValue::Undefined.is_symbol(), false);
    assert_eq!(ECMAScriptValue::from(Symbol::ToPrimitive).is_symbol(), true);
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
fn symbol_debug() {
    assert_ne!(format!("{:?}", Symbol::ToPrimitive), "");
}
#[test]
#[allow(clippy::clone_on_copy)]
fn symbol_clone() {
    let s1 = Symbol::ToPrimitive;
    let s2 = s1.clone();
    assert_eq!(s1, s2);
}
#[test]
fn symbol_hash() {
    let s1 = Symbol::ToPrimitive;
    let s2 = Symbol::ToPrimitive;
    let s3 = Symbol::Unscopables;

    let factory = RandomState::new();

    assert_eq!(s1, s2);
    assert_eq!(calculate_hash(&factory, &s1), calculate_hash(&factory, &s2));
    assert_ne!(s1, s3);
    assert_ne!(calculate_hash(&factory, &s1), calculate_hash(&factory, &s3));
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
    assert_eq!(to_boolean(ECMAScriptValue::from(Symbol::ToPrimitive)), true);

    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
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
    let pk = PropertyKey::from(Symbol::ToPrimitive);
    assert_eq!(pk, PropertyKey::Symbol(Symbol::ToPrimitive));
}
#[test]
fn property_key_debug() {
    assert_ne!(format!("{:?}", PropertyKey::from("a")), "");
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
    assert_eq!(PropertyKey::from(Symbol::ToPrimitive).is_array_index(), false);
}
#[test]
fn property_key_try_from() {
    let pk = PropertyKey::from("key");
    assert_eq!(JSString::try_from(pk).unwrap(), "key");
    let pk = PropertyKey::from(Symbol::ToPrimitive);
    assert_eq!(JSString::try_from(pk).unwrap_err(), "Expected String-valued property key");
    let pk = PropertyKey::from("key");
    assert_eq!(JSString::try_from(&pk).unwrap(), "key");
    let pk = PropertyKey::from(Symbol::ToPrimitive);
    assert_eq!(JSString::try_from(&pk).unwrap_err(), "Expected String-valued property key");
}
