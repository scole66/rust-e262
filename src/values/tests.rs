use super::*;
use crate::tests::*;
use ahash::RandomState;
use num::bigint::BigInt;
use regex::Regex;
use std::cmp::Ordering;
use std::convert::TryInto;
use std::hash::BuildHasher;
use test_case::test_case;

fn check_value(expected: f64) -> impl Fn(f64) {
    move |actual: f64| {
        assert!(number_same_value(actual, expected), "{actual} did not match expected {expected}");
    }
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
    number_to_string(&mut s, 98_000_000.0).unwrap();
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

mod ecmascript_value {
    use super::*;
    use test_case::test_case;

    #[test]
    #[expect(clippy::redundant_clone)]
    fn clone() {
        let v1 = ECMAScriptValue::Undefined;
        let v2 = v1.clone();
        assert_eq!(v1, v2);
    }
    #[test]
    fn ne() {
        let v1 = ECMAScriptValue::from(45);
        let v2 = ECMAScriptValue::from(45.0);
        let v3 = ECMAScriptValue::from("apple");

        assert_eq!(v1 != v2, false);
        assert_eq!(v1 != v3, true);
    }
    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", ECMAScriptValue::Undefined), "");
    }
    #[test]
    fn default() {
        let def = ECMAScriptValue::default();
        assert_eq!(def, ECMAScriptValue::Undefined);
    }
    #[test]
    fn from() {
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
        let v = ECMAScriptValue::from(1_152_921_504_606_846_976_u64);
        assert_eq!(v, ECMAScriptValue::BigInt(Rc::new(BigInt::from(1_152_921_504_606_846_976_u64))));
        let v = ECMAScriptValue::from(1_152_921_504_606_846_976_i64);
        assert_eq!(v, ECMAScriptValue::BigInt(Rc::new(BigInt::from(1_152_921_504_606_846_976_i64))));
        let v = ECMAScriptValue::from(-1_152_921_504_606_846_976_i64);
        assert_eq!(v, ECMAScriptValue::BigInt(Rc::new(BigInt::from(-1_152_921_504_606_846_976_i64))));
        let v = ECMAScriptValue::from(Rc::new(BigInt::from(789_999_999_999_999_999_999_999_i128)));
        assert_eq!(v, ECMAScriptValue::BigInt(Rc::new(BigInt::from(789_999_999_999_999_999_999_999_i128))));
        let v = ECMAScriptValue::from(vec!['a' as u16, 'b' as u16, 'c' as u16]);
        assert_eq!(v, ECMAScriptValue::String(JSString::from("abc")));
        let v = ECMAScriptValue::from(&JSString::from("blue"));
        assert_eq!(v, ECMAScriptValue::String(JSString::from("blue")));
    }
    #[test]
    fn from_object_ref() {
        setup_test_agent();
        let o = ordinary_object_create(None);

        let val = ECMAScriptValue::from(&o);
        assert!(val.is_object());
        let copied: Object = val.try_into().unwrap();
        assert_eq!(o, copied);
    }
    #[test]
    fn from_object() {
        setup_test_agent();
        let o = ordinary_object_create(None);
        let orig_id = o.o.id();

        let val = ECMAScriptValue::from(o);
        assert!(val.is_object());
        let new_obj: Object = val.try_into().unwrap();
        assert_eq!(new_obj.o.id(), orig_id);
    }
    #[test_case(&Numeric::Number(45.3) => ECMAScriptValue::Number(45.3); "number")]
    #[test_case(&Numeric::BigInt(Rc::new(BigInt::from(9911))) => ECMAScriptValue::BigInt(Rc::new(BigInt::from(9911))); "bigint")]
    fn from_numeric_ref(n: &Numeric) -> ECMAScriptValue {
        ECMAScriptValue::from(n)
    }
    #[test_case(Numeric::Number(45.3) => ECMAScriptValue::Number(45.3); "number")]
    #[test_case(Numeric::BigInt(Rc::new(BigInt::from(9911))) => ECMAScriptValue::BigInt(Rc::new(BigInt::from(9911))); "bigint")]
    fn from_numeric(n: Numeric) -> ECMAScriptValue {
        ECMAScriptValue::from(n)
    }
    #[test_case(|| PropertyKey::String("key".into()) => "key"; "string")]
    #[test_case(|| PropertyKey::Symbol(wks(WksId::ToPrimitive)) => "Symbol(Symbol.toPrimitive)"; "symbol")]
    fn from_property_key(maker: fn() -> PropertyKey) -> String {
        setup_test_agent();
        let key = maker();
        format!("{}", ECMAScriptValue::from(key))
    }
    #[test_case(|| PropertyKey::String("key".into()) => "key"; "string")]
    #[test_case(|| PropertyKey::Symbol(wks(WksId::ToPrimitive)) => "Symbol(Symbol.toPrimitive)"; "symbol")]
    fn from_property_key_ref(maker: fn() -> PropertyKey) -> String {
        setup_test_agent();
        let key = maker();
        format!("{}", ECMAScriptValue::from(&key))
    }
    #[test_case(String::from("blue") => ECMAScriptValue::String("blue".into()); "String")]
    fn from_string(s: String) -> ECMAScriptValue {
        ECMAScriptValue::from(s)
    }
    #[test_case(56_usize => (ECMAScriptValue::Number(56.0), ValueKind::Number); "small number")]
    #[test_case(9_007_199_254_741_092_usize => (ECMAScriptValue::from(9_007_199_254_741_092_u64), ValueKind::BigInt); "big number")]
    fn from_usize(u: usize) -> (ECMAScriptValue, ValueKind) {
        let v = ECMAScriptValue::from(u);
        let kind = v.kind();
        (v, kind)
    }
    #[test_case(|| wks(WksId::ToPrimitive) => "Symbol(Symbol.toPrimitive)"; "symbol")]
    fn from_symbol_ref(maker: impl FnOnce() -> Symbol) -> String {
        setup_test_agent();
        let sym = maker();
        ECMAScriptValue::from(&sym).test_result_string()
    }
    #[test]
    fn is_undefined() {
        assert_eq!(ECMAScriptValue::Undefined.is_undefined(), true);
        assert_eq!(ECMAScriptValue::Boolean(true).is_undefined(), false);
    }
    #[test]
    fn is_null() {
        assert_eq!(ECMAScriptValue::Undefined.is_null(), false);
        assert_eq!(ECMAScriptValue::Boolean(true).is_null(), false);
        assert_eq!(ECMAScriptValue::Null.is_null(), true);
    }
    #[test]
    fn is_boolean() {
        assert_eq!(ECMAScriptValue::Undefined.is_boolean(), false);
        assert_eq!(ECMAScriptValue::Boolean(true).is_boolean(), true);
        assert_eq!(ECMAScriptValue::Null.is_boolean(), false);
    }
    #[test]
    fn is_number() {
        assert_eq!(ECMAScriptValue::Undefined.is_number(), false);
        assert_eq!(ECMAScriptValue::from(99).is_number(), true);
        assert_eq!(ECMAScriptValue::Null.is_number(), false);
    }
    #[test]
    fn is_string() {
        assert_eq!(ECMAScriptValue::Undefined.is_string(), false);
        assert_eq!(ECMAScriptValue::from("alice").is_string(), true);
        assert_eq!(ECMAScriptValue::Null.is_string(), false);
    }
    #[test]
    fn is_symbol() {
        setup_test_agent();
        assert_eq!(ECMAScriptValue::Undefined.is_symbol(), false);
        assert_eq!(ECMAScriptValue::from(Symbol::new(Some(JSString::from("Test Symbol")))).is_symbol(), true);
        assert_eq!(ECMAScriptValue::Null.is_symbol(), false);
    }
    #[test]
    fn is_bigint() {
        assert_eq!(ECMAScriptValue::Undefined.is_bigint(), false);
        assert_eq!(ECMAScriptValue::from(BigInt::from(10)).is_bigint(), true);
        assert_eq!(ECMAScriptValue::Null.is_bigint(), false);
    }
    #[test]
    fn is_object() {
        setup_test_agent();
        let o = ordinary_object_create(None);
        assert_eq!(ECMAScriptValue::Undefined.is_object(), false);
        assert_eq!(ECMAScriptValue::from(o).is_object(), true);
        assert_eq!(ECMAScriptValue::Null.is_object(), false);
    }
    #[test]
    fn is_numeric() {
        assert_eq!(ECMAScriptValue::Undefined.is_numeric(), false);
        assert_eq!(ECMAScriptValue::from(99).is_numeric(), true);
        assert_eq!(ECMAScriptValue::Null.is_numeric(), false);
        assert_eq!(ECMAScriptValue::from(BigInt::from(10)).is_numeric(), true);
    }
    #[test]
    fn concise() {
        // Calling this on our own isn't really do-able; we need to get there via Display or Debug.
        setup_test_agent();
        let obj_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(Some(obj_proto));
        define_property_or_throw(
            &obj,
            PropertyKey::from("Undefined"),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::Undefined), ..Default::default() },
        )
        .unwrap();
        define_property_or_throw(
            &obj,
            PropertyKey::from("Null"),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::Null), ..Default::default() },
        )
        .unwrap();
        define_property_or_throw(
            &obj,
            PropertyKey::from("Number"),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(10.0)), ..Default::default() },
        )
        .unwrap();
        define_property_or_throw(
            &obj,
            PropertyKey::from("String"),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from("bob")), ..Default::default() },
        )
        .unwrap();
        define_property_or_throw(
            &obj,
            PropertyKey::from("Boolean"),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(true)), ..Default::default() },
        )
        .unwrap();
        define_property_or_throw(
            &obj,
            PropertyKey::from("BigInt"),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(BigInt::from(11))), ..Default::default() },
        )
        .unwrap();
        let sym = Symbol::new(Some(JSString::from("San Francisco")));
        define_property_or_throw(
            &obj,
            PropertyKey::from("Symbol"),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(sym)), ..Default::default() },
        )
        .unwrap();
        let propobj = &intrinsic(IntrinsicId::Boolean);
        define_property_or_throw(
            &obj,
            PropertyKey::from("Object"),
            PotentialPropertyDescriptor { value: Some(ECMAScriptValue::from(propobj)), ..Default::default() },
        )
        .unwrap();

        assert_ne!(format!("{obj:?}"), "");
    }
    #[test]
    fn is_array() {
        setup_test_agent();
        let a = array_create(0, None).unwrap();
        let v1: ECMAScriptValue = a.into();
        let v2 = ECMAScriptValue::Null;
        assert!(v1.is_array().unwrap());
        assert!(!v2.is_array().unwrap());
    }

    mod display {
        use super::*;
        use test_case::test_case;

        #[test_case(&ECMAScriptValue::Undefined => "undefined"; "undefined")]
        #[test_case(&ECMAScriptValue::Null => "null"; "null")]
        #[test_case(&ECMAScriptValue::Boolean(true) => "true"; "bool true")]
        #[test_case(&ECMAScriptValue::Boolean(false) => "false"; "bool false")]
        #[test_case(&ECMAScriptValue::String("turnip".into()) => "turnip"; "string")]
        #[test_case(&ECMAScriptValue::Number(67.331) => "67.331"; "number")]
        #[test_case(&ECMAScriptValue::BigInt(Rc::new(BigInt::from(12345))) => "12345n"; "bigint")]
        fn simple(val: &ECMAScriptValue) -> String {
            format!("{val}")
        }

        #[test_case(|| ECMAScriptValue::Symbol(wks(WksId::ToPrimitive)) => "Symbol(Symbol.toPrimitive)"; "symbol")]
        #[test_case(|| {
            let obj = ordinary_object_create(None);
            ECMAScriptValue::Object(obj)
        } => with |s: String| assert!(Regex::new("^<Object [0-9]+>$").unwrap().is_match(&s)); "object")]
        fn complex(maker: fn() -> ECMAScriptValue) -> String {
            setup_test_agent();
            let val = maker();
            format!("{val}")
        }
    }

    type ValueMaker = fn() -> ECMAScriptValue;
    fn undef() -> ECMAScriptValue {
        ECMAScriptValue::Undefined
    }
    fn null() -> ECMAScriptValue {
        ECMAScriptValue::Null
    }
    fn string_a() -> ECMAScriptValue {
        ECMAScriptValue::from("A")
    }
    fn string_b() -> ECMAScriptValue {
        ECMAScriptValue::from("B")
    }
    fn bool_a() -> ECMAScriptValue {
        ECMAScriptValue::from(true)
    }
    fn bool_b() -> ECMAScriptValue {
        ECMAScriptValue::from(false)
    }
    fn symbol_a() -> ECMAScriptValue {
        wks(WksId::ToPrimitive).into()
    }
    fn symbol_b() -> ECMAScriptValue {
        wks(WksId::HasInstance).into()
    }
    fn object_a() -> ECMAScriptValue {
        intrinsic(IntrinsicId::FunctionPrototype).into()
    }
    fn object_b() -> ECMAScriptValue {
        intrinsic(IntrinsicId::ObjectPrototype).into()
    }
    fn number_10() -> ECMAScriptValue {
        10.into()
    }
    fn number_100() -> ECMAScriptValue {
        100.into()
    }
    fn number_zero() -> ECMAScriptValue {
        0.into()
    }
    fn number_neg_zero() -> ECMAScriptValue {
        (-0.0).into()
    }
    fn number_nan() -> ECMAScriptValue {
        f64::NAN.into()
    }
    fn bigint_a() -> ECMAScriptValue {
        BigInt::from(10).into()
    }
    fn bigint_b() -> ECMAScriptValue {
        BigInt::from(-1_097_631).into()
    }

    #[test_case(undef, undef => true; "undefined")]
    #[test_case(null, null => true; "null value")]
    #[test_case(string_a, string_b => false; "differing strings")]
    #[test_case(string_a, string_a => true; "equal strings")]
    #[test_case(bool_a, bool_b => false; "differing bools")]
    #[test_case(bool_a, bool_a => true; "equal bools")]
    #[test_case(symbol_a, symbol_b => false; "differing symbols")]
    #[test_case(symbol_a, symbol_a => true; "equal symbols")]
    #[test_case(object_a, object_b => false; "differing objects")]
    #[test_case(object_a, object_a => true; "equal objects")]
    #[test_case(undef, symbol_b => panics "Invalid input args"; "Invalid Input")]
    fn same_value_non_numeric(make_x: ValueMaker, make_y: ValueMaker) -> bool {
        setup_test_agent();
        let x = make_x();
        let y = make_y();

        x.same_value_non_numeric(&y)
    }

    #[test_case(undef, null => false; "different types")]
    #[test_case(number_10, number_10 => true; "equal numbers")]
    #[test_case(number_zero, number_neg_zero => true; "sign of zero irrelevant")]
    #[test_case(number_nan, number_nan => true; "NaNs compare equal")]
    #[test_case(number_10, number_100 => false; "different numbers")]
    #[test_case(bigint_a, bigint_a => true; "equal bigints")]
    #[test_case(bigint_a, bigint_b => false; "different bigints")]
    #[test_case(string_a, string_a => true; "fallthru for nonnumbers")]
    fn same_value_zero(make_x: ValueMaker, make_y: ValueMaker) -> bool {
        setup_test_agent();
        let x = make_x();
        let y = make_y();

        x.same_value_zero(&y)
    }

    #[test_case(undef, null => false; "different types")]
    #[test_case(number_10, number_10 => true; "equal numbers")]
    #[test_case(number_zero, number_neg_zero => false; "sign of zero significant")]
    #[test_case(number_nan, number_nan => true; "NaNs compare equal")]
    #[test_case(number_10, number_100 => false; "different numbers")]
    #[test_case(bigint_a, bigint_a => true; "equal bigints")]
    #[test_case(bigint_a, bigint_b => false; "different bigints")]
    #[test_case(string_a, string_a => true; "fallthru for nonnumbers")]
    fn same_value(make_x: ValueMaker, make_y: ValueMaker) -> bool {
        setup_test_agent();
        let x = make_x();
        let y = make_y();

        x.same_value(&y)
    }

    #[test_case(undef, null => false; "different types")]
    #[test_case(number_10, number_10 => true; "equal numbers")]
    #[test_case(number_zero, number_neg_zero => true; "sign of zero irrelevant")]
    #[test_case(number_nan, number_nan => false; "NaNs compare unequal")]
    #[test_case(number_10, number_100 => false; "different numbers")]
    #[test_case(bigint_a, bigint_a => true; "equal bigints")]
    #[test_case(bigint_a, bigint_b => false; "different bigints")]
    #[test_case(string_a, string_a => true; "fallthru for nonnumbers")]
    fn is_strictly_equal(make_x: ValueMaker, make_y: ValueMaker) -> bool {
        setup_test_agent();
        let x = make_x();
        let y = make_y();

        x.is_strictly_equal(&y)
    }

    #[test_case(undef => ValueKind::Undefined)]
    #[test_case(null => ValueKind::Null)]
    #[test_case(bool_a => ValueKind::Boolean)]
    #[test_case(number_10 => ValueKind::Number)]
    #[test_case(string_a => ValueKind::String)]
    #[test_case(symbol_a => ValueKind::Symbol)]
    #[test_case(bigint_a => ValueKind::BigInt)]
    #[test_case(object_a => ValueKind::Object)]
    fn kind(make_x: ValueMaker) -> ValueKind {
        setup_test_agent();
        let x = make_x();

        x.kind()
    }

    #[test_case(|| None => "undefined"; "choose-none")]
    #[test_case(
        || {
            let obj = ordinary_object_create(None);
            obj.create_data_property_or_throw("item", 10).unwrap();
            Some(obj)
        }
        => "item:10";
        "choose-some"
    )]
    fn to_obj_or_undefined(make_input: impl FnOnce() -> Option<Object>) -> String {
        setup_test_agent();
        let inp = make_input();
        ECMAScriptValue::to_obj_or_undefined(inp).test_result_string()
    }

    #[test_case(|| None => "null"; "choose-none")]
    #[test_case(
        || {
            let obj = ordinary_object_create(None);
            obj.create_data_property_or_throw("item", 10).unwrap();
            Some(obj)
        }
        => "item:10";
        "choose-some"
    )]
    fn to_obj_or_null(make_input: impl FnOnce() -> Option<Object>) -> String {
        setup_test_agent();
        let inp = make_input();
        ECMAScriptValue::to_obj_or_null(inp).test_result_string()
    }

    #[test_case(f64::NAN => Ok(0); "NaN")]
    #[test_case(f64::NEG_INFINITY => Ok(0); "neg inf")]
    #[test_case(f64::INFINITY => Ok(0); "inf")]
    #[test_case(0.0 => Ok(0); "zero")]
    #[test_case(-0.0 => Ok(0); "neg zero")]
    #[test_case(f64::from(0x7FFF) => Ok(0x7FFF); "upper limit")]
    #[test_case(32768.0 => Ok(-32768); "lower rollover")]
    #[test_case(-32768.0 => Ok(-32768); "lower limit")]
    #[test_case(BigInt::from(10) => Err("TypeError: BigInt values cannot be converted to Number values".to_string()); "throw")]
    fn to_int16(arg: impl Into<ECMAScriptValue>) -> Result<i16, String> {
        setup_test_agent();
        arg.into().to_int16().map_err(unwind_any_error)
    }

    #[test_case(f64::NAN => Ok(0); "NaN")]
    #[test_case(f64::NEG_INFINITY => Ok(0); "neg inf")]
    #[test_case(f64::INFINITY => Ok(0); "inf")]
    #[test_case(0.0 => Ok(0); "zero")]
    #[test_case(-0.0 => Ok(0); "neg zero")]
    #[test_case(f64::from(0x7F) => Ok(0x7F); "upper limit")]
    #[test_case(128.0 => Ok(-128); "lower rollover")]
    #[test_case(-128.0 => Ok(-128); "lower limit")]
    #[test_case(BigInt::from(10) => Err("TypeError: BigInt values cannot be converted to Number values".to_string()); "throw")]
    fn to_int8(arg: impl Into<ECMAScriptValue>) -> Result<i8, String> {
        setup_test_agent();
        arg.into().to_int8().map_err(unwind_any_error)
    }

    #[test_case(f64::NAN => Ok(0); "NaN")]
    #[test_case(f64::NEG_INFINITY => Ok(0); "neg inf")]
    #[test_case(f64::INFINITY => Ok(0); "inf")]
    #[test_case(0.0 => Ok(0); "zero")]
    #[test_case(-0.0 => Ok(0); "neg zero")]
    #[test_case(f64::from(0x7FFF_FFFF) => Ok(0x7FFF_FFFF); "upper limit")]
    #[test_case(2_147_483_648.0 => Ok(-2_147_483_648); "lower rollover")]
    #[test_case(-2_147_483_648.0 => Ok(-2_147_483_648); "lower limit")]
    #[test_case(BigInt::from(10) => Err("TypeError: BigInt values cannot be converted to Number values".to_string()); "throw")]
    fn to_int32(arg: impl Into<ECMAScriptValue>) -> Result<i32, String> {
        setup_test_agent();
        arg.into().to_int32().map_err(unwind_any_error)
    }

    #[test_case(f64::NAN => Ok(0); "NaN")]
    #[test_case(f64::NEG_INFINITY => Ok(0); "neg inf")]
    #[test_case(f64::INFINITY => Ok(0); "inf")]
    #[test_case(0.0 => Ok(0); "zero")]
    #[test_case(-0.0 => Ok(0); "neg zero")]
    #[test_case(4_294_967_295.0 => Ok(0xFFFF_FFFF); "upper limit")]
    #[test_case(4_294_967_296.0 => Ok(0); "rollover")]
    #[test_case(-300.0 => Ok(4_294_966_996); "negative inputs")]
    #[test_case(BigInt::from(10) => Err("TypeError: BigInt values cannot be converted to Number values".to_string()); "throw")]
    fn to_uint32(arg: impl Into<ECMAScriptValue>) -> Result<u32, String> {
        setup_test_agent();
        arg.into().to_uint32().map_err(unwind_any_error)
    }

    #[test_case(f64::NAN => Ok(0); "NaN")]
    #[test_case(f64::NEG_INFINITY => Ok(0); "neg inf")]
    #[test_case(f64::INFINITY => Ok(0); "inf")]
    #[test_case(0.0 => Ok(0); "zero")]
    #[test_case(-0.0 => Ok(0); "neg zero")]
    #[test_case(65535.0 => Ok(0xFFFF); "upper limit")]
    #[test_case(65536.0 => Ok(0); "rollover")]
    #[test_case(-300.0 => Ok(65236); "negative inputs")]
    #[test_case(BigInt::from(10) => Err("TypeError: BigInt values cannot be converted to Number values".to_string()); "throw")]
    fn to_uint16(arg: impl Into<ECMAScriptValue>) -> Result<u16, String> {
        setup_test_agent();
        arg.into().to_uint16().map_err(unwind_any_error)
    }

    #[test_case(f64::NAN => Ok(0); "NaN")]
    #[test_case(f64::NEG_INFINITY => Ok(0); "neg inf")]
    #[test_case(f64::INFINITY => Ok(0); "inf")]
    #[test_case(0.0 => Ok(0); "zero")]
    #[test_case(-0.0 => Ok(0); "neg zero")]
    #[test_case(255.0 => Ok(0xFF); "upper limit")]
    #[test_case(256.0 => Ok(0); "rollover")]
    #[test_case(-200.0 => Ok(56); "negative inputs")]
    #[test_case(BigInt::from(10) => Err("TypeError: BigInt values cannot be converted to Number values".to_string()); "throw")]
    fn to_uint8(arg: impl Into<ECMAScriptValue>) -> Result<u8, String> {
        setup_test_agent();
        arg.into().to_uint8().map_err(unwind_any_error)
    }

    #[test]
    fn to_number_01() {
        setup_test_agent();
        let input = ECMAScriptValue::Undefined;

        let result = input.to_number().unwrap();
        assert!(result.is_nan());
    }
    #[test]
    #[expect(clippy::float_cmp)]
    fn to_number_02() {
        setup_test_agent();
        let input = ECMAScriptValue::Null;

        let result = input.to_number().unwrap();
        assert_eq!(result, 0.0);
    }
    #[test]
    #[expect(clippy::float_cmp)]
    fn to_number_03() {
        setup_test_agent();
        let input = ECMAScriptValue::from(true);

        let result = input.to_number().unwrap();
        assert_eq!(result, 1.0);
    }
    #[test]
    #[expect(clippy::float_cmp)]
    fn to_number_04() {
        setup_test_agent();
        let input = ECMAScriptValue::from(false);

        let result = input.to_number().unwrap();
        assert_eq!(result, 0.0);
    }
    #[test]
    #[expect(clippy::float_cmp)]
    fn to_number_05() {
        setup_test_agent();
        let input = ECMAScriptValue::from(37.6);

        let result = input.to_number().unwrap();
        assert_eq!(result, 37.6);
    }
    #[test]
    fn to_number_06() {
        setup_test_agent();
        let input = ECMAScriptValue::from("blue");

        let result = input.to_number().unwrap();
        assert!(result.is_nan());
    }
    #[test]
    #[expect(clippy::float_cmp)]
    fn to_number_07() {
        setup_test_agent();
        let testcases = [
            ("", 0.0),
            ("1", 1.0),
            ("   12   ", 12.0),
            ("-Infinity", f64::NEG_INFINITY),
            ("0030", 30.0),
            (" \t\r\n\u{a0}\u{2029}\u{2028}\u{b}\u{c}\u{feff}", 0.0),
            ("0xabcd", 43981.0),
            ("0X6A8BB", 436_411.0),
            ("0b010", 2.0),
            ("0B110", 6.0),
            ("0o766", 502.0),
            ("0O7", 7.0),
            ("0xabcdabcdabcdabcdabcdabcdabcdabcd", 228_365_892_722_206_371_581_333_312_115_001_109_453.0),
        ];

        for (s, e) in testcases {
            let result = ECMAScriptValue::from(s).to_number().unwrap();
            assert_eq!(result, e);
        }
    }
    #[test]
    fn to_number_08() {
        setup_test_agent();
        let input = ECMAScriptValue::from(BigInt::from(10));

        let result = input.to_number().unwrap_err();
        assert_eq!(unwind_type_error(result), "BigInt values cannot be converted to Number values");
    }
    #[test]
    fn to_number_09() {
        setup_test_agent();
        let input = ECMAScriptValue::from(Symbol::new(None));

        let result = input.to_number().unwrap_err();
        assert_eq!(unwind_type_error(result), "Symbol values cannot be converted to Number values");
    }
    #[test]
    fn to_number_10() {
        setup_test_agent();
        let obj = ordinary_object_create(None);
        let input = ECMAScriptValue::from(obj);

        let result = input.to_number().unwrap_err();
        assert_eq!(unwind_type_error(result), "Cannot convert object to primitive value");
    }
    #[test]
    fn to_number_11() {
        setup_test_agent();
        let obj_proto = intrinsic(IntrinsicId::ObjectPrototype);
        let obj = ordinary_object_create(Some(obj_proto));
        let input = ECMAScriptValue::from(obj);

        let result = input.to_number().unwrap();
        assert!(result.is_nan());
    }

    #[test]
    fn to_integer_or_infinity_02() {
        setup_test_agent();
        let sym = Symbol::new(None);

        let result = ECMAScriptValue::from(sym).to_integer_or_infinity().unwrap_err();
        assert_eq!(unwind_type_error(result), "Symbol values cannot be converted to Number values");
    }

    #[test_case(|| ECMAScriptValue::from("bob") => None; "not an object")]
    #[test_case(
        || ECMAScriptValue::Object(intrinsic(IntrinsicId::Array))
        => Some(IntrinsicId::Array);
        "is constructor"
    )]
    #[test_case(
        || ECMAScriptValue::Object(intrinsic(IntrinsicId::ArrayPrototype))
        => None;
        "object but not constructor"
    )]
    fn as_constructor(make_input: impl FnOnce() -> ECMAScriptValue) -> Option<IntrinsicId> {
        setup_test_agent();
        let inp = make_input();
        inp.as_constructor().and_then(Object::which_intrinsic)
    }

    #[test_case(|| ECMAScriptValue::from(10.0) => Ok(10); "in range")]
    #[test_case(|| ECMAScriptValue::from(0.0) => Ok(0); "bottom edge")]
    #[test_case(|| ECMAScriptValue::from(-1.0) => Ok(0); "under")]
    #[test_case(|| ECMAScriptValue::from(9_007_199_254_740_991.0) => Ok(9_007_199_254_740_991); "top edge")]
    #[test_case(|| ECMAScriptValue::from(9_007_199_254_740_992.0) => Ok(9_007_199_254_740_991); "over")]
    #[test_case(|| ECMAScriptValue::from(Symbol::new(Some("test".into()))) => Err("Symbol values cannot be converted to Number values".to_string()); "not a number")]
    fn to_length(make_arg: fn() -> ECMAScriptValue) -> Result<i64, String> {
        setup_test_agent();
        let arg = make_arg();

        arg.to_length().map_err(unwind_type_error)
    }
}

#[test]
fn symbol_debug() {
    setup_test_agent();
    assert_ne!(format!("{:?}", wks(WksId::ToPrimitive)), "");
}
#[test]
fn symbol_display_normal() {
    setup_test_agent();
    let symbol = Symbol::new(Some(JSString::from("Normal")));
    assert_eq!(format!("{symbol}"), "Symbol(Normal)");
}
#[test]
fn symbol_display_empty() {
    setup_test_agent();
    let symbol = Symbol::new(None);
    assert_eq!(format!("{symbol}"), "Symbol()");
}
#[test]
#[expect(clippy::redundant_clone)]
fn symbol_clone() {
    setup_test_agent();
    let s1 = wks(WksId::ToPrimitive);
    let s2 = s1.clone();
    assert_eq!(s1, s2);
}
#[test]
#[expect(clippy::redundant_clone)]
fn symbol_hash() {
    setup_test_agent();
    let s1 = wks(WksId::ToPrimitive);
    let s2 = s1.clone();
    let s3 = wks(WksId::Unscopables);

    let factory = RandomState::new();

    assert_eq!(s1, s2);
    assert_eq!(calculate_hash(&factory, &s1), calculate_hash(&factory, &s2));
    assert_ne!(s1, s3);
    assert_ne!(calculate_hash(&factory, &s1), calculate_hash(&factory, &s3));

    let f2 = std::collections::hash_map::RandomState::new();
    assert_eq!(s1, s2);
    assert_eq!(calculate_hash(&f2, &s1), calculate_hash(&f2, &s2));
    assert_ne!(s1, s3);
    assert_ne!(calculate_hash(&f2, &s1), calculate_hash(&f2, &s3));
}
#[test]
#[expect(clippy::redundant_clone)]
fn symbol_new() {
    setup_test_agent();
    let s1 = Symbol::new(Some(JSString::from("Symbol #1")));
    let s2 = Symbol::new(Some(JSString::from("Symbol #2")));
    let s3 = Symbol::new(Some(JSString::from("Symbol #1")));
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
    setup_test_agent();
    let s1 = Symbol::new(Some(JSString::from("Test Symbol")));
    assert_eq!(s1.description(), Some(JSString::from("Test Symbol")));
}
#[test]
fn symbol_internals_debug() {
    assert_ne!(format!("{:?}", SymbolInternals { id: 10, description: Some(JSString::from("description")) }), "");
}
#[test]
#[expect(clippy::redundant_clone)]
fn symbol_internals_clone() {
    let si1 = SymbolInternals { id: 10, description: Some(JSString::from("description")) };
    let si2 = si1.clone();
    assert_eq!(si1.id, si2.id);
    assert_eq!(si1.description, si2.description);
}

mod pn {
    use super::*;
    use ahash::AHasher;

    #[test]
    #[expect(clippy::clone_on_copy)]
    fn derived_stuff() {
        let pn = PN(());
        let b = pn; // Copy
        let c = pn.clone();
        assert_ne!(format!("{b:?}"), "");
        assert_eq!(b == c, true);
        assert_eq!(b != c, false);
        assert_eq!(b.cmp(&c), Ordering::Equal);
        assert_eq!(b.partial_cmp(&c), Some(Ordering::Equal));

        let mut hasher = AHasher::new_with_keys(1234, 5678);
        b.hash(&mut hasher);
    }
}

mod private_name {
    use super::*;
    use ahash::AHashSet;
    use test_case::test_case;

    #[test_case("my name" => JSString::from("my name"); "str")]
    #[test_case(String::from("blue") => JSString::from("blue"); "String")]
    #[test_case(JSString::from("alpha") => JSString::from("alpha"); "JSString")]
    fn new(description: impl Into<JSString>) -> JSString {
        let pn = PrivateName::new(description);
        pn.description
    }
    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", PrivateName::new("private_name")), "");
    }

    #[test]
    fn eq() {
        let pn1 = PrivateName::new("a");
        let pn2 = PrivateName::new("a");
        assert_eq!(pn1 == pn2, false);
    }
    #[test]
    #[expect(clippy::redundant_clone)]
    fn clone() {
        let pn1 = PrivateName::new("a");
        let pn2 = pn1.clone();
        assert_eq!(pn1, pn2);
    }
    #[test]
    fn hash() {
        let mut s: AHashSet<PrivateName> = AHashSet::default();
        s.insert(PrivateName::new("blue"));
        s.insert(PrivateName::new("green"));
        s.insert(PrivateName::new("red"));
        s.insert(PrivateName::new("blue"));
        assert_eq!(s.len(), 4);
    }

    #[test_case(&PrivateName::new("something") => "PN[something]"; "normal")]
    fn display(pn: &PrivateName) -> String {
        format!("{pn}")
    }
}

mod private_element_kind {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        let pek = PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from("a")) };
        assert_ne!(format!("{pek:?}"), "");
    }
    #[test]
    #[expect(clippy::redundant_clone)]
    fn clone() {
        let pek = PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from("a")) };
        let pek2 = pek.clone();
        assert!(matches!(pek2, PrivateElementKind::Field { value: _ }));
        if let PrivateElementKind::Field { value } = pek2 {
            assert_eq!(*value.borrow(), ECMAScriptValue::from("a"));
        }
    }

    #[test_case(
        || PrivateElementKind::Field{value: RefCell::new(ECMAScriptValue::from("field"))} => "Field(field)"; "field"
    )]
    #[test_case(|| PrivateElementKind::Method{value: ECMAScriptValue::from("method")} => "Method(method)"; "method")]
    #[test_case(|| PrivateElementKind::Accessor{ get: None, set: None } => "Accessor(-,-)"; "empty accessor")]
    #[test_case(
        || PrivateElementKind::Accessor{ get: Some(intrinsic(IntrinsicId::IsNaN)), set: None }
        => "Accessor(isNaN,-)";
        "get-only accessor"
    )]
    #[test_case(
        || PrivateElementKind::Accessor{ get: None, set: Some(intrinsic(IntrinsicId::IsFinite)) }
        => "Accessor(-,isFinite)";
        "set-only accessor"
    )]
    #[test_case(
        || {
            PrivateElementKind::Accessor{
                get: Some(intrinsic(IntrinsicId::IsNaN)),
                set: Some(intrinsic(IntrinsicId::IsFinite))
            }
        }
        => "Accessor(isNaN,isFinite)";
        "full accessor"
    )]
    #[test_case(
        || PrivateElementKind::Accessor{ get: Some(DeadObject::object()), set: None }
        => "Accessor(unnamed,-)";
        "get-dead accessor"
    )]
    #[test_case(
        || PrivateElementKind::Accessor{ get: None, set: Some(DeadObject::object()) }
        => "Accessor(-,unnamed)";
        "set-dead accessor"
    )]
    #[test_case(
        || PrivateElementKind::Accessor{ get: Some(DeadObject::object()), set: Some(DeadObject::object()) }
        => "Accessor(unnamed,unnamed)";
        "dead accessor"
    )]
    fn display_fmt(make_item: impl FnOnce() -> PrivateElementKind) -> String {
        setup_test_agent();
        let item = make_item();
        format!("{item}")
    }

    #[test_case(
        &PrivateElementKind::Field{ value: RefCell::new(ECMAScriptValue::from("field")) },
        &PrivateElementKind::Field{value: RefCell::new(ECMAScriptValue::from("field")) }
        => true;
        "field equal"
    )]
    #[test_case(
        &PrivateElementKind::Field{value: RefCell::new(ECMAScriptValue::from("field")) },
        &PrivateElementKind::Method{value: ECMAScriptValue::from("method")}
        => false;
        "field/method"
    )]
    fn eq(left: &PrivateElementKind, right: &PrivateElementKind) -> bool {
        left == right
    }
}

mod private_element {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        let pe = PrivateElement {
            key: PrivateName::new("just a key"),
            kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from("just some data")) },
        };
        assert_ne!(format!("{pe:?}"), "");
    }

    #[test]
    fn clone() {
        let pe = PrivateElement {
            key: PrivateName::new("just a key"),
            kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from("just some data")) },
        };
        let cloned = pe.clone();
        assert_eq!(pe.key, cloned.key);
        if let PrivateElementKind::Field { value } = pe.kind {
            assert_eq!(*value.borrow(), ECMAScriptValue::from("just some data"));
        } else {
            panic!("Came back with the wrong kind!")
        }
    }

    #[test]
    fn display_fmt() {
        let pe = PrivateElement {
            key: PrivateName::new("just a key"),
            kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from("just some data")) },
        };
        assert_eq!(format!("{pe}"), "PrivateElement{PN[just a key]: Field(just some data)}");
    }

    #[test_case(
        || {
            let pe = PrivateElement {
                key: PrivateName::new("key"),
                kind: PrivateElementKind::Field { value: RefCell::new(ECMAScriptValue::from("value")) },
            };
            (pe.clone(), pe)
        }
        => true;
        "equal"
    )]
    fn eq(make_items: impl FnOnce() -> (PrivateElement, PrivateElement)) -> bool {
        setup_test_agent();
        let (left, right) = make_items();
        left == right
    }
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

    setup_test_agent();
    assert_eq!(to_boolean(ECMAScriptValue::from(wks(WksId::ToPrimitive))), true);
    let o = ordinary_object_create(None);
    assert_eq!(to_boolean(ECMAScriptValue::from(o)), true);
}

mod property_key {
    use super::*;
    use test_case::test_case;

    #[test]
    fn ne() {
        let pk1 = PropertyKey::from("bob");
        let pk2 = PropertyKey::from("phil");
        let pk3 = PropertyKey::from("bob");

        assert_eq!(pk1 != pk2, true);
        assert_eq!(pk1 != pk3, false);
    }

    #[test_case(|| "a" => "a"; "_ &str")]
    #[test_case(|| JSString::from("b") => "b"; "_ JSString")]
    #[test_case(|| wks(WksId::ToPrimitive) => "Symbol(Symbol.toPrimitive)"; "_ Symbol")]
    #[test_case(|| String::from("d") => "d"; "_ String")]
    #[test_case(|| 848_183_usize => "848183"; "_ usize")]
    #[test_case(|| 282_i32 => "282"; "_ i32")]
    #[test_case(|| 828_u64 => "828"; "_ u64")]
    fn from<X>(make_val: impl FnOnce() -> X) -> String
    where
        PropertyKey: From<X>,
    {
        setup_test_agent();
        let val = make_val();
        let key = PropertyKey::from(val);
        format!("{key}")
    }

    #[test_case(|| JSString::from("c") => "c"; "SString ref")]
    fn from_ref<X>(make_val: impl FnOnce() -> X) -> String
    where
        PropertyKey: for<'a> From<&'a X>,
    {
        setup_test_agent();
        let val = make_val();
        let key = PropertyKey::from(&val);
        format!("{key}")
    }

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", PropertyKey::from("a")), "");
    }
    #[test]
    fn display() {
        setup_test_agent();
        let sym = wks(WksId::HasInstance);
        assert_eq!(format!("{}", PropertyKey::from("tangerine")), "tangerine");
        assert_eq!(format!("{}", PropertyKey::from(sym)), "Symbol(Symbol.hasInstance)");
    }
    #[test]
    #[expect(clippy::redundant_clone)]
    fn clone() {
        let pk1 = PropertyKey::from("a");
        let pk2 = pk1.clone();
        assert_eq!(pk1, pk2);
    }
    #[test]
    fn is_array_index() {
        setup_test_agent();
        assert_eq!(PropertyKey::from("0").is_array_index(), true);
        assert_eq!(PropertyKey::from("10").is_array_index(), true);
        assert_eq!(PropertyKey::from("0.25").is_array_index(), false);
        assert_eq!(PropertyKey::from("0  ").is_array_index(), false);
        assert_eq!(PropertyKey::from("  0").is_array_index(), false);
        assert_eq!(PropertyKey::from("-20").is_array_index(), false);
        assert_eq!(PropertyKey::from("4294967294").is_array_index(), true);
        assert_eq!(PropertyKey::from("4294967295").is_array_index(), false);
        assert_eq!(PropertyKey::from("4294967296").is_array_index(), false);
        assert_eq!(PropertyKey::from("010").is_array_index(), false);
        assert_eq!(PropertyKey::from("000").is_array_index(), false);
        assert_eq!(PropertyKey::from(wks(WksId::ToPrimitive)).is_array_index(), false);
    }
    #[test_case(|| PropertyKey::from("alice"), || ECMAScriptValue::from("alice"); "string")]
    #[test_case(|| PropertyKey::from(wks(WksId::ToPrimitive)), || ECMAScriptValue::from(wks(WksId::ToPrimitive)); "symbol")]
    fn into_ecmascriptvalue(make_key: fn() -> PropertyKey, make_expected: fn() -> ECMAScriptValue) {
        setup_test_agent();
        let key = make_key();
        let expected = make_expected();
        assert_eq!(ECMAScriptValue::from(key), expected);
    }

    mod try_from {
        use super::*;
        use test_case::test_case;

        #[test_case(ECMAScriptValue::from(10) => serr("Bad type for property key"); "not key")]
        #[test_case(ECMAScriptValue::from("key") => Ok(PropertyKey::String("key".into())); "string")]
        fn simple(val: ECMAScriptValue) -> Result<PropertyKey, String> {
            PropertyKey::try_from(val).map_err(|e| e.to_string())
        }

        #[test]
        fn symbol() {
            setup_test_agent();
            let val = ECMAScriptValue::from(wks(WksId::ToPrimitive));
            let pk = PropertyKey::try_from(val).unwrap();
            assert_eq!(pk, PropertyKey::from(wks(WksId::ToPrimitive)));
        }
    }

    #[test_case(&ahash::RandomState::new(); "ahash hasher")]
    #[test_case(&std::collections::hash_map::RandomState::new(); "std hasher")]
    fn hash(factory: &impl BuildHasher) {
        setup_test_agent();

        let code_a = PropertyKey::from("bob");
        let code_b = PropertyKey::from(Symbol::new(Some("alice".into())));

        let hash_a = calculate_hash(factory, &code_a);
        let hash_b = calculate_hash(factory, &code_b);

        assert_ne!(hash_a, hash_b);
        assert_eq!(hash_a, calculate_hash(factory, &code_a));
    }
}

mod jsstring {
    use super::*;
    use test_case::test_case;

    mod try_from {
        use super::*;
        use test_case::test_case;

        #[test_case(|| PropertyKey::from("key") => sok("key"); "string")]
        #[test_case(|| PropertyKey::from(wks(WksId::ToPrimitive)) => serr("Expected String-valued property key"); "symbol")]
        fn property_key(maker: fn() -> PropertyKey) -> Result<String, String> {
            setup_test_agent();
            let key = maker();
            JSString::try_from(key).map_err(|e| e.to_string()).map(String::from)
        }
        #[test_case(|| PropertyKey::from("key") => sok("key"); "string")]
        #[test_case(|| PropertyKey::from(wks(WksId::ToPrimitive)) => serr("Expected String-valued property key"); "symbol")]
        fn property_key_ref(maker: fn() -> PropertyKey) -> Result<String, String> {
            setup_test_agent();
            let key = maker();
            JSString::try_from(&key).map_err(|e| e.to_string()).map(String::from)
        }

        #[test_case(|| ECMAScriptValue::Undefined => sok("undefined"); "undefined")]
        #[test_case(|| ECMAScriptValue::Null => sok("null"); "null")]
        #[test_case(|| ECMAScriptValue::Boolean(true) => sok("true"); "bool true")]
        #[test_case(|| ECMAScriptValue::Boolean(false) => sok("false"); "bool false")]
        #[test_case(|| ECMAScriptValue::String("boo".into()) => sok("boo"); "string")]
        #[test_case(|| ECMAScriptValue::Number(3321.3) => sok("3321.3"); "number")]
        #[test_case(|| ECMAScriptValue::BigInt(Rc::new(BigInt::from(1000))) => sok("1000"); "bigint")]
        #[test_case(|| ECMAScriptValue::Symbol(wks(WksId::ToPrimitive)) => serr("Symbols may not be converted to strings"); "symbol")]
        #[test_case(|| {
            let obj = ordinary_object_create(None);
            ECMAScriptValue::Object(obj)
        } => serr("Object to string conversions require an agent"); "object")]
        fn ecmasript_value(maker: fn() -> ECMAScriptValue) -> Result<String, String> {
            setup_test_agent();
            let value = maker();
            JSString::try_from(value).map_err(|e| e.to_string()).map(String::from)
        }
    }

    #[test_case("0" => using check_value(0.0); "zero")]
    #[test_case("-0" => using check_value(-0.0); "neg-zero")]
    #[test_case("10" => using check_value(10.0); "ten")]
    #[test_case("not-a-number" => using check_value(f64::NAN); "not-a-number")]
    #[test_case("0x1f" => using check_value(31.0); "hex value")]
    #[test_case("0o17" => using check_value(15.0); "octal value")]
    #[test_case("0b0110" => using check_value(6.0); "binary value")]
    fn to_number(s: &str) -> f64 {
        let s = JSString::from(s);
        s.to_number()
    }

    #[test_case("0" => using check_value(0.0); "zero")]
    #[test_case("-0" => using check_value(0.0); "neg-zero")]
    #[test_case("not-a-number" => using check_value(0.0); "nan")]
    #[test_case("Infinity" => using check_value(0.0); "Infinity")]
    #[test_case("10" => using check_value(10.0); "ten")]
    #[test_case("257" => using check_value(1.0); "one over the modulo")]
    #[test_case("-513" => using check_value(-1.0); "negative, one beyond")]
    fn to_core_int(s: &str) -> f64 {
        let s = JSString::from(s);
        s.to_core_int(256.0)
    }

    #[test_case("0" => 0; "zero")]
    #[test_case("-1" => 0xFFFF_FFFF; "neg one")]
    fn to_uint32(s: &str) -> u32 {
        JSString::from(s).to_uint32()
    }
}

mod numeric {
    use super::*;
    use test_case::test_case;

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", Numeric::Number(0.0)), "");
    }
    #[test]
    fn partialeq() {
        let num1 = Numeric::Number(10.0);
        let num2 = Numeric::Number(-10.0);

        assert_eq!(num1 != num2, true);
        assert_eq!(num1 == num2, false);
    }

    #[test_case(ECMAScriptValue::from("not-a-number") => serr("Value not numeric"); "not-a-number")]
    #[test_case(ECMAScriptValue::from(10) => Ok(Numeric::Number(10.0)); "float")]
    #[test_case(ECMAScriptValue::from(BigInt::from(100)) => Ok(Numeric::BigInt(Rc::new(BigInt::from(100)))); "bigint")]
    fn try_from(val: ECMAScriptValue) -> Result<Numeric, String> {
        Numeric::try_from(val).map_err(|e| e.to_string())
    }
}

#[test]
fn to_numeric_01() {
    setup_test_agent();
    let obj = ordinary_object_create(None);
    let result = to_numeric(ECMAScriptValue::from(obj)).unwrap_err();
    assert_eq!(unwind_type_error(result), "Cannot convert object to primitive value");
}
#[test]
fn to_numeric_02() {
    setup_test_agent();
    let result = to_numeric(ECMAScriptValue::from(BigInt::from(4_747_474))).unwrap();
    assert_eq!(result, Numeric::BigInt(Rc::new(BigInt::from(4_747_474))));
}
#[test]
fn to_numeric_03() {
    setup_test_agent();
    let result = to_numeric(ECMAScriptValue::from(10)).unwrap();
    assert_eq!(result, Numeric::Number(10.0));
}
#[test]
fn to_numeric_04() {
    setup_test_agent();
    let sym = Symbol::new(None);
    let result = to_numeric(ECMAScriptValue::from(sym)).unwrap_err();
    assert_eq!(unwind_type_error(result), "Symbol values cannot be converted to Number values");
}

#[test]
#[expect(clippy::float_cmp)]
fn to_integer_or_infinity_01() {
    setup_test_agent();
    let testcases = &[
        (f64::NAN, 0.0),
        (f64::INFINITY, f64::INFINITY),
        (f64::NEG_INFINITY, f64::NEG_INFINITY),
        (0.0, 0.0),
        (-0.0, 0.0),
        (10.2, 10.0),
        (-10.2, -10.0),
    ];

    for (val, expected) in testcases {
        let result = to_integer_or_infinity(*val);
        assert_eq!(result, *expected);
    }
}

#[test]
fn to_string_01() {
    setup_test_agent();
    let result = to_string(ECMAScriptValue::Undefined).unwrap();
    assert_eq!(result, "undefined");
}
#[test]
fn to_string_02() {
    setup_test_agent();
    let result = to_string(ECMAScriptValue::Null).unwrap();
    assert_eq!(result, "null");
}
#[test]
fn to_string_03() {
    setup_test_agent();
    let result = to_string(ECMAScriptValue::from(true)).unwrap();
    assert_eq!(result, "true");
}
#[test]
fn to_string_04() {
    setup_test_agent();
    let result = to_string(ECMAScriptValue::from(false)).unwrap();
    assert_eq!(result, "false");
}
#[test]
fn to_string_05() {
    setup_test_agent();
    let result = to_string(ECMAScriptValue::from(10)).unwrap();
    assert_eq!(result, "10");
}
#[test]
fn to_string_06() {
    setup_test_agent();
    let result = to_string(ECMAScriptValue::from("blue")).unwrap();
    assert_eq!(result, "blue");
}
#[test]
fn to_string_07() {
    setup_test_agent();
    let sym = Symbol::new(None);
    let result = to_string(ECMAScriptValue::Symbol(sym)).unwrap_err();
    assert_eq!(unwind_type_error(result), "Symbols may not be converted to strings");
}
#[test]
fn to_string_08() {
    setup_test_agent();
    let obj_proto = intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(Some(obj_proto));
    let result = to_string(ECMAScriptValue::from(obj)).unwrap();
    assert_eq!(result, "[object Object]");
}
#[test]
fn to_string_09() {
    setup_test_agent();
    let obj = ordinary_object_create(None);
    let result = to_string(ECMAScriptValue::from(obj)).unwrap_err();
    assert_eq!(unwind_type_error(result), "Cannot convert object to primitive value");
}
#[expect(clippy::unnecessary_wraps)]
fn tostring_symbol(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let sym = Symbol::new(None);
    Ok(ECMAScriptValue::from(sym))
}
#[test]
fn to_string_10() {
    setup_test_agent();
    let obj = ordinary_object_create(None);
    let badtostring =
        create_builtin_function(tostring_symbol, None, 0_f64, PropertyKey::from("toString"), &[], None, None, None);
    obj.create_data_property("toString", badtostring).unwrap();

    let result = to_string(ECMAScriptValue::from(obj)).unwrap_err();
    assert_eq!(unwind_type_error(result), "Symbols may not be converted to strings");
}
#[test]
fn to_string_11() {
    setup_test_agent();
    let result = to_string(ECMAScriptValue::from(BigInt::from(789_123))).unwrap();
    assert_eq!(result, "789123");
}
#[test]
fn to_string_12() {
    setup_test_agent();
    let result = DeadObject::object().to_string();
    assert_eq!(unwind_any_error(result.unwrap_err()), "TypeError: get called on DeadObject");
}

#[test]
fn to_object_01() {
    setup_test_agent();
    let err = to_object(ECMAScriptValue::Undefined).unwrap_err();
    let msg = unwind_type_error(err);
    assert_eq!(msg, "Undefined and null cannot be converted to objects");
}
#[test]
fn to_object_02() {
    setup_test_agent();
    let err = to_object(ECMAScriptValue::Null).unwrap_err();
    let msg = unwind_type_error(err);
    assert_eq!(msg, "Undefined and null cannot be converted to objects");
}
#[test]
fn to_object_03() {
    setup_test_agent();
    let test_value = true;
    let result = to_object(ECMAScriptValue::from(test_value)).unwrap();

    let boolean_obj = result.o.to_boolean_obj().unwrap();
    assert_eq!(*boolean_obj.boolean_data(), test_value);
}
#[test]
#[expect(clippy::float_cmp)]
fn to_object_04() {
    setup_test_agent();
    let test_value = 1337.0;
    let result = to_object(ECMAScriptValue::from(test_value)).unwrap();

    let number_obj = result.o.to_number_obj().unwrap();
    assert_eq!(*number_obj.number_data(), test_value);
}
#[test]
fn to_object_05() {
    setup_test_agent();
    let test_value = "orange";
    let result = to_object(ECMAScriptValue::from(test_value)).unwrap();

    let val = result.to_string().unwrap();
    assert_eq!(val, JSString::from(test_value));
}
#[test]
fn to_object_06() {
    setup_test_agent();
    let test_value = wks(WksId::ToPrimitive);
    let result = to_object(ECMAScriptValue::from(test_value)).unwrap();
    let desc = result.get(&"description".into()).unwrap();
    assert_eq!(desc, ECMAScriptValue::from("Symbol.toPrimitive"));
}
#[test]
fn to_object_07() {
    setup_test_agent();
    let test_value = BigInt::from(10);
    let result = to_object(ECMAScriptValue::from(test_value.clone())).unwrap();
    let bi_obj = result.o.to_bigint_object().unwrap();
    assert_eq!(*bi_obj.value(), test_value);
}
#[test]
fn to_object_08() {
    setup_test_agent();
    let test_value = ordinary_object_create(None);
    let id = test_value.o.id();
    let result = to_object(ECMAScriptValue::from(test_value)).unwrap();
    assert_eq!(result.o.id(), id);
}

// ordinary_to_primitive:
// * non-object value & non-object string -> two diff results
// * object value & non-object string -> always the string
// * non-object value & object string -> always the value
// * object value & object string -> type error

// non-object number
#[expect(clippy::unnecessary_wraps)]
fn faux_makes_number(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    Ok(ECMAScriptValue::from(123_456))
}
// non-object string
#[expect(clippy::unnecessary_wraps)]
fn faux_makes_string(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    Ok(ECMAScriptValue::from("test result"))
}
// object value
#[expect(clippy::unnecessary_wraps)]
fn faux_makes_obj(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
    let obj = ordinary_object_create(Some(object_prototype));
    Ok(ECMAScriptValue::from(obj))
}
// error
fn faux_errors(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    Err(create_type_error("Test Sentinel"))
}
#[derive(Copy, Clone)]
enum FauxKind {
    Object,
    Primitive,
    Error,
}
fn make_test_obj(valueof: FauxKind, tostring: FauxKind) -> Object {
    let realm = current_realm_record().unwrap();
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_proto = realm.borrow().intrinsics.function_prototype.clone();
    let target = ordinary_object_create(Some(object_prototype));
    let connect = |name, length, steps| {
        let key = PropertyKey::from(name);
        let fcn = create_builtin_function(
            steps,
            None,
            length,
            key.clone(),
            BUILTIN_FUNCTION_SLOTS,
            Some(realm.clone()),
            Some(function_proto.clone()),
            None,
        );
        define_property_or_throw(
            &target,
            key,
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::from(fcn)),
                writable: Some(true),
                enumerable: Some(false),
                configurable: Some(true),
                ..Default::default()
            },
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
pub fn make_tostring_getter_error() -> Object {
    // valueOf returns 123456; tostring is a getter that errors
    let realm = current_realm_record().unwrap();
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_proto = realm.borrow().intrinsics.function_prototype.clone();
    let target = ordinary_object_create(Some(object_prototype));
    let key = PropertyKey::from("valueOf");
    let fcn = create_builtin_function(
        faux_makes_number,
        None,
        0_f64,
        key.clone(),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm.clone()),
        Some(function_proto.clone()),
        None,
    );
    define_property_or_throw(
        &target,
        key,
        PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::from(fcn)),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();

    let key = PropertyKey::from("toString");
    let tostring_getter = create_builtin_function(
        faux_errors,
        None,
        0_f64,
        key.clone(),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm),
        Some(function_proto),
        Some(JSString::from("get")),
    );
    define_property_or_throw(
        &target,
        key,
        PotentialPropertyDescriptor {
            enumerable: Some(false),
            configurable: Some(true),
            get: Some(ECMAScriptValue::from(tostring_getter)),
            ..Default::default()
        },
    )
    .unwrap();

    target
}
pub fn make_test_obj_uncallable() -> Object {
    let realm = current_realm_record().unwrap();
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let target = ordinary_object_create(Some(object_prototype));
    let connect = |name| {
        let key = PropertyKey::from(name);
        define_property_or_throw(
            &target,
            key,
            PotentialPropertyDescriptor {
                value: Some(ECMAScriptValue::Undefined),
                writable: Some(true),
                enumerable: Some(false),
                configurable: Some(true),
                ..Default::default()
            },
        )
        .unwrap();
    };
    connect("valueOf");
    connect("toString");

    target
}

#[test]
fn ordinary_to_primitive_nonoobj() {
    setup_test_agent();
    let test_obj = make_test_obj(FauxKind::Primitive, FauxKind::Primitive);
    let result_1 = ordinary_to_primitive(&test_obj, ConversionHint::Number).unwrap();
    assert_eq!(result_1, ECMAScriptValue::from(123_456));

    let result_2 = ordinary_to_primitive(&test_obj, ConversionHint::String).unwrap();
    assert_eq!(result_2, ECMAScriptValue::from("test result"));
}
#[test]
fn ordinary_to_primitive_obj() {
    setup_test_agent();
    let test_obj = make_test_obj(FauxKind::Object, FauxKind::Object);
    let result_1 = ordinary_to_primitive(&test_obj, ConversionHint::Number).unwrap_err();
    assert_eq!(unwind_type_error(result_1), "Cannot convert object to primitive value");

    let result_2 = ordinary_to_primitive(&test_obj, ConversionHint::String).unwrap_err();
    assert_eq!(unwind_type_error(result_2), "Cannot convert object to primitive value");
}
#[test]
fn ordinary_to_primitive_obj_nonobj() {
    setup_test_agent();
    let test_obj = make_test_obj(FauxKind::Object, FauxKind::Primitive);
    let result_1 = ordinary_to_primitive(&test_obj, ConversionHint::Number).unwrap();
    assert_eq!(result_1, ECMAScriptValue::from("test result"));

    let result_2 = ordinary_to_primitive(&test_obj, ConversionHint::String).unwrap();
    assert_eq!(result_2, ECMAScriptValue::from("test result"));
}
#[test]
fn ordinary_to_primitive_nonobj_obj() {
    setup_test_agent();
    let test_obj = make_test_obj(FauxKind::Primitive, FauxKind::Object);
    let result_1 = ordinary_to_primitive(&test_obj, ConversionHint::Number).unwrap();
    assert_eq!(result_1, ECMAScriptValue::from(123_456));

    let result_2 = ordinary_to_primitive(&test_obj, ConversionHint::String).unwrap();
    assert_eq!(result_2, ECMAScriptValue::from(123_456));
}
#[test]
fn ordinary_to_primitive_call_errors() {
    setup_test_agent();
    let test_obj = make_test_obj(FauxKind::Primitive, FauxKind::Error);
    let result_1 = ordinary_to_primitive(&test_obj, ConversionHint::Number).unwrap();
    assert_eq!(result_1, ECMAScriptValue::from(123_456));

    let result_2 = ordinary_to_primitive(&test_obj, ConversionHint::String).unwrap_err();
    assert_eq!(unwind_type_error(result_2), "Test Sentinel");
}
#[test]
fn ordinary_to_primitive_get_errors() {
    setup_test_agent();
    let test_obj = make_tostring_getter_error();
    let result_1 = ordinary_to_primitive(&test_obj, ConversionHint::Number).unwrap();
    assert_eq!(result_1, ECMAScriptValue::from(123_456));

    let result_2 = ordinary_to_primitive(&test_obj, ConversionHint::String).unwrap_err();
    assert_eq!(unwind_type_error(result_2), "Test Sentinel");
}
#[test]
fn ordinary_to_primitive_uncallables() {
    setup_test_agent();
    let test_obj = make_test_obj_uncallable();
    let result_1 = ordinary_to_primitive(&test_obj, ConversionHint::Number).unwrap_err();
    assert_eq!(unwind_type_error(result_1), "Cannot convert object to primitive value");

    let result_2 = ordinary_to_primitive(&test_obj, ConversionHint::String).unwrap_err();
    assert_eq!(unwind_type_error(result_2), "Cannot convert object to primitive value");
}

#[test]
fn to_primitive_no_change() {
    setup_test_agent();
    // Undefined
    let result = to_primitive(ECMAScriptValue::Undefined, None).unwrap();
    assert!(result.is_undefined());
    // Null
    let result = to_primitive(ECMAScriptValue::Null, None).unwrap();
    assert!(result.is_null());
    // Boolean
    let result = to_primitive(ECMAScriptValue::from(true), None).unwrap();
    assert_eq!(result, ECMAScriptValue::from(true));
    // Number
    let result = to_primitive(ECMAScriptValue::from(20), None).unwrap();
    assert_eq!(result, ECMAScriptValue::from(20));
    // String
    let result = to_primitive(ECMAScriptValue::from("test"), None).unwrap();
    assert_eq!(result, ECMAScriptValue::from("test"));
    // Symbol
    let sym = Symbol::new(Some(JSString::from("Symbolic")));
    let result = to_primitive(ECMAScriptValue::from(sym.clone()), None).unwrap();
    assert_eq!(result, ECMAScriptValue::from(sym));
    // BigInt
    let bi = "123456789012345678901234567890".parse::<BigInt>().unwrap();
    let result = to_primitive(ECMAScriptValue::from(bi), None).unwrap();
    assert_eq!(result, ECMAScriptValue::from("123456789012345678901234567890".parse::<BigInt>().unwrap()));
}
#[test]
fn to_primitive_prefer_number() {
    setup_test_agent();
    let test_obj = make_test_obj(FauxKind::Primitive, FauxKind::Primitive);
    let test_value = ECMAScriptValue::from(test_obj);

    let result = to_primitive(test_value.clone(), None).unwrap();
    assert_eq!(result, ECMAScriptValue::from(123_456));
    let result = to_primitive(test_value.clone(), Some(ConversionHint::Number)).unwrap();
    assert_eq!(result, ECMAScriptValue::from(123_456));
    let result = to_primitive(test_value, Some(ConversionHint::String)).unwrap();
    assert_eq!(result, ECMAScriptValue::from("test result"));
}
#[expect(clippy::unnecessary_wraps)]
fn exotic_to_prim(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    if arguments.len() == 1 {
        if let ECMAScriptValue::String(s) = &arguments[0] {
            Ok(ECMAScriptValue::from(format!("Saw {s}")))
        } else {
            Ok(ECMAScriptValue::from(format!("Saw {:?}", arguments[0])))
        }
    } else {
        Ok(ECMAScriptValue::from(format!(
            "Incorrect arg count: there were {} elements, should have been 1",
            arguments.len()
        )))
    }
}
fn make_toprimitive_obj(
    steps: fn(&ECMAScriptValue, Option<&Object>, &[ECMAScriptValue]) -> Completion<ECMAScriptValue>,
) -> Object {
    let realm = current_realm_record().unwrap();
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_proto = realm.borrow().intrinsics.function_prototype.clone();
    let target = ordinary_object_create(Some(object_prototype));
    let key = PropertyKey::from(wks(WksId::ToPrimitive));
    let fcn = create_builtin_function(
        steps,
        None,
        1_f64,
        key.clone(),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm),
        Some(function_proto),
        None,
    );
    define_property_or_throw(
        &target,
        key,
        PotentialPropertyDescriptor {
            value: Some(ECMAScriptValue::from(fcn)),
            writable: Some(true),
            enumerable: Some(false),
            configurable: Some(true),
            ..Default::default()
        },
    )
    .unwrap();
    target
}
#[test]
fn to_primitive_uses_exotics() {
    setup_test_agent();
    let test_obj = make_toprimitive_obj(exotic_to_prim);
    let test_value = ECMAScriptValue::from(test_obj);

    let result = to_primitive(test_value.clone(), None).unwrap();
    assert_eq!(result, ECMAScriptValue::from("Saw default"));
    let result = to_primitive(test_value.clone(), Some(ConversionHint::Number)).unwrap();
    assert_eq!(result, ECMAScriptValue::from("Saw number"));
    let result = to_primitive(test_value, Some(ConversionHint::String)).unwrap();
    assert_eq!(result, ECMAScriptValue::from("Saw string"));
}
#[expect(clippy::unnecessary_wraps)]
fn exotic_returns_object(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    let realm = current_realm_record().unwrap();
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let target = ordinary_object_create(Some(object_prototype));
    Ok(ECMAScriptValue::from(target))
}
#[test]
fn to_primitive_exotic_returns_object() {
    setup_test_agent();
    let test_obj = make_toprimitive_obj(exotic_returns_object);
    let test_value = ECMAScriptValue::from(test_obj);

    let result = to_primitive(test_value, None).unwrap_err();
    assert_eq!(unwind_type_error(result), "Cannot convert object to primitive value");
}
fn exotic_throws(
    _this_value: &ECMAScriptValue,
    _new_target: Option<&Object>,
    _arguments: &[ECMAScriptValue],
) -> Completion<ECMAScriptValue> {
    Err(create_type_error("Test Sentinel"))
}
#[test]
fn to_primitive_exotic_throws() {
    setup_test_agent();
    let test_obj = make_toprimitive_obj(exotic_throws);
    let test_value = ECMAScriptValue::from(test_obj);

    let result = to_primitive(test_value, None).unwrap_err();
    assert_eq!(unwind_type_error(result), "Test Sentinel");
}
#[test]
fn to_primitive_exotic_getter_throws() {
    setup_test_agent();
    let realm = current_realm_record().unwrap();
    let object_prototype = realm.borrow().intrinsics.object_prototype.clone();
    let function_proto = realm.borrow().intrinsics.function_prototype.clone();
    let target = ordinary_object_create(Some(object_prototype));
    let key = PropertyKey::from(wks(WksId::ToPrimitive));
    let toprim_getter = create_builtin_function(
        faux_errors,
        None,
        0_f64,
        key.clone(),
        BUILTIN_FUNCTION_SLOTS,
        Some(realm),
        Some(function_proto),
        Some(JSString::from("get")),
    );
    define_property_or_throw(
        &target,
        key,
        PotentialPropertyDescriptor {
            enumerable: Some(false),
            configurable: Some(true),
            get: Some(ECMAScriptValue::from(toprim_getter)),
            ..Default::default()
        },
    )
    .unwrap();
    let test_value = ECMAScriptValue::from(target);

    let result = to_primitive(test_value, None).unwrap_err();
    assert_eq!(unwind_type_error(result), "Test Sentinel");
}

mod to_property_key {
    use super::*;
    use test_case::test_case;

    #[test_case(|| ECMAScriptValue::Undefined => Ok(PropertyKey::from("undefined")); "undefined")]
    #[test_case(|| ECMAScriptValue::from("blue") => Ok(PropertyKey::from("blue")); "string")]
    #[test_case(|| ECMAScriptValue::from(make_tostring_getter_error()) => Err("Test Sentinel".to_string()); "to_primitive error")]
    fn simple(make_value: fn() -> ECMAScriptValue) -> Result<PropertyKey, String> {
        setup_test_agent();
        let arg = make_value();
        match to_property_key(arg) {
            Ok(key) => Ok(key),
            Err(err) => Err(unwind_type_error(err)),
        }
    }

    #[test]
    fn symbol() {
        setup_test_agent();
        let sym = Symbol::new(Some("test symbol".into()));
        let argument = ECMAScriptValue::from(sym.clone());
        assert_eq!(to_property_key(argument).unwrap(), PropertyKey::from(sym));
    }
}

#[test_case(|| ECMAScriptValue::from(10.0) => Ok(10); "in range")]
#[test_case(|| ECMAScriptValue::from(0.0) => Ok(0); "bottom edge")]
#[test_case(|| ECMAScriptValue::from(-1.0) => Ok(0); "under")]
#[test_case(|| ECMAScriptValue::from(9_007_199_254_740_991.0) => Ok(9_007_199_254_740_991); "top edge")]
#[test_case(|| ECMAScriptValue::from(9_007_199_254_740_992.0) => Ok(9_007_199_254_740_991); "over")]
#[test_case(|| ECMAScriptValue::from(Symbol::new(Some("test".into()))) => Err("Symbol values cannot be converted to Number values".to_string()); "not a number")]
fn to_length(make_arg: fn() -> ECMAScriptValue) -> Result<i64, String> {
    setup_test_agent();
    let arg = make_arg();

    super::to_length(arg).map_err(unwind_type_error)
}

mod canonical_numeric_index_string {
    use super::*;
    use test_case::test_case;

    #[test_case("0" => Some(0.0); "zero")]
    #[test_case("0.25" => Some(0.25); "one quarter")]
    #[test_case("0.250000" => None; "trailing zeroes")]
    #[test_case("Infinity" => Some(f64::INFINITY); "infinity")]
    fn f(src: &str) -> Option<f64> {
        let p = src.into();
        canonical_numeric_index_string(&p)
    }
    #[test]
    fn negzero() {
        let result = canonical_numeric_index_string(&"-0".into()).unwrap();
        assert_eq!(result, 0.0);
        assert_eq!(result.signum(), -1.0);
    }
}

mod to_index {
    use super::*;
    use test_case::test_case;

    #[test_case(ECMAScriptValue::Undefined => Ok(0); "undefined")]
    #[test_case(ECMAScriptValue::from(10_i32) => Ok(10); "simple")]
    #[test_case(ECMAScriptValue::from(10.33) => Ok(10); "round down")]
    #[test_case(ECMAScriptValue::from(10.78) => Ok(10); "still rounding down")]
    #[test_case(ECMAScriptValue::from(f64::INFINITY) => Err("RangeError: inf out of range for index".to_string()); "Infinity")]
    #[test_case(ECMAScriptValue::from(-100.3) => Err("RangeError: -100 out of range for index".to_string()); "Negative")]
    #[test_case(ECMAScriptValue::from(-0.0) => Ok(0); "Negative zero")]
    #[test_case(ECMAScriptValue::from(BigInt::from(10_i32)) => Err("TypeError: BigInt values cannot be converted to Number values".to_string()); "non-number")]
    fn f(arg: ECMAScriptValue) -> Result<i64, String> {
        setup_test_agent();
        to_index(arg).map_err(unwind_any_error)
    }
}

mod array_index {
    use super::*;
    use std::cmp::Ordering;
    use test_case::test_case;

    #[test]
    fn debug() {
        assert_ne!(format!("{:?}", ArrayIndex::try_from(10932).unwrap()), "");
    }

    #[test]
    #[expect(clippy::clone_on_copy)]
    fn clone() {
        let item = ArrayIndex(10);
        let duplicate = item.clone();
        assert!(matches!(duplicate, ArrayIndex(10)));
    }

    #[test_case(ArrayIndex::try_from(10).unwrap(), ArrayIndex::try_from(10).unwrap() => true; "same")]
    #[test_case(ArrayIndex::try_from(10).unwrap(), ArrayIndex::try_from(1000).unwrap() => false; "different")]
    fn eq(v1: ArrayIndex, v2: ArrayIndex) -> bool {
        v1 == v2
    }

    #[test_case(ArrayIndex::try_from(10).unwrap(), ArrayIndex::try_from(10).unwrap() => false; "same")]
    #[test_case(ArrayIndex::try_from(10).unwrap(), ArrayIndex::try_from(1000).unwrap() => true; "different")]
    fn ne(v1: ArrayIndex, v2: ArrayIndex) -> bool {
        v1 != v2
    }

    #[test_case(ArrayIndex::try_from(10).unwrap(), ArrayIndex::try_from(10).unwrap() => Ordering::Equal; "same")]
    #[test_case(ArrayIndex::try_from(10).unwrap(), ArrayIndex::try_from(1000).unwrap() => Ordering::Less; "less")]
    #[test_case(ArrayIndex::try_from(100_000).unwrap(), ArrayIndex::try_from(1000).unwrap() => Ordering::Greater; "greater")]
    fn cmp(v1: ArrayIndex, v2: ArrayIndex) -> Ordering {
        v1.cmp(&v2)
    }

    #[test_case(ArrayIndex::try_from(10).unwrap(), ArrayIndex::try_from(10).unwrap() => Some(Ordering::Equal); "same")]
    #[test_case(ArrayIndex::try_from(10).unwrap(), ArrayIndex::try_from(1000).unwrap() => Some(Ordering::Less); "less")]
    #[test_case(ArrayIndex::try_from(100_000).unwrap(), ArrayIndex::try_from(1000).unwrap() => Some(Ordering::Greater); "greater")]
    fn partial_cmp(v1: ArrayIndex, v2: ArrayIndex) -> Option<Ordering> {
        v1.partial_cmp(&v2)
    }

    mod try_from {
        use super::*;
        use crate::agent::WksId;
        use test_case::test_case;

        #[test_case(0 => Ok(ArrayIndex(0)); "lower bound")]
        #[test_case(4_294_967_294 => Ok(ArrayIndex(4_294_967_294)); "upper bound")]
        #[test_case(4_294_967_295 => Err("The maximum array index is 4294967294".to_string()); "beyond upper bound")]
        fn from_u32(u: u32) -> Result<ArrayIndex, String> {
            ArrayIndex::try_from(u).map_err(Into::into)
        }

        #[test_case(|| wks(WksId::ToPrimitive).into() => Err("Symbols are not u32s".to_string()); "symbol")]
        #[test_case(|| "33".into() => Ok(ArrayIndex(33)); "simple")]
        #[test_case(|| "0".into() => Ok(ArrayIndex(0)); "zero")]
        #[test_case(|| "010".into() => Err("Invalid array index".to_string()); "leading zeroes")]
        #[test_case(|| "72x".into() => Err("Invalid array index".to_string()); "parse fail")]
        #[test_case(|| "4294967295".into() => Err("The maximum array index is 4294967294".to_string()); "convert fail")]
        fn property_key(make_key: fn() -> PropertyKey) -> Result<ArrayIndex, String> {
            setup_test_agent();
            let key = make_key();
            ArrayIndex::try_from(&key).map_err(Into::into)
        }
    }

    mod into {
        use super::*;
        use test_case::test_case;

        #[test_case(ArrayIndex(0) => 0; "lower")]
        #[test_case(ArrayIndex(4_294_967_294) => 4_294_967_294; "upper")]
        fn into_u32(a: ArrayIndex) -> u32 {
            u32::from(a)
        }
    }
}

#[test_case(f64::NAN, f64::NAN => true; "nan")]
#[test_case(0.0, -0.0 => false; "plus/minus zero")]
#[test_case(-0.0, 0.0 => false; "minus/plus zero")]
#[test_case(-0.0, -0.0 => true; "neg zeroes")]
#[test_case(0.0, 0.0 => true; "pos zeroes")]
#[test_case(f64::INFINITY, f64::NEG_INFINITY => false; "plus/minus inf")]
#[test_case(f64::NEG_INFINITY, f64::INFINITY => false; "minus/plus inf")]
#[test_case(f64::INFINITY, f64::INFINITY => true; "inf")]
#[test_case(f64::NEG_INFINITY, f64::NEG_INFINITY => true; "neg inf")]
#[test_case(10.0, -10.0 => false; "unequal numbers")]
#[test_case(32.0, 32.0 => true; "equal numbers")]
fn number_same_value_(x: f64, y: f64) -> bool {
    number_same_value(x, y)
}

#[test_case(f64::NAN, f64::NAN => true; "nan")]
#[test_case(0.0, -0.0 => true; "plus/minus zero")]
#[test_case(-0.0, 0.0 => true; "minus/plus zero")]
#[test_case(-0.0, -0.0 => true; "neg zeroes")]
#[test_case(0.0, 0.0 => true; "pos zeroes")]
#[test_case(f64::INFINITY, f64::NEG_INFINITY => false; "plus/minus inf")]
#[test_case(f64::NEG_INFINITY, f64::INFINITY => false; "minus/plus inf")]
#[test_case(f64::INFINITY, f64::INFINITY => true; "inf")]
#[test_case(f64::NEG_INFINITY, f64::NEG_INFINITY => true; "neg inf")]
#[test_case(10.0, -10.0 => false; "unequal numbers")]
#[test_case(32.0, 32.0 => true; "equal numbers")]
fn number_same_value_zero_(x: f64, y: f64) -> bool {
    number_same_value_zero(x, y)
}

mod is_callable {
    use super::*;
    #[test]
    fn primitive() {
        assert!(!is_callable(&true.into()));
    }
    #[test]
    fn callable() {
        setup_test_agent();
        assert!(is_callable(&intrinsic(IntrinsicId::Object).into()));
    }
}

mod is_constructor {
    use super::*;
    #[test]
    fn primitive() {
        assert!(!is_constructor(&true.into()));
    }
    #[test]
    fn constructor() {
        setup_test_agent();
        assert!(is_constructor(&intrinsic(IntrinsicId::Object).into()));
    }
}

mod option_object {
    use super::*;
    use test_case::test_case;

    fn object_with_marker() -> ECMAScriptValue {
        let obj = ordinary_object_create(None);
        define_property_or_throw(&obj, "marker", PotentialPropertyDescriptor::new().value("sentinel")).unwrap();
        ECMAScriptValue::Object(obj)
    }
    fn validate_marker(res: Result<Option<Object>, String>) {
        let oo = res.unwrap();
        let obj = oo.unwrap();
        let property = obj.get(&PropertyKey::from("marker")).unwrap();
        assert_eq!(property, ECMAScriptValue::from("sentinel"));
    }

    #[test_case(|| ECMAScriptValue::Null => Ok(None); "null")]
    #[test_case(|| ECMAScriptValue::Number(12.0) => serr("Bad type for Object/null"); "number")]
    #[test_case(object_with_marker => with validate_marker; "object")]
    fn try_from(maker: fn() -> ECMAScriptValue) -> Result<Option<Object>, String> {
        setup_test_agent();
        let val = maker();
        let result: anyhow::Result<Option<Object>> = val.try_into();
        result.map_err(|e| e.to_string())
    }
}

mod f64ish {
    use super::*;
    use test_case::test_case;

    #[test_case(ECMAScriptValue::Undefined => serr("Value not an f64"); "undefined")]
    #[test_case(ECMAScriptValue::from(99) => Ok(99.0); "number")]
    fn try_from(src: impl TryInto<f64, Error = anyhow::Error>) -> Result<f64, String> {
        src.try_into().map_err(|err| err.to_string())
    }
}

mod bigintish {
    use super::*;
    use test_case::test_case;

    #[test_case(ECMAScriptValue::Undefined => serr("Value not a bigint"); "undefined")]
    #[test_case(ECMAScriptValue::from(BigInt::from(10)) => Ok(Rc::new(BigInt::from(10))); "bigint")]
    fn try_from(src: impl TryInto<Rc<BigInt>, Error = anyhow::Error>) -> Result<Rc<BigInt>, String> {
        src.try_into().map_err(|err| err.to_string())
    }
}

mod agent {
    use super::*;
    use test_case::test_case;

    type ValueMaker = fn() -> ECMAScriptValue;
    fn undef() -> ECMAScriptValue {
        ECMAScriptValue::Undefined
    }
    fn null() -> ECMAScriptValue {
        ECMAScriptValue::Null
    }
    fn string_a() -> ECMAScriptValue {
        ECMAScriptValue::from("A")
    }
    fn string_10() -> ECMAScriptValue {
        "10".into()
    }
    fn bool_a() -> ECMAScriptValue {
        ECMAScriptValue::from(true)
    }
    fn bool_b() -> ECMAScriptValue {
        ECMAScriptValue::from(false)
    }
    fn symbol_a() -> ECMAScriptValue {
        wks(WksId::ToPrimitive).into()
    }
    fn number_10() -> ECMAScriptValue {
        10.into()
    }
    fn number_zero() -> ECMAScriptValue {
        0.into()
    }
    fn number_nan() -> ECMAScriptValue {
        f64::NAN.into()
    }
    fn number_inf() -> ECMAScriptValue {
        f64::INFINITY.into()
    }
    fn number_neg_inf() -> ECMAScriptValue {
        f64::NEG_INFINITY.into()
    }
    fn bigint_a() -> ECMAScriptValue {
        BigInt::from(10).into()
    }
    fn dead_object() -> ECMAScriptValue {
        DeadObject::object().into()
    }
    #[expect(clippy::unnecessary_wraps)]
    fn returns_10(_: &ECMAScriptValue, _: Option<&Object>, _: &[ECMAScriptValue]) -> Completion<ECMAScriptValue> {
        Ok(10.into())
    }
    fn object_10() -> ECMAScriptValue {
        let object_prototype = intrinsic(IntrinsicId::ObjectPrototype);
        let object = ordinary_object_create(Some(object_prototype));
        let to_primitive = wks(WksId::ToPrimitive);
        let realm = current_realm_record();
        let function_prototype = intrinsic(IntrinsicId::FunctionPrototype);
        let to_prim_func = create_builtin_function(
            returns_10,
            None,
            0_f64,
            to_primitive.clone().into(),
            BUILTIN_FUNCTION_SLOTS,
            realm,
            Some(function_prototype),
            None,
        );
        define_property_or_throw(
            &object,
            to_primitive,
            PotentialPropertyDescriptor::new().value(to_prim_func).writable(false).enumerable(true).configurable(true),
        )
        .unwrap();
        object.into()
    }

    #[test_case(symbol_a, symbol_a => Ok(true); "same-type fallthru")]
    #[test_case(null, undef => Ok(true); "null undef")]
    #[test_case(undef, null => Ok(true); "undef null")]
    #[test_case(number_10, string_10 => Ok(true); "right string to number")]
    #[test_case(string_10, number_10 => Ok(true); "left string to number")]
    #[test_case(bigint_a, string_a => Ok(false); "right string to bigint fail")]
    #[test_case(bigint_a, string_10 => Ok(true); "right string to bigint ok")]
    #[test_case(string_10, bigint_a => Ok(true); "left string to bigint")]
    #[test_case(bool_b, number_zero => Ok(true); "left bool vs number")]
    #[test_case(number_10, bool_a => Ok(false); "right bool vs number")]
    #[test_case(dead_object, string_10 => serr("TypeError: get called on DeadObject"); "left toprim fails")]
    #[test_case(string_10, dead_object => serr("TypeError: get called on DeadObject"); "right toprim fails")]
    #[test_case(string_10, object_10 => Ok(true); "obj on right")]
    #[test_case(object_10, string_10 => Ok(true); "obj on left")]
    #[test_case(number_10, bigint_a => Ok(true); "number bigint")]
    #[test_case(bigint_a, number_10 => Ok(true); "bigint number")]
    #[test_case(number_inf, bigint_a => Ok(false); "number inf left")]
    #[test_case(number_neg_inf, bigint_a => Ok(false); "number neg inf left")]
    #[test_case(number_nan, bigint_a => Ok(false); "number nan left")]
    #[test_case(bigint_a, number_inf => Ok(false); "number inf right")]
    #[test_case(bigint_a, number_neg_inf => Ok(false); "number neg inf right")]
    #[test_case(bigint_a, number_nan => Ok(false); "number nan right")]
    #[test_case(null, symbol_a => Ok(false); "Null & symbol")]
    fn is_loosely_equal(make_x: ValueMaker, make_y: ValueMaker) -> Result<bool, String> {
        setup_test_agent();
        let x = make_x();
        let y = make_y();

        super::is_loosely_equal(&x, &y).map_err(unwind_any_error)
    }
}

mod value_kind {
    use super::*;
    use test_case::test_case;

    #[test]
    fn fmt() {
        let k = ValueKind::Number;
        let repr = format!("{k:?}");
        assert_ne!(repr, "");
    }

    #[test]
    #[expect(clippy::clone_on_copy)]
    fn clone() {
        let item = ValueKind::Number;
        let duplicate = item.clone();
        assert!(matches!(duplicate, ValueKind::Number));
    }

    #[test_case(ValueKind::Null, ValueKind::Null => true; "same")]
    #[test_case(ValueKind::Undefined, ValueKind::Object => false; "different")]
    fn eq(v1: ValueKind, v2: ValueKind) -> bool {
        v1 == v2
    }
}

#[test_case(BigInt::from(100), 10 => "100"; "base 10")]
#[test_case(BigInt::from(-2020), 10 => "-2020"; "negative base 10")]
#[test_case(BigInt::from(65536), 16 => "10000"; "base 16")]
fn bigint_to_string_radix(bi: BigInt, radix: u32) -> String {
    String::from(super::bigint_to_string_radix(&Rc::new(bi), radix))
}

mod conversion_hint {
    use super::*;

    #[test]
    #[expect(clippy::clone_on_copy)]
    fn clone() {
        let item = ConversionHint::Number;
        let duplicate = item.clone();
        assert!(matches!(duplicate, ConversionHint::Number));
    }
}

#[test_case(0 => Ok(0.0); "zero")]
#[test_case(0xffff_ffff => Ok(4_294_967_295.0); "0xffffffff")]
#[test_case(9_007_199_254_740_992  => Ok(9_007_199_254_740_992.0); "max limit")]
#[test_case(9_007_199_254_740_993 => serr("invalid conversion of 9007199254740993 to f64"); "over limit")]
fn to_f64(arg: usize) -> Result<f64, String> {
    super::to_f64(arg).map_err(|e| e.to_string())
}

#[test_case(f64::INFINITY => serr("invalid conversion of inf to usize"); "not finite")]
#[test_case(-32.0 => serr("invalid conversion of -32 to usize"); "negative")]
#[test_case(1.0e302 => serr("invalid conversion of 100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 to usize"); "too large")]
#[test_case(10.25 => serr("invalid conversion of 10.25 to usize"); "has fraction")]
#[test_case(1024.0 => Ok(1024); "something that works")]
fn to_usize(arg: f64) -> Result<usize, String> {
    super::to_usize(arg).map_err(|e| e.to_string())
}

#[test_case(f64::NAN, f64::NAN => using check_value(f64::NAN); "exponent is nan")]
#[test_case(f64::NAN, 0.0 => using check_value(1.0); "exponent is positive 0")]
#[test_case(f64::NAN, -0.0 => using check_value(1.0); "exponent is negative 0")]
#[test_case(f64::NAN, 1.0 => using check_value(f64::NAN); "base is NAN")]
#[test_case(f64::INFINITY, 1.0 => using check_value(f64::INFINITY); "base +inf, exp positive")]
#[test_case(f64::INFINITY, -0.5 => using check_value(0.0); "base +inf, exp negative")]
#[test_case(f64::INFINITY, 0.0 => using check_value(1.0); "base +inf, exp zero")]
#[test_case(f64::NEG_INFINITY, 3.0 => using check_value(f64::NEG_INFINITY); "base neg inf; exp odd positive integer")]
#[test_case(f64::NEG_INFINITY, 4.0 => using check_value(f64::INFINITY); "base neg inf; exp even positive integer")]
#[test_case(f64::NEG_INFINITY, 3.6 => using check_value(f64::INFINITY); "base neg inf; exp positive non-integer")]
#[test_case(f64::NEG_INFINITY, -3.0 => using check_value(-0.0); "base neg inf; exp odd negative integer")]
#[test_case(f64::NEG_INFINITY, -4.0 => using check_value(0.0); "base neg inf; exp even negative integer")]
#[test_case(f64::NEG_INFINITY, -3.6 => using check_value(0.0); "base neg inf; exp negative non-integer")]
#[test_case(0.0, 2.0 => using check_value(0.0); "base pos zero; exp positive")]
#[test_case(0.0, -20.0 => using check_value(f64::INFINITY); "base pos zero; exp negative")]
#[test_case(-0.0, 3.0 => using check_value(-0.0); "base neg zero; exp positive odd integer")]
#[test_case(-0.0, 3.5 => using check_value(0.0); "base neg zero; exp positive noninteger")]
#[test_case(-0.0, 6.0 => using check_value(0.0); "base neg zero; exp positive even integer")]
#[test_case(-0.0, -3.0 => using check_value(f64::NEG_INFINITY); "base neg zero; exp negative odd integer")]
#[test_case(-0.0, -3.5 => using check_value(f64::INFINITY); "base neg zero; exp negative noninteger")]
#[test_case(-0.0, -6.0 => using check_value(f64::INFINITY); "base neg zero; exp negative even integer")]
#[test_case(1.0, f64::INFINITY => using check_value(f64::NAN); "base 1.0; exp pos inf")]
#[test_case(2.0, f64::INFINITY => using check_value(f64::INFINITY); "base more than 1.0; exp pos inf")]
#[test_case(0.5, f64::INFINITY => using check_value(0.0); "base less than 1.0; exp pos inf")]
#[test_case(1.0, f64::NEG_INFINITY => using check_value(f64::NAN); "base 1.0; exp neg inf")]
#[test_case(2.0, f64::NEG_INFINITY => using check_value(0.0); "base more than 1.0; exp neg inf")]
#[test_case(0.5, f64::NEG_INFINITY => using check_value(f64::INFINITY); "base less than 1.0; exp neg inf")]
#[test_case(-20.0, 10.25 => using check_value(f64::NAN); "base less than zero and exponent non integer")]
#[test_case(2.0, 8.0 => using check_value(256.0); "2**8 = 256")]
fn exponentiate(base: f64, exponent: f64) -> f64 {
    super::exponentiate(base, exponent)
}
