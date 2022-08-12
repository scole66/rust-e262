use super::*;
use crate::*;
use ahash::AHasher;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use test_case::test_case;

#[test]
fn from_u16_test_01() {
    let src: &[u16] = &[66, 111, 98]; // Bob
    let res = JSString::from(src);
    let display = format!("{}", res);
    assert_eq!(display, "Bob");
}

#[test]
fn from_u16_test_02() {
    let src: &[u16] = &[0x101, 0xDC67, 0xE00D, 0x1111, 0xE00E]; // not valid utf-16.
    let res = JSString::from(src);
    let display = format!("{}", res);
    assert!(display == "\u{0101}\u{FFFD}\u{E00D}\u{1111}\u{E00E}");
    assert!(res.len() == 5);
    assert!(res[0] == 0x101);
    assert!(res[1] == 0xdc67);
    assert!(res[2] == 0xe00d);
    assert!(res[3] == 0x1111);
    assert!(res[4] == 0xe00e);
}

#[test]
fn from_vec_test_01() {
    let src: Vec<u16> = vec![66, 111, 98]; // Bob
    let res = JSString::from(src);
    let display = format!("{}", res);
    //assert_eq!(display, "Bob");
    assert!(display == "Bob");
}
#[test]
fn from_u8array_01() {
    let src: &[u8] = &[66, 111, 98]; // Bob
    let res = JSString::from(src);
    let display = format!("{}", res);
    assert!(display == "Bob");
}

#[test]
fn from_str_test_01() {
    let src: &str = "Bob"; // Bob
    let res = JSString::from(src);
    let display = format!("{}", res);
    //assert_eq!(display, "Bob");
    assert!(display == "Bob");
}
#[test]
fn from_string_test_01() {
    let src: String = String::from("Bob"); // Bob
    let res = JSString::from(src);
    let display = format!("{}", res);
    //assert_eq!(display, "Bob");
    assert!(display == "Bob");
}
#[test]
fn debug_repr_test_01() {
    let jsstr = JSString::from("hello");
    let debug_str = format!("{:?}", jsstr);
    assert!(debug_str == "\"hello\"");
}
#[test]
fn equality_test_01() {
    let s1 = JSString::from("blue");
    let s2 = JSString::from("orange");
    let s3 = JSString::from("blue");
    let s4 = JSString::from("b");
    assert!(s1 == s3);
    assert!(s1 != s2);
    assert!(s2 != s3);
    assert!(s1 == "blue");
    assert!(s2 == "orange");
    assert!(s1 != "elephant");
    assert!(s1 != "orange");
    assert!(s1 != s4);
    assert!(s4 != s1);
    assert!(s1 != "blueox");
    assert!(s1 != "blu");
}

#[test]
fn clone_test() {
    let s1 = JSString::from("crocodile");
    let s2 = s1.clone();
    assert!(s1 == s2);
}

#[test]
fn code_point_at_01() {
    let mystr = JSString::from("test");
    let r1 = code_point_at(&mystr, 0);
    assert!(r1.code_point == 116);
    assert!(r1.code_unit_count == 1);
    assert!(!r1.is_unpaired_surrogate);
    let r2 = code_point_at(&mystr, 1);
    assert!(r2.code_point == 101);
    assert!(r2.code_unit_count == 1);
    assert!(!r2.is_unpaired_surrogate);
    let r3 = code_point_at(&mystr, 2);
    assert!(r3.code_point == 115);
    assert!(r3.code_unit_count == 1);
    assert!(!r3.is_unpaired_surrogate);
    let r4 = code_point_at(&mystr, 0);
    assert!(r4.code_point == 116);
    assert!(r4.code_unit_count == 1);
    assert!(!r4.is_unpaired_surrogate);
}

#[test]
fn code_point_at_02() {
    let src: Vec<u16> = vec![0x00, 0xd800, 0xdc00, 0xde00, 0xd900, 0x00e2, 0xd902];
    let mystr = JSString::from(src);
    let expected = vec![
        CodePointAtResult { code_point: 0, code_unit_count: 1, is_unpaired_surrogate: false },
        CodePointAtResult { code_point: 0x10000, code_unit_count: 2, is_unpaired_surrogate: false },
        CodePointAtResult { code_point: 0xde00, code_unit_count: 1, is_unpaired_surrogate: true },
        CodePointAtResult { code_point: 0xd900, code_unit_count: 1, is_unpaired_surrogate: true },
        CodePointAtResult { code_point: 0xe2, code_unit_count: 1, is_unpaired_surrogate: false },
        CodePointAtResult { code_point: 0xd902, code_unit_count: 1, is_unpaired_surrogate: true },
    ];
    let positions = vec![0, 1, 3, 4, 5, 6];
    for idx in 0..positions.len() {
        let result = code_point_at(&mystr, positions[idx]);
        assert!(result == expected[idx]);
    }
}

#[test]
fn code_point_at_03() {
    // if any of the fields is different, they're not equal
    let a = CodePointAtResult { code_point: 0x100, code_unit_count: 2, is_unpaired_surrogate: false };
    let b1 = CodePointAtResult { code_point: 0x100, code_unit_count: 1, is_unpaired_surrogate: false };
    let b2 = CodePointAtResult { code_point: 0x100, code_unit_count: 2, is_unpaired_surrogate: true };
    let b3 = CodePointAtResult { code_point: 0x101, code_unit_count: 2, is_unpaired_surrogate: false };
    assert!(a != b1);
    assert!(a != b2);
    assert!(a != b3);
    let c = CodePointAtResult { code_point: 0x100, code_unit_count: 2, is_unpaired_surrogate: false };
    assert!(a == c);
}

#[test]
fn string_to_code_points_01() {
    let src: Vec<u16> = vec![0x00, 0xd800, 0xdc00, 0xde00, 0xd900, 0x00e2, 0xd902];
    let mystr = JSString::from(src);
    let result = string_to_code_points(&mystr);
    let expected = vec![0, 0x10000, 0xde00, 0xd900, 0xe2, 0xd902];
    assert!(result == expected);
}

#[test]
fn string_to_vec16() {
    let src = JSString::from("blue");
    let result = Vec::<u16>::from(src);
    assert_eq!(result, &[98, 108, 117, 101]);
}

#[test]
fn string_from_vec8() {
    let src: Vec<u8> = vec![98, 108, 117, 101];
    let result = JSString::from(src);
    assert_eq!(result, "blue");
}

//#[test_case(73.into() => Err("Not a string value".to_string()); "not a string")]
#[test_case("a string".into() => Ok("a string".to_string()); "a string")]
fn try_from_value(val: ECMAScriptValue) -> Result<String, String> {
    JSString::try_from(val).map(String::from).map_err(|e| e.to_string())
}

#[test]
fn into() {
    let src = JSString::from("baloney");
    let result = String::from(&src);
    assert_eq!(result, "baloney");
}

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = AHasher::new_with_keys(1234, 5678);
    t.hash(&mut s);
    s.finish()
}
#[test_case(JSString::from("a"), JSString::from("b") => false; "not equal")]
#[test_case(JSString::from("a"), JSString::from("a") => true; "equal")]
fn hash_ahash(a: JSString, b: JSString) -> bool {
    calculate_hash(&a) == calculate_hash(&b)
}

fn calculate_def_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}
#[test_case(JSString::from("a"), JSString::from("b") => false; "not equal")]
#[test_case(JSString::from("a"), JSString::from("a") => true; "equal")]
fn hash_defhash(a: JSString, b: JSString) -> bool {
    calculate_def_hash(&a) == calculate_def_hash(&b)
}

#[test_case(JSString::from("Head: "), JSString::from("tail") => "Head: tail"; "jsstring")]
#[test_case(JSString::from("Head: "), "other tail" => "Head: other tail"; "&str value")]
fn concat(s1: JSString, s2: impl Into<JSString>) -> String {
    s1.concat(s2).to_string()
}

#[test_case("" => true; "empty")]
#[test_case("full" => false; "not empty")]
fn is_empty(s: &str) -> bool {
    JSString::from(s).is_empty()
}

#[allow(clippy::cmp_owned)]
mod lt {
    use super::*;
    use test_case::test_case;

    #[test_case("", "something" => true; "empty to thing")]
    #[test_case("blue", "blue" => false; "equal")]
    #[test_case("before", "zafter" => true; "alpha")]
    fn less_than(left: &str, right: &str) -> bool {
        JSString::from(left) < JSString::from(right)
    }
}

#[test_case("", "something" => "something"; "empty to thing")]
#[test_case("blue", "blue" => "blue"; "equal")]
#[test_case("before", "zafter" => "zafter"; "alpha")]
fn max(left: &str, right: &str) -> String {
    String::from(JSString::from(left).max(JSString::from(right)))
}

mod jsstring {
    use super::*;
    use test_case::test_case;

    #[test_case("12345", "", 1 => 1; "empty search")]
    #[test_case("12345", "12", 0 => 0; "match at start")]
    #[test_case("12345", "45", 0 => 3; "match at end")]
    #[test_case("12345", "", 10000 => -1; "empty search, large start")]
    #[test_case("12345", "g", 3 => -1; "not found")]
    fn index_of(src: impl Into<JSString>, needle: impl Into<JSString>, start: u64) -> i64 {
        let src = src.into();
        let needle = needle.into();
        src.index_of(&needle, start)
    }
}