use super::*;
use ahash::AHasher;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use test_case::test_case;

mod jsstring {
    use super::*;
    use test_case::test_case;

    mod from {
        use super::*;
        use test_case::test_case;

        #[test]
        fn from_u16_test_02() {
            let src: &[u16] = &[0x101, 0xDC67, 0xE00D, 0x1111, 0xE00E]; // not valid utf-16.
            let res = JSString::from(src);
            let display = format!("{res}");
            assert_eq!(display, "\u{0101}\u{FFFD}\u{E00D}\u{1111}\u{E00E}");
            assert_eq!(res.len(), 5);
            assert_eq!(res[0], 0x101);
            assert_eq!(res[1], 0xdc67);
            assert_eq!(res[2], 0xe00d);
            assert_eq!(res[3], 0x1111);
            assert_eq!(res[4], 0xe00e);
        }

        const BOB_U8: &[u8] = &[66, 111, 98];
        const BOB_U16: &[u16] = &[66, 111, 98];

        #[test_case(Rc::new(BigInt::from(100)) => "100"; "bigint")]
        #[test_case(Vec::<u8>::from(&[66, 111, 98]) => "Bob"; "u8 vec")]
        #[test_case(Vec::<u16>::from(&[66, 111, 98]) => "Bob"; "u16 vec")]
        #[test_case(String::from("Bob") => "Bob"; "String")]
        #[test_case(BOB_U8 => "Bob"; "&[u8]")]
        #[test_case(BOB_U16 => "Bob"; "&[u16]")]
        #[test_case("Bob" => "Bob"; "&str")]
        fn f<T>(item: T) -> String
        where
            JSString: From<T>,
        {
            String::from(JSString::from(item))
        }
    }

    #[test]
    fn debug_repr_test_01() {
        let jsstr = JSString::from("hello");
        let debug_str = format!("{jsstr:?}");
        assert_eq!(debug_str, "\"hello\"");
    }
    #[test]
    fn equality_test_01() {
        let s1 = JSString::from("blue");
        let s2 = JSString::from("orange");
        let s3 = JSString::from("blue");
        let s4 = JSString::from("b");
        assert_eq!(s1, s3);
        assert_ne!(s1, s2);
        assert_ne!(s2, s3);
        assert_eq!(s1, "blue");
        assert_eq!(s2, "orange");
        assert_ne!(s1, "elephant");
        assert_ne!(s1, "orange");
        assert_ne!(s1, s4);
        assert_ne!(s4, s1);
        assert_ne!(s1, "blueox");
        assert_ne!(s1, "blu");
    }

    #[test]
    fn clone_test() {
        let s1 = JSString::from("crocodile");
        let s2 = s1.clone();
        assert_eq!(s1, s2);
    }

    #[test]
    fn code_point_at_01() {
        let mystr = JSString::from("test");
        let r1 = code_point_at(&mystr, 0);
        assert_eq!(r1.code_point, 116);
        assert_eq!(r1.code_unit_count, 1);
        assert!(!r1.is_unpaired_surrogate);
        let r2 = code_point_at(&mystr, 1);
        assert_eq!(r2.code_point, 101);
        assert_eq!(r2.code_unit_count, 1);
        assert!(!r2.is_unpaired_surrogate);
        let r3 = code_point_at(&mystr, 2);
        assert_eq!(r3.code_point, 115);
        assert_eq!(r3.code_unit_count, 1);
        assert!(!r3.is_unpaired_surrogate);
        let r4 = code_point_at(&mystr, 0);
        assert_eq!(r4.code_point, 116);
        assert_eq!(r4.code_unit_count, 1);
        assert!(!r4.is_unpaired_surrogate);
    }

    #[test]
    fn code_point_at_02() {
        let src: Vec<u16> = vec![0x00, 0xd800, 0xdc00, 0xde00, 0xd900, 0x00e2, 0xd902];
        let mystr = JSString::from(src);
        let expected = [
            CodePointAtResult { code_point: 0, code_unit_count: 1, is_unpaired_surrogate: false },
            CodePointAtResult { code_point: 0x10000, code_unit_count: 2, is_unpaired_surrogate: false },
            CodePointAtResult { code_point: 0xde00, code_unit_count: 1, is_unpaired_surrogate: true },
            CodePointAtResult { code_point: 0xd900, code_unit_count: 1, is_unpaired_surrogate: true },
            CodePointAtResult { code_point: 0xe2, code_unit_count: 1, is_unpaired_surrogate: false },
            CodePointAtResult { code_point: 0xd902, code_unit_count: 1, is_unpaired_surrogate: true },
        ];
        let positions = [0, 1, 3, 4, 5, 6];
        for idx in 0..positions.len() {
            let result = code_point_at(&mystr, positions[idx]);
            assert_eq!(result, expected[idx]);
        }
    }

    #[test]
    fn code_point_at_03() {
        // if any of the fields is different, they're not equal
        let a = CodePointAtResult { code_point: 0x100, code_unit_count: 2, is_unpaired_surrogate: false };
        let b1 = CodePointAtResult { code_point: 0x100, code_unit_count: 1, is_unpaired_surrogate: false };
        let b2 = CodePointAtResult { code_point: 0x100, code_unit_count: 2, is_unpaired_surrogate: true };
        let b3 = CodePointAtResult { code_point: 0x101, code_unit_count: 2, is_unpaired_surrogate: false };
        assert_ne!(a, b1);
        assert_ne!(a, b2);
        assert_ne!(a, b3);
        let c = CodePointAtResult { code_point: 0x100, code_unit_count: 2, is_unpaired_surrogate: false };
        assert_eq!(a, c);
    }

    #[test]
    fn string_to_code_points_01() {
        let src: Vec<u16> = vec![0x00, 0xd800, 0xdc00, 0xde00, 0xd900, 0x00e2, 0xd902];
        let mystr = JSString::from(src);
        let result = mystr.to_code_points();
        let expected = vec![0, 0x10000, 0xde00, 0xd900, 0xe2, 0xd902];
        assert_eq!(result, expected);
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
        let mut s = AHasher::default();
        t.hash(&mut s);
        s.finish()
    }
    #[test_case(&JSString::from("a"), &JSString::from("b") => false; "not equal")]
    #[test_case(&JSString::from("a"), &JSString::from("a") => true; "equal")]
    fn hash_ahash(a: &JSString, b: &JSString) -> bool {
        calculate_hash(a) == calculate_hash(b)
    }

    fn calculate_def_hash<T: Hash>(t: &T) -> u64 {
        let mut s = DefaultHasher::new();
        t.hash(&mut s);
        s.finish()
    }
    #[test_case(&JSString::from("a"), &JSString::from("b") => false; "not equal")]
    #[test_case(&JSString::from("a"), &JSString::from("a") => true; "equal")]
    fn hash_defhash(a: &JSString, b: &JSString) -> bool {
        calculate_def_hash(a) == calculate_def_hash(b)
    }

    #[test_case(&JSString::from("Head: "), JSString::from("tail") => "Head: tail"; "jsstring")]
    #[test_case(&JSString::from("Head: "), &JSString::from("tiger") => "Head: tiger"; "jsstring ref")]
    #[test_case(&JSString::from("Head: "), utf16_const!("a") => "Head: a"; "&[u16; 1] style")]
    #[test_case(&JSString::from("Head: "), utf16_const!("abcd") => "Head: abcd"; "&[u16; 4] style")]
    #[test_case(&JSString::from("Head: "), utf16_const!("0123456789a") => "Head: 0123456789a"; "&[u16; 11] style")]
    #[test_case(&JSString::from("Head: "), &['b' as u16, 'o' as u16, 'b' as u16][..] => "Head: bob"; "&[u16] style")]
    fn concat(s1: &JSString, s2: impl AsRef<[u16]>) -> String {
        s1.concat(s2).to_string()
    }

    #[test_case("" => true; "empty")]
    #[test_case("full" => false; "not empty")]
    fn is_empty(s: &str) -> bool {
        JSString::from(s).is_empty()
    }

    #[expect(clippy::cmp_owned)]
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

    #[test_case("12345", "", 1 => Some(1); "empty search")]
    #[test_case("12345", "12", 0 => Some(0); "match at start")]
    #[test_case("12345", "45", 0 => Some(3); "match at end")]
    #[test_case("12345", "", 10000 => None; "empty search, large start")]
    #[test_case("12345", "g", 3 => None; "not found")]
    #[test_case("12345", "a", 10 => None; "start after end")]
    #[test_case("12345", "1234567890", 0 => None; "needle larger than haystack")]
    fn index_of(src: impl Into<JSString>, needle: impl Into<JSString>, start: usize) -> Option<usize> {
        let src = src.into();
        let needle = needle.into();
        src.index_of(&needle, start)
    }

    #[test_case("" => Some(Rc::new(BigInt::from(0))); "empty string")]
    #[test_case(" \t" => Some(Rc::new(BigInt::from(0))); "only whitespace (2+ chars)")]
    #[test_case(" " => Some(Rc::new(BigInt::from(0))); "only whitespace (1 char)")]
    #[test_case("  nothing  " => None; "not a number, with whitespace")]
    #[test_case("\u{26f5}" => None; "sailboat emoji ⛵")]
    #[test_case("0x10" => Some(Rc::new(BigInt::from(16))); "hex")]
    #[test_case("0b1010" => Some(Rc::new(BigInt::from(10))); "binary")]
    #[test_case("0o752" => Some(Rc::new(BigInt::from(490))); "octal")]
    #[test_case("0q1231" => None; "leading zero, but not valid")]
    #[test_case("1234567890987654321" => Some(Rc::new(BigInt::from(1_234_567_890_987_654_321_i64))); "actual number")]
    fn string_to_bigint(s: &str) -> Option<Rc<BigInt>> {
        JSString::from(s).to_bigint()
    }

    #[test_case("1234", 10 => BigInt::from(1234); "straightforward; base 10")]
    #[test_case("4321", 5 => BigInt::from(586); "base 5")]
    fn to_bigint_radix(s: &str, radix: u32) -> BigInt {
        JSString::from(s).to_bigint_radix(radix)
    }

    mod last_index_of {
        use super::*;
        use test_case::test_case;

        #[test_case("banana", "na", 4 => Some(4); "finds match exactly at from_index")]
        #[test_case("banana", "na", 3 => Some(2); "finds previous match before from_index")]
        #[test_case("banana", "ba", 4 => Some(0); "finds match at start")]
        #[test_case("banana", "x", 5 => None; "returns none when search string is absent")]
        #[test_case("banana", "xxxxxx", 0 => None; "returns none for same-length non-match")]
        #[test_case("banana", "", 6 => Some(6); "empty search string matches at end")]
        #[test_case("banana", "", 3 => Some(3); "empty search string matches at from_index")]
        #[test_case("banana", "", 0 => Some(0); "empty search string matches at start")]
        fn f(base: &str, search: &str, from_index: usize) -> Option<usize> {
            JSString::from(base).last_index_of(&JSString::from(search), from_index)
        }

        #[test_case(
            &[0x61, 0xD83D, 0xDE00, 0x62, 0xD83D, 0xDE00],
            &[0xD83D, 0xDE00],
            4
            => Some(4);
            "finds last surrogate pair by code unit index"
        )]
        #[test_case(
            &[0x61, 0xD83D, 0xDE00, 0x62],
            &[0xDE00],
            2
            => Some(2);
            "can match trailing surrogate code unit directly"
        )]
        #[test_case(
            &[0x61, 0xD83D, 0xDE00, 0x62],
            &[0xD83D],
            1
            => Some(1);
            "can match leading surrogate code unit directly"
        )]
        fn code_units(base: &[u16], search: &[u16], from_index: usize) -> Option<usize> {
            JSString::from(base).last_index_of(&JSString::from(search), from_index)
        }
    }

    // #[test_case("abcdef", "abc" => true; "simple")]
    // #[test_case("", "blue" => false; "needle is longer than haystack")]
    // #[test_case("abcdef", "1" => false; "needle not in haystack")]
    // fn starts_with(s: &str, needle: &str) -> bool {
    //     let s = JSString::from(s);
    //     let needle = JSString::from(needle);
    //     s.starts_with(&needle)
    // }

    #[test_case("blue", 'l' as u16 => true; "item is contained")]
    #[test_case("green", 'l' as u16 => false; "item is not contained")]
    #[test_case("", 0 => false; "empty string contains nothing")]
    fn contains(s: &str, needle: u16) -> bool {
        let s = JSString::from(s);
        s.contains(needle)
    }

    #[test_case("blue" => true; "ascii")]
    #[test_case("🌈✨🚀🦄🍕🎉🔥🌊🌙⭐️🐉🍄" => true; "emojiland")]
    #[test_case(&[0xDC00, 'a' as u16, 0xD832, 'b' as u16][..] => false; "unpaired unicode")]
    fn is_well_formed_unicode(s: impl Into<JSString>) -> bool {
        let s = s.into();
        s.is_well_formed_unicode()
    }

    #[test_case("blue" => (vec![98, 108, 117, 101], vec![0, 1, 2, 3]); "just ascii")]
    #[test_case(
        "🌈✨🚀🦄🍕🎉🔥🌊🌙⭐️🐉🍄"
        => (
            vec![127_752, 10_024, 128_640, 129_412, 127_829, 127_881, 128_293, 127_754, 127_769, 11_088, 65_039, 128_009, 127_812],
            vec![0, 0, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 10, 11, 11, 12, 12]
        );
        "emojiland"
    )]
    fn to_code_points_with_map(s: impl Into<JSString>) -> (Vec<u32>, Vec<usize>) {
        let s = s.into();
        s.to_code_points_with_map()
    }
}

#[test_case(0x0000 => Ok(vec![0x0000_u16]); "nul")]
#[test_case(0x0040 => Ok(vec![0x0040_u16]); "no encoding needed")]
#[test_case(0xFFFF => Ok(vec![0xFFFF_u16]); "last bmp code point")]
#[test_case(0xD800 => Ok(vec![0xD800_u16]); "high surrogate code point passed through")]
#[test_case(0xDFFF => Ok(vec![0xDFFF_u16]); "low surrogate code point passed through")]
#[test_case(0x10000 => Ok(vec![0xD800_u16, 0xDC00_u16]); "first supplementary code point")]
#[test_case(0x1F62D => Ok(vec![0xD83D_u16, 0xDE2D_u16]); "crying face emoji")]
#[test_case(0x10_FFFF => Ok(vec![0xDBFF_u16, 0xDFFF_u16]); "maximum valid code point")]
#[test_case(0x11_0000 => Err("Code point value out of range".to_string()); "first code point above unicode max")]
fn utf16_encode_code_point(cp: u32) -> Result<Vec<u16>, String> {
    let mut buf = [0; 2];
    super::utf16_encode_code_point(cp, &mut buf).map(Vec::from).map_err(|e| e.to_string())
}

mod is_str_whitespace {
    use super::*;
    use unicode_intervals::*;

    #[test]
    fn isw() {
        let white_space = unicode_intervals::query()
            .include_categories(UnicodeCategory::Zs)
            .include_characters("\t\u{000b}\u{000c}\n\r\u{feff}\u{2028}\u{2029}")
            .intervals()
            .expect("query should work");
        for (start, end) in white_space {
            assert!((start..=end).all(|ch| u16::try_from(ch).is_ok_and(is_str_whitespace)));
        }
        let not_white_space = unicode_intervals::query()
            .exclude_categories(UnicodeCategory::Zs)
            .exclude_characters("\t\u{000b}\u{000c}\n\r\u{feff}\u{2028}\u{2029}")
            .max_codepoint(0xFFFF)
            .intervals()
            .unwrap();
        for (start, end) in not_white_space {
            assert!((start..=end).all(|ch| u16::try_from(ch).is_ok_and(|ch| !is_str_whitespace(ch))));
        }
    }
}
