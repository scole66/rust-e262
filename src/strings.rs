use std::fmt;
use std::ops::Index;
use std::rc::Rc;

// So: JS String.
//
// At it's core, this is simply a Vec<u16>, but interned for speed & storage.
//
// The API for pulling out pieces of them hasn't really been defined yet.

#[derive(PartialEq, Clone)]
pub struct JSString {
    s: Rc<Vec<u16>>,
}

impl JSString {
    pub fn as_slice(&self) -> &[u16] {
        self.s.as_slice()
    }

    pub fn len(&self) -> usize {
        self.s.len()
    }
}

impl From<Vec<u16>> for JSString {
    fn from(source: Vec<u16>) -> Self {
        Self { s: Rc::new(source) }
    }
}

impl From<&[u16]> for JSString {
    fn from(source: &[u16]) -> Self {
        let mut v: Vec<u16> = Vec::with_capacity(source.len());
        v.extend_from_slice(source);
        Self::from(v)
    }
}

impl From<&str> for JSString {
    fn from(source: &str) -> Self {
        Self::from(source.encode_utf16().collect::<Vec<u16>>())
    }
}

impl From<String> for JSString {
    fn from(source: String) -> Self {
        Self::from(source.as_str())
    }
}


impl fmt::Debug for JSString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        String::from_utf16_lossy(&self.s).fmt(f)
    }
}

impl fmt::Display for JSString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        String::from_utf16_lossy(&self.s).fmt(f)
    }
}

impl Index<usize> for JSString {
    type Output = u16;
    fn index(&self, idx: usize) -> &Self::Output {
        &self.as_slice()[idx]
    }
}

impl std::cmp::PartialEq<&str> for JSString {
    fn eq(&self, other: &&str) -> bool {
        let mut ileft = self.s.iter();
        let mut iright = (*other).encode_utf16();
        loop {
            let left = ileft.next();
            let right = iright.next();
            match (left, right) {
                (None, None) => {
                    break true;
                }
                (Some(_), None) | (None, Some(_)) => {
                    break false;
                }
                (Some(&l), Some(r)) if l != r => {
                    break false;
                }
                _ => {}
            }
        }
    }
}

// Static Semantics: UTF16SurrogatePairToCodePoint ( lead, trail )
//
// The abstract operation UTF16SurrogatePairToCodePoint takes arguments lead (a code unit) and trail (a code unit). Two
// code units that form a UTF-16 surrogate pair are converted to a code point. It performs the following steps when
// called:
//
//  1. Assert: lead is a leading surrogate and trail is a trailing surrogate.
//  2. Let cp be (lead - 0xD800) × 0x400 + (trail - 0xDC00) + 0x10000.
//  3. Return the code point cp.
fn utf16_surrogate_pair_to_code_point(lead: u16, trail: u16) -> u32 {
    let cp: u32 = ((lead - 0xD800) as u32) * 0x400 + ((trail - 0xDC00) as u32) + 0x10000;
    cp
}

// Static Semantics: CodePointAt ( string, position )
//
// The abstract operation CodePointAt takes arguments string (a String) and position (a non-negative integer). It
// interprets string as a sequence of UTF-16 encoded code points, as described in 6.1.4, and reads from it a single code
// point starting with the code unit at index position. It performs the following steps when called:
//
//  1. Let size be the length of string.
//  2. Assert: position ≥ 0 and position < size.
//  3. Let first be the code unit at index position within string.
//  4. Let cp be the code point whose numeric value is that of first.
//  5. If first is not a leading surrogate or trailing surrogate, then
//      a. Return the Record { [[CodePoint]]: cp, [[CodeUnitCount]]: 1, [[IsUnpairedSurrogate]]: false }.
//  6. If first is a trailing surrogate or position + 1 = size, then
//      a. Return the Record { [[CodePoint]]: cp, [[CodeUnitCount]]: 1, [[IsUnpairedSurrogate]]: true }.
//  7. Let second be the code unit at index position + 1 within string.
//  8. If second is not a trailing surrogate, then
//      a. Return the Record { [[CodePoint]]: cp, [[CodeUnitCount]]: 1, [[IsUnpairedSurrogate]]: true }.
//  9. Set cp to ! UTF16SurrogatePairToCodePoint(first, second).
//  10. Return the Record { [[CodePoint]]: cp, [[CodeUnitCount]]: 2, [[IsUnpairedSurrogate]]: false }.
#[derive(PartialEq)]
struct CodePointAtResult {
    code_point: u32,
    code_unit_count: u8,
    is_unpaired_surrogate: bool,
}
fn code_point_at(string: &JSString, position: usize) -> CodePointAtResult {
    let size = string.len();
    let first = string[position];
    let cp: u32 = first as u32;
    if !(0xD800..=0xDFFF).contains(&first) {
        CodePointAtResult { code_point: cp, code_unit_count: 1, is_unpaired_surrogate: false }
    } else if first >= 0xDC00 || position + 1 == size {
        CodePointAtResult { code_point: cp, code_unit_count: 1, is_unpaired_surrogate: true }
    } else {
        let second = string[position + 1];
        if !(0xDC00..=0xDFFF).contains(&second) {
            CodePointAtResult { code_point: cp, code_unit_count: 1, is_unpaired_surrogate: true }
        } else {
            let cp = utf16_surrogate_pair_to_code_point(first, second);
            CodePointAtResult { code_point: cp, code_unit_count: 2, is_unpaired_surrogate: false }
        }
    }
}

// Static Semantics: StringToCodePoints ( string )
//
// The abstract operation StringToCodePoints takes argument string (a String). It returns the sequence of Unicode code
// points that results from interpreting string as UTF-16 encoded Unicode text as described in 6.1.4. It performs the
// following steps when called:
//
//  1. Let codePoints be a new empty List.
//  2. Let size be the length of string.
//  3. Let position be 0.
//  4. Repeat, while position < size,
//      a. Let cp be ! CodePointAt(string, position).
//      b. Append cp.[[CodePoint]] to codePoints.
//      c. Set position to position + cp.[[CodeUnitCount]].
//  5. Return codePoints.
fn string_to_code_points(string: &JSString) -> Vec<u32> {
    // Note that this happily glosses over encoding errors. Storing in a Vec<u32> for now.
    let size = string.len();
    let mut code_points: Vec<u32> = Vec::with_capacity(size);
    let mut position = 0;
    while position < size {
        let cp = code_point_at(string, position);
        code_points.push(cp.code_point);
        position += cp.code_unit_count as usize;
    }
    code_points
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_u16_test_01() {
        let src: &[u16] = &[66, 111, 98]; // Bob
        let res = JSString::from(src);
        let display = format!("{}", res);
        //assert_eq!(display, "Bob");
        assert!(display == "Bob");
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
        let mystr = JSString::from(vec![0x00, 0xd800, 0xdc00, 0xde00, 0xd900, 0x00e2, 0xd902]);
        let expected = vec![
            CodePointAtResult { code_point: 0, code_unit_count: 1, is_unpaired_surrogate: false },
            CodePointAtResult { code_point: 0x10000, code_unit_count: 2, is_unpaired_surrogate: false },
            CodePointAtResult { code_point: 0xde00, code_unit_count: 1, is_unpaired_surrogate: true },
            CodePointAtResult { code_point: 0xd900, code_unit_count: 1, is_unpaired_surrogate: true },
            CodePointAtResult { code_point: 0xe2, code_unit_count: 1, is_unpaired_surrogate: false },
            CodePointAtResult { code_point: 0xd902, code_unit_count: 1, is_unpaired_surrogate: true }
        ];
        let positions = vec![ 0, 1, 3, 4, 5, 6 ];
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
        let mystr = JSString::from(vec![0x00, 0xd800, 0xdc00, 0xde00, 0xd900, 0x00e2, 0xd902]);
        let result = string_to_code_points(&mystr);
        let expected = vec![0, 0x10000, 0xde00, 0xd900, 0xe2, 0xd902];
        assert!(result == expected);
    }
}
