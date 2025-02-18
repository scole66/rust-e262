use super::*;
use num::BigInt;
use std::fmt;
use std::ops::Index;
use std::rc::Rc;

// So: JS String.
//

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct JSString {
    s: Rc<[u16]>,
}

impl JSString {
    pub fn as_slice(&self) -> &[u16] {
        &self.s
    }

    pub fn len(&self) -> usize {
        self.s.len()
    }

    pub fn is_empty(&self) -> bool {
        self.s.is_empty()
    }

    #[must_use]
    pub fn concat(&self, s: impl Into<JSString>) -> JSString {
        let tail = s.into();
        let combined = [self.clone().s, tail.s].concat().into_boxed_slice();
        JSString { s: Rc::from(combined) }
    }

    pub fn index_of(&self, search_value: &JSString, from_index: usize) -> i64 {
        let len = self.len();
        if search_value.is_empty() && from_index <= len {
            i64::try_from(from_index).unwrap()
        } else {
            let search_len = search_value.len();
            if search_len > len {
                return -1;
            }
            for i in from_index..=(len - search_len) {
                if self.s[i..(i + search_len)] == search_value.s[..] {
                    return i64::try_from(i).unwrap();
                }
            }
            -1
        }
    }

    pub fn contains(&self, ch: u16) -> bool {
        self.s.contains(&ch)
    }

    pub fn starts_with(&self, search_value: &JSString) -> bool {
        let len = self.len();
        let search_len = search_value.len();
        if search_len > len {
            false
        } else {
            self.s[0..search_len] == search_value.s[..]
        }
    }
}

impl From<Vec<u16>> for JSString {
    fn from(source: Vec<u16>) -> Self {
        Self { s: Rc::from(source.into_boxed_slice()) }
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

impl From<&[u8]> for JSString {
    fn from(source: &[u8]) -> Self {
        let v: Vec<u16> = source.iter().map(|v| u16::from(*v)).collect();
        Self::from(v)
    }
}
impl From<Vec<u8>> for JSString {
    fn from(source: Vec<u8>) -> Self {
        Self::from(source.as_slice())
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

impl From<JSString> for String {
    fn from(source: JSString) -> Self {
        String::from_utf16_lossy(&source.s)
    }
}

impl From<&JSString> for String {
    fn from(source: &JSString) -> Self {
        String::from_utf16_lossy(&source.s)
    }
}

impl From<JSString> for Vec<u16> {
    fn from(source: JSString) -> Self {
        source.s.to_vec()
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
pub fn utf16_surrogate_pair_to_code_point(lead: u16, trail: u16) -> u32 {
    let cp: u32 = u32::from(lead - 0xD800) * 0x400 + u32::from(trail - 0xDC00) + 0x10000;
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
#[derive(PartialEq, Eq)]
pub struct CodePointAtResult {
    pub code_point: u32,
    pub code_unit_count: u8,
    pub is_unpaired_surrogate: bool,
}

pub fn code_point_at(string: &JSString, position: usize) -> CodePointAtResult {
    let size = string.len();
    let first = string[position];
    let cp: u32 = u32::from(first);
    if !(0xD800..=0xDFFF).contains(&first) {
        CodePointAtResult { code_point: cp, code_unit_count: 1, is_unpaired_surrogate: false }
    } else if first >= 0xDC00 || position + 1 == size {
        CodePointAtResult { code_point: cp, code_unit_count: 1, is_unpaired_surrogate: true }
    } else {
        let second = string[position + 1];
        if (0xDC00..=0xDFFF).contains(&second) {
            let cp = utf16_surrogate_pair_to_code_point(first, second);
            CodePointAtResult { code_point: cp, code_unit_count: 2, is_unpaired_surrogate: false }
        } else {
            CodePointAtResult { code_point: cp, code_unit_count: 1, is_unpaired_surrogate: true }
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
impl JSString {
    pub fn to_code_points(&self) -> Vec<u32> {
        // Note that this happily glosses over encoding errors. Storing in a Vec<u32> for now.
        let size = self.len();
        let mut code_points: Vec<u32> = Vec::with_capacity(size);
        let mut position = 0;
        while position < size {
            let cp = code_point_at(self, position);
            code_points.push(cp.code_point);
            position += cp.code_unit_count as usize;
        }
        code_points
    }
}

#[allow(clippy::cast_possible_truncation)]
pub fn utf16_encode_code_point(cp: u32, dst: &mut [u16; 2]) -> anyhow::Result<&mut [u16]> {
    // Static Semantics: UTF16EncodeCodePoint ( cp )
    // The abstract operation UTF16EncodeCodePoint takes argument cp (a Unicode code point) and returns a String. It performs the following steps when called:
    //
    // 1. Assert: 0 ≤ cp ≤ 0x10FFFF.
    // 2. If cp ≤ 0xFFFF, return the String value consisting of the code unit whose numeric value is cp.
    // 3. Let cu1 be the code unit whose numeric value is floor((cp - 0x10000) / 0x400) + 0xD800.
    // 4. Let cu2 be the code unit whose numeric value is ((cp - 0x10000) modulo 0x400) + 0xDC00.
    // 5. Return the string-concatenation of cu1 and cu2.
    if cp <= 0xFFFF {
        dst[0] = cp as u16;
        Ok(&mut dst[0..1])
    } else if cp <= 0x10_FFFF {
        let c = (cp - 0x10_0000) as u16;
        dst[0] = c / 0x400 + 0xD800;
        dst[1] = c % 0x400 + 0xDC00;
        Ok(&mut dst[0..2])
    } else {
        Err(InternalRuntimeError::CodePointOutOfRange)?
    }
}

pub fn is_str_whitespace(ch: u16) -> bool {
    (0x09..=0x0d).contains(&ch)
        || ch == 0x20
        || ch == 0x00a0
        || ch == 0x2028
        || ch == 0x2029
        || ch == 0xfeff
        || ch == 0x1680
        || (0x2000..=0x200a).contains(&ch)
        || ch == 0x202f
        || ch == 0x205f
        || ch == 0x3000
}

pub fn is_radix_digit(ch: u16, radix: i32) -> bool {
    assert!((2..=36).contains(&radix));
    let radix = u16::try_from(radix).unwrap();
    if radix <= 10 {
        (0x30..(0x30 + radix)).contains(&ch)
    } else {
        (0x30..=0x39).contains(&ch)
            || (0x61..(0x61 + radix - 10)).contains(&ch)
            || (0x41..(0x41 + radix - 10)).contains(&ch)
    }
}

impl JSString {
    pub fn to_bigint(&self) -> Option<Rc<BigInt>> {
        // StringToBigInt ( str )
        // The abstract operation StringToBigInt takes argument str (a String) and returns a BigInt or undefined. It
        // performs the following steps when called:
        //
        //  1. Let text be StringToCodePoints(str).
        //  2. Let literal be ParseText(text, StringIntegerLiteral).
        //  3. If literal is a List of errors, return undefined.
        //  4. Let mv be the MV of literal.
        //  5. Assert: mv is an integer.
        //  6. Return ℤ(mv).
        let mut code_units = self.s.as_ref();
        while let [first, rest @ ..] = code_units {
            if is_str_whitespace(*first) {
                code_units = rest;
            } else {
                break;
            }
        }
        while let [rest @ .., last] = code_units {
            if is_str_whitespace(*last) {
                code_units = rest;
            } else {
                break;
            }
        }
        if code_units.is_empty() {
            return Some(Rc::new(BigInt::from(0)));
        }
        let radix = if code_units.len() >= 2 && code_units[0] == 0x30 {
            match code_units[1] {
                98 | 66 => 2,
                120 | 88 => 16,
                79 | 111 => 8,
                _ => 10,
            }
        } else {
            10
        };
        if radix != 10 {
            code_units = &code_units[2..];
        }
        let digits = code_units.iter().map(|word| u8::try_from(*word).ok()).collect::<Option<Vec<u8>>>()?;
        BigInt::parse_bytes(&digits, radix).map(Rc::new)
    }

    pub fn to_bigint_radix(&self, radix: u32) -> BigInt {
        let digits = self.as_slice().iter().map(|word| u8::try_from(*word).unwrap()).collect::<Vec<u8>>();
        BigInt::parse_bytes(digits.as_slice(), radix).unwrap()
    }
}

impl From<Rc<BigInt>> for JSString {
    fn from(value: Rc<BigInt>) -> Self {
        JSString::from(value.to_str_radix(10))
    }
}

impl JSString {
    pub fn string_index_of(&self, search_value: &JSString, from_index: usize) -> Option<usize> {
        // StringIndexOf ( string, searchValue, fromIndex )
        // The abstract operation StringIndexOf takes arguments string (a String), searchValue (a String), and
        // fromIndex (a non-negative integer) and returns a non-negative integer or not-found. It performs the
        // following steps when called:
        //
        //  1. Let len be the length of string.
        //  2. If searchValue is the empty String and fromIndex ≤ len, return fromIndex.
        //  3. Let searchLen be the length of searchValue.
        //  4. For each integer i such that fromIndex ≤ i ≤ len - searchLen, in ascending order, do
        //      a. Let candidate be the substring of string from i to i + searchLen.
        //      b. If candidate is searchValue, return i.
        //  5. Return not-found.
        //
        // Note 1 | If searchValue is the empty String and fromIndex ≤ the length of string, this algorithm
        //        | returns fromIndex. The empty String is effectively found at every position within a
        //        | string, including after the last code unit.
        //
        // Note 2 | This algorithm always returns not-found if fromIndex + the length of searchValue > the
        //        | length of string.
        let len = self.len();
        if search_value.is_empty() && from_index <= len {
            return Some(from_index);
        }
        let search_len = search_value.len();
        if from_index <= len - search_len {
            for i in from_index..=len - search_len {
                let candidate = &self.as_slice()[i..i + search_len];
                if candidate == search_value.as_slice() {
                    return Some(i);
                }
            }
        }
        None
    }
}

#[cfg(test)]
mod tests;
