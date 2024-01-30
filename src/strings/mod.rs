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

    pub fn concat(&self, s: impl Into<JSString>) -> JSString {
        let tail = s.into();
        let combined = [self.clone().s, tail.s].concat().into_boxed_slice();
        JSString { s: Rc::from(combined) }
    }

    pub fn index_of(&self, search_value: &JSString, from_index: u64) -> i64 {
        let len = self.len();
        if search_value.is_empty() && from_index as usize <= len {
            i64::try_from(from_index).unwrap()
        } else {
            let search_len = search_value.len();
            for i in from_index as usize..=(len - search_len) {
                if self.s[i..(i + search_len)] == search_value.s[..] {
                    return i64::try_from(i).unwrap();
                }
            }
            -1
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
        let v: Vec<u16> = source.iter().map(|v| *v as u16).collect();
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
#[derive(PartialEq, Eq)]
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

fn is_str_whitespace(ch: u16) -> bool {
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

pub fn string_to_bigint(value: JSString) -> Option<Rc<BigInt>> {
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
    let mut code_units = value.s.as_ref();
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

impl From<Rc<BigInt>> for JSString {
    fn from(value: Rc<BigInt>) -> Self {
        JSString::from(value.to_str_radix(10))
    }
}

#[cfg(test)]
mod tests;
