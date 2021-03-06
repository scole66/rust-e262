use lasso::{Rodeo, Spur};
use std::cell::RefCell;
use std::char::{decode_utf16, DecodeUtf16};
use std::fmt;
use std::iter::Cloned;
use std::slice::Iter;
use std::str::EncodeUtf16;

// So: JS String.
//
// At it's core, this is simply a Vec<u16>, but interned for speed & storage.
//
// The API for pulling out pieces of them hasn't really been defined yet.

thread_local! {
    static STRING_POOL: RefCell<Rodeo<Spur>> = Default::default();
}

#[derive(PartialEq, Clone)]
pub struct JSString {
    s: Spur,
}

struct CharToU16Iterator<'a> {
    source: EncodeUtf16<'a>,
}

impl<'a> CharToU16Iterator<'a> {
    fn new<T>(source: T) -> Self
    where
        T: Into<&'a str>,
    {
        CharToU16Iterator { source: source.into().encode_utf16() }
    }
}

impl<'a> Iterator for CharToU16Iterator<'a> {
    type Item = u16;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.source.next()?;
        match ch {
            0xE00E | 0xE00D => {
                let ch2 = self.source.next();
                match ch2 {
                    None => Some(ch),
                    Some(c) => Some(((ch & 0xF) << 12) | c),
                }
            }
            c => Some(c),
        }
    }
}

struct U16ToCharIterator<'a> {
    source: DecodeUtf16<Cloned<Iter<'a, u16>>>,
    next_result: Option<char>,
}

impl<'a> U16ToCharIterator<'a> {
    fn new(source: Cloned<Iter<'a, u16>>) -> Self {
        let new_iter = decode_utf16(source);
        U16ToCharIterator { source: new_iter, next_result: None }
    }
}

impl<'a> Iterator for U16ToCharIterator<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ch) = self.next_result {
            self.next_result = None;
            return Some(ch);
        }
        let ch = self.source.next()?;
        match ch {
            Err(e) => {
                let unpaired = e.unpaired_surrogate() as u32;
                self.next_result = Some(std::char::from_u32(unpaired & 0xFFF).unwrap());
                Some('\u{E00D}')
            }
            Ok('\u{E00D}') => {
                self.next_result = Some('\u{000D}');
                Some('\u{E00E}')
            }
            Ok('\u{E00E}') => {
                self.next_result = Some('\u{000E}');
                Some('\u{E00E}')
            }
            Ok(c) => Some(c),
        }
    }
}

impl From<&[u16]> for JSString {
    fn from(source: &[u16]) -> Self {
        let iter = U16ToCharIterator::new(source.iter().cloned());
        let encoded: String = iter.collect();
        Self::from(encoded)
    }
}

impl From<Vec<u16>> for JSString {
    fn from(source: Vec<u16>) -> Self {
        Self::from(source.as_slice())
    }
}

impl From<&str> for JSString {
    fn from(source: &str) -> Self {
        STRING_POOL.with(|sp| {
            let mut pool = sp.borrow_mut();
            let key = pool.get_or_intern(source);
            JSString { s: key }
        })
    }
}

impl From<String> for JSString {
    fn from(source: String) -> Self {
        Self::from(source.as_str())
    }
}

impl fmt::Debug for JSString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        STRING_POOL.with(|sp| {
            let pool = sp.borrow();
            let s = pool.resolve(&self.s);
            f.write_fmt(format_args!("{:?}", s))
        })
    }
}

impl fmt::Display for JSString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        STRING_POOL.with(|sp| {
            let pool = sp.borrow();
            let s = pool.resolve(&self.s);
            f.write_fmt(format_args!("{}", s))
        })
    }
}

impl std::cmp::PartialEq<&str> for JSString {
    fn eq(&self, other: &&str) -> bool {
        STRING_POOL.with(|sp| {
            let pool = sp.borrow();
            let maybe_other_key = pool.get(*other);
            if let Some(other_key) = maybe_other_key {
                self.s == other_key
            } else {
                false
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_u16_test() {
        let src: &[u16] = &[66, 111, 98]; // Bob
        let res = JSString::from(src);
        let display = format!("{}", res);
        assert_eq!(display, "Bob");
    }
}
