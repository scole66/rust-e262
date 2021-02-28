use std::fmt;
use std::rc::Rc;

#[derive(PartialEq, Clone)]
pub struct JSString {
    string: Rc<Vec<u16>>,
}

impl fmt::Debug for JSString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", String::from_utf16_lossy(&self.string)))
    }
}

impl fmt::Display for JSString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", String::from_utf16_lossy(&self.string)))
    }
}

impl std::cmp::PartialEq<&str> for JSString {
    fn eq(&self, other: &&str) -> bool {
        let mut iter_vec = self.string.iter();
        let mut iter_chars = (*other).chars();
        loop {
            let left = iter_vec.next();
            let right = iter_chars.next();
            if left.is_none() && right.is_none() {
                return true;
            }
            if left.is_none() || right.is_none() {
                return false;
            }
            if *(left.unwrap()) as u32 != right.unwrap() as u32 {
                return false;
            }
        }
    }
}

impl From<&str> for JSString {
    fn from(source: &str) -> Self {
        let mut result = Vec::with_capacity(source.len());
        for val in source.encode_utf16() {
            result.push(val);
        }
        JSString { string: Rc::new(result) }
    }
}

impl From<&[u16]> for JSString {
    fn from(source: &[u16]) -> Self {
        let mut result = Vec::with_capacity(source.len());
        result.extend_from_slice(source);
        JSString { string: Rc::new(result) }
    }
}

impl From<Vec<u16>> for JSString {
    fn from(source: Vec<u16>) -> Self {
        JSString { string: Rc::new(source) }
    }
}
