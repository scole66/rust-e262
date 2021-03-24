use crate::agent::Agent;
use crate::cr::Completion;
use crate::dtoa_r::dtoa;
use crate::errors::create_type_error;
use crate::object::{call, get, get_method, Object};
use crate::strings::JSString;
use num::bigint::BigInt;
use std::io;
use std::rc::Rc;

#[derive(Clone, PartialEq, Debug)]
pub enum ECMAScriptValue {
    Undefined,
    Null,
    Boolean(bool),
    String(JSString),
    Number(f64),
    BigInt(Rc<BigInt>),
    Symbol(Symbol),
    Object(Object),
}

impl ECMAScriptValue {
    pub fn is_object(&self) -> bool {
        matches!(self, ECMAScriptValue::Object(_))
    }
    pub fn is_undefined(&self) -> bool {
        matches!(self, ECMAScriptValue::Undefined)
    }
    pub fn is_null(&self) -> bool {
        matches!(self, ECMAScriptValue::Null)
    }
    pub fn is_boolean(&self) -> bool {
        matches!(self, ECMAScriptValue::Boolean(_))
    }
    pub fn is_string(&self) -> bool {
        matches!(self, ECMAScriptValue::String(_))
    }
    pub fn is_number(&self) -> bool {
        matches!(self, ECMAScriptValue::Number(_))
    }
    pub fn is_bigint(&self) -> bool {
        matches!(self, ECMAScriptValue::BigInt(_))
    }
    pub fn is_symbol(&self) -> bool {
        matches!(self, ECMAScriptValue::Symbol(_))
    }
    pub fn is_numeric(&self) -> bool {
        self.is_number() || self.is_bigint()
    }
}

impl From<&Object> for ECMAScriptValue {
    fn from(source: &Object) -> Self {
        Self::Object(source.clone())
    }
}

impl From<&str> for ECMAScriptValue {
    fn from(source: &str) -> Self {
        Self::String(JSString::from(source))
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum PropertyKey {
    String(JSString),
    Symbol(Symbol),
}

impl PropertyKey {
    pub fn is_array_index(&self) -> bool {
        // An array index is an integer index whose numeric value i is in the range +0𝔽 ≤ i < 𝔽(2^32 - 1).
        match self {
            PropertyKey::Symbol(_) => false,
            PropertyKey::String(s) => String::from_utf16(s.as_slice()).map(|sss| sss.parse::<u32>().is_ok()).unwrap_or(false),
        }
    }
}

impl From<&str> for PropertyKey {
    fn from(source: &str) -> Self {
        Self::String(JSString::from(source))
    }
}

impl From<Symbol> for PropertyKey {
    fn from(source: Symbol) -> Self {
        Self::Symbol(source)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Symbol {
    ToPrimitive,
}

pub fn number_to_string<T>(writer: &mut T, value: f64) -> io::Result<()>
where
    T: io::Write,
{
    if value.is_nan() {
        return write!(writer, "NaN");
    }
    if value == 0.0 {
        return write!(writer, "0");
    }
    if value < 0.0 {
        write!(writer, "-")?;
        return number_to_string(writer, -value);
    }
    if value.is_infinite() {
        return write!(writer, "Infinity");
    }
    let info = dtoa(value);

    let k = info.chars.find('\u{0}').unwrap() as i64;
    let n = info.decpt as i64;
    let mut iter = info.chars.chars();
    if k <= n && n <= 21 {
        for _ in 0..k {
            let ch = iter.next().unwrap();
            write!(writer, "{}", ch)?;
        }
        let zeros = n - k;
        for _ in 0..zeros {
            write!(writer, "0")?;
        }
        return Ok(());
    }
    if 0 < n && n <= 21 {
        for _ in 0..n {
            let ch = iter.next().unwrap();
            write!(writer, "{}", ch)?;
        }
        write!(writer, ".")?;
        for _ in 0..k - n {
            let ch = iter.next().unwrap();
            write!(writer, "{}", ch)?;
        }
        return Ok(());
    }
    if -6 < n && n <= 0 {
        write!(writer, "0.")?;
        for _ in n..0 {
            write!(writer, "0")?;
        }
        for _ in 0..k {
            let ch = iter.next().unwrap();
            write!(writer, "{}", ch)?;
        }
        return Ok(());
    }
    let ch = iter.next().unwrap();
    write!(writer, "{}", ch)?;
    if k > 1 {
        write!(writer, ".")?;
        for _ in 1..k {
            let ch = iter.next().unwrap();
            write!(writer, "{}", ch)?;
        }
    }
    write!(writer, "e")?;
    let v = n - 1;
    write!(writer, "{}{}", if v > 0 { '+' } else { '-' }, v.abs())?;
    Ok(())
}

// OrdinaryToPrimitive ( O, hint )
//
// The abstract operation OrdinaryToPrimitive takes arguments O and hint. It performs the following steps when called:
//
//  1. Assert: Type(O) is Object.
//  2. Assert: hint is either string or number.
//  3. If hint is string, then
//      a. Let methodNames be « "toString", "valueOf" ».
//  4. Else,
//      a. Let methodNames be « "valueOf", "toString" ».
//  5. For each element name of methodNames, do
//      a. Let method be ? Get(O, name).
//      b. If IsCallable(method) is true, then
//          i. Let result be ? Call(method, O).
//          ii. If Type(result) is not Object, return result.
//  6. Throw a TypeError exception.
pub enum ConversionHint {
    String,
    Number,
}
pub fn ordinary_to_primitive(agent: &mut Agent, obj: &Object, hint: ConversionHint) -> Completion {
    let method_names = match hint {
        ConversionHint::String => vec![PropertyKey::from("toString"), PropertyKey::from("valueOf")],
        ConversionHint::Number => vec![PropertyKey::from("valueOf"), PropertyKey::from("toString")],
    };
    for name in method_names.iter() {
        let method = get(agent, obj, name)?;
        if is_callable(agent, &method) {
            let result = call(agent, &method, &ECMAScriptValue::from(obj), None)?;
            if !result.is_object() {
                return Ok(result);
            }
        }
    }
    Err(create_type_error(agent, "Cannot convert object to primitive value"))
}

// ToPrimitive ( input [ , preferredType ] )
//
// The abstract operation ToPrimitive takes argument input and optional argument preferredType. It converts its input
// argument to a non-Object type. If an object is capable of converting to more than one primitive type, it may use the
// optional hint preferredType to favour that type. It performs the following steps when called:
//
//  1. Assert: input is an ECMAScript language value.
//  2. If Type(input) is Object, then
//      a. Let exoticToPrim be ? GetMethod(input, @@toPrimitive).
//      b. If exoticToPrim is not undefined, then
//          i. If preferredType is not present, let hint be "default".
//          ii. Else if preferredType is string, let hint be "string".
//          iii. Else,
//              1. Assert: preferredType is number.
//              2. Let hint be "number".
//          iv. Let result be ? Call(exoticToPrim, input, « hint »).
//          v. If Type(result) is not Object, return result.
//          vi. Throw a TypeError exception.
//      c. If preferredType is not present, let preferredType be number.
//      d. Return ? OrdinaryToPrimitive(input, preferredType).
//  3. Return input.
//
// NOTE     When ToPrimitive is called with no hint, then it generally behaves as if the hint were number. However,
//          objects may over-ride this behaviour by defining a @@toPrimitive method. Of the objects defined in this
//          specification only Date objects (see 21.4.4.45) and Symbol objects (see 20.4.3.5) over-ride the default
//          ToPrimitive behaviour. Date objects treat no hint as if the hint were string.
pub fn to_primitive(agent: &mut Agent, input: &ECMAScriptValue, preferred_type: Option<ConversionHint>) -> Completion {
    if let ECMAScriptValue::Object(obj) = input {
        let exotic_to_prim = get_method(agent, input, &PropertyKey::from(agent.symbols.to_primitive))?;
        if !exotic_to_prim.is_undefined() {
            let hint = ECMAScriptValue::from(match preferred_type {
                None => "default",
                Some(ConversionHint::Number) => "number",
                Some(ConversionHint::String) => "string",
            });
            let result = call(agent, &exotic_to_prim, input, Some(vec![hint]))?;
            if !result.is_object() {
                return Ok(result);
            }
            return Err(create_type_error(agent, "Cannot covert object to primitive value"));
        }
        let pt = preferred_type.unwrap_or(ConversionHint::Number);
        ordinary_to_primitive(agent, obj, pt)
    } else {
        Ok(input.clone())
    }
}

// IsCallable ( argument )
//
// The abstract operation IsCallable takes argument argument (an ECMAScript language value). It determines if argument
// is a callable function with a [[Call]] internal method. It performs the following steps when called:
//
//  1. If Type(argument) is not Object, return false.
//  2. If argument has a [[Call]] internal method, return true.
//  3. Return false.
pub fn is_callable(agent: &mut Agent, value: &ECMAScriptValue) -> bool {
    if !value.is_object() {
        false
    } else {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
