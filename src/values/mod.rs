use super::agent::{Agent, WksId};
use super::bigint_object::create_bigint_object;
use super::boolean_object::create_boolean_object;
use super::cr::{AltCompletion, Completion};
use super::dtoa_r::dtoa;
use super::errors::create_type_error;
use super::number_object::create_number_object;
use super::object::{call, get, get_method, to_callable, Object};
use super::string_object::create_string_object;
use super::strings::JSString;
use super::symbol_object::create_symbol_object;
use lazy_static::lazy_static;
use num::{BigInt, BigUint, Num, ToPrimitive};
use regex::Regex;
use std::cell::RefCell;
use std::convert::TryFrom;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::io;
use std::rc::Rc;
use uid::Id as IdT;

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

    pub fn concise(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ECMAScriptValue::Undefined => write!(f, "undefined"),
            ECMAScriptValue::Null => write!(f, "null"),
            ECMAScriptValue::Boolean(x) => write!(f, "{}", x),
            ECMAScriptValue::String(s) => write!(f, "{:?}", format!("{}", s)),
            ECMAScriptValue::Number(n) => write!(f, "{:?}", n),
            ECMAScriptValue::BigInt(b) => write!(f, "{:?}", b),
            ECMAScriptValue::Symbol(sym) => write!(f, "{}", sym),
            ECMAScriptValue::Object(o) => o.concise(f),
        }
    }
}

impl From<&Object> for ECMAScriptValue {
    fn from(source: &Object) -> Self {
        Self::Object(source.clone())
    }
}

impl From<Object> for ECMAScriptValue {
    fn from(source: Object) -> Self {
        // Consumes the input object, transforming it into a value.
        Self::Object(source)
    }
}

impl From<&JSString> for ECMAScriptValue {
    fn from(source: &JSString) -> Self {
        Self::String(source.clone())
    }
}

impl<T> From<T> for ECMAScriptValue
where
    T: Into<JSString>,
{
    fn from(source: T) -> Self {
        Self::String(source.into())
    }
}

impl From<bool> for ECMAScriptValue {
    fn from(source: bool) -> Self {
        Self::Boolean(source)
    }
}

impl From<f64> for ECMAScriptValue {
    fn from(source: f64) -> Self {
        Self::Number(source)
    }
}

impl From<u32> for ECMAScriptValue {
    fn from(source: u32) -> Self {
        Self::Number(source as f64)
    }
}

impl From<i32> for ECMAScriptValue {
    fn from(source: i32) -> Self {
        Self::Number(source as f64)
    }
}

impl From<u64> for ECMAScriptValue {
    fn from(val: u64) -> Self {
        if val <= 1 << 53 {
            Self::from(val as f64)
        } else {
            Self::from(BigInt::from(val))
        }
    }
}

impl From<i64> for ECMAScriptValue {
    fn from(val: i64) -> Self {
        if -(1 << 53) <= val && val <= 1 << 53 {
            Self::from(val as f64)
        } else {
            Self::from(BigInt::from(val))
        }
    }
}

impl From<BigInt> for ECMAScriptValue {
    fn from(source: BigInt) -> Self {
        Self::BigInt(Rc::new(source))
    }
}

impl From<Symbol> for ECMAScriptValue {
    fn from(source: Symbol) -> Self {
        Self::Symbol(source)
    }
}

impl Default for ECMAScriptValue {
    fn default() -> Self {
        Self::Undefined
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PropertyKey {
    String(JSString),
    Symbol(Symbol),
}

impl PropertyKey {
    pub fn is_array_index(&self) -> bool {
        // An array index is an integer index whose numeric value i is in the range +0𝔽 ≤ i < 𝔽(2^32 - 1).
        match self {
            PropertyKey::Symbol(_) => false,
            PropertyKey::String(s) => {
                String::from_utf16(s.as_slice()).map(|sss| sss.parse::<u32>().map(|int_val| (int_val == 0 && sss.len() == 1) || !sss.starts_with('0')).unwrap_or(false)).unwrap_or(false)
            }
        }
    }
}

impl fmt::Display for PropertyKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PropertyKey::String(string) => string.fmt(f),
            PropertyKey::Symbol(sym) => sym.fmt(f),
        }
    }
}

impl From<&str> for PropertyKey {
    fn from(source: &str) -> Self {
        Self::from(JSString::from(source))
    }
}

impl From<&JSString> for PropertyKey {
    fn from(source: &JSString) -> Self {
        Self::from(source.clone())
    }
}

impl From<JSString> for PropertyKey {
    fn from(source: JSString) -> Self {
        Self::String(source)
    }
}

impl From<Symbol> for PropertyKey {
    fn from(source: Symbol) -> Self {
        Self::Symbol(source)
    }
}

impl From<String> for PropertyKey {
    fn from(source: String) -> Self {
        Self::from(JSString::from(source))
    }
}

impl TryFrom<PropertyKey> for JSString {
    type Error = &'static str;
    fn try_from(key: PropertyKey) -> Result<Self, Self::Error> {
        match key {
            PropertyKey::String(s) => Ok(s),
            PropertyKey::Symbol(_) => Err("Expected String-valued property key"),
        }
    }
}

impl TryFrom<&PropertyKey> for JSString {
    type Error = &'static str;
    fn try_from(key: &PropertyKey) -> Result<Self, Self::Error> {
        match key {
            PropertyKey::String(s) => Ok(s.clone()),
            PropertyKey::Symbol(_) => Err("Expected String-valued property key"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolInternals {
    pub id: usize,
    pub description: Option<JSString>,
}

#[derive(Debug, Clone)]
pub struct Symbol(pub Rc<SymbolInternals>);

impl PartialEq for Symbol {
    fn eq(&self, other: &Symbol) -> bool {
        self.0.id == other.0.id
    }
}
impl Eq for Symbol {}
impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.id.hash(state);
    }
}

impl Symbol {
    pub fn new(agent: &mut Agent, description: Option<JSString>) -> Self {
        Self(Rc::new(SymbolInternals { id: agent.next_symbol_id(), description }))
    }
    pub fn description(&self) -> Option<JSString> {
        self.0.description.as_ref().cloned()
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Symbol({})", self.description().unwrap_or_else(|| JSString::from("")))
    }
}

// Private Names
//
// The Private Name specification type is used to describe a globally unique value (one which differs from any other
// Private Name, even if they are otherwise indistinguishable) which represents the key of a private class element
// (field, method, or accessor). Each Private Name has an associated immutable [[Description]] which is a String value.
// A Private Name may be installed on any ECMAScript object with PrivateFieldAdd or PrivateMethodOrAccessorAdd, and
// then read or written using PrivateGet and PrivateSet.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
struct PN(());
type PNId = IdT<PN>;

#[derive(Debug, Clone, Eq)]
pub struct PrivateName {
    pub description: JSString,
    id: PNId,
}
impl PartialEq for PrivateName {
    fn eq(&self, other: &Self) -> bool {
        // Ids are unique, so we don't need to look at the description field.
        self.id == other.id
    }
}
impl Hash for PrivateName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // No need to hash the description; the id is unique already.
        self.id.hash(state)
    }
}

impl PrivateName {
    pub fn new(description: impl Into<JSString>) -> Self {
        PrivateName { description: description.into(), id: PNId::new() }
    }
}

// The PrivateElement Specification Type
//
// The PrivateElement type is a Record used in the specification of private class fields, methods, and accessors.
// Although Property Descriptors are not used for private elements, private fields behave similarly to
// non-configurable, non-enumerable, writable data properties, private methods behave similarly to non-configurable,
// non-enumerable, non-writable data properties, and private accessors behave similarly to non-configurable,
// non-enumerable accessor properties.
//
// Values of the PrivateElement type are Record values whose fields are defined by Table 11. Such values are referred
// to as PrivateElements.
//
// Table 11: PrivateElement Fields
// +------------+------------------+-------------------------------+---------------------------------------------+
// | Field Name | Values of the    | Value                         | Meaning                                     |
// |            | [[Kind]] field   |                               |                                             |
// |            | for which it is  |                               |                                             |
// |            | present          |                               |                                             |
// +------------+------------------+-------------------------------+---------------------------------------------+
// | [[Key]]    | All              | a Private Name                | The name of the field, method, or accessor. |
// +------------+------------------+-------------------------------+---------------------------------------------+
// | [[Kind]]   | All              | field, method, or accessor    | The kind of the element.                    |
// +------------+------------------+-------------------------------+---------------------------------------------+
// | [[Value]]  | field and method | any ECMAScript language value | The value of the field.                     |
// +------------+------------------+-------------------------------+---------------------------------------------+
// | [[Get]]    | accessor         | Function or Undefined         | The getter for a private accessor.          |
// +------------+------------------+-------------------------------+---------------------------------------------+
// | [[Set]]    | accessor         | Function or Undefined         | The setter for a private accessor.          |
// +------------+------------------+-------------------------------+---------------------------------------------+
#[derive(Debug, Clone)]
pub enum PrivateElementKind {
    Field { value: RefCell<ECMAScriptValue> },
    Method { value: ECMAScriptValue },
    Accessor { get: Option<Object>, set: Option<Object> },
}
#[derive(Debug, Clone)]
pub struct PrivateElement {
    pub key: PrivateName,
    pub kind: PrivateElementKind,
}
impl PrivateElement {}

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
        if is_callable(&method) {
            let result = call(agent, &method, &ECMAScriptValue::from(obj), &[])?;
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
pub fn to_primitive(agent: &mut Agent, input: ECMAScriptValue, preferred_type: Option<ConversionHint>) -> Completion {
    if let ECMAScriptValue::Object(obj) = &input {
        let exotic_to_prim = get_method(agent, &input, &PropertyKey::from(agent.wks(WksId::ToPrimitive)))?;
        if !exotic_to_prim.is_undefined() {
            let hint = ECMAScriptValue::from(match preferred_type {
                None => "default",
                Some(ConversionHint::Number) => "number",
                Some(ConversionHint::String) => "string",
            });
            let result = call(agent, &exotic_to_prim, &input, &[hint])?;
            if !result.is_object() {
                return Ok(result);
            }
            return Err(create_type_error(agent, "Cannot convert object to primitive value"));
        }
        let pt = preferred_type.unwrap_or(ConversionHint::Number);
        ordinary_to_primitive(agent, obj, pt)
    } else {
        Ok(input)
    }
}

// ToBoolean ( argument )
//
// The abstract operation ToBoolean takes argument argument. It converts argument to a value of type Boolean according
// to Table 11:
//
// Table 11: ToBoolean Conversions
// +---------------+-----------------------------------------------------------------------------------------+
// | Argument Type | Result                                                                                  |
// +---------------+-----------------------------------------------------------------------------------------+
// | Undefined     | Return false.                                                                           |
// | Null          | Return false.                                                                           |
// | Boolean       | Return argument.                                                                        |
// | Number        | If argument is +0𝔽, -0𝔽, or NaN, return false; otherwise return true.                   |
// | String        | If argument is the empty String (its length is 0), return false; otherwise return true. |
// | Symbol        | Return true.                                                                            |
// | BigInt        | If argument is 0ℤ, return false; otherwise return true.                                 |
// | Object        | Return true.                                                                            |
// +---------------+-----------------------------------------------------------------------------------------+
// NOTE     An alternate algorithm related to the [[IsHTMLDDA]] internal slot is mandated in section B.3.7.1.
impl From<ECMAScriptValue> for bool {
    fn from(val: ECMAScriptValue) -> bool {
        match val {
            ECMAScriptValue::Undefined => false,
            ECMAScriptValue::Null => false,
            ECMAScriptValue::Boolean(b) => b,
            ECMAScriptValue::Number(num) => !(num.is_nan() || num == 0.0),
            ECMAScriptValue::String(s) => s.len() > 0,
            ECMAScriptValue::Symbol(_) => true,
            ECMAScriptValue::BigInt(b) => *b != BigInt::from(0),
            ECMAScriptValue::Object(_) => true,
        }
    }
}
pub fn to_boolean(val: ECMAScriptValue) -> bool {
    bool::from(val)
}

// ToNumeric ( value )
//
// The abstract operation ToNumeric takes argument value. It returns value converted to a Number or a BigInt. It
// performs the following steps when called:
//
//      1. Let primValue be ? ToPrimitive(value, number).
//      2. If Type(primValue) is BigInt, return primValue.
//      3. Return ? ToNumber(primValue).
#[derive(Debug, PartialEq)]
pub enum Numeric {
    Number(f64),
    BigInt(Rc<BigInt>),
}
pub fn to_numeric(agent: &mut Agent, value: ECMAScriptValue) -> AltCompletion<Numeric> {
    let prim_value = to_primitive(agent, value, Some(ConversionHint::Number))?;
    if let ECMAScriptValue::BigInt(bi) = prim_value {
        Ok(Numeric::BigInt(bi))
    } else {
        Ok(Numeric::Number(to_number(agent, prim_value)?))
    }
}

// ToNumber ( argument )
//
// The abstract operation ToNumber takes argument argument. It converts argument to a value of type Number according to
// Table 14:
//
//   Table 14: ToNumber Conversions
// +---------------+-------------------------------------------------------------------+
// | Argument Type | Result                                                            |
// +---------------+-------------------------------------------------------------------+
// | Undefined     | Return NaN.                                                       |
// +---------------+-------------------------------------------------------------------+
// | Null          | Return +0𝔽.                                                       |
// +---------------+-------------------------------------------------------------------+
// | Boolean       | If argument is true, return 1𝔽. If argument is false, return +0𝔽. |
// +---------------+-------------------------------------------------------------------+
// | Number        | Return argument (no conversion).                                  |
// +---------------+-------------------------------------------------------------------+
// | String        | See grammar and conversion algorithm below.                       |
// +---------------+-------------------------------------------------------------------+
// | Symbol        | Throw a TypeError exception.                                      |
// +---------------+-------------------------------------------------------------------+
// | BigInt        | Throw a TypeError exception.                                      |
// +---------------+-------------------------------------------------------------------+
// | Object        | Apply the following steps:                                        |
// |               |     1. Let primValue be ? ToPrimitive(argument, number).          |
// |               |     2. Return ? ToNumber(primValue).                              |
// +---------------+-------------------------------------------------------------------+
pub fn to_number(agent: &mut Agent, value: ECMAScriptValue) -> AltCompletion<f64> {
    match value {
        ECMAScriptValue::Undefined => Ok(f64::NAN),
        ECMAScriptValue::Null => Ok(0_f64),
        ECMAScriptValue::Boolean(b) => Ok(if b { 1_f64 } else { 0_f64 }),
        ECMAScriptValue::Number(n) => Ok(n),
        ECMAScriptValue::String(s) => Ok(string_to_number(s)),
        ECMAScriptValue::BigInt(_) => Err(create_type_error(agent, "BigInt values cannot be converted to Number values")),
        ECMAScriptValue::Symbol(_) => Err(create_type_error(agent, "Symbol values cannot be converted to Number values")),
        ECMAScriptValue::Object(o) => {
            let prim_value = to_primitive(agent, ECMAScriptValue::from(o), Some(ConversionHint::Number))?;
            to_number(agent, prim_value)
        }
    }
}

fn string_to_number(string: JSString) -> f64 {
    lazy_static! {
        static ref STR_WHITE_SPACE: &'static str = r"(?:[\t\v\f \u{a0}\u{feff}\n\r\u{2028}\u{2029}]+)";
        static ref DECIMAL_DIGITS: &'static str = "(?:[0-9]+)";
        static ref EXPONENT_PART: &'static str = "(?:[eE][-+]?[0-9]+)";
        static ref STR_UNSIGNED_DECIMAL_LITERAL: String =
            format!(r"(?:Infinity|{}\.{}?{}?|\.{}{}?|{}{}?)", *DECIMAL_DIGITS, *DECIMAL_DIGITS, *EXPONENT_PART, *DECIMAL_DIGITS, *EXPONENT_PART, *DECIMAL_DIGITS, *EXPONENT_PART);
        static ref STR_DECIMAL_LITERAL: String = format!(r"(?P<decimal>[-+]?{})", *STR_UNSIGNED_DECIMAL_LITERAL);
        static ref BINARY_INTEGER_LITERAL: &'static str = "(?:0[bB](?P<binary>[01]+))";
        static ref OCTAL_INTEGER_LITERAL: &'static str = "(?:0[oO](?P<octal>[0-7]+))";
        static ref HEX_INTEGER_LITERAL: &'static str = "(?:0[xX](?P<hex>[0-9a-fA-F]+))";
        static ref NONDECIMAL_INTEGER_LITERAL: String = format!("(?:{}|{}|{})", *BINARY_INTEGER_LITERAL, *OCTAL_INTEGER_LITERAL, *HEX_INTEGER_LITERAL);
        static ref STR_NUMERIC_LITERAL: String = format!("(?:{}|{})", *STR_DECIMAL_LITERAL, *NONDECIMAL_INTEGER_LITERAL);
        static ref STRING_NUMERIC_LITERAL: String = format!("^(?:{}?|{}?{}{}?)$", *STR_WHITE_SPACE, *STR_WHITE_SPACE, *STR_NUMERIC_LITERAL, *STR_WHITE_SPACE);
        static ref MATCHER: Regex = Regex::new(&*STRING_NUMERIC_LITERAL).unwrap();
    }

    let number_string = String::from(string);
    match MATCHER.captures(&number_string) {
        None => f64::NAN,
        Some(captures) => captures.name("decimal").map_or_else(
            || {
                captures.name("binary").map_or_else(
                    || {
                        captures.name("octal").map_or_else(
                            || captures.name("hex").map_or(0.0, |hex| BigUint::from_str_radix(hex.as_str(), 16).unwrap().to_f64().unwrap()),
                            |octal| BigUint::from_str_radix(octal.as_str(), 8).unwrap().to_f64().unwrap(),
                        )
                    },
                    |binary| BigUint::from_str_radix(binary.as_str(), 2).unwrap().to_f64().unwrap(),
                )
            },
            |s| s.as_str().parse::<f64>().unwrap(),
        ),
    }
}

// ToIntegerOrInfinity ( argument )
//
// The abstract operation ToIntegerOrInfinity takes argument argument (an ECMAScript language value). It converts
// argument to an integer representing its Number value with fractional part truncated, or to +∞ or -∞ when that Number
// value is infinite. It performs the following steps when called:
//
//  1. Let number be ? ToNumber(argument).
//  2. If number is NaN, +0𝔽, or -0𝔽, return 0.
//  3. If number is +∞𝔽, return +∞.
//  4. If number is -∞𝔽, return -∞.
//  5. Let integer be floor(abs(ℝ(number))).
//  6. If number < +0𝔽, set integer to -integer.
//  7. Return integer.
pub fn to_integer_or_infinity(agent: &mut Agent, argument: ECMAScriptValue) -> AltCompletion<f64> {
    let number = to_number(agent, argument)?;
    if number.is_nan() || number == 0.0 {
        Ok(0.0)
    } else if number.is_infinite() {
        Ok(number)
    } else {
        let integer = number.abs().floor();
        if number < 0.0 {
            Ok(-integer)
        } else {
            Ok(integer)
        }
    }
}

// ToString ( argument )
//
// The abstract operation ToString takes argument argument. It converts argument to a value of type String according to
// Table 16:
//
// Table 16: ToString Conversions
// +---------------+-----------------------------------------------------------+
// | Argument Type | Result                                                    |
// +---------------+-----------------------------------------------------------+
// | Undefined     | Return "undefined".                                       |
// +---------------+-----------------------------------------------------------+
// | Null          | Return "null".                                            |
// +---------------+-----------------------------------------------------------+
// | Boolean       | If argument is true, return "true".                       |
// |               | If argument is false, return "false".                     |
// +---------------+-----------------------------------------------------------+
// | Number        | Return ! Number::toString(argument).                      |
// +---------------+-----------------------------------------------------------+
// | String        | Return argument.                                          |
// +---------------+-----------------------------------------------------------+
// | Symbol        | Throw a TypeError exception.                              |
// +---------------+-----------------------------------------------------------+
// | BigInt        | Return ! BigInt::toString(argument).                      |
// +---------------+-----------------------------------------------------------+
// | Object        | Apply the following steps:                                |
// |               |      1. Let primValue be ? ToPrimitive(argument, string). |
// |               |      2. Return ? ToString(primValue).                     |
// +---------------+-----------------------------------------------------------+
pub fn to_string(agent: &mut Agent, val: ECMAScriptValue) -> AltCompletion<JSString> {
    match val {
        ECMAScriptValue::Undefined => Ok(JSString::from("undefined")),
        ECMAScriptValue::Null => Ok(JSString::from("null")),
        ECMAScriptValue::Boolean(b) => Ok(JSString::from(if b { "true" } else { "false" })),
        ECMAScriptValue::Number(n) => {
            let mut s = Vec::new();
            number_to_string(&mut s, n).unwrap();
            Ok(JSString::from(s))
        }
        ECMAScriptValue::String(s) => Ok(s),
        ECMAScriptValue::Symbol(_) => Err(create_type_error(agent, "Symbols may not be converted to strings")),
        ECMAScriptValue::BigInt(bi) => Ok(JSString::from(format!("{}", bi))),
        ECMAScriptValue::Object(o) => {
            let prim_value = to_primitive(agent, ECMAScriptValue::from(o), Some(ConversionHint::String))?;
            to_string(agent, prim_value)
        }
    }
}

// ToObject ( argument )
//
// The abstract operation ToObject takes argument argument. It converts argument to a value of type Object according to
// Table 15:
//
// Table 15: ToObject Conversions
// +---------------+-------------------------------------------------------------------------------------+
// | Argument Type | Result                                                                              |
// +---------------+-------------------------------------------------------------------------------------+
// | Undefined     | Throw a TypeError exception.                                                        |
// | Null          | Throw a TypeError exception.                                                        |
// | Boolean       | Return a new Boolean object whose [[BooleanData]] internal slot is set to argument. |
// | Number        | Return a new Number object whose [[NumberData]] internal slot is set to argument.   |
// | String        | Return a new String object whose [[StringData]] internal slot is set to argument.   |
// | Symbol        | Return a new Symbol object whose [[SymbolData]] internal slot is set to argument.   |
// | BigInt        | Return a new BigInt object whose [[BigIntData]] internal slot is set to argument.   |
// | Object        | Return argument.                                                                    |
// +---------------+-------------------------------------------------------------------------------------+
pub fn to_object(agent: &mut Agent, val: ECMAScriptValue) -> AltCompletion<Object> {
    match val {
        ECMAScriptValue::Null | ECMAScriptValue::Undefined => Err(create_type_error(agent, "Undefined and null cannot be converted to objects")),
        ECMAScriptValue::Boolean(b) => Ok(create_boolean_object(agent, b)),
        ECMAScriptValue::Number(n) => Ok(create_number_object(agent, n)),
        ECMAScriptValue::String(s) => Ok(create_string_object(agent, s)),
        ECMAScriptValue::Symbol(s) => Ok(create_symbol_object(agent, s)),
        ECMAScriptValue::BigInt(b) => Ok(create_bigint_object(agent, b)),
        ECMAScriptValue::Object(o) => Ok(o),
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
pub fn is_callable(value: &ECMAScriptValue) -> bool {
    to_callable(value).is_some()
}

#[cfg(test)]
mod tests;
