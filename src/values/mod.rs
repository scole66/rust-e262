use super::*;
use anyhow::anyhow;
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

impl fmt::Display for ECMAScriptValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ECMAScriptValue::Undefined => write!(f, "undefined"),
            ECMAScriptValue::Null => write!(f, "null"),
            ECMAScriptValue::Boolean(true) => write!(f, "true"),
            ECMAScriptValue::Boolean(false) => write!(f, "false"),
            ECMAScriptValue::String(s) => write!(f, "{s}"),
            ECMAScriptValue::Number(n) => write!(f, "{n}"),
            ECMAScriptValue::BigInt(bi) => write!(f, "{bi}n"),
            ECMAScriptValue::Symbol(s) => write!(f, "{s}"),
            ECMAScriptValue::Object(o) => write!(f, "{o}"),
        }
    }
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

    // IsArray ( argument )
    //
    // The abstract operation IsArray takes argument argument. It performs the following steps when called:
    //
    //  1. If Type(argument) is not Object, return false.
    //  2. If argument is an Array exotic object, return true.
    //  3. If argument is a Proxy exotic object, then
    //      a. If argument.[[ProxyHandler]] is null, throw a TypeError exception.
    //      b. Let target be argument.[[ProxyTarget]].
    //      c. Return ? IsArray(target).
    //  4. Return false.
    pub fn is_array(&self) -> Completion<bool> {
        match self {
            ECMAScriptValue::Object(obj) => obj.is_array(),
            _ => Ok(false),
        }
    }

    pub fn concise(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ECMAScriptValue::Undefined => write!(f, "undefined"),
            ECMAScriptValue::Null => write!(f, "null"),
            ECMAScriptValue::Boolean(x) => write!(f, "{x}"),
            ECMAScriptValue::String(s) => write!(f, "{:?}", s.to_string()),
            ECMAScriptValue::Number(n) => write!(f, "{n}"),
            ECMAScriptValue::BigInt(b) => write!(f, "{b}n"),
            ECMAScriptValue::Symbol(sym) => write!(f, "{sym}"),
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

impl From<JSString> for ECMAScriptValue {
    fn from(source: JSString) -> Self {
        Self::String(source)
    }
}

impl From<&str> for ECMAScriptValue {
    fn from(source: &str) -> Self {
        Self::String(source.into())
    }
}

impl From<String> for ECMAScriptValue {
    fn from(source: String) -> Self {
        Self::String(source.into())
    }
}

impl From<Vec<u16>> for ECMAScriptValue {
    fn from(source: Vec<u16>) -> Self {
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
    #[allow(clippy::cast_precision_loss)]
    fn from(val: u64) -> Self {
        if val <= 1 << 53 {
            Self::from(val as f64)
        } else {
            Self::from(BigInt::from(val))
        }
    }
}

impl From<usize> for ECMAScriptValue {
    #[allow(clippy::cast_precision_loss)]
    fn from(val: usize) -> Self {
        if val <= 1 << 53 {
            Self::from(val as f64)
        } else {
            Self::from(BigInt::from(val))
        }
    }
}
impl From<i64> for ECMAScriptValue {
    #[allow(clippy::cast_precision_loss)]
    fn from(val: i64) -> Self {
        if (-(1 << 53)..=1 << 53).contains(&val) {
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
impl From<Rc<BigInt>> for ECMAScriptValue {
    fn from(src: Rc<BigInt>) -> Self {
        Self::BigInt(src)
    }
}

impl From<Numeric> for ECMAScriptValue {
    fn from(src: Numeric) -> Self {
        match src {
            Numeric::Number(n) => n.into(),
            Numeric::BigInt(n) => n.into(),
        }
    }
}

impl From<&Numeric> for ECMAScriptValue {
    fn from(src: &Numeric) -> Self {
        match src {
            Numeric::Number(n) => ECMAScriptValue::from(*n),
            Numeric::BigInt(n) => Rc::clone(n).into(),
        }
    }
}

impl From<Symbol> for ECMAScriptValue {
    fn from(source: Symbol) -> Self {
        Self::Symbol(source)
    }
}

impl From<&Symbol> for ECMAScriptValue {
    fn from(value: &Symbol) -> Self {
        Self::Symbol(value.clone())
    }
}

impl From<&PropertyKey> for ECMAScriptValue {
    fn from(value: &PropertyKey) -> Self {
        match value {
            PropertyKey::String(s) => s.into(),
            PropertyKey::Symbol(sym) => sym.into(),
        }
    }
}

impl TryFrom<ECMAScriptValue> for Numeric {
    type Error = anyhow::Error;
    fn try_from(src: ECMAScriptValue) -> Result<Self, Self::Error> {
        match src {
            ECMAScriptValue::Number(n) => Ok(Numeric::Number(n)),
            ECMAScriptValue::BigInt(bi) => Ok(Numeric::BigInt(bi)),
            _ => Err(anyhow!("Value not numeric")),
        }
    }
}

impl TryFrom<ECMAScriptValue> for f64 {
    type Error = anyhow::Error;
    fn try_from(src: ECMAScriptValue) -> Result<Self, Self::Error> {
        match src {
            ECMAScriptValue::Number(n) => Ok(n),
            _ => Err(anyhow!("Value not an f64")),
        }
    }
}

impl TryFrom<ECMAScriptValue> for Rc<BigInt> {
    type Error = anyhow::Error;
    fn try_from(src: ECMAScriptValue) -> Result<Self, Self::Error> {
        match src {
            ECMAScriptValue::BigInt(bi) => Ok(bi),
            _ => Err(anyhow!("Value not a bigint")),
        }
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
        // A String property name P is an array index if and only if ToString(ToUint32(P)) equals P and ToUint32(P) is not the same value as 𝔽(2**32 - 1).
        match self {
            PropertyKey::Symbol(_) => false,
            PropertyKey::String(s) => {
                let as_u32 = to_uint32_agentless(s).expect("strings always convert to numbers");
                let restrung = to_string_agentless(as_u32).expect("numbers always convert to strings");
                as_u32 != 0xFFFF_FFFF && restrung == *s
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

impl From<usize> for PropertyKey {
    fn from(num: usize) -> Self {
        Self::from(num.to_string())
    }
}

impl From<u64> for PropertyKey {
    fn from(num: u64) -> Self {
        Self::from(num.to_string())
    }
}

impl From<i32> for PropertyKey {
    fn from(num: i32) -> Self {
        Self::from(num.to_string())
    }
}

impl From<PropertyKey> for ECMAScriptValue {
    fn from(source: PropertyKey) -> Self {
        match source {
            PropertyKey::Symbol(sym) => sym.into(),
            PropertyKey::String(string) => string.into(),
        }
    }
}

impl TryFrom<PropertyKey> for JSString {
    type Error = anyhow::Error;
    fn try_from(key: PropertyKey) -> Result<Self, Self::Error> {
        match key {
            PropertyKey::String(s) => Ok(s),
            PropertyKey::Symbol(_) => Err(anyhow!("Expected String-valued property key")),
        }
    }
}

impl TryFrom<&PropertyKey> for JSString {
    type Error = anyhow::Error;
    fn try_from(key: &PropertyKey) -> Result<Self, Self::Error> {
        match key {
            PropertyKey::String(s) => Ok(s.clone()),
            PropertyKey::Symbol(_) => Err(anyhow!("Expected String-valued property key")),
        }
    }
}

impl TryFrom<ECMAScriptValue> for PropertyKey {
    type Error = anyhow::Error;
    fn try_from(key: ECMAScriptValue) -> Result<Self, Self::Error> {
        match key {
            ECMAScriptValue::String(s) => Ok(PropertyKey::String(s)),
            ECMAScriptValue::Symbol(sym) => Ok(PropertyKey::Symbol(sym)),
            _ => Err(anyhow!("Bad type for property key")),
        }
    }
}

impl TryFrom<ECMAScriptValue> for Option<Object> {
    type Error = anyhow::Error;
    fn try_from(val: ECMAScriptValue) -> anyhow::Result<Self> {
        match val {
            ECMAScriptValue::Object(o) => Ok(Some(o)),
            ECMAScriptValue::Null => Ok(None),
            _ => Err(anyhow!("Bad type for Object/null")),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord)]
pub struct ArrayIndex(u32);

impl TryFrom<u32> for ArrayIndex {
    type Error = &'static str;
    fn try_from(val: u32) -> Result<ArrayIndex, Self::Error> {
        if val < 0xFFFF_FFFF {
            Ok(ArrayIndex(val))
        } else {
            Err("The maximum array index is 4294967294")
        }
    }
}

impl From<ArrayIndex> for u32 {
    fn from(val: ArrayIndex) -> Self {
        val.0
    }
}

impl TryFrom<&PropertyKey> for ArrayIndex {
    type Error = &'static str;
    fn try_from(key: &PropertyKey) -> Result<ArrayIndex, Self::Error> {
        match key {
            PropertyKey::Symbol(_) => Err("Symbols are not u32s"),
            PropertyKey::String(s) => {
                let s = String::from_utf16_lossy(s.as_slice());
                if s == "0" {
                    Ok(ArrayIndex(0))
                } else if s.starts_with(|ch| ('1'..='9').contains(&ch)) {
                    let val = s.parse::<u32>().map_err(|_| "Invalid array index")?;
                    ArrayIndex::try_from(val)
                } else {
                    Err("Invalid array index")
                }
            }
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
    pub fn new(description: Option<JSString>) -> Self {
        Self(Rc::new(SymbolInternals { id: next_symbol_id(), description }))
    }
    pub fn description(&self) -> Option<JSString> {
        self.0.description.as_ref().cloned()
    }
    pub fn descriptive_string(&self) -> JSString {
        let desc = self.description().unwrap_or_else(|| JSString::from(""));
        JSString::from("Symbol(").concat(desc).concat(")")
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.descriptive_string())
    }
}

#[derive(Debug, PartialEq)]
pub enum ValueKind {
    Undefined,
    Null,
    Boolean,
    String,
    Symbol,
    Number,
    BigInt,
    Object,
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
impl fmt::Display for PrivateName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "PN[{}]", self.description)
    }
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
        self.id.hash(state);
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
#[derive(Debug, Clone, PartialEq)]
pub enum PrivateElementKind {
    Field { value: RefCell<ECMAScriptValue> },
    Method { value: ECMAScriptValue },
    Accessor { get: Option<Object>, set: Option<Object> },
}
impl fmt::Display for PrivateElementKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrivateElementKind::Field { value } => write!(f, "Field({})", value.borrow()),
            PrivateElementKind::Method { value } => write!(f, "Method({value})"),
            PrivateElementKind::Accessor { get, set } => match (get.as_ref(), set.as_ref()) {
                (None, None) => write!(f, "Accessor(-,-)"),
                (None, Some(set)) => write!(
                    f,
                    "Accessor(-,{})",
                    set.get(&"name".into()).unwrap_or_else(|_| ECMAScriptValue::from("unnamed"))
                ),
                (Some(get), None) => write!(
                    f,
                    "Accessor({},-)",
                    get.get(&"name".into()).unwrap_or_else(|_| ECMAScriptValue::from("unnamed"))
                ),
                (Some(get), Some(set)) => write!(
                    f,
                    "Accessor({},{})",
                    get.get(&"name".into()).unwrap_or_else(|_| ECMAScriptValue::from("unnamed")),
                    set.get(&"name".into()).unwrap_or_else(|_| ECMAScriptValue::from("unnamed"))
                ),
            },
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct PrivateElement {
    pub key: PrivateName,
    pub kind: PrivateElementKind,
}
impl fmt::Display for PrivateElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "PrivateElement{{{}: {}}}", self.key, self.kind)
    }
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
            write!(writer, "{ch}")?;
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
            write!(writer, "{ch}")?;
        }
        write!(writer, ".")?;
        for _ in 0..k - n {
            let ch = iter.next().unwrap();
            write!(writer, "{ch}")?;
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
            write!(writer, "{ch}")?;
        }
        return Ok(());
    }
    let ch = iter.next().unwrap();
    write!(writer, "{ch}")?;
    if k > 1 {
        write!(writer, ".")?;
        for _ in 1..k {
            let ch = iter.next().unwrap();
            write!(writer, "{ch}")?;
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
pub fn ordinary_to_primitive(obj: &Object, hint: ConversionHint) -> Completion<ECMAScriptValue> {
    let method_names = match hint {
        ConversionHint::String => {
            vec![PropertyKey::from("toString"), PropertyKey::from("valueOf")]
        }
        ConversionHint::Number => {
            vec![PropertyKey::from("valueOf"), PropertyKey::from("toString")]
        }
    };
    for name in method_names.iter() {
        let method = obj.get(name)?;
        if is_callable(&method) {
            let result = call(&method, &ECMAScriptValue::from(obj), &[])?;
            if !result.is_object() {
                return Ok(result);
            }
        }
    }
    Err(create_type_error("Cannot convert object to primitive value"))
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
pub fn to_primitive(input: ECMAScriptValue, preferred_type: Option<ConversionHint>) -> Completion<ECMAScriptValue> {
    if let ECMAScriptValue::Object(obj) = &input {
        let exotic_to_prim = input.get_method(&PropertyKey::from(wks(WksId::ToPrimitive)))?;
        if !exotic_to_prim.is_undefined() {
            let hint = ECMAScriptValue::from(match preferred_type {
                None => "default",
                Some(ConversionHint::Number) => "number",
                Some(ConversionHint::String) => "string",
            });
            let result = call(&exotic_to_prim, &input, &[hint])?;
            if !result.is_object() {
                return Ok(result);
            }
            return Err(create_type_error("Cannot convert object to primitive value"));
        }
        let pt = preferred_type.unwrap_or(ConversionHint::Number);
        ordinary_to_primitive(obj, pt)
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
            ECMAScriptValue::Undefined | ECMAScriptValue::Null => false,
            ECMAScriptValue::Boolean(b) => b,
            ECMAScriptValue::Number(num) => !(num.is_nan() || num == 0.0),
            ECMAScriptValue::String(s) => !s.is_empty(),
            ECMAScriptValue::Symbol(_) | ECMAScriptValue::Object(_) => true,
            ECMAScriptValue::BigInt(b) => *b != BigInt::from(0),
        }
    }
}
pub fn to_boolean(val: impl Into<ECMAScriptValue>) -> bool {
    bool::from(val.into())
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
pub fn to_numeric(value: ECMAScriptValue) -> Completion<Numeric> {
    let prim_value = to_primitive(value, Some(ConversionHint::Number))?;
    if let ECMAScriptValue::BigInt(bi) = prim_value {
        Ok(Numeric::BigInt(bi))
    } else {
        Ok(Numeric::Number(to_number(prim_value)?))
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
pub fn to_number(value: impl Into<ECMAScriptValue>) -> Completion<f64> {
    match value.into() {
        ECMAScriptValue::Undefined => Ok(f64::NAN),
        ECMAScriptValue::Null => Ok(0_f64),
        ECMAScriptValue::Boolean(b) => Ok(if b { 1_f64 } else { 0_f64 }),
        ECMAScriptValue::Number(n) => Ok(n),
        ECMAScriptValue::String(s) => Ok(string_to_number(s)),
        ECMAScriptValue::BigInt(_) => Err(create_type_error("BigInt values cannot be converted to Number values")),
        ECMAScriptValue::Symbol(_) => Err(create_type_error("Symbol values cannot be converted to Number values")),
        ECMAScriptValue::Object(o) => {
            let prim_value = to_primitive(ECMAScriptValue::from(o), Some(ConversionHint::Number))?;
            to_number(prim_value)
        }
    }
}
pub fn to_number_agentless(value: impl Into<ECMAScriptValue>) -> anyhow::Result<f64> {
    match value.into() {
        ECMAScriptValue::Undefined => Ok(f64::NAN),
        ECMAScriptValue::Null => Ok(0_f64),
        ECMAScriptValue::Boolean(b) => Ok(if b { 1_f64 } else { 0_f64 }),
        ECMAScriptValue::Number(n) => Ok(n),
        ECMAScriptValue::String(s) => Ok(string_to_number(s)),
        ECMAScriptValue::BigInt(_) => Err(anyhow!("BigInt values cannot be converted to Number values")),
        ECMAScriptValue::Symbol(_) => Err(anyhow!("Symbol values cannot be converted to Number values")),
        ECMAScriptValue::Object(_) => Err(anyhow!("Number conversion from objects requires an agent")),
    }
}

fn string_to_number(string: JSString) -> f64 {
    lazy_static! {
        static ref STR_WHITE_SPACE: &'static str = r"(?:[\t\v\f \u{a0}\u{feff}\n\r\u{2028}\u{2029}]+)";
        static ref DECIMAL_DIGITS: &'static str = "(?:[0-9]+)";
        static ref EXPONENT_PART: &'static str = "(?:[eE][-+]?[0-9]+)";
        static ref STR_UNSIGNED_DECIMAL_LITERAL: String = format!(
            r"(?:Infinity|{}\.{}?{}?|\.{}{}?|{}{}?)",
            *DECIMAL_DIGITS,
            *DECIMAL_DIGITS,
            *EXPONENT_PART,
            *DECIMAL_DIGITS,
            *EXPONENT_PART,
            *DECIMAL_DIGITS,
            *EXPONENT_PART
        );
        static ref STR_DECIMAL_LITERAL: String = format!(r"(?P<decimal>[-+]?{})", *STR_UNSIGNED_DECIMAL_LITERAL);
        static ref BINARY_INTEGER_LITERAL: &'static str = "(?:0[bB](?P<binary>[01]+))";
        static ref OCTAL_INTEGER_LITERAL: &'static str = "(?:0[oO](?P<octal>[0-7]+))";
        static ref HEX_INTEGER_LITERAL: &'static str = "(?:0[xX](?P<hex>[0-9a-fA-F]+))";
        static ref NONDECIMAL_INTEGER_LITERAL: String =
            format!("(?:{}|{}|{})", *BINARY_INTEGER_LITERAL, *OCTAL_INTEGER_LITERAL, *HEX_INTEGER_LITERAL);
        static ref STR_NUMERIC_LITERAL: String =
            format!("(?:{}|{})", *STR_DECIMAL_LITERAL, *NONDECIMAL_INTEGER_LITERAL);
        static ref STRING_NUMERIC_LITERAL: String =
            format!("^(?:{}?|{}?{}{}?)$", *STR_WHITE_SPACE, *STR_WHITE_SPACE, *STR_NUMERIC_LITERAL, *STR_WHITE_SPACE);
        static ref MATCHER: Regex = Regex::new(&STRING_NUMERIC_LITERAL).unwrap();
    }

    let number_string = String::from(string);
    match MATCHER.captures(&number_string) {
        None => f64::NAN,
        Some(captures) => captures.name("decimal").map_or_else(
            || {
                captures.name("binary").map_or_else(
                    || {
                        captures.name("octal").map_or_else(
                            || {
                                captures.name("hex").map_or(0.0, |hex| {
                                    BigUint::from_str_radix(hex.as_str(), 16).unwrap().to_f64().unwrap()
                                })
                            },
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
pub fn to_integer_or_infinity(argument: impl Into<ECMAScriptValue>) -> Completion<f64> {
    let number = to_number(argument)?;
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

#[allow(clippy::cast_possible_truncation)]
#[allow(clippy::cast_precision_loss)]
pub fn to_usize(arg: f64) -> anyhow::Result<usize> {
    if arg.is_finite() && arg >= 0.0 && arg <= usize::MAX as f64 && arg.fract() == 0.0 {
        Ok(arg as usize)
    } else {
        Err(anyhow!("invalid conversion of {arg} to usize"))
    }
}

// ToInt32 ( argument )
//
// The abstract operation ToInt32 takes argument argument. It converts argument to one of 232 integral Number values in
// the range 𝔽(-2**31) through 𝔽(2**31 - 1), inclusive. It performs the following steps when called:
//
//  1. Let number be ? ToNumber(argument).
//  2. If number is NaN, +0𝔽, -0𝔽, +∞𝔽, or -∞𝔽, return +0𝔽.
//  3. Let int be the mathematical value whose sign is the sign of number and whose magnitude is floor(abs(ℝ(number))).
//  4. Let int32bit be int modulo 2**32.
//  5. If int32bit ≥ 2**31, return 𝔽(int32bit - 2**32); otherwise return 𝔽(int32bit).
//
// NOTE | Given the above definition of ToInt32:
//      |
//      | * The ToInt32 abstract operation is idempotent: if applied to a result that it produced, the second
//      |   application leaves that value unchanged.
//      | * ToInt32(ToUint32(x)) is the same value as ToInt32(x) for all values of x. (It is to preserve this latter
//      |   property that +∞𝔽 and -∞𝔽 are mapped to +0𝔽.)
//      | * ToInt32 maps -0𝔽 to +0𝔽.
fn to_core_int(modulo: f64, argument: impl Into<ECMAScriptValue>) -> Completion<f64> {
    Ok({
        let number = to_number(argument)?;
        if !number.is_finite() || number == 0.0 {
            0.0
        } else {
            let i = number.signum() * number.abs().floor();
            i % modulo
        }
    })
}
fn to_core_int_agentless(modulo: f64, argument: impl Into<ECMAScriptValue>) -> anyhow::Result<f64> {
    let number = to_number_agentless(argument)?;
    if !number.is_finite() || number == 0.0 {
        Ok(0.0)
    } else {
        let i = number.signum() * number.abs().floor();
        Ok(i % modulo)
    }
}
fn to_core_signed(modulo: f64, argument: impl Into<ECMAScriptValue>) -> Completion<f64> {
    Ok({
        let intval = to_core_int(modulo, argument)?;
        if intval >= modulo / 2.0 {
            intval - modulo
        } else {
            intval
        }
    })
}
#[allow(clippy::cast_possible_truncation)]
pub fn to_int32(argument: impl Into<ECMAScriptValue>) -> Completion<i32> {
    Ok(to_core_signed(4_294_967_296.0, argument)? as i32)
}

// ToUint32 ( argument )
//
// The abstract operation ToUint32 takes argument argument. It converts argument to one of 2**32 integral Number values
// in the range +0𝔽 through 𝔽(2**32 - 1), inclusive. It performs the following steps when called:
//
//  1. Let number be ? ToNumber(argument).
//  2. If number is NaN, +0𝔽, -0𝔽, +∞𝔽, or -∞𝔽, return +0𝔽.
//  3. Let int be the mathematical value whose sign is the sign of number and whose magnitude is floor(abs(ℝ(number))).
//  4. Let int32bit be int modulo 2**32.
//  5. Return 𝔽(int32bit).
//
// NOTE | Given the above definition of ToUint32:
//      |
//      | * Step 5 is the only difference between ToUint32 and ToInt32.
//      | * The ToUint32 abstract operation is idempotent: if applied to a result that it produced, the second
//      |   application leaves that value unchanged.
//      | * ToUint32(ToInt32(x)) is the same value as ToUint32(x) for all values of x. (It is to preserve this latter
//      |   property that +∞𝔽 and -∞𝔽 are mapped to +0𝔽.)
//      | * ToUint32 maps -0𝔽 to +0𝔽.
#[allow(clippy::cast_possible_truncation)]
pub fn to_uint32(argument: impl Into<ECMAScriptValue>) -> Completion<u32> {
    let i = to_core_int(4_294_967_296.0, argument)? as i64;
    Ok((if i < 0 { i + 4_294_967_296 } else { i }).try_into().expect("Math results in in-bounds calculation"))
}
#[allow(clippy::cast_possible_truncation)]
pub fn to_uint32_agentless(argument: impl Into<ECMAScriptValue>) -> anyhow::Result<u32> {
    let i = to_core_int_agentless(4_294_967_296.0, argument)? as i64;
    Ok((if i < 0 { i + 4_294_967_296 } else { i }).try_into().expect("Math results in in-bounds calculation"))
}

// ToInt16 ( argument )
//
// The abstract operation ToInt16 takes argument argument. It converts argument to one of 2**16 integral Number values in
// the range 𝔽(-2**15) through 𝔽(2**15 - 1), inclusive. It performs the following steps when called:
//
//  1. Let number be ? ToNumber(argument).
//  2. If number is NaN, +0𝔽, -0𝔽, +∞𝔽, or -∞𝔽, return +0𝔽.
//  3. Let int be the mathematical value whose sign is the sign of number and whose magnitude is floor(abs(ℝ(number))).
//  4. Let int16bit be int modulo 2**16.
//  5. If int16bit ≥ 2**15, return 𝔽(int16bit - 2**16); otherwise return 𝔽(int16bit).
#[allow(clippy::cast_possible_truncation)]
pub fn to_int16(argument: impl Into<ECMAScriptValue>) -> Completion<i16> {
    Ok(to_core_signed(65536.0, argument)? as i16)
}

// ToUint16 ( argument )
//
// The abstract operation ToUint16 takes argument argument. It converts argument to one of 2**16 integral Number values
// in the range +0𝔽 through 𝔽(2**16 - 1), inclusive. It performs the following steps when called:
//
//  1. Let number be ? ToNumber(argument).
//  2. If number is NaN, +0𝔽, -0𝔽, +∞𝔽, or -∞𝔽, return +0𝔽.
//  3. Let int be the mathematical value whose sign is the sign of number and whose magnitude is floor(abs(ℝ(number))).
//  4. Let int16bit be int modulo 2**16.
//  5. Return 𝔽(int16bit).
//
// NOTE | Given the above definition of ToUint16:
//      |
//      | * The substitution of 2**16 for 2**32 in step 4 is the only difference between ToUint32 and ToUint16.
//      | * ToUint16 maps -0𝔽 to +0𝔽.
#[allow(clippy::cast_possible_truncation)]
pub fn to_uint16(argument: impl Into<ECMAScriptValue>) -> Completion<u16> {
    let i = to_core_int(65536.0, argument)? as i64;
    Ok((if i < 0 { i + 65536 } else { i }).try_into().expect("Math results in in-bounds calculation"))
}

// ToInt8 ( argument )
//
// The abstract operation ToInt8 takes argument argument. It converts argument to one of 2**8 integral Number values in
// the range -128𝔽 through 127𝔽, inclusive. It performs the following steps when called:
//
//  1. Let number be ? ToNumber(argument).
//  2. If number is NaN, +0𝔽, -0𝔽, +∞𝔽, or -∞𝔽, return +0𝔽.
//  3. Let int be the mathematical value whose sign is the sign of number and whose magnitude is floor(abs(ℝ(number))).
//  4. Let int8bit be int modulo 2**8.
//  5. If int8bit ≥ 2**7, return 𝔽(int8bit - 2**8); otherwise return 𝔽(int8bit).
#[allow(clippy::cast_possible_truncation)]
pub fn to_int8(argument: impl Into<ECMAScriptValue>) -> Completion<i8> {
    Ok(to_core_signed(256.0, argument)? as i8)
}

// ToUint8 ( argument )
//
// The abstract operation ToUint8 takes argument argument. It converts argument to one of 2**8 integral Number values
// in the range +0𝔽 through 255𝔽, inclusive. It performs the following steps when called:
//
//  1. Let number be ? ToNumber(argument).
//  2. If number is NaN, +0𝔽, -0𝔽, +∞𝔽, or -∞𝔽, return +0𝔽.
//  3. Let int be the mathematical value whose sign is the sign of number and whose magnitude is floor(abs(ℝ(number))).
//  4. Let int8bit be int modulo 2**8.
//  5. Return 𝔽(int8bit).
#[allow(clippy::cast_possible_truncation)]
pub fn to_uint8(argument: impl Into<ECMAScriptValue>) -> Completion<u8> {
    let i = to_core_int(256.0, argument)? as i64;
    Ok((if i < 0 { i + 256 } else { i }).try_into().expect("Math results in in-bounds calculation"))
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
pub fn to_string(val: impl Into<ECMAScriptValue>) -> Completion<JSString> {
    let val = val.into();
    if val.is_object() {
        let prim_value = to_primitive(val, Some(ConversionHint::String))?;
        to_string(prim_value)
    } else {
        JSString::try_from(val).map_err(|e| create_type_error(e.to_string()))
    }
}

pub fn to_string_agentless(val: impl Into<ECMAScriptValue>) -> anyhow::Result<JSString> {
    JSString::try_from(val.into())
}

impl TryFrom<ECMAScriptValue> for JSString {
    type Error = anyhow::Error;
    fn try_from(val: ECMAScriptValue) -> Result<Self, Self::Error> {
        match val {
            ECMAScriptValue::Undefined => Ok(JSString::from("undefined")),
            ECMAScriptValue::Null => Ok(JSString::from("null")),
            ECMAScriptValue::Boolean(b) => Ok(JSString::from(if b { "true" } else { "false" })),
            ECMAScriptValue::String(s) => Ok(s),
            ECMAScriptValue::Number(n) => {
                let mut s = Vec::new();
                number_to_string(&mut s, n).unwrap();
                Ok(JSString::from(s))
            }
            ECMAScriptValue::BigInt(bi) => Ok(JSString::from(format!("{bi}"))),
            ECMAScriptValue::Symbol(_) => Err(anyhow!("Symbols may not be converted to strings")),
            ECMAScriptValue::Object(_) => Err(anyhow!("Object to string conversions require an agent")),
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
pub fn to_object(val: impl Into<ECMAScriptValue>) -> Completion<Object> {
    match val.into() {
        ECMAScriptValue::Null | ECMAScriptValue::Undefined => {
            Err(create_type_error("Undefined and null cannot be converted to objects"))
        }
        ECMAScriptValue::Boolean(b) => Ok(create_boolean_object(b)),
        ECMAScriptValue::Number(n) => Ok(create_number_object(n)),
        ECMAScriptValue::String(s) => Ok(create_string_object(s)),
        ECMAScriptValue::Symbol(s) => Ok(create_symbol_object(s)),
        ECMAScriptValue::BigInt(b) => Ok(create_bigint_object(b)),
        ECMAScriptValue::Object(o) => Ok(o),
    }
}

// ToPropertyKey ( argument )
//
// The abstract operation ToPropertyKey takes argument argument. It converts argument to a value that can be used as a
// property key. It performs the following steps when called:
//
//  1. Let key be ? ToPrimitive(argument, string).
//  2. If Type(key) is Symbol, then
//      a. Return key.
//  3. Return ! ToString(key).
pub fn to_property_key(argument: ECMAScriptValue) -> Completion<PropertyKey> {
    let key = to_primitive(argument, Some(ConversionHint::String))?;
    match key {
        ECMAScriptValue::Symbol(sym) => Ok(PropertyKey::from(sym)),
        _ => Ok(PropertyKey::from(to_string(key).unwrap())),
    }
}

// ToLength ( argument )
//
// The abstract operation ToLength takes argument argument (an ECMAScript language value). It clamps argument to an
// integral Number suitable for use as the length of an array-like object. It performs the following steps when called:
//
//  1. Let len be ? ToIntegerOrInfinity(argument).
//  2. If len ≤ 0, return +0𝔽.
//  3. Return 𝔽(min(len, 2**53 - 1)).
#[allow(clippy::cast_possible_truncation)]
pub fn to_length(argument: impl Into<ECMAScriptValue>) -> Completion<i64> {
    let len = to_integer_or_infinity(argument)?;
    Ok(len.clamp(0.0, 9_007_199_254_740_991.0) as i64)
}

// CanonicalNumericIndexString ( argument )
//
// The abstract operation CanonicalNumericIndexString takes argument argument (a String). It returns argument converted
// to a Number value if it is a String representation of a Number that would be produced by ToString, or the string
// "-0". Otherwise, it returns undefined. It performs the following steps when called:
//
//  1. If argument is "-0", return -0𝔽.
//  2. Let n be ! ToNumber(argument).
//  3. If SameValue(! ToString(n), argument) is false, return undefined.
//  4. Return n.
//
// A canonical numeric string is any String value for which the CanonicalNumericIndexString abstract operation does not
// return undefined.
pub fn canonical_numeric_index_string(argument: JSString) -> Option<f64> {
    if argument == "-0" {
        Some(-0.0)
    } else {
        let n = to_number_agentless(argument.clone()).unwrap();
        if argument == to_string_agentless(n).unwrap() {
            Some(n)
        } else {
            None
        }
    }
}

// ToIndex ( value )
//
// The abstract operation ToIndex takes argument value (an ECMAScript language value). It converts value to a
// non-negative integer if the corresponding decimal representation, as a String, is an integer index. It performs the
// following steps when called:
//
//  1. If value is undefined, then
//      a. Return 0.
//  2. Else,
//      a. Let integer be ? ToIntegerOrInfinity(value).
//      b. Let clamped be ! ToLength(𝔽(integer)).
//      c. If ! SameValue(𝔽(integer), clamped) is false, throw a RangeError exception.
//      d. Assert: 0 ≤ integer ≤ 2**53 - 1.
//      e. Return integer.
pub fn to_index(value: impl Into<ECMAScriptValue>) -> Completion<i64> {
    let value = value.into();
    if value == ECMAScriptValue::Undefined {
        Ok(0)
    } else {
        let integer = to_integer_or_infinity(value)?;
        let clamped = to_length(integer).unwrap();
        #[allow(clippy::cast_precision_loss)]
        if clamped as f64 != integer {
            Err(create_range_error(format!("{integer} out of range for index").as_str()))
        } else {
            Ok(clamped)
        }
    }
}

pub fn number_same_value(x: f64, y: f64) -> bool {
    (x.is_nan() && y.is_nan()) || (x == y && x.signum() == y.signum())
}
pub fn number_same_value_zero(x: f64, y: f64) -> bool {
    (x.is_nan() && y.is_nan()) || (x == y)
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

// IsConstructor ( argument )
//
// The abstract operation IsConstructor takes argument argument (an ECMAScript language value). It determines if
// argument is a function object with a [[Construct]] internal method. It performs the following steps when called:
//
//  1. If Type(argument) is not Object, return false.
//  2. If argument has a [[Construct]] internal method, return true.
//  3. Return false.
pub fn is_constructor(value: &ECMAScriptValue) -> bool {
    to_constructor(value).is_some()
}

impl ECMAScriptValue {
    #[inline]
    pub fn same_value_non_numeric(&self, other: &ECMAScriptValue) -> bool {
        match (self, other) {
            (ECMAScriptValue::Undefined, ECMAScriptValue::Undefined)
            | (ECMAScriptValue::Null, ECMAScriptValue::Null) => true,
            (ECMAScriptValue::String(a), ECMAScriptValue::String(b)) => a == b,
            (ECMAScriptValue::Boolean(a), ECMAScriptValue::Boolean(b)) => a == b,
            (ECMAScriptValue::Symbol(a), ECMAScriptValue::Symbol(b)) => a == b,
            (ECMAScriptValue::Object(a), ECMAScriptValue::Object(b)) => a == b,
            _ => panic!("Invalid input args"),
        }
    }

    pub fn same_value_zero(&self, other: &ECMAScriptValue) -> bool {
        match (self, other) {
            (ECMAScriptValue::Number(a), ECMAScriptValue::Number(b)) => number_same_value_zero(*a, *b),
            (ECMAScriptValue::BigInt(a), ECMAScriptValue::BigInt(b)) => a == b,
            (ECMAScriptValue::Undefined, ECMAScriptValue::Undefined)
            | (ECMAScriptValue::Null, ECMAScriptValue::Null)
            | (ECMAScriptValue::String(_), ECMAScriptValue::String(_))
            | (ECMAScriptValue::Boolean(_), ECMAScriptValue::Boolean(_))
            | (ECMAScriptValue::Symbol(_), ECMAScriptValue::Symbol(_))
            | (ECMAScriptValue::Object(_), ECMAScriptValue::Object(_)) => self.same_value_non_numeric(other),
            _ => false,
        }
    }

    pub fn same_value(&self, other: &ECMAScriptValue) -> bool {
        match (self, other) {
            (ECMAScriptValue::Number(a), ECMAScriptValue::Number(b)) => number_same_value(*a, *b),
            (ECMAScriptValue::BigInt(a), ECMAScriptValue::BigInt(b)) => a == b,
            (ECMAScriptValue::Undefined, ECMAScriptValue::Undefined)
            | (ECMAScriptValue::Null, ECMAScriptValue::Null)
            | (ECMAScriptValue::String(_), ECMAScriptValue::String(_))
            | (ECMAScriptValue::Boolean(_), ECMAScriptValue::Boolean(_))
            | (ECMAScriptValue::Symbol(_), ECMAScriptValue::Symbol(_))
            | (ECMAScriptValue::Object(_), ECMAScriptValue::Object(_)) => self.same_value_non_numeric(other),
            _ => false,
        }
    }

    pub fn is_strictly_equal(&self, other: &ECMAScriptValue) -> bool {
        match (self, other) {
            (&ECMAScriptValue::Number(x), &ECMAScriptValue::Number(y)) => x == y,
            (ECMAScriptValue::BigInt(x), ECMAScriptValue::BigInt(y)) => x == y,
            (ECMAScriptValue::Undefined, ECMAScriptValue::Undefined)
            | (ECMAScriptValue::Null, ECMAScriptValue::Null)
            | (ECMAScriptValue::Boolean(_), ECMAScriptValue::Boolean(_))
            | (ECMAScriptValue::String(_), ECMAScriptValue::String(_))
            | (ECMAScriptValue::Symbol(_), ECMAScriptValue::Symbol(_))
            | (ECMAScriptValue::Object(_), ECMAScriptValue::Object(_)) => self.same_value_non_numeric(other),
            _ => false,
        }
    }

    pub fn kind(&self) -> ValueKind {
        match self {
            ECMAScriptValue::Undefined => ValueKind::Undefined,
            ECMAScriptValue::Null => ValueKind::Null,
            ECMAScriptValue::Boolean(_) => ValueKind::Boolean,
            ECMAScriptValue::String(_) => ValueKind::String,
            ECMAScriptValue::Number(_) => ValueKind::Number,
            ECMAScriptValue::BigInt(_) => ValueKind::BigInt,
            ECMAScriptValue::Symbol(_) => ValueKind::Symbol,
            ECMAScriptValue::Object(_) => ValueKind::Object,
        }
    }
}

pub fn is_loosely_equal(x: &ECMAScriptValue, y: &ECMAScriptValue) -> Completion<bool> {
    match (x, y) {
        (ECMAScriptValue::Number(_), ECMAScriptValue::Number(_))
        | (ECMAScriptValue::BigInt(_), ECMAScriptValue::BigInt(_))
        | (ECMAScriptValue::Undefined, ECMAScriptValue::Undefined)
        | (ECMAScriptValue::Null, ECMAScriptValue::Null)
        | (ECMAScriptValue::String(_), ECMAScriptValue::String(_))
        | (ECMAScriptValue::Boolean(_), ECMAScriptValue::Boolean(_))
        | (ECMAScriptValue::Symbol(_), ECMAScriptValue::Symbol(_))
        | (ECMAScriptValue::Object(_), ECMAScriptValue::Object(_)) => Ok(x.is_strictly_equal(y)),
        (ECMAScriptValue::Undefined, ECMAScriptValue::Null) | (ECMAScriptValue::Null, ECMAScriptValue::Undefined) => {
            Ok(true)
        }
        (ECMAScriptValue::Number(_), ECMAScriptValue::String(y)) => {
            let new_y = ECMAScriptValue::from(to_number(y).expect("Strings are always convertable to numbers"));
            is_loosely_equal(x, &new_y)
        }
        (ECMAScriptValue::String(x), ECMAScriptValue::Number(_)) => {
            let new_x = ECMAScriptValue::from(to_number(x).expect("Strings are always convertable to numbers"));
            is_loosely_equal(&new_x, y)
        }
        (ECMAScriptValue::BigInt(_), ECMAScriptValue::String(y)) => {
            let n = String::from(y).parse::<BigInt>();
            match n {
                Err(_) => Ok(false),
                Ok(bi) => is_loosely_equal(x, &bi.into()),
            }
        }
        (ECMAScriptValue::String(_), ECMAScriptValue::BigInt(_)) => is_loosely_equal(y, x),
        (ECMAScriptValue::Boolean(_), _) => {
            let new_x =
                ECMAScriptValue::from(to_number(x.clone()).expect("Booleans are always convertable to numbers"));
            is_loosely_equal(&new_x, y)
        }
        (_, ECMAScriptValue::Boolean(_)) => {
            let new_y =
                ECMAScriptValue::from(to_number(y.clone()).expect("Booleans are always convertable to numbers"));
            is_loosely_equal(x, &new_y)
        }
        (
            ECMAScriptValue::String(_)
            | ECMAScriptValue::Number(_)
            | ECMAScriptValue::BigInt(_)
            | ECMAScriptValue::Symbol(_),
            ECMAScriptValue::Object(_),
        ) => {
            let new_y = to_primitive(y.clone(), None)?;
            is_loosely_equal(x, &new_y)
        }
        (
            ECMAScriptValue::Object(_),
            ECMAScriptValue::String(_)
            | ECMAScriptValue::Number(_)
            | ECMAScriptValue::BigInt(_)
            | ECMAScriptValue::Symbol(_),
        ) => {
            let new_x = to_primitive(x.clone(), None)?;
            is_loosely_equal(&new_x, y)
        }
        (&ECMAScriptValue::Number(n), ECMAScriptValue::BigInt(b))
        | (ECMAScriptValue::BigInt(b), &ECMAScriptValue::Number(n)) => {
            Ok(n.is_finite() && n == b.to_f64().expect("BigInts always transform to floats ok"))
        }
        (ECMAScriptValue::Undefined | ECMAScriptValue::Null | ECMAScriptValue::Symbol(_), _)
        | (_, ECMAScriptValue::Undefined | ECMAScriptValue::Null | ECMAScriptValue::Symbol(_)) => Ok(false),
    }
}

pub fn exponentiate(base: f64, exponent: f64) -> f64 {
    // Number::exponentiate ( base, exponent )
    // The abstract operation Number::exponentiate takes arguments base (a Number) and exponent (a Number) and returns a
    // Number. It returns an implementation-approximated value representing the result of raising base to the exponent
    // power. It performs the following steps when called:
    //
    //  1. If exponent is NaN, return NaN.
    //  2. If exponent is either +0𝔽 or -0𝔽, return 1𝔽.
    //  3. If base is NaN, return NaN.
    //  4. If base is +∞𝔽, then
    //      a. If exponent > +0𝔽, return +∞𝔽. Otherwise, return +0𝔽.
    //  5. If base is -∞𝔽, then
    //      a. If exponent > +0𝔽, then
    //          i. If exponent is an odd integral Number, return -∞𝔽. Otherwise, return +∞𝔽.
    //      b. Else,
    //          i. If exponent is an odd integral Number, return -0𝔽. Otherwise, return +0𝔽.
    //  6. If base is +0𝔽, then
    //      a. If exponent > +0𝔽, return +0𝔽. Otherwise, return +∞𝔽.
    //  7. If base is -0𝔽, then
    //      a. If exponent > +0𝔽, then
    //          i. If exponent is an odd integral Number, return -0𝔽. Otherwise, return +0𝔽.
    //      b. Else,
    //          i. If exponent is an odd integral Number, return -∞𝔽. Otherwise, return +∞𝔽.
    //  8. Assert: base is finite and is neither +0𝔽 nor -0𝔽.
    //  9. If exponent is +∞𝔽, then
    //      a. If abs(ℝ(base)) > 1, return +∞𝔽.
    //      b. If abs(ℝ(base)) = 1, return NaN.
    //      c. If abs(ℝ(base)) < 1, return +0𝔽.
    //  10. If exponent is -∞𝔽, then
    //      a. If abs(ℝ(base)) > 1, return +0𝔽.
    //      b. If abs(ℝ(base)) = 1, return NaN.
    //      c. If abs(ℝ(base)) < 1, return +∞𝔽.
    //  11. Assert: exponent is finite and is neither +0𝔽 nor -0𝔽.
    //  12. If base < -0𝔽 and exponent is not an integral Number, return NaN.
    //  13. Return an implementation-approximated Number value representing the result of raising ℝ(base) to the
    //      ℝ(exponent) power.
    // NOTE The result of base ** exponent when base is 1𝔽 or -1𝔽 and exponent is +∞𝔽 or -∞𝔽, or when base is 1𝔽
    // and exponent is NaN, differs from IEEE 754-2019. The first edition of ECMAScript specified a result of NaN for
    // this operation, whereas later revisions of IEEE 754 specified 1𝔽. The historical ECMAScript behaviour is
    // preserved for compatibility reasons.
    if exponent.is_nan() {
        return f64::NAN;
    }
    if exponent == 0.0 {
        return 1.0;
    }
    if base.is_nan() {
        return f64::NAN;
    }
    if base.is_infinite() || base == 0.0 || exponent.is_finite() {
        return base.powf(exponent);
    }

    let b = base.abs();
    if exponent > 0.0 {
        if b > 1.0 {
            f64::INFINITY
        } else if b < 1.0 {
            0.0
        } else {
            f64::NAN
        }
    } else if b > 1.0 {
        0.0
    } else if b < 1.0 {
        f64::INFINITY
    } else {
        f64::NAN
    }
}

#[cfg(test)]
pub mod tests;
