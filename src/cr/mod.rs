use super::*;
use anyhow::{anyhow, bail};
use std::fmt;
use std::ptr::addr_of;

#[derive(Clone, Debug)]
pub enum NormalCompletion {
    Empty,
    Value(ECMAScriptValue),
    Reference(Box<Reference>),
    Environment(Rc<dyn EnvironmentRecord>),
    IteratorRecord(Rc<IteratorRecord>),
    PrivateName(PrivateName),
    PrivateElement(Box<PrivateElement>),
    ClassItem(Box<ClassItem>),
}
impl PartialEq for NormalCompletion {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Value(left), Self::Value(right)) => left == right,
            (Self::Reference(left), Self::Reference(right)) => left == right,
            (Self::Environment(left), Self::Environment(right)) => {
                //Rc::ptr_eq(left, right) <<-- Can't do this because fat pointers aren't comparable. Convert to thin pointers to the allocated memory instead.
                let left = addr_of!(**left).cast::<u8>();
                let right = addr_of!(**right).cast::<u8>();
                std::ptr::eq(left, right)
            }
            (Self::Empty, Self::Empty) => true,
            (Self::IteratorRecord(left), Self::IteratorRecord(right)) => Rc::ptr_eq(left, right),
            (Self::PrivateName(left), Self::PrivateName(right)) => left == right,
            (Self::PrivateElement(left), Self::PrivateElement(right)) => *left == *right,
            (Self::ClassItem(left), Self::ClassItem(right)) => *left == *right,
            (
                Self::Value(_)
                | Self::Reference(_)
                | Self::Environment(_)
                | Self::Empty
                | Self::IteratorRecord(_)
                | Self::PrivateName(_)
                | Self::PrivateElement(_)
                | Self::ClassItem(_),
                _,
            ) => false,
        }
    }
}
impl fmt::Display for NormalCompletion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NormalCompletion::Empty => write!(f, "[empty]"),
            NormalCompletion::Value(v) => v.concise(f),
            NormalCompletion::Reference(r) => {
                write!(f, "{}Ref({}->{})", if r.strict { "S" } else { "" }, r.base, r.referenced_name)
            }
            NormalCompletion::Environment(x) => write!(f, "{x:?}"),
            NormalCompletion::IteratorRecord(ir) => f.write_str(&ir.concise()),
            NormalCompletion::PrivateName(pn) => write!(f, "{pn}"),
            NormalCompletion::PrivateElement(pe) => write!(f, "{pe}"),
            NormalCompletion::ClassItem(ci) => write!(f, "{ci}"),
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum AbruptCompletion {
    Break { value: NormalCompletion, target: Option<JSString> },
    Continue { value: NormalCompletion, target: Option<JSString> },
    Return { value: ECMAScriptValue },
    Throw { value: ECMAScriptValue },
}

impl fmt::Display for AbruptCompletion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AbruptCompletion::Break { value: NormalCompletion::Empty, target: None } => write!(f, "Break{{}}"),
            AbruptCompletion::Break { value: v, target: None } => {
                write!(f, "Break{{V:{v}}}")
            }
            AbruptCompletion::Break { value: NormalCompletion::Empty, target: Some(t) } => write!(f, "Break{{T:{t}}}"),
            AbruptCompletion::Break { value: v, target: Some(t) } => {
                write!(f, "Break{{V:{v},T:{t}}}")
            }
            AbruptCompletion::Continue { value: NormalCompletion::Empty, target: None } => write!(f, "Continue{{}}"),
            AbruptCompletion::Continue { value: v, target: None } => write!(f, "Continue{{V:{v}}}"),
            AbruptCompletion::Continue { value: NormalCompletion::Empty, target: Some(t) } => {
                write!(f, "Continue{{T:{t}}}")
            }
            AbruptCompletion::Continue { value: v, target: Some(t) } => write!(f, "Continue{{V:{v},T:{t}}}"),
            AbruptCompletion::Return { value } => write!(f, "Return{{{value}}}"),
            AbruptCompletion::Throw { value } => write!(f, "Throw{{{value}}}"),
        }
    }
}
impl From<AbruptCompletion> for String {
    fn from(value: AbruptCompletion) -> Self {
        unwind_any_error(value)
    }
}

pub type Completion<T> = Result<T, AbruptCompletion>;
pub type FullCompletion = Completion<NormalCompletion>;

impl<T> From<T> for NormalCompletion
where
    T: Into<ECMAScriptValue>,
{
    fn from(src: T) -> Self {
        Self::Value(src.into())
    }
}

impl From<Reference> for NormalCompletion {
    fn from(src: Reference) -> Self {
        Self::Reference(Box::new(src))
    }
}

impl From<()> for NormalCompletion {
    fn from((): ()) -> Self {
        Self::Empty
    }
}

impl From<IteratorRecord> for NormalCompletion {
    fn from(value: IteratorRecord) -> Self {
        Self::IteratorRecord(Rc::new(value))
    }
}

impl From<Rc<IteratorRecord>> for NormalCompletion {
    fn from(value: Rc<IteratorRecord>) -> Self {
        Self::IteratorRecord(value)
    }
}

impl From<PrivateName> for NormalCompletion {
    fn from(value: PrivateName) -> Self {
        Self::PrivateName(value)
    }
}

impl From<PrivateElement> for NormalCompletion {
    fn from(value: PrivateElement) -> Self {
        Self::PrivateElement(Box::new(value))
    }
}

impl From<Option<PrivateElement>> for NormalCompletion {
    fn from(value: Option<PrivateElement>) -> Self {
        match value {
            Some(pe) => Self::from(pe),
            None => Self::Empty,
        }
    }
}

impl TryFrom<NormalCompletion> for ECMAScriptValue {
    type Error = anyhow::Error;
    fn try_from(src: NormalCompletion) -> anyhow::Result<Self> {
        match src {
            NormalCompletion::Value(v) => Ok(v),
            NormalCompletion::IteratorRecord(_)
            | NormalCompletion::Reference(_)
            | NormalCompletion::Empty
            | NormalCompletion::Environment(..)
            | NormalCompletion::PrivateName(_)
            | NormalCompletion::PrivateElement(_)
            | NormalCompletion::ClassItem(_) => Err(anyhow!("Not a language value!")),
        }
    }
}

impl TryFrom<NormalCompletion> for Option<ECMAScriptValue> {
    type Error = anyhow::Error;
    fn try_from(src: NormalCompletion) -> anyhow::Result<Self> {
        match src {
            NormalCompletion::Value(v) => Ok(Some(v)),
            NormalCompletion::Empty => Ok(None),
            NormalCompletion::IteratorRecord(_)
            | NormalCompletion::Reference(_)
            | NormalCompletion::Environment(_)
            | NormalCompletion::PrivateName(_)
            | NormalCompletion::PrivateElement(_)
            | NormalCompletion::ClassItem(_) => Err(anyhow!("Not a language value!")),
        }
    }
}

impl TryFrom<NormalCompletion> for Object {
    type Error = anyhow::Error;
    fn try_from(src: NormalCompletion) -> anyhow::Result<Self> {
        let v: ECMAScriptValue = src.try_into()?;
        let o: Object = v.try_into()?;
        Ok(o)
    }
}

impl TryFrom<NormalCompletion> for PropertyKey {
    type Error = anyhow::Error;
    fn try_from(src: NormalCompletion) -> anyhow::Result<Self> {
        let v: ECMAScriptValue = src.try_into()?;
        let key: PropertyKey = v.try_into()?;
        Ok(key)
    }
}

impl TryFrom<NormalCompletion> for Numeric {
    type Error = anyhow::Error;
    fn try_from(src: NormalCompletion) -> anyhow::Result<Self> {
        let v: ECMAScriptValue = src.try_into()?;
        let numeric: Numeric = v.try_into()?;
        Ok(numeric)
    }
}

impl TryFrom<NormalCompletion> for Rc<dyn EnvironmentRecord> {
    type Error = anyhow::Error;
    fn try_from(value: NormalCompletion) -> Result<Self, Self::Error> {
        match value {
            NormalCompletion::Environment(cd) => Ok(cd),
            _ => Err(anyhow!("Not environment data")),
        }
    }
}

impl TryFrom<NormalCompletion> for Rc<IteratorRecord> {
    type Error = anyhow::Error;

    fn try_from(value: NormalCompletion) -> Result<Self, Self::Error> {
        match value {
            NormalCompletion::IteratorRecord(ir) => Ok(ir),
            _ => Err(anyhow!("Not an iterator record")),
        }
    }
}

impl TryFrom<NormalCompletion> for PrivateName {
    type Error = anyhow::Error;

    fn try_from(value: NormalCompletion) -> Result<Self, Self::Error> {
        match value {
            NormalCompletion::PrivateName(pn) => Ok(pn),
            _ => Err(anyhow!("Not a private name")),
        }
    }
}

impl TryFrom<NormalCompletion> for Box<Reference> {
    type Error = anyhow::Error;

    fn try_from(value: NormalCompletion) -> Result<Self, Self::Error> {
        match value {
            NormalCompletion::Reference(r) => Ok(r),
            _ => Err(anyhow!("reference expected")),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ThrowValue(ECMAScriptValue);
impl TryFrom<AbruptCompletion> for ThrowValue {
    type Error = anyhow::Error;
    fn try_from(value: AbruptCompletion) -> Result<Self, Self::Error> {
        match value {
            AbruptCompletion::Break { .. } => bail!("Break found when Throw expected"),
            AbruptCompletion::Continue { .. } => bail!("Continue found when Throw expected"),
            AbruptCompletion::Return { .. } => bail!("Return found when Throw expected"),
            AbruptCompletion::Throw { value } => Ok(Self(value)),
        }
    }
}
impl From<ThrowValue> for ECMAScriptValue {
    fn from(tv: ThrowValue) -> Self {
        tv.0
    }
}

pub fn update_empty(completion_record: FullCompletion, old_value: NormalCompletion) -> FullCompletion {
    match completion_record {
        Ok(NormalCompletion::Empty) => Ok(old_value),
        Err(AbruptCompletion::Break { value: NormalCompletion::Empty, target }) => {
            Err(AbruptCompletion::Break { value: old_value, target })
        }
        Err(AbruptCompletion::Continue { value: NormalCompletion::Empty, target }) => {
            Err(AbruptCompletion::Continue { value: old_value, target })
        }
        Ok(_)
        | Err(
            AbruptCompletion::Return { .. }
            | AbruptCompletion::Throw { .. }
            | AbruptCompletion::Break { .. }
            | AbruptCompletion::Continue { .. },
        ) => completion_record,
    }
}

#[cfg(test)]
mod tests;
