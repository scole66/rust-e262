use crate::object::Object;
use crate::reference::Reference;
use crate::strings::JSString;
use crate::values::{ECMAScriptValue, Numeric, PropertyKey};
use anyhow::anyhow;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum NormalCompletion {
    Empty,
    Value(ECMAScriptValue),
    Reference(Box<Reference>),
}
impl fmt::Display for NormalCompletion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NormalCompletion::Empty => write!(f, "[empty]"),
            NormalCompletion::Value(v) => v.concise(f),
            NormalCompletion::Reference(r) => {
                write!(f, "{}Ref({}->{})", if r.strict { "S" } else { "" }, r.base, r.referenced_name)
            }
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
            AbruptCompletion::Continue { value: v, target: None } => {
                write!(f, "Continue{{V:{v}}}")
            }
            AbruptCompletion::Continue { value: NormalCompletion::Empty, target: Some(t) } => {
                write!(f, "Continue{{T:{t}}}")
            }
            AbruptCompletion::Continue { value: v, target: Some(t) } => {
                write!(f, "Continue{{V:{v},T:{t}}}")
            }
            AbruptCompletion::Return { value } => write!(f, "Return{{{value}}}"),
            AbruptCompletion::Throw { value } => write!(f, "Throw{{{value}}}"),
        }
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
    fn from(_: ()) -> Self {
        Self::Empty
    }
}

impl TryFrom<NormalCompletion> for ECMAScriptValue {
    type Error = anyhow::Error;
    fn try_from(src: NormalCompletion) -> anyhow::Result<Self> {
        match src {
            NormalCompletion::Value(v) => Ok(v),
            NormalCompletion::Reference(_) | NormalCompletion::Empty => Err(anyhow!("Not a language value!")),
        }
    }
}

impl TryFrom<NormalCompletion> for Option<ECMAScriptValue> {
    type Error = anyhow::Error;
    fn try_from(src: NormalCompletion) -> anyhow::Result<Self> {
        match src {
            NormalCompletion::Value(v) => Ok(Some(v)),
            NormalCompletion::Empty => Ok(None),
            NormalCompletion::Reference(_) => Err(anyhow!("Not a language value!")),
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
        | Err(AbruptCompletion::Return { .. })
        | Err(AbruptCompletion::Throw { .. })
        | Err(AbruptCompletion::Break { .. })
        | Err(AbruptCompletion::Continue { .. }) => completion_record,
    }
}

#[cfg(test)]
mod tests;
