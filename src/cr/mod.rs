use super::*;
use anyhow::{anyhow, bail};
use std::fmt;

#[derive(Clone, Debug)]
pub enum NormalCompletion {
    Empty,
    Value(ECMAScriptValue),
    Reference(Box<Reference>),
    Environment(Rc<dyn EnvironmentRecord>),
}
impl PartialEq for NormalCompletion {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Value(left), Self::Value(right)) => left == right,
            (Self::Reference(left), Self::Reference(right)) => left == right,
            (Self::Environment(left), Self::Environment(right)) => {
                //Rc::ptr_eq(left, right) <<-- Can't do this because fat pointers aren't comparable. Convert to thin pointers to the allocated memory instead.
                let left = &**left as *const dyn EnvironmentRecord as *const u8;
                let right = &**right as *const dyn EnvironmentRecord as *const u8;
                std::ptr::eq(left, right)
            }
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
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
            NormalCompletion::Environment(x) => write!(f, "{:?}", x),
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
            NormalCompletion::Reference(_) | NormalCompletion::Empty | NormalCompletion::Environment(..) => {
                Err(anyhow!("Not a language value!"))
            }
        }
    }
}

impl TryFrom<NormalCompletion> for Option<ECMAScriptValue> {
    type Error = anyhow::Error;
    fn try_from(src: NormalCompletion) -> anyhow::Result<Self> {
        match src {
            NormalCompletion::Value(v) => Ok(Some(v)),
            NormalCompletion::Empty => Ok(None),
            NormalCompletion::Reference(_) | NormalCompletion::Environment(_) => Err(anyhow!("Not a language value!")),
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
        | Err(AbruptCompletion::Return { .. })
        | Err(AbruptCompletion::Throw { .. })
        | Err(AbruptCompletion::Break { .. })
        | Err(AbruptCompletion::Continue { .. }) => completion_record,
    }
}

#[cfg(test)]
mod tests;
