use crate::reference::Reference;
use crate::strings::JSString;
use crate::values::ECMAScriptValue;

#[derive(Clone, Debug, PartialEq)]
pub enum NormalCompletion {
    Empty,
    Value(ECMAScriptValue),
    Reference(Box<Reference>),
}
#[derive(Clone, Debug, PartialEq)]
pub enum AbruptCompletion {
    Break { value: Option<ECMAScriptValue>, target: Option<JSString> },
    Continue { value: Option<ECMAScriptValue>, target: Option<JSString> },
    Return { value: ECMAScriptValue },
    Throw { value: ECMAScriptValue },
}

pub type Completion<T> = Result<T, AbruptCompletion>;
pub type FullCompletion = Completion<NormalCompletion>;

impl From<ECMAScriptValue> for NormalCompletion {
    fn from(src: ECMAScriptValue) -> Self {
        Self::Value(src)
    }
}

impl From<Reference> for NormalCompletion {
    fn from(src: Reference) -> Self {
        Self::Reference(Box::new(src))
    }
}

pub fn update_empty(completion_record: FullCompletion, old_value: Option<ECMAScriptValue>) -> FullCompletion {
    match completion_record {
        Ok(NormalCompletion::Empty) => match old_value {
            None => Ok(NormalCompletion::Empty),
            Some(v) => Ok(NormalCompletion::Value(v)),
        },
        Ok(_)
        | Err(AbruptCompletion::Return { .. })
        | Err(AbruptCompletion::Throw { .. })
        | Err(AbruptCompletion::Break { value: Some(_), .. })
        | Err(AbruptCompletion::Continue { value: Some(_), .. }) => completion_record,
        Err(AbruptCompletion::Break { value: None, target }) => Err(AbruptCompletion::Break { value: old_value, target }),
        Err(AbruptCompletion::Continue { value: None, target }) => Err(AbruptCompletion::Continue { value: old_value, target }),
    }
}

#[cfg(test)]
mod tests;
