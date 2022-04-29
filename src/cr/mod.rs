use crate::reference::Reference;
use crate::strings::JSString;
use crate::values::ECMAScriptValue;

#[derive(Clone, Debug, PartialEq)]
pub struct CompletionInfo {
    pub value: Option<ECMAScriptValue>,
    pub target: Option<JSString>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AbruptCompletion {
    Break(CompletionInfo),
    Continue(CompletionInfo),
    Return(CompletionInfo),
    Throw(CompletionInfo),
}

pub type Completion = Result<ECMAScriptValue, AbruptCompletion>;
pub type AltCompletion<T> = Result<T, AbruptCompletion>;

pub enum NormalCompletion {
    Empty,
    Value(ECMAScriptValue),
    Reference(Box<Reference>),
}
pub enum AbruptCompletion2 {
    Break { value: Option<ECMAScriptValue>, target: Option<JSString> },
    Continue { value: Option<ECMAScriptValue>, target: Option<JSString> },
    Return { value: ECMAScriptValue },
    Throw { value: ECMAScriptValue },
}

pub type Completion2 = Result<NormalCompletion, AbruptCompletion2>;

pub fn update_empty(completion_record: Completion2, old_value: Option<ECMAScriptValue>) -> Completion2 {
    match completion_record {
        Ok(NormalCompletion::Empty) => match old_value {
            None => Ok(NormalCompletion::Empty),
            Some(v) => Ok(NormalCompletion::Value(v)),
        },
        Ok(_)
        | Err(AbruptCompletion2::Return { .. })
        | Err(AbruptCompletion2::Throw { .. })
        | Err(AbruptCompletion2::Break { value: Some(_), .. })
        | Err(AbruptCompletion2::Continue { value: Some(_), .. }) => completion_record,
        Err(AbruptCompletion2::Break { value: None, target }) => Err(AbruptCompletion2::Break { value: old_value, target }),
        Err(AbruptCompletion2::Continue { value: None, target }) => Err(AbruptCompletion2::Continue { value: old_value, target }),
    }
}

#[cfg(test)]
mod tests;
