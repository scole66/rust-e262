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

#[cfg(test)]
mod tests;
