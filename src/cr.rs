use crate::strings::JSString;
use crate::values::ECMAScriptValue;

#[derive(Clone)]
pub struct CompletionInfo {
    value: Option<ECMAScriptValue>,
    target: Option<JSString>,
}

#[derive(Clone)]
pub enum AbruptCompletion {
    Break(CompletionInfo),
    Continue(CompletionInfo),
    Return(CompletionInfo),
    Throw(CompletionInfo),
}

pub type Completion = Result<ECMAScriptValue, AbruptCompletion>;
