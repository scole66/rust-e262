use crate::strings::JSString;
use crate::values::ECMAScriptValue;

#[derive(Clone, Debug)]
pub struct CompletionInfo {
    pub value: Option<ECMAScriptValue>,
    pub target: Option<JSString>,
}

#[derive(Clone, Debug)]
pub enum AbruptCompletion {
    Break(CompletionInfo),
    Continue(CompletionInfo),
    Return(CompletionInfo),
    Throw(CompletionInfo),
}

pub type Completion = Result<ECMAScriptValue, AbruptCompletion>;
