use super::agent::Agent;
use super::cr::{AbruptCompletion, CompletionInfo};
use super::object::{get, Object};
use super::values::{ECMAScriptValue, PropertyKey};
use ahash::RandomState;
use std::hash::{BuildHasher, Hash, Hasher};
use std::io::Result as IoResult;
use std::io::Write;

pub struct MockWriter<T>
where
    T: Write,
{
    writer: T,
    pub count: usize,
    target: usize,
    pub error_generated: bool,
}
impl<T> std::io::Write for MockWriter<T>
where
    T: Write,
{
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.count += 1;
        if self.count >= self.target {
            self.error_generated = true;
            Err(std::io::Error::new(std::io::ErrorKind::Other, "oh no!"))
        } else {
            self.writer.write(buf)
        }
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.writer.flush()
    }
}
impl<T> MockWriter<T>
where
    T: Write,
{
    pub fn new(writer: T, errat: usize) -> Self {
        MockWriter { writer, count: 0, target: errat, error_generated: false }
    }
}
pub fn printer_validate<U>(func: U)
where
    U: Fn(&mut MockWriter<Vec<u8>>) -> IoResult<()>,
{
    let mut target = 1;
    loop {
        let mut writer = MockWriter::new(Vec::new(), target);
        let result = func(&mut writer);
        assert!(result.is_err() || !writer.error_generated);
        if !writer.error_generated {
            break;
        }
        target += 1;
    }
}

pub fn unwind_error_object(agent: &mut Agent, kind: &str, err: Object) -> String {
    assert!(err.o.to_error_obj().is_some());
    let name = get(agent, &err, &PropertyKey::from("name")).expect("Error object was missing 'name' property");
    assert!(matches!(name, ECMAScriptValue::String(_)));
    if let ECMAScriptValue::String(name_value) = name {
        assert_eq!(name_value, kind);
    }
    let message = get(agent, &err, &PropertyKey::from("message")).expect("Error object was missing 'message' property");
    assert!(matches!(message, ECMAScriptValue::String(_)));
    if let ECMAScriptValue::String(message_value) = message {
        String::from(message_value)
    } else {
        unreachable!()
    }
}

pub fn unwind_error(agent: &mut Agent, kind: &str, completion: AbruptCompletion) -> String {
    assert!(matches!(completion, AbruptCompletion::Throw(CompletionInfo { value: Some(ECMAScriptValue::Object(_)), target: None })));
    if let AbruptCompletion::Throw(CompletionInfo { value: Some(ECMAScriptValue::Object(err)), target: None }) = completion {
        unwind_error_object(agent, kind, err)
    } else {
        unreachable!()
    }
}

pub fn unwind_type_error(agent: &mut Agent, completion: AbruptCompletion) -> String {
    unwind_error(agent, "TypeError", completion)
}

pub fn unwind_syntax_error(agent: &mut Agent, completion: AbruptCompletion) -> String {
    unwind_error(agent, "SyntaxError", completion)
}

pub fn unwind_syntax_error_object(agent: &mut Agent, err: Object) -> String {
    unwind_error_object(agent, "SyntaxError", err)
}

pub fn calculate_hash<T: Hash>(factory: &RandomState, t: &T) -> u64 {
    let mut s = factory.build_hasher();
    t.hash(&mut s);
    s.finish()
}

pub fn test_agent() -> Agent {
    let mut agent = Agent::new();
    agent.initialize_host_defined_realm();
    agent
}
