use crate::opcodes::{Insn, Opcode};
use crate::strings::JSString;
use anyhow::anyhow;

/// A compilation unit
#[derive(Debug)]
pub struct Chunk {
    strings: Vec<JSString>,
    opcodes: Vec<Opcode>,
}

impl Chunk {
    pub fn new() -> Self {
        Self { strings: vec![], opcodes: vec![] }
    }

    pub fn add_to_string_pool(&mut self, s: JSString) -> anyhow::Result<u16> {
        for (idx, name) in self.strings.iter().enumerate() {
            if name == &s {
                // This unwrap is safe, as we're returning something that was ok at least once before.
                return Ok(idx.try_into().unwrap());
            }
        }
        self.strings.push(s);
        (self.strings.len() - 1).try_into().map_err(|_| anyhow!("Out of room for strings in this compilation unit"))
    }

    pub fn op(&mut self, opcode: Insn) {
        self.opcodes.push(opcode.into());
    }

    pub fn op_plus_arg(&mut self, opcode: Insn, arg: u16) {
        self.opcodes.push(opcode.into());
        self.opcodes.push(arg);
    }
}
