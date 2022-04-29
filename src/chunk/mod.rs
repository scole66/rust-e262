use crate::compiler::{Insn, Opcode};
use crate::strings::JSString;
use anyhow::anyhow;
use num::bigint::BigInt;
use std::rc::Rc;

/// A compilation unit
#[derive(Debug, Default)]
pub struct Chunk {
    pub name: String,
    pub strings: Vec<JSString>,
    pub opcodes: Vec<Opcode>,
    pub floats: Vec<f64>,
    pub bigints: Vec<Rc<BigInt>>,
}

impl Chunk {
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into(), ..Default::default() }
    }

    pub fn add_to_string_pool(&mut self, s: JSString) -> anyhow::Result<u16> {
        Self::add_to_pool(&mut self.strings, s, "strings")
    }

    pub fn add_to_float_pool(&mut self, n: f64) -> anyhow::Result<u16> {
        Self::add_to_pool(&mut self.floats, n, "floats")
    }

    pub fn add_to_bigint_pool(&mut self, n: Rc<BigInt>) -> anyhow::Result<u16> {
        Self::add_to_pool(&mut self.bigints, n, "big ints")
    }

    fn add_to_pool<Item>(collection: &mut Vec<Item>, item: Item, collection_name: &str) -> anyhow::Result<u16>
    where
        Item: PartialEq,
    {
        for (idx, existing) in collection.iter().enumerate() {
            if existing == &item {
                // This unwrap is safe, as we're returning something that was ok at least once before.
                return Ok(idx.try_into().unwrap());
            }
        }
        collection.push(item);
        (collection.len() - 1).try_into().map_err(|_| anyhow!("Out of room for {} in this compilation unit", collection_name))
    }

    pub fn op(&mut self, opcode: Insn) {
        self.opcodes.push(opcode.into());
    }

    pub fn op_plus_arg(&mut self, opcode: Insn, arg: u16) {
        self.opcodes.push(opcode.into());
        self.opcodes.push(arg);
    }

    pub fn op_jump(&mut self, opcode: Insn) -> usize {
        self.opcodes.push(opcode.into());
        self.opcodes.push(0);
        self.opcodes.len() - 1
    }

    pub fn fixup(&mut self, mark: usize) -> anyhow::Result<()> {
        let len = self.opcodes.len();
        let offset = if len > mark { i16::try_from(len - mark - 1)? } else { -i16::try_from(mark + 1 - len)? };
        self.opcodes[mark] = offset as u16;
        Ok(())
    }

    pub fn disassemble(&self) -> Vec<String> {
        let mut idx = 0;
        let mut result = vec![];
        result.push(format!("====== {} ======", self.name));
        while idx < self.opcodes.len() {
            let insn = Insn::try_from(self.opcodes[idx]).unwrap();
            idx += 1;
            match insn {
                Insn::String => {
                    let arg = self.opcodes[idx] as usize;
                    idx += 1;
                    result.push(format!("    {:<20}{} ({})", insn, arg, self.strings[arg]));
                }
                Insn::Float => {
                    let arg = self.opcodes[idx] as usize;
                    idx += 1;
                    result.push(format!("    {:<20}{} ({})", insn, arg, self.floats[arg]));
                }
                Insn::Bigint => {
                    let arg = self.opcodes[idx] as usize;
                    idx += 1;
                    result.push(format!("    {:<20}{} ({})", insn, arg, self.bigints[arg]));
                }
                Insn::Resolve | Insn::StrictResolve | Insn::This | Insn::Null | Insn::True | Insn::False | Insn::GetValue | Insn::UpdateEmpty => {
                    result.push(format!("    {insn}"));
                }
                Insn::JumpIfAbrupt => {
                    let arg = self.opcodes[idx] as i16;
                    idx += 1;
                    result.push(format!("    {:<20}{}", insn, arg));
                }
            }
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::testhelp::Maker;
    use test_case::test_case;

    #[test_case("Boolean")]
    #[test_case("67.2")]
    #[test_case("'Thanksgiving'")]
    #[test_case("null")]
    #[test_case("true")]
    #[test_case("false")]
    #[test_case("34791057890163987078960471890743891203561n")]
    #[test_case("(undefined)")]
    #[test_case("a+b" => panics "not yet implemented")]
    fn compile_an_expression(src: &str) {
        let node = Maker::new(src).script();
        let mut my_chunk = Chunk::new("test chunk");
        node.compile(&mut my_chunk).unwrap();

        for line in my_chunk.disassemble() {
            println!("{line}");
        }
    }
}
