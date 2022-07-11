use super::*;
use anyhow::anyhow;
use num::bigint::BigInt;
use std::rc::Rc;

#[derive(Debug)]
pub struct StashedFunctionData {
    pub source_text: String,
    pub params: ParamSource,
    pub body: BodySource,
    pub to_compile: FunctionSource,
    pub strict: bool,
}

/// A compilation unit
#[derive(Debug, Default)]
pub struct Chunk {
    pub name: String,
    pub strings: Vec<JSString>,
    pub opcodes: Vec<Opcode>,
    pub floats: Vec<f64>,
    pub bigints: Vec<Rc<BigInt>>,
    pub function_object_data: Vec<StashedFunctionData>,
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
        (collection.len() - 1)
            .try_into()
            .map_err(|_| anyhow!("Out of room for {} in this compilation unit", collection_name))
    }

    pub fn add_to_func_stash(&mut self, fd: StashedFunctionData) -> anyhow::Result<u16> {
        let result = self.function_object_data.len();
        self.function_object_data.push(fd);
        result.try_into().map_err(|_| anyhow!("Out of room for more functions!"))
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
        if mark >= len {
            anyhow::bail!("Fixup location out of range");
        }
        let offset = i16::try_from(len - mark - 1)?;
        self.opcodes[mark] = offset as u16;
        Ok(())
    }

    pub fn insn_repr_at(&self, starting_idx: usize) -> (usize, String) {
        let mut idx = starting_idx;
        let insn = Insn::try_from(self.opcodes[idx]).unwrap();
        idx += 1;
        match insn {
            Insn::String | Insn::CreateStrictImmutableLexBinding | Insn::CreatePermanentMutableLexBinding => {
                let arg = self.opcodes[idx] as usize;
                (2, format!("    {:<20}{} ({})", insn, arg, self.strings[arg]))
            }
            Insn::Float => {
                let arg = self.opcodes[idx] as usize;
                (2, format!("    {:<20}{} ({})", insn, arg, self.floats[arg]))
            }
            Insn::Bigint => {
                let arg = self.opcodes[idx] as usize;
                (2, format!("    {:<20}{} ({})", insn, arg, self.bigints[arg]))
            }
            Insn::Unwind | Insn::InstantiateIdFreeFunctionExpression | Insn::InstantiateArrowFunctionExpression => {
                let arg = self.opcodes[idx] as usize;
                (2, format!("    {:<20}{}", insn, arg))
            }
            Insn::Ref
            | Insn::StrictRef
            | Insn::Resolve
            | Insn::StrictResolve
            | Insn::Nop
            | Insn::InitializeReferencedBinding
            | Insn::PopLexEnv
            | Insn::PushNewLexEnv
            | Insn::InitializeLexBinding
            | Insn::CreateDataProperty
            | Insn::SetPrototype
            | Insn::ToPropertyKey
            | Insn::CopyDataProps
            | Insn::This
            | Insn::Null
            | Insn::Undefined
            | Insn::True
            | Insn::False
            | Insn::Empty
            | Insn::GetValue
            | Insn::PutValue
            | Insn::Call
            | Insn::UpdateEmpty
            | Insn::Swap
            | Insn::Pop
            | Insn::Pop2Push3
            | Insn::Dup
            | Insn::ToNumeric
            | Insn::Increment
            | Insn::Decrement
            | Insn::PreIncrement
            | Insn::PreDecrement
            | Insn::Delete
            | Insn::Void
            | Insn::TypeOf
            | Insn::UnaryPlus
            | Insn::UnaryMinus
            | Insn::UnaryComplement
            | Insn::UnaryNot
            | Insn::Exponentiate
            | Insn::Multiply
            | Insn::Divide
            | Insn::Modulo
            | Insn::Add
            | Insn::Subtract
            | Insn::LeftShift
            | Insn::SignedRightShift
            | Insn::UnsignedRightShift
            | Insn::Less
            | Insn::Greater
            | Insn::LessEqual
            | Insn::GreaterEqual
            | Insn::InstanceOf
            | Insn::In
            | Insn::Equal
            | Insn::NotEqual
            | Insn::StrictEqual
            | Insn::StrictNotEqual
            | Insn::BitwiseAnd
            | Insn::BitwiseOr
            | Insn::BitwiseXor
            | Insn::Throw
            | Insn::Object => (1, format!("    {insn}")),
            Insn::JumpIfAbrupt
            | Insn::Jump
            | Insn::JumpIfNormal
            | Insn::JumpIfFalse
            | Insn::JumpIfTrue
            | Insn::JumpIfNotNullish => {
                let arg = self.opcodes[idx] as i16;
                (2, format!("    {:<20}{}", insn, arg))
            }
        }
    }

    pub fn disassemble(&self) -> Vec<String> {
        let mut idx = 0;
        let mut result = vec![];
        result.push(format!("====== {} ======", self.name));
        while idx < self.opcodes.len() {
            let (inc, repr) = self.insn_repr_at(idx);
            idx += inc;
            result.push(repr);
        }
        result
    }
}

#[cfg(test)]
mod tests;
