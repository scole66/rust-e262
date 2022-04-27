use num_enum::IntoPrimitive;
use num_enum::TryFromPrimitive;
use std::fmt;

pub type Opcode = u16;

#[derive(Debug, PartialEq, IntoPrimitive, TryFromPrimitive)]
#[repr(u16)]
pub enum Insn {
    String,
    Resolve,
    StrictResolve,
    This,
    Null,
    True,
    False,
    Float,
    Bigint,
    GetValue,
    JumpIfAbrupt,
    UpdateEmpty,
}

impl fmt::Display for Insn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.pad(match self {
            Insn::String => "STRING",
            Insn::Resolve => "RESOLVE",
            Insn::StrictResolve => "STRICT_RESOLVE",
            Insn::This => "THIS",
            Insn::Null => "NULL",
            Insn::True => "TRUE",
            Insn::False => "FALSE",
            Insn::Float => "FLOAT",
            Insn::Bigint => "BIGINT",
            Insn::GetValue => "GET_VALUE",
            Insn::JumpIfAbrupt => "JUMP_IF_ABRUPT",
            Insn::UpdateEmpty => "UPDATE_EMPTY",
        })
    }
}
