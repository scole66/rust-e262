use anyhow::anyhow;

pub type Opcode = u16;

#[derive(Debug, PartialEq)]
pub enum Insn {
    PushStr,
    Resolve,
    StrictResolve,
}

impl From<Insn> for Opcode {
    fn from(src: Insn) -> Self {
        src as Opcode
    }
}

impl TryFrom<Opcode> for Insn {
    type Error = anyhow::Error;
    fn try_from(src: Opcode) -> anyhow::Result<Insn> {
        match src {
            x if x == Insn::PushStr as Opcode => Ok(Insn::PushStr),
            x if x == Insn::Resolve as Opcode => Ok(Insn::Resolve),
            x if x == Insn::StrictResolve as Opcode => Ok(Insn::StrictResolve),
            _ => Err(anyhow!("Invalid opcode {}", src)),
        }
    }
}
