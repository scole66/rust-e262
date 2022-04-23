use super::chunk::Chunk;
use super::opcodes::*;
use super::parser::additive_operators::AdditiveExpression;
use super::parser::assignment_operators::AssignmentExpression;
use super::parser::identifiers::*;

impl AdditiveExpression {
    #[allow(unused_variables)]
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            AdditiveExpression::MultiplicativeExpression(me) => me.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl AssignmentExpression {
    #[allow(unused_variables)]
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            AssignmentExpression::FallThru(ce) => ce.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl IdentifierReference {
    /// Generate the code for IdentifierReference
    ///
    /// See [IdentifierReference Evaluation](https://tc39.es/ecma262/#sec-identifiers-runtime-semantics-evaluation) from ECMA-262.
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        // Runtime Semantics: Evaluation
        //  IdentifierReference : Identifier
        //      1. Return ? ResolveBinding(StringValue of Identifier).
        //  IdentifierReference : yield
        //      1. Return ? ResolveBinding("yield").
        //  IdentifierReference : await
        //      1. Return ? ResolveBinding("await").
        //
        // NOTE 1   | The result of evaluating an IdentifierReference is always a value of type Reference.
        // NOTE 2   | In non-strict code, the keyword yield may be used as an identifier. Evaluating the
        //          | IdentifierReference resolves the binding of yield as if it was an Identifier. Early Error
        //          | restriction ensures that such an evaluation only can occur for non-strict code.

        // Add the identifier string to this chunk's string pool.
        let string_id = chunk.add_to_string_pool(match &self.kind {
            IdentifierReferenceKind::Identifier(id) => id.string_value(),
            IdentifierReferenceKind::Yield => "yield".into(),
            IdentifierReferenceKind::Await => "await".into(),
        })?;
        chunk.op_plus_arg(Insn::String, string_id);
        chunk.op(if strict { Insn::StrictResolve } else { Insn::Resolve });
        Ok(())
    }
}
