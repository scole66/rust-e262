use super::chunk::Chunk;
use super::opcodes::*;
use super::parser::additive_operators::*;
use super::parser::assignment_operators::*;
use super::parser::block::*;
use super::parser::identifiers::*;
use super::parser::primary_expressions::*;

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

impl PrimaryExpression {
    /// Generate the code for PrimaryExpression
    ///
    /// References from ECMA-262:
    /// * [Evaluation of the `this` keyword](https://tc39.es/ecma262/#sec-this-keyword-runtime-semantics-evaluation)
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            PrimaryExpression::IdentifierReference(id) => id.compile(chunk, strict),
            PrimaryExpression::This => {
                // Runtime Semantics: Evaluation
                //  PrimaryExpression : this
                //      1. Return ? ResolveThisBinding().
                chunk.op(Insn::This);
                Ok(())
            }
            PrimaryExpression::Literal(lit) => lit.compile(chunk),
            PrimaryExpression::Parenthesized(exp) => exp.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl Literal {
    /// Generate the code for Literal
    ///
    /// See [Evaluation for Literal](https://tc39.es/ecma262/#sec-literals-runtime-semantics-evaluation) from ECMA-262.
    pub fn compile(&self, chunk: &mut Chunk) -> anyhow::Result<()> {
        match &self.kind {
            LiteralKind::NullLiteral => {
                // Literal : NullLiteral
                //  1. Return null.
                chunk.op(Insn::Null);
            }
            LiteralKind::BooleanLiteral(is_true) => {
                // Literal : BooleanLiteral
                //  1. If BooleanLiteral is the token false, return false.
                //  2. If BooleanLiteral is the token true, return true.
                chunk.op(if *is_true { Insn::True } else { Insn::False });
            }
            LiteralKind::StringLiteral(s) => {
                // Literal : StringLiteral
                //  1. Return the SV of StringLiteral as defined in [12.8.4.2](https://tc39.es/ecma262/#sec-static-semantics-sv).
                let idx = chunk.add_to_string_pool(s.value.clone())?;
                chunk.op_plus_arg(Insn::String, idx);
            }
            LiteralKind::NumericLiteral(numeric) => {
                // Literal : NumericLiteral
                //  1. Return the NumericValue of NumericLiteral as defined in [12.8.3.3](https://tc39.es/ecma262/#sec-numericvalue).
                match numeric {
                    Numeric::Number(n) => {
                        let idx = chunk.add_to_float_pool(*n)?;
                        chunk.op_plus_arg(Insn::Float, idx);
                    }
                    Numeric::BigInt(bi) => {
                        let idx = chunk.add_to_bigint_pool(bi.clone())?;
                        chunk.op_plus_arg(Insn::Bigint, idx);
                    }
                }
            }
        }
        Ok(())
    }
}

impl StatementList {
    #[allow(unused_variables)]
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            StatementList::Item(sli) => sli.compile(chunk, strict),
            StatementList::List(sl, sli) => {
                sl.compile(chunk, strict)?;
                let mark = chunk.op_jump(Insn::JumpIfAbrupt);
                sli.compile(chunk, strict)?;
                chunk.op(Insn::UpdateEmpty);
                chunk.fixup(mark)?;
                Ok(())
            }
        }
    }
}
