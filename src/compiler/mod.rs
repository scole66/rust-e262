use super::chunk::Chunk;
use super::parser::additive_operators::*;
use super::parser::assignment_operators::*;
use super::parser::binary_bitwise_operators::*;
use super::parser::binary_logical_operators::*;
use super::parser::bitwise_shift_operators::*;
use super::parser::block::*;
use super::parser::comma_operator::*;
use super::parser::conditional_operator::*;
use super::parser::declarations_and_variables::*;
use super::parser::equality_operators::*;
use super::parser::exponentiation_operator::*;
use super::parser::expression_statement::*;
use super::parser::identifiers::*;
use super::parser::left_hand_side_expressions::*;
use super::parser::multiplicative_operators::*;
use super::parser::primary_expressions::*;
use super::parser::relational_operators::*;
use super::parser::scripts::*;
use super::parser::statements_and_declarations::*;
use super::parser::unary_operators::*;
use super::parser::update_expressions::*;
use super::scanner::*;
use num_enum::IntoPrimitive;
use num_enum::TryFromPrimitive;
use std::fmt;

pub type Opcode = u16;

#[derive(Debug, Copy, Clone, PartialEq, IntoPrimitive, TryFromPrimitive)]
#[repr(u16)]
pub enum Insn {
    String,
    Resolve,
    StrictResolve,
    This,
    Null,
    Undefined,
    True,
    False,
    Empty,
    Float,
    Bigint,
    GetValue,
    PutValue,
    Jump,
    JumpIfAbrupt,
    JumpIfNormal,
    UpdateEmpty,
    Swap,
    Pop,
    Pop2Push3,
    Ref,
    StrictRef,
    InitializeReferencedBinding,
}

impl fmt::Display for Insn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.pad(match self {
            Insn::String => "STRING",
            Insn::Resolve => "RESOLVE",
            Insn::StrictResolve => "STRICT_RESOLVE",
            Insn::This => "THIS",
            Insn::Null => "NULL",
            Insn::Undefined => "UNDEFINED",
            Insn::True => "TRUE",
            Insn::False => "FALSE",
            Insn::Empty => "EMPTY",
            Insn::Float => "FLOAT",
            Insn::Bigint => "BIGINT",
            Insn::GetValue => "GET_VALUE",
            Insn::PutValue => "PUT_VALUE",
            Insn::Jump => "JUMP",
            Insn::JumpIfAbrupt => "JUMP_IF_ABRUPT",
            Insn::JumpIfNormal => "JUMP_IF_NORMAL",
            Insn::UpdateEmpty => "UPDATE_EMPTY",
            Insn::Swap => "SWAP",
            Insn::Pop => "POP",
            Insn::Pop2Push3 => "POP2_PUSH3",
            Insn::Ref => "REF",
            Insn::StrictRef => "STRICT_REF",
            Insn::InitializeReferencedBinding => "IRB",
        })
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

impl ParenthesizedExpression {
    /// Generate the code for ParenthesizedExpression
    ///
    /// See [Evaluation for Grouping Operator](https://tc39.es/ecma262/#sec-grouping-operator-runtime-semantics-evaluation) from ECMA-262.
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        // Runtime Semantics: Evaluation
        //  ParenthesizedExpression : ( Expression )
        //      1. Return the result of evaluating Expression. This may be of type Reference.
        //
        // NOTE | This algorithm does not apply GetValue to the result of evaluating Expression. The principal
        //      | motivation for this is so that operators such as delete and typeof may be applied to parenthesized
        //      | expressions.
        let ParenthesizedExpression::Expression(e) = self;
        e.compile(chunk, strict)
    }
}

impl MemberExpression {
    /// See [EvaluatePropertyAccessWithIdentifierKey ](https://tc39.es/ecma262/#sec-evaluate-property-access-with-identifier-key)
    fn evaluate_property_access_with_identifier_key(chunk: &mut Chunk, identifier_name: &IdentifierData, strict: bool) -> anyhow::Result<()> {
        // Stack: base ...
        let idx = chunk.add_to_string_pool(identifier_name.string_value.clone())?;
        chunk.op_plus_arg(Insn::String, idx);
        // Stack: name base ...
        chunk.op(if strict { Insn::StrictRef } else { Insn::Ref });
        // Stack: ref
        Ok(())
    }

    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match &self.kind {
            MemberExpressionKind::PrimaryExpression(pe) => pe.compile(chunk, strict),
            MemberExpressionKind::IdentifierName(me, id) => {
                me.compile(chunk, strict)?;
                chunk.op(Insn::GetValue);
                let mark = chunk.op_jump(Insn::JumpIfAbrupt);
                Self::evaluate_property_access_with_identifier_key(chunk, id, strict)?;
                chunk.fixup(mark)?;
                Ok(())
            }
            _ => todo!(),
        }
    }
}

impl NewExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match &self.kind {
            NewExpressionKind::MemberExpression(me) => me.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl LeftHandSideExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            LeftHandSideExpression::New(ne) => ne.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl UpdateExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            UpdateExpression::LeftHandSideExpression(lhse) => lhse.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl UnaryExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            UnaryExpression::UpdateExpression(ue) => ue.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl ExponentiationExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            ExponentiationExpression::UnaryExpression(ue) => ue.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl MultiplicativeExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            MultiplicativeExpression::ExponentiationExpression(ee) => ee.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl AdditiveExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            AdditiveExpression::MultiplicativeExpression(me) => me.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl ShiftExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            ShiftExpression::AdditiveExpression(ae) => ae.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl RelationalExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            RelationalExpression::ShiftExpression(se) => se.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl EqualityExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            EqualityExpression::RelationalExpression(re) => re.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl BitwiseANDExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            BitwiseANDExpression::EqualityExpression(ee) => ee.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl BitwiseXORExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            BitwiseXORExpression::BitwiseANDExpression(bae) => bae.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl BitwiseORExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            BitwiseORExpression::BitwiseXORExpression(bxe) => bxe.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl LogicalANDExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            LogicalANDExpression::BitwiseORExpression(boe) => boe.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl LogicalORExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            LogicalORExpression::LogicalANDExpression(lae) => lae.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl ShortCircuitExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            ShortCircuitExpression::LogicalORExpression(loe) => loe.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl ConditionalExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            ConditionalExpression::FallThru(sce) => sce.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl AssignmentExpression {
    #[allow(unused_assignments)]
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            AssignmentExpression::FallThru(ce) => ce.compile(chunk, strict),
            AssignmentExpression::Assignment(lhse, ae) => {
                // Runtime Semantics: Evaluation
                //  AssignmentExpression : LeftHandSideExpression = AssignmentExpression
                //      a. Let lref be the result of evaluating LeftHandSideExpression.
                //      b. ReturnIfAbrupt(lref).
                //      c. If IsAnonymousFunctionDefinition(AssignmentExpression) and IsIdentifierRef of LeftHandSideExpression are both true, then
                //          i. Let rval be ? NamedEvaluation of AssignmentExpression with argument lref.[[ReferencedName]].
                //      d. Else,
                //          i. Let rref be the result of evaluating AssignmentExpression.
                //          ii. Let rval be ? GetValue(rref).
                //      e. Perform ? PutValue(lref, rval).
                //      f. Return rval.
                lhse.compile(chunk, strict)?;
                let mark = chunk.op_jump(Insn::JumpIfAbrupt);
                // Stack: lref ...
                let mut mark2 = None;
                if ae.is_anonymous_function_definition() && lhse.is_identifier_ref() {
                    todo!()
                } else {
                    ae.compile(chunk, strict)?;
                    // Stack: rref lref ...
                    chunk.op(Insn::GetValue);
                    let close = chunk.op_jump(Insn::JumpIfNormal);
                    // (haven't jumped) Stack: err lref
                    chunk.op(Insn::Swap);
                    // Stack: lref err
                    chunk.op(Insn::Pop);
                    // Stack: err
                    mark2 = Some(chunk.op_jump(Insn::Jump));
                    chunk.fixup(close)?;
                }
                // Stack: rval lref ...
                chunk.op(Insn::Pop2Push3);
                // Stack: rval lref rval ...
                chunk.op(Insn::PutValue);
                // Stack: empty rval ...
                chunk.op(Insn::UpdateEmpty);
                // Stack: rval ...

                chunk.fixup(mark)?;
                if let Some(m) = mark2 {
                    chunk.fixup(m)?;
                }
                Ok(())
            }
            _ => todo!(),
        }
    }
}

impl Expression {
    #[allow(unused_variables)]
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            Expression::FallThru(ae) => ae.compile(chunk, strict),
            Expression::Comma(e, ae) => todo!(),
        }
    }
}

impl ExpressionStatement {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        let ExpressionStatement::Expression(node) = self;
        node.compile(chunk, strict)?;
        chunk.op(Insn::GetValue);
        Ok(())
    }
}

impl StatementList {
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

impl StatementListItem {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            StatementListItem::Statement(stmt) => stmt.compile(chunk, strict),
            StatementListItem::Declaration(decl) => decl.compile(chunk, strict),
        }
    }
}

impl Statement {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            Statement::Expression(exp) => exp.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl Declaration {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            Declaration::Class(_) => todo!(),
            Declaration::Hoistable(_) => todo!(),
            Declaration::Lexical(lex) => lex.compile(chunk, strict),
        }
    }
}

impl LexicalDeclaration {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        let LexicalDeclaration::List(_, bi) = self;
        bi.compile(chunk, strict)?;
        let mark = chunk.op_jump(Insn::JumpIfAbrupt);
        chunk.op(Insn::Pop);
        chunk.op(Insn::Empty);
        chunk.fixup(mark)?;
        Ok(())
    }
}

impl BindingList {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            BindingList::Item(item) => item.compile(chunk, strict),
            BindingList::List(lst, item) => {
                lst.compile(chunk, strict)?;
                let mark = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Pop);
                item.compile(chunk, strict)?;
                chunk.fixup(mark)?;
                Ok(())
            }
        }
    }
}

impl LexicalBinding {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        match self {
            LexicalBinding::Identifier(bi, init) => {
                let id = chunk.add_to_string_pool(bi.string_value())?;
                chunk.op_plus_arg(Insn::String, id);
                // Stack: name ...
                chunk.op(if strict { Insn::StrictResolve } else { Insn::Resolve });
                // Stack: lhs ...
                let exit_tgt = match init {
                    None => {
                        chunk.op(Insn::Undefined);
                        None
                    }
                    Some(izer) => {
                        if izer.is_anonymous_function_definition() {
                            todo!();
                        } else {
                            izer.compile(chunk, strict)?;
                            // Stack: rref lhs ...
                            chunk.op(Insn::GetValue);
                            // Stack: value lhs ...
                            let normal = chunk.op_jump(Insn::JumpIfNormal);
                            // Stack: err lhs ...
                            chunk.op(Insn::Swap);
                            // Stack: lhs err ...
                            chunk.op(Insn::Pop);
                            // Stack: err ...
                            let exit_tgt = Some(chunk.op_jump(Insn::Jump));
                            chunk.fixup(normal)?;
                            exit_tgt
                        }
                    }
                };
                // Stack: value lhs ...
                chunk.op(Insn::InitializeReferencedBinding);
                // Stack: empty ...
                if let Some(mark) = exit_tgt {
                    chunk.fixup(mark)?;
                }
                Ok(())
            }
            LexicalBinding::Pattern(_, _) => todo!(),
        }
    }
}

impl Initializer {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<()> {
        let Initializer::AssignmentExpression(ae) = self;
        ae.compile(chunk, strict)
    }
}

impl Script {
    pub fn compile(&self, chunk: &mut Chunk) -> anyhow::Result<()> {
        match &self.0 {
            None => Ok(()),
            Some(sb) => sb.compile(chunk),
        }
    }
}

impl ScriptBody {
    pub fn compile(&self, chunk: &mut Chunk) -> anyhow::Result<()> {
        let strict = self.contains_use_strict();
        self.statement_list.compile(chunk, strict)
    }
}

#[cfg(test)]
mod tests;
