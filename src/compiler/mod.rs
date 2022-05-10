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
use super::strings::*;
use super::values::*;
use num_enum::IntoPrimitive;
use num_enum::TryFromPrimitive;
use std::fmt;
use std::rc::Rc;

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
    Dup,
    Ref,
    StrictRef,
    InitializeReferencedBinding,
    Object,
    CreateDataProperty,
    SetPrototype,
    ToPropertyKey,
    CopyDataProps,
    ToNumeric,
    Increment,
    Decrement,
    PreIncrement,
    PreDecrement,
    Delete,
    Void,
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
            Insn::Dup => "DUP",
            Insn::Ref => "REF",
            Insn::StrictRef => "STRICT_REF",
            Insn::InitializeReferencedBinding => "IRB",
            Insn::Object => "OBJECT",
            Insn::CreateDataProperty => "CR_PROP",
            Insn::SetPrototype => "SET_PROTO",
            Insn::ToPropertyKey => "TO_KEY",
            Insn::CopyDataProps => "COPY_DATA_PROPS",
            Insn::ToNumeric => "TO_NUMERIC",
            Insn::Increment => "INCREMENT",
            Insn::Decrement => "DECREMENT",
            Insn::PreDecrement => "PRE_DECREMENT",
            Insn::PreIncrement => "PRE_INCREMENT",
            Insn::Delete => "DELETE",
            Insn::Void => "VOID",
        })
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct CompilerStatusFlags {
    pub can_be_abrupt: bool,
    pub can_be_reference: bool,
}
impl CompilerStatusFlags {
    pub fn new() -> Self {
        Self { ..Default::default() }
    }
    pub fn abrupt(self) -> Self {
        Self { can_be_abrupt: true, ..self }
    }
    pub fn reference(self) -> Self {
        Self { can_be_reference: true, ..self }
    }
}

impl IdentifierReference {
    /// Generate the code for IdentifierReference
    ///
    /// See [IdentifierReference Evaluation](https://tc39.es/ecma262/#sec-identifiers-runtime-semantics-evaluation) from ECMA-262.
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
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
        Ok(CompilerStatusFlags::new().abrupt().reference())
    }
}

impl PrimaryExpression {
    /// Generate the code for PrimaryExpression
    ///
    /// References from ECMA-262:
    /// * [Evaluation of the `this` keyword](https://tc39.es/ecma262/#sec-this-keyword-runtime-semantics-evaluation)
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            PrimaryExpression::IdentifierReference(id) => id.compile(chunk, strict),
            PrimaryExpression::This => {
                // Runtime Semantics: Evaluation
                //  PrimaryExpression : this
                //      1. Return ? ResolveThisBinding().
                chunk.op(Insn::This);
                Ok(CompilerStatusFlags::new().abrupt())
            }
            PrimaryExpression::Literal(lit) => lit.compile(chunk),
            PrimaryExpression::Parenthesized(exp) => exp.compile(chunk, strict),
            PrimaryExpression::ObjectLiteral(ol) => ol.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl Literal {
    /// Generate the code for Literal
    ///
    /// See [Evaluation for Literal](https://tc39.es/ecma262/#sec-literals-runtime-semantics-evaluation) from ECMA-262.
    pub fn compile(&self, chunk: &mut Chunk) -> anyhow::Result<CompilerStatusFlags> {
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
        Ok(CompilerStatusFlags::new())
    }
}

impl ParenthesizedExpression {
    /// Generate the code for ParenthesizedExpression
    ///
    /// See [Evaluation for Grouping Operator](https://tc39.es/ecma262/#sec-grouping-operator-runtime-semantics-evaluation) from ECMA-262.
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
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

impl ObjectLiteral {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            ObjectLiteral::Empty => {
                chunk.op(Insn::Object);
                Ok(CompilerStatusFlags::new())
            }
            ObjectLiteral::Normal(pdl) | ObjectLiteral::TrailingComma(pdl) => {
                // Stack: ...
                chunk.op(Insn::Object);
                // Stack: obj ...
                let status = pdl.property_definition_evaluation(chunk, strict)?;
                // Stack: obj ...
                Ok(status)
            }
        }
    }
}

impl PropertyDefinitionList {
    pub fn property_definition_evaluation(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            PropertyDefinitionList::OneDef(pd) => pd.property_definition_evaluation(chunk, strict),
            PropertyDefinitionList::ManyDefs(pdl, pd) => {
                let mut exit = None;
                let first = pdl.property_definition_evaluation(chunk, strict)?;
                if first.can_be_abrupt {
                    exit = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                let second = pd.property_definition_evaluation(chunk, strict)?;
                if let Some(mark) = exit {
                    chunk.fixup(mark)?;
                }
                Ok(CompilerStatusFlags { can_be_abrupt: first.can_be_abrupt || second.can_be_abrupt, can_be_reference: second.can_be_reference })
            }
        }
    }
}

impl PropertyDefinition {
    pub fn property_definition_evaluation(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            PropertyDefinition::IdentifierReference(idr) => {
                let mut exit_status = CompilerStatusFlags::new();
                // Stack: obj ...
                let name = idr.string_value();
                let name_idx = chunk.add_to_string_pool(name)?;
                chunk.op_plus_arg(Insn::String, name_idx);
                // Stack: name obj ...
                let status = idr.compile(chunk, strict)?;
                let mut escape = None;
                if status.can_be_reference {
                    // Stack: ref name obj ...
                    chunk.op(Insn::GetValue);
                }
                if status.can_be_abrupt || status.can_be_reference {
                    // Stack: value name obj ...
                    let isok = chunk.op_jump(Insn::JumpIfNormal);
                    // Stack: err name obj ...
                    chunk.op(Insn::Swap);
                    // Stack: name err obj ...
                    chunk.op(Insn::Pop);
                    // Stack: err obj ...
                    chunk.op(Insn::Swap);
                    // Stack: obj err ...
                    chunk.op(Insn::Pop);
                    // Stack: err ...
                    escape = Some(chunk.op_jump(Insn::Jump));
                    chunk.fixup(isok)?;
                    exit_status = exit_status.abrupt();
                }
                // Stack: value name obj ...
                chunk.op(Insn::CreateDataProperty);
                // Stack: obj ...

                if let Some(mark) = escape {
                    chunk.fixup(mark)?;
                }
                Ok(exit_status)
            }
            PropertyDefinition::CoverInitializedName(_) => unreachable!(),
            PropertyDefinition::PropertyNameAssignmentExpression(pn, ae) => {
                let mut exit_status = CompilerStatusFlags::new();
                let mut exits = Vec::with_capacity(2);
                // Stack: obj ...
                let status = pn.compile(chunk, strict)?;
                // Stack: propKey obj ...
                if status.can_be_abrupt {
                    let mark = chunk.op_jump(Insn::JumpIfNormal);
                    // Stack: err obj ...
                    chunk.op(Insn::Swap);
                    // Stack: obj err ...
                    chunk.op(Insn::Pop);
                    // Stack: err ...
                    exits.push(chunk.op_jump(Insn::Jump));
                    chunk.fixup(mark)?;
                    exit_status = exit_status.abrupt();
                }
                // Stack: propKey obj ...
                let is_proto_setter = pn.is_literal_proto();
                if !is_proto_setter && ae.is_anonymous_function_definition() {
                    todo!();
                } else {
                    let status = ae.compile(chunk, strict)?;
                    if status.can_be_reference {
                        // Stack: exprValueRef propKey obj ...
                        chunk.op(Insn::GetValue);
                    }
                    if status.can_be_abrupt || status.can_be_reference {
                        // Stack: propValue propKey obj ...
                        let mark = chunk.op_jump(Insn::JumpIfNormal);
                        // Stack: err propKey obj ...
                        chunk.op(Insn::Swap);
                        // Stack: propKey err obj ...
                        chunk.op(Insn::Pop);
                        // Stack: err obj ...
                        chunk.op(Insn::Swap);
                        // Stack: obj err ...
                        chunk.op(Insn::Pop);
                        // Stack: err ...
                        exits.push(chunk.op_jump(Insn::Jump));
                        chunk.fixup(mark)?;
                        exit_status = exit_status.abrupt();
                    }
                }
                // Stack: propValue propKey obj ...
                if is_proto_setter {
                    chunk.op(Insn::Swap);
                    // Stack: propKey propValue obj ...
                    chunk.op(Insn::Pop);
                    // Stack: propValue obj ...
                    chunk.op(Insn::SetPrototype);
                    // Stack: obj ...
                } else {
                    chunk.op(Insn::CreateDataProperty);
                    // Stack: obj ...
                }
                for mark in exits {
                    chunk.fixup(mark)?;
                }
                Ok(exit_status)
            }
            PropertyDefinition::MethodDefinition(_) => todo!(),
            PropertyDefinition::AssignmentExpression(ae) => {
                let mut exits = vec![];
                // Stack: obj ...
                let status = ae.compile(chunk, strict)?;
                // Stack: exprValue obj ...
                if status.can_be_reference {
                    chunk.op(Insn::GetValue);
                }
                if status.can_be_abrupt || status.can_be_reference {
                    let close = chunk.op_jump(Insn::JumpIfNormal);
                    // Stack: err obj ...
                    chunk.op(Insn::Swap);
                    // Stack: obj err ...
                    chunk.op(Insn::Pop);
                    // Stack: err ...
                    let mark = chunk.op_jump(Insn::Jump);
                    exits.push(mark);
                    chunk.fixup(close)?;
                }
                // Stack: fromValue obj ...
                chunk.op(Insn::CopyDataProps);
                // Stack: obj ...
                for mark in exits {
                    chunk.fixup(mark)?;
                }
                Ok(CompilerStatusFlags::new().abrupt())
            }
        }
    }
}

impl PropertyName {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            PropertyName::LiteralPropertyName(lpn) => lpn.compile(chunk),
            PropertyName::ComputedPropertyName(cpn) => cpn.compile(chunk, strict),
        }
    }

    pub fn is_literal_proto(&self) -> bool {
        match self {
            PropertyName::LiteralPropertyName(lpn) => lpn.is_literal_proto(),
            PropertyName::ComputedPropertyName(_) => false,
        }
    }
}

impl LiteralPropertyName {
    pub fn compile(&self, chunk: &mut Chunk) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            LiteralPropertyName::IdentifierName(id) => {
                let idx = chunk.add_to_string_pool(id.string_value.clone())?;
                chunk.op_plus_arg(Insn::String, idx);
                Ok(CompilerStatusFlags::new())
            }
            LiteralPropertyName::StringLiteral(st) => {
                let idx = chunk.add_to_string_pool(st.value.clone())?;
                chunk.op_plus_arg(Insn::String, idx);
                Ok(CompilerStatusFlags::new())
            }
            LiteralPropertyName::NumericLiteral(n) => {
                let name = JSString::try_from(ECMAScriptValue::from(n)).unwrap();
                let idx = chunk.add_to_string_pool(name)?;
                chunk.op_plus_arg(Insn::String, idx);
                Ok(CompilerStatusFlags::new())
            }
        }
    }

    pub fn is_literal_proto(&self) -> bool {
        match self {
            LiteralPropertyName::IdentifierName(id) => id.string_value == "__proto__",
            LiteralPropertyName::StringLiteral(sl) => sl.value == "__proto__",
            LiteralPropertyName::NumericLiteral(_) => false,
        }
    }
}

impl ComputedPropertyName {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        let mut exits = vec![];
        // Stack: ...
        let ComputedPropertyName::AssignmentExpression(ae) = self;
        let status = ae.compile(chunk, strict)?;
        // Stack: exprValue ...
        if status.can_be_reference {
            chunk.op(Insn::GetValue);
        }
        if status.can_be_abrupt || status.can_be_reference {
            let mark = chunk.op_jump(Insn::JumpIfAbrupt);
            exits.push(mark);
        }
        // Stack:: name ...
        chunk.op(Insn::ToPropertyKey);
        // Stack:: key ...

        for mark in exits {
            chunk.fixup(mark)?;
        }
        Ok(CompilerStatusFlags::new().abrupt())
    }
}

impl MemberExpression {
    /// See [EvaluatePropertyAccessWithIdentifierKey ](https://tc39.es/ecma262/#sec-evaluate-property-access-with-identifier-key)
    fn evaluate_property_access_with_identifier_key(chunk: &mut Chunk, identifier_name: &IdentifierData, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        // Stack: base ...
        let idx = chunk.add_to_string_pool(identifier_name.string_value.clone())?;
        chunk.op_plus_arg(Insn::String, idx);
        // Stack: name base ...
        chunk.op(if strict { Insn::StrictRef } else { Insn::Ref });
        // Stack: ref
        Ok(CompilerStatusFlags::new().reference())
    }

    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match &self.kind {
            MemberExpressionKind::PrimaryExpression(pe) => pe.compile(chunk, strict),
            MemberExpressionKind::IdentifierName(me, id) => {
                let mut mark = None;
                let mut might_be_abrupt = false;
                let status = me.compile(chunk, strict)?;
                if status.can_be_reference {
                    chunk.op(Insn::GetValue);
                }
                if status.can_be_abrupt || status.can_be_reference {
                    mark = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                    might_be_abrupt = true;
                }
                let status = Self::evaluate_property_access_with_identifier_key(chunk, id, strict)?;
                if let Some(mark) = mark {
                    chunk.fixup(mark)?;
                }
                Ok(CompilerStatusFlags { can_be_abrupt: status.can_be_abrupt || might_be_abrupt, can_be_reference: status.can_be_reference })
            }
            MemberExpressionKind::Expression(_, _) => todo!(),
            MemberExpressionKind::TemplateLiteral(_, _) => todo!(),
            MemberExpressionKind::SuperProperty(_) => todo!(),
            MemberExpressionKind::MetaProperty(_) => todo!(),
            MemberExpressionKind::NewArguments(_, _) => todo!(),
            MemberExpressionKind::PrivateId(_, _) => todo!(),
        }
    }
}

impl NewExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match &self.kind {
            NewExpressionKind::MemberExpression(me) => me.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl LeftHandSideExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            LeftHandSideExpression::New(ne) => ne.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl UpdateExpression {
    fn post_op(chunk: &mut Chunk, strict: bool, exp: &Rc<LeftHandSideExpression>, insn: Insn) -> anyhow::Result<CompilerStatusFlags> {
        // Stack: ...
        let status = exp.compile(chunk, strict)?;
        assert!(status.can_be_reference); // Early errors eliminate non-refs

        // Stack: lref/err1 ...
        chunk.op(Insn::Dup);
        // Stack: lref/err1 lref/err1 ...
        chunk.op(Insn::GetValue);
        // Stack: lval/err1/2 lref/err1
        let mark = chunk.op_jump(Insn::JumpIfNormal);
        // Stack: err1/2 lref/err1 ...
        chunk.op(Insn::Swap);
        // Stack: lref/err1 err1/2
        chunk.op(Insn::Pop);
        // Stack: err1/2 ...
        let exit1 = chunk.op_jump(Insn::Jump);
        chunk.fixup(mark)?;
        // Stack: lval lref ...
        chunk.op(Insn::ToNumeric);
        let mark = chunk.op_jump(Insn::JumpIfNormal);
        // Stack: err lref ...
        chunk.op(Insn::Swap);
        // Stack: lref err ...
        chunk.op(Insn::Pop);
        // Stack: err ...
        let exit2 = chunk.op_jump(Insn::Jump);
        chunk.fixup(mark)?;
        // Stack: oldValue lref ...
        chunk.op(Insn::Pop2Push3);
        // Stack: oldValue lref oldValue ...
        chunk.op(insn);
        // Stack: newValue lref oldValue ...
        chunk.op(Insn::PutValue);
        // Stack: [empty]/err oldValue ...
        chunk.op(Insn::UpdateEmpty);
        // Stack: oldValue/err ...

        chunk.fixup(exit1)?;
        chunk.fixup(exit2)?;

        Ok(CompilerStatusFlags::new().abrupt())
    }

    fn pre_op(chunk: &mut Chunk, strict: bool, exp: &Rc<UnaryExpression>, insn: Insn) -> anyhow::Result<CompilerStatusFlags> {
        // Stack: ...
        exp.compile(chunk, strict)?;
        // Stack: exp/err
        chunk.op(insn);
        Ok(CompilerStatusFlags::new().abrupt())
    }

    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            UpdateExpression::LeftHandSideExpression(lhse) => lhse.compile(chunk, strict),
            UpdateExpression::PostIncrement(exp) => Self::post_op(chunk, strict, exp, Insn::Increment),
            UpdateExpression::PostDecrement(exp) => Self::post_op(chunk, strict, exp, Insn::Decrement),
            UpdateExpression::PreIncrement(exp) => Self::pre_op(chunk, strict, exp, Insn::PreIncrement),
            UpdateExpression::PreDecrement(exp) => Self::pre_op(chunk, strict, exp, Insn::PreDecrement),
        }
    }
}

impl UnaryExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            UnaryExpression::UpdateExpression(ue) => ue.compile(chunk, strict),
            UnaryExpression::Delete(exp) => {
                exp.compile(chunk, strict)?;
                chunk.op(Insn::Delete);
                Ok(CompilerStatusFlags::new().abrupt())
            }
            UnaryExpression::Void(exp) => {
                exp.compile(chunk, strict)?;
                chunk.op(Insn::Void);
                Ok(CompilerStatusFlags::new().abrupt())
            }
            UnaryExpression::Typeof(_) => todo!(),
            UnaryExpression::NoOp(_) => todo!(),
            UnaryExpression::Negate(_) => todo!(),
            UnaryExpression::Complement(_) => todo!(),
            UnaryExpression::Not(_) => todo!(),
            UnaryExpression::Await(_) => todo!(),
        }
    }
}

impl ExponentiationExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            ExponentiationExpression::UnaryExpression(ue) => ue.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl MultiplicativeExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            MultiplicativeExpression::ExponentiationExpression(ee) => ee.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl AdditiveExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            AdditiveExpression::MultiplicativeExpression(me) => me.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl ShiftExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            ShiftExpression::AdditiveExpression(ae) => ae.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl RelationalExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            RelationalExpression::ShiftExpression(se) => se.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl EqualityExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            EqualityExpression::RelationalExpression(re) => re.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl BitwiseANDExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            BitwiseANDExpression::EqualityExpression(ee) => ee.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl BitwiseXORExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            BitwiseXORExpression::BitwiseANDExpression(bae) => bae.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl BitwiseORExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            BitwiseORExpression::BitwiseXORExpression(bxe) => bxe.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl LogicalANDExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            LogicalANDExpression::BitwiseORExpression(boe) => boe.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl LogicalORExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            LogicalORExpression::LogicalANDExpression(lae) => lae.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl ShortCircuitExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            ShortCircuitExpression::LogicalORExpression(loe) => loe.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl ConditionalExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            ConditionalExpression::FallThru(sce) => sce.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl AssignmentExpression {
    #[allow(unused_assignments)]
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
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
                let mut exits = vec![];
                let status = lhse.compile(chunk, strict)?;
                if status.can_be_abrupt {
                    let mark = chunk.op_jump(Insn::JumpIfAbrupt);
                    exits.push(mark);
                }
                // Stack: lref ...
                if ae.is_anonymous_function_definition() && lhse.is_identifier_ref() {
                    todo!()
                } else {
                    let status = ae.compile(chunk, strict)?;
                    // Stack: rref lref ...
                    if status.can_be_reference {
                        chunk.op(Insn::GetValue);
                    }
                    if status.can_be_abrupt || status.can_be_reference {
                        let close = chunk.op_jump(Insn::JumpIfNormal);
                        // (haven't jumped) Stack: err lref
                        chunk.op(Insn::Swap);
                        // Stack: lref err
                        chunk.op(Insn::Pop);
                        // Stack: err
                        let mark2 = chunk.op_jump(Insn::Jump);
                        exits.push(mark2);
                        chunk.fixup(close)?;
                    }
                }
                // Stack: rval lref ...
                chunk.op(Insn::Pop2Push3);
                // Stack: rval lref rval ...
                chunk.op(Insn::PutValue);
                // Stack: empty rval ...
                chunk.op(Insn::UpdateEmpty);
                // Stack: rval ...

                for mark in exits {
                    chunk.fixup(mark)?;
                }
                Ok(CompilerStatusFlags::new().abrupt())
            }
            _ => todo!(),
        }
    }
}

impl Expression {
    #[allow(unused_variables)]
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            Expression::FallThru(ae) => ae.compile(chunk, strict),
            Expression::Comma(e, ae) => todo!(),
        }
    }
}

impl ExpressionStatement {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        let ExpressionStatement::Expression(node) = self;
        let status = node.compile(chunk, strict)?;
        if status.can_be_reference {
            chunk.op(Insn::GetValue);
        }
        Ok(CompilerStatusFlags { can_be_abrupt: status.can_be_abrupt || status.can_be_reference, can_be_reference: false })
    }
}

impl StatementList {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            StatementList::Item(sli) => sli.compile(chunk, strict),
            StatementList::List(sl, sli) => {
                let mut mark = None;
                let status = sl.compile(chunk, strict)?;
                if status.can_be_abrupt {
                    mark = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                let second_status = sli.compile(chunk, strict)?;
                chunk.op(Insn::UpdateEmpty);
                if let Some(mark) = mark {
                    chunk.fixup(mark)?;
                }
                Ok(CompilerStatusFlags { can_be_abrupt: status.can_be_abrupt || second_status.can_be_abrupt, can_be_reference: second_status.can_be_reference })
            }
        }
    }
}

impl StatementListItem {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            StatementListItem::Statement(stmt) => stmt.compile(chunk, strict),
            StatementListItem::Declaration(decl) => decl.compile(chunk, strict),
        }
    }
}

impl Statement {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            Statement::Expression(exp) => exp.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl Declaration {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            Declaration::Class(_) => todo!(),
            Declaration::Hoistable(_) => todo!(),
            Declaration::Lexical(lex) => lex.compile(chunk, strict),
        }
    }
}

impl LexicalDeclaration {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        let mut mark = None;
        let LexicalDeclaration::List(_, bi) = self;
        let status = bi.compile(chunk, strict)?;
        if status.can_be_abrupt {
            mark = Some(chunk.op_jump(Insn::JumpIfAbrupt));
        }
        chunk.op(Insn::Pop);
        chunk.op(Insn::Empty);
        if let Some(mark) = mark {
            chunk.fixup(mark)?;
        }
        Ok(CompilerStatusFlags { can_be_abrupt: status.can_be_abrupt, can_be_reference: false })
    }
}

impl BindingList {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            BindingList::Item(item) => item.compile(chunk, strict),
            BindingList::List(lst, item) => {
                let mut mark = None;
                let status = lst.compile(chunk, strict)?;
                if status.can_be_abrupt {
                    mark = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                chunk.op(Insn::Pop);
                let second_status = item.compile(chunk, strict)?;
                if let Some(mark) = mark {
                    chunk.fixup(mark)?;
                }
                Ok(CompilerStatusFlags { can_be_abrupt: status.can_be_abrupt || second_status.can_be_abrupt, can_be_reference: false })
            }
        }
    }
}

impl LexicalBinding {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
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
                            let status = izer.compile(chunk, strict)?;
                            // Stack: rref lhs ...
                            if status.can_be_reference {
                                chunk.op(Insn::GetValue);
                                // Stack: value lhs ...
                            }
                            if status.can_be_abrupt || status.can_be_reference {
                                let normal = chunk.op_jump(Insn::JumpIfNormal);
                                // Stack: err lhs ...
                                chunk.op(Insn::Swap);
                                // Stack: lhs err ...
                                chunk.op(Insn::Pop);
                                // Stack: err ...
                                let exit_tgt = Some(chunk.op_jump(Insn::Jump));
                                chunk.fixup(normal)?;
                                exit_tgt
                            } else {
                                None
                            }
                        }
                    }
                };
                // Stack: value lhs ...
                chunk.op(Insn::InitializeReferencedBinding);
                // Stack: empty ...
                if let Some(mark) = exit_tgt {
                    chunk.fixup(mark)?;
                }
                Ok(CompilerStatusFlags::new().abrupt())
            }
            LexicalBinding::Pattern(_, _) => todo!(),
        }
    }
}

impl Initializer {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        let Initializer::AssignmentExpression(ae) = self;
        ae.compile(chunk, strict)
    }
}

impl Script {
    pub fn compile(&self, chunk: &mut Chunk) -> anyhow::Result<CompilerStatusFlags> {
        match &self.0 {
            None => Ok(CompilerStatusFlags::new()),
            Some(sb) => sb.compile(chunk),
        }
    }
}

impl ScriptBody {
    pub fn compile(&self, chunk: &mut Chunk) -> anyhow::Result<CompilerStatusFlags> {
        let strict = self.contains_use_strict();
        self.statement_list.compile(chunk, strict)
    }
}

#[cfg(test)]
mod tests;
