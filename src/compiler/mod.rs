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
use num::BigInt;
use num_enum::IntoPrimitive;
use num_enum::TryFromPrimitive;
use std::fmt;
use std::rc::Rc;

pub type Opcode = u16;

#[derive(Debug, Copy, Clone, PartialEq, IntoPrimitive, TryFromPrimitive)]
#[repr(u16)]
pub enum Insn {
    Nop,
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
    Call,
    UpdateEmpty,
    Swap,
    Pop,
    Pop2Push3,
    Dup,
    Unwind,
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
    TypeOf,
    UnaryPlus,
    UnaryMinus,
    UnaryComplement,
    UnaryNot,
    Exponentiate,
    Multiply,
    Divide,
    Modulo,
    Add,
    Subtract,
}

impl fmt::Display for Insn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.pad(match self {
            Insn::Nop => "NOP",
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
            Insn::Call => "CALL",
            Insn::UpdateEmpty => "UPDATE_EMPTY",
            Insn::Swap => "SWAP",
            Insn::Pop => "POP",
            Insn::Pop2Push3 => "POP2_PUSH3",
            Insn::Dup => "DUP",
            Insn::Unwind => "UNWIND",
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
            Insn::TypeOf => "TYPEOF",
            Insn::UnaryPlus => "UNARY_PLUS",
            Insn::UnaryMinus => "UNARY_MINUS",
            Insn::UnaryComplement => "UNARY_COMPLEMENT",
            Insn::UnaryNot => "UNARY_NOT",
            Insn::Exponentiate => "EXPONENTIATE",
            Insn::Multiply => "MULTIPLY",
            Insn::Divide => "DIVIDE",
            Insn::Modulo => "MODULO",
            Insn::Add => "ADD",
            Insn::Subtract => "SUBTRACT",
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
        Self::default()
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
        let string_id = chunk.add_to_string_pool(match self {
            IdentifierReference::Identifier { identifier: id, .. } => id.string_value(),
            IdentifierReference::Yield { .. } => "yield".into(),
            IdentifierReference::Await { .. } => "await".into(),
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
            PrimaryExpression::IdentifierReference { node: id } => id.compile(chunk, strict),
            PrimaryExpression::This { .. } => {
                // Runtime Semantics: Evaluation
                //  PrimaryExpression : this
                //      1. Return ? ResolveThisBinding().
                chunk.op(Insn::This);
                Ok(CompilerStatusFlags::new().abrupt())
            }
            PrimaryExpression::Literal { node: lit } => lit.compile(chunk),
            PrimaryExpression::Parenthesized { node: exp } => exp.compile(chunk, strict),
            PrimaryExpression::ObjectLiteral { node: ol } => ol.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl Literal {
    /// Generate the code for Literal
    ///
    /// See [Evaluation for Literal](https://tc39.es/ecma262/#sec-literals-runtime-semantics-evaluation) from ECMA-262.
    pub fn compile(&self, chunk: &mut Chunk) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            Literal::NullLiteral { .. } => {
                // Literal : NullLiteral
                //  1. Return null.
                chunk.op(Insn::Null);
            }
            Literal::BooleanLiteral { val: is_true, .. } => {
                // Literal : BooleanLiteral
                //  1. If BooleanLiteral is the token false, return false.
                //  2. If BooleanLiteral is the token true, return true.
                chunk.op(if *is_true { Insn::True } else { Insn::False });
            }
            Literal::StringLiteral { val: s, .. } => {
                // Literal : StringLiteral
                //  1. Return the SV of StringLiteral as defined in [12.8.4.2](https://tc39.es/ecma262/#sec-static-semantics-sv).
                let idx = chunk.add_to_string_pool(s.value.clone())?;
                chunk.op_plus_arg(Insn::String, idx);
            }
            Literal::NumericLiteral { val: numeric, .. } => {
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
            Literal::DebugLiteral { val: ch, .. } => {
                if cfg!(test) {
                    match *ch {
                        '@' => {
                            // Break future jumps (by adding enough instructions that the offsets don't fit in an i16)
                            for _ in 0..32768 {
                                chunk.op(Insn::Nop);
                            }
                            chunk.op(Insn::False);
                        }
                        '!' => {
                            // Fill the string table.
                            chunk.strings.resize(65536, JSString::from(""));
                            chunk.op(Insn::False);
                        }
                        '#' => {
                            // Fill the float table.
                            chunk.floats.resize(65536, 10.1);
                            chunk.op(Insn::False);
                        }
                        '$' => {
                            // Fill the bigint table.
                            chunk
                                .bigints
                                .resize(65536, Rc::new(BigInt::from(97687897890734187890106587314876543219_u128)));
                            chunk.op(Insn::False);
                        }
                        _ => (),
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
        self.exp.compile(chunk, strict)
    }
}

impl ObjectLiteral {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            ObjectLiteral::Empty { .. } => {
                chunk.op(Insn::Object);
                Ok(CompilerStatusFlags::new())
            }
            ObjectLiteral::Normal { pdl, .. } | ObjectLiteral::TrailingComma { pdl, .. } => {
                // Stack: ...
                chunk.op(Insn::Object);
                // Stack: obj ...
                pdl.property_definition_evaluation(chunk, strict)
            }
        }
    }
}

impl PropertyDefinitionList {
    pub fn property_definition_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
    ) -> anyhow::Result<CompilerStatusFlags> {
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
                Ok(CompilerStatusFlags {
                    can_be_abrupt: first.can_be_abrupt || second.can_be_abrupt,
                    can_be_reference: second.can_be_reference,
                })
            }
        }
    }
}

impl PropertyDefinition {
    pub fn property_definition_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
    ) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            PropertyDefinition::IdentifierReference(idr) => {
                // Stack: obj ...
                let name = idr.string_value();
                let name_idx = chunk.add_to_string_pool(name)?;
                chunk.op_plus_arg(Insn::String, name_idx);
                // Stack: name obj ...

                // Following is unwrapped: idr compilation only fails if the string table is full and we can't insert,
                // but since we _just_ inserted this string, above, it will never have a problem.
                let status = idr.compile(chunk, strict).unwrap();
                // idr always returns a reference (it's in the name), so don't bother with the logic of when it would
                // otherwise be false.
                assert!(status.can_be_reference);

                // Stack: ref name obj ...
                chunk.op(Insn::GetValue);
                // Stack: value name obj ...
                let isok = chunk.op_jump(Insn::JumpIfNormal);
                // Stack: err name obj ...
                chunk.op_plus_arg(Insn::Unwind, 2);
                // Stack: err ...
                let escape = chunk.op_jump(Insn::Jump);
                chunk.fixup(isok).unwrap(); // two instructions is never gonna be too far.
                                            // Stack: value name obj ...
                chunk.op(Insn::CreateDataProperty);
                // Stack: obj ...
                chunk.fixup(escape).unwrap(); // one instruction. won't overrun.
                Ok(CompilerStatusFlags::new().abrupt())
            }
            PropertyDefinition::CoverInitializedName(_) => unreachable!(),
            PropertyDefinition::PropertyNameAssignmentExpression(pn, ae) => {
                let mut exit_status = CompilerStatusFlags::new();
                let mut exits = Vec::with_capacity(2);
                let is_proto_setter = pn.is_literal_proto();
                // Stack: obj ...
                if !is_proto_setter {
                    let status = pn.compile(chunk, strict)?;
                    // Stack: propKey obj ...
                    if status.can_be_abrupt {
                        let mark = chunk.op_jump(Insn::JumpIfNormal);
                        // Stack: err obj ...
                        chunk.op_plus_arg(Insn::Unwind, 1);
                        // Stack: err ...
                        exits.push(chunk.op_jump(Insn::Jump));
                        chunk.fixup(mark).unwrap();
                        exit_status = exit_status.abrupt();
                    }
                }
                // Stack: propKey obj ...
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
                        chunk.op_plus_arg(Insn::Unwind, 2);
                        // Stack: err ...
                        exits.push(chunk.op_jump(Insn::Jump));
                        chunk.fixup(mark).unwrap();
                        exit_status = exit_status.abrupt();
                    }
                }
                if is_proto_setter {
                    // Stack: propValue obj ...
                    chunk.op(Insn::SetPrototype);
                } else {
                    // Stack: propValue propKey obj ...
                    chunk.op(Insn::CreateDataProperty);
                }
                // Stack: obj ...
                for mark in exits {
                    chunk.fixup(mark)?;
                }
                Ok(exit_status)
            }
            PropertyDefinition::MethodDefinition(_) => todo!(),
            PropertyDefinition::AssignmentExpression(ae, _) => {
                // Stack: obj ...
                let status = ae.compile(chunk, strict)?;
                // Stack: exprValue obj ...
                if status.can_be_reference {
                    chunk.op(Insn::GetValue);
                }
                let exit = if status.can_be_abrupt || status.can_be_reference {
                    let close = chunk.op_jump(Insn::JumpIfNormal);
                    // Stack: err obj ...
                    chunk.op_plus_arg(Insn::Unwind, 1);
                    // Stack: err ...
                    let exit = Some(chunk.op_jump(Insn::Jump));
                    chunk.fixup(close).unwrap();
                    exit
                } else {
                    None
                };
                // Stack: fromValue obj ...
                chunk.op(Insn::CopyDataProps);
                // Stack: obj ...
                if let Some(mark) = exit {
                    chunk.fixup(mark).unwrap();
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
            LiteralPropertyName::IdentifierName { data: id, .. } => {
                let idx = chunk.add_to_string_pool(id.string_value.clone())?;
                chunk.op_plus_arg(Insn::String, idx);
                Ok(CompilerStatusFlags::new())
            }
            LiteralPropertyName::StringLiteral { data: st, .. } => {
                let idx = chunk.add_to_string_pool(st.value.clone())?;
                chunk.op_plus_arg(Insn::String, idx);
                Ok(CompilerStatusFlags::new())
            }
            LiteralPropertyName::NumericLiteral { data: n, .. } => {
                let name = JSString::try_from(ECMAScriptValue::from(n)).unwrap();
                let idx = chunk.add_to_string_pool(name)?;
                chunk.op_plus_arg(Insn::String, idx);
                Ok(CompilerStatusFlags::new())
            }
        }
    }

    pub fn is_literal_proto(&self) -> bool {
        match self {
            LiteralPropertyName::IdentifierName { data: id, .. } => id.string_value == "__proto__",
            LiteralPropertyName::StringLiteral { data: sl, .. } => sl.value == "__proto__",
            LiteralPropertyName::NumericLiteral { .. } => false,
        }
    }
}

impl ComputedPropertyName {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        let mut exits = vec![];
        // Stack: ...
        let status = self.ae.compile(chunk, strict)?;
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
            chunk.fixup(mark).unwrap();
        }
        Ok(CompilerStatusFlags::new().abrupt())
    }
}

impl MemberExpression {
    /// See [EvaluatePropertyAccessWithIdentifierKey](https://tc39.es/ecma262/#sec-evaluate-property-access-with-identifier-key)
    fn evaluate_property_access_with_identifier_key(
        chunk: &mut Chunk,
        identifier_name: &IdentifierData,
        strict: bool,
    ) -> anyhow::Result<CompilerStatusFlags> {
        // Stack: base ...
        let idx = chunk.add_to_string_pool(identifier_name.string_value.clone())?;
        chunk.op_plus_arg(Insn::String, idx);
        // Stack: name base ...
        chunk.op(if strict { Insn::StrictRef } else { Insn::Ref });
        // Stack: ref
        Ok(CompilerStatusFlags::new().reference())
    }

    /// See [EvaluatePropertyAccessWithExpressionKey](https://tc39.es/ecma262/#sec-evaluate-property-access-with-expression-key)
    fn evaluate_property_access_with_expression_key(
        chunk: &mut Chunk,
        expression: &Rc<Expression>,
        strict: bool,
    ) -> anyhow::Result<CompilerStatusFlags> {
        let mut exits = vec![];
        // Stack: base ...
        let state = expression.compile(chunk, strict)?;
        // Stack: propertyNameReference/error1 base ...
        if state.can_be_reference {
            chunk.op(Insn::GetValue);
        }
        // Stack: propertyNameValue/error1/error2 base ...
        if state.can_be_abrupt || state.can_be_reference {
            let norm = chunk.op_jump(Insn::JumpIfNormal);
            // Stack: error1/error2 base ...
            chunk.op_plus_arg(Insn::Unwind, 1);
            // stack: error1/error2 ...
            let exit = chunk.op_jump(Insn::Jump);
            exits.push(exit);
            chunk.fixup(norm).unwrap();
        }
        // Stack: nameValue base ...
        chunk.op(Insn::ToPropertyKey);
        // Stack: key/err base ...
        let norm = chunk.op_jump(Insn::JumpIfNormal);
        chunk.op_plus_arg(Insn::Unwind, 1);
        let exit = chunk.op_jump(Insn::Jump);
        exits.push(exit);
        chunk.fixup(norm).unwrap();

        // Stack: key base ...
        chunk.op(if strict { Insn::StrictRef } else { Insn::Ref });
        // Stack: ref ...

        for exit in exits {
            chunk.fixup(exit).unwrap();
        }
        Ok(CompilerStatusFlags::new().abrupt().reference())
    }

    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            MemberExpression::PrimaryExpression(pe) => pe.compile(chunk, strict),
            MemberExpression::IdentifierName(me, id) => {
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
                    chunk.fixup(mark).unwrap();
                }
                Ok(CompilerStatusFlags {
                    can_be_abrupt: status.can_be_abrupt || might_be_abrupt,
                    can_be_reference: true,
                })
            }
            MemberExpression::Expression(me, exp) => {
                // Stack: ...
                let status = me.compile(chunk, strict)?;
                // Stack: base/err ...
                if status.can_be_reference {
                    chunk.op(Insn::GetValue);
                }
                let exit = if status.can_be_abrupt || status.can_be_reference {
                    Some(chunk.op_jump(Insn::JumpIfAbrupt))
                } else {
                    None
                };
                // Stack: base ...
                let status = Self::evaluate_property_access_with_expression_key(chunk, exp, strict)?;
                // expressions are always: abrupt/ref, so we can avoid further boolean logic.
                assert!(status.can_be_abrupt && status.can_be_reference);

                // Stack: ref/err ...
                if let Some(mark) = exit {
                    chunk.fixup(mark)?;
                }
                Ok(CompilerStatusFlags::new().abrupt().reference())
            }
            MemberExpression::TemplateLiteral(_, _) => todo!(),
            MemberExpression::SuperProperty(_) => todo!(),
            MemberExpression::MetaProperty(_) => todo!(),
            MemberExpression::NewArguments(_, _) => todo!(),
            MemberExpression::PrivateId(_, _) => todo!(),
        }
    }
}

impl NewExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            NewExpression::MemberExpression(me) => me.compile(chunk, strict),
            _ => todo!(),
        }
    }
}

impl CallExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            CallExpression::CallMemberExpression(cme) => cme.compile(chunk, strict),
            CallExpression::SuperCall(_) => todo!(),
            CallExpression::ImportCall(_) => todo!(),
            CallExpression::CallExpressionArguments(_, _) => todo!(),
            CallExpression::CallExpressionExpression(_, _) => todo!(),
            CallExpression::CallExpressionIdentifierName(_, _) => todo!(),
            CallExpression::CallExpressionTemplateLiteral(_, _) => todo!(),
            CallExpression::CallExpressionPrivateId(_, _) => todo!(),
        }
    }
}

impl CallMemberExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        let mut exits = vec![];
        // Stack: ...
        let status = self.member_expression.compile(chunk, strict)?;
        // Stack: ref/err ...
        chunk.op(Insn::Dup);
        // Stack: ref/err ref/err ...
        if status.can_be_reference {
            chunk.op(Insn::GetValue);
        }
        if status.can_be_abrupt || status.can_be_reference {
            let happy = chunk.op_jump(Insn::JumpIfNormal);
            // Stack: err err ...
            chunk.op_plus_arg(Insn::Unwind, 1);
            exits.push(chunk.op_jump(Insn::Jump));
            chunk.fixup(happy).unwrap();
        }
        // Stack: func ref ...
        let arg_status = self.arguments.argument_list_evaluation(chunk, strict)?;
        // Stack: N arg(n-1) arg(n-2) ... arg1 arg0 func ref ...
        // or: Stack: err func ref ...
        if arg_status.can_be_abrupt {
            let happy = chunk.op_jump(Insn::JumpIfNormal);
            chunk.op_plus_arg(Insn::Unwind, 2);
            exits.push(chunk.op_jump(Insn::Jump));
            chunk.fixup(happy).unwrap();
        }
        chunk.op(Insn::Call);
        for mark in exits {
            chunk.fixup(mark)?;
        }
        Ok(CompilerStatusFlags::new().abrupt())
    }
}

impl LeftHandSideExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            LeftHandSideExpression::New(ne) => ne.compile(chunk, strict),
            LeftHandSideExpression::Call(ce) => ce.compile(chunk, strict),
            LeftHandSideExpression::Optional(_) => todo!(),
        }
    }
}

impl Arguments {
    pub fn argument_list_evaluation(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            Arguments::Empty => {
                let index = chunk.add_to_float_pool(0.0)?;
                chunk.op_plus_arg(Insn::Float, index);
                Ok(CompilerStatusFlags::new())
            }
            Arguments::ArgumentList(al) | Arguments::ArgumentListComma(al) => {
                let (arg_list_len, status) = al.argument_list_evaluation(chunk, strict)?;
                let exit = if status.can_be_abrupt {
                    // Stack: arg(n) arg(n-1) arg(n-2) ... arg2 arg1 ...
                    // or Stack: err ...
                    Some(chunk.op_jump(Insn::JumpIfAbrupt))
                } else {
                    None
                };
                let index = chunk.add_to_float_pool(arg_list_len as f64)?;
                chunk.op_plus_arg(Insn::Float, index);
                if let Some(mark) = exit {
                    chunk.fixup(mark).unwrap();
                }
                Ok(CompilerStatusFlags { can_be_abrupt: status.can_be_abrupt, can_be_reference: false })
            }
        }
    }
}

impl ArgumentList {
    pub fn argument_list_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
    ) -> anyhow::Result<(u16, CompilerStatusFlags)> {
        match self {
            ArgumentList::FallThru(item) => {
                // Stack: ...
                let status = item.compile(chunk, strict)?;
                // Stack: ref/err ...
                if status.can_be_reference {
                    chunk.op(Insn::GetValue);
                }
                // Stack val/err ...
                Ok((
                    1,
                    CompilerStatusFlags {
                        can_be_abrupt: status.can_be_abrupt || status.can_be_reference,
                        can_be_reference: false,
                    },
                ))
            }
            ArgumentList::Dots(_) => todo!(),
            ArgumentList::ArgumentList(lst, item) => {
                // Stack: ...
                let (prev_count, status) = lst.argument_list_evaluation(chunk, strict)?;
                assert!(!status.can_be_reference);
                // Stack: val(N) val(N-1) ... val(0) ...
                // or err ...
                let exit = if status.can_be_abrupt { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                let status2 = item.compile(chunk, strict)?;
                // Stack: val/err val(n) val(n-1) ... val(0) ...
                if status2.can_be_reference {
                    chunk.op(Insn::GetValue);
                }
                if status2.can_be_reference || status2.can_be_abrupt {
                    let happy = chunk.op_jump(Insn::JumpIfNormal);
                    chunk.op_plus_arg(Insn::Unwind, prev_count);
                    chunk.fixup(happy).unwrap();
                }
                if let Some(mark) = exit {
                    chunk.fixup(mark)?;
                }
                Ok((
                    prev_count + 1,
                    CompilerStatusFlags {
                        can_be_abrupt: status.can_be_abrupt || status2.can_be_abrupt || status2.can_be_reference,
                        can_be_reference: false,
                    },
                ))
            }
            ArgumentList::ArgumentListDots(_, _) => todo!(),
        }
    }
}

impl UpdateExpression {
    fn post_op(
        chunk: &mut Chunk,
        strict: bool,
        exp: &Rc<LeftHandSideExpression>,
        insn: Insn,
    ) -> anyhow::Result<CompilerStatusFlags> {
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

    fn pre_op(
        chunk: &mut Chunk,
        strict: bool,
        exp: &Rc<UnaryExpression>,
        insn: Insn,
    ) -> anyhow::Result<CompilerStatusFlags> {
        // Stack: ...
        exp.compile(chunk, strict)?;
        // Stack: exp/err
        chunk.op(insn);
        Ok(CompilerStatusFlags::new().abrupt())
    }

    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            UpdateExpression::LeftHandSideExpression(lhse) => lhse.compile(chunk, strict),
            UpdateExpression::PostIncrement { lhs: exp, .. } => Self::post_op(chunk, strict, exp, Insn::Increment),
            UpdateExpression::PostDecrement { lhs: exp, .. } => Self::post_op(chunk, strict, exp, Insn::Decrement),
            UpdateExpression::PreIncrement { ue: exp, .. } => Self::pre_op(chunk, strict, exp, Insn::PreIncrement),
            UpdateExpression::PreDecrement { ue: exp, .. } => Self::pre_op(chunk, strict, exp, Insn::PreDecrement),
        }
    }
}

impl UnaryExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        fn unary_op(
            exp: &Rc<UnaryExpression>,
            chunk: &mut Chunk,
            strict: bool,
            insn: Insn,
        ) -> anyhow::Result<CompilerStatusFlags> {
            exp.compile(chunk, strict)?;
            chunk.op(insn);
            Ok(CompilerStatusFlags::new().abrupt())
        }
        match self {
            UnaryExpression::UpdateExpression(ue) => ue.compile(chunk, strict),
            UnaryExpression::Delete { ue, .. } => unary_op(ue, chunk, strict, Insn::Delete),
            UnaryExpression::Void { ue, .. } => unary_op(ue, chunk, strict, Insn::Void),
            UnaryExpression::Typeof { ue, .. } => unary_op(ue, chunk, strict, Insn::TypeOf),
            UnaryExpression::NoOp { ue, .. } => unary_op(ue, chunk, strict, Insn::UnaryPlus),
            UnaryExpression::Negate { ue, .. } => unary_op(ue, chunk, strict, Insn::UnaryMinus),
            UnaryExpression::Complement { ue, .. } => unary_op(ue, chunk, strict, Insn::UnaryComplement),
            UnaryExpression::Not { ue, .. } => unary_op(ue, chunk, strict, Insn::UnaryNot),
            UnaryExpression::Await(_) => todo!(),
        }
    }
}

macro_rules! compile_binary_expression {
    ( $chunk:expr, $strict:expr, $left:expr, $right:expr, $op:expr ) => {{
        // Stack: ...
        let left_status = $left.compile($chunk, $strict)?;
        // Stack: err/ref/val ...
        if left_status.can_be_reference {
            $chunk.op(Insn::GetValue);
        }
        // Stack: err/val
        let first_exit = if left_status.can_be_reference || left_status.can_be_abrupt {
            Some($chunk.op_jump(Insn::JumpIfAbrupt))
        } else {
            None
        };
        // Stack: val
        let right_status = $right.compile($chunk, $strict)?;
        // Stack: err/ref/val val ...
        if right_status.can_be_reference {
            $chunk.op(Insn::GetValue);
        }
        // Stack: err/val val ...
        let second_exit = if right_status.can_be_reference || right_status.can_be_abrupt {
            let nearby = $chunk.op_jump(Insn::JumpIfNormal);
            // Stack: err val ...
            $chunk.op_plus_arg(Insn::Unwind, 1);
            // Stack: err ...
            let exit = $chunk.op_jump(Insn::Jump);
            $chunk.fixup(nearby).unwrap();
            Some(exit)
        } else {
            None
        };
        // Stack: val val ...
        $chunk.op($op);
        // Stack: result/err ...
        if let Some(mark) = first_exit {
            $chunk.fixup(mark)?;
        }
        if let Some(mark) = second_exit {
            $chunk.fixup(mark).unwrap();
        }
        Ok(CompilerStatusFlags::new().abrupt())
    }};
}

impl ExponentiationExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            ExponentiationExpression::UnaryExpression(ue) => ue.compile(chunk, strict),
            ExponentiationExpression::Exponentiation(left, right) => {
                compile_binary_expression!(chunk, strict, left, right, Insn::Exponentiate)
            }
        }
    }
}

impl MultiplicativeExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            MultiplicativeExpression::ExponentiationExpression(ee) => ee.compile(chunk, strict),
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(left, op, right) => {
                compile_binary_expression!(
                    chunk,
                    strict,
                    left,
                    right,
                    match **op {
                        MultiplicativeOperator::Multiply => Insn::Multiply,
                        MultiplicativeOperator::Divide => Insn::Divide,
                        MultiplicativeOperator::Modulo => Insn::Modulo,
                    }
                )
            }
        }
    }
}

impl AdditiveExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            AdditiveExpression::MultiplicativeExpression(me) => me.compile(chunk, strict),
            AdditiveExpression::Add(left, right) => compile_binary_expression!(chunk, strict, left, right, Insn::Add),
            AdditiveExpression::Subtract(left, right) => {
                compile_binary_expression!(chunk, strict, left, right, Insn::Subtract)
            }
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
        let status = self.exp.compile(chunk, strict)?;
        if status.can_be_reference {
            chunk.op(Insn::GetValue);
        }
        Ok(CompilerStatusFlags {
            can_be_abrupt: status.can_be_abrupt || status.can_be_reference,
            can_be_reference: false,
        })
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
                Ok(CompilerStatusFlags {
                    can_be_abrupt: status.can_be_abrupt || second_status.can_be_abrupt,
                    can_be_reference: second_status.can_be_reference,
                })
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
                Ok(CompilerStatusFlags {
                    can_be_abrupt: status.can_be_abrupt || second_status.can_be_abrupt,
                    can_be_reference: false,
                })
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
        self.ae.compile(chunk, strict)
    }
}

impl Script {
    pub fn compile(&self, chunk: &mut Chunk) -> anyhow::Result<CompilerStatusFlags> {
        match &self.body {
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
