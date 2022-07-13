use super::*;
#[cfg(test)]
use num::BigInt;
use num_enum::IntoPrimitive;
use num_enum::TryFromPrimitive;
use std::fmt;
use std::rc::Rc;

pub type Opcode = u16;

#[derive(Debug, Copy, Clone, PartialEq, Eq, IntoPrimitive, TryFromPrimitive)]
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
    JumpIfFalse,
    JumpIfTrue,
    JumpIfNotNullish,
    JumpPopIfTrue,
    JumpPopIfFalse,
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
    PushNewLexEnv,
    PopLexEnv,
    CreateStrictImmutableLexBinding,
    CreatePermanentMutableLexBinding,
    InitializeLexBinding,
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
    LeftShift,
    SignedRightShift,
    UnsignedRightShift,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    InstanceOf,
    In,
    Equal,
    NotEqual,
    StrictEqual,
    StrictNotEqual,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Throw,
    HandleEmptyBreak,
    HandleTargetedBreak,
    CoalesceValue,
    LoopContinues,
    Continue,
    TargetedContinue,
    Break,
    TargetedBreak,
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
            Insn::JumpIfFalse => "JUMP_IF_FALSE",
            Insn::JumpIfTrue => "JUMP_IF_TRUE",
            Insn::JumpIfNotNullish => "JUMP_NOT_NULLISH",
            Insn::JumpPopIfTrue => "JUMPPOP_TRUE",
            Insn::JumpPopIfFalse => "JUMPPOP_FALSE",
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
            Insn::PushNewLexEnv => "PNLE",
            Insn::PopLexEnv => "PLE",
            Insn::CreateStrictImmutableLexBinding => "CSILB",
            Insn::CreatePermanentMutableLexBinding => "CPMLB",
            Insn::InitializeLexBinding => "ILB",
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
            Insn::LeftShift => "LSH",
            Insn::SignedRightShift => "SRSH",
            Insn::UnsignedRightShift => "URSH",
            Insn::Less => "LT",
            Insn::Greater => "GT",
            Insn::LessEqual => "LE",
            Insn::GreaterEqual => "GE",
            Insn::InstanceOf => "INSTANCEOF",
            Insn::In => "IN",
            Insn::Equal => "EQ",
            Insn::NotEqual => "NE",
            Insn::StrictEqual => "SEQ",
            Insn::StrictNotEqual => "SNE",
            Insn::BitwiseAnd => "AND",
            Insn::BitwiseOr => "OR",
            Insn::BitwiseXor => "XOR",
            Insn::Throw => "THROW",
            Insn::HandleEmptyBreak => "HEB",
            Insn::HandleTargetedBreak => "HTB",
            Insn::CoalesceValue => "COALESCE",
            Insn::LoopContinues => "LOOP_CONT",
            Insn::Continue => "CONTINUE",
            Insn::TargetedContinue => "CONTINUE_WITH",
            Insn::Break => "BREAK",
            Insn::TargetedBreak => "BREAK_FROM",
        })
    }
}

/// A compilation might leave a value on the runtime stack that could be a reference. We want to communicate back to the
/// parent compilation step about whether that's possible or not. The values are "Might leave a reference on top of the
/// stack" and "Will never leave a reference on the top of the stack".
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RefResult {
    Maybe,
    Never,
}

impl Default for RefResult {
    fn default() -> Self {
        Self::Never
    }
}

impl From<bool> for RefResult {
    fn from(src: bool) -> Self {
        if src {
            RefResult::Maybe
        } else {
            RefResult::Never
        }
    }
}

/// A compilation might leave a value on top of the runtime stack that could be an error. We want to communicate back to
/// the parent compilation step about whether that's possible or not. The values are "Might leave an abrupt completion
/// on top of the stack" and "Will never leave an abrupt completion on top of the stack".
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AbruptResult {
    Maybe,
    Never,
}

impl Default for AbruptResult {
    fn default() -> Self {
        Self::Never
    }
}

impl From<CompilerStatusFlags> for AbruptResult {
    fn from(src: CompilerStatusFlags) -> Self {
        src.can_be_abrupt
    }
}

impl From<bool> for AbruptResult {
    fn from(src: bool) -> Self {
        if src {
            AbruptResult::Maybe
        } else {
            AbruptResult::Never
        }
    }
}

impl From<NeverAbruptRefResult> for AbruptResult {
    fn from(_: NeverAbruptRefResult) -> Self {
        AbruptResult::Never
    }
}

impl From<AlwaysAbruptResult> for AbruptResult {
    fn from(_: AlwaysAbruptResult) -> Self {
        AbruptResult::Maybe
    }
}

impl AbruptResult {
    fn maybe_abrupt(&self) -> bool {
        *self == AbruptResult::Maybe
    }
    fn maybe_ref(&self) -> bool {
        false
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct CompilerStatusFlags {
    pub can_be_abrupt: AbruptResult,
    pub can_be_reference: RefResult,
}
impl CompilerStatusFlags {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn abrupt(self, potentially_abrupt: bool) -> Self {
        Self { can_be_abrupt: potentially_abrupt.into(), ..self }
    }
    pub fn reference(self, potentially_reference: bool) -> Self {
        Self { can_be_reference: potentially_reference.into(), ..self }
    }
    pub fn maybe_abrupt(&self) -> bool {
        self.can_be_abrupt == AbruptResult::Maybe
    }
    pub fn maybe_ref(&self) -> bool {
        self.can_be_reference == RefResult::Maybe
    }
}

impl From<AbruptResult> for CompilerStatusFlags {
    fn from(src: AbruptResult) -> Self {
        Self { can_be_abrupt: src, ..Default::default() }
    }
}

impl From<RefResult> for CompilerStatusFlags {
    fn from(src: RefResult) -> Self {
        Self { can_be_reference: src, ..Default::default() }
    }
}

#[derive(Debug)]
pub struct AlwaysAbruptResult {}
impl From<AlwaysAbruptResult> for CompilerStatusFlags {
    fn from(_: AlwaysAbruptResult) -> Self {
        Self::new().abrupt(true)
    }
}
impl AlwaysAbruptResult {
    fn maybe_abrupt(&self) -> bool {
        true
    }
    fn maybe_ref(&self) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct AlwaysRefResult {}
impl From<AlwaysRefResult> for CompilerStatusFlags {
    fn from(_: AlwaysRefResult) -> Self {
        Self::new().reference(true)
    }
}

#[derive(Debug)]
pub struct AlwaysAbruptRefResult {}
impl From<AlwaysAbruptRefResult> for CompilerStatusFlags {
    fn from(_: AlwaysAbruptRefResult) -> Self {
        Self::new().reference(true).abrupt(true)
    }
}

#[derive(Debug)]
pub struct NeverAbruptRefResult {}
impl From<NeverAbruptRefResult> for CompilerStatusFlags {
    fn from(_: NeverAbruptRefResult) -> Self {
        Self::new()
    }
}
impl NeverAbruptRefResult {
    fn maybe_abrupt(&self) -> bool {
        false
    }
    fn maybe_ref(&self) -> bool {
        false
    }
}

impl IdentifierReference {
    /// Generate the code for IdentifierReference
    ///
    /// See [IdentifierReference Evaluation](https://tc39.es/ecma262/#sec-identifiers-runtime-semantics-evaluation) from ECMA-262.
    pub fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<AlwaysAbruptRefResult> {
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
        Ok(AlwaysAbruptRefResult {})
    }
}

impl PrimaryExpression {
    /// Generate the code for PrimaryExpression
    ///
    /// References from ECMA-262:
    /// * [Evaluation of the `this` keyword](https://tc39.es/ecma262/#sec-this-keyword-runtime-semantics-evaluation)
    #[allow(unused_variables)]
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            PrimaryExpression::IdentifierReference { node: id } => {
                id.compile(chunk, strict).map(CompilerStatusFlags::from)
            }
            PrimaryExpression::This { .. } => {
                // Runtime Semantics: Evaluation
                //  PrimaryExpression : this
                //      1. Return ? ResolveThisBinding().
                chunk.op(Insn::This);
                Ok(CompilerStatusFlags::new().abrupt(true))
            }
            PrimaryExpression::Literal { node: lit } => lit.compile(chunk).map(CompilerStatusFlags::from),
            PrimaryExpression::Parenthesized { node: exp } => exp.compile(chunk, strict, text),
            PrimaryExpression::ObjectLiteral { node: ol } => {
                ol.compile(chunk, strict, text).map(CompilerStatusFlags::from)
            }
            PrimaryExpression::ArrayLiteral { node } => todo!(),
            PrimaryExpression::TemplateLiteral { node } => todo!(),
            PrimaryExpression::Function { node } => todo!(),
            PrimaryExpression::Class { node } => todo!(),
            PrimaryExpression::Generator { node } => todo!(),
            PrimaryExpression::AsyncFunction { node } => todo!(),
            PrimaryExpression::AsyncGenerator { node } => todo!(),
            PrimaryExpression::RegularExpression { regex, location } => todo!(),
        }
    }
}

#[cfg(test)]
fn compile_debug_lit(chunk: &mut Chunk, ch: &char) {
    match *ch {
        '@' => {
            // Break future jumps (by adding enough instructions that the offsets don't fit in an i16)
            for _ in 0..32768 {
                chunk.op(Insn::Nop);
            }
            chunk.op(Insn::False);
        }
        '3' => {
            // Break some future jumps (by adding enough instructions that the larger offsets don't fit in an i16)
            for _ in 0..32768 - 3 {
                chunk.op(Insn::Nop);
            }
            chunk.op(Insn::False);
        }
        '!' => {
            // Fill the string table.
            chunk.strings.resize(65536, JSString::from("not to be used from integration tests"));
            chunk.op(Insn::False);
        }
        '#' => {
            // Fill the float table.
            chunk.floats.resize(65536, 10.1);
            chunk.op(Insn::False);
        }
        '$' => {
            // Fill the bigint table.
            chunk.bigints.resize(65536, Rc::new(BigInt::from(97687897890734187890106587314876543219_u128)));
            chunk.op(Insn::False);
        }
        _ => (),
    }
}
#[cfg(not(test))]
fn compile_debug_lit(_: &mut Chunk, _: &char) {}

impl Literal {
    /// Generate the code for Literal
    ///
    /// See [Evaluation for Literal](https://tc39.es/ecma262/#sec-literals-runtime-semantics-evaluation) from ECMA-262.
    pub fn compile(&self, chunk: &mut Chunk) -> anyhow::Result<NeverAbruptRefResult> {
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
                compile_debug_lit(chunk, ch);
            }
        }
        Ok(NeverAbruptRefResult {})
    }
}

impl ParenthesizedExpression {
    /// Generate the code for ParenthesizedExpression
    ///
    /// See [Evaluation for Grouping Operator](https://tc39.es/ecma262/#sec-grouping-operator-runtime-semantics-evaluation) from ECMA-262.
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        // Runtime Semantics: Evaluation
        //  ParenthesizedExpression : ( Expression )
        //      1. Return the result of evaluating Expression. This may be of type Reference.
        //
        // NOTE | This algorithm does not apply GetValue to the result of evaluating Expression. The principal
        //      | motivation for this is so that operators such as delete and typeof may be applied to parenthesized
        //      | expressions.
        self.exp.compile(chunk, strict, text)
    }
}

impl ObjectLiteral {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match self {
            ObjectLiteral::Empty { .. } => {
                chunk.op(Insn::Object);
                Ok(AbruptResult::Never)
            }
            ObjectLiteral::Normal { pdl, .. } | ObjectLiteral::TrailingComma { pdl, .. } => {
                // Stack: ...
                chunk.op(Insn::Object);
                // Stack: obj ...
                pdl.property_definition_evaluation(chunk, strict, text)
            }
        }
    }
}

impl PropertyDefinitionList {
    pub fn property_definition_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AbruptResult> {
        match self {
            PropertyDefinitionList::OneDef(pd) => pd.property_definition_evaluation(chunk, strict, text),
            PropertyDefinitionList::ManyDefs(pdl, pd) => {
                let mut exit = None;
                let first = pdl.property_definition_evaluation(chunk, strict, text)?;
                if first == AbruptResult::Maybe {
                    exit = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                let second = pd.property_definition_evaluation(chunk, strict, text)?;
                if let Some(mark) = exit {
                    chunk.fixup(mark)?;
                }
                Ok(AbruptResult::from(first == AbruptResult::Maybe || second == AbruptResult::Maybe))
            }
        }
    }
}

impl PropertyDefinition {
    pub fn property_definition_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AbruptResult> {
        match self {
            PropertyDefinition::IdentifierReference(idr) => {
                // Stack: obj ...
                let name = idr.string_value();
                let name_idx = chunk.add_to_string_pool(name)?;
                chunk.op_plus_arg(Insn::String, name_idx);
                // Stack: name obj ...

                // Following is unwrapped: idr compilation only fails if the string table is full and we can't insert,
                // but since we _just_ inserted this string, above, it will never have a problem.
                idr.compile(chunk, strict).expect("Compilation cannot fail as string is already stored.");
                // idr always returns a reference (it's in the name), so don't bother with the logic of when it would
                // otherwise be false.

                // Stack: ref name obj ...
                chunk.op(Insn::GetValue);
                // Stack: value name obj ...
                let isok = chunk.op_jump(Insn::JumpIfNormal);
                // Stack: err name obj ...
                chunk.op_plus_arg(Insn::Unwind, 2);
                // Stack: err ...
                let escape = chunk.op_jump(Insn::Jump);
                chunk.fixup(isok).expect("Jump is too short to overflow.");
                // Stack: value name obj ...
                chunk.op(Insn::CreateDataProperty);
                // Stack: obj ...
                chunk.fixup(escape).expect("Jump is too short to overflow.");
                Ok(AbruptResult::Maybe)
            }
            PropertyDefinition::CoverInitializedName(_) => unreachable!(),
            PropertyDefinition::PropertyNameAssignmentExpression(pn, ae) => {
                let mut exit_status = AbruptResult::Never;
                let mut exits = Vec::with_capacity(2);
                let is_proto_setter = pn.is_literal_proto();
                // Stack: obj ...
                if !is_proto_setter {
                    let status = pn.compile(chunk, strict, text)?;
                    // Stack: propKey obj ...
                    if status == AbruptResult::Maybe {
                        let mark = chunk.op_jump(Insn::JumpIfNormal);
                        // Stack: err obj ...
                        chunk.op_plus_arg(Insn::Unwind, 1);
                        // Stack: err ...
                        exits.push(chunk.op_jump(Insn::Jump));
                        chunk.fixup(mark).expect("Jump is too short to overflow.");
                        exit_status = AbruptResult::Maybe;
                    }
                }
                // Stack: propKey obj ...
                if !is_proto_setter && ae.is_anonymous_function_definition() {
                    todo!();
                } else {
                    let status = ae.compile(chunk, strict, text)?;
                    if status.maybe_ref() {
                        // Stack: exprValueRef propKey obj ...
                        chunk.op(Insn::GetValue);
                    }
                    if status.maybe_abrupt() || status.maybe_ref() {
                        // Stack: propValue propKey obj ...
                        let mark = chunk.op_jump(Insn::JumpIfNormal);
                        // Stack: err propKey obj ...
                        chunk.op_plus_arg(Insn::Unwind, 2);
                        // Stack: err ...
                        exits.push(chunk.op_jump(Insn::Jump));
                        chunk.fixup(mark).expect("Jump is too short to overflow.");
                        exit_status = AbruptResult::Maybe;
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
                let status = ae.compile(chunk, strict, text)?;
                // Stack: exprValue obj ...
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                let exit = if status.maybe_abrupt() || status.maybe_ref() {
                    let close = chunk.op_jump(Insn::JumpIfNormal);
                    // Stack: err obj ...
                    chunk.op_plus_arg(Insn::Unwind, 1);
                    // Stack: err ...
                    let exit = Some(chunk.op_jump(Insn::Jump));
                    chunk.fixup(close).expect("Jump is too short to overflow.");
                    exit
                } else {
                    None
                };
                // Stack: fromValue obj ...
                chunk.op(Insn::CopyDataProps);
                // Stack: obj ...
                if let Some(mark) = exit {
                    chunk.fixup(mark).expect("Jump is too short to overflow.");
                }
                Ok(AbruptResult::Maybe)
            }
        }
    }
}

impl PropertyName {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match self {
            PropertyName::LiteralPropertyName(lpn) => lpn.compile(chunk).map(AbruptResult::from),
            PropertyName::ComputedPropertyName(cpn) => cpn.compile(chunk, strict, text).map(AbruptResult::from),
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
    pub fn compile(&self, chunk: &mut Chunk) -> anyhow::Result<NeverAbruptRefResult> {
        match self {
            LiteralPropertyName::IdentifierName { data: id, .. } => {
                let idx = chunk.add_to_string_pool(id.string_value.clone())?;
                chunk.op_plus_arg(Insn::String, idx);
                Ok(NeverAbruptRefResult {})
            }
            LiteralPropertyName::StringLiteral { data: st, .. } => {
                let idx = chunk.add_to_string_pool(st.value.clone())?;
                chunk.op_plus_arg(Insn::String, idx);
                Ok(NeverAbruptRefResult {})
            }
            LiteralPropertyName::NumericLiteral { data: n, .. } => {
                let name = JSString::try_from(ECMAScriptValue::from(n)).expect("Numbers always have string forms.");
                let idx = chunk.add_to_string_pool(name)?;
                chunk.op_plus_arg(Insn::String, idx);
                Ok(NeverAbruptRefResult {})
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
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AlwaysAbruptResult> {
        let mut exits = vec![];
        // Stack: ...
        let status = self.ae.compile(chunk, strict, text)?;
        // Stack: exprValue ...
        if status.maybe_ref() {
            chunk.op(Insn::GetValue);
        }
        if status.maybe_abrupt() || status.maybe_ref() {
            let mark = chunk.op_jump(Insn::JumpIfAbrupt);
            exits.push(mark);
        }
        // Stack:: name ...
        chunk.op(Insn::ToPropertyKey);
        // Stack:: key ...

        for mark in exits {
            chunk.fixup(mark).expect("Jump is too short to overflow.");
        }
        Ok(AlwaysAbruptResult {})
    }
}

impl MemberExpression {
    /// See [EvaluatePropertyAccessWithIdentifierKey](https://tc39.es/ecma262/#sec-evaluate-property-access-with-identifier-key)
    fn evaluate_property_access_with_identifier_key(
        chunk: &mut Chunk,
        identifier_name: &IdentifierData,
        strict: bool,
    ) -> anyhow::Result<AlwaysRefResult> {
        // Stack: base ...
        let idx = chunk.add_to_string_pool(identifier_name.string_value.clone())?;
        chunk.op_plus_arg(Insn::String, idx);
        // Stack: name base ...
        chunk.op(if strict { Insn::StrictRef } else { Insn::Ref });
        // Stack: ref
        Ok(AlwaysRefResult {})
    }

    /// See [EvaluatePropertyAccessWithExpressionKey](https://tc39.es/ecma262/#sec-evaluate-property-access-with-expression-key)
    fn evaluate_property_access_with_expression_key(
        chunk: &mut Chunk,
        expression: &Rc<Expression>,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AlwaysAbruptRefResult> {
        let mut exits = vec![];
        // Stack: base ...
        let state = expression.compile(chunk, strict, text)?;
        // Stack: propertyNameReference/error1 base ...
        if state.maybe_ref() {
            chunk.op(Insn::GetValue);
        }
        // Stack: propertyNameValue/error1/error2 base ...
        if state.maybe_abrupt() || state.maybe_ref() {
            let norm = chunk.op_jump(Insn::JumpIfNormal);
            // Stack: error1/error2 base ...
            chunk.op_plus_arg(Insn::Unwind, 1);
            // stack: error1/error2 ...
            let exit = chunk.op_jump(Insn::Jump);
            exits.push(exit);
            chunk.fixup(norm).expect("Jump is too short to overflow.");
        }
        // Stack: nameValue base ...
        chunk.op(Insn::ToPropertyKey);
        // Stack: key/err base ...
        let norm = chunk.op_jump(Insn::JumpIfNormal);
        chunk.op_plus_arg(Insn::Unwind, 1);
        let exit = chunk.op_jump(Insn::Jump);
        exits.push(exit);
        chunk.fixup(norm).expect("Jump is too short to overflow.");

        // Stack: key base ...
        chunk.op(if strict { Insn::StrictRef } else { Insn::Ref });
        // Stack: ref ...

        for exit in exits {
            chunk.fixup(exit).expect("Jump is too short to overflow.");
        }
        Ok(AlwaysAbruptRefResult {})
    }

    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            MemberExpression::PrimaryExpression(pe) => pe.compile(chunk, strict, text),
            MemberExpression::IdentifierName(me, id, ..) => {
                let mut mark = None;
                let mut might_be_abrupt = false;
                let status = me.compile(chunk, strict, text)?;
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                if status.maybe_abrupt() || status.maybe_ref() {
                    mark = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                    might_be_abrupt = true;
                }
                Self::evaluate_property_access_with_identifier_key(chunk, id, strict)?;
                if let Some(mark) = mark {
                    chunk.fixup(mark).expect("Jump is too short to overflow.");
                }
                Ok(CompilerStatusFlags::new().abrupt(might_be_abrupt).reference(true))
            }
            MemberExpression::Expression(me, exp, ..) => {
                // Stack: ...
                let status = me.compile(chunk, strict, text)?;
                // Stack: base/err ...
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                let exit = if status.maybe_abrupt() || status.maybe_ref() {
                    Some(chunk.op_jump(Insn::JumpIfAbrupt))
                } else {
                    None
                };
                // Stack: base ...
                Self::evaluate_property_access_with_expression_key(chunk, exp, strict, text)?;
                // expressions are always: abrupt/ref, so we can avoid further boolean logic.

                // Stack: ref/err ...
                if let Some(mark) = exit {
                    chunk.fixup(mark)?;
                }
                Ok(CompilerStatusFlags::new().abrupt(true).reference(true))
            }
            MemberExpression::TemplateLiteral(_, _) => todo!(),
            MemberExpression::SuperProperty(_) => todo!(),
            MemberExpression::MetaProperty(_) => todo!(),
            MemberExpression::NewArguments(..) => todo!(),
            MemberExpression::PrivateId(..) => todo!(),
        }
    }
}

impl NewExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            NewExpression::MemberExpression(me) => me.compile(chunk, strict, text),
            _ => todo!(),
        }
    }
}

impl CallExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            CallExpression::CallMemberExpression(cme) => {
                cme.compile(chunk, strict, text).map(CompilerStatusFlags::from)
            }
            CallExpression::SuperCall(_) => todo!(),
            CallExpression::ImportCall(_) => todo!(),
            CallExpression::CallExpressionArguments(_, _) => todo!(),
            CallExpression::CallExpressionExpression(_, _, _) => todo!(),
            CallExpression::CallExpressionIdentifierName(_, _, _) => todo!(),
            CallExpression::CallExpressionTemplateLiteral(_, _) => todo!(),
            CallExpression::CallExpressionPrivateId(_, _, _) => todo!(),
        }
    }
}

impl CallMemberExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AlwaysAbruptResult> {
        // On return: top of stack might be an abrupt completion, but will never be a reference.
        let mut exits = vec![];
        // Stack: ...
        let status = self.member_expression.compile(chunk, strict, text)?;
        // Stack: ref/err ...
        chunk.op(Insn::Dup);
        // Stack: ref/err ref/err ...
        if status.maybe_ref() {
            chunk.op(Insn::GetValue);
        }
        if status.maybe_abrupt() || status.maybe_ref() {
            let happy = chunk.op_jump(Insn::JumpIfNormal);
            // Stack: err err ...
            chunk.op_plus_arg(Insn::Unwind, 1);
            exits.push(chunk.op_jump(Insn::Jump));
            chunk.fixup(happy).expect("Jump is too short to overflow.");
        }
        // Stack: func ref ...
        let arg_status = self.arguments.argument_list_evaluation(chunk, strict, text)?;
        // Stack: N arg(n-1) arg(n-2) ... arg1 arg0 func ref ...
        // or: Stack: err func ref ...
        if arg_status == AbruptResult::Maybe {
            let happy = chunk.op_jump(Insn::JumpIfNormal);
            chunk.op_plus_arg(Insn::Unwind, 2);
            exits.push(chunk.op_jump(Insn::Jump));
            chunk.fixup(happy).expect("Jump is too short to overflow.");
        }
        chunk.op(Insn::Call);
        for mark in exits {
            chunk.fixup(mark)?;
        }
        Ok(AlwaysAbruptResult {})
    }
}

impl LeftHandSideExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            LeftHandSideExpression::New(ne) => ne.compile(chunk, strict, text),
            LeftHandSideExpression::Call(ce) => ce.compile(chunk, strict, text),
            LeftHandSideExpression::Optional(_) => todo!(),
        }
    }
}

impl Arguments {
    pub fn argument_list_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AbruptResult> {
        match self {
            Arguments::Empty { .. } => {
                let index = chunk.add_to_float_pool(0.0)?;
                chunk.op_plus_arg(Insn::Float, index);
                Ok(AbruptResult::Never)
            }
            Arguments::ArgumentList(al, _) | Arguments::ArgumentListComma(al, _) => {
                let (arg_list_len, status) = al.argument_list_evaluation(chunk, strict, text)?;
                let exit = if status == AbruptResult::Maybe {
                    // Stack: arg(n) arg(n-1) arg(n-2) ... arg2 arg1 ...
                    // or Stack: err ...
                    Some(chunk.op_jump(Insn::JumpIfAbrupt))
                } else {
                    None
                };
                let index = chunk.add_to_float_pool(arg_list_len as f64)?;
                chunk.op_plus_arg(Insn::Float, index);
                if let Some(mark) = exit {
                    chunk.fixup(mark).expect("Jump is too short to overflow.");
                }
                Ok(status)
            }
        }
    }
}

impl ArgumentList {
    pub fn argument_list_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<(u16, AbruptResult)> {
        match self {
            ArgumentList::FallThru(item) => {
                // Stack: ...
                let status = item.compile(chunk, strict, text)?;
                // Stack: ref/err ...
                if status.can_be_reference == RefResult::Maybe {
                    chunk.op(Insn::GetValue);
                }
                // Stack val/err ...
                Ok((
                    1,
                    (status.can_be_abrupt == AbruptResult::Maybe || status.can_be_reference == RefResult::Maybe).into(),
                ))
            }
            ArgumentList::Dots(_) => todo!(),
            ArgumentList::ArgumentList(lst, item) => {
                // Stack: ...
                let (prev_count, status) = lst.argument_list_evaluation(chunk, strict, text)?;
                // Stack: val(N) val(N-1) ... val(0) ...
                // or err ...
                let exit = if status == AbruptResult::Maybe { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                let status2 = item.compile(chunk, strict, text)?;
                // Stack: val/err val(n) val(n-1) ... val(0) ...
                if status2.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                if status2.maybe_ref() || status2.maybe_abrupt() {
                    let happy = chunk.op_jump(Insn::JumpIfNormal);
                    chunk.op_plus_arg(Insn::Unwind, prev_count);
                    chunk.fixup(happy).expect("Jump is too short to overflow.");
                }
                if let Some(mark) = exit {
                    chunk.fixup(mark)?;
                }
                Ok((
                    prev_count + 1,
                    (status == AbruptResult::Maybe || status2.maybe_abrupt() || status2.maybe_ref()).into(),
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
        text: &str,
        exp: &Rc<LeftHandSideExpression>,
        insn: Insn,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Stack: ...
        let status = exp.compile(chunk, strict, text)?;
        assert!(status.maybe_ref()); // Early errors eliminate non-refs

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
        chunk.fixup(mark).expect("Jump is too short to overflow.");
        // Stack: lval lref ...
        chunk.op(Insn::ToNumeric);
        let mark = chunk.op_jump(Insn::JumpIfNormal);
        // Stack: err lref ...
        chunk.op(Insn::Swap);
        // Stack: lref err ...
        chunk.op(Insn::Pop);
        // Stack: err ...
        let exit2 = chunk.op_jump(Insn::Jump);
        chunk.fixup(mark).expect("Jump is too short to overflow.");
        // Stack: oldValue lref ...
        chunk.op(Insn::Pop2Push3);
        // Stack: oldValue lref oldValue ...
        chunk.op(insn);
        // Stack: newValue lref oldValue ...
        chunk.op(Insn::PutValue);
        // Stack: [empty]/err oldValue ...
        chunk.op(Insn::UpdateEmpty);
        // Stack: oldValue/err ...

        chunk.fixup(exit1).expect("Jump is too short to overflow.");
        chunk.fixup(exit2).expect("Jump is too short to overflow.");

        Ok(AlwaysAbruptResult {})
    }

    fn pre_op(
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        exp: &Rc<UnaryExpression>,
        insn: Insn,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Stack: ...
        exp.compile(chunk, strict, text)?;
        // Stack: exp/err
        chunk.op(insn);
        Ok(AlwaysAbruptResult {})
    }

    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            UpdateExpression::LeftHandSideExpression(lhse) => lhse.compile(chunk, strict, text),
            UpdateExpression::PostIncrement { lhs: exp, .. } => {
                Self::post_op(chunk, strict, text, exp, Insn::Increment).map(CompilerStatusFlags::from)
            }
            UpdateExpression::PostDecrement { lhs: exp, .. } => {
                Self::post_op(chunk, strict, text, exp, Insn::Decrement).map(CompilerStatusFlags::from)
            }
            UpdateExpression::PreIncrement { ue: exp, .. } => {
                Self::pre_op(chunk, strict, text, exp, Insn::PreIncrement).map(CompilerStatusFlags::from)
            }
            UpdateExpression::PreDecrement { ue: exp, .. } => {
                Self::pre_op(chunk, strict, text, exp, Insn::PreDecrement).map(CompilerStatusFlags::from)
            }
        }
    }
}

impl UnaryExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        fn unary_op(
            exp: &Rc<UnaryExpression>,
            chunk: &mut Chunk,
            strict: bool,
            text: &str,
            insn: Insn,
        ) -> anyhow::Result<AlwaysAbruptResult> {
            exp.compile(chunk, strict, text)?;
            chunk.op(insn);
            Ok(AlwaysAbruptResult {})
        }
        match self {
            UnaryExpression::UpdateExpression(ue) => ue.compile(chunk, strict, text),
            UnaryExpression::Delete { ue, .. } => {
                unary_op(ue, chunk, strict, text, Insn::Delete).map(CompilerStatusFlags::from)
            }
            UnaryExpression::Void { ue, .. } => {
                unary_op(ue, chunk, strict, text, Insn::Void).map(CompilerStatusFlags::from)
            }
            UnaryExpression::Typeof { ue, .. } => {
                unary_op(ue, chunk, strict, text, Insn::TypeOf).map(CompilerStatusFlags::from)
            }
            UnaryExpression::NoOp { ue, .. } => {
                unary_op(ue, chunk, strict, text, Insn::UnaryPlus).map(CompilerStatusFlags::from)
            }
            UnaryExpression::Negate { ue, .. } => {
                unary_op(ue, chunk, strict, text, Insn::UnaryMinus).map(CompilerStatusFlags::from)
            }
            UnaryExpression::Complement { ue, .. } => {
                unary_op(ue, chunk, strict, text, Insn::UnaryComplement).map(CompilerStatusFlags::from)
            }
            UnaryExpression::Not { ue, .. } => {
                unary_op(ue, chunk, strict, text, Insn::UnaryNot).map(CompilerStatusFlags::from)
            }
            UnaryExpression::Await(_) => todo!(),
        }
    }
}

macro_rules! compile_binary_expression {
    ( $chunk:expr, $strict:expr, $text:expr, $left:expr, $right:expr, $op:expr ) => {{
        // Stack: ...
        let left_status = $left.compile($chunk, $strict, $text)?;
        // Stack: err/ref/val ...
        if left_status.maybe_ref() {
            $chunk.op(Insn::GetValue);
        }
        // Stack: err/val
        let first_exit = if left_status.maybe_ref() || left_status.maybe_abrupt() {
            Some($chunk.op_jump(Insn::JumpIfAbrupt))
        } else {
            None
        };
        // Stack: val
        let right_status = $right.compile($chunk, $strict, $text)?;
        // Stack: err/ref/val val ...
        if right_status.maybe_ref() {
            $chunk.op(Insn::GetValue);
        }
        // Stack: err/val val ...
        let second_exit = if right_status.maybe_ref() || right_status.maybe_abrupt() {
            let nearby = $chunk.op_jump(Insn::JumpIfNormal);
            // Stack: err val ...
            $chunk.op_plus_arg(Insn::Unwind, 1);
            // Stack: err ...
            let exit = $chunk.op_jump(Insn::Jump);
            $chunk.fixup(nearby).expect("Jump is too short to overflow.");
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
            $chunk.fixup(mark).expect("Jump is too short to overflow.");
        }
        Ok(AlwaysAbruptResult {})
    }};
}

impl ExponentiationExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            ExponentiationExpression::UnaryExpression(ue) => ue.compile(chunk, strict, text),
            ExponentiationExpression::Exponentiation(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::Exponentiate)
                    .map(CompilerStatusFlags::from)
            }
        }
    }
}

impl MultiplicativeExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            MultiplicativeExpression::ExponentiationExpression(ee) => ee.compile(chunk, strict, text),
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(left, op, right) => {
                compile_binary_expression!(
                    chunk,
                    strict,
                    text,
                    left,
                    right,
                    match **op {
                        MultiplicativeOperator::Multiply => Insn::Multiply,
                        MultiplicativeOperator::Divide => Insn::Divide,
                        MultiplicativeOperator::Modulo => Insn::Modulo,
                    }
                )
                .map(CompilerStatusFlags::from)
            }
        }
    }
}

impl AdditiveExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            AdditiveExpression::MultiplicativeExpression(me) => me.compile(chunk, strict, text),
            AdditiveExpression::Add(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::Add).map(CompilerStatusFlags::from)
            }
            AdditiveExpression::Subtract(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::Subtract)
                    .map(CompilerStatusFlags::from)
            }
        }
    }
}

impl ShiftExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            ShiftExpression::AdditiveExpression(ae) => ae.compile(chunk, strict, text),
            ShiftExpression::LeftShift(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::LeftShift)
                    .map(CompilerStatusFlags::from)
            }
            ShiftExpression::SignedRightShift(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::SignedRightShift)
                    .map(CompilerStatusFlags::from)
            }
            ShiftExpression::UnsignedRightShift(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::UnsignedRightShift)
                    .map(CompilerStatusFlags::from)
            }
        }
    }
}

impl RelationalExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            RelationalExpression::ShiftExpression(se) => se.compile(chunk, strict, text),
            RelationalExpression::Less(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::Less).map(CompilerStatusFlags::from)
            }
            RelationalExpression::Greater(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::Greater)
                    .map(CompilerStatusFlags::from)
            }
            RelationalExpression::LessEqual(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::LessEqual)
                    .map(CompilerStatusFlags::from)
            }
            RelationalExpression::GreaterEqual(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::GreaterEqual)
                    .map(CompilerStatusFlags::from)
            }
            RelationalExpression::InstanceOf(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::InstanceOf)
                    .map(CompilerStatusFlags::from)
            }
            RelationalExpression::In(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::In).map(CompilerStatusFlags::from)
            }
            RelationalExpression::PrivateIn(_, _, _) => todo!(),
        }
    }
}

impl EqualityExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            EqualityExpression::RelationalExpression(re) => re.compile(chunk, strict, text),
            EqualityExpression::Equal(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::Equal).map(CompilerStatusFlags::from)
            }
            EqualityExpression::NotEqual(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::NotEqual)
                    .map(CompilerStatusFlags::from)
            }
            EqualityExpression::StrictEqual(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::StrictEqual)
                    .map(CompilerStatusFlags::from)
            }
            EqualityExpression::NotStrictEqual(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::StrictNotEqual)
                    .map(CompilerStatusFlags::from)
            }
        }
    }
}

impl BitwiseANDExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            BitwiseANDExpression::EqualityExpression(ee) => ee.compile(chunk, strict, text),
            BitwiseANDExpression::BitwiseAND(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::BitwiseAnd)
                    .map(CompilerStatusFlags::from)
            }
        }
    }
}

impl BitwiseXORExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            BitwiseXORExpression::BitwiseANDExpression(bae) => bae.compile(chunk, strict, text),
            BitwiseXORExpression::BitwiseXOR(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::BitwiseXor)
                    .map(CompilerStatusFlags::from)
            }
        }
    }
}

impl BitwiseORExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            BitwiseORExpression::BitwiseXORExpression(bxe) => bxe.compile(chunk, strict, text),
            BitwiseORExpression::BitwiseOR(left, right) => {
                compile_binary_expression!(chunk, strict, text, left, right, Insn::BitwiseOr)
                    .map(CompilerStatusFlags::from)
            }
        }
    }
}

impl LogicalANDExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            LogicalANDExpression::BitwiseORExpression(boe) => boe.compile(chunk, strict, text),
            LogicalANDExpression::LogicalAND(left, right) => {
                let mut first_exit = None;
                // Stack: ...
                let left_status = left.compile(chunk, strict, text)?;
                // Stack: lval/lref/err ...
                if left_status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                // Stack: lval/err ...
                if left_status.maybe_ref() || left_status.maybe_abrupt() {
                    first_exit = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                // Stack: lval ...
                let second_exit = chunk.op_jump(Insn::JumpIfFalse);
                chunk.op(Insn::Pop);
                // Stack: ...
                let right_status = right.compile(chunk, strict, text)?;
                // Stack: rval/rref/err ...
                if right_status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                // Stack: rval/err ...
                if let Some(offset) = first_exit {
                    chunk.fixup(offset)?;
                }
                chunk.fixup(second_exit)?;
                // Stack: lval/rval/err ...

                Ok(CompilerStatusFlags::new().abrupt(
                    left_status.maybe_ref()
                        || left_status.maybe_abrupt()
                        || right_status.maybe_ref()
                        || right_status.maybe_abrupt(),
                ))
            }
        }
    }
}

impl LogicalORExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            LogicalORExpression::LogicalANDExpression(lae) => lae.compile(chunk, strict, text),
            LogicalORExpression::LogicalOR(left, right) => {
                let mut first_exit = None;
                // Stack: ...
                let left_status = left.compile(chunk, strict, text)?;
                // Stack: lval/lref/err ...
                if left_status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                // Stack: lval/err ...
                if left_status.maybe_ref() || left_status.maybe_abrupt() {
                    first_exit = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                // Stack: lval ...
                let second_exit = chunk.op_jump(Insn::JumpIfTrue);
                chunk.op(Insn::Pop);
                // Stack: ...
                let right_status = right.compile(chunk, strict, text)?;
                // Stack: rval/rref/err ...
                if right_status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                // Stack: rval/err ...
                if let Some(offset) = first_exit {
                    chunk.fixup(offset)?;
                }
                chunk.fixup(second_exit)?;
                // Stack: lval/rval/err ...

                Ok(CompilerStatusFlags::new().abrupt(
                    left_status.maybe_ref()
                        || left_status.maybe_abrupt()
                        || right_status.maybe_ref()
                        || right_status.maybe_abrupt(),
                ))
            }
        }
    }
}

impl CoalesceExpressionHead {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            CoalesceExpressionHead::CoalesceExpression(coal) => coal.compile(chunk, strict, text),
            CoalesceExpressionHead::BitwiseORExpression(bor) => bor.compile(chunk, strict, text),
        }
    }
}

impl CoalesceExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        let mut first_exit = None;
        // Stack ...
        let head_status = self.head.compile(chunk, strict, text)?;
        // Stack: lref/lval/err ...
        if head_status.maybe_ref() {
            chunk.op(Insn::GetValue);
        }
        // Stack: lval/err ...
        if head_status.maybe_ref() || head_status.maybe_abrupt() {
            first_exit = Some(chunk.op_jump(Insn::JumpIfAbrupt));
        }
        // Stack: lval ...
        let second_exit = chunk.op_jump(Insn::JumpIfNotNullish);
        chunk.op(Insn::Pop);
        let tail_status = self.tail.compile(chunk, strict, text)?;
        // Stack: rval/rref/err ...
        if tail_status.maybe_ref() {
            chunk.op(Insn::GetValue);
        }
        // Stack: rval/err ...
        if let Some(fixup) = first_exit {
            chunk.fixup(fixup)?;
        }
        chunk.fixup(second_exit)?;
        // Stack: lval/rval/err
        Ok(CompilerStatusFlags::new().abrupt(
            head_status.maybe_ref()
                || head_status.maybe_abrupt()
                || tail_status.maybe_ref()
                || head_status.maybe_abrupt(),
        ))
    }
}

impl ShortCircuitExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            ShortCircuitExpression::LogicalORExpression(loe) => loe.compile(chunk, strict, text),
            ShortCircuitExpression::CoalesceExpression(coal) => coal.compile(chunk, strict, text),
        }
    }
}

impl ConditionalExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            ConditionalExpression::FallThru(sce) => sce.compile(chunk, strict, text),
            ConditionalExpression::Conditional(expr, truthy, falsey) => {
                let mut first_exit = None;
                let expr_status = expr.compile(chunk, strict, text)?;
                // Stack: lval/lref/err ...
                if expr_status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                // Stack: lval/err ...
                if expr_status.maybe_ref() || expr_status.maybe_abrupt() {
                    first_exit = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                // Stack: lval ...
                let falsey_spot = chunk.op_jump(Insn::JumpIfFalse);
                chunk.op(Insn::Pop);
                // Stack: ...
                let truthy_status = truthy.compile(chunk, strict, text)?;
                // Stack: trueRef/trueVal/err
                if truthy_status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                // Stack: trueVal/err
                let second_exit = chunk.op_jump(Insn::Jump);
                chunk.fixup(falsey_spot)?;
                // Stack: lval ...
                chunk.op(Insn::Pop);
                // Stack: ...
                let falsey_status = falsey.compile(chunk, strict, text)?;
                // Stack: falseRef/falseVal/err
                if falsey_status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                // Stack: falseVal/err
                if let Some(spot) = first_exit {
                    chunk.fixup(spot)?;
                }
                chunk.fixup(second_exit)?;
                // Stack: trueVal/falseVal/err

                Ok(CompilerStatusFlags::new().abrupt(
                    expr_status.maybe_ref()
                        || expr_status.maybe_abrupt()
                        || truthy_status.maybe_abrupt()
                        || truthy_status.maybe_ref()
                        || falsey_status.maybe_abrupt()
                        || falsey_status.maybe_ref(),
                ))
            }
        }
    }
}

impl AssignmentExpression {
    #[allow(unused_assignments)]
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            AssignmentExpression::FallThru(ce) => ce.compile(chunk, strict, text),
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
                let status = lhse.compile(chunk, strict, text)?;
                if status.maybe_abrupt() {
                    let mark = chunk.op_jump(Insn::JumpIfAbrupt);
                    exits.push(mark);
                }
                // Stack: lref ...
                if ae.is_anonymous_function_definition() && lhse.is_identifier_ref() {
                    todo!()
                } else {
                    let status = ae.compile(chunk, strict, text)?;
                    // Stack: rref lref ...
                    if status.maybe_ref() {
                        chunk.op(Insn::GetValue);
                    }
                    if status.maybe_abrupt() || status.maybe_ref() {
                        let close = chunk.op_jump(Insn::JumpIfNormal);
                        // (haven't jumped) Stack: err lref
                        chunk.op(Insn::Swap);
                        // Stack: lref err
                        chunk.op(Insn::Pop);
                        // Stack: err
                        let mark2 = chunk.op_jump(Insn::Jump);
                        exits.push(mark2);
                        chunk.fixup(close).expect("Jump is too short to overflow.");
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
                Ok(AlwaysAbruptResult {}.into())
            }
            _ => todo!(),
        }
    }
}

impl Expression {
    #[allow(unused_variables)]
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            Expression::FallThru(ae) => ae.compile(chunk, strict, text),
            Expression::Comma(e, ae) => {
                let mut first_exit = None;
                let left_status = e.compile(chunk, strict, text)?;
                // Stack: lref/lval/err
                if left_status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                // Stack: lval/err
                if left_status.maybe_ref() || left_status.maybe_abrupt() {
                    first_exit = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                // Stack: lval
                chunk.op(Insn::Pop);
                // Stack: ...
                let right_status = ae.compile(chunk, strict, text)?;
                // Stack: rref/rval/err
                if right_status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                // Stack: rval/err
                if let Some(fixup) = first_exit {
                    chunk.fixup(fixup)?;
                }
                Ok(CompilerStatusFlags::new().abrupt(
                    left_status.maybe_ref()
                        || left_status.maybe_abrupt()
                        || right_status.maybe_ref()
                        || right_status.maybe_abrupt(),
                ))
            }
        }
    }
}

impl ExpressionStatement {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        let status = self.exp.compile(chunk, strict, text)?;
        if status.maybe_ref() {
            chunk.op(Insn::GetValue);
        }
        Ok((status.maybe_abrupt() || status.maybe_ref()).into())
    }
}

impl StatementList {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match self {
            StatementList::Item(sli) => sli.compile(chunk, strict, text),
            StatementList::List(sl, sli) => {
                let mut mark = None;
                let status = sl.compile(chunk, strict, text)?;
                if status == AbruptResult::Maybe {
                    mark = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                let second_status = sli.compile(chunk, strict, text)?;
                chunk.op(Insn::UpdateEmpty);
                if let Some(mark) = mark {
                    chunk.fixup(mark)?;
                }
                Ok((status == AbruptResult::Maybe || second_status == AbruptResult::Maybe).into())
            }
        }
    }
}

impl StatementListItem {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match self {
            StatementListItem::Statement(stmt) => stmt.compile(chunk, strict, text),
            StatementListItem::Declaration(decl) => decl.compile(chunk, strict, text),
        }
    }
}

impl Statement {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match self {
            Statement::Expression(exp) => exp.compile(chunk, strict, text),
            Statement::Block(bs) => bs.compile(chunk, strict, text),
            Statement::Variable(var_statement) => var_statement.compile(chunk, strict, text),
            Statement::Empty(empty) => Ok(empty.compile(chunk).into()),
            Statement::If(if_stmt) => if_stmt.compile(chunk, strict, text),
            Statement::Breakable(breakable_statement) => breakable_statement.compile(chunk, strict, text),
            Statement::Continue(c) => c.compile(chunk).map(AbruptResult::from),
            Statement::Break(b) => b.compile(chunk).map(AbruptResult::from),
            Statement::Return(_) => todo!(),
            Statement::With(_) => todo!(),
            Statement::Labelled(lbl) => lbl.compile(chunk, strict, text),
            Statement::Throw(throw_statement) => throw_statement.compile(chunk, strict, text).map(AbruptResult::from),
            Statement::Try(_) => todo!(),
            Statement::Debugger(_) => todo!(),
        }
    }

    pub fn labelled_compile(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        label_set: &[JSString],
    ) -> anyhow::Result<AbruptResult> {
        match self {
            Statement::Expression(_)
            | Statement::Block(_)
            | Statement::Variable(_)
            | Statement::Empty(_)
            | Statement::If(_)
            | Statement::Continue(_)
            | Statement::Break(_)
            | Statement::Return(_)
            | Statement::With(_)
            | Statement::Throw(_)
            | Statement::Try(_)
            | Statement::Debugger(_) => self.compile(chunk, strict, text),

            Statement::Breakable(bs) => bs.labelled_compile(chunk, strict, text, label_set),
            Statement::Labelled(lbl) => lbl.labelled_compile(chunk, strict, text, label_set),
        }
    }
}

impl Declaration {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match self {
            Declaration::Class(_) => todo!(),
            Declaration::Hoistable(_) => todo!(),
            Declaration::Lexical(lex) => lex.compile(chunk, strict, text).map(AbruptResult::from),
        }
    }
}

impl BreakableStatement {
    fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        self.labelled_compile(chunk, strict, text, &[])
    }

    fn labelled_compile(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        label_set: &[JSString],
    ) -> anyhow::Result<AbruptResult> {
        let state = match self {
            BreakableStatement::Iteration(iter) => iter.loop_compile(chunk, strict, text, label_set),
            BreakableStatement::Switch(swtch) => swtch.compile(chunk, strict, text),
        }?;
        if state.maybe_abrupt() {
            chunk.op(Insn::HandleEmptyBreak);
        }
        Ok(state)
    }
}

impl BlockStatement {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        let BlockStatement::Block(block) = self;
        block.compile(chunk, strict, text)
    }
}

impl FcnDef {
    pub fn compile_fo_instantiation(
        &self,
        _chunk: &mut Chunk,
        _strict: bool,
        _text: &str,
    ) -> anyhow::Result<AbruptResult> {
        match self {
            FcnDef::Function(_) => todo!(),
            FcnDef::Generator(_) => todo!(),
            FcnDef::AsyncFun(_) => todo!(),
            FcnDef::AsyncGen(_) => todo!(),
        }
    }
}

impl Block {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match &self.statements {
            None => {
                chunk.op(Insn::Empty);
                Ok(NeverAbruptRefResult {}.into())
            }
            Some(sl) => {
                chunk.op(Insn::PushNewLexEnv);
                let declarations = sl.lexically_scoped_declarations();
                for d in declarations {
                    let is_constant = d.is_constant_declaration();
                    for dn in d.bound_names() {
                        let string_idx = chunk.add_to_string_pool(dn)?;
                        if is_constant {
                            chunk.op_plus_arg(Insn::CreateStrictImmutableLexBinding, string_idx);
                        } else {
                            chunk.op_plus_arg(Insn::CreatePermanentMutableLexBinding, string_idx);
                        }
                    }
                    if let Ok(fcn) = FcnDef::try_from(d) {
                        let fcn_name = fcn.bound_name();
                        let string_idx =
                            chunk.add_to_string_pool(fcn_name).expect("will work, because we're re-adding this");
                        fcn.compile_fo_instantiation(chunk, strict, text)?;
                        chunk.op_plus_arg(Insn::InitializeLexBinding, string_idx);
                    }
                }

                let statement_status = sl.compile(chunk, strict, text)?;
                chunk.op(Insn::PopLexEnv);

                Ok(statement_status)
            }
        }
    }
}

impl LexicalDeclaration {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AlwaysAbruptResult> {
        self.list.compile(chunk, strict, text)?;
        let mark = chunk.op_jump(Insn::JumpIfAbrupt);
        chunk.op(Insn::Pop);
        chunk.op(Insn::Empty);
        chunk.fixup(mark).expect("Jump is too short to overflow.");
        Ok(AlwaysAbruptResult {})
    }
}

impl BindingList {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AlwaysAbruptResult> {
        match self {
            BindingList::Item(item) => item.compile(chunk, strict, text),
            BindingList::List(lst, item) => {
                lst.compile(chunk, strict, text)?;
                let mark = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Pop);
                item.compile(chunk, strict, text)?;
                chunk.fixup(mark)?;
                Ok(AlwaysAbruptResult {})
            }
        }
    }
}

impl LexicalBinding {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AlwaysAbruptResult> {
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
                            let status = izer.compile(chunk, strict, text)?;
                            // Stack: rref lhs ...
                            if status.maybe_ref() {
                                chunk.op(Insn::GetValue);
                                // Stack: value lhs ...
                            }
                            if status.maybe_abrupt() || status.maybe_ref() {
                                let normal = chunk.op_jump(Insn::JumpIfNormal);
                                // Stack: err lhs ...
                                chunk.op(Insn::Swap);
                                // Stack: lhs err ...
                                chunk.op(Insn::Pop);
                                // Stack: err ...
                                let exit_tgt = Some(chunk.op_jump(Insn::Jump));
                                chunk.fixup(normal).expect("Jump is too short to overflow.");
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
                    chunk.fixup(mark).expect("Jump is too short to overflow.");
                }
                Ok(AlwaysAbruptResult {})
            }
            LexicalBinding::Pattern(_, _) => todo!(),
        }
    }
}

impl Initializer {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        self.ae.compile(chunk, strict, text)
    }
}

impl VariableStatement {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        // Runtime Semantics: Evaluation
        //      VariableStatement : var VariableDeclarationList ;
        //  1. Let next be the result of evaluating VariableDeclarationList.
        //  2. ReturnIfAbrupt(next).
        //  3. Return empty.

        // Stack: ...
        self.list.compile(chunk, strict, text)
        // Stack: empty/err ...
    }
}

impl VariableDeclarationList {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match self {
            VariableDeclarationList::Item(item) => item.compile(chunk, strict, text),
            VariableDeclarationList::List(list, item) => {
                // Runtime Semantics: Evaluation
                //      VariableDeclarationList : VariableDeclarationList , VariableDeclaration
                //  1. Let next be the result of evaluating VariableDeclarationList.
                //  2. ReturnIfAbrupt(next).
                //  3. Return the result of evaluating VariableDeclaration.

                // Stack: ...
                let first = list.compile(chunk, strict, text)?; // Stack: empty/err ...
                let tgt = if first.maybe_abrupt() {
                    Some(chunk.op_jump(Insn::JumpIfAbrupt)) // Stack: empty ...
                } else {
                    None
                };
                chunk.op(Insn::Pop); // Stack: ...
                let second = item.compile(chunk, strict, text)?; // Stack: empty/err ...
                if let Some(tgt) = tgt {
                    chunk.fixup(tgt)?; // Stack: empty/err ...
                }
                Ok((first.maybe_abrupt() || second.maybe_abrupt()).into())
            }
        }
    }
}

impl VariableDeclaration {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        // Runtime Semantics: Evaluation
        match self {
            VariableDeclaration::Identifier(_, None) => {
                // VariableDeclaration : BindingIdentifier
                //  1. Return empty.

                // Stack: ...
                chunk.op(Insn::Empty); // Stack: empty ...
                Ok(AbruptResult::Never)
            }
            VariableDeclaration::Identifier(id, Some(izer)) => {
                // VariableDeclaration : BindingIdentifier Initializer
                //  1. Let bindingId be StringValue of BindingIdentifier.
                //  2. Let lhs be ? ResolveBinding(bindingId).
                //  3. If IsAnonymousFunctionDefinition(Initializer) is true, then
                //      a. Let value be ? NamedEvaluation of Initializer with argument bindingId.
                //  4. Else,
                //      a. Let rhs be the result of evaluating Initializer.
                //      b. Let value be ? GetValue(rhs).
                //  5. Perform ? PutValue(lhs, value).
                //  6. Return empty.

                let mut exits = vec![];
                // Stack: ...
                let idx = chunk.add_to_string_pool(id.string_value())?;
                chunk.op_plus_arg(Insn::String, idx); // Stack: bindingId ...
                chunk.op(if strict { Insn::StrictResolve } else { Insn::Resolve }); // Stack: lhs/err ...
                exits.push(chunk.op_jump(Insn::JumpIfAbrupt)); // Stack: lhs ...
                if izer.is_anonymous_function_definition() {
                    todo!();
                } else {
                    let izer_flags = izer.compile(chunk, strict, text)?; // Stack: rhs/rref/err lhs ...
                    if izer_flags.maybe_ref() {
                        chunk.op(Insn::GetValue); // Stack: rhs/err lhs ...
                    }
                    if izer_flags.maybe_abrupt() || izer_flags.maybe_ref() {
                        let ok_tgt = chunk.op_jump(Insn::JumpIfNormal); // Stack: err lhs ...
                        chunk.op_plus_arg(Insn::Unwind, 1); // Stack: err ...
                        exits.push(chunk.op_jump(Insn::Jump));
                        chunk.fixup(ok_tgt).expect("Jump too short to overflow.");
                    }
                }
                // Stack: rhs lhs ...
                chunk.op(Insn::PutValue); // Stack: err/empty ...
                for exit in exits {
                    chunk.fixup(exit)?;
                }
                Ok(AbruptResult::Maybe)
            }
            VariableDeclaration::Pattern(_, _) => todo!(),
        }
    }
}

impl EmptyStatement {
    fn compile(&self, chunk: &mut Chunk) -> NeverAbruptRefResult {
        chunk.op(Insn::Empty);
        NeverAbruptRefResult {}
    }
}

impl IfStatement {
    fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        let mut first_exit = None;
        let mut second_exit = None;
        let mut third_exit = None;
        let expr_status = self.expression().compile(chunk, strict, text)?;
        // Stack: exprRef/exprValue/err
        if expr_status.maybe_ref() {
            chunk.op(Insn::GetValue)
        }
        // Stack: exprValue/err
        if expr_status.maybe_abrupt() {
            first_exit = Some(chunk.op_jump(Insn::JumpIfAbrupt));
        }
        // Stack: exprValue
        let expr_false = chunk.op_jump(Insn::JumpPopIfFalse);
        // "True" path
        let true_path_status = self.first_statement().compile(chunk, strict, text)?;
        if true_path_status.maybe_abrupt() {
            second_exit = Some(chunk.op_jump(Insn::JumpIfAbrupt));
        }
        let true_complete = chunk.op_jump(Insn::Jump);
        // "False" path
        chunk.fixup(expr_false)?;
        let false_path_status = match self {
            IfStatement::WithElse(_, _, false_path, _) => false_path.compile(chunk, strict, text)?,
            IfStatement::WithoutElse(..) => {
                chunk.op(Insn::Undefined);
                NeverAbruptRefResult {}.into()
            }
        };
        if false_path_status.maybe_abrupt() {
            third_exit = Some(chunk.op_jump(Insn::JumpIfAbrupt));
        }
        chunk.fixup(true_complete)?;
        chunk.op(Insn::Undefined);
        chunk.op(Insn::Swap);
        chunk.op(Insn::UpdateEmpty);
        if let Some(mark) = first_exit {
            chunk.fixup(mark)?;
        }
        if let Some(mark) = second_exit {
            chunk.fixup(mark)?;
        }
        if let Some(mark) = third_exit {
            chunk.fixup(mark).expect("Jump too short to overflow");
        }

        Ok(AbruptResult::from(
            expr_status.maybe_abrupt() || true_path_status.maybe_abrupt() || false_path_status.maybe_abrupt(),
        ))
    }
}

impl IterationStatement {
    fn loop_compile(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        label_set: &[JSString],
    ) -> anyhow::Result<AbruptResult> {
        match self {
            IterationStatement::DoWhile(dws) => dws.do_while_loop_compile(chunk, strict, text, label_set),
            IterationStatement::While(_) => todo!(),
            IterationStatement::For(_) => todo!(),
            IterationStatement::ForInOf(_) => todo!(),
        }
    }
}

impl DoWhileStatement {
    #[allow(unused_variables)]
    fn do_while_loop_compile(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        label_set: &[JSString],
    ) -> anyhow::Result<AbruptResult> {
        // DoWhileStatement : do Statement while ( Expression ) ;
        //  1. Let V be undefined.
        //  2. Repeat,
        //      a. Let stmtResult be the result of evaluating Statement.
        //      b. If LoopContinues(stmtResult, labelSet) is false, return ? UpdateEmpty(stmtResult, V).
        //      c. If stmtResult.[[Value]] is not empty, set V to stmtResult.[[Value]].
        //      d. Let exprRef be the result of evaluating Expression.
        //      e. Let exprValue be ? GetValue(exprRef).
        //      f. If ToBoolean(exprValue) is false, return V.
        chunk.op(Insn::Undefined);
        // Stack: V ...
        let loop_top = chunk.pos();
        let stmt_status = self.stmt.compile(chunk, strict, text)?;
        // Stack: stmtResult V ...
        let loop_ends = if stmt_status.maybe_abrupt() {
            let label_set_id = chunk.add_to_label_set_pool(label_set)?;
            chunk.op_plus_arg(Insn::LoopContinues, label_set_id);
            // Stack: LCResult stmtResult V ...
            Some(chunk.op_jump(Insn::JumpPopIfFalse))
        } else {
            None
        };
        // Stack: stmtResult V ...
        chunk.op(Insn::CoalesceValue);
        // Stack: V ...
        let expr_status = self.exp.compile(chunk, strict, text)?;
        // Stack: exprRef/exprVal/err V ...
        if expr_status.maybe_ref() {
            chunk.op(Insn::GetValue);
        }
        // Stack: exprVal/err V ...
        let expr_err_exit = if expr_status.maybe_abrupt() || expr_status.maybe_ref() {
            let short_target = chunk.op_jump(Insn::JumpIfNormal);
            // Stack: err V ...
            chunk.op_plus_arg(Insn::Unwind, 1);
            // Stack: err ...
            let expr_err_exit = chunk.op_jump(Insn::Jump);
            chunk.fixup(short_target).expect("Jump too short to fail");
            Some(expr_err_exit)
        } else {
            None
        };
        // Stack: exprVal V ...
        chunk.op_jump_back(Insn::JumpPopIfTrue, loop_top)?;
        // Stack: V ...
        if let Some(mark) = loop_ends {
            let done_exit = chunk.op_jump(Insn::Jump);
            chunk.fixup(mark).expect("This is shorter than the jump back. We'll be fine.");
            // Stack: stmtResult V ...
            chunk.op(Insn::UpdateEmpty);
            chunk.fixup(done_exit).expect("Jump too short to fail");
        }
        // Stack: V ...
        if let Some(mark) = expr_err_exit {
            chunk.fixup(mark).expect("Jump too short to fail");
        }
        Ok(AbruptResult::from(expr_status.maybe_abrupt() || expr_status.maybe_ref() || stmt_status.maybe_abrupt()))
    }
}

impl ContinueStatement {
    fn compile(&self, chunk: &mut Chunk) -> anyhow::Result<AlwaysAbruptResult> {
        match self {
            ContinueStatement::Bare { .. } => chunk.op(Insn::Continue),
            ContinueStatement::Labelled { label, .. } => {
                let str_idx = chunk.add_to_string_pool(label.string_value())?;
                chunk.op_plus_arg(Insn::TargetedContinue, str_idx);
            }
        }
        Ok(AlwaysAbruptResult {})
    }
}

impl BreakStatement {
    fn compile(&self, chunk: &mut Chunk) -> anyhow::Result<AlwaysAbruptResult> {
        match self {
            BreakStatement::Bare { .. } => chunk.op(Insn::Break),
            BreakStatement::Labelled { label, .. } => {
                let str_idx = chunk.add_to_string_pool(label.string_value())?;
                chunk.op_plus_arg(Insn::TargetedBreak, str_idx);
            }
        }
        Ok(AlwaysAbruptResult {})
    }
}

impl SwitchStatement {
    #[allow(unused_variables)]
    fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        todo!()
    }
}

impl LabelledStatement {
    fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        self.labelled_compile(chunk, strict, text, &[])
    }

    fn labelled_compile(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        label_set: &[JSString],
    ) -> anyhow::Result<AbruptResult> {
        let label = self.identifier.string_value();
        let mut label_set = label_set.to_vec();
        label_set.push(label.clone());
        let item_status = self.item.labelled_compile(chunk, strict, text, &label_set)?;
        if item_status.maybe_abrupt() {
            let str_idx = chunk.add_to_string_pool(label)?;
            chunk.op_plus_arg(Insn::HandleTargetedBreak, str_idx);
        }
        Ok(item_status)
    }
}

impl LabelledItem {
    fn labelled_compile(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        label_set: &[JSString],
    ) -> anyhow::Result<AbruptResult> {
        match self {
            LabelledItem::Function(f) => f.compile(chunk, strict, text),
            LabelledItem::Statement(s) => s.labelled_compile(chunk, strict, text, label_set),
        }
    }
}

impl ThrowStatement {
    /// Compile the ThrowStatement production
    ///
    /// The instruction sequence will always leave a Throw completion on the stack.
    ///
    /// See [ThrowStatement evaluation](https://tc39.es/ecma262/#sec-throw-statement-runtime-semantics-evaluation) in ECMA-262.
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: Evaluation
        // ThrowStatement : throw Expression ;
        // 1. Let exprRef be the result of evaluating Expression.
        // 2. Let exprValue be ? GetValue(exprRef).
        // 3. Return ThrowCompletion(exprValue).
        let status = self.exp.compile(chunk, strict, text)?; // Stack: exp/ref/err ...
        if status.maybe_ref() {
            chunk.op(Insn::GetValue); // Stack: exp/err ...
        }
        let abort_target =
            if status.maybe_abrupt() || status.maybe_ref() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
        chunk.op(Insn::Throw);
        if let Some(tgt) = abort_target {
            chunk.fixup(tgt).expect("Jump too short to overflow");
        }
        Ok(AlwaysAbruptResult {})
    }
}

impl FunctionDeclaration {
    #[allow(unused_variables)]
    fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        todo!()
    }
}

impl Script {
    pub fn compile(&self, chunk: &mut Chunk, text: &str) -> anyhow::Result<AbruptResult> {
        match &self.body {
            None => Ok(AbruptResult::Never),
            Some(sb) => sb.compile(chunk, text),
        }
    }
}

impl ScriptBody {
    pub fn compile(&self, chunk: &mut Chunk, text: &str) -> anyhow::Result<AbruptResult> {
        let strict = self.contains_use_strict();
        self.statement_list.compile(chunk, strict, text)
    }
}

#[cfg(test)]
mod tests;
