use super::*;
use ahash::AHashSet;
use anyhow::anyhow;
use counter::Counter;
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
    ToDo,
    String,
    Resolve,
    StrictResolve,
    This,
    Null,
    Undefined,
    True,
    False,
    Zero,
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
    JumpIfNotUndef,
    JumpNotThrow,
    Call,
    EndFunction,
    Return,
    UpdateEmpty,
    Swap,
    Pop,
    PopOrPanic,
    Pop2Push3,
    Dup,
    RotateUp,
    RotateDown,
    RotateDownList,
    Unwind,
    UnwindList,
    AppendList,
    Ref,
    StrictRef,
    InitializeReferencedBinding,
    PushNewLexEnv,
    PopLexEnv,
    PushNewVarEnvFromLex,
    PushNewLexEnvFromVar,
    SetLexEnvToVarEnv,
    CreateStrictImmutableLexBinding,
    CreateNonStrictImmutableLexBinding,
    CreatePermanentMutableLexBinding,
    CreatePermanentMutableVarBinding,
    CreateInitializedPermanentMutableLexIfMissing,
    CreatePermanentMutableLexIfMissing,
    InitializeLexBinding,
    GetLexBinding,
    InitializeVarBinding,
    SetMutableVarBinding,
    CreatePerIterationEnvironment,
    Object,
    Array,
    CreateDataProperty,
    SetPrototype,
    ToPropertyKey,
    CopyDataProps,
    CopyDataPropsWithExclusions,
    ToString,
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
    InstantiateIdFreeFunctionExpression,
    InstantiateOrdinaryFunctionExpression,
    InstantiateArrowFunctionExpression,
    InstantiateOrdinaryFunctionObject,
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
    CreateUnmappedArguments,
    CreateMappedArguments,
    AddMappedArgument,
    HandleEmptyBreak,
    HandleTargetedBreak,
    CoalesceValue,
    LoopContinues,
    Continue,
    TargetedContinue,
    Break,
    TargetedBreak,
    ExtractArg,
    FinishArgs,
    ExtractThrownValue,
    SwapList,
    PopList,
    RequireConstructor,
    Construct,
    IteratorAccumulate,
    IterateArguments,
    RequireCoercible,
    GetSyncIterator,
    IteratorCloseIfNotDone,
    GetV,
    IteratorDAEElision,
    EmbellishedIteratorStep,
    IteratorRest,
}

impl fmt::Display for Insn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.pad(match self {
            Insn::Nop => "NOP",
            Insn::ToDo => "TODO",
            Insn::String => "STRING",
            Insn::Resolve => "RESOLVE",
            Insn::StrictResolve => "STRICT_RESOLVE",
            Insn::This => "THIS",
            Insn::Null => "NULL",
            Insn::Undefined => "UNDEFINED",
            Insn::True => "TRUE",
            Insn::False => "FALSE",
            Insn::Zero => "ZERO",
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
            Insn::JumpIfNotUndef => "JUMP_NOT_UNDEF",
            Insn::JumpNotThrow => "JUMP_NOT_THROW",
            Insn::Call => "CALL",
            Insn::EndFunction => "END_FUNCTION",
            Insn::Return => "RETURN",
            Insn::UpdateEmpty => "UPDATE_EMPTY",
            Insn::Swap => "SWAP",
            Insn::Pop => "POP",
            Insn::PopOrPanic => "POP_PANIC",
            Insn::Pop2Push3 => "POP2_PUSH3",
            Insn::Dup => "DUP",
            Insn::RotateUp => "ROTATEUP",
            Insn::RotateDown => "ROTATEDOWN",
            Insn::RotateDownList => "ROTATEDOWN_LIST",
            Insn::Unwind => "UNWIND",
            Insn::UnwindList => "UNWIND_LIST",
            Insn::AppendList => "APPEND_LIST",
            Insn::Ref => "REF",
            Insn::StrictRef => "STRICT_REF",
            Insn::InitializeReferencedBinding => "IRB",
            Insn::PushNewLexEnv => "PNLE",
            Insn::PopLexEnv => "PLE",
            Insn::PushNewVarEnvFromLex => "PNVEFL",
            Insn::PushNewLexEnvFromVar => "PNLEFV",
            Insn::SetLexEnvToVarEnv => "SLETVE",
            Insn::CreateStrictImmutableLexBinding => "CSILB",
            Insn::CreateNonStrictImmutableLexBinding => "CNSILB",
            Insn::CreatePermanentMutableLexBinding => "CPMLB",
            Insn::CreateInitializedPermanentMutableLexIfMissing => "CIPMLBM",
            Insn::CreatePermanentMutableLexIfMissing => "CPMLBM",
            Insn::CreatePermanentMutableVarBinding => "CPMVB",
            Insn::InitializeLexBinding => "ILB",
            Insn::GetLexBinding => "GLB",
            Insn::InitializeVarBinding => "IVB",
            Insn::SetMutableVarBinding => "SMVB",
            Insn::CreatePerIterationEnvironment => "CPIE",
            Insn::Object => "OBJECT",
            Insn::Array => "ARRAY",
            Insn::CreateDataProperty => "CR_PROP",
            Insn::SetPrototype => "SET_PROTO",
            Insn::ToPropertyKey => "TO_KEY",
            Insn::CopyDataProps => "COPY_DATA_PROPS",
            Insn::CopyDataPropsWithExclusions => "COPY_DATAPROPS_WE",
            Insn::ToString => "TO_STRING",
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
            Insn::InstantiateIdFreeFunctionExpression => "FUNC_IIFE",
            Insn::InstantiateOrdinaryFunctionExpression => "FUNC_IOFE",
            Insn::InstantiateArrowFunctionExpression => "FUNC_IAE",
            Insn::InstantiateOrdinaryFunctionObject => "FUNC_OBJ",
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
            Insn::CreateUnmappedArguments => "CUA",
            Insn::CreateMappedArguments => "CMA",
            Insn::AddMappedArgument => "AMA",
            Insn::HandleEmptyBreak => "HEB",
            Insn::HandleTargetedBreak => "HTB",
            Insn::CoalesceValue => "COALESCE",
            Insn::LoopContinues => "LOOP_CONT",
            Insn::Continue => "CONTINUE",
            Insn::TargetedContinue => "CONTINUE_WITH",
            Insn::Break => "BREAK",
            Insn::TargetedBreak => "BREAK_FROM",
            Insn::ExtractArg => "EXTRACT_ARG",
            Insn::FinishArgs => "FINISH_ARGS",
            Insn::ExtractThrownValue => "EXTRACT_THROW",
            Insn::SwapList => "SWAP_LIST",
            Insn::PopList => "POP_LIST",
            Insn::RequireConstructor => "REQ_CSTR",
            Insn::Construct => "CONSTRUCT",
            Insn::IteratorAccumulate => "ITERATOR_ACCUM",
            Insn::IterateArguments => "ITER_ARGS",
            Insn::RequireCoercible => "REQ_COER",
            Insn::GetSyncIterator => "GET_SYNC_ITER",
            Insn::IteratorCloseIfNotDone => "ITER_CLOSE_IF_NOT_DONE",
            Insn::GetV => "GETV",
            Insn::IteratorDAEElision => "IDAE_ELISION",
            Insn::EmbellishedIteratorStep => "ITER_STEP",
            Insn::IteratorRest => "ITER_REST",
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
pub struct AlwaysAbruptResult;
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
pub struct AlwaysRefResult;
impl From<AlwaysRefResult> for CompilerStatusFlags {
    fn from(_: AlwaysRefResult) -> Self {
        Self::new().reference(true)
    }
}

#[derive(Debug)]
pub struct AlwaysAbruptRefResult;
impl From<AlwaysAbruptRefResult> for CompilerStatusFlags {
    fn from(_: AlwaysAbruptRefResult) -> Self {
        Self::new().reference(true).abrupt(true)
    }
}

#[derive(Debug)]
pub struct NeverAbruptRefResult;
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

#[derive(Debug)]
pub enum NameableProduction {
    Function(Rc<FunctionExpression>),
    Generator(Rc<GeneratorExpression>),
    AsyncFunction(Rc<AsyncFunctionExpression>),
    AsyncGenerator(Rc<AsyncGeneratorExpression>),
    Class(Rc<ClassExpression>),
    Arrow(Rc<ArrowFunction>),
    AsyncArrow(Rc<AsyncArrowFunction>),
}
impl fmt::Display for NameableProduction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NameableProduction::Function(node) => node.fmt(f),
            NameableProduction::Generator(node) => node.fmt(f),
            NameableProduction::AsyncFunction(node) => node.fmt(f),
            NameableProduction::AsyncGenerator(node) => node.fmt(f),
            NameableProduction::Class(node) => node.fmt(f),
            NameableProduction::Arrow(node) => node.fmt(f),
            NameableProduction::AsyncArrow(node) => node.fmt(f),
        }
    }
}
impl TryFrom<Rc<Initializer>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<Initializer>) -> Result<Self, Self::Error> {
        NameableProduction::try_from(value.ae.clone())
    }
}
impl TryFrom<Rc<AssignmentExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<AssignmentExpression>) -> Result<Self, Self::Error> {
        match &*value {
            AssignmentExpression::FallThru(child) => NameableProduction::try_from(child.clone()),
            AssignmentExpression::Arrow(child) => Ok(NameableProduction::Arrow(child.clone())),
            AssignmentExpression::AsyncArrow(child) => Ok(NameableProduction::AsyncArrow(child.clone())),
            AssignmentExpression::Yield(_)
            | AssignmentExpression::Assignment(_, _)
            | AssignmentExpression::OpAssignment(_, _, _)
            | AssignmentExpression::LandAssignment(_, _)
            | AssignmentExpression::LorAssignment(_, _)
            | AssignmentExpression::CoalAssignment(_, _)
            | AssignmentExpression::Destructuring(_, _) => Err(anyhow!("Production not nameable")),
        }
    }
}
impl TryFrom<Rc<ConditionalExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<ConditionalExpression>) -> Result<Self, Self::Error> {
        match &*value {
            ConditionalExpression::FallThru(child) => NameableProduction::try_from(child.clone()),
            ConditionalExpression::Conditional(_, _, _) => Err(anyhow!("Production not nameable")),
        }
    }
}
impl TryFrom<Rc<ShortCircuitExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<ShortCircuitExpression>) -> Result<Self, Self::Error> {
        match &*value {
            ShortCircuitExpression::LogicalORExpression(child) => NameableProduction::try_from(child.clone()),
            ShortCircuitExpression::CoalesceExpression(_) => Err(anyhow!("Production not nameable")),
        }
    }
}
impl TryFrom<Rc<LogicalORExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<LogicalORExpression>) -> Result<Self, Self::Error> {
        match &*value {
            LogicalORExpression::LogicalANDExpression(child) => NameableProduction::try_from(child.clone()),
            LogicalORExpression::LogicalOR(..) => Err(anyhow!("Production not nameable")),
        }
    }
}
impl TryFrom<Rc<LogicalANDExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<LogicalANDExpression>) -> Result<Self, Self::Error> {
        match &*value {
            LogicalANDExpression::BitwiseORExpression(child) => NameableProduction::try_from(child.clone()),
            LogicalANDExpression::LogicalAND(..) => Err(anyhow!("Production not nameable")),
        }
    }
}
impl TryFrom<Rc<BitwiseORExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<BitwiseORExpression>) -> Result<Self, Self::Error> {
        match &*value {
            BitwiseORExpression::BitwiseXORExpression(child) => NameableProduction::try_from(child.clone()),
            BitwiseORExpression::BitwiseOR(..) => Err(anyhow!("Production not nameable")),
        }
    }
}
impl TryFrom<Rc<BitwiseXORExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<BitwiseXORExpression>) -> Result<Self, Self::Error> {
        match &*value {
            BitwiseXORExpression::BitwiseANDExpression(child) => NameableProduction::try_from(child.clone()),
            BitwiseXORExpression::BitwiseXOR(..) => Err(anyhow!("Production not nameable")),
        }
    }
}
impl TryFrom<Rc<BitwiseANDExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<BitwiseANDExpression>) -> Result<Self, Self::Error> {
        match &*value {
            BitwiseANDExpression::EqualityExpression(child) => NameableProduction::try_from(child.clone()),
            BitwiseANDExpression::BitwiseAND(..) => Err(anyhow!("Production not nameable")),
        }
    }
}
impl TryFrom<Rc<EqualityExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<EqualityExpression>) -> Result<Self, Self::Error> {
        match &*value {
            EqualityExpression::RelationalExpression(child) => NameableProduction::try_from(child.clone()),
            EqualityExpression::Equal(_, _)
            | EqualityExpression::NotEqual(_, _)
            | EqualityExpression::StrictEqual(_, _)
            | EqualityExpression::NotStrictEqual(_, _) => Err(anyhow!("Production not nameable")),
        }
    }
}
impl TryFrom<Rc<RelationalExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<RelationalExpression>) -> Result<Self, Self::Error> {
        match &*value {
            RelationalExpression::ShiftExpression(child) => NameableProduction::try_from(child.clone()),
            RelationalExpression::Less(_, _)
            | RelationalExpression::Greater(_, _)
            | RelationalExpression::LessEqual(_, _)
            | RelationalExpression::GreaterEqual(_, _)
            | RelationalExpression::InstanceOf(_, _)
            | RelationalExpression::In(_, _)
            | RelationalExpression::PrivateIn(_, _, _) => Err(anyhow!("Production not nameable")),
        }
    }
}
impl TryFrom<Rc<ShiftExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<ShiftExpression>) -> Result<Self, Self::Error> {
        match &*value {
            ShiftExpression::AdditiveExpression(child) => NameableProduction::try_from(child.clone()),
            ShiftExpression::LeftShift(_, _)
            | ShiftExpression::SignedRightShift(_, _)
            | ShiftExpression::UnsignedRightShift(_, _) => Err(anyhow!("Production not nameable")),
        }
    }
}
impl TryFrom<Rc<AdditiveExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<AdditiveExpression>) -> Result<Self, Self::Error> {
        match &*value {
            AdditiveExpression::MultiplicativeExpression(child) => NameableProduction::try_from(child.clone()),
            AdditiveExpression::Add(_, _) | AdditiveExpression::Subtract(_, _) => {
                Err(anyhow!("Production not nameable"))
            }
        }
    }
}
impl TryFrom<Rc<MultiplicativeExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<MultiplicativeExpression>) -> Result<Self, Self::Error> {
        match &*value {
            MultiplicativeExpression::ExponentiationExpression(child) => NameableProduction::try_from(child.clone()),
            MultiplicativeExpression::MultiplicativeExpressionExponentiationExpression(..) => {
                Err(anyhow!("Production not nameable"))
            }
        }
    }
}
impl TryFrom<Rc<ExponentiationExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<ExponentiationExpression>) -> Result<Self, Self::Error> {
        match &*value {
            ExponentiationExpression::UnaryExpression(child) => NameableProduction::try_from(child.clone()),
            ExponentiationExpression::Exponentiation(..) => Err(anyhow!("Production not nameable")),
        }
    }
}
impl TryFrom<Rc<UnaryExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<UnaryExpression>) -> Result<Self, Self::Error> {
        match &*value {
            UnaryExpression::UpdateExpression(child) => NameableProduction::try_from(child.clone()),
            UnaryExpression::Delete { .. }
            | UnaryExpression::Void { .. }
            | UnaryExpression::Typeof { .. }
            | UnaryExpression::NoOp { .. }
            | UnaryExpression::Negate { .. }
            | UnaryExpression::Complement { .. }
            | UnaryExpression::Not { .. }
            | UnaryExpression::Await(..) => Err(anyhow!("Production not nameable")),
        }
    }
}
impl TryFrom<Rc<UpdateExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<UpdateExpression>) -> Result<Self, Self::Error> {
        match &*value {
            UpdateExpression::LeftHandSideExpression(child) => NameableProduction::try_from(child.clone()),
            UpdateExpression::PostIncrement { .. }
            | UpdateExpression::PostDecrement { .. }
            | UpdateExpression::PreIncrement { .. }
            | UpdateExpression::PreDecrement { .. } => Err(anyhow!("Production not nameable")),
        }
    }
}
impl TryFrom<Rc<LeftHandSideExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<LeftHandSideExpression>) -> Result<Self, Self::Error> {
        match &*value {
            LeftHandSideExpression::New(child) => NameableProduction::try_from(child.clone()),
            LeftHandSideExpression::Call(_) | LeftHandSideExpression::Optional(_) => {
                Err(anyhow!("Production not nameable"))
            }
        }
    }
}
impl TryFrom<Rc<NewExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<NewExpression>) -> Result<Self, Self::Error> {
        match &*value {
            NewExpression::MemberExpression(child) => NameableProduction::try_from(child.clone()),
            NewExpression::NewExpression(_, _) => Err(anyhow!("Production not nameable")),
        }
    }
}
impl TryFrom<Rc<MemberExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<MemberExpression>) -> Result<Self, Self::Error> {
        match &*value {
            MemberExpression::PrimaryExpression(child) => NameableProduction::try_from(child.clone()),
            MemberExpression::Expression(_, _, _)
            | MemberExpression::IdentifierName(_, _, _)
            | MemberExpression::TemplateLiteral(_, _)
            | MemberExpression::SuperProperty(_)
            | MemberExpression::MetaProperty(_)
            | MemberExpression::NewArguments(_, _, _)
            | MemberExpression::PrivateId(_, _, _) => Err(anyhow!("Production not nameable")),
        }
    }
}
impl TryFrom<Rc<PrimaryExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<PrimaryExpression>) -> Result<Self, Self::Error> {
        match &*value {
            PrimaryExpression::Function { node } => Ok(NameableProduction::Function(node.clone())),
            PrimaryExpression::Class { node } => Ok(NameableProduction::Class(node.clone())),
            PrimaryExpression::Generator { node } => Ok(NameableProduction::Generator(node.clone())),
            PrimaryExpression::AsyncFunction { node } => Ok(NameableProduction::AsyncFunction(node.clone())),
            PrimaryExpression::AsyncGenerator { node } => Ok(NameableProduction::AsyncGenerator(node.clone())),
            PrimaryExpression::Parenthesized { node } => NameableProduction::try_from(node.clone()),
            PrimaryExpression::This { .. }
            | PrimaryExpression::IdentifierReference { .. }
            | PrimaryExpression::Literal { .. }
            | PrimaryExpression::ArrayLiteral { .. }
            | PrimaryExpression::ObjectLiteral { .. }
            | PrimaryExpression::TemplateLiteral { .. }
            | PrimaryExpression::RegularExpression { .. } => Err(anyhow!("Production not nameable")),
        }
    }
}
impl TryFrom<Rc<ParenthesizedExpression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<ParenthesizedExpression>) -> Result<Self, Self::Error> {
        NameableProduction::try_from(value.exp.clone())
    }
}
impl TryFrom<Rc<Expression>> for NameableProduction {
    type Error = anyhow::Error;
    fn try_from(value: Rc<Expression>) -> Result<Self, Self::Error> {
        match &*value {
            Expression::FallThru(child) => NameableProduction::try_from(child.clone()),
            Expression::Comma(_, _) => Err(anyhow!("Production not nameable")),
        }
    }
}
impl NameableProduction {
    fn compile_named_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        id: NameLoc,
    ) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            NameableProduction::Function(child) => {
                child.compile_named_evaluation(chunk, strict, text, child.clone(), id).map(CompilerStatusFlags::from)
            }
            NameableProduction::Generator(_) => todo!(),
            NameableProduction::AsyncFunction(_) => todo!(),
            NameableProduction::AsyncGenerator(_) => todo!(),
            NameableProduction::Class(_) => todo!(),
            NameableProduction::Arrow(child) => {
                child.compile_named_evaluation(chunk, strict, text, child.clone(), id).map(CompilerStatusFlags::from)
            }
            NameableProduction::AsyncArrow(_) => todo!(),
        }
    }

    pub fn is_named_function(&self) -> bool {
        match self {
            NameableProduction::Function(node) => node.is_named_function(),
            NameableProduction::Generator(node) => node.is_named_function(),
            NameableProduction::AsyncFunction(node) => node.is_named_function(),
            NameableProduction::AsyncGenerator(node) => node.is_named_function(),
            NameableProduction::Class(node) => node.is_named_function(),
            NameableProduction::Arrow(_) => false,
            NameableProduction::AsyncArrow(_) => false,
        }
    }

    pub fn params(&self) -> ParamSource {
        match self {
            NameableProduction::Function(node) => node.params.clone().into(),
            NameableProduction::Generator(node) => node.params.clone().into(),
            NameableProduction::AsyncFunction(node) => node.params.clone().into(),
            NameableProduction::AsyncGenerator(node) => node.params.clone().into(),
            NameableProduction::Class(_) => panic!("Trying to get the parameters block from a class"),
            NameableProduction::Arrow(node) => node.parameters.clone().into(),
            NameableProduction::AsyncArrow(node) => node.params(),
        }
    }

    pub fn body(&self) -> BodySource {
        match self {
            NameableProduction::Function(node) => node.body.clone().into(),
            NameableProduction::Generator(node) => node.body.clone().into(),
            NameableProduction::AsyncFunction(node) => node.body.clone().into(),
            NameableProduction::AsyncGenerator(node) => node.body.clone().into(),
            NameableProduction::Class(_) => panic!("Trying to get the body of a class"),
            NameableProduction::Arrow(node) => node.body.clone().into(),
            NameableProduction::AsyncArrow(node) => node.body(),
        }
    }
}

impl AsyncArrowFunction {
    pub fn params(&self) -> ParamSource {
        match self {
            AsyncArrowFunction::IdentOnly(id, _, _) => id.clone().into(),
            AsyncArrowFunction::Formals(ah, _) => ah.params.clone().into(),
        }
    }

    pub fn body(&self) -> BodySource {
        match self {
            AsyncArrowFunction::IdentOnly(_, body, _) | AsyncArrowFunction::Formals(_, body) => body.clone().into(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum EnvUsage {
    UsePutValue,
    UseCurrentLexical,
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
        Ok(AlwaysAbruptRefResult)
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
            PrimaryExpression::ArrayLiteral { node: al } => {
                al.compile(chunk, strict, text).map(CompilerStatusFlags::from)
            }
            PrimaryExpression::TemplateLiteral { node } => {
                node.compile(chunk, strict, text).map(CompilerStatusFlags::from)
            }
            PrimaryExpression::Function { node } => {
                node.compile(chunk, strict, text, node.clone()).map(CompilerStatusFlags::from)
            }
            PrimaryExpression::Class { node } => todo!(),
            PrimaryExpression::Generator { node } => todo!(),
            PrimaryExpression::AsyncFunction { node } => todo!(),
            PrimaryExpression::AsyncGenerator { node } => todo!(),
            PrimaryExpression::RegularExpression { regex, location } => todo!(),
        }
    }
}

#[cfg(test)]
fn compile_debug_lit(chunk: &mut Chunk, ch: &DebugKind) {
    match *ch {
        DebugKind::Char('@') => {
            // Break future jumps (by adding enough instructions that the offsets don't fit in an i16)
            for _ in 0..32768 {
                chunk.op(Insn::Nop);
            }
            chunk.op(Insn::False);
        }
        DebugKind::Char('3') => {
            // Break some future jumps (by adding enough instructions that the larger offsets don't fit in an i16)
            for _ in 0..32768 - 3 {
                chunk.op(Insn::Nop);
            }
            chunk.op(Insn::False);
        }
        DebugKind::Char('4') => {
            // Break some future jumps (by adding enough instructions that the larger offsets don't fit in an i16)
            for _ in 0..32768 - 4 {
                chunk.op(Insn::Nop);
            }
            chunk.op(Insn::False);
        }
        DebugKind::Char('9') => {
            // Break some future jumps (by adding enough instructions that the larger offsets don't fit in an i16)
            for _ in 0..32768 - 50 {
                chunk.op(Insn::Nop);
            }
            chunk.op(Insn::False);
        }
        DebugKind::Number(num) => {
            // Break some future jumps (by adding enough instructions that the larger offsets don't fit in an i16)
            for _ in 0..32768 - num {
                chunk.op(Insn::Nop);
            }
            chunk.op(Insn::False);
        }
        DebugKind::Char('!') => {
            // Fill the string table.
            chunk.strings.resize(65536, JSString::from("not to be used from integration tests"));
            chunk.op(Insn::False);
        }
        DebugKind::Char('#') => {
            // Fill the float table.
            chunk.floats.resize(65536, 10.1);
            chunk.op(Insn::False);
        }
        DebugKind::Char('$') => {
            // Fill the bigint table.
            chunk.bigints.resize(65536, Rc::new(BigInt::from(97687897890734187890106587314876543219_u128)));
            chunk.op(Insn::False);
        }
        _ => (),
    }
}
#[cfg(not(test))]
fn compile_debug_lit(_: &mut Chunk, _: &DebugKind) {}

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
        Ok(NeverAbruptRefResult)
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

impl Elisions {
    pub fn array_accumulation(&self, chunk: &mut Chunk) -> anyhow::Result<AlwaysAbruptResult> {
        // start:               next_index array
        // FLOAT self.count     count next_index array
        // ADD                  next_index array
        // SWAP                 array next_index
        // DUP                  array array next_index
        // STRING "length"      str array array next_index
        // STRICT_REF           ref array next_index
        // ROTATE_UP 3          next_index ref array
        // POP2PUSH3            next_index ref next_index array
        // PUT_VALUE            [empty]/err next_index array
        // JUMP_IF_NORMAL fwd
        // UNWIND 2             err
        // JUMP exit
        // fwd:                 [empty] next_index array
        // POP                  next_index array
        // exit:

        let count = self.count as f64; // loss of accuracy for large values.
        let count_index = chunk.add_to_float_pool(count)?;
        let length_index = chunk.add_to_string_pool(JSString::from("length"))?;

        // stack: next_index array
        chunk.op_plus_arg(Insn::Float, count_index);
        // stack: count next_index array
        chunk.op(Insn::Add);
        // stack: next_index array
        chunk.op(Insn::Swap);
        // stack: array next_index
        chunk.op(Insn::Dup);
        // stack: array array next_index
        chunk.op_plus_arg(Insn::String, length_index);
        // stack: "length" array array next_index
        chunk.op(Insn::StrictRef);
        // stack: ref array next_index
        chunk.op_plus_arg(Insn::RotateUp, 3);
        // stack: next_index ref array
        chunk.op(Insn::Pop2Push3);
        // stack: next_index ref next_index array
        chunk.op(Insn::PutValue);
        // stack: [empty]/err next_index array
        let fwd = chunk.op_jump(Insn::JumpIfNormal);
        // stack: err next_index array
        chunk.op_plus_arg(Insn::Unwind, 2);
        // stack: err
        let exit = chunk.op_jump(Insn::Jump);
        chunk.fixup(fwd).expect("Jump too short to fail");
        // stack: [empty] next_index array
        chunk.op(Insn::Pop);
        // stack: next_index array
        chunk.fixup(exit).expect("Jump too short to fail");

        // Stack: (next_index array) or (err)
        Ok(AlwaysAbruptResult)
    }

    pub fn iterator_destructuring_assignment_evaluation(
        &self,
        chunk: &mut Chunk,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: IteratorDestructuringAssignmentEvaluation
        // The syntax-directed operation IteratorDestructuringAssignmentEvaluation takes argument iteratorRecord (an
        // Iterator Record) and returns either a normal completion containing unused or an abrupt completion. It is
        // defined piecewise over the following productions:

        // Elision : ,
        //  1. If iteratorRecord.[[Done]] is false, then
        //      a. Let next be Completion(IteratorStep(iteratorRecord)).
        //      b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
        //      c. ReturnIfAbrupt(next).
        //      d. If next is false, set iteratorRecord.[[Done]] to true.
        //  2. Return unused.
        // Elision : Elision ,
        //  1. Perform ? IteratorDestructuringAssignmentEvaluation of Elision with argument iteratorRecord.
        //  2. If iteratorRecord.[[Done]] is false, then
        //      a. Let next be Completion(IteratorStep(iteratorRecord)).
        //      b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
        //      c. ReturnIfAbrupt(next).
        //      d. If next is false, set iteratorRecord.[[Done]] to true.
        //  3. Return unused.

        // start:                      ir
        //   FLOAT count               count ir
        //   IDAE_ELISION              ir/err

        let count_val = self.count as f64; // loss of accuracy for large values.
        let count = chunk.add_to_float_pool(count_val)?;
        // have to store count on the stack, as it can easily overflow the u16 that is an instruction parameter
        chunk.op_plus_arg(Insn::Float, count);
        chunk.op(Insn::IteratorDAEElision);

        Ok(AlwaysAbruptResult)
    }
}

impl ElementList {
    pub fn array_accumulation(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match self {
            ElementList::AssignmentExpression { elision, ae } => {
                // elision_start:     next_index array
                // <elision>          (next_index array) or (err)
                // JUMP_IF_ABRUPT     exit
                // no_elision_start:  next_index array
                // POP2PUSH3          next_index array next_index
                // TO_PROPKEY         key array next_index
                // <ass_expr>         ref/val/err key array next_index
                // GET_VALUE          val/err key array next_index
                // JUMP_IF_NORMAL fwd
                // UNWIND 3           err
                // JUMP exit
                // fwd:               val key array next_index
                // CDP                array next_index
                // SWAP               next_index array
                // INC                next_index array
                // exit: (next_index array) or (err)

                let mut exit_status = AbruptResult::Never;
                // Stack: next_index array ...
                let elision_exit = if let Some(elision) = elision {
                    exit_status = AbruptResult::Maybe;
                    elision.array_accumulation(chunk)?;
                    // Stack: (next_index array) or (err) ...
                    Some(chunk.op_jump(Insn::JumpIfAbrupt))
                    // Stack: next_index array
                } else {
                    None
                };
                // Stack: next_index array ...
                chunk.op(Insn::Pop2Push3);
                // Stack: next_index array next_index ...
                chunk.op(Insn::ToPropertyKey);
                // Stack: key array next_index ...
                let status = ae.compile(chunk, strict, text)?;
                // Stack: val/ref/err key array next_index ...
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                let ae_exit = if status.maybe_abrupt() || status.maybe_ref() {
                    exit_status = AbruptResult::Maybe;
                    // Stack: val/err key array next_index ...
                    let mark = chunk.op_jump(Insn::JumpIfNormal);
                    // Stack: err key array next_index ...
                    chunk.op_plus_arg(Insn::Unwind, 3);
                    // Stack: err ...
                    let exit = chunk.op_jump(Insn::Jump);
                    chunk.fixup(mark).expect("Jump is too short to overflow.");
                    Some(exit)
                } else {
                    None
                };
                // Stack: val key array next_index ...
                chunk.op(Insn::CreateDataProperty);
                // Stack: array next_index ...
                chunk.op(Insn::Swap);
                // Stack: next_index array ...
                chunk.op(Insn::Increment);
                // Stack: next_index array ...
                if let Some(exit) = elision_exit {
                    chunk.fixup(exit)?;
                }
                if let Some(exit) = ae_exit {
                    chunk.fixup(exit).expect("Jump too short to fail");
                }
                // Stack: (next_index array) or (err)
                Ok(exit_status)
            }
            ElementList::SpreadElement { elision, se } => {
                // elision_start:     next_index array
                // <elision>          (next_index array) or (err)
                // JUMP_IF_ABRUPT     exit
                // no_elision_start:  next_index array
                // <se>               (next_index array) or (err)
                // exit:
                // Stack: next_index array ...
                let elision_exit = if let Some(elision) = elision {
                    elision.array_accumulation(chunk)?;
                    // Stack: (next_index array) or (err) ...
                    Some(chunk.op_jump(Insn::JumpIfAbrupt))
                    // Stack: next_index array
                } else {
                    None
                };
                // Stack: next_index array ...
                se.array_accumulation(chunk, strict, text)?;
                if let Some(exit) = elision_exit {
                    chunk.fixup(exit)?;
                }
                // Stack: (next_index array) or (err)
                Ok(AbruptResult::Maybe)
            }
            ElementList::ElementListAssignmentExpression { el, elision, ae } => {
                // start:               next_index array
                // <el>                 (next_index array) or (err)
                // JUMP_IF_ABRUPT exit
                // ----- if elsision present ------
                // <elision>            (next_index array) or (err)
                // JUMP_IF_ABRUPT exit
                // --------------------------------
                //                    next_index array
                // POP2PUSH3          next_index array next_index
                // TO_PROPKEY         key array next_index
                // <ass_expr>         ref/val/err key array next_index
                // GET_VALUE          val/err key array next_index
                // JUMP_IF_NORMAL fwd
                // UNWIND 3           err
                // JUMP exit
                // fwd:               val key array next_index
                // CDP                array next_index
                // SWAP               next_index array
                // INC                next_index array
                // exit: (next_index array) or (err)

                let mut exit_status = AbruptResult::Never;
                let mut exits = vec![];
                // Stack: next_index array ...
                let status = el.array_accumulation(chunk, strict, text)?;
                // Stack: err / (next_index array) ...
                if status.maybe_abrupt() {
                    exit_status = AbruptResult::Maybe;
                    exits.push(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                // Stack: next_index array ...
                if let Some(elision) = elision {
                    elision.array_accumulation(chunk)?;
                    exits.push(chunk.op_jump(Insn::JumpIfAbrupt));
                    exit_status = AbruptResult::Maybe;
                }
                // Stack: next_index array ...
                chunk.op(Insn::Pop2Push3);
                // Stack: next_index array next_index ...
                chunk.op(Insn::ToPropertyKey);
                // Stack: key array next_index ...
                let status = ae.compile(chunk, strict, text)?;
                // Stack: val/ref/err key array next_index ...
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                if status.maybe_abrupt() || status.maybe_ref() {
                    // Stack: val/err key array next_index ...
                    let mark = chunk.op_jump(Insn::JumpIfNormal);
                    // Stack: err key array next_index ...
                    chunk.op_plus_arg(Insn::Unwind, 3);
                    // Stack: err ...
                    exits.push(chunk.op_jump(Insn::Jump));
                    chunk.fixup(mark).expect("Jump is too short to overflow.");
                    exit_status = AbruptResult::Maybe;
                }
                // Stack: val key array next_index ...
                chunk.op(Insn::CreateDataProperty);
                // Stack: array next_index ...
                chunk.op(Insn::Swap);
                // Stack: next_index array ...
                chunk.op(Insn::Increment);
                // Stack: next_index array ...
                for exit in exits {
                    chunk.fixup(exit)?;
                }
                // Stack: (err) or (next_index array) ...
                Ok(exit_status)
            }
            ElementList::ElementListSpreadElement { el, elision, se } => {
                // start:               next_index array
                // <el>                 (next_index array) or (err)
                // JUMP_IF_ABRUPT exit
                // ----- if elsision present ------
                // <elision>            (next_index array) or (err)
                // JUMP_IF_ABRUPT exit
                // --------------------------------
                //                    next_index array
                // <se>               (next_index array) or (err)
                // exit: (next_index array) or (err)
                let mut exits = vec![];
                let status = el.array_accumulation(chunk, strict, text)?;
                if status.maybe_abrupt() {
                    exits.push(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                if let Some(elision) = elision {
                    elision.array_accumulation(chunk)?;
                    exits.push(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                se.array_accumulation(chunk, strict, text)?;
                for exit in exits {
                    chunk.fixup(exit).expect("Jumps are too short to fail");
                }
                Ok(AbruptResult::Maybe)
            }
        }
    }
}

impl SpreadElement {
    pub fn array_accumulation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // start:               next_index array
        // <ae>                 ref/val/err next_index array
        // GET_VALUE            val/err next_index array
        // JUMP_IF_NORMAL fwd
        // UNWIND 2             err
        // JUMP exit
        // fwd:                 val next_index array
        // ITERATOR_ACCUMULATE  (next_index array) or err
        // exit:

        let mut exit = None;
        // Stack: next_index array ...
        let status = self.ae.compile(chunk, strict, text)?;
        // Stack: val/ref/err next_index array ...
        if status.maybe_ref() {
            chunk.op(Insn::GetValue);
        }
        // Stack: val/err next_index array ...
        if status.maybe_abrupt() || status.maybe_ref() {
            let mark = chunk.op_jump(Insn::JumpIfNormal);
            // Stack: err next_index array ...
            chunk.op_plus_arg(Insn::Unwind, 2);
            // Stack: err ...
            exit = Some(chunk.op_jump(Insn::Jump));
            chunk.fixup(mark).expect("Jump is too short to overflow.");
        }
        // Stack: val next_index array ...
        chunk.op(Insn::IteratorAccumulate);
        // Stack: (next_index array) or (err) ...

        if let Some(exit) = exit {
            chunk.fixup(exit).expect("Jump too short to fail");
        }
        Ok(AlwaysAbruptResult)
    }
}

impl ArrayLiteral {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match self {
            ArrayLiteral::Empty { elision, .. } => {
                // start:
                // ARRAY             array
                // ---- if elision ----
                // ZERO              next_index array
                // <elision>         (err) or (next_index array)
                // JUMP_IF_ABRUPT exit
                // POP               array
                // --------------------
                // exit:             err/array
                let mut exit_status = AbruptResult::Never;
                chunk.op(Insn::Array);
                if let Some(elisions) = elision {
                    chunk.op(Insn::Zero);
                    elisions.array_accumulation(chunk)?;
                    exit_status = AbruptResult::Maybe;
                    let mark = chunk.op_jump(Insn::JumpIfAbrupt);
                    chunk.op(Insn::Pop);
                    chunk.fixup(mark).expect("Too short to fail");
                }
                Ok(exit_status)
            }
            ArrayLiteral::ElementList { el, .. } => {
                // start:
                // ARRAY             array
                // ZERO              next_index array
                // <elem_list>       err/(next_index array)
                // JUMP_IF_ABRUPT exit
                // POP               array
                // exit:
                let mut exit_status = AbruptResult::Never;
                chunk.op(Insn::Array);
                chunk.op(Insn::Zero);
                let status = el.array_accumulation(chunk, strict, text)?;
                let mark = if status.maybe_abrupt() {
                    exit_status = AbruptResult::Maybe;
                    Some(chunk.op_jump(Insn::JumpIfAbrupt))
                } else {
                    None
                };
                chunk.op(Insn::Pop);
                if let Some(mark) = mark {
                    chunk.fixup(mark).expect("Too short to fail");
                }
                Ok(exit_status)
            }
            ArrayLiteral::ElementListElision { el, elision, .. } => {
                // start:
                // ARRAY             array
                // ZERO              next_index array
                // <elem_list>       err/(next_index array)
                // JUMP_IF_ABRUPT exit
                // ----- elision present ------
                //                   next_index array
                // <elision>         err/(next_index array)
                // JUMP_IF_ABRUPT exit
                // ----------------------------
                //                   next_index array
                // POP               array
                // exit:             err/array
                let mut exits = vec![];
                let mut exit_status = AbruptResult::Never;

                chunk.op(Insn::Array);
                chunk.op(Insn::Zero);
                // Stack: next_index array ...
                let status = el.array_accumulation(chunk, strict, text)?;
                // Stack: err/(next_index array) ...
                if status.maybe_abrupt() {
                    exit_status = AbruptResult::Maybe;
                    exits.push(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                if let Some(elisions) = elision {
                    exit_status = AbruptResult::Maybe;
                    // Stack: next_index array ...
                    elisions.array_accumulation(chunk)?;
                    exits.push(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                chunk.op(Insn::Pop);
                for exit in exits {
                    chunk.fixup(exit).expect("Jump is too short to fail");
                }
                Ok(exit_status)
            }
        }
    }
}

impl TemplateLiteral {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match self {
            TemplateLiteral::NoSubstitutionTemplate { data, .. } => {
                // Runtime Semantics: Evaluation
                // TemplateLiteral : NoSubstitutionTemplate
                //  1. Return the TV of NoSubstitutionTemplate.
                let tv = data.tv.as_ref().expect("Template literals as expressions should not be raw-only");
                let idx = chunk.add_to_string_pool(tv.clone())?;
                chunk.op_plus_arg(Insn::String, idx);
                Ok(AbruptResult::Never)
            }
            TemplateLiteral::SubstitutionTemplate(st) => st.compile(chunk, strict, text).map(AbruptResult::from),
        }
    }
}

impl SubstitutionTemplate {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AlwaysAbruptResult> {
        // SubstitutionTemplate : TemplateHead Expression TemplateSpans
        //  1. Let head be the TV of TemplateHead as defined in 12.9.6.
        //  2. Let subRef be ? Evaluation of Expression.
        //  3. Let sub be ? GetValue(subRef).
        //  4. Let middle be ? ToString(sub).
        //  5. Let tail be ? Evaluation of TemplateSpans.
        //  6. Return the string-concatenation of head, middle, and tail.
        // NOTE: The string conversion semantics applied to the Expression value are like
        // String.prototype.concat rather than the + operator.

        //    STRING head                        head
        //    <expression>                       subRef/sub/err head
        //    GET_VALUE                          sub/err head
        //    JUMP_IF_ABRUPT unwind              sub head
        //    TO_STRING                          middle/err head
        //    JUMP_IF_ABRUPT unwind              middle head
        //    ADD                                leading
        //    <templatespans>                    tail/err leading
        //    JUMP_IF_ABRUPT unwind              tail leading
        //    ADD                                result
        //    JUMP exit
        // unwind:                               err other
        //    UNWIND 1                           err
        // exit:                                 result/err

        let mut unwinds = vec![];

        let tv = self.template_head.tv.as_ref().expect("Templates in expressions should not be raw-only");
        let head = chunk.add_to_string_pool(tv.clone())?;
        chunk.op_plus_arg(Insn::String, head);
        let status = self.expression.compile(chunk, strict, text)?;
        if status.maybe_ref() {
            chunk.op(Insn::GetValue);
        }
        if status.maybe_ref() || status.maybe_abrupt() {
            unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
        }
        chunk.op(Insn::ToString);
        unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
        chunk.op(Insn::Add);
        let status = self.template_spans.compile(chunk, strict, text)?;
        if status.maybe_abrupt() {
            unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
        }
        chunk.op(Insn::Add);
        let exit = chunk.op_jump(Insn::Jump);
        for mark in unwinds {
            chunk.fixup(mark)?;
        }
        chunk.op_plus_arg(Insn::Unwind, 1);
        chunk.fixup(exit).expect("jump too short to fail");

        Ok(AlwaysAbruptResult)
    }
}

impl TemplateSpans {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match self {
            TemplateSpans::Tail { data, .. } => {
                // TemplateSpans : TemplateTail
                //  1. Return the TV of TemplateTail.
                let idx = chunk.add_to_string_pool(
                    data.tv.as_ref().expect("templates as expressions should not be raw").clone(),
                )?;
                chunk.op_plus_arg(Insn::String, idx);
                Ok(AbruptResult::Never)
            }
            TemplateSpans::List { tml, data, .. } => {
                // TemplateSpans : TemplateMiddleList TemplateTail
                //  1. Let head be ? Evaluation of TemplateMiddleList.
                //  2. Let tail be the TV of TemplateTail as defined in 12.9.6.
                //  3. Return the string-concatenation of head and tail.

                //    <tml>                  head/err
                //    JUMP_IF_ABRUPT exit    head
                //    STRING tail            tail head
                //    ADD                    result
                // exit:                     result/err
                tml.compile(chunk, strict, text)?;
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                let tail = chunk.add_to_string_pool(
                    data.tv.as_ref().expect("templates as expressions should not be raw").clone(),
                )?;
                chunk.op_plus_arg(Insn::String, tail);
                chunk.op(Insn::Add);
                chunk.fixup(exit).expect("jump too short to fail");

                Ok(AbruptResult::Maybe)
            }
        }
    }
}

impl TemplateMiddleList {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AlwaysAbruptResult> {
        match self {
            TemplateMiddleList::ListHead { data, exp, .. } => {
                // TemplateMiddleList : TemplateMiddle Expression
                //  1. Let head be the TV of TemplateMiddle as defined in 12.9.6.
                //  2. Let subRef be ? Evaluation of Expression.
                //  3. Let sub be ? GetValue(subRef).
                //  4. Let middle be ? ToString(sub).
                //  5. Return the string-concatenation of head and middle.

                //   STRING head               head
                //   <expression>              subref/err head
                //   GET_VALUE                 sub/err head
                //   JUMP_IF_ABRUPT unwind     sub head
                //   TO_STRING                 middle/err head
                //   JUMP_IF_ABRUPT unwind     middle head
                //   ADD                       result
                //   JUMP exit
                // unwind:                     err head
                //   UNWIND 1                  err
                // exit:                       result/err

                let mut unwinds = vec![];
                let head = chunk.add_to_string_pool(
                    data.tv.as_ref().expect("templates used in expressions should not be raw").clone(),
                )?;
                chunk.op_plus_arg(Insn::String, head);
                let status = exp.compile(chunk, strict, text)?;
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                if status.maybe_abrupt() || status.maybe_ref() {
                    unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                chunk.op(Insn::ToString);
                unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
                chunk.op(Insn::Add);
                let exit = chunk.op_jump(Insn::Jump);
                for mark in unwinds {
                    chunk.fixup(mark).expect("jumps are too short to fail");
                }
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit).expect("Jump should be too short to fail");

                Ok(AlwaysAbruptResult)
            }
            TemplateMiddleList::ListMid(tml, mid, exp, _) => {
                // TemplateMiddleList : TemplateMiddleList TemplateMiddle Expression
                //  1. Let rest be ? Evaluation of TemplateMiddleList.
                //  2. Let middle be the TV of TemplateMiddle.
                //  3. Let subRef be ? Evaluation of Expression.
                //  4. Let sub be ? GetValue(subRef).
                //  5. Let last be ? ToString(sub).
                //  6. Return the string-concatenation of rest, middle, and last.

                //   <templatemiddlelist>     rest/err
                //   JUMP_IF_ABRUPT exit      rest
                //   STRING middle            middle rest
                //   ADD                      leader
                //   <expression>             subref/err leader
                //   GET_VALUE                sub/err leader
                //   JUMP_IF_ABRUPT unwind    sub leader
                //   TO_STRING                last/err leader
                //   JUMP_IF_ABRUPT unwind    last leader
                //   ADD                      result
                //   JUMP exit
                // unwind:                    err leader
                //   UNWIND 1                 err
                // exit:                      result/err

                let mut exits = vec![];
                let mut unwinds = vec![];

                tml.compile(chunk, strict, text)?;
                exits.push(chunk.op_jump(Insn::JumpIfAbrupt));
                let middle = chunk.add_to_string_pool(
                    mid.tv.as_ref().expect("template expressions should not be raw-only").clone(),
                )?;
                chunk.op_plus_arg(Insn::String, middle);
                chunk.op(Insn::Add);
                let status = exp.compile(chunk, strict, text)?;
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                if status.maybe_abrupt() || status.maybe_ref() {
                    unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                chunk.op(Insn::ToString);
                unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
                chunk.op(Insn::Add);
                exits.push(chunk.op_jump(Insn::Jump));
                for mark in unwinds {
                    chunk.fixup(mark).expect("Jumps too short to fail");
                }
                chunk.op_plus_arg(Insn::Unwind, 1);
                for mark in exits {
                    chunk.fixup(mark)?;
                }

                Ok(AlwaysAbruptResult)
            }
        }
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
                let status = if let (false, Some(np)) = (is_proto_setter, ae.anonymous_function_definition()) {
                    chunk.op(Insn::Dup);
                    np.compile_named_evaluation(chunk, strict, text, NameLoc::OnStack)?
                } else {
                    ae.compile(chunk, strict, text)?
                };
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
                Ok(NeverAbruptRefResult)
            }
            LiteralPropertyName::StringLiteral { data: st, .. } => {
                let idx = chunk.add_to_string_pool(st.value.clone())?;
                chunk.op_plus_arg(Insn::String, idx);
                Ok(NeverAbruptRefResult)
            }
            LiteralPropertyName::NumericLiteral { data: n, .. } => {
                let name = JSString::try_from(ECMAScriptValue::from(n)).expect("Numbers always have string forms.");
                let idx = chunk.add_to_string_pool(name)?;
                chunk.op_plus_arg(Insn::String, idx);
                Ok(NeverAbruptRefResult)
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
        Ok(AlwaysAbruptResult)
    }
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
    Ok(AlwaysAbruptRefResult)
}

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
    Ok(AlwaysRefResult)
}

impl MemberExpression {
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
                evaluate_property_access_with_identifier_key(chunk, id, strict)?;
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
                evaluate_property_access_with_expression_key(chunk, exp, strict, text)?;
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
            MemberExpression::NewArguments(me, args, ..) => {
                compile_new_evaluator(chunk, strict, text, ConstructExpr::Member(me.clone()), Some(args.clone()))
            }
            MemberExpression::PrivateId(..) => todo!(),
        }
    }
}

impl NewExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            NewExpression::MemberExpression(me) => me.compile(chunk, strict, text),
            NewExpression::NewExpression(ne, ..) => {
                compile_new_evaluator(chunk, strict, text, ConstructExpr::New(ne.clone()), None)
            }
        }
    }
}

#[derive(Debug)]
enum ConstructExpr {
    Member(Rc<MemberExpression>),
    New(Rc<NewExpression>),
}

impl ConstructExpr {
    fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            ConstructExpr::Member(member) => member.compile(chunk, strict, text),
            ConstructExpr::New(newexp) => newexp.compile(chunk, strict, text),
        }
    }
}

fn compile_new_evaluator(
    chunk: &mut Chunk,
    strict: bool,
    text: &str,
    expr: ConstructExpr,
    args: Option<Rc<Arguments>>,
) -> anyhow::Result<CompilerStatusFlags> {
    // EvaluateNew ( constructExpr, arguments )
    //
    // The abstract operation EvaluateNew takes arguments constructExpr (a NewExpression Parse Node or a
    // MemberExpression Parse Node) and arguments (empty or an Arguments Parse Node) and returns either a
    // normal completion containing an ECMAScript language value or an abrupt completion. It performs the
    // following steps when called:
    //
    //  1. Let ref be the result of evaluating constructExpr.
    //  2. Let constructor be ? GetValue(ref).
    //  3. If arguments is empty, let argList be a new empty List.
    //  4. Else,
    //      a. Let argList be ? ArgumentListEvaluation of arguments.
    //  5. If IsConstructor(constructor) is false, throw a TypeError exception.
    //  6. Return ? Construct(constructor, argList).
    let expr_status = expr.compile(chunk, strict, text)?;
    // Stack: ref/val/err ...
    if expr_status.maybe_ref() {
        chunk.op(Insn::GetValue);
    }
    // Stack: val/err ...
    let exit = if expr_status.maybe_abrupt() || expr_status.maybe_ref() {
        Some(chunk.op_jump(Insn::JumpIfAbrupt))
    } else {
        None
    };
    // Stack: val ...
    chunk.op(Insn::Dup);
    // Stack: val val ...
    chunk.op(Insn::Dup);
    // Stack: val val val ...
    let exit2 = match args {
        None => {
            let zero_idx = chunk.add_to_float_pool(0.0)?;
            chunk.op_plus_arg(Insn::Float, zero_idx);
            None
        }
        Some(arglist) => {
            let arg_status = arglist.argument_list_evaluation(chunk, strict, text)?;
            //     Stack: N arg(n-1) arg(n-2) ... arg1 arg0 val val val ...
            // or: Stack: err val val val ...
            if arg_status.maybe_abrupt() {
                let short = chunk.op_jump(Insn::JumpIfNormal);
                // Stack: err val val val ...
                chunk.op_plus_arg(Insn::Unwind, 3);
                // Stack: err ...
                let exit = chunk.op_jump(Insn::Jump);
                chunk.fixup(short).expect("Jump too short to overflow");
                Some(exit)
            } else {
                None
            }
        }
    };
    // Stack: N arg(n-1) arg(n-2) ... arg1 arg0 val val val ...
    chunk.op(Insn::SwapList);
    // Stack: val N arg(n-1) arg(n-2) ... arg1 arg0 val val ...
    chunk.op(Insn::RequireConstructor);
    // Stack: err/empty N arg(n-1) arg(n-2) ... arg1 arg0 val val ...
    let good_spot = chunk.op_jump(Insn::JumpIfNormal);
    // Stack: err N arg(n-1) arg(n-2) ... arg 1 arg 0 val val ...
    chunk.op(Insn::UnwindList);
    // Stack: err val val ...
    chunk.op_plus_arg(Insn::Unwind, 2);
    // Stack: err ...
    let exit3 = chunk.op_jump(Insn::Jump);
    chunk.fixup(good_spot).expect("Jump too short to overflow");
    // Stack: empty N arg(n-1) arg(n-2) ... arg1 arg0 val val ...
    chunk.op(Insn::Pop);
    // Stack: N arg(n-1) arg(n-2) ... arg1 arg0 val val ...
    chunk.op(Insn::Construct);
    // Stack: val/err ...

    if let Some(mark) = exit {
        chunk.fixup(mark)?;
    }
    if let Some(mark) = exit2 {
        chunk.fixup(mark).expect("Jump too short to overflow");
    }
    chunk.fixup(exit3).expect("Jump too short to overflow");
    Ok(CompilerStatusFlags::new().abrupt(true))
}

impl CallExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            CallExpression::CallMemberExpression(cme) => {
                cme.compile(chunk, strict, text).map(CompilerStatusFlags::from)
            }
            CallExpression::SuperCall(_) => todo!(),
            CallExpression::ImportCall(_) => todo!(),
            CallExpression::CallExpressionArguments(ce, args) => {
                // CallExpression : CallExpression Arguments
                //  1. Let ref be ? Evaluation of CallExpression.
                //  2. Let func be ? GetValue(ref).
                //  3. Let thisCall be this CallExpression.
                //  4. Let tailCall be IsInTailPosition(thisCall).
                //  5. Return ? EvaluateCall(func, ref, Arguments, tailCall).

                //    <callexpression>       ref/err
                //    DUP                    ref/err ref/err
                //    GET_VALUE              val/err ref/err
                //    JUMP_IF_ABRUPT unwind
                //    <compile_call>         val/err
                //    JUMP exit
                // unwind:
                //    UNWIND 1               err
                // exit:

                ce.compile(chunk, strict, text)?;
                chunk.op(Insn::Dup);
                chunk.op(Insn::GetValue);
                let unwind = chunk.op_jump(Insn::JumpIfAbrupt);
                compile_call(chunk, strict, text, args)?;
                let exit = chunk.op_jump(Insn::Jump);
                chunk.fixup(unwind)?;
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit).expect("jump too short to fail");
                Ok(AlwaysAbruptResult.into())
            }
            CallExpression::CallExpressionExpression(ce, exp, _) => {
                // CallExpression : CallExpression [ Expression ]
                //  1. Let baseReference be ? Evaluation of CallExpression.
                //  2. Let baseValue be ? GetValue(baseReference).
                //  3. If the source text matched by this CallExpression is strict mode code, let strict be
                //     true; else let strict be false.
                //  4. Return ? EvaluatePropertyAccessWithExpressionKey(baseValue, Expression, strict).

                //   <ce>                   ref/err
                //   GET_VALUE              val/err
                //   JUMP_IF_ABRUPT exit    val
                //   <evaluate_property_access_with_expression_key>
                // exit:

                let status = ce.compile(chunk, strict, text)?;
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                evaluate_property_access_with_expression_key(chunk, exp, strict, text)?;
                chunk.fixup(exit).expect("Jump too short to fail");

                Ok(AlwaysAbruptRefResult.into())
            }
            CallExpression::CallExpressionIdentifierName(ce, id, _) => {
                // CallExpression : CallExpression . IdentifierName
                //  1. Let baseReference be ? Evaluation of CallExpression.
                //  2. Let baseValue be ? GetValue(baseReference).
                //  3. If the source text matched by this CallExpression is strict mode code, let strict be
                //     true; else let strict be false.
                //  4. Return EvaluatePropertyAccessWithIdentifierKey(baseValue, IdentifierName, strict).

                //   <ce>                 ref/err
                //   GET_VALUE            val/err
                //   JUMP_IF_ABRUPT exit
                //   <evaluate_property_access_with_identifier_key>
                // exit:

                let status = ce.compile(chunk, strict, text)?;
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                evaluate_property_access_with_identifier_key(chunk, id, strict)?;
                chunk.fixup(exit).expect("Jump too short to fail");

                Ok(AlwaysAbruptRefResult.into())
            }
            CallExpression::CallExpressionTemplateLiteral(_, _) => todo!(),
            CallExpression::CallExpressionPrivateId(_, _, _) => todo!(),
        }
    }
}

pub fn compile_call(
    chunk: &mut Chunk,
    strict: bool,
    text: &str,
    arguments: &Rc<Arguments>,
) -> anyhow::Result<AlwaysAbruptResult> {
    // EvaluateCall ( func, ref, arguments, tailPosition )
    // The abstract operation EvaluateCall takes arguments func (an ECMAScript language value), ref (an
    // ECMAScript language value or a Reference Record), arguments (a Parse Node), and tailPosition (a
    // Boolean) and returns either a normal completion containing an ECMAScript language value or an abrupt
    // completion. It performs the following steps when called:
    //
    //  1. If ref is a Reference Record, then
    //      a. If IsPropertyReference(ref) is true, then
    //          i. Let thisValue be GetThisValue(ref).
    //      b. Else,
    //          i. Let refEnv be ref.[[Base]].
    //          ii. Assert: refEnv is an Environment Record.
    //          iii. Let thisValue be refEnv.WithBaseObject().
    //  2. Else,
    //      a. Let thisValue be undefined.
    //  3. Let argList be ? ArgumentListEvaluation of arguments.
    //  4. If func is not an Object, throw a TypeError exception.
    //  5. If IsCallable(func) is false, throw a TypeError exception.
    //  6. If tailPosition is true, perform PrepareForTailCall().
    //  7. Return ? Call(func, thisValue, argList).

    // On the top of the stack are "func" and "ref". We're ignoring tail calls for now. All the "this value"
    // calculation, and the error checking are handled in the execution of the "CALL" instruction.

    // start:                       func ref
    //    <arguments>               (N arg(n-1) arg(n-2) ... arg0 func ref) or (err func ref)
    //    JUMP_IF_NORMAL call
    //    UNWIND 2                  err
    //    JUMP exit
    // call:                        N arg(n-1) ... arg0 func ref
    //    CALL                      val/err
    // exit:                        val/err

    let status = arguments.argument_list_evaluation(chunk, strict, text)?;
    let mut exit = None;
    if status.maybe_abrupt() {
        let call = chunk.op_jump(Insn::JumpIfNormal);
        chunk.op_plus_arg(Insn::Unwind, 2);
        exit = Some(chunk.op_jump(Insn::Jump));
        chunk.fixup(call).expect("jump too short to fail");
    }
    chunk.op(Insn::Call);
    if let Some(mark) = exit {
        chunk.fixup(mark).expect("jump too short to fail");
    }
    Ok(AlwaysAbruptResult)
}

impl CallMemberExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AlwaysAbruptResult> {
        // On return: top of stack might be an abrupt completion, but will never be a reference.
        let mut exit = None;
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
            exit = Some(chunk.op_jump(Insn::Jump));
            chunk.fixup(happy).expect("Jump is too short to overflow.");
        }
        // Stack: func ref ...
        compile_call(chunk, strict, text, &self.arguments)?;
        if let Some(mark) = exit {
            chunk.fixup(mark)?;
        }
        Ok(AlwaysAbruptResult)
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ArgListSizeHint {
    fixed_len: u16,
    has_variable: bool, // If true, the list is appended with another stack list (length included)
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
                // Arguments : ( )
                //  1. Return a new empty List.

                //   FLOAT 0

                let index = chunk.add_to_float_pool(0.0)?;
                chunk.op_plus_arg(Insn::Float, index);
                Ok(AbruptResult::Never)
            }
            Arguments::ArgumentList(al, _) | Arguments::ArgumentListComma(al, _) => {
                // Arguments : ( ArgumentList )
                // Arguments : ( ArgumentList , )
                //  1. Return ? ArgumentListEvaluation of ArgumentList

                //    <argument_list>        (err) or (arg(n+m-1) .. arg(m) M arg(m-1) .. arg0) (prev <- N, M existence)
                //  ---- if N > 0 || M not present
                //    JUMP_IF_ABRUPT exit
                //    ---- if M present
                //    ROTATE_UP N
                //    ----
                //    FLOAT N
                //    ---- if M present
                //    ADD
                //    ----
                //  ----

                let (ArgListSizeHint { fixed_len, has_variable }, status) =
                    al.argument_list_evaluation(chunk, strict, text)?;
                if !has_variable {
                    // size known at compile time:
                    //   <argument_list>       (err) or (arg(n-1) arg(n-2) ... arg0)
                    //   JUMP_IF_ABRUPT exit   arg(n-1) arg(n-2) ... arg0
                    //   FLOAT n               n arg(n-1) arg(n-2) ... arg0
                    // exit:
                    let exit = if status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                    let index = chunk.add_to_float_pool(fixed_len as f64)?;
                    chunk.op_plus_arg(Insn::Float, index);
                    if let Some(mark) = exit {
                        chunk.fixup(mark).expect("Jump is too short to overflow.");
                    }
                } else {
                    // size variable at compile time
                    //   <argument_list>       (err) or (arg(m+n-1) ... arg(m+0) M arg(m-1) ... arg0)
                    //   --- if N > 0 ---
                    //   JUMP_IF_ABRUPT exit   arg(m+n-1) ... arg(m+0) M arg(m-1) ... arg0
                    //   ROTATE_UP N+1         M arg(m+n-1) ... arg(m) arg(m-1) ... arg0
                    //   FLOAT N               N M arg(m+n-1) ... arg(m) arg(m-1) ... arg0
                    //   ADD                   len arg(len-1) ... arg0
                    //   ----
                    // exit:

                    if fixed_len > 0 {
                        // A variable sized list always winds up being abrupt at this point.
                        assert!(status.maybe_abrupt());
                        let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                        chunk.op_plus_arg(Insn::RotateUp, fixed_len + 1);
                        let idx = chunk.add_to_float_pool(fixed_len as f64)?;
                        chunk.op_plus_arg(Insn::Float, idx);
                        chunk.op(Insn::Add);
                        chunk.fixup(exit).expect("Jump too short to fail");
                    }
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
    ) -> anyhow::Result<(ArgListSizeHint, AbruptResult)> {
        match self {
            ArgumentList::FallThru(ae) => {
                // ArgumentList : AssignmentExpression
                //  1. Let ref be ? Evaluation of AssignmentExpression.
                //  2. Let arg be ? GetValue(ref).
                //  3. Return « arg ».

                //    <ae>             ref/err
                //    GET_VALUE        val/err
                // exit:
                // (returning length = known:1)

                let status = ae.compile(chunk, strict, text)?;
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                // Stack val/err ...
                Ok((ArgListSizeHint { fixed_len: 1, has_variable: false }, status.into()))
            }
            ArgumentList::Dots(ae) => {
                // ArgumentList : ... AssignmentExpression
                //  1. Let list be a new empty List.
                //  2. Let spreadRef be ? Evaluation of AssignmentExpression.
                //  3. Let spreadObj be ? GetValue(spreadRef).
                //  4. Let iteratorRecord be ? GetIterator(spreadObj, sync).
                //  5. Repeat,
                //      a. Let next be ? IteratorStep(iteratorRecord).
                //      b. If next is false, return list.
                //      c. Let nextArg be ? IteratorValue(next).
                //      d. Append nextArg to list.

                //   <ae>                   spreadref/err
                //   GET_VALUE              spreadobj/err
                //   JUMP_IF_ABRUPT exit    spreadobj
                //   ITER_ARGS              (err) or (N arg(n-1) arg(n-2) ... arg(0))
                // exit:
                // (returning length = mystery:0)

                let status = ae.compile(chunk, strict, text)?;
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                let exit = if status.maybe_abrupt() || status.maybe_ref() {
                    Some(chunk.op_jump(Insn::JumpIfAbrupt))
                } else {
                    None
                };
                chunk.op(Insn::IterateArguments);
                if let Some(mark) = exit {
                    chunk.fixup(mark).expect("jump too short to fail");
                }
                Ok((ArgListSizeHint { fixed_len: 0, has_variable: true }, AlwaysAbruptResult.into()))
            }
            ArgumentList::ArgumentList(lst, item) => {
                // ArgumentList : ArgumentList , AssignmentExpression
                //  1. Let precedingArgs be ? ArgumentListEvaluation of ArgumentList.
                //  2. Let ref be ? Evaluation of AssignmentExpression.
                //  3. Let arg be ? GetValue(ref).
                //  4. Return the list-concatenation of precedingArgs and « arg ».

                //    <list>                 err/list  (length received as kind(amt))
                //    JUMP_IF_ABRUPT exit    list
                //    <ae>                   ref/err list
                //    GET_VALUE              val/err list
                //    JUMP_IF_NORMAL exit    err list  (returning length is kind(amt+1))
                //    --- if amt > 0 ---
                //    UNWIND amt
                //    ---
                //    --- if kind == mystery
                //    UNWIND_LIST
                // exit:

                // Stack: ...
                let (prev_count, status) = lst.argument_list_evaluation(chunk, strict, text)?;
                // Stack: val(N) val(N-1) ... val(0) ...
                // or err ...
                let exit = if status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                let status2 = item.compile(chunk, strict, text)?;
                // Stack: val/err val(n) val(n-1) ... val(0) ...
                if status2.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                if status2.maybe_ref() || status2.maybe_abrupt() {
                    let happy = chunk.op_jump(Insn::JumpIfNormal);
                    let amt = prev_count.fixed_len;
                    if amt > 0 {
                        chunk.op_plus_arg(Insn::Unwind, amt);
                    }
                    if prev_count.has_variable {
                        chunk.op(Insn::UnwindList);
                    }
                    chunk.fixup(happy).expect("Jump is too short to overflow.");
                }
                if let Some(mark) = exit {
                    chunk.fixup(mark)?;
                }
                Ok((
                    ArgListSizeHint { fixed_len: prev_count.fixed_len + 1, has_variable: prev_count.has_variable },
                    (status == AbruptResult::Maybe || status2.maybe_abrupt() || status2.maybe_ref()).into(),
                ))
            }
            ArgumentList::ArgumentListDots(list, ae) => {
                // ArgumentList : ArgumentList , ... AssignmentExpression
                //  1. Let precedingArgs be ? ArgumentListEvaluation of ArgumentList.
                //  2. Let spreadRef be ? Evaluation of AssignmentExpression.
                //  3. Let iteratorRecord be ? GetIterator(? GetValue(spreadRef), sync).
                //  4. Repeat,
                //      a. Let next be ? IteratorStep(iteratorRecord).
                //      b. If next is false, return precedingArgs.
                //      c. Let nextArg be ? IteratorValue(next).
                //      d. Append nextArg to precedingArgs.

                //    <list>                 err/preceding_args  (length received as fixed/has_var)
                //    JUMP_IF_ABRUPT exit    preceding_args
                //  --- if has_var
                //    --- if fixed > 0
                //    ROTATE_UP fixed+1
                //    FLOAT fixed
                //    ADD
                //    ---
                //  --- else
                //    FLOAT fixed
                //  ---
                //    <ae>                   spreadRef/err preceding_args
                //    GET_VALUE              spreadVal/err preceding_args
                //    JUMP_IF_ABRUPT unwind  spreadVal preceding_args
                //    ITER_ARGS              new_args/err preceding_args
                //    JUMP_IF_ABRUPT unwind  new_args preceding_args
                //    APPEND_LIST            args (fixed=0/has_var=true)
                //    JUMP exit
                // unwind:
                //    UNWIND_LIST
                // exit:

                let mut unwinds = vec![];

                let (ArgListSizeHint { fixed_len, has_variable }, status) =
                    list.argument_list_evaluation(chunk, strict, text)?;
                let exit1 = if status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                if has_variable {
                    if fixed_len > 0 {
                        chunk.op_plus_arg(Insn::RotateUp, fixed_len + 1);
                        let idx = chunk.add_to_float_pool(fixed_len as f64)?;
                        chunk.op_plus_arg(Insn::Float, idx);
                        chunk.op(Insn::Add);
                    }
                } else {
                    let idx = chunk.add_to_float_pool(fixed_len as f64)?;
                    chunk.op_plus_arg(Insn::Float, idx);
                }

                let status = ae.compile(chunk, strict, text)?;
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                if status.maybe_abrupt() || status.maybe_ref() {
                    unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                chunk.op(Insn::IterateArguments);
                unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
                chunk.op(Insn::AppendList);
                let exit2 = chunk.op_jump(Insn::Jump);

                for mark in unwinds {
                    chunk.fixup(mark).expect("jumps too short to fail");
                }
                chunk.op(Insn::UnwindList);
                if let Some(mark) = exit1 {
                    chunk.fixup(mark)?;
                }
                chunk.fixup(exit2).expect("jump too short to fail");

                Ok((ArgListSizeHint { fixed_len: 0, has_variable: true }, AbruptResult::Maybe))
            }
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

        Ok(AlwaysAbruptResult)
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
        Ok(AlwaysAbruptResult)
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
            Ok(AlwaysAbruptResult)
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
        Ok(AlwaysAbruptResult)
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

                let status =
                    if let (Some(np), Some(lhse_id)) = (ae.anonymous_function_definition(), lhse.identifier_ref()) {
                        let idx = chunk
                            .add_to_string_pool(lhse_id.string_value())
                            .expect("This string already added during lhse compile");
                        np.compile_named_evaluation(chunk, strict, text, NameLoc::Index(idx))?
                    } else {
                        ae.compile(chunk, strict, text)?
                    };
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
                Ok(AlwaysAbruptResult.into())
            }
            AssignmentExpression::Yield(_) => todo!(),
            AssignmentExpression::Arrow(arrow_function) => {
                arrow_function.compile(chunk, strict, text, arrow_function.clone()).map(CompilerStatusFlags::from)
            }
            AssignmentExpression::AsyncArrow(_) => todo!(),
            AssignmentExpression::OpAssignment(lhse, op, rhs) => {
                // Stack: ...
                let lhs_status = lhse.compile(chunk, strict, text)?;
                let lhs_exit = if lhs_status.maybe_abrupt() {
                    let mark = chunk.op_jump(Insn::JumpIfAbrupt);
                    Some(mark)
                } else {
                    None
                };
                // Stack: lref ...
                chunk.op(Insn::Dup);
                // Stack: lref lref ...
                chunk.op(Insn::GetValue);
                // Stack: lval/err lref ...
                let short = chunk.op_jump(Insn::JumpIfNormal);
                // Stack: err lref ...
                chunk.op_plus_arg(Insn::Unwind, 1);
                // Stack: err ...
                let lval_exit = chunk.op_jump(Insn::Jump);
                chunk.fixup(short).expect("Short jump won't fail");
                // Stack: lval lref ...
                let rhs_status = rhs.compile(chunk, strict, text)?;
                // Stack: rval/rref/err lval lref ...
                if rhs_status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                // Stack: rval/err lval lref ...
                let rhs_exit = if rhs_status.maybe_abrupt() || rhs_status.maybe_ref() {
                    let short = chunk.op_jump(Insn::JumpIfNormal);
                    // Stack: err lval lref ...
                    chunk.op_plus_arg(Insn::Unwind, 2);
                    // Stack: err ...
                    let exit = chunk.op_jump(Insn::Jump);
                    chunk.fixup(short).expect("Short jump won't overflow");
                    Some(exit)
                } else {
                    None
                };
                // Stack: rval lval lref ...
                chunk.op(match op {
                    AssignmentOperator::Multiply => Insn::Multiply,
                    AssignmentOperator::Divide => Insn::Divide,
                    AssignmentOperator::Modulo => Insn::Modulo,
                    AssignmentOperator::Add => Insn::Add,
                    AssignmentOperator::Subtract => Insn::Subtract,
                    AssignmentOperator::LeftShift => Insn::LeftShift,
                    AssignmentOperator::SignedRightShift => Insn::SignedRightShift,
                    AssignmentOperator::UnsignedRightShift => Insn::UnsignedRightShift,
                    AssignmentOperator::BitwiseAnd => Insn::BitwiseAnd,
                    AssignmentOperator::BitwiseXor => Insn::BitwiseXor,
                    AssignmentOperator::BitwiseOr => Insn::BitwiseOr,
                    AssignmentOperator::Exponentiate => Insn::Exponentiate,
                });
                // Stack: r/err lref ...
                let short = chunk.op_jump(Insn::JumpIfNormal);
                // Stack: err lref ...
                chunk.op_plus_arg(Insn::Unwind, 1);
                let op_exit = chunk.op_jump(Insn::Jump);
                chunk.fixup(short).expect("Short jumps won't fail");
                // Stack: r lref ...
                chunk.op(Insn::Pop2Push3);
                // Stack: r lref r ...
                chunk.op(Insn::PutValue);
                // Stack: empty/err r ...
                let short = chunk.op_jump(Insn::JumpIfNormal);
                // Stack: err r ...
                chunk.op_plus_arg(Insn::Unwind, 1);
                let set_exit = chunk.op_jump(Insn::Jump);
                chunk.fixup(short).expect("Short jumps won't fail");
                // Stack: empty r ...
                chunk.op(Insn::Pop);
                // Stack: r ...
                chunk.fixup(set_exit).expect("Short jumps won't fail");
                // Stack: err/r ...
                chunk.fixup(op_exit).expect("Short jumps won't fail");
                if let Some(mark) = rhs_exit {
                    chunk.fixup(mark).expect("Short jumps won't fail");
                }
                if let Some(mark) = lhs_exit {
                    chunk.fixup(mark)?;
                }
                chunk.fixup(lval_exit)?;
                Ok(AlwaysAbruptResult.into())
            }
            AssignmentExpression::LandAssignment(_, _) => todo!(),
            AssignmentExpression::LorAssignment(_, _) => todo!(),
            AssignmentExpression::CoalAssignment(_, _) => todo!(),
            AssignmentExpression::Destructuring(_, _) => todo!(),
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
        let mut status = self.list[0].compile(chunk, strict, text)?;
        let mut exits = vec![];
        for item in self.list[1..].iter() {
            if status.maybe_abrupt() {
                exits.push(chunk.op_jump(Insn::JumpIfAbrupt));
            }
            status = item.compile(chunk, strict, text)?;
            chunk.op(Insn::UpdateEmpty);
        }
        let might_have_been_abrupt = status.maybe_abrupt() || !exits.is_empty();
        for mark in exits {
            chunk.fixup(mark)?;
        }
        Ok(AbruptResult::from(might_have_been_abrupt))
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
            Statement::Return(r) => r.compile(chunk, strict, text).map(AbruptResult::from),
            Statement::With(_) => todo!(),
            Statement::Labelled(lbl) => lbl.compile(chunk, strict, text),
            Statement::Throw(throw_statement) => throw_statement.compile(chunk, strict, text).map(AbruptResult::from),
            Statement::Try(try_statement) => try_statement.compile(chunk, strict, text),
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
            Declaration::Hoistable(_) => {
                chunk.op(Insn::Empty);
                Ok(AbruptResult::Never)
            }
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
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AbruptResult> {
        match self {
            FcnDef::Function(f) => {
                return f.compile_fo_instantiation(chunk, strict, text, f.clone()).map(AbruptResult::from);
            }
            FcnDef::Generator(_) | FcnDef::AsyncFun(_) | FcnDef::AsyncGen(_) => chunk.op(Insn::ToDo),
        }
        Ok(NeverAbruptRefResult.into())
    }
}

impl Block {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match &self.statements {
            None => {
                chunk.op(Insn::Empty);
                Ok(NeverAbruptRefResult.into())
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
                    let x: Result<FcnDef, anyhow::Error> = FcnDef::try_from(d);
                    if let Ok(fcn) = x {
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
        Ok(AlwaysAbruptResult)
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
                Ok(AlwaysAbruptResult)
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
                        let status = if let Some(np) = izer.anonymous_function_definition() {
                            np.compile_named_evaluation(chunk, strict, text, NameLoc::Index(id))?
                        } else {
                            izer.compile(chunk, strict, text)?
                        };
                        // Stack: rref lhs ...
                        if status.maybe_ref() {
                            chunk.op(Insn::GetValue);
                            // Stack: value lhs ...
                        }
                        if status.maybe_abrupt() || status.maybe_ref() {
                            let normal = chunk.op_jump(Insn::JumpIfNormal);
                            // Stack: err lhs ...
                            chunk.op_plus_arg(Insn::Unwind, 1);
                            // Stack: err ...
                            let exit_tgt = Some(chunk.op_jump(Insn::Jump));
                            chunk.fixup(normal).expect("Jump is too short to overflow.");
                            exit_tgt
                        } else {
                            None
                        }
                    }
                };
                // Stack: value lhs ...
                chunk.op(Insn::InitializeReferencedBinding);
                // Stack: empty ...
                if let Some(mark) = exit_tgt {
                    chunk.fixup(mark).expect("Jump is too short to overflow.");
                }
                Ok(AlwaysAbruptResult)
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
        // Stack: ...
        let mut status = self.list[0].compile(chunk, strict, text)?;
        // Stack: empty/err ...
        let mut exits = vec![];
        for item in self.list[1..].iter() {
            // Stack: empty/err ...
            if status.maybe_abrupt() {
                exits.push(chunk.op_jump(Insn::JumpIfAbrupt));
            }
            // Stack: empty ...
            chunk.op(Insn::Pop);
            // Stack: ...
            status = item.compile(chunk, strict, text)?;
            // Stack: empty/err ...
        }
        let might_have_been_abrupt = status.maybe_abrupt() || !exits.is_empty();
        // Stack: emtpy/err ...
        for mark in exits {
            chunk.fixup(mark)?;
        }
        Ok(AbruptResult::from(might_have_been_abrupt))
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
                let izer_flags = if let Some(np) = izer.anonymous_function_definition() {
                    np.compile_named_evaluation(chunk, strict, text, NameLoc::Index(idx))?
                } else {
                    izer.compile(chunk, strict, text)? // Stack: rhs/rref/err lhs ...
                };
                if izer_flags.maybe_ref() {
                    chunk.op(Insn::GetValue); // Stack: rhs/err lhs ...
                }
                if izer_flags.maybe_abrupt() || izer_flags.maybe_ref() {
                    let ok_tgt = chunk.op_jump(Insn::JumpIfNormal); // Stack: err lhs ...
                    chunk.op_plus_arg(Insn::Unwind, 1); // Stack: err ...
                    exits.push(chunk.op_jump(Insn::Jump));
                    chunk.fixup(ok_tgt).expect("Jump too short to overflow.");
                }
                // Stack: rhs lhs ...
                chunk.op(Insn::PutValue); // Stack: err/empty ...
                for exit in exits {
                    chunk.fixup(exit)?;
                }
                Ok(AbruptResult::Maybe)
            }
            VariableDeclaration::Pattern(bp, init) => {
                // VariableDeclaration : BindingPattern Initializer
                //  1. Let rhs be ? Evaluation of Initializer.
                //  2. Let rval be ? GetValue(rhs).
                //  3. Return ? BindingInitialization of BindingPattern with arguments rval and undefined.

                //   <initializer>                 rhs/err
                //   GET_VALUE                     rval/err
                //   JUMP_IF_ABRUPT exit           rval
                //   <bp.binding_initialization>   result/err
                // exit:

                let status = init.compile(chunk, strict, text)?;
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                let exit = if status.maybe_abrupt() || status.maybe_ref() {
                    Some(chunk.op_jump(Insn::JumpIfAbrupt))
                } else {
                    None
                };

                let bp_status = bp.compile_binding_initialization(chunk, strict, text, EnvUsage::UsePutValue)?;
                if let Some(mark) = exit {
                    chunk.fixup(mark)?;
                }

                Ok((status.maybe_abrupt() || status.maybe_ref() || bp_status.maybe_abrupt()).into())
            }
        }
    }
}

impl EmptyStatement {
    fn compile(&self, chunk: &mut Chunk) -> NeverAbruptRefResult {
        chunk.op(Insn::Empty);
        NeverAbruptRefResult
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
                NeverAbruptRefResult.into()
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
            IterationStatement::While(ws) => ws.while_loop_compile(chunk, strict, text, label_set),
            IterationStatement::For(f) => f.compile_for_loop(chunk, strict, text, label_set),
            IterationStatement::ForInOf(_) => todo!(),
        }
    }
}

impl WhileStatement {
    fn while_loop_compile(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        label_set: &[JSString],
    ) -> anyhow::Result<AbruptResult> {
        //  1. Let V be undefined.
        //  2. Repeat,
        //      a. Let exprRef be ? Evaluation of Expression.
        //      b. Let exprValue be ? GetValue(exprRef).
        //      c. If ToBoolean(exprValue) is false, return V.
        //      d. Let stmtResult be Completion(Evaluation of Statement).
        //      e. If LoopContinues(stmtResult, labelSet) is false, return ? UpdateEmpty(stmtResult, V).
        //      f. If stmtResult.[[Value]] is not empty, set V to stmtResult.[[Value]].

        // start:
        // UNDEFINED                v
        // top:
        // <expression>             ref/val/err v
        // GET_VALUE                val/err v
        // JUMP_IF_NORMAL fwd
        // UNWIND 1                 err
        // JUMP exit
        // fwd:                     val v
        // JUMPPOP_IF_FALSE exit    v
        // <stmt>                   val/err v
        // LOOP_CONTINUES lsid      should_continue val/err v
        // JUMPPOP_IF_FALSE leaving val/err v
        // COALESCE                 v
        // JUMP top
        // leaving:                 val/err v
        // UPDATE_EMPTY             val/err
        // exit:

        chunk.op(Insn::Undefined);
        let loop_top = chunk.pos();
        let exp_status = self.exp.compile(chunk, strict, text)?;
        if exp_status.maybe_ref() {
            chunk.op(Insn::GetValue);
        }
        let mut exits = vec![];
        if exp_status.maybe_abrupt() || exp_status.maybe_ref() {
            let mark = chunk.op_jump(Insn::JumpIfNormal);
            chunk.op_plus_arg(Insn::Unwind, 1);
            exits.push(chunk.op_jump(Insn::Jump));
            chunk.fixup(mark).expect("jump too short to fail");
        }
        exits.push(chunk.op_jump(Insn::JumpPopIfFalse));
        let stmt_status = self.stmt.compile(chunk, strict, text)?;
        let leaving = if stmt_status.maybe_abrupt() {
            let label_set_id = chunk.add_to_string_set_pool(label_set)?;
            chunk.op_plus_arg(Insn::LoopContinues, label_set_id);
            Some(chunk.op_jump(Insn::JumpPopIfFalse))
        } else {
            None
        };
        chunk.op(Insn::CoalesceValue);
        chunk.op_jump_back(Insn::Jump, loop_top)?;
        if let Some(mark) = leaving {
            chunk.fixup(mark).expect("jump too short to fail");
        }
        chunk.op(Insn::UpdateEmpty);
        for mark in exits {
            chunk.fixup(mark).expect("these should always be smaller than the loop back, which already was successful");
        }
        Ok((exp_status.maybe_abrupt() || exp_status.maybe_ref() || stmt_status.maybe_abrupt()).into())
    }
}

impl DoWhileStatement {
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
            let label_set_id = chunk.add_to_string_set_pool(label_set)?;
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

impl ForStatement {
    #[allow(clippy::too_many_arguments)]
    fn compile_for_body(
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        test: Option<Rc<Expression>>,
        increment: Option<Rc<Expression>>,
        stmt: Rc<Statement>,
        per_iteration_bindings: &[JSString],
        label_set: &[JSString],
    ) -> anyhow::Result<AbruptResult> {
        // ForBodyEvaluation ( test, increment, stmt, perIterationBindings, labelSet )
        // The abstract operation ForBodyEvaluation takes arguments test (an Expression Parse Node or empty),
        // increment (an Expression Parse Node or empty), stmt (a Statement Parse Node), perIterationBindings
        // (a List of Strings), and labelSet (a List of Strings) and returns either a normal completion
        // containing an ECMAScript language value or an abrupt completion. It performs the following steps
        // when called:
        //
        //  1. Let V be undefined.
        //  2. Perform ? CreatePerIterationEnvironment(perIterationBindings).
        //  3. Repeat,
        //      a. If test is not empty, then
        //          i. Let testRef be ? Evaluation of test.
        //          ii. Let testValue be ? GetValue(testRef).
        //          iii. If ToBoolean(testValue) is false, return V.
        //      b. Let result be Completion(Evaluation of stmt).
        //      c. If LoopContinues(result, labelSet) is false, return ? UpdateEmpty(result, V).
        //      d. If result.[[Value]] is not empty, set V to result.[[Value]].
        //      e. Perform ? CreatePerIterationEnvironment(perIterationBindings).
        //      f. If increment is not empty, then
        //          i. Let incRef be ? Evaluation of increment.
        //          ii. Perform ? GetValue(incRef).

        //    UNDEFINED                        v
        //  --- if pib not empty ---
        //    CPIE pib                         [empty]/err v
        //    JUMP_IF_ABRUPT unwind
        //    POP                              v
        //  -- end pib not empty
        // loop_top:                           v
        //  --- if test is present ---
        //    <test>                           ref/val/err v
        //    GET_VALUE                        val/err v
        //    JUMP_IF_ABRUPT unwind
        //    JUMPPOP_IF_FALSE exit            v
        //  --- end test is present ---
        //    <stmt>                           result v
        //    LOOP_CONTINUES label_set         true/false result v
        //    JUMPPOP_IF_FALSE update_exit     result v
        //    COALESCE                         v
        //  --- if pib not empty ---
        //    CPIE pib                         [empty]/err v
        //    JUMP_IF_ABRUPT unwind
        //    POP                              v
        //  --- end pib not empty
        //  --- if increment is present ---
        //    <increment>                      rev/val/err v
        //    GET_VALUE                        val/err v
        //    JUMP_IF_ABRUPT unwind
        //    POP                              v
        //  --- end increment is present ---
        //    JUMP loop_top
        //  --- if had any jumps to "unwind"
        // unwind:                             err v
        //    UNWIND 1                         err
        //    JUMP exit
        //  --- end had any "unwind" jumps
        // update_exit:                        result v
        //    UPDATE_EMPTY                     result
        // exit:                               result/err

        let pib_id = if per_iteration_bindings.is_empty() {
            None
        } else {
            Some(chunk.add_to_string_set_pool(per_iteration_bindings)?)
        };
        let mut unwinds = vec![];
        let mut exits = vec![];
        let mut maybe_abrupt = AbruptResult::from(pib_id.is_some());

        chunk.op(Insn::Undefined);
        if let Some(pib_id) = pib_id {
            chunk.op_plus_arg(Insn::CreatePerIterationEnvironment, pib_id);
            unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
            chunk.op(Insn::Pop);
        }
        let loop_top = chunk.pos();
        if let Some(test) = test {
            let status = test.compile(chunk, strict, text)?;
            if status.maybe_ref() {
                chunk.op(Insn::GetValue);
            }
            if status.maybe_ref() || status.maybe_abrupt() {
                unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
                maybe_abrupt = AbruptResult::Maybe;
            }
            exits.push(chunk.op_jump(Insn::JumpPopIfFalse));
        }
        let mut update_exit = None;
        let status = stmt.compile(chunk, strict, text)?;
        if status.maybe_abrupt() {
            maybe_abrupt = AbruptResult::Maybe;
            let label_set_id = chunk.add_to_string_set_pool(label_set)?;
            chunk.op_plus_arg(Insn::LoopContinues, label_set_id);
            update_exit = Some(chunk.op_jump(Insn::JumpPopIfFalse));
        }
        chunk.op(Insn::CoalesceValue);
        if let Some(pib_id) = pib_id {
            chunk.op_plus_arg(Insn::CreatePerIterationEnvironment, pib_id);
            unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
            chunk.op(Insn::Pop);
        }
        if let Some(increment) = increment {
            let status = increment.compile(chunk, strict, text)?;
            if status.maybe_ref() {
                chunk.op(Insn::GetValue);
            }
            if status.maybe_ref() || status.maybe_abrupt() {
                unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
                maybe_abrupt = AbruptResult::Maybe;
            }
            chunk.op(Insn::Pop);
        }
        chunk.op_jump_back(Insn::Jump, loop_top)?;
        if !unwinds.is_empty() {
            for exit in unwinds {
                chunk.fixup(exit)?;
            }
            chunk.op_plus_arg(Insn::Unwind, 1);
            if update_exit.is_some() {
                exits.push(chunk.op_jump(Insn::Jump));
            }
        }
        if let Some(exit) = update_exit {
            chunk.fixup(exit).expect("This jump should always be shorter than the jump back");
            chunk.op(Insn::UpdateEmpty);
        }
        for exit in exits {
            chunk.fixup(exit).expect("This jump should be shorter than the loop back or the unwind jump");
        }

        Ok(maybe_abrupt)
    }

    fn compile_for_loop(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        label_set: &[JSString],
    ) -> anyhow::Result<AbruptResult> {
        match self {
            ForStatement::For(init, test, incr, stmt, _) => {
                // ForStatement : for ( Expressionopt ; Expressionopt ; Expressionopt ) Statement
                //  1. If the first Expression is present, then
                //      a. Let exprRef be ? Evaluation of the first Expression.
                //      b. Perform ? GetValue(exprRef).
                //  2. If the second Expression is present, let test be the second Expression; otherwise, let
                //     test be empty.
                //  3. If the third Expression is present, let increment be the third Expression; otherwise,
                //     let increment be empty.
                //  4. Return ? ForBodyEvaluation(test, increment, Statement, « », labelSet).

                // --- if init is present ---
                //   <init>            val/ref/err
                //   GET_VALUE         val/err
                //   JUMP_IF_ABRUPT exit
                //   POP
                // --- end init is present
                //   <for body evaluation>
                // exit:
                let mut maybe_abrupt = AbruptResult::Never;
                let mut exit = None;

                if let Some(init) = init {
                    let status = init.compile(chunk, strict, text)?;
                    if status.maybe_ref() {
                        chunk.op(Insn::GetValue);
                    }
                    if status.maybe_ref() || status.maybe_abrupt() {
                        exit = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                        maybe_abrupt = AbruptResult::Maybe;
                    }
                    chunk.op(Insn::Pop);
                }
                let body_status = Self::compile_for_body(
                    chunk,
                    strict,
                    text,
                    test.clone(),
                    incr.clone(),
                    stmt.clone(),
                    &[],
                    label_set,
                )?;
                if body_status.maybe_abrupt() {
                    maybe_abrupt = AbruptResult::Maybe;
                }
                if let Some(exit) = exit {
                    chunk.fixup(exit)?;
                }

                Ok(maybe_abrupt)
            }
            ForStatement::ForVar(vdl, test, incr, stmt, _) => {
                // ForStatement : for ( var VariableDeclarationList ; Expressionopt ; Expressionopt ) Statement
                //  1. Perform ? Evaluation of VariableDeclarationList.
                //  2. If the first Expression is present, let test be the first Expression; otherwise, let
                //     test be empty.
                //  3. If the second Expression is present, let increment be the second Expression; otherwise,
                //     let increment be empty.
                //  4. Return ? ForBodyEvaluation(test, increment, Statement, « », labelSet).
                let mut maybe_abrupt = AbruptResult::Never;
                let mut exit = None;
                let vdl_status = vdl.compile(chunk, strict, text)?;
                if vdl_status.maybe_abrupt() {
                    exit = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                    maybe_abrupt = AbruptResult::Maybe;
                }
                chunk.op(Insn::Pop);
                let body_status = Self::compile_for_body(
                    chunk,
                    strict,
                    text,
                    test.clone(),
                    incr.clone(),
                    stmt.clone(),
                    &[],
                    label_set,
                )?;
                if body_status.maybe_abrupt() {
                    maybe_abrupt = AbruptResult::Maybe;
                }
                if let Some(exit) = exit {
                    chunk.fixup(exit)?;
                }
                Ok(maybe_abrupt)
            }
            ForStatement::ForLex(lexdecl, test, incr, stmt, _) => {
                // ForStatement : for ( LexicalDeclaration Expressionopt ; Expressionopt ) Statement
                //  1. Let oldEnv be the running execution context's LexicalEnvironment.
                //  2. Let loopEnv be NewDeclarativeEnvironment(oldEnv).
                //  3. Let isConst be IsConstantDeclaration of LexicalDeclaration.
                //  4. Let boundNames be the BoundNames of LexicalDeclaration.
                //  5. For each element dn of boundNames, do
                //      a. If isConst is true, then
                //          i. Perform ! loopEnv.CreateImmutableBinding(dn, true).
                //      b. Else,
                //          i. Perform ! loopEnv.CreateMutableBinding(dn, false).
                //  6. Set the running execution context's LexicalEnvironment to loopEnv.
                //  7. Let forDcl be Completion(Evaluation of LexicalDeclaration).
                //  8. If forDcl is an abrupt completion, then
                //      a. Set the running execution context's LexicalEnvironment to oldEnv.
                //      b. Return ? forDcl.
                //  9. If isConst is false, let perIterationLets be boundNames; otherwise let perIterationLets
                //     be a new empty List.
                //  10. If the first Expression is present, let test be the first Expression; otherwise, let
                //      test be empty.
                //  11. If the second Expression is present, let increment be the second Expression;
                //      otherwise, let increment be empty.
                //  12. Let bodyResult be Completion(ForBodyEvaluation(test, increment, Statement,
                //      perIterationLets, labelSet)).
                //  13. Set the running execution context's LexicalEnvironment to oldEnv.
                //  14. Return ? bodyResult.

                //   PNLE
                // --- if isConst, for each bn ---
                //   CSILB bn
                // ---
                // --- if not isConst, for each bn ---
                //   CPMLB bn
                // ---
                //   <lexdecl>                forDcl/err
                //   JUMP_IF_ABRUPT popenv
                //   POP
                //   <forbody>                result/err
                // popenv:                    result/err
                //   PLE                      result/err
                // exit:

                chunk.op(Insn::PushNewLexEnv);
                let bound_names = lexdecl.bound_names();
                let is_const = lexdecl.is_constant_declaration();
                for bn in bound_names.iter() {
                    let idx = chunk.add_to_string_pool(bn.clone())?;
                    chunk.op_plus_arg(
                        if is_const {
                            Insn::CreateStrictImmutableLexBinding
                        } else {
                            Insn::CreatePermanentMutableLexBinding
                        },
                        idx,
                    );
                }
                let per_iteration_lets = if is_const { &[] } else { bound_names.as_slice() };
                lexdecl.compile(chunk, strict, text)?;
                let popenv = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Pop);
                Self::compile_for_body(
                    chunk,
                    strict,
                    text,
                    test.clone(),
                    incr.clone(),
                    stmt.clone(),
                    per_iteration_lets,
                    label_set,
                )?;
                chunk.fixup(popenv)?;
                chunk.op(Insn::PopLexEnv);

                Ok(AbruptResult::Maybe)
            }
        }
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
        Ok(AlwaysAbruptResult)
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
        Ok(AlwaysAbruptResult)
    }
}

impl ReturnStatement {
    fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AlwaysAbruptResult> {
        match self {
            ReturnStatement::Bare { .. } => {
                chunk.op(Insn::Undefined);
                chunk.op(Insn::Return);
            }
            ReturnStatement::Expression { exp, .. } => {
                let status = exp.compile(chunk, strict, text)?;
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                let exit = if status.maybe_abrupt() || status.maybe_ref() {
                    Some(chunk.op_jump(Insn::JumpIfAbrupt))
                } else {
                    None
                };
                chunk.op(Insn::Return);
                if let Some(mark) = exit {
                    chunk.fixup(mark).expect("jump too short to fail");
                }
            }
        }
        Ok(AlwaysAbruptResult)
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
            LabelledItem::Function(f) => f.compile(chunk).map(AbruptResult::from),
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
        Ok(AlwaysAbruptResult)
    }
}

impl TryStatement {
    /// Compile the TryStatement production
    ///
    /// See [TryStatement evaluation](https://tc39.es/ecma262/#sec-try-statement-runtime-semantics-evaluation)
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match self {
            TryStatement::Catch { block, catch, .. } => {
                // TryStatement : try Block Catch
                //  1. Let B be the result of evaluating Block.
                //  2. If B.[[Type]] is throw, let C be Completion(CatchClauseEvaluation of Catch with argument B.[[Value]]).
                //  3. Else, let C be B.
                //  4. Return ? UpdateEmpty(C, undefined).
                chunk.op(Insn::Undefined);
                let status = block.compile(chunk, strict, text)?;
                // Stack: val/empty/err undefined ...
                if status.maybe_abrupt() {
                    let exit = chunk.op_jump(Insn::JumpNotThrow);
                    // Stack: err undefined ...
                    catch.compile_catch_clause_evaluation(chunk, strict, text)?;
                    // Stack: catch: val/empty/err undefined ...
                    chunk.fixup(exit)?;
                };
                // Stack: (block: val/empty/abrupt -or- catch: val/empty/err) undefined ...
                chunk.op(Insn::UpdateEmpty);
                // Stack: val/err ...
                Ok(AbruptResult::from(status.maybe_abrupt()))
            }
            TryStatement::Finally { block, finally, .. } => {
                // TryStatement : try Block Finally
                //  1. Let B be the result of evaluating Block.
                //  2. Let F be the result of evaluating Finally.
                //  3. If F.[[Type]] is normal, set F to B.
                //  4. Return ? UpdateEmpty(F, undefined).
                chunk.op(Insn::Undefined);
                // Stack: undefined ...
                let status = block.compile(chunk, strict, text)?;
                // Stack: block:val/empty/err undefined ...
                let finally_status = finally.compile(chunk, strict, text)?;
                // Stack: finally:val/empty/err block:val/empty/err undefined ...
                let short_jump =
                    if finally_status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                // Stack: finally:val/empty block:val/empty/err undefined ...
                chunk.op(Insn::Pop);
                // Stack: block: val/empty/err undefined ...
                chunk.op(Insn::UpdateEmpty);
                // Stack: block: val/err ...
                if finally_status.maybe_abrupt() {
                    let short_exit = chunk.op_jump(Insn::Jump);
                    chunk.fixup(short_jump.expect("properly set")).expect("Jump too short to fail");
                    // Stack: finally:err block: val/empty/err undefined ...
                    chunk.op_plus_arg(Insn::Unwind, 2);
                    // Stack: finally:err ...
                    chunk.fixup(short_exit).expect("Jump too short to fail");
                }
                // Stack: finally:err or block:val/err ...
                Ok(AbruptResult::from(status.maybe_abrupt() || finally_status.maybe_abrupt()))
            }
            TryStatement::Full { block, catch, finally, .. } => {
                // TryStatement : try Block Catch Finally
                //  1. Let B be the result of evaluating Block.
                //  2. If B.[[Type]] is throw, let C be Completion(CatchClauseEvaluation of Catch with argument B.[[Value]]).
                //  3. Else, let C be B.
                //  4. Let F be the result of evaluating Finally.
                //  5. If F.[[Type]] is normal, set F to C.
                //  6. Return ? UpdateEmpty(F, undefined).
                chunk.op(Insn::Undefined);
                // Stack: undefined ...
                let block_status = block.compile(chunk, strict, text)?;
                // Stack: val/empty/err undefined ...
                if block_status.maybe_abrupt() {
                    let after_catch = chunk.op_jump(Insn::JumpNotThrow);
                    // Stack: err undefined ...
                    catch.compile_catch_clause_evaluation(chunk, strict, text)?;
                    // Stack: catch: val/empty/err undefined ...
                    chunk.fixup(after_catch)?;
                };
                // Stack: (block: val/empty/abt -or- catch: val/empty/err) undefined
                let finally_status = finally.compile(chunk, strict, text)?;
                // Stack: finally: val/empty/err (block: val/empty/abt -or- catch: val/empty/err) undefined
                let short_jump =
                    if finally_status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                // Stack: finally:val/empty (block: val/empty/abt -or- catch: val/empty/err) undefined ...
                chunk.op(Insn::Pop);
                // Stack: (block: val/empty/abt -or- catch: val/empty/err) undefined ...
                chunk.op(Insn::UpdateEmpty);
                // Stack: (block: val/abt -or- catch: val/err) ...
                if finally_status.maybe_abrupt() {
                    let short_exit = chunk.op_jump(Insn::Jump);
                    chunk.fixup(short_jump.expect("properly set")).expect("Jump too short to fail");
                    // Stack: finally:err (block: val/empty/abt -or- catch: val/empty/err) undefined ...
                    chunk.op_plus_arg(Insn::Unwind, 2);
                    // Stack: finally:err ...
                    chunk.fixup(short_exit).expect("Jump too short to fail");
                }
                // Stack: (finally:err -or- block: val/abt -or- catch: val/err) ...
                Ok(AbruptResult::from(block_status.maybe_abrupt() || finally_status.maybe_abrupt()))
            }
        }
    }
}

impl Catch {
    fn compile_catch_clause_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AbruptResult> {
        // Stack: Throw(value) ...
        match &self.parameter {
            None => {
                chunk.op(Insn::Pop);
                self.block.compile(chunk, strict, text)
            }
            Some(catch_parameter) => {
                // Catch : catch ( CatchParameter ) Block
                //  1. Let oldEnv be the running execution context's LexicalEnvironment.
                //  2. Let catchEnv be NewDeclarativeEnvironment(oldEnv).
                //  3. For each element argName of the BoundNames of CatchParameter, do
                //      a. Perform ! catchEnv.CreateMutableBinding(argName, false).
                //  4. Set the running execution context's LexicalEnvironment to catchEnv.
                //  5. Let status be Completion(BindingInitialization of CatchParameter with arguments
                //     thrownValue and catchEnv).
                //  6. If status is an abrupt completion, then
                //      a. Set the running execution context's LexicalEnvironment to oldEnv.
                //      b. Return ? status.
                //  7. Let B be the result of evaluating Block.
                //  8. Set the running execution context's LexicalEnvironment to oldEnv.
                //  9. Return ? B.
                chunk.op(Insn::PushNewLexEnv);
                for arg_name in catch_parameter.bound_names() {
                    let idx = chunk.add_to_string_pool(arg_name)?;
                    chunk.op_plus_arg(Insn::CreatePermanentMutableLexBinding, idx);
                }
                chunk.op(Insn::ExtractThrownValue);
                let param_status = catch_parameter.compile_binding_initialization(chunk, strict, text)?;
                if param_status.maybe_abrupt() {
                    todo!();
                    // I don't have identifier binding init putting anything back on the stack, but the
                    // unimplemented binding patterns might. Without anything on the stack, we can't even
                    // _check_ for errors. So until patterns are in, this is going to stay todo.
                }
                let block_status = self.block.compile(chunk, strict, text)?;
                chunk.op(Insn::PopLexEnv);

                Ok(AbruptResult::from(param_status.maybe_abrupt() || block_status.maybe_abrupt()))
            }
        }
    }
}

impl Finally {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        self.block.compile(chunk, strict, text)
    }
}

impl CatchParameter {
    fn compile_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AbruptResult> {
        match self {
            CatchParameter::Ident(node) => {
                node.compile_binding_initialization(chunk, strict, EnvUsage::UseCurrentLexical).map(AbruptResult::from)
            }
            CatchParameter::Pattern(node) => node
                .compile_binding_initialization(chunk, strict, text, EnvUsage::UseCurrentLexical)
                .map(AbruptResult::from),
        }
    }
}

impl FunctionDeclaration {
    fn compile(&self, chunk: &mut Chunk) -> anyhow::Result<NeverAbruptRefResult> {
        chunk.op(Insn::Empty);
        Ok(NeverAbruptRefResult)
    }

    #[allow(unused_variables)]
    fn compile_fo_instantiation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        self_as_rc: Rc<Self>,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: InstantiateOrdinaryFunctionObject
        //
        // The syntax-directed operation InstantiateOrdinaryFunctionObject takes arguments env and privateEnv
        // and returns a function object. It is defined piecewise over the following productions:
        //
        // FunctionDeclaration : function BindingIdentifier ( FormalParameters ) { FunctionBody }
        //  1. Let name be StringValue of BindingIdentifier.
        //  2. Let sourceText be the source text matched by FunctionDeclaration.
        //  3. Let F be OrdinaryFunctionCreate(%Function.prototype%, sourceText, FormalParameters,
        //     FunctionBody, non-lexical-this, env, privateEnv).
        //  4. Perform SetFunctionName(F, name).
        //  5. Perform MakeConstructor(F).
        //  6. Return F.
        //
        // FunctionDeclaration : function ( FormalParameters ) { FunctionBody }
        //  1. Let sourceText be the source text matched by FunctionDeclaration.
        //  2. Let F be OrdinaryFunctionCreate(%Function.prototype%, sourceText, FormalParameters,
        //     FunctionBody, non-lexical-this, env, privateEnv).
        //  3. Perform SetFunctionName(F, "default").
        //  4. Perform MakeConstructor(F).
        //  5. Return F.
        //
        // NOTE: An anonymous FunctionDeclaration can only occur as part of an export default declaration, and
        // its function code is therefore always strict mode code.

        let name_id = chunk.add_to_string_pool(match &self.ident {
            None => JSString::from("default"),
            Some(id) => id.string_value(),
        })?;

        let span = self.location().span;
        let source_text = text[span.starting_index..(span.starting_index + span.length)].to_string();
        let params = ParamSource::from(Rc::clone(&self.params));
        let body = BodySource::from(Rc::clone(&self.body));
        let function_data = StashedFunctionData {
            source_text,
            params,
            body,
            strict,
            to_compile: FunctionSource::from(self_as_rc),
            this_mode: ThisLexicality::NonLexicalThis,
        };
        let func_id = chunk.add_to_func_stash(function_data)?;
        chunk.op_plus_two_args(Insn::InstantiateOrdinaryFunctionObject, name_id, func_id);
        Ok(AlwaysAbruptResult)
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

pub enum NameLoc {
    None,
    OnStack,
    Index(u16),
}

impl FunctionExpression {
    /// Generate code to create a potentially named function object
    ///
    /// See [InstantiateOrdinaryFunctionExpression](https://tc39.es/ecma262/#sec-runtime-semantics-instantiateordinaryfunctionexpression) in ECMA-262.
    #[allow(unused_variables)]
    fn instantiate_ordinary_function_expression(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        name: NameLoc,
        text: &str,
        self_as_rc: Rc<Self>,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: InstantiateOrdinaryFunctionExpression
        match &self.ident {
            None => {
                // The syntax-directed operation InstantiateOrdinaryFunctionExpression takes optional argument name and
                // returns a function object. It is defined piecewise over the following productions:
                //
                //  FunctionExpression : function ( FormalParameters ) { FunctionBody }
                //      1. If name is not present, set name to "".
                //      2. Let env be the LexicalEnvironment of the running execution context.
                //      3. Let privateEnv be the running execution context's PrivateEnvironment.
                //      4. Let sourceText be the source text matched by FunctionExpression.
                //      5. Let closure be OrdinaryFunctionCreate(%Function.prototype%, sourceText, FormalParameters,
                //         FunctionBody, non-lexical-this, env, privateEnv).
                //      6. Perform SetFunctionName(closure, name).
                //      7. Perform MakeConstructor(closure).
                //      8. Return closure.
                if let Some(name_id) = match name {
                    NameLoc::None => Some(chunk.add_to_string_pool(JSString::from(""))?),
                    NameLoc::Index(id) => Some(id),
                    NameLoc::OnStack => None,
                } {
                    chunk.op_plus_arg(Insn::String, name_id);
                }

                let span = self.location().span;
                let params = ParamSource::from(Rc::clone(&self.params));
                let body = BodySource::from(Rc::clone(&self.body));
                let function_data = StashedFunctionData {
                    source_text: text[span.starting_index..(span.starting_index + span.length)].to_string(),
                    params,
                    body,
                    strict,
                    to_compile: FunctionSource::from(self_as_rc),
                    this_mode: ThisLexicality::NonLexicalThis,
                };
                let func_id = chunk.add_to_func_stash(function_data)?;
                chunk.op_plus_arg(Insn::InstantiateIdFreeFunctionExpression, func_id);
                Ok(AlwaysAbruptResult)
            }
            Some(bi) => {
                let name = bi.string_value();
                let name_idx = chunk.add_to_string_pool(name)?;
                chunk.op_plus_arg(Insn::String, name_idx);

                let span = self.location().span;
                let params = ParamSource::from(Rc::clone(&self.params));
                let body = BodySource::from(Rc::clone(&self.body));
                let function_data = StashedFunctionData {
                    source_text: text[span.starting_index..(span.starting_index + span.length)].to_string(),
                    params,
                    body,
                    strict,
                    to_compile: FunctionSource::from(self_as_rc),
                    this_mode: ThisLexicality::NonLexicalThis,
                };
                let func_id = chunk.add_to_func_stash(function_data)?;
                chunk.op_plus_arg(Insn::InstantiateOrdinaryFunctionExpression, func_id);
                Ok(AlwaysAbruptResult)
            }
        }
    }

    /// Generate the code to evaluate a ['FunctionExpression'].
    ///
    /// See [FunctionExpression Evaluation](https://tc39.es/ecma262/#sec-function-definitions-runtime-semantics-evaluation) from ECMA-262.
    pub fn compile(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        self_as_rc: Rc<Self>,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: Evaluation
        //  FunctionExpression : function BindingIdentifier[opt] ( FormalParameters ) { FunctionBody }
        //      1. Return InstantiateOrdinaryFunctionExpression of FunctionExpression.
        //
        // NOTE     | A "prototype" property is automatically created for every function defined using a
        //          | FunctionDeclaration or FunctionExpression, to allow for the possibility that the
        //          | function will be used as a constructor.
        //
        self.instantiate_ordinary_function_expression(chunk, strict, NameLoc::None, text, self_as_rc)
    }

    fn compile_named_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        self_as_rc: Rc<Self>,
        id: NameLoc,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        self.instantiate_ordinary_function_expression(chunk, strict, id, text, self_as_rc)
    }
}

/// Generates the code necessary to set up function execution
pub fn compile_fdi(chunk: &mut Chunk, text: &str, info: &StashedFunctionData) -> anyhow::Result<AbruptResult> {
    // FunctionDeclarationInstantiation ( func, argumentsList )
    //
    // The abstract operation FunctionDeclarationInstantiation takes arguments func (a function object) and
    // argumentsList and returns either a normal completion containing unused or an abrupt completion. func is the
    // function object for which the execution context is being established.
    //
    // NOTE 1  | When an execution context is established for evaluating an ECMAScript function a new function
    //         | Environment Record is created and bindings for each formal parameter are instantiated in that
    //         | Environment Record. Each declaration in the function body is also instantiated. If the function's
    //         | formal parameters do not include any default value initializers then the body declarations are
    //         | instantiated in the same Environment Record as the parameters. If default value parameter
    //         | initializers exist, a second Environment Record is created for the body declarations. Formal
    //         | parameters and functions are initialized as part of FunctionDeclarationInstantiation. All other
    //         | bindings are initialized during evaluation of the function body.
    //
    // It performs the following steps when called: (🏃 = Run time; 🧑‍💻 = Compile time)
    //
    //🏃   1. Let calleeContext be the running execution context.
    //🧑‍💻   2. Let code be func.[[ECMAScriptCode]].
    //🧑‍💻   3. Let strict be func.[[Strict]].
    //🧑‍💻   4. Let formals be func.[[FormalParameters]].
    //🧑‍💻   5. Let parameterNames be the BoundNames of formals.
    //🧑‍💻   6. If parameterNames has any duplicate entries, let hasDuplicates be true. Otherwise, let hasDuplicates be
    //🧑‍💻      false.
    //🧑‍💻   7. Let simpleParameterList be IsSimpleParameterList of formals.
    //🧑‍💻   8. Let hasParameterExpressions be ContainsExpression of formals.
    //🧑‍💻   9. Let varNames be the VarDeclaredNames of code.
    //🧑‍💻  10. Let varDeclarations be the VarScopedDeclarations of code.
    //🧑‍💻  11. Let lexicalNames be the LexicallyDeclaredNames of code.
    //🧑‍💻  12. Let functionNames be a new empty List.
    //🧑‍💻  13. Let functionsToInitialize be a new empty List.
    //🧑‍💻  14. For each element d of varDeclarations, in reverse List order, do
    //🧑‍💻      a. If d is neither a VariableDeclaration nor a ForBinding nor a BindingIdentifier, then
    //🧑‍💻            i. Assert: d is either a FunctionDeclaration, a GeneratorDeclaration, an
    //🧑‍💻               AsyncFunctionDeclaration, or an AsyncGeneratorDeclaration.
    //🧑‍💻           ii. Let fn be the sole element of the BoundNames of d.
    //🧑‍💻          iii. If fn is not an element of functionNames, then
    //🧑‍💻              1. Insert fn as the first element of functionNames.
    //🧑‍💻              2. NOTE: If there are multiple function declarations for the same name, the last declaration is
    //🧑‍💻                 used.
    //🧑‍💻              3. Insert d as the first element of functionsToInitialize.
    //🧑‍💻  15. Let argumentsObjectNeeded be true.
    //🧑‍💻  16. If func.[[ThisMode]] is lexical, then
    //🧑‍💻      a. NOTE: Arrow functions never have an arguments object.
    //🧑‍💻      b. Set argumentsObjectNeeded to false.
    //🧑‍💻  17. Else if "arguments" is an element of parameterNames, then
    //🧑‍💻      a. Set argumentsObjectNeeded to false.
    //🧑‍💻  18. Else if hasParameterExpressions is false, then
    //🧑‍💻      a. If "arguments" is an element of functionNames or if "arguments" is an element of lexicalNames, then
    //🧑‍💻            i. Set argumentsObjectNeeded to false.
    //🧑‍💻  19. If strict is true or if hasParameterExpressions is false, then
    //🧑‍💻      a. NOTE: Only a single Environment Record is needed for the parameters, since calls to eval in strict
    //🧑‍💻         mode code cannot create new bindings which are visible outside of the eval.
    //🏃      b. Let env be the LexicalEnvironment of calleeContext.
    //🧑‍💻  20. Else,
    //🏃      a. NOTE: A separate Environment Record is needed to ensure that bindings created by direct eval calls
    //🏃         in the formal parameter list are outside the environment where parameters are declared.
    //🏃      b. Let calleeEnv be the LexicalEnvironment of calleeContext.
    //🏃      c. Let env be NewDeclarativeEnvironment(calleeEnv).
    //🏃      d. Assert: The VariableEnvironment of calleeContext is calleeEnv.
    //🏃      e. Set the LexicalEnvironment of calleeContext to env.
    //🧑‍💻  21. For each String paramName of parameterNames, do
    //🏃      a. Let alreadyDeclared be ! env.HasBinding(paramName).
    //🏃      b. NOTE: Early errors ensure that duplicate parameter names can only occur in non-strict functions that
    //🏃         do not have parameter default values or rest parameters.
    //🏃      c. If alreadyDeclared is false, then
    //🏃            i. Perform ! env.CreateMutableBinding(paramName, false).
    //🧑‍💻           ii. If hasDuplicates is true, then
    //🏃              1. Perform ! env.InitializeBinding(paramName, undefined).
    //🧑‍💻  22. If argumentsObjectNeeded is true, then
    //🧑‍💻      a. If strict is true or if simpleParameterList is false, then
    //🏃            i. Let ao be CreateUnmappedArgumentsObject(argumentsList).
    //🧑‍💻      b. Else,
    //🏃            i. NOTE: A mapped argument object is only provided for non-strict functions that don't have a
    //🏃               rest parameter, any parameter default value initializers, or any destructured parameters.
    //🏃           ii. Let ao be CreateMappedArgumentsObject(func, formals, argumentsList, env).
    //🧑‍💻      c. If strict is true, then
    //🏃            i. Perform ! env.CreateImmutableBinding("arguments", false).
    //🏃           ii. NOTE: In strict mode code early errors prevent attempting to assign to this binding, so its
    //🏃               mutability is not observable.
    //🧑‍💻      d. Else,
    //🏃            i. Perform ! env.CreateMutableBinding("arguments", false).
    //🏃      e. Perform ! env.InitializeBinding("arguments", ao).
    //🧑‍💻      f. Let parameterBindings be the list-concatenation of parameterNames and « "arguments" ».
    //🧑‍💻  23. Else,
    //🧑‍💻      a. Let parameterBindings be parameterNames.
    //🏃  24. Let iteratorRecord be CreateListIteratorRecord(argumentsList).
    //🧑‍💻  25. If hasDuplicates is true, then
    //🏃      a. Perform ? IteratorBindingInitialization of formals with arguments iteratorRecord and undefined.
    //🧑‍💻  26. Else,
    //🏃      a. Perform ? IteratorBindingInitialization of formals with arguments iteratorRecord and env.
    //🧑‍💻  27. If hasParameterExpressions is false, then
    //🧑‍💻      a. NOTE: Only a single Environment Record is needed for the parameters and top-level vars.
    //🧑‍💻      b. Let instantiatedVarNames be a copy of the List parameterBindings.
    //🧑‍💻      c. For each element n of varNames, do
    //🧑‍💻            i. If n is not an element of instantiatedVarNames, then
    //🧑‍💻              1. Append n to instantiatedVarNames.
    //🏃              2. Perform ! env.CreateMutableBinding(n, false).
    //🏃              3. Perform ! env.InitializeBinding(n, undefined).
    //🏃      d. Let varEnv be env.
    //🧑‍💻  28. Else,
    //🏃      a. NOTE: A separate Environment Record is needed to ensure that closures created by expressions in the
    //🏃        formal parameter list do not have visibility of declarations in the function body.
    //🏃      b. Let varEnv be NewDeclarativeEnvironment(env).
    //🏃      c. Set the VariableEnvironment of calleeContext to varEnv.
    //🧑‍💻      d. Let instantiatedVarNames be a new empty List.
    //🧑‍💻      e. For each element n of varNames, do
    //🧑‍💻            i. If n is not an element of instantiatedVarNames, then
    //🧑‍💻              1. Append n to instantiatedVarNames.
    //🏃              2. Perform ! varEnv.CreateMutableBinding(n, false).
    //🧑‍💻              3. If n is not an element of parameterBindings or if n is an element of functionNames, let
    //🏃                 initialValue be undefined.
    //🧑‍💻              4. Else,
    //🏃                  a. Let initialValue be ! env.GetBindingValue(n, false).
    //🏃              5. Perform ! varEnv.InitializeBinding(n, initialValue).
    //🏃              6. NOTE: A var with the same name as a formal parameter initially has the same value as the
    //🏃                 corresponding initialized parameter.
    //🧑‍💻  29. NOTE: Annex B.3.2.1 adds additional steps at this point.
    //🧑‍💻  30. If strict is false, then
    //🏃      a. Let lexEnv be NewDeclarativeEnvironment(varEnv).
    //🏃      b. NOTE: Non-strict functions use a separate Environment Record for top-level lexical declarations so
    //🏃         that a direct eval can determine whether any var scoped declarations introduced by the eval code
    //🏃         conflict with pre-existing top-level lexically scoped declarations. This is not needed for strict
    //🏃         functions because a strict direct eval always places all declarations into a new Environment Record.
    //🧑‍💻  31. Else,
    //🏃      a. Let lexEnv be varEnv.
    //🏃  32. Set the LexicalEnvironment of calleeContext to lexEnv.
    //🧑‍💻  33. Let lexDeclarations be the LexicallyScopedDeclarations of code.
    //🧑‍💻  34. For each element d of lexDeclarations, do
    //🧑‍💻      a. NOTE: A lexically declared name cannot be the same as a function/generator declaration, formal
    //🧑‍💻         parameter, or a var name. Lexically declared names are only instantiated here but not initialized.
    //🧑‍💻      b. For each element dn of the BoundNames of d, do
    //🧑‍💻            i. If IsConstantDeclaration of d is true, then
    //🏃              1. Perform ! lexEnv.CreateImmutableBinding(dn, true).
    //🧑‍💻           ii. Else,
    //🏃              1. Perform ! lexEnv.CreateMutableBinding(dn, false).
    //🏃  35. Let privateEnv be the PrivateEnvironment of calleeContext.
    //🧑‍💻  36. For each Parse Node f of functionsToInitialize, do
    //🧑‍💻      a. Let fn be the sole element of the BoundNames of f.
    //🏃      b. Let fo be InstantiateFunctionObject of f with arguments lexEnv and privateEnv.
    //🏃      c. Perform ! varEnv.SetMutableBinding(fn, fo, false).
    //🧑‍💻  37. Return unused.
    //
    //  NOTE 2   | B.3.2 provides an extension to the above algorithm that is necessary for backwards compatibility
    //           | with web browser implementations of ECMAScript that predate ECMAScript 2015.

    // Stack: N arg[n-1] arg[n-2] ... arg[1] arg[0] func

    let code = &info.body;
    let strict = info.strict || code.contains_use_strict();
    let formals = &info.params;
    let mut parameter_names = formals.bound_names();
    let has_duplicates = parameter_names.iter().collect::<Counter<_>>().into_iter().filter(|&(_, n)| n > 1).count() > 0;
    let simple_parameter_list = formals.is_simple_parameter_list();
    let has_parameter_expressions = formals.contains_expression();
    let var_names = code.var_declared_names();
    let var_declarations = code.var_scoped_declarations();
    let lexical_names = code.lexically_declared_names();
    let mut function_names = vec![];
    let mut functions_to_initialize = vec![];
    for d in var_declarations.iter().rev().cloned().filter_map(|decl| FcnDef::try_from(decl).ok()) {
        let func_name = d.bound_name();
        if !function_names.contains(&func_name) {
            function_names.insert(0, func_name);
            functions_to_initialize.insert(0, d);
        }
    }
    let a = JSString::from("arguments");
    let arguments_object_needed = info.this_mode == ThisLexicality::NonLexicalThis
        && !parameter_names.contains(&a)
        && !(!has_parameter_expressions && (function_names.contains(&a) || lexical_names.contains(&a)));

    if !strict && has_parameter_expressions {
        chunk.op(Insn::PushNewLexEnv);
    }

    for param_name in parameter_names.iter() {
        let sidx = chunk.add_to_string_pool(param_name.clone())?;
        chunk.op_plus_arg(
            if has_duplicates {
                Insn::CreateInitializedPermanentMutableLexIfMissing
            } else {
                Insn::CreatePermanentMutableLexIfMissing
            },
            sidx,
        );
    }

    // 22-23.
    if arguments_object_needed {
        if strict || !simple_parameter_list {
            chunk.op(Insn::CreateUnmappedArguments);
        } else {
            chunk.op(Insn::CreateMappedArguments);
        }
        let args_idx = chunk.add_to_string_pool("arguments".into())?;
        chunk.op_plus_arg(
            if strict { Insn::CreateNonStrictImmutableLexBinding } else { Insn::CreatePermanentMutableLexBinding },
            args_idx,
        );
        chunk.op_plus_arg(Insn::InitializeLexBinding, args_idx);
        parameter_names.push(JSString::from("arguments"));
    }

    // 24-26.
    let status = formals.compile_binding_initialization(
        chunk,
        strict,
        text,
        if has_duplicates { EnvUsage::UsePutValue } else { EnvUsage::UseCurrentLexical },
    )?;
    // Stack: N arg[N-1] ... arg[0] func ... ---or--- err func ...
    let mut exit = None;
    if status.maybe_abrupt() {
        exit = Some(chunk.op_jump(Insn::JumpIfAbrupt));
    }
    chunk.op(Insn::FinishArgs);
    // Stack: func ...

    // 27-28.
    if !has_parameter_expressions {
        let mut instantiated_var_names = parameter_names.iter().cloned().collect::<AHashSet<_>>();
        for n in var_names {
            if !instantiated_var_names.contains(&n) {
                let idx = chunk.add_to_string_pool(n.clone())?;
                chunk.op_plus_arg(Insn::CreatePermanentMutableLexBinding, idx);
                chunk.op(Insn::Undefined);
                chunk.op_plus_arg(Insn::InitializeLexBinding, idx);
                instantiated_var_names.insert(n);
            }
        }
        // let varEnv be env == current lexical environment
    } else {
        // b. Let varEnv be NewDeclarativeEnvironment(env).
        // c. Set the VariableEnvironment of calleeContext to varEnv.
        chunk.op(Insn::PushNewVarEnvFromLex);
        let mut instantiated_var_names = AHashSet::<JSString>::new();
        for n in var_names {
            if !instantiated_var_names.contains(&n) {
                instantiated_var_names.insert(n.clone());
                let idx = chunk.add_to_string_pool(n.clone())?;
                chunk.op_plus_arg(Insn::CreatePermanentMutableVarBinding, idx);
                if !parameter_names.contains(&n) || function_names.contains(&n) {
                    chunk.op(Insn::Undefined);
                } else {
                    chunk.op_plus_arg(Insn::GetLexBinding, idx);
                }
                chunk.op_plus_arg(Insn::InitializeVarBinding, idx);
            }
        }
        // now varEnv == current variable environment
    }

    // 30-32.
    // If strict && has_parameter_expressions
    //    set current_lexical_environment = current variable environment
    // else if strict && !has_parameter_expressions
    //    // nothing
    // else if !strict && has_parameter_expressions
    //    set current_lexical_environment = new_from(current variable environment)
    // else if !strict && !has_parameter_expressions
    //    set current_lexical_environment = new_from(current lexical environment)
    // ==> lexenv is now the current lexical environment;
    //     varenv is now current lexical environment if !has_parameter_expressions else current variable environment
    match (strict, has_parameter_expressions) {
        (true, true) => chunk.op(Insn::SetLexEnvToVarEnv),
        (true, false) => (),
        (false, true) => chunk.op(Insn::PushNewLexEnvFromVar),
        (false, false) => chunk.op(Insn::PushNewLexEnv),
    }

    // 33-34.
    let lex_declarations = code.lexically_scoped_declarations();
    for d in lex_declarations {
        for dn in d.bound_names() {
            let idx = chunk.add_to_string_pool(dn)?;
            chunk.op_plus_arg(
                if d.is_constant_declaration() {
                    Insn::CreateStrictImmutableLexBinding
                } else {
                    Insn::CreatePermanentMutableLexBinding
                },
                idx,
            );
        }
    }

    // 35-36.
    for f in functions_to_initialize {
        let fname = f.bound_name();
        let idx = chunk.add_to_string_pool(fname).expect("Name already present (steps 27-28)");
        f.compile_fo_instantiation(chunk, strict, text)?;
        chunk.op_plus_arg(Insn::SetMutableVarBinding, idx);
    }

    // Done
    if let Some(&mark) = exit.as_ref() {
        chunk.fixup(mark)?;
    }

    // Stack: err/func ...

    Ok(AbruptResult::from(exit.is_some()))
}

impl ArrowFunction {
    fn instantiate_arrow_function_expression(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        name: NameLoc,
        self_as_rc: Rc<Self>,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        if let Some(name_id) = match name {
            NameLoc::None => Some(chunk.add_to_string_pool(JSString::from(""))?),
            NameLoc::Index(id) => Some(id),
            NameLoc::OnStack => None,
        } {
            chunk.op_plus_arg(Insn::String, name_id);
        }

        let span = self.location().span;
        let source_text = text[span.starting_index..(span.starting_index + span.length)].to_string();
        let params = ParamSource::from(Rc::clone(&self.parameters));
        let body = BodySource::from(Rc::clone(&self.body));
        let function_data = StashedFunctionData {
            source_text,
            params,
            body,
            strict,
            to_compile: FunctionSource::from(self_as_rc),
            this_mode: ThisLexicality::LexicalThis,
        };
        let func_id = chunk.add_to_func_stash(function_data)?;
        chunk.op_plus_arg(Insn::InstantiateArrowFunctionExpression, func_id);
        Ok(AlwaysAbruptResult)
    }

    pub fn compile(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        self_as_rc: Rc<Self>,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        self.instantiate_arrow_function_expression(chunk, strict, text, NameLoc::None, self_as_rc)
    }

    pub fn compile_named_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        self_as_rc: Rc<Self>,
        id: NameLoc,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        self.instantiate_arrow_function_expression(chunk, strict, text, id, self_as_rc)
    }
}

impl ConciseBody {
    pub fn compile_body(
        &self,
        chunk: &mut Chunk,
        text: &str,
        info: &StashedFunctionData,
    ) -> anyhow::Result<AbruptResult> {
        // Runtime Semantics: EvaluateBody
        // ConciseBody : ExpressionBody
        //  1. Return ? EvaluateConciseBody of ConciseBody with arguments functionObject and argumentsList.

        // Runtime Semantics: EvaluateConciseBody
        //
        // The syntax-directed operation EvaluateConciseBody takes arguments functionObject and argumentsList
        // (a List) and returns either a normal completion containing an ECMAScript language value or an
        // abrupt completion. It is defined piecewise over the following productions:
        //
        // ConciseBody : ExpressionBody
        //  1. Perform ? FunctionDeclarationInstantiation(functionObject, argumentsList).
        //  2. Return the result of evaluating ExpressionBody.
        match self {
            ConciseBody::Function { body, .. } => body.compile_body(chunk, text, info),
            ConciseBody::Expression(exp) => {
                // Stack: N arg[n-1] arg[n-2] ... arg[1] arg[0] func
                let fdi_status = compile_fdi(chunk, text, info)?;
                let exit = if fdi_status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };

                // Stack: func ...
                let strict = info.strict;
                let eval_status = exp.compile(chunk, strict, text)?;
                // Stack: result func ...

                if let Some(mark) = exit {
                    chunk.fixup(mark)?;
                }

                chunk.op(Insn::EndFunction);

                Ok(AbruptResult::from(fdi_status.maybe_abrupt() || eval_status.maybe_abrupt()))
            }
        }
    }
}

impl ExpressionBody {
    fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AlwaysAbruptResult> {
        // ExpressionBody : AssignmentExpression
        //  1. Let exprRef be the result of evaluating AssignmentExpression.
        //  2. Let exprValue be ? GetValue(exprRef).
        //  3. Return Completion Record { [[Type]]: return, [[Value]]: exprValue, [[Target]]: empty }.
        let status = self.expression.compile(chunk, strict, text)?;
        if status.maybe_ref() {
            chunk.op(Insn::GetValue);
        }
        let exit =
            if status.maybe_abrupt() || status.maybe_ref() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
        chunk.op(Insn::Return);
        if let Some(mark) = exit {
            chunk.fixup(mark).expect("Jump too short to overflow");
        }
        Ok(AlwaysAbruptResult)
    }
}

impl ParamSource {
    pub fn compile_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AbruptResult> {
        match self {
            ParamSource::FormalParameters(params) => params.compile_binding_initialization(chunk, strict, text, env),
            ParamSource::ArrowParameters(params) => params.compile_binding_initialization(chunk, strict, text, env),
            ParamSource::AsyncArrowBinding(_) => todo!(),
            ParamSource::ArrowFormals(_) => todo!(),
        }
    }
}

impl FormalParameters {
    #[allow(unused_variables)]
    pub fn compile_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AbruptResult> {
        match self {
            FormalParameters::Empty(_) => Ok(AbruptResult::from(false)),
            FormalParameters::Rest(frp) => frp.compile_binding_initialization(chunk, strict, text, env),
            FormalParameters::List(fpl) | FormalParameters::ListComma(fpl, _) => {
                fpl.compile_binding_initialization(chunk, strict, text, env)
            }
            FormalParameters::ListRest(list, rest) => {
                let list_status = list.compile_binding_initialization(chunk, strict, text, env)?;
                todo!()
                //let rest_status = rest.compile_binding_initialization(chunk, strict, text, has_duplicates)?;
                //Ok(AbruptResult::from(list_status.maybe_abrupt() || rest_status.maybe_abrupt()))
            }
        }
    }
}

impl ArrowParameters {
    pub fn compile_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AbruptResult> {
        // Stack: N arg[n-1] ... arg[0]
        match self {
            ArrowParameters::Identifier(bi) => {
                chunk.op(Insn::ExtractArg);
                // Stack: val n-1 arg[n-1] ... arg[1]
                bi.compile_binding_initialization(chunk, strict, env).map(AbruptResult::from)
            }
            ArrowParameters::Formals(afp) => afp.compile_binding_initialization(chunk, strict, text, env),
        }
    }
}

impl ArrowFormalParameters {
    fn compile_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AbruptResult> {
        self.params.compile_binding_initialization(chunk, strict, text, env)
    }
}

impl UniqueFormalParameters {
    fn compile_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AbruptResult> {
        self.formals.compile_binding_initialization(chunk, strict, text, env)
    }
}

fn compile_initialize_bound_name(chunk: &mut Chunk, strict: bool, env: EnvUsage, idx: u16) -> NeverAbruptRefResult {
    match env {
        EnvUsage::UsePutValue => {
            chunk.op_plus_arg(Insn::String, idx);
            chunk.op(if strict { Insn::StrictResolve } else { Insn::Resolve });
            chunk.op(Insn::Swap);
            chunk.op(Insn::PutValue);
            // The spec has this PutValue marked with '?'. But I can't figure out how to get there. If this panic
            // happens, add a test!
            chunk.op(Insn::PopOrPanic);
        }
        EnvUsage::UseCurrentLexical => chunk.op_plus_arg(Insn::InitializeLexBinding, idx),
    }
    NeverAbruptRefResult
}

impl BindingIdentifier {
    pub fn compile_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        env: EnvUsage,
    ) -> anyhow::Result<NeverAbruptRefResult> {
        // Stack: val ...
        let binding_id = match self {
            BindingIdentifier::Identifier { identifier, .. } => identifier.string_value(),
            BindingIdentifier::Yield { .. } => JSString::from("yield"),
            BindingIdentifier::Await { .. } => JSString::from("await"),
        };
        let id_idx = chunk.add_to_string_pool(binding_id)?;
        compile_initialize_bound_name(chunk, strict, env, id_idx);
        Ok(NeverAbruptRefResult)
    }
}

impl FormalParameterList {
    pub fn compile_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AbruptResult> {
        match self {
            FormalParameterList::Item(item) => item.compile_binding_initialization(chunk, strict, text, env),
            FormalParameterList::List(list, item) => {
                let list_status = list.compile_binding_initialization(chunk, strict, text, env)?;
                let item_status = item.compile_binding_initialization(chunk, strict, text, env)?;
                Ok(AbruptResult::from(list_status.maybe_abrupt() || item_status.maybe_abrupt()))
            }
        }
    }
}

impl FormalParameter {
    pub fn compile_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AbruptResult> {
        self.element.compile_binding_initialization(chunk, strict, text, env)
    }
}

impl BindingElement {
    pub fn compile_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AbruptResult> {
        // Stack: N arg[n-1] ... arg[0]
        match self {
            BindingElement::Single(single) => single.compile_binding_initialization(chunk, strict, text, env),
            BindingElement::Pattern(bp, None) => {
                // start:                       N arg(n-1) ... arg(0)
                //   EXTRACT_ARG                arg0 (n-1) arg(n-1) ... arg(1)
                //   <bp.bi(env)>               [empty]/err (n-1) arg(n-1) ... arg(1)
                //   JUMP_IF_ABRUPT unwind      [empty] (n-1) arg(n-1) ... arg(1)
                //   POP
                //   JUMP exit
                // unwind:
                //   UNWIND_LIST
                // exit:
                chunk.op(Insn::ExtractArg);
                bp.compile_binding_initialization(chunk, strict, text, env)?;
                let unwind = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Pop);
                let exit = chunk.op_jump(Insn::Jump);
                chunk.fixup(unwind).expect("Jump too short to fail");
                chunk.op(Insn::UnwindList);
                chunk.fixup(exit).expect("Jump too short to fail");
                Ok(AbruptResult::Maybe)
            }
            BindingElement::Pattern(bp, Some(init)) => {
                // start:                       N arg(n-1) ... arg(0)
                //   EXTRACT_ARG                arg0 (n-1) arg(n-1) ... arg(1)
                //   JUMP_IF_NOT_UNDEF ok       undefined (n-1) arg(n-1) ... arg(1)
                //   POP                        (n-1) arg(n-1) ... arg(1)
                //   <init>                     ref/val/err (n-1) arg(n-1) ... arg(1)
                //   GET_VALUE                  val/err (n-1) arg(n-1) ... arg(1)
                //   JUMP_IF_ABRUPT unwind      val (n-1) arg(n-1) ... arg(1)
                // ok:                          val (n-1) arg(n-1) ... arg(1)
                //   <bp.bi(env)>               [empty]/err (n-1) arg(n-1) ... arg(1)
                //   JUMP_IF_NORMAL exit
                // unwind:
                //   UNWIND_LIST
                // exit:                        ([empty] (n-1) arg(n-1) ... arg(1)) or err

                chunk.op(Insn::ExtractArg);
                let mark = chunk.op_jump(Insn::JumpIfNotUndef);
                chunk.op(Insn::Pop);
                let izer_status = init.compile(chunk, strict, text)?;
                if izer_status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                let mut unwind = None;
                if izer_status.maybe_abrupt() || izer_status.maybe_ref() {
                    unwind = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                chunk.fixup(mark)?;
                bp.compile_binding_initialization(chunk, strict, text, env)?;
                let exit = chunk.op_jump(Insn::JumpIfNormal);
                if let Some(unwind) = unwind {
                    chunk.fixup(unwind)?;
                }
                chunk.op(Insn::UnwindList);
                chunk.fixup(exit).expect("jump too short to fail");
                Ok(AbruptResult::Maybe)
            }
        }
    }

    pub fn keyed_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: KeyedBindingInitialization
        // The syntax-directed operation KeyedBindingInitialization takes arguments value (an ECMAScript language
        // value), environment (an Environment Record or undefined), and propertyName (a property key) and returns
        // either a normal completion containing unused or an abrupt completion.
        //
        // NOTE: When undefined is passed for environment it indicates that a PutValue operation should be used to
        //       assign the initialization value. This is the case for formal parameter lists of non-strict functions.
        //       In that case the formal parameter bindings are preinitialized in order to deal with the possibility of
        //       multiple parameters with the same name.
        //
        // It is defined piecewise over the following productions:
        match self {
            BindingElement::Single(snb) => {
                // BindingElement : SingleNameBinding
                //  1. Return ? KeyedBindingInitialization of SingleNameBinding with arguments value, environment, and
                //     propertyName.
                snb.keyed_binding_initialization(chunk, strict, text, env)
            }
            BindingElement::Pattern(bp, izer) => {
                // BindingElement : BindingPattern Initializeropt
                //  1. Let v be ? GetV(value, propertyName).
                //  2. If Initializer is present and v is undefined, then
                //      a. Let defaultValue be ? Evaluation of Initializer.
                //      b. Set v to ? GetValue(defaultValue).
                //  3. Return ? BindingInitialization of BindingPattern with arguments v and environment.

                // start:                         propertyName value
                //   GETV                         v/err
                //   JUMP_IF_ABRUPT exit          v
                // --- if Initializer
                //   JUMP_IF_NOT_UNDEF vok        undefined
                //   POP
                //   <izer>                       defaultValue/err
                //   GET_VALUE                    v/err
                //   JUMP_IF_ABRUPT exit          v
                // vok:
                // ---
                //   <bp.binding_initialization(env)>  [empty]/err
                // exit:

                chunk.op(Insn::GetV);
                let mut exits = vec![];
                exits.push(chunk.op_jump(Insn::JumpIfAbrupt));
                if let Some(izer) = izer {
                    let vok = chunk.op_jump(Insn::JumpIfNotUndef);
                    chunk.op(Insn::Pop);
                    let status = izer.compile(chunk, strict, text)?;
                    if status.maybe_ref() {
                        chunk.op(Insn::GetValue);
                    }
                    if status.maybe_abrupt() || status.maybe_ref() {
                        exits.push(chunk.op_jump(Insn::JumpIfAbrupt));
                    }
                    chunk.fixup(vok)?;
                }
                bp.compile_binding_initialization(chunk, strict, text, env)?;
                for mark in exits {
                    chunk.fixup(mark)?;
                }
                Ok(AlwaysAbruptResult)
            }
        }
    }

    pub fn iterator_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: IteratorBindingInitialization
        // The syntax-directed operation IteratorBindingInitialization takes arguments iteratorRecord (an Iterator
        // Record) and environment (an Environment Record or undefined) and returns either a normal completion
        // containing unused or an abrupt completion.
        //
        // (scole: switching this to returning a normal completion containing the iterator record or an abrupt
        // completion.)
        //
        // NOTE | When undefined is passed for environment it indicates that a PutValue operation should be used to
        //      | assign the initialization value. This is the case for formal parameter lists of non-strict functions.
        //      | In that case the formal parameter bindings are preinitialized in order to deal with the possibility of
        //      | multiple parameters with the same name.
        //
        // It is defined piecewise over the following productions:
        match self {
            BindingElement::Single(snb) => {
                // BindingElement : SingleNameBinding
                //  1. Return ? IteratorBindingInitialization of SingleNameBinding with arguments iteratorRecord and
                //     environment.
                snb.iterator_binding_initialization(chunk, strict, text, env)
            }
            BindingElement::Pattern(bp, izer) => {
                // BindingElement : BindingPattern Initializeropt
                //  1. Let v be undefined.
                //  2. If iteratorRecord.[[Done]] is false, then
                //      a. Let next be Completion(IteratorStep(iteratorRecord)).
                //      b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
                //      c. ReturnIfAbrupt(next).
                //      d. If next is false, set iteratorRecord.[[Done]] to true.
                //      e. Else,
                //          i. Set v to Completion(IteratorValue(next)).
                //          ii. If v is an abrupt completion, set iteratorRecord.[[Done]] to true.
                //          iii. ReturnIfAbrupt(v).
                //  3. If Initializer is present and v is undefined, then
                //      a. Let defaultValue be ? Evaluation of Initializer.
                //      b. Set v to ? GetValue(defaultValue).
                //  4. Return ? BindingInitialization of BindingPattern with arguments v and environment.

                // start:                        ir
                //   IR_STEP                     err or (v ir)
                //   JUMP_IF_ABRUPT exit         v ir
                // -- if initializer is present --
                //   JUMP_IF_NOT_UNDEF no_init   undef ir
                //   POP                         ir
                //   <izer>                      ref/err ir
                //   GET_VALUE                   v/err ir
                //   JUMP_IF_ABRUPT unwind       v ir
                // no_init:                      v ir
                // --
                //   <bp.bi(env)>                [empty]/err ir
                //   JUMP_IF_ABRUPT unwind       [empty] ir
                //   POP                         ir
                //   JUMP exit
                // unwind:                       err ir
                //   UNWIND 1                    err
                // exit:                         ir/err

                chunk.op(Insn::EmbellishedIteratorStep);
                let mut exits = vec![];
                let mut unwinds = vec![];
                exits.push(chunk.op_jump(Insn::JumpIfAbrupt));
                if let Some(izer) = izer {
                    let no_init = chunk.op_jump(Insn::JumpIfNotUndef);
                    chunk.op(Insn::Pop);
                    let status = izer.compile(chunk, strict, text)?;
                    if status.maybe_ref() {
                        chunk.op(Insn::GetValue);
                    }
                    if status.maybe_abrupt() || status.maybe_ref() {
                        unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
                    }
                    chunk.fixup(no_init)?;
                }
                bp.compile_binding_initialization(chunk, strict, text, env)?;
                unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
                chunk.op(Insn::Pop);
                exits.push(chunk.op_jump(Insn::Jump));
                for mark in unwinds {
                    chunk.fixup(mark)?;
                }
                chunk.op_plus_arg(Insn::Unwind, 1);
                for mark in exits {
                    chunk.fixup(mark)?;
                }
                Ok(AlwaysAbruptResult)
            }
        }
    }
}

impl SingleNameBinding {
    pub fn compile_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AbruptResult> {
        // Stack: N arg[n-1] ... arg[0]
        let (id, maybe_init) = {
            let SingleNameBinding::Id(id, maybe_init) = &self;
            (id, maybe_init.as_ref())
        };
        chunk.op(Insn::ExtractArg);
        // Stack: val N-1 arg[n-1] ... arg[1]
        let binding_id = id.string_value();
        let id_idx = chunk.add_to_string_pool(binding_id)?;
        chunk.op_plus_arg(Insn::String, id_idx);
        chunk.op(if strict { Insn::StrictResolve } else { Insn::Resolve });
        // Stack: ref val N-1 arg[n-1] ... arg[1]
        chunk.op(Insn::Swap);
        // Stack: val ref N-1 arg[n-1] ... arg[1]
        let mut exit = None;
        let mut no_init = None;
        if let Some(init) = maybe_init {
            no_init = Some(chunk.op_jump(Insn::JumpIfNotUndef));
            // Stack: undefined ref N-1 arg[n-1] ... arg[1]
            chunk.op(Insn::Pop);
            // Stack: ref N-1 arg[n-1] ... arg[1]
            let init_status = if let Some(np) = init.anonymous_function_definition() {
                np.compile_named_evaluation(chunk, strict, text, NameLoc::Index(id_idx))?
            } else {
                init.compile(chunk, strict, text)?
            };
            // Stack: ref/val/err ref N-1 arg[n-1] ... arg[1]
            if init_status.maybe_ref() {
                chunk.op(Insn::GetValue);
            }
            // Stack: val/err ref N-1 arg[n-1] ... arg[1]
            if init_status.maybe_abrupt() || init_status.maybe_ref() {
                let close = chunk.op_jump(Insn::JumpIfNormal);
                // Stack: err ref N-1 arg[n-1] ... arg[1]
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.op(Insn::UnwindList);
                // Stack: err
                exit = Some(chunk.op_jump(Insn::Jump));
                chunk.fixup(close).expect("Jump too close to overflow");
            }
            // Stack: val ref N-1 arg[n-1] ... arg[1]
        }
        if let Some(mark) = no_init {
            chunk.fixup(mark)?;
        }
        match env {
            EnvUsage::UsePutValue => chunk.op(Insn::PutValue),
            EnvUsage::UseCurrentLexical => chunk.op(Insn::InitializeReferencedBinding),
        }
        chunk.op(Insn::Pop);
        if let Some(&mark) = exit.as_ref() {
            chunk.fixup(mark).expect("jump too short to overflow");
        }
        // Stack: N-1 arg[n-1] ... arg[1] ...  --or-- err ...

        Ok(AbruptResult::from(exit.is_some()))
    }

    pub fn keyed_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: KeyedBindingInitialization
        // The syntax-directed operation KeyedBindingInitialization takes arguments value (an ECMAScript language
        // value), environment (an Environment Record or undefined), and propertyName (a property key) and returns
        // either a normal completion containing unused or an abrupt completion.
        //
        // NOTE | When undefined is passed for environment it indicates that a PutValue operation should be used to
        //      | assign the initialization value. This is the case for formal parameter lists of non-strict functions.
        //      | In that case the formal parameter bindings are preinitialized in order to deal with the possibility of
        //      | multiple parameters with the same name.
        //
        // It is defined piecewise over the following productions:
        match self {
            SingleNameBinding::Id(id, izer) => {
                // SingleNameBinding : BindingIdentifier Initializeropt
                //  1. Let bindingId be StringValue of BindingIdentifier.
                //  2. Let lhs be ? ResolveBinding(bindingId, environment).
                //  3. Let v be ? GetV(value, propertyName).
                //  4. If Initializer is present and v is undefined, then
                //      a. If IsAnonymousFunctionDefinition(Initializer) is true, then
                //          i. Set v to ? NamedEvaluation of Initializer with argument bindingId.
                //      b. Else,
                //          i. Let defaultValue be ? Evaluation of Initializer.
                //          ii. Set v to ? GetValue(defaultValue).
                //  5. If environment is undefined, return ? PutValue(lhs, v).
                //  6. Return ? InitializeReferencedBinding(lhs, v).

                // start:                         propertyName value
                //    STRING bindingId            bindingId propertyName value
                //    RESOLVE/STRICT_RESOLVE      lhs/err propertyName value
                //    JUMP_IF_ABRUPT unwind2      lhs propertyName value
                //    ROTATE_DOWN 3               propertyName value lhs
                //    GETV                        v/err lhs
                //    JUMP_IF_ABRUPT unwind1      v lhs
                //    --- initializer + anonymous
                //    JUMP_IF_NOT_UNDEFINED vok   undefined lhs
                //    POP                         lhs
                //    STRING bindingId            bindingId lhs
                //    <init.named_evaluation>     v/err lhs
                //    JUMP_IF_ABRUPT unwind1      v lhs
                //    ---
                //    --- initializer but not anonymous
                //    JUMP_IF_NOT_UNDEFINED vok   undefined lhs
                //    POP                         lhs
                //    <init>                      vRef/err lhs
                //    GET_VALUE                   v/err lhs
                //    JUMP_IF_ABRUPT unwind1      v lhs
                //    ---
                // vok:                           v lhs
                //    --- EnvUsage:PutValue
                //    PUT_VALUE                   [empty]/err
                //    ---
                //    --- EnvUsage::Current
                //    IRB                         [empty]/err
                //    ---
                //    JUMP exit
                // unwind2:
                //    UNWIND 1
                // unwind1:
                //    UNWIND 1
                // exit:
                let binding_id_val = id.string_value();
                let binding_id = chunk.add_to_string_pool(binding_id_val)?;
                chunk.op_plus_arg(Insn::String, binding_id);
                chunk.op(if strict { Insn::StrictResolve } else { Insn::Resolve });
                let unwind2 = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op_plus_arg(Insn::RotateDown, 3);
                chunk.op(Insn::GetV);
                let mut unwind1s = vec![];
                unwind1s.push(chunk.op_jump(Insn::JumpIfAbrupt));
                if let Some(izer) = izer {
                    let vok = chunk.op_jump(Insn::JumpIfNotUndef);
                    chunk.op(Insn::Pop);
                    let status = if let Some(np) = izer.anonymous_function_definition() {
                        np.compile_named_evaluation(chunk, strict, text, NameLoc::Index(binding_id))?
                    } else {
                        izer.compile(chunk, strict, text)?
                    };
                    if status.maybe_ref() {
                        chunk.op(Insn::GetValue);
                    }
                    if status.maybe_ref() || status.maybe_abrupt() {
                        unwind1s.push(chunk.op_jump(Insn::JumpIfAbrupt));
                    }
                    chunk.fixup(vok)?;
                }
                chunk.op(match env {
                    EnvUsage::UsePutValue => Insn::PutValue,
                    EnvUsage::UseCurrentLexical => Insn::InitializeReferencedBinding,
                });
                let exit = chunk.op_jump(Insn::Jump);
                chunk.fixup(unwind2)?;
                chunk.op_plus_arg(Insn::Unwind, 1);
                for mark in unwind1s {
                    chunk.fixup(mark).expect("jump shorter than unwind2, which was already successful");
                }
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit).expect("jump too short to fail");

                Ok(AlwaysAbruptResult)
            }
        }
    }

    pub fn iterator_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: IteratorBindingInitialization
        // The syntax-directed operation IteratorBindingInitialization takes arguments iteratorRecord (an Iterator
        // Record) and environment (an Environment Record or undefined) and returns either a normal completion
        // containing unused or an abrupt completion.
        //
        // (scole: switching this to returning a normal completion containing the iterator record or an abrupt
        // completion.)
        //
        // NOTE | When undefined is passed for environment it indicates that a PutValue operation should be used to
        //      | assign the initialization value. This is the case for formal parameter lists of non-strict functions.
        //      | In that case the formal parameter bindings are preinitialized in order to deal with the possibility of
        //      | multiple parameters with the same name.
        //
        // It is defined piecewise over the following productions:
        match self {
            SingleNameBinding::Id(bi, izer) => {
                // SingleNameBinding : BindingIdentifier Initializeropt
                //  1. Let bindingId be StringValue of BindingIdentifier.
                //  2. Let lhs be ? ResolveBinding(bindingId, environment).
                //  3. Let v be undefined.
                //  4. If iteratorRecord.[[Done]] is false, then
                //      a. Let next be Completion(IteratorStep(iteratorRecord)).
                //      b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
                //      c. ReturnIfAbrupt(next).
                //      d. If next is false, set iteratorRecord.[[Done]] to true.
                //      e. Else,
                //          i. Set v to Completion(IteratorValue(next)).
                //          ii. If v is an abrupt completion, set iteratorRecord.[[Done]] to true.
                //          iii. ReturnIfAbrupt(v).
                //  5. If Initializer is present and v is undefined, then
                //      a. If IsAnonymousFunctionDefinition(Initializer) is true, then
                //          i. Set v to ? NamedEvaluation of Initializer with argument bindingId.
                //      b. Else,
                //          i. Let defaultValue be ? Evaluation of Initializer.
                //          ii. Set v to ? GetValue(defaultValue).
                //  6. If environment is undefined, return ? PutValue(lhs, v).
                //  7. Return ? InitializeReferencedBinding(lhs, v).

                // start:                         ir
                //    STRING bindingId            bindingId ir
                //    RESOLVE/STRICT_RESOLVE      lhs/err ir
                //    JUMP_IF_ABRUPT unwind       lhs ir
                //    SWAP                        ir lhs
                //    ITER_STEP                   (err or (v ir)) lhs
                //    JUMP_IF_ABRUPT unwind       v ir lhs
                // --- if Initializer + anonymous
                //    JUMP_IF_NOT_UNDEF vok       undefined ir lhs
                //    POP                         ir lhs
                //    <init.named_evaluation>     v/err ir lhs
                //    JUMP_IF_ABRUPT unwind2      v ir lhs
                // -- else if initializer + not anonymous
                //    JUMP_IF_NOT_UNDEFINED vok   undefined ir lhs
                //    POP                         ir lhs
                //    <init>                      vRef/err ir lhs
                //    GET_VALUE                   v/err ir lhs
                //    JUMP_IF_ABRUPT unwind2      v ir lhs
                // --
                // vok:                           v ir lhs
                //    SWAP                        ir v lhs
                //    ROTATE_DOWN 3               v lhs ir
                //    --- EnvUsage:PutValue
                //    PUT_VALUE                   [empty]/err ir
                //    ---
                //    --- EnvUsage::Current
                //    IRB                         [empty]/err ir
                //    ---
                //    JUMP_IF_ABRUPT unwind1      [empty] ir
                //    POP                         ir
                //    JUMP exit
                // unwind2:                       err ir lhs
                //    UNWIND 1                    err lhs
                // unwind1:                       err lhs/ir
                //    UNWIND 1                    err
                // exit:                          ir/err

                let binding_id_val = bi.string_value();
                let binding_id = chunk.add_to_string_pool(binding_id_val)?;
                let mut unwind1 = vec![];
                let mut unwind2 = None;
                chunk.op_plus_arg(Insn::String, binding_id);
                chunk.op(if strict { Insn::StrictResolve } else { Insn::Resolve });
                unwind1.push(chunk.op_jump(Insn::JumpIfAbrupt));
                chunk.op(Insn::Swap);
                chunk.op(Insn::EmbellishedIteratorStep);
                unwind1.push(chunk.op_jump(Insn::JumpIfAbrupt));
                if let Some(izer) = izer {
                    let vok = chunk.op_jump(Insn::JumpIfNotUndef);
                    chunk.op(Insn::Pop);
                    let status = if let Some(np) = izer.anonymous_function_definition() {
                        np.compile_named_evaluation(chunk, strict, text, NameLoc::Index(binding_id))?
                    } else {
                        izer.compile(chunk, strict, text)?
                    };
                    if status.maybe_ref() {
                        chunk.op(Insn::GetValue);
                    }
                    if status.maybe_ref() || status.maybe_abrupt() {
                        unwind2 = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                    }
                    chunk.fixup(vok)?;
                }
                chunk.op(Insn::Swap);
                chunk.op_plus_arg(Insn::RotateDown, 3);
                chunk.op(match env {
                    EnvUsage::UsePutValue => Insn::PutValue,
                    EnvUsage::UseCurrentLexical => Insn::InitializeReferencedBinding,
                });
                unwind1.push(chunk.op_jump(Insn::JumpIfAbrupt));
                chunk.op(Insn::Pop);
                let exit = chunk.op_jump(Insn::Jump);
                if let Some(mark) = unwind2 {
                    chunk.fixup(mark).expect("Jump too short to fail");
                    chunk.op_plus_arg(Insn::Unwind, 1);
                }
                for mark in unwind1 {
                    chunk.fixup(mark)?;
                }
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit).expect("jump too short to fail");
                Ok(AlwaysAbruptResult)
            }
        }
    }
}

impl BindingPattern {
    pub fn compile_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // The syntax-directed operation BindingInitialization takes arguments value (an ECMAScript language value) and
        // environment (an Environment Record or undefined) and returns either a normal completion containing unused or
        // an abrupt completion.
        match self {
            BindingPattern::Object(obp) => {
                // BindingPattern : ObjectBindingPattern
                //  1. Perform ? RequireObjectCoercible(value).
                //  2. Return ? BindingInitialization of ObjectBindingPattern with arguments value and environment.

                // start:                   value
                //   REQ_COER               value/err
                //   JUMP_IF_ABRUPT exit    value
                //   <obp.bi(env)>          empty/err
                // exit:                    empty/err

                chunk.op(Insn::RequireCoercible);
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                obp.compile_binding_initialization(chunk, strict, text, env)?;
                chunk.fixup(exit)?;

                Ok(AlwaysAbruptResult)
            }
            BindingPattern::Array(abp) => {
                // BindingPattern : ArrayBindingPattern
                //  1. Let iteratorRecord be ? GetIterator(value, sync).
                //  2. Let result be Completion(IteratorBindingInitialization of ArrayBindingPattern with arguments
                //     iteratorRecord and environment).
                //  3. If iteratorRecord.[[Done]] is false, return ? IteratorClose(iteratorRecord, result).
                //  4. Return ? result.

                // start:                    value
                //    GET_SYNC_ITER          ir/err
                //    JUMP_IF_ABRUPT exit    ir
                //    DUP                    ir ir
                //    <abp.ibi(env)>         ir/err ir
                //    IR_CLOSE_IF_NOT_DONE   [empty]/err   (steps 3 & 4, above)
                // exit:                     [empty]/err

                chunk.op(Insn::GetSyncIterator);
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Dup);
                abp.iterator_binding_initialization(chunk, strict, text, env)?;
                chunk.op(Insn::IteratorCloseIfNotDone);
                chunk.fixup(exit)?;

                Ok(AlwaysAbruptResult)
            }
        }
    }
}

impl ObjectBindingPattern {
    pub fn compile_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AbruptResult> {
        // Runtime Semantics: BindingInitialization
        // The syntax-directed operation BindingInitialization takes arguments value (an ECMAScript language value) and
        // environment (an Environment Record or undefined) and returns either a normal completion containing unused or
        // an abrupt completion.
        //
        // NOTE | undefined is passed for environment to indicate that a PutValue operation should be used to assign the
        //      | initialization value. This is the case for var statements and formal parameter lists of some
        //      | non-strict functions (See 10.2.11). In those cases a lexical binding is hoisted and preinitialized
        //      | prior to evaluation of its initializer.
        //
        // It is defined piecewise over the following productions:
        match self {
            ObjectBindingPattern::Empty { .. } => {
                // ObjectBindingPattern : { }
                //  1. Return unused.

                // start:           value
                //   POP
                //   EMPTY          empty
                chunk.op(Insn::Pop);
                chunk.op(Insn::Empty);
                Ok(AbruptResult::Never)
            }
            ObjectBindingPattern::ListOnly { bpl, .. } | ObjectBindingPattern::ListRest { bpl, brp: None, .. } => {
                // ObjectBindingPattern :
                //      { BindingPropertyList }
                //      { BindingPropertyList , }
                //  1. Perform ? PropertyBindingInitialization of BindingPropertyList with arguments value and
                //     environment.
                //  2. Return unused.

                // start:                                             value
                //    <bpl.property_binding_initialization(env)>      list/err
                //    JUMP_IF_ABRUPT exit                             list
                //    POP_LIST
                //    EMPTY                                           [empty]
                // exit:

                let status = bpl.property_binding_initialization(chunk, strict, text, env)?;
                let exit = if status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                chunk.op(Insn::PopList);
                chunk.op(Insn::Empty);
                if let Some(mark) = exit {
                    chunk.fixup(mark).expect("Jump should be to short to fail");
                }
                Ok(status)
            }
            ObjectBindingPattern::RestOnly { brp, .. } => {
                // ObjectBindingPattern : { BindingRestProperty }
                //  1. Let excludedNames be a new empty List.
                //  2. Return ? RestBindingInitialization of BindingRestProperty with arguments value, environment, and
                //     excludedNames.

                // start:                                        value
                //   ZERO                                        excludedNames value
                //   brp.rest_binding_initialization(env)        [empty]/err
                chunk.op(Insn::Zero);
                let status = brp.rest_binding_initialization(chunk, strict, text, env)?;
                Ok(status)
            }
            ObjectBindingPattern::ListRest { bpl, brp: Some(brp), .. } => {
                // ObjectBindingPattern : { BindingPropertyList , BindingRestProperty }
                //  1. Let excludedNames be ? PropertyBindingInitialization of BindingPropertyList with arguments value
                //     and environment.
                //  2. Return ? RestBindingInitialization of BindingRestProperty with arguments value, environment, and
                //     excludedNames.

                // start:                                           value
                //   DUP                                            value value
                //   <bpl.property_binding_initialization(env)>     excludedNames/err value
                //   JUMP_IF_ABRUPT unwind                          excludedNames value
                //   <brp.rest_binding_initialization(env)          [empty]/err
                //   JUMP exit
                // unwind:                                          err value
                //   UNWIND 1                                       err
                // exit:                                            [empty]/err
                chunk.op(Insn::Dup);
                let bpl_status = bpl.property_binding_initialization(chunk, strict, text, env)?;
                let unwind = if bpl_status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                let brp_status = brp.rest_binding_initialization(chunk, strict, text, env)?;
                if let Some(mark) = unwind {
                    let exit = chunk.op_jump(Insn::Jump);
                    chunk.fixup(mark)?;
                    chunk.op_plus_arg(Insn::Unwind, 1);
                    chunk.fixup(exit).expect("Jump too short to fail");
                }
                Ok((bpl_status.maybe_abrupt() || brp_status.maybe_abrupt()).into())
            }
        }
    }
}

impl BindingPropertyList {
    pub fn property_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AbruptResult> {
        // Runtime Semantics: PropertyBindingInitialization
        // The syntax-directed operation PropertyBindingInitialization takes arguments value (an ECMAScript language
        // value) and environment (an Environment Record or undefined) and returns either a normal completion containing
        // a List of property keys or an abrupt completion. It collects a list of all bound property names. It is
        // defined piecewise over the following productions:
        match self {
            BindingPropertyList::Item(bp) => {
                // BindingPropertyList : BindingProperty
                //  1. Return ? PropertyBindingInitialization of BindingProperty with arguments value and environment.
                bp.property_binding_initialization(chunk, strict, text, env).map(AbruptResult::from)
            }
            BindingPropertyList::List(bpl, bp) => {
                // BindingPropertyList : BindingPropertyList , BindingProperty
                //  1. Let boundNames be ? PropertyBindingInitialization of BindingPropertyList with arguments value and
                //     environment.
                //  2. Let nextNames be ? PropertyBindingInitialization of BindingProperty with arguments value and
                //     environment.
                //  3. Return the list-concatenation of boundNames and nextNames.

                // start:                                        value
                //   DUP                                         value value
                //   <bpl.property_binding_initialization(env)>  boundNames/err value
                //   JUMP_IF_ABRUPT unwind_value                 boundNames value
                //   SWAP_LIST                                   value boundNames
                //   <bp.property_binding_initialization(env)>   nextNames/err boundNames
                //   JUMP_IF_ABRUPT unwind_list                  nextNames boundNames
                //   APPEND_LIST                                 allNames
                //   JUMP exit
                // unwind_value:
                //   UNWIND 1
                //   JUMP exit
                // unwind_list:
                //   UNWIND_LIST
                // exit:

                chunk.op(Insn::Dup);
                let bpl_status = bpl.property_binding_initialization(chunk, strict, text, env)?;
                let unwind_value =
                    if bpl_status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                chunk.op(Insn::SwapList);
                let bp_status = bp.property_binding_initialization(chunk, strict, text, env)?;
                let unwind_list = if bp_status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                chunk.op(Insn::AppendList);
                if unwind_value.is_some() || unwind_list.is_some() {
                    let mut exits = vec![];
                    exits.push(chunk.op_jump(Insn::Jump));
                    if let Some(mark) = unwind_value {
                        chunk.fixup(mark)?;
                        chunk.op_plus_arg(Insn::Unwind, 1);
                        if unwind_list.is_some() {
                            exits.push(chunk.op_jump(Insn::Jump));
                        }
                    }
                    if let Some(mark) = unwind_list {
                        chunk.fixup(mark)?;
                        chunk.op(Insn::UnwindList);
                    }
                    for mark in exits {
                        chunk.fixup(mark).expect("jumps too short to overflow");
                    }
                }

                Ok((bp_status.maybe_abrupt() || bpl_status.maybe_abrupt()).into())
            }
        }
    }
}

impl BindingProperty {
    pub fn property_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: PropertyBindingInitialization
        // The syntax-directed operation PropertyBindingInitialization takes arguments value (an ECMAScript language
        // value) and environment (an Environment Record or undefined) and returns either a normal completion containing
        // a List of property keys or an abrupt completion. It collects a list of all bound property names. It is
        // defined piecewise over the following productions:
        match self {
            BindingProperty::Single(snb) => {
                // BindingProperty : SingleNameBinding
                //  1. Let name be the sole element of the BoundNames of SingleNameBinding.
                //  2. Perform ? KeyedBindingInitialization of SingleNameBinding with arguments value, environment, and
                //     name.
                //  3. Return « name ».

                // start:                    value
                //    STRING name            name value
                //    <snb.keyed_binding_initialization(env)>    [empty]/err
                //    JUMP_IF_ABRUPT exit    [empty]
                //    POP
                //    STRING name            name
                //    FLOAT 1                1 name (i.e.: the output list)
                // exit:
                let name_val = snb.bound_names().pop().expect("snb should have exactly one bound name");
                let name = chunk.add_to_string_pool(name_val)?;
                let one = chunk.add_to_float_pool(1.0)?;
                chunk.op_plus_arg(Insn::String, name);
                snb.keyed_binding_initialization(chunk, strict, text, env)?;
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Pop);
                chunk.op_plus_arg(Insn::String, name);
                chunk.op_plus_arg(Insn::Float, one);
                chunk.fixup(exit).expect("jump too short to fail");

                Ok(AlwaysAbruptResult)
            }
            BindingProperty::Property(pn, be) => {
                // BindingProperty : PropertyName : BindingElement
                //  1. Let P be ? Evaluation of PropertyName.
                //  2. Perform ? KeyedBindingInitialization of BindingElement with arguments value, environment, and P.
                //  3. Return « P ».

                // start:                                     value
                //   <pn>                                     P/err value
                //   JUMP_IF_ABRUPT unwind                    P value
                //   POP2PUSH3                                P value P
                //   <be.keyed_binding_initialization(env)    [empty]/err P
                //   JUMP_IF_ABRUPT unwind                    [empty] P
                //   POP                                      P
                //   FLOAT 1                                  1 P (i.e.: the output list)
                //   JUMP exit
                // unwind:
                //   UNWIND 1
                // exit:
                let pn_status = pn.compile(chunk, strict, text)?;
                let mut unwinds = vec![];
                if pn_status.maybe_abrupt() {
                    unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                chunk.op(Insn::Pop2Push3);
                be.keyed_binding_initialization(chunk, strict, text, env)?;
                unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
                chunk.op(Insn::Pop);
                let one = chunk.add_to_float_pool(1.0)?;
                chunk.op_plus_arg(Insn::Float, one);
                let exit = chunk.op_jump(Insn::Jump);
                for mark in unwinds {
                    chunk.fixup(mark)?;
                }
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit).expect("jump too short to fail");
                Ok(AlwaysAbruptResult)
            }
        }
    }
}

impl BindingRestProperty {
    pub fn rest_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        _text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AbruptResult> {
        // Runtime Semantics: RestBindingInitialization
        // The syntax-directed operation RestBindingInitialization takes arguments value (an ECMAScript language value),
        // environment (an Environment Record or undefined), and excludedNames (a List of property keys) and returns
        // either a normal completion containing unused or an abrupt completion. It is defined piecewise over the
        // following productions:
        match self {
            BindingRestProperty::Id(bi) => {
                // BindingRestProperty : ... BindingIdentifier
                //  1. Let lhs be ? ResolveBinding(StringValue of BindingIdentifier, environment).
                //  2. Let restObj be OrdinaryObjectCreate(%Object.prototype%).
                //  3. Perform ? CopyDataProperties(restObj, value, excludedNames).
                //  4. If environment is undefined, return ? PutValue(lhs, restObj).
                //  5. Return ? InitializeReferencedBinding(lhs, restObj).

                // start:                                excludedNames value
                //   STRING bindingId                    bindingId excludedNames value
                //   RESOLVE/STRICT_RESOLVE              lhs/err excludedNames value
                //   JUMP_IF_ABRUPT unwind_list_plus_1   lhs excludedNames value
                //   ROTATE_DNLIST 1                     excludedNames value lhs
                //   OBJECT                              restObj excludedNames value lhs
                //   ROTATE_DNLIST 1                     excludedNames value restObj lhs
                //   COPY_DATAPROPS_WE                   restObj/err lhs
                //   JUMP_IF_ABRUPT unwind_1
                //   PUT_VALUE/IRB                       [empty]/err
                //   JUMP exit
                // unwind_list_plus_1:
                //   UNWIND_LIST
                // unwind_1:
                //   UNWIND 1
                // exit:
                let binding_id_val = bi.string_value();
                let binding_id = chunk.add_to_string_pool(binding_id_val)?;
                chunk.op_plus_arg(Insn::String, binding_id);
                chunk.op(if strict { Insn::StrictResolve } else { Insn::Resolve });
                let unwind_list_plus_1 = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op_plus_arg(Insn::RotateDownList, 1);
                chunk.op(Insn::Object);
                chunk.op_plus_arg(Insn::RotateDownList, 1);
                chunk.op(Insn::CopyDataPropsWithExclusions);
                let unwind_1 = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(match env {
                    EnvUsage::UsePutValue => Insn::PutValue,
                    EnvUsage::UseCurrentLexical => Insn::InitializeReferencedBinding,
                });
                let exit = chunk.op_jump(Insn::Jump);
                chunk.fixup(unwind_list_plus_1).expect("jump too short to fail");
                chunk.op(Insn::UnwindList);
                chunk.fixup(unwind_1).expect("Jump too short to fail");
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit).expect("jump too short to fail");

                Ok(AbruptResult::Maybe)
            }
        }
    }
}

impl ArrayBindingPattern {
    pub fn iterator_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AbruptResult> {
        // Runtime Semantics: IteratorBindingInitialization
        // The syntax-directed operation IteratorBindingInitialization takes arguments iteratorRecord (an Iterator
        // Record) and environment (an Environment Record or undefined) and returns either a normal completion
        // containing unused or an abrupt completion.
        //
        // (scole: switching this to returning a normal completion containing the iterator record or an abrupt
        // completion.)
        //
        // NOTE | When undefined is passed for environment it indicates that a PutValue operation should be used to
        //      | assign the initialization value. This is the case for formal parameter lists of non-strict functions.
        //      | In that case the formal parameter bindings are preinitialized in order to deal with the possibility of
        //      | multiple parameters with the same name.
        //
        // It is defined piecewise over the following productions:
        match self {
            ArrayBindingPattern::RestOnly { elision: None, bre: None, .. } => {
                // ArrayBindingPattern : [ ]
                //  1. Return unused.

                // start:            ir

                Ok(AbruptResult::Never)
            }
            ArrayBindingPattern::RestOnly { elision: Some(elision), bre: None, .. } => {
                // ArrayBindingPattern : [ Elision ]
                //  1. Return ? IteratorDestructuringAssignmentEvaluation of Elision with argument iteratorRecord.

                // start:             ir
                //   <elision.idae>   ir/err
                elision.iterator_destructuring_assignment_evaluation(chunk).map(AbruptResult::from)
            }
            ArrayBindingPattern::RestOnly { elision, bre: Some(bre), .. } => {
                // ArrayBindingPattern : [ Elisionopt BindingRestElement ]
                //  1. If Elision is present, then
                //      a. Perform ? IteratorDestructuringAssignmentEvaluation of Elision with argument iteratorRecord.
                //  2. Return ? IteratorBindingInitialization of BindingRestElement with arguments iteratorRecord and
                //     environment.

                // start:                   ir
                // --- if elision
                //   <elision.idae>         ir/err
                //   JUMP_IF_ABRUPT exit    ir
                // ---
                //   <bre.ibi(env)>         ir/err
                // exit:

                let exit = if let Some(elision) = elision {
                    let status = elision.iterator_destructuring_assignment_evaluation(chunk)?;
                    assert!(status.maybe_abrupt());
                    Some(chunk.op_jump(Insn::JumpIfAbrupt))
                } else {
                    None
                };
                bre.iterator_binding_initialization(chunk, strict, text, env)?;
                if let Some(exit) = exit {
                    chunk.fixup(exit)?;
                }
                Ok(AbruptResult::Maybe)
            }
            ArrayBindingPattern::ListOnly { bel, .. }
            | ArrayBindingPattern::ListRest { bel, elision: None, bre: None, .. } => {
                // ArrayBindingPattern : [ BindingElementList ]
                // ArrayBindingPattern : [ BindingElementList , ]
                //  1. Return ? IteratorBindingInitialization of BindingElementList with arguments iteratorRecord and
                //     environment.
                bel.iterator_binding_initialization(chunk, strict, text, env).map(AbruptResult::from)
            }
            ArrayBindingPattern::ListRest { bel, elision: Some(elision), bre: None, .. } => {
                // ArrayBindingPattern : [ BindingElementList , Elision ]
                //  1. Perform ? IteratorBindingInitialization of BindingElementList with arguments iteratorRecord and
                //     environment.
                //  2. Return ? IteratorDestructuringAssignmentEvaluation of Elision with argument iteratorRecord.

                // start:                     ir
                //   <bel.ibi(env)>           ir/err
                //   JUMP_IF_ABRUPT exit      ir
                //   <elision.idae>           ir/err
                // exit:

                let status = bel.iterator_binding_initialization(chunk, strict, text, env)?;
                assert!(status.maybe_abrupt());
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                let status = elision.iterator_destructuring_assignment_evaluation(chunk)?;
                assert!(status.maybe_abrupt());
                chunk.fixup(exit)?;
                Ok(AbruptResult::Maybe)
            }
            ArrayBindingPattern::ListRest { bel, elision, bre: Some(bre), .. } => {
                // ArrayBindingPattern : [ BindingElementList , Elisionopt BindingRestElement ]
                //  1. Perform ? IteratorBindingInitialization of BindingElementList with arguments iteratorRecord and
                //     environment.
                //  2. If Elision is present, then
                //      a. Perform ? IteratorDestructuringAssignmentEvaluation of Elision with argument iteratorRecord.
                //  3. Return ? IteratorBindingInitialization of BindingRestElement with arguments iteratorRecord and
                //     environment.

                // start:                     ir
                //   <bel.ibi(env)>           ir/error
                //   JUMP_IF_ABRUPT exit      ir
                // -- elision present --
                //   <elision.idae>           ir/error
                //   JUMP_IF_ABRUPT exit      ir
                // --
                //   <bre.ibi(env)>           ir/error
                // exit:

                let status = bel.iterator_binding_initialization(chunk, strict, text, env)?;
                assert!(status.maybe_abrupt());
                let mut exits = vec![];
                exits.push(chunk.op_jump(Insn::JumpIfAbrupt));
                if let Some(elisions) = elision {
                    let status = elisions.iterator_destructuring_assignment_evaluation(chunk)?;
                    assert!(status.maybe_abrupt());
                    exits.push(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                let status = bre.iterator_binding_initialization(chunk, strict, text, env)?;
                assert!(status.maybe_abrupt());
                for mark in exits {
                    chunk.fixup(mark)?;
                }
                Ok(AbruptResult::Maybe)
            }
        }
    }
}

impl BindingElementList {
    pub fn iterator_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: IteratorBindingInitialization
        // The syntax-directed operation IteratorBindingInitialization takes arguments iteratorRecord (an Iterator
        // Record) and environment (an Environment Record or undefined) and returns either a normal completion
        // containing unused or an abrupt completion.
        //
        // (scole: switching this to returning a normal completion containing the iterator record or an abrupt
        // completion.)
        //
        // NOTE | When undefined is passed for environment it indicates that a PutValue operation should be used to
        //      | assign the initialization value. This is the case for formal parameter lists of non-strict functions.
        //      | In that case the formal parameter bindings are preinitialized in order to deal with the possibility of
        //      | multiple parameters with the same name.
        //
        // It is defined piecewise over the following productions:
        match self {
            BindingElementList::Item(bee) => {
                // BindingElementList : BindingElisionElement
                //  1. Return ? IteratorBindingInitialization of BindingElisionElement with arguments iteratorRecord and
                //     environment.
                bee.iterator_binding_initialization(chunk, strict, text, env)
            }
            BindingElementList::List(bel, bee) => {
                // BindingElementList : BindingElementList , BindingElisionElement
                //  1. Perform ? IteratorBindingInitialization of BindingElementList with arguments iteratorRecord and
                //     environment.
                //  2. Return ? IteratorBindingInitialization of BindingElisionElement with arguments iteratorRecord and
                //     environment.

                // start:                     ir
                //   <bel.ibi(env)>           ir/err
                //   JUMP_IF_ABRUPT exit      ir
                //   <bee.ibi(env)>           ir/err
                // exit:

                let status = bel.iterator_binding_initialization(chunk, strict, text, env)?;
                assert!(status.maybe_abrupt());
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                let status = bee.iterator_binding_initialization(chunk, strict, text, env)?;
                assert!(status.maybe_abrupt());
                chunk.fixup(exit)?;
                Ok(AlwaysAbruptResult)
            }
        }
    }
}

impl BindingElisionElement {
    pub fn iterator_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: IteratorBindingInitialization
        // The syntax-directed operation IteratorBindingInitialization takes arguments iteratorRecord (an Iterator
        // Record) and environment (an Environment Record or undefined) and returns either a normal completion
        // containing unused or an abrupt completion.
        //
        // (scole: switching this to returning a normal completion containing the iterator record or an abrupt
        // completion.)
        //
        // NOTE | When undefined is passed for environment it indicates that a PutValue operation should be used to
        //      | assign the initialization value. This is the case for formal parameter lists of non-strict functions.
        //      | In that case the formal parameter bindings are preinitialized in order to deal with the possibility of
        //      | multiple parameters with the same name.
        //
        // It is defined piecewise over the following productions:
        match self {
            BindingElisionElement::Element(None, be) => {
                // BindingElisionElement : BindingElement
                //  1. Return ? IteratorBindingInitialization of BindingElement with arguments iteratorRecord and
                //     environment.
                be.iterator_binding_initialization(chunk, strict, text, env)
            }
            BindingElisionElement::Element(Some(elision), be) => {
                // BindingElisionElement : Elision BindingElement
                //  1. Perform ? IteratorDestructuringAssignmentEvaluation of Elision with argument iteratorRecord.
                //  2. Return ? IteratorBindingInitialization of BindingElement with arguments iteratorRecord and
                //     environment.

                // start:                     ir
                //   <elision.idae>           ir/err
                //   JUMP_IF_ABRUPT exit      ir
                //   <be.ibi(env)>            ir/err
                // exit:

                elision.iterator_destructuring_assignment_evaluation(chunk)?;
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                let status = be.iterator_binding_initialization(chunk, strict, text, env)?;
                assert!(status.maybe_abrupt());
                chunk.fixup(exit)?;

                Ok(AlwaysAbruptResult)
            }
        }
    }
}

impl BindingRestElement {
    pub fn iterator_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: IteratorBindingInitialization
        // The syntax-directed operation IteratorBindingInitialization takes arguments iteratorRecord (an Iterator
        // Record) and environment (an Environment Record or undefined) and returns either a normal completion
        // containing unused or an abrupt completion.
        //
        // (scole: switching this to returning a normal completion containing the iterator record or an abrupt
        // completion.)
        //
        // NOTE | When undefined is passed for environment it indicates that a PutValue operation should be used to
        //      | assign the initialization value. This is the case for formal parameter lists of non-strict functions.
        //      | In that case the formal parameter bindings are preinitialized in order to deal with the possibility of
        //      | multiple parameters with the same name.
        //
        // It is defined piecewise over the following productions:
        match self {
            BindingRestElement::Identifier(bi, _) => {
                // BindingRestElement : ... BindingIdentifier
                //  1. Let lhs be ? ResolveBinding(StringValue of BindingIdentifier, environment).
                //  2. Let A be ! ArrayCreate(0).
                //  3. Let n be 0.
                //  4. Repeat,
                //      a. If iteratorRecord.[[Done]] is false, then
                //          i. Let next be Completion(IteratorStep(iteratorRecord)).
                //          ii. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
                //          iii. ReturnIfAbrupt(next).
                //          iv. If next is false, set iteratorRecord.[[Done]] to true.
                //      b. If iteratorRecord.[[Done]] is true, then
                //          i. If environment is undefined, return ? PutValue(lhs, A).
                //          ii. Return ? InitializeReferencedBinding(lhs, A).
                //      c. Let nextValue be Completion(IteratorValue(next)).
                //      d. If nextValue is an abrupt completion, set iteratorRecord.[[Done]] to true.
                //      e. ReturnIfAbrupt(nextValue).
                //      f. Perform ! CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), nextValue).
                //      g. Set n to n + 1.

                // start                                 ir
                //   STRING bindingId                    bindingId ir
                //   STRICT_RESOLVE/RESOLVE              lhs/err ir
                //   JUMP_IF_ABRUPT unwind               lhs ir
                //   SWAP                                ir lhs
                //   ITER_REST                           (A ir)/err lhs
                //   JUMP_IF_ABRUPT unwind               A ir lhs
                //   SWAP                                ir A lhs
                //   ROTATE_DOWN 3                       A lhs ir
                //   PUT_VALUE/IRB                       [empty]/err ir
                //   JUMP_IF_ABRUPT unwind               [empty] ir
                //   POP                                 ir
                //   JUMP exit
                // unwind:                               err ir
                //   UNWIND 1                            err
                // exit:                                 ir/err

                let binding_id_val = bi.string_value();
                let binding_id = chunk.add_to_string_pool(binding_id_val)?;
                chunk.op_plus_arg(Insn::String, binding_id);
                chunk.op(if strict { Insn::StrictResolve } else { Insn::Resolve });
                let mut unwinds = vec![];
                unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
                chunk.op(Insn::Swap);
                chunk.op(Insn::IteratorRest);
                unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
                chunk.op(Insn::Swap);
                chunk.op_plus_arg(Insn::RotateDown, 3);
                chunk.op(match env {
                    EnvUsage::UsePutValue => Insn::PutValue,
                    EnvUsage::UseCurrentLexical => Insn::InitializeReferencedBinding,
                });
                unwinds.push(chunk.op_jump(Insn::JumpIfAbrupt));
                chunk.op(Insn::Pop);
                let exit = chunk.op_jump(Insn::Jump);
                for mark in unwinds {
                    chunk.fixup(mark).expect("Jumps too short to fail");
                }
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit).expect("jump too short to fail");

                Ok(AlwaysAbruptResult)
            }
            BindingRestElement::Pattern(bp, _) => {
                // BindingRestElement : ... BindingPattern
                //  1. Let A be ! ArrayCreate(0).
                //  2. Let n be 0.
                //  3. Repeat,
                //      a. If iteratorRecord.[[Done]] is false, then
                //          i. Let next be Completion(IteratorStep(iteratorRecord)).
                //          ii. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
                //          iii. ReturnIfAbrupt(next).
                //          iv. If next is false, set iteratorRecord.[[Done]] to true.
                //      b. If iteratorRecord.[[Done]] is true, then
                //          i. Return ? BindingInitialization of BindingPattern with arguments A and environment.
                //      c. Let nextValue be Completion(IteratorValue(next)).
                //      d. If nextValue is an abrupt completion, set iteratorRecord.[[Done]] to true.
                //      e. ReturnIfAbrupt(nextValue).
                //      f. Perform ! CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), nextValue).
                //      g. Set n to n + 1.

                // start                                 ir
                //   ITER_REST                           (A ir)/err
                //   JUMP_IF_ABRUPT exit                 A ir
                //   <bp.binding_initialization(env)>    [empty]/err ir
                //   JUMP_IF_ABRUPT unwind               [empty] ir
                //   POP                                 ir
                //   JUMP exit
                // unwind:                               err ir
                //   UNWIND 1                            err
                // exit:                                 ir/err

                chunk.op(Insn::IteratorRest);
                let mut exits = vec![];
                exits.push(chunk.op_jump(Insn::JumpIfAbrupt));
                let status = bp.compile_binding_initialization(chunk, strict, text, env)?;
                let unwind = if status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                chunk.op(Insn::Pop);
                if let Some(mark) = unwind {
                    exits.push(chunk.op_jump(Insn::Jump));
                    chunk.fixup(mark).expect("jump too short to fail");
                    chunk.op_plus_arg(Insn::Unwind, 1);
                }
                for mark in exits {
                    chunk.fixup(mark)?;
                }
                Ok(AlwaysAbruptResult)
            }
        }
    }
}

impl FunctionRestParameter {
    #[allow(unused_variables)]
    pub fn compile_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AbruptResult> {
        todo!()
    }
}

impl FunctionBody {
    pub fn compile_body(
        &self,
        chunk: &mut Chunk,
        text: &str,
        info: &StashedFunctionData,
    ) -> anyhow::Result<AbruptResult> {
        // Runtime Semantics: EvaluateBody
        //
        // The syntax-directed operation EvaluateBody takes arguments functionObject and argumentsList (a List) and
        // returns either a normal completion containing an ECMAScript language value or an abrupt completion. It is
        // defined piecewise over the following productions:
        //
        // FunctionBody : FunctionStatementList
        //  1. Return ? EvaluateFunctionBody of FunctionBody with arguments functionObject and argumentsList.

        // Runtime Semantics: EvaluateFunctionBody
        //
        // The syntax-directed operation EvaluateFunctionBody takes arguments functionObject and argumentsList (a List)
        // and returns either a normal completion containing an ECMAScript language value or an abrupt completion. It
        // is defined piecewise over the following productions:
        //
        // FunctionBody : FunctionStatementList
        //  1. Perform ? FunctionDeclarationInstantiation(functionObject, argumentsList).
        //  2. Return the result of evaluating FunctionStatementList.

        // Both steps 1 and 2 have compilable code.

        // Stack: N arg[n-1] arg[n-2] ... arg[1] arg[0] func
        let fdi_status = compile_fdi(chunk, text, info)?;
        let exit = if fdi_status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };

        // Stack: func ...
        let strict = info.strict || self.function_body_contains_use_strict();
        let eval_status = self.statements.compile(chunk, strict, text)?;
        // Stack: result func ...

        if let Some(mark) = exit {
            chunk.fixup(mark)?;
        }

        chunk.op(Insn::EndFunction);

        Ok(AbruptResult::from(fdi_status.maybe_abrupt() || eval_status.maybe_abrupt()))
    }
}

impl FunctionStatementList {
    fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match self {
            FunctionStatementList::Statements(s) => s.compile(chunk, strict, text),
            FunctionStatementList::Empty(_) => {
                chunk.op(Insn::Undefined);
                Ok(NeverAbruptRefResult.into())
            }
        }
    }
}

#[cfg(test)]
mod tests;
