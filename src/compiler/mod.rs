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
    EmptyIfNotError,
    Float,
    Bigint,
    GetValue,
    PutValue,
    FunctionPrototype,
    ObjectPrototype,
    Jump,
    JumpIfAbrupt,
    JumpIfNormal,
    JumpIfFalse,
    JumpIfTrue,
    JumpIfNotNullish,
    JumpIfNullish,
    JumpPopIfTrue,
    JumpPopIfFalse,
    JumpIfNotUndef,
    JumpNotThrow,
    Call,
    StrictCall,
    EndFunction,
    Return,
    UpdateEmpty,
    Swap,
    Pop,
    PopOrPanic,
    Pop2Push3,
    Dup,
    DupAfterList,
    RotateUp,
    RotateDown,
    RotateDownList,
    RotateListDown,
    RotateListUp,
    Unwind,
    UnwindList,
    UnwindIfAbrupt,
    AppendList,
    SwapDeepList,
    PopOutList,
    ListToArray,
    Ref,
    StrictRef,
    InitializeReferencedBinding,
    PushNewLexEnv,
    PopLexEnv,
    PushNewVarEnvFromLex,
    PushNewLexEnvFromVar,
    SetLexEnvToVarEnv,
    SetAsideLexEnv,
    RestoreLexEnv,
    PushNewPrivateEnv,
    PopPrivateEnv,
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
    CreatePrivateNameIfMissing,
    CreatePerIterationEnvironment,
    Object,
    ObjectWithProto,
    Array,
    CreateDataProperty,
    SetPrototype,
    ToPropertyKey,
    CopyDataProps,
    CopyDataPropsWithExclusions,
    ToString,
    ToNumeric,
    ToObject,
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
    InstantiateGeneratorFunctionExpression,
    InstantiateOrdinaryFunctionObject,
    InstantiateGeneratorFunctionObject,
    InstantiateGeneratorMethod,
    GeneratorStartFromFunction,
    Yield,
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
    IteratorClose,
    IteratorCloseIfNotDone,
    IteratorNext,
    IteratorResultComplete,
    IteratorResultToValue,
    GetV,
    IteratorDAEElision,
    EmbellishedIteratorStep,
    IteratorRest,
    EnumerateObjectProperties,
    PrivateIdLookup,
    EvaluateInitializedClassFieldDefinition,
    EvaluateInitializedClassStaticFieldDefinition,
    EvaluateClassStaticBlockDefinition,
    DefineMethod,
    SetFunctionName,
    DefineMethodProperty,
    DefineGetter,
    DefineSetter,
    GetParentsFromSuperclass,
    CreateDefaultConstructor,
    MakeClassConstructorAndSetName,
    MakeConstructor,
    MakeConstructorWithProto,
    SetDerived,
    AttachElements,
    AttachSourceText,
    NameOnlyFieldRecord,
    NameOnlyStaticFieldRecord,
    MakePrivateReference,
    GetNewTarget,
    GetSuperConstructor,
    ConstructorCheck,
    BindThisAndInit,
    StaticClassItem,
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
            Insn::EmptyIfNotError => "EMPTY_IF_NOT_ERR",
            Insn::Float => "FLOAT",
            Insn::Bigint => "BIGINT",
            Insn::GetValue => "GET_VALUE",
            Insn::PutValue => "PUT_VALUE",
            Insn::FunctionPrototype => "FUNC_PROTO",
            Insn::ObjectPrototype => "OBJ_PROTO",
            Insn::Jump => "JUMP",
            Insn::JumpIfAbrupt => "JUMP_IF_ABRUPT",
            Insn::JumpIfNormal => "JUMP_IF_NORMAL",
            Insn::JumpIfFalse => "JUMP_IF_FALSE",
            Insn::JumpIfTrue => "JUMP_IF_TRUE",
            Insn::JumpIfNotNullish => "JUMP_NOT_NULLISH",
            Insn::JumpIfNullish => "JUMP_NULLISH",
            Insn::JumpPopIfTrue => "JUMPPOP_TRUE",
            Insn::JumpPopIfFalse => "JUMPPOP_FALSE",
            Insn::JumpIfNotUndef => "JUMP_NOT_UNDEF",
            Insn::JumpNotThrow => "JUMP_NOT_THROW",
            Insn::Call => "CALL",
            Insn::StrictCall => "CALL_STRICT",
            Insn::EndFunction => "END_FUNCTION",
            Insn::Return => "RETURN",
            Insn::UpdateEmpty => "UPDATE_EMPTY",
            Insn::Swap => "SWAP",
            Insn::Pop => "POP",
            Insn::PopOrPanic => "POP_PANIC",
            Insn::Pop2Push3 => "POP2_PUSH3",
            Insn::Dup => "DUP",
            Insn::DupAfterList => "DUP_AFTER_LIST",
            Insn::RotateUp => "ROTATEUP",
            Insn::RotateDown => "ROTATEDOWN",
            Insn::RotateDownList => "ROTATEDOWN_LIST",
            Insn::RotateListDown => "ROTATE_LIST_DOWN",
            Insn::RotateListUp => "ROTATE_LIST_UP",
            Insn::Unwind => "UNWIND",
            Insn::UnwindList => "UNWIND_LIST",
            Insn::UnwindIfAbrupt => "UNWIND_IF_ABRUPT",
            Insn::AppendList => "APPEND_LIST",
            Insn::SwapDeepList => "SWAP_DEEP_LIST",
            Insn::PopOutList => "POP_OUT_LIST",
            Insn::ListToArray => "LIST_TO_ARRAY",
            Insn::Ref => "REF",
            Insn::StrictRef => "STRICT_REF",
            Insn::InitializeReferencedBinding => "IRB",
            Insn::PushNewLexEnv => "PNLE",
            Insn::PopLexEnv => "PLE",
            Insn::PushNewVarEnvFromLex => "PNVEFL",
            Insn::PushNewLexEnvFromVar => "PNLEFV",
            Insn::SetLexEnvToVarEnv => "SLETVE",
            Insn::SetAsideLexEnv => "SALE",
            Insn::RestoreLexEnv => "RLE",
            Insn::PushNewPrivateEnv => "PNPE",
            Insn::PopPrivateEnv => "PPE",
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
            Insn::CreatePrivateNameIfMissing => "CPNIM",
            Insn::CreatePerIterationEnvironment => "CPIE",
            Insn::Object => "OBJECT",
            Insn::ObjectWithProto => "OBJ_WITH_PROTO",
            Insn::Array => "ARRAY",
            Insn::CreateDataProperty => "CR_PROP",
            Insn::SetPrototype => "SET_PROTO",
            Insn::ToPropertyKey => "TO_KEY",
            Insn::CopyDataProps => "COPY_DATA_PROPS",
            Insn::CopyDataPropsWithExclusions => "COPY_DATAPROPS_WE",
            Insn::ToString => "TO_STRING",
            Insn::ToNumeric => "TO_NUMERIC",
            Insn::ToObject => "TO_OBJECT",
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
            Insn::InstantiateGeneratorFunctionExpression => "FUNC_GENE",
            Insn::InstantiateOrdinaryFunctionObject => "FUNC_OBJ",
            Insn::InstantiateGeneratorFunctionObject => "FUNC_GENO",
            Insn::InstantiateGeneratorMethod => "GEN_METHOD",
            Insn::GeneratorStartFromFunction => "GEN_START",
            Insn::Yield => "YIELD",
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
            Insn::IteratorClose => "ITER_CLOSE",
            Insn::IteratorCloseIfNotDone => "ITER_CLOSE_IF_NOT_DONE",
            Insn::IteratorNext => "ITER_NEXT",
            Insn::IteratorResultComplete => "IRES_COMPLETE",
            Insn::IteratorResultToValue => "IRES_TOVAL",
            Insn::GetV => "GETV",
            Insn::IteratorDAEElision => "IDAE_ELISION",
            Insn::EmbellishedIteratorStep => "ITER_STEP",
            Insn::IteratorRest => "ITER_REST",
            Insn::EnumerateObjectProperties => "ENUM_PROPS",
            Insn::PrivateIdLookup => "PRIV_ID_LOOKUP",
            Insn::EvaluateInitializedClassFieldDefinition => "EVAL_CLASS_FIELD_DEF",
            Insn::EvaluateInitializedClassStaticFieldDefinition => "EVAL_CLS_STC_FLD_DEF",
            Insn::EvaluateClassStaticBlockDefinition => "EVAL_CLASS_SBLK_DEF",
            Insn::DefineMethod => "DEFINE_METHOD",
            Insn::SetFunctionName => "SET_FUNC_NAME",
            Insn::DefineMethodProperty => "DEF_METH_PROP",
            Insn::DefineGetter => "DEF_GETTER",
            Insn::DefineSetter => "DEF_SETTER",
            Insn::GetParentsFromSuperclass => "GPFS",
            Insn::CreateDefaultConstructor => "DEFAULT_CSTR",
            Insn::MakeClassConstructorAndSetName => "MAKE_CC_SN",
            Insn::MakeConstructor => "MAKE_CSTR",
            Insn::MakeConstructorWithProto => "MAKE_CSTR_PROTO",
            Insn::SetDerived => "SET_DERIVED",
            Insn::AttachElements => "ATTACH_ELEMENTS",
            Insn::AttachSourceText => "ATTACH_SOURCE",
            Insn::NameOnlyFieldRecord => "NAME_ONLY_FIELD_REC",
            Insn::NameOnlyStaticFieldRecord => "NAME_ONLY_STATIC_FIELD",
            Insn::MakePrivateReference => "PRIVATE_REF",
            Insn::GetNewTarget => "GET_NEW_TARGET",
            Insn::GetSuperConstructor => "GET_SUPER_CSTR",
            Insn::ConstructorCheck => "CSTR_CHECK",
            Insn::BindThisAndInit => "BIND_THIS_AND_INIT",
            Insn::StaticClassItem => "STATIC_ITEM",
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
    fn maybe_abrupt(self) -> bool {
        self == AbruptResult::Maybe
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
    #[must_use]
    pub fn abrupt(self, potentially_abrupt: bool) -> Self {
        Self { can_be_abrupt: potentially_abrupt.into(), ..self }
    }
    #[must_use]
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

#[derive(Debug, Copy, Clone)]
pub struct AlwaysAbruptResult;
impl From<AlwaysAbruptResult> for CompilerStatusFlags {
    fn from(_: AlwaysAbruptResult) -> Self {
        Self::new().abrupt(true)
    }
}
impl AlwaysAbruptResult {
    #[expect(clippy::unused_self)]
    fn maybe_abrupt(self) -> bool {
        true
    }
}

#[derive(Debug, Copy, Clone)]
pub struct AlwaysRefResult;
impl From<AlwaysRefResult> for CompilerStatusFlags {
    fn from(_: AlwaysRefResult) -> Self {
        Self::new().reference(true)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct AlwaysAbruptRefResult;
impl From<AlwaysAbruptRefResult> for CompilerStatusFlags {
    fn from(_: AlwaysAbruptRefResult) -> Self {
        Self::new().reference(true).abrupt(true)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct NeverAbruptRefResult;
impl From<NeverAbruptRefResult> for CompilerStatusFlags {
    fn from(_: NeverAbruptRefResult) -> Self {
        Self::new()
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
        id: Option<NameLoc>,
    ) -> anyhow::Result<CompilerStatusFlags> {
        // Runtime Semantics: NamedEvaluation
        // The syntax-directed operation NamedEvaluation takes argument name (a property key or a Private Name) and
        // returns either a normal completion containing a function object or an abrupt completion.
        match self {
            NameableProduction::Function(child) => {
                child.compile_named_evaluation(chunk, strict, text, child.clone(), id).map(CompilerStatusFlags::from)
            }
            NameableProduction::Generator(generator) => {
                GeneratorExpression::named_evaluation(generator, chunk, strict, text, id).map(CompilerStatusFlags::from)
            }
            NameableProduction::AsyncFunction(_) => todo!(),
            NameableProduction::AsyncGenerator(_) => todo!(),
            NameableProduction::Class(class_expression) => {
                let name = id.expect("named class expressions should have a name");
                class_expression.named_evaluation(chunk, strict, text, name).map(CompilerStatusFlags::from)
            }
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
            NameableProduction::Arrow(_) | NameableProduction::AsyncArrow(_) => false,
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
    #[expect(unused_variables)]
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
            PrimaryExpression::Class { node } => node.compile(chunk, strict, text).map(CompilerStatusFlags::from),
            PrimaryExpression::Generator { node } => node.compile(chunk, strict, text).map(CompilerStatusFlags::from),
            PrimaryExpression::AsyncFunction { node } => todo!(),
            PrimaryExpression::AsyncGenerator { node } => todo!(),
            PrimaryExpression::RegularExpression { regex, location } => todo!(),
        }
    }
}

#[cfg(test)]
fn compile_debug_lit(chunk: &mut Chunk, ch: &DebugKind) -> anyhow::Result<NeverAbruptRefResult> {
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
            chunk.bigints.resize(65536, Rc::new(BigInt::from(97_687_897_890_734_187_890_106_587_314_876_543_219_u128)));
            chunk.op(Insn::False);
        }
        DebugKind::Char('~') => {
            // Be a compiler error
            anyhow::bail!("@@~ token detected. aborting compilation.")
        }
        DebugKind::Char(_) => (),
    }
    Ok(NeverAbruptRefResult)
}
#[cfg(not(test))]
fn compile_debug_lit(_: &mut Chunk, _: &DebugKind) -> anyhow::Result<NeverAbruptRefResult> {
    Ok(NeverAbruptRefResult)
}

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
                compile_debug_lit(chunk, ch)?;
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

        assert!(self.count < 1 << 53);
        #[expect(clippy::cast_precision_loss)]
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

        assert!(self.count < 1 << 53);
        #[expect(clippy::cast_precision_loss)]
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
                    np.compile_named_evaluation(chunk, strict, text, Some(NameLoc::OnStack))?
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
            PropertyDefinition::MethodDefinition(md) => {
                // stack:                     obj
                //   DUP                      obj obj
                //   <md.mde>                 err/empty/privateelement obj
                //   JUMP_IF_ABRUPT unwind    empty/privateelement obj
                //   POP                      obj
                // unwind:
                //   UNWIND_IF_ABRUPT 1       err/obj
                chunk.op(Insn::Dup);
                md.method_definition_evaluation(true, chunk, strict, text)?;
                let unwind = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Pop);
                chunk.fixup(unwind).expect("short jumps should work");
                chunk.op_plus_arg(Insn::UnwindIfAbrupt, 1);

                Ok(AbruptResult::Maybe)
            }
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
    // start:                         base
    //  <expression>                  err/ref base
    //  GET_VALUE                     err/val base
    //  JUMP_IF_ABRUPT unwind_1       val base
    //  TO_KEY                        key/err base
    //  JUMP_IF_ABRUPT unwind_1       key base
    //  REF/STRICT_REF                ref
    // unwind_1:                      (err base) / ref
    //  UNWIND_IF_ABRUPT 1            err/ref
    let state = expression.compile(chunk, strict, text)?;
    if state.maybe_ref() {
        chunk.op(Insn::GetValue);
    }
    let unwind_a =
        if state.maybe_ref() || state.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
    chunk.op(Insn::ToPropertyKey);
    let unwind_b = chunk.op_jump(Insn::JumpIfAbrupt);
    chunk.op(if strict { Insn::StrictRef } else { Insn::Ref });

    if let Some(unwind_a) = unwind_a {
        chunk.fixup(unwind_a).expect("jump too short to fail");
    }
    chunk.fixup(unwind_b).expect("jump too short to fail");

    chunk.op_plus_arg(Insn::UnwindIfAbrupt, 1);
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
            MemberExpression::MetaProperty(mp) => mp.compile(chunk, strict, text),
            MemberExpression::NewArguments(me, args, ..) => {
                compile_new_evaluator(chunk, strict, text, &ConstructExpr::Member(me.clone()), Some(args.clone()))
            }
            MemberExpression::PrivateId(me, id, ..) => {
                // MemberExpression : MemberExpression . PrivateIdentifier
                //  1. Let baseReference be ? Evaluation of MemberExpression.
                //  2. Let baseValue be ? GetValue(baseReference).
                //  3. Let fieldNameString be the StringValue of PrivateIdentifier.
                //  4. Return MakePrivateReference(baseValue, fieldNameString).

                // start:
                //   <memberexpression>                err/baseReference
                //   GET_VALUE                         err/baseValue
                //   JUMP_IF_ABRUPT exit               baseValue
                //   MAKE_PRIV_REF fieldName           ref
                // exit:

                let status = me.compile(chunk, strict, text)?;
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                let exit = if status.maybe_abrupt() || status.maybe_ref() {
                    Some(chunk.op_jump(Insn::JumpIfAbrupt))
                } else {
                    None
                };
                let idx = chunk.add_to_string_pool(id.string_value.clone())?;
                chunk.op_plus_arg(Insn::MakePrivateReference, idx);
                let maybe_abrupt = if let Some(exit) = exit {
                    chunk.fixup(exit).expect("Jump too short to fail");
                    true
                } else {
                    false
                };
                Ok(CompilerStatusFlags::new().abrupt(maybe_abrupt).reference(true))
            }
        }
    }
}

impl MetaProperty {
    #[expect(unused_variables)]
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            MetaProperty::NewTarget { .. } => {
                // Runtime Semantics: Evaluation
                // NewTarget : new . target
                //  1. Return GetNewTarget().
                chunk.op(Insn::GetNewTarget);
                Ok(CompilerStatusFlags::new())
            }
            MetaProperty::ImportMeta { .. } => todo!(),
        }
    }
}

impl NewExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            NewExpression::MemberExpression(me) => me.compile(chunk, strict, text),
            NewExpression::NewExpression(ne, ..) => {
                compile_new_evaluator(chunk, strict, text, &ConstructExpr::New(ne.clone()), None)
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
    expr: &ConstructExpr,
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
            CallExpression::SuperCall(sc) => sc.compile(chunk, strict, text).map(CompilerStatusFlags::from),
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
    chunk.op(if strict { Insn::StrictCall } else { Insn::Call });
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

impl OptionalExpression {
    fn common_portion(
        status: CompilerStatusFlags,
        oc: &Rc<OptionalChain>,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<CompilerStatusFlags> {
        let exit_a = if status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
        chunk.op(Insn::Dup);
        let unwind_1 = if status.maybe_ref() {
            chunk.op(Insn::GetValue);
            Some(chunk.op_jump(Insn::JumpIfAbrupt))
        } else {
            None
        };
        let cont = chunk.op_jump(Insn::JumpIfNotNullish);
        chunk.op(Insn::Pop);
        chunk.op(Insn::Pop);
        chunk.op(Insn::Undefined);
        let exit_b = chunk.op_jump(Insn::Jump);
        chunk.fixup(cont).expect("Jump should be too short to fail");
        let chain_status = oc.chain_evaluation(chunk, strict, text)?;
        if let Some(mark) = unwind_1 {
            let exit_c = chunk.op_jump(Insn::Jump);
            chunk.fixup(mark)?;
            chunk.op_plus_arg(Insn::Unwind, 1);
            chunk.fixup(exit_c).expect("Jump should be too short to fail");
        }
        chunk.fixup(exit_b)?;
        if let Some(mark) = exit_a {
            chunk.fixup(mark)?;
        }

        Ok(CompilerStatusFlags::new()
            .reference(chain_status.maybe_ref())
            .abrupt(chain_status.maybe_abrupt() || status.maybe_abrupt()))
    }

    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            OptionalExpression::Member(me, oc) => {
                // OptionalExpression :
                //      MemberExpression OptionalChain
                //  1. Let baseReference be ? Evaluation of MemberExpression.
                //  2. Let baseValue be ? GetValue(baseReference).
                //  3. If baseValue is either undefined or null, then
                //      a. Return undefined.
                //  4. Return ? ChainEvaluation of OptionalChain with arguments baseValue and baseReference.

                //   <ME>                               ref/err
                //   JUMP_IF_ABRUPT exit                ref
                //   DUP                                ref ref
                //   GET_VALUE                          val/err ref
                //   JUMP_IF_ABRUPT unwind_1            val ref
                //   JUMP_IF_NOT_NULLISH continue
                //   POP                                ref
                //   POP
                //   UNDEFINED                          undefined
                //   JUMP exit
                // continue:                            val ref
                //   <OC.chain_evaluation>              ref/val/err
                //   JUMP exit
                // unwind_1:                            err ref
                //   UNWIND 1                           err
                // exit:                                ref/val/err
                let status = me.compile(chunk, strict, text)?;
                Self::common_portion(status, oc, chunk, strict, text)
            }
            OptionalExpression::Call(ce, oc) => {
                let status = ce.compile(chunk, strict, text)?;
                Self::common_portion(status, oc, chunk, strict, text)
            }
            OptionalExpression::Opt(oe, oc) => {
                let status = oe.compile(chunk, strict, text)?;
                Self::common_portion(status, oc, chunk, strict, text)
            }
        }
    }
}

impl OptionalChain {
    fn chain_evaluation(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        // The syntax-directed operation ChainEvaluation takes arguments baseValue (an ECMAScript language
        // value) and baseReference (an ECMAScript language value or a Reference Record) and returns either a
        // normal completion containing either an ECMAScript language value or a Reference Record, or an
        // abrupt completion. It is defined piecewise over the following productions:
        match self {
            OptionalChain::Args(args, _) => {
                // OptionalChain : ?. Arguments
                //  1. Let thisChain be this OptionalChain.
                //  2. Let tailCall be IsInTailPosition(thisChain).
                //  3. Return ? EvaluateCall(baseValue, baseReference, Arguments, tailCall).
                compile_call(chunk, strict, text, args).map(CompilerStatusFlags::from)
            }
            OptionalChain::Exp(ex, _) => {
                // OptionalChain : ?. [ Expression ]
                //  1. If the source text matched by this OptionalChain is strict mode code, let strict be
                //     true; else let strict be false.
                //  2. Return ? EvaluatePropertyAccessWithExpressionKey(baseValue, Expression, strict).
                chunk.op_plus_arg(Insn::Unwind, 1);
                evaluate_property_access_with_expression_key(chunk, ex, strict, text).map(CompilerStatusFlags::from)
            }
            OptionalChain::Ident(id, _) => {
                // OptionalChain : ?. IdentifierName
                //  1. If the source text matched by this OptionalChain is strict mode code, let strict be
                //     true; else let strict be false.
                //  2. Return EvaluatePropertyAccessWithIdentifierKey(baseValue, IdentifierName, strict).
                chunk.op_plus_arg(Insn::Unwind, 1);
                evaluate_property_access_with_identifier_key(chunk, id, strict).map(CompilerStatusFlags::from)
            }
            OptionalChain::Template(_, _) => todo!(),
            OptionalChain::PrivateId(_, _) => todo!(),
            OptionalChain::PlusArgs(oc, args) => {
                // OptionalChain : OptionalChain Arguments
                //  1. Let optionalChain be OptionalChain.
                //  2. Let newReference be ? ChainEvaluation of optionalChain with arguments baseValue and
                //     baseReference.
                //  3. Let newValue be ? GetValue(newReference).
                //  4. Let thisChain be this OptionalChain.
                //  5. Let tailCall be IsInTailPosition(thisChain).
                //  6. Return ? EvaluateCall(newValue, newReference, Arguments, tailCall).

                // start                            val ref
                //   <oc.chain_evaluation>          ref/err
                //   JUMP_IF_ABRUPT exit            ref
                //   DUP                            ref ref
                //   GET_VALUE                      val/err ref
                //   JUMP_IF_ABRUPT unwind          val ref
                //   <EvalCall>                     val/err
                //   JUMP exit
                // unwind:
                //   UNWIND 1
                // exit:
                let status = oc.chain_evaluation(chunk, strict, text)?;
                let exit = if status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                chunk.op(Insn::Dup);
                let unwind = if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                    Some(chunk.op_jump(Insn::JumpIfAbrupt))
                } else {
                    None
                };

                compile_call(chunk, strict, text, args)?;

                if let Some(unwind) = unwind {
                    let exit2 = chunk.op_jump(Insn::Jump);
                    chunk.fixup(unwind)?;
                    chunk.op_plus_arg(Insn::Unwind, 1);
                    chunk.fixup(exit2).expect("Jump too short to fail");
                }
                if let Some(exit) = exit {
                    chunk.fixup(exit)?;
                }
                Ok(CompilerStatusFlags::new().reference(false).abrupt(true))
            }
            OptionalChain::PlusExp(oc, exp, _) => {
                // OptionalChain : OptionalChain [ Expression ]
                //  1. Let optionalChain be OptionalChain.
                //  2. Let newReference be ? ChainEvaluation of optionalChain with arguments baseValue and
                //     baseReference.
                //  3. Let newValue be ? GetValue(newReference).
                //  4. If the source text matched by this OptionalChain is strict mode code, let strict be
                //     true; else let strict be false.
                //  5. Return ? EvaluatePropertyAccessWithExpressionKey(newValue, Expression, strict).

                // start                            val ref
                //   <oc.chain_evaluation>          val/ref/err
                //   GET_VALUE                      val/err
                //   JUMP_IF_ABRUPT exit            val
                //   <EPAWEK>                       ref/err
                // exit:
                let status = oc.chain_evaluation(chunk, strict, text)?;
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                assert!(status.maybe_ref() || status.maybe_abrupt());
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                evaluate_property_access_with_expression_key(chunk, exp, strict, text)?;
                chunk.fixup(exit)?;

                Ok(CompilerStatusFlags::new().reference(true).abrupt(true))
            }
            OptionalChain::PlusIdent(oc, id, _) => {
                // OptionalChain : OptionalChain . IdentifierName
                //  1. Let optionalChain be OptionalChain.
                //  2. Let newReference be ? ChainEvaluation of optionalChain with arguments baseValue and
                //     baseReference.
                //  3. Let newValue be ? GetValue(newReference).
                //  4. If the source text matched by this OptionalChain is strict mode code, let strict be
                //     true; else let strict be false.
                //  5. Return EvaluatePropertyAccessWithIdentifierKey(newValue, IdentifierName, strict).

                // start                            val ref
                //   <oc.chain_evaluation>          val/ref/err
                //   GET_VALUE                      val/err
                //   JUMP_IF_ABRUPT exit            val
                //   <EPAWIK>                       ref/err
                // exit:
                let status = oc.chain_evaluation(chunk, strict, text)?;
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                assert!(status.maybe_ref() || status.maybe_abrupt());
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                evaluate_property_access_with_identifier_key(chunk, id, strict)?;
                chunk.fixup(exit).expect("Jump should be too short to fail");
                Ok(CompilerStatusFlags::from(AlwaysAbruptRefResult))
            }
            OptionalChain::PlusTemplate(_, _) => todo!(),
            OptionalChain::PlusPrivateId(_, _, _) => todo!(),
        }
    }
}

impl LeftHandSideExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            LeftHandSideExpression::New(ne) => ne.compile(chunk, strict, text),
            LeftHandSideExpression::Call(ce) => ce.compile(chunk, strict, text),
            LeftHandSideExpression::Optional(oe) => oe.compile(chunk, strict, text),
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
                if has_variable {
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
                        let idx = chunk.add_to_float_pool(f64::from(fixed_len))?;
                        chunk.op_plus_arg(Insn::Float, idx);
                        chunk.op(Insn::Add);
                        chunk.fixup(exit).expect("Jump too short to fail");
                    }
                } else {
                    // size known at compile time:
                    //   <argument_list>       (err) or (arg(n-1) arg(n-2) ... arg0)
                    //   JUMP_IF_ABRUPT exit   arg(n-1) arg(n-2) ... arg0
                    //   FLOAT n               n arg(n-1) arg(n-2) ... arg0
                    // exit:
                    let exit = if status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                    let index = chunk.add_to_float_pool(f64::from(fixed_len))?;
                    chunk.op_plus_arg(Insn::Float, index);
                    if let Some(mark) = exit {
                        chunk.fixup(mark).expect("Jump is too short to overflow.");
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
                        let idx = chunk.add_to_float_pool(f64::from(fixed_len))?;
                        chunk.op_plus_arg(Insn::Float, idx);
                        chunk.op(Insn::Add);
                    }
                } else {
                    let idx = chunk.add_to_float_pool(f64::from(fixed_len))?;
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

// This needs to be a macro (and not a function) because left & right have different types depending on the particular
// parse node that's being compiled.
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
        let (insn, left, right) = match self {
            AdditiveExpression::MultiplicativeExpression(me) => return me.compile(chunk, strict, text),
            AdditiveExpression::Add(left, right) => (Insn::Add, left, right),
            AdditiveExpression::Subtract(left, right) => (Insn::Subtract, left, right),
        };
        compile_binary_expression!(chunk, strict, text, left, right, insn).map(CompilerStatusFlags::from)
    }
}

impl ShiftExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        let (insn, left, right) = match self {
            ShiftExpression::AdditiveExpression(ae) => return ae.compile(chunk, strict, text),
            ShiftExpression::LeftShift(left, right) => (Insn::LeftShift, left, right),
            ShiftExpression::SignedRightShift(left, right) => (Insn::SignedRightShift, left, right),
            ShiftExpression::UnsignedRightShift(left, right) => (Insn::UnsignedRightShift, left, right),
        };
        compile_binary_expression!(chunk, strict, text, left, right, insn).map(CompilerStatusFlags::from)
    }
}

impl RelationalExpression {
    fn insn(&self) -> Result<Insn, anyhow::Error> {
        Ok(match self {
            RelationalExpression::Less(_, _) => Insn::Less,
            RelationalExpression::Greater(_, _) => Insn::Greater,
            RelationalExpression::LessEqual(_, _) => Insn::LessEqual,
            RelationalExpression::GreaterEqual(_, _) => Insn::GreaterEqual,
            RelationalExpression::InstanceOf(_, _) => Insn::InstanceOf,
            RelationalExpression::In(_, _) => Insn::In,
            _ => anyhow::bail!("RelationalExpression has no binary instruction"),
        })
    }

    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            RelationalExpression::ShiftExpression(se) => se.compile(chunk, strict, text),
            RelationalExpression::Less(left, right)
            | RelationalExpression::Greater(left, right)
            | RelationalExpression::LessEqual(left, right)
            | RelationalExpression::GreaterEqual(left, right)
            | RelationalExpression::InstanceOf(left, right)
            | RelationalExpression::In(left, right) => {
                let insn = self.insn().expect("relational exp should be binary");
                compile_binary_expression!(chunk, strict, text, left, right, insn).map(CompilerStatusFlags::from)
            }
            RelationalExpression::PrivateIn(_, _, _) => todo!(),
        }
    }
}

impl EqualityExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        fn equality_binary(
            chunk: &mut Chunk,
            strict: bool,
            text: &str,
            left: &Rc<EqualityExpression>,
            right: &Rc<RelationalExpression>,
            insn: Insn,
        ) -> anyhow::Result<CompilerStatusFlags> {
            compile_binary_expression!(chunk, strict, text, left, right, insn).map(CompilerStatusFlags::from)
        }
        match self {
            EqualityExpression::RelationalExpression(re) => re.compile(chunk, strict, text),
            EqualityExpression::Equal(left, right) => equality_binary(chunk, strict, text, left, right, Insn::Equal),
            EqualityExpression::NotEqual(left, right) => {
                equality_binary(chunk, strict, text, left, right, Insn::NotEqual)
            }
            EqualityExpression::StrictEqual(left, right) => {
                equality_binary(chunk, strict, text, left, right, Insn::StrictEqual)
            }
            EqualityExpression::NotStrictEqual(left, right) => {
                equality_binary(chunk, strict, text, left, right, Insn::StrictNotEqual)
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

impl YieldExpression {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            Self::Simple { .. } => {
                // YieldExpression : yield
                //  1. Return ? Yield(undefined).

                // start:
                //   UNDEFINED         undefined
                //   YIELD             val/err
                chunk.op(Insn::Undefined);
                chunk.op(Insn::Yield);
                Ok(CompilerStatusFlags::new().abrupt(true))
            }
            Self::Expression { exp, .. } => {
                // YieldExpression : yield AssignmentExpression
                //  1. Let exprRef be ? Evaluation of AssignmentExpression.
                //  2. Let value be ? GetValue(exprRef).
                //  3. Return ? Yield(value).

                // start:
                //   <ae>            val/ref/err
                //   GET_VALUE       val/err
                //   JUMP_IF_ABRUPT  exit
                //   YIELD           val/err
                // exit:

                let status = exp.compile(chunk, strict, text)?;
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                let exit_1 = if status.maybe_abrupt() || status.maybe_ref() {
                    Some(chunk.op_jump(Insn::JumpIfAbrupt))
                } else {
                    None
                };
                chunk.op(Insn::Yield);
                if let Some(exit) = exit_1 {
                    chunk.fixup(exit).expect("Jump too short to fail");
                }
                Ok(CompilerStatusFlags::new().abrupt(true))
            }
            Self::From { .. } => todo!(),
        }
    }
}

impl AssignmentExpression {
    fn lor_land_coal_assign(
        lhse: &Rc<LeftHandSideExpression>,
        ae: &Rc<AssignmentExpression>,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        jump_insn: Insn,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // AssignmentExpression : LeftHandSideExpression &&= AssignmentExpression
        //  1. Let lref be ? Evaluation of LeftHandSideExpression.
        //  2. Let lval be ? GetValue(lref).
        //  3. Let lbool be ToBoolean(lval).
        //  4. If lbool is false, return lval.
        //  5. If IsAnonymousFunctionDefinition(AssignmentExpression) is true and IsIdentifierRef of
        //     LeftHandSideExpression is true, then
        //      a. Let rval be ? NamedEvaluation of AssignmentExpression with argument lref.[[ReferencedName]].
        //  6. Else,
        //      a. Let rref be ? Evaluation of AssignmentExpression.
        //      b. Let rval be ? GetValue(rref).
        //  7. Perform ? PutValue(lref, rval).
        //  8. Return rval.

        // AssignmentExpression : LeftHandSideExpression ||= AssignmentExpression
        //  1. Let lref be ? Evaluation of LeftHandSideExpression.
        //  2. Let lval be ? GetValue(lref).
        //  3. Let lbool be ToBoolean(lval).
        //  4. If lbool is true, return lval.
        //  5. If IsAnonymousFunctionDefinition(AssignmentExpression) is true and IsIdentifierRef of
        //     LeftHandSideExpression is true, then
        //      a. Let rval be ? NamedEvaluation of AssignmentExpression with argument lref.[[ReferencedName]].
        //  6. Else,
        //      a. Let rref be ? Evaluation of AssignmentExpression.
        //      b. Let rval be ? GetValue(rref).
        //  7. Perform ? PutValue(lref, rval).
        //  8. Return rval.

        // start:
        //   <lhse.evaluation>         err/lref
        //   JUMP_IF_ABRUPT exit       lref
        //   DUP                       lref lref
        //   GET_VALUE                 err/lval lref
        //   JUMP_IF_ABRUPT unwind     lval lref
        //   JUMP_IF_FALSE/TRUE unwind      lval lref
        //   POP                       lref
        //   <ae.evaluation>           err/rref lref
        //   GET_VALUE                 err/rval lref
        //   JUMP_IF_ABRUPT unwind     rval lref
        //   DUP                       rval rval lref
        //   ROTATE_DN 3               rval lref rval
        //   PUT_VALUE                 err/[empty] rval
        //   JUMP_IF_ABRUPT unwind     [empty] rval
        //   POP                       rval
        //   JUMP exit
        // unwind:
        //   UNWIND 1                  err/lval
        // exit:                       err/lval/rval

        let status = lhse.compile(chunk, strict, text)?;
        let exit_1 = if status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
        chunk.op(Insn::Dup);
        let unwind_1 = if status.maybe_ref() {
            chunk.op(Insn::GetValue);
            Some(chunk.op_jump(Insn::JumpIfAbrupt))
        } else {
            None
        };
        let unwind_2 = chunk.op_jump(jump_insn);
        chunk.op(Insn::Pop);
        let ae_status =
            if let (Some(anonymous), Some(reference)) = (ae.anonymous_function_definition(), lhse.identifier_ref()) {
                let idx = chunk.add_to_string_pool(reference.string_value()).expect("would already have been added");
                anonymous.compile_named_evaluation(chunk, strict, text, Some(NameLoc::Index(idx)))?
            } else {
                ae.compile(chunk, strict, text)?
            };
        if ae_status.maybe_ref() {
            chunk.op(Insn::GetValue);
        }
        let unwind_3 = if ae_status.maybe_abrupt() || ae_status.maybe_ref() {
            Some(chunk.op_jump(Insn::JumpIfAbrupt))
        } else {
            None
        };
        chunk.op(Insn::Dup);
        chunk.op_plus_arg(Insn::RotateDown, 3);
        chunk.op(Insn::PutValue);
        let unwind_4 = chunk.op_jump(Insn::JumpIfAbrupt);
        chunk.op(Insn::Pop);
        let exit_2 = chunk.op_jump(Insn::Jump);
        chunk.fixup(unwind_4).expect("Jump too short to fail");
        if let Some(unwind_3) = unwind_3 {
            chunk.fixup(unwind_3).expect("Jump too short to fail");
        }
        chunk.fixup(unwind_2)?;
        if let Some(unwind_1) = unwind_1 {
            chunk.fixup(unwind_1)?;
        }
        chunk.op_plus_arg(Insn::Unwind, 1);
        chunk.fixup(exit_2).expect("Jump too short to fail");
        if let Some(exit_1) = exit_1 {
            chunk.fixup(exit_1)?;
        }
        Ok(AlwaysAbruptResult)
    }

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
                        np.compile_named_evaluation(chunk, strict, text, Some(NameLoc::Index(idx)))?
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
            AssignmentExpression::Yield(ye) => ye.compile(chunk, strict, text),
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
            AssignmentExpression::LandAssignment(lhse, ae) => {
                Self::lor_land_coal_assign(lhse, ae, chunk, strict, text, Insn::JumpIfFalse)
                    .map(CompilerStatusFlags::from)
            }
            AssignmentExpression::LorAssignment(lhse, ae) => {
                Self::lor_land_coal_assign(lhse, ae, chunk, strict, text, Insn::JumpIfTrue)
                    .map(CompilerStatusFlags::from)
            }
            AssignmentExpression::CoalAssignment(lhse, ae) => {
                // AssignmentExpression : LeftHandSideExpression ??= AssignmentExpression
                //  1. Let lref be ? Evaluation of LeftHandSideExpression.
                //  2. Let lval be ? GetValue(lref).
                //  3. If lval is neither undefined nor null, return lval.
                //  4. If IsAnonymousFunctionDefinition(AssignmentExpression) is true and IsIdentifierRef of
                //     LeftHandSideExpression is true, then
                //      a. Let rval be ? NamedEvaluation of AssignmentExpression with argument lref.[[ReferencedName]].
                //  5. Else,
                //      a. Let rref be ? Evaluation of AssignmentExpression.
                //      b. Let rval be ? GetValue(rref).
                //  6. Perform ? PutValue(lref, rval).
                //  7. Return rval.
                Self::lor_land_coal_assign(lhse, ae, chunk, strict, text, Insn::JumpIfNotNullish)
                    .map(CompilerStatusFlags::from)
            }
            AssignmentExpression::Destructuring(ap, ae) => {
                //  2. Let assignmentPattern be the AssignmentPattern that is covered by LeftHandSideExpression.
                //  3. Let rref be ? Evaluation of AssignmentExpression.
                //  4. Let rval be ? GetValue(rref).
                //  5. Perform ? DestructuringAssignmentEvaluation of assignmentPattern with argument rval.
                //  6. Return rval.

                // start:
                //   <ae.evaluate>           rref/err
                //   GET_VALUE               rval/err
                //   JUMP_IF_ABRUPT exit     rval
                //   <ap.destructuring_assignment_evaluation>  rval/err
                // exit:
                let expr_status = ae.compile(chunk, strict, text)?;
                let mut exit = None;
                if expr_status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                if expr_status.maybe_abrupt() || expr_status.maybe_ref() {
                    exit = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                let pattern_status = ap.destructuring_assignment_evaluation(chunk, strict, text)?;
                if let Some(exit) = exit {
                    chunk.fixup(exit)?;
                }
                Ok(CompilerStatusFlags::new()
                    .abrupt(expr_status.maybe_abrupt() || expr_status.maybe_ref() || pattern_status.maybe_abrupt()))
            }
        }
    }
}

impl AssignmentPattern {
    fn destructuring_assignment_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: DestructuringAssignmentEvaluation
        // The syntax-directed operation DestructuringAssignmentEvaluation takes argument value (an ECMAScript language
        // value) and returns either a normal completion containing unused or an abrupt completion. It is defined
        // piecewise over the following productions:
        //
        // Our changes: rather than returning unused/err, we return value/err (the input value, unchanged).
        match self {
            AssignmentPattern::Object(oap) => oap.destructuring_assignment_evaluation(chunk, strict, text),
            AssignmentPattern::Array(aap) => aap.destructuring_assignment_evaluation(chunk, strict, text),
        }
    }
}

impl ObjectAssignmentPattern {
    fn destructuring_assignment_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: DestructuringAssignmentEvaluation
        // The syntax-directed operation DestructuringAssignmentEvaluation takes argument value (an ECMAScript language
        // value) and returns either a normal completion containing unused or an abrupt completion. It is defined
        // piecewise over the following productions:
        //
        // Our changes: rather than returning unused/err, we return value/err (the input value, unchanged).
        match self {
            ObjectAssignmentPattern::Empty { .. } => {
                // ObjectAssignmentPattern : { }
                // 1. Perform ? RequireObjectCoercible(value).
                // 2. Return unused.

                // start:             value
                //   REQ_COER         value/err
                // exit:
                chunk.op(Insn::RequireCoercible);
                Ok(AlwaysAbruptResult)
            }
            ObjectAssignmentPattern::RestOnly { arp, .. } => {
                // ObjectAssignmentPattern : { AssignmentRestProperty }
                //  1. Perform ? RequireObjectCoercible(value).
                //  2. Let excludedNames be a new empty List.
                //  3. Return ? RestDestructuringAssignmentEvaluation of AssignmentRestProperty with arguments value and
                //     excludedNames.

                // start:                                               value
                //   REQ_COER                                           value/err
                //   JUMP_IF_ABRUPT exit                                value
                //   ZERO                                               excludedNames value
                //   <arp.rest_destructuring_assignment_evaluation>     value/err
                // exit:

                chunk.op(Insn::RequireCoercible);
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Zero);
                arp.rest_destructuring_assignment_evaluation(chunk, strict, text)?;
                chunk.fixup(exit)?;
                Ok(AlwaysAbruptResult)
            }
            ObjectAssignmentPattern::ListOnly { apl, .. }
            | ObjectAssignmentPattern::ListRest { apl, arp: None, .. } => {
                // ObjectAssignmentPattern :
                //      { AssignmentPropertyList }
                //      { AssignmentPropertyList , }
                //  1. Perform ? RequireObjectCoercible(value).
                //  2. Perform ? PropertyDestructuringAssignmentEvaluation of AssignmentPropertyList with argument
                //     value.
                //  3. Return unused.

                // start:                                                value
                //   REQ_COER                                            value/err
                //   JUMP_IF_ABRUPT exit                                 value
                //   <apl.property_destructuring_assignment_evaluation>  excludedNames/err value
                //   JUMP_IF_ABRUPT unwind                               excludedNames value
                //   POP_LIST                                            value
                //   JUMP exit
                // unwind:                                               err value
                //   UNWIND 1                                            err
                // exit:

                chunk.op(Insn::RequireCoercible);
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                apl.property_destructuring_assignment_evaluation(chunk, strict, text)?;
                let unwind = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::PopList);

                let exit2 = chunk.op_jump(Insn::Jump);
                chunk.fixup(unwind).expect("Jump too short to fail");
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit2).expect("Jump too short to fail");

                chunk.fixup(exit)?;
                Ok(AlwaysAbruptResult)
            }
            ObjectAssignmentPattern::ListRest { apl, arp: Some(arp), .. } => {
                // ObjectAssignmentPattern : { AssignmentPropertyList , AssignmentRestProperty }
                //  1. Perform ? RequireObjectCoercible(value).
                //  2. Let excludedNames be ? PropertyDestructuringAssignmentEvaluation of AssignmentPropertyList with
                //     argument value.
                //  3. Return ? RestDestructuringAssignmentEvaluation of AssignmentRestProperty with arguments value and
                //     excludedNames.

                // start:                                                value
                //   REQ_COER                                            value/err
                //   JUMP_IF_ABRUPT exit                                 value
                //   <apl.property_destructuring_assignment_evaluation>  excludedNames/err value
                //   JUMP_IF_ABRUPT unwind                               exclucedNames value
                //   <arp.rest_destructuring_assignment_evaluation>      value/err
                //   JUMP exit
                // unwind:                                               err value
                //   UNWIND 1                                            err
                // exit:
                chunk.op(Insn::RequireCoercible);
                let exit1 = chunk.op_jump(Insn::JumpIfAbrupt);
                apl.property_destructuring_assignment_evaluation(chunk, strict, text)?;
                let unwind = chunk.op_jump(Insn::JumpIfAbrupt);
                arp.rest_destructuring_assignment_evaluation(chunk, strict, text)?;

                let exit2 = chunk.op_jump(Insn::Jump);
                chunk.fixup(unwind)?;
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit2).expect("jump too short to fail");

                chunk.fixup(exit1)?;
                Ok(AlwaysAbruptResult)
            }
        }
    }
}

impl AssignmentPropertyList {
    fn property_destructuring_assignment_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: PropertyDestructuringAssignmentEvaluation
        // The syntax-directed operation PropertyDestructuringAssignmentEvaluation takes argument value (an ECMAScript
        // language value) and returns either a normal completion containing a List of property keys or an abrupt
        // completion. It collects a list of all destructured property keys. It is defined piecewise over the following
        // productions:
        //
        // Our API:
        //    input:       value
        //    output:      key_list/err value
        match self {
            AssignmentPropertyList::Item(item) => {
                // AssignmentPropertyList : AssignmentProperty
                //  1. Return ? PropertyDestructuringAssignmentEvaluation of AssignmentProperty with argument value.
                item.property_destructuring_assignment_evaluation(chunk, strict, text)
            }
            AssignmentPropertyList::List(list, item) => {
                // AssignmentPropertyList : AssignmentPropertyList , AssignmentProperty
                //  1. Let propertyNames be ? PropertyDestructuringAssignmentEvaluation of AssignmentPropertyList with
                //     argument value.
                //  2. Let nextNames be ? PropertyDestructuringAssignmentEvaluation of AssignmentProperty with argument
                //     value.
                //  3. Return the list-concatenation of propertyNames and nextNames.

                // start:                                                      value
                //   <list.property_destructuring_assignment_evaluation>       property_names/err value
                //   JUMP_IF_ABRUPT exit                                       property_names value
                //   SWAP_LIST                                                 value property_names
                //   <item.property_destructuring_assignment_evaluation>       next_names/err value property_names
                //   JUMP_IF_ABRUPT unwind_deep_list                           next_names value property_names
                //   SWAP_DEEP_LIST                                            next_names property_names value
                //   APPEND_LIST                                               all_names value
                //   JUMP exit
                // unwind_deep_list:                                           err value property_names
                //   POP_OUT_LIST 3                                            err value
                // exit:
                list.property_destructuring_assignment_evaluation(chunk, strict, text)?;
                let exit1 = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::SwapList);
                item.property_destructuring_assignment_evaluation(chunk, strict, text)?;
                let unwind_deep_list = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::SwapDeepList);
                chunk.op(Insn::AppendList);
                let exit2 = chunk.op_jump(Insn::Jump);
                chunk.fixup(unwind_deep_list).expect("Jump too short to fail");
                chunk.op_plus_arg(Insn::PopOutList, 3);
                chunk.fixup(exit2).expect("Jump too short to fail");
                chunk.fixup(exit1)?;
                Ok(AlwaysAbruptResult)
            }
        }
    }
}

impl AssignmentProperty {
    fn property_destructuring_assignment_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: PropertyDestructuringAssignmentEvaluation The syntax-directed operation
        // PropertyDestructuringAssignmentEvaluation takes argument value (an ECMAScript language value) and returns
        // either a normal completion containing a List of property keys or an abrupt completion. It collects a list of
        // all destructured property keys. It is defined piecewise over the following productions:
        //
        // Our change: don't consume the value
        // Input:   value
        // Output:  key_names/err value

        match self {
            AssignmentProperty::Ident(ident, izer) => {
                // AssignmentProperty : IdentifierReference Initializeropt
                //  1. Let P be StringValue of IdentifierReference.
                //  2. Let lref be ? ResolveBinding(P).
                //  3. Let v be ? GetV(value, P).
                //  4. If Initializeropt is present and v is undefined, then
                //      a. If IsAnonymousFunctionDefinition(Initializer) is true, then
                //          i. Set v to ? NamedEvaluation of Initializer with argument P.
                //      b. Else,
                //          i. Let defaultValue be ? Evaluation of Initializer.
                //          ii. Set v to ? GetValue(defaultValue).
                //  5. Perform ? PutValue(lref, v).
                //  6. Return « P ».

                // start:                                                      value
                //   STRING p                                                  name value
                //   RESOLVE/STRICT_RESOLVE                                    lref/err value
                //   JUMP_IF_ABRUPT exit                                       lref value
                //   SWAP                                                      value lref
                //   POP2PUSH3                                                 value lref value
                //   STRING p                                                  name value lref value
                //   GETV                                                      v/err lref value
                //   JUMP_IF_ABRUPT unwind                                     v lref value
                // --- Some(izer)
                //   JUMP_NOT_UNDEF v_ok                                       undefined lref value
                //   POP                                                       lref value
                //   --- if IsAnonymousFunctionDefinition
                //   <izer.named_evaluation(p)>                                v/err lref value
                //   JUMP_IF_ABRUPT unwind                                     v lref value
                //   --- else
                //   <izer>                                                    val/ref/err lref value
                //   GET_VALUE                                                 v/err lref value
                //   JUMP_IF_ABRUPT unwind                                     v lref value
                //   ---
                // v_ok:
                // ---
                //   PUT_VALUE                                                 [empty]/err value
                //   JUMP_IF_ABRUPT exit                                       [empty] value
                //   POP                                                       value
                //   STRING p                                                  name value
                //   FLOAT 1                                                   name_list value
                //   JUMP exit
                // unwind:                                                     err lref value
                //   UNWIND 1                                                  err value
                // exit:
                let p_value = ident.string_value();
                let p = chunk.add_to_string_pool(p_value)?;
                chunk.op_plus_arg(Insn::String, p);
                chunk.op(if strict { Insn::StrictResolve } else { Insn::Resolve });
                let exit1 = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Swap);
                chunk.op(Insn::Pop2Push3);
                chunk.op_plus_arg(Insn::String, p);
                chunk.op(Insn::GetV);
                let unwind1 = chunk.op_jump(Insn::JumpIfAbrupt);

                let mut unwind2 = None;
                if let Some(izer) = izer {
                    let v_ok = chunk.op_jump(Insn::JumpIfNotUndef);
                    chunk.op(Insn::Pop);
                    let status = if let Some(np) = izer.anonymous_function_definition() {
                        np.compile_named_evaluation(chunk, strict, text, Some(NameLoc::Index(p)))?
                    } else {
                        izer.compile(chunk, strict, text, CompileMod::Unmodified)?
                    };
                    if status.maybe_ref() {
                        chunk.op(Insn::GetValue);
                    }
                    if status.maybe_abrupt() || status.maybe_ref() {
                        unwind2 = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                    }
                    chunk.fixup(v_ok)?;
                }

                chunk.op(Insn::PutValue);
                let exit2 = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Pop);
                chunk.op_plus_arg(Insn::String, p);
                let one = chunk.add_to_float_pool(1.0)?;
                chunk.op_plus_arg(Insn::Float, one);
                let exit3 = chunk.op_jump(Insn::Jump);

                chunk.fixup(unwind1)?;
                if let Some(unwind2) = unwind2 {
                    chunk.fixup(unwind2).expect("jump too short to fail");
                }
                chunk.op_plus_arg(Insn::Unwind, 1);

                chunk.fixup(exit1)?;
                chunk.fixup(exit2).expect("jump too short to fail");
                chunk.fixup(exit3).expect("jump too short to fail");
                Ok(AlwaysAbruptResult)
            }
            AssignmentProperty::Property(prop_name, ae) => {
                // AssignmentProperty : PropertyName : AssignmentElement
                //  1. Let name be ? Evaluation of PropertyName.
                //  2. Perform ? KeyedDestructuringAssignmentEvaluation of AssignmentElement with arguments value and
                //     name.
                //  3. Return « name ».

                // start:                                                      value
                //   DUP                                                       value value
                //   <prop_name>                                               name/err value value
                //   JUMP_IF_ABRUPT unwind                                     name value value
                //   POP2PUSH3                                                 name value name value
                //   <ae.keyed_destructuring_assignment_evaluation>            [empty]/err name value
                //   JUMP_IF_ABRUPT unwind                                     [empty] name value
                //   POP                                                       name value
                //   FLOAT 1                                                   name_list value
                //   JUMP exit
                // unwind:                                                     err name value
                //   UNWIND 1                                                  err value
                // exit:

                chunk.op(Insn::Dup);
                let status = prop_name.compile(chunk, strict, text)?;
                let mut unwind1 = None;
                if status.maybe_abrupt() {
                    unwind1 = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                chunk.op(Insn::Pop2Push3);
                ae.keyed_destructuring_assignment_evaluation(chunk, strict, text)?;
                let unwind2 = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Pop);
                let one = chunk.add_to_float_pool(1.0)?;
                chunk.op_plus_arg(Insn::Float, one);
                let exit = chunk.op_jump(Insn::Jump);
                if let Some(unwind) = unwind1 {
                    chunk.fixup(unwind)?;
                }
                chunk.fixup(unwind2).expect("Jump too short to fail");
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit).expect("jump too short to fail");
                Ok(AlwaysAbruptResult)
            }
        }
    }
}

impl AssignmentRestProperty {
    fn rest_destructuring_assignment_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: RestDestructuringAssignmentEvaluation
        // The syntax-directed operation RestDestructuringAssignmentEvaluation takes arguments value (an ECMAScript
        // language value) and excludedNames (a List of property keys) and returns either a normal completion containing
        // unused or an abrupt completion. It is defined piecewise over the following productions:
        //
        // Our API:
        // Input:    excludedNames value
        // Output:   value/err (the input value, unchanged)
        //
        // AssignmentRestProperty : ... DestructuringAssignmentTarget
        //  1. Let lref be ? Evaluation of DestructuringAssignmentTarget.
        //  2. Let restObj be OrdinaryObjectCreate(%Object.prototype%).
        //  3. Perform ? CopyDataProperties(restObj, value, excludedNames).
        //  4. Return ? PutValue(lref, restObj).

        // start:                                         excluded_names value
        //   DUP_AFTER_LIST                               excluded_names value value
        //   <dat>                                        lref/err excluded_names value value
        //   JUMP_IF_ABRUPT unwind_list_plus_2            lref excluded_names value value
        //   ROTATE_DNLIST 1                              excluded_names value lref value
        //   OBJECT                                       rest_obj excluded_names value lref value
        //   ROTATE_DNLIST 1                              excluded_names value rest_obj lref value
        //   COPY_DATAPROPS_WE                            rest_obj/err lref value
        //   JUMP_IF_ABRUPT unwind_2                      rest_obj lref value
        //   PUT_VALUE                                    [empty]/err value
        //   JUMP_IF_ABRUPT unwind_1                      [empty] value
        //   POP                                          value
        //   JUMP exit
        // unwind_list_plus_2:                            err excluded_names value value
        //   UNWIND_LIST                                  err value value
        // unwind_2:
        //   UNWIND 1                                     err value
        // unwind_1:
        //   UNWIND 1                                     err
        // exit:                                          value/err

        chunk.op(Insn::DupAfterList);
        let dat_status = self.0.compile(chunk, strict, text)?;
        let unwind_list_plus_2 = if dat_status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
        chunk.op_plus_arg(Insn::RotateDownList, 1);
        chunk.op(Insn::Object);
        chunk.op_plus_arg(Insn::RotateDownList, 1);
        chunk.op(Insn::CopyDataPropsWithExclusions);
        let unwind_2 = chunk.op_jump(Insn::JumpIfAbrupt);
        chunk.op(Insn::PutValue);
        let unwind_1 = chunk.op_jump(Insn::JumpIfAbrupt);
        chunk.op(Insn::Pop);
        let exit = chunk.op_jump(Insn::Jump);
        if let Some(unwind_list_plus_2) = unwind_list_plus_2 {
            chunk.fixup(unwind_list_plus_2).expect("jump too short to fail");
            chunk.op(Insn::UnwindList);
        }
        chunk.fixup(unwind_2).expect("jump too short to fail");
        chunk.op_plus_arg(Insn::Unwind, 1);
        chunk.fixup(unwind_1).expect("jump too short to fail");
        chunk.op_plus_arg(Insn::Unwind, 1);
        chunk.fixup(exit).expect("jump too short to fail");

        Ok(AlwaysAbruptResult)
    }
}

impl ArrayAssignmentPattern {
    fn destructuring_assignment_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: DestructuringAssignmentEvaluation
        // The syntax-directed operation DestructuringAssignmentEvaluation takes argument value (an ECMAScript language
        // value) and returns either a normal completion containing unused or an abrupt completion. It is defined
        // piecewise over the following productions:
        //
        // Our changes: rather than returning unused/err, we return value/err (the input value, unchanged).
        match self {
            ArrayAssignmentPattern::RestOnly { elision: None, are: None, .. } => {
                // ArrayAssignmentPattern : [ ]
                //  1. Let iteratorRecord be ? GetIterator(value, sync).
                //  2. Return ? IteratorClose(iteratorRecord, NormalCompletion(unused)).

                // start                    value
                //   DUP                    value value
                //   GET_SYNC_ITERATOR      ir/err value
                //   JUMP_IF_ABRUPT unwind  ir value
                //   EMPTY                  [empty] ir value
                //   ITERATOR_CLOSE         [empty]/err value
                //   JUMP_IF_ABRUPT unwind  [empty] value
                //   POP                    value
                //   JUMP exit
                // unwind:
                //   UNWIND 1
                // exit:
                chunk.op(Insn::Dup);
                chunk.op(Insn::GetSyncIterator);
                let unwind1 = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Empty);
                chunk.op(Insn::IteratorClose);
                let unwind2 = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Pop);
                let exit = chunk.op_jump(Insn::Jump);
                chunk.fixup(unwind1).expect("jump too short to fail");
                chunk.fixup(unwind2).expect("jump too short to fail");
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit).expect("jump too short to fail");
                Ok(AlwaysAbruptResult)
            }
            ArrayAssignmentPattern::RestOnly { elision: Some(elision), are: None, .. } => {
                // ArrayAssignmentPattern : [ Elision ]
                //  1. Let iteratorRecord be ? GetIterator(value, sync).
                //  2. Let result be Completion(IteratorDestructuringAssignmentEvaluation of Elision with argument
                //     iteratorRecord).
                //  3. If iteratorRecord.[[Done]] is false, return ? IteratorClose(iteratorRecord, result).
                //  4. Return result.

                // start:                            value
                //   DUP                             value value
                //   GET_SYNC_ITERATOR               ir/err value
                //   JUMP_IF_ABRUPT unwind           ir value
                //   DUP                             ir ir value
                //   <elision.iterator_destructuring_assignment_evaluation> ir/err ir value
                //   EMPTY_IF_NOT_ERR                [empty]/err ir value
                //   IR_CLOSE_IF_NOT_DONE            [empty]/err value
                //   UPDATE_EMPTY                    value/err
                //   JUMP exit
                // unwind:
                //   UNWIND 1
                // exit:

                chunk.op(Insn::Dup);
                chunk.op(Insn::GetSyncIterator);
                let unwind = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Dup);
                elision.iterator_destructuring_assignment_evaluation(chunk)?;
                chunk.op(Insn::EmptyIfNotError);
                chunk.op(Insn::IteratorCloseIfNotDone);
                chunk.op(Insn::UpdateEmpty);
                let exit = chunk.op_jump(Insn::Jump);
                chunk.fixup(unwind).expect("jump too short to fail");
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit).expect("jump too short to fail");

                Ok(AlwaysAbruptResult)
            }
            ArrayAssignmentPattern::RestOnly { elision, are: Some(are), .. } => {
                // ArrayAssignmentPattern : [ Elisionopt AssignmentRestElement ]
                //  1. Let iteratorRecord be ? GetIterator(value, sync).
                //  2. If Elision is present, then
                //      a. Let status be Completion(IteratorDestructuringAssignmentEvaluation of Elision with argument
                //         iteratorRecord).
                //      b. If status is an abrupt completion, then
                //          i. Assert: iteratorRecord.[[Done]] is true.
                //          ii. Return ? status.
                //  3. Let result be Completion(IteratorDestructuringAssignmentEvaluation of AssignmentRestElement with
                //     argument iteratorRecord).
                //  4. If iteratorRecord.[[Done]] is false, return ? IteratorClose(iteratorRecord, result).
                //  5. Return result.

                // start:                                                    value
                //   DUP                                                     value value
                //   GET_SYNC_ITER                                           ir/err value
                //   JUMP_IF_ABRUPT unwind                                   ir value
                //   DUP                                                     ir ir value
                // --- if elision
                //   <elision.iterator_destructuring_assignment_evaluation>  ir/err ir value
                //   JUMP_IF_ABRUPT unwind2                                  ir ir value
                // ---
                //   <are.iterator_destructuring_assignment_evaluation>      ir/err ir value
                //   EMPTY_IF_NOT_ERR                                        [empty]/err ir value
                //   IR_CLOSE_IF_NOT_DONE                                    [empty]/err value
                //   UPDATE_EMPTY                                            value/err
                //   JUMP exit
                // unwind2:                                                  err ir value
                //   UNWIND 1                                                err value
                // unwind:                                                   err value
                //   UNWIND 1                                                err
                // exit:                                                     value/err
                chunk.op(Insn::Dup);
                chunk.op(Insn::GetSyncIterator);
                let unwind = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Dup);
                let mut unwind2 = None;
                if let Some(elision) = elision {
                    elision.iterator_destructuring_assignment_evaluation(chunk)?;
                    unwind2 = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                are.iterator_destructuring_assignment_evaluation(chunk, strict, text)?;
                chunk.op(Insn::EmptyIfNotError);
                chunk.op(Insn::IteratorCloseIfNotDone);
                chunk.op(Insn::UpdateEmpty);
                let exit = chunk.op_jump(Insn::Jump);
                if let Some(unwind2) = unwind2 {
                    chunk.fixup(unwind2)?;
                    chunk.op_plus_arg(Insn::Unwind, 1);
                }
                chunk.fixup(unwind)?;
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit).expect("jump too short to fail");

                Ok(AlwaysAbruptResult)
            }
            ArrayAssignmentPattern::ListOnly { ael, .. } => {
                // ArrayAssignmentPattern : [ AssignmentElementList ]
                //  1. Let iteratorRecord be ? GetIterator(value, sync).
                //  2. Let result be Completion(IteratorDestructuringAssignmentEvaluation of AssignmentElementList with
                //     argument iteratorRecord).
                //  3. If iteratorRecord.[[Done]] is false, return ? IteratorClose(iteratorRecord, result).
                //  4. Return result.

                // start:                                                    value
                //   DUP                                                     value value
                //   GET_SYNC_ITER                                           ir/err value
                //   JUMP_IF_ABRUPT unwind                                   ir value
                //   DUP                                                     ir ir value
                //   <ael.iterator_destructuring_assignment_evaluation>      ir/err ir value
                //   EMPTY_IF_NOT_ERR                                        [empty]/err ir value
                //   IR_CLOSE_IF_NOT_DONE                                    [empty]/err value
                //   UPDATE_EMPTY                                            value/err
                //   JUMP exit
                // unwind:                                                   err value
                //   UNWIND 1                                                err
                // exit:                                                     value/err

                chunk.op(Insn::Dup);
                chunk.op(Insn::GetSyncIterator);
                let unwind = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Dup);
                ael.iterator_destructuring_assignment_evaluation(chunk, strict, text)?;
                chunk.op(Insn::EmptyIfNotError);
                chunk.op(Insn::IteratorCloseIfNotDone);
                chunk.op(Insn::UpdateEmpty);
                let exit = chunk.op_jump(Insn::Jump);
                chunk.fixup(unwind)?;
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit).expect("jump too short to fail");

                Ok(AlwaysAbruptResult)
            }
            ArrayAssignmentPattern::ListRest { ael, elision, are, .. } => {
                // ArrayAssignmentPattern : [ AssignmentElementList , Elisionopt AssignmentRestElementopt ]
                //  1. Let iteratorRecord be ? GetIterator(value, sync).
                //  2. Let status be Completion(IteratorDestructuringAssignmentEvaluation of AssignmentElementList with
                //     argument iteratorRecord).
                //  3. If status is an abrupt completion, then
                //      a. If iteratorRecord.[[Done]] is false, return ? IteratorClose(iteratorRecord, status).
                //      b. Return ? status.
                //  4. If Elision is present, then
                //      a. Set status to Completion(IteratorDestructuringAssignmentEvaluation of Elision with argument
                //         iteratorRecord).
                //      b. If status is an abrupt completion, then
                //          i. Assert: iteratorRecord.[[Done]] is true.
                //          ii. Return ? status.
                //  5. If AssignmentRestElement is present, then
                //      a. Set status to Completion(IteratorDestructuringAssignmentEvaluation of AssignmentRestElement
                //         with argument iteratorRecord).
                //  6. If iteratorRecord.[[Done]] is false, return ? IteratorClose(iteratorRecord, status).
                //  7. Return ? status.

                // start:                                                    value
                //   DUP                                                     value value
                //   GET_SYNC_ITER                                           ir/err value
                //   JUMP_IF_ABRUPT unwind                                   ir value
                //   DUP                                                     ir ir value
                //   <ael.iterator_destructuring_assignment_evaluation>      ir/err ir value
                //   JUMP_IF_NORMAL after_list                               status ir value  [status is abrupt]
                //   IR_CLOSE_IF_NOT_DONE                                    status/err value
                //   JUMP unwind
                // after_list:                                               ir ir value  [status = [empty]]
                // ----- elision present -----
                //   <elision.iterator_destructuring_assignment_evaluation>  ir/err ir value
                //   JUMP_IF_ABRUPT unwind2                                  ir ir value  [status = [empty]]
                // -----
                // ----- AssignmentRestElement present -----
                //   <are.iterator_destructuring_assignment_evaluation>      ir/err ir value [status = [empty]/err]
                // -----
                // _:                                                        ir/err ir value [status = [empty]/err]
                //   EMPTY_IF_NOT_ERR                                        [empty]/err ir value
                //   IR_CLOSE_IF_NOT_DONE                                    [empty]/err value
                //   UPDATE_EMPTY                                            value/err
                //   JUMP exit
                // ----- elision present -----
                // unwind2:                                                  err ir value
                //   UNWIND 1                                                err value
                // -----
                // unwind:                                                   err value
                //   UNWIND 1                                                err
                // exit:                                                     value/err
                chunk.op(Insn::Dup);
                chunk.op(Insn::GetSyncIterator);
                let unwind = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Dup);
                ael.iterator_destructuring_assignment_evaluation(chunk, strict, text)?;
                let after_list = chunk.op_jump(Insn::JumpIfNormal);
                chunk.op(Insn::IteratorCloseIfNotDone);
                let unwind_alt = chunk.op_jump(Insn::Jump);
                chunk.fixup(after_list).expect("Jump too short to fail");

                let unwind2 = if let Some(elision) = elision.as_ref() {
                    elision.iterator_destructuring_assignment_evaluation(chunk)?;
                    Some(chunk.op_jump(Insn::JumpIfAbrupt))
                } else {
                    None
                };

                if let Some(are) = are.as_ref() {
                    are.iterator_destructuring_assignment_evaluation(chunk, strict, text)?;
                }

                chunk.op(Insn::EmptyIfNotError);
                chunk.op(Insn::IteratorCloseIfNotDone);
                chunk.op(Insn::UpdateEmpty);
                let exit = chunk.op_jump(Insn::Jump);

                if let Some(unwind2) = unwind2 {
                    chunk.fixup(unwind2)?;
                    chunk.op_plus_arg(Insn::Unwind, 1);
                }
                chunk.fixup(unwind)?;
                chunk.fixup(unwind_alt).expect("If unwind worked, unwind_alt will work");
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit).expect("Jump too short to fail");

                Ok(AlwaysAbruptResult)
            }
        }
    }
}

impl AssignmentRestElement {
    pub fn iterator_destructuring_assignment_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: IteratorDestructuringAssignmentEvaluation The syntax-directed operation
        // IteratorDestructuringAssignmentEvaluation takes argument iteratorRecord (an Iterator Record) and returns
        // either a normal completion containing unused or an abrupt completion.
        //
        // (Altered in rust-e262 to return either a normal completion containing the input iteratorRecord, or an abrupt
        // completion.)
        //
        // AssignmentRestElement : ... DestructuringAssignmentTarget
        //
        //  1. If DestructuringAssignmentTarget is neither an ObjectLiteral nor an ArrayLiteral, then a. Let lref be ?
        //      Evaluation of DestructuringAssignmentTarget.
        //  2. Let A be ! ArrayCreate(0).
        //  3. Let n be 0.
        //  4. Repeat, while iteratorRecord.[[Done]] is false,
        //      a. Let next be Completion(IteratorStep(iteratorRecord)).
        //      b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
        //      c. ReturnIfAbrupt(next).
        //      d. If next is false, then
        //          i. Set iteratorRecord.[[Done]] to true.
        //      e. Else,
        //          i. Let nextValue be Completion(IteratorValue(next)).
        //          ii. If nextValue is an abrupt completion, set iteratorRecord.[[Done]] to true.
        //          iii. ReturnIfAbrupt(nextValue).
        //          iv. Perform ! CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), nextValue).
        //          v. Set n to n + 1.
        //  5. If DestructuringAssignmentTarget is neither an ObjectLiteral nor an ArrayLiteral, then
        //      a. Return ? PutValue(lref, A).
        //  6. Let nestedAssignmentPattern be the AssignmentPattern that is covered by DestructuringAssignmentTarget.
        //  7. Return ? DestructuringAssignmentEvaluation of nestedAssignmentPattern with argument A.

        // start:                                    ir
        //   --- not a/o literal
        //   <dat.evaluate>                          lref/err ir
        //   JUMP_IF_ABRUPT unwind                   lref ir
        //   SWAP                                    ir lref
        //   ---                                     ir lref/--
        //   ITER_REST                               (A ir)/err lref/--
        //   --- not a/o literal
        //   JUMP_IF_ABRUPT unwind                   A ir lref
        //   ROTATE_UP 3                             lref A ir
        //   SWAP                                    A lref ir
        //   PUT_VALUE                               [empty]/err ir
        //   -- else
        //   JUMP_IF_ABRUPT exit                     A ir
        //   <dat.ap.destructuring_assignment_evaluation>  A/err ir
        //   EMPTY_IF_NOT_ERROR                      [empty]/err ir
        //   ---
        //   UPDATE_EMPTY                            ir/err
        //   JUMP exit
        // unwind:                                   err ir/lref
        //   UNWIND 1                                err
        // exit:                                     ir/err

        match self.0.as_ref() {
            DestructuringAssignmentTarget::LeftHandSideExpression(lhse) => {
                let status = lhse.compile(chunk, strict, text)?;
                let unwind_a = if status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                chunk.op(Insn::Swap);
                chunk.op(Insn::IteratorRest);
                let unwind_b = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op_plus_arg(Insn::RotateUp, 3);
                chunk.op(Insn::Swap);
                chunk.op(Insn::PutValue);
                chunk.op(Insn::UpdateEmpty);
                let exit = chunk.op_jump(Insn::Jump);
                if let Some(unwind_a) = unwind_a {
                    chunk.fixup(unwind_a).expect("jump too short to fail");
                }
                chunk.fixup(unwind_b).expect("jump too short to fail");
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit).expect("jump too short to fail");
                Ok(AlwaysAbruptResult)
            }
            DestructuringAssignmentTarget::AssignmentPattern(ap) => {
                chunk.op(Insn::IteratorRest);
                let unwind = chunk.op_jump(Insn::JumpIfAbrupt);
                ap.destructuring_assignment_evaluation(chunk, strict, text)?;
                chunk.op(Insn::EmptyIfNotError);
                chunk.op(Insn::UpdateEmpty);
                let exit = chunk.op_jump(Insn::Jump);
                chunk.fixup(unwind).expect("Jump too short to fail");
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit).expect("jump too short fo fail");
                Ok(AlwaysAbruptResult)
            }
        }
    }
}

impl AssignmentElementList {
    pub fn iterator_destructuring_assignment_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: IteratorDestructuringAssignmentEvaluation The syntax-directed operation
        // IteratorDestructuringAssignmentEvaluation takes argument iteratorRecord (an Iterator Record) and returns
        // either a normal completion containing unused or an abrupt completion.
        //
        // (Altered in rust-e262 to return either a normal completion containing the input iteratorRecord, or an abrupt
        // completion.)
        //

        match self {
            AssignmentElementList::Item(item) => {
                // AssignmentElementList : AssignmentElisionElement
                //  1. Return ? IteratorDestructuringAssignmentEvaluation of AssignmentElisionElement with argument
                //     iteratorRecord.
                item.iterator_destructuring_assignment_evaluation(chunk, strict, text)
            }
            AssignmentElementList::List(list, item) => {
                // AssignmentElementList : AssignmentElementList , AssignmentElisionElement
                //  1. Perform ? IteratorDestructuringAssignmentEvaluation of AssignmentElementList with argument
                //     iteratorRecord.
                //  2. Return ? IteratorDestructuringAssignmentEvaluation of AssignmentElisionElement with argument
                //     iteratorRecord.

                // start:                                                 ir
                //   <list.iterator_destructuring_assignment_evaluation>   ir/err
                //   JUMP_IF_ABRUPT exit                                  ir
                //   <item.iterator_destructuring_assignment_evaluation>   ir/err
                //   exit:
                list.iterator_destructuring_assignment_evaluation(chunk, strict, text)?;
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                item.iterator_destructuring_assignment_evaluation(chunk, strict, text)?;
                chunk.fixup(exit)?;
                Ok(AlwaysAbruptResult)
            }
        }
    }
}

impl AssignmentElisionElement {
    pub fn iterator_destructuring_assignment_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: IteratorDestructuringAssignmentEvaluation The syntax-directed operation
        // IteratorDestructuringAssignmentEvaluation takes argument iteratorRecord (an Iterator Record) and returns
        // either a normal completion containing unused or an abrupt completion.
        //
        // (Altered in rust-e262 to return either a normal completion containing the input iteratorRecord, or an abrupt
        // completion.)
        //
        match &self.elisions {
            Some(elision) => {
                // AssignmentElisionElement : Elision AssignmentElement
                //  1. Perform ? IteratorDestructuringAssignmentEvaluation of Elision with argument iteratorRecord.
                //  2. Return ? IteratorDestructuringAssignmentEvaluation of AssignmentElement with argument
                //     iteratorRecord.
                // start:                                                     ir
                //   <elision.iterator_destructuring_assignment_evaluation>   ir/err
                //   JUMP_IF_ABRUPT exit                                      ir
                //   <item.iterator_destructuring_assignment_evaluation>      ir/err
                //   exit:
                elision.iterator_destructuring_assignment_evaluation(chunk)?;
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                self.element.iterator_destructuring_assignment_evaluation(chunk, strict, text)?;
                chunk.fixup(exit)?;
                Ok(AlwaysAbruptResult)
            }
            None => {
                // AssignmentElisionElement : AssignmentElement
                //  1. Return ? IteratorDestructuringAssignmentEvaluation of AssignmentElement with argument
                //     iteratorRecord.
                self.element.iterator_destructuring_assignment_evaluation(chunk, strict, text)
            }
        }
    }
}

impl AssignmentElement {
    pub fn iterator_destructuring_assignment_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: IteratorDestructuringAssignmentEvaluation The syntax-directed operation
        // IteratorDestructuringAssignmentEvaluation takes argument iteratorRecord (an Iterator Record) and returns
        // either a normal completion containing unused or an abrupt completion.
        //
        // (Altered in rust-e262 to return either a normal completion containing the input iteratorRecord, or an abrupt
        // completion.)
        //
        // AssignmentElement : DestructuringAssignmentTarget Initializeropt
        //  1. If DestructuringAssignmentTarget is neither an ObjectLiteral nor an ArrayLiteral, then
        //      a. Let lref be ? Evaluation of DestructuringAssignmentTarget.
        //  2. If iteratorRecord.[[Done]] is false, then
        //      a. Let next be Completion(IteratorStep(iteratorRecord)).
        //      b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
        //      c. ReturnIfAbrupt(next).
        //      d. If next is false, then
        //          i. Set iteratorRecord.[[Done]] to true.
        //      e. Else,
        //          i. Let value be Completion(IteratorValue(next)).
        //          ii. If value is an abrupt completion, set iteratorRecord.[[Done]] to true.
        //          iii. ReturnIfAbrupt(value).
        //  3. If iteratorRecord.[[Done]] is true, let value be undefined.
        //  4. If Initializer is present and value is undefined, then
        //      a. If IsAnonymousFunctionDefinition(Initializer) is true and IsIdentifierRef of
        //         DestructuringAssignmentTarget is true, then
        //          i. Let v be ? NamedEvaluation of Initializer with argument lref.[[ReferencedName]].
        //      b. Else,
        //          i. Let defaultValue be ? Evaluation of Initializer.
        //          ii. Let v be ? GetValue(defaultValue).
        //  5. Else,
        //      a. Let v be value.
        //  6. If DestructuringAssignmentTarget is either an ObjectLiteral or an ArrayLiteral, then
        //      a. Let nestedAssignmentPattern be the AssignmentPattern that is covered by
        //         DestructuringAssignmentTarget.
        //      b. Return ? DestructuringAssignmentEvaluation of nestedAssignmentPattern with argument v.
        //  7. Return ? PutValue(lref, v).

        // start:                                         ir
        // --- if not pattern
        //   <dat.evaluate>                               lref/err ir
        //   JUMP_IF_ABRUPT unwind_1                      lref ir
        //   SWAP                                         ir lref
        // --- endif
        //   IR_STEP                                      PATTERN: [ err or (value ir) ]; NOTPATTERN: [ (err or (value ir)) lref ]
        // --- if not pattern
        //   JUMP_IF_ABRUPT unwind_1                      value ir lref
        //   SWAP                                         ir value lref
        //   ROTATE_DOWN 3                                value lref ir
        // --- else if pattern
        //   JUMP_IF_ABRUPT exit                          value ir
        // --- endif
        // --- if initializer is present
        //   JUMP_IF_NOT_UNDEF has_value
        //   POP                                          PATTERN: [ ir ]; NOTPATTERN: [ lref ir ]
        // --- if is_anonymous_function && is_identifier_ref
        //   <izer.named_evaluation(dat.referenced_name)> v/err ir
        // --- else
        //   <izer.evaluate>                              PATTERN: [ vref/err ir ]; NOTPATTERN: [ vref/err lref ir ]
        //   GET_VALUE                                    PATTERN: [ v/err ir ]; NOTPATTERN: [ v/err lref ir ]
        // --- endif (anonymous)
        // --- if not pattern
        //   JUMP_IF_ABRUPT unwind_2                      v lref ir
        // --- else if pattern
        //   JUMP_IF_ABRUPT unwind_1                      v ir
        // --- endif
        // has_value:                                     PATTERN: [ v ir ]; NOTPATTERN: [ v lref ir ]
        // --- endif (initializer)
        // --- if pattern
        //   <dat.destructuring_assignment_evaluation>    v/err ir
        //   EMPTY_IF_NOT_ERROR                           [empty]/err ir
        // --- else
        //   PUT_VALUE                                    [empty]/err ir
        // --- endif
        //   UPDATE_EMPTY                                 ir/err
        //   JUMP exit
        // --- if not pattern
        // unwind_2:                                      err lref ir
        //   UNWIND 1                                     err ir
        // --- endif
        // unwind_1:                                      err ir
        //   UNWIND 1                                     err
        // exit:                                          ir/err
        let dat = &self.target;
        let mut unwind_1_a = None;
        let not_pattern = if let DestructuringAssignmentTarget::LeftHandSideExpression(lhse) = dat.as_ref() {
            let lhse_status = lhse.compile(chunk, strict, text)?;
            if lhse_status.maybe_abrupt() {
                unwind_1_a = Some(chunk.op_jump(Insn::JumpIfAbrupt));
            }
            chunk.op(Insn::Swap);
            true
        } else {
            false
        };
        chunk.op(Insn::EmbellishedIteratorStep);
        let mut unwind_1_b = None;
        let mut exit_a = None;
        if not_pattern {
            unwind_1_b = Some(chunk.op_jump(Insn::JumpIfAbrupt));
            chunk.op(Insn::Swap);
            chunk.op_plus_arg(Insn::RotateDown, 3);
        } else {
            exit_a = Some(chunk.op_jump(Insn::JumpIfAbrupt));
        }
        let mut unwind_2 = None;
        let mut unwind_1_c = None;
        if let Some(izer) = self.initializer.as_ref() {
            let has_value = chunk.op_jump(Insn::JumpIfNotUndef);
            chunk.op(Insn::Pop);
            let izer_status =
                if let (Some(np), Some(lhse_id)) = (izer.anonymous_function_definition(), dat.identifier_ref()) {
                    let idx = chunk.add_to_string_pool(lhse_id.string_value()).expect("would already have been added");
                    np.compile_named_evaluation(chunk, strict, text, Some(NameLoc::Index(idx)))?
                } else {
                    let status = izer.compile(chunk, strict, text, CompileMod::Unmodified)?;
                    if status.maybe_ref() {
                        chunk.op(Insn::GetValue);
                    }
                    status
                };
            if izer_status.maybe_abrupt() {
                let target = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                if not_pattern {
                    unwind_2 = target;
                } else {
                    unwind_1_c = target;
                }
            }
            chunk.fixup(has_value)?;
        }
        if not_pattern {
            chunk.op(Insn::PutValue);
        } else {
            dat.destructuring_assignment_evaluation(chunk, strict, text)?;
            chunk.op(Insn::EmptyIfNotError);
        }
        chunk.op(Insn::UpdateEmpty);
        let mut exit_b = None;
        let unwind_1_needed =
            unwind_1_a.is_some() || unwind_1_b.is_some() || unwind_1_c.is_some() || unwind_2.is_some();
        if unwind_1_needed {
            exit_b = Some(chunk.op_jump(Insn::Jump));

            if let Some(unwind_2) = unwind_2 {
                chunk.fixup(unwind_2).expect("jump is too short to fail");
                chunk.op_plus_arg(Insn::Unwind, 1);
            }
            if let Some(unwind_1) = unwind_1_a {
                chunk.fixup(unwind_1)?;
            }
            if let Some(unwind_1) = unwind_1_b {
                chunk.fixup(unwind_1)?;
            }
            if let Some(unwind_1) = unwind_1_c {
                chunk.fixup(unwind_1)?;
            }
            chunk.op_plus_arg(Insn::Unwind, 1);
        }
        if let Some(exit) = exit_a {
            chunk.fixup(exit)?;
        }
        if let Some(exit) = exit_b {
            chunk.fixup(exit).expect("jump too short to fail");
        }
        Ok(AlwaysAbruptResult)
    }

    fn keyed_destructuring_assignment_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: KeyedDestructuringAssignmentEvaluation
        // The syntax-directed operation KeyedDestructuringAssignmentEvaluation takes arguments value (an ECMAScript
        // language value) and propertyName (a property key) and returns either a normal completion containing unused or
        // an abrupt completion. It is defined piecewise over the following productions:
        //
        // AssignmentElement : DestructuringAssignmentTarget Initializeropt
        //  1. If DestructuringAssignmentTarget is neither an ObjectLiteral nor an ArrayLiteral, then
        //      a. Let lref be ? Evaluation of DestructuringAssignmentTarget.
        //  2. Let v be ? GetV(value, propertyName).
        //  3. If Initializer is present and v is undefined, then
        //      a. If IsAnonymousFunctionDefinition(Initializer) and IsIdentifierRef of DestructuringAssignmentTarget
        //         are both true, then
        //          i. Let rhsValue be ? NamedEvaluation of Initializer with argument lref.[[ReferencedName]].
        //      b. Else,
        //          i. Let defaultValue be ? Evaluation of Initializer.
        //          ii. Let rhsValue be ? GetValue(defaultValue).
        //  4. Else,
        //      a. Let rhsValue be v.
        //  5. If DestructuringAssignmentTarget is either an ObjectLiteral or an ArrayLiteral, then
        //      a. Let assignmentPattern be the AssignmentPattern that is covered by DestructuringAssignmentTarget.
        //      b. Return ? DestructuringAssignmentEvaluation of assignmentPattern with argument rhsValue.
        //  6. Return ? PutValue(lref, rhsValue).

        // start:                                 name value
        // -- if not pattern
        //   <dat.evaluate>                       lref/err name value
        //   JUMP_IF_ABRUPT unwind_2              lref name value
        //   ROTATE_DOWN 3                        name value lref
        // -- endif
        //   GETV                                 v/err lref/--
        // -- if not pattern
        //   JUMP_IF_ABRUPT unwind_1              v lref
        // -- else
        //   JUMP_IF_ABRUPT exit                  v
        // -- endif
        // -- if initializer present
        //   JUMP_IF_NOT_UNDEF has_value          undefined lref/--
        //   POP                                  lref/--
        // -- if named anonymous
        //   <izer.named_evaluation(lhse.name)>   rhsValue/err lref/--
        // -- else not named anonymous
        //   <izer.evaluate()>                    rref/err lref/--
        //   GET_VALUE                            rhsValue/err lref/--
        // -- endif
        // -- if not pattern
        //   JUMP_IF_ABRUPT unwind_1              rhsValue lref
        // -- else
        //   JUMP_IF_ABRUPT exit                  rhsValue
        // -- endif
        // has_value:                             rhsValue lref/--
        // -- endif
        // -- if not pattern
        //   PUT_VALUE                            [empty]/err
        // -- else
        //   <dat.destructuring_assignment_evaluation()> rhsValue/err
        //   EMPTY_IF_NOT_ERROR                   [empty]/err
        // -- endif
        //   JUMP exit
        // unwind_2:
        //   UNWIND 1
        // unwind_1:
        //   UNWIND 1
        // exit:

        let unwind_2 = if let DestructuringAssignmentTarget::LeftHandSideExpression(lhse) = self.target.as_ref() {
            let lhse_status = lhse.compile(chunk, strict, text)?;
            let mark = if lhse_status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
            chunk.op_plus_arg(Insn::RotateDown, 3);
            mark
        } else {
            None
        };
        chunk.op(Insn::GetV);
        let (unwind_1_a, exit_a) = {
            let mark = chunk.op_jump(Insn::JumpIfAbrupt);
            if matches!(self.target.as_ref(), DestructuringAssignmentTarget::LeftHandSideExpression(_)) {
                (Some(mark), None)
            } else {
                (None, Some(mark))
            }
        };
        let (unwind_1_b, exit_b) = if let Some(izer) = self.initializer.as_ref() {
            let has_value = chunk.op_jump(Insn::JumpIfNotUndef);
            chunk.op(Insn::Pop);
            let status = if let (Some(np), Some(lhse_id)) =
                (izer.anonymous_function_definition(), self.target.as_ref().identifier_ref())
            {
                let id = chunk
                    .add_to_string_pool(lhse_id.string_value())
                    .expect("id will have been added during lhse compile");
                chunk.op_plus_arg(Insn::String, id);
                np.compile_named_evaluation(chunk, strict, text, Some(NameLoc::Index(id)))?
            } else {
                izer.compile(chunk, strict, text, CompileMod::Unmodified)?
            };
            if status.maybe_ref() {
                chunk.op(Insn::GetValue);
            }

            let (unwind_1, exit) = if status.maybe_ref() || status.maybe_abrupt() {
                let mark = chunk.op_jump(Insn::JumpIfAbrupt);
                if matches!(self.target.as_ref(), DestructuringAssignmentTarget::LeftHandSideExpression(_)) {
                    (Some(mark), None)
                } else {
                    (None, Some(mark))
                }
            } else {
                (None, None)
            };

            chunk.fixup(has_value)?;

            (unwind_1, exit)
        } else {
            (None, None)
        };

        match self.target.as_ref() {
            DestructuringAssignmentTarget::LeftHandSideExpression(_) => {
                chunk.op(Insn::PutValue);
            }
            DestructuringAssignmentTarget::AssignmentPattern(ap) => {
                ap.destructuring_assignment_evaluation(chunk, strict, text)?;
                chunk.op(Insn::EmptyIfNotError);
            }
        }
        let exit_c = if unwind_2.is_some() || unwind_1_a.is_some() || unwind_1_b.is_some() {
            Some(chunk.op_jump(Insn::Jump))
        } else {
            None
        };

        let has_unwind1 = unwind_1_a.is_some();
        if let Some(unwind) = unwind_2 {
            assert!(has_unwind1);
            chunk.fixup(unwind)?;
            chunk.op_plus_arg(Insn::Unwind, 1);
        }
        if let Some(unwind) = unwind_1_a {
            chunk.fixup(unwind)?;
            if let Some(unwind) = unwind_1_b {
                chunk.fixup(unwind).expect("jump too short to fail");
            }
            chunk.op_plus_arg(Insn::Unwind, 1);
        }

        let has_exit_a = exit_a.is_some();
        if let Some(exit) = exit_a {
            chunk.fixup(exit)?;
        }
        if let Some(exit) = exit_b {
            assert!(has_exit_a);
            chunk.fixup(exit).expect("exit_a fit, so this one will too");
        }
        if let Some(exit) = exit_c {
            chunk.fixup(exit).expect("jump too short to fail");
        }

        Ok(AlwaysAbruptResult)
    }
}

impl<'a> TryFrom<&'a DestructuringAssignmentTarget> for &'a Rc<AssignmentPattern> {
    type Error = anyhow::Error;

    fn try_from(value: &'a DestructuringAssignmentTarget) -> Result<Self, Self::Error> {
        match value {
            DestructuringAssignmentTarget::LeftHandSideExpression(_) => {
                Err(anyhow!("lhs cannot be converted to pattern"))
            }
            DestructuringAssignmentTarget::AssignmentPattern(ap) => Ok(ap),
        }
    }
}

impl DestructuringAssignmentTarget {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            DestructuringAssignmentTarget::LeftHandSideExpression(lhse) => lhse.compile(chunk, strict, text),
            DestructuringAssignmentTarget::AssignmentPattern(_) => unreachable!(),
        }
    }
    pub fn destructuring_assignment_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        let pattern: &Rc<AssignmentPattern> = self.try_into()?;
        pattern.destructuring_assignment_evaluation(chunk, strict, text)
    }
}

impl Expression {
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
        for item in &self.list[1..] {
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
            Statement::Empty(_) => Ok(EmptyStatement::compile(chunk).into()),
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
            Declaration::Class(cls) => cls.compile(chunk, strict, text),
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
            FcnDef::Function(f) => f.compile_fo_instantiation(chunk, strict, text).map(AbruptResult::from),
            FcnDef::Generator(gen) => gen.compile_go_instantiation(chunk, strict, text).map(AbruptResult::from),
            FcnDef::AsyncFun(_) | FcnDef::AsyncGen(_) => todo!(),
        }
    }
}

fn block_declaration_instantiation(
    chunk: &mut Chunk,
    strict: bool,
    text: &str,
    declarations: &[DeclPart],
) -> anyhow::Result<()> {
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
        if let Ok(fcn) = FcnDef::try_from(d.clone()) {
            let fcn_name = fcn.bound_name();
            let string_idx =
                chunk.add_to_string_pool(fcn_name).expect("should find an index, as fcn_name is already in the pool");
            fcn.compile_fo_instantiation(chunk, strict, text)?;
            chunk.op_plus_arg(Insn::InitializeLexBinding, string_idx);
        }
    }
    Ok(())
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

                block_declaration_instantiation(chunk, strict, text, &declarations)?;

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
                            np.compile_named_evaluation(chunk, strict, text, Some(NameLoc::Index(id)))?
                        } else {
                            izer.compile(chunk, strict, text, CompileMod::Unmodified)?
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
            LexicalBinding::Pattern(bp, init) => {
                // LexicalBinding : BindingPattern Initializer
                // 1. Let rhs be ? Evaluation of Initializer.
                // 2. Let value be ? GetValue(rhs).
                // 3. Let env be the running execution context's LexicalEnvironment.
                // 4. Return ? BindingInitialization of BindingPattern with arguments value and env.

                // Stack starts empty
                //   <init>                             err/rhs
                //   GET_VALUE                          err/value
                //   JUMP_IF_ABRUPT exit                value
                //   <bp.binding_initialization(env)>   err/[empty]
                // exit:                                err/[empty]

                let status = init.compile(chunk, strict, text, CompileMod::Unmodified)?;
                if status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                let exit = if status.maybe_ref() || status.maybe_abrupt() {
                    Some(chunk.op_jump(Insn::JumpIfAbrupt))
                } else {
                    None
                };
                bp.compile_binding_initialization(chunk, strict, text, EnvUsage::UseCurrentLexical)?;
                if let Some(exit) = exit {
                    chunk.fixup(exit)?;
                }
                Ok(AlwaysAbruptResult)
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum CompileMod {
    Unmodified,
    AsFunction,
}

impl Initializer {
    pub fn compile(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        modifier: CompileMod,
    ) -> anyhow::Result<CompilerStatusFlags> {
        // Sometimes we compile initializers as functions. If that's the case, we need to pop off the arguments first,
        // return the value generated by the initializer, and mark the end of the function.
        if modifier == CompileMod::AsFunction {
            chunk.op(Insn::FinishArgs);
            self.ae.compile(chunk, strict, text)?;
            chunk.op(Insn::Return);
            chunk.op(Insn::EndFunction);
            Ok(CompilerStatusFlags::new().abrupt(true))
        } else {
            self.ae.compile(chunk, strict, text)
        }
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
        for item in &self.list[1..] {
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
                    np.compile_named_evaluation(chunk, strict, text, Some(NameLoc::Index(idx)))?
                } else {
                    izer.compile(chunk, strict, text, CompileMod::Unmodified)? // Stack: rhs/rref/err lhs ...
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

                let status = init.compile(chunk, strict, text, CompileMod::Unmodified)?;
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
    fn compile(chunk: &mut Chunk) -> NeverAbruptRefResult {
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
            chunk.op(Insn::GetValue);
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
            IterationStatement::ForInOf(f) => {
                f.for_in_of_evaluation(chunk, strict, text, label_set).map(AbruptResult::from)
            }
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
    #[expect(clippy::too_many_arguments)]
    fn compile_for_body(
        chunk: &mut Chunk,
        strict: bool,
        src_text: &str,
        test: Option<Rc<Expression>>,
        increment: Option<Rc<Expression>>,
        stmt: &Rc<Statement>,
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
            let status = test.compile(chunk, strict, src_text)?;
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
        let status = stmt.compile(chunk, strict, src_text)?;
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
            let status = increment.compile(chunk, strict, src_text)?;
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
        src_text: &str,
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
                    let status = init.compile(chunk, strict, src_text)?;
                    if status.maybe_ref() {
                        chunk.op(Insn::GetValue);
                    }
                    if status.maybe_ref() || status.maybe_abrupt() {
                        exit = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                        maybe_abrupt = AbruptResult::Maybe;
                    }
                    chunk.op(Insn::Pop);
                }
                let body_status =
                    Self::compile_for_body(chunk, strict, src_text, test.clone(), incr.clone(), stmt, &[], label_set)?;
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
                let vdl_status = vdl.compile(chunk, strict, src_text)?;
                if vdl_status.maybe_abrupt() {
                    exit = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                    maybe_abrupt = AbruptResult::Maybe;
                }
                chunk.op(Insn::Pop);
                let body_status =
                    Self::compile_for_body(chunk, strict, src_text, test.clone(), incr.clone(), stmt, &[], label_set)?;
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
                for bn in &bound_names {
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
                lexdecl.compile(chunk, strict, src_text)?;
                let popenv = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Pop);
                Self::compile_for_body(
                    chunk,
                    strict,
                    src_text,
                    test.clone(),
                    incr.clone(),
                    stmt,
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

#[derive(Copy, Clone)]
#[allow(dead_code)] // AsyncIterate will come
enum IterationKind {
    Enumerate,
    Iterate,
    AsyncIterate,
}

#[derive(Copy, Clone)]
enum ForInOfExpr<'a> {
    Expression(&'a Rc<Expression>),
    AssignmentExpression(&'a Rc<AssignmentExpression>),
}

impl<'a> From<&'a Rc<Expression>> for ForInOfExpr<'a> {
    fn from(value: &'a Rc<Expression>) -> Self {
        Self::Expression(value)
    }
}

impl<'a> From<&'a Rc<AssignmentExpression>> for ForInOfExpr<'a> {
    fn from(value: &'a Rc<AssignmentExpression>) -> Self {
        Self::AssignmentExpression(value)
    }
}

impl<'a> ForInOfExpr<'a> {
    fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        match self {
            ForInOfExpr::Expression(exp) => exp.compile(chunk, strict, text),
            ForInOfExpr::AssignmentExpression(ae) => ae.compile(chunk, strict, text),
        }
    }
}

#[derive(Copy, Clone)]
enum ForInOfLHSExpr<'a> {
    LeftHandSideExpression(&'a Rc<LeftHandSideExpression>),
    AssignmentPattern(&'a Rc<AssignmentPattern>),
    ForBinding(&'a Rc<ForBinding>),
    ForDeclaration(&'a Rc<ForDeclaration>),
}

impl<'a> From<&'a Rc<LeftHandSideExpression>> for ForInOfLHSExpr<'a> {
    fn from(value: &'a Rc<LeftHandSideExpression>) -> Self {
        Self::LeftHandSideExpression(value)
    }
}

impl<'a> From<&'a Rc<AssignmentPattern>> for ForInOfLHSExpr<'a> {
    fn from(value: &'a Rc<AssignmentPattern>) -> Self {
        Self::AssignmentPattern(value)
    }
}

impl<'a> From<&'a Rc<ForBinding>> for ForInOfLHSExpr<'a> {
    fn from(value: &'a Rc<ForBinding>) -> Self {
        Self::ForBinding(value)
    }
}

impl<'a> From<&'a Rc<ForDeclaration>> for ForInOfLHSExpr<'a> {
    fn from(value: &'a Rc<ForDeclaration>) -> Self {
        Self::ForDeclaration(value)
    }
}

impl<'a> ForInOfLHSExpr<'a> {
    fn is_destructuring(&self) -> bool {
        match self {
            ForInOfLHSExpr::LeftHandSideExpression(lhs) => lhs.is_destructuring(),
            ForInOfLHSExpr::AssignmentPattern(ap) => ap.is_destructuring(),
            ForInOfLHSExpr::ForBinding(fb) => fb.is_destructuring(),
            ForInOfLHSExpr::ForDeclaration(fd) => fd.is_destructuring(),
        }
    }
}

impl ForInOfStatement {
    fn for_in_of_head_evaluation(
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        uninitialized_bound_names: &[JSString],
        exp: ForInOfExpr,
        kind: IterationKind,
    ) -> anyhow::Result<AbruptResult> {
        // ForIn/OfHeadEvaluation ( uninitializedBoundNames, expr, iterationKind )
        // The abstract operation ForIn/OfHeadEvaluation takes arguments uninitializedBoundNames (a List of
        // Strings), expr (an Expression Parse Node or an AssignmentExpression Parse Node), and iterationKind
        // (enumerate, iterate, or async-iterate) and returns either a normal completion containing an
        // Iterator Record or an abrupt completion. It performs the following steps when called:
        //
        //  1. Let oldEnv be the running execution context's LexicalEnvironment.
        //  2. If uninitializedBoundNames is not empty, then
        //      a. Assert: uninitializedBoundNames has no duplicate entries.
        //      b. Let newEnv be NewDeclarativeEnvironment(oldEnv).
        //      c. For each String name of uninitializedBoundNames, do
        //          i. Perform ! newEnv.CreateMutableBinding(name, false).
        //      d. Set the running execution context's LexicalEnvironment to newEnv.
        //  3. Let exprRef be Completion(Evaluation of expr).
        //  4. Set the running execution context's LexicalEnvironment to oldEnv.
        //  5. Let exprValue be ? GetValue(? exprRef).
        //  6. If iterationKind is enumerate, then
        //      a. If exprValue is either undefined or null, then
        //          i. Return Completion Record { [[Type]]: break, [[Value]]: empty, [[Target]]: empty }.
        //      b. Let obj be ! ToObject(exprValue).
        //      c. Let iterator be EnumerateObjectProperties(obj).
        //      d. Let nextMethod be ! GetV(iterator, "next").
        //      e. Return the Iterator Record { [[Iterator]]: iterator, [[NextMethod]]: nextMethod, [[Done]]:
        //         false }.
        //  7. Else,
        //      a. Assert: iterationKind is either iterate or async-iterate.
        //      b. If iterationKind is async-iterate, let iteratorKind be async.
        //      c. Else, let iteratorKind be sync.
        //      d. Return ? GetIterator(exprValue, iteratorKind).

        // start:
        // --- If uninitialized_bound_names ---
        //   PNLE
        //   --- for each "name" of those names
        //   CPMLB <name>
        //   ---
        // ---
        //   <expr>                    exprRef/err
        // --- if uninitialized_bound_names ---
        //   PLE
        // ---
        //   GET_VALUE                 exprVal/err
        //   JUMP_IF_ABRUPT exit       exprVal
        // --- if iteration_kind == enumerate
        //   JUMP_IF_NULLISH break
        //   TO_OBJECT                 obj
        //   ENUMERATE_OBJ_PROPS       ir
        //   JUMP exit
        // break:                      undef/null
        //   POP
        //   BREAK                     break
        // exit:
        // --- else if iteration_kind == iterate
        //   GET_SYNC_ITERATOR         ir/err
        // exit:
        // -- else if iteration_kind == async-iterate
        //   GET_ASYNC_ITERATOR        ir/err
        // exit:

        let exp_status = if uninitialized_bound_names.is_empty() {
            exp.compile(chunk, strict, text)?
        } else {
            chunk.op(Insn::PushNewLexEnv);
            for name in uninitialized_bound_names.iter().cloned() {
                let idx = chunk.add_to_string_pool(name)?;
                chunk.op_plus_arg(Insn::CreatePermanentMutableLexBinding, idx);
            }
            let exp_status = exp.compile(chunk, strict, text)?;
            chunk.op(Insn::PopLexEnv);
            exp_status
        };
        if exp_status.maybe_ref() {
            chunk.op(Insn::GetValue);
        }
        let mut exits = vec![];
        if exp_status.maybe_abrupt() || exp_status.maybe_ref() {
            exits.push(chunk.op_jump(Insn::JumpIfAbrupt));
        }
        let status = match kind {
            IterationKind::Enumerate => {
                let break_tgt = chunk.op_jump(Insn::JumpIfNullish);
                chunk.op(Insn::ToObject);
                chunk.op(Insn::EnumerateObjectProperties);
                exits.push(chunk.op_jump(Insn::Jump));
                chunk.fixup(break_tgt).expect("jump too short to fail");
                chunk.op(Insn::Pop);
                chunk.op(Insn::Break);
                AbruptResult::from(exp_status.maybe_abrupt() || exp_status.maybe_ref())
            }
            IterationKind::Iterate => {
                chunk.op(Insn::GetSyncIterator);
                AbruptResult::Maybe
            }
            IterationKind::AsyncIterate => {
                chunk.op(Insn::ToDo);
                AbruptResult::Maybe
            }
        };
        for exit in exits {
            chunk.fixup(exit).expect("Jumps too short to fail");
        }
        Ok(status)
    }

    #[expect(clippy::too_many_arguments)]
    fn for_in_of_body_evaluation(
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        lhs: ForInOfLHSExpr,
        stmt: &Rc<Statement>,
        i_kind: IterationKind,
        label_set: &[JSString],
        iterator_kind: IteratorKind,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // ForIn/OfBodyEvaluation ( lhs, stmt, iteratorRecord, iterationKind, lhsKind, labelSet [ , iteratorKind ] )
        // The abstract operation ForIn/OfBodyEvaluation takes arguments lhs (a Parse Node), stmt (a Statement
        // Parse Node), iteratorRecord (an Iterator Record), iterationKind (enumerate or iterate), lhsKind
        // (assignment, varBinding, or lexicalBinding), and labelSet (a List of Strings) and optional argument
        // iteratorKind (sync or async) and returns either a normal completion containing an ECMAScript
        // language value or an abrupt completion. It performs the following steps when called:
        //
        //  1. If iteratorKind is not present, set iteratorKind to sync.
        //  2. Let oldEnv be the running execution context's LexicalEnvironment.
        //  3. Let V be undefined.
        //  4. Let destructuring be IsDestructuring of lhs.
        //  5. If destructuring is true and lhsKind is assignment, then
        //      a. Assert: lhs is a LeftHandSideExpression.
        //      b. Let assignmentPattern be the AssignmentPattern that is covered by lhs.
        //  6. Repeat,
        //      a. Let nextResult be ? Call(iteratorRecord.[[NextMethod]], iteratorRecord.[[Iterator]]).
        //      b. If iteratorKind is async, set nextResult to ? Await(nextResult).
        //      c. If nextResult is not an Object, throw a TypeError exception.
        //      d. Let done be ? IteratorComplete(nextResult).
        //      e. If done is true, return V.
        //      f. Let nextValue be ? IteratorValue(nextResult).
        //      g. If lhsKind is either assignment or varBinding, then
        //          i. If destructuring is true, then
        //              1. If lhsKind is assignment, then
        //                  a. Let status be Completion(DestructuringAssignmentEvaluation of assignmentPattern
        //                     with argument nextValue).
        //              2. Else,
        //                  a. Assert: lhsKind is varBinding.
        //                  b. Assert: lhs is a ForBinding.
        //                  c. Let status be Completion(BindingInitialization of lhs with arguments nextValue
        //                     and undefined).
        //          ii. Else,
        //              1. Let lhsRef be Completion(Evaluation of lhs). (It may be evaluated repeatedly.)
        //              2. If lhsRef is an abrupt completion, then
        //                  a. Let status be lhsRef.
        //              3. Else,
        //                  a. Let status be Completion(PutValue(lhsRef.[[Value]], nextValue)).
        //      h. Else,
        //          i. Assert: lhsKind is lexicalBinding.
        //          ii. Assert: lhs is a ForDeclaration.
        //          iii. Let iterationEnv be NewDeclarativeEnvironment(oldEnv).
        //          iv. Perform ForDeclarationBindingInstantiation of lhs with argument iterationEnv.
        //          v. Set the running execution context's LexicalEnvironment to iterationEnv.
        //          vi. If destructuring is true, then
        //              1. Let status be Completion(ForDeclarationBindingInitialization of lhs with arguments
        //                 nextValue and iterationEnv).
        //          vii. Else,
        //              1. Assert: lhs binds a single name.
        //              2. Let lhsName be the sole element of BoundNames of lhs.
        //              3. Let lhsRef be ! ResolveBinding(lhsName).
        //              4. Let status be Completion(InitializeReferencedBinding(lhsRef, nextValue)).
        //      i. If status is an abrupt completion, then
        //          i. Set the running execution context's LexicalEnvironment to oldEnv.
        //          ii. If iteratorKind is async, return ? AsyncIteratorClose(iteratorRecord, status).
        //          iii. If iterationKind is enumerate, then
        //              1. Return ? status.
        //          iv. Else,
        //              1. Assert: iterationKind is iterate.
        //              2. Return ? IteratorClose(iteratorRecord, status).
        //      j. Let result be Completion(Evaluation of stmt).
        //      k. Set the running execution context's LexicalEnvironment to oldEnv.
        //      l. If LoopContinues(result, labelSet) is false, then
        //          i. If iterationKind is enumerate, then
        //              1. Return ? UpdateEmpty(result, V).
        //          ii. Else,
        //              1. Assert: iterationKind is iterate.
        //              2. Set status to Completion(UpdateEmpty(result, V)).
        //              3. If iteratorKind is async, return ? AsyncIteratorClose(iteratorRecord, status).
        //              4. Return ? IteratorClose(iteratorRecord, status).
        //      m. If result.[[Value]] is not empty, set V to result.[[Value]].

        // start:                              iter_record
        //   UNDEFINED                         v iter_record
        // top:                                v iter_record
        //   SWAP                              iter_record v
        // --- if iteratorKind != ASYNC
        //   ITER_NEXT_SYNC                    next_result/err iter_record v
        // --- else
        //   ITER_NEXT_ASYNC
        // ---
        //   JUMP_IF_ABRUPT unwind2                   next_result iter_record v
        //   IRES_COMPLETE                            done/err next_result iter_record v
        //   JUMP_IF_ABRUPT unwind3                   done next_result iter_record v
        //   JUMPPOP_IF_TRUE pop2_exit                next_result iter_record v
        //   IRES_TO_VALUE                            value/err iter_record v
        //   JUMP_IF_ABRUPT unwind2                   value iter_record v
        // --- if destructuring && lhsKind == assignment
        //   <lhs.destructuring_assignment_evaluation>   status iter_record v
        // --- else if destructuring && lhsKind == varBinding
        //   <lhs.binding_initialization(putvalue)>   status iter_record v
        // --- else if lhsKind == assignment || lhsKind == varBinding
        //   <lhs.execute>                            lhsRef/err value iter_record v
        //   JUMP_IF_ABRUPT inner_unwind              lhsRef value iter_record v
        //   SWAP                                     value lhsRef iter_record v
        //   PUT_VALUE                                status iter_record v
        // lhs_calc_done:                             status iter_record v
        // --- else (lhsKind == lexicalBinding)
        //   PNLE                                     value iter_record v
        //   <lhs.for_declaration_binding_instantiation> value iter_record v
        // ---
        // --- if destructuring && lhsKind == lexicalBinding
        //   <lhs.for_declaration_binding_initialization> status iter_record v
        // --- if !destructuring && lhsKind == lexicalBinding
        //   STRING lhs.bound_names()[0]              name value iter_record v
        //   RESOLVE/STRICT_RESOLVE                   ref value iter_record v
        //   SWAP                                     value ref iter_record v
        //   IRB                                      status iter_record v
        // ---
        //   JUMP_IF_NORMAL statements                err iter_record v
        // --- if lhsKind == lexicalBinding
        //   POP_LEX_ENV
        // ---
        // --- if iteratorKind == Async
        //   ROTATE_UP 3                              v err iter_record
        //   POP                                      err iter_record
        //   ASYNC_ITER_CLOSE                         status
        //   JUMP exit
        // ---
        // --- if iterationKind == Enumerate
        //   JUMP unwind2
        // --- else (iterationKind == Iterate)
        //   ROTATE_UP 3                              v err iter_record
        //   POP                                      err iter_record
        //   ITER_CLOSE                               status
        //   JUMP exit
        // ---
        // --- if !destructuring && [assignment, varBinding].contains(lhsKind)
        // inner_unwind:                              err value iter_record v
        //   UNWIND 1                                 status iter_record v
        //   JUMP lhs_calc_done
        // ---
        // statements:                                status iter_record v
        //   POP                                      iter_record v
        //   SWAP                                     v iter_record
        //   <stmt.evaluate>                          result v iter_record
        // --- if lhsKind == lexicalBinding
        //   POP_LEX_ENV
        // ---
        //   LOOP_CONT label_set                      true/false result v iter_record
        //   JUMPPOP_IF_FALSE loop_termination        result v iter_record
        //   COALESCE                                 v iter_record
        //   JUMP top
        // loop_termination:                          result v iter_record
        //   UPDATE_EMPTY                             status iter_record
        // --- if iterationKind == Enumerate
        //   UNWIND 1                                 status
        //   JUMP exit
        // --- else
        //   --- if iteratorKind == Async
        //   ASYNC_ITER_CLOSE                         status
        //   --- else
        //   ITER_CLOSE                               status
        //   ---
        //   JUMP exit
        // ---
        // pop2_exit:
        //   POP
        //   POP
        //   JUMP exit
        // unwind3:
        //   UNWIND 1
        // unwind2:
        //   UNWIND 2
        // exit:

        let destructuring = lhs.is_destructuring();

        chunk.op(Insn::Undefined);
        let top = chunk.pos();
        chunk.op(Insn::Swap);
        chunk.op(match iterator_kind {
            IteratorKind::Sync => Insn::IteratorNext,
            IteratorKind::Async => Insn::ToDo, // Insn::AsyncIteratorNext
        });
        let mut unwind2s = vec![];
        unwind2s.push(chunk.op_jump(Insn::JumpIfAbrupt));
        chunk.op(Insn::IteratorResultComplete);
        let unwind3 = chunk.op_jump(Insn::JumpIfAbrupt);
        let pop2_exit = chunk.op_jump(Insn::JumpPopIfTrue);
        chunk.op(Insn::IteratorResultToValue);
        unwind2s.push(chunk.op_jump(Insn::JumpIfAbrupt));

        let mut lhs_assignment_spots = None;

        match lhs {
            ForInOfLHSExpr::AssignmentPattern(lhs) => {
                assert!(destructuring);
                lhs.destructuring_assignment_evaluation(chunk, strict, text)?;
            }
            ForInOfLHSExpr::ForBinding(fb) => {
                if destructuring {
                    fb.binding_initialization(chunk, strict, text, EnvUsage::UsePutValue)?;
                } else {
                    fb.compile(chunk, strict)?;
                    let inner_unwind = chunk.op_jump(Insn::JumpIfAbrupt);
                    chunk.op(Insn::Swap);
                    chunk.op(Insn::PutValue);
                    let lhs_calc_done = chunk.pos();
                    lhs_assignment_spots = Some((inner_unwind, lhs_calc_done));
                }
            }
            ForInOfLHSExpr::LeftHandSideExpression(lhs) => {
                assert!(!destructuring);
                lhs.compile(chunk, strict, text)?;
                let inner_unwind = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Swap);
                chunk.op(Insn::PutValue);
                let lhs_calc_done = chunk.pos();
                lhs_assignment_spots = Some((inner_unwind, lhs_calc_done));
            }
            ForInOfLHSExpr::ForDeclaration(lhs) => {
                chunk.op(Insn::PushNewLexEnv);
                lhs.for_declaration_binding_instantiation(chunk)?;
                if destructuring {
                    lhs.for_declaration_binding_initialization(chunk, strict, text, EnvUsage::UseCurrentLexical)?;
                } else {
                    let mut bn = lhs.bound_names();
                    let name = chunk
                        .add_to_string_pool(bn.pop().expect("should be exactly one name"))
                        .expect("names should have already been added to string pool in instantiation");
                    chunk.op_plus_arg(Insn::String, name);
                    chunk.op(if strict { Insn::StrictResolve } else { Insn::Resolve });
                    chunk.op(Insn::Swap);
                    chunk.op(Insn::InitializeReferencedBinding);
                }
            }
        }
        let statements = chunk.op_jump(Insn::JumpIfNormal);
        if matches!(lhs, ForInOfLHSExpr::ForDeclaration(_)) {
            chunk.op(Insn::PopLexEnv);
        }
        let mut exits = vec![];
        match (iterator_kind, i_kind) {
            (IteratorKind::Async, _) | (_, IterationKind::AsyncIterate) => {
                chunk.op_plus_arg(Insn::RotateUp, 3);
                chunk.op(Insn::Pop);
                chunk.op(Insn::ToDo); // ASYNC_ITER_CLOSE
                exits.push(chunk.op_jump(Insn::Jump));
            }
            (IteratorKind::Sync, IterationKind::Enumerate) => {
                unwind2s.push(chunk.op_jump(Insn::Jump));
            }
            (IteratorKind::Sync, IterationKind::Iterate) => {
                chunk.op_plus_arg(Insn::RotateUp, 3);
                chunk.op(Insn::Pop);
                chunk.op(Insn::IteratorClose);
                exits.push(chunk.op_jump(Insn::Jump));
            }
        }

        if let Some((inner_unwind, lhs_calc_done)) = lhs_assignment_spots {
            chunk.fixup(inner_unwind).expect("Jump too short to overflow");
            chunk.op_plus_arg(Insn::Unwind, 1);
            chunk.op_jump_back(Insn::Jump, lhs_calc_done).expect("Jump too short to overflow");
        }

        chunk.fixup(statements).expect("Jump too short to overflow");
        chunk.op(Insn::Pop);
        chunk.op(Insn::Swap);
        stmt.compile(chunk, strict, text)?;
        if matches!(lhs, ForInOfLHSExpr::ForDeclaration(_)) {
            chunk.op(Insn::PopLexEnv);
        }
        let lsid = chunk.add_to_string_set_pool(label_set)?;
        chunk.op_plus_arg(Insn::LoopContinues, lsid);
        let loop_termination = chunk.op_jump(Insn::JumpPopIfFalse);
        chunk.op(Insn::CoalesceValue);
        chunk.op_jump_back(Insn::Jump, top)?;

        chunk.fixup(loop_termination).expect("jump too short to fail");
        chunk.op(Insn::UpdateEmpty);
        match (iterator_kind, i_kind) {
            (_, IterationKind::Enumerate) => {
                chunk.op_plus_arg(Insn::Unwind, 1);
            }
            (IteratorKind::Async, _) => {
                chunk.op(Insn::ToDo); // ASYNC_ITER_CLOSE
            }
            (IteratorKind::Sync, _) => {
                chunk.op(Insn::IteratorClose);
            }
        }
        exits.push(chunk.op_jump(Insn::Jump));

        chunk.fixup(pop2_exit).expect("jump shorter than the jump back to top, which was already successful");
        chunk.op(Insn::Pop);
        chunk.op(Insn::Pop);
        exits.push(chunk.op_jump(Insn::Jump));

        chunk.fixup(unwind3)?;
        chunk.op_plus_arg(Insn::Unwind, 1);
        for unwind2 in unwind2s {
            chunk.fixup(unwind2)?;
        }
        chunk.op_plus_arg(Insn::Unwind, 2);
        for exit in exits {
            chunk.fixup(exit).expect("Longest exit jump is shorter than unwind2, which was successful");
        }
        Ok(AlwaysAbruptResult)
    }

    pub fn for_in_of_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        label_set: &[JSString],
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: ForInOfLoopEvaluation
        // The syntax-directed operation ForInOfLoopEvaluation takes argument labelSet (a List of Strings) and
        // returns either a normal completion containing an ECMAScript language value or an abrupt completion.
        // It is defined piecewise over the following productions:
        let (lhs, names_to_bind, iterator_kind, iteration_kind, exp, stmt) = match self {
            ForInOfStatement::In(lhs, exp, stmt, _) => {
                // ForInOfStatement : for ( LeftHandSideExpression in Expression ) Statement
                //  1. Let keyResult be ? ForIn/OfHeadEvaluation(« », Expression, enumerate).
                //  2. Return ? ForIn/OfBodyEvaluation(LeftHandSideExpression, Statement, keyResult,
                //     enumerate, assignment, labelSet).
                (lhs.into(), vec![], IteratorKind::Sync, IterationKind::Enumerate, exp.into(), stmt)
            }
            ForInOfStatement::DestructuringIn(ap, exp, stmt, _) => {
                // ForInOfStatement : for ( LeftHandSideExpression in Expression ) Statement
                //  1. Let keyResult be ? ForIn/OfHeadEvaluation(« », Expression, enumerate).
                //  2. Return ? ForIn/OfBodyEvaluation(LeftHandSideExpression, Statement, keyResult,
                //     enumerate, assignment, labelSet).
                (ap.into(), vec![], IteratorKind::Sync, IterationKind::Enumerate, exp.into(), stmt)
            }
            ForInOfStatement::VarIn(fb, exp, stmt, _) => {
                // ForInOfStatement : for ( var ForBinding in Expression ) Statement
                //  1. Let keyResult be ? ForIn/OfHeadEvaluation(« », Expression, enumerate).
                //  2. Return ? ForIn/OfBodyEvaluation(ForBinding, Statement, keyResult, enumerate, varBinding,
                //     labelSet).
                (fb.into(), vec![], IteratorKind::Sync, IterationKind::Enumerate, exp.into(), stmt)
            }
            ForInOfStatement::LexIn(fd, exp, stmt, _) => {
                // ForInOfStatement : for ( ForDeclaration in Expression ) Statement
                //  1. Let keyResult be ? ForIn/OfHeadEvaluation(BoundNames of ForDeclaration, Expression, enumerate).
                //  2. Return ? ForIn/OfBodyEvaluation(ForDeclaration, Statement, keyResult, enumerate, lexicalBinding,
                //     labelSet).
                (fd.into(), fd.bound_names(), IteratorKind::Sync, IterationKind::Enumerate, exp.into(), stmt)
            }
            ForInOfStatement::Of(lhs, exp, stmt, _) => {
                // ForInOfStatement : for ( LeftHandSideExpression of AssignmentExpression ) Statement
                //  1. Let keyResult be ? ForIn/OfHeadEvaluation(« », AssignmentExpression, iterate).
                //  2. Return ? ForIn/OfBodyEvaluation(LeftHandSideExpression, Statement, keyResult, iterate,
                //     assignment, labelSet).
                (lhs.into(), vec![], IteratorKind::Sync, IterationKind::Iterate, exp.into(), stmt)
            }
            ForInOfStatement::DestructuringOf(lhs, exp, stmt, _) => {
                // ForInOfStatement : for ( LeftHandSideExpression of AssignmentExpression ) Statement
                //  1. Let keyResult be ? ForIn/OfHeadEvaluation(« », AssignmentExpression, iterate).
                //  2. Return ? ForIn/OfBodyEvaluation(LeftHandSideExpression, Statement, keyResult, iterate,
                //     assignment, labelSet).
                (lhs.into(), vec![], IteratorKind::Sync, IterationKind::Iterate, exp.into(), stmt)
            }
            ForInOfStatement::VarOf(fb, exp, stmt, _) => {
                (fb.into(), vec![], IteratorKind::Sync, IterationKind::Iterate, exp.into(), stmt)
            }
            ForInOfStatement::LexOf(fd, exp, stmt, _) => {
                (fd.into(), fd.bound_names(), IteratorKind::Sync, IterationKind::Iterate, exp.into(), stmt)
            }
            ForInOfStatement::AwaitOf(_, _, _, _) => todo!(),
            ForInOfStatement::DestructuringAwaitOf(_, _, _, _) => todo!(),
            ForInOfStatement::AwaitVarOf(_, _, _, _) => todo!(),
            ForInOfStatement::AwaitLexOf(_, _, _, _) => todo!(),
        };
        // start:
        //   <head_eval([], exp, ENUMERATE)>        keyresult/err
        //   JUMP_IF_ABRUPT exit
        //   <body_eval(lhs, stmt, ENUMERATE, ASSIGNMENT, label_set)
        // exit:
        let head_status = Self::for_in_of_head_evaluation(chunk, strict, text, &names_to_bind, exp, iteration_kind)?;
        let mut exit = None;
        if head_status.maybe_abrupt() {
            exit = Some(chunk.op_jump(Insn::JumpIfAbrupt));
        }
        Self::for_in_of_body_evaluation(chunk, strict, text, lhs, stmt, iteration_kind, label_set, iterator_kind)?;
        if let Some(exit) = exit {
            chunk.fixup(exit)?;
        }
        Ok(AlwaysAbruptResult)
    }
}

impl ForBinding {
    fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        // Runtime Semantics: Evaluation
        // The syntax-directed operation Evaluation takes no arguments and returns a Completion Record.
        match self {
            ForBinding::Identifier(ident) => ident.compile(chunk, strict),
            ForBinding::Pattern(_) => panic!("Patterns not expected to compile."),
        }
    }

    fn binding_initialization(
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
        // NOTE undefined is passed for environment to indicate that a PutValue operation should be used to assign the
        // initialization value. This is the case for var statements and formal parameter lists of some non-strict
        // functions (See 10.2.11). In those cases a lexical binding is hoisted and preinitialized prior to evaluation
        // of its initializer.
        //
        // It is defined piecewise over the following productions:
        match self {
            ForBinding::Identifier(ident) => {
                ident.compile_binding_initialization(chunk, strict, env).map(AbruptResult::from)
            }
            ForBinding::Pattern(pat) => {
                pat.compile_binding_initialization(chunk, strict, text, env).map(AbruptResult::from)
            }
        }
    }
}

impl ForDeclaration {
    fn for_declaration_binding_instantiation(&self, chunk: &mut Chunk) -> anyhow::Result<NeverAbruptRefResult> {
        // Runtime Semantics: ForDeclarationBindingInstantiation
        // The syntax-directed operation ForDeclarationBindingInstantiation takes argument environment (a Declarative
        // Environment Record) and returns unused. It is defined piecewise over the following productions:

        // ForDeclaration : LetOrConst ForBinding
        //  1. For each element name of the BoundNames of ForBinding, do
        //      a. If IsConstantDeclaration of LetOrConst is true, then
        //          i. Perform ! environment.CreateImmutableBinding(name, true).
        //      b. Else,
        //          i. Perform ! environment.CreateMutableBinding(name, false).
        //  2. Return unused.
        let insn = if self.loc.is_constant_declaration() {
            Insn::CreateStrictImmutableLexBinding
        } else {
            Insn::CreatePermanentMutableLexBinding
        };
        for name_val in self.binding.bound_names() {
            let name = chunk.add_to_string_pool(name_val)?;
            chunk.op_plus_arg(insn, name);
        }

        Ok(NeverAbruptRefResult)
    }

    fn for_declaration_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AbruptResult> {
        // Runtime Semantics: ForDeclarationBindingInitialization
        // The syntax-directed operation ForDeclarationBindingInitialization takes arguments value (an ECMAScript
        // language value) and environment (an Environment Record or undefined) and returns either a normal completion
        // containing UNUSED or an abrupt completion.
        //
        // NOTE undefined is passed for environment to indicate that a PutValue operation should be used to assign the
        // initialization value. This is the case for var statements and the formal parameter lists of some non-strict
        // functions (see 10.2.11). In those cases a lexical binding is hoisted and preinitialized prior to evaluation
        // of its initializer.
        //
        // It is defined piecewise over the following productions:
        //
        // ForDeclaration : LetOrConst ForBinding
        //  1. Return ? BindingInitialization of ForBinding with arguments value and environment.

        self.binding.binding_initialization(chunk, strict, text, env)
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
    fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        // SwitchStatement : switch ( Expression ) CaseBlock
        //  1. Let exprRef be ? Evaluation of Expression.
        //  2. Let switchValue be ? GetValue(exprRef).
        //  3. Let oldEnv be the running execution context's LexicalEnvironment.
        //  4. Let blockEnv be NewDeclarativeEnvironment(oldEnv).
        //  5. Perform BlockDeclarationInstantiation(CaseBlock, blockEnv).
        //  6. Set the running execution context's LexicalEnvironment to blockEnv.
        //  7. Let R be Completion(CaseBlockEvaluation of CaseBlock with argument switchValue).
        //  8. Set the running execution context's LexicalEnvironment to oldEnv.
        //  9. Return R.
        // NOTE: No matter how control leaves the SwitchStatement the LexicalEnvironment is always restored to
        // its former state.

        // start:
        //   <evaluation of Expression>                exprRef/err
        //   GET_VALUE                                 switchValue/err
        //   JUMP_IF_ABRUPT exit                       switchValue
        //   PNLE                                      switchValue
        //   <BDI(CaseBlock)>                          switchValue
        //   <CaseBlock.CaseBlockEvalution>            R/err
        //   PLE                                       R/err
        // exit:

        let status = self.expression.compile(chunk, strict, text)?;
        if status.maybe_ref() {
            chunk.op(Insn::GetValue);
        }
        let exit =
            if status.maybe_ref() || status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
        chunk.op(Insn::PushNewLexEnv);
        let declarations = self.case_block.lexically_scoped_declarations();
        block_declaration_instantiation(chunk, strict, text, &declarations)?;
        let blocks_status = self.case_block.case_block_evaluation(chunk, strict, text)?;
        chunk.op(Insn::PopLexEnv);
        if let Some(exit) = exit {
            chunk.fixup(exit)?;
        }
        Ok(AbruptResult::from(status.maybe_ref() || status.maybe_abrupt() || blocks_status.maybe_abrupt()))
    }
}

impl CaseBlock {
    fn case_block_evaluation(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        // The syntax-directed operation CaseBlockEvaluation takes argument input (an ECMAScript language
        // value) and returns either a normal completion containing an ECMAScript language value or an abrupt
        // completion.
        match self {
            CaseBlock::NoDefault(None, _) => {
                // CaseBlock : { }
                //  1. Return undefined.
                chunk.op(Insn::Pop);
                chunk.op(Insn::Undefined);
                Ok(AbruptResult::Never)
            }
            CaseBlock::NoDefault(Some(clauses), _) => {
                // CaseBlock : { CaseClauses }
                //  1. Let V be undefined.
                //  2. Let A be the List of CaseClause items in CaseClauses, in source text order.
                //  3. Let found be false.
                //  4. For each CaseClause C of A, do
                //      a. If found is false, then
                //          i. Set found to ? CaseClauseIsSelected(C, input).
                //      b. If found is true, then
                //          i. Let R be Completion(Evaluation of C).
                //          ii. If R.[[Value]] is not EMPTY, set V to R.[[Value]].
                //          iii. If R is an abrupt completion, return ? UpdateEmpty(R, V).
                //  5. Return V.

                // start:                               input
                //  UNDEFINED                           V  input
                //  SWAP                                input  V
                //  FALSE                               false  input  V
                //  --------REPEAT FOR EACH CASE CLAUSE-----------------
                //                                      (false input)/true V
                //  JUMP_IF_TRUE do_eval_cont           false input V
                //  POP                                 input V
                //  <C.CaseClauseIsSelected>            true/false/err input V
                //  JUMP_IF_ABRUPT unwind_2             true/false input V
                //  JUMP_IF_FALSE skip_eval             true input V
                //  POP                                 input V
                // do_eval_cont:                        input/true V
                //  POP                                 V
                //  <C.evaluate>                        val/err V
                //  UPDATE_EMPTY                        V/err
                //  JUMP_IF_ABRUPT exit                 V
                //  TRUE                                true V
                // skip_eval:                           (false input)/true V
                //  -----------------------------------------------------
                //  JUMP_POP_IF_TRUE exit               input V
                //  POP                                 V
                //  JUMP exit
                // unwind_2:                            err input V
                //  UNWIND 2                            err
                // exit:                                V/err

                chunk.op(Insn::Undefined);
                chunk.op(Insn::Swap);
                chunk.op(Insn::False);

                let mut unwind_2 = vec![];
                let mut exit = vec![];

                for clause in clauses.to_vec() {
                    let do_eval_cont = chunk.op_jump(Insn::JumpIfTrue);
                    chunk.op(Insn::Pop);
                    let check_status = clause.case_clause_is_selected(chunk, strict, text)?;
                    if check_status.maybe_abrupt() {
                        unwind_2.push(chunk.op_jump(Insn::JumpIfAbrupt));
                    }
                    let skip_eval = chunk.op_jump(Insn::JumpIfFalse);
                    chunk.op(Insn::Pop);
                    chunk.fixup(do_eval_cont)?;
                    chunk.op(Insn::Pop);
                    let clause_status = clause.compile(chunk, strict, text)?;
                    chunk.op(Insn::UpdateEmpty);
                    if clause_status.maybe_abrupt() {
                        exit.push(chunk.op_jump(Insn::JumpIfAbrupt));
                    }
                    chunk.op(Insn::True);
                    chunk.fixup(skip_eval)?;
                }

                exit.push(chunk.op_jump(Insn::JumpPopIfTrue));
                chunk.op(Insn::Pop);
                let maybe_abrupt = !unwind_2.is_empty() || exit.len() > 1;
                if !unwind_2.is_empty() {
                    exit.push(chunk.op_jump(Insn::Jump));
                    for u in unwind_2 {
                        chunk.fixup(u)?;
                    }
                    chunk.op_plus_arg(Insn::Unwind, 2);
                }
                for e in exit {
                    chunk.fixup(e)?;
                }

                Ok(AbruptResult::from(maybe_abrupt))
            }
            CaseBlock::HasDefault(before, default, after, _) => {
                // CaseBlock : { CaseClausesopt DefaultClause CaseClausesopt }
                //  1. Let V be undefined.
                //  2. If the first CaseClauses is present, then
                //      a. Let A be the List of CaseClause items in the first CaseClauses, in source text order.
                //  3. Else,
                //      a. Let A be a new empty List.
                //  4. Let found be false.
                //  5. For each CaseClause C of A, do
                //      a. If found is false, then
                //          i. Set found to ? CaseClauseIsSelected(C, input).
                //      b. If found is true, then
                //          i. Let R be Completion(Evaluation of C).
                //          ii. If R.[[Value]] is not EMPTY, set V to R.[[Value]].
                //          iii. If R is an abrupt completion, return ? UpdateEmpty(R, V).
                //  6. Let foundInB be false.
                //  7. If the second CaseClauses is present, then
                //      a. Let B be the List of CaseClause items in the second CaseClauses, in source text order.
                //  8. Else,
                //      a. Let B be a new empty List.
                //  9. If found is false, then
                //      a. For each CaseClause C of B, do
                //          i. If foundInB is false, then
                //              1. Set foundInB to ? CaseClauseIsSelected(C, input).
                //          ii. If foundInB is true, then
                //              1. Let R be Completion(Evaluation of CaseClause C).
                //              2. If R.[[Value]] is not EMPTY, set V to R.[[Value]].
                //              3. If R is an abrupt completion, return ? UpdateEmpty(R, V).
                //  10. If foundInB is true, return V.
                //  11. Let defaultR be Completion(Evaluation of DefaultClause).
                //  12. If defaultR.[[Value]] is not EMPTY, set V to defaultR.[[Value]].
                //  13. If defaultR is an abrupt completion, return ? UpdateEmpty(defaultR, V).
                //  14. NOTE: The following is another complete iteration of the second CaseClauses.
                //  15. For each CaseClause C of B, do
                //      a. Let R be Completion(Evaluation of CaseClause C).
                //      b. If R.[[Value]] is not EMPTY, set V to R.[[Value]].
                //      c. If R is an abrupt completion, return ? UpdateEmpty(R, V).
                //  16. Return V.

                // start:                               input
                //  UNDEFINED                           V  input
                //  SWAP                                input  V
                //  FALSE                               found input V
                //  --------REPEAT FOR EACH FIRST CASE CLAUSE-----------------
                //                                      found input V
                //  JUMP_IF_TRUE do_eval_cont           false input V
                //  POP                                 input V
                //  <C.CaseClauseIsSelected>            found/err input V
                //  JUMP_IF_ABRUPT unwind_2             found input V
                //  JUMP_IF_FALSE skip_eval             found input V
                // do_eval_cont:                        found input V
                //  ROTATE_UP 3                         V found input
                //  <C.evaluate>                        val/err V found input
                //  UPDATE_EMPTY                        V/err found input
                //  JUMP_IF_ABRUPT unwind_2             V found input
                //  ROTATE_DOWN 3                       found input V
                // skip_eval:                           found input V
                // ------------------------------------------------------------
                //  FALSE                               foundInB found input V
                //  SWAP                                found foundInB input V
                //  JUMP_POP_IF_TRUE skip_second_clauses  foundInB input V
                //  --------REPEAT FOR EACH SECOND CASE CLAUSE-----------------
                //                                      foundInB input V
                //  JUMP_IF_TRUE do_eval2               foundInB input V
                //  POP                                 input V
                //  <C.CaseClauseIsSelected>            foundInB/err input V
                //  JUMP_IF_ABRUPT unwind_2             foundInB input V
                //  JUMP_IF_FALSE skip_eval2            foundInB input V
                // do_eval2:                            foundInB input V
                //  ROTATE_UP 3                         V foundInB input
                //  <C.evaluate>                        val/err V foundInB input
                //  UPDATE_EMPTY                        V/err foundInB input
                //  JUMP_IF_ABRUPT unwind_2             V foundInB input
                //  ROTATE_DOWN 3                       foundInB input V
                // skip_eval2:                          foundInB input V
                // ------------------------------------------------------------
                // skip_second_clauses:                 foundInB input V
                //  SWAP                                input foundInB V
                //  POP                                 foundInB V
                //  JUMP_POP_IF_TRUE exit               V
                //  <default.evaluate>                  val/err V
                //  UPDATE_EMPTY                        V/err
                //  JUMP_IF_ABRUPT exit                 V
                //  --------REPEAT FOR EACH SECOND CASE CLAUSE-----------------
                //  <C.evaluate>                        val/err V
                //  UPDATE_EMPTY                        V/err
                //  JUMP_IF_ABRUPT exit                 V
                // ------------------------------------------------------------
                //  JUMP exit
                // unwind_2:
                //  UNWIND 2
                // exit:

                chunk.op(Insn::Undefined);
                chunk.op(Insn::Swap);
                chunk.op(Insn::False);
                let mut unwind2 = vec![];
                let mut exit = vec![];
                if let Some(clauses) = before.as_ref() {
                    for clause in clauses.to_vec() {
                        let do_eval_cont = chunk.op_jump(Insn::JumpIfTrue);
                        chunk.op(Insn::Pop);
                        let check_status = clause.case_clause_is_selected(chunk, strict, text)?;
                        if check_status.maybe_abrupt() {
                            unwind2.push(chunk.op_jump(Insn::JumpIfAbrupt));
                        }
                        let skip_eval = chunk.op_jump(Insn::JumpIfFalse);
                        chunk.fixup(do_eval_cont)?;
                        chunk.op_plus_arg(Insn::RotateUp, 3);
                        let compute_status = clause.compile(chunk, strict, text)?;
                        chunk.op(Insn::UpdateEmpty);
                        if compute_status.maybe_abrupt() {
                            unwind2.push(chunk.op_jump(Insn::JumpIfAbrupt));
                        }
                        chunk.op_plus_arg(Insn::RotateDown, 3);
                        chunk.fixup(skip_eval)?;
                    }
                }
                chunk.op(Insn::False);
                chunk.op(Insn::Swap);
                if let Some(clauses) = after.as_ref() {
                    let skip_second_clauses = chunk.op_jump(Insn::JumpPopIfTrue);
                    for clause in clauses.to_vec() {
                        let do_eval2 = chunk.op_jump(Insn::JumpIfTrue);
                        chunk.op(Insn::Pop);
                        let check_status = clause.case_clause_is_selected(chunk, strict, text)?;
                        if check_status.maybe_abrupt() {
                            unwind2.push(chunk.op_jump(Insn::JumpIfAbrupt));
                        }
                        let skip_eval2 = chunk.op_jump(Insn::JumpIfFalse);
                        chunk.fixup(do_eval2)?;
                        chunk.op_plus_arg(Insn::RotateUp, 3);
                        let compute_status = clause.compile(chunk, strict, text)?;
                        chunk.op(Insn::UpdateEmpty);
                        if compute_status.maybe_abrupt() {
                            unwind2.push(chunk.op_jump(Insn::JumpIfAbrupt));
                        }
                        chunk.op_plus_arg(Insn::RotateDown, 3);
                        chunk.fixup(skip_eval2)?;
                    }
                    chunk.fixup(skip_second_clauses)?;
                } else {
                    chunk.op(Insn::Pop);
                }
                chunk.op(Insn::Swap);
                chunk.op(Insn::Pop);
                exit.push(chunk.op_jump(Insn::JumpPopIfTrue));
                let default_status = default.compile(chunk, strict, text)?;
                chunk.op(Insn::UpdateEmpty);
                if let Some(clauses) = after.as_ref() {
                    if default_status.maybe_abrupt() {
                        exit.push(chunk.op_jump(Insn::JumpIfAbrupt));
                    }
                    for clause in clauses.to_vec() {
                        let status = clause
                            .compile(chunk, strict, text)
                            .expect("This compiled once before, it should compile fine now, too.");
                        chunk.op(Insn::UpdateEmpty);
                        if status.maybe_abrupt() {
                            exit.push(chunk.op_jump(Insn::JumpIfAbrupt));
                        }
                    }
                }
                let maybe_abrupt = !unwind2.is_empty() || default_status.maybe_abrupt();
                if !unwind2.is_empty() {
                    exit.push(chunk.op_jump(Insn::Jump));
                    for mark in unwind2 {
                        chunk.fixup(mark)?;
                    }
                    chunk.op_plus_arg(Insn::Unwind, 2);
                }
                for mark in exit {
                    chunk.fixup(mark)?;
                }

                Ok(AbruptResult::from(maybe_abrupt))
            }
        }
    }
}

impl DefaultClause {
    fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match &self.0 {
            Some(sl) => {
                // DefaultClause : default : StatementList
                //  1. Return ? Evaluation of StatementList.
                sl.compile(chunk, strict, text)
            }
            None => {
                // DefaultClause : default :
                //  1. Return EMPTY.
                chunk.op(Insn::Empty);
                Ok(AbruptResult::Never)
            }
        }
    }
}

impl CaseClause {
    fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match &self.statements {
            Some(sl) => {
                // CaseClause : case Expression : StatementList
                //  1. Return ? Evaluation of StatementList.
                sl.compile(chunk, strict, text)
            }
            None => {
                // CaseClause : case Expression :
                //  1. Return EMPTY.
                chunk.op(Insn::Empty);
                Ok(AbruptResult::Never)
            }
        }
    }

    fn case_clause_is_selected(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        // The abstract operation CaseClauseIsSelected takes arguments C (a CaseClause Parse Node) and input
        // (an ECMAScript language value) and returns either a normal completion containing a Boolean or an
        // abrupt completion. It determines whether C matches input. It performs the following steps when
        // called:
        //
        //  1. Assert: C is an instance of the production CaseClause : case Expression : StatementListopt .
        //  2. Let exprRef be ? Evaluation of the Expression of C.
        //  3. Let clauseSelector be ? GetValue(exprRef).
        //  4. Return IsStrictlyEqual(input, clauseSelector).
        // NOTE: This operation does not execute C's StatementList (if any). The CaseBlock algorithm uses its
        // return value to determine which StatementList to start executing.

        // start:                        input
        //  DUP                          input input
        //  <expression.evaluate>        val/ref/err input input
        //  GET_VALUE                    val/err input input
        //  JUMP_IF_ABRUPT unwind_1      val input input
        //  SEQ                          bool input
        //  JUMP exit
        // unwind_1:                     err input input
        //  UNWIND 1                     err input
        // exit:                         bool/err input

        chunk.op(Insn::Dup);
        let status = self.expression.compile(chunk, strict, text)?;
        if status.maybe_ref() {
            chunk.op(Insn::GetValue);
        }
        let unwind_1 =
            if status.maybe_ref() || status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
        chunk.op(Insn::StrictEqual);
        if let Some(unwind) = unwind_1 {
            let exit = chunk.op_jump(Insn::Jump);
            chunk.fixup(unwind).expect("Short jump should fit");
            chunk.op_plus_arg(Insn::Unwind, 1);
            chunk.fixup(exit).expect("Short jump should fit");
            Ok(AbruptResult::Maybe)
        } else {
            Ok(AbruptResult::Never)
        }
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
            LabelledItem::Function(_) => Ok(AbruptResult::from(FunctionDeclaration::compile(chunk))),
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
    fn compile(chunk: &mut Chunk) -> NeverAbruptRefResult {
        chunk.op(Insn::Empty);
        NeverAbruptRefResult
    }

    fn compile_fo_instantiation(
        self: &Rc<Self>,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
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
            to_compile: FunctionSource::from(self.clone()),
            this_mode: ThisLexicality::NonLexicalThis,
        };
        let func_id = chunk.add_to_func_stash(function_data)?;
        chunk.op_plus_two_args(Insn::InstantiateOrdinaryFunctionObject, name_id, func_id);
        Ok(AlwaysAbruptResult)
    }
}

impl Script {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match &self.body {
            None => Ok(AbruptResult::Never),
            Some(sb) => sb.compile(chunk, strict, text),
        }
    }
}

impl ScriptBody {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        let strict = strict || self.contains_use_strict();
        self.statement_list.compile(chunk, strict, text)
    }
}

#[derive(Copy, Clone)]
pub enum NameLoc {
    OnStack,
    Index(u16),
}

impl FunctionExpression {
    /// Generate code to create a potentially named function object
    ///
    /// See [InstantiateOrdinaryFunctionExpression](https://tc39.es/ecma262/#sec-runtime-semantics-instantiateordinaryfunctionexpression) in ECMA-262.
    fn instantiate_ordinary_function_expression(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        name: Option<NameLoc>,
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
                    None => Some(chunk.add_to_string_pool(JSString::from(""))?),
                    Some(NameLoc::Index(id)) => Some(id),
                    Some(NameLoc::OnStack) => None,
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

    /// Generate the code to evaluate a [`FunctionExpression`].
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
        self.instantiate_ordinary_function_expression(chunk, strict, None, text, self_as_rc)
    }

    fn compile_named_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        self_as_rc: Rc<Self>,
        id: Option<NameLoc>,
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

    let param_name_indexes =
        parameter_names.iter().map(|name| chunk.add_to_string_pool(name.clone())).collect::<Result<Vec<_>, _>>()?;
    for sidx in param_name_indexes.iter().copied() {
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
            let mut mapped_names = vec![];
            for (idx, &name) in param_name_indexes.iter().enumerate().rev() {
                if !mapped_names.contains(&name) {
                    mapped_names.push(name);
                    let idx = u16::try_from(idx)?;
                    chunk.op_plus_two_args(Insn::AddMappedArgument, name, idx);
                }
            }
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
    if has_parameter_expressions {
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
    } else {
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
        name: Option<NameLoc>,
        self_as_rc: Rc<Self>,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        if let Some(name_id) = match name {
            None => Some(chunk.add_to_string_pool(JSString::from(""))?),
            Some(NameLoc::Index(id)) => Some(id),
            Some(NameLoc::OnStack) => None,
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
        self.instantiate_arrow_function_expression(chunk, strict, text, None, self_as_rc)
    }

    pub fn compile_named_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        self_as_rc: Rc<Self>,
        id: Option<NameLoc>,
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
            ParamSource::UniqueFormalParameters(params) => {
                params.compile_binding_initialization(chunk, strict, text, env)
            }
            ParamSource::PropertySetParameterList(params) => {
                params.compile_binding_initialization(chunk, strict, text, env)
            }
        }
    }
}

impl PropertySetParameterList {
    pub fn compile_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AbruptResult> {
        self.node.compile_binding_initialization(chunk, strict, text, env)
    }
}

impl FormalParameters {
    pub fn compile_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AbruptResult> {
        match self {
            FormalParameters::Empty(_) => Ok(AbruptResult::from(false)),
            FormalParameters::Rest(frp) => {
                frp.compile_binding_initialization(chunk, strict, text, env).map(AbruptResult::from)
            }
            FormalParameters::List(fpl) | FormalParameters::ListComma(fpl, _) => {
                fpl.compile_binding_initialization(chunk, strict, text, env)
            }
            FormalParameters::ListRest(list, rest) => {
                // stack: N arg[n-1] ... arg[0]
                //   <list.compile_binding_initialization(env)>   err/(Q arg[q-1] ... arg[0])
                //   JUMP_IF_ABRUPT exit
                //   <rest.compile_binding_initialization(env)>   err/0
                // exit:
                let list_status = list.compile_binding_initialization(chunk, strict, text, env)?;
                let exit = if list_status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                rest.compile_binding_initialization(chunk, strict, text, env)?;
                if let Some(exit) = exit {
                    chunk.fixup(exit).expect("jump too short to fail");
                }
                Ok(AlwaysAbruptResult.into())
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

    fn compile(&self, chunk: &mut Chunk, strict: bool) -> anyhow::Result<CompilerStatusFlags> {
        // Runtime Semantics: Evaluation
        // The syntax-directed operation Evaluation takes no arguments and returns a Completion Record.

        // Runtime Semantics: Evaluation
        // BindingIdentifier :
        //      Identifier
        //      yield
        //      await
        //  1. Let bindingId be StringValue of BindingIdentifier.
        //  2. Return ? ResolveBinding(bindingId).
        let binding_id = chunk.add_to_string_pool(match self {
            BindingIdentifier::Identifier { identifier, .. } => identifier.string_value(),
            BindingIdentifier::Yield { .. } => "yield".into(),
            BindingIdentifier::Await { .. } => "await".into(),
        })?;
        chunk.op_plus_arg(Insn::String, binding_id);
        chunk.op(if strict { Insn::StrictResolve } else { Insn::Resolve });
        Ok(CompilerStatusFlags::new().abrupt(true).reference(true))
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
                // start:                             N arg[N-1] ... arg[0]
                //    <list.compile_binding_init>     err/(M arg[M-1] ... arg[0])
                //    JUMP_IF_ABRUPT exit             M arg[M-1] ... arg[0]
                //    <item.compile_binding_init>     err/(Q arg[Q-1] ... arg[0])
                // exit:
                let list_status = list.compile_binding_initialization(chunk, strict, text, env)?;
                let exit = if list_status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                let item_status = item.compile_binding_initialization(chunk, strict, text, env)?;
                if let Some(mark) = exit {
                    chunk.fixup(mark)?;
                }
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
                //   POP                        (n-1) arg(n-1) ... arg(1)
                //   JUMP exit
                // unwind:                      err (n-1) arg(n-1) ... arg(1)
                //   UNWIND_LIST                err
                // exit:                        ((n-1) arg(n-1) ... arg(1)) or err
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
                //   JUMP_IF_ABRUPT unwind      [empty] (n-1) arg(n-1) ... arg(1)
                //   POP                        (n-1) arg(n-1) ... arg(1)
                //   JUMP exit
                // unwind:                      err (n-1) arg(n-1) ... arg(1)
                //   UNWIND_LIST                err
                // exit:                        ((n-1) arg(n-1) ... arg(1)) or err

                chunk.op(Insn::ExtractArg);
                let mark = chunk.op_jump(Insn::JumpIfNotUndef);
                chunk.op(Insn::Pop);
                let izer_status = init.compile(chunk, strict, text, CompileMod::Unmodified)?;
                if izer_status.maybe_ref() {
                    chunk.op(Insn::GetValue);
                }
                let mut unwind = None;
                if izer_status.maybe_abrupt() || izer_status.maybe_ref() {
                    unwind = Some(chunk.op_jump(Insn::JumpIfAbrupt));
                }
                chunk.fixup(mark)?;
                bp.compile_binding_initialization(chunk, strict, text, env)?;
                let unwind2 = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Pop);
                let exit = chunk.op_jump(Insn::Jump);
                if let Some(unwind) = unwind {
                    chunk.fixup(unwind)?;
                }
                chunk.fixup(unwind2).expect("jump too short to fail");
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
                    let status = izer.compile(chunk, strict, text, CompileMod::Unmodified)?;
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
                    let status = izer.compile(chunk, strict, text, CompileMod::Unmodified)?;
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
                np.compile_named_evaluation(chunk, strict, text, Some(NameLoc::Index(id_idx)))?
            } else {
                init.compile(chunk, strict, text, CompileMod::Unmodified)?
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
                        np.compile_named_evaluation(chunk, strict, text, Some(NameLoc::Index(binding_id)))?
                    } else {
                        izer.compile(chunk, strict, text, CompileMod::Unmodified)?
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
                        np.compile_named_evaluation(chunk, strict, text, Some(NameLoc::Index(binding_id)))?
                    } else {
                        izer.compile(chunk, strict, text, CompileMod::Unmodified)?
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
                //    EMPTY_IF_NOT_ERR       [empty]/err ir
                //    IR_CLOSE_IF_NOT_DONE   [empty]/err   (steps 3 & 4, above)
                // exit:                     [empty]/err

                chunk.op(Insn::GetSyncIterator);
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Dup);
                abp.iterator_binding_initialization(chunk, strict, text, env)?;
                chunk.op(Insn::EmptyIfNotError);
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

                bpl.property_binding_initialization(chunk, strict, text, env)?;
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::PopList);
                chunk.op(Insn::Empty);
                chunk.fixup(exit).expect("Jump should be to short to fail");
                Ok(AbruptResult::Maybe)
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
                brp.rest_binding_initialization(chunk, strict, env)?;
                Ok(AbruptResult::Maybe)
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
                bpl.property_binding_initialization(chunk, strict, text, env)?;
                let unwind = chunk.op_jump(Insn::JumpIfAbrupt);
                brp.rest_binding_initialization(chunk, strict, env)?;
                let exit = chunk.op_jump(Insn::Jump);
                chunk.fixup(unwind).expect("Jump too short to fail");
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit).expect("Jump too short to fail");
                Ok(AbruptResult::Maybe)
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
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: PropertyBindingInitialization
        // The syntax-directed operation PropertyBindingInitialization takes arguments value (an ECMAScript language
        // value) and environment (an Environment Record or undefined) and returns either a normal completion containing
        // a List of property keys or an abrupt completion. It collects a list of all bound property names. It is
        // defined piecewise over the following productions:
        match self {
            BindingPropertyList::Item(bp) => {
                // BindingPropertyList : BindingProperty
                //  1. Return ? PropertyBindingInitialization of BindingProperty with arguments value and environment.
                bp.property_binding_initialization(chunk, strict, text, env)
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
                bpl.property_binding_initialization(chunk, strict, text, env)?;
                let unwind_value = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::SwapList);
                bp.property_binding_initialization(chunk, strict, text, env)?;
                let unwind_list = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::AppendList);
                let mut exits = vec![];
                exits.push(chunk.op_jump(Insn::Jump));
                chunk.fixup(unwind_value)?;
                chunk.op_plus_arg(Insn::Unwind, 1);
                exits.push(chunk.op_jump(Insn::Jump));
                chunk.fixup(unwind_list).expect("jump too short to overflow");
                chunk.op(Insn::UnwindList);
                for mark in exits {
                    chunk.fixup(mark).expect("jumps too short to overflow");
                }

                Ok(AlwaysAbruptResult)
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
        env: EnvUsage,
    ) -> anyhow::Result<AlwaysAbruptResult> {
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

                Ok(AlwaysAbruptResult)
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
                    chunk.fixup(exit).expect("jump too short to fail");
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
                chunk.fixup(exit).expect("jump too short to fail");
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
                    chunk.fixup(mark).expect("Jumps too short to fail");
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
            BindingElementList::List(list, element) => {
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

                let status = list.iterator_binding_initialization(chunk, strict, text, env)?;
                assert!(status.maybe_abrupt());
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                let status = element.iterator_binding_initialization(chunk, strict, text, env)?;
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
                bp.compile_binding_initialization(chunk, strict, text, env)?;
                let unwind = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Pop);
                exits.push(chunk.op_jump(Insn::Jump));
                chunk.fixup(unwind).expect("jump too short to fail");
                chunk.op_plus_arg(Insn::Unwind, 1);
                for mark in exits {
                    chunk.fixup(mark)?;
                }
                Ok(AlwaysAbruptResult)
            }
        }
    }

    pub fn compile_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // This is like iterator_binding_initialization, except that the list is on the stack, rather than sitting in an
        // iterator object somewhere in the heap.
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
                //
                // Or, in other words: take the args left on the stack, wrap them up into an array, assign that array to
                // our identifier, and then put a zero-length list back on the stack.

                // start:   N arg[n-1] arg[n-2] ... arg[0]  (aka: 'arglist')
                //   STRING bindingId               bindingId arglist
                //   STRICT_RESOLVE/RESOLVE         lhs/err arglist
                //   JUMP_IF_ABRUPT unwind_list     lhs arglist
                //   ROTATE_DN_LIST 0               arglist lhs
                //   LIST_TO_ARRAY                  A lhs
                //   PUT_VALUE/IRB                  [empty]/err
                //   JUMP_IF_ABRUPT exit            [empty]
                //   POP                            <nothing>
                //   ZERO                           0
                //   JUMP exit
                // unwind_list:                     err arglist
                //   UNWIND_LIST                    err
                // exit:                            err/0
                let binding_id_val = bi.string_value();
                let binding_id = chunk.add_to_string_pool(binding_id_val)?;
                chunk.op_plus_arg(Insn::String, binding_id);
                chunk.op(if strict { Insn::StrictResolve } else { Insn::Resolve });
                let unwind_list = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op_plus_arg(Insn::RotateDownList, 0);
                chunk.op(Insn::ListToArray);
                chunk.op(match env {
                    EnvUsage::UsePutValue => Insn::PutValue,
                    EnvUsage::UseCurrentLexical => Insn::InitializeReferencedBinding,
                });
                let exit1 = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Pop);
                chunk.op(Insn::Zero);
                let exit2 = chunk.op_jump(Insn::Jump);
                chunk.fixup(unwind_list).expect("Jump too short to fail");
                chunk.op(Insn::UnwindList);
                chunk.fixup(exit1).expect("Jump too short to fail");
                chunk.fixup(exit2).expect("Jump too short to fail");
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

                // start:   N arg[n-1] arg[n-2] ... arg[0]  (aka: 'arglist')
                //   LIST_TO_ARRAY                       A
                //   <bp.binding_initialization(env)>    err/[empty]
                //   JUMP_IF_ABRUPT exit
                //   POP
                //   ZERO
                // exit:

                chunk.op(Insn::ListToArray);
                bp.compile_binding_initialization(chunk, strict, text, env)?;
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Pop);
                chunk.op(Insn::Zero);
                chunk.fixup(exit).expect("jump too short to fail");

                Ok(AlwaysAbruptResult)
            }
        }
    }
}

impl FunctionRestParameter {
    pub fn compile_binding_initialization(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        env: EnvUsage,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // This is like iterator_binding_initialization, except that the list is on the stack, rather than sitting in an
        // iterator object somewhere in the heap.
        self.element.compile_binding_initialization(chunk, strict, text, env)
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
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match self {
            FunctionStatementList::Statements(s) => s.compile(chunk, strict, text),
            FunctionStatementList::Empty(_) => {
                chunk.op(Insn::Undefined);
                Ok(NeverAbruptRefResult.into())
            }
        }
    }
}

impl ClassDeclaration {
    fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        // Runtime Semantics: Evaluation
        // ClassDeclaration : class BindingIdentifier ClassTail
        //  1. Perform ? BindingClassDeclarationEvaluation of this ClassDeclaration.
        //  2. Return EMPTY.
        // NOTE
        // ClassDeclaration : class ClassTail only occurs as part of an ExportDeclaration and is never directly
        // evaluated.

        // <self.binding_class_declaration_evaluation>    anything/err
        // EMPTY_IF_NOT_ERROR                             [empty]/e

        let decl_status = self.binding_class_declaration_evaluation(chunk, strict, text)?;
        chunk.op(Insn::EmptyIfNotError);
        Ok(decl_status)
    }

    fn binding_class_declaration_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AbruptResult> {
        // Runtime Semantics: BindingClassDeclarationEvaluation
        // The syntax-directed operation BindingClassDeclarationEvaluation takes no arguments and returns either a
        // normal completion containing a function object or an abrupt completion. It is defined piecewise over the
        // following productions:
        //
        // NOTE
        // ClassDeclaration : class ClassTail only occurs as part of an ExportDeclaration and establishing its binding
        // is handled as part of the evaluation action for that production. See 16.2.3.7.

        match self {
            ClassDeclaration::Named { ident, tail, location } => {
                // ClassDeclaration : class BindingIdentifier ClassTail
                //  1. Let className be StringValue of BindingIdentifier.
                //  2. Let value be ? ClassDefinitionEvaluation of ClassTail with arguments className and className.
                //  3. Set value.[[SourceText]] to the source text matched by ClassDeclaration.
                //  4. Let env be the running execution context's LexicalEnvironment.
                //  5. Perform ? InitializeBoundName(className, value, env).
                //  6. Return value.

                // start:
                //   <tail.class_definition_evaluation>      err/F
                //   JUMP_IF_ABRUPT exit                     F
                //   ATTACH_SOURCE                           F
                //   DUP                                     F F
                //   <initialize_bound_name>                 F
                // exit:
                let class_name = ident.string_value();
                let idx = chunk.add_to_string_pool(class_name)?;
                tail.class_definition_evaluation(chunk, strict, text, Some(idx), NameLoc::Index(idx))?;
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                let src_idx = chunk.add_to_string_pool(JSString::from(
                    &text[location.span.starting_index..location.span.starting_index + location.span.length],
                ))?;
                chunk.op_plus_arg(Insn::AttachSourceText, src_idx);
                chunk.op(Insn::Dup);
                compile_initialize_bound_name(chunk, strict, EnvUsage::UseCurrentLexical, idx);
                chunk.fixup(exit).expect("jump too short to fail");
                Ok(AbruptResult::Maybe)
            }
            ClassDeclaration::Unnamed { tail, location } => {
                // ClassDeclaration : class ClassTail
                //  1. Let value be ? ClassDefinitionEvaluation of ClassTail with arguments undefined and "default".
                //  2. Set value.[[SourceText]] to the source text matched by ClassDeclaration.
                //  3. Return value.
                let default_idx = chunk.add_to_string_pool("default".into())?;
                tail.class_definition_evaluation(chunk, strict, text, None, NameLoc::Index(default_idx))?;
                let exit = chunk.op_jump(Insn::JumpIfAbrupt);
                let src_idx = chunk.add_to_string_pool(JSString::from(
                    &text[location.span.starting_index..location.span.starting_index + location.span.length],
                ))?;
                chunk.op_plus_arg(Insn::AttachSourceText, src_idx);
                chunk.fixup(exit).expect("jump too short to fail");
                Ok(AbruptResult::Maybe)
            }
        }
    }
}

impl ClassExpression {
    fn named_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        name: NameLoc,
    ) -> anyhow::Result<AbruptResult> {
        // Runtime Semantics: NamedEvaluation
        // The syntax-directed operation NamedEvaluation takes argument name (a property key or a Private Name) and
        // returns either a normal completion containing a function object or an abrupt completion.
        //
        // ClassExpression : class ClassTail
        //  1. Let value be ? ClassDefinitionEvaluation of ClassTail with arguments undefined and name.
        //  2. Set value.[[SourceText]] to the source text matched by ClassExpression.
        //  3. Return value.

        // start:                                                  name?
        //   <tail.class_definition_evaluation(undefined, name)>   err/F
        //   JUMP_IF_ABRUPT exit                                   F
        //   ATTACH_SOURCE <text>                                  F
        // exit:

        let status = self.tail.class_definition_evaluation(chunk, strict, text, None, name)?;
        let exit = if status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
        let source_loc = self.location().span;
        let source_text = &text[source_loc.starting_index..source_loc.starting_index + source_loc.length];
        let source_idx = chunk.add_to_string_pool(JSString::from(source_text))?;
        chunk.op_plus_arg(Insn::AttachSourceText, source_idx);
        if let Some(exit) = exit {
            chunk.fixup(exit).expect("Jump too short to fail");
        }
        Ok(status)
    }

    fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match &self.ident {
            Some(binding_id) => {
                // ClassExpression : class BindingIdentifier ClassTail
                //  1. Let className be the StringValue of BindingIdentifier.
                //  2. Let value be ? ClassDefinitionEvaluation of ClassTail with arguments className and className.
                //  3. Set value.[[SourceText]] to the source text matched by ClassExpression.
                //  4. Return value.

                // start:
                //   <tail.class_definition_evaluation(binding_id, binding_id)>     err/F
                //   JUMP_IF_ABRUPT exit                                            F
                //   ATTACH_SOURCE <text>                                           F
                // exit:
                let binding_id = chunk.add_to_string_pool(binding_id.string_value())?;
                let status = self.tail.class_definition_evaluation(
                    chunk,
                    strict,
                    text,
                    Some(binding_id),
                    NameLoc::Index(binding_id),
                )?;
                let exit = if status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                let source_loc = self.location().span;
                let source_text = &text[source_loc.starting_index..source_loc.starting_index + source_loc.length];
                let source_idx = chunk.add_to_string_pool(JSString::from(source_text))?;
                chunk.op_plus_arg(Insn::AttachSourceText, source_idx);
                if let Some(exit) = exit {
                    chunk.fixup(exit).expect("Jump too short to fail");
                }
                Ok(status)
            }
            None => {
                // ClassExpression : class ClassTail
                //  1. Let value be ? ClassDefinitionEvaluation of ClassTail with arguments undefined and "".
                //  2. Set value.[[SourceText]] to the source text matched by ClassExpression.
                //  3. Return value.

                // start:
                //   <tail.class_definition_evaluation(undefined, "")>     err/F
                //   JUMP_IF_ABRUPT exit                                   F
                //   ATTACH_SOURCE <text>                                  F
                // exit:

                let emptystr = chunk.add_to_string_pool(JSString::from(""))?;
                let status =
                    self.tail.class_definition_evaluation(chunk, strict, text, None, NameLoc::Index(emptystr))?;
                let exit = if status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                let source_loc = self.location().span;
                let source_text = &text[source_loc.starting_index..source_loc.starting_index + source_loc.length];
                let source_idx = chunk.add_to_string_pool(JSString::from(source_text))?;
                chunk.op_plus_arg(Insn::AttachSourceText, source_idx);
                if let Some(exit) = exit {
                    chunk.fixup(exit).expect("Jump too short to fail");
                }
                Ok(status)
            }
        }
    }
}

impl ClassTail {
    fn class_definition_evaluation(
        &self,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        class_binding: Option<u16>,
        class_name: NameLoc,
    ) -> anyhow::Result<AbruptResult> {
        // Runtime Semantics: ClassDefinitionEvaluation
        //
        // The syntax-directed operation ClassDefinitionEvaluation takes arguments classBinding (a String or undefined)
        // and className (a property key or a Private Name) and returns either a normal completion containing a function
        // object or an abrupt completion.
        //
        // ClassTail : ClassHeritageopt { ClassBodyopt }
        //  1. Let env be the LexicalEnvironment of the running execution context.
        //  2. Let classEnv be NewDeclarativeEnvironment(env).
        //  3. If classBinding is not undefined, then
        //      a. Perform ! classEnv.CreateImmutableBinding(classBinding, true).
        //  4. Let outerPrivateEnvironment be the running execution context's PrivateEnvironment.
        //  5. Let classPrivateEnvironment be NewPrivateEnvironment(outerPrivateEnvironment).
        //  6. If ClassBody is present, then
        //      a. For each String dn of the PrivateBoundIdentifiers of ClassBody, do
        //          i. If classPrivateEnvironment.[[Names]] contains a Private Name pn such that pn.[[Description]] is
        //             dn, then
        //              1. Assert: This is only possible for getter/setter pairs.
        //          ii. Else,
        //              1. Let name be a new Private Name whose [[Description]] is dn.
        //              2. Append name to classPrivateEnvironment.[[Names]].
        //  7. If ClassHeritage is not present, then
        //      a. Let protoParent be %Object.prototype%.
        //      b. Let constructorParent be %Function.prototype%.
        //  8. Else,
        //      a. Set the running execution context's LexicalEnvironment to classEnv.
        //      b. NOTE: The running execution context's PrivateEnvironment is outerPrivateEnvironment when evaluating
        //         ClassHeritage.
        //      c. Let superclassRef be Completion(Evaluation of ClassHeritage).
        //      d. Set the running execution context's LexicalEnvironment to env.
        //      e. Let superclass be ? GetValue(? superclassRef).
        //      f. If superclass is null, then
        //          i. Let protoParent be null.
        //          ii. Let constructorParent be %Function.prototype%.
        //      g. Else if IsConstructor(superclass) is false, then
        //          i. Throw a TypeError exception.
        //      h. Else,
        //          i. Let protoParent be ? Get(superclass, "prototype").
        //          ii. If protoParent is not an Object and protoParent is not null, throw a TypeError exception.
        //          iii. Let constructorParent be superclass.
        //  9. Let proto be OrdinaryObjectCreate(protoParent).
        //  10. If ClassBody is not present, let constructor be empty.
        //  11. Else, let constructor be the ConstructorMethod of ClassBody.
        //  12. Set the running execution context's LexicalEnvironment to classEnv.
        //  13. Set the running execution context's PrivateEnvironment to classPrivateEnvironment.
        //  14. If constructor is empty, then
        //      a. Let defaultConstructor be a new Abstract Closure with no parameters that captures nothing and
        //         performs the following steps when called:
        //          i. Let args be the List of arguments that was passed to this function by [[Call]] or [[Construct]].
        //          ii. If NewTarget is undefined, throw a TypeError exception.
        //          iii. Let F be the active function object.
        //          iv. If F.[[ConstructorKind]] is derived, then
        //              1. NOTE: This branch behaves similarly to constructor(...args) { super(...args); }. The most
        //                 notable distinction is that while the aforementioned ECMAScript source text observably calls
        //                 the %Symbol.iterator% method on %Array.prototype%, this function does not.
        //              2. Let func be ! F.[[GetPrototypeOf]]().
        //              3. If IsConstructor(func) is false, throw a TypeError exception.
        //              4. Let result be ? Construct(func, args, NewTarget).
        //          v. Else,
        //              1. NOTE: This branch behaves similarly to constructor() {}.
        //              2. Let result be ? OrdinaryCreateFromConstructor(NewTarget, "%Object.prototype%").
        //          vi. Perform ? InitializeInstanceElements(result, F).
        //          vii. Return result.
        //      b. Let F be CreateBuiltinFunction(defaultConstructor, 0, className, « [[ConstructorKind]],
        //         [[SourceText]] », the current Realm Record, constructorParent).
        //  15. Else,
        //      a. Let constructorInfo be ! DefineMethod of constructor with arguments proto and constructorParent.
        //      b. Let F be constructorInfo.[[Closure]].
        //      c. Perform MakeClassConstructor(F).
        //      d. Perform SetFunctionName(F, className).
        //  16. Perform MakeConstructor(F, false, proto).
        //  17. If ClassHeritage is present, set F.[[ConstructorKind]] to derived.
        //  18. Perform ! DefineMethodProperty(proto, "constructor", F, false).
        //  19. If ClassBody is not present, let elements be a new empty List.
        //  20. Else, let elements be the NonConstructorElements of ClassBody.
        //  21. Let instancePrivateMethods be a new empty List.
        //  22. Let staticPrivateMethods be a new empty List.
        //  23. Let instanceFields be a new empty List.
        //  24. Let staticElements be a new empty List.
        //  25. For each ClassElement e of elements, do
        //      a. If IsStatic of e is false, then
        //          i. Let element be Completion(ClassElementEvaluation of e with argument proto).
        //      b. Else,
        //          i. Let element be Completion(ClassElementEvaluation of e with argument F).
        //      c. If element is an abrupt completion, then
        //          i. Set the running execution context's LexicalEnvironment to env.
        //          ii. Set the running execution context's PrivateEnvironment to outerPrivateEnvironment.
        //          iii. Return ? element.
        //      d. Set element to ! element.
        //      e. If element is a PrivateElement, then
        //          i. Assert: element.[[Kind]] is either method or accessor.
        //          ii. If IsStatic of e is false, let container be instancePrivateMethods.
        //          iii. Else, let container be staticPrivateMethods.
        //          iv. If container contains a PrivateElement pe such that pe.[[Key]] is element.[[Key]], then
        //              1. Assert: element.[[Kind]] and pe.[[Kind]] are both accessor.
        //              2. If element.[[Get]] is undefined, then
        //                  a. Let combined be PrivateElement { [[Key]]: element.[[Key]], [[Kind]]: accessor, [[Get]]:
        //                     pe.[[Get]], [[Set]]: element.[[Set]] }.
        //              3. Else,
        //                  a. Let combined be PrivateElement { [[Key]]: element.[[Key]], [[Kind]]: accessor, [[Get]]:
        //                     element.[[Get]], [[Set]]: pe.[[Set]] }.
        //              4. Replace pe in container with combined.
        //          v. Else,
        //              1. Append element to container.
        //      f. Else if element is a ClassFieldDefinition Record, then
        //          i. If IsStatic of e is false, append element to instanceFields.
        //          ii. Else, append element to staticElements.
        //      g. Else if element is a ClassStaticBlockDefinition Record, then
        //          i. Append element to staticElements.
        //  26. Set the running execution context's LexicalEnvironment to env.
        //  27. If classBinding is not undefined, then
        //      a. Perform ! classEnv.InitializeBinding(classBinding, F).
        //  28. Set F.[[PrivateMethods]] to instancePrivateMethods.
        //  29. Set F.[[Fields]] to instanceFields.
        //  30. For each PrivateElement method of staticPrivateMethods, do
        //      a. Perform ! PrivateMethodOrAccessorAdd(F, method).
        //  31. For each element elementRecord of staticElements, do
        //      a. If elementRecord is a ClassFieldDefinition Record, then
        //          i. Let result be Completion(DefineField(F, elementRecord)).
        //      b. Else,
        //          i. Assert: elementRecord is a ClassStaticBlockDefinition Record.
        //          ii. Let result be Completion(Call(elementRecord.[[BodyFunction]], F)).
        //      c. If result is an abrupt completion, then
        //          i. Set the running execution context's PrivateEnvironment to outerPrivateEnvironment.
        //          ii. Return ? result.
        //  32. Set the running execution context's PrivateEnvironment to outerPrivateEnvironment.
        //  33. Return F.

        // start:
        // --- if class-name is not on the stack ---
        //    STRING <class_name>             className
        // ---
        //                                    className
        //    PNLE                            className
        // --- if class binding is some ---
        //    CSILB <classBinding>            className
        // ---
        // --- if ClassHeritage is not present ---
        //    FUNC_PROTO                      constructorParent className
        //    OBJ_PROTO                       protoParent constructorParent className
        // --- else ---
        //    <ClassHeritage>                 err/superclassRef className
        //    SALE                            classEnv err/superclassRef className
        //    SWAP                            err/superclassRef classEnv className
        //    GPFS                            err/(protoParent constructorParent) classEnv className
        //    JUMP_ABRUPT unwind2             protoParent constructorParent classEnv className
        //    ROTATE_UP 3                     classEnv protoParent constructorParent className
        //    RLE                             protoParent constructorParent className
        // ---
        //    OBJ_WITH_PROTO                  proto constructorParent className
        //    PNPE                            proto constructorParent className
        // --- if ClassBody is present ---
        //    --- for name in ClassBody.PrivateBoundIdentifiers
        //    CPNIM <name>                    proto constructorParent className
        //    ---
        // ---
        // --- if constructor is empty ---    proto constructorParent className
        //    ROTATE_DN 3                     constructorParent className proto
        //    DEFAULT_CSTR                    F proto
        // --- else ---                       proto constructorParent className
        //    DUP                             proto proto constructorParent className
        //    ROTATE_DN 4                     proto constructorParent className proto
        //    constructor.<define_method>     err/(key F) className proto
        //    JUMP_IF_ABRUPT fix_envs_and_exit
        //    POP                             F className proto
        //    MAKE_CC_SN                      F proto
        // ---
        //    MAKE_CSTR_WITH_PROTO            F proto
        // --- if ClassHeritage exists ---
        //    SET_DERIVED                     F proto
        // ---
        //    DUP                             F F proto
        //    ROTATE_UP 3                     proto F F
        //    DUP                             proto proto F F
        //    ROTATE_DN 4                     proto F F proto
        //    SWAP                            F proto F proto
        //    STRING "constructor"            "constructor" F proto F proto
        //    DEFMETHPROP(false)              empty F proto
        //    POP                             F proto
        //
        //    ready_for_static = true
        //    count = 0
        // --- for element in elements ---              F proto elements...
        //    --- if ready_for_static != element.is_static() ---
        //    SWAP                                      proto F elements...
        //    ready_for_static = !ready_for_static
        //    ---
        //    <element.class_element_evaluation>        err/element proto/F proto/F elements...
        //    JUMP_IF_ABRUPT fix_envs_and_unwrap_elems(count)  element proto/F proto/F elements...
        //    ROTATE_DN 3                               proto/F proto/F element elements...
        //    count += 1
        // ---
        // --- if ready_for_static                      F proto elements...
        //    SWAP                                      proto F elements...
        // ---                                          proto F elements...
        //    POP                                       F elements...
        // --- if classBinding is not empty
        //    DUP                                       F F elements...
        //    ILB classBinding                          F elements...
        // ---
        //    PLE                                       F elements...
        //    ATTACH_ELEMENTS(count)                    err/F
        //    PPE                                       err/F
        //    JUMP exit                                 err/F
        // --- for each backref in fix_envs_and_unwrap_elems(count)
        // fix_envs_and_unwrap_elems(count)
        //    UNWIND count
        //    JUMP fix_envs_and_exit
        // ---
        // fix_envs_and_exit:
        //    PLE
        //    PPE
        // unwind2:
        //    UNWIND 2
        // exit:

        match class_name {
            NameLoc::OnStack => {}
            NameLoc::Index(idx) => {
                chunk.op_plus_arg(Insn::String, idx);
            }
        };
        chunk.op(Insn::PushNewLexEnv);
        if let Some(binding_idx) = class_binding {
            chunk.op_plus_arg(Insn::CreateStrictImmutableLexBinding, binding_idx);
        }
        let unwind_2 = match &self.heritage {
            None => {
                chunk.op(Insn::FunctionPrototype);
                chunk.op(Insn::ObjectPrototype);
                None
            }
            Some(heritage) => {
                heritage.compile(chunk, strict, text)?;
                chunk.op(Insn::SetAsideLexEnv);
                chunk.op(Insn::Swap);
                chunk.op(Insn::GetParentsFromSuperclass);
                let jmp_src = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op_plus_arg(Insn::RotateUp, 3);
                chunk.op(Insn::RestoreLexEnv);
                Some(jmp_src)
            }
        };
        chunk.op(Insn::ObjectWithProto);
        chunk.op(Insn::PushNewPrivateEnv);
        let constructor = if let Some(body) = &self.body {
            for binding in body.private_bound_identifiers() {
                let idx = chunk.add_to_string_pool(binding.name)?;
                chunk.op_plus_arg(Insn::CreatePrivateNameIfMissing, idx);
            }
            body.constructor_method()
        } else {
            None
        };

        let mut fix_envs_and_exit = vec![];

        match constructor {
            None => {
                chunk.op_plus_arg(Insn::RotateDown, 3);
                chunk.op(Insn::CreateDefaultConstructor);
            }
            Some(element) => {
                chunk.op(Insn::Dup);
                chunk.op_plus_arg(Insn::RotateDown, 4);
                element.define_method(chunk, strict, text)?;
                fix_envs_and_exit.push(chunk.op_jump(Insn::JumpIfAbrupt));
                chunk.op(Insn::Pop);
                chunk.op(Insn::MakeClassConstructorAndSetName);
            }
        }
        chunk.op(Insn::MakeConstructorWithProto);
        if self.heritage.is_some() {
            chunk.op(Insn::SetDerived);
        }
        chunk.op(Insn::Dup);
        chunk.op_plus_arg(Insn::RotateUp, 3);
        chunk.op(Insn::Dup);
        chunk.op_plus_arg(Insn::RotateDown, 4);
        chunk.op(Insn::Swap);
        let cstr_idx = chunk.add_to_string_pool(JSString::from("constructor"))?;
        chunk.op_plus_arg(Insn::String, cstr_idx);
        chunk.op_plus_arg(Insn::DefineMethodProperty, 0);
        chunk.op(Insn::Pop);

        let elements = if let Some(body) = &self.body { body.non_constructor_elements() } else { vec![] };

        let mut ready_for_static = true;
        let mut count = 0;
        let mut jump_targets = vec![];
        for e in elements {
            if ready_for_static != e.is_static() {
                chunk.op(Insn::Swap);
                ready_for_static = !ready_for_static;
            }
            let status = e.class_element_evaluation(chunk, strict, text)?;
            if status.maybe_abrupt() {
                let tgt = chunk.op_jump(Insn::JumpIfAbrupt);
                jump_targets.push((count, tgt));
            }
            chunk.op_plus_arg(Insn::RotateDown, 3);
            count += 1;
        }
        if ready_for_static {
            chunk.op(Insn::Swap);
        }
        chunk.op(Insn::Pop);
        if let Some(binding_idx) = class_binding {
            chunk.op(Insn::Dup);
            chunk.op_plus_arg(Insn::InitializeLexBinding, binding_idx);
        }
        chunk.op(Insn::PopLexEnv);
        chunk.op_plus_arg(Insn::AttachElements, count);
        chunk.op(Insn::PopPrivateEnv);

        let elements_can_throw = if jump_targets.is_empty() && fix_envs_and_exit.is_empty() {
            if let Some(unwind_backref) = unwind_2 {
                chunk.fixup(unwind_backref)?;
                chunk.op_plus_arg(Insn::UnwindIfAbrupt, 2);
            }
            false
        } else {
            let exit = chunk.op_jump(Insn::Jump);
            for (count, target) in jump_targets {
                chunk.fixup(target)?;
                chunk.op_plus_arg(Insn::Unwind, count);
                fix_envs_and_exit.push(chunk.op_jump(Insn::Jump));
            }
            for tgt in fix_envs_and_exit {
                chunk.fixup(tgt).expect("jump too short to fail");
            }
            chunk.op(Insn::PopLexEnv);
            chunk.op(Insn::PopPrivateEnv);
            if let Some(unwind_backref) = unwind_2 {
                chunk.fixup(unwind_backref)?;
            }
            chunk.op_plus_arg(Insn::Unwind, 2);
            chunk.fixup(exit).expect("jump too short to fail");
            true
        };

        Ok(AbruptResult::from(self.heritage.is_some() || elements_can_throw))
    }
}

impl ClassHeritage {
    fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<CompilerStatusFlags> {
        self.exp.compile(chunk, strict, text)
    }
}

impl ClassElement {
    fn define_method(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AlwaysAbruptResult> {
        match self {
            ClassElement::Standard { method } => method.define_method(chunk, strict, text),
            ClassElement::Static { .. }
            | ClassElement::Field { .. }
            | ClassElement::StaticField { .. }
            | ClassElement::StaticBlock { .. }
            | ClassElement::Empty { .. } => unreachable!(),
        }
    }

    fn class_element_evaluation(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        // Runtime Semantics: ClassElementEvaluation
        // The syntax-directed operation ClassElementEvaluation takes argument object (an Object) and returns either a
        // normal completion containing either a ClassFieldDefinition Record, a ClassStaticBlockDefinition Record, a
        // PrivateElement, or unused, or an abrupt completion. It is defined piecewise over the following productions:

        match self {
            ClassElement::Standard { method } => {
                // ClassElement :
                //      MethodDefinition
                //      static MethodDefinition
                //  1. Return ? MethodDefinitionEvaluation of MethodDefinition with arguments object and false.
                chunk.op(Insn::Dup);
                method.method_definition_evaluation(false, chunk, strict, text).map(AbruptResult::from)
            }
            ClassElement::Static { method, .. } => {
                // ClassElement :
                //      MethodDefinition
                //      static MethodDefinition
                //  1. Return ? MethodDefinitionEvaluation of MethodDefinition with arguments object and false.
                chunk.op(Insn::Dup);
                method.method_definition_evaluation(false, chunk, strict, text)?;
                chunk.op(Insn::StaticClassItem);
                Ok(AbruptResult::Maybe)
            }
            ClassElement::Field { field, .. } => {
                // ClassElement :
                //      FieldDefinition ;
                //  1. Return ? ClassFieldDefinitionEvaluation of FieldDefinition with argument object.
                // start:                                               obj
                //   <method.class_field_definition_eval(is_static)>    elem/err obj
                field.class_field_definition_evaluation(chunk, strict, text, Static::No)
            }
            ClassElement::StaticField { field, .. } => {
                // ClassElement :
                //      static FieldDefinition ;
                //  1. Return ? ClassFieldDefinitionEvaluation of FieldDefinition with argument object.
                // start:                                               obj
                //   <method.class_field_definition_eval(is_static)>    elem/err obj
                field.class_field_definition_evaluation(chunk, strict, text, Static::Yes)
            }
            ClassElement::StaticBlock { block } => {
                // ClassElement : ClassStaticBlock
                //  1. Return the ClassStaticBlockDefinitionEvaluation of ClassStaticBlock with argument object.
                // start:                                               obj
                //   <block.class_static_block_definition_evaluation>   elem/err obj
                block.class_static_block_definition_evaluation(chunk, strict).map(AbruptResult::from)
            }
            ClassElement::Empty { .. } => {
                // ClassElement : ;
                //  1. Return unused.
                chunk.op(Insn::Empty);
                Ok(AbruptResult::Never)
            }
        }
    }
}

impl ClassElementName {
    fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match self {
            ClassElementName::PropertyName(pn) => pn.compile(chunk, strict, text),
            ClassElementName::PrivateIdentifier { data, .. } => {
                // ClassElementName : PrivateIdentifier
                //  1. Let privateIdentifier be StringValue of PrivateIdentifier.
                //  2. Let privateEnvRec be the running execution context's PrivateEnvironment.
                //  3. Let names be privateEnvRec.[[Names]].
                //  4. Assert: Exactly one element of names is a Private Name whose [[Description]] is privateIdentifier.
                //  5. Let privateName be the Private Name in names whose [[Description]] is privateIdentifier.
                //  6. Return privateName.
                let private_identifier = data.string_value.clone();
                let private_identifier_idx = chunk.add_to_string_pool(private_identifier)?;
                chunk.op_plus_arg(Insn::PrivateIdLookup, private_identifier_idx);
                Ok(AbruptResult::Never)
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Static {
    No,
    Yes,
}

impl FieldDefinition {
    fn class_field_definition_evaluation(
        self: &Rc<Self>,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        staticness: Static,
    ) -> anyhow::Result<AbruptResult> {
        // Runtime Semantics: ClassFieldDefinitionEvaluation
        // The syntax-directed operation ClassFieldDefinitionEvaluation takes argument homeObject (an Object) and
        // returns either a normal completion containing a ClassFieldDefinition Record or an abrupt completion. It is
        // defined piecewise over the following productions:
        //
        // FieldDefinition : ClassElementName Initializeropt
        //  1. Let name be ? Evaluation of ClassElementName.
        //  2. If Initializeropt is present, then
        //      a. Let formalParameterList be an instance of the production FormalParameters : [empty] .
        //      b. Let env be the LexicalEnvironment of the running execution context.
        //      c. Let privateEnv be the running execution context's PrivateEnvironment.
        //      d. Let sourceText be the empty sequence of Unicode code points.
        //      e. Let initializer be OrdinaryFunctionCreate(%Function.prototype%, sourceText, formalParameterList,
        //         Initializer, NON-LEXICAL-THIS, env, privateEnv).
        //      f. Perform MakeMethod(initializer, homeObject).
        //      g. Set initializer.[[ClassFieldInitializerName]] to name.
        //  3. Else,
        //      a. Let initializer be EMPTY.
        //  4. Return the ClassFieldDefinition Record { [[Name]]: name, [[Initializer]]: initializer }.
        //
        // NOTE: The function created for initializer is never directly accessible to ECMAScript code.
        //
        // INPUT ON STACK         homeObject
        // OUTPUT ON STACK        FieldRecord/err homeObject

        // --- initialzer not present ---
        //   <name.evaluate>          name/err homeObject
        //   JUMP_IF_ABRUPT exit
        //   NAME_ONLY_FIELDRECORD    FieldRecord homeObject
        // exit:
        // --- else ---
        //   <name.evaluate>      // name/err homeObject
        //   JUMP_IF_ABRUPT exit
        //   EVAL_CLASS_FIELD_DEF(initializer)  // FieldRecord homeObject
        // exit:

        let status = self.name.compile(chunk, strict, text)?;
        let exit = if status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
        match &self.init {
            Some(init) => {
                let info = StashedFunctionData {
                    source_text: String::new(),
                    params: Rc::new(FormalParameters::Empty(Location::default())).into(),
                    body: init.clone().into(),
                    to_compile: self.clone().into(),
                    strict,
                    this_mode: ThisLexicality::NonLexicalThis,
                };
                let func_id = chunk.add_to_func_stash(info)?;
                let opcode = if staticness == Static::Yes {
                    Insn::EvaluateInitializedClassStaticFieldDefinition
                } else {
                    Insn::EvaluateInitializedClassFieldDefinition
                };
                chunk.op_plus_arg(opcode, func_id);
            }
            None => chunk.op(if staticness == Static::Yes {
                Insn::NameOnlyStaticFieldRecord
            } else {
                Insn::NameOnlyFieldRecord
            }),
        }
        if let Some(exit) = exit {
            chunk.fixup(exit).expect("jump too short to fail");
            Ok(AbruptResult::Maybe)
        } else {
            Ok(AbruptResult::Never)
        }
    }
}

impl ClassStaticBlock {
    pub fn class_static_block_definition_evaluation(
        self: &Rc<Self>,
        chunk: &mut Chunk,
        strict: bool,
    ) -> anyhow::Result<NeverAbruptRefResult> {
        // Runtime Semantics: ClassStaticBlockDefinitionEvaluation
        // The syntax-directed operation ClassStaticBlockDefinitionEvaluation takes argument homeObject (an Object) and
        // returns a ClassStaticBlockDefinition Record. It is defined piecewise over the following productions:
        //
        // ClassStaticBlock : static { ClassStaticBlockBody }
        //  1. Let lex be the running execution context's LexicalEnvironment.
        //  2. Let privateEnv be the running execution context's PrivateEnvironment.
        //  3. Let sourceText be the empty sequence of Unicode code points.
        //  4. Let formalParameters be an instance of the production FormalParameters : [empty] .
        //  5. Let bodyFunction be OrdinaryFunctionCreate(%Function.prototype%, sourceText, formalParameters,
        //     ClassStaticBlockBody, NON-LEXICAL-THIS, lex, privateEnv).
        //  6. Perform MakeMethod(bodyFunction, homeObject).
        //  7. Return the ClassStaticBlockDefinition Record { [[BodyFunction]]: bodyFunction }.
        //
        // NOTE: The function bodyFunction is never directly accessible to ECMAScript code.
        //
        // start:                                          obj
        //   EVAL_CSBD(code)                               err/elem obj
        let info = StashedFunctionData {
            source_text: String::new(),
            params: Rc::new(FormalParameters::Empty(Location::default())).into(),
            body: self.block.clone().into(),
            to_compile: self.clone().into(),
            strict,
            this_mode: ThisLexicality::NonLexicalThis,
        };
        let func_id = chunk.add_to_func_stash(info)?;
        chunk.op_plus_arg(Insn::EvaluateClassStaticBlockDefinition, func_id);
        Ok(NeverAbruptRefResult)
    }
}

impl ClassStaticBlockBody {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        chunk.op(Insn::FinishArgs);
        self.0.compile(chunk, strict, text)?;
        chunk.op(Insn::EndFunction);
        Ok(AbruptResult::Maybe)
    }
}

impl ClassStaticBlockStatementList {
    pub fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AbruptResult> {
        match self {
            ClassStaticBlockStatementList::Statements(sl) => sl.compile(chunk, strict, text),
            ClassStaticBlockStatementList::Empty(_) => {
                // ClassStaticBlockStatementList : [empty]
                //  1. Return undefined.
                chunk.op(Insn::Undefined);
                Ok(AbruptResult::Never)
            }
        }
    }
}

impl MethodDefinition {
    pub fn define_method(
        self: &Rc<Self>,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Stack at input:
        //    object prototype
        // stack at output:
        //    err/(PropertyKey Closure)
        match self.as_ref() {
            MethodDefinition::NamedFunction(class_element_name, unique_formal_parameters, function_body, location) => {
                // Runtime Semantics: DefineMethod
                // The syntax-directed operation DefineMethod takes argument object (an Object) and optional argument
                // functionPrototype (an Object) and returns either a normal completion containing a Record with fields
                // [[Key]] (a property key) and [[Closure]] (an ECMAScript function object) or an abrupt completion. It
                // is defined piecewise over the following productions:
                //
                // MethodDefinition : ClassElementName ( UniqueFormalParameters ) { FunctionBody }
                //  1. Let propKey be ? Evaluation of ClassElementName.
                //  2. Let env be the running execution context's LexicalEnvironment.
                //  3. Let privateEnv be the running execution context's PrivateEnvironment.
                //  4. If functionPrototype is present, then
                //      a. Let prototype be functionPrototype.
                //  5. Else,
                //      a. Let prototype be %Function.prototype%.
                //  6. Let sourceText be the source text matched by MethodDefinition.
                //  7. Let closure be OrdinaryFunctionCreate(prototype, sourceText, UniqueFormalParameters,
                //     FunctionBody, NON-LEXICAL-THIS, env, privateEnv).
                //  8. Perform MakeMethod(closure, object).
                //  9. Return the Record { [[Key]]: propKey, [[Closure]]: closure }.

                // start:                    object prototype
                //  <cen.evaluate>           propkey/err object prototype
                //  JUMP_IF_ABRUPT unwind_2  propkey object prototype
                //  ROTATE_DN 3              object prototype propkey
                //  DEFINE_METHOD(self)      err/closure propkey
                //  JUMP_IF_ABRUPT unwind_1  closure propkey
                //  SWAP                     propkey closure
                //  JUMP exit
                // unwind_2:                 err object prototype
                //  UNWIND 1                 err prototype
                // unwind_1:                 err (prototype/propkey)
                //  UNWIND 1                 err
                // exit:                     err/(propkey closure)

                let status = class_element_name.compile(chunk, strict, text)?;
                let unwind_2 = if status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                chunk.op_plus_arg(Insn::RotateDown, 3);
                let source_text =
                    text[location.span.starting_index..location.span.starting_index + location.span.length].to_string();
                let info = StashedFunctionData {
                    source_text,
                    params: ParamSource::from(unique_formal_parameters.clone()),
                    body: function_body.clone().into(),
                    to_compile: self.clone().into(),
                    strict,
                    this_mode: ThisLexicality::NonLexicalThis,
                };
                let idx = chunk.add_to_func_stash(info)?;
                chunk.op_plus_arg(Insn::DefineMethod, idx);
                let unwind_1 = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::Swap);
                let exit = chunk.op_jump(Insn::Jump);
                if let Some(spot) = unwind_2 {
                    chunk.fixup(spot).expect("jump too short to fail");
                    chunk.op_plus_arg(Insn::Unwind, 1);
                }
                chunk.fixup(unwind_1).expect("jump too short to fail");
                chunk.op_plus_arg(Insn::Unwind, 1);
                chunk.fixup(exit).expect("jump too short to fail");
                Ok(AlwaysAbruptResult)
            }
            MethodDefinition::Generator(_)
            | MethodDefinition::Async(_)
            | MethodDefinition::AsyncGenerator(_)
            | MethodDefinition::Getter(_, _, _)
            | MethodDefinition::Setter(_, _, _, _) => unreachable!(),
        }
    }

    fn method_definition_evaluation(
        self: &Rc<Self>,
        enumerable: bool,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: MethodDefinitionEvaluation
        //
        // The syntax-directed operation MethodDefinitionEvaluation takes arguments object (an Object) and enumerable (a
        // Boolean) and returns either a normal completion containing either a PrivateElement or UNUSED, or an abrupt
        // completion.

        // On the stack at input:
        //     object
        // On the stack at output:
        //     err/empty/PrivateElement
        match self.as_ref() {
            MethodDefinition::NamedFunction(_name, _params, _body, _) => {
                // MethodDefinition : ClassElementName ( UniqueFormalParameters ) { FunctionBody }
                //  1. Let methodDef be ? DefineMethod of MethodDefinition with argument object.
                //  2. Perform SetFunctionName(methodDef.[[Closure]], methodDef.[[Key]]).
                //  3. Return DefineMethodProperty(object, methodDef.[[Key]], methodDef.[[Closure]], enumerable).
                // start:                        object
                //  DUP                          object object
                //  FCN_PROTO                    prototype object object
                //  SWAP                         object prototype object
                //  <md.define_method>           err/(propertykey closure) object
                //  JUMP_IF_ABRUPT unwind        propertykey closure object
                //  SET_FUNC_NAME                propertykey closure object
                //  DEFMETHPROP(enumerable)      empty/PrivateElement
                // unwind:
                //  UNWIND_IF_ABRUPT 1

                chunk.op(Insn::Dup);
                chunk.op(Insn::FunctionPrototype);
                chunk.op(Insn::Swap);
                self.define_method(chunk, strict, text)?;
                let unwind = chunk.op_jump(Insn::JumpIfAbrupt);
                chunk.op(Insn::SetFunctionName);
                chunk.op_plus_arg(Insn::DefineMethodProperty, u16::from(enumerable));
                chunk.fixup(unwind).expect("Short jumps should work");
                chunk.op_plus_arg(Insn::UnwindIfAbrupt, 1);

                Ok(AlwaysAbruptResult)
            }
            MethodDefinition::Generator(gen) => gen.method_definition_evaluation(enumerable, chunk, strict, text),
            MethodDefinition::Async(_) => todo!(),
            MethodDefinition::AsyncGenerator(_) => todo!(),
            MethodDefinition::Getter(name, body, location) => {
                // MethodDefinition : get ClassElementName ( ) { FunctionBody }
                //  1. Let propKey be ? Evaluation of ClassElementName.
                //  2. Let env be the running execution context's LexicalEnvironment.
                //  3. Let privateEnv be the running execution context's PrivateEnvironment.
                //  4. Let sourceText be the source text matched by MethodDefinition.
                //  5. Let formalParameterList be an instance of the production FormalParameters : [empty] .
                //  6. Let closure be OrdinaryFunctionCreate(%Function.prototype%, sourceText, formalParameterList, FunctionBody, NON-LEXICAL-THIS, env, privateEnv).
                //  7. Perform MakeMethod(closure, object).
                //  8. Perform SetFunctionName(closure, propKey, "get").
                //  9. If propKey is a Private Name, then
                //      a. Return PrivateElement { [[Key]]: propKey, [[Kind]]: ACCESSOR, [[Get]]: closure, [[Set]]: undefined }.
                //  10. Else,
                //      a. Let desc be the PropertyDescriptor { [[Get]]: closure, [[Enumerable]]: enumerable, [[Configurable]]: true }.
                //      b. Perform ? DefinePropertyOrThrow(object, propKey, desc).
                //      c. Return UNUSED.

                // start:                                        object
                //   <name.evaluate>                             err/propKey object
                //   JUMP_IF_ABRUPT unwind_1                     propKey object
                //   DEFINE_GETTER                               err/empty/PrivateElement
                //   JUMP exit
                // unwind_1:                                     err object
                //   UNWIND 1                                    err
                // exit:                                         err/empty/PrivateElement
                let status = name.compile(chunk, strict, text)?;
                let unwind = if status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                let source_text =
                    text[location.span.starting_index..location.span.starting_index + location.span.length].to_string();
                let info = StashedFunctionData {
                    source_text,
                    params: ParamSource::from(Rc::new(FormalParameters::Empty(Location::default()))),
                    body: body.clone().into(),
                    to_compile: self.clone().into(),
                    strict,
                    this_mode: ThisLexicality::NonLexicalThis,
                };
                let idx = chunk.add_to_func_stash(info)?;
                chunk.op_plus_two_args(Insn::DefineGetter, idx, u16::from(enumerable));

                if let Some(unwind) = unwind {
                    let exit = chunk.op_jump(Insn::Jump);
                    chunk.fixup(unwind).expect("short jumps should be ok");
                    chunk.op_plus_arg(Insn::Unwind, 1);
                    chunk.fixup(exit).expect("short jumps should be ok");
                }

                Ok(AlwaysAbruptResult)
            }
            MethodDefinition::Setter(name, pl, body, location) => {
                // MethodDefinition : set ClassElementName ( PropertySetParameterList ) { FunctionBody }
                //  1. Let propKey be ? Evaluation of ClassElementName.
                //  2. Let env be the running execution context's LexicalEnvironment.
                //  3. Let privateEnv be the running execution context's PrivateEnvironment.
                //  4. Let sourceText be the source text matched by MethodDefinition.
                //  5. Let closure be OrdinaryFunctionCreate(%Function.prototype%, sourceText, PropertySetParameterList,
                //     FunctionBody, NON-LEXICAL-THIS, env, privateEnv).
                //  6. Perform MakeMethod(closure, object).
                //  7. Perform SetFunctionName(closure, propKey, "set").
                //  8. If propKey is a Private Name, then
                //      a. Return PrivateElement { [[Key]]: propKey, [[Kind]]: ACCESSOR, [[Get]]: undefined,
                //         [[Set]]: closure }.
                //  9. Else,
                //      a. Let desc be the PropertyDescriptor { [[Set]]: closure, [[Enumerable]]: enumerable, [[Configurable]]: true }.
                //      b. Perform ? DefinePropertyOrThrow(object, propKey, desc).
                //      c. Return UNUSED.

                // start:                                        object
                //   <name.evaluate>                             err/propKey object
                //   JUMP_IF_ABRUPT unwind_1                     propKey object
                //   DEFINE_SETTER                               err/empty/PrivateElement
                //   JUMP exit
                // unwind_1:                                     err object
                //   UNWIND 1                                    err
                // exit:                                         err/empty/PrivateElement
                let status = name.compile(chunk, strict, text)?;
                let unwind = if status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
                let source_text =
                    text[location.span.starting_index..location.span.starting_index + location.span.length].to_string();
                let info = StashedFunctionData {
                    source_text,
                    params: ParamSource::from(pl.clone()),
                    body: body.clone().into(),
                    to_compile: self.clone().into(),
                    strict,
                    this_mode: ThisLexicality::NonLexicalThis,
                };
                let idx = chunk.add_to_func_stash(info)?;
                chunk.op_plus_two_args(Insn::DefineSetter, idx, u16::from(enumerable));

                if let Some(unwind) = unwind {
                    let exit = chunk.op_jump(Insn::Jump);
                    chunk.fixup(unwind).expect("short jumps should be ok");
                    chunk.op_plus_arg(Insn::Unwind, 1);
                    chunk.fixup(exit).expect("short jumps should be ok");
                }

                Ok(AlwaysAbruptResult)
            }
        }
    }
}

impl GeneratorExpression {
    fn compile(self: &Rc<Self>, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AlwaysAbruptResult> {
        let id = match &self.ident {
            Some(ident) => Some(NameLoc::Index(chunk.add_to_string_pool(ident.string_value())?)),
            None => None,
        };
        self.instantiate_generator_function_expression(chunk, strict, text, id)
    }

    fn instantiate_generator_function_expression(
        self: &Rc<Self>,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        id: Option<NameLoc>,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: InstantiateGeneratorFunctionExpression
        // The syntax-directed operation InstantiateGeneratorFunctionExpression takes optional argument name (a property
        // key or a Private Name) and returns a function object. It is defined piecewise over the following productions:
        //
        // GeneratorExpression : function * ( FormalParameters ) { GeneratorBody }
        //  1. If name is not present, set name to "".
        //  2. Let env be the LexicalEnvironment of the running execution context.
        //  3. Let privateEnv be the running execution context's PrivateEnvironment.
        //  4. Let sourceText be the source text matched by GeneratorExpression.
        //  5. Let closure be OrdinaryFunctionCreate(%GeneratorFunction.prototype%, sourceText, FormalParameters,
        //     GeneratorBody, non-lexical-this, env, privateEnv).
        //  6. Perform SetFunctionName(closure, name).
        //  7. Let prototype be OrdinaryObjectCreate(%GeneratorFunction.prototype.prototype%).
        //  8. Perform ! DefinePropertyOrThrow(closure, "prototype", PropertyDescriptor { [[Value]]: prototype,
        //     [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false }).
        //  9. Return closure.
        if let Some(name_id) = match id {
            None => Some(chunk.add_to_string_pool(JSString::from(""))?),
            Some(NameLoc::Index(id)) => Some(id),
            Some(NameLoc::OnStack) => None,
        } {
            chunk.op_plus_arg(Insn::String, name_id);
        }

        let span = self.location().span;
        let source_text = text[span.starting_index..(span.starting_index + span.length)].to_string();
        let params = ParamSource::from(Rc::clone(&self.params));
        let body = BodySource::from(Rc::clone(&self.body));
        let function_data = StashedFunctionData {
            source_text,
            params,
            body,
            strict,
            to_compile: FunctionSource::from(self.clone()),
            this_mode: ThisLexicality::NonLexicalThis,
        };
        let func_id = chunk.add_to_func_stash(function_data)?;
        chunk.op_plus_arg(Insn::InstantiateGeneratorFunctionExpression, func_id);
        Ok(AlwaysAbruptResult)
    }

    fn named_evaluation(
        self_as_rc: &Rc<Self>,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
        id: Option<NameLoc>,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: NamedEvaluation
        // The syntax-directed operation NamedEvaluation takes argument name (a property key or a Private Name) and
        // returns either a normal completion containing a function object or an abrupt completion. It is defined
        // piecewise over the following productions:

        // GeneratorExpression : function * ( FormalParameters ) { GeneratorBody }
        //  1. Return InstantiateGeneratorFunctionExpression of GeneratorExpression with argument name.
        Self::instantiate_generator_function_expression(self_as_rc, chunk, strict, text, id)
    }
}

impl GeneratorDeclaration {
    pub fn compile_go_instantiation(
        self: &Rc<Self>,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: InstantiateGeneratorFunctionObject
        // The syntax-directed operation InstantiateGeneratorFunctionObject takes arguments env (an Environment Record)
        // and privateEnv (a PrivateEnvironment Record or null) and returns an ECMAScript function object. It is defined
        // piecewise over the following productions:
        //
        // GeneratorDeclaration : function * BindingIdentifier ( FormalParameters ) { GeneratorBody }
        //  1. Let name be the StringValue of BindingIdentifier.
        //  2. Let sourceText be the source text matched by GeneratorDeclaration.
        //  3. Let F be OrdinaryFunctionCreate(%GeneratorFunction.prototype%, sourceText, FormalParameters,
        //     GeneratorBody, NON-LEXICAL-THIS, env, privateEnv).
        //  4. Perform SetFunctionName(F, name).
        //  5. Let prototype be OrdinaryObjectCreate(%GeneratorFunction.prototype.prototype%).
        //  6. Perform ! DefinePropertyOrThrow(F, "prototype", PropertyDescriptor { [[Value]]: prototype, [[Writable]]:
        //     true, [[Enumerable]]: false, [[Configurable]]: false }).
        //  7. Return F.
        //
        // GeneratorDeclaration : function * ( FormalParameters ) { GeneratorBody }
        //  1. Let sourceText be the source text matched by GeneratorDeclaration.
        //  2. Let F be OrdinaryFunctionCreate(%GeneratorFunction.prototype%, sourceText, FormalParameters,
        //     GeneratorBody, NON-LEXICAL-THIS, env, privateEnv).
        //  3. Perform SetFunctionName(F, "default").
        //  4. Let prototype be OrdinaryObjectCreate(%GeneratorFunction.prototype.prototype%).
        //  5. Perform ! DefinePropertyOrThrow(F, "prototype", PropertyDescriptor { [[Value]]: prototype, [[Writable]]:
        //     true, [[Enumerable]]: false, [[Configurable]]: false }).
        //  6. Return F.
        //
        // NOTE An anonymous GeneratorDeclaration can only occur as part of an export default declaration, and its
        // function code is therefore always strict mode code.
        let name = if let Some(id) = &self.ident { id.string_value() } else { JSString::from("default") };
        let name_id = chunk.add_to_string_pool(name)?;
        let span = self.location().span;
        let source_text = text[span.starting_index..(span.starting_index + span.length)].to_string();
        let params = ParamSource::from(Rc::clone(&self.params));
        let body = BodySource::from(Rc::clone(&self.body));
        let function_data = StashedFunctionData {
            source_text,
            params,
            body,
            strict,
            to_compile: FunctionSource::from(self.clone()),
            this_mode: ThisLexicality::NonLexicalThis,
        };
        let func_id = chunk.add_to_func_stash(function_data)?;
        chunk.op_plus_two_args(Insn::InstantiateGeneratorFunctionObject, name_id, func_id);
        Ok(AlwaysAbruptResult)
    }
}

impl GeneratorMethod {
    fn method_definition_evaluation(
        self: &Rc<Self>,
        enumerable: bool,
        chunk: &mut Chunk,
        strict: bool,
        text: &str,
    ) -> anyhow::Result<AlwaysAbruptResult> {
        // Runtime Semantics: MethodDefinitionEvaluation
        // The syntax-directed operation MethodDefinitionEvaluation takes arguments object (an Object) and enumerable (a
        // Boolean) and returns either a normal completion containing either a PrivateElement or unused, or an abrupt
        // completion.
        //
        // GeneratorMethod : * ClassElementName ( UniqueFormalParameters ) { GeneratorBody }
        //  1. Let propKey be ? Evaluation of ClassElementName.
        //  2. Let env be the running execution context's LexicalEnvironment.
        //  3. Let privateEnv be the running execution context's PrivateEnvironment.
        //  4. Let sourceText be the source text matched by GeneratorMethod.
        //  5. Let closure be OrdinaryFunctionCreate(%GeneratorFunction.prototype%, sourceText, UniqueFormalParameters,
        //     GeneratorBody, non-lexical-this, env, privateEnv).
        //  6. Perform MakeMethod(closure, object).
        //  7. Perform SetFunctionName(closure, propKey).
        //  8. Let prototype be OrdinaryObjectCreate(%GeneratorFunction.prototype.prototype%).
        //  9. Perform ! DefinePropertyOrThrow(closure, "prototype", PropertyDescriptor { [[Value]]: prototype,
        //     [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false }).
        //  10. Return ? DefineMethodProperty(object, propKey, closure, enumerable).

        // start:                             obj
        //   <cen.evaluate>                   err/name obj
        //   JUMP_IF_ABRUPT unwind_1          name obj
        //   GEN_METHOD(enumerable)           err/empty/private_element
        //   JUMP exit
        // unwind_1:
        //   UNWIND 1
        // exit:

        let name_status = self.name.compile(chunk, strict, text)?;
        let unwind = if name_status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
        let span = self.location().span;
        let source_text = text[span.starting_index..(span.starting_index + span.length)].to_string();
        let params = ParamSource::from(Rc::clone(&self.params));
        let body = BodySource::from(Rc::clone(&self.body));
        let function_data = StashedFunctionData {
            source_text,
            params,
            body,
            strict,
            to_compile: FunctionSource::from(self.clone()),
            this_mode: ThisLexicality::NonLexicalThis,
        };
        let func_id = chunk.add_to_func_stash(function_data)?;
        chunk.op_plus_two_args(Insn::InstantiateGeneratorMethod, func_id, u16::from(enumerable));
        if let Some(unwind) = unwind {
            let exit = chunk.op_jump(Insn::Jump);
            chunk.fixup(unwind).expect("jump too short to fail");
            chunk.op_plus_arg(Insn::Unwind, 1);
            chunk.fixup(exit).expect("jump too short to fail");
        }
        Ok(AlwaysAbruptResult)
    }
}

impl GeneratorBody {
    pub fn evaluate_generator_body(
        &self,
        chunk: &mut Chunk,
        text: &str,
        info: &StashedFunctionData,
    ) -> anyhow::Result<AbruptResult> {
        // Runtime Semantics: EvaluateGeneratorBody
        // The syntax-directed operation EvaluateGeneratorBody takes arguments functionObject (an ECMAScript function
        // object) and argumentsList (a List of ECMAScript language values) and returns a throw completion or a return
        // completion. It is defined piecewise over the following productions:
        //
        // GeneratorBody : FunctionBody
        //  1. Perform ? FunctionDeclarationInstantiation(functionObject, argumentsList).
        //  2. Let G be ? OrdinaryCreateFromConstructor(functionObject, "%GeneratorFunction.prototype.prototype%", «
        //     [[GeneratorState]], [[GeneratorContext]], [[GeneratorBrand]] »).
        //  3. Set G.[[GeneratorBrand]] to EMPTY.
        //  4. Perform GeneratorStart(G, FunctionBody).
        //  5. Return Completion Record { [[Type]]: RETURN, [[Value]]: G, [[Target]]: EMPTY }.

        // At runtime, step 4 removes the current execution context from the stack and saves it away for later use.
        // Since this function is still compiling for that context, the next instruction should be the "later use" set
        // of instructions.

        // "Later use" is defined in GeneratorStart:
        //  4. Let closure be a new Abstract Closure with no parameters that captures generatorBody and performs the
        //     following steps when called:
        //      a. Let acGenContext be the running execution context.
        //      b. Let acGenerator be the Generator component of acGenContext.
        //      c. Let result be Completion(Evaluation of generatorBody).
        //      d. Assert: If we return here, the generator either threw an exception or performed either an implicit or
        //         explicit return.
        //      e. Remove acGenContext from the execution context stack and restore the execution context that is at the
        //         top of the execution context stack as the running execution context.
        //      f. Set acGenerator.[[GeneratorState]] to COMPLETED.
        //      g. NOTE: Once a generator enters the COMPLETED state it never leaves it and its associated execution
        //         context is never resumed. Any execution state associated with acGenerator can be discarded at this
        //         point.
        //      h. If result is a normal completion, then
        //          i. Let resultValue be undefined.
        //      i. Else if result is a return completion, then
        //          i. Let resultValue be result.[[Value]].
        //      j. Else,
        //          i. Assert: result is a throw completion.
        //          ii. Return ? result.
        //      k. Return CreateIterResultObject(resultValue, true).

        // start:                   N arg[N-1] ... arg[0] func
        //   <fdi>                  (err func) or (func)
        //   JUMP_IF_ABRUPT endf    func
        //   GENSTART_FUNC          func  (steps 2-5)
        //   <genbody>              err/val func
        //   JUMP exit
        // endf:
        //   END_FUNCTION
        // exit:

        let status = compile_fdi(chunk, text, info)?;
        let endf = if status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
        chunk.op(Insn::GeneratorStartFromFunction);

        let strict = info.strict || self.0.function_body_contains_use_strict();
        let _ = self.0.statements.compile(chunk, strict, text)?;

        if let Some(endf) = endf {
            let exit = chunk.op_jump(Insn::Jump);
            chunk.fixup(endf)?;
            chunk.op(Insn::EndFunction);
            chunk.fixup(exit).expect("Jump too short to fail");
        }

        Ok(AbruptResult::Maybe)
    }
}

impl SuperCall {
    fn compile(&self, chunk: &mut Chunk, strict: bool, text: &str) -> anyhow::Result<AlwaysAbruptResult> {
        // SuperCall : super Arguments
        //  1. Let newTarget be GetNewTarget().
        //  2. Assert: newTarget is an Object.
        //  3. Let func be GetSuperConstructor().
        //  4. Let argList be ? ArgumentListEvaluation of Arguments.
        //  5. If IsConstructor(func) is false, throw a TypeError exception.
        //  6. Let result be ? Construct(func, argList, newTarget).
        //  7. Let thisER be GetThisEnvironment().
        //  8. Perform ? thisER.BindThisValue(result).
        //  9. Let F be thisER.[[FunctionObject]].
        //  10. Assert: F is an ECMAScript function object.
        //  11. Perform ? InitializeInstanceElements(result, F).
        //  12. Return result.

        // start:
        //   GET_NEW_TARGET                        newTarget
        //   GET_SUPER_CSTR                        func newTarget
        //   <arguments.argument_list_evaluation>  err/(N arg(n-1) ... arg(0)) func newTarget
        //   JUMP_IF_ABRUPT unwind_2               N arg(n-1) ... arg(0) func newTarget
        //   ROTATE_LIST_DN 2                      func newTarget N arg(n-1) ... arg(0)
        //   CSTR_CHECK                            err/func newTarget N arg(n-1) ... arg(0)
        //   JUMP_IF_ABRUPT unwind_1_plus_list     func newTarget N arg(n-1) ... arg(0)
        //   SWAP                                  newTarget func N arg(n-1) ... arg(0)
        //   ROTATE_LIST_UP 2                      N arg(n-1) ... arg(0) newTarget func
        //   CONSTRUCT                             err/result
        //   JUMP_IF_ABRUPT exit                   result
        //   BIND_THIS_AND_INIT                    err/result    // steps 7-11.
        //   JUMP exit
        // unwind_1_plus_list:
        //   UNWIND 1
        //   UNWIND_LIST
        //   JUMP exit
        // unwind_2:
        //   UNWIND 2
        // exit:
        chunk.op(Insn::GetNewTarget);
        chunk.op(Insn::GetSuperConstructor);
        let args_status = self.arguments.argument_list_evaluation(chunk, strict, text)?;
        let unwind2 = if args_status.maybe_abrupt() { Some(chunk.op_jump(Insn::JumpIfAbrupt)) } else { None };
        chunk.op_plus_arg(Insn::RotateListDown, 2);
        chunk.op(Insn::ConstructorCheck);
        let unwind1 = chunk.op_jump(Insn::JumpIfAbrupt);
        chunk.op(Insn::Swap);
        chunk.op_plus_arg(Insn::RotateListUp, 2);
        chunk.op(Insn::Construct);
        let exit = chunk.op_jump(Insn::JumpIfAbrupt);

        chunk.op(Insn::BindThisAndInit);
        let exit2 = chunk.op_jump(Insn::Jump);
        chunk.fixup(unwind1).expect("jump too short to fail");
        chunk.op_plus_arg(Insn::Unwind, 1);
        chunk.op(Insn::UnwindList);
        if let Some(unwind) = unwind2 {
            let exit3 = chunk.op_jump(Insn::Jump);
            chunk.fixup(unwind).expect("jump too short to fail");
            chunk.op_plus_arg(Insn::Unwind, 2);
            chunk.fixup(exit3).expect("jump in range");
        }
        chunk.fixup(exit2).expect("jump in range");
        chunk.fixup(exit).expect("jump in range");
        Ok(AlwaysAbruptResult)
    }
}

#[cfg(test)]
mod tests;
